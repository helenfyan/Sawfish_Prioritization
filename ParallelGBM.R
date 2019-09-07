# This script runs the parallel GBM across cores 

install.packages('tidyverse')
install.packages('gbm')
install.packages('ROCR')
install.packages('ModelMetrics')
install.packages('pdp')
install.packages('broom')
install.packages('dismo')
install.packages('parallel')

library(parallel)
library(tidyverse)
library(gbm)
library(ROCR)
library(ModelMetrics)
library(pdp)
library(broom)
library(dismo)

setwd('')
trainSpp <- read_csv('GBMtrain_190119.csv')
testSpp <- read_csv('GBMtest_190119.csv')

total <- list()
bootend <- 1000
progbar <- txtProgressBar(min = 0, max = bootend, style = 3)

pdpdf <- lapply(seq(1:bootend), function(x) {
  
  for(i in names(sppTrain)[2:23]) {
    
    randomI <- sample(1:nrow(sppTrain), nrow(sppTrain))
    randomtest <- sppTrain[randomI, ]
    
    # train a gbm model
    pdpgbm <- gbm(formula = occurrence ~ .,
                  distribution = 'bernoulli',
                  data = randomtest,
                  n.trees = 3000,
                  interaction.depth = 10,
                  shrinkage = 0.001,
                  bag.fraction = 0.5,
                  cv.folds = 10,
                  n.cores = NULL,
                  verbose = FALSE)
    
    # create dataframes of pdp values predicted by gbm
    df <- 
      pdpgbm %>% 
      pdp::partial(pred.var = paste(i),
                   grid.resolution = 102,
                   n.trees = pdpgbm$n.trees,
                   prob = TRUE)
    
    df$run <- x
    total[[i]] <- df
    
    setTxtProgressBar(progbar, x)
    
  }
  return(total)
})


# write a loop to rbind each dataframe together

totaldf <- data.frame()
alldf <- 
  lapply(seq(1:22), function(x) {
    for(i in 1:1000) {
      
      singledf <- data.frame(pdpdf[[i]][[x]])
      totaldf <- rbind(totaldf, singledf)
      
    }
    return(totaldf)
  })


# turn each df into its own csv for saving

cols <- names(sppTrain)
vars <- cols[2:23]
dfvars <- paste0('GBMResults', vars, '_190121', sep = '')
spreaddf <- list()

for(i in 1:22) {
  
  spreaddf[[i]] <- 
    alldf[[i]] %>% 
    mutate(id = rep(1:102, length.out = n())) %>% 
    mutate(refid = paste(.[[1]], id, sep = '_')) %>% 
    group_by(refid) %>% 
    summarise(totalmean = mean(yhat),
              totalsd = sd(yhat),
              totaln = n(),
              totalse = totalsd/sqrt(totaln),
              totalmax = max(yhat),
              totalmin = min(yhat),
              lowerci = totalmean - qt(1 - (0.1/2), totaln - 1) * totalse,
              upperci = totalmean + qt(1 - (0.1/2), totaln - 1) * totalse) %>% 
    separate(refid, into = c(paste(vars[i]), 'id'), sep = '_') 
  
  write.csv(spreaddf[[i]], paste0(dfvars[i], '.csv'))
  
}

