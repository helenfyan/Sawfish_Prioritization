# This script analyzes the data using regression trees #

library(tidyverse)
#library(MuMIn)
library(gbm)
library(ROCR)
library(ModelMetrics)
#library(caret)
library(pdp)
library(broom)
library(dismo)
library(beepr)
library(countrycode)

rawSpp <- read_csv('../../../Datasets/ProcessedCovariates_190119.csv')

rawDD <- read_csv('../../../Datasets/ProcessedDataDeficients_190208.csv')

# regression tree analysis ---------------------------------

sppData <-
  rawSpp %>%
  # remove collinear covariates
  dplyr::select(-X1, -ISO3, -logFishProd, -logIuu,
                -logChondLand, -EPI, -ReefFishers, -logFinUSD) %>%
  as.data.frame()


# clean data-deficient data ---------------------------

preddata <- 
  rawDD %>% 
  dplyr::select(-X1, -ISO3) %>% 
  as.data.frame()


# need to randomize the data ---------------------------
set.seed(123)

randomIndex <- sample(1:nrow(sppData), nrow(sppData))
sppData <- sppData[randomIndex, ]



# separate the data into training set (80%) and test set (20%) ------------------
n <- nrow(sppData)
ntrain <- round(0.8*n)
trainIndex <- sample(1:n, ntrain)

sppTrain <- sppData[trainIndex, ]
sppTest <- sppData[-trainIndex, ]

write.csv(sppTrain, '../../../Datasets/GBMtrain_191011.csv')
write.csv(sppTest, '../../../Datasets/GBMtest_191011.csv')

#sppTrain <- read_csv('../../../Datasets/GBMtrain_190119.csv')

sppTrain <- 
  sppTrain %>% 
  # need to reorder for analysis
  .[, c(6, 1:5, 7:20)] %>% 
  as.data.frame()

#sppTest <- read_csv('../../../Datasets/GBMtest_190119.csv')

sppTest <- 
  sppTest %>% 
  # need to reorder for analysis
  .[, c(6, 1:5, 7:20)] %>% 
  as.data.frame()

# tune a gbm to find hyperparameters ---------------------------
tunegrid <- expand.grid(shrinkage = c(0.0001, 0.005, 0.001),
                        interaction.depth = c(5, 10),
                        n.minobsinnode = c(5, 10),
                        bag.fraction = c(0.5, 0.9),
                        opttrees = 0,
                        rmse = 0)

prog <- 1
endtune <- 24
progbar <- txtProgressBar(min = 0, max = endtune, style = 3)

# grid search for hyperparameters
for(i in 1:nrow(tunegrid)) {
  
  setTxtProgressBar(progbar, value = prog)
  
  gbmtune <- gbm(formula = occurrence ~ .,
                 distribution = 'bernoulli',
                 data = sppTrain,
                 n.trees = 10000,
                 interaction.depth = tunegrid$interaction.depth[i],
                 shrinkage = tunegrid$shrinkage[i],
                 n.minobsinnode = tunegrid$n.minobsinnode[i],
                 bag.fraction = tunegrid$bag.fraction[i],
                 train.fraction = 0.75,
                 cv.folds = 10,
                 n.cores = NULL,
                 verbose = FALSE)
  
  # add min training error and trees to tunegrid
  tunegrid$opttrees[i] <- which.min(gbmtune$valid.error)
  tunegrid$rmse[i] <- sqrt(min(gbmtune$valid.error))
  
  prog <- prog + 1
}
beepr::beep()


# find best hyperparameter combination
tunegrid %>%
  dplyr::arrange(rmse) %>%
  head(50)

test <- gbm(formula = occurrence ~ .,
            distribution = 'bernoulli',
            data = sppTrain,
            n.trees = 5000,
            interaction.depth = 10,
            shrinkage = 0.001,
            bag.fraction = 0.5,
            n.minobsinnode = 5,
            cv.folds = 10,
            n.cores = 4,
            verbose = FALSE)

test
summary(test, cBars = 10)

testpred <- predict(object = test,
                    newdata = sppTest,
                    n.trees = 2700)


auc1 <- auc(actual = sppTest$occurrence, predicted = testpred)

print(auc1)

# evaluate model performance and obtain performance values -----------------

# make a df to infill with model eval values
cvresults <- expand.grid(run = seq(1:1000),
                         cvauc = 0,
                         cvdev = 0,
                         cvcorr = 0,
                         intnull = 0,
                         intdev = 0,
                         evresdev = 0,
                         evnulldev = 0,
                         evdev = 0,
                         evauc = 0)

# make empty list to infill with relative influence values
RIresults <- list()

# make empty dataframe to infill with predictions of the test set values

cvgbm <- gbm.step(data = sppTrain,
                  gbm.x = .,
                  gbm.y = 'occurrence',
                  family = 'bernoulli',
                  tree.complexity = 10,
                  learning.rate = 0.001,
                  bag.fraction = 0.5,
                  n.folds = 10)

pred <- predict.gbm(object = test, 
                    newdata = sppTest,
                    n.trees = test$gbm.call$best.trees,
                    type = 'response')

# make empty list to infill with predictions of data-deficient values
Predresults <- list()

for(i in 1:1000) {
  
  # randomize the data everytime
  randomI <- sample(1:nrow(sppTrain), nrow(sppTrain))
  randomSpp <- sppTrain[randomI, ]
  
  # run cv gbm
  cvgbm <- gbm.step(data = randomSpp,
                    gbm.x = 2:20,
                    gbm.y = 1,
                    family = 'bernoulli',
                    tree.complexity = 10,
                    learning.rate = 0.001,
                    bag.fraction = 0.5,
                    n.folds = 10)
  
  # extract model outputs
  cvresults$cvauc[i] <- cvgbm$cv.statistics$discrimination.mean
  cvresults$cvdev[i] <- cvgbm$cv.statistics$deviance.mean 
  cvresults$cvcorr[i] <- cvgbm$cv.statistics$correlation.mean
  cvresults$intnull[i] <- cvgbm$self.statistics$mean.null
  cvresults$intdev[i] <- (cvresults$intnull - cvresults$cvdev)/cvresults$intnull
  
  # model external metrics
  pred <- predict.gbm(object = cvgbm, 
                      newdata = sppTest,
                      n.trees = cvgbm$gbm.call$best.trees,
                      type = 'response')
  cvresults$evauc[i] <- gbm.roc.area(sppTest$occurrence, pred)
  cvresults$evresdev[i] <- calc.deviance(sppTest$occurrence, pred, calc.mean = TRUE)
  cvresults$evnulldev[i] <- calc.deviance(sppTest$occurrence, 
                                          rep(mean(sppTest$occurrence),
                                              nrow(sppTest)),
                                          calc.mean = TRUE)
  cvresults$evdev[i] <- (cvresults$evnulldev - cvresults$evresdev)/cvresults$evnulldev 
  
  # relative influence results
  relinf <- cvgbm$contributions
  relinf$run <- i
  
  RIresults[[i]] <- relinf

  # make predictions on data-deficient values
  pred <- 
    predict(object = cvgbm,
            newdata = preddata,
            n.trees = cvgbm$n.trees,
            type = 'response') %>% 
    as.data.frame()
  
  pred$ISO3 <- rawDD$ISO3
  pred$run <- i
  
  Predresults[[i]] <- pred
  
}
beepr::beep()

write.csv(cvresults, 'BRTBootstrapResults_190208.csv')
cvresults <- read_csv('BRTBootstrapResults_190208.csv')

cvsummary <- 
  cvresults %>% 
  mutate(cvr2 = cvcorr^2) %>% 
  summarise_all(funs(mean, sd))


totalRI <- data.frame()
totalPred <- data.frame()

for(i in 1:1000) {
  
  # bind all relative influences
  ridf <- RIresults[[i]]
  totalRI <- rbind(totalRI, ridf)
  
  # bind all predictions 
  preddf <- Predresults[[i]]
  totalPred <- rbind(totalPred, preddf)
  
}

write.csv(totalPred, 'GBMPredicted_190208.csv')
totalPred <- read_csv('../../../Datasets/GBMPredicted_190208.csv')

write.csv(totalRI, 'GBMRelativeInf_190208.csv')
totalRI <- read_csv('GBMRelativeInf_190208.csv')

# calculate RI for each variable 
RIsum <- 
  totalRI %>% 
  group_by(var) %>% 
  summarise(meanRI = mean(rel.inf),
            maxRI = max(rel.inf),
            minRI = min(rel.inf),
            sdRI = dplyr::sd(rel.inf)) %>% 
  arrange(desc(meanRI))

# calculate RI for each index 
RIind <- 
  RIsum %>% 
  dplyr::select(var, meanRI) %>% 
  mutate(index = case_when(var == 'logCoastLength'|
                             var == 'SstMean' |
                             var == 'logPprodMean'|
                             var == 'logEstDis' |
                             var == 'logMang' ~ 'ecology',
                           #var == 'logFinUSD' |
                             var == 'logChondCatch' |
                             var == 'logtotalGearTonnes' |
                             var == 'logCoastPop' |
                             var == 'logProteinDiet' ~ 'fishing',
                             #var == 'ReefFishers' ~ 'fishing',
                           var == 'NBI' |
                            # var == 'EPI' |
                             var == 'logGDP' |
                             var == 'HDI' |
                             var == 'OHI' |
                             var == 'WGI' ~ 'management',
                           var == 'speciesdwarf' |
                             var == 'speciesgreen' |
                             var == 'specieslarge' |
                             var == 'speciesnarrow' |
                             var == 'speciessmall' ~ 'species')) %>% 
  group_by(index) %>% 
  summarise(totalRI = sum(meanRI))

# clean predictions
predSum <- 
  totalPred %>% 
  dplyr::select(-X1) %>% 
  dplyr::rename(pred = '.') %>% 
  group_by(ISO3) %>% 
  summarise(predPres = mean(pred),
            predPresmin = min(pred),
            predPresmax = max(pred)) %>% 
  mutate(ISO3 = countrycode(ISO3, 'iso3c', 'country.name')) %>% 
  mutate(probExt = (1 - predPres) * 100,
         probExtmin = (1 - predPresmax) * 100,
         probExtmax = (1 - predPresmin) * 100) %>% 
  select(ISO3, probExt, probExtmin, probExtmax) %>%
  dplyr::arrange(desc(probExt))


# make all partial dependence plots ---------------------------------------------


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
dfvars <- paste0('GBMResults', vars, '_181211', sep = '')
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


#as.data.frame(matrix(alldf[[1]][, 2], nrow = 102, ncol = 1000)) loses the first col
testdf <- 
  alldf[[13]][1:306, ]

testdf3 <- 
  testdf %>% 
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
  separate(refid, into = c('logCoastLength', 'id'), sep = '_') %>% 
  mutate(logCoastLength = as.numeric(logCoastLength))

testplot <- 
  ggplot(testdf3, aes(x = logCoastLength, y = totalmean, group = 1)) +
  geom_ribbon(aes(ymin = totalmin, ymax = totalmax),
              alpha = 0.6, fill = 'steelblue4') +
  geom_line() +
  theme_classic()

print(testplot)


# Check the predictive accuracy of model with Cohen's Kappa -----------------------------------

# load and clean predictions dataset
pred_raw <- read_csv('../../../Datasets/BRTBootstrapResults_190208.csv')

pred <- 
  pred_raw %>% 
  dplyr::select(-X1) %>% 
  dplyr::rename('predicted_value' = '.')  

head(pred)

# load and clean actual values

# need to calculate predictive accuracy using Cohen's kappa for different value cut-offs


































