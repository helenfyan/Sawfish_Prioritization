# This script goes through the regression tree analysis as well as the dynamic
# geography analysis

# Load libraries ----------------------------------------------------------------------
library(tidyverse)
library(gbm)
library(dismo)
library(ROCR)
library(ModelMetrics)
library(parallel)
library(doSNOW)
library(foreach)
library(brms)

# Part 1: Regression tree analysis ----------------------------------------------------
# This section involves running 1000 bootstrapped BRTs in parallel to calculate the 
# relative importance of each variable, calculate the partial dependence of each variable,
# and calculate the predicted probability of extinctions for the Presence Uncertain nations

# read-in data
OccData <- 
  read_csv('Data/OccurrenceData.csv')

BRTpred <- 
  read_csv('Data/PresenceUncertain.csv') %>% 
  # convert to a dataframe
  as.data.frame()

# for reproducibility
set.seed(123)

# start by randomizing the data
randomIndex <- sample(1:nrow(OccData), nrow(OccData))
OccData <- OccData[randomIndex, ]

# separate the data into a training set (80%) and a test set (20%)
n <- nrow(OccData)
nTrain <- round(0.8 * n)
trainIndex <- sample(1:n, nTrain)

BRTtrain <- OccData[trainIndex, ]
BRTtest <- OccData[-trainIndex, ]

# remove ISO3 identifier and covert to data frame
BRTtrain <- 
  BRTtrain %>% 
  # remove ISO3 column
  dplyr::select(-ISO3) %>% 
  as.data.frame()

BRTtest <- 
  BRTtest %>% 
  # remove ISO3 column
  #dplyr::select(-ISO3) %>% 
  as.data.frame()

# make a cluster
cl <- makeCluster(4)

# register the clusters
registerDoSNOW(cl)

# let each cluster read from the environment
clusterExport(cl, c('BRTpred', 'BRTtrain', 'BRTtest'),
              envir = environment())

clusterEvalQ(cl, c(library(tidyverse),
                   library(gbm),
                   library(dismo),
                   library(ROCR),
                   library(ModelMetrics)))

# make an empty list to infill with relative influence values
RIresults <- list()
totalRI <- data.frame()

# make an empty list to infill with predictions of the test set values
PredTest <- list()
totalTest <- data.frame()

# make empty list to infill with predictions of data-deficient values
Predresults <- list()
totalPred <- data.frame()

# make empty list to infill with PDP values
PDPResults <- list()

# run the BRTs
bootend <- 2

foreach (i = 1:bootend) %dopar% {
  
  # randomize the data again
  randomI <- sample(1:nrow(BRTtrain), nrow(BRTtrain))
  randomTrain <- BRTtrain[randomI, ]
  
  # run cross-validated gbm
  cvgbm <- 
    dismo::gbm.step(data = randomTrain,
                    gbm.x = 2:19,
                    gbm.y = 1,
                    family = 'bernoulli',
                    tree.complexity = 10,
                    learning.rate = 0.005,
                    bag.fraction = 0.5,
                    n.folds = 10)
  
  # write down the results 
  CVnonpred <- 
    data.frame(run = paste(i)) %>% 
    mutate(cvauc = cvgbm$cv.statistics$discrimination.mean, 
           cvcorr = cvgbm$cv.statistics$correlation.mean,
           intnull = cvgbm$self.statistics$mean.null,
           residual_deviance = cvgbm$self.statistics$mean.resid,
           dev_exp = (intnull - residual_deviance)/intnull)
  
  # model external metrics
  pred <- 
    predict.gbm(object = cvgbm,
                newdata = BRTtest,
                n.trees = cvgbm$gbm.call$best.trees,
                type = 'response')
  
  # combine all results
  CVresults <- 
    CVnonpred %>% 
    mutate(evauc = gbm.roc.area(BRTtest$occurrence, pred),
           evresdev = calc.deviance(BRTtest$occurrence, pred, calc.mean = TRUE),
           evnulldev = calc.deviance(BRTtest$occurrence, 
                                     rep(mean(BRTtest$occurrence),
                                         nrow(BRTtest)),
                                     calc.mean = TRUE),
           evdev = (evnulldev - evresdev)/evnulldev)
  
  # record predicted values for test set
  TestPred <- 
    BRTtest %>% 
    dplyr::select(ISO3, occurrence) %>% 
    mutate(run = paste(i),
           PredValue = gbm::predict.gbm(object = cvgbm,
                                        newdata = BRTtest,
                                        n.trees = cvgbm$gbm.call$best.trees,
                                        type = 'response'))
  
  PredTest[[i]] <- TestPred
  
  # record relative influences
  relinf <- cvgbm$contributions
  relinf$run <- i
  
  RIresults[[i]] <- relinf
  
  # make predictions on data-deficient values
  pred <- 
    BRTpred %>% 
    dplyr::select(ISO3) %>% 
    mutate(Prediction = predict(object = cvgbm,
                                newdata = BRTpred,
                                n.trees = cvgbm$n.trees,
                                type = 'response'),
           run = paste(i))
  
  Predresults[[i]] <- pred
  
  # bind together all relative influences
  RIdf <- RIresults[[i]]
  totalRI <- rbind(totalRI, RIdf)
  
  # bind together all test predictions
  testdf <- PredTest[[i]]
  totalTest <- rbind(totalTest, testdf)
  
  # bind together all predictions
  preddf <- Predresults[[i]]
  totalPred <- rbind(totalPred, preddf)
  
  # write a csv for each run - this results in duplicates but that will be
  # dealt with later
  FileNameRI <- 
    paste('ModelOutputs/BRT/CvGBMRelInf', i, '.csv', sep = '_')
  FileNameTest <- 
    paste('ModelOutputs/BRT/CvGBMTestPred', i, '.csv', sep = '_')
  FileNamePred <- 
    paste('ModelOutputs/BRT/CvGBMPred', i,  '.csv', sep = '_')
  FileNameCv <- 
    paste('ModelOutputs/BRT/CvResults', i, '.csv', sep = '_')
  
  write.csv(totalRI, paste(FileNameRI))
  write.csv(totalTest, paste(FileNameTest))
  write.csv(totalPred, paste(FileNamePred))
  write.csv(CVresults, paste(FileNameCv))
  
  
  # now let's calculate the partial dependence for each variable
  # train a gbm model
  pdpgbm <- gbm(formula = occurrence ~ .,
                distribution = 'bernoulli',
                data = BRTtrain,
                n.trees = 4000,
                interaction.depth = 10,
                shrinkage = 0.005,
                bag.fraction = 0.5,
                cv.folds = 10,
                verbose = FALSE)
  
  for(j in names(BRTtrain)[2:19]) {
    
    # create dataframes of pdp values predicted by gbm
    PDPdf <- 
      pdpgbm %>% 
      pdp::partial(pred.var = paste(j),
                   grid.resolution = 102,
                   n.trees = pdpgbm$n.trees,
                   prob = TRUE) 
    
    PDPdf$run <-  paste(i)
    
    PDPResults[[j]] <- 
      as_tibble(PDPdf) %>% 
      mutate(variable = paste0(j)) %>% 
      dplyr::rename(independent_value = 1,
                    pdp_value = 'yhat')
    
    # save each dataframe separately 
    FileNamePDP <- paste('ModelOutputs/BRT/PDP/PDP', j, i, '.csv', sep = '_')
    
    write.csv(PDPResults[[j]], paste(FileNamePDP))
  }
}
stopCluster(cl)

# Part 2: Dynamic geography analysis ------------------------------------------------------
# This section is a logistic regression regressing occurrence as a function of continental
# shelf area, gear-specific landings, mangrove area, and country ID as a grouping factor

DGmod <- 
  brm(occurrence ~ logShelfAreaShallow + logtotalGearTonnes + logMang + (1|ISO3),
      data = OccData,
      family = 'bernoulli',
      seed = 123)

savedRDS(DGmod, 'ModelOutputs/DGmod.rds')
