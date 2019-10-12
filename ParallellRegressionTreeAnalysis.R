# This script repeats the BRT with parallelization 

library(parallel)
library(doSNOW)
library(foreach)
library(dismo)
library(tidyverse)
library(gbm)
library(ROCR)
library(ModelMetrics)

n_cores <- parallel::detectCores()

rawSpp <- read_csv('../../../Datasets/ProcessedCovariates_190119.csv')
rawDD <- read_csv('../../../Datasets/ProcessedDataDeficients_190208.csv')

# regression tree analysis ---------------------------------

sppData <-
  rawSpp %>%
  # remove collinear covariates
  dplyr::select(-X1, -logFishProd, -logIuu,
                -logChondLand, -EPI, -ReefFishers, -logFinUSD) %>%
  as.data.frame() 

# clean data-deficient data ---------------------------

preddata <- 
  rawDD %>% 
  dplyr::select(-X1) %>% 
  as.data.frame()


# need to randomize the data ---------------------------
set.seed(123)

randomIndex <- sample(1:nrow(sppData), nrow(sppData))
sppData <- sppData[randomIndex, ]

# separate the data into training set (80%) and test set (20%) ------------------
n <- nrow(sppData)
ntrain <- round(0.8*n)
trainIndex <- sample(1:n, ntrain)

sppTrain_raw <- sppData[trainIndex, ]
sppTest_raw <- sppData[-trainIndex, ]

write.csv(sppTrain_raw, '../../../Datasets/GBMtrain_191011.csv')
write.csv(sppTest_raw, '../../../Datasets/GBMtest_191011.csv')

# clean and reorder the dataframes to make dismo happy
#sppTrain <- read_csv('../../../Datasets/GBMtrain_190119.csv')

sppTrain <- 
  sppTrain_raw %>% 
  # need to reorder for analysis
  .[, c(1, 7, 2:6, 8:21)] %>% 
  #mutate_at(c(8:21), list(~ scale(.))) %>% 
  as.data.frame()

#sppTest <- read_csv('../../../Datasets/GBMtest_190119.csv')

sppTest <- 
  sppTest_raw %>% 
  # need to reorder for analysis
  .[, c(1, 7, 2:6, 8:21)] %>% 
  #mutate_at(c(8:21), list(~ scale(.))) %>% 
  arrange(ISO3) %>% 
  as.data.frame()

# evaluate model performance and obtain performance values -----------------

# make a cluster
cl <- makeCluster(3)

# register the clusters - to find so it does what I want
registerDoSNOW(cl)

# let each cluster read from the environment
clusterExport(cl, c('preddata', 'sppTrain', 'sppTest'), envir = environment())
clusterEvalQ(cl, c(library(gbm), library(dismo),
                   library(tidyverse), library(ROCR),
                   library(ModelMetrics)))

# make a df to infill with model eval values
cvresults <- expand.grid(run = seq(1:1000),
                         cvauc = 0,
                         cvdev = 0,
                         cvcorr = 0,
                         intnull = 0,
                         #intdev = 0,
                         evresdev = 0,
                         #evnulldev = 0,
                         #evdev = 0,
                         evauc = 0)

# make empty list to infill with relative influence values
RIresults <- list()
totalRI <- data.frame()

# make empty dataframe to infill with predictions of the test set values
PredTest <- list()
totalTest <- data.frame()

# make empty list to infill with predictions of data-deficient values
Predresults <- list()
totalPred <- data.frame()

bootend <- 1000
foreach (i = 1:bootend) %dopar% {
  
  # randomize the data 
  randomI <- sample(1 : nrow(sppTrain), nrow(sppTrain))
  randomSpp <- sppTrain[randomI, ]
  
  # run cv gbm
  cvgbm <- 
    dismo::gbm.step(data = randomSpp,
             gbm.x = 3:21,
             gbm.y = 2, 
             family = 'bernoulli',
             tree.complexity = 10,
             learning.rate = 0.001,
             bag.fragtion = 0.5,
             n.folds = 10)
  
  # extract model outputs
  cvresults$cvauc[i] <- cvgbm$cv.statistics$discrimination.mean
  cvresults$cvdev[i] <- cvgbm$cv.statistics$deviance.mean 
  cvresults$cvcorr[i] <- cvgbm$cv.statistics$correlation.mean
  cvresults$intnull[i] <- cvgbm$self.statistics$mean.null
  #cvresults$intdev[i] <- (cvresults$intnull - cvresults$cvdev)/cvresults$intnull
  
  # model external metrics
  pred <- predict.gbm(object = cvgbm, 
                      newdata = sppTest,
                      n.trees = cvgbm$gbm.call$best.trees,
                      type = 'response')
  cvresults$evauc[i] <- gbm.roc.area(sppTest$occurrence, pred)
  cvresults$evresdev[i] <- calc.deviance(sppTest$occurrence, pred, calc.mean = TRUE)
  #cvresults$evnulldev[i] <- calc.deviance(sppTest$occurrence, 
   #                                       rep(mean(sppTest$occurrence),
    #                                          nrow(sppTest)),
     #                                     calc.mean = TRUE)
  #cvresults$evdev[i] <- (cvresults$evnulldev - cvresults$evresdev)/cvresults$evnulldev 
  
  # record predicted values for test set
  TestPred <- 
    sppTest %>% 
    dplyr::select(ISO3, occurrence) %>% 
    mutate(RunNo = paste(i)) %>% 
    mutate(PredValue = gbm::predict.gbm(object = cvgbm, 
                                   newdata = sppTest,
                                   n.trees = cvgbm$gbm.call$best.trees,
                                   type = 'response'))
  
  PredTest[[i]] <- TestPred
  
  # relative influence results
  relinf <- cvgbm$contributions
  relinf$run <- i
  
  RIresults[[i]] <- relinf
  
  # make predictions on data-deficient values
  pred <- 
    rawDD %>% 
    dplyr::select(ISO3) %>% 
    mutate(Prediction = predict(object = cvgbm,
                                newdata = preddata,
                                n.trees = cvgbm$n.trees,
                                type = 'response'),
           RunNo = paste(i))
  
  Predresults[[i]] <- pred
  
  # bind all relative influences
  ridf <- RIresults[[i]]
  totalRI <- rbind(totalRI, ridf)
  
  # bind all test predictions
  testdf <- PredTest[[i]]
  totalTest <- rbind(totalTest, testdf)
  
  # bind all predictions 
  preddf <- Predresults[[i]]
  totalPred <- rbind(totalPred, preddf)
  
  FileNameRI <- 
    paste('../../../ModelOutputs/GBM/191011_CvGBMRelInf', i, '.csv', sep = '_')
  FileNameTest <- 
    paste('../../../ModelOutputs/GBM/191011_CvGBMTestPred', i, '.csv', sep = '_')
  FileNamePred <- 
    paste('../../../ModelOutputs/GBM/191011_CvGBMPred', i,  '.csv', sep = '_')
  
  write.csv(totalRI, paste(FileNameRI))
  write.csv(totalTest, paste(FileNameTest))
  write.csv(totalPred, paste(FileNamePred))
  
  if (i%%2 == 0) {
    
    print(paste('Bootstrap ', i, ' done at ', Sys.time(), sep = ''))
    
  }
  
}
stopCluster(cl)
