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

rawSpp <- read_csv('../../../Datasets/ProcessedCovariates_200824.csv')
rawDD <- read_csv('../../../Datasets/ProcessedDataDeficients_200824.csv')

# regression tree analysis ---------------------------------

sppData <-
  rawSpp %>%
  # remove collinear covariates
  dplyr::select(-X1, -logFishProd, -logIuu,
                -logChondLand, -EPI, -ReefFishers, -logFinUSD,
                -logShelfAreaDeep, -logCoastLength, -logCoastLengthNew,
                -NBI, -OHI) %>%
  as.data.frame() 

# clean data-deficient data ---------------------------

preddata <- 
  rawDD %>% 
  dplyr::select(-X1,-logShelfAreaDeep, -logCoastLength, -logCoastLengthNew,
                -NBI, -OHI) %>% 
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

#write.csv(sppTrain_raw, '../../../Datasets/GBMtrain_191011.csv')
#write.csv(sppTest_raw, '../../../Datasets/GBMtest_191011.csv')

# clean and reorder the dataframes to make dismo happy
#sppTrain <- read_csv('../../../Datasets/GBMtrain_190119.csv')

sppTrain <- 
  sppTrain_raw %>% 
  # need to reorder for analysis
  #.[, c(15, 1:14, 16:20)] %>% 
  #mutate_at(c(8:21), list(~ scale(.))) %>% 
  dplyr::select(-ISO3) %>% 
  as.data.frame()

#sppTest <- read_csv('../../../Datasets/GBMtest_190119.csv')

sppTest <- 
  sppTest_raw %>% 
  # need to reorder for analysis
  #.[, c(1, 7, 2:6, 8:21)] %>% 
  #mutate_at(c(8:21), list(~ scale(.))) %>% 
  arrange(ISO3) %>% 
  as.data.frame()

# evaluate model performance and obtain performance values -----------------
# make a cluster
cl <- makeCluster(10)

# register the clusters - to find so it does what I want
registerDoSNOW(cl)

# let each cluster read from the environment
clusterExport(cl, c('preddata', 'sppTrain', 'sppTest'), envir = environment())
clusterEvalQ(cl, c(library(gbm), library(dismo),
                   library(tidyverse), library(ROCR),
                   library(ModelMetrics)))

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
             gbm.x = 2:19,
             gbm.y = 1, 
             family = 'bernoulli',
             tree.complexity = 10,
             learning.rate = 0.005,
             bag.fragtion = 0.5,
             n.folds = 10)

  # extract model outputs
  #cvresults$cvauc[i] <- cvgbm$cv.statistics$discrimination.mean
  #cvresults$cvdev[i] <- cvgbm$cv.statistics$deviance.mean 
  #cvresults$cvcorr[i] <- cvgbm$cv.statistics$correlation.mean
  #cvresults$intnull[i] <- cvgbm$self.statistics$mean.null
  #cvresults$intdev[i] <- (cvresults$intnull - cvresults$cvdev)/cvresults$intnull
  
  cvresults_nonpred <- 
    data.frame(run = paste(i)) %>% 
    mutate(cvauc = cvgbm$cv.statistics$discrimination.mean,
           cvdev = cvgbm$cv.statistics$deviance.mean,
           cvcorr = cvgbm$cv.statistics$correlation.mean,
           intnull = cvgbm$self.statistics$mean.null,
           intdev = (intnull - cvdev)/intnull,
           # not from devrill
           residual_deviance = cvgbm$self.statistics$mean.resid,
           dev_exp = (intnull - residual_deviance)/intnull)
  
  # model external metrics
  pred <- predict.gbm(object = cvgbm, 
                      newdata = sppTest,
                      n.trees = cvgbm$gbm.call$best.trees,
                      type = 'response')
  
  cvresults_pred <- 
    cvresults_nonpred %>% 
    mutate(evauc = gbm.roc.area(sppTest$occurrence, pred),
           evresdev = calc.deviance(sppTest$occurrence, pred, calc.mean = TRUE),
           evnulldev = calc.deviance(sppTest$occurrence, 
                                     rep(mean(sppTest$occurrence),
                                         nrow(sppTest)),
                                     calc.mean = TRUE),
           evdev = (evnulldev - evresdev)/evnulldev)
  
  #cvresults$evauc[i] <- gbm.roc.area(sppTest$occurrence, pred)
  #cvresults$evresdev[i] <- calc.deviance(sppTest$occurrence, pred, calc.mean = TRUE)
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
    paste('../../../ModelOutputs/GBM/200824_CvGBMRelInf', i, '.csv', sep = '_')
  FileNameTest <- 
    paste('../../../ModelOutputs/GBM/200824_CvGBMTestPred', i, '.csv', sep = '_')
  FileNamePred <- 
    paste('../../../ModelOutputs/GBM/200824_CvGBMPred', i,  '.csv', sep = '_')
  FileNameCv <- 
    paste('../../../ModelOutputs/GBM/200824_CvResults', i, '.csv', sep = '_')
  
  write.csv(totalRI, paste(FileNameRI))
  write.csv(totalTest, paste(FileNameTest))
  write.csv(totalPred, paste(FileNamePred))
  write.csv(cvresults_pred, paste(FileNameCv))
  
  if (i%%2 == 0) {
    
    print(paste('Bootstrap ', i, ' done at ', Sys.time(), sep = ''))
    
  }
  
}
beepr::beep(sound = 8)
stopCluster(cl)

# -----------------------------------------------------------------------------------------------
# Calculate CV results because it didn't work the first time ------------------------------------
# -----------------------------------------------------------------------------------------------
# make a cluster
cl <- makeCluster(12)

# register the clusters - to find so it does what I want
registerDoSNOW(cl)

# let each cluster read from the environment
clusterExport(cl, c('preddata', 'sppTrain', 'sppTest'), envir = environment())
clusterEvalQ(cl, c(library(gbm), library(dismo),
                   library(tidyverse), library(ROCR),
                   library(ModelMetrics)))

bootend <- 1000
foreach (i = 1:bootend) %dopar% {
  
  if (i%%2 == 0) {
    
    print(paste('Bootstrap ', i, ' started at ', Sys.time(), sep = ''))
    
  }
  
  # randomize the data 
  randomI <- sample(1 : nrow(sppTrain), nrow(sppTrain))
  randomSpp <- sppTrain[randomI, ]
  
  # run cv gbm
  cvgbm <- 
    dismo::gbm.step(data = randomSpp,
                    gbm.x = 3:20,
                    gbm.y = 2, 
                    family = 'bernoulli',
                    tree.complexity = 10,
                    learning.rate = 0.005,
                    bag.fragtion = 0.5,
                    n.folds = 10)
  
  # extract model outputs
  cvresults_nonpred <- 
    data.frame(run = paste(i)) %>% 
    mutate(cvauc = cvgbm$cv.statistics$discrimination.mean,
           cvdev = cvgbm$cv.statistics$deviance.mean,
           cvcorr = cvgbm$cv.statistics$correlation.mean,
           intnull = cvgbm$self.statistics$mean.null,
           intdev = (intnull - cvdev)/intnull,
           # not from devrill
           residual_deviance = cvgbm$self.statistics$mean.resid,
           dev_exp = (intnull - residual_deviance)/intnull)
  
  # model external metrics
  preds <- predict.gbm(object = cvgbm, 
                      newdata = sppTest,
                      n.trees = cvgbm$gbm.call$best.trees,
                      type = 'response')
  
  cvresults_pred <- 
    cvresults_nonpred %>% 
    mutate(evauc = gbm.roc.area(sppTest$occurrence, preds),
           evresdev = calc.deviance(sppTest$occurrence, preds, calc.mean = TRUE),
           evnulldev = calc.deviance(sppTest$occurrence, 
                                     rep(mean(sppTest$occurrence),
                                         nrow(sppTest)),
                                     calc.mean = TRUE),
           evdev = (evnulldev - evresdev)/evnulldev)
  
  FileNameCv <- 
    paste('../../../ModelOutputs/GBM/200814_CvResults', i, '.csv', sep = '_')
  
  write.csv(cvresults_pred, paste(FileNameCv))
  
}
beepr::beep(sound = 8)
stopCluster(cl)

# -----------------------------------------------------------------------------------------------
# Calculate partial dependence values for each variable -----------------------------------------
# -----------------------------------------------------------------------------------------------

# make a cluster
cl <- makeCluster(14)

# register the clusters - to find so it does what I want
registerDoSNOW(cl)

# clean the dataframe
sppTrain_pdp <- 
  sppTrain

# let each cluster read from the environment
clusterExport(cl, c('sppTrain', 'sppTrain_pdp',
                    'RandomTrainData'), envir = environment())
clusterEvalQ(cl, c(library(gbm), library(dismo),
                   library(tidyverse), library(ROCR),
                   library(ModelMetrics)))

PDPResults <- list()

bootend <- 1000
foreach (i = 1:bootend) %dopar% {
  
  RandomIndex <- sample(1:nrow(sppTrain_pdp), nrow(sppTrain_pdp))
  RandomTrainData <- sppTrain_pdp[RandomIndex, ]
  
  # train a gbm model
  pdpgbm <- gbm(formula = occurrence ~ .,
                distribution = 'bernoulli',
                data = RandomTrainData,
                n.trees = 4000,
                interaction.depth = 10,
                shrinkage = 0.005,
                bag.fraction = 0.5,
                cv.folds = 10,
                verbose = FALSE)
  
  for(j in names(sppTrain_pdp)[2:19]) {
    
    # create dataframes of pdp values predicted by gbm
    pdpDF <- 
      pdpgbm %>% 
      pdp::partial(pred.var = paste(j),
                   grid.resolution = 102,
                   n.trees = pdpgbm$n.trees,
                   prob = TRUE)
    
    pdpDF$run <- paste(i)
    
    PDPResults[[j]] <- 
      pdpDF %>% 
      mutate(variable = paste(j)) %>% 
      rename(independent_value = 1,
             pdp_value = 'yhat')
    
    # need to separately save each file because it sucks and you can't
    # rbind inside dopar properly
    FileName <- 
      paste('../../../ModelOutputs/GBM/PDP/200824_PDP', j, i,  '.csv', sep = '_')
    
    write.csv(PDPResults[[j]], paste(FileName))
    
  }
  
  if (i%%2 == 0) {
    
    print(paste('Bootstrap ', i, ' done at ', Sys.time(), sep = ''))
    
  }
  
}
beepr::beep(sound = 8)
stopCluster(cl)






# calculate mean, min, and max response of each variable ----------------
cols <- names(TrainData)
vars <- cols[2:20]
spreaddf <- list()

for(i in 1:19) {
  
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
}
