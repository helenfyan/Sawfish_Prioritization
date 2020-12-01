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

rawSpp <- read_csv('../../../Datasets/ProcessedCovariates_200824.csv')

rawDD <- read_csv('../../../Datasets/ProcessedDataDeficients_200824.csv')

# regression tree analysis ---------------------------------

sppData <-
  rawSpp %>%
  # remove collinear covariates
  dplyr::select(-X1, -ISO3, -logFishProd, -logIuu,
                -logChondLand, -EPI, -ReefFishers, -logFinUSD,
                -logCoastLength, -logCoastLengthNew) %>%
  # remove what the reviewers what
  dplyr::select(-OHI, -NBI, -logShelfAreaDeep) %>% 
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

#write.csv(sppTrain, '../../../Datasets/GBMtrain_191011.csv')
#write.csv(sppTest, '../../../Datasets/GBMtest_191011.csv')

#sppTrain <- read_csv('../../../Datasets/GBMtrain_190119.csv')

sppTrain <- 
  sppTrain %>% 
  # need to reorder for analysis
  #.[, c(6, 1:5, 7:18)] %>% 
  as.data.frame()

#sppTest <- read_csv('../../../Datasets/GBMtest_190119.csv')

sppTest <- 
  sppTest %>% 
  # need to reorder for analysis
  #.[, c(6, 1:5, 7:18)] %>% 
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
            shrinkage = 0.005,
            bag.fraction = 0.5,
            n.minobsinnode = 5,
            cv.folds = 10,
            n.cores = 4,
            verbose = FALSE)

test
summary(test, cBars = 10)

testpred <- predict(object = test,
                    newdata = sppTest,
                    n.trees = 5000)


auc1 <- auc(actual = sppTest$occurrence, predicted = testpred)

print(auc1)

# evaluate model performance and obtain performance values -----------------

# make a df to infill with model eval values
cvresults <- expand.grid(run = seq(1:100),
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

# run this with coastline length (and no shelf area)
for(i in 1:100) {
  
  # randomize the data everytime
  randomI <- sample(1:nrow(sppTrain), nrow(sppTrain))
  randomSpp <- sppTrain[randomI, ]
  
  # run cv gbm
  cvgbm <- gbm.step(data = randomSpp,
                    gbm.x = 2:19,
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
  
  if (i%%2 == 0) {
    
    print(paste('Bootstrap ', i, ' done at ', Sys.time(), sep = ''))
    
  }
  
}
beepr::beep(sound = 8)

write.csv(cvresults, '../../../Datasets/GBM Out/200721/BRTBootstrapResultsCoastLength100_200721.csv')
cvresults <- read_csv('BRTBootstrapResults_190208.csv')

cvsummary <- 
  cvresults %>% 
  mutate(cvr2 = cvcorr^2) %>% 
  summarise_all(funs(mean, sd))


totalRI <- data.frame()
totalPred <- data.frame()

for(i in 1:100) {
  
  # bind all relative influences
  ridf <- RIresults[[i]]
  totalRI <- rbind(totalRI, ridf)
  
  # bind all predictions 
  preddf <- Predresults[[i]]
  totalPred <- rbind(totalPred, preddf)
  
}

write.csv(totalPred, '../../../Datasets/GBM Out/200630/GBMPredictedCoastLength100_200630.csv')
totalPred <- read_csv('../../../Datasets/GBMPredicted_190208.csv')

write.csv(totalRI, '../../../Datasets/GBM Out/200630/GBMRelativeInfCoastLength100_200630.csv')
totalRI <- read_csv('GBMRelativeInf_190208.csv')

# calculate RI for each variable 
RIsum <- 
  totalRI %>% 
  group_by(var) %>% 
  summarise(meanRI = mean(rel.inf),
            maxRI = max(rel.inf),
            minRI = min(rel.inf),
            sdRI = sd(rel.inf)) %>% 
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
  #dplyr::select(-X1) %>% 
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


#--------------------------------------------------------------------------------------------------
# run the BRT for shelf area instead of coastline leght -------------------------------------------
#--------------------------------------------------------------------------------------------------
rawSpp_shelf <- read_csv('../../../Datasets/ProcessedCovariates_200824.csv')

rawDD_shelf <- read_csv('../../../Datasets/ProcessedDataDeficients_200824.csv')

# regression tree analysis ---------------------------------

# shallow is coastlength
sppData_shallow <-
  rawSpp_shelf %>%
  # remove collinear covariates
  dplyr::select(-X1, -ISO3, -logFishProd, -logIuu,
                -logChondLand, -EPI, -ReefFishers, -logFinUSD) %>%
  # remove what the reviewers what
  dplyr::select(-OHI, -NBI, -logCoastLength,
                -logShelfAreaDeep, -logShelfAreaShallow) %>% 
  as.data.frame()


sppData_deep <-
  rawSpp_shelf %>%
  # remove collinear covariates
  dplyr::select(-X1, -ISO3, -logFishProd, -logIuu,
                -logChondLand, -EPI, -ReefFishers, -logFinUSD) %>%
  # remove what the reviewers what
  dplyr::select(-OHI, -NBI, -logCoastLength,
                -logShelfAreaShallow, -logCoastLengthNew) %>% 
  as.data.frame()

# clean data-deficient data ---------------------------

preddata_shallow <- 
  rawDD_shelf %>% 
  dplyr::select(-X1, -ISO3,
                -logShelfAreaDeep) %>% 
  as.data.frame()

preddata_deep <- 
  rawDD_shelf %>% 
  dplyr::select(-X1, -ISO3,
                -logShelfAreaShallow) %>% 
  as.data.frame()


# need to randomize the data ---------------------------
set.seed(123)

randomIndex <- sample(1:nrow(sppData), nrow(sppData))
sppData_shallow <- sppData_shallow[randomIndex, ]
sppData_deep <- sppData_deep[randomIndex, ]

# separate the data into training set (80%) and test set (20%) ------------------
n <- nrow(sppData)
ntrain <- round(0.8*n)
trainIndex <- sample(1:n, ntrain)

# shallow
sppTrain_shallow <- sppData_shallow[trainIndex, ]
sppTest_shallow <- sppData_shallow[-trainIndex, ]

# deep
sppTrain_deep <- sppData_deep[trainIndex, ]
sppTest_deep <- sppData_deep[-trainIndex, ]

# evaluate model performance and obtain performance values -----------------

# shallow first
# make a df to infill with model eval values
cvresults_shallow <- expand.grid(run = seq(1:100),
                         cvauc = 0,
                         cvdev = 0,
                         cvcorr = 0,
                         intnull = 0,
                         intdev = 0,
                         residual_deviance = 0,
                         dev_exp = 0,
                         evresdev = 0,
                         evnulldev = 0,
                         evdev = 0,
                         evauc = 0)

# make empty list to infill with relative influence values
RIresults_shallow <- list()

# make empty dataframe to infill with predictions of the test set values
Predresults_shallow <- list()

# run this with coastline length (and no shelf area)
for(i in 1:100) {
  
  # randomize the data everytime
  randomI <- sample(1:nrow(sppTrain_shallow), nrow(sppTrain_shallow))
  randomSpp <- sppTrain_shallow[randomI, ]
  
  # run cv gbm
  cvgbm <- gbm.step(data = randomSpp,
                    gbm.x = 2:19,
                    gbm.y = 1,
                    family = 'bernoulli',
                    tree.complexity = 10,
                    learning.rate = 0.001,
                    bag.fraction = 0.5,
                    n.folds = 10)
  
  # extract model outputs
  cvresults_shallow$cvauc[i] <- cvgbm$cv.statistics$discrimination.mean
  cvresults_shallow$cvdev[i] <- cvgbm$cv.statistics$deviance.mean 
  cvresults_shallow$cvcorr[i] <- cvgbm$cv.statistics$correlation.mean
  cvresults_shallow$intnull[i] <- cvgbm$self.statistics$mean.null
  cvresults_shallow$intdev[i] <- 
    (cvresults_shallow$intnull - cvresults_shallow$cvdev)/cvresults_shallow$intnull
  cvresults_shallow$residual_deviance[i] <-  cvgbm$self.statistics$mean.resid
  cvresults_shallow$dev_exp[i] <- 
    (cvresults_shallow$intnull - cvresults_shallow$residual_deviance)/cvresults_shallow$intnull
  
  # model external metrics
  pred <- predict.gbm(object = cvgbm, 
                      newdata = sppTest_shallow,
                      n.trees = cvgbm$gbm.call$best.trees,
                      type = 'response')
  
  cvresults_shallow$evauc[i] <- gbm.roc.area(sppTest_shallow$occurrence, pred)
  cvresults_shallow$evresdev[i] <- calc.deviance(sppTest_shallow$occurrence, pred, calc.mean = TRUE)
  cvresults_shallow$evnulldev[i] <- calc.deviance(sppTest_shallow$occurrence, 
                                          rep(mean(sppTest_shallow$occurrence),
                                              nrow(sppTest_shallow)),
                                          calc.mean = TRUE)
  cvresults_shallow$evdev[i] <- 
    (cvresults_shallow$evnulldev - cvresults_shallow$evresdev)/cvresults_shallow$evnulldev 
  
  # relative influence results
  relinf <- cvgbm$contributions
  relinf$run <- i
  
  RIresults_shallow[[i]] <- relinf
  
  # make predictions on data-deficient values
  pred <- 
    predict(object = cvgbm,
            newdata = preddata_shallow,
            n.trees = cvgbm$n.trees,
            type = 'response') %>% 
    as.data.frame()
  
  pred$ISO3 <- rawDD$ISO3
  pred$run <- i
  
  Predresults_shallow[[i]] <- pred
  
  if (i%%2 == 0) {
    
    print(paste('Bootstrap ', i, ' done at ', Sys.time(), sep = ''))
    
  }
  
}
beepr::beep(sound = 8)

write.csv(cvresults_shallow,
          '../../../Datasets/GBM Out/BRTBootstrapResultsCoastline_200825.csv')
#cvresults <- read_csv('../../../Datasets/GBM Out/200630/BRTBootstrapResultsShelfAreaShallow_200630.csv')

cvsummary_coast <- 
  cvresults_shallow %>% 
  mutate(cvr2 = cvcorr^2) %>% 
  summarise_all(~median(.))


totalRI_shallow <- data.frame()
totalPred_shallow <- data.frame()

for(i in 1:100) {
  
  # bind all relative influences
  ridf <- RIresults_shallow[[i]]
  totalRI_shallow <- rbind(totalRI_shallow, ridf)
  
  # bind all predictions 
  preddf <- Predresults_shallow[[i]]
  totalPred_shallow <- rbind(totalPred_shallow, preddf)
  
}

write.csv(totalPred_shallow, '../../../Datasets/GBM Out/200630/GBMPredictedShelfAreaShallow_200630.csv')
#totalPred <- read_csv('../../../Datasets/GBM Out/200630/GBMPredictedShelfAreaShallow_200630.csv')

write.csv(totalRI_shallow, '../../../Datasets/GBM Out/200630/GBMRelativeInfShelfAreaShallow_200630.csv')
#totalRI <- read_csv('../../../Datasets/GBM Out/200630/GBMRelativeInfShelfAreaShallow_200630.csv')

# calculate RI for each variable 
RIsum_shallow  <- 
  totalRI_shallow %>% 
  group_by(var) %>% 
  summarise(meanRI = mean(rel.inf),
            maxRI = max(rel.inf),
            minRI = min(rel.inf),
            sdRI = sd(rel.inf)) %>% 
  arrange(desc(meanRI))

# clean predictions
predSum_shallow  <- 
  totalPred_shallow  %>% 
  #dplyr::select(-X1) %>% 
  dplyr::rename(pred = '.') %>% 
  group_by(ISO3) %>% 
  summarise(predPres = mean(pred),
            predPresmin = min(pred),
            predPresmax = max(pred)) %>% 
  mutate(ISO3 = countrycode(ISO3, 'iso3c', 'country.name')) %>% 
  mutate(probExt = (1 - predPres) * 100,
         probExtmin = (1 - predPresmax) * 100,
         probExtmax = (1 - predPresmin) * 100) %>% 
  dplyr::select(ISO3, probExt, probExtmin, probExtmax) %>%
  dplyr::arrange(desc(probExt))



# deep ---------------------------------------------------------------------
sppTrain_deep <- sppData_deep[trainIndex, ]
sppTest_deep <- sppData_deep[-trainIndex, ]

# evaluate model performance and obtain performance values -----------------

# shallow first
# make a df to infill with model eval values
cvresults_deep <- expand.grid(run = seq(1:100),
                                 cvauc = 0,
                                 cvdev = 0,
                                 cvcorr = 0,
                                 intnull = 0,
                                 intdev = 0,
                              residual_deviance = 0,
                              dev_exp = 0,
                                 evresdev = 0,
                                 evnulldev = 0,
                                 evdev = 0,
                                 evauc = 0)

# make empty list to infill with relative influence values
RIresults_deep <- list()

# make empty dataframe to infill with predictions of the test set values
Predresults_deep <- list()

# run this with coastline length (and no shelf area)
for(i in 1:100) {
  
  # randomize the data everytime
  randomI <- sample(1:nrow(sppTrain_deep), nrow(sppTrain_deep))
  randomSpp <- sppTrain_deep[randomI, ]
  
  # run cv gbm
  cvgbm <- gbm.step(data = randomSpp,
                    gbm.x = 2:19,
                    gbm.y = 1,
                    family = 'bernoulli',
                    tree.complexity = 10,
                    learning.rate = 0.001,
                    bag.fraction = 0.5,
                    n.folds = 10)
  
  # extract model outputs
  cvresults_deep$cvauc[i] <- cvgbm$cv.statistics$discrimination.mean
  cvresults_deep$cvdev[i] <- cvgbm$cv.statistics$deviance.mean 
  cvresults_deep$cvcorr[i] <- cvgbm$cv.statistics$correlation.mean
  cvresults_deep$intnull[i] <- cvgbm$self.statistics$mean.null
  cvresults_deep$intdev[i] <- 
    (cvresults_deep$intnull - cvresults_deep$cvdev)/cvresults_deep$intnull
  cvresults_deep$residual_deviance[i] <-  cvgbm$self.statistics$mean.resid
  cvresults_deep$dev_exp[i] <- 
    (cvresults_deep$intnull - cvresults_deep$residual_deviance)/cvresults_deep$intnull
  
  # model external metrics
  pred <- predict.gbm(object = cvgbm, 
                      newdata = sppTest_deep,
                      n.trees = cvgbm$gbm.call$best.trees,
                      type = 'response')
  
  cvresults_deep$evauc[i] <- gbm.roc.area(sppTest_deep$occurrence, pred)
  cvresults_deep$evresdev[i] <- calc.deviance(sppTest_deep$occurrence, pred, calc.mean = TRUE)
  cvresults_deep$evnulldev[i] <- calc.deviance(sppTest_deep$occurrence, 
                                                  rep(mean(sppTest_deep$occurrence),
                                                      nrow(sppTest_deep)),
                                                  calc.mean = TRUE)
  cvresults_deep$evdev[i] <- 
    (cvresults_deep$evnulldev - cvresults_deep$evresdev)/cvresults_deep$evnulldev 
  
  # relative influence results
  relinf <- cvgbm$contributions
  relinf$run <- i
  
  RIresults_deep[[i]] <- relinf
  
  # make predictions on data-deficient values
  pred <- 
    predict(object = cvgbm,
            newdata = preddata_deep,
            n.trees = cvgbm$n.trees,
            type = 'response') %>% 
    as.data.frame()
  
  pred$ISO3 <- rawDD$ISO3
  pred$run <- i
  
  Predresults_deep[[i]] <- pred
  
  if (i%%2 == 0) {
    
    print(paste('Bootstrap ', i, ' done at ', Sys.time(), sep = ''))
    
  }
  
}
beepr::beep(sound = 8)

write.csv(cvresults_deep,
          '../../../Datasets/GBM Out/BRTBootstrapResultsShelfAreaDeep_200825.csv')
#cvresults <- read_csv('../../../Datasets/GBM Out/200630/BRTBootstrapResultsShelfAreaDeep_200630.csv')

cvsummary_deep <- 
  cvresults_deep%>% 
  mutate(cvr2 = cvcorr^2) %>% 
  summarise_all(~median(.))


totalRI_deep <- data.frame()
totalPred_deep <- data.frame()

for(i in 1:100) {
  
  # bind all relative influences
  ridf <- RIresults_deep[[i]]
  totalRI_deep <- rbind(totalRI_deep, ridf)
  
  # bind all predictions 
  preddf <- Predresults_deep[[i]]
  totalPred_deep <- rbind(totalPred_deep, preddf)
  
}

write.csv(totalPred_deep, '../../../Datasets/GBM Out/200630/GBMPredictedShelfAreaDeep_200630.csv')
#totalPred <- read_csv('../../../Datasets/GBM Out/200630/GBMPredictedShelfAreaShallow_200630.csv')

write.csv(totalRI_deep, '../../../Datasets/GBM Out/200630/GBMRelativeInfShelfAreaDeep_200630.csv')
#totalRI <- read_csv('../../../Datasets/GBM Out/200630/GBMRelativeInfShelfAreaShallow_200630.csv')

# calculate RI for each variable 
RIsum_deep  <- 
  totalRI_deep %>% 
  group_by(var) %>% 
  summarise(meanRI = mean(rel.inf),
            maxRI = max(rel.inf),
            minRI = min(rel.inf),
            sdRI = sd(rel.inf)) %>% 
  arrange(desc(meanRI))

# clean predictions
predSum_deep  <- 
  totalPred_deep  %>% 
  #dplyr::select(-X1) %>% 
  dplyr::rename(pred = '.') %>% 
  group_by(ISO3) %>% 
  summarise(predPres = mean(pred),
            predPresmin = min(pred),
            predPresmax = max(pred)) %>% 
  mutate(ISO3 = countrycode(ISO3, 'iso3c', 'country.name')) %>% 
  mutate(probExt = (1 - predPres) * 100,
         probExtmin = (1 - predPresmax) * 100,
         probExtmax = (1 - predPresmin) * 100) %>% 
  dplyr::select(ISO3, probExt, probExtmin, probExtmax) %>%
  dplyr::arrange(desc(probExt))
