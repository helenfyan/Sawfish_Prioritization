# This script analyzes the scaled data using regression trees #
# need to find the combination of metrics to minimize the error 

library(tidyverse)
library(randomForest)
library(MuMIn)
library(gbm)
library(ROCR)
library(ModelMetrics)
library(caret)

setwd('/users/helenyan/desktop/school/directed studies 2018/datasets')

rawSpp <- read_csv('CompleteSpeciesCovariates_181112.csv')
rawAll <- read_csv('CountryCovariates_181108.csv')

cleanSpp <-
  rawSpp %>%
  select(1:27) %>%
  # remove covariates based on PCA analysis
  select(-OHI, -EPI, -NBI, -HDI) %>%
  filter(occurrence != '2')

# GBM using species ----------------------------------------------------------

sppData <- 
  cleanSpp %>%
  select(4, 6, 8, 10:23) %>%
  mutate(species = as.factor(species)) %>%
  .[, c(2, 1, 3:17)]

gbmSpp1 <- gbm(formula = occurrence ~ .,
              data = sppData,
              distribution = 'bernoulli',
              n.trees = 10000,
              cv.folds = 5,
              shrinkage = 0.005)

print(gbmSpp1)
summary(gbmSpp1)

# this likely isn't right
auc(gbmSpp1)

best.iter <- gbm.perf(gbmSpp1, method = 'cv', plot.it = TRUE)


# GBM using species and specifying a training set and a test set -----------
set.seed(123)

n <- nrow(sppData)
nTrain <- round(0.8 * n)

trainIndices <- sample(1:n, nTrain)

sppTrain <- sppData[trainIndices, ]
sppTest <- sppData[-trainIndices, ]

gbmTrain <- gbm(formula = occurrence ~ .,
                data = sppTrain,
                distribution = 'bernoulli',
                n.trees = 10000,
                cv.folds = 5,
                shrinkage = 0.005)

print(gbmTrain)
summary(gbmTrain)

bestCv <- gbm.perf(gbmTrain, 
                   method = 'cv', 
                   plot.it = TRUE)

bestOob <- gbm.perf(gbmTrain, 
                    method = 'OOB',
                    plot.it = TRUE)

gbmPred <- predict(object = gbmTrain,
                   newdata = sppTest,
                   n.trees = best2)

auc1 <- auc(actual = sppTest$occurrence, predicted = gbmPred)


# --------------------------------------------------------------------------------
# CARET PACKAGE  -----------------------------------------------------------------
# --------------------------------------------------------------------------------

sppFac <-
  sppData %>%
  mutate(occurrence = as.factor(occurrence)) %>%
  mutate(occurrence = ifelse(occurrence == '1', 'present', 'absent'))

# create stratified random sample of data into training and test set
set.seed(998)

inTrainingFac <- createDataPartition(sppFac$occurrence, 
                                  p = 0.8,
                                  list = FALSE)

trainSppFac <- sppFac[inTraining, ]
testSppFac <- sppFac[-inTraining, ]

# resample using cross-validation
fitControl <- trainControl(method = 'cv',
                           number = 10,
                           #repeats = 10,
                           classProbs = TRUE,
                           summaryFunction = twoClassSummary)


# tuning grid 
gbmGrid <- expand.grid(interaction.depth = c(1, 5, 9),
                       n.trees = (1:50)*200,
                       shrinkage = c(0.1, 0.01),
                       n.minobsinnode = 10)

gbmFit1 <- train(occurrence ~ .,
                 data = trainSppFac,
                 method = 'gbm',
                 trControl = fitControl,
                 verbose = FALSE,
                 tuneGrid = gbmGrid,
                 metric = 'ROC')


best1 <- best(gbmFit1$results,
              metric = 'ROC',
              maximize = TRUE)

gbmFit1$results[best1, 1:7]

# predicting
preds <- predict(gbmFit1, newdata = head(testSppFac))


# without using a fac

 # merp --------------------------------------------------------------------------
cleanAll <-
  rawAll %>%
  filter(ISO3 != 'LAO') %>%
  mutate(country = case_when(ISO3 == 'GIN' ~ 'Guinea',
                             TRUE ~ as.character(country))) %>%
  select(country, ISO3, species, nospecies, occurrence,
         totalGearTonnes, totalGearValue, PprodMean,
         NBI, CoastPop, CoastLength, EPI,
         EstDis, HkExp, ProteinSup, GDP, HDI,
         OHI, Iuu, ManImp, ManMean, ReefFishers, Saltmarsh,
         ChondLand, WGI, SstCountMean) %>%
  rename(SstMean = SstCountMean) %>%
  # remove covariates based on PCA analysis
  select(-OHI, -EPI, -NBI, -HDI, -ReefFishers, -Saltmarsh, -totalGearValue, -ManImp)

# maximum model 
vars <- names(cleanAll)
covs <- vars[c(6:22)]
formula1 <- paste('occurrence ~ ', paste(covs, collapse = ' + '))

# make a dataset with only the variables ---------------------------
allData <-
  cleanAll %>%
  select(c(5:18))

sapply(allData, function(x) sum(is.na(x)))

# Gradient boosted regression analysis without species as a factor ------------
set.seed(123)

# create training set and test set --------
n <- nrow(allData)
nTrain <- round(0.8 * n)

trainIndices <- sample(1:n, nTrain)

allTrain <- allData[trainIndices, ]
allTest <- allData[-trainIndices, ]


allGbmModel <- gbm(formula = occurrence ~ .,
              distribution = 'bernoulli',
              data = allTrain,
              n.trees = 10000,
              cv.folds = 5)

ntreeOptCv <- gbm.perf(object = allGbm,
                       method = 'cv')

newAllGbm <- gbm(formula = occurrence ~ .,
                 distribution = 'bernoulli',
                 data = allTrain,
                 n.trees = 55,
                 cv.folds = 5)

ntreeOptCv2 <- gbm.perf(object = newAllGbm,
                        method = 'cv')


test <- gbm(formula = occurrence ~ .,
                 distribution = 'bernoulli',
                 data = allTrain,
                 n.trees = 19,
                 cv.folds = 5)

testntree <- gbm.perf(object = test,
                      method = 'cv')

predtest <- predict(object = test,
                    newdat = allTest,
                    n.trees = testntree)

auctest <- auc(actual = allTest$occurrence, predicted = predtest)




# generate predictions on the test set using ntreeOptCv ----
pred1 <- predict(object = allGbmModel,
                newdata = allTest,
                n.trees = ntreeOptCv)


# generate the test set AUC ------
auc1 <- auc(actual = allTest$occurrence, predicted = pred1)


# prediction and auc when n.trees cut to 55
pred2 <- predict(object = newAllGbm,
                 newdata = allTest,
                 n.trees = ntreeOptCv2)


auc2 <- auc(actual = allTest$occurrence, predicted = pred2)


# plot the ROCR curve
pred <- prediction(pred2, allTest$occurrence)
rocs <- performance(pred, 'tpr', 'fpr')
plot(rocs)


# dismo package ---------------------------------------------------------------
library(dismo)


































