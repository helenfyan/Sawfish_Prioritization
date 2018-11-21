# This script analyzes the unscaled data using regression trees #
# need to find the combination of metrics to minimize the error 

library(tidyverse)
library(randomForest)
library(MuMIn)
library(gbm)
library(ROCR)
library(ModelMetrics)
library(caret)

setwd('/users/helenyan/desktop/school/directed studies 2018/datasets')

rawSpp <- read_csv('CompleteSpeciesCovariates_181119.csv')

sppData <-
  rawSpp %>%
  select(1:26) %>%
  # remove covariates based on PCA analysis
  # select(-OHI, -EPI, -NBI, -HDI) %>%
  filter(occurrence != '2') %>%
  select(4, 6, 8:26) %>%
  mutate(species = as.factor(species))
  
sapply(sppData, function(x) sum(x == 0))

# check the distribution of every covariate ---------------------------

sppDatahist <-
  rawSpp %>%
  select(2, 8:26) %>%
  distinct(., ISO3, .keep_all = TRUE) %>%
  select(-ISO3) %>%
  gather(key = covs)


for(i in unique(sppDatahist$covs)) {
  
  print(
    sppDatahist %>%
      filter(covs == i) %>%
      ggplot(., aes(value)) +
      geom_histogram(bins = 30) +
      labs(title = paste(i), x = paste(i)) +
      theme_classic()
  )
}

# processing the data -------------------------------------------------

sppProc <-
  # create dummy variables for species
  rawSpp %>%
  filter(occurrence != '2') %>%
  select(2, 4, 6, 8:26) %>%
  mutate(refID = paste(ISO3, species, sep = '_')) %>%
  select(-ISO3) %>%
  mutate(var = 1) %>%
  spread(species, var, fill = 0, sep = '') %>%
  select(-refID) %>%
  .[, c(21:25, 1:20)] %>%
  # log transform FishProd, PprodMean, CoastPop, CoastLength, GDP, and Iuu
  mutate_at(vars('FishProd', 'PprodMean', 'CoastPop', 'CoastLength', 'GDP', 'Iuu'), log) %>%
  dplyr::rename(logFishProd = FishProd,
                logPprodMean = PprodMean,
                logCoastPop = CoastPop,
                logCoastLength = CoastLength,
                logGDP = GDP,
                logIuu = Iuu)


# NEED TO FIND GBM MODEL USING THE PROCESSED DATA FRAME   
# make a training set and a test set ----------------------------------
set.seed(123)

inTraining <- createDataPartition(sppData$occurrence,
                                  p = 0.8,
                                  list = FALSE)

trainSpp <- sppData[inTraining, ]
testSpp <- sppData[-inTraining, ]


# using gbm package ----------------------------------------------------

# create hyperparameter grid
hyperGrid <- expand.grid(
  shrinkage = c(0.01, 0.02, 0.08),
  interaction.depth = c(3, 5, 7),
  n.minobsinnode = c(10, 15, 17),
  # introduce stochastic gradient descent when bag.fraction < 1
  bag.fraction = c(0.8, 0.9, 0.95),
  optimal_trees = 0, # a place to dump results
  min_rmse = 0) # a place to dump results

# total number of combinations
nrow(hyperGrid)

# need to randomize data
randomIndex <- sample(1:nrow(trainSpp), nrow(trainSpp))
randomTrainSpp <- trainSpp[randomIndex, ]

# grid search
for(i in 1:nrow(hyperGrid)) {
  
  # reproducibility
  set.seed(123)
  
  # train model
  gbmTune <- gbm(
    formula = occurrence ~ .,
    distribution = 'bernoulli',
    data = randomTrainSpp,
    n.trees = 10000,
    interaction.depth = hyperGrid$interaction.depth[i],
    shrinkage = hyperGrid$shrinkage[i],
    n.minobsinnode = hyperGrid$n.minobsinnode[i],
    bag.fraction = hyperGrid$bag.fraction[i],
    train.fraction = 0.75,
    n.cores = NULL,
    verbose = FALSE
  )
  
  # add min training error and trees to grid
  hyperGrid$optimal_trees[i] <- which.min(gbmTune$valid.error)
  hyperGrid$min_rmse[i] <- sqrt(min(gbmTune$valid.error))
}

hyperGrid %>%
  dplyr::arrange(min_rmse) %>%
  head(10)

# train a GBM model

gbmFinal <- gbm(formula = occurrence ~ .,
                distribution = 'bernoulli',
                data = trainSpp,
                n.trees = 500,
                cv.folds = 5,
                interaction.depth = 5,
                shrinkage = 0.05,
                n.minobsinnode = 10,
                bag.fraction = 0.8,
                train.fraction = 1,
                n.cores = NULL,
                verbose = FALSE)

summary(gbmFinal,
        cBars = 10,
        #method = permutation.test.gbm)
        method = relative.influence)

optTree <- gbm.perf(object = gbmFinal,
                    method = 'cv',
                    plot.it = TRUE)

pred <- predict.gbm(gbmFinal,
                    newdat = testSpp,
                    n.trees = optTree)

auc(actual = testSpp$occurrence, predicted = pred)


# partial dependence plots -------------------------------------------

library(pdp)

coastpdp <- 
  gbmFinal %>%
  partial(pred.var = 'CoastLength',
          n.trees = gbmFinal$n.trees,
          grid.resolution = 100) %>%
  autoplot(rug = TRUE, train = trainSpp) +
  theme_classic()

print(coastpdp)


# ICE curve -------------------------------------
ice1 <-
  gbmFinal %>%
  partial(pred.var = 'CoastLength',
          n.trees = gbmFinal$n.trees,
          grid.resolution = 100,
          ice = TRUE) %>%
  autoplot(rug = TRUE, train = trainSpp, alpha = 0.1) +
  theme_classic() +
  ggtitle('Non-Centered')

ice2 <- 
gbmFinal %>%
  partial(pred.var = 'CoastLength',
          n.trees = gbmFinal$n.trees,
          grid.resolution = 100,
          ice = TRUE) %>%
  autoplot(rug = TRUE, train = trainSpp, alpha = 0.1, center = TRUE) +
  ggtitle('Centered') +
  theme_classic()

grid.arrange(ice1, ice2, nrow = 1)





gbm1 <- gbm(occurrence ~ .,
            data = trainSpp,
            distribution = 'bernoulli',
            n.trees = 10000,
            shrinkage = 0.001,
            cv.folds = 5,
            interaction.depth = 5,
            n.minobsinnode = 10)

summary(gbm1)

# get MSE and compute RMSE
sqrt(min(gbm1$cv.error))

optTree1 <- gbm.perf(object = gbm1,
                     method = 'cv',
                     plot.it = TRUE)

pred1 <- predict.gbm(gbm1, 
                     newdata = testSpp,
                     n.trees = optTree1)

auc1 <- auc(actual = testSpp$occurrence, predicted = pred1)


# using caret ----------------------------------------------------------
# use tuneGrid to get the optimal hyperparameters to put back into gbm
library(caret)

sppData2 <-
  sppData %>%
  mutate(occurrence = as.factor(ifelse(occurrence == 1, 'present', 'absent')))

set.seed(123)
fitControl <- trainControl(method = 'repeatedcv',
                           number = 5,
                           repeats = 10,
                           classProbs = TRUE,
                           summaryFunction = twoClassSummary)

inTraining2 <- createDataPartition(sppData2$occurrence,
                                   p = 0.8,
                                   list = FALSE)

trainSpp2 <- sppData2[inTraining2, ]
testSpp2 <- sppData2[-inTraining2, ]

gbmGrid <- expand.grid(interaction.depth = c(1, 5, 9),
                       n.trees = (1:200)*50,
                       shrinkage = c(0.1, 0.001),
                       n.minobsinnode = 10)

gbm2 <- train(occurrence ~ .,
              data = trainSpp2,
              method = 'gbm',
              trControl = fitControl,
              verbose = FALSE,
              tuneGrid = gbmGrid,
              metric = 'ROC')

print(gbm2)

# plot resampling profile
ggplot(gbm2)

best1 <- best(gbm2$results,
              metric = 'ROC',
              maximize = TRUE)

gbm2$results[best1, 1:7]












