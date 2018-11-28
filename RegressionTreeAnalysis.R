# This script analyzes the data using regression trees #

library(tidyverse)
#library(MuMIn)
library(gbm)
#library(ROCR)
library(ModelMetrics)
#library(caret)
#library(pdp)

setwd('/users/helenyan/desktop/school/directed studies 2018/datasets')

rawSpp <- read_csv('ProcessedCovariates_181128.csv')

sppData <-
  rawSpp %>%
  dplyr::select(-X1, -ISO3)


# dismo package using gbm.step ---------------------------------

library(dismo)

allSppdf <- data.frame(sppData)

randomIndex <- sample(1:nrow(allSppdf), nrow(allSppdf))
randomSpp <- allSppdf[randomIndex, ]

allgbm <- gbm.step(data = randomSpp,
                   gbm.x = 2:25,
                   gbm.y = 1,
                   family = 'bernoulli',
                   tree.complexity = 10,
                   learning.rate = 0.001,
                   bag.fraction = 0.5,
                   n.folds = 10)

summary(allgbm, cBars = 10)


# partial dependence plots -------------------------------------------

coastpdp <- 
  gbmFinal %>%
  partial(pred.var = 'CoastLength',
          n.trees = gbmFinal$n.trees,
          grid.resolution = 100,
          # gives the yaxis of the plot on the probably scale 
          prob = TRUE) %>%
  autoplot(rug = TRUE, train = trainSpp) +
  theme_classic()

print(coastpdp)


# using gbm package ----------------------------------------------------

# create hyperparameter grid
hyperGrid <- expand.grid(
  shrinkage = c(0.0001, 0.005, 0.01, 0.05, 0.08, 0.1),
  interaction.depth = c(1, 3, 5, 7, 10),
  n.minobsinnode = c(5, 8, 10, 15, 17),
  # introduce stochastic gradient descent when bag.fraction < 1
  bag.fraction = c(0.5, 0.7, 0.9),
  optimal_trees = 0, # a place to dump results
  min_rmse = 0) # a place to dump results

# total number of combinations
nrow(hyperGrid)

# need to randomize data
randomIndex <- sample(1:nrow(trainSpp), nrow(trainSpp))
randomTrainSpp <- trainSpp[randomIndex, ]

# randomize complete dataset
randomI <- sample(1:nrow(sppProc), nrow(sppProc))
randomSpp <- sppProc[randomI, ]

# grid search
for(i in 1:nrow(hyperGrid)) {
  
  # reproducibility
  set.seed(123)
  
  # train model
  gbmTune <- gbm(
    formula = occurrence ~ .,
    distribution = 'bernoulli',
    data = randomSpp,
    n.trees = 10000,
    interaction.depth = hyperGrid$interaction.depth[i],
    shrinkage = hyperGrid$shrinkage[i],
    n.minobsinnode = hyperGrid$n.minobsinnode[i],
    bag.fraction = hyperGrid$bag.fraction[i],
    cv.folds = 10,
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
                data = sppProc,
                n.trees = 10000,
                cv.folds = 10,
                interaction.depth = 3,
                shrinkage = 0.1,
                n.minobsinnode = 8,
                bag.fraction = 0.5,
                train.fraction = 1,
                n.cores = NULL,
                verbose = FALSE)

summary(gbmFinal,
        cBars = 10,
        #method = permutation.test.gbm)
        method = relative.influence)



optTree <- gbm.perf(object = gbmFinal,
                    method = 'cv',
                    #method = 'OOB',
                    plot.it = TRUE)

pred <- predict.gbm(gbmFinal,
                    newdat = testSpp,
                    n.trees = 53,
                    # this keeps the response bounded between 0 and 1
                    type = 'response')

# plot a ROC curve
predMat <- prediction(pred, testSpp$occurrence)
perfMat <- performance(predMat, 'tpr', 'fpr')

ROCdf <- data.frame(FalsePositive = perfMat@x.values[[1]],
                    TruePositive = perfMat@y.values[[1]])

ggplot(ROCdf, aes(x = FalsePositive, y = TruePositive)) +
  geom_line(size = 1) +
  labs(title = 'GBM ROC Curve',
       x = 'False Positive Rate',
       y = 'True Positive Rate') +
  theme_classic()

aucGbm <- auc(actual = testSpp$occurrence, predicted = pred)


