# This script analyzes the unscaled data using regression trees #
# need to find the combination of metrics to minimize the error 

library(tidyverse)
library(MuMIn)
library(gbm)
library(ROCR)
library(ModelMetrics)

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
  rawSpp %>%
  filter(occurrence != '2') %>%
  select(2, 4, 6, 8:26) %>%
  # create dummy variables for species
  mutate(refID = paste(ISO3, species, occurrence, sep = '_')) %>%
  select(-ISO3) %>%
  mutate(var = 1) %>%
  spread(species, var, fill = 0, sep = '') %>%
  separate(refID, into = c('ISO3', 'spp', 'occ'), sep = '_') %>%
  select(-spp, -occ) %>%
  .[, c(21, 1, 22:26, 2:20)] %>%
  # divide FinUSD/1000 like FAO raw presents
  mutate(FinUSD = FinUSD/1000) %>%
  # log+1 transform the data
  mutate_at(vars('FinUSD', 'ChondLand', 'FishProd', 'totalGearTonnes', 'PprodMean',
                 'CoastPop', 'CoastLength', 'EstDis', 'ProteinDiet', 'GDP', 
                 'Iuu', 'Mang'), log1p) %>%
  select(-ISO3)

# check normality around these variables ----------------

procHist <-
  test %>%
  distinct(., ISO3, .keep_all = TRUE) %>%
  select(-ISO3, -occurrence, -speciesdwarf, -speciesgreen, -specieslarge, 
         -speciesnarrow, -speciessmall) %>%
  gather()


for(i in unique(procHist$key)) {
  
  print(
    procHist %>%
      filter(key == i) %>%
      ggplot(., aes(value)) +
      geom_histogram(bins = 30) +
      labs(x = paste(i), title = paste(i)) +
      theme_classic()
  )
}

# make a training set and a test set ----------------------------------
set.seed(123)

inTraining <- createDataPartition(sppProc$occurrence,
                                  p = 0.8,
                                  list = FALSE)

trainSpp <- sppProc[inTraining, ]
testSpp <- sppProc[-inTraining, ]


# using gbm package ----------------------------------------------------

# create hyperparameter grid
hyperGrid <- expand.grid(
  shrinkage = c(0.0002, 0.005, 0.01, 0.03, 0.05, 0.08),
  interaction.depth = c(1, 3, 5, 7),
  n.minobsinnode = c(10, 15, 17),
  # introduce stochastic gradient descent when bag.fraction < 1
  bag.fraction = c(0.5, 0.6, 0.7, 0.8),
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
                n.trees = 53,
                cv.folds = 5,
                interaction.depth = 1,
                shrinkage = 0.08,
                n.minobsinnode = 15,
                bag.fraction = 0.7,
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


# partial dependence plots -------------------------------------------

library(pdp)

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


# write a loop to print a partial dependence plot for all variables -------------------

