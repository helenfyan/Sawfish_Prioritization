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

setwd('/users/helenyan/desktop/school/directed studies 2018/datasets')

rawSpp <- read_csv('ProcessedCovariates_181128.csv')

# test for collinearity ------------------------------------

fishprod <- cor.test(rawSpp$logCoastPop, rawSpp$logFishProd, method = 'pearson')
iuu <- cor.test(rawSpp$logIuu, rawSpp$logtotalGearTonnes, method = 'pearson')

# regression tree analysis ---------------------------------

sppData <-
  rawSpp %>%
  # remove collinear covariates
  dplyr::select(-X1, -ISO3, -logFishProd, -logIuu) %>%
  as.data.frame()

# need to randomize the data ---------------------------
randomIndex <- sample(1:nrow(sppData), nrow(sppData))
sppData <- sppData[randomIndex, ]

# separate the data into training set (80%) and test set (20%) ------------------
n <- nrow(sppData)
ntrain <- round(0.8*n)
trainIndex <- sample(1:n, ntrain)

sppTrain <- sppData[trainIndex, ]
sppTest <- sppData[-trainIndex, ]


# tune a gbm to find hyperparameters ---------------------------
tunegrid <- expand.grid(shrinkage = c(0.0001, 0.005, 0.001),
                        interaction.depth = c(2, 5, 10),
                        n.minobsinnode = c(5, 10, 17),
                        bag.fraction = c(0.5, 0.9),
                        opttrees = 0,
                        rmse = 0)

# grid search for hyperparameters
for(i in 1:nrow(tunegrid)) {
  
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
}


# find best hyperparameter combination
tunegrid %>%
  dplyr::arrange(rmse) %>%
  head(50)


test <- gbm(formula = occurrence ~ .,
            distribution = 'bernoulli',
            data = sppTrain,
            n.trees = 10000,
            interaction.depth = 10,
            shrinkage = 0.001,
            bag.fraction = 0.5,
            n.minobsinnode = 5,
            cv.folds = 10,
            n.cores = NULL,
            verbose = FALSE)

testpred <- predict(object = test,
                    newdata = sppTest,
                    n.trees = 3656)


auc1 <- auc(actual = sppTest$occurrence, predicted = testpred)

print(auc1)

# evaluate model performance and obtain performance values 

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


for(i in 1:1000) {
  
  # randomize the data everytime
  randomI <- sample(1:nrow(sppTrain), nrow(sppTrain))
  randomSpp <- sppTrain[randomI, ]
  
  # run cv gbm
  cvgbm <- gbm.step(data = randomSpp,
                    gbm.x = 2:23,
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
  
}


write.csv(cvresults, 'BRTBootstrapResults_181203.csv')

# make all partial dependence plots 



total <- list()
pdpdf <- 
  lapply(names(sppTrain)[2:23], function(x) {
    for(i in 1:1000) {
      
      # randomize the data everytime
      randomI <- sample(1:nrow(sppTrain), nrow(sppTrain))
      randomSpp <- sppTrain[randomI, ]
      
      # train a gbm model
      pdpgbm <- gbm(formula = occurrence ~ .,
                    distribution = 'bernoulli',
                    data = randomSpp,
                    n.trees = 3000,
                    interaction.depth = 10,
                    shrinkage = 0.001,
                    bag.fraction = 0.5,
                    cv.folds = 10,
                    n.cores = NULL,
                    verbose = FALSE)
      
      # create a dataframe of pdp values predicted by pdpgbm
      df <- 
        pdpgbm %>% 
        pdp::partial(pred.var = paste(x),
                     grid.resolution = 102,
                     n.trees = pdpgbm$n.trees,
                     prob = TRUE)
      df$run <- i
      total[[i]] <- df
    }
    return(total)
  })

hditest <- bind_rows(merp[[1]])


pdpresults <- 
  expand.grid(run = rep(1:2, times = 102),
                    dwarf = 0, dwarfy = 0,
                    green = 0, greeny = 0,
                    large = 0, largey = 0,
                    narrow = 0, narrowy = 0,
                    small = 0, smally = 0,
                    sst = 0, ssty = 0,
                    fin = 0, finy = 0,
                    chondland = 0, chondlandy = 0,
                    gear = 0, geary = 0,
                    pprod = 0, pprody = 0,
                    nbi = 0, nbiy = 0,
                    coastpop = 0, coastpopy = 0,
                    coastlength = 0, coastlengthy = 0,
                    epi = 0, epiy = 0,
                    estdis = 0, estdisy = 0,
                    prodiet = 0, prodiety = 0,
                    gdp = 0, gdpy = 0,
                    hdi = 0, hdiy = 0,
                    ohi = 0, ohiy = 0,
                    mang = 0, mangy = 0,
                    reef = 0, reefy = 0,
                    wgi = 0, wgiy = 0) %>% 
  arrange(run)



  
  
  hdi <- 
    pdpgbm %>% 
    pdp::partial(pred.var = 'HDI',
                 grid.resolution = 102,
                 n.trees = pdpgbm$n.trees,
                 prob = TRUE) %>% 
    dplyr::rename(hdiy = yhat)
  
  hdi$run <- i
  
  hditest[[i]] <- hdi
  
}

biglist <- dplyr::bind_rows(hditest)

pdpresults$hdi[i] <- hdi$HDI
pdpresults$hdiy[i] <- hdi$yhat



coastpdp <- 
  test %>%
  pdp::partial(pred.var = 'HDI',
               grid.resolution = 102,
               n.trees = test$n.trees,
               prob = TRUE) %>% 
  mutate(run = paste(i))







# train the GBM model using dismo package --------------------------
gbmfit <- gbm(formula = occurrence ~ .,
              distribution = 'bernoulli',
              data = sppTrain,
              n.trees = 5000,
              interaction.depth = 10,
              shrinkage = 0.001,
              bag.fraction = 0.5,
              cv.folds = 10,
              n.cores = NULL,
              verbose = FALSE )

opttrees <- gbm.perf(object = gbmfit,
                     method = 'cv')





# do the cross-validation separately with the gbm package -----------

# make the two data frames to input values

cvout <- expand.grid(cvfold = seq(1:10), cvcorr = 0, cvauc = 0,
                     cvdev = 0)

# randomize data then partition the data into 10-folds 

randomIndex <- sample(1:nrow(sppData), nrow(sppData))
randomSpp <- sppData[randomIndex, ]

k <- 10
n <- floor(nrow(randomSpp)/k)

for(i in 1:k) {
  
  # split the subsets
  s1 <- ((i - 1) * (n + 1))
  s2 <- (i * n)
  
  subs <- s1:s2
  
  spptrain <- randomSpp[-subs, ]
  spptest <- randomSpp[subs, ]
  
  # run the model
  gbmmod <- gbm(formula = occurrence ~ .,
                distribution = 'bernoulli',
                data = spptrain,
                n.trees = 5000,
                interaction.depth = 10,
                shrinkage = 0.001,
                bag.fraction = 0.5,
                n.cores = NULL,
                verbose = FALSE)
  
  # calculate cv value corr and cv dev
  cvout$cvcorr[i] <- cor(gbmmod$fit, spptrain$occurrence)
  cvout$cvdev[i] <- min(gbmmod$train.error)
  
  # optimum number of trees per run
  treeopt <- gbm.perf(object = gbmmod,
                      method = 'OOB')
  
  # calculate cv auc
  cvpred <- predict(object = gbmmod,
                    newdata = spptest,
                    n.trees = treeopt)
  cvout$cvauc[i] <- gbm.roc.area(spptest$occurrence, cvpred)
  
  # get the partial dependence values for each run 
  
}


# try to write a loop or function to repeat model 5 times and maintain the output

cvdf <- expand.grid(run = seq(1:5), auc = 0, corr = 0, dev = 0)
relinf <- list()
  #expand.grid(run = seq(1:10),
          #            vars = names(sppData),
          #            inf = 0)
  
for(i in 1:5) {
    
  # randomize the data everytime
  randomIndex <- sample(1:nrow(sppData), nrow(sppData))
  randomSpp <- sppData[randomIndex, ]
    
  # run the gbm
  allgbm <- gbm.step(data = randomSpp,
                     gbm.x = 2:25,
                     gbm.y = 1,
                     family = 'bernoulli',
                     tree.complexity = 10,
                     learning.rate = 0.001,
                     bag.fraction = 0.5,
                     n.folds = 10)
    
  # extract model outputs into df
  cvdf$auc[i] <- allgbm$cv.statistics$discrimination.mean
  cvdf$corr[i] <- allgbm$cv.statistics$correlation.mean
  cvdf$dev[i] <- allgbm$cv.statistics$deviance.mean
  
}

dismodf <-
  as.tibble(cvdf) %>%
  mutate(rsq = corr^2)

# dismo package using gbm.step ---------------------------------


allSppdf <- data.frame(sppData)

randomIndex <- sample(1:nrow(allSppdf), nrow(allSppdf))
randomSpp <- allSppdf[randomIndex, ]

dismogbm <- gbm.step(data = sppData,
                   gbm.x = 2:25,
                   gbm.y = 1,
                   family = 'bernoulli',
                   tree.complexity = 10,
                   learning.rate = 0.001,
                   bag.fraction = 0.5,
                   n.folds = 10)

gbm.plot(dismogbm, variable.no = 14, rug = TRUE, plot.layout = c(1, 1))


for(i in 1:24) {
  
  print(plot(alltest, i.var = i, type = 'response', rug = TRUE))
  
}

# gbm package --------------------------------------

gbmgbm <- gbm(formula = occurrence ~ .,
                distribution = 'bernoulli',
                data = sppData,
                n.trees = 5000,
                #cv.folds = 10,
                interaction.depth = 10,
                shrinkage = 0.001,
                bag.fraction = 0.5,
                n.cores = NULL,
                verbose = FALSE)

summary(gbmgbm, cBars = 10)

gbmopt <- gbm.perf(object = gbmgbm,
                   method = 'cv')
# gives cv deviance
dev <- min(gbmgbm$cv.error)

# gives cv correlation
gbmcvr <- (cor(gbmgbm$cv.fitted, sppData$occurrence))

# training auc? 

predgbmmat <- prediction(predgbm, sppData$occurrence)
perfgbmmat <- performance(predgbmmat, 'tpr', 'fpr')

rocgbm <- data.frame(FalsePositive = perfgbmmat@x.values[[1]],
                     TruePositive = perfgbmmat@y.values[[1]])

ggplot(rocgbm, aes(x = FalsePositive, y = TruePositive)) +
  geom_line(size = 1) +
  labs(title = 'GBM ROC Curve',
       x = 'False Positive Rate',
       y = 'True Positive Rate') +
  theme_classic()

# partial dependence plots -------------------------------------------
# pdp::partial needs a gbm object not gbm.step
coastpdp <- 
  gbmFinal %>%
  pdp::partial(pred.var = 'HDI',
               grid.resolution = 100,
               n.trees = gbmFinal$n.trees,
               prob = TRUE) 

test <- 
  gbmgbm %>% 
  pdp::partial(pred.var = c('speciesdwarf', 'speciesgreen',
                            'specieslarge', 'speciesnarrow',
                            'speciessmall'),
               grid.resolution = 100,
               n.trees = gbmgbm$n.trees,
               prob = TRUE)
  

  gbmFinal %>%
  pdp::partial(pred.var = 'HDI',
          n.trees = gbmFinal$n.trees,
          grid.resolution = 100,
          # gives the yaxis of the plot on the probably scale 
          prob = TRUE) %>%
  autoplot(rug = TRUE, train = sppData) +
  theme_classic()


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


