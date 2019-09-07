# This script cleans the BRT code to make it reproducible

library(tidyverse)

# Clean the master dataset and the data-deficient datasetes ------------------
raw <- read_csv('../../../Datasets/ProcessedCovariates_190119.csv')
dd <- read_csv('../../../Datasets/ProcessedDataDeficients_190208.csv')

# Full master dataset
master <- 
  raw %>% 
  select(-X1, -logFishProd, -logIuu,
         -logChondLand, -EPI, -ReefFishers, -logFinUSD) %>% 
  rename(logGearRestrictedLandings = logtotalGearTonnes,
         logPrimaryProd = logPprodMean,
         logMangrove = logMang,
         SST = SstMean) %>% 
  .[c(1, 7, 2:6, 8:21)]

write.csv(master,
          '../../../../../../Dropbox/HelenYan/SawfishStuff/Final/DATAmaster.csv')

# Full data-deficient dataset
deficient <- 
  dd %>% 
  select(-X1) %>% 
  rename(logGearRestrictedLandings = logtotalGearTonnes,
         logPrimaryProd = logPprodMean,
         logMangrove = logMang,
         SST = SstMean) %>% 
  .[c(1, 7, 2:6, 8:21)]

write.csv(deficient,
          '../../../../../../Dropbox/HelenYan/SawfishStuff/Final/DATAdDeficient.csv')

# Clean the training set and test set ------------------------------
trainRaw <- read_csv('../../../Datasets/GBMtrain_190119.csv')
testRaw <- read_csv('../../../Datasets/GBMtest_190119.csv')

train <-
  trainRaw %>% 
  select(-X1, -logFinUSD) %>% 
  rename(logGearRestrictedLandings = logtotalGearTonnes,
         logPrimaryProd = logPprodMean,
         logMangrove = logMang,
         SST = SstMean) %>% 
  .[c(6, 1:5, 7:20)]

write.csv(train,
          '../../../../../../Dropbox/HelenYan/SawfishStuff/Final/DATAtrainSet.csv')

testset <-
  testRaw %>% 
  select(-X1, -logFinUSD) %>% 
  rename(logGearRestrictedLandings = logtotalGearTonnes,
       logPrimaryProd = logPprodMean,
       logMangrove = logMang,
       SST = SstMean) %>% 
  .[c(6, 1:5, 7:20)]

write.csv(testset,
          '../../../../../../Dropbox/HelenYan/SawfishStuff/Final/DATAtestSet.csv')
