# This script calculates Cohen's Kappa from predicted test set

library(tidyverse)
library(irr)

setwd('../../../ModelOutputs/GBM/')

# read in all of the .csv files
files_raw <- list.files(pattern = '191011_CvGBMTestPred_*')

files_read <- 
  lapply(files_raw, function(x) {
  
  the_data <- 
    read_csv(x) %>% 
    dplyr::select(-X1) %>% 
    # create a reference ID to elimiate duplicates
    mutate(refid = paste(ISO3, occurrence, RunNo, PredValue, sep = '_'))
  
  return(the_data)
  
})

# now bind all of the data together
all_data_bound <- data.frame()

for(i in 1:1000) {
  
  all_data_bound <- rbind(all_data_bound, files_read[[i]])
  
}

all_data <- 
  all_data_bound %>% 
  # get rid of duplicates 
  distinct(refid, .keep_all = TRUE) %>% 
  dplyr::select(-refid)

head(all_data)
nrow(all_data)

# want to create a figure measuring the accuracy (Cohen's Kappa) with different cut-offs

cutoff_data <- 
  all_data %>% 
  mutate(Pred_30 = case_when(PredValue <= 0.30 ~ 0,
                             PredValue >= 0.70 ~ 1,
                             TRUE ~ as.numeric(PredValue)),
         Pred_35 = case_when(PredValue <= 0.35 ~ 0,
                             PredValue >= 0.65 ~ 1,
                             TRUE ~ as.numeric(PredValue)),
         Pred_40 = case_when(PredValue <= 0.40 ~ 0,
                             PredValue >= 0.60 ~ 1,
                             TRUE ~ as.numeric(PredValue)),
         Pred_45 = case_when(PredValue <= 0.45 ~ 0,
                             PredValue >= 0.55 ~ 1,
                             TRUE ~ as.numeric(PredValue)),
         Pred_50 = case_when(PredValue <= 0.50 ~ 0,
                             PredValue > 0.50 ~ 1,
                             TRUE ~ as.numeric(PredValue))) %>% 
  arrange(RunNo)

head(cutoff_data)  
  
# now let's calculate kappa
test <- 
  cutoff_data %>% 
  dplyr::select(RunNo, occurrence, Pred_30)

test

kappa_30 <- 
  kappa2(test[, c(2, 3)])

kappa_30
str(kappa_30)
kappa_30$value

# I want to group_by RunNo and calculate the kappa between the
# true occurrence and the predicted cutoff
# so I want a df with a column RunNo, pred cut-off, and kapa number

long_cutoff <- 
  cutoff_data %>% 
  dplyr::select(-ISO3, -PredValue) %>% 
  gather(key = pred_value, value = measurement, -occurrence, -RunNo)

head(long_cutoff)

kappa_calc <- 
  long_cutoff %>% 
  group_by(RunNo, pred_value) %>% 
  do(kappa2())

# this works! Lit try to extract the kappa value to put into it's own dataframe
test <- 
  long_cutoff %>% 
  dplyr::filter(pred_value == 'Pred_30') %>% 
  select(occurrence, measurement) %>% 
  as.data.frame() %>% 
  kappa2(.)



str(test)

kappa_table <- data.frame()
new_kappa <- data.frame()

all_kappas <- 
  lapply(c('Pred_30', 'Pred_35', 'Pred_40', 'Pred_45', 'Pred_50'), 
         function(x) {
    
    # select column occurrence and cutoff 
    df <- 
      cutoff_data %>% 
      dplyr::select(RunNo, occurrence, x)
    
    # now calculate a kappa value for each run
    for(i in 1:1000) {
      
      df2 <- 
        df %>% 
        dplyr::filter(RunNo == i)
      
      kappa_value <- 
        kappa2(df2[, c(2, 3)])
      
      kappa_table$kappa[i] <- kappa_value$value
      
    }
    return(kappa_table)
  })

pred30 <- 
  all_kappas[[1]] %>% 
  dplyr::rename('Pred_30' = 'kappa')

head(pred30)

rename_kappa <- 
  function(data_no, newname) {
    
    the_data <- 
      all_kappas[[data_no]] %>% 
      dplyr::rename_(paste(newname) = kappa)
    
    return(the_data)
    
  }

pred30 <- rename_kappa(1, 'Pred_30')
head(pred30)
