# This script calculates Cohen's Kappa from predicted test set

library(tidyverse)
library(irr)
library(broom)
library(psych)

publication_theme <- function(axis_text_size = 13) {
  theme(panel.background = element_blank(),
        panel.grid = element_blank(),
        axis.line.y = element_line(colour = 'grey60'),
        axis.line.x = element_line(colour = 'grey60'),
        axis.text = element_text(size = axis_text_size, colour = 'grey20'),
        axis.title = element_text(size = 15))
}

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
  dplyr::select(-PredValue) %>% 
  pivot_longer(cols = starts_with('Pred_'), 
               names_to = 'cutoff_value',
               values_to = 'pred') %>% 
  arrange(cutoff_value)
  

head(cutoff_data)  

# function to turn kappa output into a dataframe 
tidy_kappa <- function(x) {
  broom::fix_data_frame(x$confid, newcol = "type")
}

kappa <- 
  cutoff_data %>% 
  group_by(RunNo, cutoff_value) %>% 
  do(tidy_kappa(cohen.kappa(cbind(.$occurrence, .$pred))))

head(kappa)

kappa_clean <- 
  kappa %>% 
  arrange(cutoff_value) %>% 
  group_by(cutoff_value, type) %>% 
  summarise(mean_k = mean(estimate),
            sd_k = sd(estimate))

kappa_plot <- 
  kappa %>% 
  mutate(cut_num = case_when(cutoff_value == 'Pred_30' ~ 30,
                             cutoff_value == 'Pred_35' ~ 35,
                             cutoff_value == 'Pred_40' ~ 40,
                             cutoff_value == 'Pred_45' ~ 45,
                             cutoff_value == 'Pred_50' ~ 50)) %>% 
  dplyr::filter(type == 'weighted kappa') %>% 
  ggplot(aes(x = cut_num, y = estimate, 
             colour = cutoff_value, fill = cutoff_value)) +
  geom_violin(trim = FALSE, alpha = 0.5) +
  #geom_boxplot(aes(group = cut_num)) +
  #geom_point(size = 2, alpha = 0.5, colour = 'darkslategray',
  #           position = position_jitterdodge()) +
  expand_limits(y = c(0.25, 0.55)) +
  publication_theme()
  

kappa_plot



















