# this plot makes the goldilocks plot for sawfishes

library(tidyverse)
library(gbm)
library(dismo)
library(countrycode)

rawSpp <- read_csv('../../../Datasets/ProcessedCovariates_190119.csv')
rawDD <- read_csv('../../../Datasets/ProcessedDataDeficients_190208.csv')

# show where the countries that are predicted with Pext for DD
# Make fishing 0 in DD countries and see how this shakes out

sppTrain <- read_csv('../../../Datasets/GBMtrain_190119.csv')

sppTrain <- 
  sppTrain %>% 
  dplyr::select(-X1, -logFinUSD) %>% 
  # need to reorder for analysis
  .[, c(6, 1:5, 7:20)] %>% 
  as.data.frame()

sppTest <- read_csv('../../../Datasets/GBMtest_190119.csv')

sppTest <- 
  sppTest %>% 
  dplyr::select(-X1, -logFinUSD) %>% 
  # need to reorder for analysis
  .[, c(6, 1:5, 7:20)] %>% 
  as.data.frame()

cvgbm <- gbm.step(data = sppTrain,
                  gbm.x = 2:20,
                  gbm.y = 1,
                  family = 'bernoulli',
                  tree.complexity = 10,
                  learning.rate = 0.001,
                  bag.fraction = 0.5,
                  n.folds = 10)



# clean data-deficient data ---------------------------

preddata <- 
  rawDD %>% 
  dplyr::select(-X1, -ISO3) %>% 
  as.data.frame()

# predicted actual
pred_act <- 
  predict(object = cvgbm,
          newdata = preddata,
          n.trees = cvgbm$n.trees,
          type = 'response') %>% 
  as.data.frame() %>% 
  dplyr::rename('pred' = '.') %>% 
  mutate(pred = 1 - pred,
         level_pred = 'actual') %>% 
  mutate(ISO3 = rawDD$ISO3) %>% 
  distinct(ISO3, .keep_all = TRUE)
  
head(pred_act)

# create sorting ID
sort_df <- 
  pred_act %>% 
  arrange(pred) %>% 
  mutate(sort_id = seq(1, 42, by = 1)) %>% 
  dplyr::select(ISO3, sort_id)

head(sort_df)

# predicted one zero fishing metric
pred_zero1_df <- 
  preddata %>% 
  dplyr::mutate(logProteinDiet = 0)

head(pred_zero1_df)

pred_zero1 <- 
  predict(object = cvgbm,
          newdata = pred_zero1_df,
          n.trees = cvgbm$n.trees,
          type = 'response') %>% 
  as.data.frame() %>% 
  dplyr::rename('pred' = '.') %>% 
  mutate(pred = 1 - pred,
         level_pred = 'zero1') %>% 
  mutate(ISO3 = rawDD$ISO3) %>% 
  distinct(ISO3, .keep_all = TRUE)

head(pred_zero1)

# predicted all zero fishing metric
pred_zero_df <- 
  preddata %>% 
  dplyr::mutate(logProteinDiet = 0,
                logChondCatch = 0,
                logtotalGearTonnes = 0,
                logCoastPop = 0)

head(pred_zero_df)

pred_zero <- 
  predict(object = cvgbm,
          newdata = pred_zero_df,
          n.trees = cvgbm$n.trees,
          type = 'response') %>% 
  as.data.frame() %>% 
  dplyr::rename('pred' = '.') %>% 
  mutate(pred = 1 - pred,
         level_pred = 'zero2') %>% 
  mutate(ISO3 = rawDD$ISO3) %>% 
  distinct(ISO3, .keep_all = TRUE)

head(pred_zero)

all_pred <- 
  bind_rows(pred_act, pred_zero1, pred_zero) %>% 
  mutate(country = countrycode(ISO3, 'iso3c', 'country.name')) %>% 
  left_join(., sort_df, by = c('ISO3' = 'ISO3'))
  

head(all_pred)

gold_plot <- 
  ggplot(all_pred, aes(x = pred, 
                       y = reorder(country, -sort_id), 
                       colour = level_pred)) +
  geom_line(data = all_pred, aes(x = pred, y = reorder(country, -sort_id), 
                                 group = country),
            colour = 'grey40') +
  geom_point(size = 5) +
  geom_vline(xintercept = 0.5, colour = 'grey70') + 
  labs(y = '',
       x = 'Probability of extinction',
       colour = 'Fishing Banned') +
  scale_colour_manual(values = c('#800026', '#e31a1c', '#fd8d3c'),
                      labels = c('No ban', 'Single ban', 'All banned')) +
  publication_theme() +
  theme(legend.key = element_blank())

gold_plot

ggsave('../../../Figures/Publication/Goldilocks.pdf', gold_plot,
       width = 20, height = 30, dpi = 600, units = c('cm'))










