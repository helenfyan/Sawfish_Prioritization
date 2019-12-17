# this plot makes the goldilocks plot for sawfishes

library(tidyverse)
library(gbm)
library(dismo)
library(countrycode)
library(cowplot)

publication_theme <- function(axis_text_size = 13) {
  theme(panel.background = element_blank(),
        panel.grid = element_blank(),
        axis.line.y = element_line(colour = 'grey60'),
        axis.line.x = element_line(colour = 'grey60'),
        axis.text = element_text(size = axis_text_size, colour = 'grey20'),
        axis.title = element_text(size = 15),
        legend.text = element_text(size = 9, colour = 'grey20'),
        legend.title = element_text(size = 10, colour = 'grey20'),
        legend.title.align = 0.5)
}

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

# predicted actual for both fishing and ECC
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

# predict for 10% increase in mangrove area
pred_man_df <- 
  preddata %>% 
  mutate(logMang = logMang + (0.1*logMang))
  
pred_man <- 
  predict(object = cvgbm,
          newdata = pred_man_df,
          n.trees = cvgbm$n.trees,
          type = 'response') %>% 
  as.data.frame() %>% 
  dplyr::rename('pred' = '.') %>% 
  mutate(pred = 1 - pred,
         level_pred = 'man_10') %>% 
  mutate(ISO3 = rawDD$ISO3) %>% 
  distinct(ISO3, .keep_all = TRUE)

head(pred_man)

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

# bind all together for fishing ------------------
all_fish <- 
  bind_rows(pred_act, pred_zero) %>% 
  mutate(country = countrycode(ISO3, 'iso3c', 'country.name')) %>% 
  mutate(country = dplyr::recode(country,
                                 'Myanmar (Burma)' = 'Myanmar')) %>% 
  left_join(., sort_df, by = c('ISO3' = 'ISO3'))
  
head(all_fish)

fish_plot <- 
  ggplot(all_fish, aes(x = pred, 
                       y = reorder(country, -sort_id), 
                       colour = level_pred)) +
  geom_line(data = all_fish, aes(x = pred, y = reorder(country, -sort_id), 
                                 group = country),
            colour = 'grey40') +
  geom_point(size = 5) +
  geom_vline(xintercept = 0.5, colour = 'grey70') + 
  labs(y = '',
       x = 'Probability of extinction') +
  scale_colour_manual(values = c('#800026', '#fd8d3c'),
                      labels = c('Current risk', 'Zero fishing\nmortality'),
                      name = stringr::str_wrap('Conservation potential for sawfishes',
                                               width = 25)) +
  scale_x_continuous(position = 'top',
                     limits = c(0, 1.0),
                     breaks = seq(0, 1.0, 0.25)) +
  publication_theme() +
  theme(legend.key = element_blank(),
        legend.position = c(0.8, 0.92))

fish_plot


# bind all together for mangrove ---------------------
all_man <- 
  bind_rows(pred_act, pred_man) %>% 
  mutate(country = countrycode(ISO3, 'iso3c', 'country.name')) %>% 
  mutate(country = dplyr::recode(country,
                                 'Myanmar (Burma)' = 'Myanmar')) %>% 
  left_join(., sort_df, by = c('ISO3' = 'ISO3'))

head(all_man)

man_plot <- 
  ggplot(all_man, aes(x = pred, 
                       y = reorder(country, -sort_id), 
                       colour = level_pred)) +
  geom_line(data = all_man, aes(x = pred, y = reorder(country, -sort_id), 
                                 group = country),
            colour = 'grey40') +
  geom_point(size = 5) +
  geom_vline(xintercept = 0.5, colour = 'grey70') + 
  labs(y = '',
       x = 'Probability of extinction') +
  scale_colour_manual(values = c('#004529', '#41ab5d'),
                      labels = c('Current risk', '10% increase in\nmangrove area'),
                      name = stringr::str_wrap('Conservation potential for sawfishes',
                                               width = 25)) +
  scale_x_continuous(position = 'top',
                     limits = c(0, 1.0),
                     breaks = seq(0, 1.0, 0.25)) +
  publication_theme() +
  theme(legend.key = element_blank(),
        legend.position = c(0.8, 0.92))

man_plot

goldilocks <- 
  plot_grid(fish_plot, man_plot, ncol = 2)

goldilocks

ggsave('../../../Figures/Publication/Goldilocks_191217.pdf', goldilocks,
       width = 30, height = 20, dpi = 600, units = c('cm'))
 









