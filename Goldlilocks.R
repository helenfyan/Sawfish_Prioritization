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

rawSpp <- read_csv('../../../Datasets/ProcessedCovariates_200824.csv')
rawDD <- read_csv('../../../Datasets/ProcessedDataDeficients_200824.csv')

# show where the countries that are predicted with Pext for DD
# Make fishing 0 in DD countries and see how this shakes out

# regression tree analysis ---------------------------------

sppData <-
  rawSpp %>%
  # remove collinear covariates
  dplyr::select(-X1, -logFishProd, -logIuu,
                -logChondLand, -EPI, -ReefFishers, -logFinUSD,
                -logShelfAreaDeep, -logCoastLength, -logCoastLengthNew,
                -NBI, -OHI) %>%
  as.data.frame() 

# clean data-deficient data ---------------------------

preddata <- 
  rawDD %>% 
  dplyr::select(-X1,-logShelfAreaDeep, -logCoastLength, -logCoastLengthNew,
                -NBI, -OHI) %>% 
  as.data.frame()


# need to randomize the data ---------------------------
set.seed(123)

randomIndex <- sample(1:nrow(sppData), nrow(sppData))
sppData <- sppData[randomIndex, ]

# separate the data into training set (80%) and test set (20%) ------------------
n <- nrow(sppData)
ntrain <- round(0.8*n)
trainIndex <- sample(1:n, ntrain)

sppTrain_raw <- sppData[trainIndex, ]
sppTest_raw <- sppData[-trainIndex, ]

sppTrain <- 
  sppTrain_raw %>% 
  # need to reorder for analysis
  #.[, c(15, 1:14, 16:20)] %>% 
  dplyr::select(-ISO3) %>% 
  #mutate_at(c(8:21), list(~ scale(.))) %>% 
  as.data.frame()

sppTest <- 
  sppTest_raw %>% 
  # need to reorder for analysis
  #.[, c(1, 7, 2:6, 8:21)] %>% 
  #mutate_at(c(8:21), list(~ scale(.))) %>% 
  arrange(ISO3) %>% 
  as.data.frame()

cvgbm <- gbm.step(data = sppTrain,
                  gbm.x = 2:19,
                  gbm.y = 1,
                  family = 'bernoulli',
                  tree.complexity = 10,
                  learning.rate = 0.005,
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

# predict for 100% increase in mangrove area
pred_man_df <- 
  preddata %>% 
  mutate(logMang = logMang + (1*logMang))
  
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
                #logCoastPop = 0)
                logFishEffort = 0)

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
  left_join(., sort_df, by = c('ISO3' = 'ISO3')) %>% 
  mutate(pred_bin = case_when(pred <= 0.2 ~ 'avlow',
                              pred > 0.2 & pred <= 0.4 ~ 'blow',
                              pred > 0.4 & pred <= 0.6 ~ 'cmod',
                              pred > 0.6 & pred <= 0.8 ~ 'dhigh',
                              pred > 0.8 & pred <= 1 ~ 'evhigh'))
  
head(all_fish)

fish_plot <- 
  ggplot(all_fish, aes(x = pred, 
                       y = reorder(country, -sort_id), 
                       colour = pred_bin,
                       alpha = level_pred)) +
  geom_vline(xintercept = 0.5, colour = 'grey70') + 
  geom_line(data = all_fish, aes(x = pred, y = reorder(country, -sort_id), 
                                 group = country),
            colour = 'grey40') +
  geom_point(size = 5) +
  labs(y = '',
       x = 'Probability of extinction') +
  scale_colour_manual(values = c('#1e2456', '#225fa9',
                                 '#a6a8ce',
                                 '#f05132', '#bd2029')) +
  scale_alpha_manual(values = c(1, 0.5)) +
  #scale_colour_manual(values = c('#800026', '#fd8d3c'),
  #                    labels = c('Current risk', 'Zero fishing\nmortality'),
  #                    name = stringr::str_wrap('Conservation potential for sawfishes',
  #                                             width = 25)) +
  scale_x_continuous(position = 'bottom',
                     limits = c(0, 1.0),
                     breaks = seq(0, 1.0, 0.25)) +
  publication_theme() +
  theme(legend.position = 'none') 
  #theme(legend.key = element_blank(),
  #      legend.position = c(0.8, 0.92))

fish_plot


# bind all together for mangrove ---------------------
all_man <- 
  bind_rows(pred_act, pred_man) %>% 
  mutate(country = countrycode(ISO3, 'iso3c', 'country.name')) %>% 
  mutate(country = dplyr::recode(country,
                                 'Myanmar (Burma)' = 'Myanmar')) %>% 
  left_join(., sort_df, by = c('ISO3' = 'ISO3')) %>% 
  mutate(pred_bin = case_when(pred <= 0.2 ~ 'avlow',
                              pred > 0.2 & pred <= 0.4 ~ 'blow',
                              pred > 0.4 & pred <= 0.6 ~ 'cmod',
                              pred > 0.6 & pred <= 0.8 ~ 'dhigh',
                              pred > 0.8 & pred <= 1 ~ 'evhigh'))

head(all_man)

man_plot <- 
  ggplot(all_man, aes(x = pred, 
                       y = reorder(country, -sort_id), 
                       colour = pred_bin,
                      alpha = level_pred)) +
  geom_vline(xintercept = 0.5, colour = 'grey70') + 
  geom_line(data = all_man, aes(x = pred, y = reorder(country, -sort_id), 
                                 group = country),
            colour = 'grey40') +
  geom_point(size = 5) +
  labs(y = '',
       x = 'Probability of extinction') +
  scale_colour_manual(values = c('#1e2456', '#225fa9',
                                 '#a6a8ce',
                                 '#f05132', '#bd2029')) +
  scale_alpha_manual(values = c(1, 0.5)) +
  #scale_colour_manual(values = c('#004529', '#41ab5d'),
  #                    labels = c('Current risk', '10% increase in\nmangrove area'),
  #                    name = stringr::str_wrap('Conservation potential for sawfishes',
  #                                             width = 25)) +
  scale_x_continuous(position = 'bottom',
                     limits = c(0, 1.0),
                     breaks = seq(0, 1.0, 0.25)) +
  publication_theme() +
  theme(legend.position = 'none',
        plot.background = element_rect(fill = 'transparent', colour = NA),
        panel.background = element_rect(fill = 'transparent', colour = 'transparent'))
  #theme(legend.key = element_blank(),
  #      legend.position = c(0.8, 0.92))

man_plot

goldilocks <- 
  plot_grid(fish_plot, NULL, man_plot, ncol = 3,
            rel_widths = c(1, -0.15, 1), align = 'hv')

goldilocks

ggsave('../../../Figures/Publication/Goldilocks_200825.pdf', goldilocks,
       width = 30, height = 20, dpi = 600, units = c('cm'))
