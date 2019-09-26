# This script does the statistical analysis for dynamic geography

#library(rstan)
library(tidyverse)
library(modelr)
library(tidybayes)

rstan_options(auto_write = TRUE)
options(mc.cores = 4)

library(brms)

pc_data <- read_csv('../../../Datasets/DynamicGeographyBinned_190925.csv')

mod_fishBin3 <- 
  brm(occurrence ~ logCoastLength*fishBin3,
      data = pc_data, 
      family = 'bernoulli',
      seed = 123)

summary(mod_fishBin3)

mod_plot <- plot(marginal_effects(mod), points = TRUE)

test <- 
  pc_data %>% 
  group_by(fishBin3) %>% 
  data_grid(logCoastLength = seq_range(logCoastLength, 50)) %>% 
  add_fitted_draws(mod_fishBin3) %>% 
  ggplot(aes(x = logCoastLength, y = occurrence, colour = fishBin3)) +
  stat_lineribbon(aes(y = .value)) +
  geom_point(data = pc_data) +
  scale_fill_brewer(palette = 'Greys') +
  scale_colour_brewer(palette = 'Set2')
  
test

test2 <- 
  pc_data %>% 
  group_by(fishBin3) %>% 
  data_grid(logCoastLength = seq_range(logCoastLength, 101)) %>% 
  add_fitted_draws(mod_fishBin3, n = 100) %>% 
  ggplot(aes(x = logCoastLength, y = occurrence, colour = fishBin3)) +
  geom_line(aes(y = .value, group = paste(fishBin3, .draw)), alpha = 0.2) +
  scale_colour_brewer(palette = 'Set2') +
  theme_classic()

test2











# this model didn't converge
mod_gearBin3 <- 
  brm(occurrence ~ logCoastLength*gearBin3,
      data = pc_data, 
      family = 'bernoulli',
      seed = 123)
summary(mod_gearBin3)
