# This script does the statistical analysis for dynamic geography

#library(rstan)
library(tidyverse)
library(modelr)
library(tidybayes)
library(brms)

rstan_options(auto_write = TRUE)
options(mc.cores = 4)

publication_theme <- function(axis_text_size = 13) {
  theme(panel.background = element_blank(),
        panel.grid = element_blank(),
        axis.line.y = element_line(colour = 'grey60'),
        axis.line.x = element_line(colour = 'grey60'),
        axis.text = element_text(size = axis_text_size, colour = 'grey20'),
        axis.title = element_text(size = 15))
}

pc_data <- 
  read_csv('../../../Datasets/DynamicGeographyBinned_190925.csv') %>% 
  mutate(fishBin3 = dplyr::recode(fishBin3,
                                  'low' = 'Low',
                                  'moderate' = 'Moderate',
                                  'high' = 'High'))

mod_fishBin3 <- 
  brm(occurrence ~ logCoastLength*fishBin3,
      data = pc_data, 
      family = 'bernoulli',
      seed = 123)

summary(mod_fishBin3)
saveRDS(mod_fishBin3, '../../../ModelOutputs/DGBinned_190925.rds')

# to load the model file: object <- readRDS('modellocation')

mod_plot <- plot(marginal_effects(mod), points = TRUE)


fitlines_plot <- 
  pc_data %>% 
  mutate(fishBin3 = factor(fishBin3, levels = c('Low', 'Moderate', 'High'))) %>% 
  group_by(fishBin3) %>% 
  data_grid(logCoastLength = seq_range(logCoastLength, 101)) %>% 
  add_fitted_draws(mod_fishBin3, n = 100) %>% 
  ggplot(aes(x = logCoastLength, y = occurrence, 
             colour = fishBin3)) +
  geom_line(aes(y = .value, group = paste(fishBin3, .draw)), alpha = 0.15) +
  scale_colour_manual(values = c('#fd8d3c', '#fc4e2a', '#bd0026')) + 
  geom_point(data = pc_data, size = 4, shape = '|',
             colour = 'grey60', alpha = 0.7) +
  stat_lineribbon(aes(y = .value),
                  .width = c(0, 0, 0),
                  show.legend = FALSE) +
  scale_y_continuous(limits = c(0, 1), breaks = c(0, 1)) +
  labs(y = 'Occurrence', 
       x = 'log Coastline Length (km)',
       colour = 'Fishing Pressure') +
  theme(legend.key = element_rect(fill = NA),
        legend.title = element_text(size = 14, colour = 'grey20'),
        legend.text = element_text(size = 13, colour = 'grey20')) +
  guides(colour = guide_legend(override.aes = list(size = 2,
                                                   alpha = 0.8))) +
  publication_theme()

fitlines_plot











# this model didn't converge
mod_gearBin3 <- 
  brm(occurrence ~ logCoastLength*gearBin3,
      data = pc_data, 
      family = 'bernoulli',
      seed = 123)
summary(mod_gearBin3)
