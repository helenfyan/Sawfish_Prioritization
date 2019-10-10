# This script does the statistical analysis for dynamic geography

library(rstan)
library(tidyverse)
library(modelr)
library(tidybayes)
library(brms)
library(ggstance)
library(cowplot)

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

#saveRDS(mod_fishBin3, '../../../ModelOutputs/DGBinned_190925.rds')
mod_fishBin3 <- readRDS('../../../ModelOutputs/DGBinned_190925.rds')
summary(mod_fishBin3)

get_variables(mod_fishBin3)

# plot with draws ---------------
fitlines_plot <- 
  pc_data %>% 
  mutate(fishBin3 = factor(fishBin3, levels = c('Low', 'Moderate', 'High'))) %>% 
  group_by(fishBin3) %>% 
  data_grid(logCoastLength = seq_range(logCoastLength, 101)) %>% 
  add_fitted_draws(mod_fishBin3, n = 200) %>% 
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

# plot of posterior distributions ---------------------------
post_values <- 
  posterior_samples(mod_fishBin3, '^b') %>% 
  dplyr::select(b_Intercept, b_fishBin3Low, b_fishBin3Moderate) %>% 
  dplyr::rename('High' = 'b_Intercept',
                'Moderate' = 'b_fishBin3Moderate',
                'Low' = 'b_fishBin3Low') %>% 
  gather(key = fishing, value = post_pred) %>% 
  mutate(fishing = factor(fishing, levels = c('Low', 'Moderate', 'High')))

post_plot <- 
  ggplot(post_values, aes(x = post_pred, fill = fishing, colour = fishing)) +
  geom_density(alpha = 0.6) +
  scale_fill_manual(values = c('#fd8d3c', '#fc4e2a', '#bd0026')) +
  scale_colour_manual(values = c('#fd8d3c', '#fc4e2a', '#bd0026')) +
  expand_limits(y = c(0, 0.15),
                x = c(-22, 22)) +
  facet_wrap(~ fishing, nrow = 3,
             strip.position = 'left') +
  geom_vline(xintercept = 0, linetype = 'dashed', colour = 'grey60', size = 0.7) +
  theme(legend.position = 'none',
        strip.background = element_blank(),
        strip.placement = 'outside',
        strip.text.y = element_text(size = 13, colour = 'grey20', angle = 180),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        panel.spacing = unit(0, 'lines')) +
  publication_theme() +
  labs(x = 'Posterior Prediction',
       y = '')

post_plot

  
# to colour based on overlap with zero ------------------------
make_density <- 
  function(fishing_pressure, colour_right, colour_left) {
    
    # make basic plot to build on
    basic_dens <- 
      post_values %>% 
      dplyr::filter(fishing == fishing_pressure) %>% 
      ggplot(aes(x = post_pred)) +
      geom_density(fill = colour_right, colour = 'grey90') +
      expand_limits(x = c(-23, 23),
                    y = c(0, 0.2)) +
      geom_vline(xintercept = 0, linetype = 'dashed', colour = 'grey60',
                 size = 0.7) +
      theme(axis.text = element_blank(),
            axis.line.x = element_blank(),
            axis.line.y = element_line(),
            panel.grid = element_blank(),
            panel.background = element_blank(),
            axis.title = element_blank(),
            axis.ticks = element_blank(),
            plot.margin = unit(c(-0.08, 0, 0, 1), 'cm'))
      
    
    # make plot dataframe
    dens_df <- 
      ggplot_build(basic_dens)$data[[1]]
    
    plot <- 
      basic_dens +
      geom_area(data = dens_df %>% 
                  dplyr::filter(x < 0), 
                aes(x = x, y = y), fill = colour_left)
    
    return(plot)
    
  }

low <- 
  make_density('Low', '#fd8d3c', '#EEC5AA') +
  labs(y = 'Low') +
  theme(axis.title.y = element_text(size = 13, colour = 'grey20', angle = 0, hjust = 0,
                                    vjust = 0.5))
  
low

mod <- 
  make_density('Moderate', '#FCA091', '#fc4e2a') +
  labs(y = 'Moderate') +
  theme(axis.title.y = element_text(size = 13, colour = 'grey20', angle = 0, hjust = 0,
                                    vjust = 0.5))

mod

high <- 
  make_density('High', '#BE827E', '#bd0026') +
  theme(axis.line.x = element_line(),
        axis.text.x = element_text(size = 13, colour = 'grey20'),
        axis.title.y = element_text(size = 13, colour = 'grey20', angle = 0, hjust = 0,
                                    vjust = 0.5)) +
  labs(y = 'High',
       x = 'Posterior Prediction')

high

all_density <- 
  plot_grid(low, mod, high, ncol = 1, align = 'v', scale = 1)

all_density
ggdraw(add_sub(all_density, 'Posterior Prediction', hjust = 0.1))
