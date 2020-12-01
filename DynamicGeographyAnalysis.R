# This script does the statistical analysis for dynamic geography

library(rstan)
library(tidyverse)
library(modelr)
library(tidybayes)
library(brms)
library(ggstance)
library(patchwork)
library(fishualize)

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

# function to colour density plot based on amount crossing 0
make_density <- 
  function(data_frame, intercept_value, colour_right, 
           colour_left, axis_title_size = 14) {
    
    # make basic plot to build on
    basic_dens <- 
      data_frame %>% 
      dplyr::filter(intercept == intercept_value) %>% 
      ggplot(aes(x = value)) +
      geom_density(fill = colour_right, colour = 'grey50') +
      expand_limits(x = c(-5.7, 11.4),
                    y = c(0, 0.2)) +
      geom_vline(xintercept = 0, colour = 'grey60',
                 size = 0.7) +
      theme(axis.line.x = element_blank(),
            axis.line.y = element_line(colour = 'grey60'),
            panel.grid = element_blank(),
            panel.background = element_blank(),
            axis.text.x = element_blank(),
            axis.title.x = element_blank(),
            axis.ticks.x = element_blank(),
            axis.text.y = element_text(colour = 'grey40', size = 11),
            axis.ticks.y = element_line(colour = 'grey40'),
            axis.title.y = element_text(colour = 'grey20', size = axis_title_size),
            plot.margin = unit(c(-0.08, 0, -0.08, 1), 'cm'))
    
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

# Fishing and mangrove area have the highest variable importances --------
mod_data <- read_csv('../../../Datasets/ProcessedCovariates_200824.csv')

full_mod <- 
  brm(occurrence ~ logShelfAreaShallow + logtotalGearTonnes + logMang + (1|ISO3),
      data = mod_data,
      family = 'bernoulli',
      seed = 123)

summary(full_mod)
quartz()
plot(full_mod)

#saveRDS(full_mod, '../../../ModelOutputs/DGShelfMangFish_200825.rds')
full_mod <- readRDS('../../../ModelOutputs/DGShelfMangFish_200825.rds')

# make density plots ------------------------------------------------------
post_pred <- 
  posterior_samples(full_mod, '^b') %>% 
  gather(key = intercept) %>%
  dplyr::filter(intercept != 'b_Intercept')

head(post_pred)

post_shelf <- 
  make_density(post_pred, 'b_logShelfAreaShallow',
               '#1d91c0', '#d3eef8', 10) +
  labs(y = 'ln Shelf area')

post_mang <- 
  make_density(post_pred, 'b_logMang',
               '#1d91c0', '#d3eef8', 10) +
  labs(y = 'ln Mangrove area') +
  scale_y_continuous(limits = c(0, 0.6),
                     breaks = seq(0, 0.5, 0.25))

post_fish <- 
  make_density(post_pred, 'b_logtotalGearTonnes',
               '#fbd0d2', '#a50f15', 10) +
  theme(axis.text.x = element_text(size = 10, colour = 'grey40'),
        axis.ticks.x = element_line(colour = 'grey40'),
        axis.title.x = element_text(colour = 'grey20', size = 10),
        axis.line.x = element_line(colour = 'grey60'),
        plot.margin = unit(c(-0.08, 0, 0, 1), 'cm')) +
  scale_y_continuous(limits = c(0, 0.63),
                     breaks = seq(0, 0.5, 0.25)) +
  labs(y = 'ln Fishing pressure',
       x = 'Coefficient estimate')

post_plots <- 
  post_shelf + post_mang + post_fish + plot_layout(ncol = 1)

post_plots

# fitlines plot -----------------------------------------------------------
quantile(mod_data$logtotalGearTonnes)

QminF <- quantile(mod_data$logtotalGearTonnes, 0)
Q1F <- quantile(c(0, max(mod_data$logtotalGearTonnes)), 0.25)
Q2F <- quantile(c(0, max(mod_data$logtotalGearTonnes)), 0.5)
Q3F <- quantile(c(0, max(mod_data$logtotalGearTonnes)), 0.75)
QmaxF <- quantile(mod_data$logtotalGearTonnes, 1)

quarts <- 
  lapply(c(QminF, Q1F, Q2F, Q3F, QmaxF), function(x) {
    
    qs_df <- 
     mod_data %>% 
      #group_by(ISO3) %>% 
      #data_grid(logCoastLength = seq_range(logCoastLength, 101)) %>% 
      data_grid(logShelfAreaShallow = seq(from = 0, to = 14.2, by = 0.1)) %>% 
      #ungroup() %>% 
      #data_grid(logCoastLength, ISO3) %>% 
      mutate(logtotalGearTonnes = as.numeric(paste(x)),
             logMang = mean(mod_data$logMang)) %>% 
      add_fitted_draws(full_mod, n = 150,
                       seed = 123,
                       # ignore group-level effects
                       re_formula = NA)
    
    return(qs_df)
    
  })

pred_Qf <- 
  do.call(rbind, quarts)

pred_meansQ <- 
  pred_Qf %>% 
  group_by(logtotalGearTonnes, logShelfAreaShallow) %>% 
  summarise(mean_val = mean(.value))

fitlines_fish <- 
  #ggplot(pred_qsP, aes(x = logCoastLength, y = occurrence, 
  #                    colour = factor(logProteinDiet))) +
  # change it so there are no posterior draws for zero and max
  pred_Qf %>% 
  #dplyr::filter(!logProteinDiet %in% c(QminP, QmaxP)) %>% 
  ggplot(aes(x = logShelfAreaShallow, y = occurrence,
             colour = factor(logtotalGearTonnes))) +
  geom_line(aes(y = .value, group = paste(logtotalGearTonnes, .draw)), 
            alpha = 0.15) +
  geom_smooth(data = pred_meansQ, aes(x = logShelfAreaShallow, y = mean_val,
                                      colour = factor(logtotalGearTonnes)),
              se = FALSE, fullrange = TRUE,
              method = 'glm', 
              method.args = list(family = 'quasibinomial'),
              size = 2) +
  geom_point(data = mod_data, size = 6, shape = '|',
             colour = 'grey60', alpha = 0.7) +
  scale_y_continuous(limits = c(0, 1), breaks = c(0, 1)) +
  theme(legend.position = 'none') +
  #legend.key = element_rect(fill = NA),
  #legend.title = element_text(size = 14, colour = 'grey20'),
  #legend.text = element_text(size = 13, colour = 'grey20')) +
  guides(colour = guide_legend(override.aes = list(size = 2,
                                                   alpha = 0.8))) +
  scale_colour_manual(values = c('#fc9272', '#fb6a4a',
                                 '#ef3b2c', '#a50f15',
                                 '#67000d'),
                      name = 'Fishing\npressure',
                      labels = c('Zero',
                                 'Low',
                                 'Moderate',
                                 'High',
                                 'Maximum')) +
  publication_theme() +
  expand_limits(x = c(2.5, 14.5)) +
  labs(y = 'Occupancy',
       x = 'Habitat availability (ln Shelf Area)') +
  geom_hline(yintercept = 0.05, colour = 'grey20', linetype = 'solid',
             size = 1.2)

fitlines_fish

# 5% occupancy plot --------------------------------------------------
head(pred_Qf)

pred_5 <- 
  pred_Qf %>% 
  mutate(.value = round(.value, 2)) %>% 
  dplyr::filter(.value == 0.05) %>% 
  # janky way of dealing with this but whatever
  mutate(fishBin1 = round(logtotalGearTonnes, 0),
         fishBin = case_when(fishBin1 == 0 ~ 'Zero',
                             fishBin1 == 5 ~ 'Low',
                             fishBin1 == 10 ~ 'Moderate',
                             fishBin1 == 14 ~ 'High',
                             fishBin1 == 19 ~ 'Maximum'),
         fishBin = factor(fishBin, levels = c('Zero', 'Low',
                                              'Moderate', 'High',
                                              'Maximum')))


pred_5_med <- 
  pred_5 %>% 
  group_by(fishBin) %>% 
  summarise(med_f = median(logShelfAreaShallow),
            sd_f = sd(logShelfAreaShallow),
            n = n(),
            se_f = sd_f/sqrt(n),
            ci_95 = se_f*1.96) %>% 
  ungroup() %>% 
  mutate(fishBin = factor(fishBin, levels = c('Zero', 'Low',
                                              'Moderate', 'High',
                                              'Maximum')),
         line_val = case_when(fishBin == 'Zero' ~ 1,
                              fishBin == 'Low' ~ 2,
                              fishBin == 'Moderate' ~ 3,
                              fishBin == 'High' ~ 4,
                              fishBin == 'Maximum' ~ 5))

plot_5 <- 
  ggplot(pred_5, aes(x = fishBin, y = logShelfAreaShallow,
                     fill = fishBin, colour = fishBin)) +
  geom_point(size = 2, alpha = 0.4, 
             position = position_jitter(width = 0.15)) +
  geom_violin(trim = FALSE, alpha = 0.6, colour = NA) + 
  geom_segment(data = pred_5_med, 
               aes(x = line_val - 0.4, xend = line_val + 0.4,
                   y = med_f, yend = med_f),
               colour = 'grey20',
               lwd = 1.5) +
  scale_fill_manual(values = c('#fc9272', '#fb6a4a',
                               '#ef3b2c', '#a50f15',
                               '#67000d')) +
  scale_colour_manual(values = c('#fc9272', '#fb6a4a',
                                 '#ef3b2c', '#a50f15',
                                 '#67000d')) +
  publication_theme() +
  #scale_y_continuous(limits = c(1, 13.5),
  #                   breaks = seq(1.5, 13.5, 2)) +
  theme(legend.position = 'none',
        plot.margin = unit(c(0.1, 0.5, 0.1, 0.5), 'cm'),
        axis.title.y = element_text(size = 10),
        axis.text.x = element_text(size = 8),
        axis.title.x = element_text(size = 10),
        plot.background = element_rect(fill = 'transparent')) +
  labs(y = 'ln Shelf area',
       x = 'ln Fishing pressure')

plot_5

# combine all the plots into one
dg_plot <- 
  fitlines_fish + 
  {{post_shelf + post_mang + post_fish + plot_layout(ncol = 1)} +
      plot_5 + plot_layout(ncol = 1, heights = c(0.5, 0.5, 0.5, 1))} +
  plot_layout(ncol = 2, widths = c(2, 1, 1, 1, 1))

dg_plot

ggsave('../../../Figures/EcoCarryCapacity/DgPost5_200825.pdf',
       dg_plot, height = 21, width = 30, units = c('cm'))


# fitlines for mangrove area -----------------------------------------
quantile(mod_data$logMang)

QminM <- quantile(mod_data$logMang, 0)
Q1M <- quantile(mod_data$logMang, 0.25)
Q2M <- quantile(mod_data$logMang, 0.5)
Q3M <- quantile(mod_data$logMang, 0.75)
QmaxM <- quantile(mod_data$logMang, 1)

quartsM <- 
  lapply(c(QminM, Q1M, Q2M, Q3M, QmaxM), function(x) {
    
    qs_df <- 
      mod_data %>% 
      #group_by(ISO3) %>% 
      #data_grid(logCoastLength = seq_range(logCoastLength, 101)) %>% 
      data_grid(logShelfAreaShallow = seq(from = 2.4, to = 14.2, by = 0.1)) %>% 
      #ungroup() %>% 
      #data_grid(logCoastLength, ISO3) %>% 
      mutate(logMang = as.numeric(paste(x)),
             logtotalGearTonnes = mean(mod_data$logtotalGearTonnes)) %>% 
      add_fitted_draws(full_mod, n = 150,
                       seed = 123,
                       # ignore group-level effects
                       re_formula = NA)
    
    return(qs_df)
    
  })

pred_QM <- 
  do.call(rbind, quartsM)

pred_meansQM <- 
  pred_QM %>% 
  group_by(logMang, logShelfAreaShallow) %>% 
  summarise(mean_val = mean(.value))

fitlines_mang <- 
  pred_QM %>% 
  ggplot(aes(x = logShelfAreaShallow, y = occurrence,
             colour = factor(logMang))) +
  geom_line(aes(y = .value, group = paste(logMang, .draw)), 
            alpha = 0.15) +
  geom_point(data = mod_data, size = 4, shape = '|',
             colour = 'grey60', alpha = 0.7) +
  geom_smooth(data = pred_meansQM, aes(x = logShelfAreaShallow, y = mean_val,
                                      colour = factor(logMang)),
              se = FALSE, fullrange = TRUE,
              method = 'glm', 
              method.args = list(family = 'quasibinomial'),
              size = 1.5) +
  scale_y_continuous(limits = c(0, 1), breaks = c(0, 1)) +
  scale_x_continuous(limits = c(2.4, 14.5),
                     breaks = seq(2, 14, 2)) +
  #guides(colour = guide_legend(override.aes = list(size = 2,
  #                                                 alpha = 0.8))) +
  scale_colour_manual(values = c('#d3eef8', '#7acbeb',
                                 '#21a8de', '#146585',
                                 '#0a3343'),
                      name = 'Mangrove area',
                      labels = c('Zero',
                                 'Low',
                                 'Moderate',
                                 'High',
                                 'Maximum')) +
  publication_theme() +
  theme(legend.position = c(0.15, 0.85),
        legend.key = element_rect(fill = NA),
        legend.title = element_text(size = 12, colour = 'grey20'),
        legend.text = element_text(size = 11, colour = 'grey20')) +
  labs(y = 'Occupancy',
       x = 'Habitat availability (ln Shelf Area)') +
  geom_hline(yintercept = 0.05, colour = 'grey20', linetype = 'solid',
             size = 1.2) +
  annotate("text", x = 13.5, y = 0.07, label = "5% Occupancy",
           size = 4, colour = 'grey20')

fitlines_mang

# 5% occupancy for mangrove area -------------------------------------
pred_5M <- 
  pred_QM %>% 
  mutate(.value = round(.value, 2)) %>% 
  dplyr::filter(.value == 0.05) %>% 
  # janky way of dealing with this but whatever
  mutate(mangBin1 = round(logMang, 0),
         mangBin = case_when(mangBin1 == 0 ~ 'Zero',
                             mangBin1 == 3 ~ 'Low',
                             mangBin1 == 5 ~ 'Moderate',
                             mangBin1 == 7 ~ 'High',
                             mangBin1 == 9 ~ 'Maximum'),
         mangBin = factor(mangBin, levels = c('Zero', 'Low',
                                              'Moderate', 'High',
                                              'Maximum')))


pred_5_medM <- 
  pred_5M %>% 
  group_by(mangBin) %>% 
  summarise(med_f = median(logShelfAreaShallow),
            sd_f = sd(logShelfAreaShallow),
            n = n(),
            se_f = sd_f/sqrt(n),
            ci_95 = se_f*1.96) %>% 
  ungroup() %>% 
  mutate(mangBin = factor(mangBin, levels = c('Zero', 'Low',
                                              'Moderate', 'High',
                                              'Maximum')),
         line_val = case_when(mangBin == 'Zero' ~ 1,
                              mangBin == 'Low' ~ 2,
                              mangBin == 'Moderate' ~ 3,
                              mangBin == 'High' ~ 4,
                              mangBin == 'Maximum' ~ 5))

plot_5M <- 
  ggplot(pred_5M, aes(x = mangBin, y = logShelfAreaShallow,
                     fill = mangBin, colour = mangBin)) +
  geom_point(size = 2, alpha = 0.4, 
             position = position_jitter(width = 0.15)) +
  geom_violin(trim = FALSE, alpha = 0.6, colour = NA) + 
  geom_segment(data = pred_5_medM, 
               aes(x = line_val - 0.4, xend = line_val + 0.4,
                   y = med_f, yend = med_f),
               colour = 'grey20',
               lwd = 1.5) +
  scale_fill_manual(values = c('#d3eef8', '#7acbeb',
                               '#21a8de', '#146585',
                               '#0a3343')) +
  scale_colour_manual(values = c('#d3eef8', '#7acbeb',
                                 '#21a8de', '#146585',
                                 '#0a3343')) +
  publication_theme() +
  #scale_y_continuous(limits = c(1, 13.5),
  #                   breaks = seq(1.5, 13.5, 2)) +
  theme(legend.position = 'none',
        plot.margin = unit(c(0.1, 0.5, 0.1, 0.5), 'cm'),
        axis.title.y = element_text(size = 15),
        axis.text.x = element_text(size = 13),
        axis.title.x = element_text(size = 15),
        plot.background = element_rect(fill = 'transparent')) +
  labs(y = 'ln Shelf area',
       x = 'ln Mangrove area')

plot_5M

mang <- 
  fitlines_mang + plot_5M + plot_layout(ncol = 2, 
                                        widths = c(1.5, 1)) +
  plot_annotation(tag_levels = 'A') & theme(plot.tag = element_text(face = 'bold',
                                                                    size = 18))

mang


ggsave('../../../Figures/Publication/DGmangrove_201118.png',
       mang, height = 21, width = 30, units = c('cm'), dpi = 600)


# this is all old code -----------------------------------------------
# --------------------------------------------------------------------

# bind all of them together
pred_qsP <- 
  rbind(quarts[[1]], quarts[[2]], quarts[[3]], quarts[[4]], quarts[[5]])

# calculate means to draw mean line
pred_meansP <- 
  pred_qsP %>% 
  group_by(logProteinDiet, logCoastLength) %>% 
  summarise(mean_val = mean(.value))

# plot this badboy
preds_fitlines_protein <- 
  #ggplot(pred_qsP, aes(x = logCoastLength, y = occurrence, 
  #                    colour = factor(logProteinDiet))) +
  # change it so there are no posterior draws for zero and max
  pred_qsP %>% 
  #dplyr::filter(!logProteinDiet %in% c(QminP, QmaxP)) %>% 
  ggplot(aes(x = logCoastLength, y = occurrence,
             colour = factor(logProteinDiet))) +
  geom_line(aes(y = .value, group = paste(logProteinDiet, .draw)), 
            alpha = 0.15) +
  geom_point(data = pc_data, size = 4, shape = '|',
             colour = 'grey60', alpha = 0.7) +
  geom_smooth(data = pred_meansP, aes(x = logCoastLength, y = mean_val,
                                      colour = factor(logProteinDiet)),
              se = FALSE, fullrange = TRUE,
              method = 'glm', 
              method.args = list(family = 'quasibinomial'),
              size = 1.5) +
  scale_y_continuous(limits = c(0, 1), breaks = c(0, 1)) +
  theme(legend.position = 'none') +
  #legend.key = element_rect(fill = NA),
  #legend.title = element_text(size = 14, colour = 'grey20'),
  #legend.text = element_text(size = 13, colour = 'grey20')) +
  guides(colour = guide_legend(override.aes = list(size = 2,
                                                   alpha = 0.8))) +
  scale_colour_manual(values = c('#fc9272', '#fb6a4a',
                                 '#ef3b2c', '#a50f15',
                                 '#67000d'),
                      name = 'Fishing\npressure',
                      labels = c('Zero',
                                 'Low',
                                 'Moderate',
                                 'High',
                                 'Maximum')) +
  publication_theme() +
  labs(y = 'Occupancy',
       x = 'Habitat availability (log coastline length)') +
  geom_hline(yintercept = 0.05, colour = 'grey20', linetype = 'solid',
             size = 1.2)

preds_fitlines_protein


# -------------------------------------------------------------------------
pc_data <- 
  read_csv('../../../Datasets/DynamicGeographyBinned_190925.csv') %>% 
  mutate(fishBin3 = dplyr::recode(fishBin3,
                                  'low' = 'Low',
                                  'moderate' = 'Moderate',
                                  'high' = 'High'))

# ----------------------------------------------------------------------
# WITH FISHING ---------------------------------------------------------
# ----------------------------------------------------------------------

# model where fishing is binned
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
set.seed(123)

fitlines_plot <- 
  pc_data %>% 
  mutate(fishBin3 = factor(fishBin3, levels = c('Low', 'Moderate', 'High'))) %>% 
  group_by(fishBin3) %>% 
  # extend the lines beyond the limits of the data
  data_grid(logCoastLength = seq_range(pc_data$logCoastLength, 101)) %>% 
  add_fitted_draws(mod_fishBin3, n = 100) %>% 
  ggplot(aes(x = logCoastLength, y = occurrence, 
             colour = fishBin3)) +
  geom_line(aes(y = .value, group = paste(fishBin3, .draw)), alpha = 0.15) +
  scale_colour_manual(values = c('#fd8d3c', '#fc4e2a', '#bd0026')) + 
  geom_point(data = pc_data, size = 4, shape = '|',
             colour = 'grey60', alpha = 0.7) +
  geom_smooth(data = pc_data, method = 'glm',
             method.args = list(family = 'binomial'),
             se = FALSE, size = 1.5,
             fullrange = TRUE) +
  scale_y_continuous(limits = c(0, 1), breaks = c(0, 1)) +
  labs(y = 'Occurrence', 
       x = 'log Coastline Length (km)',
       colour = 'Fishing Pressure') +
  theme(legend.position = 'none') +
  #theme(legend.key = element_rect(fill = NA),
  #      legend.title = element_text(size = 14, colour = 'grey20'),
  #      legend.text = element_text(size = 13, colour = 'grey20'),
  #      legend.position = c(0.02, 0.9)) +
  #guides(colour = guide_legend(override.aes = list(size = 2,
  #                                                 alpha = 0.8))) +
  # overlay the zero fishing pressure curve
  # first need to load the data below
  #geom_line(data = zero_df, aes(y = .value, group = .draw), alpha = 0.15,
  #          colour = 'darkblue') +
  #geom_line(data = zero_pred, aes(x = logCoastLength, y = mean_value),
  #          colour = 'darkblue', size = 1.5) +
  publication_theme()

fitlines_plot

#ggsave('../../../Figures/EcoCarryCapacity/DynamicGeography_191017.pdf',
#       height = 20, width = 30, units = c('cm'))

# plot of posterior distributions ---------------------------
post_values <- 
  posterior_samples(mod_fishBin3, '^b') %>% 
  dplyr::select(b_Intercept, b_fishBin3Low, b_fishBin3Moderate,
                b_logCoastLength) %>% 
  # add beta coefficients to b_Intercept b/c categorical variable
  mutate(b_fishBin3Moderate = b_fishBin3Moderate + b_Intercept,
         b_fishBin3Low = b_fishBin3Low + b_Intercept) %>% 
  dplyr::rename('High' = 'b_Intercept',
                'Moderate' = 'b_fishBin3Moderate',
                'Low' = 'b_fishBin3Low',
                'Coastline Length' = 'b_logCoastLength') %>% 
  gather(key = intercept, value = post_pred) %>% 
  mutate(intercept = factor(intercept, levels = c('Coastline Length',
                                                  'Low', 'Moderate', 'High')))
  
head(post_values)
# to colour based on overlap with zero ------------------------


# changed function to include dataframe in command
# these won't work
coast <- 
  make_density('Coastline Length', 'darkblue', 'lightblue') +
  labs(y = 'Coastline Length') +
  theme(axis.title.y = element_text(size = 13, 
                                    colour = 'grey20', angle = 0, hjust = 0,
                                    vjust = 0.5))

coast

low <- 
  make_density('Low', '#EEC5AA', '#fd8d3c') +
  labs(y = 'Low') +
  theme(axis.title.y = element_text(size = 13, 
                                    colour = 'grey20', angle = 0, hjust = 0,
                                    vjust = 0.5))
  
low

mod <- 
  make_density('Moderate', '#FCA091', '#fc4e2a') +
  labs(y = 'Moderate') +
  theme(axis.title.y = element_text(size = 13, 
                                    colour = 'grey20', angle = 0, hjust = 0,
                                    vjust = 0.5))

mod

high <- 
  make_density('High', '#BE827E', '#bd0026') +
  theme(axis.line.x = element_line(colour = 'grey60'),
        axis.text.x = element_text(size = 13, colour = 'grey20'),
        axis.title.y = element_text(size = 13, colour = 'grey20', 
                                    angle = 0, hjust = 0,
                                    vjust = 0.5)) +
  labs(y = 'High',
       x = 'Posterior Prediction')

high

# ----------------------------------------------------------------------
# WITHOUT FISHING ------------------------------------------------------
# ----------------------------------------------------------------------

# model where fishing is kept continuous
mod_fishcont_int <- 
  brm(occurrence ~ logCoastLength * logProteinDiet,
      data = pc_data,
      family = 'bernoulli',
      seed = 123)

#saveRDS(mod_fishcont_int, '../../../ModelOutputs/DGContinuous_191016.rds')
mod_fishcont_int <- readRDS('../../../ModelOutputs/DGContinuous_191016.rds')
summary(mod_fishcont_int)

plot(mod_fishcont_int)

# ---------------------------------------------------------------------
# USE THIS MODEL ------------------------------------------------------
# ---------------------------------------------------------------------
cont_data <- read_csv('../../../Datasets/ProcessedCovariates_200824.csv')

# let's see how this looks if we use shelf area instead of coastline length
mod_fishshelf <- 
  brm(occurrence ~ logShelfAreaDeep + logProteinDiet + (1|ISO3),
      data = cont_data,
      family = 'bernoulli',
      seed = 123)

quartz()
summary(mod_fishshelf)
plot(mod_fishshelf)

kfold(mod_fishshelf, K = 10)
kfold(mod_fishcont, K = 10)

# based on k-fold cross validation, shelf area is the better model
# kfoldic = 122.2 with se = 15.2 (shelf area)
# kfoldic = 137.3 with se = 17.0 (coastline length)

# model with coastline length as the measure of habitat availability
fishcont_priors <- 
  get_prior(occurrence ~ logCoastLength + logProteinDiet + (1|ISO3),
             data = cont_data,
             family = 'bernoulli')

mod_fishcont <- 
  brm(occurrence ~ logCoastLength + logProteinDiet + (1|ISO3),
      data = cont_data,
      warmup = 90000,
      iter = 100000,
      family = 'bernoulli',
      prior = fishcont_priors,
      seed = 123)

#saveRDS(mod_fishcont, '../../../ModelOutputs/DGContinuous_200309.rds')
mod_fishcont <- readRDS('../../../ModelOutputs/DGContinuous_200309.rds')
summary(mod_fishcont)

plot(mod_fishcont)

# lower LOOIC values demonstrate a better fit
loo(mod_fishcont)
loo(mod_fishcont_int)

get_variables(mod_fishcont)

# traceplot of posteriors ----------------------------------------
post_trace <- as.array(mod_fishcont)

trace_labels <- 
  c('b_logCoastLength' = 'log Coastline length',
    'b_logProteinDiet' = 'log Fishing pressure',
    'b_Intercept' = 'Intercept')

bayesplot::color_scheme_set('blue')
trace_plots <- 
  bayesplot::mcmc_trace(post_trace, pars = c('b_logCoastLength',
                                             'b_logProteinDiet',
                                             'b_Intercept'),
                        facet_args = list(ncol = 1, strip.position = 'left',
                                          labeller = as_labeller(trace_labels))) +
  theme(strip.placement = 'outside',
        strip.background = element_blank(),
        strip.text = element_text(vjust = 0.8, colour = 'grey20',
                                  size = 12),
        legend.text = element_text(colour = 'grey20'),
        legend.title = element_text(colour = 'grey20'),
        legend.key = element_blank()) +
  publication_theme() +
  labs(y = 'Coefficients') +
  guides(colour = guide_legend(override.aes = list(size = 2)))

trace_plots

ggsave('../../../Figures/EcoCarryCapacity/TracePlot_200309.pdf', trace_plots,
       height = 22, width = 30.58, units = c('cm'))



# posterior of continuous variables --------------
# need to plot the density plots separately because facet_wrap fucks the alignment later
post_pred <- 
  posterior_samples(mod_fishcont, '^b') %>% 
  gather(key = intercept) %>%
  dplyr::filter(intercept != 'b_Intercept')

mod_diag <- 
  post_pred %>% 
  mutate(post_class = case_when(value >= 0 ~ 'positive',
                                value < 0 ~ 'negative')) %>% 
  group_by(intercept, post_class) %>% 
  summarise(n = n())
  
mod_diag

post_coast <- 
  post_pred %>% 
  dplyr::filter(intercept == 'b_logCoastLength') %>% 
  ggplot(aes(x = value)) +
  geom_density(colour = 'grey50', alpha = 0.9, fill = '#1d91c0') +
  expand_limits(x = c(-16.2, 8.5)) + 
  scale_y_continuous(limits = c(-0.1, 0.6),
                     breaks = seq(0, 0.6, 0.2)) +
  labs(x = '',
       y = '') +
  geom_vline(xintercept = 0, linetype = 'solid',
             colour = 'grey50', size = 1) +
  publication_theme() +
  labs(y = 'log Coastline length') +
  theme(plot.margin = unit(c(0.1, 0.5, -1, 0.5), 'cm'),
        axis.title.y = element_text(size = 12, colour = 'grey20'),
        axis.text.x = element_blank())

post_coast

post_fish <- 
  post_pred %>% 
  dplyr::filter(intercept == 'b_logProteinDiet') %>% 
  ggplot(aes(x = value)) +
  geom_density(colour = 'grey50', alpha = 0.9, fill = '#a50f15') +
  expand_limits(x = c(-16.2, 8.5)) + 
  labs(x = '',
       y = '') +
  scale_y_continuous(limits = c(0, 0.3),
                     breaks = seq(0, 0.3, 0.1)) +
  geom_vline(xintercept = 0, linetype = 'solid',
             colour = 'grey50', size = 1) +
  publication_theme() +
  labs(y = 'log Fishing pressure',
       x = 'Coefficient estimate') +
  theme(plot.margin = unit(c(0, 0.5, 0.1, 0.5), 'cm'),
        axis.title.y = element_text(size = 12, colour = 'grey20'),
        axis.title.x = element_text(size = 12, colour = 'grey20'))

fish_basic <- 
  post_pred %>% 
  dplyr::filter(intercept == 'b_logProteinDiet') %>% 
  ggplot(aes(x = value)) +
  geom_density(colour = 'grey50', alpha = 0.9, fill = '#a50f15') +
  expand_limits(x = c(-16.2, 8.5)) + 
  labs(x = '',
       y = '') +
  scale_y_continuous(limits = c(0, 0.3),
                     breaks = seq(0, 0.3, 0.1)) +
  geom_vline(xintercept = 0, linetype = 'solid',
             colour = 'grey50', size = 1) +
  publication_theme() +
  labs(y = 'log Fishing pressure',
       x = 'Coefficient estimate') +
  theme(plot.margin = unit(c(0, 0.5, 0.1, 0.5), 'cm'),
        axis.title.y = element_text(size = 12, colour = 'grey20'),
        axis.title.x = element_text(size = 12, colour = 'grey20'))

fish_dens <- 
  ggplot_build(fish_basic)$data[[1]]

post_fish <- 
  fish_basic +
  geom_area(data = fish_dens %>% 
              dplyr::filter(x > 0),
            aes(x = x, y = y),
            fill = '#f48a8e')

post_fish

# don't need to use post_plots
post_plots <- post_coast + post_fish + plot_layout(ncol = 1)
post_plots
#ggsave('../../../Figures/EcoCarryCapacity/DynamicGeographyContCoef_191130.pdf',
#       post_plots, height = 20, width = 25, units = c('cm'))


# -------------------------------------------------------------------------
# Make fitlines plot and predict each of the curves using cont model ------
# -------------------------------------------------------------------------

# protein consumption
QsProtein <- summary(pc_data$logProteinDiet)
QminP <- QsProtein[1]
Q1P <- QsProtein[2]
Q2P <- QsProtein[3]
Q3P <- QsProtein[5]
QmaxP <- QsProtein[6]

quarts <- 
  lapply(c(QminP, Q1P, Q2P, Q3P, QmaxP), function(x) {
    
    qs_df <- 
      cont_data %>% 
      #group_by(ISO3) %>% 
      #data_grid(logCoastLength = seq_range(logCoastLength, 101)) %>% 
      data_grid(logCoastLength = seq(from = 2, to = 12.5, by = 0.1)) %>% 
      #ungroup() %>% 
      #data_grid(logCoastLength, ISO3) %>% 
      mutate(logProteinDiet = paste(x),
             logProteinDiet = as.numeric(logProteinDiet)) %>% 
      add_fitted_draws(mod_fishcont, n = 150,
                       seed = 321,
                       # ignore group-level effects
                       re_formula = NA)
    
    return(qs_df)
    
  })

# --------------------------------------------------------------------

# bind all of them together
pred_qsP <- 
  rbind(quarts[[1]], quarts[[2]], quarts[[3]], quarts[[4]], quarts[[5]])

# calculate means to draw mean line
pred_meansP <- 
  pred_qsP %>% 
  group_by(logProteinDiet, logCoastLength) %>% 
  summarise(mean_val = mean(.value))

# plot this badboy
preds_fitlines_protein <- 
  #ggplot(pred_qsP, aes(x = logCoastLength, y = occurrence, 
  #                    colour = factor(logProteinDiet))) +
  # change it so there are no posterior draws for zero and max
  pred_qsP %>% 
  #dplyr::filter(!logProteinDiet %in% c(QminP, QmaxP)) %>% 
  ggplot(aes(x = logCoastLength, y = occurrence,
             colour = factor(logProteinDiet))) +
  geom_line(aes(y = .value, group = paste(logProteinDiet, .draw)), 
            alpha = 0.15) +
  geom_point(data = pc_data, size = 4, shape = '|',
             colour = 'grey60', alpha = 0.7) +
  geom_smooth(data = pred_meansP, aes(x = logCoastLength, y = mean_val,
                                     colour = factor(logProteinDiet)),
              se = FALSE, fullrange = TRUE,
              method = 'glm', 
              method.args = list(family = 'quasibinomial'),
              size = 1.5) +
  scale_y_continuous(limits = c(0, 1), breaks = c(0, 1)) +
  theme(legend.position = 'none') +
        #legend.key = element_rect(fill = NA),
        #legend.title = element_text(size = 14, colour = 'grey20'),
        #legend.text = element_text(size = 13, colour = 'grey20')) +
  guides(colour = guide_legend(override.aes = list(size = 2,
                                                   alpha = 0.8))) +
  scale_colour_manual(values = c('#fc9272', '#fb6a4a',
                                 '#ef3b2c', '#a50f15',
                                 '#67000d'),
                      name = 'Fishing\npressure',
                      labels = c('Zero',
                                 'Low',
                                 'Moderate',
                                 'High',
                                 'Maximum')) +
  publication_theme() +
  labs(y = 'Occupancy',
       x = 'Habitat availability (log coastline length)') +
  geom_hline(yintercept = 0.05, colour = 'grey20', linetype = 'solid',
             size = 1.2)
  
preds_fitlines_protein

ggsave('../../../Figures/EcoCarryCapacity/DynamicGeography_200309.png',
       preds_fitlines_protein, height = 20, width = 30, units = c('cm'))


# predict coastline size to have 0.05 occupancy at different fishing levels -------------------

head(pred_qsP)

pred_5 <- 
  pred_qsP %>% 
  mutate(.value = round(.value, 2)) %>% 
  dplyr::filter(.value == 0.05) %>% 
  mutate(fishBin = case_when(logProteinDiet == QminP ~ 'Zero',
                             logProteinDiet == Q1P ~ 'Low',
                             logProteinDiet == Q2P ~ 'Moderate',
                             logProteinDiet == Q3P ~ 'High',
                             logProteinDiet == QmaxP ~ 'Maximum'),  
         fishBin = ifelse(is.na(fishBin), 'High', fishBin),
         fishBin = factor(fishBin, levels = c('Zero', 'Low',
                                                 'Moderate', 'High',
                                                 'Maximum')))


pred_5_med <- 
  pred_5 %>% 
  group_by(fishBin) %>% 
  summarise(med_f = median(logCoastLength),
            sd_f = sd(logCoastLength),
            n = n(),
            se_f = sd_f/sqrt(n),
            ci_95 = se_f*1.96) %>% 
  ungroup() %>% 
  mutate(fishBin = factor(fishBin, levels = c('Zero', 'Low',
                                              'Moderate', 'High',
                                              'Maximum')),
         line_val = case_when(fishBin == 'Zero' ~ 1,
                              fishBin == 'Low' ~ 2,
                              fishBin == 'Moderate' ~ 3,
                              fishBin == 'High' ~ 4,
                              fishBin == 'Maximum' ~ 5))

plot_5 <- 
  ggplot(pred_5, aes(x = fishBin, y = logCoastLength,
             fill = fishBin, colour = fishBin)) +
  geom_point(size = 2, alpha = 0.4, 
             position = position_jitter(width = 0.15)) +
  geom_violin(trim = FALSE, alpha = 0.6, colour = NA) + 
  geom_segment(data = pred_5_med, 
               aes(x = line_val - 0.4, xend = line_val + 0.4,
                   y = med_f, yend = med_f),
               colour = 'grey20',
               lwd = 1.5) +
  scale_fill_manual(values = c('#fc9272', '#fb6a4a',
                               '#ef3b2c', '#a50f15',
                               '#67000d')) +
  scale_colour_manual(values = c('#fc9272', '#fb6a4a',
                                 '#ef3b2c', '#a50f15',
                                 '#67000d')) +
  publication_theme() +
  scale_y_continuous(limits = c(1, 13.5),
                     breaks = seq(1.5, 13.5, 2)) +
  theme(legend.position = 'none',
        plot.margin = unit(c(0.1, 0.5, 0.1, 0.5), 'cm'),
        axis.title.y = element_text(size = 12),
        axis.text.x = element_text(size = 11),
        axis.title.x = element_text(size = 12)) +
  labs(y = 'log Coastline length',
       x = 'log Fishing pressure')

plot_5

post_5 <- post_plots + plot_5 + plot_layout(ncol = 1, heights = c(0.5, 0.5, 1))

post_5

#ggsave('../../../Figures/EcoCarryCapacity/DensOcc5_191201.png',
#       post_5, height = 20, width = 12, units = c('cm'))

# combine all the plots into one
fit_post_5 <- 
  preds_fitlines_protein + 
  {{post_coast + post_fish + plot_layout(ncol = 1)} +
      plot_5 + plot_layout(ncol = 1, heights = c(0.5, 0.5, 1))} +
  plot_layout(ncol = 2, widths = c(2, 1, 1, 1))

fit_post_5

ggsave('../../../Figures/EcoCarryCapacity/DgPost5_200309.pdf',
       fit_post_5, height = 21, width = 30, units = c('cm'))

# do the same for gear-specific landings ---------------------------------------
# make a model with gear-specific landings
mod_gearcont <- 
  brm(occurrence ~ logCoastLength*logtotalGearTonnes,
       data = pc_data,
       family = 'bernoulli',
       seed = 123)

#saveRDS(mod_gearcont, '../../../ModelOutputs/DGContinuousGear_191104.rds')
mod_gearcont <- readRDS('../../../ModelOutputs/DGContinuousGear_191104.rds')
summary(mod_gearcont)
plot(mod_gearcont)

# fishery landings
QsGear <- summary(pc_data$logtotalGearTonnes)
QminG <- QsGear[1]
Q1G <- QsGear[2]
Q2G <- QsGear[3]
Q3G <- QsGear[5]
QmaxG <- QsGear[6]


quartsG <- 
  lapply(c(QminG, Q1G, Q2G, Q3G, QmaxG), function(x) {
    
    set.seed(123)
    
    qs_df <- 
      pc_data %>% 
      data_grid(logCoastLength = seq_range(logCoastLength, 101)) %>% 
      mutate(logtotalGearTonnes = paste(x),
             logtotalGearTonnes = as.numeric(logtotalGearTonnes)) %>% 
      add_fitted_draws(mod_gearcont, n = 110)
    
    return(qs_df)
    
  })

# bind all of them together
pred_qsG <- 
  rbind(quartsG[[1]], quartsG[[2]], quartsG[[3]], quartsG[[4]], quartsG[[5]])

# calculate means to draw mean line
pred_meansG <- 
  pred_qsG %>% 
  group_by(logtotalGearTonnes, logCoastLength) %>% 
  summarise(mean_val = mean(.value))

# plot with gear-restricted landings
preds_fitlines_gear <- 
  ggplot(pred_qsG, aes(x = logCoastLength, y = occurrence, 
                       colour = factor(logtotalGearTonnes))) +
  geom_line(aes(y = .value, group = paste(logtotalGearTonnes, .draw)), 
            alpha = 0.15) +
  geom_point(data = pc_data, size = 4, shape = '|',
             colour = 'grey60', alpha = 0.7) +
  geom_smooth(data = pred_meansG, aes(x = logCoastLength, y = mean_val,
                                      colour = factor(logtotalGearTonnes)),
              se = FALSE, fullrange = TRUE,
              method = 'glm', 
              method.args = list(family = 'quasibinomial')) +
  scale_y_continuous(limits = c(0, 1), breaks = c(0, 1)) +
  theme(legend.key = element_rect(fill = NA),
        legend.title = element_text(size = 14, colour = 'grey20'),
        legend.text = element_text(size = 13, colour = 'grey20')) +
  guides(colour = guide_legend(override.aes = list(size = 2,
                                                   alpha = 0.8))) +
  scale_colour_manual(values = c('#fed976', '#fd8d3c',
                                 '#fc4e2a', '#e31a1c',
                                 '#800026'),
                      name = 'Fishing\npressure',
                      labels = c('Zero',
                                 'Low',
                                 'Moderate',
                                 'High',
                                 'Maximum')) +
  publication_theme() +
  labs(y = 'Occupancy',
       x = 'log Coastline length',
       title = 'Gear-restricted landings')

preds_fitlines_gear
# -------------------------------------------------------------------------
# NLS model with extra intercept value ------------------------------------
# -------------------------------------------------------------------------

set.seed(123)
mod <- 
  nls(occurrence ~ (1/(1 + exp((a - b_c*logCoastLength) + (b_f*logProteinDiet)))) + c,
           data = pc_data,
           start = list(a = 0.1, b_c = 0.2, b_f = 0.1, c = 0.1))

summary(mod)

# nasty baseplot stuff but whatever ---------------------
x <- seq(min(-3), max(pc_data$logCoastLength), length = 100)
y0 <- predict(mod, list(logCoastLength = x, logProteinDiet = 0))
y1 <- predict(mod, list(logCoastLength = x, logProteinDiet = 0.5))
y2 <- predict(mod, list(logCoastLength = x, logProteinDiet = 1))
y3 <- predict(mod, list(logCoastLength = x, logProteinDiet = 1.5))
y4 <- predict(mod, list(logCoastLength = x, logProteinDiet = 2.5))

plot(occurrence ~ logCoastLength, pc_data, xlim = c(-3, 12),
     col = 'grey60', pch = '|')
points(x, y0, type = 'l', col = 'darkblue', lwd = 2)
points(x, y1, type = 'l', col = '#fd8d3c', lwd = 2)
points(x, y2, type = 'l', col = '#fc4e2a', lwd = 2)
points(x, y3, type = 'l', col = '#e31a1c', lwd = 2)
points(x, y4, type = 'l', col = '#800026', lwd = 2)


coefs <- 
  data.frame(summary(mod)$parameters) %>% 
  dplyr::select(Estimate, Std..Error) %>% 
  rownames_to_column('Coefficients') %>% 
  mutate(Coefficients = dplyr::recode(Coefficients,
                                      'c' = 'New_intercept',
                                      'b_f' = 'Beta_fishing',
                                      'b_c' = 'Beta_coastline',
                                      'a' = 'Beta')) %>% 
  ggplot(aes(x = Estimate, y = Coefficients)) +
  geom_point(size = 4, colour = 'darkslategray') +
  geom_vline(xintercept = 0,
             linetype = 2,
             colour = 'grey40') +
  ggplot2::geom_errorbarh(aes(xmin = Estimate - Std..Error, 
                              xmax = Estimate + Std..Error,
                     height = 0), 
                 colour = 'darkslategray') +
  labs(y = '',
       title = 'NLS Coefficients with mean estimate ± SE') +
  theme_classic()

coefs

ggsave('../../../Figures/EcoCarryCapacity/NLS_coefplot_191119.pdf', coefs,
       width = 28, height = 20, units = c('cm'))

# ----------------------------------------------------------------------
# TESTING MANGROVES ----------------------------------------------------
# ----------------------------------------------------------------------

mod_Ma <- 
  brm(occurrence ~ logCoastLength + logMangroveArea,
      data = pc_data,
      family = 'bernoulli',
      seed = 123)

plot(mod_Ma)

# mangrove cover
QsMang <- summary(pc_data$logMangroveArea)
QminM <- QsMang[1]
Q1M <- QsMang[2]
Q2M <- QsMang[3]
Q3M <- QsMang[5]
QmaxM <- QsMang[6]

quarts <- 
  lapply(c(QminM, Q1M, Q2M, Q3M, QmaxM), function(x) {
    
    set.seed(123)
    
    qs_df <- 
      pc_data %>% 
      #data_grid(logCoastLength = seq_range(logCoastLength, 101)) %>% 
      data_grid(logCoastLength = seq(from = 2, to = 12.5, by = 0.1)) %>% 
      mutate(logMangroveArea = paste(x),
             logMangroveArea = as.numeric(logMangroveArea)) %>% 
      add_fitted_draws(mod_Ma, n = 110)
    
    return(qs_df)
    
  })

# bind all of them together
pred_qsM <- 
  rbind(quarts[[1]], quarts[[2]], quarts[[3]], quarts[[4]], quarts[[5]])

# calculate means to draw mean line
pred_meansM <- 
  pred_qsM %>% 
  group_by(logMangroveArea, logCoastLength) %>% 
  summarise(mean_val = mean(.value))

# plot this badboy
preds_fitlines_man <- 
  pred_qsM %>% 
  ggplot(aes(x = logCoastLength, y = occurrence,
             colour = factor(logMangroveArea))) +
  geom_line(aes(y = .value, group = paste(logMangroveArea, .draw)), 
            alpha = 0.15) +
  geom_point(data = pc_data, size = 4, shape = '|',
             colour = 'grey60', alpha = 0.7) +
  geom_smooth(data = pred_meansM, aes(x = logCoastLength, y = mean_val,
                                      colour = factor(logMangroveArea)),
              se = FALSE, fullrange = TRUE,
              method = 'glm', 
              method.args = list(family = 'quasibinomial'),
              size = 1.5) +
  scale_y_continuous(limits = c(0, 1), breaks = c(0, 1)) +
  theme(legend.position = 'none') +
  guides(colour = guide_legend(override.aes = list(size = 2,
                                                   alpha = 0.8))) +
  scale_colour_manual(values = c('#78c679', '#41ab5d',
                                 '#238443', '#006837',
                                 '#004529'),
                      name = 'Mangrove Area',
                      labels = c('Zero',
                                 'Low',
                                 'Moderate',
                                 'High',
                                 'Maximum')) +
  publication_theme() +
  labs(y = 'Occupancy',
       x = 'log Coastline length')

preds_fitlines_man

ggsave('../../../Figures/EcoCarryCapacity/MangrovePost_191212.pdf', 
       preds_fitlines_man,
       width = 30, height = 20, units = c('cm'))


# ----------------------------------------------------------------------
# TESTING MANAGEMENT ---------------------------------------------------
# ----------------------------------------------------------------------

man <- read_csv('../../../Datasets/ProcessedCovariates_181128.csv')

mod_gov <- 
  brm(occurrence ~ logCoastLength + OHI,
      data = man,
      family = 'bernoulli',
      seed = 123)

plot(mod_gov)

# mangrove cover
QsGov <- summary(man$OHI)
QminG <- QsGov[1]
Q1G <- QsGov[2]
Q2G <- QsGov[3]
Q3G <- QsGov[5]
QmaxG <- QsGov[6]

quarts <- 
  lapply(c(QminG, Q1G, Q2G, Q3G, QmaxG), function(x) {
    
    set.seed(123)
    
    qs_df <- 
      man %>% 
      #data_grid(logCoastLength = seq_range(logCoastLength, 101)) %>% 
      data_grid(logCoastLength = seq(from = 2, to = 12.5, by = 0.1)) %>% 
      mutate(OHI = paste(x),
             OHI = as.numeric(OHI)) %>% 
      add_fitted_draws(mod_gov, n = 110)
    
    return(qs_df)
    
  })

# bind all of them together
pred_qsG <- 
  rbind(quarts[[1]], quarts[[2]], quarts[[3]], quarts[[4]], quarts[[5]])

head(pred_qsG)

# calculate means to draw mean line
pred_meansG <- 
  pred_qsG %>% 
  group_by(OHI, logCoastLength) %>% 
  summarise(mean_val = mean(.value))

# plot this badboy
preds_fitlines_gov <- 
  pred_qsG %>% 
  ggplot(aes(x = logCoastLength, y = occurrence,
             colour = factor(OHI))) +
  geom_line(aes(y = .value, group = paste(OHI, .draw)), 
            alpha = 0.15) +
  geom_point(data = man, size = 4, shape = '|',
             colour = 'grey60', alpha = 0.7) +
  geom_smooth(data = pred_meansG, aes(x = logCoastLength, y = mean_val,
                                      colour = factor(OHI)),
              se = FALSE, fullrange = TRUE,
              method = 'glm', 
              method.args = list(family = 'quasibinomial'),
              size = 1.5) +
  scale_y_continuous(limits = c(0, 1), breaks = c(0, 1)) +
  theme(legend.position = 'none') +
  guides(colour = guide_legend(override.aes = list(size = 2,
                                                 alpha = 0.8))) +
  scale_colour_manual(values = c('#49006a', '#7a0177',
                                 '#ae017e', '#dd3497',
                                 '#f768a1'),
                      name = 'Ocean Health Index',
                      labels = c('Zero',
                                 'Low',
                                 'Moderate',
                                 'High',
                                 'Maximum')) +
  publication_theme() +
  labs(y = 'Occupancy',
       x = 'log Coastline length')

preds_fitlines_gov

ggsave('../../../Figures/EcoCarryCapacity/GovPost_191212.pdf', 
       preds_fitlines_gov,
       width = 30, height = 20, units = c('cm'))















