# This script does the statistical analysis for dynamic geography

library(rstan)
library(tidyverse)
library(modelr)
library(tidybayes)
library(brms)
library(ggstance)
library(cowplot)
library(RColorBrewer)
#library(bayesplot) # loaded by other packages


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


# traceplot of posteriors ----------------------------------------
post_trace <- as.array(mod_fishBin3)

trace_labels <- 
  c('b_logCoastLength' = 'Coastline Length',
    'b_fishBin3Low' = 'Low Fishing',
    'b_fishBin3Moderate' = 'Moderate Fishing',
    'b_Intercept' = 'High Fishing')

bayesplot::color_scheme_set('blue')
trace_plots <- 
  bayesplot::mcmc_trace(post_trace, pars = c('b_logCoastLength',
                                             'b_fishBin3Low',
                                             'b_fishBin3Moderate',
                                             'b_Intercept'),
                        facet_args = list(ncol = 1, strip.position = 'left',
                                          labeller = as_labeller(trace_labels))) +
  theme(strip.placement = 'outside',
        strip.background = element_blank(),
        strip.text = element_text(vjust = 0.8, colour = 'grey20',
                                  size = 12),
        legend.text = element_text(colour = 'grey20'),
        legend.title = element_text(colour = 'grey20')) +
  publication_theme() +
  labs(y = 'Coefficients') +
  guides(colour = guide_legend(override.aes = list(size = 2)))

trace_plots

#ggsave('../../../Figures/EcoCarryCapacity/TracePlot_191017.pdf', trace_plots,
#       height = 22, width = 30.58, units = c('cm'))

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
make_density <- 
  function(intercept_value, colour_right, colour_left) {
    
    # make basic plot to build on
    basic_dens <- 
      post_values %>% 
      dplyr::filter(intercept == intercept_value) %>% 
      ggplot(aes(x = post_pred)) +
      geom_density(fill = colour_right, colour = 'grey90') +
      expand_limits(x = c(-23, 23),
                    y = c(0, 0.2)) +
      geom_vline(xintercept = 0, linetype = 'dashed', colour = 'grey60',
                 size = 0.7) +
      theme(axis.text = element_blank(),
            axis.line.x = element_blank(),
            axis.line.y = element_line(colour = 'grey60'),
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
mod_fishcont <- 
  brm(occurrence ~ logCoastLength*logProteinDiet,
      data = pc_data,
      family = 'bernoulli',
      seed = 123)

#saveRDS(mod_fishcont, '../../../ModelOutputs/DGContinuous_191016.rds')
mod_fishcont <- readRDS('../../../ModelOutputs/DGContinuous_191016.rds')
summary(mod_fishcont)

plot(mod_fishcont)

get_variables(mod_fishcont)

# simulate and predict data with zero fishing pressure
zero_df <- 
  pc_data %>% 
  data_grid(logCoastLength = seq_range(logCoastLength, 101)) %>% 
  mutate(logProteinDiet = 0) %>% 
  add_fitted_draws(mod_fishcont, n = 100)

zero_df

zero_pred <- 
  zero_df %>% 
  group_by(logCoastLength) %>% 
  summarise(mean_value = mean(.value))

zero_pred
# fitlines plot ----------------------
fitlines_nofish <- 
  pc_data %>% 
  data_grid(logCoastLength = seq_range(logCoastLength, 101)) %>% 
  mutate(logProteinDiet = 0) %>% 
  add_fitted_draws(mod_fishcont, n = 100) %>% 
  ggplot(aes(x = logCoastLength, y = occurrence)) +
  geom_line(aes(y = .value, group = paste(.draw)), alpha = 0.1,
            colour = 'darkblue') +
  geom_point(data = pc_data, size = 4, shape = '|',
             colour = 'grey60', alpha = 0.7) +
  geom_line(data = zero_pred, aes(x = logCoastLength, y = mean_value),
            colour = 'darkblue',
            size = 1.5) +
  scale_y_continuous(limits = c(0, 1), breaks = c(0, 1)) +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank()) +
  labs(y = 'Occurrence') +
  publication_theme()
  

fitlines_nofish

# fitlines plot for all fishing pressures 
fitlines_all <- 
  plot_grid(fitlines_nofish, fitlines_plot, ncol = 1,
            align = 'v', scale = 1)

fitlines_all

#posterior distrbution plot --------------
post_values_nofish <- 
  posterior_samples(mod_fishcont, '^b') %>% 
  dplyr::select('b_Intercept')

basic_dens <- 
  ggplot(post_values_nofish, aes(x = b_Intercept)) +
  geom_density(colour = 'grey90') +
  expand_limits(x = c(-23, 23),
                y = c(0, 0.4)) +
  geom_vline(xintercept = 0, linetype = 'dashed', colour = 'grey60',
             size = 0.7) +
  theme(axis.text = element_blank(),
        axis.line.y = element_line(colour = 'grey60'),
        axis.line.x = element_line(colour = 'grey60'),
        panel.grid = element_blank(),
        panel.background = element_blank(),
        axis.title.x = element_blank(),
        axis.ticks = element_blank(),
        axis.title.y = element_text(size = 13, colour = 'grey20', angle = 0, hjust = 0,
                                    vjust = 0.5)) +
  labs(y = 'No Fishing')
  
dens_df <- ggplot_build(basic_dens)$data[[1]]

nofish_plot <- 
  basic_dens +
  geom_area(data = dens_df %>% 
              dplyr::filter(x < 0),
            aes(x = x, y = y),
            fill = 'darkblue')

nofish_plot



all_density <- 
  plot_grid(nofish_plot, low, mod, high, ncol = 1, align = 'v', scale = 1)

all_density2 <- ggdraw(add_sub(all_density, 'Posterior Prediction', hjust = 0.1))
all_density2


# now combine all density plots and fitlines plots
# this has fishing pressures AND zero fishing
all_plots <- 
  plot_grid(fitlines_all, all_density2, ncol = 2)

all_plots

ggsave('../../../Figures/EcoCarryCapacity/DynamicGeographyZeroFish_191017.pdf', 
       all_plots,
       height = 20, width = 30, units = c('cm'))

# without zero fishing
allfish_density_unlabelled <- 
  plot_grid(low, mod, high, ncol = 1, align = 'v', scale = 1)


allfish_density <- 
  ggdraw(add_sub(allfish_density_unlabelled, 'Posterior Prediction', hjust = 0.1))

allfish_all_plots <- 
  plot_grid(fitlines_plot, allfish_density, ncol = 2,
            scale = 1)

allfish_all_plots

ggsave('../../../Figures/EcoCarryCapacity/DynamicGeography_191017.pdf',
       allfish_all_plots, height = 20, width = 40, units = c('cm'))

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
    
    set.seed(123)
    
    qs_df <- 
      pc_data %>% 
      data_grid(logCoastLength = seq_range(logCoastLength, 101)) %>% 
      mutate(logProteinDiet = paste(x),
             logProteinDiet = as.numeric(logProteinDiet)) %>% 
      add_fitted_draws(mod_fishcont, n = 110)
    
    return(qs_df)
    
  })

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
  dplyr::filter(!logProteinDiet %in% c(QminP, QmaxP)) %>% 
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
              size = 1) +
  scale_y_continuous(limits = c(0, 1), breaks = c(0, 1)) +
  theme(legend.key = element_rect(fill = NA),
        legend.title = element_text(size = 14, colour = 'grey20'),
        legend.text = element_text(size = 13, colour = 'grey20')) +
  guides(colour = guide_legend(override.aes = list(size = 2,
                                                   alpha = 0.8))) +
  scale_colour_manual(values = c('darkblue', '#fd8d3c',
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
       title = 'Fishing pressure = protein consumption')
  

preds_fitlines_protein

ggsave('../../../Figures/EcoCarryCapacity/DynamicGeography_191114.pdf',
       preds_fitlines_protein, height = 20, width = 25, units = c('cm'))

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
# Re-write continuous model with intercept term and STAN ------------------
# -------------------------------------------------------------------------
library(bayesplot)

stan_cont <- "
  data {
    int N;
    vector[N] logCoastLength;
    //int<lower = 0, upper = 1> occurrence[N];
  }
  
  parameters {
    real alpha;
    real b_coast;
    
  }
  
  model {
    
    occurrence ~ (1 / (1 + exp(alpha - b_coast*logCoastLength)));
    //occurrence ~ bernoulli(inv_logit(alpha + b_coast*logCoastLength));
   
  }
  
"

stan_data <- 
  list(N = nrow(pc_data),
       logCoastLength = pc_data$logCoastLength,
       occurrence = pc_data$occurrence)
       #logCoastPop = pc_data$logCoastPop)


mod_stan_cont <- stan(model_code = stan_cont,
                      data = stan_data,
                      chains = 2,
                      iter = 2000,
                      pars = c('alpha', 'b_coast', 'c'))

y = 1 / (1 + exp[a - bx] ) + c

stan_cont <- "
  data {
    int N;
    vector[N] logCoastLength;
    int<lower = 0, upper = 1> occurrence;
  }
  
  parameters {
    real alpha;
    real b_coast;
    real c;
  }
  
  model {
    occurrence ~ bernoulli_logit(alpha + b_coast * logCoastLength) + c;
  }
  
"





stan_data <- 
  list(N = nrow(pc_data),
       logCoastLength = pc_data$logCoastLength,
       logProteinDiet = pc_data$logProteinDiet,
       occurrence = pc_data$occurrence)

summary(mod_stan_cont)
posterior <- as.matrix(mod_stan_cont)
bayesplot::mcmc_areas(posterior,
                      pars = c('alpha', 'b_coast', 'sigma'),
                      prob = 0.95)


# let's try to run this shit frequentist?









  y = 1 / (1 + exp[a - bx] ) + c


mod <- nls(occurrence ~ 1/(1 + exp(a - b*logCoastLength)),
           data = pc_data, a = 1, b = 1)

mod <- nls(occurrence ~ 1/(1 + exp(a - b*logCoastLength)),
           data = pc_data, start = list(a = 1, b = 1))





#simulate some data
set.seed(20160227)
x<-seq(0,50,1)

#intercept set at -10
y<-(((runif(1,10,20)*x)/(runif(1,0,10)+x))+rnorm(51,0,1)-10)
#for simple models nls find good starting values for the parameters even 
#if it throw a warning
m<-nls(y~(a*x/(b+x))+c,
       start = list(a = 1, b = 1, c=-2))
#get some estimation of goodness of fit
cor(y,predict(m))

#plot
plot(x,y)
lines(x,predict(m),lty=2,col="red",lwd=3)

summary(m)

set.seed(123)
mod <- nls(occurrence ~ (1/(1 + exp(a - b_c*logCoastLength) + b_f*logProteinDiet)) + c,
           data = pc_data,
           start = list(a = 0.1, b_c = 0.2, b_f = 0.1, c = 0.1))

mod <- nls(occurrence ~ (1/(1 + exp(a - b_c*logCoastLength))) + c,
           data = pc_data,
           start = list(a = 0.1, b_c = 0.2, c = 0.1))

summary(mod)

plot(occurrence ~ logCoastLength, pc_data)
lines(pc_data$logCoastLength, predict(mod))








