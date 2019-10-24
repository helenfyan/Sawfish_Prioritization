# This script makes presentation figures 

library(tidyverse)
library(mapview)
library(broom)
library(RColorBrewer)

# make presentation theme for all figures ------------------------
presentation_theme <- function(axis_text_size = 13, axis_y_ticks = element_line()) {
  theme(panel.background = element_blank(),
        axis.line = element_line(colour = 'black'),
        axis.text.y = element_text(size = axis_text_size, colour = 'black'),
        axis.text.x = element_text(colour = 'black'),
        axis.ticks.y = axis_y_ticks,
        axis.title = element_text(size = 14))
}

# --------------------------------------------------------------------------------------
# CLEAN PREDICTED DATA  ----------------------------------------------------------------
# --------------------------------------------------------------------------------------
setwd('/users/helenyan/desktop/school/directed studies 2018/datasets')
predRaw <- read_csv('GBMPredicted_181214.csv')

pred <- 
  predRaw %>% 
  dplyr::select(-X1) %>% 
  dplyr::rename('yhat' = '.') %>% 
  group_by(ISO3) %>% 
  summarise(value = mean(yhat))

write.csv(pred, 'GBMPredictedMeans_190122.csv')

# --------------------------------------------------------------------------------------
# MAPS  --------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------

map_theme <- function(legendSpace = 0.2, legendText = 13) {
  
  theme(plot.background = element_rect(fill = 'transparent', colour = NA),
        panel.background = element_rect(fill = '#080818', colour = '#080818'),
        panel.grid = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        legend.text = element_text(size = legendText),
        legend.title = element_blank(),
        legend.spacing.x = unit(legendSpace, 'cm'),
        legend.background = element_rect(fill = 'transparent', colour = 'transparent'),
        legend.key = element_rect(fill = 'transparent', colour = 'transparent'))
  
}

# Map of the world for conservation stories --------------------
worldmap <- map_data('world')
worldmap <- 
  worldmap %>% 
  filter(region != 'Antarctica') 

condf <- 
  data.frame(region = c('China', 'Indonesia', 'USA', 'Canada'),
             value = c('panda', 'orang', 'orca', 'orca'),
             stringsAsFactors = FALSE)

conmap <- 
  ggplot() +
  geom_map(data = worldmap, map = worldmap,
           aes(x = long, y = lat, group = group, map_id = region),
           fill = '#CBCBCB', colour = '#080818', size = 0.5) +
  geom_map(data = condf, map = worldmap,
           aes(fill = value, map_id = region),
           colour = '#080818', size = 0.5) +
  scale_fill_manual(values = c('#fed9a6', '#fbb4ae', '#beaed4')) +
  coord_map('rectangular', lat0 = 0, xlim = c(-180, 180), ylim = c(-60, 90)) +
  labs(x = ' ', y = ' ') +
  map_theme() + 
  theme(legend.position = 'none')

regmap <- 
  ggplot() +
  geom_map(data = worldmap, map = worldmap,
           aes(x = long, y = lat, group = group, map_id = region),
           fill = 'grey15', colour = '#080818', size = 0.5) +
  coord_map('rectangular', lat0 = 0, xlim = c(-180, 180), ylim = c(-60, 90)) +
  labs(x = ' ', y = ' ') +
  map_theme() + 
  theme(legend.position = 'none')

print(regmap)
# --------------------------------------------------------------------------------------
# PDPs  --------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------

setwd('/users/helenyan/desktop/school/directed studies 2018/datasets/GBM out')

# Read in all results 
temp = list.files(pattern="GBMResults*")
dfs <- lapply(temp, read.csv)

for(i in 1:20) {
  
  #pdf(paste('../Figures/2GBMout', i, '_181212.pdf', sep = ''))
  
  pdpplot <- 
    ggplot(dfs[[i]], aes_string(x = colnames(dfs[[i]][2]), y = colnames(dfs[[i]][4]), 
                                group = 1)) +
    geom_ribbon(aes(ymin = totalmin, ymax = totalmax),
                alpha = 0.6, fill = 'steelblue4') +
    geom_line(size = 1) +
    #scale_y_continuous(limits = c(0, 1), breaks = c(0, 1)) +
    geom_rug(sides = 'b') +
    labs(y = 'Partial Response on Occurrence') +
    presentation_theme()
  
  print(pdpplot)
  
  #dev.off()
  
}

pdpplots <- 
  lapply(names(sppTrain), function(x) {
    
    dfplot <- 
      pdpgbm %>% 
      pdp::partial(pred.var = paste(x),
                   grid.resolution = 102,
                   n.trees = pdpgbm$n.trees,
                   prob = TRUE) %>% 
      autoplot(rug = TRUE, train = sppTrain)
    theme_classic()
    
    print(dfplot)
    
  })

#  --------------------------------------------------------------------------------
# Clean Results -------------------------------------------------------------------
#  --------------------------------------------------------------------------------
rm(list=ls())
setwd('/users/helenyan/desktop/school/directed studies 2018/datasets/gbm out/')

dark_theme <- function(axis_text_size = 13, axis_y_ticks = element_line()) {
  theme(panel.grid = element_blank(),
        panel.background = element_rect(fill = '#080818', colour = '#080818'),
        plot.background = element_rect(fill = '#080818', colour = 'white', size = 1),
        axis.title.x = element_text(colour = 'white', size = 13),
        axis.title.y = element_text(colour = 'white', size = 13),
        axis.line = element_line(colour = 'white'),
        axis.text.y = element_text(size = axis_text_size, colour = 'white'),
        axis.text.x = element_text(colour = 'white'),
        axis.ticks.y = axis_y_ticks,
        axis.title = element_text(size = 14))
}

# Make PDP plots
figs <- list.files(pattern = 'CleanGBMout*')
figsdf <- lapply(figs, read_csv)

for(i in 1:20) {
  
  dfscols <- colnames(figsdf[[i]])
  col <- dfscols[1]
  
  #pdf(paste('GBMout', col, '_190124.pdf', sep = ''))
  
  pdpplot <- 
    ggplot(figsdf[[i]], aes_string(x = colnames(figsdf[[i]][2]), y = colnames(figsdf[[i]][4]), 
                                group = 1)) +
    geom_ribbon(aes(ymin = totalmin, ymax = totalmax),
                alpha = 0.6, fill = 'steelblue4') +
    geom_line(size = 1) +
    #scale_y_continuous(limits = c(0, 1), breaks = c(0, 1)) +
    geom_rug(sides = 'b') +
    labs(y = 'Partial Response on Occurrence')
  
  print(pdpplot)
  
  #dev.off()
  
}


#  --------------------------------------------------------------------------------
# IDEAS figures -------------------------------------------------------------------
#  --------------------------------------------------------------------------------
rm(list=ls())
setwd('/users/helenyan/desktop/school/directed studies 2018/datasets/gbm out/')

dark_theme <- function(axis_text_size = 13, axis_y_ticks = element_line()) {
  theme(panel.grid = element_blank(),
        panel.background = element_rect(fill = '#080818', colour = '#080818'),
        plot.background = element_rect(fill = '#080818', colour = '#080818', size = 1),
        axis.title.x = element_text(colour = 'white', size = 18),
        axis.title.y = element_text(colour = 'white', size = 18),
        axis.line = element_line(colour = 'white'),
        axis.text.y = element_text(size = axis_text_size, colour = 'white'),
        axis.text.x = element_text(colour = 'white'),
        axis.ticks.y = axis_y_ticks,
        axis.title = element_text(size = 14))
}

# Make PDP plots
figs <- list.files(pattern = 'CleanGBMout*')
figsdf <- lapply(figs, read_csv)

coast <- figsdf[[3]]
protein <- figsdf[[9]]
gear <- figsdf[[10]]
mangrove <- figsdf[[7]]
ohi <- figsdf[[13]]
hdi <- figsdf[[1]]


# ecological carrying capacity ------
library(gridExtra)
library(ggpubr)

setwd('..')

coastplot <- 
  ggplot(coast, aes(x = logCoastLength, y = totalmean)) +
  geom_ribbon(aes(ymin = totalmin, ymax = totalmax),
              alpha = 0.8, fill = '#01FF75') +
  geom_line(size = 2, colour = 'white') +
  labs(y = 'Marginal effect on presence\n',
       x = 'log(Coastline length km)') +
  geom_rug(sides = 'b', colour = 'grey30') +
  dark_theme()

print(coastplot)
ggsave('../Presentations/IDEAS/PDPCoastline.pdf', coastplot, 
       height = 17.06, width = 15.51, units = c('cm'))

mangroveplot <- 
  ggplot(mangrove, aes(x = logMang, y = totalmean)) +
  geom_ribbon(aes(ymin = totalmin, ymax = totalmax),
              alpha = 0.8, fill = '#01FF75') +
  geom_line(size = 2, colour = 'white') +
  labs(y = 'Marginal effect on presence\n', x = expression('log(Mangrove area' ~ km^2~ ')')) +
  geom_rug(sides = 'b', colour = 'grey30') +
  dark_theme()

print(mangroveplot)
ggsave('../Presentations/IDEAS/PDPMangroveP.pdf', mangroveplot, 
       height = 17.06, width = 15.51, units = c('cm'))

# fishing pressures ------
proteinplot <- 
  ggplot(protein, aes(x = logProteinDiet, y = totalmean)) +
  geom_ribbon(aes(ymin = totalmin, ymax = totalmax),
              alpha = 0.8, fill = '#00B0F0') +
  geom_line(size = 2, colour = 'white') +
  labs(y = 'Marginal effect on presence\n', 
       x = 'log(Marine protein consumption g/capita)') +
  geom_rug(sides = 'b', colour = 'grey30') +
  dark_theme()

print(proteinplot)
ggsave('../Presentations/IDEAS/PDPProtein.pdf', proteinplot, 
       height = 17.06, width = 15.51, units = c('cm'))

gearplot <- 
  ggplot(gear, aes(x = logtotalGearTonnes, y = totalmean)) +
  geom_ribbon(aes(ymin = totalmin, ymax = totalmax),
              alpha = 0.8, fill = '#00B0F0') +
  geom_line(size = 2, colour = 'white') +
  labs(y = 'Marginal effect on presence\n', x = 'log(Gear-specific landings t)') +
  geom_rug(sides = 'b', colour = 'grey30') +
  dark_theme()

print(gearplot)
ggsave('../Presentations/IDEAS/PDPFishingGear.pdf', gearplot, 
       height = 17.06, width = 15.51, units = c('cm'))

# management capacity ------
ohiplot <- 
  ggplot(ohi, aes(x = OHI, y = totalmean)) +
  geom_ribbon(aes(ymin = totalmin, ymax = totalmax),
              alpha = 0.8, fill = '#FA1484') +
  geom_line(size = 2, colour = 'white') +
  labs(y = 'Marginal effect on presence\n', x = 'Ocean Health Index') +
  geom_rug(sides = 'b', colour = 'grey30') +
  dark_theme()

print(ohiplot)
ggsave('../Presentations/IDEAS/PDPOHI.pdf', ohiplot, 
       height = 17.06, width = 15.51, units = c('cm'))

hdiplot <- 
  ggplot(hdi, aes(x = HDI, y = totalmean)) +
  geom_ribbon(aes(ymin = totalmin, ymax = totalmax),
              alpha = 0.8, fill = '#FA1484') +
  geom_line(size = 2, colour = 'white') +
  labs(y = 'Marginal effect on presence\n', x = 'Human Development Index') +
  geom_rug(sides = 'b', colour = 'grey30') +
  dark_theme()

print(hdiplot)
ggsave('../Presentations/IDEAS/PDPHDI.pdf', hdiplot, 
       height = 17.06, width = 15.51, units = c('cm'))


#  --------------------------------------------------------------------------------
# ECO/EVO figures -----------------------------------------------------------------
#  --------------------------------------------------------------------------------

# load dark theme from above
library(tidybayes)
library(modelr)

pc_data <- 
  read_csv('../../../Datasets/DynamicGeographyBinned_190925.csv') %>% 
  mutate(fishBin3 = dplyr::recode(fishBin3,
                                  'low' = 'Low',
                                  'moderate' = 'Moderate',
                                  'high' = 'High'))

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
  geom_line(aes(y = .value, group = paste(fishBin3, .draw)), alpha = 0.2,
            size = 0.8) +
  scale_colour_manual(values = c('#fd8d3c', '#fc4e2a', '#bd0026')) + 
  geom_point(data = pc_data, size = 4, shape = '|',
             colour = 'grey60', alpha = 0.7) +
  geom_smooth(data = pc_data, method = 'glm',
              method.args = list(family = 'binomial'),
              se = FALSE, size = 2,
              fullrange = TRUE) +
  scale_y_continuous(limits = c(0, 1), breaks = c(0, 1)) +
  labs(y = 'Occupancy', 
       x = 'Habitat availability (coastline length (km))') +
  theme(legend.position = 'none') +
  dark_theme()

fitlines_plot

# just low fising ----
set.seed(123)
low_df <- 
  pc_data %>% 
  group_by(fishBin3) %>% 
  data_grid(logCoastLength = seq_range(pc_data$logCoastLength, 101)) %>% 
  add_fitted_draws(mod_fishBin3, n = 100) %>% 
  dplyr::filter(fishBin3 == 'Low')

fitlines_low <- 
  pc_data %>% 
  ggplot(aes(x = logCoastLength, y = occurrence,
             colour = fishBin3)) +
  geom_point(size = 4, shape = '|',
             colour = 'grey60', alpha = 0.7) +
  geom_smooth(method = 'glm',
              method.args = list(family = 'binomial'),
              se = FALSE, size = 2,
              fullrange = TRUE) +
  scale_colour_manual(values = c('#080818', '#fd8d3c', '#080818')) +
  geom_line(data = low_df, aes(y = .value, group = paste(.draw)), alpha = 0.2,
            size = 0.8, colour = '#fd8d3c') +
  scale_y_continuous(limits = c(0, 1), breaks = c(0, 1)) +
  labs(y = 'Occupancy', 
       x = 'Habitat availability (coastline length (km))') +
  theme(legend.position = 'none') +
  dark_theme()

fitlines_low

ggsave('../../../Presentations/EcoEvo/LowFishing_191023.png',
       fitlines_low, 
       height = 20, width = 30, units = c('cm'),
       bg = '#080818')

# just mod fishing ----
set.seed(123)
low_df <- 
  pc_data %>% 
  group_by(fishBin3) %>% 
  data_grid(logCoastLength = seq_range(pc_data$logCoastLength, 101)) %>% 
  add_fitted_draws(mod_fishBin3, n = 100) %>% 
  dplyr::filter(fishBin3 %in% c('Low', 'Moderate'))

mod <- 
  pc_data %>% 
  dplyr::filter(fishBin3 %in% c('Low', 'Moderate'))

fitlines_plot <- 
  ggplot(pc_data, aes(x = logCoastLength, y = occurrence,
                      colour = fishBin3)) +
  geom_point(size = 4, shape = '|',
             colour = 'grey60', alpha = 0.7) +
  geom_smooth(data = mod, 
              aes(colour = fishBin3),
              method = 'glm',
              method.args = list(family = 'binomial'),
              se = FALSE, size = 2,
              fullrange = TRUE) +
  #scale_colour_manual(values = c('#fd8d3c', '#fc4e2a')) +
  geom_line(data = low_df, aes(y = .value, group = paste(fishBin3, .draw)), alpha = 0.2,
            size = 0.8) +
  scale_colour_manual(values = c('#fd8d3c', '#fc4e2a')) +
  scale_y_continuous(limits = c(0, 1), breaks = c(0, 1)) +
  labs(y = 'Occupancy', 
       x = 'Habitat availability (coastline length (km))') +
  theme(legend.position = 'none') +
  dark_theme()

fitlines_plot

ggsave('../../../Presentations/EcoEvo/ModFishing_191023.png',
       fitlines_plot, 
       height = 20, width = 30, units = c('cm'),
       bg = '#080818')

# -------------------------------------------
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
  geom_line(aes(y = .value, group = paste(fishBin3, .draw)), alpha = 0.2,
            size = 0.8) +
  scale_colour_manual(values = c('#fd8d3c', '#fc4e2a', '#bd0026')) + 
  geom_point(data = pc_data, size = 4, shape = '|',
             colour = 'grey60', alpha = 0.7) +
  geom_smooth(data = pc_data, method = 'glm',
              method.args = list(family = 'binomial'),
              se = FALSE, size = 2,
              fullrange = TRUE) +
  scale_y_continuous(limits = c(0, 1), breaks = c(0, 1)) +
  labs(y = 'Occupancy', 
       x = 'Habitat availability (coastline length (km))') +
  theme(legend.position = 'none') +
  dark_theme()

fitlines_plot

ggsave('../../../Presentations/EcoEvo/HighFishing_191023.png',
       fitlines_plot, 
       height = 20, width = 30, units = c('cm'),
       bg = '#080818')

fitlines_points <- 
  ggplot(pc_data, aes(x = logCoastLength, y = occurrence)) +
  geom_point(data = pc_data, size = 4, shape = '|',
             colour = 'grey60', alpha = 0.7) +
  scale_y_continuous(limits = c(0, 1), breaks = c(0, 1)) +
  labs(y = 'Occupancy', 
       x = 'Habitat availability (coastline length (km))') +
  dark_theme()

fitlines_points

ggsave('../../../Presentations/EcoEvo/JustPoints_191023.png',
       fitlines_points, 
       height = 20, width = 30, units = c('cm'),
       bg = '#080818')
