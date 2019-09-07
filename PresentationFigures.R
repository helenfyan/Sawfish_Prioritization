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
        plot.background = element_rect(fill = '#080818', colour = 'white', size = 1),
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
