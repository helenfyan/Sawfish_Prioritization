# This script makes publication figures for the paper

library(tidyverse)

setwd('/users/helenyan/desktop/school/directed studies 2018/datasets/')

# make presentation theme for all figures ------------------------
presentation_theme <- function(axis_text_size = 13, axis_y_ticks = element_line()) {
  theme(panel.background = element_blank(),
        axis.line = element_line(colour = 'black'),
        axis.text.y = element_text(size = axis_text_size, colour = 'black'),
        axis.text.x = element_text(colour = 'black'),
        axis.ticks.y = axis_y_ticks,
        axis.title = element_text(size = 14))
}

# this makes the background dark navy blue for presentations 
#panel.background = element_rect(fill = '#080818', colour = '#080818')

#  -----------------------------------------------------------------
#  Collinearity heatmap plot ---------------------------------------
#  -----------------------------------------------------------------

library(reshape2)

raw <- read_csv('ProcessedCovariates_190119.csv')

dat <- 
  raw %>% 
  select(2, 9:28) %>% 
  distinct(ISO3, .keep_all = TRUE) %>% 
  select(-ISO3) %>% 
  select(-ReefFishers, -EPI)
  
sapply(dat, function(x) sum(is.na(x)))

cormat <- round(cor(dat), 2)

# reorder the correlation matrix according to the magnitude of correlation
reorderCormat <- function(cormat) {
  
  # use correlation between variables as distance
  dist <- as.dist((1 - cormat)/2)
  hc <- hclust(dist)
  cormat <- cormat[hc$order, hc$order]
  
}

cormat <- reorderCormat(cormat)

# get rid of half of the cormat

getLowerTri <- function(cormat) {
  
  cormat[upper.tri(cormat)] <- NA
  return(cormat)
  
}

lowerTri <- getLowerTri(cormat)

corfig <-
  melt(lowerTri, na.rm = TRUE) %>%
  mutate(Var2 = dplyr::recode(Var2, 'logEstDis' = 'Estuaries Discharge (ESD)',
                              'logPprodMean' = 'Primary Productivity (PPD)',
                              'logFinUSD' = 'Fin Exports (FXP)',
                              'logCoastPop' = 'Coastal Population (CTP)',
                              'logChondLand' = 'Chondrichthyes Landings (CHL)',
                              'SstMean' = 'Sea Surface Temperature (°C; SST)',
                              'logMang' = 'Mangrove Area (MNG)',
                              'HDI' = 'Human Development Index (HDI)',
                              #'EPI' = 'Environmental Performance Index (EPI)',
                              'WGI' = 'World Governance Index (WGI)',
                              'OHI' = 'Ocean Health Index (OHI)',
                              'NBI' = 'National Biodiversity Index (NBI)',
                              'logIuu' = 'Illegal Unreported and Unregulated Fishing (IUU)',
                              'logGDP' = 'Gross Domestic Product (GDP)',
                              'logCoastLength' = 'Coastline length (km; CLL)',
                              'logProteinDiet' = 'Marine Protein Supply (MPS)',
                              'logtotalGearTonnes' = 'Fishing Gear Landed Tonnes (FGT)',
                              #'ReefFishers' = 'Reef Fishers (RFF)',
                              'logFishProd' = 'Marine Fisheries Production (MFP)',
                              'logChondCatch' = 'Chondrichthyes Catches (CHC)')) %>%
  mutate(Var1 = dplyr::recode(Var1, 'logEstDis' = 'ESD',
                              'logPprodMean' = 'PPD',
                              'logFinUSD' = 'FXP',
                              'logCoastPop' = 'CTP',
                              'logChondLand' = 'CHL',
                              'SstMean' = 'SST',
                              'logMang' = 'MNG',
                              'HDI' = 'HDI',
                              #'EPI' = 'EPI',
                              'WGI' = 'WGI',
                              'OHI' = 'OHI',
                              'NBI' = 'NBI',
                              'logIuu' = 'IUU',
                              'logGDP' = 'GDP',
                              'logCoastLength' = 'CLL',
                              'logProteinDiet' = 'MPS',
                              'logtotalGearTonnes' = 'FGT',
                              #'ReefFishers' = 'RFF',
                              'logFishProd' = 'MFP',
                              'logChondCatch' = 'CHC')) %>%
  # reverse the order of x to get plot aligned with axes
  mutate(Var1 = as.factor(Var1)) %>%
  mutate(Var1 = factor(Var1, levels = rev(levels(Var1)))) %>%
  ggplot(., aes(x = Var1, y = Var2, fill = value)) +
  geom_tile(colour = 'white') +
  scale_fill_gradient2(high = 'red', low = 'blue', mid = 'white',
                      midpoint = 0, limit = c(-1, 1), space = 'Lab',
                      name = 'Pearson\nCorrelation') +
  coord_fixed() +
  geom_text(aes(x = Var1, y = Var2, label = value), colour = 'black', size = 3) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1,
                                   size = 12, hjust = 1),
        legend.justification = c(1, 0),
        legend.position = c(0.95, 0.85),
        legend.direction = 'horizontal') +
  guides(fill = guide_colourbar(barwidth = 7, barheight = 1,
                                title.position = 'top', title.hjust = 0.5)) +
  labs(title = 'Correlation Heatmap', 
       y = ' ',
       x = ' ') +
  presentation_theme()

print(corfig)


#  -----------------------------------------------------------------
#  Partial dependence plots ----------------------------------------
#  -----------------------------------------------------------------

setwd('/users/helenyan/desktop/school/directed studies 2018/datasets/GBM out')

figs <- list.files(pattern = '*_190201.csv')
dfs <- lapply(figs, read.csv)

for(i in 1:20) {
  
  #pdf(paste('../Figures/2GBMout', i, '_181212.pdf', sep = ''))
  
  pdpplot <- 
    ggplot(dfs[[i]], aes_string(x = colnames(dfs[[i]][2]), y = colnames(dfs[[i]][3]), 
                         group = 1)) +
    geom_ribbon(aes(ymin = totmin, ymax = totmax),
                alpha = 0.6, fill = 'steelblue4') +
    geom_line(size = 1) +
    #scale_y_continuous(limits = c(0.1, 0.7)) +
    geom_rug(sides = 'b') +
    labs(y = 'Partial Response on Occurrence') +
    presentation_theme()
  
  print(pdpplot)
  
  #dev.off()
  
}

#  -----------------------------------------------------------------
#  Map of EOO for each spp -----------------------------------------
#  -----------------------------------------------------------------
library(tidyverse)
library(maptools)
library(broom)
library(RColorBrewer)

# presentation theme for maps

map_theme <- function(legendSpace = 0.2, legendText = 13) {
  
  theme(plot.background = element_rect(fill = 'transparent', colour = NA),
        panel.background = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        legend.text = element_text(size = legendText),
        legend.title = element_blank(),
        legend.spacing.x = unit(legendSpace, 'cm'),
        legend.background = element_rect(fill = 'transparent', colour = 'transparent'),
        legend.key = element_rect(fill = 'transparent', colour = 'transparent'))
  
}

# make a dataframe for each species
raw <- read_csv('CompleteSpeciesISO_180924.csv')

all <- 
  raw %>% 
  dplyr::select(country, ISO3, species, presence) %>% 
  mutate(presence = dplyr::recode(presence, 'present' = 'Extant',
                                  'absent' = 'Extinct',
                                  'unknown' = 'Presence Unknown'))

large <- 
  all %>% 
  filter(species == 'large')

small <- 
  all %>% 
  filter(species == 'small')

narrow <- 
  all %>% 
  filter(species == 'narrow')

green <- 
  all %>% 
  filter(species == 'green')

dwarf <- 
  all %>% 
  filter(species == 'dwarf')

data(wrld_simpl)
wrld <- broom::tidy(wrld_simpl, region = 'ISO3')

wrld <- 
  wrld %>% 
  filter(id != 'ATA') %>% 
  filter(between(lat, -50, 55)) %>% 
  filter(long >= -130)

SEasia <- 
  wrld %>% 
  filter(long >= 10)

Atla <- 
  wrld %>% 
  filter(long < 30)

lrg <- 
  ggplot() +
  geom_map(data = wrld, map = wrld, 
           aes(map_id = id, x = long, y = lat),
           fill = 'grey90', colour = 'black', size = 0.25) +
  geom_map(data = large, map = wrld,
           aes(map_id = ISO3, fill = presence), colour = 'black', size = 0.25) +
  scale_fill_manual(values = c('Extinct' = '#b2182b',
                               'Extant' = '#2166ac',
                               'Presence Unknown' = '#f4a582')) +
  theme(plot.background = element_rect(fill = 'transparent', colour = NA),
        panel.background = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        legend.text = element_text(size = 10),
        legend.title = element_blank(),
        legend.spacing.x = unit(0.2, 'cm'),
        legend.background = element_rect(fill = 'transparent', colour = 'transparent'),
        legend.key = element_rect(fill = 'transparent', colour = 'transparent'),
        legend.position = c(0.12, 0.12)) +
  coord_map() +
  annotate('text', x = -90, y = 60, label = 'e) Pristis pristis', 
           size = 7, fontface = 'italic') +
  labs(y = '', title = 'Largetooth Sawfish', x = '')

pdf('../Figures/MapSpLargeEOO_181219.pdf')
print(lrg)
dev.off()


sml <- 
  ggplot() +
  geom_map(data = Atla, map = Atla, 
           aes(map_id = id, x = long, y = lat),
           fill = 'grey90', colour = 'black', size = 0.25) +
  geom_map(data = small, map = Atla,
           aes(map_id = ISO3, fill = presence), colour = 'black', size = 0.25) +
  scale_fill_manual(values = c('Extinct' = '#8e0152',
                               'Extant' = '#01665e',
                               'Presence Unknown' = '#f1b6da')) +
  coord_map() +
  map_theme() +
  theme(legend.position = c(0.18, 0.12)) +
  annotate('text', x = -100, y = 60, label = 'c) Pristis pectinata',
           size = 7, fontface = 'italic') +
  labs(y = '', x = '', title = 'Smalltooth Sawfish')

pdf('../Figures/MapSpSmallEOO_181219.pdf')
print(sml)
dev.off()


nar <- 
  ggplot() +
  geom_map(data = SEasia, map = SEasia, 
           aes(map_id = id, x = long, y = lat),
           fill = 'grey90', colour = 'black', size = 0.25) +
  geom_map(data = narrow, map = SEasia,
           aes(map_id = ISO3, fill = presence), colour = 'black', size = 0.25) +
  scale_fill_manual(values = c('Extinct' = '#67001f',
                               'Extant' = '#4393c3',
                               'Presence Unknown' = '#f4a582')) +
  coord_map() +
  map_theme() +
  theme(legend.position = c(0.3, 0.14)) +
  annotate('text', x = 55, y = 60, label = 'a) Anoxypristis cuspidata', 
           size = 7, fontface = 'italic') +
  labs(y = '', x = '', title = 'Narrow Sawfish')

pdf('../Figures/MapSpNarrowEOO_181219.pdf')
print(nar)
dev.off()


gre <- 
  ggplot() +
  geom_map(data = SEasia, map = SEasia, 
           aes(map_id = id, x = long, y = lat),
           fill = 'grey90', colour = 'black', size = 0.25) +
  geom_map(data = green, map = SEasia,
           aes(map_id = ISO3, fill = presence), colour = 'black', size = 0.25) +
  scale_fill_manual(values = c('Extinct' = '#c51b7d',
                               'Extant' = '#276419',
                               'Presence Unknown' = '#f1b6da')) +
  coord_map() +
  map_theme() +
  theme(legend.position = c(0.3, 0.14)) +
  annotate('text', x = 40, y = 60, label = 'd) Pristis zijsron',
           size = 7, fontface = 'italic') +
  labs(y = '', x = '', title = 'Green Sawfish')

pdf('../Figures/MapSpGreenEOO_181219.pdf')
print(gre)
dev.off()


dwf <- 
  ggplot() +
  geom_map(data = SEasia, map = SEasia, 
           aes(map_id = id, x = long, y = lat),
           fill = 'grey90', colour = 'black', size = 0.25) +
  geom_map(data = dwarf, map = SEasia,
           aes(map_id = ISO3, fill = presence), colour = 'black', size = 0.25) +
  scale_fill_manual(values = c('Extinct' = '#762a83',
                               'Extant' = '#1b7837',
                               'Presence Unknown' = '#c2a5cf')) +
  coord_map() +
  map_theme() +
  theme(legend.position = c(0.3, 0.14)) +
  annotate('text', x = 45, y = 60, label = 'b) Pristis clavata',
           size = 7, fontface = 'italic') +
  labs(y = '', x = '', title = 'Dwarf Sawfish')

pdf('../Figures/MapSpDwarfEOO_181219.pdf')
print(dwf)
dev.off()


#  -----------------------------------------------------------------
#  Map of predictions ----------------------------------------------
#  -----------------------------------------------------------------
library(tidyverse)
library(maptools)
library(broom)
library(RColorBrewer)

rawpred <- read_csv('GBMPredicted_181214.csv')

pred <- 
  rawpred %>% 
  dplyr::select(-X1, -run) %>% 
  dplyr::rename('probOcc' = '.') %>% 
  group_by(ISO3) %>% 
  summarise(totalprob = mean(probOcc)) %>% 
  mutate(Probability = cut(totalprob, breaks = c(0, 0.2, 0.4, 0.6, 0.8, 1),
                        labels = c('<0.2', '0.2-0.4', '0.4-0.6', '0.6-0.8', '>0.8')))
  
data(wrld_simpl)
wrld <- broom::tidy(wrld_simpl, region = 'ISO3')

wrld <- 
  wrld %>% 
  filter(id != 'ATA')

predmap <- 
  ggplot() +
  geom_map(data = wrld, map = wrld,
           aes(map_id = id, x = long, y = lat),
           fill = 'grey90', colour = 'black', size = 0.25) +
  geom_map(data = pred, map = wrld, 
           aes(map_id = ISO3, fill = Probability), colour = 'black', size = 0.25) +
  scale_fill_manual(values = c('<0.2' = '#67001f',
                               '0.2-0.4' = '#d6604d',
                               '0.4-0.6' = '#f4a582',
                               '0.6-0.8' = '#4393c3',
                               '>0.8' = '#053061')) +
  coord_map() +
  theme(plot.background = element_rect(fill = 'transparent', colour = NA),
        panel.background = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        legend.text = element_text(size = 10),
        legend.title = element_text(size = 11), 
        legend.spacing.x = unit(0.2, 'cm'),
        legend.background = element_rect(fill = 'transparent', colour = 'transparent'),
        legend.key = element_rect(fill = 'transparent', colour = 'transparent'),
        legend.position = c(0.1, 0.3)) +
  annotate('text', x = -70, y = 60, label = 'f) Predicted Occurrence', size = 7) +
  labs(y = '', x = '', title = 'Predicted presence')

pdf('../Figures/MapPredicted_181219.pdf')
print(predmap)
dev.off()


predmap2 <- 
  ggplot() +
  geom_map(data = wrld, map = wrld,
           aes(map_id = id, x = long, y = lat),
           fill = 'grey90', colour = 'black', size = 0.25) +
  geom_map(data = pred, map = wrld, 
           aes(map_id = ISO3, fill = Probability), colour = 'black', size = 0.25) +
  scale_fill_manual(values = c('<0.2' = '#40004b',
                               '0.2-0.4' = '#762a83',
                               '0.4-0.6' = '#c2a5cf',
                               '0.6-0.8' = '#35978f',
                               '>0.8' = '#003c30')) +
  coord_map() +
  theme(plot.background = element_rect(fill = 'transparent', colour = NA),
        panel.background = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        legend.text = element_text(size = 13),
        legend.title = element_text(size = 14)) +
  labs(y = '', x = '', title = 'Predicted presence')

pdf('../Figures/MapPredicted2_181219.pdf')
print(predmap2)
dev.off()


#  -----------------------------------------------------------------
#  Maps pacakge in ggplot ------------------------------------------
#  -----------------------------------------------------------------