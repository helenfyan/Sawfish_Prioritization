# This script makes maps 
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