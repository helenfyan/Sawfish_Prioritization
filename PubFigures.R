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

#  -----------------------------------------------------------------
# Collinearity heatmap plot ----------------------------------------
#  -----------------------------------------------------------------

library(reshape2)

raw <- read_csv('CompleteSpeciesCovariates_181119.csv')

# need scaled of FinUSD, ChondLand, FishProd, totalGearTonnes, PprodMean,
# CoastPop, CoastLength, EstDis, ProteinDiet, GDP, Iuu, and Mang

dat <-
  raw %>%
  select(2, 8:26) %>%
  distinct(ISO3, .keep_all = TRUE) %>%
  select(-ISO3) %>%
  # log+1 transform 
  mutate_at(vars('FinUSD', 'ChondLand', 'FishProd', 'totalGearTonnes', 'PprodMean',
                 'CoastPop', 'CoastLength', 'EstDis', 'ProteinDiet', 'GDP', 
                 'Iuu', 'Mang'), log1p)

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
  mutate(Var2 = dplyr::recode(Var2, 'EstDis' = 'Estuaries Discharge (ESD)',
                              'PprodMean' = 'Primary Productivity (PPD)',
                              'FinUSD' = 'Fin Exports (FXP)',
                              'CoastPop' = 'Coastal Population (CTP)',
                              'ChondLand' = 'Chondrichthyes Landings (CHL)',
                              'SstMean' = 'Sea Surface Temperature (°C; SST)',
                              'Mang' = 'Mangrove Area (MNG)',
                              'HDI' = 'Human Development Index (HDI)',
                              'EPI' = 'Environmental Performance Index (EPI)',
                              'WGI' = 'World Governance Index (WGI)',
                              'OHI' = 'Ocean Health Index (OHI)',
                              'NBI' = 'National Biodiversity Index (NBI)',
                              'Iuu' = 'Illegal Unreported and Unregulated Fishing (IUU)',
                              'GDP' = 'Gross Domestic Product (GDP)',
                              'CoastLength' = 'Coastline length (km; CLL)',
                              'ProteinDiet' = 'Marine Protein Supply (MPS)',
                              'totalGearTonnes' = 'Fishing Gear Landed Tonnes (FGT)',
                              'ReefFishers' = 'Reef Fishers (RFF)',
                              'FishProd' = 'Marine Fisheries Production (MFP)')) %>%
  mutate(Var1 = dplyr::recode(Var1, 'EstDis' = 'ESD',
                              'PprodMean' = 'PPD',
                              'FinUSD' = 'FXP',
                              'CoastPop' = 'CTP',
                              'ChondLand' = 'CHL',
                              'SstMean' = 'SST',
                              'Mang' = 'MNG',
                              'HDI' = 'HDI',
                              'EPI' = 'EPI',
                              'WGI' = 'WGI',
                              'OHI' = 'OHI',
                              'NBI' = 'NBI',
                              'Iuu' = 'IUU',
                              'GDP' = 'GDP',
                              'CoastLength' = 'CLL',
                              'ProteinDiet' = 'MPS',
                              'totalGearTonnes' = 'FGT',
                              'ReefFishers' = 'RFF',
                              'FishProd' = 'MFP')) %>%
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
