# THIS SCRIPT ANALYZES THE DATA WITH UNSCALED COVARIATES #
# Cannot calculate scores for unscaled data #

library(tidyverse)
library(broom)
library(reshape2)
library(car)
library(MuMIn)
library(dotwhisker)

setwd('/users/helenyan/desktop/school/directed studies 2018/datasets')

options(na.action = na.fail)

# make presentation theme for all figures ------------------------
presentation_theme <- function(axis_text_size = 13, axis_y_ticks = element_line()) {
  theme(panel.background = element_blank(),
        axis.line = element_line(colour = 'black'),
        axis.text.y = element_text(size = axis_text_size, colour = 'black'),
        axis.text.x = element_text(colour = 'black'),
        axis.ticks.y = axis_y_ticks,
        axis.title = element_text(size = 14))
}

# Models using species as a factor  --------------------------------------

rawspp <- read_csv('CompleteSpeciesCovariates_181109.csv')

spp <-
  rawspp %>%
  select(1:26) %>%
  filter(occurrence != '2') %>%
  select(-totalGearValue) %>%
  drop_na()

# maximum model --------------------------------------

vars <- names(spp)
covsMax <- vars[8:25]
covs1 <- vars[c(8:9, 11:12, 14:17, 20:25)]

formulaMax <- paste('occurrence ~ ', paste(covs, collapse = ' + '))
formula1 <- paste('occurrence ~ ', paste(covs1, collapse = ' + '))

# largetooth sawfish max model ---------------------------------

large <-
  spp %>%
  filter(species == 'large') 

maxLarge <- glm(formulaMax, family = binomial, large)
summary(maxLarge)

# dredge the model after dropping certain indices -------

modLarge1 <- glm(formula1, family = binomial, large)
summary(modLarge1)

largeDredge <-
  dredge(modLarge1)

largeDredgeDf <-
  largeDredge %>%
  as.tibble(.) %>%
  filter(delta <= 2)


#---------------------------------------------------------------------------------
# Analyze without using species as a factor --------------------------------------
#---------------------------------------------------------------------------------
raw <- read_csv('CountryCovariates_181109.csv')

nosp <- 
  raw %>%
  .[, c(1:7, 9:25)] %>%
  drop_na()

# correlation heatmap of variables

cormatdat <-
  nosp %>%
  .[, c(6:24)]

cormat <- round(cor(cormatdat), 2)

# reorder the cormat according to magnitude of correlation
reorderCormat <- function(cormat) {
  # use correlation between variables as distance
  dd <- as.dist((1-cormat)/2)
  hc <- hclust(dd)
  cormat <- cormat[hc$order, hc$order]
}

cormat <- reorderCormat(cormat)

# get rid of half the info
GetLowerTri <- function(cormat) {
  cormat[upper.tri(cormat)] <- NA
  return(cormat)
}

lowerTri <- GetLowerTri(cormat)


melt(lowerTri, na.rm = TRUE) %>%
  mutate(Var2 = dplyr::recode(Var2, 'EstDis' = 'Estuaries Discharge (ESD)',
                              'PprodMean' = 'Primary Productivity (PPD)',
                              'HkExp' = 'Hong Kong Exports (HKE)',
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
                              'Saltmarsh' = 'Saltmarsh Area (SMA)',
                              'ReefFishers' = 'Reef Fishers (RFF)')) %>%
  mutate(Var1 = dplyr::recode(Var1, 'EstDis' = 'ESD',
                              'PprodMean' = 'PPD',
                              'HkExp' = 'HKE',
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
                              'Saltmarsh' = 'SMA',
                              'ReefFishers' = 'RFF')) %>%
  mutate(Var1 = as.factor(Var1)) %>%
  mutate(Var1 = factor(Var1, levels = rev(levels(Var1)))) %>%
  ggplot(., aes(x = Var1, y = Var2, fill = value)) +
  geom_tile(colour = 'white') +
  scale_fill_gradient2(high = 'red', low = 'blue', mid = 'white',
                       midpoint = 0, limit = c(-1, 1), space = 'Lab',
                       name = 'Pearson\nCorrelation') +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1,
                                   size = 12, hjust = 1)) +
  coord_fixed() +
  geom_text(aes(x = Var1, y = Var2, label = value), colour = 'black', size = 3) +
  theme(legend.justification = c(1, 0),
  legend.position = c(0.95, 0.85),
  legend.direction = 'horizontal') +
  guides(fill = guide_colourbar(barwidth = 7, barheight = 1,
                                title.position = 'top', title.hjust = 0.5)) +
  labs(title = 'Correlation Heatmap with Raw Covariates', y = ' ', x = ' ')


# maximum model -------------------------------------------------------

maxAll <- glm(formulaMax, family = binomial, nosp)
summary(maxAll)
















