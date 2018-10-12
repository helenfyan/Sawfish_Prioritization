# THIS SCRIPT GOES THROUGH THE DIFFERENT COVARIATE COMBINATIONS #

library(tidyverse)
library(broom)
library(reshape2)
library(car)

setwd('/users/helenyan/desktop/school/directed studies 2018/datasets')

allData <- read_csv('CompleteSpeciesCovariates_181012.csv')

allScaled <-
  allData %>%
  select(-NBI, -coastal.pop, -Lengthkm, -EPI, -Discharge, -ExporttoHK2010,
         -MeanPro, -GDP, -HDI, -OHI, -UnreportedPercent, -impact.percentage, 
         -no_reef_fishers, -AreaSqKM, -TotalTonnes, -WGI, -SstMean,
         -SstMax, -SstMin, -PprodMean, -PprodMax, -PprodMin) %>%
  filter(presence != 'unknown') %>%
  # remove saltmarsh and reef fishers because there are too many gaps in the data
  select(-SaltmarshScale, -ReefFisherScale)


# Maximum model for each species after dropping the covariates ---------------------
# If there are no interaction terms, the maximum model is strictly additive

modData <-
  allScaled %>%
  select(-HdiScale, -OhiScale, -EpiScale, -NbiScale, -SstMaxScale,
         -SstMinScale, -PprodMaxScale, -PprodMinScale)

vars <- names(modData)
covs <- vars[8:19]

formula <- paste('occurrence ~ ', paste(covs, collapse = ' + '))


# largetooth sawfish ---------------------------------

modLarge <-
  modData %>%
  filter(species == 'large') %>%
  distinct(., country, .keep_all = TRUE)

modLarge1 <- glm(formula, family = binomial, modLarge)
summary(modLarge1)

# dwarf sawfish ---------------------------------

modDwarf <- 
  modData %>%
  filter(species == 'dwarf')

View(modDwarf)

modDwarf1 <- glm(formula, family = binomial, modDwarf)
summary(modDwarf1)

# green sawfish ---------------------------------

modGreen <-
  modData %>%
  filter(species == 'green')

modGreen1 <- glm(formula, family = binomial, modGreen)
summary(modGreen1)

# narrow sawfish ---------------------------------

modNarrow <-
  modData %>%
  filter(species == 'narrow')

modNarrow1 <- glm(formula, family = binomial, modNarrow)
summary(modNarrow1)

# smalltooth sawfish ---------------------------------

modSmall <-
  modData %>%
  filter(species == 'small')

modSmall1 <- glm(formula, family = binomial, modSmall)
summary(modSmall1)
# NAs in model outputs due to collinearity

# -----------------------------------------------------------------------------------
# Test for collinearity in covariates -----------------------------------------------

# Make a heatmap of the pairwise variable comparisons (have not dropped any covariates)
cormatdat <- 
  allScaled %>%
  .[, c(8, 11:25)] %>%
  drop_na()

cormat <- round(cor(cormatdat), 2)

# reorder the cormat according to correlational coefficient
reorderCormat <- function(cormat) {
  # use correlatio between variables as distance
  dd <- as.dist((1-cormat)/2)
  hc <- hclust(dd)
  cormat <- cormat[hc$order, hc$order]
}

cormat <- reorderCormat(cormat)

# get rid of half the info - redundant
GetLowerTri <- 
  function(cormat) {
    cormat[upper.tri(cormat)] <- NA
    return(cormat)
  }

lowerTri <- GetLowerTri(cormat)

meltedlowertri <- 
  melt(lowerTri, na.rm = TRUE) %>%
  mutate(Var2 = dplyr::recode(Var2, 'EstDisScale' = 'Estuaries Discharge (ESD)',
                              'PprodMeanScale' = 'Primary Productivity (PPD)',
                              'HkExpScale' = 'Hong Kong Exports (HKE)',
                              'CoastPopScale' = 'Coastal Population (CTP)',
                              'ChondLandScale' = 'Chondrichthyes Landings (CHL)',
                              'SstMeanScale' = 'Sea Surface Temperature (C; SST)',
                              'ManImpScale' = 'Mangrove Loss (MGL)',
                              'HdiScale' = 'Human Development Index (HDI)',
                              'EpiScale' = 'Environmental Performance Index (EPI)',
                              'WgiScale' = 'World Governance Index (WGI)',
                              'OhiScale' = 'Ocean Health Index (OHI)',
                              'NbiScale' = 'National Biodiversity Index (NBI)',
                              'IuuScale' = 'Illegal Unreported and Unregulated Fishing (IUU)',
                              'GdpScale' = 'Gross Domestic Product (GDP)',
                              'CoastLengthScale' = 'Coastline length (km; CLL)',
                              'ProteinSupScale' = 'Marine Product Consumption (MPC)')) %>%
  mutate(Var1 = dplyr::recode(Var1, 'EstDisScale' = 'ESD',
                              'PprodMeanScale' = 'PPD',
                              'HkExpScale' = 'HKE',
                              'CoastPopScale' = 'CTP',
                              'ChondLandScale' = 'CHL',
                              'SstMeanScale' = 'SST',
                              'ManImpScale' = 'MGL',
                              'HdiScale' = 'HDI',
                              'EpiScale' = 'EPI',
                              'WgiScale' = 'EGI',
                              'OhiScale' = 'OHI',
                              'NbiScale' = 'NBI',
                              'IuuScale' = 'IUU',
                              'GdpScale' = 'GDP',
                              'CoastLengthScale' = 'CLL',
                              'ProteinSupScale' = 'MPC'))


lowertriplot <- 
  ggplot(meltedlowertri, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile(colour = 'white') +
  scale_fill_gradient2(low = 'blue', high = 'red', mid = 'white',
                       midpoint = 0, limit = c(-1, 1), space = 'Lab',
                       name = 'Pearson\nCorrelation') +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1,
                                   size = 12, hjust = 1)) +
  coord_fixed() +
  geom_text(aes(x = Var1, y = Var2, label = value), colour = 'black', size = 3) +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.major = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank(),
    legend.justification = c(1, 0),
    legend.position = c(0.3, 0.85),
    legend.direction = 'horizontal') +
  guides(fill = guide_colourbar(barwidth = 7, barheight = 1,
                                title.position = 'top', title.hjust = 0.5))

print(lowertriplot)


# Test for collinearity after removing the covariates based on PCA ------------------

cormatdat2 <-
  allScaled %>%
  select(CoastPopScale, CoastLengthScale, EstDisScale, HkExpScale, ProteinSupScale,
         GdpScale, IuuScale, ManImpScale, ChondLandScale, WgiScale, SstMeanScale,
         PprodMeanScale) %>%
  drop_na()

cormat2 <- round(cor(cormatdat2), 2)
cormat2 <- reorderCormat(cormat2)

lowerTri2 <- GetLowerTri(cormat2)
meltedlowertri2 <- melt(lowerTri2, na.rm = TRUE)

lowertriplot2 <-
  ggplot(meltedlowertri2, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile(colour = 'white') +
  scale_fill_gradient2(low = 'blue', high = 'red', mid = 'white',
                       midpoint = 0, limit = c(-1, 1), space = 'Lab',
                       name = 'Person\nCorrelation') +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, size = 12, hjust = 1)) +
  coord_fixed() +
  geom_text(aes(x = Var1, y = Var2, label = value), colour = 'black', size = 3) +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.major = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank(),
    legend.justification = c(1, 0),
    legend.position = c(0.3, 0.85),
    legend.direction = 'horizontal') +
  guides(fill = guide_colourbar(barwidth = 7, barheight = 1,
                                title.position = 'top', title.hjust = 0.5))

print(lowertriplot2)
  

# -----------------------------------------------------------------------------------
# Maximum model for each species after removing collinear covariates
# removed: SSTmin, Coastline length, and HkExp by setting r2 = 0.6 threshold 

covs2 <- vars[c(8, 9, 11, 12:18)]
formula2 <- paste('occurrence ~ ', paste(covs2, collapse = ' + '))

modLarge2 <- glm(formula2, family = binomial, modLarge)
summary(modLarge2)

modSmall2 <- glm(formula2, family = binomial, modSmall)
summary(modSmall2)

modGreen2 <- glm(formula2, family = binomial, modGreen)
summary(modGreen2)

modNarrow2 <- glm(formula2, family = binomial, modNarrow)
summary(modNarrow2)

modDwarf2 <- glm(formula2, family = binomial, modDwarf)
summary(modDwarf2)


# -----------------------------------------------------------------------------------
# Make broad categories (economics, government, and biological) and calculate the max
# model for each category
# Keep all covariates but drop HDI, OHI, EPI, NBI, and saltmarsh
# Drop HK exports because there are too many NAs

scoresData <-
  modData %>%
  # drop SSTmin 
  select(-HkExpScale) %>%
  
  mutate(BioScore = rowMeans(data.frame(CoastLengthScale, EstDisScale, 
                                        ManImpScale, SstMeanScale,
                                        PprodMeanScale))) %>%
  mutate(GovScore = rowMeans(data.frame(WgiScale, IuuScale))) %>%
  mutate(EconScore = rowMeans(data.frame(CoastPopScale, ProteinSupScale,
                                         GdpScale, ChondLandScale)))


# largetooth sawfish ----------------------------
scoresLarge <-
  scoresData %>%
  filter(species == 'large')

modLarge3 <- glm(occurrence ~ BioScore + GovScore + EconScore, family = binomial,
                 scoresLarge)
summary(modLarge3)


# smalltooth sawfish ----------------------------
scoresSmall <-
  scoresData %>%
  filter(species == 'small')

modSmall3 <- glm(occurrence ~ BioScore + GovScore + EconScore, family = binomial,
                 scoresSmall)
summary(modSmall3)


# green sawfish ----------------------------
scoresGreen <-
  scoresData %>%
  filter(species == 'green')

modGreen3 <- glm(occurrence ~ BioScore + GovScore + EconScore, family = binomial,
                 scoresGreen)
summary(modGreen3)


# narrow sawfish ----------------------------
scoresNarrow <-
  scoresData %>%
  filter(species == 'narrow')

modNarrow3 <- glm(occurrence ~ BioScore + GovScore + EconScore, family = binomial,
                 scoresNarrow)
summary(modNarrow3)


# dwarf sawfish ----------------------------
scoresDwarf <-
  scoresData %>%
  filter(species == 'dwarf')

modDwarf3 <- glm(occurrence ~ BioScore + GovScore + EconScore, family = binomial,
                 scoresDwarf)
summary(modDwarf3)



# For all species try every combination of covariates -------------------------------
# Dredge the models to produce only the ones with 

# try out every combination of covariates
out <- unlist(lapply(1:9, function(n) {
  # get combinations
  combinations <- t(combn(covs2, n))
  # collapse them into usable formulas
  formulas <- apply(combinations, 1, function(row) paste('occurrence ~ ', 
                                                         paste(row, 
                                                               collapse = ' + ')))
}))

test <- 
  bind_rows(lapply(out, function(frml) {
    a = glance(glm(frml, family = binomial, large))
    a$frml <- frml
    return(a)
  }))

# don't run this - it freezes RStudio