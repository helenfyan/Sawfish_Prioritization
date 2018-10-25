# THIS SCRIPT ANALYZES THE DATA WITH SCALED VALUES #

library(tidyverse)
library(broom)
library(reshape2)
library(car)
library(MuMIn)
library(dotwhisker)

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

# Presentation theme for all plots --------------

presentation_theme <- function(axis_text_size = 13, axis_y_ticks = element_line()) {
  theme(panel.background = element_blank(),
        panel.grid = element_blank(),
        axis.line = element_line(colour = 'black'),
        axis.text.y = element_text(size = axis_text_size, colour = 'black'),
        axis.text.x = element_text(colour = 'black'),
        axis.ticks.y = axis_y_ticks,
        axis.title = element_text(size = 14))
}


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

covs2 <- vars[c(8, 9, 11, 13:19)]
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


# need to write down the covariates that I'm keeping in the model
# this is as a result of either the PCA or heatmap (with a r2 = 0.6 threshold)

# Global models for individual species ---------------------------------------------
# REDO THIS BECAUSE HK EXP WERE REMOVED

# Largetooth sawfish ------------------------------------
options(na.action = na.fail)

gloLargedf <-
  modLarge %>%
  drop_na() 

gloLarge <- glm(formula2, family = binomial, gloLargedf)

gloModLarge <- 
  dredge(gloLarge) %>%
  as_tibble(.) %>%
  filter(delta <= 2) %>%
  mutate(species = 'largetooth')

write_csv(gloModLarge, 'DredgeLarge_181018.csv')

gloModLarge <- glm(formula, family = binomial, gloLargedf)

gloLargesel <-
  dredge(gloModLarge)

# Get relative importance for largetooth ------------------------

RIlargeExpHk <-
  gloLargesel %>%
  as_tibble(.) %>%
  drop_na(HkExpScale) %>%
  summarise(RIexp = sum(weight))

RIlargeGdp <-
  gloLargesel %>%
  as_tibble(.) %>%
  drop_na(GdpScale) %>%
  summarise(RIGdp = sum(weight))

RIlargeConsumpt <-
  gloLargesel %>%
  as_tibble(.) %>%
  drop_na(ProteinSupScale) %>%
  summarise(RIconsump = sum(weight))


LargeTop <-
  get.models(gloLargesel, subset = delta <= 2)

# Get relative importance for largetooth ------------------------

largeCoefPlot <-
  dwplot(list(LargeTop[[1]], LargeTop[[2]], LargeTop[[3]]),
         vline = geom_vline(xintercept = 0, colour = 'black', linetype = 1),
         dot_args = list(size = 4),
         whisker_args = list(size = 1)) %>%
  relabel_predictors(c(HkExpScale = 'Exports to HK',
                       GdpScale = 'GDP',
                       ProteinSupScale = 'Marine Protein\nConsumption')) +
  scale_colour_manual(values = c('cadetblue4', 'cadetblue3', 'cadetblue1')) +
  expand_limits(x = 5) +
  theme(legend.key = element_blank(),
        legend.background = element_blank(),
        legend.text = element_text(size = 13),
        legend.position = c(0.07, 0.1)) +
  presentation_theme() +
  scale_x_continuous(breaks = c(-10:3)) +
  guides(colour = guide_legend(override.aes = list(size = 5))) +
  labs(x = '\nCoefficient Estimate (SD units)', colour = ' ',
       title = 'Top Models with 95% CI - Pristis pristis') +
  annotate('text', x = 4.2, y = 3.4, label = 'Relative\nimportance', size = 5) +
  annotate('text', x = 4.2, y = c(2.9, 2, 1.15), label = c('0.41', '0.49', '0.44'),
           size = 6)

print(largeCoefPlot)

# Smalltooth sawfish ------------------------------------
gloSmalldf <-
  modSmall %>%
  select(-HkExpScale) %>%
  drop_na()

gloSmall <- glm(formula2, family = binomial, gloSmalldf)

gloModSmall <-
  dredge(gloSmall) %>%
  as_tibble(.) %>%
  filter(delta <= 2) %>%
  mutate(species = 'smalltooth')

write_csv(gloModSmall, 'DredgeSmall_181018.csv')

# Narrow sawfish ------------------------------------
gloNarrowdf <-
  modNarrow %>%
  select(-HkExpScale) %>%
  drop_na()

gloNarrow <- glm(formula2, family = binomial, gloNarrowdf)

gloModNarrow <-
  dredge(gloNarrow) %>%
  as_tibble(.) %>%
  filter(delta <= 2) %>%
  mutate(species = 'narrow')

write_csv(gloModNarrow, 'DredgeNarrow_181018.csv')

# Green sawfish ------------------------------------
gloGreendf <-
  modGreen %>%
  select(-HkExpScale) %>%
  drop_na()

gloGreen <- glm(formula2, family = binomial, gloGreendf)

gloModGreen <-
  dredge(gloGreen) %>%
  as_tibble(.) %>%
  filter(delta <= 2) %>%
  mutate(species = 'green')

write_csv(gloModGreen, 'DredgeGreen_181018.csv')

# Dwarf sawfish ------------------------------------
gloDwarfdf <-
  modDwarf %>%
  select(-HkExpScale) %>%
  drop_na()

gloDwarf <- glm(formula2, family = binomial, gloDwarfdf)

gloModDwarf <-
  dredge(gloDwarf) %>%
  as_tibble(.) %>%
  filter(delta <= 2) %>%
  mutate(species = 'dwarf')

write_csv(gloModDwarf, 'DredgeDwarf_181018.csv')

# All species combined -------------------------------------------

NoSpp1 <- read_csv('CountryCovariates_181018.csv')

# number of NAs per column
sapply(NoSpp1, function(x) sum(is.na(x)))

NoSppAll <-
  NoSpp1 %>%
  select(-HdiScale, -OhiScale, -EpiScale, -NbiScale, -SaltmarshScale, -ReefFisherScale,
         -PprodMinScale, -PprodMaxScale) %>%
  drop_na()

varsNoSpp <- names(NoSppAll)
covsNoSppAll <- varsNoSpp[6:17]

formulaNoSppAll <- paste('occurrence ~ ', paste(covsNoSppAll, collapse = ' + '))

gloNoSppAll <- glm(formulaNoSppAll, family = binomial, NoSppAll)

gloModNoSppAll <- 
  dredge(gloNoSppAll)

# convert gloModNoSppAll to tibble to write csv
#write_csv(gloModNoSppAll, '../Results/DredgeAll_181018.csv')

# Create coefficient plot 
# Get relative importance = sum of weight of all models containing given variable

RIexpHk <- 
  gloModNoSppAll %>%
  as_tibble(.) %>%
  drop_na(HkExpScale) %>%
  summarize(RIHkExp = sum(weight))

RIManImp <-
  gloModNoSppAll %>%
  as_tibble(.) %>%
  drop_na(ManImpScale) %>%
  summarize(RIManImp = sum(weight))

RIWgi <-
  gloModNoSppAll %>%
  as_tibble(.) %>%
  drop_na(WgiScale) %>%
  summarize(RIWgi = sum(weight))

modNoSppsel <- 
  get.models(gloModNoSppAll, subset = delta <= 2)

NoSppCoefPlot <-
  dwplot(list(modNoSppsel[[1]], modNoSppsel[[2]], modNoSppsel[[3]]),
         vline = geom_vline(xintercept = 0, colour = 'black', linetype = 1),
         dot_args = list(size = 4),
         whisker_args = list(size = 1)) %>%
  relabel_predictors(c(HkExpScale = 'Exports to HK',
                       ManImpScale = 'Mangroves',
                       WgiScale = 'WGI')) +
  expand_limits(x = 6) +
  scale_colour_manual(values = c('darkslategray', 'darkslategray4', 'darkslategray3')) +
  theme(legend.key = element_blank(),
        legend.position = c(0.07, 0.1),
        legend.background = element_blank(),
        legend.text = element_text(size = 13)) +
  presentation_theme() +
  scale_x_continuous(breaks = c(-10:4)) +
  guides(colour = guide_legend(override.aes = list(size = 5))) +
  labs(x = '\nCoefficient Estimate (SD units)', colour = ' ', 
       title = 'Top Models with 95% CI - All Species') +
  annotate('text', x = 5.2, y = 3.4, label = 'Relative\nimportance', size = 5) +
  annotate('text', x = 5.2, y = c(2.9, 2, 1.15), label = c('0.47', '0.42', '0.51'),
           size = 6)

print(NoSppCoefPlot)


# --------------------------------------------------------------------------------
# Figures with best models -------------------------------------------------------
# --------------------------------------------------------------------------------


# Top 3 models for all species 

SppPlot1 <-
  ggplot(NoSppAll, aes(x = HkExpScale, y = occurrence)) +
  geom_point(size = 3, shape = '|', colour = 'grey30') +
  geom_smooth(method = 'glm', method.args = list(family = 'binomial')) +
  presentation_theme()

print(SppPlot1)


SppPlot2 <-
  ggplot(NoSppAll, aes(x = WgiScale, y = occurrence)) +
  geom_point(size = 3, shape = '|', colour = 'grey30') +
  geom_smooth(method = 'glm', method.args = list(family = 'binomial')) +
  presentation_theme()

print(SppPlot2)









