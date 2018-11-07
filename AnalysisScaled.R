# THIS SCRIPT ANALYZES THE DATA WITH SCALED VALUES #

library(tidyverse)
library(broom)
library(reshape2)
library(car)
library(MuMIn)
library(dotwhisker)
library(GGally)

setwd('/users/helenyan/desktop/school/directed studies 2018/datasets')

allData <- read_csv('CompleteSpeciesCovariates_181106.csv')

allScaled <-
  allData %>%
  select(-NBI, -CoastPop, -CoastLength, -EPI, -EstDis, -HkExp,
         -ProteinSup, -GDP, -HDI, -OHI, -Iuu, -ManImp, 
         -ReefFishers, -Saltmarsh, -ChondLand, -WGI, -SstMean,
         -SstMax, -SstMin, -PprodMean, -PprodMax, -PprodMin, -totalGearTonnes,
         -totalGearValue) %>%
  dplyr::filter(presence != 'unknown') %>%
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
modData <-
  allScaled %>%
  select(-HdiScale, -OhiScale, -EpiScale, -NbiScale, -SstMaxScale,
         -SstMinScale, -PprodMaxScale, -PprodMinScale, -totalGearValScale)

vars <- names(modData)
covs <- vars[8:20]

formula <- paste('occurrence ~ ', paste(covs, collapse = ' + '))


# largetooth sawfish ---------------------------------

modLarge <-
  modData %>%
  dplyr::filter(species == 'large') %>%
  distinct(., country, .keep_all = TRUE) %>%
  drop_na()

modLarge1 <- glm(formula, family = binomial, modLarge)
summary(modLarge1)

# -----------------------------------------------------------------------------------
# Test for collinearity in covariates -----------------------------------------------

collmatdf <-
  allScaled %>%
  distinct(country, .keep_all = TRUE)

# Make a heatmap of the pairwise variable comparisons (have not dropped any covariates)
cormatdat <- 
  collmatdf %>%
  .[, c(8:10, 13:27)] %>%
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

lowertriplot <- 
  melt(lowerTri, na.rm = TRUE) %>%
  mutate(Var2 = dplyr::recode(Var2, 'EstDisScale' = 'Estuaries Discharge (ESD)',
                              'PprodMeanScale' = 'Primary Productivity (PPD)',
                              'HkExpScale' = 'Hong Kong Exports (HKE)',
                              'CoastPopScale' = 'Coastal Population (CTP)',
                              'ChondLandScale' = 'Chondrichthyes Landings (CHL)',
                              'SstMeanScale' = 'Sea Surface Temperature (°C; SST)',
                              'ManImpScale' = 'Mangrove Loss (MGL)',
                              'HdiScale' = 'Human Development Index (HDI)',
                              'EpiScale' = 'Environmental Performance Index (EPI)',
                              'WgiScale' = 'World Governance Index (WGI)',
                              'OhiScale' = 'Ocean Health Index (OHI)',
                              'NbiScale' = 'National Biodiversity Index (NBI)',
                              'IuuScale' = 'Illegal Unreported and Unregulated Fishing (IUU)',
                              'GdpScale' = 'Gross Domestic Product (GDP)',
                              'CoastLengthScale' = 'Coastline length (km; CLL)',
                              'ProteinSupScale' = 'Marine Protein Consumption (MPC)',
                              'totalGearTonScale' = 'Fishing Gear Landed Tonnes (FGT)',
                              'totalGearValScale' = 'Fishing Gear Landed Value (FGV)')) %>%
  mutate(Var1 = dplyr::recode(Var1, 'EstDisScale' = 'ESD',
                              'PprodMeanScale' = 'PPD',
                              'HkExpScale' = 'HKE',
                              'CoastPopScale' = 'CTP',
                              'ChondLandScale' = 'CHL',
                              'SstMeanScale' = 'SST',
                              'ManImpScale' = 'MGL',
                              'HdiScale' = 'HDI',
                              'EpiScale' = 'EPI',
                              'WgiScale' = 'WGI',
                              'OhiScale' = 'OHI',
                              'NbiScale' = 'NBI',
                              'IuuScale' = 'IUU',
                              'GdpScale' = 'GDP',
                              'CoastLengthScale' = 'CLL',
                              'ProteinSupScale' = 'MPC',
                              'totalGearTonScale' = 'FGT',
                              'totalGearValScale' = 'FGV')) %>%
  ggplot(., aes(x = Var1, y = Var2, fill = value)) +
  geom_tile(colour = 'white') +
  scale_fill_gradient2(high = 'blue', low = 'red', mid = 'white',
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
                                title.position = 'top', title.hjust = 0.5)) +
  ggtitle('Correlation Heatmap with Non-Reversed Covariates')

print(lowertriplot)


# Test for collinearity after removing the covariates based on PCA ------------------

cormatdat2 <-
  collmatdf %>%
  select(CoastPopScale, CoastLengthScale, EstDisScale, HkExpScale, ProteinSupScale,
         GdpScale, IuuScale, ManImpScale, ChondLandScale, WgiScale, SstMeanScale,
         PprodMeanScale, totalGearTonScale) %>%
  drop_na()

cormat2 <- round(cor(cormatdat2), 2)
cormat2 <- reorderCormat(cormat2)

lowerTri2 <- GetLowerTri(cormat2)


lowertriplot2 <- 
  melt(lowerTri2, na.rm = TRUE) %>%
  mutate(Var2 = dplyr::recode(Var2, 'ProteinSupScale' = 'Marine Protein Consumption (MPC)',
                              'IuuScale' = 'Illegal Unreported and Unregulated Fishing (IUU)',
                              'WgiScale' = 'World Governance Index (WGI)',
                              'CoastLengthScale' = 'Coastline length (km; CLL)',
                              'GdpScale' = 'Gross Domestic Product (GDP)',
                              'ManImpScale' = 'Mangrove Loss (MGL)',
                              'SstMeanScale' = 'Sea Surface Temperature (°C; SST)',
                              'ChondLandScale' = 'Chondrichthyes Landings (CHL)',
                              'HkExpScale' = 'Hong Kong Exports (HKE)',
                              'EstDisScale' = 'Estuaries Discharge (ESD)',
                              'PprodMeanScale' = 'Primary Productivity (PPD)',
                              'CoastPopScale' = 'Coastal Population (CTP)',
                              'totalGearTonScale' = 'Fishing Gear Landed Tonnes (FGT)')) %>%
  mutate(Var1 = dplyr::recode(Var1, 'EstDisScale' = 'ESD',
                              'PprodMeanScale' = 'PPD',
                              'HkExpScale' = 'HKE',
                              'CoastPopScale' = 'CTP',
                              'ChondLandScale' = 'CHL',
                              'SstMeanScale' = 'SST',
                              'ManImpScale' = 'MGL',
                              'WgiScale' = 'WGI',
                              'IuuScale' = 'IUU',
                              'GdpScale' = 'GDP',
                              'CoastLengthScale' = 'CLL',
                              'ProteinSupScale' = 'MPC',
                              'totalGearTonScale' = 'FGT')) %>%
  ggplot(., aes(x = Var1, y = Var2, fill = value)) +
  geom_tile(colour = 'white') +
  scale_fill_gradient2(low = 'blue', high = 'red', mid = 'white',
                       midpoint = 0, limit = c(-1, 1), space = 'Lab',
                       name = 'Pearson\nCorrelation') +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, size = 12, hjust = 1)) +
  coord_fixed() +
  geom_text(aes(x = Var1, y = Var2, label = value), colour = 'black', size = 3) +
  presentation_theme() +
  theme(legend.justification = c(1, 0),
    legend.position = c(0.3, 0.85),
    legend.direction = 'horizontal',
    axis.title = element_blank()) +
  guides(fill = guide_colourbar(barwidth = 7, barheight = 1,
                                title.position = 'top', title.hjust = 0.5))

print(lowertriplot2)


# -----------------------------------------------------------------------------------
# Maximum model for each species with scores
# Dropped HDI, OHI, EPI, NBI, and saltmarsh

scoresData <-
  modData %>%
  mutate(BioScore = rowMeans(data.frame(CoastLengthScale, EstDisScale, 
                                        ManImpScale, SstMeanScale,
                                        PprodMeanScale))) %>%
  mutate(GovScore = rowMeans(data.frame(WgiScale, IuuScale))) %>%
  mutate(EconScore = rowMeans(data.frame(CoastPopScale, ProteinSupScale,
                                         GdpScale, ChondLandScale, HkExpScale))) %>%
  drop_na()


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


# Dredged models for individual species ---------------------------------------------

#------------------------------------------------------------------------
# Largetooth sawfish ------------------------------------
options(na.action = na.fail)

gloLargedf <-
  modLarge %>%
  drop_na()

gloLarge <- glm(formula, family = binomial, modLarge)

gloModLarge <- 
  dredge(gloLarge) %>%
  as_tibble(.) %>%
  dplyr::filter(delta <= 2) %>%
  mutate(species = 'largetooth')

write_csv(gloModLarge, '../Results/DredgeLarge_181106.csv')

gloLargesel <-
  dredge(gloLarge)

# Get relative importance for largetooth ------------------------

LargeTop <-
  get.models(gloLargesel, subset = delta <= 2)

RIlargeExpHk <-
  gloLargesel %>%
  as_tibble(.) %>%
  drop_na(HkExpScale) %>%
  summarise(RIexp = sum(weight))

RIlargecoastpop <-
  gloLargesel %>%
  as_tibble(.) %>%
  drop_na(CoastPopScale) %>%
  summarise(RIcoastpop = sum(weight))

RIlargefishgear <-
  gloLargesel %>%
  as_tibble(.) %>%
  drop_na(totalGearTonScale) %>%
  summarise(RIgear = sum(weight))


# Create coefficient plot for largetooth ------------------------

largeCoefPlot <-
  dwplot(list(LargeTop[[1]], LargeTop[[2]], LargeTop[[3]]),
         vline = geom_vline(xintercept = 0, colour = 'black', linetype = 1),
         dot_args = list(size = 4),
         whisker_args = list(size = 1)) %>%
  relabel_predictors(c(HkExpScale = 'Exports to HK',
                       CoastPopScale = 'Coastal Population',
                       totalGearTonScale = 'Fishing Gear\nLanded Tonnes')) +
  scale_colour_manual(values = c('cadetblue4', 'cadetblue3', 'cadetblue1')) +
  expand_limits(x = 12) +
  theme(legend.key = element_blank(),
        legend.background = element_blank(),
        legend.text = element_text(size = 13),
        legend.position = c(0.07, 0.97)) +
  presentation_theme() +
  scale_x_continuous(breaks = c(-10:10)) +
  guides(colour = guide_legend(override.aes = list(size = 5))) +
  labs(x = '\nCoefficient Estimate (SD units)', colour = ' ',
       title = 'Top Models with 95% CI - Pristis pristis') +
  annotate('text', x = 11, y = 3.4, label = 'Relative\nimportance', size = 5) +
  annotate('text', x = 11, y = c(3, 1.9, 1), label = c('0.47', '0.48', '0.45'),
           size = 6)

print(largeCoefPlot)

#------------------------------------------------------------------------
# All species combined -------------------------------------------

NoSppRaw <- read_csv('CountryCovariates_181106.csv')

# number of NAs per column
sapply(NoSppRaw, function(x) sum(is.na(x)))

NoSppAll <-
  NoSppRaw %>%
  select(country, ISO3, species, nospecies, occurrence, SstCountMeanScale,
         totalGearTonScale, totalGearValScale, PprodMeanScale, PprodMaxScale,
         PprodMinScale, NbiScale, CoastPopScale, CoastLengthScale, EpiScale,
         EstDisScale, HkExpScale, ProteinSupScale, GdpScale, HdiScale, OhiScale,
         IuuScale, ManImpScale, ReefFisherScale, SaltmarshScale, ChondLandScale, WgiScale) %>%
  select(-HdiScale, -OhiScale, -EpiScale, -NbiScale, -SaltmarshScale, -ReefFisherScale,
         -PprodMinScale, -PprodMaxScale) %>%
  drop_na()

varsNoSpp <- names(NoSppAll)
covsNoSppAll <- varsNoSpp[6:18]

formulaNoSppAll <- paste('occurrence ~ ', paste(covsNoSppAll, collapse = ' + '))

gloNoSppAll <- glm(formulaNoSppAll, family = binomial, NoSppAll)

gloModNoSppAll <- 
  dredge(gloNoSppAll)

modNoSppAlldf <-
  gloModNoSppAll %>%
  as_tibble(.) %>%
  dplyr::filter(delta <= 2)

write_csv(modNoSppAlldf, '../Results/DredgeAll_181106.csv')

# Create coefficient plot 
# Get relative importance = sum of weight of all models containing given variable

modNoSppsel <- 
  get.models(gloModNoSppAll, subset = delta <= 2)

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

RICoastL <-
  gloModNoSppAll %>%
  as_tibble(.) %>%
  drop_na(CoastLengthScale) %>%
  summarize(RICL = sum(weight))


NoSppCoefPlot <-
  dwplot(list(modNoSppsel[[1]], modNoSppsel[[2]], modNoSppsel[[3]]),
         vline = geom_vline(xintercept = 0, colour = 'black', linetype = 1),
         dot_args = list(size = 4),
         whisker_args = list(size = 1)) %>%
  relabel_predictors(c(HkExpScale = 'Exports to HK',
                       ManImpScale = 'Mangroves',
                       CoastLengthScale = 'Coastline Length')) +
  expand_limits(x = 12) +
  scale_colour_manual(values = c('darkslategray', 'darkslategray4', 'darkslategray3')) +
  theme(legend.key = element_blank(),
        legend.position = c(0.07, 0.1),
        legend.background = element_blank(),
        legend.text = element_text(size = 13)) +
  presentation_theme() +
  scale_x_continuous(breaks = c(-10:9)) +
  guides(colour = guide_legend(override.aes = list(size = 9))) +
  labs(x = '\nCoefficient Estimate (SD units)', colour = ' ', 
       title = 'Top Models with 95% CI - All Species') +
  annotate('text', x = 11, y = 3.4, label = 'Relative\nimportance', size = 5) +
  annotate('text', x = 11, y = c(3, 1.95, 1), label = c('0.61', '0.53', '0.39'),
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
  presentation_theme() +
  scale_y_continuous(breaks = c(0, 1)) +
  labs(x = 'Fin Exports to Hong Kong',
       y = 'Occurrence',
       title = 'All Species')

print(SppPlot1)

# Remove outlier that is Indonesia

NoSppAll %>%
  dplyr::filter(country != 'Indonesia') %>%
  ggplot(., aes(x = HkExpScale, y = occurrence)) +
  geom_point(size = 3, shape = '|', colour = 'grey30') +
  geom_smooth(method = 'glm', method.args = list(family = 'binomial')) +
  presentation_theme() +
  scale_y_continuous(breaks = c(0,1)) +
  labs(x = 'Fin Exports to Hong Kong',
       y = 'Occurrence',
       title = 'All Species - Removed Indonesia')


SppPlot2 <-
  ggplot(NoSppAll, aes(x = WgiScale, y = occurrence)) +
  geom_point(size = 3, shape = '|', colour = 'grey30') +
  geom_smooth(method = 'glm', method.args = list(family = 'binomial')) +
  presentation_theme() +
  scale_y_continuous(breaks = c(0, 1)) +
  labs(x = 'World Governance Index', y = 'Occurrence')

print(SppPlot2)

ggplot(NoSppAll, aes(x = ManImpScale, y = occurrence)) +
  geom_point() +
  geom_smooth(method = 'glm', method.args = list(family = 'binomial'))

#------------------------------------------------------------------------
# All species combined - scores -------------------------------------------

NoSppScores <-
  NoSppAll %>%
  mutate(BioScore = rowMeans(data.frame(SstCountMeanScale, PprodMeanScale, 
                                        CoastLengthScale, EstDisScale,
                                        ManImpScale))) %>%
  mutate(EconScore = rowMeans(data.frame(GdpScale, CoastPopScale,
                                         HkExpScale, ProteinSupScale,
                                         ChondLandScale))) %>%
  mutate(GovScore = rowMeans(data.frame(WgiScale, IuuScale))) %>%
  mutate(TotalScore = rowMeans(data.frame(BioScore, EconScore, GovScore)))

modScores <- glm(occurrence ~ BioScore + EconScore + GovScore,
                 family = binomial, NoSppScores)
summary(modScores)

allModsScores <-
  dredge(modScores)

topScores <-
  get.models(allModsScores, subset = delta <= 2)

# Calculate relative importance -----------------

RIGov <-
  allModsScores %>%
  as_tibble(.) %>%
  drop_na(GovScore) %>%
  summarise(RIgovval = sum(weight))

RIBio <-
  allModsScores %>%
  as_tibble(.) %>%
  drop_na(BioScore) %>%
  summarise(RIbioval = sum(weight))

scoreCoefPlot <-
  dwplot(list(topScores[[1]], topScores[[2]]),
         vline = geom_vline(xintercept = 0, colour = 'black', linetype = 1),
         dot_args = list(size = 4),
         whisker_args = list(size = 1)) %>%
  relabel_predictors(c(GovScore = 'Governance',
                       BioScore = 'Biological\nFactors')) +
  scale_colour_manual(values = c('darkslategray', 'darkslategray3')) +
  expand_limits(x = 4) +
  theme(legend.key = element_blank(),
        legend.background = element_blank(),
        legend.position = c(0.07, 0.1),
        legend.text = element_text(size = 13)) +
  presentation_theme() +
  labs(x = '\nCoefficient Estimate (SD units)',
       colour = ' ',
       title = 'Top Models of Scores with 95% CI - All Species') +
  annotate('text', x = 3.7, y = 2.5, label = 'Relative\nimportance', size = 5) +
  annotate('text', x = 3.7, y = c(2, 1.1), label = c('0.62', '0.35'), size = 6)

print(scoreCoefPlot)

# Redredging by dropping certain covariates

# drop the biological scores ----

NoBio <-
  NoSppAll %>%
  select(-EstDisScale, -PprodMeanScale, -SstCountMeanScale) %>%
  drop_na()

covsNoBio <- varsNoSpp[c(7:8, 10:11, 13:19)]

formulaNoBio <- paste('occurrence ~ ', paste(covsNoBio, collapse = ' + '))

modNoBio <- glm(formulaNoBio, family = binomial, NoBio)

NoBioMods <-
  dredge(modNoBio) #%>%
  #as_tibble(.) %>%
  #dplyr::filter(delta <= 2)

#write.csv(NoBioMods, '../Results/DredgeAllNoBio_181107.csv')

topNoBio <-
  get.models(NoBioMods, subset = delta <= 2)


RICoast <- 
  NoBioMods %>%
  as_tibble(.) %>%
  drop_na(CoastLengthScale) %>%
  summarise(val = sum(weight))

RIMan <- 
  NoBioMods %>%
  as_tibble(.) %>%
  drop_na(ManImpScale) %>%
  summarise(val = sum(weight))

RIHK <- 
  NoBioMods %>%
  as_tibble(.) %>%
  drop_na(HkExpScale) %>%
  summarise(val = sum(weight))

RIWgi <- 
  NoBioMods %>%
  as_tibble(.) %>%
  drop_na(WgiScale) %>%
  summarise(val = sum(weight))

dwplot(list(topNoBio[[1]], topNoBio[[2]], topNoBio[[3]]),
       vline = geom_vline(xintercept = 0, colour = 'black', linetype = 1),
       dot_args = list(size = 4),
       whisker_args = list(size = 1)) %>% 
  relabel_predictors(c(HkExpScale = 'Fin Exports\nto Hong Kong',
                       ManImpScale = 'Change in Mangroves',
                       CoastLengthScale = 'Coastline length')) +
  expand_limits(x = c(-1, 11)) +
  scale_colour_manual(values = c('darkblue', 'cyan4', 'cyan3')) +
  presentation_theme() +
  labs(x = '\nCoefficient Estimate (SD units)',
       colour = ' ',
       title = 'Top Models w/out Biological Covariates with 95% CI - All Species') +
  theme(legend.position = c(0.06, 0.95),
        legend.key = element_blank(),
        legend.background = element_blank(),
        legend.text = element_text(size = 12)) +
  scale_x_continuous(breaks = c(-1:10)) +
  annotate('text', x = 10.5, y = 3.4, label = 'Relative\nimportance', size = 5) +
  annotate('text', x = 10.5, y = c(3, 2, 1), 
           label = c('0.51', '0.51', '0.37'), size = 6)


# Collinearity scatterplot matrix with non-biological covariates for max model above

CorMatNoBio <-
  NoBio %>%
  select(occurrence, CoastLengthScale, ManImpScale, HkExpScale) %>%
  dplyr::rename(`CoastLength` = CoastLengthScale,
                `Mangrove` = ManImpScale,
                `HKExps` = HkExpScale) %>%
  mutate(presence = occurrence) %>%
  mutate(presence = dplyr::recode(presence, `1` = 'present', `0` = 'absent')) %>%
  ggpairs(., columns = c(2:4), aes(colour = presence)) +
  theme_bw()

print(CorMatNoBio)

             