# THIS SCRIPT ANALYZES THE DATA WITH UNSCALED COVARIATES #

library(tidyverse)
library(broom)
library(reshape2)
library(car)
library(MuMIn)

setwd('/users/helenyan/desktop/school/directed studies 2018/datasets')

# Can separate analysis to be species-specific --------------------------
sppData <- read_csv('CompleteSpeciesCovariates_181012.csv')

sppRaw <-
  sppData %>%
  select(country, ISO3, region, species, presence, occurrence,
         PprodMean, NBI, coastal.pop, Lengthkm, EPI, Discharge, ExporttoHK2010,
         MeanPro, GDP, HDI, OHI, UnreportedPercent, impact.percentage,
         no_reef_fishers, AreaSqKM, TotalTonnes, WGI, SstMean) %>%
  rename(Nbi = NBI, 
         CoastPop = coastal.pop,
         CoastLength = Lengthkm,
         Epi = EPI,
         EstDist = Discharge,
         ExpHK = ExporttoHK2010,
         ProteinSup = MeanPro,
         Gdp = GDP,
         Hdi = HDI,
         Ohi = OHI,
         Iuu = UnreportedPercent,
         ManImp = impact.percentage,
         ReefFisher = no_reef_fishers,
         Saltmarsh = AreaSqKM,
         ChondLand = TotalTonnes,
         Wgi = WGI) %>%
  filter(presence != 'unknown')

# Drop covariates that have too many missing values or are redundant --------
modSpp <-
  sppRaw %>%
  select(-Saltmarsh, -ReefFisher, -Hdi, -Ohi, -Epi, -Nbi)

# Model formula for all covariates used ------------------
cols <- names(modSpp)
allCovs <- cols[7:18]

allFormula <- paste('occurrence ~ ', paste(allCovs, collapse = ' + '))

options(na.action = na.fail)

# Maximum model for largetooth sawfish ------------------------------------

large <-
  modSpp %>%
  filter(species == 'large') %>%
  drop_na()

allLarge <-
  glm(allFormula, family = binomial, large)

modsLarge <-
  dredge(allLarge) %>%
  as_tibble(.) %>%
  filter(delta < 2)


# Maximum model for all sawfish spp combined ------------------------------------

allData <- read_csv('CountryCovariates_181022.csv')

allRaw <-
  allData %>%
  select(country, ISO3, species, nospecies, occurrence,
         PprodMean, NBI, coastal.pop, Lengthkm, EPI, Discharge, ExporttoHK2010,
         MeanPro, GDP, HDI, OHI, UnreportedPercent, impact.percentage,
         no_reef_fishers, AreaSqKM, TotalTonnes, WGI, SstCountMean) %>%
  rename(Nbi = NBI, 
         CoastPop = coastal.pop,
         CoastLength = Lengthkm,
         Epi = EPI,
         EstDist = Discharge,
         ExpHK = ExporttoHK2010,
         ProteinSup = MeanPro,
         Gdp = GDP,
         Hdi = HDI,
         Ohi = OHI,
         Iuu = UnreportedPercent,
         ManImp = impact.percentage,
         ReefFisher = no_reef_fishers,
         Saltmarsh = AreaSqKM,
         ChondLand = TotalTonnes,
         Wgi = WGI,
         SstMean = SstCountMean) 

modAll <- 
  allRaw %>%
  select(-Saltmarsh, -ReefFisher) %>%
  drop_na()

#sapply(modAll, function(x) sum(is.na(x)))


allSpp <-
  glm(allFormula, family = binomial, modAll)

modsSpp <-
  dredge(allSpp) %>%
  as_tibble(.) %>%
  filter(delta <= 2)

noHKcovs <- cols[c(7:10, 12:18)]
noHKFormula <- paste('occurrence ~ ', paste(noHKcovs, collapse = ' + '))

noHK <-
  allRaw %>%
  select(-Saltmarsh, -ReefFisher, -ExpHK) %>%
  drop_na()

noHKSpp <-
  glm(noHKFormula, family = binomial, noHK)

modsnoHK <-
  dredge(noHKSpp) %>%
  as_tibble(.) %>%
  filter(delta <= 2)


