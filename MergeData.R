##### MERGING ALL DATASETS TO CREATE MASTER DATA SET ######

library(tidyverse)

setwd("/users/helenyan/desktop/school/directed studies 2018/datasets/")

iso <- read.csv('CountryISO3.csv')

# Remove duplicates #
iso <-
  iso %>%
  distinct(ISO, .keep_all = TRUE) %>%
  mutate(ISO3 = ISO) %>%
  dplyr::select(-ISO)

SauClean <- read.csv('SauFishingGear_181109.csv')
BioInd <- read.csv('BiodiversityIndexData_180513.csv')
CoastPop <- read.csv('CoastalPopClean_181109.csv')
CoastLength <- read.csv('CoastLineLength.csv')
EnvPerf<- read.csv('EnvPerfIndexData_180513.csv')
EstDis <- read.csv('EstuariesDischarge_180516.csv')
#HKExp <- read.csv('exportHK2010_180222.csv')
Exports <- read_csv('ChondExpsClean_181119.csv')
Consumpt <- read.csv('FaoConsumptionClean_181109.csv')
GdpHdiOhi <- read.csv('GdpHdiOhi_180515.csv')
Iuu <- read.csv('IuuClean_181109.csv')
Mangrove <- read.csv('MangroveImpact_180515.csv')
ReefFishers <- read.csv('ReefFishers.csv')
Saltmarsh <- read.csv('SaltmarshMeanArea_180316.csv')
#ChondLand <- read.csv('TotalLandings_180512.csv')
Landings <- read_csv('ChondLandClean_181119.csv')
Wgi <- read.csv('WorldGovInd_180515.csv')
pprod <- read.csv('PProdRaw_181012.csv')
ManNew <- read_csv('Mangrove_Area_ISO_181107.csv')
Fishery <- read_csv('FisheryProductionClean_181119.csv')
#Lcatch <- read_csv('LDavidsonCatch_190115.csv')
ChondCatch <- read_csv('FAOChondcatch_190119.csv')

# Can't merge sst data with this dataset because there's a different
# value for each country depending on the range of the species in the region

# Clean fin export data
FinExp <-
  Exports %>%
  dplyr::select(-X1, -year, -unit, -country) %>%
  filter(product == 'fins') %>%
  dplyr::rename(FinUSD = value,
                ISO3 = ISO) %>%
  dplyr::select(-product) 

# Clean chond landings data
Land <-
  Landings %>%
  dplyr::select(-X1, -year, -unit, -country) %>%
  dplyr::rename(ChondLand = total,
                ISO3 = ISO)

# Clean fishery landings data
Fish <-
  Fishery %>%
  dplyr::select(-X1, -year, -unit, -country) %>%
  dplyr::rename(FishProd = total,
         ISO3 = ISO)

# Clean SAU fishing gear data
SauClean <-
  SauClean %>%
  dplyr::select(-X, -totalGearValue) %>%
  dplyr::rename(ISO3 = ISO)

# Clean GIS-derived data 
PriProd <-
  pprod %>%
  dplyr::select(ISO_Ter1, MEAN_PPROD) %>%
  dplyr::rename(ISO3 = ISO_Ter1, PprodMean = MEAN_PPROD)

# Rename NBI column 
BioInd <-
  BioInd %>%
  dplyr::select(-X, -Country, -NBIScale)

# Clean coastal pop 
CoastPop <-
  CoastPop %>%
  dplyr::select(ISO3, CoastPop)

# Clean coastline length 
CoastLength <-
  CoastLength %>%
  dplyr::select(ISO3, Lengthkm) %>%
  dplyr::rename(CoastLength = Lengthkm)

# Clean EPI
EnvPerf <-
  EnvPerf %>%
  dplyr::select(ISO, EPI) %>%
  dplyr::rename(ISO3 = ISO)

# Clean estuaries discharge 
EstDis <-
  EstDis %>%
  dplyr::select(-DischargeScale) %>%
  dplyr::rename(ISO3 = X, EstDis = Discharge)

# Clean fin exports to HK 
#HKExp <-
#  HKExp %>%
#  select(ISO3, ExporttoHK2010t) %>%
#  rename(HkExp = ExporttoHK2010t)

# Clean protein diet
Consumpt <-
  Consumpt %>%
  dplyr::select(ISO3, ProteinDiet)

# Clean GDP, HDI, and OHI 
GdpHdiOhi <-
  GdpHdiOhi %>%
  dplyr::select(-Country)

# Clean IUU fishing 

Iuu <-
  Iuu %>%
  dplyr::select(ISO3, Iuu)

# Clean columns, calculate impact, and scale mangrove loss #
# No need to reverse direction of mangrove loss because it's reflected 
# in the impact calculation 
#MangroveImp <-
#  Mangrove %>%
#  mutate(impact.percentage = ((AreaHa00 - AreaHa1980)/AreaHa1980)*100) %>%
#  select(Country, ISO3, impact.percentage) %>%
#  mutate(ManImpScale = impact.percentage) %>%
#  mutate_at('ManImpScale', funs(scale(.) %>%
#                                  as.vector))

ManNew <-
  ManNew %>%
  dplyr::select(PARENT_ISO, MEAN_GIS_AREA_K) %>%
  dplyr::rename(ISO3 = PARENT_ISO, Mang = MEAN_GIS_AREA_K)

# Clean coral reef fishers
ReefFishers <-
  ReefFishers %>%
  dplyr::select(ISO, no_reef_fishers) %>%
  dplyr::rename(ISO3 = ISO, ReefFishers = no_reef_fishers) %>%
  .[1:98, ] %>%
  mutate(ReefFishers = as.numeric(ReefFishers))

# Clean saltmarsh area 
Saltmarsh <-
  Saltmarsh %>%
  dplyr::rename(ISO3 = ISO, Saltmarsh = AreaSqKM)

# Clean chond landings
#ChondLand <-
#  ChondLand %>%
#  select(ISO, TotalTonnes) %>%
#  rename(ISO3 = ISO, ChondLand = TotalTonnes)

# Clean world governance index 
Wgi <-
  Wgi %>%
  dplyr::rename(ISO3 = X)

# Clean Lindsay's catch data
#Lcatch <- 
#  Lcatch %>% 
#  select(ISO.x, sum.catch) %>% 
#  dplyr::rename(ISO3 = ISO.x, ChondCatch = sum.catch)

# Clean total summed catch
ChondCatch <- 
  ChondCatch %>% 
  select(-X1, -country) %>% 
  dplyr::rename(ChondCatch = totalCatch)

# Combine all scores into single df ----------------------------------------------------------------------
CountSum <-
  iso %>%
  left_join(., FinExp, by = c('ISO3' = 'ISO3')) %>%
  left_join(., Land, by = c('ISO3' = 'ISO3')) %>%
  left_join(., Fish, by = c('ISO3' = 'ISO3')) %>%
  left_join(., SauClean, by = c('ISO3' = 'ISO3')) %>%
  left_join(., PriProd, by = c('ISO3' = 'ISO3')) %>%
  left_join(., BioInd, by = c('ISO3' = 'ISO')) %>%
  left_join(., CoastPop, by = c('ISO3' = "ISO3")) %>%
  left_join(., CoastLength, by = c('ISO3' = 'ISO3')) %>%
  left_join(., EnvPerf, by = c('ISO3' = 'ISO3')) %>%
  left_join(., EstDis, by = c('ISO3' = 'ISO3')) %>%
  #left_join(., HKExp, by = c('ISO3' = 'ISO3')) %>%
  left_join(., Consumpt, by = c('ISO3' = 'ISO3')) %>%
  left_join(., GdpHdiOhi, by = c('ISO3' = 'ISO')) %>%
  left_join(., Iuu, by = c('ISO3' = 'ISO3')) %>%
  left_join(., ManNew, by = c('ISO3' = 'ISO3')) %>%
  left_join(., ReefFishers, by = c('ISO3' = 'ISO3')) %>%
  # remove saltmarsh because too many zeros
  #left_join(., Saltmarsh, by = c('ISO3' = 'ISO3')) %>%
  #left_join(., ChondLand, by = c('ISO3' = 'ISO3')) %>%
  left_join(., Wgi, by = c('ISO3' = 'ISO3')) %>%
  #left_join(., Lcatch, by = c('ISO3' = 'ISO3')) %>% 
  left_join(., ChondCatch, by = c('ISO3' = 'ISO3')) %>% 
  distinct(ISO3, .keep_all = TRUE) %>%
  replace(., is.na(.), 0)

#allcols <- names(CountSum)
#cols <- allcols[3:221]
#colsScaled <- paste(cols, 'Scale', sep = '')

#CountSumScale <-
#  CountSum %>%
#  rename_at(vars(cols), ~ colsScaled) %>%
#  mutate_at(3:21, funs(scale(.))) 


#CountSumAll <-
#  CountSum %>%
#  left_join(., CountSumScale, by = c('ISO3' = 'ISO3')) %>%
#  dplyr::select(-Country.y) %>%
#  dplyr::rename(country = Country.x)

sapply(CountSumAll, function(x) sum(is.na(x)))


# Combine covarites with sawfish occurrence data -----------------------------------------------------------

SppIso <- read_csv('CompleteSpeciesISO_180924.csv')

SppCov <-
  SppIso %>%
  left_join(., CountSum, by = c('ISO3' = 'ISO3')) %>%
  mutate(matchid = paste(ISO3, species, sep = '-'))

sst <- read.csv('SSTRaw_181011.csv', na.strings = ' ')

# Clean data columns and remove unwanted columns
# Need to scale SST but there is no directionality to it
sstDat <-
  sst %>%
  mutate(species = `BINOMIAL`) %>%
  mutate(species = dplyr::recode(species, `Anoxypristis cuspidata` = 'narrow',
                                 `Pristis clavata` = 'dwarf',
                                 `Pristis pectinata Eastern Atlantic` = 'small',
                                 `Pristis pectinata Western Atlantic` = 'small',
                                 `Pristis pristis Eastern Atlantic` = 'large',
                                 `Pristis pristis Eastern Pacific` = 'large',
                                 `Pristis pristis Indo-west Pacific` = 'large',
                                 `Pristis pristis Western Atlantic` = 'large',
                                 `Pristis Zijsron` = 'green')) %>%
  mutate(SstMean = MEAN_SST) %>%
  dplyr::rename(ISO3 = ISO_TER1, occurrence = `PRESENCE`, country = `TERRITORY1`,
                SstMeanScale = MEAN_SST) %>%
  dplyr::select(-Rowid, -FID, -BINOMIAL, -UNIQUE_FID, -GEONAME, -SOVEREIGN1,
         -FREQUENCY, -MAX_SST, -MIN_SST) %>%
  mutate_at('SstMeanScale', funs(scale(.) %>% as.vector)) %>%
  mutate(matchid2 = paste(ISO3, species, sep = '-'))


AllCov <-
  SppCov %>%
  left_join(., sstDat, by = c('matchid' = 'matchid2')) %>%
  dplyr::select(-country.y, -country.x, -matchid, -occurrence.y, -ISO3.y, -species.y) %>%
  dplyr::rename(ISO3 = ISO3.x, species = species.x, 
         occurrence = occurrence.x) %>%
  mutate(refid2 = paste(ISO3, species, sep = '-')) %>%
  distinct(., refid2, .keep_all = TRUE) %>%
  dplyr::select(-refid2) %>% 
  .[, c(7, 1:6, 8:26, 28)] %>%
  filter(ISO3 != 'LAO') %>%
  mutate(Country = case_when(ISO3 == 'TLS' ~ 'Timor Leste',
                             TRUE ~ as.character(Country)))

sapply(AllCov, function(x) sum(is.na(x)))

write_csv(AllCov, 'CompleteSpeciesCovariates_190119.csv')


# Create generic dataframe that is not species specific --------------------------------

# Country specific ---------

# Calculate the mean of the SST for the country based on
# the geographic range of the species that resides there -------------------

sstCountryScaled <-
  sstDat %>%
  dplyr::select(c(1:5)) %>%
  dplyr::filter(occurrence != '2') %>%
  mutate(geo = paste(country, ISO3, sep = '-')) %>%
  dplyr::select(-country, -ISO3) %>%
  group_by(geo, occurrence) %>%
  dplyr::summarise(SstCountMeanScale = mean(SstMeanScale), 
            spp = paste(species, collapse = '-'), 
            sppres = length(species)) %>%
  mutate(sppres = case_when(occurrence == '0' ~ 0,
                            TRUE ~ as.numeric(sppres))) %>%
  dplyr::select(geo, SstCountMeanScale) %>%
  separate(col = geo, into = c('country', 'ISO3'), sep = '-')

sstCountryRaw <-
  sstDat %>%
  dplyr::select(c(1:3, 5:6)) %>%
  dplyr::filter(occurrence != '2') %>%
  mutate(geo = paste(country, ISO3, sep = '_')) %>%
  dplyr::select(-country, -ISO3) %>%
  group_by(geo, occurrence) %>%
  dplyr::summarise(SstCountMean = mean(SstMean), 
            spp = paste(species, collapse = '-'), 
            sppres = length(species)) %>%
  mutate(sppres = case_when(occurrence == '0' ~ 0,
                            TRUE ~ as.numeric(sppres))) %>%
  dplyr::select(geo, SstCountMean) %>%
  separate(col = geo, into = c('country', 'ISO3'), sep = '_') %>%
  distinct(., country, .keep_all = TRUE)


NoSpp <-
  AllCov %>%
  dplyr::select(c(1:7)) %>%
  dplyr::select(-c(1, 3, 7), -region, -presence, -country) %>%
  dplyr::filter(occurrence !='2') %>%
  group_by(ISO3, occurrence) %>%
  dplyr::summarise(spp = paste(species, collapse = '-'),
            sppres = length(species)) %>%
  mutate(sppres = case_when(occurrence == '0' ~ 0,
                            TRUE ~ as.numeric(sppres))) %>%
  dplyr::rename(species = spp, nospecies = sppres) %>%
  arrange(desc(nospecies)) %>%
  distinct(., ISO3, .keep_all = TRUE) %>%
  left_join(., sstCountryRaw, by = c('ISO3' = 'ISO3')) %>%
  .[, c(5, 1, 3, 4, 2, 6)] %>%
  left_join(., sstCountryScaled, by = c('ISO3' = 'ISO3')) %>%
  dplyr::select(-country.y) %>%
  dplyr::rename(country = country.x) %>%
  left_join(., CountSumAll, by = c('ISO3' = 'ISO3')) %>%
  dplyr::select(-country.y) %>%
  dplyr::rename(country = country.x,
                SstMean = SstCountMean,
                SstMeanScale = SstCountMeanScale) %>%
  distinct(., ISO3, .keep_all = TRUE) %>% 
  .[, c(1:6, 8:25, 7, 26:43)]

sapply(NoSpp, function(x) sum(is.na(x)))

write_csv(NoSpp, 'CountryCovariates_181109.csv')


# Tranformations ------------------------------------------------------------------

spp <- read_csv('CompleteSpeciesCovariates_190119.csv')

# make histograms to assess which variables to transform
sppDatahist <-
  spp %>%
  dplyr::select(2, 8:27) %>%
  distinct(., ISO3, .keep_all = TRUE) %>%
  dplyr::select(-ISO3) %>%
  gather(key = covs)


for(i in unique(sppDatahist$covs)) {
  
  print(
    sppDatahist %>%
      filter(covs == i) %>%
      ggplot(., aes(value)) +
      geom_histogram(bins = 30) +
      labs(title = paste(i), x = paste(i)) +
      theme_classic()
  )
}

# processing the data -------------------------------------------------

sppProc <- 
  spp %>%
  filter(occurrence != '2') %>% 
  dplyr::select(2, 4, 6, 8:27) %>%
  # create dummy variables for species
  mutate(refID = paste(ISO3, species, occurrence, sep = '_')) %>%
  dplyr::select(-ISO3) %>%
  mutate(var = 1) %>%
  spread(species, var, fill = 0, sep = '') %>%
  separate(refID, into = c('ISO3', 'spp', 'occ'), sep = '_') %>%
  dplyr::select(-spp, -occ) %>% 
  .[, c(22:27, 1:21)] %>%
  # divide FinUSD/1000 like FAO raw presents
  mutate(FinUSD = FinUSD/1000) %>%
  # log+1 transform the data
  mutate_at(vars('FinUSD', 'ChondLand', 'FishProd', 'totalGearTonnes', 'PprodMean',
                 'CoastPop', 'CoastLength', 'EstDis', 'ProteinDiet', 'GDP', 
                 'Iuu', 'Mang', 'ChondCatch'), log1p) %>%
  dplyr::rename('logFinUSD' = 'FinUSD',
                'logChondLand' = 'ChondLand',
                'logFishProd' = 'FishProd',
                'logtotalGearTonnes' = 'totalGearTonnes',
                'logPprodMean' = 'PprodMean',
                'logCoastPop' = 'CoastPop',
                'logCoastLength' = 'CoastLength', 
                'logEstDis' = 'EstDis', 
                'logProteinDiet' = 'ProteinDiet', 
                'logGDP' = 'GDP', 
                'logIuu' = 'Iuu', 
                'logMang' = 'Mang',
                'logChondCatch' = 'ChondCatch')

write.csv(sppProc, 'ProcessedCovariates_190119.csv')


# check normality around these variables ----------------
# don't need to run this every time 

procHist <-
  sppProc %>%
  distinct(., ISO3, .keep_all = TRUE) %>%
  select(-ISO3, -occurrence, -speciesdwarf, -speciesgreen, -specieslarge, 
         -speciesnarrow, -speciessmall) %>%
  gather()


for(i in unique(procHist$key)) {
  
  print(
    procHist %>%
      filter(key == i) %>%
      ggplot(., aes(value)) +
      geom_histogram(bins = 30) +
      labs(x = paste(i), title = paste(i)) +
      theme_classic()
  )
}


# get data-deficients ---------------------------

ddProc <- 
  AllCov %>%
  filter(occurrence == '2') %>% 
  dplyr::select(2, 4, 6, 8:26) %>%
  # create dummy variables for species
  mutate(refID = paste(ISO3, species, occurrence, sep = '_')) %>% 
  dplyr::select(-ISO3) %>%
  mutate(var = 1) %>%
  spread(species, var, fill = 0, sep = '') %>% 
  separate(refID, into = c('ISO3', 'spp', 'occ'), sep = '_') %>%
  dplyr::select(-spp, -occ) %>% 
  .[, c(21, 1, 22:26, 2:20)] %>%
  # divide FinUSD/1000 like FAO raw presents
  mutate(FinUSD = FinUSD/1000) %>%
  # log+1 transform the data
  mutate_at(vars('FinUSD', 'ChondLand', 'FishProd', 'totalGearTonnes', 'PprodMean',
                 'CoastPop', 'CoastLength', 'EstDis', 'ProteinDiet', 'GDP', 
                 'Iuu', 'Mang', 'ChondCatch'), log1p) %>%
  dplyr::rename('logFinUSD' = 'FinUSD',
                'logChondLand' = 'ChondLand',
                'logFishProd' = 'FishProd',
                'logtotalGearTonnes' = 'totalGearTonnes',
                'logPprodMean' = 'PprodMean',
                'logCoastPop' = 'CoastPop',
                'logCoastLength' = 'CoastLength', 
                'logEstDis' = 'EstDis', 
                'logProteinDiet' = 'ProteinDiet', 
                'logGDP' = 'GDP', 
                'logIuu' = 'Iuu', 
                'logMang' = 'Mang',
                'logChondCatch' = 'ChondCatch')

write.csv(ddProc, 'ProcessedDataDeficients_181214.csv')
