##### MERGING ALL DATASETS TO CREATE MASTER DATA SET ######

library(tidyverse)

setwd("/users/helenyan/desktop/school/directed studies 2018/datasets/")

iso <- read.csv('CountryISO3.csv')

# Remove duplicates #
iso <-
  iso %>%
  distinct(ISO, .keep_all = TRUE) %>%
  mutate(ISO3 = ISO) %>%
  select(-ISO)

SauClean <- read.csv('SauFishingGear_181109.csv')
BioInd <- read.csv('BiodiversityIndexData_180513.csv')
CoastPop <- read.csv('CoastalPopClean_181109.csv')
CoastLength <- read.csv('CoastLineLength.csv')
EnvPerf<- read.csv('EnvPerfIndexData_180513.csv')
EstDis <- read.csv('EstuariesDischarge_180516.csv')
HKExp <- read.csv('exportHK2010_180222.csv')
Consumpt <- read.csv('FaoConsumptionClean_181109.csv')
GdpHdiOhi <- read.csv('GdpHdiOhi_180515.csv')
Iuu <- read.csv('IuuClean_181109.csv')
Mangrove <- read.csv('MangroveImpact_180515.csv')
ReefFishers <- read.csv('ReefFishers.csv')
Saltmarsh <- read.csv('SaltmarshMeanArea_180316.csv')
ChondLand <- read.csv('TotalLandings_180512.csv')
Wgi <- read.csv('WorldGovInd_180515.csv')
pprod <- read.csv('PProdRaw_181012.csv')
ManNew <- read_csv('Mangrove_Area_ISO_181107.csv')

# Can't merge sst data with this dataset because there's a different
# value for each country depending on the range of the species in the region

# Clean SAU fishing gear data
SauClean <-
  SauClean %>%
  select(-X) %>%
  rename(ISO3 = ISO)

# Clean GIS-derived data 
PriProd <-
  pprod %>%
  select(ISO_Ter1, MEAN_PPROD) %>%
  dplyr::rename(ISO3 = ISO_Ter1, PprodMean = MEAN_PPROD)

# Rename NBI column 
BioInd <-
  BioInd %>%
  select(-X, -Country, -NBIScale)

# Clean coastal pop 
CoastPop <-
  CoastPop %>%
  select(ISO3, CoastPop)

# Clean coastline length 
CoastLength <-
  CoastLength %>%
  select(ISO3, Lengthkm) %>%
  rename(CoastLength = Lengthkm)

# Clean EPI
EnvPerf <-
  EnvPerf %>%
  select(ISO, EPI) %>%
  rename(ISO3 = ISO)

# Clean estuaries discharge 
EstDis <-
  EstDis %>%
  select(-DischargeScale) %>%
  rename(ISO3 = X, EstDis = Discharge)

# Clean fin exports to HK 
HKExp <-
  HKExp %>%
  select(ISO3, ExporttoHK2010t) %>%
  rename(HkExp = ExporttoHK2010t)

# Clean protein diet
Consumpt <-
  Consumpt %>%
  select(ISO3, ProteinDiet)

# Clean GDP, HDI, and OHI 
GdpHdiOhi <-
  GdpHdiOhi %>%
  select(-Country)

# Clean =IUU fishing 

Iuu <-
  Iuu %>%
  select(ISO3, Iuu)

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
  select(PARENT_ISO, MEAN_GIS_AREA_K) %>%
  rename(ISO3 = PARENT_ISO, Mang = MEAN_GIS_AREA_K)

# Clean coral reef fishers
ReefFishers <-
  ReefFishers %>%
  select(ISO, no_reef_fishers) %>%
  rename(ISO3 = ISO, ReefFishers = no_reef_fishers) %>%
  .[1:98, ] %>%
  mutate(ReefFishers = as.numeric(ReefFishers))

# Clean saltmarsh area 
Saltmarsh <-
  Saltmarsh %>%
  rename(ISO3 = ISO, Saltmarsh = AreaSqKM)

# Clean chond landings
ChondLand <-
  ChondLand %>%
  select(ISO, TotalTonnes) %>%
  rename(ISO3 = ISO, ChondLand = TotalTonnes)

# Clean world governance index 
Wgi <-
  Wgi %>%
  rename(ISO3 = X)



# Combine all scores into single df ----------------------------------------------------------------------
CountSum <-
  iso %>%
  left_join(., SauClean, by = c('ISO3' = 'ISO3')) %>%
  left_join(., PriProd, by = c('ISO3' = 'ISO3')) %>%
  left_join(., BioInd, by = c('ISO3' = 'ISO')) %>%
  left_join(., CoastPop, by = c('ISO3' = "ISO3")) %>%
  left_join(., CoastLength, by = c('ISO3' = 'ISO3')) %>%
  left_join(., EnvPerf, by = c('ISO3' = 'ISO3')) %>%
  left_join(., EstDis, by = c('ISO3' = 'ISO3')) %>%
  left_join(., HKExp, by = c('ISO3' = 'ISO3')) %>%
  left_join(., Consumpt, by = c('ISO3' = 'ISO3')) %>%
  left_join(., GdpHdiOhi, by = c('ISO3' = 'ISO')) %>%
  left_join(., Iuu, by = c('ISO3' = 'ISO3')) %>%
  left_join(., ManNew, by = c('ISO3' = 'ISO3')) %>%
  left_join(., ReefFishers, by = c('ISO3' = 'ISO3')) %>%
  mutate(ReefFishers = as.numeric(ReefFishers)) %>%
  left_join(., Saltmarsh, by = c('ISO3' = 'ISO3')) %>%
  left_join(., ChondLand, by = c('ISO3' = 'ISO3')) %>%
  left_join(., Wgi, by = c('ISO3' = 'ISO3')) %>%
  distinct(ISO3, .keep_all = TRUE) %>%
  replace(., is.na(.), 0)

sapply(CountSum, function(x) sum(is.na(x)))

allcols <- names(CountSum)
cols <- allcols[3:21]
colsScaled <- paste(cols, 'Scale', sep = '')

CountSumScale <-
  CountSum %>%
  rename_at(vars(cols), ~ colsScaled) %>%
  mutate_at(3:21, funs(scale(.))) 


CountSumAll <-
  CountSum %>%
  left_join(., CountSumScale, by = c('ISO3' = 'ISO3')) %>%
  select(-Country.y) %>%
  rename(country = Country.x)



# Combine covarites with sawfish occurrence data -----------------------------------------------------------

SppIso <- read_csv('CompleteSpeciesISO_180924.csv')

SppCov <-
  SppIso %>%
  left_join(., CountSumAll, by = c('ISO3' = 'ISO3')) %>%
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
  select(-Rowid, -FID, -BINOMIAL, -UNIQUE_FID, -GEONAME, -SOVEREIGN1,
         -FREQUENCY, -MAX_SST, -MIN_SST) %>%
  mutate_at('SstMeanScale', funs(scale(.) %>% as.vector)) %>%
  mutate(matchid2 = paste(ISO3, species, sep = '-'))


AllCov <-
  SppCov %>%
  left_join(., sstDat, by = c('matchid' = 'matchid2')) %>%
  select(-country.y, -country.x, -matchid, -occurrence.y, -ISO3.y, -species.y) %>%
  dplyr::rename(ISO3 = ISO3.x, species = species.x, 
         occurrence = occurrence.x) %>%
  mutate(refid2 = paste(ISO3, species, sep = '-')) %>%
  distinct(., refid2, .keep_all = TRUE) %>%
  select(-refid2) %>%
  .[, c(45, 1:25, 47, 26:44, 46)]

write_csv(AllCov, 'CompleteSpeciesCovariates_181109.csv')


# Create generic dataframe that is not species specific --------------------------------

# Country specific ---------

# Calculate the mean of the SST for the country based on
# the geographic range of the species that resides there -------------------

sstCountryScaled <-
  sstDat %>%
  select(c(1:5)) %>%
  dplyr::filter(occurrence != '2') %>%
  mutate(geo = paste(country, ISO3, sep = '-')) %>%
  select(-country, -ISO3) %>%
  group_by(geo, occurrence) %>%
  dplyr::summarise(SstCountMeanScale = mean(SstMeanScale), 
            spp = paste(species, collapse = '-'), 
            sppres = length(species)) %>%
  mutate(sppres = case_when(occurrence == '0' ~ 0,
                            TRUE ~ as.numeric(sppres))) %>%
  select(geo, SstCountMeanScale) %>%
  separate(col = geo, into = c('country', 'ISO3'), sep = '-')

sstCountryRaw <-
  sstDat %>%
  select(c(1:3, 5:6)) %>%
  dplyr::filter(occurrence != '2') %>%
  mutate(geo = paste(country, ISO3, sep = '_')) %>%
  select(-country, -ISO3) %>%
  group_by(geo, occurrence) %>%
  dplyr::summarise(SstCountMean = mean(SstMean), 
            spp = paste(species, collapse = '-'), 
            sppres = length(species)) %>%
  mutate(sppres = case_when(occurrence == '0' ~ 0,
                            TRUE ~ as.numeric(sppres))) %>%
  select(geo, SstCountMean) %>%
  separate(col = geo, into = c('country', 'ISO3'), sep = '_') %>%
  distinct(., country, .keep_all = TRUE)


NoSpp <-
  AllCov %>%
  select(c(1:7)) %>%
  select(-c(1, 3, 7), -region, -presence, -country) %>%
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
  select(-country.y) %>%
  dplyr::rename(country = country.x) %>%
  left_join(., CountSumAll, by = c('ISO3' = 'ISO3')) %>%
  select(-country.y) %>%
  dplyr::rename(country = country.x,
                SstMean = SstCountMean,
                SstMeanScale = SstCountMeanScale) %>%
  distinct(., ISO3, .keep_all = TRUE) %>%
  .[, c(1:6, 8:26, 7, 27:45)]

write_csv(NoSpp, 'CountryCovariates_181109.csv')











