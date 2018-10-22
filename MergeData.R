##### MERGING ALL DATASETS TO CREATE MASTER DATA SET ######

setwd("/users/helenyan/desktop/school/directed studies 2018/datasets")

library(tidyverse)

iso <- read.csv('CountryISO3.csv')
BioInd <- read.csv('BiodiversityIndexData_180513.csv')
CoastPop <- read.csv('CoastalPopulation_180530.csv')
CoastLength <- read.csv('CoastLineLength.csv')
EnvPerf<- read.csv('EnvPerfIndexData_180513.csv')
EstDis <- read.csv('EstuariesDischarge_180516.csv')
HKExp <- read.csv('exportHK2010_180222.csv')
Consumpt <- read.csv('FAOConsumptData_180513.csv')
GdpHdiOhi <- read.csv('GdpHdiOhi_180515.csv')
Iuu <- read.csv('IUUFishing_180511.csv')
Mangrove <- read.csv('MangroveImpact_180515.csv')
ReefFishers <- read.csv('ReefFishers.csv')
Saltmarsh <- read.csv('SaltmarshMeanArea_180316.csv')
ChondLand <- read.csv('TotalLandings_180512.csv')
Wgi <- read.csv('WorldGovInd_180515.csv')
pprod <- read.csv('PProdRaw_181012.csv')

# Can't merge sst data with this dataset because there's a different
# value for each country depending on the range of the species in the region


# Remove duplicates #
iso <-
  iso %>%
  distinct(ISO, .keep_all = TRUE) %>%
  mutate(ISO3 = ISO) %>%
  select(-ISO)

# Clean GIS-derived data and scale all 3 productivity values
PriProd <-
  pprod %>%
  select(-OBJECTID, -FREQUENCY) %>%
  dplyr::rename(ISO3 = ISO_Ter1, PprodMean = MEAN_PPROD, PprodMax = MAX_PPROD, 
                PprodMin = MIN_PPROD) %>%
  mutate(PprodMeanScale = PprodMean) %>%
  mutate_at('PprodMeanScale', funs(scale(.) %>% 
                                     as.vector)) %>%
  mutate(PprodMaxScale = PprodMax) %>%
  mutate_at('PprodMaxScale', funs(scale(.) %>%
                                    as.vector)) %>%
  mutate(PprodMinScale = PprodMin) %>%
  mutate_at('PprodMinScale', funs(scale(.) %>% 
                                    as.vector))

# Rename NBI column #
BioInd <-
  BioInd %>%
  mutate(NbiScale = NBIScale) %>%
  select(-NBIScale) %>%
  mutate(ISO3 = ISO) %>%
  select(-ISO)

# Scale and reverse direction of coastal population #
CoastPop <-
  CoastPop %>%
  mutate(CoastPopScale = coastal.pop*(-1)) %>%
  mutate_at('CoastPopScale', funs(scale(.) %>% 
                                    as.vector)) 

# Clean columns and scale coastline length #
CoastLength <-
  CoastLength %>%
  select(Country.Name, ISO3, Lengthkm) %>%
  mutate(CoastLengthScale = Lengthkm) %>%
  mutate_at('CoastLengthScale', funs(scale(.) %>%
                                       as.vector))


# Clean columns and scale EPI #
EnvPerf <-
  EnvPerf %>%
  select(-EPIScale) %>%
  mutate(EpiScale = EPI) %>%
  mutate_at('EpiScale', funs(scale(.) %>%
                               as.vector)) %>%
  mutate(ISO3 = ISO) %>%
  select(-ISO)

# Clean columns and scale estuaries discharge #
EstDis <-
  EstDis %>%
  select(-DischargeScale) %>%
  mutate(ISO3 = X) %>%
  select(-X) %>%
  mutate(EstDisScale = Discharge) %>%
  mutate_at('EstDisScale', funs(scale(.) %>%
                                  as.vector))

# Clean columns, scale, and reverse direction of fin exports to HK #
HKExp <-
  HKExp %>%
  select(Country, ISO3, ExporttoHK2010) %>%
  mutate(HkExpScale = ExporttoHK2010*(-1)) %>%
  mutate_at('HkExpScale', funs(scale(.) %>%
                                 as.vector))

# Clean columns, scale, and reverse direction of protein supply
Consumpt <-
  Consumpt %>%
  select(-MeanProScale) %>%
  mutate(ProteinSupScale = MeanPro*(-1)) %>%
  mutate_at('ProteinSupScale', funs(scale(.) %>%
                                      as.vector)) %>%
  mutate(country = X) %>%
  select(-X)

# Scale GDP, HDI, and OHI #
GdpHdiOhi <-
  GdpHdiOhi %>%
  mutate(ISO3 = ISO) %>%
  select(-ISO) %>%
  mutate(GdpScale = GDP) %>%
  mutate(HdiScale = HDI) %>%
  mutate(OhiScale = OHI) %>%
  mutate_at(c('GdpScale', 'HdiScale', 'OhiScale'), funs(scale(.) %>%
                                                          as.vector))

# Clean columns, scale, and reverse direction of IUU fishing #
Iuu <-
  Iuu %>%
  select(-UnreportedPercStand) %>%
  mutate(IuuScale = UnreportedPercent*(-1)) %>%
  mutate_at('IuuScale', funs(scale(.) %>%
                               as.vector))

# Clean columns, calculate impact, and scale mangrove loss #
# No need to reverse direction of mangrove loss because it's reflected 
# in the impact calculation 
Mangrove <-
  Mangrove %>%
  mutate(impact.percentage = ((AreaHa00 - AreaHa1980)/AreaHa1980)*100) %>%
  select(Country, ISO3, impact.percentage) %>%
  mutate(ManImpScale = impact.percentage) %>%
  mutate_at('ManImpScale', funs(scale(.) %>%
                                  as.vector))


# Scale and reverse direction of coral reef fishers #
ReefFishers <-
  ReefFishers %>%
  mutate(ISO3 = ISO) %>%
  select(-ISO) %>%
  mutate(no_reef_fishers = gsub(',', '', no_reef_fishers)) %>%
  mutate(no_reef_fishers = as.numeric(no_reef_fishers)) %>%
  mutate(ReefFisherScale = no_reef_fishers*(-1)) %>%
  mutate_at('ReefFisherScale', funs(scale(.) %>%
                                      as.vector))

# Clean columns and scale saltmarsh area #
Saltmarsh <-
  Saltmarsh %>%
  mutate(ISO3 = ISO) %>%
  select(-ISO) %>%
  mutate(SaltmarshScale = AreaSqKM) %>%
  mutate_at('SaltmarshScale', funs(scale(.) %>%
                                     as.vector))

# Clean columns, scale, and reverse direction of chond landings #
ChondLand <-
  ChondLand %>%
  mutate(ISO3 = ISO) %>%
  select(-ISO, -TonnesScaled) %>%
  mutate(ChondLandScale = TotalTonnes*(-1)) %>%
  mutate_at('ChondLandScale', funs(scale(.) %>%
                                     as.vector))

# Clean columns and re-scale world governance index #
Wgi <-
  Wgi %>%
  mutate(ISO3 = X) %>%
  select(-X) %>%
  mutate(WgiScale = WGI) %>%
  mutate_at('WgiScale', funs(scale(.) %>%
                               as.vector))



# Combine all scores into single df ----------------------------------------------------------------------

CountSum <-
  iso %>%
  left_join(., PriProd, by = c('ISO3' = 'ISO3')) %>%
  left_join(., BioInd, by = c('ISO3' = 'ISO3')) %>%
  select(-X, -Country.y) %>%
  left_join(., CoastPop, by = c('ISO3' = "ISO3")) %>%
  select(-COUNTRYNAME..unique.entries.only., -Country) %>%
  left_join(., CoastLength, by = c('ISO3' = 'ISO3')) %>%
  select(-Country.Name) %>% 
  left_join(., EnvPerf, by = c('ISO3' = 'ISO3')) %>%
  select(-X, -Country) %>%
  left_join(., EstDis, by = c('ISO3' = 'ISO3')) %>%
  left_join(., HKExp, by = c('ISO3' = 'ISO3')) %>%
  select(-Country) %>%
  left_join(., Consumpt, by = c('ISO3' = 'ISO3')) %>%
  select(-country) %>%
  left_join(., GdpHdiOhi, by = c('ISO3' = 'ISO3')) %>%
  select(-Country) %>%
  left_join(., Iuu, by = c('ISO3' = 'ISO3')) %>%
  select(-Country, -UnreportedTonnes, -ReportedTonnes, -TotalTonnes) %>%
  left_join(., Mangrove, by = c('ISO3' = 'ISO3')) %>%
  select(-Country) %>%
  left_join(., ReefFishers, by = c('ISO3' = 'ISO3')) %>%
  select(-country) %>%
  left_join(., Saltmarsh, by = c('ISO3' = 'ISO3')) %>%
  left_join(., ChondLand, by = c('ISO3' = 'ISO3')) %>%
  select(-X, -Country, -TonnesChond, -TonnesElas) %>%
  left_join(., Wgi, by = c('ISO3' = 'ISO3')) %>%
  mutate(country = Country.x) %>%
  select(-Country.x) %>%
  distinct(ISO3, .keep_all = TRUE) %>%
  .[, c(40, 1, 2:39)]

write.csv(CountSum, 'CountryData_180921.csv')


# Combine covarites with sawfish occurrence data -----------------------------------------------------------

SppIso <- read_csv('CompleteSpeciesISO_180924.csv')

SppCov <-
  SppIso %>%
  left_join(., CountSum, by = c('ISO3' = 'ISO3')) %>%
  select(-country.y) %>%
  dplyr::rename(country = country.x) %>%
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
  mutate(SstMax = MAX_SST) %>%
  mutate(SstMin = MIN_SST) %>%
  dplyr::rename(ISO3 = ISO_TER1, occurrence = `PRESENCE`, country = `TERRITORY1`,
                SstMeanScale = MEAN_SST, SstMaxScale = MAX_SST, 
                SstMinScale = MIN_SST) %>%
  select(-Rowid, -FID, -BINOMIAL, -UNIQUE_FID, -GEONAME, -SOVEREIGN1,
         -FREQUENCY) %>%
  mutate_at('SstMeanScale', funs(scale(.) %>% as.vector)) %>%
  mutate_at('SstMaxScale', funs(scale(.) %>% as.vector)) %>%
  mutate_at('SstMinScale', funs(scale(.) %>% as.vector)) %>%
  mutate(matchid2 = paste(ISO3, species, sep = '-'))


AllCov <-
  SppCov %>%
  left_join(., sstDat, by = c('matchid' = 'matchid2')) %>%
  select(-country.y, -matchid, -occurrence.y, -ISO3.y, -species.y) %>%
  dplyr::rename(country = country.x, ISO3 = ISO3.x, species = species.x, 
         occurrence = occurrence.x) %>%
  mutate(refid2 = paste(ISO3, species, sep = '-')) %>%
  distinct(., refid2, .keep_all = TRUE) %>%
  select(-refid2)

write_csv(AllCov, 'CompleteSpeciesCovariates_181012.csv')


# Create generic dataframe that is not species specific --------------------------------

# Country specific ---------

# Calculate the mean of the SST for the country based on
# the geographic range of the species that resides there -------------------

sstCountryScaled <-
  sstDat %>%
  select(c(1:4, 7)) %>%
  filter(occurrence != '2') %>%
  mutate(geo = paste(country, ISO3, sep = '-')) %>%
  select(-country, -ISO3) %>%
  group_by(geo, occurrence) %>%
  summarise(SstCountMeanScale = mean(SstMeanScale), 
            spp = paste(species, collapse = '-'), 
            sppres = length(species)) %>%
  mutate(sppres = case_when(occurrence == '0' ~ 0,
                            TRUE ~ as.numeric(sppres))) %>%
  select(geo, SstCountMeanScale) %>%
  separate(col = geo, into = c('country', 'ISO3'), sep = '-')

sstCountryRaw <-
  sstDat %>%
  select(c(1:3, 8, 7)) %>%
  filter(occurrence != '2') %>%
  mutate(geo = paste(country, ISO3, sep = '-')) %>%
  select(-country, -ISO3) %>%
  group_by(geo, occurrence) %>%
  summarise(SstCountMean = mean(SstMean), 
            spp = paste(species, collapse = '-'), 
            sppres = length(species)) %>%
  mutate(sppres = case_when(occurrence == '0' ~ 0,
                            TRUE ~ as.numeric(sppres))) %>%
  select(geo, SstCountMean) %>%
  separate(col = geo, into = c('country', 'ISO3'), sep = '-') %>%
  distinct(., country, .keep_all = TRUE)



head(sstCountry)
View(sstCountryRaw)

NoSpp <-
  AllCov %>%
  select(c(1:7)) %>%
  select(-c(1, 3, 7), -region, -presence, -country) %>%
  filter(occurrence !='2') %>%
  group_by(ISO3, occurrence) %>%
  summarise(spp = paste(species, collapse = '-'),
            sppres = length(species)) %>%
  mutate(sppres = case_when(occurrence == '0' ~ 0,
                            TRUE ~ as.numeric(sppres))) %>%
  rename(species = spp, nospecies = sppres) %>%
  arrange(desc(nospecies)) %>%
  distinct(., ISO3, .keep_all = TRUE) %>%
  left_join(., sstCountryRaw, by = c('ISO3' = 'ISO3')) %>%
  .[, c(5, 1, 3, 4, 2, 6)] %>%
  left_join(., sstCountryScaled, by = c('ISO3' = 'ISO3')) %>%
  select(-country.y) %>%
  rename(country = country.x) %>%
  left_join(., CountSum, by = c('ISO3' = 'ISO3')) %>%
  select(-country.y) %>%
  rename(country = country.x) %>%
  distinct(., ISO3, .keep_all = TRUE)

write_csv(NoSpp, 'CountryCovariates_181022.csv')











