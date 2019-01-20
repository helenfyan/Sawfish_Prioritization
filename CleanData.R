# THIS SCRIPT CLEANS AND COMBINES THE DATA FROM THE WORKSHOP #

setwd('/users/helenyan/desktop/school/directed studies 2018/datasets/')

library(tidyverse)
library(countrycode)

# Clean occurrence data -------------------------------------------------------------

library(readxl)

sppSheets <- excel_sheets('Sawfish prioritization scores 31 Oct.xlsx')

spp <-
  lapply(sppSheets, function(sheet) {
    df <- read_excel('Sawfish prioritization scores 31 Oct.xlsx', sheet = sheet) %>%
      mutate(species = sheet) %>%
      slice(., 3:n()) %>%
      drop_na(X__3) %>%
      select(X__1, X__2, X__3, species) %>%
      dplyr::rename(country = X__1, region = X__2, occurrence = X__3) %>%
      mutate(presence = occurrence) %>%
      mutate(presence = dplyr::recode(presence, `3` = 'present', 
                                      `2` = 'unknown', 
                                      `1` = 'absent')) %>%
      mutate(occurrence = dplyr::recode(occurrence, `3` = 1, `2` = 2, `1` = 0)) %>%
      drop_na(occurrence)
    return(df)
  })

AllSpecies <-
  bind_rows(spp[[2]], spp[[3]], spp[[4]], spp[[5]], 
            spp[[6]], spp[[7]], spp[[8]], spp[[9]]) %>%
  mutate(species = as.factor(species)) %>%
  mutate(species = dplyr::recode(species, 'LT_WA' = 'large',
                                 'LT_EA' = 'large',
                                 'LT_IWP' = 'large', 
                                 'LT_EP' = 'large')) %>%
  rename(countryUnstd = country)

FormalData <-
  AllSpecies %>%
  select(-occurrence) %>%
  rename(country = countryUnstd) %>%
  mutate(species = recode(species, 'large' = 'Pristis pristis',
                          'small' = 'Pristis pectinata',
                          'green' = 'Pristis zijsron',
                          'narrow' = 'Anoxypristis cuspidata',
                          'dwarf' = 'Pristis clavata'))

write_csv(FormalData, 'SawfishOccurrence_181015.csv')


# Combine species occurrences with ISO data ---------------------------------------------------------

iso <- read_csv('CountryISO3.csv')


AllSpeciesISO <-
  AllSpecies %>%
  mutate(countryUnstd = dplyr::recode(countryUnstd, `Bahamas, The` = 'Bahamas',
                                      `Colombia (ATL)` = 'Colombia', 
                                      `Colombia (ETP)` = 'Colombia',
                                      `Costa Rica (ATL)` = 'Costa Rica', 
                                      `Costa Rica (ETP)` = 'Costa Rica',
                                      `Guatemala (ATL)` = 'Guatemala', 
                                      `Guatemala (ETP)` = 'Guatemala',
                                      `Mexico (ATL)` = 'Mexico', 
                                      `Mexico (ETP)` = 'Mexico',
                                      `Nicaragua (ATL)` = 'Nicaragua', 
                                      `Nicaragua (ETP)` = 'Nicaragua',
                                      `Panama (ATL)` = 'Panama', 
                                      `Panama (ETP)` = 'Panama',
                                      'Saint Vinctente and Grenadines' = 'Saint Vincent and Grenadines',
                                      `Congo, Democratic Republic of` = 'DR Congo', 
                                      `Congo, Republic of` = 'R Congo',
                                      'Domincia' = 'Dominica')) %>%
  left_join(., iso, by = c('countryUnstd' = 'Country')) %>%
  rename(ISO3 = ISO, country = countryUnstd) %>%
  .[c(1, 6, 2, 4, 5, 3)] %>%
  mutate(refID = paste(ISO3, region, species, sep = '')) %>%
  drop_na(refID)


# Remove the repetitions per species per country ----------------------------------------

large <- 
  AllSpeciesISO %>%
  filter(species == 'large') %>%
  distinct(country, .keep_all = TRUE)

narrow <-
  AllSpeciesISO %>%
  filter(species == 'narrow') 

green <-
  AllSpeciesISO %>%
  filter(species == 'green')

dwarf <-
  AllSpeciesISO %>%
  filter(species == 'dwarf')

small <-
  AllSpeciesISO %>%
  filter(species == 'small')

allClean <-
  rbind(large, narrow, green, dwarf, small)

write_csv(allClean, 'CompleteSpeciesISO_180924.csv')  

# Clean and merge all SAU fishing gear data ----------------------------------------------

sau1 <- read_csv('SAU/SAU Fishing Gear/SAU EEZ01/SAUEEZ01.csv')
sau2 <- read_csv('SAU/SAU Fishing Gear/SAU EEZ02/SAUEEZ02.csv')
sau3 <- read_csv('SAU/SAU Fishing Gear/SAU EEZ03/SAUEEZ03.csv')
sau4 <- read_csv('SAU/SAU Fishing Gear/SAU EEZ04/SAUEEZ04.csv')
sau5 <- read_csv('SAU/SAU Fishing Gear/SAU EEZ05/SAUEEZ05.csv')
sau6 <- read_csv('SAU/SAU Fishing Gear/SAU EEZ06/SAUEEZ06.csv')
sau7 <- read_csv('SAU/SAU Fishing Gear/SAU EEZ07/SAUEEZ07.csv')
sau8 <- read_csv('SAU/SAU Fishing Gear/SAU EEZ08/SAUEEZ08.csv')
sau9 <- read_csv('SAU/SAU Fishing Gear/SAU EEZ09/SAUEEZ09.csv')
sau10 <- read_csv('SAU/SAU Fishing Gear/SAU EEZ10/SAUEEZ10.csv')
sau11 <- read_csv('SAU/SAU Fishing Gear/SAU EEZ11/SAUEEZ11.csv')
sau12 <- read_csv('SAU/SAU Fishing Gear/SAU EEZ12/SAUEEZ12.csv')
sau13 <- read_csv('SAU/SAU Fishing Gear/SAU EEZ13/SAUEEZ13.csv')
sau14 <- read_csv('SAU/SAU Fishing Gear/SAU EEZ14/SAUEEZ14.csv')
sau15 <- read_csv('SAU/SAU Fishing Gear/SAU EEZ15/SAUEEZ15.csv')


SauClean <-
  rbind(sau1, sau2, sau3, sau4, sau5, sau6, sau7, sau8, sau9, sau10, sau11, sau12,
        sau13, sau14, sau15) %>%
  dplyr::filter((gear_type == 'bottom trawl') |
                  (gear_type == 'gillnet') |
                  (gear_type == 'otter trawl') |
                  (gear_type == 'shrimp trawl') |
                  (gear_type == 'small scale gillnets') |
                  (gear_type == 'small scale longline') |
                  (gear_type == 'small scale trammel net') |
                  (gear_type == 'trammel nets')) %>%
  droplevels(.) %>%
  select(-fishing_entity, -data_layer, -uncertainty_score, -area_type) %>%
  mutate(area_name = dplyr::recode(area_name, 'Congo (ex-Zaire)' = 'DR Congo',
                                   `Côte d'Ivoire` = "Cote d'Ivoire",
                                   `Guatemala (Caribbean)` = 'Guatemala',
                                   `Guatemala (Pacific)` = 'Guatemala',
                                   `Howland & Baker Isl. (USA)` = 'USA',
                                   `Jarvis Isl. (USA)` = 'USA',
                                   `Johnston Atoll (USA)` = 'USA',
                                   `Mexico (Atlantic)` = 'Mexico',
                                   `Mexico (Pacific)` = 'Mexico',
                                   `Oman (Musandam)` = 'Oman',
                                   `Palmyra Atoll & Kingman Reef (USA)` = 'USA',
                                   `Panama (Caribbean)` = 'Panama',
                                   `Panama (Pacific)` = 'Panama',
                                   `Saudi Arabia (Persian Gulf)` = 'Saudi Arabia',
                                   `Saudi Arabia (Red Sea)` = 'Saudi Arabia',
                                   `South Africa (Atlantic and Cape)` = 'South Africa',
                                   `South Africa (Indian Ocean Coast)` = 'South Africa',
                                   `Thailand (Andaman Sea)` = 'Thailand',
                                   `Thailand (Gulf of Thailand)` = 'Thailand',
                                   `USA (Alaska, Arctic)` = 'USA',
                                   `USA (Alaska, Subarctic)` = 'USA',
                                   `USA (East Coast)` = 'USA',
                                   `USA (Gulf of Mexico)` = 'USA',
                                   `USA (West Coast)` = 'USA',
                                   `Wake Isl. (USA)` = 'USA',
                                   `Yemen (Arabian Sea)` = 'Yemen',
                                   `Yemen (Red Sea)` = 'Yemen',
                                   `Brazil (mainland)` = 'Brazil',
                                   `Colombia (Caribbean)` = 'Colombia',
                                   `Colombia (Pacific)` = 'Colombia',
                                   `Costa Rica (Caribbean)` = 'Costa Rica',
                                   `Costa Rica (Pacific)` = 'Costa Rica',
                                   `Ecuador (mainland)` = 'Ecuador',
                                   `Egypt (Mediterranean)` = 'Egypt',
                                   `Egypt (Red Sea)` = 'Egypt',
                                   `Honduras (Caribbean)` = 'Honduras',
                                   `Honduras (Pacific)` = 'Honduras',
                                   `Hong Kong (China)` = 'China',
                                   `India (mainland)` = 'India',
                                   `Indonesia (Central)` = 'Indonesia',
                                   `Indonesia (Eastern)` = 'Indonesia',
                                   `Indonesia (Indian Ocean)` = 'Indonesia',
                                   `Iran (Persian Gulf)` = 'Iran',
                                   `Iran (Sea of Oman)` = 'Iran',
                                   `Japan (Daito Islands)` = 'Japan',
                                   `Japan (main islands)` = 'Japan',
                                   `Japan (Ogasawara Islands)` = 'Japan',
                                   `Malaysia (Peninsula East)` = 'Malaysia',
                                   `Malaysia (Peninsula West)` = 'Malaysia',
                                   `Malaysia (Sabah)` = 'Malaysia',
                                   `Malaysia (Sarawak)` = 'Malaysia',
                                   `Morocco (Central)` = 'Morocco',
                                   `Morocco (Mediterranean)` = 'Morocco',
                                   `Morocco (South)` = 'Morocco',
                                   `Nicaragua (Caribbean)` = 'Nicaragua',
                                   `Nicaragua (Pacific)` = 'Nicaragua',
                                   `United Arab Emirates (Fujairah)` = 'United Arab Emirates')) %>%
  left_join(., iso, by = c('area_name' = 'Country')) %>%
  dplyr::group_by(ISO) %>%
  dplyr::summarise(totalGearTonnes = sum(tonnes),
                   totalGearValue = sum(landed_value))

write.csv(SauClean, 'SauFishingGear_181109.csv')


# clean consumption data ------------------------------------------------

faoRaw <- read_csv('./FAO Consumption/FAOConsumptRaw_180513.csv')

faoClean <-
  faoRaw %>%
  dplyr::select(Country, Element, Item, Year, Unit, Value) %>%
  filter(Element == 'Protein supply quantity (g/capita/day)') %>%
  mutate(Country = dplyr::recode(Country, 
                                 `China, Hong Kong SAR` = 'China',
                                 `China, Macao SAR` = 'China',
                                 `China, mainland` = 'China',
                                 `China, Taiwan Province of` = 'Taiwan',
                                 `Congo` = 'DR Congo',
                                 `Iran (Islamic Republic of)` = 'Iran',
                                 `Venezuela (Bolivarian Republic of)` = 'Venezuela',
                                 `Democratic People's Republic of Korea` = 'Korea (North)',
                                 `Antigua and Barbuda` = 'Antigua & Barbuda',
                                 `Côte d'Ivoire` = "Cote d'Ivoire",
                                 `Republic of Korea` = 'Korea (South)',
                        `Saint Vincent and the Grenadines` = 'Saint Vincent & the Grenadines',
                                 `Timor-Leste` = 'Timor Leste',
                                 `Trinidad and Tobago` = 'Trinidad & Tobago',
                                 `United States of America` = 'USA')) %>%
  group_by(Country) %>%
  summarise(ProteinDiet = mean(Value)) %>%
  left_join(., iso, by = c('Country' = 'Country'))

write.csv(faoClean, 'FaoConsumptionClean_181109.csv')


# clean IUU data ------------------------------------------------

iuu1 <- read_csv('./SAU/SAU IUU Fishing/SAUFishRep1.csv')
iuu2 <- read_csv('./SAU/SAU IUU Fishing/SAUFishRep2.csv')
iuu3 <- read_csv('./SAU/SAU IUU Fishing/SAUFishRep3.csv')
iuu4 <- read_csv('./SAU/SAU IUU Fishing/SAUFishRep4.csv')
iuu5 <- read_csv('./SAU/SAU IUU Fishing/SAUFishRep5.csv')
iuu6 <- read_csv('./SAU/SAU IUU Fishing/SAUFishRep6.csv')
iuu7 <- read_csv('./SAU/SAU IUU Fishing/SAUFishRep7.csv')
iuu8 <- read_csv('./SAU/SAU IUU Fishing/SAUFishRep8.csv')
iuu9 <- read_csv('./SAU/SAU IUU Fishing/SAUFishRep9.csv')
iuu10 <- read_csv('./SAU/SAU IUU Fishing/SAUFishRep10.csv')
iuu11 <- read_csv('./SAU/SAU IUU Fishing/SAUFishRep11.csv')
iuu12 <- read_csv('./SAU/SAU IUU Fishing/SAUFishRep12.csv')
iuu13 <- read_csv('./SAU/SAU IUU Fishing/SAUFishRep13.csv')
iuu14 <- read_csv('./SAU/SAU IUU Fishing/SAUFishRep14.csv')


IuuClean <-
  rbind(iuu1, iuu2, iuu3, iuu4, iuu5, iuu6, iuu7, iuu8, iuu9, iuu10,
        iuu11, iuu12, iuu13, iuu14) %>%
  select(area_name, reporting_status, tonnes) %>%
  filter(reporting_status == 'Unreported') %>%
  mutate(area_name = dplyr::recode(area_name, 'Congo (ex-Zaire)' = 'DR Congo',
                                 `Côte d'Ivoire` = "Cote d'Ivoire",
                                 `Guatemala (Caribbean)` = 'Guatemala',
                                 `Guatemala (Pacific)` = 'Guatemala',
                                 `Howland & Baker Isl. (USA)` = 'USA',
                                 `Jarvis Isl. (USA)` = 'USA',
                                 `Johnston Atoll (USA)` = 'USA',
                                 `Mexico (Atlantic)` = 'Mexico',
                                 `Mexico (Pacific)` = 'Mexico',
                                 `Oman (Musandam)` = 'Oman',
                                 `Palmyra Atoll & Kingman Reef (USA)` = 'USA',
                                 `Panama (Caribbean)` = 'Panama',
                                 `Panama (Pacific)` = 'Panama',
                                 `Saudi Arabia (Persian Gulf)` = 'Saudi Arabia',
                                 `Saudi Arabia (Red Sea)` = 'Saudi Arabia',
                                 `South Africa (Atlantic and Cape)` = 'South Africa',
                                 `South Africa (Indian Ocean Coast)` = 'South Africa',
                                 `Thailand (Andaman Sea)` = 'Thailand',
                                 `Thailand (Gulf of Thailand)` = 'Thailand',
                                 `USA (Alaska, Arctic)` = 'USA',
                                 `USA (Alaska, Subarctic)` = 'USA',
                                 `USA (East Coast)` = 'USA',
                                 `USA (Gulf of Mexico)` = 'USA',
                                 `USA (West Coast)` = 'USA',
                                 `Wake Isl. (USA)` = 'USA',
                                 `Yemen (Arabian Sea)` = 'Yemen',
                                 `Yemen (Red Sea)` = 'Yemen',
                                 `Brazil (mainland)` = 'Brazil',
                                 `Colombia (Caribbean)` = 'Colombia',
                                 `Colombia (Pacific)` = 'Colombia',
                                 `Costa Rica (Caribbean)` = 'Costa Rica',
                                 `Costa Rica (Pacific)` = 'Costa Rica',
                                 `Ecuador (mainland)` = 'Ecuador',
                                 `Egypt (Mediterranean)` = 'Egypt',
                                 `Egypt (Red Sea)` = 'Egypt',
                                 `Honduras (Caribbean)` = 'Honduras',
                                 `Honduras (Pacific)` = 'Honduras',
                                 `Hong Kong (China)` = 'China',
                                 `India (mainland)` = 'India',
                                 `Indonesia (Central)` = 'Indonesia',
                                 `Indonesia (Eastern)` = 'Indonesia',
                                 `Indonesia (Indian Ocean)` = 'Indonesia',
                                 `Iran (Persian Gulf)` = 'Iran',
                                 `Iran (Sea of Oman)` = 'Iran',
                                 `Japan (Daito Islands)` = 'Japan',
                                 `Japan (main islands)` = 'Japan',
                                 `Japan (Ogasawara Islands)` = 'Japan',
                                 `Malaysia (Peninsula East)` = 'Malaysia',
                                 `Malaysia (Peninsula West)` = 'Malaysia',
                                 `Malaysia (Sabah)` = 'Malaysia',
                                 `Malaysia (Sarawak)` = 'Malaysia',
                                 `Morocco (Central)` = 'Morocco',
                                 `Morocco (Mediterranean)` = 'Morocco',
                                 `Morocco (South)` = 'Morocco',
                                 `Nicaragua (Caribbean)` = 'Nicaragua',
                                 `Nicaragua (Pacific)` = 'Nicaragua',
                                 `United Arab Emirates (Fujairah)` = 'United Arab Emirates',
                                 `Galapagos Isl. (Ecuador)` = 'Ecuador',
                                 `Hawaii Main Islands (USA)` = 'USA',
                                 `Hawaii Northwest Islands (USA)` = 'USA',
                                 `Prince Edward Isl. (South Africa)` = 'South Africa')) %>% 
  group_by(area_name) %>%
  summarise(Iuu = sum(tonnes)) %>%
  left_join(., iso, by = c('area_name' = 'Country')) %>%
  drop_na()

write.csv(IuuClean, 'IuuClean_181109.csv')


# clean coastal population data ------------------------------------------------

CoastPop <- read_csv('CoastalPopulation_180530.csv')

CoastPopClean <-
  CoastPop %>%
  select(Country, coastal.pop) %>%
  rename(CoastPop = coastal.pop) %>%
  mutate(Country = dplyr::recode(Country,
                               `East Timor` = 'Timor Leste',
                               `Antigua and Barbuda` = 'Antigua & Barbuda',
                               `Congo, Dem. Rep. of the` = 'DR Congo',
                               `Congo, Republic of` = 'Congo, R. of',
                               `Cote D'Ivoire` = "Cote d'Ivoire",
                               `Korea, Dem. People's Rep. of` = 'Korea (North)',
                               `Korea, Republic of` = 'Korea (South)',
                               `Saint Vincent and Grenadines` = 'Saint Vincent & the Grenadines',
                               `Trinidad and Tobago` = 'Trinidad & Tobago',
                               `United States of America` = 'USA',
                               `Solomon Islands` = 'Solomon Isl.')) %>%
  left_join(., iso, by = c('Country' = 'Country'))

write.csv(CoastPopClean, 'CoastalPopClean_181109.csv')


# FAO Shark fin data ----------------------------------------------------------
finsRaw <- read.csv('FAOChondExImports_181116.csv')

# change years to numeric
years <- (1976:2016)
cols <- names(finsRaw)
colsYears <- cols[5:45]

fins <-
  finsRaw %>%
  rename_at(vars(colsYears), ~ years) %>%
  rename(country = `Country..Country.`,
         productRaw = `Commodity..Commodity.`,
         trade = `Trade.flow..Trade.flow.`) %>%
  na_if(., '...') %>%
  na_if(., '-') %>%
  gather(key = year, value = value, '1976':'2016') %>%
  drop_na(value) %>%
  mutate(value = as.numeric(value)) %>%
  # convert 0 0 to NA to 1
  mutate(value = case_when(is.na(value) ~ 1, 
                           value == 0 ~ 1,
                           TRUE ~ as.numeric(value))) %>%
  mutate(value = value*1000) %>%
  dplyr::rename(unit = Unit) %>%
  mutate(year = as.numeric(year)) %>%
  # only want exports
  filter(trade == 'Exports') %>%
  mutate(product = 
        case_when(productRaw == 'Shark fins, dried, unsalted' ~ 'fins', 
                  productRaw == 'Shark fins, dried, whether or not salted, etc.' ~ 'fins',
                  productRaw == 'Shark fins, fresh or chilled' ~ 'fins',
                  productRaw == 'Shark fins, frozen' ~ 'fins',
                  productRaw == 'Shark fins, salted and in brine but not dried or smoked' ~ 'fins',
                  productRaw == 'Shark fins, prepared or preserved' ~ 'fins',
                  TRUE ~ 'nonfins')) %>%
  # add HK to China's total but remove Macao because they have data from the same year 
  mutate(value = 
           case_when(country == 'China' & year == 2016 & product == 'fins' ~ 9206000,
                     TRUE ~ as.numeric(value))) %>%
  mutate(country = dplyr::recode(country, 
                                 `China, Hong Kong SAR` = 'Hong Kong',
                                 `China, Macao SAR` = 'Macao',
                                 `Congo` = 'R Congo',
                                 `Congo, Dem. Rep. of the` = 'DR Congo',
                                 `Cura\xe7ao` = 'Curacao',
                                 `Iran (Islamic Rep. of)` = 'Iran',
                                 `Korea, Dem. People's Rep` = 'Korea (North)',
                                 `Korea, Republic of` = 'Korea (South)',
                                 `Saint Vincent/Grenadines` = 'Saint Vincent and Grenadines',
                                 `Taiwan Province of China` = 'Taiwan',
                                 `Tanzania, United Rep. of` = 'Tanzania',
                                 `Timor-Leste` = 'Timor Leste',
                                 `Venezuela, Boliv Rep of` = 'Venezuela')) %>%
  left_join(., iso, by = c('country' = 'Country')) %>%
  group_by(country, product) %>%
  slice(which.max(year)) %>%
  select(-productRaw, -trade) %>%
  mutate(unit = 'USD-Exports') %>%
  .[, c(1, 6, 2:5)]

write.csv(fins, 'ChondExpsClean_181119.csv')

# Clean Chond Landings data ----------------------------------------------------------------

landRaw <- read_csv('FAOChondLand_181116.csv')

cols <- names(landRaw)

# sum the catch data from 2003-2016
landsum <- 
  landRaw %>% 
  dplyr::rename(country = `Country (Country)`,
               species = `Species (ASFIS species)`,
               area = `Fishing area (FAO major fishing area)`,
               unit = `Unit (Unit)`) %>% 
  gather(key = year, value = value, '1950':'2016') %>% 
  na_if(., '...') %>% 
  na_if(., '-') %>% 
  #replace(., is.na(.), 0) %>% 
  drop_na(value) %>% 
  mutate(value = dplyr::recode(value, '0 0' = '0')) %>% 
  separate(value, into = c('value', 'val2'), sep = ' ') %>% 
  select(-val2) %>% 
  mutate(value = as.numeric(value)) %>% 
  mutate(year = as.numeric(year)) %>% 
  filter(year > 2002) %>% 
  mutate(year = paste('X', year, sep = '')) %>% 
  group_by(country) %>% 
  summarise(totalCatch = sum(value)) %>% 
  mutate(ISO3 = countrycode(landsum$country, 'country.name', 'iso3c')) %>% 
  drop_na() %>% 
  .[, c(1, 3, 2)]

write.csv(landsum, 'FAOChondCatch_190119.csv')


land <-
  landRaw %>%
  dplyr::rename(country = `Country (Country)`,
                species = `Species (ASFIS species)`,
                area = `Fishing area (FAO major fishing area)`,
                unit = `Unit (Unit)`) %>%
  gather(key = year, value = value, '1950':'2016') %>%
  na_if(., '...') %>%
  na_if(., '-') %>%
  mutate(value = as.numeric(value)) %>%
  drop_na(value) %>%
  mutate(country = dplyr::recode(country,
                                 # don't inclue HK to China because dates are too different
                                 `China, Hong Kong SAR` = 'Hong Kong',
                                 `China, Macao SAR` = 'Macao',
                                 `Congo` = 'R Congo',
                                 `Congo, Dem. Rep. of the` = 'DR Congo',
                                 `Cura\xe7ao` = 'Curacao',
                                 `Iran (Islamic Rep. of)` = 'Iran',
                                 `Korea, Dem. People's Rep` = 'Korea (North)',
                                 `Korea, Republic of` = 'Korea (South)',
                                 `Saint Vincent/Grenadines` = 'Saint Vincent and Grenadines',
                                 `Taiwan Province of China` = 'Taiwan',
                                 `Tanzania, United Rep. of` = 'Tanzania',
                                 `Timor-Leste` = 'Timor Leste',
                                 `Venezuela, Boliv Rep of` = 'Venezuela')) %>%
  # create a total across species for each year for each country
  group_by(country, year) %>%
  summarise(total = sum(value)) %>%
  slice(which.max(year)) %>%
  mutate(unit = 'tonnes') %>%
  left_join(., iso, by = c('country' = 'Country')) %>%
  .[, c(1, 5, 2:4)]

write.csv(land, 'ChondLandClean_181119.csv')

# Clean total fishery production data ----------------------------------------

prodRaw <- read_csv('FAOProducts_181116.csv')

prod <-
  prodRaw %>%
  dplyr::rename(country = `Country (Country)`,
                species = `Group of species (Group of species)`,
                element = `Element (Element)`,
                unit = `Unit (Unit)`) %>%
  gather(key = year, value = value, '1961':'2013') %>%
  na_if(., '...') %>%
  mutate(value = as.numeric(value)) %>%
  filter(element == 'Production') %>%
  mutate(country = dplyr::recode(country,
                                 # don't inclue HK to China because dates are too different
                                 `China, Hong Kong SAR` = 'Hong Kong',
                                 `China, Macao SAR` = 'Macao',
                                 `Congo` = 'R Congo',
                                 `Congo, Dem. Rep. of the` = 'DR Congo',
                                 `Cura\xe7ao` = 'Curacao',
                                 `Iran (Islamic Rep. of)` = 'Iran',
                                 `Korea, Dem. People's Rep` = 'Korea (North)',
                                 `Korea, Republic of` = 'Korea (South)',
                                 `Saint Vincent/Grenadines` = 'Saint Vincent and Grenadines',
                                 `Taiwan Province of China` = 'Taiwan',
                                 `Tanzania, United Rep. of` = 'Tanzania',
                                 `Timor-Leste` = 'Timor Leste',
                                 `Venezuela, Boliv Rep of` = 'Venezuela')) %>%
  drop_na(value) %>%
  # calculate the sum per year per country
  group_by(country, year) %>%
  summarise(total = sum(value)) %>%
  slice(which.max(year)) %>%
  # combine china with hong kong because they have data from the same year 
  mutate(total = case_when(country == 'China' ~ 37560261,
                           TRUE ~ as.numeric(total))) %>%
  left_join(., iso, by = c('country' = 'Country')) %>%
  mutate(unit = 'productionTonnes')

write.csv(prod, 'FisheryProductionClean_181119.csv')


# Create .csv file for each spp EOO ----------------------------------------

all <- read_csv('CompleteSpeciesISO_180924.csv')

large <- 
  all %>% 
  dplyr::filter(species == 'large')

write.csv(large, 'EOOLarge_190114.csv')

small <- 
  all %>% 
  dplyr::filter(species == 'small')

write.csv(small, 'EOOSmall_190114.csv')

dwarf <- 
  all %>% 
  dplyr::filter(species == 'dwarf')

write.csv(dwarf, 'EOODwarf_190114.csv')

narrow <- 
  all %>% 
  dplyr::filter(species == 'narrow')

write.csv(narrow, 'EOONarrow_190114.csv')

green <- 
  all %>% 
  dplyr::filter(species == 'green')

write.csv(green, 'EOOGreen_190114.csv')


# SAUP catch data from Lindsay ----------------------------------------

sau <- read_csv('LDavidson/ForLindsay_Chondrichthyes_EEZ_160716.csv')
tax <- read_csv('LDavidson/Chondrichthyes_Taxa_original.csv')

# get country and last year of entry
maxyear <- 
  sau %>% 
  group_by(name) %>% 
  slice(which.max(year))

# entries with correct max year
maxyearonly <- 
  maxyear %>% 
  select(name, year, catch_sum, ISO) %>% 
  left_join(., sau, by = c('name', 'year'))

# sum of catch for each country with max year
saufinal <- 
  maxyearonly %>% 
  group_by(name, ISO.x) %>% 
  summarise(sum.catch = sum(catch_sum.y)) %>% 
  ungroup()

write.csv(saufinal, 'LDavidsonCatch_190115.csv')
