# THIS SCRIPT CLEANS AND COMBINES THE DATA FROM THE WORKSHOP #

setwd('/users/helenyan/desktop/school/directed studies 2018/datasets')

library(readxl)
library(tidyverse)

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


# Combine species occurrences with ISO data -------------------------------------------------------------------

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
