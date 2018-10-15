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
  bind_rows(spp[[2]], spp[[3]], spp[[4]], spp[[5]], spp[[6]], spp[[7]], spp[[8]], spp[[9]]) %>%
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
                                      `Colombia (ATL)` = 'Colombia', `Colombia (ETP)` = 'Colombia',
                                      `Costa Rica (ATL)` = 'Costa Rica', `Costa Rica (ETP)` = 'Costa Rica',
                                      `Guatemala (ATL)` = 'Guatemala', `Guatemala (ETP)` = 'Guatemala',
                                      `Mexico (ATL)` = 'Mexico', `Mexico (ETP)` = 'Mexico',
                                      `Nicaragua (ATL)` = 'Nicaragua', `Nicaragua (ETP)` = 'Nicaragua',
                                      `Panama (ATL)` = 'Panama', `Panama (ETP)` = 'Panama',
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
