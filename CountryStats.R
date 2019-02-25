# This script gets all the summarised statistics for each country

library(tidyverse)
library(countrycode)
setwd('/users/helenyan/desktop/school/directed studies 2018/datasets')

# ---------------------------------------------------------------------------------------------------
# Number of individuals lost from a single country etc. ---------------------------------------------
# ---------------------------------------------------------------------------------------------------

allsppraw <- read_csv('SawfishOccurrence_181015.csv')

allspp <- 
  allsppraw %>% 
  mutate(ISO3 = countrycode(country, 'country.name', 'iso3c')) %>% 
  mutate(ISO3 = case_when(country == 'Domincia' ~ 'DMA',
                          country == 'Saint Vinctente and Grenadines' ~ 'VCT', 
                          TRUE ~ as.character(ISO3))) %>% 
  separate(species, into = c('genus', 'species')) %>% 
  # remove unknown countries
  #dplyr::filter(presence != 'unknown') %>% 
  dplyr::select(-country, -genus, -region) %>% 
  mutate(nosppHist = 1) %>% 
  mutate(nosppNow = case_when(presence == 'present' ~ 1,
                              presence == 'absent' ~ 0,
                              presence == 'unknown' ~ 10))

head(allspp)
lapply(allspp, function(x) sum(is.na(x)))

# number of countries which had sawfish ----------------------------
# this includes Taiwan and Viet Nam separately 
allcountries <- 
  allspp %>% 
  distinct(ISO3, .keep_all = TRUE)

nrow(allcountries)

# number of species currently in each country ---------------------

# number of countries which have lost species ---------------------
# historial number in each country
hist <- 
  allspp %>%
  group_by(ISO3) %>% 
  summarise(hist = sum(nosppHist))

head(hist)

# number of species lost
lost <-
  allspp %>% 
  dplyr::filter(presence != 'unknown') %>% 
  group_by(ISO3) %>% 
  summarise(present = sum(nosppNow))

head(lost)

# change in species 
changespp <- 
  hist %>%
  left_join(., lost, by = c('ISO3' = 'ISO3')) %>% 
  drop_na()


# countries where sawfishes are completely extinct 
allextinct <- 
  changespp %>% 
  filter(present == 0)

nrow(allextinct)

# countries where at least one species remains 
somepres <- 
  changespp %>% 
  filter(present != 0) %>% 
  mutate(change = present - hist) %>% 
  filter(change != 0)

head(somepres)

# at least one species lost
onelost <- 
  somepres %>% 
  filter(change == -1)

# more than one species lost 
morelost <- 
  somepres %>% 
  filter(change != -1)

# data-deficients -----------------------------------------------
dd <- 
  allspp %>%
  dplyr::filter(presence == 'unknown') %>% 
  group_by(ISO3) %>% 
  summarise(nospp10 = sum(nosppNow)) %>% 
  mutate(nospp = nospp10/10) %>% 
  dplyr::select(-nospp10)

head(dd)

unknown <- 
  hist %>%
  left_join(., dd, by = c('ISO3' = 'ISO3')) %>% 
  drop_na() %>% 
  mutate(change = hist - nospp)

head(unknown)

# data deficients - don't know if any given species is present
allunknown <- 
  unknown %>% 
  filter(change == 0)
nrow(allunknown)

# data deficients - don't know if specific species is present
singleunknown <- 
  unknown %>% 
  filter(change != 0)

nrow(singleunknown)
