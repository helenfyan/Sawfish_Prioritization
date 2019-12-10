# This script gets all the summarised statistics for each country

library(tidyverse)
library(countrycode)
library(broom)

# ---------------------------------------------------------------------------------------------------
# Number of individuals lost from a single country etc. ---------------------------------------------
# ---------------------------------------------------------------------------------------------------

allsppraw <- read_csv('../../../Datasets/SawfishOccurrence_181015.csv')

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
                              presence == 'unknown' ~ 10)) %>% 
  # duplicates removal
  mutate(refid = paste(species, ISO3, presence, sep = '_')) %>% 
  distinct(refid, .keep_all = TRUE)

head(allspp)
lapply(allspp, function(x) sum(is.na(x)))

# clean dataframe for danielle to analyze ----------------------
# make a column for predictions 
pred_raw <- read_csv('../../../Datasets/GBMPredictedMeans_190122.csv') %>% 
  dplyr::select(-X1)

pred <- 
  pred_raw %>% 
  mutate(action = case_when(ISO3 == 'YEM' ~ 'middle',
                            value > 0.75 ~ 'protect',
                            value <= 0.25 ~ 'extinct',
                            TRUE ~ as.character('middle'))) %>% 
  dplyr::filter(action != 'middle') %>% 
  dplyr::select(-value)

head(pred)

area_calc <- 
  allspp %>% 
  dplyr::select(-refid, -nosppNow, -nosppHist) %>% 
  dplyr::filter(ISO3 != 'LAO') %>% 
  spread(species, presence) %>% 
  left_join(., pred, by = c('ISO3' = 'ISO3'))


head(area_calc)
View(area_calc)

write_csv(area_calc, '../../../Datasets/SawfishOccurrence_191209.csv')

# historical distribution of sawfishes
hist <- 
  allspp %>% 
  group_by(ISO3) %>% 
  summarise(hist_spp = sum(nosppHist))

# number of species still present 
pres_still <- 
  allspp %>% 
  dplyr::filter(presence != 'unknown') %>% 
  group_by(ISO3) %>% 
  summarise(present_spp = sum(nosppNow))

# species lost
lost <- 
  hist %>% 
  left_join(., pres_still, by = c('ISO3' = 'ISO3')) %>% 
  mutate(lost = hist_spp - present_spp)

# number of unknowns
unknwn <- 
  allspp %>% 
  dplyr::filter(presence == 'unknown') %>% 
  group_by(ISO3) %>% 
  summarise(unknown_spp = sum(nosppHist))

# join columns together to make one dataframe
spp_change <- 
  lost %>%
  left_join(., unknwn, by = c('ISO3', 'ISO3')) %>% 
  # replace all NAs with zeros
  mutate_all(funs(replace(., is.na(.), 0)))


lapply(spp_change, function(x) sum(is.na(x)))

write_csv(spp_change, '../../../Datasets/Publication Maps/SpeciesChange_190718.csv')

# more random stats -------------------------------------------------------

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
  filter(change != -1) %>% 
  mutate(country = countrycode(ISO3, 'iso3c', 'country.name'))

# ---------------------------------------------------------------------------------------------------
# Data-deficient countries --------------------------------------------------------------------------
# ---------------------------------------------------------------------------------------------------
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

# Make GIS dataframe for the number of unknown species 
ddMap <- 
  changespp %>% 
  # join to other dataframe to get all 91 countries in list
  left_join(., unknown, by = c('ISO3' = 'ISO3')) %>% 
  select(-hist.x, -present, -change, -hist.y) %>% 
  rename(noDDspp = nospp) %>% 
  mutate_all(funs(replace(., is.na(.), 0)))

head(ddMap)
write.csv(ddMap, 'Publication Maps/SpeciesDataDeficients_190227.csv')


# ---------------------------------------------------------------------------------------------------
# Predictions ---------------------------------------------------------------------------------------
# ---------------------------------------------------------------------------------------------------

predsraw <- read_csv('../../../Datasets/GBMPredictedMeans_190122.csv')

preds <- 
  predsraw %>% 
  select(-X1) %>% 
  # join to other dataframe to get all 91 countries in list
  right_join(., changespp, by = c('ISO3' = 'ISO3')) %>% 
  select(-hist, -present) %>% 
  rename(probPresent = value) %>% 
  mutate(probExtinct = 1 - probPresent) %>% 
  mutate(bins = cut(probPresent, breaks = seq(0, 1, 0.2))) %>% 
  mutate(bins = as.vector(bins)) %>% 
  mutate_at('bins', funs(replace(., is.na(.), 'known')))

head(preds)
nrow(preds)

write.csv(preds, 'Publication Maps/SpeciesPredictions_190227.csv')


# ---------------------------------------------------------------------------------------------------
# Predictions with ranges ---------------------------------------------------------------------------
# ---------------------------------------------------------------------------------------------------

preds2 <- read_csv('GBMPredicted_190208.csv')

predsRange <- 
  preds2 %>%
  select(-X1) %>% 
  rename(value = '.') %>% 
  group_by(ISO3) %>% 
  summarise(predmean = mean(value),
            predmax = max(value),
            predmin = min(value)) %>% 
  mutate(country = countrycode(ISO3, 'iso3c', 'country.name')) %>% 
  .[c(5, 1:4)]

head(predsRange)
nrow(predsRange)

write.csv(predsRange, 'GBMPredictedRange_190305.csv')


# ---------------------------------------------------------------------------------------------------
# Predictions combined with known presence -------------------------------------------
# ---------------------------------------------------------------------------------------------------

knownPred <- 
  allsppraw %>% 
  mutate(country = gsub('Saint Vinctente and Grenadines', 
                        'Saint Vincent and Grenadines', 
                        country),
         country = gsub('Domincia', 'Dominica', country)) %>% 
  mutate(ISO3 = countrycode(country, 'country.name', 'iso3c')) %>% 
  dplyr::filter(presence != 'unknown') %>% 
  mutate(value = case_when(presence == 'present' ~ 1,
                                presence == 'absent' ~ 0)) %>% 
  select(-country, -region, -species, -presence) %>% 
  arrange(desc(value)) %>% 
  distinct(ISO3, .keep_all = TRUE) %>% 
  mutate(X1 = 0) %>% 
  rbind(predsraw) %>% 
  select(-X1) %>% 
  arrange(value) %>% 
  distinct(ISO3, .keep_all = TRUE)

head(knownPred)
lapply(knownPred, function(x) sum(is.na(x)))
View(knownPred)

write_csv(knownPred, '../../../Datasets/Publication Maps/PredictionsWKnown_190510.csv')
