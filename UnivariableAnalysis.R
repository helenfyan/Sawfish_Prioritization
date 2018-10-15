# THIS CODE MAKES FIGURES FOR EACH SP AGAINST EACH COVARIATE #

library(tidyverse)
library(broom)

setwd('/users/helenyan/desktop/school/directed studies 2018/datasets/')

AllSpecies <- read_csv('CompleteSpeciesCovariates_181012.csv')

AllSpecies <-
  AllSpecies %>%
  select(-NBI, -coastal.pop, -Lengthkm, -EPI, -Discharge, -ExporttoHK2010,
         -MeanPro, -GDP, -HDI, -OHI, -UnreportedPercent, -impact.percentage, 
         -no_reef_fishers, -AreaSqKM, -TotalTonnes, -WGI, -PprodMax, -PprodMin,
         -PprodMaxScale, -PprodMinScale, -PprodMean, -SstMaxScale, -SstMax,
         -SstMin, -SstMinScale, -SstMean)

AllSpeciesKnown <-
  AllSpecies %>%
  filter(presence != 'unknown') %>%
  mutate(species = as.factor(species))

# Make a seperate dataframe of just the covariate per species -----------------
# Not pretty can probably clean this up 

vars <- names(AllSpeciesKnown)
spp <- levels(AllSpeciesKnown$species)

dataframetot <- list()
dataframesp <- list()
df01 <- list()

df <-
  lapply(spp, function(spe) {
    dataframesp <- 
      AllSpeciesKnown %>%
      filter(species == spe)
    for(i in 2:19) {
      df01[[i]] <- paste(vars[6 + i])
      dataframetot[[i]] <- 
        dataframesp %>%
        select(country, species, occurrence, df01[[i]]) %>%
        drop_na(vars[6 + i])
    }
    return(dataframetot)
  })

df[[1]][[2]]
df[[5]][[2]]

# GLM for each covariate against each model ----------------------------------------

formula <- list()
model <- list()
vars01 <- list()

for(i in 1:5) {
  for(j in 2:19) {
    formula[[j]] <- paste('occurrence', ' ~ ', vars[6 + j])
    model[[i]][[j]] <- glm(formula[[j]], family = binomial, df[[i]][[j]])
    print(tidy(model[[i]][[j]]))
  }
}

# Get the GLM for each species (for presentation and organization sake) -------------

# dwarf - spp[1]

moddf <- list()
dwarf <- list()
dwarfmod <- list()

for(i in 2:19) {
  formula[[i]] <- paste('occurrence', ' ~ ', vars[6 + i])
  moddf[[i]] <- glm(formula[[i]], family = binomial, df[[1]][[i]])
  dwarf[[i]] <- (glance(moddf[[i]]))
  #dwarfmod[[i]] <- moddf[[i]]
  print(dwarf[[i]])
  #return(dwarfmod[[i]])
}

# green - spp[2]

modgr <- list()
green <- list()

for(i in 2:19) {
  formula[[i]] <- paste('occurrence', ' ~ ', vars[6 + i])
  modgr[[i]] <- glm(formula[[i]], family = binomial, df[[2]][[i]])
  green[[i]] <- (glance(modgr[[i]]))
  print(green[[i]])
  #modgr[[i]] <- (tidy(model[[i]]))
  #green <- glance(green, modgr[[i]])
}

# largetooth - spp[3]

modlt <- list()
large <- list()

for(i in 2:19) {
  formula[[i]] <- paste('occurrence', ' ~ ', vars[6 + i])
  modlt[[i]] <- glm(formula[[i]], family = binomial, df[[3]][[i]])
  large[[i]] <- (glance(modlt[[i]]))
  print(large[[i]])
  #modlt[[i]] <- (tidy(model[[i]]))
  #large <- rbind(large, modlt[[i]])
}


# narrowtooth - spp[4]

modna <- list()
narrow <- list()

for(i in 2:19) {
  formula[[i]] <- paste('occurrence', ' ~ ', vars[6 + i])
  modna[[i]] <- glm(formula[[i]], family = binomial, df[[4]][[i]])
  print(glance(modna[[i]]))
  #modna[[i]] <- (glance(model[[i]]))
  #narrow <- rbind(narrow, modna[[i]])
}


#smalltooth - spp[5]

modsm <- list()
small <- list()

for(i in 2:19) {
  formula[[i]] <- paste('occurrence', ' ~ ', vars[6 + i])
  modsm[[i]] <- glm(formula[[i]], family = binomial, df[[5]][[i]])
  print(glance(modsm[[i]]))
  #modsm[[i]] <- (tidy(model[[i]]))
  #small <- rbind(small, modsm[[i]])
}


# Analysis of SST in relation to EO ----------------------------------------------------

# occurrence ~ SST for each species ------
tempplot <- list()

for(i in 1:5) {
    tempplot[[i]] <- ggplot(df[[i]][[19]], aes(x = SstMeanScale, y = occurrence)) +
      geom_point(size = 3, colour = 'navyblue') +
      theme_classic() +
      xlab('SSTmean') +
      ylab('Presence') +
      scale_y_continuous(breaks = c(0, 1)) +
      ggtitle(paste(spp[i]))
  }


# occurrence ~ SST for all species combined ------

# checking discrepencies of species and countries - if some are present and some are absent
AllSstCheck <-
  AllSpeciesKnown %>%
  #distinct(country, .keep_all = TRUE)
  select(country, species, occurrence)

l1 <- 
  AllSstCheck %>%
  filter(species == 'large')

s1 <-
  AllSstCheck %>%
  filter(species == 'small')

n1 <-
  AllSstCheck %>%
  filter(species == 'narrow')

d1 <-
  AllSstCheck %>%
  filter(species == 'dwarf')

g1 <-
  AllSstCheck %>%
  filter(species == 'green')


dupcheck <-
  l1 %>%
  left_join(., s1, by = c('country' = 'country')) %>%
  left_join(., n1, by = c('country' = 'country')) %>%
  left_join(., d1, by = c('country' = 'country')) %>%
  left_join(., g1, by = c('country' = 'country')) %>%
  mutate(sp = paste(species.x, species.y, species.x.x, species.y.y, species, sep = '-')) %>%
  mutate(oc = paste(occurrence.x, occurrence.y, occurrence.x.x, occurrence.y.y, occurrence,
                    sep = '-')) %>%
  select(country, sp, oc)

SstChange <-
  AllSpeciesKnown %>%
  distinct(country, .keep_all = TRUE) %>%
  filter(country == c('United States of America', 'Malaysia', 'Oman')) %>%
  mutate(occurrence = sub('0', '1', occurrence))

AllSst <-
  AllSpeciesKnown %>%
  distinct(country, .keep_all = TRUE) %>%
  filter(country != c('United States of America', 'Malaysia', 'Oman')) %>%
  rbind(SstChange) %>%
  select(-region) %>%
  mutate(occurrence = as.numeric(occurrence))

AllSstPlot <- 
  ggplot(AllSst, aes(x = SstMeanScale, y = occurrence)) +
  geom_point(size = 3, colour = 'darkslategray', shape = '|') +
  scale_y_continuous(breaks = c(0, 1)) +
  ylab('Occurrence') +
  xlab('SSTmean') +
  theme_classic()

print(AllSstPlot)

modSstAll <- glm(occurrence ~ SstMeanScale, family = binomial, AllSst)
summary(modSstAll)