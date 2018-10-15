# THIS CODE MAKES FIGURES FOR EACH SP AGAINST EACH COVARIATE #

library(tidyverse)
library(broom)

setwd('/users/helenyan/desktop/school/directed studies 2018/datasets/')

AllSpecies <- read_csv('CompleteSpeciesCovariates_180924.csv')

AllSpecies <-
  AllSpecies %>%
  select(-NBI, -coastal.pop, -Lengthkm, -EPI, -Discharge, -ExporttoHK2010,
         -MeanPro, -GDP, -HDI, -OHI, -UnreportedPercent, -impact.percentage, 
         -no_reef_fishers, -AreaSqKM, -TotalTonnes, -WGI)

AllSpeciesKnown <-
  AllSpecies %>%
  filter(presence != 'unknown') %>%
  mutate(species = as.factor(species)) %>%
  drop_na(NbiScale)


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
    for(i in 2:17) {
      df01[[i]] <- paste(vars[6 + i])
      dataframetot[[i]] <- 
        dataframesp %>%
        select(country, species, occurrence, df01[[i]]) %>%
        drop_na(vars[6 + i])
    }
    return(dataframetot)
  })

df[[1]][[2]]

# GLM for each covariate against each model ----------------------------------------

formula <- list()
model <- list()
vars01 <- list()

for(i in 1:5) {
  for(j in 2:17) {
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

for(i in 2:17) {
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

for(i in 2:17) {
  formula[[i]] <- paste('occurrence', ' ~ ', vars[6 + i])
  modgr[[i]] <- glm(formula[[i]], family = binomial, df[[2]][[i]])
  green[[i]] <- (glance(modgr[[i]]))
  return(green[[2]][[i]])
  #modgr[[i]] <- (tidy(model[[i]]))
  #green <- glance(green, modgr[[i]])
}

# largetooth - spp[3]

modlt <- list()
large <- list()

for(i in 2:17) {
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

for(i in 2:17) {
  formula[[i]] <- paste('occurrence', ' ~ ', vars[6 + i])
  modna[[i]] <- glm(formula[[i]], family = binomial, df[[4]][[i]])
  print(glance(modna[[i]]))
  #modna[[i]] <- (glance(model[[i]]))
  #narrow <- rbind(narrow, modna[[i]])
}


#smalltooth - spp[5]

modsm <- list()
small <- list()

for(i in 2:17) {
  formula[[i]] <- paste('occurrence', ' ~ ', vars[6 + i])
  modsm[[i]] <- glm(formula[[i]], family = binomial, df[[5]][[i]])
  print(glance(modsm[[i]]))
  #modsm[[i]] <- (tidy(model[[i]]))
  #small <- rbind(small, modsm[[i]])
}


# Make a figure for each sp for each cov -------------------------------------------------



# Everything below this doesn't work ------------------------------------------------------

# Create multiple dataframes to drop column-specific NAs -------------------------------
# make a loop or function to make a dataframe for each covariate 

makedf01 <-
  AllSpeciesKnown %>%
  select(country, ISO3, region, species, occurrence)

covcol <- list()
makedf <-
  function(covcol) {
    for(i in 2:17) {
      covcol[[i]] <- paste('AllSpeciesKnown$', vars[2], 'AllSpeciesKnown$', vars[6 + i])
      
    }
  }

df1 <- 
  df <- rbind(makedf01, covcol[[i]], by = c('ISO3' = 'ISO3'))
return(df)


covformula <- list()
covmodel <- list()

for(i in 2:17) {
  covformula[[i]] <- paste(vars[6], ' ~ ', vars[6 + i])
  covmodel[[i]] <- glm(covformula[[i]], family = binomial, AllSpeciesKnown)
}

covmodeltb <-
  function(covmodel) {
    for(i in 2:17) {
      covformula[[i]] <- paste(vars[6], ' ~ ', vars[6 + i])
      covmodel[[i]] <- glm(covformula[[i]], family = binomial, AllSpeciesKnown)
    }
    return(tidy(covmodel[[i]]))
  }

covmodeltb(covmodel[[2]])

# try writing function to keep data generic to use in dplyr - this doesn't work
# either try writing a model with a generic data frame that I call on later
# or make a loop inside a loop where the first loop calls on a level in species and the second carries model
# or use lapply to apply the model to each subset for species 

for(i in spp){
  print(i)
}

test <-
  function(spp) {
    for(i in spp) {
      for(i in 2:17) {
        covformula[[i]] <- paste(vars[6], ' ~ ', vars[6 + i])
        covmodel[[i]] <- glm(covformula[[i]], family = binomial, AllSpeciesKnown)
      }
    }
    return(tidy(covmodel[[i]]))
  }

test01 <- 
  AllSpeciesKnown %>%
  group_by(species) %>%
  lapply(., function(.) {
    for(i in 2:17) {
      covformula[[i]] <- paste(vars[6], ' ~ ', vars[6 + i])
      covmodel[[i]] <- glm(covformula[[i]], family = binomial, .)
    }
  })


test02 <-
  function(sppdf) {
    for(i in 2:17) {
      covformula[[i]] <- paste(vars[6], ' ~ ', vars[6 + i])
      covmodel[[i]] <- glm(covformula[[i]], family = binomial, sppdf)
    }
    return(tidy(covmodel[[i]]))
  }

sppdf <- list()

test03 <-
  for(i in spp) {
    datspp[[i]] <- subset(AllSpeciesKnown, species == i)
    for(j in 2:17) {
      covformula[[j]] <- paste(vars[6], ' ~ ', vars[6 + j])
      covmodel[[j]] <- glm(covformula[[j]], family = binomial, datspp[[i]])
    }
  }


# this subsets the data by species 
datspp <- list()
test04 <-
  for(i in spp) {
    datspp[[i]] <- subset(AllSpeciesKnown, species == i)
  }


test05 <-
  for(i in spp) {
    datspp[[i]] <- subset(AllSpeciesKnown, species == i)
    for(j in 2:17) {
      covformula[[j]] <- paste(vars[6], ' ~ ', vars[6 + j])
      covmodel[[j]] <- glm(covformula[[j]], family = binomial, datspp[[i]])
    }
  }

test06 <-
  for(j in 2:17) {
    covformula[[j]] <- paste(vars[6], ' ~ ', vars[6 + j])
    covmodel[[j]] <- glm(covformula[[j]], family = binomial, datspp[[1]])
  }

covdatasp <- list()
test07 <-
  for(i in 2:17) {
    covformula[[i]] <- paste(vars[6], ' ~ ', vars[6 + i])
    for(j in 1:5) {
      covdatasp[[j]] <- paste('datspp', '[[', j, ']]')
      covmodel[[i]] <- glm(covformula[[i]], family = binomial, covdatasp[[j]])
    }
  }



test08 <-
  for(i in 1:5) {
    for(j in 2:17) {
      datspp[[i]] <- subset(AllSpeciesKnown, species == i)
      covformula[[j]] <- paste(vars[6], ' ~ ', vars[6 + j])
      covmodel[[j]] <- glm(covformula[[j]], family = binomial, datspp[[i]])
    }
  }

dwarf <- 
  AllSpeciesKnown %>%
  filter(species == 'dwarf')

dwarfformula <- list()
dwarfmodel <- list()

for(i in 2:17) {
  dwarfformula[[i]] <- paste(vars[6], ' ~ ', vars[6 + i])
  dwarfmodel[[i]] <- glm(dwarfformula[[i]], family = binomial, dwarf)
}

datspp <- data.frame()
spp01 <- list(AllSpeciesKnown$species)

for(i in 1:5) {
  datspp[[i]] <- 
    AllSpeciesKnown %>%
    filter(species == 'i')
  print(datspp[[i]])
}

for(i in 1:5) {
  spp02[[i]] <- paste(spp[i])
}



# ----------------------------------------------------------------------------

df <- list()
dataframecov <- list()
spp01 <- list()

for(j in 2:17) {
  df[[j]] <- paste(vars[6 + j])
  dataframecov[[j]] <- 
    AllSpeciesKnown %>%
    select(country, species, occurrence, df[[j]]) %>%
    drop_na(vars[6 + j]) %>%
    mutate(metricname = df[[j]])
}





speciesdf <- 
  lapply(spp, function(spe) {
    dataframesp <- AllSpeciesKnown %>%
      filter(species == spe)
    return(dataframesp)
  })




no01 <- 
  lapply(spp, function(spe) {
    dataframesp <- 
      AllSpeciesKnown %>%
      filter(species == spe)
    for(i in 2:17) {
      df01[[i]] <- paste(vars[6 + i])
      dataframetot[[i]] <- 
        dataframesp %>%
        select(country, species, occurrence, df01[[i]]) %>%
        drop_na(vars[6 + i])
      formula[[i]] <- paste('occurrence', ' ~ ', df01[[i]])
      model[[i]] <- glm(formula[[i]], family = binomial, dataframetot[[i]])
    }
    return(summary(model[[i]]))
  })


no02 <-
  function(model) {
    for(i in 1:5) {
      for(j in 2:17) {
        formula[[j]] <- paste('occurrence', ' ~ ', vars[6 + j])
        model[[j]] <- glm(formula[[j]], family = binomial, df[[i]][[j]])
      }
    }
    return(summary(model[[j]]))
  }

no03 <- 
  lapply(spp, function(spe) {
    dataframesp <-
      AllSpeciesKnown %>%
      filter(species == spe)
    for(i in 2:17) {
      df01[[i]] <- paste(vars[6 + i])
      dataframetot[[i]] <-
        dataframesp %>%
        select(country, species, occurrence, df01[[i]]) %>%
        drop_na(df01[[i]])
    }
    formula[[i]] <- paste('occurrence', ' ~ ', vars[6 + i])
    model[[i]] <- glm(formula[[i]], family = binomial, dataframetot[[i]])
    return(summary(model[[i]]))
  })


# this gives the model output for each spp and each cov 

for(i in 1:5) {
  for(j in 2:17) {
    formula[[j]] <- paste('occurrence', ' ~ ', vars[6 + j])
    model[[i]][[j]] <- glm(formula[[j]], family = binomial, df[[i]][[j]])
  }
}

for(i in 1:5) {
  for(j in 2:17) {
    formula[[j]] <- paste('occurrence', ' ~ ', vars[6 + j])
    model[[i]][[j]] <- glm(formula[[j]], family = binomial, df[[i]][[j]])
    print(tidy(model[[i]][[j]]))
  }
}



# try to get model output into df form ---------------------------------------

test <-
  function(model) {
    for(i in 1:5) {
      for(j in 2:17) {
        formula[[j]] <- paste('occurrence', ' ~ ', vars[6 + j])
        model[[i]][[j]] <- glm(formula[[j]], family = binomial, df[[i]][[j]])
        
      }
    }
    return(summary(model))
  }


test(model[[1]][[2]])

test01 <-
  function(mod) {
    for(i in 1:5) {
      for(j in 2:17) {
        mod <- model[[i]][[j]]
      }
    }
    return(summary(mod))
  }

#this prints every data frame from above
dontrun <-
  for(i in 1:5) {
    for(j in 2:17) {
      print(df[[i]][[j]])
    }
  }


# This is also fucked up because it gives the same values for all i values -----

covmodeltb <-
  function(covmodel) {
    for(i in 2:17) {
      covformula[[i]] <- paste(vars[6], ' ~ ', vars[6 + i])
      covmodel[[i]] <- glm(covformula[[i]], family = binomial, AllSpeciesKnown)
      return(tidy(covmodel[[i]]))
    }
  }

covmodeltb(covmodel[[2]])











