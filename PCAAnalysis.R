##### PRINCIPAL COMPONENT ANALYSIS FOR EXPLANATORY VARIABLES #####

rm(list=ls())
setwd("/users/helenyan/desktop/school/directed studies 2018/datasets")


# Need to load dplyr first because other packages interfere with rename function #
library(stats)
library(tidyverse)
library(devtools)
library(ggbiplot)

AllData1 <- read.csv('CountryData_180921.csv')

AllData <-
  AllData1 %>%
  # Remove saltmarsh data because there are too many NAs
  .[, c(2, 35, 6, 16, 20, 22, 32, 10, 4, 28, 14, 8, 12, 26, 34, 24, 21)]

# Can't just drop NAs - need to subset first to keep majority of data
# My data is already scaled and centered therefore do not need to do those in PCA 

# PCA for economic data -------------------------------------------------

econ <-
  AllData %>%
  select(ISO3:HkExpScale) %>%
  drop_na()

EconPCA <- prcomp(econ[, c(3:11)], center = FALSE, scale. = FALSE)

summary(EconPCA)
EconPCA

EconPlot <-
  ggbiplot(EconPCA) +
  scale_x_continuous(limits = c(-4, 2.5)) +
  ggtitle('Economic Covariates') +
  theme_classic()

print(EconPlot)


# PCA for biological data -------------------------------------------------

bio <-
  AllData %>%
  select(ISO3:country, CoastLengthScale:ManImpScale) %>%
  drop_na()

BioPCA <- prcomp(bio[, c(3:5)], center = FALSE, scale. = FALSE)

summary(BioPCA)
BioPCA

BioPlot <-
  ggbiplot(BioPCA) +
  ggtitle('Biological Covariates') +
  theme_classic()

print(BioPlot)


# PCA for governmental data -------------------------------------------------

gov <- 
  AllData %>%
  select(ISO3:country, WgiScale:HdiScale) %>%
  drop_na()

GovPCA <- prcomp(gov[, c(3:5)])

summary(GovPCA)
GovPCA

GovPlot <-
  ggbiplot(GovPCA) +
  theme_classic() +
  ggtitle('Goverment Covariates')

print(GovPlot)


# Created a .csv file manually that requires cleaning ------------------------

raw1 <- read_csv('PCAraw_181009.csv')
View(raw1)

cols1 <- names(raw1)

clean1 <-
  raw1 %>%
  select(-X2, -X3, -X4, -X5) %>%
  separate(col = `PC1         PC2         PC3         PC4          PC5`,
           into = c('cov1', 'x1', 'x2',
                    'x3', 'x4', 'x5', 'x6', 'x7', 'x8',
                    'x9', 'x10', 'x11', 'x12', 'x13', 'x14',
                    'x15'), sep = ' ') %>%
  separate(col = `PC6         PC7         PC8         PC9`,
           into = c('cov2', 'x16', 'x17', 'x18', 'x19', 'x20', 'x21', 'x22',
                    'x23', 'x24', 'x25', 'x26', 'x27', 'x28', 'x29', 'x30'), 
           sep = ' ') %>%
  select(-cov2)
View(clean1)

# easier to manually clean data in excel once columns are separated 
write_csv(clean1, 'PCAclean_181009.csv')
