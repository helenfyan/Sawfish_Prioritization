##### PRINCIPAL COMPONENT ANALYSIS FOR EXPLANATORY VARIABLES #####

rm(list=ls())
setwd("/users/helenyan/desktop/school/directed studies 2018/datasets")

library(stats)
library(tidyverse)
library(devtools)
library(ggbiplot)

AllData1 <- read.csv('CompleteSpeciesCovariates_181011.csv')

AllData <-
  AllData1 %>%
  # Remove saltmarsh data because there are too many NAs
  select(ISO3, NbiScale, CoastPopScale, CoastLengthScale,
         EpiScale, EstDisScale, HkExpScale, ProteinSupScale, GdpScale, HdiScale, 
         OhiScale, IuuScale, ManImpScale, ReefFisherScale, ChondLandScale, WgiScale,
         SstMeanScale, SstMaxScale, SstMinScale) %>%
  distinct(., ISO3, .keep_all = TRUE)

# Can't just drop NAs - need to subset first to keep majority of data
# My data is already scaled and centered therefore do not need to do those in PCA 

# PCA for economic data -------------------------------------------------

econ <-
  AllData %>%
  select(ISO3, GdpScale, NbiScale, OhiScale, EpiScale, HkExpScale, ReefFisherScale,
         CoastPopScale, ChondLandScale, ProteinSupScale) %>%
  drop_na()

EconPCA <- prcomp(econ[, c(2:10)], center = FALSE, scale. = FALSE)

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
  select(ISO3, CoastLengthScale, EstDisScale, ManImpScale, SstMeanScale,
         SstMaxScale, SstMinScale) %>%
  drop_na()

BioPCA <- prcomp(bio[, c(2:7)], center = FALSE, scale. = FALSE)

summary(BioPCA)
BioPCA

BioPlot <-
  ggbiplot(BioPCA) +
  scale_x_continuous(limits = c(-2, 4)) +
  ggtitle('Biological Covariates') +
  theme_classic()

print(BioPlot)


# PCA for governmental data -------------------------------------------------

gov <- 
  AllData %>%
  select(ISO3, WgiScale, IuuScale, HdiScale) %>%
  drop_na()

GovPCA <- prcomp(gov[, c(2:4)])

summary(GovPCA)
GovPCA

GovPlot <-
  ggbiplot(GovPCA) +
  theme_classic() +
  ggtitle('Goverment Covariates')

print(GovPlot)
