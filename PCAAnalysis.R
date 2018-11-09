##### PRINCIPAL COMPONENT ANALYSIS FOR EXPLANATORY VARIABLES #####

rm(list=ls())
setwd("/users/helenyan/desktop/school/directed studies 2018/datasets")

library(stats)
library(tidyverse)
library(devtools)
library(ggbiplot)

AllDataRaw <- read.csv('CompleteSpeciesCovariates_181109.csv')

AllData <-
  AllDataRaw %>%
  select(1:27) %>%
  distinct(., ISO3, .keep_all = TRUE)

# Can't just drop NAs - need to subset first to keep majority of data
# My data is already scaled and centered therefore do not need to do those in PCA 

# PCA for economic data -------------------------------------------------

econ <-
  AllData %>%
  select(ISO3, GDP, NBI, OHI, EPI, HkExp, ReefFishers, CoastPop, 
         ChondLand, ProteinDiet, totalGearTonnes, totalGearValue) %>%
  dplyr::rename(`Fin Exp to HK` = HkExp,
                `Total Chondrichthyes Landings` = ChondLand,
                `Marine Protein Consumption` = ProteinDiet,
                `Fishing Gear Landed Tonnes` = totalGearTonnes)

EconPCA <- prcomp(econ[, c(2:11)], center = TRUE, scale. = TRUE)

ggbiplot(EconPCA) +
  ggtitle('Economic Covariates') +
  theme_classic()


# PCA for biological data -------------------------------------------------

bio <-
  AllData %>%
  select(2, 10, 13, 15, 22, 24, 27) %>%
  dplyr::rename(`Coastline Length` = CoastLength,
                `Estuaries Discharge` = EstDis,
                `Mangrove Area` = Mang,
                `SST` = SstMean,
                `Primary Productivity` = PprodMean) %>%
  drop_na()

BioPCA <- prcomp(bio[, c(2:7)], center = TRUE, scale. = TRUE)

ggbiplot(BioPCA) +
  ggtitle('Biological Covariates') +
  theme_classic()


# PCA for governmental data -------------------------------------------------

gov <- 
  AllData %>%
  select(2, 26, 21, 19) %>%
  dplyr::rename(`World Governance Index` = WGI,
                `IUU Fishing` = Iuu,
                `Human Development Index` = HDI)

GovPCA <- prcomp(gov[, c(2:4)], center = TRUE, scale. = TRUE)

ggbiplot(GovPCA) +
  expand_limits(x = -3) +
  theme_classic() +
  ggtitle('Goverment Covariates')


