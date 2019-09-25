##### PRINCIPAL COMPONENT ANALYSIS FOR EXPLANATORY VARIABLES #####

rm(list=ls())

library(stats)
library(tidyverse)
library(devtools)
library(ggbiplot)

dataRaw <- read_csv('../../../Datasets/CompleteSpeciesCovariates_181119.csv')

allData <-
  dataRaw %>%
  dplyr::select(2, 8:26) %>%
  distinct(., ISO3, .keep_all = TRUE)

# PCA for all data -----------------------------------------------------

datatrans <- read_csv('../../../ProcessedCovariates_181128.csv')

alltrans <- 
  datatrans %>% 
  dplyr::select(2, 9:27) %>% 
  distinct(., ISO3, .keep_all = TRUE)

fullPCA <- prcomp(alltrans[, c(2:20)], center = TRUE, scale. = TRUE)
  
fullPCAfig <- 
  ggbiplot(fullPCA) +
  ggtitle('Transformed Covariates') +
  theme_classic()


# PCA for economic data -------------------------------------------------

econ <-
  allData %>%
  select(ISO3, GDP, NBI, OHI, EPI, FinUSD, ReefFishers, CoastPop, 
         ChondLand, ProteinDiet, totalGearTonnes, FishProd) %>%
  dplyr::rename(`Fin Exports` = FinUSD,
                `Total Chondrichthyes Landings` = ChondLand,
                `Marine Protein Consumption` = ProteinDiet,
                `Fishing Gear Landed Tonnes` = totalGearTonnes,
                `Fishery Production` = FishProd)

EconPCA <- prcomp(econ[, c(2:12)], center = TRUE, scale. = TRUE)

ggbiplot(EconPCA) +
  ggtitle('Economic Covariates') +
  theme_classic()


# PCA for biological data -------------------------------------------------

bio <-
  allData %>%
  select(ISO3, SstMean, PprodMean, CoastLength, EstDis, Mang) %>%
  dplyr::rename(`Coastline Length` = CoastLength,
                `Estuaries Discharge` = EstDis,
                `Mangrove Area` = Mang,
                `SST` = SstMean,
                `Primary Productivity` = PprodMean)

BioPCA <- prcomp(bio[, c(2:6)], center = TRUE, scale. = TRUE)

ggbiplot(BioPCA) +
  ggtitle('Biological Covariates') +
  theme_classic()


# PCA for governmental data -------------------------------------------------

gov <- 
  allData %>%
  select(ISO3, WGI, Iuu, HDI) %>%
  dplyr::rename(`World Governance Index` = WGI,
                `IUU Fishing` = Iuu,
                `Human Development Index` = HDI)

GovPCA <- prcomp(gov[, c(2:4)], center = TRUE, scale. = TRUE)

ggbiplot(GovPCA) +
  expand_limits(x = -3) +
  theme_classic() +
  ggtitle('Goverment Covariates')
