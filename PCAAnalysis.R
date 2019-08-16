##### PRINCIPAL COMPONENT ANALYSIS FOR EXPLANATORY VARIABLES #####

rm(list=ls())
setwd("/users/helenyan/desktop/school/directed studies 2018/datasets")

library(stats)
library(tidyverse)
library(devtools)
library(ggbiplot)

dataRaw <- read_csv('CompleteSpeciesCovariates_181119.csv')

allData <-
  dataRaw %>%
  dplyr::select(2, 8:26) %>%
  distinct(., ISO3, .keep_all = TRUE)

# PCA for all data -----------------------------------------------------

datatrans <- read_csv('ProcessedCovariates_181128.csv')

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


# PCA for ecological carrying capacity ---------------------------------------
rm(list=ls())

library(gridExtra)

datatrans <- read_csv('../ProcessedCovariates_181128.csv')


# first plot presence/absence of each species against coastline length

coast <- 
  datatrans %>% 
  select(ISO3, occurrence, speciesdwarf, speciesgreen, specieslarge, speciesnarrow, speciessmall,
         logCoastLength) %>% 
  mutate(species = case_when(speciesdwarf == 1 ~ 'Dwarf',
                             speciesgreen == 1 ~ 'Green',
                             specieslarge == 1 ~ 'Largetooth',
                             speciesnarrow == 1 ~ 'Narrow',
                             speciessmall == 1 ~ 'Smalltooth')) %>% 
  select(-speciesdwarf, -speciesgreen, -specieslarge, -speciesnarrow, -speciessmall)


head(coast)

plot_occ <- 
  function(spp) {
    
    occplot <- 
      coast %>% 
      select(species, occurrence, logCoastLength) %>% 
      dplyr::filter(species == spp) %>% 
      ggplot(., aes(x = logCoastLength, y = occurrence)) +
      geom_point(size = 4, shape = '|',
                 colour = 'grey30', alpha = 0.9) +
      geom_smooth(method = 'glm', method.args = list(family = 'binomial'),
                  colour = 'darkslategray') +
      scale_y_continuous(limits = c(0, 1), breaks = c(0, 1)) +
      theme(axis.text = element_text(size = 14),
            axis.title = element_text(size = 16),
            panel.grid = element_blank(), 
            panel.background = element_blank(),
            axis.line = element_line()) +
      labs(x = 'log Coastline Length (km)',
           y = 'Occurrence',
           title = paste(spp))
    
    return(occplot)
    
  }

large <- plot_occ('Largetooth')
large

dwarf <- plot_occ('Dwarf')
dwarf

green <- plot_occ('Green')
green


small <- plot_occ('Smalltooth')
small


narr <- plot_occ('Narrow')
narr

sp_grid <- grid.arrange(large, dwarf, green, small, narr, ncol = 2)

ggsave('../../Figures/EcoCarryCapacity/OccCoastLength_190815.pdf', sp_grid,
       height = 19.05, width = 30.59, units = c('cm'))

# plot of all species together -------------------

spp_coast <- 
  ggplot(coast, aes(x = logCoastLength, y = occurrence)) +
  geom_point(size = 4, shape = '|',
             colour = 'grey40', alpha = 0.9) +
  geom_smooth(method = 'glm', method.args = list(family = 'binomial'),
              colour = 'darkslategray') +
  scale_y_continuous(limits = c(0, 1), breaks = c(0, 1)) +
  theme(axis.text = element_text(size = 14),
        axis.title = element_text(size = 16),
        panel.grid = element_blank(), 
        panel.background = element_blank(),
        axis.line = element_line()) +
  labs(x = 'log Coastline Length (km)',
       y = 'Occurrence',
       title = 'All Species')

spp_coast

ggsave('../../Figures/EcoCarryCapacity/OccCoastLengthAllSpp_190815.pdf', spp_coast,
       height = 19.05, width = 30.59, units = c('cm'))


# PCA of all ecological carrying capacity values ------------------------------
library(broom)
library(knitr)
library(ggfortify)

ecc_pca <- 
  datatrans %>% 
  select(occurrence, speciesdwarf, speciesgreen, specieslarge, speciesnarrow, speciessmall,
         SstMean, logPprodMean, logCoastLength, logEstDis, logMang) %>% 
  mutate(species = case_when(speciesdwarf == 1 ~ 'Dwarf',
                             speciesgreen == 1 ~ 'Green',
                             specieslarge == 1 ~ 'Largetooth',
                             speciesnarrow == 1 ~ 'Narrow',
                             speciessmall == 1 ~ 'Smalltooth')) %>% 
  select(-speciesdwarf, -speciesgreen, -specieslarge, -speciesnarrow, -speciessmall)
 
# dataframe is clean now just need to run the PCA



