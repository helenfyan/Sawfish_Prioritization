# This script analyzes the dynamic geography theory 

library(tidyverse)
library(gridExtra)
library(broom)
library(knitr)
library(ggfortify)
library(MuMIn)

# ----------------------------------------------------------------------------
# Exploratory data analysis - no full analyses done here ---------------------
# ----------------------------------------------------------------------------

# PCA for ecological carrying capacity ---------------------------------------
datatrans <- read_csv('../../../Datasets/ProcessedCovariates_181128.csv')


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

#ggsave('../../Figures/EcoCarryCapacity/OccCoastLength_190815.pdf', sp_grid,
#       height = 19.05, width = 30.59, units = c('cm'))

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

#ggsave('../../Figures/EcoCarryCapacity/OccCoastLengthAllSpp_190815.pdf', spp_coast,
#       height = 19.05, width = 30.59, units = c('cm'))

mod_coast <- glm(occurrence ~ logCoastLength, coast, family = 'binomial')
summary(mod_coast)

# calculate PCA for ECC and fishing pressures ------------------------------

ecc_pca_df <- 
  datatrans %>% 
  select(occurrence, speciesdwarf, speciesgreen, specieslarge, speciesnarrow, speciessmall,
         SstMean, logPprodMean, logCoastLength, logEstDis, logMang, logProteinDiet,
         logtotalGearTonnes, logCoastPop, logChondLand) %>% 
  mutate(species = case_when(speciesdwarf == 1 ~ 'Dwarf',
                             speciesgreen == 1 ~ 'Green',
                             specieslarge == 1 ~ 'Largetooth',
                             speciesnarrow == 1 ~ 'Narrow',
                             speciessmall == 1 ~ 'Smalltooth')) %>% 
  select(-speciesdwarf, -speciesgreen, -specieslarge, -speciesnarrow, -speciessmall) 

# dataframe is clean now just need to run the PCA

ecc_pca <- 
  ecc_pca_df %>% 
  dplyr::rename('logPrimaryProductivity' = 'logPprodMean',
                'logEstuarineDischargeRate' = 'logEstDis',
                'logMangroveArea' = 'logMang',
                'SST' = 'SstMean') %>% 
  nest() %>% 
  mutate(pca = map(data, ~ prcomp(.x %>% 
                                    select(-occurrence, -species, -logProteinDiet,
                                           -logtotalGearTonnes, -logCoastPop, -logChondLand),
                                  center = TRUE,
                                  scale = TRUE)),
         pca_aug = map2(pca, data, ~ augment(.x, data = .y)))

# check variance explained
var_exp <- 
  ecc_pca %>% 
  unnest(pca_aug) %>% 
  summarise_at(.vars = vars(contains('PC')), .funs = funs(var)) %>% 
  gather(key = pc, value = variance) %>% 
  mutate(var_exp = variance/sum(variance),
         cum_var_exp = cumsum(var_exp),
         pc = str_replace(pc, '.fitted', ''))

var_exp

# make pca plot

ecc_pca %>% 
  mutate(
    pca_graph = map2(
      .x = pca,
      .y = data,
      ~ autoplot(.x, loadings = TRUE, loadings.label = TRUE,
                 loading.label.repel = TRUE,
                 data = .y, label = FALSE, loadings.colour = 'blue') +
        theme_classic()
    )
  ) %>% 
  pull(pca_graph)

# ggsave won't let you save it this way - I cheated and used the click icon to save

# check PC1 against species -----------------

ecc_pc1 <- 
  ecc_pca %>% 
  select(pca_aug) %>% 
  unnest() %>% 
  select(-.rownames, -.fittedPC2, -.fittedPC3, -.fittedPC4, -.fittedPC5) %>% 
  dplyr::rename(ECC_PC1 = .fittedPC1)

ecc_pc1

# now calculate PCA for fishing pressures
fish_pca <- 
  ecc_pc1 %>% 
  nest() %>% 
  mutate(pca = map(data, ~ prcomp(.x %>% 
                                    select(-occurrence, -species, -logPrimaryProductivity,
                                           -logEstuarineDischargeRate, -logMangroveArea, -SST),
                                  center = TRUE,
                                  scale = TRUE)),
         pca_aug = map2(pca, data, ~ augment(.x, data = .y)))

fish_pca %>% 
  mutate(
    pca_graph = map2(
      .x = pca,
      .y = data,
      ~ autoplot(.x, loadings = TRUE, loadings.label = TRUE,
                 loading.label.repel = TRUE,
                 data = .y, label = FALSE, loadings.colour = 'blue') +
        theme_classic()
    )
  ) %>% 
  pull(pca_graph)

all_pc1 <- 
  fish_pca %>% 
  select(pca_aug) %>% 
  unnest() %>% 
  select(-.rownames, -.fittedPC2, -.fittedPC3, -.fittedPC4, -.fittedPC5, -.fittedPC6) %>% 
  dplyr::rename(Fish_PC1 = .fittedPC1)


all_pc1

# plot each species
plot_pca <-
  function(spp) {
    
    pca_spp <- 
      all_pc1 %>% 
      select(species, occurrence, ECC_PC1) %>% 
      dplyr::filter(species == spp) %>% 
      ggplot(., aes(x = ECC_PC1, y = occurrence)) +
      geom_point(size = 4, shape = '|',
                 colour = 'grey30', alpha = 0.9) +
      geom_smooth(method = 'glm', method.args = list(family = 'binomial'),
                  colour = '#4E021B') +
      scale_y_continuous(limits = c(0, 1), breaks = c(0, 1)) +
      theme(axis.text = element_text(size = 14),
            axis.title = element_text(size = 16),
            panel.grid = element_blank(),
            panel.background = element_blank(),
            axis.line = element_line()) +
      labs(x = 'PC1 (42.15%)',
           y = 'Occurrence',
           title = paste(spp))
    
    return(pca_spp)
    
  }


spp_pca_plots <- lapply(unique(all_pc1$species), plot_pca)

pca_grid <- grid.arrange(spp_pca_plots[[1]], spp_pca_plots[[4]], spp_pca_plots[[3]],
                         spp_pca_plots[[5]], spp_pca_plots[[2]], ncol = 2)

#ggsave('../../../Figures/EcoCarryCapacity/OccPC1_190830.pdf', pca_grid,
#      height = 19.05, width = 30.59, units = c('cm'))


# all species combined ----------------------
spp_pc1 <- 
  ggplot(all_pc1, aes(x = ECC_PC1, y = occurrence)) +
  geom_point(size = 4, shape = '|',
             colour = 'grey40', alpha = 0.9) +
  geom_smooth(method = 'glm', method.args = list(family = 'binomial'),
              colour = '#4E021B') +
  scale_y_continuous(limits = c(0, 1), breaks = c(0, 1)) +
  theme(axis.text = element_text(size = 14),
        axis.title = element_text(size = 16),
        panel.grid = element_blank(), 
        panel.background = element_blank(),
        axis.line = element_line()) +
  labs(x = 'PC1 (42.15%)',
       y = 'Occurrence',
       title = 'All Species')


spp_pc1

#ggsave('../../../Figures/EcoCarryCapacity/OccPC1AllSpp_190930.pdf', spp_pc1,
#       height = 19.05, width = 30.59, units = c('cm'))

mod_pca <- glm(occurrence ~ ECC_PC1, all_pc1, family = 'binomial')
summary(mod_pca)

# ----------------------------------------------------------------------------
# Plot ECC with effects of fishing pressure on occurrence --------------------
# ----------------------------------------------------------------------------

# get quartiles 

proteinQ <- summary(all_pc1$logProteinDiet)
proteinQ1 <- proteinQ[2]
proteinQ3 <- proteinQ[5]

landQ <- summary(all_pc1$logtotalGearTonnes)
landQ1 <- landQ[2]
landQ3 <- landQ[5]

fish_pc1 <- 
  all_pc1 %>% 
  mutate(fishBin3 = case_when(logProteinDiet <= proteinQ1 ~ 'low',
                             logProteinDiet >= proteinQ3 ~ 'high',
                             TRUE ~ as.character('moderate'))) %>% 
  mutate(fishBin4 = case_when(logProteinDiet == 0 ~ 'none',
                              logProteinDiet <= proteinQ1 ~ 'low',
                              logProteinDiet >= proteinQ3 ~ 'high',
                              TRUE ~ as.character('moderate'))) %>% 
  # don't bin into 4 categories because there are only two zeros
  mutate(gearBin3 = case_when(logtotalGearTonnes <= landQ1 ~ 'low',
                              logtotalGearTonnes >= landQ3 ~ 'high',
                              TRUE ~ as.character('moderate')))

# use the PC1 of fishing pressure and bin that
fish_pc1Q <- summary(fish_pc1$Fish_PC1)
fish_pc1Q1 <- fish_pc1Q[2]
fish_pc1Q3 <- fish_pc1Q[5]


fish_pc1_bin <- 
  fish_pc1 %>% 
  mutate(fish_pc_bin = case_when(Fish_PC1 <= fish_pc1Q1 ~ 'low',
                                 Fish_PC1 >= fish_pc1Q3 ~ 'high',
                                 TRUE ~ as.character('moderate')))

# let's bullshit and dredge this shit up --------------------
vars <- names(fish_pc1_bin)
covs <- vars[c(4, 7:8, 12:17)]

glob_formula <- paste('occurrence ~ ', paste(covs, collapse = ' + '))

options(na.action = na.fail)

glob_mod <- glm(glob_formula, family = 'binomial', fish_pc1_bin)

mod_dredge <- 
  dredge(glob_mod) %>% 
  as_tibble(.) %>% 
  dplyr::filter(delta <= 2)

View(mod_dredge)

# let's plot the top model

plot_mod <- function(covariate, line_colour) {
  
  occPlot <- 
    ggplot(fish_pc1_bin, aes(x = logCoastLength, y = occurrence, 
                             colour = covariate, fill = covariate)) +
    geom_point(size = 4, shape = '|', colour = 'grey40', alpha = 0.8) +
    geom_smooth(method = 'glm', method.args = list(family = 'binomial'),
                colour = line_colour) +
    scale_y_continuous(limits = c(0, 1), breaks = c(0, 1)) +
    theme_classic()
  
  return(occPlot)
  
}

# fishBin3
fishb3 <- plot_mod(fish_pc1_bin$fishBin3, '#4d004b')
fishb3


# gearBin3 - this doesn't work because complete separation occurrs
gearb3 <- plot_mod(fish_pc1_bin$gearBin3, '#00441b')
gearb3

mod1 <- glm(occurrence ~ logCoastLength*fishBin3*gearBin3, 
            family = 'binomial', fish_pc1_bin)
summary(mod1)
