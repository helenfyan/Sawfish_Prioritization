# This script makes publication figures for the paper

library(tidyverse)

setwd('/users/helenyan/desktop/school/directed studies 2018/datasets/')

# make presentation theme for all figures ------------------------
presentation_theme <- function(axis_text_size = 13, axis_y_ticks = element_line()) {
  theme(panel.background = element_blank(),
        axis.line = element_line(colour = 'black'),
        axis.text.y = element_text(size = axis_text_size, colour = 'black'),
        axis.text.x = element_text(colour = 'black'),
        axis.ticks.y = axis_y_ticks,
        axis.title = element_text(size = 14))
}

# this makes the background dark navy blue for presentations 
#panel.background = element_rect(fill = '#080818', colour = '#080818')

#  -----------------------------------------------------------------
#  Collinearity heatmap plot ---------------------------------------
#  -----------------------------------------------------------------

library(reshape2)

raw <- read_csv('ProcessedCovariates_190119.csv')

dat <- 
  raw %>% 
  dplyr::select(ISO3, logtotalGearTonnes, logPprodMean, NBI,
               logCoastPop, logCoastLength, logEstDis, logProteinDiet,
               logGDP, HDI, OHI, logMang, WGI, logChondCatch, SstMean,
               logFishProd, logIuu) %>% 
  distinct(ISO3, .keep_all = TRUE) %>% 
  dplyr::select(-ISO3)
  
sapply(dat, function(x) sum(is.na(x)))

cormat <- round(cor(dat), 2)

# reorder the correlation matrix according to the magnitude of correlation
reorderCormat <- function(cormat) {
  
  # use correlation between variables as distance
  dist <- as.dist((1 - cormat)/2)
  hc <- hclust(dist)
  cormat <- cormat[hc$order, hc$order]
  
}

cormat <- reorderCormat(cormat)

# get rid of half of the cormat

getLowerTri <- function(cormat) {
  
  cormat[upper.tri(cormat)] <- NA
  return(cormat)
  
}

lowerTri <- getLowerTri(cormat)

corfig <-
  melt(lowerTri, na.rm = TRUE) %>%
  mutate(Var2 = dplyr::recode(Var2, 'logEstDis' = 'Estuaries Discharge (ESD)',
                              'logPprodMean' = 'Primary Productivity (PPD)',
                              'logFinUSD' = 'Fin Exports (FXP)',
                              'logCoastPop' = 'Coastal Population (CTP)',
                              #'logChondLand' = 'Chondrichthyes Landings (CHL)',
                              'SstMean' = 'Sea Surface Temperature (SST)',
                              'logMang' = 'Mangrove Area (MNG)',
                              'HDI' = 'Human Development Index (HDI)',
                              #'EPI' = 'Environmental Performance Index (EPI)',
                              'WGI' = 'World Governance Index (WGI)',
                              'OHI' = 'Ocean Health Index (OHI)',
                              'NBI' = 'National Biodiversity Index (NBI)',
                              'logIuu' = 'Illegal Unreported and Unregulated Fishing (IUU)',
                              'logGDP' = 'Gross Domestic Product (GDP)',
                              'logCoastLength' = 'Coastline length (CLL)',
                              'logProteinDiet' = 'Marine Protein Consumption (MPC)',
                              'logtotalGearTonnes' = 'Gear-Restricted Landed Tonnes (GLT)',
                              #'ReefFishers' = 'Reef Fishers (RFF)',
                              'logFishProd' = 'Marine Fisheries Production (MFP)',
                              'logChondCatch' = 'Chondrichthyes Catches (CHC)')) %>%
  mutate(Var1 = dplyr::recode(Var1, 'logEstDis' = 'ESD',
                              'logPprodMean' = 'PPD',
                              'logFinUSD' = 'FXP',
                              'logCoastPop' = 'CTP',
                              #'logChondLand' = 'CHL',
                              'SstMean' = 'SST',
                              'logMang' = 'MNG',
                              'HDI' = 'HDI',
                              #'EPI' = 'EPI',
                              'WGI' = 'WGI',
                              'OHI' = 'OHI',
                              'NBI' = 'NBI',
                              'logIuu' = 'IUU',
                              'logGDP' = 'GDP',
                              'logCoastLength' = 'CLL',
                              'logProteinDiet' = 'MPC',
                              'logtotalGearTonnes' = 'GLT',
                              #'ReefFishers' = 'RFF',
                              'logFishProd' = 'MFP',
                              'logChondCatch' = 'CHC')) %>%
  # reverse the order of x to get plot aligned with axes
  mutate(Var1 = as.factor(Var1)) %>%
  mutate(Var1 = factor(Var1, levels = rev(levels(Var1)))) %>%
  ggplot(., aes(x = Var1, y = Var2, fill = value)) +
  geom_tile(colour = 'white') +
  scale_fill_gradient2(high = 'red', low = 'blue', mid = 'white',
                      midpoint = 0, limit = c(-1, 1), space = 'Lab') +
                      #name = 'Pearson\nCorrelation') +
  coord_fixed() +
  geom_text(aes(x = Var1, y = Var2, label = value), colour = 'black', size = 3) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1,
                                   size = 12, hjust = 1),
        legend.position = 'none') +
        #legend.justification = c(1, 0),
        #legend.position = c(0.95, 0.85),
        #legend.direction = 'horizontal') +
  guides(fill = guide_colourbar(barwidth = 7, barheight = 1,
                                title.position = 'top', title.hjust = 0.5)) +
  labs(title = '', 
       y = ' ',
       x = ' ') +
  presentation_theme()

print(corfig)

ggsave('../Figures/Publication/CorHeatMap_190213.pdf', corfig,
       height = 19.05, width = 30.59, units = c('cm'))

#  -----------------------------------------------------------------
#  Partial dependence plots ----------------------------------------
#  -----------------------------------------------------------------

setwd('/users/helenyan/desktop/school/directed studies 2018/datasets/GBM out')

library(tidyverse)

figs <- list.files(pattern = '*_190201.csv')
dfs <- lapply(figs, read.csv)

for(i in 1:20) {
  
  #pdf(paste('../Figures/2GBMout', i, '_181212.pdf', sep = ''))
  
  pdpplot <- 
    ggplot(dfs[[i]], aes_string(x = colnames(dfs[[i]][2]), y = colnames(dfs[[i]][3]), 
                                group = 1)) +
    geom_ribbon(aes(ymin = totmin, ymax = totmax),
                alpha = 0.6, fill = 'steelblue4') +
    geom_line(size = 1) +
    #scale_y_continuous(limits = c(0.1, 0.7)) +
    geom_rug(sides = 'b') +
    labs(y = 'Partial Response on Occurrence') +
    presentation_theme()
  
  print(pdpplot)
  
  #dev.off()
  
}

#  -----------------------------------------------------------------
#  Multipanel plots ------------------------------------------------
#  -----------------------------------------------------------------

# Match Trevor Branch's figures with standardized axes -------------
library(facetscales)
library(gridExtra)
library(grid)

pdp_theme <- 
  function(axis_text_size = 13, axis_y_ticks = element_line()) {
  theme(panel.background = element_blank(),
        axis.line.y = element_line(colour = 'grey60'),
        axis.line.x = element_line(colour = 'grey60'),
        axis.text.y = element_text(size = axis_text_size, 
                                   colour = 'grey20'),
        axis.text.x = element_text(colour = 'grey20', size = 13),
        axis.ticks.y = axis_y_ticks,
        axis.title = element_text(size = 20, 
                                  colour = 'grey20'),
        strip.text = element_blank(),
        panel.spacing = unit(0, 'lines'),
        panel.border = element_rect(colour = 'grey60', 
                                    size = 1, fill = NA))
}

# try for pressures -----------------------
diet <- dfs[[9]]
land <- dfs[[10]] 
catch <- dfs[[2]]
pop <- dfs[[4]] 

#long_fun <- function(x) {
#  x %>% 
#    dplyr::select(-X) %>% 
#    gather(variable, stdvalue, 1) %>% 
#    mutate_at(vars(stdvalue), funs(scale(.)))
#}

long_fun <- function(x) {
  x %>% 
    dplyr::select(-X) %>% 
    gather(variable, stdvalue, 1) %>% 
    mutate_at(vars(stdvalue), list(~ scale(.)))
}

fishpressures <- list(diet, land, catch, pop) %>% 
  lapply(long_fun)

finalfish <- data.frame()
for(i in 1:4) {
  df <- fishpressures[[i]]
  finalfish <- rbind(finalfish, df)
}

finalfish_f <- 
  finalfish %>% 
  mutate(variable = factor(variable, 
                           levels = c('logProteinDiet', 'logtotalGearTonnes',
                                      'logChondCatch', 'logCoastPop')))

fishscales <- list(
  'logProteinDiet' = scale_y_continuous(limits = c(0.15, 0.7),
                                        breaks = seq(0.1, 0.7, 0.1)),
  'logtotalGearTonnes' = scale_y_continuous(limits = c(0.2, 0.6), 
                                            breaks = seq(0.2, 0.6, 0.1)),
  'logChondCatch' = scale_y_continuous(limits = c(0.34, 0.45),
                                       breaks = seq(0.36, 0.42, 0.06)),
  'logCoastPop' = scale_y_continuous(limits = c(0.35, 0.45),
                                     breaks = seq(0.36, 0.4, 0.04))
)

fishrug <- c(0.15, 0.2, 0.34, 0.35)

fishtitle <- 
  data.frame(
    label = c('a) Marine protein consumption', 
              'b) Gear-restricted landings',
              'c) Chondrichthyes landings', 
              'd) Coastal population'),
    variable = c('logProteinDiet', 
                 'logtotalGearTonnes', 
                 'logChondCatch', 
                 'logCoastPop'),
    x = c(-1, -1.1, -1.09, -1.26),
    y = c(0.7, 0.6, 0.43, 0.43)
  )


fish <- 
  ggplot(finalfish_f, aes(x = stdvalue, y = totmean)) +
  geom_ribbon(aes(ymin = totmin, ymax = totmax),
              alpha = 0.6, fill = 'dodgerblue') +
  geom_line(size = 1) +
  geom_rug(sides = 'b') +
  #facet_grid(variable ~ ., space = 'free_y', scale = 'free_y')
  facet_grid_sc(rows = vars(variable), space = 'free_y', 
                scales = list(y = fishscales)) +
  pdp_theme() +
  labs(x = '', y = 'Marginal Effect on Occurrence') +
  theme(plot.margin = unit(c(0.1, 0, -0.5, 1), 'cm'))
  #geom_text(data = fishtitle, mapping = aes(x = x, y = y, label = label), size = 5)

print(fish)


fishscales_test <- list(
  'logProteinDiet' = scale_y_continuous(limits = c(0.15, 0.7),
                                        breaks = seq(0.1, 0.7, 0.1)),
  'logtotalGearTonnes' = scale_y_continuous(limits = c(0.2, 0.6), 
                                            breaks = seq(0.2, 0.6, 0.1)),
  'logChondCatch' = scale_y_continuous(limits = c(0.2, 0.45),
                                       breaks = seq(0.30, 0.42, 0.06)),
  'logCoastPop' = scale_y_continuous(limits = c(0.2, 0.45),
                                     breaks = seq(0.30, 0.4, 0.04))
)

test_fish <- 
  ggplot(finalfish_f, aes(x = stdvalue, y = totmean)) +
  geom_ribbon(aes(ymin = totmin, ymax = totmax),
              alpha = 0.6, fill = 'dodgerblue') +
  geom_line(size = 1) +
  geom_rug(sides = 'b') +
  #facet_grid(variable ~ ., space = 'free_y', scale = 'free_y')
  facet_grid_sc(rows = vars(variable), space = 'free_y',
                scales = list(y = fishscales_test)) +
  pdp_theme() +
  labs(x = '', y = 'Marginal Effect on Occurrence') +
  theme(plot.margin = unit(c(0.1, 0, -0.5, 1), 'cm'))

print(test_fish)

# try for management -----------------------
ohi <- dfs[[13]]
nbi <- dfs[[11]]
hdi <- dfs[[1]]
wgi <- dfs[[20]]
gdp <- dfs[[6]]

manage <- list(ohi, nbi, hdi, wgi, gdp) %>% 
  lapply(long_fun)

finalmanage <- data.frame()
for(i in 1:5) {
  df <- manage[[i]]
  finalmanage <- rbind(finalmanage, df)
}

finalmanage_f <- 
  finalmanage %>% 
  mutate(variable = factor(variable, levels = c('OHI', 'NBI', 'HDI', 
                                                'WGI', 'logGDP')))

manscales <- 
  list(
    'OHI' = scale_y_continuous(limits = c(0.3, 0.5), breaks = seq(0.3, 0.45, 0.05)),
    'NBI' = scale_y_continuous(limits = c(0.32, 0.5), breaks = seq(0.35, 0.45, 0.05)),
    'HDI' = scale_y_continuous(limits = c(0.32, 0.45), breaks = seq(0.36, 0.42, 0.06)),
    'WGI' = scale_y_continuous(limits = c(0.32, 0.5), breaks = seq(0.35, 0.45, 0.05)),
    'logGDP' = scale_y_continuous(limits = c(0.3, 0.45), breaks = seq(0.32, 0.4, 0.08))
  )

mantitle <- 
  data.frame(
    label = c('e) Ocean Health Index', 'f) National Biodiversity Index',
              'g) Human Development Index', 'h) World Governance Index', 
              'i) Gross Domestic Product'),
    variable = c('OHI', 'NBI', 'HDI', 'WGI', 'logGDP'),
    x = c(-1.2, -1.05, -1, -1.07, -1.1),
    y = c(0.5, 0.48, 0.45, 0.44, 0.48)
  )

man <- 
  ggplot(finalmanage_f, aes(x = stdvalue, y = totmean)) +
  geom_ribbon(aes(ymin = totmin, ymax = totmax),
              alpha = 0.6, fill = 'deeppink4') +
  geom_line(size = 1) +
  facet_grid_sc(rows = vars(variable), space = 'free_y', scales = list(y = manscales)) +
  pdp_theme() +
  labs(y = '', x = '') +
  theme(plot.margin = unit(c(0.1, 0, -0.5, -0.05), 'cm'))
  #geom_text(data = mantitle, mapping = aes(x = x, y = y, label = label), size = 5)

print(man)

# try for ecology -----------------------
coast <- dfs[[3]]
mang <- dfs[[7]]
est <- dfs[[5]]
prod <- dfs[[8]]
sst <- dfs[[19]]

eco <- list(coast, mang, est, prod, sst) %>% 
  lapply(long_fun)

finaleco <- data.frame()
for(i in 1:5) {
  df <- eco[[i]]
  finaleco <- rbind(finaleco, df)
}

finaleco_f <- 
  finaleco %>% 
  mutate(variable = factor(variable, levels = c('logCoastLength', 'logMang',
                                                'logEstDis', 'logPprodMean',
                                                'SstMean')))

ecoscales <- list(
  'logCoastLength' = scale_y_continuous(limits = c(0.1, 0.8),
                                        breaks = seq(0.1, 0.8, 0.1)),
  'logMang' = scale_y_continuous(limits = c(0.3, 0.6),
                                 breaks = seq(0.3, 0.6, 0.1)),
  'logEstDis' = scale_y_continuous(limits = c(0.3, 0.6),
                                   breaks = seq(0.3, 0.5, 0.1)),
  'logPprodMean' = scale_y_continuous(limits = c(0.34, 0.5),
                                      breaks = seq(0.35, 0.45, 0.1)),
  'SstMean' = scale_y_continuous(limits = c(0.35, 0.5), 
                                 breaks = seq(0.35, 0.45, 0.1))
)

ecotitle <- 
  data.frame(
    label = c('j) Coastline length', 'k) Mangrove cover', 'l) Estuaries discharge rate',
              'm) Primary productivity', 'n) Sea surface temperature'),
    variable = c('logCoastLength', 'logMang', 'logEstDis', 'logPprodMean', 'SstMean'),
    x = c(-1.02, -1.02, -0.7, -0.85, -0.65),
    y = c(0.8, 0.59, 0.59, 0.46, 0.46)
  )

ecology <- 
  ggplot(finaleco_f, aes(x = stdvalue, y = totmean)) +
  geom_ribbon(aes(ymin = totmin, ymax = totmax),
              alpha = 0.6, fill = 'darkgreen') +
  geom_line(size = 1) +
  #facet_grid(variable ~ ., space = 'free_y', scales = 'free_y') +
  facet_grid_sc(rows = vars(variable), space = 'free_y', 
                scales = list(y = ecoscales)) +
  pdp_theme() +
  labs(y = '', x = '') +
  theme(plot.margin = unit(c(0.1, 0.1, -0.5, -0.05), 'cm'))
  #geom_text(data = ecotitle, mapping = aes(x = x, y = y, label = label), size = 5)

print(ecology)

allstd <- grid.arrange(fish, man, ecology, ncol = 3, 
             bottom = grid::textGrob('Standardized Value', 
                               gp = gpar(fontsize = 18, col = 'grey20'), 
                               vjust = 1e-5, hjust = 0.35))

ggsave('../../Figures/Publication/UnlabelledPDPstd_190206.pdf', allstd,
       height = 19.05, width = 30.59, units = c('cm'))


# ------------------------------------------------------------------------------
# Trevor Branch's figures with unstandardized axes -----------------------------
# ------------------------------------------------------------------------------

setwd('/users/helenyan/desktop/school/directed studies 2018/datasets/GBM out')
library(cowplot)
figs <- list.files(pattern = '*_190201.csv')

dfs <- 
  lapply(figs, read.csv)

pdp_plot <- function(data, colour) {
  ggplot(data, aes_string(x = colnames(data[2]), y = colnames(data[3]))) +
    geom_ribbon(aes(ymin = totmin, ymax = totmax), alpha = 0.6, fill = colour) +
    geom_line(size = 1) +
    labs(y = '', x = '') +
    panel_border(remove = FALSE) +
    theme(axis.line.y = element_line(colour = 'grey60', size = 0.5),
          axis.line.x = element_line(colour = 'grey60', size = 0.5),
          panel.background = element_blank(),
          axis.text.y = element_text(size = 13, colour = 'grey20'),
          axis.text.x = element_text(size = 10, colour = 'grey20'),
          axis.title.y = element_text(size = 15, colour = 'grey20'),
          axis.title.x = element_text(size = 15, colour = 'grey20'),
          panel.border = element_rect(size = 1, colour = 'grey60', fill = NA))
}

# fishing figs ------------------
protein <- pdp_plot(dfs[[9]], 'dodgerblue') +
  theme(plot.margin = unit(c(0.5, 0, -1, -0.05), 'cm')) +
  scale_y_continuous(limits = c(0.15, 0.7), breaks = seq(0.2, 0.7, 0.1))

gear <- pdp_plot(dfs[[10]], 'dodgerblue') +
  theme(plot.margin = unit(c(0.2, 0, -1, -0.05), 'cm')) +
  scale_y_continuous(limits = c(0.2, 0.6), breaks = seq(0.2, 0.6, 0.1)) +
  ylab('Marginal Effect on Occurrence')

catch <- pdp_plot(dfs[[2]], 'dodgerblue') +
  theme(plot.margin = unit(c(0.2, 0, -1, -0.13), 'cm')) +
  scale_y_continuous(limits = c(0.34, 0.45), breaks = seq(0.35, 0.45, 0.1))

pop <- pdp_plot(dfs[[4]], 'dodgerblue') +
  theme(plot.margin = unit(c(0.2, 0, 0.3, -0.13), 'cm')) +
  scale_y_continuous(limits = c(0.35, 0.45), breaks = seq(0.36, 0.4, 0.04)) +
  scale_x_continuous(limits = c(10, 20), breaks = seq(10, 20, 2.5))


fishing <- plot_grid(protein, gear, catch, pop, ncol = 1, align = 'v',
          rel_heights = c(3.8, 3.6, 1.5, 2), rel_widths = c(1, 1, 1, 1))

# management figs ----------------------
ohi <- pdp_plot(dfs[[13]], 'deeppink4') +
  theme(plot.margin = unit(c(0.5, 0, -1, -0.05), 'cm')) +
  scale_x_continuous(limits = c(40, 80), breaks = seq(40, 80, 10))

nbi <- pdp_plot(dfs[[11]], 'deeppink4') +
  theme(plot.margin = unit(c(0.2, 0, -1, -0.05), 'cm'))

hdi <- pdp_plot(dfs[[1]], 'deeppink4') +
  theme(plot.margin = unit(c(0.2, 0, -1, -0.05), 'cm'))

wgi <- pdp_plot(dfs[[20]], 'deeppink4') +
  theme(plot.margin = unit(c(0.2, 0, -1, -0.05), 'cm'))

gdp <- pdp_plot(dfs[[6]], 'deeppink4') +
  theme(plot.margin = unit(c(0.2, 0, 0.3, -0.13), 'cm')) +
  scale_y_continuous(limits = c(0.30, 0.48), breaks = seq(0.32, 0.44, 0.06)) +
  xlab('Value')


management <- plot_grid(ohi, nbi, hdi, wgi, gdp, ncol = 1, align = 'v',
                        rel_heights = c(1.2, 1.1, 0.8, 0.8, 0.9))

# ecology figs ----------------------
coast <- pdp_plot(dfs[[3]], 'darkgreen') +
  theme(plot.margin = unit(c(0.5, 0.3, -1, -0.05), 'cm'))

mang <- pdp_plot(dfs[[7]], 'darkgreen') +
  theme(plot.margin = unit(c(0.2, 0.3, -1, -0.05), 'cm'))

est <- pdp_plot(dfs[[5]], 'darkgreen') +
  theme(plot.margin = unit(c(0.2, 0.3, -1, -0.05), 'cm'))

prod <- pdp_plot(dfs[[8]], 'darkgreen') +
  theme(plot.margin = unit(c(0.2, 0.3, -1, -0.05), 'cm')) +
  scale_y_continuous(limits = c(0.34, 0.43), breaks = seq(0.35, 0.4, 0.05))

sst <- pdp_plot(dfs[[19]], 'darkgreen') +
  theme(plot.margin = unit(c(0.2, 0.3, 0.3, -0.13), 'cm')) +
  scale_y_continuous(limits = c(0.35, 0.45), breaks = seq(0.35, 0.4, 0.05))

ecology <- plot_grid(coast, mang, est, prod, sst, ncol = 1, align = 'v',
                     rel_heights = c(3, 1.3, 1.3, 1, 1.5))


allplots <- plot_grid(fishing, management, ecology, ncol = 3, align = 'hv')
print(allplots)

ggsave('../../Figures/Publication/UnlabelledPDP_190207.pdf', allplots,
       height = 19.05, width = 30.58, units = c('cm'))




dfslong <- 
  lapply(figs, function(x) {
    df <- read.csv(x) %>% 
      dplyr::select(-1) %>% 
      gather(variable, value, 1)
    
    return(df)
  })

alldfs <- data.frame()
for(i in 1:20) {
  datf <- dfs[[i]]
  alldfs <- rbind(alldfs, datf)
}

longdfs <- 
  alldfs %>% 
  dplyr::filter(variable != 'occurrence' |
                variable != 'speciesdwarf' |
                variable != 'specieslarge' |
                variable != 'speciesgreen' |
                variable != 'speciessmall' |
                variable != 'speciesnarrow') %>% 
  mutate(category = case_when(variable == 'HDI' ~ 'management',
                              variable == 'logGDP' ~ 'management',
                              variable == 'NBI' ~ 'management',
                              variable == 'OHI' ~ 'management',
                              variable == 'WGI' ~ 'management',
                              variable == 'logChondCatch' ~ 'fishing',
                              variable == 'logCoastPop' ~ 'fishing',
                              variable == 'logProteinDiet' ~ 'fishing',
                              variable == 'logtotalGearTonnes' ~ 'fishing',
                              variable == 'logCoastLength' ~ 'ecology',
                              variable == 'logEstDis' ~ 'ecology',
                              variable == 'logMang' ~ 'ecology',
                              variable == 'logPprodMean' ~ 'ecology',
                              variable == 'SstMean' ~ 'ecology'))


# fishing pressures ---------------------------------

fish <- 
  longdfs %>% 
  filter(category == 'fishing')



# ------------------------------------------------------------------
# Sawfish Search Figure --------------------------------------------
# ------------------------------------------------------------------
searchRaw <- read_csv('../../../Datasets/SawfishSearchMethods_190312.csv')
searchCoord <- read_csv('../SawfishSearchCountriesComplete_190414.csv')

# want long style where twe have a study type in the country - size of the dot
# corresponding to the number of studies done in the area

searchClean <- 
  searchRaw %>% 
  # remove species from further analyses
  # also remove reference IDs, citation, notes, and year
  select(-species1:-species5, -citation, -notes, -year) %>% 
  # gather the different survey types
  gather(key = key, value = method, method1:method4) %>% 
  # gather the different countries now
  gather(key = key, value = country, country1:country25) %>% 
  select(-key) %>% 
  drop_na() %>% 
  mutate(method = gsub(' ', '_', method)) %>% 
  mutate(method = recode(method,
                         'fisheries' = 'Bather Protection Nets & Fisheries',
                         'bather_net' = 'Bather Protection Nets & Fisheries',
                         'collection' = 'Museum Records',
                         'museum' = 'Museum Records',
                         'media' = 'Media Reports',
                         'historical_data' = 'Media Reports',
                         'direct_survey' = 'Literature & Expert Advice',
                         'encounter_data' = 'Literature & Expert Advice',
                         'literature' = 'Literature & Expert Advice',
                         'expert' = 'Literature & Expert Advice',
                         'photo' = 'Photographs',
          'fishers_ecological_knowledge' = "Fishers' Ecological Knowledge")) %>% 
  group_by(country, method) %>% 
  summarise(total = n()) %>% 
  left_join(searchCoord, by = c('country' = 'country'))
  

unique(searchClean$method)

# make a dataframe of the countries 
#searchCountries <- 
#  searchClean %>% 
#  select(country) %>% 
#  distinct(country, .keep_all = TRUE)

#write.csv(searchCountries, '../SawfishSearchCountriesEmpty_190414.csv')
library(rnaturalearth)
library(rnaturalearthdata)
library(sf)

world <- 
  ne_countries(scale = 'medium',
               type = 'countries',
               returnclass = 'sf') %>% 
  dplyr::filter(continent != 'Antarctica')

methodsMap <- 
  ggplot(data = world) +
  geom_sf(fill = 'grey75', color = 'grey80', size = 0.2) +
  #geom_sf(fill = 'grey25', color = 'grey30', size = 0.2) +
  geom_point(data = searchClean, 
             aes(x = long, y = lat,
                 colour = method,
                 size = total),
             alpha = 0.5) +
  scale_size(range = c(4, 14)) +
  theme(panel.background = element_blank(),
        panel.border = element_blank(),
        axis.title = element_blank(),
        axis.text = element_blank(),
        legend.key = element_blank(),
        legend.background = element_rect(fill = 'white'),
        legend.position = c(0.17, 0.25),
        legend.text = element_text(size = 11),
        legend.title = element_text(size = 13)) +
  guides(size = FALSE,
         colour = guide_legend(override.aes = list(size = 6))) +
  labs(colour = 'Method')

ggsave('../../Figures/Publication/MapMethods_190414.pdf', methodsMap, 
       height = 19.05, width = 30.58, units = c('cm'))
  






  