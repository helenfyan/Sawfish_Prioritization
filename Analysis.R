# THIS SCRIPT GOES THROUGH THE DIFFERENT COVARIATE COMBINATIONS #

library(tidyverse)
library(broom)
library(reshape2)
library(car)

setwd('/users/helenyan/desktop/school/directed studies 2018/datasets')

allData <- read_csv('CompleteSpeciesCovariates_180924.csv')

allScaled <-
  allData %>%
  select(-NBI, -coastal.pop, -Lengthkm, -EPI, -Discharge, -ExporttoHK2010,
         -MeanPro, -GDP, -HDI, -OHI, -UnreportedPercent, -impact.percentage, 
         -no_reef_fishers, -AreaSqKM, -TotalTonnes, -WGI) %>%
  filter(presence != 'unknown') %>%
  select(-SaltmarshScale)

# Test for collinearity in covariates -----------------------------------------------

# Make a heatmap of the pairwise variable comparisons
cormatdat <- 
  allScaled %>%
  .[, c(8:22)] %>%
  drop_na()

cormat <- round(cor(cormatdat), 2)

# reorder the cormat according to correlational coefficient
reorderCormat <- function(cormat) {
  # use correlatio between variables as distance
  dd <- as.dist((1-cormat)/2)
  hc <- hclust(dd)
  cormat <- cormat[hc$order, hc$order]
}

cormat <- reorderCormat(cormat)

# get rid of half the info - redundant
GetLowerTri <- 
  function(cormat) {
    cormat[upper.tri(cormat)] <- NA
    return(cormat)
  }

lowerTri <- GetLowerTri(cormat)

meltedlowertri <- 
  melt(lowerTri, na.rm = TRUE)

lowertriplot <- 
  ggplot(meltedlowertri, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile(colour = 'white') +
  scale_fill_gradient2(low = 'blue', high = 'red', mid = 'white',
                       midpoint = 0, limit = c(-1, 1), space = 'Lab',
                       name = 'Pearson\nCorrelation') +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1,
                                   size = 12, hjust = 1)) +
  coord_fixed() +
  geom_text(aes(x = Var1, y = Var2, label = value), colour = 'black', size = 3) +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.major = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank(),
    legend.justification = c(1, 0),
    legend.position = c(0.3, 0.85),
    legend.direction = 'horizontal') +
  guides(fill = guide_colourbar(barwidth = 7, barheight = 1,
                                title.position = 'top', title.hjust = 0.5))

print(lowertriplot)

#formula <- paste('occurrence ~ ', paste(covs, collapse = ' + '))
#model1 <- glm(formula, family = binomial, large)


# Maximum model for each species after dropping the covariates ---------------------

modData <-
  allScaled %>%
  select(-HdiScale, -OhiScale, -EpiScale, -NbiScale)

vars <- names(modData)
covs <- vars[8:18]

# largetooth sawfish

modLarge <-
  modData %>%
  filter(species == 'large') %>%
  distinct(., country)



# For all species try every combination of covariates -------------------------------

# try out every combination of covariates
out <- unlist(lapply(1:15, function(n) {
  # get combinations
  combinations <- t(combn(covs, n))
  # collapse them into usable formulas
  formulas <- apply(combinations, 1, function(row) paste('occurrence ~ ', 
                                                         paste(row, 
                                                               collapse = ' + ')))
}))

test <- 
  bind_rows(lapply(out, function(frml) {
    a = glance(glm(frml, family = binomial, large))
    a$frml <- frml
    return(a)
  }))

# don't run this - it freezes RStudio