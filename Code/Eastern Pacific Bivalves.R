### Transition Zone Cast Study Code

# Library Load ----
## Helpers
library(tidyverse)
library(scico)

## Data access/cleaner
library(robis)
library(CoordinateCleaner)

## Analysis packages
library(psych)

# Custom functions and settings ----
# Remove the check for internet connection, needed on some machines for robis to function
remove_has_internet <- function(){
  unlockBinding(sym = "has_internet", asNamespace("curl"))
  assign("has_internet", function() return(TRUE), envir = asNamespace("curl"))
  lockBinding(sym = "has_internet", asNamespace("curl"))
}
remove_has_internet()

# Return number of decimal places
decimal_places <- function(x) {
  if(!is.na(x)){if ((x %% 1) != 0) {
    nchar(strsplit(sub('0+$', '', as.character(x)), ".", fixed=TRUE)[[1]][[2]])
  } else {
    return(0)
  }} else {
    return(NA)
  }
}

# Case Study 1: Eastern Pacific Bivalves ----

## Get the data from robis and clean
eastPac_bivalves_raw <- robis::occurrence(scientificname = "Bivalvia", areaid = "31908,34312,265") %>%
  filter(., !is.na(species)) %>%
  filter(., date_year >= 1900 | as.numeric(year) >= 1900) %>%
  filter(., decimalLongitude < 0)

## Location accuracy
eastPac_bivalves_raw <- eastPac_bivalves_raw[which(unlist(lapply(eastPac_bivalves_raw$decimalLatitude, decimal_places)) >= 2),]

## Bin into latitudinal bands
bivalves_bins <- floor(eastPac_bivalves_raw$decimalLatitude)
eastPac_bivalves_raw$LatitudeBin <- bivalves_bins

## Only keep easternmost part of each band
eastPac_bivalves <- eastPac_bivalves_raw[0,]
for(i in unique(bivalves_bins)){
  coast <- max(eastPac_bivalves_raw$decimalLongitude[which(bivalves_bins == i)])
  coast_limit <- coast - 0.5
  
  eastPac_bivalves <- rbind(eastPac_bivalves, eastPac_bivalves_raw[which(eastPac_bivalves_raw$decimalLongitude >= coast_limit & bivalves_bins == i),])
}

## Build presence absence table
pa_bivalve <- table(eastPac_bivalves$species, eastPac_bivalves$LatitudeBin)
pa_bivalve[pa_bivalve > 1] <- 1

## Calculate objective number of factors

psych::fa.parallel(x = pa_bivalve)


## RUn factor analyses:
# Using number from parallel
fa_bivalve_objective <- psych::fa(r = pa_bivalve, nfactors = 11)
fa_bivalve_objective_loadings <- as.data.frame(fa_bivalve_objective$loadings[,c(1:11)]) %>%
  rownames_to_column(var = "LatitudinalBin") %>%
  pivot_longer(cols = -"LatitudinalBin",
               names_to = "Factor",
               values_to = "value")

fa_bivalve_objective_loadings$Factor <- as.factor(fa_bivalve_objective_loadings$Factor)
fa_bivalve_objective_loadings$LatitudinalBin <- as.numeric(fa_bivalve_objective_loadings$LatitudinalBin)

# Using number from Valentine
fa_bivalve_valentine <- psych::fa(r = pa_bivalve, nfactors = 6)
fa_bivalve_valentine_loadings <- as.data.frame(fa_bivalve_valentine$loadings[,c(1:6)]) %>%
  rownames_to_column(var = "LatitudinalBin") %>%
  pivot_longer(cols = -"LatitudinalBin",
               names_to = "Factor",
               values_to = "value")

fa_bivalve_valentine_loadings$Factor <- as.factor(fa_bivalve_valentine_loadings$Factor)
fa_bivalve_valentine_loadings$LatitudinalBin <- as.numeric(fa_bivalve_valentine_loadings$LatitudinalBin)

## Plot:
ggplot(data = fa_bivalve_objective_loadings, aes(y = value, x = LatitudinalBin, group = Factor, colour = Factor, fill = Factor)) + 
  geom_smooth(fill = NA) + theme_bw() + xlim(0,66) + coord_flip() + scale_color_scico_d(palette = "batlowK") + 
  scale_fill_scico_d(palette = "batlowK") + geom_vline(xintercept = c(24, 36, 56, 63, 28))

ggplot(data = fa_bivalve_valentine_loadings, aes(y = value, x = LatitudinalBin, group = Factor, colour = Factor, fill = Factor)) + 
  geom_smooth(fill = NA, method = "loess") + theme_bw() + xlim(0,66) + coord_flip() + scale_color_scico_d(palette = "batlowK") + 
  scale_fill_scico_d(palette = "batlowK") + geom_vline(xintercept = c(24, 36, 56, 63, 28))
  