### Transition Zone Cast Study Code

# Library Load ----
## Helpers
library(tidyverse)

## Data access
library(robis)

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
eastPac_bivalves <- robis::occurrence(scientificname = "Bivalvia", areaid = "31908,34312,265") %>%
  filter(., !is.na(species)) %>%
  filter(., date_year >= 1900 | as.numeric(year) >= 1900)

## Location accuracy
eastPac_bivalves <- eastPac_bivalves[which(unlist(lapply(eastPac_bivalves$decimalLatitude, decimal_places)) >= 2),]

## Bin into latitudinal bands
bivalves_bins <- floor(eastPac_bivalves$decimalLatitude)

## Only keep easternmost part of each band

