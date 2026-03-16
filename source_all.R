library(tidyverse)
library(here)
library(rnaturalearth)
library(sf)
library(countrycode)

source("settings.R")
lapply(
  list.files("code",
    pattern = "\\.R$",
    ignore.case = TRUE,
    full.names = TRUE,
    recursive = TRUE
  ),
  source
)
