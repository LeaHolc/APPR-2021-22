library(knitr)
library(rvest)
library(gsubfn)
library(tidyr)
library(tmap)
library(shiny)
library(readr)
library(dplyr)
library(tibble)

library(readxl)
library(tidyverse)

library(ggplot2)
library(sp)
library(rgdal)
library(rgeos)
library(raster)

library(GGally)




options(gsubfn.engine="R")

# Uvozimo funkcije za pobiranje in uvoz zemljevida.
source("lib/uvozi.zemljevid.r", encoding="UTF-8")
