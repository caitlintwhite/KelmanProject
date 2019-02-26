Kelman Preliminary Results
================
ek, jl, cw
Feb 25 2019

Load libraries and read in data

``` r
library(tidyverse)
library(ggplot2)

#set relative pathway to Google Drive --> user will need to adjust this <---
# **uncomment whichever path is yours when running script
gdrive <- "/Users/emilykelman/Google\ Drive" #emily's path
#gdrive <- "../../../Google\ Drive" #ctw path
#gdrive <- "" #julie's path

CWV <- read.csv(paste0(gdrive, "/KelmanProject/Data/CWV_climate_merge.csv"))
CWM_regressions <- read.csv(paste0(gdrive, "/KelmanProject/Data/CWM_for_regressions.csv"))
CWM_figures <- read.csv(paste0(gdrive, "/KelmanProject/Data/CWM_for_figures.csv"))
```
