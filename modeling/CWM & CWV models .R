#' ---
#' title: "Kelman Preliminary Results"
#' author: "ek, jl, cw"
#' date: "Feb 25 2019"
#' output: github_document
#' ---
#'  Load libraries and read in data
#+ results=FALSE, message=FALSE, warning=FALSE 
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

#'
#'
CWV$spei_lag <- lag(CWV$spei_12, k=1)

