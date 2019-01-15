#Kelman 1/15/19 Trait Species BOS overlap

#load needed libraries
library(tidyverse)
library(readxl)

#read in dataset
bos_dat <- read_csv("/Users/emilykelman/Google\ Drive/KelmanProject/tgsna_monitoring_19912016.csv")
trait_dat <- read_xlsx("/Users/emilykelman/Google\ Drive/KelmanProject/Trait_species_list_veg_dormancy.xlsx")

#look at structure of data frames
glimpse(bos_dat) #tidyverse function
str(bos_dat) #base R function
glimpse(trait_dat)

#what species in trait dataset are in bos dataset (without data cleaning)
trait_species <- unique(trait_dat$Species..1)
bos_species <- unique(bos_dat$OSMP_Code)
summary(trait_species %in% bos_species)
