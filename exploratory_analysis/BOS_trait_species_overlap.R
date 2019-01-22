# Trait Species BOS overlap
# authors: Kelman, ctw 
# date started: 1/15/19 

# script purpose:
# determine and visualize percent of xeric tallgrass (XTG) community composition captured by species in Julie Larson's trait dataset
# address whether there is enough trait species-BOS overlap to justify using functional trait community weighted means calculated from the trait dataset as representative of XTG the community
# steps:
# 1) read in bos long-term monitoring dataset, trait dataset species list
# 2) **EK fill in
# 3) ** add however many steps you think there are

# notes:
# **EK: this space is for adding any notes you want to communicate to other potential users or reviewers about your script



# ========================
# ----- SETUP -----
#**EK: you can use whatever names and characters you want for headers; i like to block sections so I can find them easily when looking through code 

#load needed libraries
library(tidyverse)
library(readxl) # for reading in excel workbooks

#set relative pathway to Google Drive --> user will need to adjust this <---
# **uncomment whichever path is yours when running script
gdrive <- "/Users/emilykelman/Google\ Drive" #emily's path
#gdrive <- "../Google\ Drive" #ctw path
#gdrive <- "" #julie's path

# set path to datasets
# **EK: what is the difference between the function 'paste0' and 'paste'? why did I use paste0 here?
# **can query help on function by typing in console ?[function name](), e.g. ?paste0()
bos_datpath <-paste0(gdrive, "/KelmanProject/Data/tgsna_monitoring_19912016.csv")
# there are two trait datasets in Kelman Project? reading one from data folder with more recent timestamp
trait_datpath <-paste0(gdrive, "/KelmanProject/Data/Trait_species_list_veg_dormancy.xlsx")

#read in datasets
bos_dat <- read_csv(bos_datpath)
trait_dat <- read_xlsx(trait_datpath)



# --- PREP DATASETS FOR ANALYSIS -----
#look at structure of data frames
glimpse(bos_dat) #method using tidyverse function
str(bos_dat) #method using base R function
summary(bos_dat)
glimpse(trait_dat)

# subset datasets to what's needed for analysis
cover_dat <- subset(bos_dat, DataType == "COVER")


#what species in trait dataset are in bos dataset (without data cleaning)
trait_species <- unique(trait_dat$`Species code`)
bos_species <- unique(bos_dat$OSMP_Code)
table(trait_species %in% bos_species) #of trait spp, how many overlap with bos species?
summary(bos_species %in% trait_species) #of bos pp, how many overlap with trait species?
# **EK: 'table' and 'summary' are two functions that can do similar things (for logical [T/F] statements)
# ** another way to look at this:
# percent of trait species in bos species
sum(trait_species %in% bos_species)/length(trait_species)
# percent of trait species in bos species
sum(bos_species %in% trait_species)/length(bos_species) 

# which trait species are not in bos dataset?
trait_species[which(!trait_species %in% bos_species)]
# what are their scientific names?
trait_dat[!trait_dat$`Species code` %in% bos_species, c("Species", "Genus")]
## ** EK: are these species really not in the BOS dataset?
## What can we do if they actually are? What correction needs to be made?



#assign column to cover_dat to indicate whether species in trait dataset
## **EK: look up help on 'ifelse' to see what it does, what its terms are
cover_dat$trait_sp <- ifelse(cover_dat$OSMP_Code %in% trait_species, "yes", "no")
correctspeciesname <-c("ambpsic", "carpenh", "helrigs", "tradubm")
cover_dat$trait_sp <- ifelse(cover_dat$OSMP_Code %in% correctspeciesname, "yes", cover_dat$trait_sp)

# summarize cover dataset by what's in trait dataset vs what's not in trait dataset
# each area-transect combo, per year, should have 2 rows: total cover for trait species and total cover for not-trait-species
# start you code here and assign issue when you get stuck...
grpd_cover <- cover_dat %>% 
  mutate(transect_ID = paste(Area, Transect,sep = "_")) %>%
  group_by(Year, transect_ID, trait_sp) %>%
  summarize(summed_cover = sum(Cov_freq_val))






# ------ EXPLORATORY ANALYSIS -----
# make visual summary using ggplot
## **EK: try on your own once you have the summarize cover dataset made
## ** if you get stuck we can try troubleshooting via GitHub and/or work on it together next Tuesday

ggplot(grpd_cover, aes(x=trait_sp, y=summed_cover)) +
  geom_col() +
  #facet_wrap(~transect_ID) +
  facet_grid(transect_ID ~ Year)
  
  
