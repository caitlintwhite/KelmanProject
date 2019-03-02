# community weighted variance


# -- SETUP -----
rm(list=ls()) # start with clean environment
options(stringsAsFactors = FALSE) #character variables never factor by default 
# set working directory to folder where this script lives
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#install/load needed libraries
libs <- c("tidyverse", "picante", "questionr") # FD for CWM; vegan for transforming spp abundance matrix and multivar ordination
for(l in libs){
  if(!require(l, character.only = T)){ # this checks whether package is installed or not; if not, install package
    install.packages(l, dep = T)
  }
  library(l,character.only = T) # load library
}

# source CWV function
source("traitMoments.test.R")

#set relative pathway to Google Drive --> user will need to adjust this <---
# **uncomment whichever path is yours when running script
GDrive <- "/Users/emilykelman/Google\ Drive" #emily's path
#GDrive <- "/Users/serahsierra/Google\ Drive" #ctw path
#GDrive <- "" #julie's path

#set pathway to data folder on Google Drive
datapath <- paste0(GDrive, "/KelmanProject/Data/")

# read in desired abundance matrix and functional traits data frame
comm <- read.csv(paste0(datapath, "bos_pooled_relabundance.csv"))
traits <- read.csv(paste0(datapath, "fxnl_trait_community_df.csv"))
clim_dat <- read.csv(paste0(GDrive, "/KelmanProject/Data/boulder_climate.csv"))
#clim_dat <- clim_dat[c(1:23, 25, 26), ] # <-- EK fix here bc more variables added, so want to check this code is still grabbing the ones you want


# -- PREP DATA FOR CWV FUNCTION -----
# prep community abundance and traits df for community weighted variance
# check both
str(comm) # X = rownames
str(traits) # X = rownames
# **keep comm as data frame because null.mom function uses names() instead of colnames() on matrix and names will return null for matrix
rownames(comm) <- comm$X
comm <- comm[!colnames(comm) %in% "X"] # remove first column x
# store spp in case want
comm_spp <- colnames(comm)

# turn traits back into data frame with spp as rownames
rownames(traits) <- traits$X
traits <- traits[!colnames(traits) %in% "X"] # remove first column x
study_traits <- colnames(traits)
study_traits

# check rownames in fxnl dt = colnames in spp matrix
summary(colnames(comm) == rownames(traits)) # should be all true


# -- CALCULATE COMMUNITY WEIGHTED VARIANCE -----
# specify which trait you want to run
# could store each trait CWV in its own data frame object? (i.e repeat this line x times, with object name something like rmr_cwv, sla_cwv, etc.)
bos_RMRcwv <- null.mom(comm, "RMR", df = traits, nreps = 100) #lets try 100 reps (?), got similar values with 200 reps and 100 is faster
bos_RDMCcwv <- null.mom(comm, "RDMC", df = traits, nreps = 100)
bos_SLAcwv <- null.mom(comm, "SLA", df = traits, nreps = 100)
bos_seedmass_cwv <- null.mom(comm, "seed_mass", df = traits, nreps = 100)
bos_height_cwv <- null.mom(comm, "final_height_cm", df = traits, nreps = 100)

### Combine all CWV columns into a single dataframe
CWV <- cbind(bos_seedmass_cwv$obs.cwv, bos_RMRcwv$obs.cwv, bos_RDMCcwv$obs.cwv, bos_SLAcwv$obs.cwv, bos_height_cwv$obs.cwv)
CWV <- data.frame(CWV)
### set column names according to cwv output order
colnames(CWV) <- c("seedmass", "RMR", "RDMC", "SLA", "height")
# add year as common field to use in joining climate data
CWV$year <- rownames(bos_height_cwv)

CWV_climate_merge <- merge(CWV, clim_dat[c("year", "spei_12")])


#export CWV as a csv
write.csv(CWV_climate_merge, paste0(GDrive,"/KelmanProject/Data/CWV_climate_merge.csv"), row.names = F)
