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
#gdrive <- "../Users/emilykelman/Google\ Drive" #emily's path
gdrive <- "../../../Google\ Drive" #ctw path
#gdrive <- "" #julie's path
#set pathway to data folder on Google Drive
datapath <- paste0(gdrive, "/KelmanProject/Data/")

# read in desired abundance matrix and functional traits data frame
comm <- read.csv(paste0(datapath, "bos_pooled_relabundance.csv"))
traits <- read.csv(paste0(datapath, "fxnl_trait_community_df.csv"))

# prep community abundance and traits df for community weighted variance
# check both
str(comm) # X = rownames
str(traits) # X = rownames
# turn comm back into matrix with sites as rownames
rownames(comm) <- comm$X
comm <- as.matrix(comm)
comm <- comm[,-1] # remove first column x
# store spp in case want
comm_spp <- colnames(comm)

# turn traits back into data frame with spp as rownames
rownames(traits) <- traits$X
traits <- traits[,-1] # remove first column x
study_traits <- colnames(traits)
study_traits

bos_cwv <- null.mom(comm, "RMR", df = traits, nreps = 9999)

