# community weighted variance


# -- SETUP -----
rm(list=ls()) # start with clean environment
options(stringsAsFactors = FALSE) #character variables never factor by default 
# set working directory to folder where this script lives
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#install/load needed libraries
libs <- c("tidyverse", "picante") # FD for CWM; vegan for transforming spp abundance matrix and multivar ordination
for(l in libs){
  if(!require(l, character.only = T)){ # this checks whether package is installed or not; if not, install package
    install.packages(l, dep = T)
  }
  library(l,character.only = T) # load library
}

# source CWV function
source(traitMoments.test.R)
