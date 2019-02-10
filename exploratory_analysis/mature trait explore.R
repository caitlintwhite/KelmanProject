#Exploratory Analysis of Mature Plant Traits Dataset
#Authors EK, ctw, jl
#date started 2/3/19

#script purpose to explore the different traits and range of values in order to connect traits back to overall species abundance
#steps 

# setup environment
rm(list=ls()) # start with clean enviro
options(stringsAsFactors = FALSE) # character strings never factors by default

#load libraries
libs <- c("tidyverse", "corrplot", "FD") # vector of libraries needed
for(l in libs){
  if(!require(l, character.only = T)){ # this checks whether package is installed or not; if not, install package
    install.packages(l, dep = T)
  }
  library(l,character.only = T) # load library
}

# ** above as an alernative to this chunk of code
# library(tidyverse)
# #install.packages("corrplot")
# library(corrplot)
# #install.packages(FD)
# library(FD)

#set relative pathways to google drive, uncomment your path when working on script 
gdrive <- "/Users/emilykelman/Google\ Drive" #emily's path
#gdrive <- "../../Google\ Drive" #ctw path
#gdrive <- jl

#set pathway to mature trait dataset and read in 
maturetraits  <- read.csv(paste0(gdrive, "/KelmanProject/Data/traits_mature_w_seedmass.csv"))
glimpse(maturetraits)
summary(maturetraits)

#read in mature trait dataset and update by removing uneccessary first column 
#maturetraits <- dat[ ,2:35] 


traitsubset <- maturetraits[ ,c(10,12:14,17,18,36)]
pairs(traitsubset)
#create a new column to quantify total increase in height. wanted to check that i correctly grouped the variables?
maturetraits <- maturetraits %>%
  group_by(species) %>%
  mutate(totalgrowth_cm = final_height_cm - start_height_cm)

#=========
#exploring with correlation matrices 

#create correlation matrix to test relationships between different traits
#SLA and final height
cor.test(maturetraits$SLA, maturetraits$final_height_cm) 
#correlation coefficient is only .166 
#create correlation matrix between SRL AND SLA
cor.test(maturetraits$SRL, maturetraits$SLA) 
#correlation coefficient .4563

#plot scatterplot of SRL and SLA to visualize relationship 
plot(maturetraits$SRL, maturetraits$SLA, xlab="Specific root length (SRL)", ylab="Specific leaf area (SLA)", main="Relationship between SLA and SRL traits")
#should i make this graph an object?

#correlation between total growth and SRL
cor.test(maturetraits$final_height_cm, maturetraits$SRL)
plot(maturetraits$final_height_cm, maturetraits$SRL)
#correlation coefficient -.1662

#how do i run a correlation for multiple variables at once?
#cor.test(maturetraits$days_to_first_measure, maturetraits$days_to_harvest, maturetraits$start_height_cm, maturetraits$start_canopyvol_cm3, maturetraits$final_height_cm, maturetraits$final_canopyvol_cm3, maturetraits$est_rootlength_m, maturetraits$RMR, maturetraits$RMR_w_rhizomes, maturetraits$RDMC, maturetraits$SRL, maturetraits$Rdiam, maturetraits$LDMC, maturetraits$SLA)

cor.test(maturetraits$LDMC, maturetraits$RDMC)
#Correlation coefficient .278

cor.test(maturetraits$est_rootlength_m, maturetraits$final_canopyvol_cm3)
#correlation coefficient .4858
rm(dat)
