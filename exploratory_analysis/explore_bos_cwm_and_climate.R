# community weighted means calculation and exploratory analysis
# authors: ek, jl, ctw
# feb 2019


# script purpose: 
# calculate community weighted means for bos xtg communities by year, explore relationship to environmental fluctuation
# create figures to visualize relationship btwn cwm and environment (for ek draft #2 due feb 15)

# script steps:
# 1) read in: cleaned bos dataset, mature traits dataset, spp lookup table (to crosswalk bos and trait dat), climate dataset
# 2) prep datasets as needed
# 3) set dyanmic components of community weighted means calculation (e.g. desired transects, years, traits, list of spp that overlap in trait and veg dataset for those conditions)
# 4) create abundance matrix, functional traits data frame, and community weighted means
# 5) explore CWM results with environment and cover data, create figures that visualize those relationships



# -- SETUP -----
rm(list=ls()) # start with clean environment
options(stringsAsFactors = FALSE) #character variables never factor by default 


#install/load needed libraries
libs <- c("tidyverse", "FD", "vegan") # FD for CWM; vegan for transforming spp abundance matrix and multivar ordination
for(l in libs){
  if(!require(l, character.only = T)){ # this checks whether package is installed or not; if not, install package
    install.packages(l, dep = T)
  }
  library(l,character.only = T) # load library
}


#set relative pathway to Google Drive --> user will need to adjust this <---
# **uncomment whichever path is yours when running script
#gdrive <- "/Users/emilykelman/Google\ Drive" #emily's path
gdrive <- "../../Google\ Drive" #ctw path
#gdrive <- "" #julie's path

#read in datasets
bos_dat <- read.csv(paste0(gdrive, "/KelmanProject/Data/tgsna_monitoring_19962016_clean.csv"))
trait_dat <- read.csv(paste0(gdrive, "/KelmanProject/Data/traits_mature_w_seedmass.csv"))
spp_lookup <- read.csv(paste0(gdrive, "/KelmanProject/Data/tgsna_trait_spp_lookup.csv"))
clim_dat <- read.csv(paste0(gdrive, "/KelmanProject/Data/boulder_climate.csv"))

# did all datasets read in as expected?
## bos xeric tallgrass cover data
names(bos_dat)
str(bos_dat)
bos_dat <- bos_dat[,!names(bos_dat) %in% ("X")] #remove index column created when wrote out csv from R
lapply(bos_dat, function(x) summary(is.na(x))) # looks goods

## mature trait data
names(trait_dat)
str(trait_dat) 
# what is diff between species and species.1? are they the same
summary(trait_dat$species == trait_dat$species.1) #yes, remove species.1 to avoid confusion
trait_dat <- trait_dat[,!names(trait_dat) %in% "species.1"]
names(trait_dat)[names(trait_dat)=="species"] <- "JL_Code" #rename "species" code to "JL_code" to match spp lookup table

## bos-trait spp lookup table
names(spp_lookup)
str(spp_lookup)
spp_lookup <- spp_lookup[,!names(spp_lookup) %in% "X"] # remove index created when wrote out csv from R
lapply(spp_lookup, function(x) summary(is.na(x))) # expect 1 NA in OSMP cols (no tall oatgrass in bos dat), looks good


## climate dataset
names(clim_dat)
str(clim_dat)
summary(clim_dat)



# -- DATA EXPLORE -----
#Trait steps
trait_species <- unique(maturetraits$`species`)
bos_species <- unique(cover7_1$OSMP_Code)
table(trait_species %in% bos_species) #of trait spp, how many overlap with bos species?
summary(bos_species %in% trait_species) #of bos pp, how many overlap with trait species?

# which trait species are not in bos dataset?
bos_species[which(!bos_species %in%trait_species )]
# what are their scientific names?
trait_dat[!trait_dat$`Species code` %in% bos_species, c("Species", "Genus")]




##############################################
# --- CALCULATE COMMUNITY WEIGHTED MEANS -----
##############################################
# Community steps:
# 1) specify conditions of community weighted means calculate (i.e. desired sites, year, traits to use, species that overlap in both bos and trait dataset that those site-years)
# 2) create abundance matrix (site-year = rows, spp = cols) ** spp must match spp in fxnl trait df
# 3) create functional traits data frame (spp = rows, trait = cols) ** spp must match spp in abundance matrix
# 4) run FD::functcomp() to create community weighted means


# -- (1) Set dynamic components ----- 
# ** EK: modify this part, all code below should be generic (so not creating a bunch of data frames and hand editing many lines of code)

# specify transects and yrs to be used in community weighted means calculation
transects <- c("7_1") # transect_ID value; if want all use: unique(bos_dat$transect_ID)
yrs <- c(1991:2016) # yrs in dataset range 1991-2016

# specify vector of desired traits
traits <- colnames(trait_dat)[c(10,12:14,17,18)] # this will be only line to modify, everything below here generic
traits # visually check if traits you expected


# create vector of spp codes that exist in both veg data and trait data for selected sites, yrs, and traits
abundance_spp <- with(bos_dat, unique(OSMP_Code.clean[transect_ID %in% transects & Year %in% yrs]))
abundance_spp <- abundance_spp[abundance_spp %in% spp_lookup$OSMP_Code.clean] #only keep spp that are in bos-trait lookup table

trait_subset <- trait_dat[colnames(trait_dat) %in% c("JL_Code", traits)]
trait_subset$NAcheck <- apply(trait_subset[2:ncol(trait_subset)], 1, function(x) sum(is.na(x))) #this calculates how many NAs there are across traits selected per species
trait_spp <- trait_subset$JL_Code[!trait_subset$NAcheck > 0] # <-- define here how many NAs are okay to have (or can use other criteria [e.g. it's okay for certain fxnl traits to be NA but not others])

# these are the species that overlap for the traits, sites, and years selected
overlap <- subset(spp_lookup, JL_Code %in% trait_spp & OSMP_Code.clean %in% abundance_spp)
# use overlap to filter abundance matrix and fxnl trait data frame for the CMW below


# -- (1) Create species abundance matrix -----
#Formatting community data for community weighted means
#1 subset cover data for desired sites, yrs
abundance <- subset(bos_dat, transect_ID %in% transects & Year %in% yrs) %>%
  group_by(transect_ID, Year, OSMP_Code.clean) %>%
  summarize(cover = sum(Cov_freq_val)) %>% # sum all hits per species per transect per year
  ungroup() %>% # remove grouping
  left_join(overlap[c("JL_Code", "OSMP_Code.clean")]) %>% # add lookup table columns for subsetting to spp in trait dataset
  subset(!is.na(JL_Code)) %>% # only keep spp in trait dataset
  mutate(sitekey = paste(transect_ID, Year, sep = ".")) %>% # concatenate transect and year for unique site ID
  dplyr::select(-OSMP_Code.clean) %>% # remove OSMP_Code and use JL_code for spp names
  spread(JL_Code, cover, fill = 0) %>% # spread out species cover, fill any empty cells (i.e. species not present) with "0"
  as.data.frame()

# pause to create site-environmental matrix in case want to do exploratory ordination later
site_matrix <- abundance[c("transect_ID", "Year", "sitekey")]

# continue making spp abundance matrix
rownames(abundance) <- abundance$sitekey #assign unique transect-year as rowname

abundance <- dplyr::select(abundance, -c(transect_ID, Year, sitekey)) %>% #remove transect_ID, Year, and site
  as.matrix() #reclass object as matrix

# convert to relative abundance
rel_abundance <- decostand(abundance, 'total')

# check that relative abundances add to 1
apply(rel_abundance, 1, sum)
sort(apply(rel_abundance, 2, sum))

# abundance matrix ready to go!


# -- (3) Create functional traits data frame ----- 
# make fxnl trait data frame
fxnl_df <- trait_dat[trait_dat$JL_Code %in% overlap$JL_Code, # <-- ROWS: select only spp that are in the overlap table
                     c("JL_Code", traits)] # <-- COLS: select only desired traits
#set species as rownames
rownames(fxnl_df) <- fxnl_df$JL_Code
fxnl_df <- fxnl_df[,!names(fxnl_df) %in% "JL_Code"] #remove species code as variable

# final check: do spp in abundance = spp in trait df?
summary(sort(unique(colnames(rel_abundance))) == sort(unique(rownames(fxnl_df)))) #should be only TRUE

# if check okay, proceed to community weighted means!


# -- (4) Create community weighted means -----
bos_cwm <- functcomp(x = fxnl_df, a = abundance)
# back out transect_ID and year
bos_cwm$sitekey <- rownames(bos_cwm) #store unique site-year key in its own column
bos_cwm$transect_ID <- gsub("[.].*$", "", bos_cwm$sitekey) #everything after period in sitekey is removed
bos_cwm$Year <- as.numeric(gsub("[0-9].*[0-9][.]", "", bos_cwm$sitekey)) #everything before period in sitekey is removed, convert year from character to number
rownames(bos_cwm) <- seq(1:nrow(bos_cwm)) #clean up rownames



##########################################
# -- EXPLORE CWM WITH OTHER DATASETS -----
##########################################

# can visualize relationship between bos CWM and environment.. CWM to total annual cover.. whatever you choose..

