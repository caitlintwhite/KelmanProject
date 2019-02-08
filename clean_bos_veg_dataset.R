# clean bos cover for kelman analysis
# feb 2019
# caitlin.t.white@colorado.edu

# script purpose:
# prepare bos xtg transect cover for integration with functional trait data and futher analysis in e. kelman's honors thesis
# specific steps:
# 1) read in raw bos xtg dataset, subset cover data only (remove frequency data)
# 2) preserve only columns needed for community weighted means analysis
# 3) prepare species code lookup table that matches OSMP spp code with trait dataset spp code
# 4) write out clean up, subsettted cover data and species code lookup table



# -- SETUP ----
# load needed libraries
library(readxl)
library(dplyr)
library(tidyr)
options(stringsAsFactors = FALSE) # treat characters as characters by default (not as factors)

# set relative path
google_drive <- "../../Google\ Drive" #ctw path

# set pathway to data subfolder in Google Drive Kelman Project
datapath <- paste0(google_drive, "/KelmanProject/Data/")

# set possible na values in datasets
NAvals <- c(" ", "", NA, "NA")

# read in raw datasets
veg_dat <- read.csv(paste0(datapath, "tgsna_monitoring_19912016.csv"),
                    stringsAsFactors = FALSE, na.strings = NAvals)
osmp_changes <- read.csv(paste0(datapath, "OSMPcodechanges.csv"),
                         stringsAsFactors = FALSE, na.strings = NAvals)
trait_dat <- read.csv(paste0(datapath,"traits_mature_w_seedmass.csv"),
                      stringsAsFactors = FALSE, na.strings = NAvals)
spp_descrip <- read_excel(paste0(datapath, "Trait_species_list_veg_dormancy.xlsx"), 
                                         sheet = "Full_species_List", na = NAvals)

# -- SUBSET AND PREP BOS DATASET ----
# need cover data, year, location, spp, spp codes, keep all hits, can remove ESCO fields
cover_dat <- subset(veg_dat, DataType == "COVER") %>% #cover data only
  unite(transect_ID, Area, Transect, remove = FALSE) %>%
  # bring in code corrections per A. Leezburg 
  left_join(osmp_changes[c("OSMP_Code", "OSMPSciName", "study.code", "study.SciName")]) %>%
  mutate(study.code = ifelse(is.na(study.code), OSMP_Code, study.code),
         study.SciName = ifelse(is.na(study.SciName), OSMPSciName, study.SciName)) %>%
  # rename to be more informative (still OSMP codes, but cleaned up per A. Leezburg recs)
  rename(OSMP_Code.clean = study.code,
         OSMPSciName.clean = study.SciName) %>%
  # create variable in cover_data that is just the genus and species (no subspecies or variant names)
  mutate(shortSciName = trimws(gsub(" ssp*.+| var*.+", "", OSMPSciName.clean))) %>% 
  dplyr::select(Project, DataType, Year, Area, transect_ID, shortSciName, OSMP_Code, OSMPSciName, OSMP_Code.clean, OSMPSciName.clean, Cov_freq_val, Frst_hit, Nativity:PSNPathway)

# -- SUSBET AND CLEAN UP SPP DESCRIP -----
spp_descrip <- spp_descrip %>%
  # correct Species and Genus in spp descrip (are reversed)
  rename(JL_code = 'Species code', # rename to remove space, indicate it's code used in larson dataset and distinguish from 'Species' field name
         Species = Genus,
         Genus = Species) %>%
  # correct spelling of various species.. 
  mutate(Species = gsub("pennsylvanica", "pensylvanica", Species), # c. pensylvanica (JL has two n's)
         Species = gsub("macanthra", "macrantha", Species), #Koeleria macrantha
         Genus = (gsub("Erysisimum", "Erysimum", Genus)), #JL has extra "si"
         Lifeform = paste0(toupper(substr(Lifeform, 1,1)),substr(Lifeform, 2, nchar(Lifeform))),
         Origin = paste0(toupper(substr(Origin, 1,1)),substr(Origin, 2, nchar(Origin)))) %>% # so casing consistent with other lifeform values
  # subset trait spp descriptive dataset
  dplyr::select(JL_code:Origin) %>%
  # create full latin name
  unite(JLSciName, Genus, Species, sep = " ", remove = FALSE)


# -- CREATE SPP LOOKUP TABLE -----
# join cover data OSMP_code and OSCPSciName columns
spp_lookup <- left_join(spp_descrip, unique(cover_dat[, c("shortSciName", "OSMP_Code.clean", "OSMPSciName.clean", "PSNPathway")]), by = c("JLSciName" = "shortSciName"))

# manual corrections
## elytrigia repens (OSMP) == elymus repens (trait data)
spp_lookup[which(spp_lookup$JL_code == "elyrep"),c("OSMP_Code", "OSMPSciName", "PSNPathway")] <- unique(cover_dat[which(cover_dat$OSMP_Code.clean=="elyrep"), c("OSMP_Code", "OSMPSciName", "PSNPathway")])
## helianthus rigidus (OSMP) == helianthus rigida (trait data)
spp_lookup[which(spp_lookup$JL_code == "helrig"),c("OSMP_Code", "OSMPSciName", "PSNPathway")] <- unique(cover_dat[which(grepl("Helianthus rig",cover_dat$OSMPSciName.clean)==TRUE), c("OSMP_Code", "OSMPSciName", "PSNPathway")])
## alyaly-alydes-alypar (OSMP) == alypar (trait data)
spp_lookup[which(spp_lookup$JL_code == "alypar"),c("OSMP_Code", "OSMPSciName", "PSNPathway")] <- unique(cover_dat[which(grepl("alypar",cover_dat$OSMP_Code.clean)==TRUE), c("OSMP_Code", "OSMPSciName", "PSNPathway")])

# arrange columns for final output
spp_lookup <- dplyr::select(spp_lookup, JL_code:Species, OSMP_Code, OSMPSciName, Lifeform:Origin, PSNPathway) %>%
  rename(LifeHistory = 'Life history')



# -- FINALIZE BOS DATASET -----
# correct missing spp names, remove spp descriptive data (since will have for trait dataset spp)
sort(unique(cover_dat$OSMP_Code))
BOS_spp <- unique(veg_dat[c("OSMP_Code", "OSMPSciName", "Esco_spcode", "Esco_specname")]) %>% arrange(OSMP_Code, OSMPSciName)
# create vector of row numbers where ESCO code is duplicated but OSMP Code/SciName is not (mismatch in OSMP)
dup_esco <- BOS_spp$Esco_spcode[duplicated(BOS_spp$Esco_spcode) & !is.na(BOS_spp$Esco_spcode)]
problem_spp <- BOS_spp[BOS_spp$Esco_spcode %in% dup_esco,] %>% arrange(., Esco_spcode)

# eliminate rows that duplicate OSMP_Code but OSMPSciName is NA (will defer to filled in OSMPSciName)
problem_spp <- arrange(problem_spp, OSMP_Code, OSMPSciName) %>% # to be sure NA in OSMPSciName comes after OSMPSciName with value for duplicated codes
  subset(!duplicated(OSMP_Code)) %>% arrange(Esco_spcode)

# also correct spp that A. Leezburg recommends combining due to inconsistent ID over the years

