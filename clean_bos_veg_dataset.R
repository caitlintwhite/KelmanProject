# clean bos cover for kelman analysis
# feb 2019
# caitlin.t.white@colorado.edu

# script purpose: prepare bos xtg transect cover for integration with functional trait data and futher analysis in e. kelman's honors thesis
# script steps:
# 1) read in needed datasets: raw bos xtg dataset, OSMP code change dataset (CTW created, per A. Lezburg), mature traits dataset, trait species list excel sheet
# 2) subset cover data only (remove frequency data)
# 3) QA and clean spp codes, spp names, and descriptive data, in cover data using logical QA checks (e.g. duplicated codes but mismatched descriptive values) + OSMP code changes
# 4) prepare species code lookup table that matches QA's OSMP spp code with trait dataset spp code
# 4) write out cleaned up, subsettted cover data and species code lookup table



# -- SETUP ----
rm(list = ls()) # clear environment

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
veg_dat <- read.csv(paste0(datapath, "raw/tgsna_monitoring_19912016.csv"),
                    stringsAsFactors = FALSE, na.strings = NAvals)
osmp_changes <- read.csv(paste0(datapath, "raw/OSMPcodechanges.csv"),
                         stringsAsFactors = FALSE, na.strings = NAvals)
trait_dat <- read.csv(paste0(datapath,"traits_mature_w_seedmass.csv"),
                      stringsAsFactors = FALSE, na.strings = NAvals)
spp_descrip <- read_excel(paste0(datapath, "Trait_species_list_veg_dormancy.xlsx"), 
                                         sheet = "Full_species_List", na = NAvals)



# -- CLEAN UP AND SUBSET BOS DATASET: PART 1 ----
# fill in osmp_changes study.SciName NAs
# if study.Code is an "genus sp", sciname should be "genus spp."
for(i in osmp_changes$study.code[grep(" sp$", osmp_changes$study.code)]){
  osmp_changes$study.SciName[osmp_changes$study.code == i] <- paste0(i,"p.")
}
# fill in the rest with value in OSMPSciName
for(i in which(is.na(osmp_changes$study.SciName))){
  temp_val <-  unique(veg_dat$OSMPSciName[veg_dat$OSMP_Code == osmp_changes$study.code[i]]) 
  stopifnot(length(temp_val)==1)
  osmp_changes$study.SciName[i] <- temp_val
}

# clean up and standardize OSMP Codes & OSMPSciName
# start by grabbing unique {OSMP_Code, OSMPSciName, Esco_spcode, Esco_specname} combos
BOS_spp <- unique(veg_dat[c("OSMP_Code", "OSMPSciName", "Esco_spcode", "Esco_specname")]) %>% arrange(OSMP_Code, OSMPSciName)
# create vector of OSMP Codes where ESCO code is duplicated but OSMP Code/SciName is not (mismatch in OSMP)
dup_esco <- BOS_spp$Esco_spcode[duplicated(BOS_spp$Esco_spcode) & !is.na(BOS_spp$Esco_spcode)]
mismatch <- unique(BOS_spp$OSMP_Code) %>% subset(duplicated(tolower(.)))
# create vector of OSMP codes for general genera (not species specific)
genus_unk <- unique(BOS_spp$OSMP_Code[grep(" sp", BOS_spp$OSMP_Code)])
genus_unk <- genus_unk[c(grep("1", genus_unk), which(duplicated(gsub(" 1", "", genus_unk)))-1)] # grab codes that have "1" or are duplicated when remove the 1
# create vector of OSMP codes where OSMPSciName is missing
NASciName <- unique(BOS_spp$OSMP_Code[is.na(BOS_spp$OSMPSciName)])

# subset sp codes that are problematic
problem_spp <- BOS_spp[BOS_spp$Esco_spcode %in% dup_esco |
                         BOS_spp$OSMP_Code %in% c(mismatch, tolower(mismatch)) |
                         BOS_spp$OSMP_Code %in% genus_unk |
                         BOS_spp$OSMP_Code %in% NASciName,] %>% distinct() %>% arrange(., Esco_spcode) %>%
  subset(!grepl("unk As", OSMP_Code)) # take out unk Asteracea 1 bc OSMP_Code and OSMPSciName is consistent (even tho differs on Esco Code)


# eliminate rows that duplicate OSMP_Code but OSMPSciName is NA (will defer to filled in OSMPSciName)
problem_spp <- arrange(problem_spp, OSMP_Code, OSMPSciName) %>% # to be sure NA in OSMPSciName comes after OSMPSciName with value for duplicated codes
  subset(!duplicated(OSMP_Code)) %>% arrange(Esco_spcode)
# remove any codes in osmp_changes, since that table will clean those codes
#problem_spp <- problem_spp[!problem_spp$OSMP_Code %in% osmp_changes$OSMP_Code,] #junarc, vieriri, virgulus, Callon
# create cols for corrected OSMP Code and OSMP Sci Name 
problem_spp$OSMP_Code.corrected <- problem_spp$OSMP_Code
problem_spp$OSMPSciName.corrected <- problem_spp$OSMPSciName

# automated edits..
## capitalize name if is unk sp
problem_spp$OSMP_Code.corrected <- with(problem_spp, ifelse(grepl(" sp", OSMP_Code.corrected),
                                                            # if "sp" in name, concatenate upper-case first character and rest of string as is
                                                            paste0(toupper(substr(OSMP_Code.corrected,1,1)),
                                                                   substr(OSMP_Code.corrected, 2,nchar(OSMP_Code.corrected))),
                                                            # if doesn't "sp" in name, keep as is
                                                            OSMP_Code.corrected))
# remove periods and 1s (all unknown genera will be "[genus] sp" with SciName of "[genus] spp.")
problem_spp$OSMP_Code.corrected <- trimws(gsub("[.]|1", "", problem_spp$OSMP_Code.corrected))
# if is a 6-letter code, make lower case
problem_spp$OSMP_Code.corrected <- with(problem_spp, ifelse(grepl(" ", OSMP_Code.corrected)==FALSE, # only unknown items will have space in Code
                                                            tolower(OSMP_Code.corrected), OSMP_Code.corrected))

# if code name is genus sp, sci name should be genus spp.
for(i in problem_spp$OSMP_Code.corrected[grep(" sp$", problem_spp$OSMP_Code.corrected)]){
  problem_spp$OSMPSciName.corrected[problem_spp$OSMP_Code.corrected == i] <- paste0(i,"p.")
}

# manual edits
problem_spp$OSMP_Code.corrected[problem_spp$OSMP_Code == "meloft"] <- "meloff"
problem_spp$OSMP_Code.corrected[problem_spp$OSMP_Code == "junarc"] <- "junarca"
problem_spp$OSMPSciName.corrected[problem_spp$OSMP_Code == "carnutm"] <- "Calochortus nuttallii"
problem_spp$OSMPSciName.corrected[problem_spp$OSMP_Code == "phyhedc"] <- "Physalis hederifolia var. comata"

# resort by clean vars
problem_spp <- arrange(problem_spp, OSMP_Code.corrected, OSMPSciName.corrected)

# store row numbers where OSMPSciName.correct is NA (when correct code is duplicated)
blanks <- which(is.na(problem_spp$OSMPSciName.corrected))
# iterate through and fill in NA with correct name
for(b in blanks){
  # grab unique value
  temp_val <- unique(problem_spp$OSMPSciName.corrected[problem_spp$OSMP_Code.corrected == problem_spp$OSMP_Code.corrected[b]]) %>% na.omit()
  stopifnot(length(temp_val)==1) # make sure just one value
  # assign value
  problem_spp$OSMPSciName.corrected[b] <- temp_val
}


# need cover data, year, location, spp, spp codes, keep all hits, can remove ESCO fields
cover_dat <- subset(veg_dat, DataType == "COVER") %>% #cover data only
  unite(transect_ID, Area, Transect, remove = FALSE) %>%
  left_join(problem_spp[c("OSMP_Code", "OSMP_Code.corrected", "OSMPSciName.corrected")], by = "OSMP_Code") %>%
  mutate(OSMP_Code = ifelse(is.na(OSMP_Code.corrected), OSMP_Code, OSMP_Code.corrected),
         OSMPSciName = ifelse(is.na(OSMPSciName.corrected), OSMPSciName, OSMPSciName.corrected)) %>%
  # bring in code corrections per A. Leezburg 
  left_join(osmp_changes[c("OSMP_Code", "OSMPSciName", "study.code", "study.SciName")]) %>%
  mutate(study.code = ifelse(is.na(study.code), OSMP_Code, study.code),
         study.SciName = ifelse(is.na(study.SciName), OSMPSciName, study.SciName)) %>%
  # rename to be more informative (still OSMP codes, but cleaned up per A. Leezburg recs)
  rename(OSMP_Code.clean = study.code,
         OSMPSciName.clean = study.SciName)

# look for duplicate codes one more time
clean_names <- unique(cover_dat[c("OSMP_Code.clean", "OSMPSciName.clean")]) %>% arrange(OSMP_Code.clean)
clean_names[duplicated(clean_names$OSMP_Code.clean),] # nothing duplicated anymore, codes and scinames cleaned!
# clean_problems <- cover_dat[cover_dat$OSMP_Code.clean %in% clean_names$OSMP_Code.clean[duplicated(clean_names$OSMP_Code.clean)] |
#                               is.na(cover_dat$OSMPSciName.clean),] # no problems!

# compare original OSMP names vs. Leezburg-corrected names
length(unique(cover_dat$OSMP_Code)) #419
length(unique(cover_dat$OSMP_Code.clean)) #397 (good, should be less)
# does number of unique OSCPSciName.clean? match unique (OSMP_Code.clean?)
length(unique(cover_dat$OSMP_Code.clean)) == length(unique(cover_dat$OSMPSciName.clean)) # True!


# finish subsetting cover dataset .. for now
cover_dat <- cover_dat %>%  
  # create variable in cover_data that is just the genus and species (no subspecies or variant names)
  mutate(shortSciName = trimws(gsub(" ssp*.+| var*.+", "", OSMPSciName.clean))) %>% 
  dplyr::select(Project, DataType, Year, Area, transect_ID, shortSciName, OSMP_Code, OSMPSciName, OSMP_Code.clean, OSMPSciName.clean, Cov_freq_val, Frst_hit, Nativity:PSNPathway)



# --- TGSNA DESCRIPTIVE CLEAN UP -----
# clean up and standardize species descriptive info
# extract species descriptive data (will merge back in later)
tgsna_descrip <- dplyr::select(cover_dat, shortSciName, OSMP_Code.clean, OSMPSciName.clean, Nativity:PSNPathway) %>% # A. Leezburg says can drop Priority Weed bc needs internal review/cleaning by OSMP first, is irrelevant data
   distinct()

descrip_dups <- tgsna_descrip$OSMP_Code.clean[duplicated(tgsna_descrip$OSMP_Code.clean)] %>% sort()
descrip_problems <- tgsna_descrip[tgsna_descrip$OSMP_Code.clean %in% descrip_dups,] %>% 
  left_join(unique(dplyr::select(cover_dat, shortSciName:OSMPSciName.clean, Nativity:PSNPathway))) %>%
  dplyr::select(OSMP_Code, OSMPSciName, shortSciName:PSNPathway) %>%
  arrange(OSMP_Code.clean, OSMPSciName.clean, Nativity) %>%
  subset(!(duplicated(OSMP_Code.clean) & is.na(Nativity))) %>% # remove rows where code duplicated and descriptive info is NA
  #subset(!(grepl("spp.",OSMPSciName.clean) & OSMPSciName != OSMPSciName.clean)) # if is a general spp, select the matching row where OSMP Code is also general spp 
  subset(!(OSMPSciName != OSMPSciName.clean)) # defer to descriptive record where original OSMPSciName matches the cleaned name (e.g. anything changed to genus spp will match the original genus spp record, any sp that was corrected due to questionable ID matches the descriptive record for species it was switched to)

# manual edits
descrip_problems <- with(descrip_problems, descrip_problems[!(grepl("Cheno.* spp", OSMPSciName.clean) & Nativity == "Native"),])

# pair unproblematic tgnsa_descrip with cleaned up descrip_problems
tgsna_descrip <- subset(tgsna_descrip, !OSMP_Code.clean %in% descrip_problems$OSMP_Code.clean) %>%
  rbind(dplyr::select(descrip_problems, -c(OSMP_Code, OSMPSciName))) %>%
  arrange(OSMP_Code.clean, OSMPSciName.clean)

# manual edits
# Agropyron sp. (Esco coded as "AGROPYRON X REPENS HYBRID") has all NAs for descriptive info
tgsna_descrip[grepl("Agrop.* sp", tgsna_descrip$OSMP_Code.clean ), 
              c("Nativity", "Lifeform", "LifeHistory", "CValue", "OSMPRareSensitive","PSNPathway")] <- c("Unknown", "Graminoid", "Perennial", NA, FALSE, "C3")
# capitalize "unknown" to match styling of Nativity values
tgsna_descrip$Nativity <- gsub("unk", "Unk", tgsna_descrip$Nativity)

# which species still have missing descriptive info?
missing_descrip <- mapply(is.na, dplyr::select(tgsna_descrip,Nativity:LifeHistory, PSNPathway))
missing_descrip <- apply(missing_descrip, 1, function(x) sum(x)>0) #if any col has TRUE (is.na), sum of row will be >0
missing_recs <- tgsna_descrip[missing_descrip,] 
missing_recs <- subset(missing_recs,!Lifeform %in% "Ground cover")

# manual edits..
forbs <- c("Anten|Clayt|Cymo|Gaill|Lyco|Oeno")
perenn <- c("Anten|Cymo|Lyco")
native <- c("Anten|Gaill|Lyco|Cymo|Oen") #there is a non-native Lycopus europaeus USDA plants, but only documents in eastern US + BC (guessing needs more temperate climate)
missing_recs$Lifeform[grepl(forbs, missing_recs$shortSciName)] <- "Forb"
missing_recs$Lifeform[grepl("Erag", missing_recs$shortSciName)] <- "Graminoid"
missing_recs$LifeHistory[grepl("Erag", missing_recs$shortSciName)] <- "Unknown"
missing_recs$Nativity[grepl("Erag", missing_recs$shortSciName)] <- "Unknown"
missing_recs$OSMPRareSensitive[grepl("Anten", missing_recs$shortSciName)] <- FALSE
missing_recs[missing_recs$OSMP_Code.clean == "phyhedc", 
             c("Nativity", "Lifeform", "LifeHistory")] <- c("Native", "Forb/Subshrub", "Perennial")
missing_recs$LifeHistory[grepl(perenn, missing_recs$shortSciName)] <- "Perennial"
missing_recs$LifeHistory[is.na(missing_recs$LifeHistory) & missing_recs$Lifeform == "Forb"] <- "Annual/Perennial"
missing_recs$LifeHistory[is.na(missing_recs$LifeHistory) & grepl("Forb|Gram", missing_recs$Lifeform)==FALSE] <- "Unknown"
missing_recs$Nativity[is.na(missing_recs$Nativity) & grepl(native, missing_recs$shortSciName)] <- "Native"
missing_recs$PSNPathway[is.na(missing_recs$PSNPathway)] <- "Unknown"
# finish cleaning BOS dataset after make spp lookup table

# pair complete tgsna_descrip with cleaned up missing_recs
tgsna_descrip <- subset(tgsna_descrip, !OSMP_Code.clean %in% missing_recs$OSMP_Code.clean) %>%
  rbind(missing_recs) %>%
  arrange(OSMP_Code.clean, OSMPSciName.clean)

# be sure codes in cover dat and descriptive dat match up
summary(sort(unique(cover_dat$OSMP_Code.clean)) == sort(unique(tgsna_descrip$OSMP_Code.clean))) #yes!

# PSNPathway check (only things that will have data are likely graminoids)
with(tgsna_descrip, lapply(split(PSNPathway, Lifeform), unique)) # as expected, only grams.. Chamaesyce spp. is only Forb with data (C4.. looks reasonable in Google search)
#QA grams
grams <- subset(tgsna_descrip, grepl("Gram", Lifeform))
grams$genus <- gsub(" .*", "", grams$shortSciName)
problem_grams <- unique(grams[c("genus", "PSNPathway")]) %>% subset(genus %in% duplicated(genus) | PSNPathway == "Unknown")
# > NOTE: in quick google search, all wetland spp + Poa can be cool or warm season, so will leave as unknown.
# manual edit: Recode Agrostis PSNPathway to match other Agrostic spp in dataset (that is the only gram to fix)
tgsna_descrip$PSNPathway[grepl("Agrost",tgsna_descrip$shortSciName) & tgsna_descrip$PSNPathway == "Unknown"] <- unique(tgsna_descrip$PSNPathway[grepl("Agrost",tgsna_descrip$shortSciName) & tgsna_descrip$PSNPathway != "Unknown"]) 

#tgsna descrip clean!


# -- SUSBET AND CLEAN UP SPP DESCRIP -----
spp_descrip <- spp_descrip %>%
  # correct Species and Genus in spp descrip (are reversed)
  rename(JL_Code = 'Species code', # rename to remove space, indicate it's code used in larson dataset and distinguish from 'Species' field name
         Species = Genus,
         Genus = Species) %>%
  # correct spelling of various species.. 
  mutate(Species = gsub("pennsylvanica", "pensylvanica", Species), # c. pensylvanica (JL has two n's)
         Species = gsub("macanthra", "macrantha", Species), #Koeleria macrantha
         Genus = (gsub("Erysisimum", "Erysimum", Genus)), #JL has extra "si"
         Lifeform = paste0(toupper(substr(Lifeform, 1,1)),substr(Lifeform, 2, nchar(Lifeform))),
         Origin = paste0(toupper(substr(Origin, 1,1)),substr(Origin, 2, nchar(Origin)))) %>% # so casing consistent with other lifeform values
  # subset trait spp descriptive dataset
  dplyr::select(JL_Code:Origin) %>%
  # create full latin name
  unite(JLSciName, Genus, Species, sep = " ", remove = FALSE)

# QA check: is number of distinct codes in mature trait data same as in spp_descrip?
length(unique(trait_dat$species)) == length(unique(spp_descrip$JL_Code)) #falso
# are at least spp codes in mature trait data IN the trait spp list? (if yes, proceed)
if(sum(unique(trait_dat$species) %in% unique(spp_descrip$JL_Code)) != length(unique(trait_dat$species))){
  print("Error! spp mismatch between trait data and trait spp list. Go investigate!")
} else{ print("Bueno! Mature trait spp & trait spp list match! Proceed to lookup table.")}

#investigate..
# which mature trait dataset codes not in trait spp list?
tr_dat_missing <- unique(trait_dat$species[!unique(trait_dat$species) %in% unique(spp_descrip$JL_Code)]) 
tr_dat_missing #"brotec" "ermfen" "lesmon"

# which trait spp list codes not in mature trait dataset?
tr_list_missing <- unique(spp_descrip$JL_Code[!unique(spp_descrip$JL_Code) %in% unique(trait_dat$species)]) 
tr_list_missing #"ambpsi" "anitec" "carpen" "erefen" "muhmon"

# ctw looked at veg dat, assume ermfen is type and should be erefen (esp since in trait spp list and is missing from trait dat)
tr_dat_missing <- tr_dat_missing[!tr_dat_missing == "ermfen"]
# brotec = anitec, lesmon is in boulder open space veg data (but not in spp list)



# -- CREATE SPP LOOKUP TABLE -----
# join cover data OSMP_code and OSCPSciName columns
spp_lookup <- left_join(spp_descrip, unique(cover_dat[, c("shortSciName", "OSMP_Code.clean", "OSMPSciName.clean")]), by = c("JLSciName" = "shortSciName")) %>%
  dplyr::select(JL_Code:Species, OSMP_Code.clean, OSMPSciName.clean, Lifeform:Origin) %>% #rearrange columns
  rename(LifeHistory = 'Life history',
         Nativity = Origin) #match OSMP column name

# manual corrections
## elytrigia repens (OSMP) == elymus repens (trait data)
spp_lookup[which(spp_lookup$JL_Code == "elyrep"),c("OSMP_Code.clean", "OSMPSciName.clean")] <- unique(cover_dat[which(cover_dat$OSMP_Code.clean=="elyrep"), c("OSMP_Code.clean", "OSMPSciName.clean")])
## helianthus rigidus (OSMP) == helianthus rigida (trait data)
spp_lookup[which(spp_lookup$JL_Code == "helrig"),c("OSMP_Code.clean", "OSMPSciName.clean")] <- unique(cover_dat[which(grepl("Helianthus rig",cover_dat$OSMPSciName.clean)==TRUE), c("OSMP_Code.clean", "OSMPSciName.clean")])
## alyaly-alydes-alypar (OSMP) == alypar (trait data)
spp_lookup[which(spp_lookup$JL_Code == "alypar"),c("OSMP_Code.clean", "OSMPSciName.clean")] <- unique(cover_dat[which(grepl("alypar",cover_dat$OSMP_Code.clean)==TRUE), c("OSMP_Code.clean", "OSMPSciName.clean")])
# Virgulus sp (OSMP) = virfal (trait data)
spp_lookup[which(spp_lookup$JL_Code == "virfal"),c("OSMP_Code.clean", "OSMPSciName.clean")] <- unique(cover_dat[which(grepl("Virgulus",cover_dat$OSMP_Code.clean)==TRUE), c("OSMP_Code.clean", "OSMPSciName.clean")])


# manually enter trait codes in lookup missing from list in the trait spp list
spp_lookup[(nrow(spp_lookup)+1):(nrow(spp_lookup)+length(tr_dat_missing)), ncol(spp_lookup)] <- NA #create rows
spp_lookup$JL_Code[(nrow(spp_lookup)-length(tr_dat_missing)+1):(nrow(spp_lookup))] <- c(tr_dat_missing)
# fix brotec
spp_lookup[spp_lookup$JL_Code == "brotec", c("JLSciName", "Genus", "Species", "OSMP_Code.clean")] <- c("Bromus tectorum", "Bromus", "tectorum", "anitec")
spp_lookup[spp_lookup$JL_Code == "brotec", c("OSMP_Code.clean", "OSMPSciName.clean", "Lifeform", "LifeHistory", "Nativity")] <- tgsna_descrip[tgsna_descrip$OSMP_Code.clean == "anitec", c("OSMP_Code.clean", "OSMPSciName.clean", "Lifeform", "LifeHistory", "Nativity")]
# fix lesmon
spp_lookup[spp_lookup$JL_Code == "lesmon", c("OSMP_Code.clean")] <- "lesmon"
spp_lookup[spp_lookup$JL_Code == "lesmon", c("OSMP_Code.clean", "OSMPSciName.clean", "Lifeform", "LifeHistory", "Nativity")] <- tgsna_descrip[tgsna_descrip$OSMP_Code.clean == "lesmon", c("OSMP_Code.clean", "OSMPSciName.clean", "Lifeform", "LifeHistory", "Nativity")]
spp_lookup$JLSciName[spp_lookup$JL_Code == "lesmon"] <- spp_lookup$OSMPSciName.clean[spp_lookup$JL_Code == "lesmon"]
spp_lookup[spp_lookup$JL_Code == "lesmon", c("Genus", "Species")] <- strsplit(spp_lookup$OSMPSciName.clean[spp_lookup$JL_Code == "lesmon"], " ") %>% unlist()

# rename JL Lifeform, LifeHistory, Nativity to not confused with OSMP Lifeform 
# > all the same except JL uses "Shrub/Woody" in Lifeform so OSMP cols will not join as should if keep colnames as is
spp_lookup <- rename(spp_lookup, 
                     JL_Lifeform = Lifeform,
                     JL_LifeHistory = LifeHistory,
                     JL_Nativity = Nativity)

# check for any missing values
summary(is.na(spp_lookup)) #expect 1 bc tall oatgrass not in BOS dataset



# -- CLEAN UP + PREP BOS DATASET: PART 2 -----
# only keep clean codes and scinames, take out descriptive cols (can add back in later)
cover_out <- dplyr::select(cover_dat, Project:shortSciName, OSMP_Code.clean:Frst_hit) %>%
  #join descriptive info
  left_join(tgsna_descrip)



# -- FINISHING -----
# write out cleaned cover dat
write_csv(cover_out, paste0(datapath, "tgsna_monitoring_19962016_clean.csv"))
# write out lookup table
write.csv(spp_lookup, paste0(datapath, "tgsna_trait_spp_lookup.csv"))
