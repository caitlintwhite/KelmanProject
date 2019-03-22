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
libs <- c("tidyverse", "FD", "vegan", "graphics") # FD for CWM; vegan for transforming spp abundance matrix and multivar ordination
for(l in libs){
  if(!require(l, character.only = T)){ # this checks whether package is installed or not; if not, install package
    install.packages(l, dep = T)
  }
  library(l,character.only = T) # load library
}


#set relative pathway to Google Drive --> user will need to adjust this <---
# **uncomment whichever path is yours when running script
gdrive <- "/Users/emilykelman/Google\ Drive" #emily's path
#gdrive <- "/Users/serahsierra/Google\ Drive" #ctw path
#gdrive <- "" #julie's path

#read in datasets
bos_dat <- read.csv(paste0(gdrive, "/KelmanProject/Data/tgsna_monitoring_19912016_clean.csv"))
trait_dat <- read.csv(paste0(gdrive, "/KelmanProject/Data/traits_mature_w_seedmass.csv"))
spp_lookup <- read.csv(paste0(gdrive, "/KelmanProject/Data/tgsna_trait_spp_lookup.csv"))
clim_dat <- read.csv(paste0(gdrive, "/KelmanProject/Data/boulder_climate.csv"))

plot(trait_dat$SRL, trait_dat$Rdiam)

# did all datasets read in as expected?
## bos xeric tallgrass cover data
names(bos_dat)
str(bos_dat)
summary(is.na(bos_dat)) # looks goods
unique(bos_dat$OSMP_Code.clean[is.na(bos_dat$LifeHistory)]) # okay

## mature trait data
names(trait_dat)
str(trait_dat) 
# what is diff between species and species.1? are they the same
summary(trait_dat$species == trait_dat$species.1) #yes, remove species.1 to avoid confusion
trait_dat<- trait_dat[,!names(trait_dat) %in% "species.1"]
names(trait_dat)[names(trait_dat)=="species"] <- "JL_Code" #rename "species" code to "JL_code" to match spp lookup table

## bos-trait spp lookup table
names(spp_lookup)
str(spp_lookup)
summary(is.na(spp_lookup)) # expect 1 NA in OSMP cols (no tall oatgrass in bos dat), looks good


## climate dataset
names(clim_dat)
str(clim_dat)
summary(clim_dat)



# -- DATA EXPLORE -----
#Trait steps
trait_species <- unique(trait_dat$JL_Code)
bos_species <- unique(bos_dat$OSMP_Code.clean)
table(trait_species %in% spp_lookup$JL_Code) #of trait spp, how many overlap with bos species? (use lookup table to answer this)
summary(bos_species %in% spp_lookup$OSMP_Code.clean) #of bos pp, how many overlap with trait species?

# which trait species are not in bos dataset?
sort(bos_species[which(!bos_species %in% spp_lookup$OSMP_Code.clean)])



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
transects <- c("7_1", "7_10", "7_2", "7_3", "7_4", "7_5", "7_6", "7_7", "7_9") # transect_ID.clean value; if want all use: unique(bos_dat$transect_ID.clean)
yrs <- c(1991:2016) # yrs in dataset range 1991-2016

# specify whether want to do individual transect CWM (pool_CWM = FALSE) or "regional" pooled community CWM (pool_CWM = TRUE)
# if pooled, will sum spp abundances across all transects, by year, before creating abundance matrix
pool_CWM <- TRUE

# specify vector of desired traits
traits <- colnames(trait_dat)[c(7,10,12:14,17,18,35)] # this will be only line to modify, everything below here generic
traits # visually check if traits you expected

# create vector of spp codes that exist in both veg data and trait data for selected sites, yrs, and traits
abundance_spp <- with(bos_dat, unique(OSMP_Code.clean[transect_ID.clean %in% transects & Year %in% yrs]))
abundance_spp <- abundance_spp[abundance_spp %in% spp_lookup$OSMP_Code.clean] #only keep spp that are in bos-trait lookup table

trait_subset <- trait_dat[colnames(trait_dat) %in% c("JL_Code", traits)]
trait_subset$NAcheck <- apply(trait_subset[2:ncol(trait_subset)], 1, function(x) sum(is.na(x))) #this calculates how many NAs there are across traits selected per species
trait_spp <- trait_subset$JL_Code[!trait_subset$NAcheck > 0] # <-- define here how many NAs are okay to have (or can use other criteria [e.g. it's okay for certain fxnl traits to be NA but not others])

# these are the species that overlap for the traits, sites, and years selected
overlap <- subset(spp_lookup, JL_Code %in% trait_spp & OSMP_Code.clean %in% abundance_spp)
# use overlap to filter abundance matrix and fxnl trait data frame for the CMW below


# -- (1) Create species abundance matrix -----
#Formatting community data for community weighted means
# first aggregate cover data, depending on if poolin spp abundances across transects or not
if(pool_CWM){
  # aggregate cover by spp by year across all transects
  cov_dat <- subset(bos_dat, transect_ID.clean %in% transects & Year %in% yrs & Lifeform != "Ground cover") %>%
    group_by(Year, OSMP_Code.clean) %>%
    summarize(cover = sum(Cov_freq_val)) %>% # sum all hits per species per transect per year
    ungroup() %>% # remove grouping
    mutate(sitekey = Year) # sitekey for abundance matrix rownames will be year only
}else{
  # aggregate cover by spp by year *by transect*
  cov_dat <- subset(bos_dat, transect_ID.clean %in% transects & Year %in% yrs & Lifeform != "Ground cover") %>%
    group_by(transect_ID.clean, Year, OSMP_Code.clean) %>%
    summarize(cover = sum(Cov_freq_val)) %>% # sum all hits per species per transect per year
    ungroup() %>% # remove grouping
    mutate(sitekey = paste(transect_ID.clean, Year, sep = ".")) # concatenate transect and year for unique site ID for abundance matrix rownames
}

#1 subset cover data for desired sites, yrs
abundance <-  cov_dat %>% # start with aggregate cover that has sitekey created
  left_join(overlap[c("JL_Code", "OSMP_Code.clean")]) %>% # add lookup table columns for subsetting to spp in trait dataset
  subset(!is.na(JL_Code)) %>% # only keep spp in trait dataset
  dplyr::select(-OSMP_Code.clean) %>% # remove OSMP_Code and use JL_code for spp names
  spread(JL_Code, cover, fill = 0) %>% # spread out species cover, fill any empty cells (i.e. species not present) with "0"
  as.data.frame()

# pause to create site-environmental matrix in case want to do exploratory ordination later 
#uneccessary for now. for use in ordination 
#site_matrix <- abundance[c("transect_ID.clean", "Year", "sitekey")]

# continue making spp abundance matrix
rownames(abundance) <- abundance$sitekey #assign unique transect-year as rowname
abundance <- abundance[!colnames(abundance) %in% c("sitekey", "Year", "transect_ID.clean")] %>% # remove Year, site, and transect_ID.clean (if there)
  as.matrix() #reclass object as matrix

# convert to relative abundance
rel_abundance <- decostand(abundance, 'total')

view(rel_abundance$poaaga)
poaaga <- data.frame(rel_abundance)
poaaga <- data.frame(poaaga$poaaga)

poaaga_spei <- cbind(poaaga,lag(poolCWM_climate_merged_W$spei_12),poolCWM_climate_merged_W$spei_12)

plot(poaaga_spei$`lag(poolCWM_climate_merged_W$spei_12)`, poaaga_spei$poaaga.poaaga)
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

# back out transect_ID.clean and year
bos_cwm$sitekey <- rownames(bos_cwm) #store unique site-year key in its own column; sitekey will always be the same as Year if doing a pooled community CWM
rownames(bos_cwm) <- seq(1:nrow(bos_cwm)) #clean up rownames
bos_cwm$Year <- as.numeric(gsub("[0-9].*[0-9][.]", "", bos_cwm$sitekey)) #Year will always be in CWM; everything before period in sitekey is removed, convert year from character to number

# extract transect_ID.clean from sitekey only if pool_CWM is FALSE (i.e. it's an transect-level CWM)
if(pool_CWM==FALSE){
  bos_cwm$transect_ID.clean <- gsub("[.].*$", "", bos_cwm$sitekey) #everything after period in sitekey is removed
  # re-order columns (sitekey, Year and transect ID before trait CWMs)
  bos_cwm <- dplyr::select(bos_cwm, sitekey,Year, transect_ID.clean, traits)
  #assign bos_cwm to object with "transect" in it so know what you're plotting and writing out below
  transect_CWM <- bos_cwm
} else{
  # re-order columns (no transect_ID to select)
  bos_cwm <- dplyr::select(bos_cwm, sitekey:Year, traits)
  #assign bos_cwm to an object name with "pool" in it so know what you're plotting and writing out below
  pooled_CWM <- bos_cwm
}

# -- WRITE OUT DATA FRAMES FOR MODELING USE -----
# write out abundance and bos_cwm data frames for use in modeling
# ** uncomment which code line you want depending on whether you're running pooled or tranect-level CWM
if(pool_CWM){
  # pooled abundance matrix
  write.csv(rel_abundance, paste0(gdrive,"/KelmanProject/Data/bos_pooled_relabundance.csv"), row.names = T)
  # pooled CWMs, annually
  write.csv(pooled_CWM, paste0(gdrive,"/KelmanProject/Data/bos_pooled_CWM.csv"), row.names = F)
}else{
  # write tranect-yr abundance matrix
  write.csv(rel_abundance, paste0(gdrive,"/KelmanProject/Data/bos_transect_relabundance.csv"), row.names = T)
  # transect level CWM, annually
  write.csv(transect_CWM, paste0(gdrive,"/KelmanProject/Data/bos_transect_CWM.csv"), row.names = F)
  
}
# write out functional trait data frame (stays the same whether pooled or transect-level)
write.csv(fxnl_df, paste0(gdrive,"/KelmanProject/Data/fxnl_trait_community_df.csv"), row.names = T)


##########################################
# -- EXPLORE CWM WITH OTHER DATASETS -----
##########################################

# specify climate variables desired
climvars <- c("precip_5", "spei_5", "avgtmax5", "avgtmin5", "tmean_5", "spei_12", "tmean_12", "precip_12", "gs_halfprecip_doy", "june1_totalppt", "gs_days_dry", "gs_days_rain")

# can visualize relationship between bos CWM and environment.. CWM to total annual cover.. whatever you choose..
# name data frames differently depending on whether pooled or transect-level
if(pool_CWM){
  #pooled CWM + climate wide form for creating linear regression
  poolCWM_climate_merged_W <- left_join(pooled_CWM, clim_dat[c("year", climvars)],  by=c("Year"="year")) 
  #write csv
  write.csv(poolCWM_climate_merged_W, paste0(gdrive,"/KelmanProject/Data/pooledCWM_for_regressions.csv"), row.names = F)
  
  #pooled CWM + climate long form for creating figures 
  poolCWM_climate_merged_L <- left_join(pooled_CWM, clim_dat[c("year", climvars)],  by=c("Year"="year"))%>%
    gather( key = "trait_name", value, traits, climvars)
  #write csv
  write.csv(poolCWM_climate_merged_L, paste0(gdrive,"/KelmanProject/Data/pooledCWM_for_figures.csv"), row.names = F)  

}else{
  #transect CWM + climate wide form for creating linear regression
  tranCWM_climate_merged_W <- left_join(transect_CWM, clim_dat[c("year", climvars)],  by=c("Year"="year")) 
  #write csv
  write.csv(tranCWM_climate_merged_W, paste0(gdrive,"/KelmanProject/Data/transectCWM_for_regressions.csv"), row.names = F)
  
  #transect CWM + climate long form for creating figures 
  tranCWM_climate_merged_L <- left_join(transect_CWM, clim_dat[c("year", climvars)],  by=c("Year"="year"))%>%
    gather( key = "trait_name", value, traits, climvars)
  #write csv
  write.csv(tranCWM_climate_merged_L, paste0(gdrive,"/KelmanProject/Data/transectCWM_for_figures.csv"), row.names = F)
}

#=====TRANSECT LEVEL CWM FIGURES AND LM=====

#create figure to look at LDMC and precip 
area7_LDMC_precip_fig <- ggplot(tranCWM_climate_merged_L, mapping = aes(x=precip_5, y=LDMC))+
  geom_point(aes(col=transect_ID.clean))+
  geom_smooth(method = "lm", col ="black") +
  geom_smooth(aes(col=transect_ID.clean), method = "lm", se = F)

area7_LDMC_precip_fig

#running linear regression on LDMC and precip_5
LDMC_precip_LM <- lm(formula = LDMC ~ precip_5, data = tranCWM_climate_merged_W)
summary(LDMC_precip_LM)

#create figure to look at mean temp over growing season and SLA
SLA_tmean5_fig <- ggplot(tranCWM_climate_merged_W, mapping = aes(x=tmean_5, y=SLA))+
  geom_point(aes(col=transect_ID.clean))+
  geom_smooth(method = "lm", col="black")+
  geom_smooth(aes(col=transect_ID.clean), method = "lm", se = F)

SLA_tmean5_fig

#run linear regression for SLA and tmean_5. pvalue .18 and r^2 value .003
SLA_tmean5_LM <- lm(formula = SLA ~ tmean_5, data = tranCWM_climate_merged_W)
summary(SLA_tmean5_LM)

#plotting tmin_5 and RDMC
RDMC_tmin5_fig <-ggplot(tranCWM_climate_merged_W, mapping = aes(x=avgtmin5, y=RDMC))+
  geom_point(aes(col=transect_ID.clean))+
  geom_smooth(method = "lm", col="black")+
  geom_smooth(aes(col=transect_ID.clean), method = "lm", se = F)

RDMC_tmin5_fig

#linear regression for tmin_5 and RDMC. pvalue of .72 and r^2 value -.004 
RDMC_tmin5_LM <- lm(formula = RDMC ~ avgtmin5, data = tranCWM_climate_merged_W) 
summary(RDMC_tmin5_LM) # ctw, 3/2/19: using growing season average tmin has slightly better relationship with RDMC

#plot root mass ratio and spei_12. p value .5 and r^2 value of -.002
RMR_spei12_fig <-ggplot(tranCWM_climate_merged_W, mapping = aes(x=spei_12, y=RMR))+
  geom_point(aes(col=transect_ID.clean))+
  geom_smooth(method = "lm", col="black")+
  geom_smooth(aes(col=transect_ID.clean), method = "lm", se = F)

RMR_spei12_fig

#linear regression for RMR and spei_12
RMR_spei12_LM<- lm(formula = RMR ~ spei_12, data = tranCWM_climate_merged_W)
summary(RMR_spei12_LM)

#plotting precipitation over time. commenting it out because it's included in the panel figure
#precip_overT_fig <-ggplot(clim_dat, mapping = aes(x=year, y=precip_5)) +
#geom_line()+
#  geom_point()

#precip_overT_fig

#plot final plant height and spei_12
finalheight_spei12_fig <- ggplot(tranCWM_climate_merged_W, mapping = aes(x=spei_12, y=final_height_cm))+
  geom_point(aes(col=transect_ID.clean))+
  geom_smooth(method = "lm", col="black")+
  geom_smooth(aes(col=transect_ID.clean), method = "lm", se = F)

finalheight_spei12_fig

#linear regression for final plant height and spei_12. pvalue .03 and r^2 value .01
finalheight_spei12_LM <- lm(formula = final_height_cm ~ spei_12, data = tranCWM_climate_merged_W)
summary(finalheight_spei12_LM)

#plotting drought index over time. Also commenting out because it's included in the panel plot 
#drought_overT_fig <- ggplot(clim_dat, mapping = aes(x=year, y=spei_12))+
#geom_col()

#drought_overT_fig


#plot CWM for SLA and precip over time
SLA_RMR_env_fig <- ggplot(subset(tranCWM_climate_merged_L, trait_name%in% c("SLA", "gs_halfprecip_doy", "spei_5", "RMR")), mapping = aes(Year, value))+
  # add line only to panels that have single value per year
  geom_line(data = subset(tranCWM_climate_merged_L, trait_name%in% c("gs_halfprecip_doy", "spei_5")), aes(Year, value)) +
  # plot points for all panels
  geom_point()+
  # add mean trend line for plan trait panels
  stat_summary(data = subset(tranCWM_climate_merged_L, trait_name%in% c("RMR", "SLA")),
               aes(y = value), fun.y=mean, colour="red", lwd=1, geom="line") +
  # adjust year scaling (add major gridlines at more years)
  scale_x_continuous(breaks = seq(1990, 2016, 4)) +
  facet_grid(trait_name~., scales = "free_y")+
  theme_bw()
SLA_RMR_env_fig 


#======AREA 7 POOLED CWM FIGURES AND LM=====
#graph pooled cwm final height and spei_12
pooledCWM_height_spei12_fig <-ggplot(poolCWM_climate_merged_W, mapping = aes(x=spei_12, y=final_height_cm))+
  geom_point()+
  geom_smooth(method = "lm", se = F)

pooledCWM_height_spei12_fig

#linear regression. p value of .06 and r^2 .10
pooledLM_height_spei12<- lm(formula = final_height_cm ~ spei_12, data = poolCWM_climate_merged_W)
summary(pooledLM_height_spei12)

#graph pooled cwm LDMC and precip_12
pooledCWM_precip12_LDMC_fig <- ggplot(poolCWM_climate_merged_W, mapping = aes(x=precip_12, y=LDMC))+
  geom_point()+
  geom_smooth(method = "lm", se = F)

pooledCWM_precip12_LDMC_fig

#linear regression pooledCWM LDMC and precip_12
#p value .18 and r^2 .03
#idea to make panel plot of RDMC and LDMC? 
pooledLM_precip12_LDMC <- lm(formula = LDMC ~ precip_12, data = poolCWM_climate_merged_W)
summary(pooledLM_precip12_LDMC)


pool_tmax5_SLA_fig <- ggplot(poolCWM_climate_merged_W, mapping = aes(x=avgtmax5, y=SLA))+
  geom_point()+
  geom_smooth(method = "lm")

pool_tmax5_SLA_fig

#pooled linear model avg tmax and SLA
#p value .2 and r^2 .02 (for absolute tmax)
#p value .3 and r^2 0.005 for avg growing season tmax
pooledLM_tmax5_SLA <- lm(formula = SLA ~ avgtmax5, data = poolCWM_climate_merged_W)
summary(pooledLM_tmax5_SLA)

poolRMR_spei12_fig <- ggplot(poolCWM_climate_merged_W, mapping = aes(x=spei_12, y=RMR))+
  geom_point()+
  geom_smooth(method = "lm")

poolRMR_spei12_fig

#linear model pooled RMR and spei12
#p value .06 and adjusted r^2 .10
pooledLM_spei12_RMR <-lm(formula = RMR ~ spei_12, data = poolCWM_climate_merged_W)
summary(pooledLM_height_spei12)

#panel plot of spei and traits over time 
spei5_traits_overT_panel <-ggplot(subset(poolCWM_climate_merged_L, trait_name%in% c("spei_5", "RMR", "SLA", "RDMC", "seed_mass", "LDMC")), mapping = aes(Year, value))+
  geom_line()+
  geom_point(size = 0.5)+
  geom_point(data = subset(poolCWM_climate_merged_L, trait_name %in% "spei_5"), aes(Year, value, col = value >= 0)) +
  scale_x_continuous(breaks = seq(1992, 2016, by = 2))+
  scale_color_discrete(guide = "none") +
  theme(axis.text.x = element_text(angle=45, hjust = 1))+
  facet_grid(trait_name~., scales = "free_y")

spei5_traits_overT_panel


#plot changes in environmental variables over time (tmean_12, precip_12, spei_12)
climlabels <- c(a = "Annual mean temperature (C)" ,
               b = "Annual total precipitation (mm)",
               c = "SPEI")

envplot_df <- subset(poolCWM_climate_merged_L, trait_name%in% c("precip_12", "tmean_12", "spei_12")) %>%
  mutate(traitorder = ifelse(trait_name == "tmean_12", "a",
                             ifelse(trait_name == "precip_12", "b", "c")))
#check lm signif
templm <- lm(value ~ Year, data = subset(envplot_df, trait_name == "tmean_12"))
summary(templm)
plot(templm)
summary(lm(value ~ Year, data = subset(envplot_df, trait_name == "precip_12")))

#-------- boulder environment over time panel plot------- 
Enviro_variables_overT_panel <- ggplot(envplot_df, mapping = aes(Year, value))+
  geom_hline(data=subset(envplot_df, trait_name == "spei_12"), aes(yintercept =0), col="grey50", linetype=2) +
  geom_line()+
  geom_point(size=.5)+
  geom_point(data=subset(envplot_df, trait_name == "spei_12"), aes(Year, value, col = value), size=.5)+
  geom_smooth(data=subset(envplot_df, trait_name == "tmean_12"), aes(Year, value), method=lm)+
  scale_x_continuous(breaks = seq(1992, 2016, by = 2))+
  scale_fill_distiller(name="Annual\nSPEI", palette = "RdYlBu", direction = 1, guide = FALSE)+
  theme(axis.text.x = element_text(angle=45, hjust = 1))+
  facet_grid(traitorder~., scales = "free_y", labeller = as_labeller(climlabels))


Enviro_variables_overT_panel

#creating correlation matrix for CWM traits
traits_correlation <- cor(bos_cwm[traits])
#scatterplot of CWM root traits at community level

#points are years
pairs(bos_cwm[, c(5:7)])

#plot species level trait scatterplot 
 pairs(trait_dat[,c(12:14)])




#save figures to github
ggsave("./exploratory_analysis/figures/area7_LDMC_fig.pdf", area7_LDMC_precip_fig)
ggsave("./exploratory_analysis/figures/SLA_tmean5_fig.pdf", SLA_tmean5_fig)
ggsave("./exploratory_analysis/figures/RDMC_tmin5_fig.pdf", RDMC_tmin5_fig)
ggsave("./exploratory_analysis/figures/RMR_spei12_fig.pdf", RMR_spei12_fig )
ggsave("./exploratory_analysis/figures/SLA_RMR_env_panel.pdf", SLA_RMR_env_fig)
ggsave("./exploratory_analysis/figures/finalheight_spei12_fig.pdf", finalheight_spei12_fig)
ggsave("./exploratory_analysis/figures/Enviro_variables_overT_panel.pdf", Enviro_variables_overT_panel)
ggsave("./exploratory_analysis/figures/spei5_traits_overT_panel.pdf", spei5_traits_overT_panel)
ggsave("./exploratory_analysis/figures/pooledCWM_height_spei12_fig.pdf", pooledCWM_height_spei12_fig)
ggsave("./exploratory_analysis/figures/pooledCWM_precip12_LDMC_fig.pdf", pooledCWM_precip12_LDMC_fig)
ggsave("./exploratory_analysis/figures/pool_tmax5_SLA_fig.pdf", pool_tmax5_SLA_fig)
ggsave("./exploratory_analysis/figures/poolRMR_spei12_fig.pdf", poolRMR_spei12_fig)
