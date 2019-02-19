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
rm(list=ls()) # clear environment
options(stringsAsFactors = FALSE) # character strings never read in as factor by default

#load needed libraries
library(tidyverse) # dplyr is in this package, no need to load separately
library(readxl) # for reading in excel workbooks



#set relative pathway to Google Drive --> user will need to adjust this <---
# **uncomment whichever path is yours when running script
#gdrive <- "/Users/emilykelman/Google\ Drive" #emily's path
gdrive <- "../../Google\ Drive" #ctw path
#gdrive <- "" #julie's path

# set path to datasets
# **EK: what is the difference between the function 'paste0' and 'paste'? why did I use paste0 here?
# **can query help on function by typing in console ?[function name](), e.g. ?paste0()
bos_datpath <-paste0(gdrive, "/KelmanProject/Data/raw/tgsna_monitoring_19912016.csv")
# there are two trait datasets in Kelman Project? reading one from data folder with more recent timestamp
trait_datpath <-paste0(gdrive, "/KelmanProject/Data/Trait_species_list_veg_dormancy.xlsx")
bos_dat_clean <- paste0(gdrive, "/KelmanProject/Data/tgsna_monitoring_19962016_clean.csv")
spp_lookup <- paste0(gdrive, "/KelmanProject/Data/tgsna_trait_spp_lookup.csv")

#read in datasets
bos_dat <- read.csv(bos_datpath)
trait_dat <- read_xlsx(trait_datpath)
bos_dat_clean <-read.csv(bos_dat_clean)
spp_lookup <-read.csv(spp_lookup)
spp_lookup <- spp_lookup[2:ncol(spp_lookup)] #get rid of junk csv index columns "X" 



# --- EXPLORE RAW DATASETS -----
# ** this code is looking at the original bosmp data (not cleaned) to see how to screen data, and matching spp codes in BOSMP dataset manually with spp codes in trait dataset
#look at structure of data frames
glimpse(bos_dat) #method using tidyverse function
str(bos_dat) #method using base R function
summary(bos_dat)
glimpse(trait_dat)

# subset datasets to what's needed for analysis
cover_dat <- subset(bos_dat, DataType == "COVER" & Lifeform!="Ground cover")

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
# correct for species in BOS dataset that have different species codes than in trait dataset
correctspeciesname <-c("ambpsic", "carpenh", "helrigs", "tradubm") #OSMP codes that are trait dataset species (but mismatch with trait dataset code names)
cover_dat$trait_sp <- ifelse(cover_dat$OSMP_Code %in% correctspeciesname, "yes", cover_dat$trait_sp)
#make correction for juncus arcticus codes being different in trait dataset vs. OSMP dataset
cover_dat$trait_sp <- ifelse(cover_dat$OSMP_Code == "junarca", "yes", cover_dat$trait_sp)
# correct scientific name for junarc (is NA in OSMP dataset)
cover_dat$OSMPSciName <-  ifelse(cover_dat$OSMP_Code == "junarc", 
                                               # if yes, match OSMP scientific name for code junarca
                                               unique(cover_dat$OSMPSciName[cover_dat$OSMP_Code == "junarca"]), 
                                               # if not, leave as is
                                               cover_dat$OSMPSciName)


# **Non-veg cover removed above; rel cover created below; commenting out this code
# #trying to create subset of vegetation cover to remove rock, standing dead, and litter 
# remove <- c("rock", "litter", "baregd", "standing dead")
# 
# vegcoverdat <- cover_dat %>%
#   dplyr::filter(!OSMP_Code %in%remove) #can specify you want the filter function from dplyr by using "dplyr::"
# 
# #check to see no standing dead, rock, litter, or baregd in remaining OSMP_Code vegcoverdat column 
# unique(vegcoverdat$OSMP_Code)
# 
# #create a new column to quantify relative cover now that we've removed rock litter etc.
#vegcoverdat <- vegcoverdat %>%
#  group_by(Year, Area, Transect) %>%
# mutate(RelCov = Cov_freq_val/sum(Cov_freq_val))


# -- PREPARE CLEAN BOS DATA FOR TRAIT VS NON-TRAIT COVER FIGURES ----
# ** from here on, uses clean BOSMP dataset
cover_dat_clean <- subset(bos_dat_clean, DataType == "COVER")

# summarize cover dataset by what's in trait dataset vs what's not in trait dataset
# each area-transect combo, per year, should have 2 rows: total cover for trait species and total cover for not-trait-species
# start you code here and assign issue when you get stuck...
grpd_cover <- cover_dat_clean %>% 
  subset(Lifeform != "Ground cover") %>%  # <-- **remove non-veg cover in one line of code. see Lifeform variable in bos_dat ** 
  #mutate(transect_ID = paste(Area, transect_ID,sep = "_")) %>% # ctw already made transect_ID for bos_dat_clean, reads in with it 
  mutate(trait_sp = ifelse(OSMP_Code.clean %in% spp_lookup$OSMP_Code.clean,"yes", "no")) %>% # add column to indicate whether in trait dataset or not
  group_by(Year, transect_ID, trait_sp) %>%
  summarize(summed_cover = sum(Cov_freq_val)) %>%
  ungroup() %>%
  group_by(Year, transect_ID) %>%
  mutate(total_cover = sum(summed_cover),
         rel_cover = summed_cover/total_cover) 

  

# ------ EXPLORATORY ANALYSIS -----
# make visual summary using ggplot
## **EK: try on your own once you have the summarize cover dataset made
## ** if you get stuck we can try troubleshooting via GitHub and/or work on it together next Tuesday

ggplot(grpd_cover, aes(x=trait_sp, y=summed_cover)) +
  geom_col() +
  facet_grid(transect_ID ~ Year)

ggplot(grpd_cover, aes(x=trait_sp, y=summed_cover)) +
  geom_col() +
  facet_wrap(~Year)

ggplot(grpd_cover, aes(Year, y=summed_cover)) +
  geom_point() +
  facet_wrap(~transect_ID)


# -- CTW ADDED FIGURES -----
# ** EK: look at the code for one plot and try to figure out what each line is doing (use ? helper in console, or google the command)
# we can talk about lines of code you're not sure about. ggplot standard for a "grammar of graphics", so with each line you add on an additional graphical component to the plot
totcov_fig <- ggplot(grpd_cover, aes(Year, summed_cover)) +
  geom_col(aes(col = trait_sp, fill = trait_sp)) +
  labs(y = "Transect total cover (%)",
       title = "Total vegetative cover is captured well by trait species in many transects across time, but not universally") + # plot titles describe not just what's plotted, but also what the fig suggests (succinctly)+
  scale_color_manual(name = NULL, values = c("no" = "orchid2", "yes" = "royalblue3"), guide_legend(NULL)) +
  scale_fill_manual(name = "In trait\ndatabase?", values = c("no" = "mistyrose2", "yes" = "steelblue2")) +
  facet_wrap(~transect_ID) +
  theme_bw() +
  theme(legend.title = element_text(size = 9))

totcov_fig

# if we want to see relative cover, we can group the dataset one step further before plotting
# instead of creating new data frame of newly aggregated dataset, can pipe it to ggplot
relcov_fig <- grpd_cover %>%
  group_by(Year, transect_ID) %>%
  mutate(total_cover = sum(summed_cover)) %>%
  ungroup() %>%
  group_by(Year, transect_ID, trait_sp) %>%
  summarize(relative_cover = summed_cover/total_cover) %>%
  ggplot(aes(Year, relative_cover, group = transect_ID, col = trait_sp)) +
  geom_hline(aes(yintercept = .50), col = "grey40", lwd = 1.5, alpha = .3) +
  geom_point() +
  labs(y = "Transect relative cover (%)",
       title = "Trait species capture over majority* of relative total vegetation cover on many transects over time",
       subtitle = "*grey line demarcates 50 percent relative cover threshold") + # plot titles describe not just what's plotted, but also what the fig suggests (succinctly)+
  scale_color_manual(name = "In trait\ndatabase?", values = c("no" = "orchid2", "yes" = "royalblue3")) +
  #scale_fill_manual(name = "In trait database?", values = c("no" = "mistyrose2", "yes" = "steelblue2")) +
  facet_wrap(~transect_ID) +
  theme_bw() +
  theme(legend.title = element_text(size = 9))

relcov_fig

# in the way we calculated the grouped_cover dataset, we summed all vertical hits on the transect
# multiple hits could be recorded if the vegetation was layered (e.g. grass over forb over another forb; shrub over grass over forb, etc.)
# what if we only consider the first hit species?
# we'll need to start with the cover dataset again to subset only first hit cover (Frst_hit == "Yes")
firstcov_fig <- cover_dat_clean %>%
  subset(Frst_hit == "Yes" & Lifeform != "Ground cover") %>%
  # then aggregate (summarize) as we did about for grouped_cover and pipe to ggpplot
  mutate(trait_sp = ifelse(OSMP_Code.clean %in% spp_lookup$OSMP_Code.clean,"yes", "no")) %>% # add column to indicate whether in trait dataset or not
  group_by(Year, transect_ID, trait_sp) %>%
  summarize(cover_tophit = sum(Cov_freq_val)) %>%
  # left_join(grpd_cover) %>%
  # mutate(delta = summed_1st_cover - summed_cover) # to look at difference of removing non-first-hit cover
  ggplot(aes(Year, cover_tophit)) +
  geom_col(aes(col = trait_sp, fill = trait_sp)) +
  labs(y = "Transect total cover (%)",
       title = "Transect vegetation total cover, by species in and not in trait dataset, first hit only",
       subtitle = "Considering top hit only doesn't seem to improve transect cover captured by trait species") + # plot titles describe not just what's plotted, but also what the fig suggests (succinctly)+
  scale_color_manual(name = NULL, values = c("no" = "orchid2", "yes" = "royalblue3"), guide_legend(NULL)) +
  scale_fill_manual(name = "In trait\ndatabase?", values = c("no" = "mistyrose2", "yes" = "steelblue2")) +
  facet_wrap(~transect_ID) +
  theme_bw() +
  theme(legend.title = element_text(size = 9))

firstcov_fig

# assume we pursue the community wgtd mean analysis only selecting some transects where trait species capture a majority of cover over time
# we can plotaverage cover over time, by transect, to inspect which transects pass our cover criterion (whatever JL [or EK!] thinks is appropriate)
# let's keep this dataset as a separate object bc we might want to make multiple figures from it
temporal_meancover <- grpd_cover %>% # start with grpd_cover since it didn't seem like subsetting to first hit made any difference
  group_by(transect_ID, trait_sp) %>% # we're going to average over time, so take out Year as a grouping variable
  summarize(avg_cover = mean(summed_cover), # mean absolute cover
            avg_relcover = mean(rel_cover), # mean relative cover
            nobs = length(Year), # number of annual observations in the record per transect (i.e. number of times transect sampled)
            std_dev = sd(summed_cover), #std dev of mean absolute cover
            std_error = std_dev/sqrt(nobs), #std error of mean absolute cover
            rel_sd = sd(summed_cover), # std dev of mean relative cover
            rel_se = rel_sd/sqrt(nobs)) # std error of mean relative cover

# temporal mean by site, split by species in trait dataset/not in trait dataset  
meancov_fig <- ggplot(temporal_meancover, aes(avg_cover, transect_ID)) +
  geom_vline(aes(xintercept = mean(avg_cover)), lty = 2) + #will create dotted line at overall mean cover of trait spp and non trait spp mean cover
  geom_errorbarh(aes(xmax = avg_cover + std_error, xmin = avg_cover - std_error, col = trait_sp)) + 
  geom_point(aes(fill = trait_sp), pch=21, size = 2, alpha = 0.7) +
  labs(y = "Transect", x = "Temporal mean cover (%)",
       title = "Temporal mean transect vegetative cover (1991-2016), by species in and not in trait dataset, by site",
       subtitle = "Mean of summed species cover, bars show ±1 standard error, dotted line = grand mean") +
  scale_fill_manual(name = "In trait database?", values = c("no" = "orchid2", "yes" = "royalblue3")) +
  scale_color_discrete(guide = "none") +
  theme_light() +
  theme(legend.title = element_text(size = 8))

meancov_fig

# ** EK: you could add a plot here for mean RELATIVE cover (with error bars) over time

meanrel_fig <- ggplot(temporal_meancover, aes(avg_relcover, transect_ID)) +
  geom_vline(aes(xintercept=mean(avg_relcover)), lty = 2) +
  geom_errorbarh(aes(xmax = avg_relcover + std_error, xmin = avg_relcover - std_error, col = trait_sp)) +
  geom_point(aes(fill(trait_sp), pch=21, size=2, alpha=.7))

 


#meanrel_fig


# what about cover of individual trait dataset species over space and time?
# similary, instead of creating a new dataset, we can just aggregate the cover dataset as we want and then pipe it to ggplot
spcov_fig <- cover_dat_clean %>% # start with this dataset, and continue pipe..
  mutate(trait_sp = ifelse(OSMP_Code.clean %in% spp_lookup$OSMP_Code.clean,"yes", "no")) %>% # add column to indicate whether in trait dataset or not
  subset(trait_sp == "yes") %>%
  group_by(Year, transect_ID, OSMP_Code.clean, OSMPSciName.clean, shortSciName) %>%
  summarise(summed_cover = sum(Cov_freq_val)) %>%
  ungroup() %>%
  group_by(Year, OSMP_Code.clean, OSMPSciName.clean, shortSciName) %>%
  summarise(total_cover = sum(summed_cover),
            nobs = length(transect_ID)) %>%
  #mutate(short_sciname = gsub(" var.+| ssp.+", "", OSMPSciName)) %>% # remove varietal or sub-species names for plotting
  ggplot(aes(Year, total_cover)) +
  geom_point(alpha = 0.5) +
  labs(y = "Total cover (%)",
       title = "BOS xeric tallgrass regional summed cover (all transects), by trait dataset species, by year") +
  facet_wrap(~shortSciName, labeller = label_wrap_gen(width = 20)) +
  theme_light() +
  theme(strip.text.x = element_text(face = "italic"))

spcov_fig


# -- WRITE OUT FIGURES ----
# save figures to GitHub repo: exploratory_analysis/figures folder
# can control output through using terms like width, height, units, and scale
ggsave("./exploratory_analysis/figures/BOS_totalcover.pdf", totcov_fig,
       width = 6, height = 4, units = "in", scale = 1.5)
ggsave("./exploratory_analysis/figures/BOS_firsthit_cover.pdf", firstcov_fig,
       width = 6, height = 4, units = "in", scale = 1.5)
ggsave("./exploratory_analysis/figures/BOS_relativecover.pdf", relcov_fig,
       width = 6, height = 4, units = "in", scale = 1.5)
ggsave("./exploratory_analysis/figures/BOS_mean_abscover.pdf", meancov_fig, 
       width = 6, height = 4, units = "in", scale = 1.5)
#ggsave("./exploratory_analysis/figures/BOS_mean_relcover.pdf", meanrel_fig, ## **EK: if make this figure, save to github/figures folder
#       width = 6, height = 4, units = "in", scale = 1.5)
ggsave("./exploratory_analysis/figures/BOS_trait_indsp_cov.pdf", spcov_fig, scale = 1.35)


#write.csv(grpd_cover,paste0(gdrive, "/KelmanProject/Data/groupedcover.csv"))