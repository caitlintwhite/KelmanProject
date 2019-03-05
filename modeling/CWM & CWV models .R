#' ---
#' title: "Kelman Preliminary Results"
#' author: "ek, jl, cw"
#' date: "Feb 25 2019"
#' output: github_document
#' ---
#' script purpose: feed in CWM and CWV outputs to create figures and models for analyses 
#'
#' SETUP 
rm(list=ls()) # start with clean environment
options(stringsAsFactors = FALSE) #character variables never factor by default 

#'  Load libraries 
#+ results=FALSE, message=FALSE, warning=FALSE 
library(tidyverse) #tidyverse has ggplot2, no need to load separately


#set relative pathway to Google Drive --> user will need to adjust this <---
# **uncomment whichever path is yours when running script
gdrive <- "/Users/emilykelman/Google\ Drive" #emily's path
#gdrive <- "/Users/serahsierra/Google\ Drive" #ctw path
#gdrive <- "" #julie's path

#'read in data
CWV <- read.csv(paste0(gdrive, "/KelmanProject/Data/CWV_climate_merge.csv"))
tranCWM_regressions <- read.csv(paste0(gdrive, "/KelmanProject/Data/transectCWM_for_regressions.csv"))
tranCWM_figures <- read.csv(paste0(gdrive, "/KelmanProject/Data/transectCWM_for_figures.csv"))
poolCWM_regressions <- read.csv(paste0(gdrive, "/KelmanProject/Data/pooledCWM_for_figures.csv"))
poolCWM_figures <- read.csv(paste0(gdrive, "/KelmanProject/Data/pooledCWM_for_figures.csv"))
clim_dat <- read.csv(paste0(gdrive, "/KelmanProject/Data/boulder_climate.csv"))

#'
#'
#'create vector for spei_lag and add to tranCWM_regressions, tranCWM_figures, & CWV
CWV$spei_lag <- lag(CWV$spei_12, k=1)
tranCWM_regressions$spei_lag <- lag(tranCWM_regressions$spei_12, k=1)
CWV$lagged_spei12 <- CWV$spei_lag
tranCWM_figures$lagged_spei12 <- CWV$spei_lag


#'create long form CWV DF for creating figures
#'
CWV_figures <- CWV %>% gather("trait_name", value, 2:6)

#'=====create CWV figures (current and lagged)======
#'
#'
#'fig 1: variation in trait values (CWV) in relation to current spei_12 (pooled data)
#'
#'fig 2: variation in trait values (CWV) in relation to lagged spei_12 (pooled data)
#'  
#plot fig 1: panel plot of CWV of traits and current spei_12
current_CWV_spei_panel<- ggplot(subset(CWV_figures, trait_name%in% c("height", "RMR", "SLA", "RDMC", "seedmass")), 
mapping = aes(x=spei_12, y=value))+
  geom_point(size = 0.75)+
  geom_smooth(method=lm)+
  facet_grid(trait_name~., scales = "free_y")

current_CWV_spei_panel

#plot fig 2: panel plot of CWV of traits and lagged spei_12
lag_CWV_spei_panel <- ggplot(subset(CWV_figures, trait_name%in% c("height", "RMR", "SLA", "RDMC", "seedmass")),
   mapping = aes(x=spei_lag, y=value))+
  geom_point(size = 0.75)+
  geom_smooth(method=lm)+
  facet_grid(trait_name~., scales = "free_y")
  
lag_CWV_spei_panel            

#'====exploratory CWV figures======
#plot CWV RMR and spei_12
CWV_RMR_spei_fig <- ggplot(CWV, mapping = aes(x=spei_12, y=RMR))+
  geom_point()+
  geom_smooth(method=lm)

CWV_RMR_spei_fig

#'====run linear regressions for CWV and traits (current)=====
#LM of CWV RMR and spei_12
#p value .6 and r^2 -.03
#not significant
current_CWV_RMR_spei_LM<- lm(formula = RMR ~ spei_12, data = CWV)
summary(current_CWV_RMR_spei_LM)

#LM of CWV final height and spei_12
#p value .002 r^2 .2951
#significant!
current_CWV_height_spei_LM <- lm(formula = height ~ spei_12, data = CWV)
summary(current_CWV_height_spei_LM)

#LM CWV RDMC and spei_12
#p alue .19 r^2 .031
#not significant
current_CWV_RDMC_spei_LM <- lm(formula = RDMC ~ spei_12, data = CWV)
summary(current_CWV_RDMC_spei_LM)

#LM CWV seed mass and spei_12
#p value .449 r^2 -.017
#not significant
current_CWV_seedmass_spei_LM <- lm(formula = seedmass ~ spei_12, data = CWV)
summary(current_CWV_seedmass_spei_LM)

#LM CWV SLA and spei_12
#p value .42 r^2 -.014
current_CWV_SLA_spei_LM <- lm(formula = SLA ~ spei_12, data = CWV)
summary(current_CWV_SLA_spei_LM)

#'====run linear regressions for CWV and traits (lagged)=====
#LM of CWV RMR and lagged spei
#p value .02 r^2 .1624
#significant! 
lag_CWV_RMR_spei_LM <- lm(formula = RMR ~ lagged_spei12, data = CWV)
summary(lag_CWV_RMR_spei_LM)

#LM of CWV final height and lagged spei
#p value .89 r^2 -.044
#not significant
lag_CWV_height_spei_LM <- lm(formula = height ~ lagged_spei12, data = CWV)
summary(lag_CWV_height_spei_LM)

#LM of CWV RDMC and lagged spei
#p value .15 r^2 .04
#not significant
lag_CWV_RDMC_spei_LM <- lm(formula = RDMC ~ lagged_spei12, data = CWV)
summary(lag_CWV_RDMC_spei_LM)

#LM of CWV seedmass and lagged spei
#p value .73 r^2 -.04
#not significant
lag_CWV_seedmass_spei_LM <-lm(formula = seedmass ~ lagged_spei12, data = CWV)
summary(lag_CWV_seedmass_spei_LM)

#LM of CWV SLA and lagged spei
#p value .159 r^2 .04
#not significant
lag_CWV_SLA_spei_LM <- lm(formula = SLA ~ lagged_spei12, data = CWV)
summary(lag_CWV_SLA_spei_LM)

#'=====create CWM figures=====
#'
#'panel plot of CWMs, spei_12 on x, trait on y
#'
#'transect level
#'
#'fig 3: current spei_12
#'
#'fig 4: lagged spei_12
#'


#issue: trying to figure out how to show spei_12 on x-axis 
# **solution: need to have climate data in their own column if want to use on the x-axis, so just merge climate data with long-form trait value
# ** it's okay that climate data are both in the trait_name column, and in their own column. 
# ** it's more columns in the data frame, but also makes the data frame flexible so you can plot however you like
tranCWM_figures <- left_join(tranCWM_figures, clim_dat, by = c("Year" = "year"))

#plot fig 3: panel plot of CWM traits at transect level with current spei_12 on x
current_tranCWM_spei_panel <- ggplot(subset(tranCWM_figures, trait_name%in% c("final_height_cm", "RMR", "SLA", "RDMC", "seed_mass")), 
               mapping = aes(x=spei_12, y=value))+
  geom_point(aes(col = transect_ID.clean), size = 0.75)+
  geom_smooth(method=lm)+
  facet_grid(trait_name~., scales = "free_y")+
  ggtitle("Relationship between Transect level CWM and spei_12")

current_tranCWM_spei_panel

#'plot fig 4: panel plot of CWM traits at transect level with lagged spei_12 on x
lagged_tranCWM_spei_panel <- ggplot(subset(tranCWM_figures, trait_name%in% c("final_height_cm", "RMR", "SLA", "RDMC", "seed_mass")), 
                                    mapping = aes(x=lagged_spei12, y=value))+
  geom_point(aes(col = transect_ID.clean), size = 0.75)+
  geom_smooth(method=lm)+
  facet_grid(trait_name~., scales = "free_y")+
  ggtitle("relationship between CWM by transect and lagged spei_12")

lagged_tranCWM_spei_panel

#'====run linear regressions for CWM traits and spei_12 (current)=====
#'
#LM for CWM RMR and spei
#p value .5 r^2 -.002
#not significant
current_tranCWM_RMR_spei_LM <- lm(formula = RMR ~ spei_12, data = tranCWM_regressions)
summary(current_tranCWM_RMR_spei_LM)

#LM for CWM height and spei_12
#p value .037 r^2 .015
#significant!
current_tranCWM_height_spei_LM <- lm(formula = final_height_cm ~ spei_12, data = tranCWM_regressions)
summary(current_tranCWM_height_spei_LM)

#LM for CWM RDMC and spei_12
#p vaule .35 r^2 -.0006
#not significant
current_tranCWM_RDMC_spei_LM <- lm(formula = RDMC ~ spei_12, data = tranCWM_regressions)
summary(current_tranCWM_RDMC_spei_LM)

#LM for CWM seedmass and spei_12
#p value .66 r^2 -.003
#not significant
current_tranCWM_seedmass_spei_LM <-lm(formula = seed_mass ~ spei_12, data = tranCWM_regressions)
summary(current_tranCWM_seedmass_spei_LM)

#LM for CWM Sla and spei_12
# pvalue .43 r^2 -.0017
#not significant
current_tranCWM_SLA_spei_LM <- lm(formula = SLA ~spei_12, data = tranCWM_regressions)
summary(current_tranCWM_SLA_spei_LM)
#'
#'====run linear regressions for CWM traits and spei_12 (lagged)=====
#'
#LM of CWM height and lagged spei_12
#pvalue .2 r^2 .002
#not significant
lagged_CWM_height_spei_LM <-lm(formula = final_height_cm ~ spei_lag, data = tranCWM_regressions)
summary(lagged_CWM_height_spei_LM)

#LM of CWM RMR and lagged spei_12
#pvalue .004 r^2 .03144
#significant!
lagged_CWM_RMR_spei_LM <- lm(formula = RMR ~ spei_lag, data = tranCWM_regressions)
summary(lagged_CWM_RMR_spei_LM)

#LM of CWM RDMC and lagged spei_12
#pvalue .04 r^2 .0146
#significant
lagged_CWM_RDMC_spei_LM <-lm(formula=RDMC ~ spei_lag, data = tranCWM_regressions)
summary(lagged_CWM_RDMC_spei_LM)

#LM of CWM seedmass and lagged spei_12
#pvalue .86 r^2 -.004
#not significant
lagged_CWM_seedmass_spei_LM<-lm(formula = seed_mass ~ spei_lag, data = tranCWM_regressions)
summary(lagged_CWM_seedmass_spei_LM)

#LM of CWM SLA and lagged spei_12
#pvalue .08 r^2 .008
#not significant
lagged_CWM_SLA_spei_LM <-lm(formula = SLA ~ spei_lag, data = tranCWM_regressions)
summary(lagged_CWM_SLA_spei_LM)


  
