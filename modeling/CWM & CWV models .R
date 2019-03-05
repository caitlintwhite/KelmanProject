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

#'#issue: trying to figure out how to show spei_12 on x-axis 
# **solution: need to have climate data in their own column if want to use on the x-axis, so just merge climate data with long-form trait value
# ** it's okay that climate data are both in the trait_name column, and in their own column. 
# ** it's more columns in the data frame, but also makes the data frame flexible so you can plot however you like
tranCWM_figures <- left_join(tranCWM_figures, clim_dat, by = c("Year" = "year"))
#'
#'create vector for spei_lag and add to tranCWM_regressions, tranCWM_figures, & CWV
CWV <-CWV%>% 
  mutate(lagged_spei12=lag(spei_12))

tranCWM_figures <- tranCWM_figures%>%
  group_by(transect_ID.clean)%>%
   mutate(lagged_spei12=lag(spei_12))

tranCWM_regressions <-tranCWM_regressions%>%
  group_by(transect_ID.clean)%>%
  mutate(lagged_spei12=lag(spei_12))
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
   mapping = aes(x=lagged_spei12, y=value))+
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




#plot fig 3: panel plot of CWM traits at transect level with current spei_12 on x. shows overall trend
current_tranCWM_spei_panel <- ggplot(subset(tranCWM_figures, trait_name%in% c("final_height_cm", "RMR", "SLA", "RDMC", "seed_mass")), 
                                     mapping = aes( x=spei_12, y=value))+
  geom_point(size = 0.75)+
  geom_smooth(method=lm, se=FALSE)+
  facet_grid(trait_name~., scales = "free_y")+
  ggtitle("Relationship between Transect level CWM and spei_12")

current_tranCWM_spei_panel

#plot CWM traits and current spei_12. Shows changes in mean value across transects
current_tranCWM_spei_panel <- ggplot(subset(tranCWM_figures, trait_name%in% c("final_height_cm", "RMR", "SLA", "RDMC", "seed_mass")), 
               mapping = aes(col = transect_ID.clean, x=spei_12, y=value))+
  geom_point(aes(col = transect_ID.clean), size = 0.75)+
  geom_smooth(method=lm, se=FALSE)+
  facet_grid(trait_name~transect_ID.clean, scales = "free_y")+
  ggtitle("Relationship between Transect level CWM and spei_12")

current_tranCWM_spei_panel

#'plot fig 4: panel plot of CWM traits at transect level with lagged spei_12 on x. shows different transects explicitly
lagged_tranCWM_spei_panel<- ggplot(subset(tranCWM_figures, trait_name%in% c("final_height_cm", "RMR", "SLA", "RDMC", "seed_mass")), 
                                    mapping = aes(col=transect_ID.clean, x=lagged_spei12, y=value))+
  geom_point( size = 0.75)+
  geom_smooth(method=lm)+
  facet_grid(trait_name~transect_ID.clean, scales = "free_y")+
  ggtitle("relationship between CWM by transect and lagged spei_12")

lagged_tranCWM_spei_panel


#panel plot CWM traits and lagged spei_12 across transects to observe global trends
lagged_tranCWM_spei_panel_global <- ggplot(subset(tranCWM_figures, trait_name%in% c("final_height_cm", "RMR", "SLA", "RDMC", "seed_mass")), 
                                    mapping = aes( x=lagged_spei12, y=value))+
  geom_point( size = 0.75)+
  geom_smooth(method=lm)+
  facet_grid(trait_name~., scales = "free_y")+
  ggtitle("relationship between CWM by transect and lagged spei_12")

lagged_tranCWM_spei_panel_global


#'====run linear regressions for CWM traits and spei_12 (current)=====
#'
#LM for CWM RMR and spei
#p value .5 r^2 -.002
#not significant
current_tranCWM_RMR_spei_LM <- lm(formula = RMR ~ spei_12*transect_ID.clean, data = tranCWM_regressions)
summary(current_tranCWM_RMR_spei_LM)

current_tranCWM_RMR_spei_LM2 <- lm(formula = RMR ~ spei_12+transect_ID.clean, data = tranCWM_regressions)
current_tranCWM_RMR_spei_LM3 <- lm(formula = RMR ~ spei_12, data = tranCWM_regressions)

aov(current_tranCWM_RMR_spei_LM, current_tranCWM_RMR_spei_LM2)

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
lagged_CWM_height_spei_LM <-lm(formula = final_height_cm ~ lagged_spei12, data = tranCWM_regressions)
summary(lagged_CWM_height_spei_LM)

#LM of CWM RMR and lagged spei_12
#pvalue .004 r^2 .03144
#significant!
lagged_CWM_RMR_spei_LM <- lm(formula = RMR ~ lagged_spei12, data = tranCWM_regressions)
summary(lagged_CWM_RMR_spei_LM)

#LM of CWM RDMC and lagged spei_12
#pvalue .04 r^2 .0146
#significant
lagged_CWM_RDMC_spei_LM <-lm(formula=RDMC ~ lagged_spei12, data = tranCWM_regressions)
summary(lagged_CWM_RDMC_spei_LM)

plot(tranCWM_regressions$lagged_spei12,tranCWM_regressions$RDMC)

#LM of CWM seedmass and lagged spei_12
#pvalue .86 r^2 -.004
#not significant
lagged_CWM_seedmass_spei_LM<-lm(formula = seed_mass ~ lagged_spei12, data = tranCWM_regressions)
summary(lagged_CWM_seedmass_spei_LM)

#LM of CWM SLA and lagged spei_12
#pvalue .08 r^2 .008
#not significant
lagged_CWM_SLA_spei_LM <-lm(formula = SLA ~ lagged_spei12, data = tranCWM_regressions)
summary(lagged_CWM_SLA_spei_LM)




# -- COMPILE ALL LM RESULTS -----
# 1) create vector of linear model objects in your global environment
# list all objects in your global environment
env_objects <- ls()
# grab the objects that are linear models 
## this line iterates over env_objects and tests whether it is an lm objects or not, it returns TRUE or FALSE
lms_only <- sapply(env_objects, function(x) class(get(x)) == "lm")
## this line subsets the env_objects vector to keep only the positions where lms_only == TRUE
## i.e. it pulls out the names of lm objects only
lm_objects <- env_objects[lms_only]

# 2) # initiate empty data frames for lm results
# to store overall model results
model_df <- data.frame()
# to store coefficient results
coeff_df <- data.frame()

# 3) use a for-loop to grab model results in all models generated
# a for-loop will run the code in between the curly brackets for every object in your lm_objects, one at a time
for(o in 1:length(lm_objects)){
  # isolate the lm of interest, store in  temporary model object
  temp.lm  <- get(lm_objects[o])
  model_call <- as.character((summary(temp.lm)[[1]]))
  dataset = model_call[3]
  model = model_call[2]
  terms <- names(temp.lm$coefficients)
  # iterate through each term in the linear model and grab results
  for(t in 1:length(terms)){
    # this inner-for loop grabs model results for individual terms 
    coeff.name = terms[t]
    coeff.value = temp.lm$coefficients[[t]]
    CI.lower.bound = confint(temp.lm)[t]
    CI.upper.bound = confint(temp.lm)[t+2]
    coeff.pval = summary(temp.lm)[[4]][colnames(summary(temp.lm)[[4]]) == "Pr(>|t|)"][t]
    # compile coefficient term results data frame, adding in results for term isolated at top of inner-for loop
    coeff_df <- rbind(coeff_df, 
                      data.frame(cbind(dataset, model, coeff.name, coeff.value, coeff.pval, CI.upper.bound, CI.lower.bound)))
    
  }
  # number of observations used in model (i.e. sample size)
  n = nrow(temp.lm$model)
  # overall model p-value
  model.pval = anova(temp.lm)$`Pr(>F)`[1]
  # model adjusted r-squared
  adj.r2 = summary(temp.lm)$adj.r.squared
  # compile overall model results data frame, adding in results for model isolated at top of for loop
  model_df <- rbind(model_df,
                    data.frame(cbind(dataset, model, n, model.pval, adj.r2)))
}

# 4) compile everything in a master results data frame
## overall model values will repeat with each coefficient term (because those terms are from the same model)
master_lm_results <- merge(model_df, coeff_df)




  
