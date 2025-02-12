#################################################################################################################
#*** Mackerel assessment
#** based on CCAM package
#################################################################################################################

#################################################################################################################
########### READ IN DATA ########################################################################################
#################################################################################################################
library(CCAM)
library(ggpubr)
library(tidyverse)
source("R/saveplot.R")
library(catchR)
theme_set(theme_mackerel())   

#to add a new year
source("Rscripts/2024/add_year_ices.R")
year <- 2024
#.dat should be edited in word pad to avoid problems

# data / configuration / parameter initialisation
source(paste0('Rscripts/',year,'/surplus/read_data.R'))

# plots
source(paste0('Rscripts/',year,'/surplus/plot_data.R'))

#################################################################################################################
########### fit model ###########################################################################################
#################################################################################################################

fit <- ccam.fit(dat,conf,par,debug=T)           
fit

save(fit, file=paste0('Rdata/',year,'/fit.Rdata'))
load(paste0('Rdata/',year,'/fit.Rdata'))

# plots
 x <- fit
 name <- 'fitBase'
 #be sure to clear environment (excepth year and fit) before running retro. 
 #If R session fails, that is probably the reason
 
 rm(list=ls()[! ls() %in% c("year","fit", "x", "name","saveplot")])
 
 source(paste0('Rscripts/',year,'/surplus/plot_fit.R'))
 #source(paste0('Rscripts/',year,'/surplus/retro.R')) bug for some peels 
 source(paste0("Rscripts/",year,"/surplus/retro_hack.R"))
 
 
 
 source(paste0('Rscripts/',year,'/surplus/save_csv.R'))
 my.year=year
 source(paste0("Rscripts/",year,"/surplus/probability_above_LRP.R"))
 
 source("R/multi.forecast.R")#patch for CCAM
 #patch
 cumsum.bounded <- function(x, lower = 0, upper = 500) {
     bsum <- function(x, y) min(upper, max(lower, x+y))
     if (length(x) > 1) Reduce(bsum, x, acc = TRUE) else x
 }
 source(paste0("Rscripts/",year,"/surplus/Tmin.R"))
 source(paste0("Rscripts/",year,"/projections.R")) 
 source(paste0("Rscripts/",year,"/surplus/plot_missingCatch.R"))
 
  #sensitivity
 source(paste0("Rscripts/",year,"/surplus/ssbplot0.R"))
 
 source(paste0("Rscripts/",year,"/surplus/compare_OM_varObs.R"))
 source(paste0("Rscripts/",year,"/surplus/compare_OM_CAA.R"))
 source(paste0("Rscripts/",year,"/surplus/compare_OM_sel.R"))
 
 source(paste0("Rscripts/",year,"/surplus/sensitivity_TEP.R"))
 source(paste0("Rscripts/",year,"/surplus/compare_OM_C.R"))
 source(paste0("Rscripts/",year,"/surplus/compare_OM_eggsurveyyears.R"))

 source(paste0("Rscripts/",year,"/surplus/compare_OM_Mct.R"))
 source(paste0("Rscripts/",year,"/surplus/compare_OM_Mtimevarying.R"))
 

 source(paste0("Rscripts/",year,"/surplus/forecast_patch.R"))
 source(paste0("Rscripts/",year,"/surplus/demo_projection_validation.R"))
 
 #values to intefrate in document
 
 refBase <- ypr(fit)
 refBase$f40ssb

 refBase$LRP
 
  source("Rscripts/2024/surplus/fbar.R")
  tail(summary(fit),2)
f1.10  
 
  