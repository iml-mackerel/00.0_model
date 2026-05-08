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
year <- 2025

#to add a new year
source("Rscripts/2025/add_year_ices.R")

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

#save(fit, file=paste0('Rdata/',year,'/fit.Rdata'))
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
 source(paste0("Rscripts/",year,"/surplus/probability_above_FF.R"))
 
 source("R/multi.forecast.R")#patch for CCAM
 #patch
 cumsum.bounded <- function(x, lower = 0, upper = 500) {
     bsum <- function(x, y) min(upper, max(lower, x+y))
     if (length(x) > 1) Reduce(bsum, x, acc = TRUE) else x
 }
 #source(paste0("Rscripts/",year,"/surplus/Tmin.R")) #not done in 2026
 source(paste0("Rscripts/",year,"/projections.R")) 
 
 #what is the effect of the US increasing their TAC
 source(paste0("Rscripts/",year,"/projections_additional_runs_Us.R"))
 #no additionnal removals
 source(paste0("Rscripts/",year,"/projections_additional_runs_norec.R"))
 
 
 source(paste0("Rscripts/",year,"/surplus/plot_missingCatch.R"))
 
  #sensitivity
 source(paste0("Rscripts/",year,"/surplus/ssbplot0.R"))
 source(paste0("Rscripts/",year,"/surplus/compare_OM_varObs.R"))
 source(paste0("Rscripts/",year,"/surplus/compare_OM_rec_random_UP2015.R"))
#source(paste0("Rscripts/",year,"/surplus/compare_OM_CAA.R"))?TO DI?
source(paste0("Rscripts/",year,"/surplus/compare_OM_sel.R"))# does not converge the one of interest. 
 
#source(paste0("Rscripts/",year,"/surplus/sensitivity_TEP.R"))#NOT DONE.....
#source(paste0("Rscripts/",year,"/surplus/compare_OM_C.R"))# little importance. landings were low in recent years. 
#source(paste0("Rscripts/",year,"/surplus/compare_OM_eggsurveyyears.R")) #not done

 source(paste0("Rscripts/",year,"/surplus/compare_OM_Mct.R"))
 source(paste0("Rscripts/",year,"/surplus/compare_OM_Mtimevarying.R"))
 source(paste0("Rscripts/",year,"/surplus/compare_OM_Mconsumption.R"))
 
 load(file=paste0('Rdata/',year,'/sensitivity/fit.Mconsum.Rdata'))
 x <- fitM
 name <- 'Mconsum'
 source(paste0('Rscripts/',year,'/surplus/plot_fit.R'))
 #Including consumption mess with the stock recruitment relationship. Higher intervals on parameters.  
 #As such spawner per recruit suggest that F was OK and the stock always above the LRP. 
 #However, this not supported by the productivity of the stock that is impared in recent years
 
 
 
 #values to intefrate in document
 
 refBase <- ypr(fit)
 refBase$f40ssb

 refBase$LRP
 
  source("Rscripts/2024/surplus/fbar.R")
  tail(summary(fit),2)
f1.10  
 


#recruitment comparison
rec <- read_csv("csv/2024/rec.csv")
rec %>%  mutate(group.year= if_else(year <= 2010, "1", 
                if_else(year >=2011,"2", NA))) %>% dplyr::filter(!is.na(group.year)) %>% 
    group_by(group.year) %>% summarize(mean(Estimate
                                            )/1000
                                       ) 
#56.8/222*100 = 26%

#porpotion of age 2-3
default<- read.csv(paste0("../02.0_catch-at-age/csv/",year,"/caa_interpol.csv"))[1:3] %>% 
    pivot_wider(names_from=age, values_from="caan") %>%  as.data.frame() %>%  filter(year>=2022
                                                                                     ) %>% 
    rowwise() %>% 
    mutate(across(`1`:`10`, ~ (. / sum(c_across(`1`:`10`))) * 100)) %>% 
mutate(total_pct = sum(c_across(`1`:`10`)),
       age1_3= sum(c_across(`1`:`3`))) 

