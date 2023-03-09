#################################################################################################################
#*** Mackerel assessment
#** based on CCAM package
#################################################################################################################

#################################################################################################################
########### READ IN DATA ########################################################################################
#################################################################################################################
year <- 2022

source("C:/Users/VANBE/Desktop/post-doc/DATA/CCAM/R/reading.R")
source("C:/Users/VANBE/Desktop/post-doc/DATA/CCAM/R/plot.R")
source("C:/Users/VANBE/Desktop/post-doc/DATA/CCAM/R/tables.R")
source("C:/Users/VANBE/Desktop/post-doc/DATA/CCAM/R/retro.R")
# data / configuration / parameter initialisation
# source(paste0('Rscripts/',year,'/surplus/read_data.R'))

# plots
# source(paste0('Rscripts/',year,'/surplus/plot_data.R'))

#################################################################################################################
########### fit model ###########################################################################################
#################################################################################################################

fit <- ccam.fit(dat,conf,par,debug=T)           
fit

save(fit, file=paste0('Rdata/',year,'/fit.Rdata'))
load(paste0('Rdata/',year,'/fit.Rdata'))

# plots
# x <- fit
# name <- 'fitBase'
# source(paste0('Rscripts/',year,'/surplus/plot_fit.R'))

# source(paste0('Rscripts/',year,'/surplus/save_csv.R'))


