#####################   NWA mackerel (northern population/contingent) stock assessment 2021  #################### 

##  - Model: CCAM (a state-space censured catch-at-age-model) https://github.com/elisvb/CCAM (Author = Dr. Elisabeth Van Beveren)
##  - Based on SAM (Stock assessment model; https://github.com/fishfollower/SAM) 
#     & NCAM (Noel Cadigan's asseessment model for Northern Cod (NL - Canada)) 
##  - Script amended for 2021 stock assessment by Dr. Andrew D. Smith 
##   - Contact: (DFO - IML; andrew.d.smith@dfo-mpo.gc.ca) 

#####################  Load libraries  ####################

# Note: You need Rtools, Rccp, CCAM, ggplot2, and plyr to run most of the code. 
# devtools::install_github("elisvb/CCAM",force = TRUE)

packages = c('magrittr','lubridate','tidyverse','readr','readxl','CCAM','cowplot')
invisible(lapply(packages, function(x) {if (!require(x, character.only = T)) {install.packages(x);require(x)}}))

#####################  Load data  ####################

# Landings data
ct <- read.ices("data/2020/ct_2020.dat") # Landings in Canadian EEZ (Canadian and Foreign vessels) 
ctUSA <- read.ices("data//2020/ctUSA_2020.dat") # Landings by US vessels in US EEZ "us_eez_u.csv" 
# ctUSA[53,1] <- 8025 # 2020 update from Kiersten Curti
ctForeign <- read.ices("data/2020/ctForeign_2020.dat") # Landings by foreign vessels in US EEZ "us_eez_f.csv"


# Bio data
cn <- read.ices("data/2020/cn_2020.dat") # catch at age (N)
cw <- read.ices("data/2020/cw_2020.dat") # catch mean mass at age (Kg)
dw <- read.ices("data/2020/dw_2020.dat") # discard mean mass at age (Kg)
lf <- read.ices("data/2020/lf_2020.dat") # landed fraction
lw <- read.ices("data/2020/lw_2020.dat") 
mo <- read.ices("data/2020/mo_2020.dat") # proportion mature (glm binomial probit link; 1968-2016 - Francois, Thomas, and EVB); 2018-2020 ADS (glm logit link)
# mo_raw <- read.ices("data/2020/mo_2020_raw_emp.dat") # proportion mature raw empirical
# mo_mod <- read.ices("data/2020/mo_2020_mod.dat") # proportion mature (glm binomial logit link) ADS full time series and NAs filled with linear interpolation
nm <- read.ices("data/2020/nm_2020.dat") # natural mortality (M)
nm[] <- 0.27
pf <- read.ices("data/2020/pf_2020.dat") # proportion of fishing mortality (F) before spawning
pm <- read.ices("data/2020/pm_2020.dat") # proportion of natural mortality (M) before spawning
sw <- read.ices("data/2020/sw_2020.dat") # stock mean mass at age (Kg) - presently = cw
sw0 <- read.ices("data/2020/sw0_2020.dat") # stock mean mass at age (Kg) on January 1st - presently = cw. Use Rivard eqn in NOAA toolbox in future
pfem <- read.ices("data/2020/propFemale_2020.dat") # proportion female per year and age (i.e. sex ratio) - presently all ages same in given year (mean of ages)
# fec <- read.ices("data/2020/fec_2020.dat") # fecundity by year and age - recalculated from Line Pelletier's (1986) data. fecundity ~ age + gonad mass (g)
# fec_old <- read.ices("data/2020/fec_2020_old.dat") # fecundity by year and age - Pelletier's eqn for fecundity ~ age. Non time varying
fec <- read.ices("data/2020/fec_2020_update.dat") # fecundity by year and age - recalculated from Line Pelletier's (1986) data. fecundity ~ age + gonad mass (g)

# Smoothing options for some variables using a cubic spline smoother (custom function to be sourced that relies on matplot and smooth.spline. Recommended by external reviewer during the 2018 assessment but smoother set at 99% which seems unrealistic so for this assessment I set it at 50% which looks more reasonable 
mo_smoothed <- smoothmatrix(mo, smooth = 0.5, subset = 0:nrow(mo),max = 1,plot = TRUE) # cubic spline smoother. 
fec_smooth <- smoothmatrix(fec, subset = 0:nrow(fec), smooth = 0.5, max = 8e+5, plot = TRUE) # subset is for the first 7 years where we just used the mean so no need to smooth
cw_smooth <- smoothmatrix(cw, subset = 0:nrow(cw),smooth = 0.5,max = 2,plot = TRUE) # subset is for the first 7 years where we just used the mean so no need to smooth

# Icthyoplankton survey data
# surveys <- read.ices("data/2020/survey_2020.dat") # SSB (tonnes) = (daily_egg_production (N/m^2) * survey area (m^2) * mean mass (g)) / (spawning fraction at median mission date * mean fecundity (fec~gonad mass - Pelletier 1986) * sex ratio * 10^6)
# surveys[[1]] <- surveys[[1]][!is.na(surveys[[1]]), 1, drop = FALSE] # remove survey years that didn't happen or had equipment failures
# attr(surveys[[1]],'time') <- c(0.47) # set time to mean peak spawning time i.e. the solstice (Julian day of year 171/365)

tep <- read.ices("data/2020/tep_2020.dat") # Total egg production (trillions of eggs) = (daily_egg_production (N/m^2) * survey area (m^2)) / spawning fraction at median mission date
tep[[1]] <- tep[[1]][!is.na(tep[[1]]), 1, drop = FALSE] 
tep[[1]][,1] <- tep[[1]][,1] * 1000000000000 #ie 1e+12 trillion
attr(tep[[1]],'time') <- c(0.47)

# # sensitivity runs to capture potential missed spawning events outside the survey area and/or skipped spawning
# tep_plus_30 <- read.ices("data/2020/tep_2020_+30perc.dat") # + 30% 
# tep_min_30 <- read.ices("data/2020/tep_2020_-30perc.dat") # - 30%
# tep_na_fill <- read.ices("data/2020/tep_2020_fill_na.dat") # - filled in missing years with means of bookending years
# 
# tep_plus_30[[1]] <- tep_plus_30[[1]][!is.na(tep_plus_30[[1]]), 1, drop = FALSE]
# tep_plus_30[[1]][,1] <- tep_plus_30[[1]][,1]*1000000000000
# attr(tep_plus_30[[1]],'time') <- c(0.47)
# 
# tep_min_30[[1]] <- tep_min_30[[1]][!is.na(tep_min_30[[1]]), 1, drop = FALSE]
# tep_min_30[[1]][,1] <- tep_min_30[[1]][,1]*1000000000000
# attr(tep_min_30[[1]],'time') <- c(0.47)
# 
# tep_na_fill[[1]] <- tep_na_fill[[1]][!is.na(tep_na_fill[[1]]), 1, drop = FALSE]
# tep_na_fill[[1]][,1] <- tep_na_fill[[1]][,1]*1000000000000
# attr(tep_na_fill[[1]],'time') <- c(0.47)

# sensitivity different upper bounds in 2018:2020 1: use same as 2018 assessment, 2: bigger, 3 constant 20%

# ct[51,2] <- 22627 # 3320+8380=11700;    11700+10927=22627
# ct[52,2] <- 20404 # 3320+8380=11700;   11700+8704=20404
# ct[53,2] <- 19538 # 3320+8380=11700;  11700+7838=19538
# 
# ct[,2] <- ct[,1]*3 # 3x bigger than lower bounds
# 
# ct[,2] <- ct[,1]*1.2 # 20 % bigger than lower bounds


#####################  Model configuration  ####################

# Define lower and upper bounds of catch censure

# Base Model: 25-50% US catch added to lower and upper bounds respectively as well as a 10% smudge
ctwusa <- ct # catch with USA
ctwusa[,1] <- ct[,1]*1.10 + ctUSA[,1]*0.25   # difference between ct.dat for 2016 and 2018 is that there is an extra +10% on the max value (after bait estimates added), be careful not to double multiply any of the bounds. 
ctwusa[,2] <- ct[,2]*1.10 + ctUSA[,1]*0.50

# ctwusa <- ct # catch with USA
# ctwusa[,1] <- ct[,1]*1.10 + ctUSA[,1]+ctForeign[,1]   # difference between ct.dat for 2016 and 2018 is that there is an extra +10% on the max value (after bait estimates added), be careful not to double multiply any of the bounds. 
# ctwusa[,2] <- ct[,2]*1.10 + ctUSA[,1]+ctForeign[,1] 
# Fill slots with input data. can subset with [] if retro not working

# if no data due to moratorium....

dat <- setup.ccam.data(surveys = tep,
                       residual.fleet = cn,
                       total.catch = ctwusa,
                       prop.mature = mo_smoothed,
                       stock.mean.weight = cw_smooth,
                       stock.start.weight = cw_smooth,
                       catch.mean.weight = cw_smooth,
                       dis.mean.weight = cw_smooth,
                       land.mean.weight = cw_smooth,
                       prop.f = pf,
                       prop.m = pm,
                       natural.mortality = nm,
                       land.frac = lf,
                       prop.fem = pfem,
                       fec = fec_smooth)

# Setup basic minimal configuration for CCAM model. See ?CCAM::defcon for definitions of terms
conf <- defcon(dat)

conf$keySel <- matrix(c(0,1,2,3,4,4,4,4,4,4), nrow = nrow(conf$keySel), ncol = ncol(conf$keySel),byrow = T) # selectivity structure
conf$keyVarObs[1,] = -1 # observation variance structure                    
conf$keyVarObs[2,1:9] = c(0,1,2,2,2,2,2,2,1) 
conf$keyVarObs[3,1] = 3           
conf$stockRecruitmentModelCode = 2 # options: 0 = random walk, 1 = Ricker (i.e. density dependance), 2 = Beverton-Holt, 3 = mean (over n years?)
conf$obsLikelihoodFlag[1] = 'CE'# fleet type 3
conf$keyBiomassTreat[3] = 5
conf$fbarRange = c(5,10) # Fully recruited fish. Based on sensitivity runs of selectivity by EVB in 2017

# model parameterization
par <- defpar(dat,conf)

# save each runs input 
save(dat,file = 'Rdata/2020/input/dat_2020.Rdata')
save(conf,file = 'Rdata/2020/input/conf_2020.Rdata')
save(par,file = 'Rdata/2020/input/par_2020.Rdata')

# Plot input data (automatically stored in "./img/data/)

source('Rscripts/2020/surplus/plot_data.R')

    
#####################  Fit Model  ####################
  

# Run the model and name the object something appropriate
fit <- ccam.fit(dat, conf, par, silent = FALSE, paracheck = FALSE, debug = T)            
fit # check convergence
AIC(fit) # model AIC - Important for comparing different M scenarios only

# save the run and name it appropriately to not overwrite your data
# save(fit, file = 'Rdata/2020/OMs/fit_base_2020_corrected.Rdata')
save(fit, file = 'Rdata/2020/fit_compare/fit_2020_base_model.Rdata')

#load(file='Rdata/OMs/fitBase.Rdata')

 
#####################  Figures  ####################

# Automatically generate and save model fit figures in "./img/fit/   model name has to be "fit" 
# fit <- fit_base_2020
name <- 'fit'
source('Rscripts/2020/surplus/plot_fit.R')

# Compare many models
ssbplot(c(fit, fitnwa))
fbarplot(c(fit, fitnwa))
recplot(c(fit, fitnwa),ci=F)+scale_x_continuous(limits = c(1969,2020)) + scale_y_continuous(limits=c(0,1.2e+6))

fit

########### Tables ###########################################################################################
# Parameter table
par <- partable(fit)

# Full model summary
modsummary <- summary(fit_ctnwa1000)

# Age disagregated instantaneous fishing mortality
fay <- faytable(fit)

# numbers at age
nay <- ntable(fit)/1000
pnay <- prop.table(nay, margin = 1)
# catch
catchtab <- catchtable(fit)
########### Retrospective Analysis ###########################################################################################
# retro() not working so use runwithout() iteratively?
n <- ntable(fit)
pcn <- prop.table(cn, margin = 1)
pn <- prop.table(n, margin = 1)
pcn <- as_tibble(pcn) %>% mutate(year = seq(1968,2020,1)) %>% pivot_longer(cols = 1:10, names_to = "age")
pn <- as_tibble(pn) %>% mutate(year = seq(1968,2020,1)) %>% pivot_longer(cols = 1:10, names_to = "age")
dfn <- left_join(pcn,pn, by = c("year", "age"))
dfn %>% ggplot(aes(year, value.x)) + geom_point() +
    geom_line(aes(year,value.y), colour = "red") + facet_wrap(vars(age))

pcn <- prop.table(cn, margin = 2)
pn <- prop.table(n, margin = 2)
pcn <- as_tibble(pcn) %>% mutate(year = seq(1968,2020,1)) %>% pivot_longer(cols = 1:10, names_to = "age")
pn <- as_tibble(pn) %>% mutate(year = seq(1968,2020,1)) %>% pivot_longer(cols = 1:10, names_to = "age")
dfn2 <- left_join(pcn,pn, by = c("year", "age"))
dfn2 %>% ggplot(aes(year, value.x)) + geom_point() +
    geom_line(aes(year,value.y), colour = "red") + facet_wrap(vars(age))
########### EVB stuff to integrate for file hierarchy ###########################################################################################

# year <- 2020

# data / configuration / parameter initialisation
# source(paste0('Rscripts/',year,'/surplus/read_data.R'))

# plots
# source(paste0('Rscripts/',year,'/surplus/plot_data.R'))


# fit <- ccam.fit(dat,conf,par,debug=T)           
# fit

# save(fit, file = paste0('Rdata/',year,'/fit.Rdata'))
# load(paste0('Rdata/',year,'/fit.Rdata'))

# plots
# source(paste0('Rscripts/',year,'/surplus/plot_fit.R'))

# source(paste0('Rscripts/',year,'/surplus/save_csv.R'))
