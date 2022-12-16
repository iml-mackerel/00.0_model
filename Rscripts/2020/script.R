#################################################################################################################
#*** Mackerel assessment 2021
#** based on CCAM package
#*
#*# Intermediate steps are saved as Rdata, so either script can be run from scratch or from different points
# - base model definition and fitting
# - define core and stress OMs
# - define HCRs
# - create one list with all scenarios (OMs * HCRs)
# - do forecasting
# - load all forecast into one list
# - plotting in separate scripts
#
#
# November/December 2020
# updating for 2021 stock assessment

#################################################################################################################

#################################################################################################################

packages = c('magrittr','lubridate','tidyverse','cowplot','ggridges','broom','ggeffects','performance','readxl','CCAM','LSD' )
invisible(lapply(packages, function(x) {if (!require(x, character.only = T)) {install.packages(x);require(x)}}))

#################################################################################################################
########### READ IN DATA ########################################################################################
#################################################################################################################


cn <- read.ices("data/2020/cn.dat")
ct <- read.ices("data/2020/ct.dat")  
ctUSA <- read.ices("data/2020/ctUSA.dat") 
# ctForeign <- read.ices("data/ctForeign.dat")
cw <- read.ices("data/2020/cw.dat")
dw <- read.ices("data/2020/dw.dat")
lf <- read.ices("data/2020/lf.dat")
lw <- read.ices("data/2020/lw.dat")
mo <- read.ices("data/2020/mo.dat") # 1968-2016 - Francois, Thomas, and EVB, 2018-2020 ADS
mo_raw <- read.ices("data/2020/mo_raw_emp.dat") # 1968-2016 - Francois, Thomas, and EVB, 2018-2020 ADS
mo_mod <- read.ices("data/2020/mo_mod.dat") # 1968-2016 - Francois, Thomas, and EVB, 2018-2020 ADS
mo <- smoothmatrix(mo,subset=8:nrow(mo),max=1,plot=TRUE) # 99% smooth seems strong
nm <- read.ices("data/2020/nm.dat")
nm[] <- 0.27
pf <- read.ices("data/2020/pf.dat")
pm <- read.ices("data/2020/pm.dat")
sw <- read.ices("data/2020/sw.dat")
sw0 <- read.ices("data/2020/sw0.dat")
surveys <- read.ices("data/2020/survey.dat")
surveys[[1]] <- surveys[[1]][!is.na(surveys[[1]]),1,drop=FALSE]
attr(surveys[[1]],'time') <- c(0.47)
tep <- read.ices("data/2020/tep.dat")
tep_plus_30 <- read.ices("data/2020/tep_+30perc.dat")
tep_min_30 <- read.ices("data/2020/tep_-30perc.dat")
tep_na_fill <- read.ices("data/2020/tep_fill_na.dat")

tep[[1]] <- tep[[1]][!is.na(tep[[1]]),1,drop=FALSE]
tep[[1]][,1] <- tep[[1]][,1]*1000000000000
attr(tep[[1]],'time') <- c(0.47)

tep_plus_30[[1]] <- tep_plus_30[[1]][!is.na(tep_plus_30[[1]]),1,drop=FALSE]
tep_plus_30[[1]][,1] <- tep_plus_30[[1]][,1]*1000000000000
attr(tep_plus_30[[1]],'time') <- c(0.47)

tep_min_30[[1]] <- tep_min_30[[1]][!is.na(tep_min_30[[1]]),1,drop=FALSE]
tep_min_30[[1]][,1] <- tep_min_30[[1]][,1]*1000000000000
attr(tep_min_30[[1]],'time') <- c(0.47)

tep_na_fill[[1]] <- tep_na_fill[[1]][!is.na(tep_na_fill[[1]]),1,drop=FALSE]
tep_na_fill[[1]][,1] <- tep_na_fill[[1]][,1]*1000000000000
attr(tep_na_fill[[1]],'time') <- c(0.47)

pfem <- read.ices("data/2020/propFemale.dat")
fec <- read.ices("data/2020/fec.dat") 
fec_old <- read.ices("data/2020/fec_old.dat") 
fec_smooth <- smoothmatrix(fec,subset=8:nrow(fec),max=8e+5,plot=TRUE) # subset is for the first 7 years where we just used the mean so no need to smooth

# define catch limits
ctwusa <- ct # catch with usa

# prefered model fitBase_run_2
ctwusa[,1] <- ct[,1]*1.10 + ctUSA[,1]*0.50 
ctwusa[,2] <- ct[,2] + ctUSA[,1]*0.75

# sensitivity 1
# ctwusa[,1] <- ct[,1]*1.10 + ctUSA[,1]*0.25 
# ctwusa[,2] <- ct[,2] + ctUSA[,1]*0.50

# sensitivity 2
# ctwusa[,1] <- ct[,1]*1.10 + ctUSA[,1]*0.75
# ctwusa[,2] <- ct[,2] + ctUSA[,1]

# sensitivity 3
# ctwusa[,1] <- ct[,1]*1.10 
# ctwusa[,2] <- ct[,2]*1.10  

dat <- setup.ccam.data(surveys=tep,
                       residual.fleet=cn,
                       total.catch=ctwusa,
                       prop.mature=mo_mod,
                       stock.mean.weight=sw,
                       stock.start.weight=sw0,
                       catch.mean.weight=cw,
                       dis.mean.weight=dw,
                       land.mean.weight=lw,
                       prop.f=pf,
                       prop.m=pm,
                       natural.mortality=nm,
                       land.frac=lf,
                       prop.fem=pfem,
                       fec=fec)

conf <- defcon(dat)
conf$keySel <- matrix(c(0,1,2,3,4,4,4,4,4,4), nrow=nrow(conf$keySel), ncol=ncol(conf$keySel),byrow = T)
conf$keyVarObs[1,]=-1                     
conf$keyVarObs[2,1:9]=c(0,1,2,2,2,2,2,2,1) 
conf$keyVarObs[3,1]=3           
conf$stockRecruitmentModelCode=2 #0: RW, 1: ricker, 2: BH, 3:mean
conf$obsLikelihoodFlag[1]='CE'
conf$keyBiomassTreat[3]=5
conf$fbarRange=c(5,10) #fully recruited fish... overal F and fully exploited F in resdoc (pg. 8)

par <- defpar(dat,conf)

# create missing directories
# 
# dir.create("./Rdata")
# dir.create("./Rdata/fit")
# dir.create("./Rdata/2020/OMs")

save(dat,file='Rdata/2020/input/dat_run_6_2020.Rdata')
save(conf,file='Rdata/2020/input/conf_run_6_2020.Rdata')
save(par,file='Rdata/2020/input/par_run_6_2020.Rdata')

# plots
 source('Rscripts/2020/surplus/plot_data.R')


#################################################################################################################
########### Fit model ###########################################################################################
#################################################################################################################

x <- ccam.fit(dat,conf,par,silent = FALSE,paracheck = FALSE,debug = T)            
fitBase_run_6

save(fitBase_run_6, file = 'Rdata/2020/OMs/fitBase_run_6.Rdata')
#load(file='Rdata/OMs/fitBase.Rdata')
#################################################################################################################
########### READ IN DATA ########################################################################################
#################################################################################################################
# year <- 2020

# data / configuration / parameter initialisation
# source(paste0('Rscripts/',year,'/surplus/read_data.R'))

# plots
# source(paste0('Rscripts/',year,'/surplus/plot_data.R'))


#################################################################################################################
########### PLOTS & TABLES ######################################################################################
#################################################################################################################

# plots
fit <- fitBase_run_6
name <- 'fitBase_run_6'
source('Rscripts/2020/surplus/plot_fit.R')
# source('Rscripts/plot_data.R')
#################################################################################################################
########### fit model ###########################################################################################
#################################################################################################################

# fit <- ccam.fit(dat,conf,par,debug=T)           
# fit

save(fit, file=paste0('Rdata/',year,'/fit.Rdata'))
load(paste0('Rdata/',year,'/fit.Rdata'))

# plots
# source(paste0('Rscripts/',year,'/surplus/plot_fit.R'))

# source(paste0('Rscripts/',year,'/surplus/save_csv.R'))


## Modifiy Implementation error for 2021 assessment
##' IEindep2021
##' @param x number of simulations
##' @param y number of timestep
##' @param seed set.seed
##' @rdname IEindep2021
##' @details generates matrix nosim x nyears with implementation errors that will be added to the TAC00
##' @export
IEindep2021 <- function(x,y,seed=NULL){
    IEmean=c(Reduce(function(v, x) .83*v , x=numeric(3),  init=3000, accumulate=TRUE)[-1],rep(3000,y-3))[1:y]
    IEsd=IEmean/8
    if(!is.null(seed)) set.seed(seed)
    ret=mapply(function(mu,sigma){rnorm(mu,sigma,n=x)},mu=IEmean,sigma=IEsd)
    return(ret)
}
class(IEindep2021) <- append(class(IEindep2021),"IE")


# figures

# 
ssbplot(c(fit, fitBase_run_1, fitBase_run_2))
ssbplot(c(fitBase_run_4, fitBase_run_5,fitBase_run_2, fitBase_run_6, fitBase_run_7)) + 
    scale_colour_viridis_d(name = "M", labels = c("0.15", "0.2", "0.27", "0.3", "0.4"))

fbarplot(c(fitBase_run_4, fitBase_run_5,fitBase_run_2, fitBase_run_6, fitBase_run_7)) + 
    scale_colour_viridis_d(name = "M", labels = c("0.15", "0.2", "0.27", "0.3", "0.4" ))

ssbplot(c(fit, fitBase_run_1, fitBase_run_2)) + xlim(1975,2020)
recplot(c(fit, fitBase_run_1, fitBase_run_2)) + xlim(1975,2020)


recplot(c(fitBase_run_4, fitBase_run_5,fitBase_run_2, fitBase_run_7, fitBase_run_7, fitBase_run_8))
resplot(c(fitBase_run_4, fitBase_run_5,fitBase_run_2, fitBase_run_7, fitBase_run_7, fitBase_run_8))
resplot(fitBase_run_2)

    
    fbarplot(c(fitBase_run_1,fitBase_run_2)) + theme_minimal()
