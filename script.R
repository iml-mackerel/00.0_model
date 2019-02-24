#################################################################################################################
#*** Mackerel assessment
#** based on CCAM package
#################################################################################################################

#################################################################################################################
########### READ IN DATA ########################################################################################
#################################################################################################################
year <- 2018
dir <- paste0('data/',year,'/')

cn <- read.ices(paste0(dir,'cn.dat'))
ct <- read.ices(paste0(dir,'ct.dat'))
ctUSA <- read.ices(paste0(dir,'ctUSA.dat'))
ctForeign <- read.ices(paste0(dir,'ctForeign.dat'))
cw <- read.ices(paste0(dir,'cw.dat'))
dw <- read.ices(paste0(dir,'dw.dat'))
lf <- read.ices(paste0(dir,'lf.dat'))
lw <- read.ices(paste0(dir,'lw.dat'))
mo <- read.ices(paste0(dir,'mo.dat'))
nm <- read.ices(paste0(dir,'nm.dat'))
nm[]<- 0.27
pf <- read.ices(paste0(dir,'pf.dat'))
pm <- read.ices(paste0(dir,'pm.dat'))
sw <- read.ices(paste0(dir,'sw.dat'))
sw0 <- read.ices(paste0(dir,'sw0.dat'))
surveys <- read.ices(paste0(dir,'survey.dat'))
surveys[[1]] <- surveys[[1]][!is.na(surveys[[1]]),1,drop=FALSE]
attr(surveys[[1]],'time') <- c(0.47)

# redefine catch limits (add 25-50% US catch)
ctwusa <- ct
ctwusa[,1] <- ct[,1]*1.10 + ctUSA[-c(1:8),1]*0.25
ctwusa[,2] <- ct[,2] + ctUSA[-c(1:8),1]*0.50

dat <- setup.ccam.data(surveys=surveys,
                      residual.fleet=cn,
                      total.catch=ct,
                      prop.mature=mo,
                      stock.mean.weight=sw,
                      stock.start.weight=sw0,
                      catch.mean.weight=cw,
                      dis.mean.weight=dw,
                      land.mean.weight=lw,
                      prop.f=pf,
                      prop.m=pm,
                      natural.mortality=nm,
                      land.frac=lf)

conf <- defcon(dat)
conf$keySel <- matrix(c(0,1,2,3,4,4,4,4,4,4), nrow=nrow(conf$keySel), ncol=ncol(conf$keySel),byrow = T)
conf$keyVarObs[1,]=-1                      # censored
conf$keyVarObs[2,1:9]=c(0,1,2,2,2,2,2,2,1) # variance structure CAA
conf$keyVarObs[3,1]=3           
conf$stockRecruitmentModelCode=0 #0: RW, 1: ricker, 2: BH, 3:mean
conf$fbarRange=c(5,10)
conf$obsLikelihoodFlag[1]='CE'

par <- defpar(dat,conf)

save(dat,file='Rdata/input/dat.Rdata')
save(conf,file='Rdata/input/conf.Rdata')
save(par,file='Rdata/input/par.Rdata')

# plots
# source('Rscripts/plot_data.R')

#################################################################################################################
########### fit model ###########################################################################################
#################################################################################################################

fit <- ccam.fit(dat,conf,par,silent=TRUE)           
fit

save(fit, file='Rdata/fit/fit.Rdata')
load('Rdata/fit/fit.Rdata')

library(TMB)
res <- oneStepPredict(fit$obj, observation.name="logobs", data.term.indicator="keep", discrete=FALSE)

#tried conditional 542:10..
# should try the same with idx for obs as well where Total catch
# But shouldn't all obs go into cpp in same format? Though it seems to work not fail like this.


# plots
# x <- fit
# name <- 'fitBase'
# source('Rscripts/plot_fit.R')


# Jitter analyses: results sensitive to initial values?
myjit <- jit(dat,conf,par,nojit=100,parallell = FALSE)  
jittab <- jittable(myjit)
write.table(jittab,"Rdata/fit/fit_jitter.txt")


