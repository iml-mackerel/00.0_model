#################################################################################################################
#*** Mackerel assessment
#** based on CCAM package
#################################################################################################################

#################################################################################################################
########### READ IN DATA ########################################################################################
#################################################################################################################

cn <- read.ices("data/cn.dat")
ct <- read.ices("data/ct.dat")
ctUSA <- read.ices("data/ctUSA.dat")
ctForeign <- read.ices("data/ctForeign.dat")
cw <- read.ices("data/cw.dat")
dw <- read.ices("data/dw.dat")
lf <- read.ices("data/lf.dat")
lw <- read.ices("data/lw.dat")
mo <- read.ices("data/mo.dat")
nm <- read.ices("data/nm.dat")
nm[]<- 0.27
pf <- read.ices("data/pf.dat")
pm <- read.ices("data/pm.dat")
sw <- read.ices("data/sw.dat")
sw0 <- read.ices("data/sw0.dat")
surveys <- read.ices("data/survey.dat")
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

# plots
# x <- fit
# name <- 'fitBase'
# source('Rscripts/plot_fit.R')


# Jitter analyses: results sensitive to initial values?
myjit <- jit(dat,conf,par,nojit=100,parallell = FALSE)  
write.table(myjit,"Rdata/fit/fit_jitter.txt")


