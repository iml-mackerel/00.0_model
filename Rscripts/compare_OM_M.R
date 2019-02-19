#################################################################################################################
#*** Mackerel MSE
#*** Canadian mackerel (DFO, 2018)
#*** COMPARE Different M in operating models
#################################################################################################################

#################################################################################################################
########### READ IN DATA ########################################################################################
#################################################################################################################

nm.alv <- read.ices("data/nm_Alverson.dat")
nm.zhang <- read.ices("data/nm_Zhang.dat")
nm.gisl <- read.ices("data/nm_Gislason.dat")
nm.gund <- read.ices("data/nm_Gunderson.dat")
nm.DFO2017 <- read.ices("data/nm_DFO2017.dat")

load(file='Rdata/input/dat.Rdata')
load(file='Rdata/input/conf.Rdata')
load(file='Rdata/input/par.Rdata')


Mrange <- seq(0.15,0.3,0.01)
n <- length(Mrange)
ntot <- n +5

mydats <- replicate(ntot,dat,simplify=FALSE)

for(i in 1:n){
    mydats[[i]]$natMor[] <- Mrange[i]
}
mydats[[n+1]]$natMor[] <- nm.gund
mydats[[n+2]]$natMor[] <- nm.alv
mydats[[n+3]]$natMor[] <- nm.zhang
mydats[[n+4]]$natMor[] <- nm.gisl
mydats[[n+5]]$natMor[] <- nm.DFO2017

Mtypes <- c(Mrange,'Gunderson','Alverson','Zhang','Gislason','DFO2017')

#################################################################################################################
########### fit model ###########################################################################################
#################################################################################################################

Mruns <- lapply(mydats,function(x){ccam.fit(x,conf,par,silent=TRUE) })

class(Mruns) <- 'ccamset'
names(Mruns) <- Mtypes

save(Mruns,file='Rdata/fit_compare/M.Rdata')
#load(file='Rdata/fit_compare/M.Rdata')

.wd <- 'img/fit_compare/M/'
dir.create(.wd, showWarnings = FALSE)

savepng(ssbplot(Mruns,ci=FALSE),.wd,"SSB",c(21,13))
savepng(catchplot(Mruns,ci=FALSE),.wd,"catch",c(21,13))
savepng(recplot(Mruns,ci=FALSE),.wd,"recruitment",c(21,13))
savepng(fitplot(Mruns,type='AIC',n=FALSE),.wd,"AIC",c(16,8))
savepng(fitplot(Mruns,type='nll',n=FALSE),.wd,"nll",c(16,8))

