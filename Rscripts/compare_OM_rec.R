#################################################################################################################
#*** Mackerel MSE
#*** Canadian mackerel (DFO, 2018)
#*** COMPARE Different M in operating models
#################################################################################################################

#################################################################################################################
########### READ IN DATA ########################################################################################
#################################################################################################################

load(file='Rdata/input/dat.Rdata')
load(file='Rdata/input/conf.Rdata')
load(file='Rdata/input/par.Rdata')

RECrange <- c(0,2) #rw and BH
n <- length(RECrange)

myconf <- replicate(n,conf,simplify=FALSE)

for(i in 1:n){
    myconf[[i]]$stockRecruitmentModelCode <- RECrange[i]
}

RECtypes <- RECrange

#################################################################################################################
########### fit model ###########################################################################################
#################################################################################################################

RECruns <- lapply(myconf,function(x){ccam.fit(dat,x,par,silent=TRUE) })

class(RECruns) <- 'ccamset'
names(RECruns) <- RECtypes

save(RECruns,file='Rdata/fit_compare/REC.Rdata')
#load(file='Rdata/fit_compare/REC.Rdata')

.wd <- 'img/fit_compare/rec/'
dir.create(.wd, showWarnings = FALSE)

savepng(ssbplot(RECruns,ci=FALSE),.wd,"SSB",c(17,10))
savepng(catchplot(RECruns,ci=FALSE),.wd,"catch",c(17,10))
savepng(recplot(RECruns,ci=FALSE),.wd,"recruitment",c(17,10))
savepng(fitplot(RECruns,type='AIC'),.wd,"AIC",c(14,6))
savepng(fitplot(RECruns,type='nll'),.wd,"nll",c(14,6))

