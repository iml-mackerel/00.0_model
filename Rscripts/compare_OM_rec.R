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

for(i in RECrange){
    conf$stockRecruitmentModelCode <- i
    par <- defpar(dat,conf)
    fit <- ccam.fit(dat,conf,par,silent=TRUE)           
    save(fit, file=paste0('Rdata/fit_compare/rec',i,'.Rdata'))
}

#################################################################################################################
########### fit model ###########################################################################################
#################################################################################################################

filenames <- dir('Rdata/fit_compare', pattern = "rec")
files <- paste0('Rdata/fit_compare','/',filenames)
RECruns <- lapply(files, function(x) {print(x);get(load(x))})
class(RECruns) <- 'ccamset'
names(RECruns) <- c('RW','BH')

save(RECruns,file='Rdata/fit_compare/REC.Rdata')
#load(file='Rdata/fit_compare/REC.Rdata')

.wd <- 'img/fit_compare/rec/'
dir.create(.wd, showWarnings = FALSE)

savepng(ssbplot(RECruns,ci=FALSE),.wd,"SSB",c(17,10))
savepng(catchplot(RECruns,ci=FALSE),.wd,"catch",c(17,10))
savepng(recplot(RECruns,ci=FALSE),.wd,"recruitment",c(17,10))
savepng(fitplot(RECruns,type='AIC'),.wd,"AIC",c(14,6))
savepng(fitplot(RECruns,type='nll'),.wd,"nll",c(14,6))
savepng(srplot(RECruns,curve=TRUE),.wd,"sr",c(18,12))
savepng(recplot(RECruns,ci=FALSE,trans = log,linesize=1),.wd,"recruitment_log",c(10,4))



