#################################################################################################################
#*** Mackerel assessment
#*** Canadian mackerel (DFO, 2022)
#*** COMPARE different model lengths
#################################################################################################################

year <- 2022

#################################################################################################################
########### READ IN DATA AND FIT ########################################################################################
#################################################################################################################

load(file=paste0("Rdata/",year,"/fit.Rdata"))

# without the first i years
for(i in 0:7){
    y <- 1968+i
    fit.period <- runwithout(fit,year=1968:y) 
    save(fit.period, file=paste0('Rdata/',year,'/fit_compare/fit.period.start',y+1,'.Rdata'))
}
save(fit, file=paste0('Rdata/',year,'/fit_compare/fit.period.start1968.Rdata'))

f <- list.files(paste0('Rdata/',year,'/fit_compare/'),'fit.period.start',full.names = T)
startruns <- lapply(f, function(x) {print(x);get(load(x))})

names(startruns) <- sapply(regmatches(f, regexec("period.\\s*(.*?)\\s*.Rdata", f)),'[[',2)
class(startruns) <- 'ccamset'

.wd <- paste0('img/',year,'/fit_compare/timeperiod/')
savepng(ssbplot(startruns,ci=FALSE),.wd,"SSB",c(21,13))
savepng(recplot(startruns,ci=FALSE),.wd,"rec",c(21,13))
savepng(parplot(startruns),.wd,"par",c(21,13))




