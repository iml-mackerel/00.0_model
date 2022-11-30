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

test <- runwithout(fit,year=1968,conf=fit$conf)
data <- reduce(fit$data, year = 1968,conf=fit$conf)

# runwithout
data <- reduce(fit$data, year = year, fleet = 1:3, conf = fit$conf)
conf <- attr(data, "conf")
attr(data, "conf") <- NULL
par <- defpar(data, conf)
ret <- ccam.fit(data, conf, par, rm.unidentified = TRUE)

return(ret)


recplot(c(fit,ret))

.wd <- paste0('img/',year,'/fit_compare/timeperiod/')
dir.create(.wd, showWarnings = FALSE,recursive = TRUE)

savepng(ssbplot(c(fit,ret)),.wd,"SSB",c(21,13))
savepng(recplot(c(fit,ret)),.wd,"rec",c(21,13))





