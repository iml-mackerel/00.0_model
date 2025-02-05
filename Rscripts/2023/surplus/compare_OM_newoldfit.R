#################################################################################################################
#*** Mackerel assessment
#*** Canadian mackerel (DFO, 2023)
#*** Compare new to old fit
#################################################################################################################

.wd <- "img/2022/fit_compare/oldnew/"
type  <- 'png'

# load fits
load("Rdata/2022/fit.Rdata")
fitnew <- fit

load("Rdata/2020/fit.Rdata")
fitold <- fit

fits <- c(fitold,fitnew)
names(fits) <- c('2020','2022')
class(fits) <-'ccamset'

cols <- c('darkblue','darkred')

saveplot(plot(fits,col=cols),name='all',dim=c(12,16),wd=.wd,type=type)
saveplot(plot(fits,col=cols,years=2000:2022),name='all_zoom',dim=c(12,16),wd=.wd,type=type)
saveplot(recplot(fits,col=cols),name='rec',dim=c(12,10),wd=.wd,type=type)


