#################################################################################################################
#*** Mackerel assessment
#*** Canadian mackerel (DFO, 2023)
#*** include vs exclude certain egg survey years
#################################################################################################################


source('Rscripts/2022/surplus/read_data.R')
.wd <- "img/2022/fit_compare/maturity/"
dir.create(.wd, showWarnings = FALSE,recursive = T)
type  <- 'png'

# run with different options of cn in final year (or first year data removed) #################################################################
mo2 <- read.ices(paste0(dir,'/sensitivity/mo_smooth0.2.dat'))
mo3 <- read.ices(paste0(dir,'/sensitivity/mo_smooth0.9.dat'))

p1 <- prettymatplot(mo,ylab='Maturity',xlab='Year')+theme(legend.position = 'none')+labs(title = 'default (smooth 0.5')
p2 <- prettymatplot(mo2,ylab='Maturity',xlab='Year')+theme(legend.position = 'none')+labs(title='sensitivity 1 (smooth 0.2)')
p3 <- prettymatplot(mo3,ylab='Maturity',xlab='Year')+theme(legend.position = 'none')+labs(title='sensitivity 2 (smooth 0.9)')
saveplot(grid.arrange(p1,p2,p3),name='mo_line',dim=c(12,18),wd=.wd,type=type)  

dats <- lapply(list(mo,mo2,mo3),function(x){
    setup.ccam.data(surveys=survey,
                    residual.fleet=cn[-1,],
                    total.catch=ctwusa[-1,],
                    prop.mature=x[-1,],
                    stock.mean.weight=sw[-1,],
                    stock.start.weight=sw0[-1,],
                    catch.mean.weight=cw[-1,],
                    prop.f=pf[-1,],
                    prop.m=pm[-1,],
                    natural.mortality=nm[-1,],
                    prop.fem=pfem[-1,],
                    fec=fec[-1,])
})

nam <- c('default','sensitivity1','sensitivity2')

for(i in 1:length(nam)){  # if 2006, 2017 and 2019 are excluded, no convergence.
    fit <- ccam.fit(dats[[i]],conf,par,debug=T)
    save(fit, file=paste0('Rdata/',year,'/fit_compare/fit.mo.',nam[i],'.Rdata'))
}

s <- list.files(paste0('Rdata/',year,'/fit_compare/'),'mo',full.names = TRUE)
fits.mo <- lapply(s,function(x)get(load(x)))
names(fits.mo) <- gsub(".Rdata","",gsub(".*mo.","",s))
class(fits.mo) <- "ccamset"

# compare basics
saveplot(plot(fits.mo,ci=FALSE),name='ssb_noci',dim=c(12,10),wd=.wd,type=type)
saveplot(plot(fits.mo,ci=FALSE,years=2010:2022),name='ssb_noci_zoom',dim=c(12,10),wd=.wd,type=type)


