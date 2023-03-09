#################################################################################################################
#*** Mackerel assessment
#*** Canadian mackerel (DFO, 2023)
#*** include vs exclude certain egg survey years
#################################################################################################################


source('Rscripts/2022/surplus/read_data.R')
.wd <- "img/2022/fit_compare/fecundity/"
dir.create(.wd, showWarnings = FALSE,recursive = T)
type  <- 'png'

# run with different options of cn in final year (or first year data removed) #################################################################
fec2 <- read.ices(paste0(dir,'/sensitivity/fec_sens_cv4shrink0.5.dat'))

p1 <- prettymatplot(fec,ylab='Fecundity',xlab='Year')+theme(legend.position = 'none')+labs(title = 'default')
p2 <- prettymatplot(fec2,ylab='Fecundity',xlab='Year')+theme(legend.position = 'none')+labs(title='sensitivity')
saveplot(grid.arrange(p1,p2),name='fec_line',dim=c(12,18),wd=.wd,type=type)  

dats <- lapply(list(fec,fec2),function(x){
    setup.ccam.data(surveys=survey,
                    residual.fleet=cn[-1,],
                    total.catch=ctwusa[-1,],
                    prop.mature=mo[-1,],
                    stock.mean.weight=sw[-1,],
                    stock.start.weight=sw0[-1,],
                    catch.mean.weight=cw[-1,],
                    prop.f=pf[-1,],
                    prop.m=pm[-1,],
                    natural.mortality=nm[-1,],
                    prop.fem=pfem[-1,],
                    fec=x[-1,])
})

nam <- c('default','sensitivity')

for(i in 1:2){  # if 2006, 2017 and 2019 are excluded, no convergence.
    fit <- ccam.fit(dats[[i]],conf,par,debug=T)
    save(fit, file=paste0('Rdata/',year,'/fit_compare/fit.fec.',nam[i],'.Rdata'))
}

s <- list.files(paste0('Rdata/',year,'/fit_compare/'),'fec',full.names = TRUE)
fits.fec <- lapply(s,function(x)get(load(x)))
names(fits.fec) <- gsub(".Rdata","",gsub(".*fec.","",s))
class(fits.fec) <- "ccamset"

# compare basics
saveplot(plot(fits.fec,ci=FALSE),name='ssb_noci',dim=c(12,10),wd=.wd,type=type)
saveplot(plot(fits.fec,ci=FALSE,years=2010:2022),name='ssb_noci_short',dim=c(12,10),wd=.wd,type=type)


