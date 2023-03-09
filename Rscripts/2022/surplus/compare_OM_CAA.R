#################################################################################################################
#*** Mackerel assessment
#*** Canadian mackerel (DFO, 2023)
#*** what if 2022 CAA removed, what if alternative CAA
#################################################################################################################

source('Rscripts/2022/surplus/read_data.R')
.wd <- "img/2022/fit_compare/CAA/"
type  <- 'png'

# run with different options of cn in final year (or first year data removed) #################################################################
cn_modif <- read.ices(paste0(dir,'cn.dat')) 
cn_default <- read.ices(paste0(dir,'/sensitivity/cn_default.dat'))
cn_missing <- cn_modif[-nrow(cn_modif),]

dats <- lapply(list(cn_modif,cn_default,cn_missing),function(x){
    setup.ccam.data(surveys=survey,
                    residual.fleet=x[-1,],
                    total.catch=ctwusa[-1,],
                    prop.mature=mo[-1,],
                    stock.mean.weight=sw[-1,],
                    stock.start.weight=sw0[-1,],
                    catch.mean.weight=cw[-1,],
                    prop.f=pf[-1,],
                    prop.m=pm[-1,],
                    natural.mortality=nm[-1,],
                    prop.fem=pfem[-1,],
                    fec=fec[-1,])
})

fits.caa <- lapply(dats,function(x)ccam.fit(x,conf,par,debug=T))
names(fits.caa) <- c('modified','default','excluded')
class(fits.caa) <- "ccamset"

save(fits.caa, file=paste0('Rdata/',year,'/fit_compare/fits.caa.Rdata'))

# compare all three

saveplot(plot(fits.caa,years=2010:2022,ci=FALSE),name='ssb_noci',dim=c(12,10),wd=.wd,type=type)


# compare just two
sub <- fits.caa[c('modified','excluded')]
class(sub) <- "ccamset"
plot(sub,years=2005:2022)

resplot(fits.caa$modified,fleets = 2,type=4)
resplot(fits.caa$excluded,fleets = 2,type=4)

n <- ntable(fits.caa)
m <- melt(n,id=c('year','fit'))
p <- ggplot(m[m$year %in% 2020:2022,],aes(x=variable,y=value,fill=fit))+
    geom_bar(stat='identity',position='dodge')+
    facet_wrap(year~.,ncol=1)+
    labs(y="Numbers",x="age",fill='Fit')+
    scale_fill_viridis_d()

saveplot(p,name='N',dim=c(12,10),wd=.wd,type=type)

saveplot(plot(sub,years=2010:2022,col=c('darkblue','darkred')),name='ssb_ci',dim=c(12,10),wd=.wd,type=type)



# shorten model #################################################################

# for loop because easier if no convergence/error
fit <- ccam.fit(dat,conf,defpar(dat,conf),debug=T)
save(fit, file=paste0('Rdata/',year,'/fit_compare/fit.start1969.Rdata'))
for(i in 1970:1975){
    newdat <- CCAM::reduce(dat, year = 1969:i,conf=conf)
    fit <- ccam.fit(newdat,attr(newdat,'conf'),defpar(newdat,attr(newdat,'conf')),debug=T)
    save(fit, file=paste0('Rdata/',year,'/fit_compare/fit.start',i+1,'.Rdata'))
}

s <- list.files(paste0('Rdata/',year,'/fit_compare/'),'start',full.names = TRUE)
fits.start <- lapply(s,function(x)get(load(x)))
names(fits.start) <- c(1969:1976)
class(fits.start) <- "ccamset"


saveplot(plot(fits.start,ci=FALSE,col=viridis(length(fits.start))),name='all_start_noci',dim=c(12,10),wd=.wd,type=type)

f <- fits.start[1:3]
class(f) <- 'ccamset'
plot(f,ci=FALSE)

saveplot(parplot(fits.start,col = viridis(length(fits.start))),name='par',dim=c(12,10),wd=.wd,type=type)


