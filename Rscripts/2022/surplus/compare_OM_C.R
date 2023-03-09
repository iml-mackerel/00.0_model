#################################################################################################################
#*** Mackerel MSE
#*** Canadian mackerel (DFO, 2018)
#*** COMPARE Different M in operating models
#################################################################################################################

#################################################################################################################
########### DIffferent fractions US ########################################################################################
#################################################################################################################

# compared under M = 0.2

year <- 2022
dir <- paste0('data/',year,'/')

load(file=paste0('Rdata/',year,'/input/dat.Rdata'))
load(file=paste0('Rdata/',year,'/input/conf.Rdata'))
load(file=paste0('Rdata/',year,'/input/par.Rdata'))


ct <- read.ices(paste0(dir,'ct.dat'))
ctunac <- read.ices(paste0(dir,'ctUnaccounted.dat'))
ctUSA <- read.ices(paste0(dir,'ctUSA.dat'))

clow <- c(0.25,0.50,0.2)
chigh <- c(0.50,0.75,0.8)

ct[,2] <- ct[,1] + ctunac[,1] 

datalist <- lapply(1:length(clow),function(x){
    ct1 <- ct
    ct1[,1] <- ct[,1]*1.10 + ctUSA[,1]*clow[x]
    ct1[,2] <- ct[,2] + ctUSA[,1]*chigh[x]
    if(!all(ct1[,2]>ct1[,1])) print('trouble')
    log(ct1)
})

n <- length(datalist)

mydats <- replicate(n,dat,simplify=FALSE)
idx <- dat$aux[,2]==1  # first fleet = total catch

for(i in 1:n){
    mydats[[i]]$logobs[idx,] <- datalist[[i]][-1,]  #no 1968
}

Ctypes <- paste(clow*100,chigh*100,sep='-')

#################################################################################################################
########### fit model ###########################################################################################
#################################################################################################################

Cruns <- lapply(mydats,function(x){ccam.fit(x,conf,par,silent=TRUE) })

class(Cruns) <- 'ccamset'
names(Cruns) <- Ctypes

save(Cruns,file='Rdata/2022/fit_compare/fitsC.Rdata')
#load(file='Rdata/2022/fit_compare/fitsC.Rdata')

.wd <- 'img/2022/fit_compare/C/'
dir.create(.wd, showWarnings = FALSE)

savepng(plot(Cruns,ci=FALSE,year=2010:2022),.wd,"all",c(12,18))
savepng(ssbplot(Cruns,ci=FALSE),.wd,"SSB",c(17,10))
savepng(ssbplot(Cruns,ci=FALSE,year=2010:2022),.wd,"SSB_zoom",c(17,10))
savepng(catchplot(Cruns,ci=FALSE),.wd,"catch",c(17,10))
savepng(recplot(Cruns,ci=FALSE),.wd,"recruitment",c(17,10))
savepng(plot(Cruns,ci=FALSE),.wd,"all",c(14,15))
savepng(fitplot(Cruns,type='AIC',n=FALSE),.wd,"AIC",c(16,8))
savepng(fitplot(Cruns,type='nll',n=FALSE),.wd,"nll",c(16,8))

ldply(Cruns,AIC)

catch <- catchtable(Cruns)
catch$low <-NA
catch$high <-NA
e <-lapply(names(Cruns),function(x){
    d <- Cruns[[x]]$data
    ix <- d$idx1[1,]+1
    catch[catch$fit==x,c('low','high')]<<-exp(d$logobs[ix,])
})
catch <- melt(catch[,-c(2,3)],id=c('year','fit'))
pc<-ggplot(catch,aes(x=year,y=value,col=variable))+geom_line()+
    facet_wrap(~fit,ncol=1)+
    scale_color_manual(values=c('black','darkgrey','darkgrey'))+
    labs(col='',linetype='',y='Catch (t)',x='Year')+
    scale_size_manual(values=c(1,0.3,0.3))+
    theme(legend.position = 'none')


savepng(pc,.wd,"catch_limits",c(14,16))


ss <- ssb0table(Cruns)
ss[ss$year %in% c(2020:2022),]


resplot(Cruns$`25-50`,fleets = 2,type=3)
resplot(Cruns$`20-80`,fleets = 2,type=3)
resplot(Cruns$`25-50`,fleets = 3,type=1)
resplot(Cruns$`20-80`,fleets = 3,type=1)
