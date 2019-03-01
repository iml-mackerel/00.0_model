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

year <- 2018
dir <- paste0('data/',year,'/')

ct <- read.ices(paste0(dir,'ct.dat'))
ctUSA <- read.ices(paste0(dir,'ctUSA.dat'))

clow <- c(0,0.25,0.50,0.75)
chigh <- c(0.25,0.50,0.75,1)

datalist <- lapply(1:4,function(x){
    ct1 <- ct
    ct1[,1] <- ct[,1] + ctUSA[-c(1:8),1]*clow[x]
    ct1[,2] <- ct[,2] + ctUSA[-c(1:8),1]*chigh[x]
    if(!all(ct1[,2]>ct1[,1])) print('trouble')
    log(ct1)
})

n <- length(datalist)

mydats <- replicate(n,dat,simplify=FALSE)
idx <- dat$idx1[1,]+1

for(i in 1:n){
    mydats[[i]]$logobs[idx,] <- datalist[[i]]
}

Ctypes <- paste(clow*100,chigh*100,sep='-')

#################################################################################################################
########### fit model ###########################################################################################
#################################################################################################################

Cruns <- lapply(mydats,function(x){ccam.fit(x,conf,par,silent=TRUE) })

class(Cruns) <- 'ccamset'
names(Cruns) <- Ctypes

save(Cruns,file='Rdata/fit_compare/C.Rdata')
#load(file='Rdata/fit_compare/C.Rdata')

.wd <- 'img/fit_compare/C/'
dir.create(.wd, showWarnings = FALSE)

savepng(ssbplot(Cruns,ci=FALSE),.wd,"SSB",c(17,10))
savepng(catchplot(Cruns,ci=FALSE),.wd,"catch",c(17,10))
savepng(recplot(Cruns,ci=FALSE),.wd,"recruitment",c(17,10))
savepng(plot(Cruns,ci=FALSE,linesize=1),.wd,"all",c(14,15))
savepng(fitplot(Cruns,type='AIC',n=FALSE),.wd,"AIC",c(16,8))
savepng(fitplot(Cruns,type='nll',n=FALSE),.wd,"nll",c(16,8))

catch <- catchtable(Cruns)
catch$low <-NA
catch$high <-NA
e <-lapply(names(Cruns),function(x){
    d <- Cruns[[x]]$data
    ix <- d$idx1[1,]+1
    catch[catch$fit==x,c('low','high')]<<-exp(d$logobs[ix,])
})
catch <- melt(catch[,-c(2,3)],id=c('year','fit'))
pc<-ggplot(catch,aes(x=year,y=value,col=variable))+geom_line(aes(size=variable))+
    facet_wrap(~fit)+
    scale_color_manual(values=c('black','darkgrey','darkgrey'))+
    labs(col='',linetype='',y='Catch (t)',x='Year')+
    scale_size_manual(values=c(1,0.3,0.3))+
    theme(legend.position = 'none')


savepng(pc,.wd,"catch_limits",c(14,10))

partable(Cruns)[seq(1,37,length.out = 4),]
