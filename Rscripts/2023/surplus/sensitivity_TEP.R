#################################################################################################################
#*** Weight-of-Evidence approach
#*** Sensitivity index / assessment
#################################################################################################################

nsims <- 200

##' simulate random walk between 0 and 1
##' @param t number of time steps to simulate
##' @param n number of simulations
##' @param start where to initiate the simulation (t1)
##' @param diff average difference between time steps
##' @param sd standard deviation
##' @param seed seed
rw01 <- function(t,n,start=0.01,diff=0.15,sd=0.2,start.sd=sd/8,seed=NULL){
    sapply(1:n, function(x){
        if(!is.null(seed)) set.seed(x+seed)
        t1 <- max(0.0001,rnorm(1,start,start.sd))                    # start values
        s <- invlogit(logit(t1)+cumsum(rnorm(t-1,diff,sd)))          # random walk but with logit for constraining
        return(c(t1,s))
    })
} 

##' run multiple models
##' @param x list with elements containing fit parameters
##' @param dir directory to save forecasts
##' @param parallel logical
##' @return does not return an object. Save all individual runs as Rdata
##' @import parallel
##' @rdname multifit
##' @export
multifit <- function(x,dir,parallel=FALSE,ncores=NULL){
    if(parallel){
        library(parallel)
        if(is.null(ncores)) ncores <- detectCores()
        cl <- makeCluster(ncores) #set up nodes
        clusterEvalQ(cl, {library(CCAM)}) #load the package to each node
        clusterExport(cl, c('dir','x'), envir=environment())
        empty <- parLapply(cl, 1:length(x), function(y){ 
            run <- do.call(ccam.fit, x[[y]])
            save(run,file=paste0(dir,names(x)[y],'.Rdata'))
        })
        stopCluster(cl) 
    }else{
        empty <- lapply(1:length(x),function(y){
            run <- do.call(ccam.fit, x[[y]])
            save(run,file=paste0(dir,names(x)[y],'.Rdata'))
        }) 
    }
}


#################################################################################################################
########### BASE TEP ############################################################################################
#################################################################################################################

load(file='Rdata/2022/fit.Rdata')

# data: observations and predictions
d <- fit$data                                      # plotobs(d,fleets = 3)
id <- which(d$aux[,2]==3)
tep <- data.frame(year=d$aux[id,1],
                  tep=exp(d$logobs[id,1]),
                  y=d$aux[id,1]-min(d$aux[id,1])+1)
idx<-fit$data$aux[,"fleet"]%in%3
tep$predict <- exp(fit$rep$predObs[idx])

# predicted sd
sds <- exp(fit$obj$par[which(names(fit$obj$par)=='logSdLogObs')])
keysd <- fit$conf$keyVarObs[3,]
keysd <- keysd[keysd>-1]
sd <- sds[keysd+1]

# plot
tepscale <- 10^15
pbase <- ggplot()+
    geom_line(data=tep,aes(x=y,y=tep/tepscale),size=2)+
    geom_line(data=tep,aes(x=y,y=predict/tepscale),size=2,col='red')+
    labs(y='TEP (*10^15)',x='Year')+
    scale_y_continuous(expand = c(0,0),limits=c(0,max(tep$tep/tepscale)*1.6))

#################################################################################################################
########### ALTERNATIVE TEP #####################################################################################
#################################################################################################################

y <- range(tep$year)
ny <- y[2]-y[1]+1
mis <- which(!y[1]:y[2] %in% tep$year)


sim <- rw01(ny,nsims,seed=0,sd=0.1,start.sd = 0.03,diff=0.2)
dimnames(sim)=list(ny=1:ny,nsim=1:nsims)
sim[mis,] <- NA
matplot(sim*100,type='l')

# total TEP
simtep <- sim
simtep[-mis,] <-  apply(sim[-mis,],2,function(x){x*tep[,4]+tep[,4]})  

# new TEP
simtepnl <- sim
simtepnl[-mis,] <- apply(sim[-mis,],2,function(x){x*tep[,4]}) 

# new TEP with observation error
mu <- mean(log(tep$predict))
simnew <- simtepnl
set.seed(0)
simnew[-mis,] <- apply(simnew[-mis,],2,function(x) exp(rnorm(length(x),log(x),sd/(mu/mean(log(x))))))



########## plots #################################################################################################
source('Rscripts/plot_TEP.R')

#################################################################################################################
########### MODEL SENSITIVITY ###################################################################################
#################################################################################################################

########## runs #################################################################################################
conf <- fit$conf
par <- defpar(dat,conf)
inputs <- lapply(1:nsims,function(x){
    dat <- d
    dat$logobs[id,1] <- log(exp(dat$logobs[id,1]) + na.omit(simnew[,x]))
    list(data=dat,conf=conf,par=par,silent=FALSE,paracheck = FALSE)
})
names(inputs) <- 1:nsims

sub <- inputs[c(21:200)]
multifit(sub,dir = 'Rdata/2022/TEP/',parallel=FALSE)

########## output #################################################################################################
sens <- lapply(list.files('Rdata/2022/TEP/',full.names = T),function(x) get(load(x)))
out <- ldply(1:(length(sens)+1),function(x){
    if(x==1) m <- fit else m <- sens[[x-1]]
    d <- data.frame(
        class=ifelse(x==1,"base","sim"),
        model=x,
        AIC=AIC(m),
        ssb=ssbtable(m),
        ssbend=tail(ssbtable(m)[,1],1),
        LRP=ypr(m)$LRP,
        ssbLRP=tail(ssbtable(m)[,1],1)/ypr(m)$LRP
    )
    tep <- data.frame(ssb.year=m$data$aux[id,1],
                      tep=exp(m$data$logobs[id,1]),
                      y=m$data$aux[id,1]-min(m$data$aux[id,1])+1)
    d <- merge(d,tep,all.x=T)
})

p <- ggplot(melt(out[out$class=='sim' & out$ssb.year==2022,c('ssbLRP','class')],id=c('class')),aes(x=class,y=value))+
    geom_boxplot(fill='grey')+
    geom_hline(yintercept=1)+
    geom_hline(aes(yintercept=out[out$class=='base'& out$ssb.year==2022,'ssbLRP']),col='green',size=2)+
    ylab('SSB/LRP')

saveplot(p,'fits_ssblrp','img/2022/TEP/',c(4,8))

p <- ggplot(out[out$class=='sim',],aes(x=ssb.year,y=ssb.Estimate,group=model))+
    geom_line(col='darkgrey',size=0.2)+
    geom_line(data=out[out$class=='base',],col='red',size=1)+
    labs(y='SSB (kg)',x='Year')

saveplot(p,'fits_ssb','img/2022/TEP/',c(12,8))

p <- ggplot(out[out$class=='sim' & !is.na(out$tep),],aes(x=ssb.year,y=tep,group=model))+
    geom_line(col='darkgrey',size=0.2)+
    geom_line(data=out[out$class=='base'&!is.na(out$tep),],col='red')

saveplot(p,'tep_new','img/2022/TEP/',c(12,8))

high <- out[out$ssbLRP>0.9 & !is.na(out$tep),]
p <- ggplot(high,aes(x=ssb.year,y=tep,group=model))+
    geom_line(col='darkgrey')+
    geom_line(data=out[out$class=='base'&!is.na(out$tep),],col='red')+
    scale_y_continuous(expand=c(0,0),limits = c(0,max(out$tep,na.rm=T)))+
    labs(x='Year',y='TEP')

saveplot(p,'tep_new_highstate','img/2022/TEP/',c(12,8))

lrpr <- out[out$class=='base'& out$ssb.year==2022,'ssbLRP']
low <- out[out$ssbLRP<lrpr & !is.na(out$tep),]
p <- ggplot(low,aes(x=ssb.year,y=tep,group=model))+
    geom_line(col='darkgrey')+
    geom_line(data=out[out$class=='base'&!is.na(out$tep),],col='red')+
    scale_y_continuous(expand=c(0,0),limits = c(0,max(out$tep,na.rm=T)))+
    labs(x='Year',y='TEP')   
saveplot(p,'tep_new_lowstate','img/2022/TEP/',c(12,8))
