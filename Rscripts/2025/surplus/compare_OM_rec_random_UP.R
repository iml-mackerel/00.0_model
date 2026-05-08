#################################################################################################################
#*** Weight-of-Evidence approach for rec fishery
#*** Sensitivity index / assessment
#################################################################################################################
nsims <- 10

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

load(file=paste0('Rdata/',my.year,'/fit.Rdata'))
   # quick check
d <- fit$data  
id <- which(d$aux[,2]==1)
idx<-fit$data$aux[,"fleet"]%in%1
max.catches<- fit$data$logobs[idx,"aux2"] # maximum . minimum would be aux1
total_catches<- exp(max.catches)


################################################################################################################
########### ALTERNATIVE TEP #####################################################################################
#################################################################################################################
ny<- length(total_catches)
runif01 <- function(t,n,start=0,end=1,seed=NULL){
    sapply(1:n, function(x){
        if(!is.null(seed)) set.seed(x+seed)
        runif(n=ny, min=0, max=1)                # start values
    })
}
sim <- runif01(ny,nsims,seed=0)
simrec <-  apply(sim,2,function(x){x*680+total_catches})  
dimnames(simrec)=list(ny=1:ny,nsim=1:nsims)

 tc_default<- data.frame(ny=1:length(total_catches), total_catches=total_catches)

 ggplot()+
    geom_line(data=na.omit(melt(simrec)),aes(x=ny,y=value,col=factor(nsim)),size=0.1)+
    scale_color_viridis_d()+
     geom_line(data=tc_default, aes(x=ny, y=total_catches), linewidth=1)+
    theme(legend.position = 'none')
#################################################################################################################
########### MODEL SENSITIVITY ###################################################################################
#################################################################################################################

########## runs #################################################################################################

inputs <- lapply(1:nsims,function(x){
    dat <- d
    dat$logobs[idx,"aux2"]<- log(simrec[,x])
    conf <- fit$conf
    par <- defpar(d,conf)
    list(data=dat,conf=conf,par=par,silent=FALSE,paracheck = FALSE)
})
names(inputs) <- 1:nsims

multifit(inputs,dir = paste0('Rdata/',my.year,'/sensitivity/rec/'),parallel=FALSE)


#############Check results#########
sens <- lapply(list.files(paste0('Rdata/',my.year,'/sensitivity/rec/'),full.names = T),function(x) get(load(x)))
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
    
    
    sens.results <- data.frame(ssb.year=m$data$aux[id,1],
                      catches=exp(m$data$logobs[id,2])
                     )
    d <- merge(d,sens.results,all.x=T)
})

p <- ggplot(melt(out[out$class=='sim' & out$ssb.year==my.year,c('ssbLRP','class')],id=c('class')),aes(x=class,y=value))+
    geom_boxplot(fill='grey')+
    geom_hline(yintercept=1)+
    geom_hline(aes(yintercept=out[out$class=='base'& out$ssb.year==my.year,'ssbLRP']),col='green',size=2)+
    ylab('SSB/LRP') +xlab("") +coord_flip() +theme(axis.text.y  = element_blank())

saveplot(p,'fits_ssblrp',paste0('img/',my.year,'/sensitivity/rec/'),c(8,4))

p <- ggplot(out[out$class=='sim',],aes(x=ssb.year,y=ssb.Estimate,group=model))+
    geom_line(col='darkgrey',size=0.2)+
    geom_line(data=out[out$class=='base',],col='red',size=1)+
    labs(y='SSB (t)',x='Année | Year') +scale_x_continuous(breaks=seq(1970, my.year, 5))

saveplot(p,'fits_ssb',paste0('img/',my.year,'/sensitivity/rec/'),c(12,8))

p <- ggplot(out[out$class=='sim' & !is.na(out$catches),],aes(x=ssb.year,y=catches,group=model))+
    geom_line(col='darkgrey',size=0.2)+
    geom_line(data=out[out$class=='base'&!is.na(out$catches),],col='red')+
    labs(y='catches',x='Année | Year') +scale_x_continuous(breaks=seq(1970, my.year, 5))

saveplot(p,'catch_new',paste0('img/',my.year,'/sensitivity/rec/'),c(12,8))
