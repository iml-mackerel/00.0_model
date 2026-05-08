#################################################################################################################
#*** Mackerel assessment projections
#*** Canadian mackerel (DFO, 2022)
#*** based on CCAM package
#################################################################################################################
#patch
cumsum.bounded <- function(x, lower = 0, upper = 500) {
    bsum <- function(x, y) min(upper, max(lower, x+y))
    if (length(x) > 1) Reduce(bsum, x, acc = TRUE) else x
}
library(stringr)
ny=2
nosim=2000



#--------------------- projections ------------------------------------------
# basic projection parameters
load(paste0('Rdata/',my.year-2,'/fit.Rdata'))

projBH <- list(fit=fit,
               nosim=nosim,
               OMlabel='OMassess',
               ave.years=tail(fit$data$years,20),
               rec.years=1969:my.year-5-2,
               rec.meth=1, #Beverton-Holt
               UL.years=tail(fit$data$years,25),
               deadzone=1000,
               Flim=2.5)

projM <- list(fit=fit,
              nosim=nosim,
              OMlabel='OMassess',
              ave.years=tail(fit$data$years,20),
              rec.years=2011:my.year-2,
              rec.meth=2, #mean with AC
              UL.years=tail(fit$data$years,25),
              deadzone=1000,
              Flim=2.5)

attr(projM$rec.meth,'AC') = 0.9  
attr(projM$rec.meth,'sd.option') = 'ci'
rec <- rectable(fit)
rho <- acf(rec[-1,1])$acf[2,1,1]


# --------------------- base MPs ----------------------------------------------
# under what harvest control rule/TAC to forecast?
nMP=4

MP1 <- list(MPlabel='MP1',
            IE=NULL,
            capLower=0,
            TAC.base=0)

copy(x=MP1,n=nMP,name=c('MP'))

avail('MP')
MP1$catchval <- rep(0,ny)
MP2$catchval <- rep(200,ny)
MP3$catchval <- rep(400,ny)
MP4$catchval <- rep(600,ny)
#******************************************************************************
#************* Create all forecasting scenarios *******************************
#******************************************************************************
# functions that generate matrices of size x (n simulations) on y (number of years)
# see avail('IE') for inspiration
#source("C:/Users/VANBE/Desktop/post-doc/DATA/CCAM/R/ie.R")
#source("C:/Users/VANBE/Desktop/post-doc/DATA/CCAM/R/forecast.R")
IEindepcan <- function(x,y,seed=NULL){
    if(!is.null(seed)) set.seed(seed)
    min <- 187
    max <- 680
    IEmean <- rep(mean(c(min,max)),y)
    IEsd <- rep((max-mean(c(min,max)))/2,y)
    
    ret <- mapply(function(mu, sigma) {
        rnorm(mu, sigma, n = x)
    }, mu = IEmean, sigma = IEsd)
    return(ret)
}
class(IEindepcan) <- append(class(IEdep2550),"IE")

IEindepus <- function(x,y,seed=NULL){
    if(!is.null(seed)) set.seed(seed)
    IErw <- t(mapply(function(x){cumsum.bounded(c(runif(1,0.2,0.8),rnorm(y-1,0,0.2)),0.2,0.8)},x=1:x)) #matplot(t(IErw),type='l')
    min <- rep(1700,y)  # based on minimun for rec fishing
    mode <- rep(3000,y) # US landings 2022
    max <- rep(3300,y)  # TAC for future
    cnew<- mapply(function(min,mode,max) {
        mc2d::rpert(x,min,mode,max)
    }, min ,mode, max)
    ret <- cnew*IErw #matplot(t(ret),type='l')
    return(ret)
}
class(IEindepus) <- append(class(IEdep2550),"IE")

OMs <- c('projBH','projM')
MPs <-paste0('MP',1:nMP)

scenmat <- expand.grid(OM=OMs,MP=MPs)
scenmat <- data.frame(apply(scenmat,2,as.character))

scen.list <- lapply(split(scenmat,1:nrow(scenmat)),function(x){
    OMx <- get(as.character(x[1,1]))
    MPx <- get(as.character(x[1,2]))
    MPx$IE <- c('IEindepcan','IEindepus')
    c(OMx,MPx)
})
scenmat$IE <- lapply(scen.list,function(x) paste(x$IE,collapse='.'))
scennames <- apply(scenmat,1,paste,collapse = ".")
names(scen.list) <- scennames

#******************************************************************************
#************* Run all forecasting scenarios **********************************
#******************************************************************************

Date = Sys.Date()

DateDir = paste0("Rdata/",my.year,"/projvalid/",Date,"/")
dir.create(DateDir,showWarnings = FALSE,recursive = T)

multi.forecast(scen.list,DateDir,parallel=F,ncores=min(detectCores(),nMP))

### plots ---------------------------
    filenames <- dir(DateDir, pattern = "")
    files <- paste0(DateDir,filenames)
    runlist <- lapply(files, function(x) {print(x);get(load(x))})
    n <-  gsub(pattern = ".IEindepcan.IEindepus.Rdata",replacement = "",x = filenames)
    n <- gsub(pattern='proj.MP',replacement='',x=n)
    names(runlist) <-n
    class(runlist) <- 'forecastset'
    save(runlist, file=paste0('Rdata/',my.year,'/projvalid/projvalid.Rdata'))

load(file=paste0('Rdata/',my.year,'/projvalid/projvalid.Rdata'))
refBase <- ypr(fit)
REF <- refBase$f40ssb
LRP <- REF*0.40
USR <- REF*0.80

ssbcy <- tail(ssbtable(fit),1)
ssbcy/LRP

p_ssb <- ssbplot(runlist,final=FALSE,year=2000:my.year,ci=FALSE)+
    geom_hline(yintercept=LRP,col='red',linetype='dashed')
saveplot(p_ssb,name="proj_ssb",dim=c(15,15),wd=paste0('img/',my.year,'/projvalid'))

p_ssb <- ssbplot(runlist,final=FALSE,year=2000:my.year,ci=FALSE)+
    geom_hline(yintercept=LRP,col='red',linetype='dashed')
saveplot(p_ssb,name="proj_ssb",dim=c(15,15),wd=paste0('img/',my.year,'/projvalid'))



p_rec <- recplot(runlist[[1]],final=FALSE,year=2000:my.year,ci=TRUE)
saveplot(p_rec,name="proj_rec",dim=c(15,15),wd=paste0('img/',my.year,'/projvalid'))

r <- runlist[[1]]
ies <- attr(r,'IE')
nam <- c('Canada','US')
ies <- lapply(1:2,function(x){
    d <- ies[[x]]
    colnames(d) <- 1:ncol(d)
    y <- data.frame(melt(d,varnames = c('sim','Year')))
    y$name <- nam[x]
    y
})
ies <- do.call('rbind',ies)
pIEbox <- ggplot(ies,aes(x=factor(Year),y=value))+geom_boxplot()+
    facet_wrap(~name,ncol=1,scale='free_y')+labs(y='Catch (t)',x='Year')
saveplot(pIEbox,name="IEbox",dim=c(6,8),wd=paste0('img/',my.year,'/projvalid'))
pIEbox <- pIEbox+labs(y='Captures (t)',x='Année')
saveplot(pIEbox,name="IEbox_FR",dim=c(6,8),wd=paste0('img/',my.year,'/projvalid'))

pIEboxBI <-  ggplot(ies,aes(x=factor(Year),y=value))+geom_boxplot()+
    facet_wrap(~name,ncol=2,scale='free_y')+labs(y='Captures | Catch (t)',x='Année | Year')
saveplot(pIEboxBI,name="IEbox_BI",dim=c(10,4),wd=paste0('img/',my.year,'/projvalid'))


pusboxBI <-  ies %>%  dplyr::filter(name == "US") %>% ggplot(aes(x=factor(Year),y=value))+geom_boxplot()+
    labs(y='Captures | Catch (t)',x='Année | Year', title="US")
saveplot(pusboxBI,name="IEboxus_BI",dim=c(6,6),wd=paste0('img/',my.year,'/projvalid'))

### output table
probgrowth <- function(x){
    y <- x[[2]]
    ssb <- y$ssb
    y2 <- x[[4]]
    nssb <- y2$ssb
    grow <- nssb>ssb
    round(length(grow[grow])/length(grow)*100) 
}


projres <- diamondplot(runlist,what='TAC',year=my.year,data=TRUE)[,c('MP','y')]
names(projres)[2]=paste0('TAC', year)
projres$y2 <- diamondplot(runlist,what='TAC',year=my.year-1,data=TRUE)$y
names(projres)[3] <- c(paste0('TAC', my.year-1))

mc <- ddply(ies,c('name'),function(x) quantile(x$value,c(0.025,0.975)))
mc <- data.frame(matrix(as.numeric(cbind(mc[1,2:3],mc[2,2:3])),nrow=nrow(projres),ncol=4,byrow=TRUE))
colnames(mc) <- c('Canada 2.5%','Canada 97.5%','US 2.5%','US 97.5%')
projres <- cbind(projres,round(mc,0))
projres$lrp1 <- diamondplot(runlist,what='probCZ',year=my.year-1,data=TRUE)$y*100
projres$lrp2 <- diamondplot(runlist,what='probCZ',year=my.year,data=TRUE)$y*100
projres$ssb1 <- round(diamondplot(runlist,what='ssb',year=my.year-1,ratio = TRUE,data=TRUE)$y,2)
projres$ssb2 <- round(diamondplot(runlist,what='ssb',year=my.year,ratio = TRUE,data=TRUE)$y,2)
names(projres)[8:11] <-  c(paste0("LRP", my.year-1),
                           paste0("LRP", my.year),
                           paste0("SSB", my.year-1),
                           paste0("SSB", my.year)
)

projres$rec <- c('BH','MEAN')[ldply(scen.list,function(x)x$rec.meth)[,2]]


projres<- projres %>%  dplyr::arrange(MP, rec)
write.csv(projres, file = paste0("csv/",my.year,"/projvalid.csv"),row.names = FALSE)



