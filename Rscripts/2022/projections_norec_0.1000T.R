#################################################################################################################
#*** Mackerel assessment projections
#*** Canadian mackerel (DFO, 2022)
#*** based on CCAM package
#################################################################################################################
source("C:/Users/VANBE/Desktop/post-doc/DATA/CCAM/R/forecast.R")
ny=3
nosim=2000

#--------------------- projections ------------------------------------------
# basic projection parameters
load('Rdata/2022/fit.Rdata')

projBH <- list(fit=fit,
              nosim=nosim,
              OMlabel='OMassess',
              ave.years=tail(fit$data$years,20),
              rec.years=1969:2018,
              rec.meth=1, #Beverton-Holt
              UL.years=tail(fit$data$years,25),
              deadzone=1000,
              Flim=2.5)

projM <- list(fit=fit,
               nosim=nosim,
               OMlabel='OMassess',
               ave.years=tail(fit$data$years,20),
               rec.years=2011:2022,
               rec.meth=2, #mean with AC
               UL.years=tail(fit$data$years,25),
               deadzone=1000,
               Flim=2.5)

attr(projM$rec.meth,'AC') = 0.9   
rec <- rectable(fit)
rho <- acf(rec[-1,1])$acf[2,1,1]

# --------------------- base MPs ----------------------------------------------
# under what harvest control rule/TAC to forecast?
nMP=20

MP1 <- list(MPlabel='MP1',
            IE=NULL,
            capLower=0,
            TAC.base=0)

copy(x=MP1,n=nMP,name=c('MP'))

avail('MP')
MP1$catchval <- rep(0,ny)
MP2$catchval <- rep(50,ny)
MP3$catchval <- rep(100,ny)
MP4$catchval <- rep(150,ny)
MP5$catchval <- rep(200,ny)
MP6$catchval <- rep(250,ny)
MP7$catchval <- rep(300,ny)
MP8$catchval <- rep(350,ny)
MP9$catchval <- rep(450,ny)
MP10$catchval <- rep(500,ny)
MP11$catchval <- rep(550,ny)
MP12$catchval <- rep(600,ny)
MP13$catchval <- rep(650,ny)
MP14$catchval <- rep(700,ny)
MP15$catchval <- rep(750,ny)
MP16$catchval <- rep(800,ny)
MP17$catchval <- rep(850,ny)
MP18$catchval <- rep(900,ny)
MP19$catchval <- rep(950,ny)
MP20$catchval <- rep(1000,ny)

#******************************************************************************
#************* Create all forecasting scenarios *******************************
#******************************************************************************
# functions that generate matrices of size x (n simulations) on y (number of years)
# see avail('IE') for inspiration
source("C:/Users/VANBE/Desktop/post-doc/DATA/CCAM/R/ie.R")
source("C:/Users/VANBE/Desktop/post-doc/DATA/CCAM/R/forecast.R")
IEindepcan2023 <- function(x,y,seed=NULL){
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
class(IEindepcan2023) <- append(class(IEdep2550),"IE")

IEindepus2023 <- function(x,y,seed=NULL){
    if(!is.null(seed)) set.seed(seed)
    IErw <- t(mapply(function(x){cumsum.bounded(c(runif(1,0.2,0.8),rnorm(y-1,0,0.2)),0.2,0.8)},x=1:x)) #matplot(t(IErw),type='l')
    min <- rep(500,y)  # based on minimun for rec fishing
    mode <- rep(3302,y) # US landings 2022
    max <- rep(3639,y)  # TAC for future
    cnew<- mapply(function(min,mode,max) {
        mc2d::rpert(x,min,mode,max)
    }, min ,mode, max)
    ret <- cnew*IErw #matplot(t(ret),type='l')
    return(ret)
}
class(IEindepus2023) <- append(class(IEdep2550),"IE")

OMs <- c('projBH','projM')
MPs <-paste0('MP',1:nMP)

scenmat <- expand.grid(OM=OMs,MP=MPs)
scenmat <- data.frame(apply(scenmat,2,as.character))

scen.list <- lapply(split(scenmat,1:nrow(scenmat)),function(x){
    OMx <- get(as.character(x[1,1]))
    MPx <- get(as.character(x[1,2]))
    MPx$IE <- c('IEconstant','IEindepus2023')
    c(OMx,MPx)
})
scenmat$IE <- lapply(scen.list,function(x) paste(x$IE,collapse='.'))
scennames <- apply(scenmat,1,paste,collapse = ".")
names(scen.list) <- scennames

#******************************************************************************
#************* Run all forecasting scenarios **********************************
#******************************************************************************

Date = Sys.Date()

DateDir = paste0("Rdata/2022/proj/",Date,"/")
dir.create(DateDir,showWarnings = FALSE,recursive = T)

multi.forecast(scen.list[49:56],DateDir,parallel=FALSE,ncores=7)

### plots ---------------------------
    filenames <- dir(DateDir, pattern = "")
    files <- paste0(DateDir,filenames)
    runlist <- lapply(files, function(x) {print(x);get(load(x))})
    n <-  gsub(pattern = ".IEconstant.IEindepus2023.Rdata",replacement = "",x = filenames)
    n <- gsub(pattern='proj.MP',replacement='',x=n)
    names(runlist) <-n
    class(runlist) <- 'forecastset'
    save(runlist, file='Rdata/2022/proj_norec.Rdata')

load(file='Rdata/2022/proj_norec.Rdata')
refBase <- ypr(fit)
REF <- refBase$f40ssb
LRP <- REF*0.40
USR <- REF*0.80

ssb2022 <- tail(ssbtable(fit),1)
ssb2022/LRP

p_ssb <- ssbplot(runlist,final=FALSE,year=c(2000:2025),ci=FALSE)+
    geom_hline(yintercept=LRP,col='red',linetype='dashed')
saveplot(p_ssb,name="proj_ssb",dim=c(15,15),wd='img/2022/proj')

p_ssb <- ssbplot(runlist,final=FALSE,year=c(2000:2025),ci=FALSE)+
    geom_hline(yintercept=LRP,col='red',linetype='dashed')
saveplot(p_ssb,name="proj_ssb",dim=c(15,15),wd='img/2022/proj')

p_rec <- recplot(runlist[[1]],final=FALSE,year=c(2000:2022),ci=TRUE)
saveplot(p_rec,name="proj_rec",dim=c(15,15),wd='img/2022/proj')

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
saveplot(pIEbox,name="IEbox_norec",dim=c(6,8),wd='img/2022/proj/proj_norec')


### output table
probgrowth <- function(x){
    y <- x[[2]]
    ssb <- y$ssb
    y2 <- x[[4]]
    nssb <- y2$ssb
    grow <- nssb>ssb
    round(length(grow[grow])/length(grow)*100) 
}


projres <- diamondplot(runlist,what='TAC',year=2023,data=TRUE)[,c('MP','y')]
names(projres)[2]='TAC2023'
projres$TAC2024<- diamondplot(runlist,what='TAC',year=2024,data=TRUE)$y
projres$TAC2025 <- diamondplot(runlist,what='TAC',year=2025,data=TRUE)$y
mc <- ddply(ies,c('name'),function(x) quantile(x$value,c(0.025,0.975)))
mc <- data.frame(matrix(as.numeric(cbind(mc[1,2:3],mc[2,2:3])),nrow=nrow(projres),ncol=4,byrow=TRUE))
colnames(mc) <- c('Canada 2.5%','Canada 97.5%','US 2.5%','US 97.5%')
projres <- cbind(projres,round(mc,0))
projres$LRP2024 <- diamondplot(runlist,what='probCZ',year=2024,data=TRUE)$y*100
projres$LRP2025 <- diamondplot(runlist,what='probCZ',year=2025,data=TRUE)$y*100
projres$ssblrp2024 <- round(diamondplot(runlist,what='ssb',year=2024,ratio = TRUE,data=TRUE)$y,2)
projres$ssblrp2025 <- round(diamondplot(runlist,what='ssb',year=2025,ratio = TRUE,data=TRUE)$y,2)
projres$rec <- c('BH','MEAN')[ldply(scen.list,function(x)x$rec.meth)[,2]]

grow <- ldply(runlist,probgrowth)
grow$MP <- as.numeric(sub(".*MP","",grow$.id))
grow$rec <- rep(c("BH","MEAN"),each=nMP)
names(grow)[1:2] <- c('file','grow')
projres <- merge(projres,grow)

write.csv(projres, file = "csv/2022/proj_norec.csv",row.names = FALSE)


projres2 <- ddply(projres,c('MP'),summarise,
                  LRP2024=paste0(round(mean(LRP2024),2),"% (",paste(LRP2024,collapse = "-"),"%)"),
                  LRP2025=paste0(round(mean(LRP2025),2),"% (",paste(LRP2025,collapse = "-"),"%)"),
                  ssblrp2024=paste0(round(mean(ssblrp2024),2)," (",paste(ssblrp2024,collapse = "-"),")"),
                  ssblrp2025=paste0(round(mean(ssblrp2025),2)," (",paste(ssblrp2025,collapse = "-"),")"),
                  grow=paste0(round(mean(grow),2)," % (",paste(grow,collapse = "-"),"%)"))

projres2 <- merge(projres2,unique(projres[,c(1,3:9)]))
write.csv(projres2, file = "csv/2022/proj_mean_nores01000b.csv",row.names = FALSE)

# figs

example <- runlist[grep("MP1",names(runlist))]
class(example) <- 'forecastset'
recplot(example)
