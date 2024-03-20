#***************************************************************************
#************* Tmin ****************** *************************************
#***************************************************************************


source("C:/Users/VANBE/Desktop/post-doc/DATA/CCAM/R/forecast.R")
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

#***************************************************************************
#************* define Operating Models *************************************
#***************************************************************************
ny=11
nosim=2000

load(paste0('Rdata/',year,'/fit.Rdata'))

#--------------------- base model ------------------------------------------
OMbase <- list(fit=fit,
               nosim=nosim,
               OMlabel='OMbase',
               IE='IEconstant', # zero
               year.base=2022,
               ave.years=tail(fit$data$years,20),
               rec.years=fit$data$years,
               rec.meth=1, # BH with AC
               UL.years=fit$data$years, # does nto matter if no MP
               deadzone=1000,
               Flim=2.5,
               catchval=rep(0,ny))

OMbasewUS <- OMbase
OMbasewUS$IE <-c("IEconstant","IEindepus2023")

copy(x=OMbase,n=c(4,4),name=c('OMcore','OMcorewUS'))

# --------------------- recruitment ------------------------------------------
OMcore1$rec.meth=2 #around average
attr(OMcore1$rec.meth,'AC')=0.9

OMcorewUS1$rec.meth=2
attr(OMcorewUS1$rec.meth,'AC')=0.9
OMcorewUS1$IE <-c("IEconstant","IEindepus2023")

# ---------------------  M ----------------------------------------------------
newdat1 <- fit$data
newdat1$natMor[,] <- 0.25

#fitM <- ccam.fit(newdat1,fit$conf,defpar(newdat1,fit$conf))     # run phase 1 + censored
#fitM
#save(fitM, file='Rdata/2022/Tmin/fitM.Rdata')
load(file='Rdata/2022/Tmin/fitM.Rdata')

OMcore2$fit=fitM
OMcorewUS2$fit <- fitM
OMcorewUS2$IE <-c("IEconstant","IEindepus2023")
# --------------------- Upper limit 25-50%USA------------------------------------------
newdat2 <- fit$dat

dir <- "data/2022/"
ct <- read.ices(paste0(dir,'ct.dat'))
ctUSA <- read.ices(paste0(dir,'ctUSA.dat'))
ctunac <- read.ices(paste0(dir,'ctUnaccounted.dat'))
ctwusa <- ct                                   # reported landings
ctwusa[,2] <- ctwusa[,1] + ctunac[,1]          # add max missing in Canada
ctwusa[,1] <- ctwusa[,1]*1.10 + ctUSA[,1]*0.25     # lower bound: increase and add US
ctwusa[,2] <- ctwusa[,2] + ctUSA[,1]*0.5         # add us upper
   
newdat2$logobs[newdat2$aux[,2]==1,] <- log(ctwusa)[-1,]

# fitC1 <- ccam.fit(newdat2,fit$conf,defpar(newdat2,fit$conf))      # run phase 1 + censored
# fitC1
# save(fitM, file='Rdata/2022/Tmin/fitC1.Rdata')
load(file='Rdata/2022/Tmin/fitC1.Rdata')

OMcore3$fit=fitC1
OMcorewUS3$fit=fitC1
OMcorewUS3$IE <-c("IEconstant","IEindepus2023")
# --------------------- Upper limit 50-75USA------------------------------------------
newdat3 <- fit$dat

dir <- "data/2022/"
ct <- read.ices(paste0(dir,'ct.dat'))
ctUSA <- read.ices(paste0(dir,'ctUSA.dat'))
ctunac <- read.ices(paste0(dir,'ctUnaccounted.dat'))
ctwusa <- ct                                   # reported landings
ctwusa[,2] <- ctwusa[,1] + ctunac[,1]          # add max missing in Canada
ctwusa[,1] <- ctwusa[,1]*1.10 + ctUSA[,1]*0.50     # lower bound: increase and add US
ctwusa[,2] <- ctwusa[,2] + ctUSA[,1]*0.75         # add us upper

newdat3$logobs[newdat3$aux[,2]==1,] <- log(ctwusa)[-1,]

# fitC2 <- ccam.fit(newdat3,fit$conf,defpar(newdat3,fit$conf))      # run phase 1 + censored
# fitC2
# save(fitM, file='Rdata/2022/Tmin/fitC2.Rdata')
load(file='Rdata/2022/Tmin/fitC2.Rdata')

OMcore4$fit=fitC2
OMcorewUS4$fit=fitC2
OMcorewUS4$IE <-c("IEconstant","IEindepus2023")


#******************************************************************************
#************* Create all forecasting scenarios *******************************
#******************************************************************************
scenlist=list(
    "OM.base"=OMbase,
    "OM.base.wUS"=OMbasewUS,
    "OM.recMean"=OMcore1,
         "OM.M0.25"=OMcore2,
         "OM.US25.50"=OMcore3,
         "OM.US50.75"=OMcore4,
         "OM.recMean.wUS1"=OMcorewUS1,
         "OM.M0.25.wUS2"=OMcorewUS2,
         "OM.US25.50.wUS3"=OMcorewUS3,
         "OM.US50.75wUS4"=OMcorewUS4)

#******************************************************************************
#************* Run all forecasting scenarios **********************************
#******************************************************************************

newdir <- TRUE

if(newdir) Date <- Sys.Date() else Date <- "2023-02-09"   # continue in old directory or set new one

DateDir <- paste0("Rdata/2022/Tmin/proj/",Date,"/")
dir.create(DateDir,showWarnings = FALSE)

# run
multi.forecast(scenlist,DateDir,parallel=FALSE)

#******************************************************************************
#************* Load all predictions *******************************************
#******************************************************************************

filenames <- dir(DateDir, pattern = ".Rdata")
files <- paste0(DateDir,'/',filenames)
runlist <- lapply(files, function(x) {print(x);get(load(x))})
names(runlist) <- gsub(pattern = ".Rdata",replacement = "",x = filenames)
class(runlist) <- 'forecastset'

save(runlist, file=paste0('Rdata/2022/Tmin.',Date,'.Rdata'))
load(file=paste0('Rdata/2022/Tmin.',Date,'.Rdata'))

## plots (quick and dirty code...)
tmin.ssb <- ssbplot(runlist,ci=FALSE)+geom_vline(xintercept=2022,color='grey',linetype='dashed')
saveplot(tmin.ssb,name="ssb",dim=c(17,10),wd='img/2022/Tmin')

cz <- foreplot(runlist,what.y='probCZ',rect=0.75,ylab='Probability out of the CZ',legendnames = names(runlist))+scale_x_continuous(breaks=2022:2033)
saveplot(cz,name="cz",dim=c(17,10),wd='img/2022/Tmin')

cz <- foreplot(runlist,what.y='probCZ',rect=0.75,ylab='Probabilité de sortir de la ZC',xlab='Année',legendnames = names(runlist))+scale_x_continuous(breaks=2022:2033)
saveplot(cz,name="cz",dim=c(17,10),wd='img/2022/Tmin_FR')


df <- foreplot(runlist,what.y='probCZ',data=TRUE)
rebuildf<- ddply(df[df$y<0.75,],c('id','OM','MP','IE'),summarise,ny=length(y))
rebuildf<- ddply(rebuildf,c('IE'),function(x){paste0(x[x$OM=='OMbase','ny'],' [',paste(range(x[,'ny']),collapse='-'),']')})
rebuildf[,1] <- c("F = 0","TACcan=0, TACus=3639t")

df$OM <- gsub("wUS","",df$OM)
df[df$OM=="OMcore1",'OM'] <- "OM.recMean"
df[df$OM=="OMcore2",'OM'] <- "OM0.25"
df[df$OM=="OMcore3",'OM'] <- "OM.US25-50"
df[df$OM=="OMcore4",'OM'] <- "OM.US50-75"
df$OM <- factor(df$OM,levels=c("OMbase","OM.recMean","M0.25","OM.US25-50","OM.US50-75"))
df[df$IE=="IEconstant","IE"] <- "F = 0"
df[df$IE=="IEconstant.IEindepus2023","IE"] <-"TACcan=0, TACus=3639t"
df$IE <- factor(df$IE,levels=c("F = 0","TACcan=0, TACus=3639t"))

p <- ggplot(df,aes(x=x,y=y*100))+
    geom_rect(xmin=-Inf,xmax=Inf,ymin=75,ymax=100,fill='lightgrey')+
    geom_point(aes(col=OM))+
    geom_line(aes(col=OM))+
    geom_text(data=rebuildf,aes(y=25,x=2031,label=V1))+
    scale_color_viridis_d()+
    #scale_color_manual(values=c(1:5))+
    labs(y="Probability (%) of getting out of the CZ",x='Year',col='')+
    facet_wrap(~IE,ncol=1)+
    scale_y_continuous(expand=c(0,0),limits = c(0,100))+
    theme(legend.position = 'bottom')+
    guides(col=guide_legend(nrow=2,byrow=T))
saveplot(p,name="cv_2panel_col",dim=c(12,14),wd='img/2022/Tmin')   


r <- runlist[[5]]
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
ggplot(ies,aes(x=factor(Year),y=value))+geom_boxplot()+
    facet_wrap(~name,ncol=1,scale='free_y')+labs(y='Catch (t)',x='Year')
