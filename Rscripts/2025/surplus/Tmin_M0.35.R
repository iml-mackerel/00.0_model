#***************************************************************************
#************* Tmin ****************** *************************************
#***************************************************************************
#ajuster pour TAC


#https://www.federalregister.gov/documents/2024/04/12/2024-07650/fisheries-of-the-northeastern-united-states-2024-and-projected-2025-specifications-for-the-atlantic
#2024-2025 commercial quota = 868

tacus=868
abc=3302-74 #74 for can wa planned

IEindepus <- function(x,y,seed=NULL){
    if(!is.null(seed)) set.seed(seed)
    IErw <- t(mapply(function(x){cumsum.bounded(c(runif(1,0.2,0.8),rnorm(y-1,0,0.2)),0.2,0.8)},x=1:x)) #matplot(t(IErw),type='l')
    min <- rep(500,y)  # based on minimun for rec fishing
    mode <- rep(tacus,y) # tac for 2024-2025  US landings 2022
    max <- rep(abc,y)  # abc TAC for future
    cnew<- mapply(function(min,mode,max) {
        mc2d::rpert(x,min,mode,max)
    }, min ,mode, max)
    ret <- cnew*IErw #matplot(t(ret),type='l')
    return(ret)
}
class(IEindepus) <- append(class(IEdep2550),"IE")

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
               year.base=year,
               ave.years=tail(fit$data$years,20),
               rec.years=fit$data$years,
               rec.meth=1, # BH with AC
               UL.years=fit$data$years, # does nto matter if no MP
               deadzone=1000,
               Flim=2.5,
               catchval=rep(0,ny))

OMbasewUS <- OMbase
OMbasewUS$IE <-c("IEconstant","IEindepus")

copy(x=OMbase,n=c(5,5),name=c('OMcore','OMcorewUS'))

# --------------------- recruitment ------------------------------------------
OMcore1$rec.meth=2 #around average
attr(OMcore1$rec.meth,'AC')=0.9
attr(OMcore1$rec.meth,'sd.option') = 'ci'

OMcorewUS1$rec.meth=2
attr(OMcorewUS1$rec.meth,'AC')=0.9
attr(OMcorewUS1$rec.meth,'sd.option') = 'ci'

OMcorewUS1$IE <-c("IEconstant","IEindepus")

# ---------------------  M ----------------------------------------------------
newdat1 <- fit$data
newdat1$natMor[,] <- 0.25

fitM <- ccam.fit(newdat1,fit$conf,defpar(newdat1,fit$conf))     # run phase 1 + censored
fitM
save(fitM, file=paste0('Rdata/',year,'/Tmin/fitM.Rdata'))
load(file=paste0('Rdata/',year,'/Tmin/fitM.Rdata'))

OMcore2$fit=fitM
OMcorewUS2$fit <- fitM
OMcorewUS2$IE <-c("IEconstant","IEindepus")
# ---------------------  M0.35 ----------------------------------------------------
newdat1 <- fit$data
newdat1$natMor[,] <- 0.35

fitM35 <- ccam.fit(newdat1,fit$conf,defpar(newdat1,fit$conf))     # run phase 1 + censored
fitM35
save(fitM35, file=paste0('Rdata/',year,'/Tmin/fitM35.Rdata'))
load(file=paste0('Rdata/',year,'/Tmin/fitM35.Rdata'))

OMcore3$fit=fitM35
OMcorewUS3$fit <- fitM35
OMcorewUS3$IE <-c("IEconstant","IEindepus")
# --------------------- Upper limit 25-50%USA------------------------------------------


newdat2 <- fit$dat

dir <- paste0("data/", year,"/")
ct <- read.ices(paste0(dir,'ct.dat'))
ctUSA <- read.ices(paste0(dir,'ctUSA.dat'))
ctunac <- read.ices(paste0(dir,'ctUnaccounted.dat'))
ctwusa <- ct                                   # reported landings
ctwusa[,2] <- ctwusa[,1] + ctunac[,1]          # add max missing in Canada
ctwusa[,1] <- ctwusa[,1]*1.10 + ctUSA[,1]*0.25     # lower bound: increase and add US
ctwusa[,2] <- ctwusa[,2] + ctUSA[,1]*0.5         # add us upper
   
newdat2$logobs[newdat2$aux[,2]==1,] <- log(ctwusa)[-1,]

 fitC1 <- ccam.fit(newdat2,fit$conf,defpar(newdat2,fit$conf))      # run phase 1 + censored
 fitC1
 save(fitC1, file=paste0('Rdata/',year,'/Tmin/fitC1.Rdata'))
load(file=paste0('Rdata/',year,'/Tmin/fitC1.Rdata'))

OMcore4$fit=fitC1
OMcorewUS4$fit=fitC1
OMcorewUS4$IE <-c("IEconstant","IEindepus")
# --------------------- Upper limit 50-75USA------------------------------------------
newdat3 <- fit$dat

dir <- paste0("data/", year,"/")
ct <- read.ices(paste0(dir,'ct.dat'))
ctUSA <- read.ices(paste0(dir,'ctUSA.dat'))
ctunac <- read.ices(paste0(dir,'ctUnaccounted.dat'))
ctwusa <- ct                                   # reported landings
ctwusa[,2] <- ctwusa[,1] + ctunac[,1]          # add max missing in Canada
ctwusa[,1] <- ctwusa[,1]*1.10 + ctUSA[,1]*0.50     # lower bound: increase and add US
ctwusa[,2] <- ctwusa[,2] + ctUSA[,1]*0.75         # add us upper

newdat3$logobs[newdat3$aux[,2]==1,] <- log(ctwusa)[-1,]

 fitC2 <- ccam.fit(newdat3,fit$conf,defpar(newdat3,fit$conf))      # run phase 1 + censored
 fitC2
 save(fitC2, file=paste0('Rdata/',year,'/Tmin/fitC2.Rdata'))
load(file=paste0('Rdata/',year,'/Tmin/fitC2.Rdata'))

OMcore5$fit=fitC2
OMcorewUS5$fit=fitC2
OMcorewUS5$IE <-c("IEconstant","IEindepus")


#******************************************************************************
#************* Create all forecasting scenarios *******************************
#******************************************************************************
scenlist=list(
    "OM.base"=OMbase,
    "OM.base.wUS"=OMbasewUS,
    "OM.recMean"=OMcore1,
         "OM.M0.25"=OMcore2,
    "OM.M0.35"=OMcore3,
         "OM.US25.50"=OMcore4,
         "OM.US50.75"=OMcore5,
         "OM.recMean.wUS1"=OMcorewUS1,
         "OM.M0.25.wUS2"=OMcorewUS2,
         "OM.M0.35.wUS2"=OMcorewUS3,
         "OM.US25.50.wUS3"=OMcorewUS4,
         "OM.US50.75wUS4"=OMcorewUS5)

#******************************************************************************
#************* Run all forecasting scenarios **********************************
#******************************************************************************

newdir <- T

if(newdir) Date <- Sys.Date() else Date <- "2024-12-17"   # continue in old directory or set new one

DateDir <- paste0("Rdata/",year,"/Tmin/proj/",Date,"/")
dir.create(DateDir,showWarnings = T)

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

save(runlist, file=paste0('Rdata/',year,'/Tmin/Tmin',Date,'.Rdata'))
load(file=paste0('Rdata/',year,'/Tmin/Tmin',Date,'.Rdata'))

## plots (quick and dirty code...)

newrunlist<- runlist
names(newrunlist) <- c("OM.base - F0",paste0("OM.base TAC US=",tacus,"t"),"OM.M0.25 - F0" ,"OM.M0.35 - F0", paste0("OM.M0.25 TAC US=",tacus,"t"), paste0("OM.M0.35 TAC US=",tacus,"t"),"OM.recMean - F0", paste0("OM.recMean TAC US=",tacus,"t"),
"OM.US25.50 - F0", paste0("OM.US25.50 TAC US=",tacus,"t"),"OM.US50.75 - F0",paste0("OM.US50.75 TAC US=",tacus,"t"))

tmin.ssb <- ssbplot(newrunlist,ci=FALSE)+
            geom_vline(xintercept=year,color='grey',linetype='dashed') +
    scale_x_continuous(breaks=seq(1970, year+11, 10))
saveplot(tmin.ssb,name="ssbM35",dim=c(17,10),wd=paste0('img/',year,'/Tmin'))

cz <- foreplot(runlist,what.y='probCZ',rect=0.75,ylab='Probability out of the CZ',legendnames = names(runlist))+
    scale_x_continuous(breaks=seq(year,year+11,2))
saveplot(cz,name="czM35",dim=c(17,10),wd=paste0('img/',year,'/Tmin'))

cz <- cz+ labs(y='Probabilité de sortir de la ZC',
               x='Année',color = names(runlist))+
      scale_x_continuous(breaks=year:(year+11))
saveplot(cz,name="czM35_FR",dim=c(17,10),wd=paste0('img/',year,'/Tmin/'))


df <- foreplot(runlist,what.y='probCZ',data=TRUE)
rebuildf<- ddply(df[df$y<0.75,],c('id','OM','MP','IE'),summarise,ny=length(y))
rebuildf<- ddply(rebuildf,c('IE'),function(x){paste0(x[x$OM=='OMbase','ny'],' [',paste(range(x[,'ny']),collapse='-'),']')})
rebuildf[,1] <- c("F = 0",paste0("TACcan=0, TACus=",tacus,"t"))

df$OM <- gsub("wUS","",df$OM)
df[df$OM=="OMcore1",'OM'] <- "OM.recMean"
df[df$OM=="OMcore2",'OM'] <- "OM0.25"
df[df$OM=="OMcore3",'OM'] <- "OM0.35"
df[df$OM=="OMcore4",'OM'] <- "OM.US25-50"
df[df$OM=="OMcore5",'OM'] <- "OM.US50-75"
df$OM <- factor(df$OM,levels=c("OMbase","OM.recMean","OM0.25","OM0.35","OM.US25-50","OM.US50-75"))
df[df$IE=="IEconstant","IE"] <- "F = 0"
df[df$IE=="IEconstant.IEindepus","IE"] <-paste0("TACcan=0, TACus=",tacus,"t")
df$IE <- factor(df$IE,levels=c("F = 0",paste0("TACcan=0, TACus=",tacus,"t")))

p <- ggplot(df,aes(x=x,y=y*100))+
    geom_rect(xmin=-Inf,xmax=Inf,ymin=75,ymax=100,fill='lightgrey')+
    geom_point(aes(col=OM))+
    geom_line(aes(col=OM))+
    geom_text(data=rebuildf,aes(y=25,x=year+5,label=paste0("Tmin = ",V1)))+
    scale_color_viridis_d()+
    #scale_color_manual(values=c(1:5))+
    facet_wrap(~IE,ncol=1)+
    scale_y_continuous(expand=c(0,0),limits = c(0,100))+
    scale_x_continuous(breaks=seq(min(df$x), max(df$x), 2))+
    theme(legend.position = 'bottom')+
    guides(col=guide_legend(nrow=2,byrow=T))

pEN<- p + labs(y="Probability (%) of getting out of the CZ",x='Year',col='')
saveplot(pEN,name="cv_2panel_colM35",dim=c(12,14),wd=paste0('img/',year,'/Tmin') )  

pFR<- p + labs(y="Probabilité de sortir de la ZC",x='Année',col='')
saveplot(pFR,name="cv_2panel_colM35FR",dim=c(12,14),wd=paste0('img/',year,'/Tmin') )  

pBI<- p + labs(y="Probabilité de sortir de la ZC\nProbability (%) of getting out of the CZ",x='Année | Year',col='')+
    theme(legend.position = c(0.8,0.7), legend.direction = "vertical")+
    guides(col=guide_legend(ncol=1))
saveplot(pBI,name="cv_2panel_colM35BI",dim=c(14,14),wd=paste0('img/',year,'/Tmin') )  


#not working but to my knowledge not used
#r <- runlist[[5]]
#ies <- attr(r,'IE')
#nam <- c('Canada','US')
#ies <- lapply(1:2,function(x){
 #   d <- ies[[x]]
  #  colnames(d) <- 1:ncol(d)
   # y <- data.frame(melt(d,varnames = c('sim','Year')))
    #y$name <- nam[x]
    #y
#})
#ies <- do.call('rbind',ies)
#ggplot(ies,aes(x=factor(Year),y=value))+geom_boxplot()+
#    facet_wrap(~name,ncol=1,scale='free_y')+labs(y='Catch (t)',x='Year')
