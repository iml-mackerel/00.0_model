#################################################################################################################
#*** Mackerel assessment projections
#*** Canadian mackerel (DFO, 2018)
#*** based on CCAM package
#################################################################################################################

ny=3
nosim=2000

#--------------------- projections ------------------------------------------
load('Rdata/2018/fit.Rdata')

proj <- list(fit=fit,
               nosim=nosim,
               OMlabel='OMassess',
               ave.years=tail(fit$data$years,10),
               rec.years=1969:2018,
               rec.meth=2, #mean with AC
               UL.years=tail(fit$data$years,25),
               deadzone=1000,
               Flim=2.5)


attr(proj$rec.meth,'AC')=0.9

# --------------------- base MPs ----------------------------------------------

nMP=11

MP1 <- list(MPlabel='MP1',
            IE=NULL,
            capLower=0,
            TAC.base=10000)

copy(x=MP1,n=nMP,name=c('MP'))

avail('MP')
#MP1$catchval <- rep(0,ny)
MP2$catchval <- rep(0,ny)
MP3$MP <- rep('MPeggsimple',ny)
MP4$MP <- rep('MPeggcomplex0',ny)
MP5$MP <- rep('MPeggcomplex',ny)
MP6$MP <- rep('MPeggcomplexramp',ny)
MP7$MP <- rep('MPeggcomplex2000',ny)
MP8$MP <- rep('MPeggcomplex4000',ny)
MP9$MP <- rep('MPeggcomplex6000',ny)
MP10$MP <- rep('MPeggcomplex8000',ny)
MP11$MP <- rep('MPeggcomplex10000',ny)

#******************************************************************************
#************* Create all forecasting scenarios *******************************
#******************************************************************************

IEs <- c("IEindep2019")
OMs <- c('proj')
MPs <-paste0('MP',2:nMP)

scenmat <- expand.grid(OM=OMs,MP=MPs, IE=IEs)
scenmat <- data.frame(apply(scenmat,2,as.character))

scen.list <- lapply(split(scenmat,1:nrow(scenmat)),function(x){
    OMx <- get(as.character(x[1,1]))
    MPx <- get(as.character(x[1,2]))
    MPx$IE <- c(as.character(x[1,3]),'IEdep2550')
    c(OMx,MPx)
})
scenmat$IE <- lapply(scen.list,function(x) paste(x$IE,collapse='.'))
scennames <- apply(scenmat,1,paste,collapse = ".")
names(scen.list) <- scennames

#******************************************************************************
#************* Run all forecasting scenarios **********************************
#******************************************************************************

Date = Sys.Date()

DateDir = paste0("Rdata/",Date,"/")
dir.create(DateDir,showWarnings = FALSE)

year <- 2018
dir <- paste0('data/',year,'/')
ctUSA <- read.ices(paste0(dir,'ctUSA.dat'))

multi.forecast(scen.list,DateDir,parallel=FALSE,ncores=7)

### plots ---------------------------
    # filenames <- dir(DateDir, pattern = "indep2019")
    # files <- paste0(DateDir,filenames)
    # runlist <- lapply(files, function(x) {print(x);get(load(x))})
    # n <-  gsub(pattern = ".IEindep2019.IEdep2550.Rdata",replacement = "",x = filenames)
    # n <- gsub(pattern='proj.MP',replacement='',x=n)
    # names(runlist) <-n
    # class(runlist) <- 'forecastset'
    # save(runlist, file='Rdata/proj.new.Rdata')

load(file='Rdata/2018/proj.Rdata')
refBase <- ypr(fit)
REF <- refBase$f40ssb
LRP <- REF*0.40
USR <- REF*0.80

ssb2018 <- tail(ssbtable(fit),1)
ssb2018/LRP

p_ssb <- ssbplot(runlist,final=FALSE,year=c(2000:2022),ci=FALSE,linesize=1)+
    geom_hline(yintercept=LRP,col='red',linetype='dashed')
saveplot(p_ssb,name="proj_ssb",dim=c(15,15),wd='img/proj')

p_rec <- recplot(runlist[[1]],final=FALSE,year=c(2000:2022),ci=TRUE,linesize=1)
saveplot(p_rec,name="proj_rec",dim=c(15,15),wd='img/proj')

IE <-  data.frame(year=1:3,mean=Reduce(function(v, x) .8*v , x=numeric(3),  init=5500, accumulate=TRUE)[-1])
IE$sd <- IE$mean/8

pIE <- ggplot(IE,aes(x=year,y=mean))+
    geom_ribbon(aes(ymin=mean-sd*2,ymax=mean+sd*2),alpha=0.2)+
    geom_line()+scale_x_continuous(expand=c(0,0))+
    labs(y='Unaccounted-for Canadian catch (t)',x='Year')

saveplot(pIE,name="IE",dim=c(12,10),wd='img/proj')

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
saveplot(pIEbox,name="IEbox",dim=c(6,8),wd='img/proj')


# source the plot.R file from CCAM
prec <- plotit(rectable(fit),ylab='Recruitment (millions)',year=c(1969:2018),scale=1000000)
pssb <- plotit(ssbtable(fit),ylab='SSB (kt)',scale=1000)+
    geom_hline(yintercept=LRP/1000,col='darkred')+
    geom_hline(yintercept=USR/1000,col='darkgreen')+
    geom_hline(yintercept=REF/1000,col='black')

saveplot(grid.arrange(pssb,prec,ncol=2),name="SAR_model_output",dim=c(18,7),wd='img/resdoc')


### output table
probgrowth <- function(x){
    y <- x[[2]]
    ssb <- y$ssb
    y2 <- x[[4]]
    nssb <- y2$ssb
    grow <- nssb>ssb
    round(length(grow[grow])/length(grow)*100,1) 
}


projres <- diamondplot(runlist,what='TAC',year=2019,data=TRUE)[,c('MP','y')]
names(projres)[2]='TAC2019'
projres$TAC2020 <- diamondplot(runlist,what='TAC',year=2020,data=TRUE)$y
projres$TAC2021 <- diamondplot(runlist,what='TAC',year=2021,data=TRUE)$y
mc <- ddply(ies,c('name'),function(x) quantile(x$value,c(0.05,0.95)))
mc <- data.frame(matrix(as.numeric(cbind(mc[1,2:3],mc[2,2:3])),nrow=nrow(projres),ncol=4,byrow=TRUE))
colnames(mc) <- c('Canada 5%','Canada 95%','US 5%','US 95%')
projres <- cbind(projres,round(mc,0))
projres$LRP2020 <- diamondplot(runlist,what='probCZ',year=2020,data=TRUE)$y
projres$LRP2021 <- diamondplot(runlist,what='probCZ',year=2021,data=TRUE)$y
projres$ssblrp2020 <- round(diamondplot(runlist,what='ssb',year=2020,ratio = TRUE,data=TRUE)$y,2)
projres$ssblrp2021 <- round(diamondplot(runlist,what='ssb',year=2021,ratio = TRUE,data=TRUE)$y,2)


grow <- ldply(runlist,probgrowth)
grow$.id <- as.numeric(grow$.id)
projres$grow <- grow[order(grow$.id),2]
projres <- projres[-c(1,4,5),]
projres

write.csv(projres, file = "proj.csv",row.names = FALSE)

