#################################################################################################################
#*** Mackerel assessment projections
#*** Canadian mackerel (DFO, 2022)
#*** based on CCAM package
#################################################################################################################
library(CCAM)
library(ggpubr)
library(tidyverse)
source("R/saveplot.R")
library(catchR)
source("R/multi.forecast.R")#patch for CCAM

theme_set(theme_mackerel())   
my.year=2025
#patch
cumsum.bounded <- function(x, lower = 0, upper = 500) {
    bsum <- function(x, y) min(upper, max(lower, x+y))
    if (length(x) > 1) Reduce(bsum, x, acc = TRUE) else x
}
library(stringr)
ny=3 
nosim=2000



#--------------------- projections ------------------------------------------
# basic projection parameters
load(paste0('Rdata/',my.year,'/fit.Rdata'))

projBH <- list(fit=fit,
               nosim=nosim,
               OMlabel='OMassess',
               ave.years=tail(fit$data$years,20), #mpyenne weight 
               rec.years=1969:2025, # recrutement
               rec.meth=1, #Beverton-Holt
               UL.years=tail(fit$data$years,25), #censoring dans le futur. pour prédire futre OBSERVATION pas états du stock 
               deadzone=1000, # valuer biomasse sous laquelle le stock est éteint. 
               Flim=2.5) # limite supérieur de mortalité par la pêche appliquée

projM <- list(fit=fit,
              nosim=nosim,
              OMlabel='OMassess',
              ave.years=tail(fit$data$years,20),
              rec.years=2011:2025,
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
nMP=2

MP1 <- list(MPlabel='MP1',
            IE=NULL,
            capLower=0,
            TAC.base=0)

copy(x=MP1,n=nMP,name=c('MP'))

avail('MP')
MP1$catchval <- rep(0,ny)
MP2$catchval <- rep(500,ny)

#******************************************************************************
#************* Create all forecasting scenarios *******************************
#******************************************************************************
# functions that generate matrices of size x (n simulations) on y (number of years)
# see avail('IE') for inspiration
#source("C:/Users/VANBE/Desktop/post-doc/DATA/CCAM/R/ie.R")
#source("C:/Users/VANBE/Desktop/post-doc/DATA/CCAM/R/forecast.R")
IEindepcan <- function(x,y,seed=NULL){
    if(!is.null(seed)) set.seed(seed)
    min <- 0
    max <- 1
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
    min <- rep(0,3)  # based on minimun for rec fishing
    mode <-rep(0.5,3)
    max <- rep(1,3)  # abc TAC for future
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

DateDir = paste0("Rdata/",my.year,"/proj/",Date,"/")
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
    save(runlist, file=paste0('Rdata/',my.year,'/proj_rec0.Rdata'))

load(file=paste0('Rdata/',my.year,'/proj_rec0.Rdata'))
refBase <- ypr(fit)
REF <- refBase$f40ssb
LRP <- REF*0.40
USR <- REF*0.80

ssbcy <- tail(ssbtable(fit),1)
ssbcy/LRP

p_ssb <- ssbplot(runlist,final=FALSE,year=2000:my.year+3,ci=FALSE)+
    geom_hline(yintercept=LRP,col='red',linetype='dashed')
saveplot(p_ssb,name="proj_ssb_rec0",dim=c(15,15),wd=paste0('img/',my.year,'/proj'))

p_ssb <- ssbplot(runlist,final=FALSE,year=2000:my.year+3,ci=FALSE)+
    geom_hline(yintercept=LRP,col='red',linetype='dashed')
saveplot(p_ssb,name="proj_ssb_rec0",dim=c(15,15),wd=paste0('img/',my.year,'/proj'))

p_rec <- recplot(runlist[[1]],final=FALSE,year=2000:my.year+3,ci=TRUE)
saveplot(p_rec,name="proj_rec_rec0",dim=c(15,15),wd=paste0('img/',my.year,'/proj'))

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


table_ies <- ies %>%  dplyr::group_by(name, Year) %>%  dplyr::summarize(quant25 = quantile(value, 0.025),
                                             quant975= quantile(value, 0.975)) %>% 
    tidyr::pivot_wider(names_from= name, values_from= quant25:quant975)

colnames(table_ies) <- c('Canada 2.5%','US 2.5%','Canada 97.5%','US 97.5%')
write.csv(table_ies, file = paste0("csv/",my.year,"/proj_table_ie_rec0.csv"),row.names = FALSE)

pIEhisto <- ggplot(ies %>%  filter(sim==""),aes(x=value, fill=as.factor(Year), col=as.factor(Year)))+geom_density(alpha=0.2, linewidth=1)+
    facet_wrap(~name,scale='free', ncol=1)+labs(x='Catch (t)', y="Probability") +theme(axis.text.y=element_blank()) +
    scale_fill_manual(values=c("black", "red", "dodgerblue"), name="")+
    scale_color_manual(values=c("black", "red", "dodgerblue"), name="") 
saveplot(pIEhisto,name="IEdensity_rec0",dim=c(6,8),wd=paste0('img/',my.year,'/proj'))

pIEbox <- ggplot(ies,aes(x=factor(Year),y=value))+geom_boxplot()+
    facet_wrap(~name,ncol=1,scale='free_y')+labs(y='Catch (t)',x='Year')
saveplot(pIEbox,name="IEbox_rec0",dim=c(6,8),wd=paste0('img/',my.year,'/proj'))
pIEbox <- pIEbox+labs(y='Captures (t)',x='Année')
saveplot(pIEbox,name="IEbox_FR_rec0",dim=c(6,8),wd=paste0('img/',my.year,'/proj'))

pIEboxBI <-  ggplot(ies,aes(x=factor(Year),y=value))+geom_boxplot()+
    facet_wrap(~name,ncol=2,scale='free_y')+labs(y='Captures | Catch (t)',x='Année | Year')
saveplot(pIEboxBI,name="IEbox_BI_rec0",dim=c(10,4),wd=paste0('img/',my.year,'/proj'))


pusboxBI <-  ies %>%  dplyr::filter(name == "US") %>% ggplot(aes(x=factor(Year),y=value))+geom_boxplot()+
    labs(y='Captures | Catch (t)',x='Année | Year', title="US")
saveplot(pusboxBI,name="IEboxus_BI_rec0",dim=c(6,6),wd=paste0('img/',my.year,'/proj'))

### output table
probgrowth <- function(x, nyear=4){
    y <- x[[2]] #y+1 (2025)
    ssb <- y$ssb
    y2 <- x[[nyear]]#y+3 projections for 2024:2027 (2027)
    nssb <- y2$ssb
    grow <- nssb>ssb
    round(length(grow[grow])/length(grow)*100) 
}


projres <- diamondplot(runlist,what='TAC',year=my.year+1,data=TRUE)[,c('MP','y')]
names(projres)[2]=paste0('TAC', my.year+1)
projres$y2 <- diamondplot(runlist,what='TAC',year=my.year+2,data=TRUE)$y
projres$y3 <- diamondplot(runlist,what='TAC',year=my.year+3,data=TRUE)$y
names(projres)[3:4] <- c(paste0('TAC', my.year+2),paste0('TAC', my.year+3))



mc <- ddply(ies,c('name'),function(x) quantile(x$value,c(0.025,0.975)))
mc <- data.frame(matrix(as.numeric(cbind(mc[1,2:3],mc[2,2:3])),nrow=nrow(projres),ncol=4,byrow=TRUE))
colnames(mc) <- c('Canada 2.5%','Canada 97.5%','US 2.5%','US 97.5%')
projres <- cbind(projres,round(mc,0))
projres$lrp1 <- diamondplot(runlist,what='probCZ',year=my.year+2,data=TRUE)$y*100
projres$lrp2 <- diamondplot(runlist,what='probCZ',year=my.year+3,data=TRUE)$y*100
projres$ssb1 <- round(diamondplot(runlist,what='ssb',year=my.year+2,ratio = TRUE,data=TRUE)$y,2)
projres$ssb2 <- round(diamondplot(runlist,what='ssb',year=my.year+3,ratio = TRUE,data=TRUE)$y,2)
names(projres)[9:12] <-  c(paste0("LRP", my.year+2),
                           paste0("LRP", my.year+3),
                           paste0("SSB", my.year+2),
                           paste0("SSB", my.year+3)
)

projres$rec <- c('BH','MEAN')[ldply(scen.list,function(x)x$rec.meth)[,2]]



grow <- ldply(runlist,function(x) probgrowth(x, nyear = 4))
grow$MP <- str_replace(grow$.id,pattern="projBH.MP",  replacement="")
grow$MP <- str_replace(grow$MP,pattern="projM.MP",  replacement="")
grow$rec <- substring(grow$.id, 5, 6) 
grow$rec <- gsub(grow$rec, pattern=".", replacement="",fixed=T) 
grow$rec[which(grow$rec=="M")] <- "MEAN"
names(grow)[1:2] <- c('file',paste0("Grow", my.year+3))
projres <- join(projres,grow, type="left")


grow2 <- ldply(runlist,function(x) probgrowth(x, nyear = 3))
grow2$MP <- str_replace(grow2$.id,pattern="projBH.MP",  replacement="")
grow2$MP <- str_replace(grow2$MP,pattern="projM.MP",  replacement="")
grow2$rec <- substring(grow2$.id, 5, 6) 
grow2$rec <- gsub(grow2$rec, pattern=".", replacement="",fixed=T) 
grow2$rec[which(grow2$rec=="M")] <- "MEAN"
names(grow2)[1:2] <- c('file',paste0("Grow", my.year+2))
projres <- join(projres,grow2, type="left")



projres<- projres %>%  dplyr::arrange(MP, rec)
write.csv(projres, file = paste0("csv/",my.year,"/proj_rec0.csv"),row.names = FALSE)

projres=read.csv(paste0("csv/",my.year,"/proj_rec0.csv"))

#colnames of LRP and SSB 2024 and 2025
ref.names<- names(projres)[c(9:12, 15,16)] 

projres2 <- ddply(projres,c('MP'),summarise,
                  LRPy1=paste0(round(mean(get(ref.names[1])),2),"% (",paste(get(ref.names[1]),collapse = "-"),"%)"),
                  LRPy2=paste0(round(mean(get(ref.names[2])),2),"% (",paste(get(ref.names[2]),collapse = "-"),"%)"),
                  SSBy1=paste0(round(mean(get(ref.names[3])),2)," (",paste(get(ref.names[3]),collapse = "-"),")"),
                  SSBy2=paste0(round(mean(get(ref.names[4])),2)," (",paste(get(ref.names[4]),collapse = "-"),")"),
                  Growy1=paste0(round(mean(get(ref.names[5])),2)," (",paste(get(ref.names[5]),collapse = "-"),")"),
                  Growy2=paste0(round(mean(get(ref.names[6])),2)," (",paste(get(ref.names[6]),collapse = "-"),")")
                  )

projres2 <- merge(projres2,unique(projres[,c(1,2,5:8)]))
write.csv(projres2, file = paste0("csv/",my.year,"/proj_mean_rec0.csv"),row.names = FALSE)

# figs

example <- runlist[c("projBH.MP1", "projM.MP1")]
class(example) <- 'forecastset'
names(example) <- c("BH", "Mean") 
p<- recplot(example) + 
    scale_x_continuous(limits=c(2010, my.year+4)) +
    scale_y_continuous(limits=c(0,5e5))+
    scale_color_manual(values=c("black", "grey"))+
    scale_fill_manual(values=c("black", "grey"))

pBI<- p+labs(x="Année | Year", y="Recrutement | Recruitment")
saveplot(pBI,name="proj_recBI_rec0",dim=c(12,6),wd=paste0('img/',my.year,'/proj') )  

