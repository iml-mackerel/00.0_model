# quick demo script to compare forecasts one assessment with new TEP observations

# 1) basic projection (example) -------------------------------
load('Rdata/2024/fit.Rdata')
ny=3
nosim=10
# 2) with MP hack ----------------------------------------------
# make MP that does the same as a catchval
MPcaro <- function(){
    catchval=c(470,440,440)  # ideally the true future TACs
    rep(catchval,nosim)
}
class(MPcaro) <- append(class(MPcaro),"MP")
attr(MPcaro,'model') <- FALSE


cumsum.bounded <- function(x, lower = 0, upper = 500) {
    bsum <- function(x, y) min(upper, max(lower, x+y))
    if (length(x) > 1) Reduce(bsum, x, acc = TRUE) else x
}



####!!!!!!!!!!!changer les TAC et landings pour US pour 2024


projBH <- list(fit=fit,
               nosim=nosim,
               OMlabel='OMassess',
               ave.years=tail(fit$data$years,20), #mpyenne weight 
               rec.years=1969:my.year, # recrutement
               rec.meth=1, #Beverton-Holt
               UL.years=tail(fit$data$years,25), #censoring dans le futur. pour prédire futre OBSERVATION pas états du stock 
               deadzone=1000, # valuer biomasse sous laquelle le stock est éteint. 
               Flim=2.5) # limite supérieur de mortalité par la pêche appliquée

projM <- list(fit=fit,
              nosim=nosim,
              OMlabel='OMassess',
              ave.years=tail(fit$data$years,20),
              rec.years=2011:my.year,
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
nMP=1

MP1 <- list(MP=rep("MPcaro",ny),
            MPlabel='MPcaro',
            IE=NULL,
            capLower=0,
            TAC.base=0)

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

comm=868
disc=115
rec=2143 *0.12
tacus=comm+disc+rec
abc=3200 #74 for can wa planned


IEindepus <- function(x,y,seed=NULL){
    if(!is.null(seed)) set.seed(seed)
    IErw <- t(mapply(function(x){cumsum.bounded(c(runif(1,0.2,0.8),rnorm(y-1,0,0.2)),0.2,0.8)},x=1:x)) #matplot(t(IErw),type='l')
    min <- rep(rec,y)  # based on minimun for rec fishing
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
MPs <-paste0('MP1')

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
projb <- forecast.patch(MP=rep("MPcaro",ny),
                  fit=fit,
                  nosim=10)


Date = Sys.Date()

DateDir = paste0("Rdata/",my.year,"/projvalid/",Date,"/")
dir.create(DateDir,showWarnings = FALSE,recursive = T)

multi.forecast(scen.list,DateDir,parallel=F,ncores=min(detectCores(),nMP))

#C:\LEHOUX\Maquereau\iml-mackerel\00.0_model\Rdata\2022\proj\2025-02-25


filenames <- dir(DateDir, pattern = "")
files <- paste0(DateDir,filenames)
runlist <- lapply(files, function(x) {print(x);get(load(x))})
n <-  gsub(pattern = ".IEindepcan.IEindepus.Rdata",replacement = "",x = filenames)
n <- gsub(pattern='proj.MP',replacement='',x=n)
names(runlist) <-n
class(runlist) <- 'forecastset'
save(runlist, file=paste0('Rdata/',my.year,'/projvalid.Rdata'))


getIndex <- function(x){
    fut <- data.frame(Estimate=do.call(rbind, lapply(x, function(xx)median(xx$index))),
                      Low=unlist(lapply(x, function(xx)quantile(xx$index,0.025, na.rm=T))),
                      High=unlist(lapply(x, function(xx)quantile(xx$index,0.975, na.rm=T))),
                      year=as.numeric(rownames(attr(x,"tab"))),
                      period="Future",
                      rec=attr(x,"parameters")$rec.meth)
    f <- attr(x,"fit")
    pass <- data.frame(cbind(f$data$aux,exp(f$data$logobs)))
    pass <- pass[pass$fleet==3,c(1,4)]
    names(pass)[2]="Estimate"
    pass$period="Passed"
    ret <- rbind.fill(fut,pass)
    return(ret)
}

fut<- lapply(runlist,getIndex)
getIndex(x=runlist[[1]])
getIndex(x=runlist[[2]])
# extract predictions
#fut <- data.frame(Estimate=do.call(rbind, lapply(projb, function(xx)median(xx$index))),
 #                   Low=unlist(lapply(projb, function(xx)quantile(xx$index,0.025))),
    #                High=unlist(lapply(projb, function(xx)quantile(xx$index,0.975))),
  ##                  year=as.numeric(rownames(attr(projb,"tab"))),
      #              period="Future")
#

f <- attr(projb,"fit")
pass <- data.frame(cbind(f$data$aux,exp(f$data$logobs)))
pass <- pass[pass$fleet==3,c(1,4)]
names(pass)[2]="Estimate"
pass$period="Passed"

# get new true observations
#à changer pour tep.dat
NEW<- read.ices("data/2024/tep.dat")$TEP %>% as.matrix() %>%  as.data.frame() %>%  rename(TEP=`-1`)  %>%  
    dplyr::mutate(Estimate=TEP* 10^9,
                  period="NEW",
                  year=1979:2024) %>%  
    dplyr::select(year, Estimate, period) %>% 
    dplyr::filter(!year %in% c(1991, 1999))

#NEW <- data.frame(year=c(2023, 2024),Estimate=rep(35860523284,2),period="NEW") # new observations

# compare predictions with reality
this <- rbind.fill(pass,fut[-1,],NEW)

pt<- ggplot(this,aes(x=year,y=Estimate*1000,col=period))+
    geom_point()+
    geom_line()+
    geom_errorbar(aes(ymin=Low*1000,ymax=High*1000))+
    theme(legend.position = "none")+
    labs(x="Year",y="TEP")+
    scale_color_manual(values=c("grey","red","black"))

pt + scale_x_continuous(limits=c(2000,2026), name = "Ann\u00E9e | Year") +
    scale_y_continuous(limits=c(0, 3e14),"Production total d'oeufs \n Total egg production ")
ggsave(paste0("img/",my.year,"/projvalid/Projections2022validation.png"), width = 7, height = 4, unit = "in", dpi = 600)

