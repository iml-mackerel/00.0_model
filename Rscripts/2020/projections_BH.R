#################################################################################################################
#*** Mackerel assessment projections
#*** Northern population(contingent) of the Northwest Atlantic stock (DFO, 2021)
#*** based on CCAM package
#*** Uses the CCAM::forecast() function and multi.forecast allows multiple OMs to be run (in parallel or not)
#*** see https://github.com/elisvb/CCAM/blob/master/R/forecast.R
#################################################################################################################

ny = 3        # number of years to forecast
nosim = 2000  # number of simulations

#--------------------- projections ------------------------------------------
# load('Rdata/2020/fit.Rdata') # Load the fitted CCAM object of choice

proj <- list(fit = fit, # fitted CCAM object
             nosim = nosim, 
             OMlabel = 'OMassess',
             ave.years = tail(fit$data$years, 10),  
             rec.years = 1968:2020,                 # years of recruitment on which projections will be based. 1968 removed
             rec.meth = 1,                          # options are 1 (Beverton Holt lag 2 autocorrelation); 2 (mean lag 1 autocorrelation); 3 (original SAM method, no autocorrelation); 4 (sample trailing years of recruitment); 5 and 6 are experimental and gave horrible results (no autocorrelation)
             UL.years = tail(fit$data$years, 25),   # years from which to draw upper/lower limits of censured catch bounds
             deadzone = 1000,                       # rough guess at a minimum viable population. if ssb goes below this it keeps the pop dead even if samples from recruitment create a zombie population
             Flim = 2.5)

# --------------------- base MPs ----------------------------------------------
# Management procedure or harvest control rule etc. the name isn't important for the model
nMP = 10

MP1 <- list(MPlabel = 'MP1',
            IE = NULL,
            capLower = 0,
            TAC.base = 8000) # last year

copy(x = MP1,n = nMP,name = c('MP'))

avail('MP')
#MP1$catchval <- rep(0,ny)
MP2$catchval <- rep(0,ny)
MP3$catchval <- rep(2000,ny)
MP4$catchval <- rep(4000,ny)
MP5$catchval <- rep(6000,ny)
MP6$catchval <- rep(8000,ny)
MP7$catchval <- rep(10000,ny)
# MP8$catchval <- rep(12000,ny)
# MP9$catchval <- rep(14000,ny)
# MP10$catchval <- rep(16000,ny)

#******************************************************************************
#************* Create all forecasting scenarios *******************************
#******************************************************************************

# Not sure how to modify ie.R without commiting on github and then re-installing. Forums say I can modify a function for the duration of a session with utils::edit()
IEindep2019 <- utils::edit(IEindep2019) # init set at 2000 and decrease by rate of 83% as per conversation with EVB and SP

IEs <- c("IEindep2019") # needs to be changed (ie.r in CCAM)
OMs <- c('proj')
MPs <- paste0('MP',2:nMP)

scenmat <- expand.grid(OM = OMs,MP = MPs, IE = IEs)
scenmat <- data.frame(apply(scenmat,2,as.character))

scen.list <- lapply(split(scenmat,1:nrow(scenmat)),function(x){
    OMx <- get(as.character(x[1,1]))
    MPx <- get(as.character(x[1,2]))
    MPx$IE <- c(as.character(x[1,3]),'IEdep2550')  # IEdep2550 = US unaccounted for catch scenario 
    c(OMx,MPx)
})
scenmat$IE <- lapply(scen.list,function(x) paste(x$IE,collapse = '.'))
scennames <- apply(scenmat,1,paste,collapse = ".")
names(scen.list) <- scennames

#******************************************************************************
#************* Run all forecasting scenarios **********************************
#******************************************************************************

Date = Sys.Date()

DateDir = paste0("Rdata/",Date,"/")
dir.create(DateDir,showWarnings = FALSE)

year <- 2020
dir <- paste0('data/',year,'/')
ctUSA <- read.ices(paste0(dir,'ctUSA_2020.dat'))

# I have 8 cores; an Intel i7-8650U CPU @ 1.90GHz 2.11 GHz; 16 GB RAM, 64 bit OS. With 7 cores running projections with 10 scenarios takes about 1-2 minutes
multi.forecast(scen.list,DateDir,parallel = FALSE, ncores = 7) # occasionally parallel processing bugs so turn it off if that happens
# takes ~ 1 min to run without parallel

## plots ---------------------------
filenames <- dir(DateDir, pattern = "indep2019")
files <- paste0(DateDir,filenames)
runlist <- lapply(files, function(x) {print(x);get(load(x))})
n <-  gsub(pattern = ".IEindep2019.IEdep2550.Rdata",replacement = "",x = filenames)
n <- gsub(pattern = 'proj.MP',replacement = '',x = n)
names(runlist) <- n
class(runlist) <- 'forecastset'
save(runlist, file = 'Rdata/2020/proj/final/proj_base_2020_BH_full_ts.Rdata')

load(file = 'Rdata/2020/OMs/fit_2020_base_model.Rdata')

refBase <- ypr(fit)
REF <- refBase$f40ssb
LRP <- REF*0.40
USR <- REF*0.80

ssb2020 <- tail(ssbtable(fit), 1)
ssb2020/LRP

IE <-  data.frame(year=1:3,mean=Reduce(function(v, x) .8*v , x=numeric(3),  init=2000, accumulate=TRUE)[-1])
IE$sd <- IE$mean/8

pIE <- ggplot(IE,aes(x=year,y=mean))+
    geom_ribbon(aes(ymin=mean-sd*2,ymax=mean+sd*2),alpha=0.2)+
    geom_line()+scale_x_continuous(expand=c(0,0))+
    labs(y='Unaccounted-for Canadian catch (t)',x='Year') + 
    scale_x_continuous(n.breaks = 3) +
    theme_minimal(base_size = 14)

saveplot(pIE,name="IE_base",dim=c(12,10),wd='img/2020/proj')

r <- runlist[[6]]
ies <- attr(r,'IE')
nam <- c('Canada','U.S.A.')
ies <- lapply(1:2,function(x){
    d <- ies[[x]]
    colnames(d) <- 1:ncol(d)
    y <- data.frame(melt(d,varnames = c('sim','Year')))
    y$name <- nam[x]
    y
})
ies <- do.call('rbind',ies)


# process implementation error figure
pIEbox1 <- ggplot(ies,aes(x=factor(Year),y=value))+geom_boxplot(fill = "grey")+
    facet_wrap(~name,ncol=1,scale='free_y')+labs(y='Catch (t)',x='Year') +
    theme_minimal(base_size = 12) + 
    scale_x_discrete(labels=c("1" = "2021", "2" = "2022", "3" = "2023")) 

saveplot(pIEbox1,name="IEbox_base_BH",dim=c(6,8),wd='img/2020/proj')



# source the plot.R file from CCAM
# source("./Rscripts/2020/surplus/")
# source('Rscripts/2020/surplus/')
prec <- plotit(rectable(fit),ylab='Recruitment (millions)',year=c(1969:2023),scale=1000000)
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

projres <- diamondplot(runlist,what='TAC',year=2021,data=TRUE)[,c('MP','y')]
names(projres)[2] = 'TAC2021'
projres$TAC2022 <- diamondplot(runlist,what='TAC',year=2022,data=TRUE)$y
projres$TAC2023 <- diamondplot(runlist,what='TAC',year=2023,data=TRUE)$y
projres$LRP2022 <- diamondplot(runlist,what='probCZ',year=2022,data=TRUE)$y
projres$LRP2023 <- diamondplot(runlist,what='probCZ',year=2023,data=TRUE)$y
projres$ssblrp2022 <- round(diamondplot(runlist,what='ssb',year=2022,ratio = TRUE,data=TRUE)$y,2)
projres$ssblrp2023 <- round(diamondplot(runlist,what='ssb',year=2023,ratio = TRUE,data=TRUE)$y,2)
mc <- ddply(ies,c('name'),function(x) quantile(x$value,c(0.05,0.95)))
mc <- data.frame(matrix(as.numeric(cbind(mc[1,2:3],mc[2,2:3])),nrow=nrow(projres),ncol=4,byrow=TRUE))
colnames(mc) <- c('Canada 5%','Canada 95%','US 5%','US 95%')
projres <- cbind(projres,round(mc,0))

grow <- ldply(runlist,probgrowth)
grow$.id <- as.numeric(grow$.id)
projres$grow <- grow[order(grow$.id),2]
projres <- projres[c(1:6),]
projres

write.csv(projres, file = "proj_mean_10y.csv",row.names = FALSE)
write.csv(projres, file = "proj_BH_full_ts.csv",row.names = FALSE)
