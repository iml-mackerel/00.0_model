#################################################################################################################
#*** Mackerel assessment
#*** Read in data
#*** based on CCAM package
#################################################################################################################

dir <- paste0('data/',year,'/') 
save <- FALSE

cn <- read.ices(paste0(dir,'cn.dat'))
ct <- read.ices(paste0(dir,'ct.dat'))
ctUSA <- read.ices(paste0(dir,'ctUSA.dat'))
ctForeign <- read.ices(paste0(dir,'ctForeign.dat'))
ctunac <- read.ices(paste0(dir,'ctUnaccounted.dat'))
cw <- read.ices(paste0(dir,'cw.dat'))
mo <- read.ices(paste0(dir,'mo.dat'))
nm <- read.ices(paste0(dir,'nm.dat'))
nm[] <- 0.3

# Mgis <- read.ices("data/2018/nm_Gislason.dat")
# rate <- colMeans(Mgis[,-1]/Mgis[,-ncol(Mgis)])
# for(r in 2:10) nm[,r] <-   nm[,r-1]*rate[r-1]

pf <- read.ices(paste0(dir,'pf.dat'))
pm <- read.ices(paste0(dir,'pm.dat'))
sw <- read.ices(paste0(dir,'sw.dat'))
sw0 <- read.ices(paste0(dir,'sw0.dat'))
survey <- read.ices(paste0(dir,'tep.dat')) # survey with NO 2022!!
survey[[1]] <- survey[[1]][!is.na(survey[[1]]),1,drop=FALSE]
survey[[1]][,1] <- survey[[1]][,1]*10^9  # this in reality should be 10^9, but in the model conversion from kg to t not done, so corrected here instead. 
attr(survey[[1]],'time') <- c(0.47)
pfem <- read.ices(paste0(dir,'propFemale.dat'))
fec <- read.ices(paste0(dir,'fec.dat'))

# redefine catch limits (add 25-50% US catch)
ctwusa <- ct                                   # reported landings
ctwusa[,2] <- ctwusa[,1] + ctunac[,1]          # add max missing in Canada
ctwusa[,1] <- ctwusa[,1]*1.10 + ctUSA[,1]*0.2     # lower bound: increase and add US
ctwusa[,2] <- ctwusa[,2] + ctUSA[,1]*0.8          # add us upper
matplot(ctwusa,type='l')                           # quick check

dat <- setup.ccam.data(surveys=survey,
                       residual.fleet=cn[-1,],
                       total.catch=ctwusa[-1,],
                       prop.mature=mo[-1,],
                       stock.mean.weight=sw[-1,],
                       stock.start.weight=sw0[-1,],
                       catch.mean.weight=cw[-1,],
                       prop.f=pf[-1,],
                       prop.m=pm[-1,],
                       natural.mortality=nm[-1,],
                       prop.fem=pfem[-1,],
                       fec=fec[-1,])

#identical(dat,dat2) # numeric if no lf, integer if lf
#ldply(1:length(dat),function(x)identical(dat[[x]],dat2[[x]]))

conf <- defcon(dat)
conf$keySel <- matrix(c(0,1,2,3,4,4,4,4,4,4), nrow=nrow(conf$keySel), ncol=ncol(conf$keySel),byrow = T)
conf$keyVarObs[1,]=-1                     
conf$keyVarObs[2,1:9]=c(0,1,2,2,2,2,2,2,1) 
conf$keyVarObs[3,1]=3           
conf$stockRecruitmentModelCode=2 #0: RW, 1: ricker, 2: BH, 3:mean
conf$obsLikelihoodFlag[1]='CE'
conf$keyBiomassTreat[3]=5
conf$fbarRange=c(5,10) #fully recruited fish

par <- defpar(dat,conf)


if(save){
    wdrdat <-paste0('Rdata/',year,'/input/')
    dir.create(wdrdat,showWarnings = F,recursive = T)
    
    save(dat,file=paste0(wdrdat,'dat.Rdata'))
    save(conf,file=paste0(wdrdat,'conf.Rdata'))
    save(par,file=paste0(wdrdat,'par.Rdata'))
}

