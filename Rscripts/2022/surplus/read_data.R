#################################################################################################################
#*** Mackerel assessment
#*** Read in data
#*** based on CCAM package
#################################################################################################################

dir <- paste0('data/',year,'/')

cn <- read.ices(paste0(dir,'cn.dat'))
# cn <- cn[-nrow(cn),] # remoe last year data
ct <- read.ices(paste0(dir,'ct.dat'))
ctUSA <- read.ices(paste0(dir,'ctUSA.dat'))
ctForeign <- read.ices(paste0(dir,'ctForeign.dat'))
cw <- read.ices(paste0(dir,'cw.dat'))
dw <- read.ices(paste0(dir,'dw.dat'))
lf <- read.ices(paste0(dir,'lf.dat'))
lw <- read.ices(paste0(dir,'lw.dat'))
mo <- read.ices(paste0(dir,'mo.dat'))
mo <- smoothmatrix(mo,subset=8:nrow(mo),max=1,plot=FALSE)
nm <- read.ices(paste0(dir,'nm.dat'))   #nm[]<- 0.2
pf <- read.ices(paste0(dir,'pf.dat'))
pm <- read.ices(paste0(dir,'pm.dat'))
sw <- read.ices(paste0(dir,'sw.dat'))
sw0 <- read.ices(paste0(dir,'sw0.dat'))
# surveys <- read.ices(paste0(dir,'survey.dat'))
# surveys[[1]] <- surveys[[1]][!is.na(surveys[[1]]),1,drop=FALSE]
# attr(surveys[[1]],'time') <- c(0.47)
survey <- read.ices(paste0(dir,'tep.dat'))
survey[[1]] <- survey[[1]][!is.na(survey[[1]]),1,drop=FALSE]
survey[[1]][,1] <- survey[[1]][,1]*10^12
attr(survey[[1]],'time') <- c(0.47)
pfem <- read.ices(paste0(dir,'propFemale.dat'))
fec <- read.ices(paste0(dir,'fec.dat'))

# redefine catch limits (add 25-50% US catch)
ctwusa <- ct
ctwusa[,1] <- ct[,1]*1.10 + ctUSA[,1]*0.25
ctwusa[,2] <- ct[,2] + ctUSA[,1]*0.50

dat <- setup.ccam.data(surveys=survey,
                       residual.fleet=cn[-nrow(cn),],
                       total.catch=ctwusa,
                       prop.mature=mo,
                       stock.mean.weight=sw,
                       stock.start.weight=sw0,
                       catch.mean.weight=cw,
                       dis.mean.weight=dw,
                       land.mean.weight=lw,
                       prop.f=pf,
                       prop.m=pm,
                       natural.mortality=nm,
                       land.frac=lf,
                       prop.fem=pfem,
                       fec=fec)
#dat$aux[,1] <- dat$aux[,1]+1967 # oddity in setup.ccam.data (lazy solution)

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

wdrdat <-paste0('Rdata/',year,'/input/')
dir.create(wdrdat,showWarnings = F,recursive = T)

#save(dat,file=paste0(wdrdat,'dat.Rdata'))
#save(conf,file=paste0(wdrdat,'conf.Rdata'))
#save(par,file=paste0(wdrdat,'par.Rdata'))
