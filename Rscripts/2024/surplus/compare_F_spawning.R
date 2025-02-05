#################################################################################################################
#*** UPDATE OBEJT
#################################################################################################################
#C lehouxsee the effect of the proportion of F before spawning. results=no effect

dir <- paste0('data/',year,'/')

load(file=paste0('Rdata/',year,'/input/dat.Rdata'))

dat$propF[dat$propF==0.47] <- 0.1

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
load(file=paste0('Rdata/',year,'/fit.Rdata'))
ypr(fit)
#################################################################################################################
########### fit model ###########################################################################################
#################################################################################################################

newfit<- ccam.fit(dat,conf,par,silent=TRUE)

ssbplot(fit)
ssbplot(newfit)

ypr(newfit)

rectable(fit)
rectable(newfit)
