do.peel <-  function(year.peel=7, year, fit, name="fitBase"){
    
    years_to_remove= c(1968, year: (year-year.peel+1))
    
    
dir <- paste0('data/',year,'/') 

cn <- read.ices(paste0(dir,'cn.dat'))
ct <- read.ices(paste0(dir,'ct.dat'))
ctUSA <- read.ices(paste0(dir,'ctUSA.dat'))
#ctUSA[which(rownames(ctUSA) %in% c("2022","2023", "2024")),"min"] <-  ctUSA[which(rownames(ctUSA) %in% c("2022","2023", "2024")),"min"]*0.1
#ctUSA[,"min"] <-  ctUSA[,"min"]*0.1

ctForeign <- read.ices(paste0(dir,'ctForeign.dat'))
ctunac <- read.ices(paste0(dir,'ctUnaccounted.dat'))
cw <- read.ices(paste0(dir,'cw.dat'))
mo <- read.ices(paste0(dir,'mo.dat'))
nm <- read.ices(paste0(dir,'nm.dat'))

pf <- read.ices(paste0(dir,'pf.dat'))
pm <- read.ices(paste0(dir,'pm.dat'))
sw <- read.ices(paste0(dir,'sw.dat'))
sw0 <- read.ices(paste0(dir,'sw0.dat'))
pfem <- read.ices(paste0(dir,'propFemale.dat'))
fec <- read.ices(paste0(dir,'fec.dat'))

# redefine catch limits (add 20-80% US catch)
ctwusa <- ct                                   # reported landings
ctwusa[,2] <- ctwusa[,1] + ctunac[,1]          # add max missing in Canada
ctwusa[,1] <- ctwusa[,1]*1.10 + ctUSA[,1]*0.2     # lower bound: increase and add US
ctwusa[,2] <- ctwusa[,2] + ctUSA[,1]*0.8          # add us upper
matplot(ctwusa,type='l')                           # quick check

no<- which(row.names(cn) %in% years_to_remove)

survey <- read.ices(paste0(dir,'tep.dat')) 
survey[[1]] <- survey[[1]][!is.na(survey[[1]]),1,drop=FALSE]
survey[[1]] <- survey[[1]][-which(row.names(survey[[1]]) %in% years_to_remove),1,drop=FALSE]
survey[[1]][,1] <- survey[[1]][,1]*10^9  #to verify this in reality should be 10^12, but in the model conversion from kg to t not done, so corrected here instead. 
attr(survey[[1]],'time') <- c(0.47)



dat <- setup.ccam.data(surveys=survey,
                       residual.fleet=cn[-no,],
                       total.catch=ctwusa[-no,],
                       prop.mature=mo[-no,],
                       stock.mean.weight=sw[-no,],
                       stock.start.weight=sw0[-no,],
                       catch.mean.weight=cw[-no,],
                       prop.f=pf[-no,],
                       prop.m=pm[-no,],
                       natural.mortality=nm[-no,],
                       prop.fem=pfem[-no,],
                       fec=fec[-no,])

#identical(dat,dat2) # numeric if no lf, integer if lf
#ldply(1:length(dat),function(x)identical(dat[[x]],dat2[[x]]))

conf <- defcon(dat)
conf$keySel <- matrix(c(0,1,2,3,4,4,4,4,4,4), nrow=nrow(conf$keySel), ncol=ncol(conf$keySel),byrow = T)
conf$keyVarObs[1,]=-1                     
conf$keyVarObs[2,1:9]=c(0,1,2,2,2,2,1,1,1) 
conf$keyVarObs[3,1]=3           
conf$stockRecruitmentModelCode=2 #0: RW, 1: ricker, 2: BH, 3:mean
conf$obsLikelihoodFlag[1]='CE'
conf$keyBiomassTreat[3]=5
conf$fbarRange=c(5,10) #fully recruited fish
par <- defpar(dat,conf)
par[!names(par)%in%c("logN", "logF", "logSW", "logCW", "logitMO", "logNM")]<-fit$pl[!names(fit$pl)%in%c("missing", "logN", "logF", "logSW", "logCW", "logitMO", "logNM")]


retfit <- tryCatch(ccam.fit(dat, conf, par, rm.unidentified=TRUE, paracheck=F),error=function(e) e)
if('ccam' %in% class(retfit)) save(retfit, file=paste0("Rdata/",year,"/retro/",name, "_peel", year.peel,".RData"))

}


do.peel(year.peel=1, year=year, fit=fit, name="fitBase")
#do.peel(year.peel=2, year=year, fit=fit, name="fitBase")# abort!
do.peel(year.peel=3, year=year, fit=fit, name="fitBase")
do.peel(year.peel=4, year=year, fit=fit, name="fitBase")
do.peel(year.peel=5, year=year, fit=fit, name="fitBase")
#do.peel(year.peel=6, year=year, fit=fit, name="fitBase")# abort! trycatch does not work on this
do.peel(year.peel=7, year=year, fit=fit, name="fitBase")





filenames <- dir(paste0('Rdata/',year,'/retro'), pattern = "peel")
files <- c(paste0('Rdata/',year,'/retro/',filenames), paste0('Rdata/',year,'/fit.Rdata'))
r <- lapply(files, function(x) {print(x);get(load(x))})

class(r) <- 'ccamset'
names(r) <- c(paste0("peel", str_extract(filenames, pattern="[:digit:]")), "default")

save(r, file=paste0("Rdata/",year,"/retro/retro_set.RData"))

load(paste0('Rdata/',year,'/fit.Rdata'))
refBase <- ypr(fit)
lrp<- refBase$LRP


load(paste0("Rdata/",year,"/retro/retro_set.RData"))
source(paste0("Rscripts/",year,"/surplus/ssbplot0.R"))
.wd <- paste0('img/',year,'/retro/')


ssbplot0(r, ci=F, language="BI", minyear=1969, year, legend=F, lrp=lrp) 
ggsave(paste0(.wd, "retro_1969_BI.png"), width=15, height=12, unit="cm", dpi=600)   
ssbplot0(r, ci=F, language="EN", minyear=1969, year, legend=F, lrp=lrp)
ggsave(paste0(.wd, "retro_1969_EN.png"), width=15, height=12, unit="cm", dpi=600)   
ssbplot0(r, ci=F, language="FR", minyear=1969, year, legend=F, lrp=lrp)
ggsave(paste0(.wd, "retro_1969_FR.png"), width=15, height=12, unit="cm", dpi=600)   


ssbplot0(r, ci=T, language="BI", minyear=1969, year, legend=F, lrp=lrp)
ggsave(paste0(.wd, "retro_1969_ciBI.png"), width=15, height=12, unit="cm", dpi=600)   
ssbplot0(r, ci=T, language="EN", minyear=1969, year, legend=F, lrp=lrp)
ggsave(paste0(.wd, "retro_1969_ciEN.png"), width=15, height=12, unit="cm", dpi=600)   
ssbplot0(r, ci=T, language="FR", minyear=1969, year, legend=F, lrp=lrp)
ggsave(paste0(.wd, "retro_1969_ciFR.png"), width=15, height=12, unit="cm", dpi=600)   


ssbplot0(r, ci=T, language="BI", minyear=2010, year, legend=F, lrp=lrp)
ggsave(paste0(.wd, "retro_2010_BI.png"), width=15, height=12, unit="cm", dpi=600)   
ssbplot0(r, ci=T, language="EN", minyear=2010, year, legend=F, lrp=lrp)
ggsave(paste0(.wd, "retro_2010_EN.png"), width=15, height=12, unit="cm", dpi=600)   
ssbplot0(r, ci=T, language="FR", minyear=2010, year, legend=F, lrp=lrp)
ggsave(paste0(.wd, "retro_2010_FR.png"), width=15, height=12, unit="cm", dpi=600)   


relative_retro_plot(fits=r, language="BI", year) 
 ggsave(paste0(.wd, "retro_rel_BI.png"), width=15, height=12, unit="cm", dpi=600)   
 relative_retro_plot(fits=r, language="EN", year) 
 ggsave(paste0(.wd, "retro_rel_EN.png"), width=15, height=12, unit="cm", dpi=600)   
 relative_retro_plot(fits=r, language="FR", year) 
 ggsave(paste0(.wd, "retro_rel_FR.png"), width=15, height=12, unit="cm", dpi=600)   
 
 
 

#name="retro",dim=c(16,16),wd=.wd,type=type)
 source(paste0("Rscripts/",year,"/surplus/mohnsrho.R"))
 mohnsrho<- round(mohn.ccamset(fits=r),2)
 write.csv( mohnsrho, paste0("csv/",year,"/mohn.csv"))
 