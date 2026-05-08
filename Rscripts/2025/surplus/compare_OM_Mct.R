#################################################################################################################
#*** Mackerel MSE
#*** Canadian mackerel (DFO, 2020)
#*** COMPARE Different M in operating models
#################################################################################################################

# function that should be integrated in CCAM
##' Mabs table
##' @param  x...
##' @param ... extra arguments not currently used
##' @details ...
##' @export
Mabstable <-function(x, ...){
    UseMethod("Mabstable")
}
##' @rdname faytable
##' @method Mabstable ccam
##' @export
Mabstable.ccam <- function(x){
    sb <- tsbtable(x)
    N. <- ntable(x)
    M. <- x$data$natMor
    F. <- faytable(x)
    Z. <- F.+M.
    Mabs <- (M./Z.)*(1-exp(-Z.))*N.*x$data$stockMeanWeight # but don't really know the WAA of fish consumed!
    sb$Mabs <- rowSums(Mabs)
    names(sb)[1:3] <- paste0('TSB.',names(sb)[1:3])
    return(sb)
}
##' @rdname Mabstable
##' @method Mabstable ccamset
##' @export
Mabstable.ccamset <- function(x){
    na <- 1:length(x)
    nm <- names(x)
    tabs <- lapply(na,function(i) {
        tab <- Mabstable(x[[i]])
        if(is.null(nm)) tab$fit <- as.factor(i) else tab$fit <- nm[i]
        return(tab)})
    ret <- do.call('rbind',tabs)
    rownames(ret) <- 1:nrow(ret)
    ret <- data.frame(ret)
    return(ret)
}




#################################################################################################################
########### READ IN DATA ########################################################################################
#################################################################################################################
dir <- paste0('data/',year,'/')

load(file=paste0('Rdata/',year,'/input/dat.Rdata'))
load(file=paste0('Rdata/',year,'/input/conf.Rdata'))
load(file=paste0('Rdata/',year,'/input/par.Rdata'))

Mrange <- seq(0.15,0.4,0.01)
n <- length(Mrange)

mydats <- replicate(n,dat,simplify=FALSE)

for(i in 1:n){
    mydats[[i]]$natMor[] <- Mrange[i]
}


#################################################################################################################
########### fit model ###########################################################################################
#################################################################################################################

for(x in 1:n){
    print(Mrange[x])
    fit <- tryCatch(ccam.fit(mydats[[x]],conf,par,silent = TRUE),error=function(e) e)
    if('ccam' %in% class(fit)) save(fit, file=paste0('Rdata/',year,'/sensitivity/fit.M.',Mrange[x],'.Rdata'))
}

s <- list.files(paste0('Rdata/',year,'/sensitivity/'),'.M.0',full.names = TRUE,)
fits.Mct <- lapply(s,function(x)get(load(x)))
names(fits.Mct) <- gsub(".Rdata","",gsub(paste0('Rdata/',year,'/sensitivity/fit.M.'),"",s))
class(fits.Mct) <- "ccamset"

#fits.Mct <- fits.Mct[Mrange<0.31]  ## subset!!!
class(fits.Mct) <- "ccamset"

.wd <- paste0('img/',my.year,'/sensitivity/Mct/')
dir.create(.wd, showWarnings = FALSE)


fref <- ldply(fits.Mct,function(x){
    r=ypr(x,Flimit = 5)
    f40=r$f40
    lrp=r$LRP
    sb=ssbtable(x)
    ratio=tail(sb,1)[,1]/lrp
    return(c(f40=f40,lrp=lrp,ssb=tail(sb,1)[,1],ratio=ratio))
    })
p <- ggplot(melt(fref,id='.id'),aes(x=as.numeric(.id),y=value))+geom_point()+facet_wrap(variable~.,scale='free_y')
savepng(p,.wd,"reftest",c(21,16))
source(paste0("Rscripts/",year,"/surplus/ssbplot0.R"))
savepng(ssb0plot(fits.Mct,ci=FALSE),.wd,"SSB",c(21,13))
savepng(catchplot(fits.Mct,ci=FALSE),.wd,"catch",c(21,13))
savepng(recplot(fits.Mct,ci=FALSE),.wd,"recruitment",c(21,13))
savepng(fitplot(fits.Mct,type='AIC',n=FALSE)+scale_x_discrete(breaks = seq(0.15,1,0.01))+labs(x='M'),.wd,"AIC",c(16,8))
savepng(fitplot(fits.Mct,type='nll',n=FALSE),.wd,"nll",c(16,8))
savepng(fitplot(fits.Mct,type='AIC',n=FALSE)+scale_x_discrete(breaks = seq(0.15,1,0.01))+labs(x='M',y='CIA'),.wd,"AIC_FR",c(16,8))

ssbplot0(fits.Mct, minyear=2010, year=year, legend=T)
ggsave(paste0(.wd,"ssb_noci2010.png"), width=6, height=5, units="in", bg="white")
ssbplot0(fits.Mct, minyear=1969, year=year, legend=T)
ggsave(paste0(.wd,"ssb_noci1969.png"), width=6, height=5, units="in", bg="white")


s <- list.files(paste0('Rdata/',year,'/sensitivity/'),'.M.0',full.names = TRUE,)
fits.Mct2 <- lapply(s,function(x)get(load(x)))
names(fits.Mct2) <- gsub(".Rdata","",gsub(paste0('Rdata/',year,'/sensitivity/fit.M.'),"",s))
class(fits.Mct2) <- "ccamset"

fits.Mct.test <- fits.Mct2[Mrange > 0.27 & Mrange<0.31]
class(fits.Mct.test) <- "ccamset"
ssbplot0(fits.Mct.test, minyear=1969, year=year, legend=T)
ggsave(paste0(.wd,"ssb_noci2010_0.28_0.3.png"), width=6, height=5, units="in", bg="white")

Mabs <- Mabstable(fits.Mct)
Mabs$fit <- as.numeric(Mabs$fit)
last <- ddply(Mabs[Mabs$year %in% 2011:my.year,],c('fit'),summarise,TSB=mean(TSB.Estimate),M=mean(Mabs))
sb <- round(last[last==0.30,'M']/1000,0)
ggplot(last[last$fit %in% seq(0.15,0.8,0.01),],aes(x=as.numeric(fit),y=TSB/1000))+
    geom_point()+
    geom_point(aes(y=M/1000),col='red')+
    geom_vline(xintercept=0.51)+
    geom_hline(yintercept = 27)+
    geom_text(x=-Inf,y=sb,label=sb,hjust=-0.1,vjust=-0.1)+
    labs(y='TSB (kt) [2011-2022]',x="M constant")

#################################################################################################################
########### fit model: age-varying M  ###########################################################################
#################################################################################################################
.wd <- paste0('img/',my.year,'/sensitivity/Md/')
dir.create(.wd, showWarnings = FALSE)


mydatsd <- replicate(n,dat,simplify=FALSE)

Mgis <- read.ices("data/2018/nm_Gislason.dat")
rate <- colMeans(Mgis[,-1]/Mgis[,-ncol(Mgis)])

for(i in 1:n){
    M <- mydatsd[[i]]$natMor
    M[] <-  Mrange[i]
    for(r in 2:10){
        M[,r] <-   M[,r-1]*rate[r-1]
    }
    mydatsd[[i]]$natMor[] <- M
}

for(x in 1:n){
    print(Mrange[x])
    fit <- tryCatch(ccam.fit(mydatsd[[x]],conf,par,silent = TRUE),error=function(e) e)
    if('ccam' %in% class(fit)) save(fit, file=paste0('Rdata/',year,'/sensitivity/fit.Md.',Mrange[x],'.Rdata'))
}

s <- list.files(paste0('Rdata/',year,'/sensitivity/'),'.Md.0',full.names = TRUE,)
fits.Mctd <- lapply(s,function(x)get(load(x)))
names(fits.Mctd) <- gsub(".Rdata","",gsub(paste0('Rdata/',year,'/sensitivity/fit.Md.'),"",s))
class(fits.Mctd) <- "ccamset"

frefd <- ldply(fits.Mctd,function(x){
    r=ypr(x,Flimit = 5)
    f40=r$f40
    lrp=r$LRP
    sb=ssbtable(x)
    ratio=tail(sb,1)[,1]/lrp
    return(c(f40=f40,lrp=lrp,ssb=tail(sb,1)[,1],ratio=ratio))
})
p <- ggplot(melt(frefd,id='.id'),aes(x=as.numeric(.id),y=value))+geom_point()+facet_wrap(variable~.,scale='free_y')
savepng(p,.wd,"reftestd",c(21,16))

savepng(ssb0plot(fits.Mctd,ci=FALSE),.wd,"SSBd",c(21,13))
savepng(catchplot(fits.Mctd,ci=FALSE),.wd,"catchd",c(21,13))
savepng(recplot(fits.Mctd,ci=FALSE),.wd,"recruitmentd",c(21,13))
savepng(fitplot(fits.Mctd,type='AIC',n=FALSE)+scale_x_discrete(breaks = seq(0.2,1,0.1)),.wd,"AICd",c(16,8))
savepng(fitplot(fits.Mctd,type='nll',n=FALSE),.wd,"nlld",c(16,8))


ssbplot0(fits.Mctd, minyear=2010, year=year, legend=T)
ggsave(paste0(.wd,"ssb_noci2010.png"), width=6, height=5, units="in", bg="white")
ssbplot0(fits.Mctd, minyear=1969, year=year, legend=T)
ggsave(paste0(.wd,"ssb_noci1969.png"), width=6, height=5, units="in", bg="white")


aicd <- ldply(fits.Mctd,AIC)
names(aicd) <- c('fit','aicd')
aicd$fit <- as.numeric(aicd$fit)

aic <- ldply(fits.Mct,AIC)
names(aic) <- c('fit','aic')
aic$fit <- as.numeric(aic$fit)

aicm <- merge(aic,aicd)
names(aicm)[2:3] <- c('M constante | Constant M',"M variable à l'âge | Age-varying M")

p <- ggplot(melt(aicm,id=c('fit')),aes(x=fit,y=value,col=variable))+
    geom_point(size=1)+
    labs(x="M",y='AIC', col="")+
    scale_color_manual(values=c('orange','darkred')) + theme(legend.position = "bottom")
savepng(p,.wd,"AIC_both",c(12,12))

p <- ggplot(melt(aicm,id=c('fit')),aes(x=fit,y=value,col=variable))+
    geom_point(size=1)+
    labs(x="M",y='AIC',col='')+
    scale_color_manual(values=c('orange','darkred'))+
    scale_x_continuous(limits=c(0.15,0.30))
savepng(p,.wd,"AIC_both_truncated",c(15,8))

Ms <- lapply(mydatsd,function(x) x$natMor[1,])
Ms <- do.call('rbind',Ms)
dimnames(Ms) <- list(M=Mrange,age=1:10)
Ms <- melt(Ms)
Ms$select <- Ms$M==0.91
p2 <- ggplot(Ms,aes(x=age,y=value,col=M,group=M))+
    geom_line(aes(size=select))+
    scale_color_viridis_c()+labs(y='M',x='Age')+
    scale_x_continuous(breaks = 1:10)+
    scale_size_manual(values=c(0.3,2))+
    guides(size='none') +theme(legend.position = "bottom", legend.key.width = unit(1.5, "cm"))
savepng(p2,.wd,"Mdecreased",c(12,12))

Mabsd <- Mabstable(fits.Mctd)
Mabsd$fit <- as.numeric(Mabsd$fit)

#################################################################################################################
########### compare consumption  ###########################################################################
#################################################################################################################
fit <- fits.Mct$`0.3`
logobs <- data.frame(cbind(fit$data$aux,fit$data$logobs))
catch <- logobs[logobs$fleet==1,]
catch <- data.frame(year=catch$year,catch.low=exp(catch$aux1),catch.high=exp(catch$aux2))
catch$catch.med <- apply(catch[2:3],1,mean)

# estimated predation
repo <- "https://github.com/iml-mackerel/99.0_consumption/blob/main/"
consum <- read.table(url(paste0(repo,'output/consum.txt',"?raw=true")),header = T)
names(consum)[2:4] <- c("cons.low",'cons.med','cons.high')

# ratio
rem <- merge(catch,consum,all.x=TRUE)

options2 <- c(fits.Mct$`0.2`,fits.Mct$`0.3`)
class(options2) <- "ccamset"
names(options2) <- c('constant 0.2','constant 0.3')
Mabscomp <-Mabstable(options2)

rem <- merge(rem,Mabscomp,id='year')

ggplot(rem,aes(x=year))+
    geom_ribbon(aes(ymin=TSB.Low,ymax=TSB.High),fill='darkgrey')+
    geom_line(aes(y=TSB.Estimate))+
    geom_line(aes(y=cons.med),col='orange')+
    geom_ribbon(aes(ymin=catch.low,ymax=catch.high),fill='blue',alpha=0.5)+
    facet_wrap(fit~.,ncol=1)+
    scale_x_continuous(expand=c(0,0))+
    labs(y='Biomass',x='Year')

p <- ggplot(rem[rem$year %in% 2010:2022,],aes(x=year))+
    geom_ribbon(aes(ymin=TSB.Low/1000,ymax=TSB.High/1000),fill='darkgrey')+
    geom_line(aes(y=TSB.Estimate/1000),size=1.5)+
    geom_ribbon(aes(ymin=cons.low/1000,ymax=cons.high/1000),fill='orange',alpha=0.5)+
    geom_line(aes(y=cons.med/1000),col='orange',size=1.5)+
    geom_ribbon(aes(ymin=catch.low/1000,ymax=catch.high/1000),fill='blue',alpha=0.5)+
    geom_line(aes(y=Mabs/1000),col='red',size=1.5)+
    facet_wrap(fit~.,ncol=1)+
    scale_x_continuous(expand=c(0,0))+
    labs(y='Biomass (t)',x='Year')

savepng(p,.wd,"M_scale_low",c(15,20))

pr <- lapply(options2,forecast,catchval=rep(4000,3))
class(pr) <- 'forecastset'
plot(pr,year=2010:2030)


#######retrospective patterns with lower M. ######
load(paste0("Rdata/",year,"/sensitivity/fit.M.0.2.Rdata"))

do.peel <-  function(year.peel=7, year, fit, name="fitBase"){
    dir <- paste0('data/',year,'/') 
    
    ## years to remove
    years_to_remove <- c(
        1968,
        seq(year, year - year.peel + 1, by = -1)
    )
    
    
    no<- which(fit$data$years %in% years_to_remove)
    
    idx <- fit$data$aux[, "fleet"] %in% 3
    TEP <- exp(fit$data$logobs[idx, "aux1"])
    years <- as.character(fit$data$aux[idx, "year"])
    
    TEP <- matrix(TEP, ncol = 1)
    dimnames(TEP) <- list(years, "-1")
    TEP=TEP[ !rownames(TEP) %in% as.character(years_to_remove) , , drop = FALSE ]
    attr(TEP, "time") <- 0.47
    
     cn <- read.ices(paste0(dir,'cn.dat'))
    
    ## 1. Select fleet
    idx <- fit$data$aux[, "fleet"] %in% 1
    # 2. Extract min and max values
    min_vals <- exp(fit$data$logobs[idx, "aux1"])
    max_vals <- exp(fit$data$logobs[idx, "aux2"])
    # 3. Extract years as character
    years <- as.character(fit$data$aux[idx, "year"])
    ## 4. Build the matrix (years × 2)
    ctwusa <- cbind(
        min = min_vals,
        max = max_vals
    )
    ## 5. Assign dimnames explicitly
    dimnames(ctwusa) <- list(
        years,
        c("min", "max")
    )
    mo<- fit$data$propMat
    sw<- fit$data$stockMeanWeight
    sw0<- fit$data$stockStartWeight
    cw<- fit$data$catchMeanWeight
    pf <- fit$data$propF
    pm <- fit$data$propM
    nm <-  fit$data$natMor
    pfem <-  fit$data$propFemale
    fec <-  fit$data$fec
    
    dat <- setup.ccam.data(surveys=TEP,
                           residual.fleet=cn[-which(row.names(cn) %in% as.character(years_to_remove)),],
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
    conf$keyVarObs[2,1:9]=c(0,1,2,2,2,2,2,1,1) 
    conf$keyVarObs[3,1]=3           
    conf$stockRecruitmentModelCode=2 #0: RW, 1: ricker, 2: BH, 3:mean
    conf$obsLikelihoodFlag[1]='CE'
    conf$keyBiomassTreat[3]=5
    conf$fbarRange=c(5,10) #fully recruited fish
    par <- defpar(dat,conf)
    par[!names(par)%in%c("logN", "logF", "logSW", "logCW", "logitMO", "logNM")]<-fit$pl[!names(fit$pl)%in%c("missing", "logN", "logF", "logSW", "logCW", "logitMO", "logNM")]
    
    
    retfit <- tryCatch(ccam.fit(dat, conf, par, rm.unidentified=TRUE, paracheck=F),error=function(e) e)
    if('ccam' %in% class(retfit)) save(retfit, file=paste0("Rdata/",year,"/sensitivity/retro/",name, "_peel", year.peel,".RData"))
    
}


do.peel(year.peel=1, year=year, fit=fit, name="m02")
#do.peel(year.peel=2, year=year, fit=fit, name="m02")# abort! # still not working with m=0.2
do.peel(year.peel=3, year=year, fit=fit, name="m02")
do.peel(year.peel=4, year=year, fit=fit, name="m02")
do.peel(year.peel=5, year=year, fit=fit, name="m02")
#do.peel(year.peel=6, year=year, fit=fit, name="m02")# abort! trycatch does not work on this
do.peel(year.peel=7, year=year, fit=fit, name="m02")





filenames <- dir(paste0('Rdata/',year,'/sensitivity/retro/'), pattern = "peel")
files <- c(paste0('Rdata/',year,'/sensitivity/retro/',filenames), paste0('Rdata/',year,'/sensitivity/fit.M.0.2.Rdata'))
r <- lapply(files, function(x) {print(x);get(load(x))})

class(r) <- 'ccamset'
names(r) <- c(paste0("peel", str_extract(filenames, pattern="[:digit:]")), "default")

save(r, file=paste0("Rdata/",year,"/sensitivity/retro//retro_set.RData"))


load(paste0("Rdata/",year,"//sensitivity/retro/retro_set.RData"))
source(paste0("Rscripts/",year,"/surplus/ssbplot0.R"))
.wd <- paste0('img/',year,'/sensitivity/retro/')


ssbplot0(r, ci=F, language="BI", minyear=1969, year, legend=F)
ggsave(paste0(.wd, "retro_1969_BI.png"), width=15, height=12, unit="cm", dpi=600)   
ssbplot0(r, ci=F, language="EN", minyear=1969, year, legend=F)
ggsave(paste0(.wd, "retro_1969_EN.png"), width=15, height=12, unit="cm", dpi=600)   
ssbplot0(r, ci=F, language="FR", minyear=1969, year, legend=F)
ggsave(paste0(.wd, "retro_1969_FR.png"), width=15, height=12, unit="cm", dpi=600)   


ssbplot0(r, ci=T, language="BI", minyear=1969, year, legend=F)
ggsave(paste0(.wd, "retro_1969_ciBI.png"), width=15, height=12, unit="cm", dpi=600)   
ssbplot0(r, ci=T, language="EN", minyear=1969, year, legend=F)
ggsave(paste0(.wd, "retro_1969_ciEN.png"), width=15, height=12, unit="cm", dpi=600)   
ssbplot0(r, ci=T, language="FR", minyear=1969, year, legend=F)
ggsave(paste0(.wd, "retro_1969_ciFR.png"), width=15, height=12, unit="cm", dpi=600)   


ssbplot0(r, ci=T, language="BI", minyear=2010, year, legend=F)
ggsave(paste0(.wd, "retro_2010_BI.png"), width=15, height=12, unit="cm", dpi=600)   
ssbplot0(r, ci=T, language="EN", minyear=2010, year, legend=F)
ggsave(paste0(.wd, "retro_2010_EN.png"), width=15, height=12, unit="cm", dpi=600)   
ssbplot0(r, ci=T, language="FR", minyear=2010, year, legend=F)
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
write.csv( mohnsrho, paste0("csv/",year,"/mohn_sensitivity_retrom02.csv"))




