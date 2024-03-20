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
year <- 2022
dir <- paste0('data/',year,'/')

load(file=paste0('Rdata/',year,'/input/dat.Rdata'))
load(file=paste0('Rdata/',year,'/input/conf.Rdata'))
load(file=paste0('Rdata/',year,'/input/par.Rdata'))

Mrange <- seq(0.15,1,0.01)
n <- length(Mrange)

mydats <- replicate(n,dat,simplify=FALSE)

for(i in 1:n){
    mydats[[i]]$natMor[] <- Mrange[i]
}


#################################################################################################################
########### fit model ###########################################################################################
#################################################################################################################

for(x in 37:n){
    print(Mrange[x])
    fit <- tryCatch(ccam.fit(mydats[[x]],conf,par,silent = TRUE),error=function(e) e)
    if('ccam' %in% class(fit)) save(fit, file=paste0('Rdata/',year,'/fit_compare/fit.M.',Mrange[x],'.Rdata'))
}

s <- list.files(paste0('Rdata/',year,'/fit_compare/'),'.M.0',full.names = TRUE,)
fits.Mct <- lapply(s,function(x)get(load(x)))
names(fits.Mct) <- gsub(".Rdata","",gsub(paste0('Rdata/',year,'/fit_compare/fit.M.'),"",s))
class(fits.Mct) <- "ccamset"

fits.Mct <- fits.Mct[Mrange<0.31]  ## subset!!!
class(fits.Mct) <- "ccamset"

.wd <- 'img/2022/fit_compare/Mct/'
dir.create(.wd, showWarnings = FALSE)

pars <- partable(fits.Mct)
qs <- pars[pars$par=='logFpar_0',c(4,7)]
qs$fit <- as.numeric(qs$fit)
names(qs)[1]='q'
p <- ggplot(qs, aes(x=fit,y=q/1000))+geom_point()+geom_hline(yintercept=1)+labs(y="Q",x='M')
savepng(p,.wd,"q",c(12,8))

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

savepng(ssb0plot(fits.Mct,ci=FALSE),.wd,"SSB",c(21,13))
savepng(catchplot(fits.Mct,ci=FALSE),.wd,"catch",c(21,13))
savepng(recplot(fits.Mct,ci=FALSE),.wd,"recruitment",c(21,13))
savepng(fitplot(fits.Mct,type='AIC',n=FALSE)+scale_x_discrete(breaks = seq(0.15,1,0.01))+labs(x='M'),.wd,"AIC",c(16,8))
savepng(fitplot(fits.Mct,type='nll',n=FALSE),.wd,"nll",c(16,8))
savepng(fitplot(fits.Mct,type='AIC',n=FALSE)+scale_x_discrete(breaks = seq(0.15,1,0.01))+labs(x='M',y='CIA'),.wd,"AIC_FR",c(16,8))

sub <- fits.Mct[1:46]
class(sub) <- 'ccamset'
savepng(fitplot(sub,type='AIC',n=FALSE)+scale_x_discrete(breaks = seq(0.2,1,0.5)),.wd,"AIC_to0.6",c(16,8))


aic <- ldply(fits.Mct,AIC)
names(aic) <- c('fit','aic')
aic$fit <- as.numeric(aic$fit)
ggplot(aic,aes(x=fit,y=aic))+
    geom_point()+
    labs(x="M",y='AIC')

Mabs <- Mabstable(fits.Mct)
Mabs$fit <- as.numeric(Mabs$fit)
last <- ddply(Mabs[Mabs$year %in% 2011:2022,],c('fit'),summarise,TSB=mean(TSB.Estimate),M=mean(Mabs))
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
    if('ccam' %in% class(fit)) save(fit, file=paste0('Rdata/',year,'/fit_compare/fit.Md.',Mrange[x],'.Rdata'))
}

s <- list.files(paste0('Rdata/',year,'/fit_compare/'),'.Md.0',full.names = TRUE,)
fits.Mctd <- lapply(s,function(x)get(load(x)))
names(fits.Mctd) <- gsub(".Rdata","",gsub(paste0('Rdata/',year,'/fit_compare/fit.Md.'),"",s))
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

aicd <- ldply(fits.Mctd,AIC)
names(aicd) <- c('fit','aicd')
aicd$fit <- as.numeric(aicd$fit)

aicm <- merge(aic,aicd)
names(aicm)[2:3] <- c('Constant M','Age-varying M')

p <- ggplot(melt(aicm,id=c('fit')),aes(x=fit,y=value,col=variable))+
    geom_point(size=1)+
    labs(x="M",y='AIC',col='')+
    scale_color_manual(values=c('orange','darkred'))
savepng(p,.wd,"AIC_both",c(15,8))

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
    guides(size='none')
savepng(p2,.wd,"Mdecreased",c(15,8))

Mabsd <- Mabstable(fits.Mctd)
Mabsd$fit <- as.numeric(Mabsd$fit)

lastd <- ddply(Mabsd[Mabsd$year %in% 2011:2022,],c('fit'),summarise,TSB=mean(TSB.Estimate),M=mean(Mabs))
sb <- round(lastd[lastd==0.91,'M']/1000,0)
ggplot(lastd,aes(x=as.numeric(fit),y=TSB/1000))+
    geom_point()+
    geom_point(aes(y=M/1000),col='red')+
    geom_vline(xintercept=0.91)+
    geom_hline(yintercept = sb)+
    geom_text(x=-Inf,y=sb,label=sb,hjust=-0.1,vjust=-0.1)+
    labs(y='SSB (kt) [2011-2022]',x="M")


options <- c(fits.Mct$`0.51`,fits.Mctd$`0.91`)
class(options) <- "ccamset"
names(options) <- c('constant 0.51','age-varying 0.91')
savepng(plot(options),.wd,"all_compare",c(15,20))
savepng(plot(options,years=2010:2022),.wd,"all_compare_zoom",c(15,8=20))

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
