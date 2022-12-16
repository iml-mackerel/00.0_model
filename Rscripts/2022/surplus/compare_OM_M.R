#################################################################################################################
#*** Mackerel MSE
#*** Canadian mackerel (DFO, 2018)
#*** COMPARE Different M in operating models
#################################################################################################################

year <- 2022

.wd <- paste0('img/',year,'/fit_compare/M/')

#################################################################################################################
########### some new functions (not in CCAM yet) ################################################################
#################################################################################################################

Mabstable <-function(x, ...){
    UseMethod("Mabstable")
}

Mabstable.ccam <- function(x){
    ssb <- ssbtable(x)
    N. <- ntable(x)
    M. <- x$data$natMor
    F. <- faytable(x)
    Z. <- F.+M.
    Mabs <- (M./Z.)*(1-exp(-Z.))*N.*x$data$stockStartWeight
    ssb$Mabs <- rowSums(Mabs)
    names(ssb)[1:3] <- paste0('SSB.',names(ssb)[1:3])
    return(ssb)
}

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
########### REDO M ##############################################################################################
#################################################################################################################

# observed catch (take middle between bounds)
load(file=paste0("Rdata/",year,"/fit.Rdata"))

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
rem$ratio <- with(rem,cons.med/catch.med)

# Z
fay <- faytable(fit)
may <- fit$data$natMor
zay <- fay+may

# new M and theoretical f
maynew <- sweep(sweep(zay,1,rem$ratio,'*'),1,(1+rem$ratio),"/")
faynew <- sweep(zay,1,(1+rem$ratio),"/")
zaynew <- faynew+maynew # validation that identical to before

savepng(
grid.arrange(
    ggplot(melt(may),aes(x=Var1,y=value,col=as.factor(Var2)))+geom_line()+labs(x='Year',y='M',col='')+scale_color_viridis_d(),
    ggplot(melt(maynew),aes(x=Var1,y=value,col=as.factor(Var2)))+geom_line()+labs(x='Year',y='Mnew',col='')+scale_color_viridis_d(),
    ggplot(melt(fay),aes(x=Var1,y=value,col=as.factor(Var2)))+geom_line()+labs(x='Year',y='F',col='')+scale_color_viridis_d(),
    ggplot(melt(faynew),aes(x=Var1,y=value,col=as.factor(Var2)))+geom_line()+labs(x='Year',y='Fnew',col='')+scale_color_viridis_d(),
    ggplot(melt(zay),aes(x=Var1,y=value,col=as.factor(Var2)))+geom_line()+labs(x='Year',y='Z',col='')+scale_color_viridis_d(),
    ggplot(melt(zaynew),aes(x=Var1,y=value,col=as.factor(Var2)))+geom_line()+labs(x='Year',y='Znew',col='')+scale_color_viridis_d()
),.wd,"mortalities_hacky",c(21,28))

#################################################################################################################
########### fit model ###########################################################################################
#################################################################################################################

dat <- fit$data
dat$natMor <- maynew

fitM <- ccam.fit(dat,fit$conf,defpar(dat,fit$conf),debug=T)           
fitM

save(fitM, file=paste0('Rdata/',year,'/fit_compare/fit.Mconsum.Rdata'))
load(file=paste0('Rdata/',year,'/fit_compare/fit.Mconsum.Rdata'))

#################################################################################################################
########### plots ###########################################################################################
#################################################################################################################

Mruns <- c("base"=fit,"predation"=fitM)
class(Mruns) <- "ccamset"
savepng(ssbplot(Mruns,ci=FALSE),.wd,"ssb",c(21,13))
savepng(recplot(Mruns,ci=FALSE),.wd,"rec",c(21,13))
savepng(parplot(Mruns),.wd,"par",c(21,13))
savepng(srplot(Mruns),.wd,"sr",c(21,13))
savepng(catchplot(Mruns),.wd,"catch",c(21,13))

Mabs <- Mabstable(fitM)
Mabs <- merge(Mabs,rem,all.x=TRUE)
p <- ggplot(Mabs,aes(x=year))+
    geom_ribbon(aes(ymin=SSB.Low,ymax=SSB.High),alpha=0.5)+
    geom_line(aes(y=SSB.Estimate),size=1)+
    geom_line(aes(y=Mabs),size=1,col='red')+
    geom_ribbon(aes(ymin=cons.low,ymax=cons.high),alpha=0.5,fill='orange')+
    geom_line(aes(y=cons.med),size=1,col='orange')
savepng(p,.wd,"ssb_Mred_consumorange",c(21,13))

faym <- faytable(fitM)
maym <- fitM$data$natMor
zaym <- faym+maym

savepng(
grid.arrange(
    ggplot(melt(maym),aes(x=Var1,y=value,col=as.factor(Var2)))+geom_line()+labs(x='Year',y='Mnew',col='')+scale_color_viridis_d(),
    ggplot(melt(faym),aes(x=Var1,y=value,col=as.factor(Var2)))+geom_line()+labs(x='Year',y='Fest',col='')+scale_color_viridis_d(),
    ggplot(melt(zaym),aes(x=Var1,y=value,col=as.factor(Var2)))+geom_line()+labs(x='Year',y='Zest',col='')+scale_color_viridis_d()
),.wd,"mortalities_modelestimated",c(15,28))

zchange <- rbind(cbind(melt(zaym),fit='M predation'),cbind(melt(zay),fit='M ct'))
pz <- ggplot(zchange,aes(x=Var1,y=value,col=fit))+geom_line(size=1)+facet_wrap(~Var2)+labs(x='Year',y='Z')
savepng(pz,.wd,"Z",c(25,15))

# residuals
type <- 'png'
saveplot(grid.arrange(resplot(fitM,fleets = 3,type=1)+ggtitle('M predation'),
                      resplot(fit,fleets = 3,type=1)+ggtitle('M ct')),name="/res_index_1",dim=c(16,12),wd=.wd,type=type)
saveplot(grid.arrange(resplot(fitM,fleets = 3,type=2,out=1)+ggtitle('M predation'),
                      resplot(fit,fleets = 3,type=2,out=1)+ggtitle('M ct')),name="/res_index_2",dim=c(16,12),wd=.wd,type=type)
saveplot(grid.arrange(resplot(fitM,fleets = 3,type=3)+ggtitle('M predation'),
                      resplot(fit,fleets = 3,type=3)+ggtitle('M ct')),name="/res_index_3",dim=c(16,12),wd=.wd,type=type)
saveplot(grid.arrange(resplot(fitM,fleets = 3,type=4)+ggtitle('M predation'),
                      resplot(fit,fleets = 3,type=4)+ggtitle('M ct')),name="/res_index_4",dim=c(16,12),wd=.wd,type=type)
saveplot(grid.arrange(resplot(fitM,fleets = 3,type=4,trans = exp)+ggtitle('M predation'),
                      resplot(fit,fleets = 3,type=4,trans = exp)+ggtitle('M ct')),name="/res_index_5exp",dim=c(16,12),wd=.wd,type=type)

saveplot(resplot(x,fleets = 2,type=1,low=c('red','orange'),high=c('grey','green','darkgreen')),name="/res_caa_1",dim=c(10,6),wd=.wd,type=type)
saveplot(resplot(x,fleets = 2,type=2,out=3),name="/res_caa_2",dim=c(10,6),wd=.wd,type=type)
saveplot(resplot(x,fleets = 2,type=3),name="/res_caa_3",dim=c(10,6),wd=.wd,type=type)
saveplot(resplot(x,fleets = 2,type=4),name="/res_caa_4",dim=c(25,20),wd=.wd,type=type)
saveplot(resplot(x,fleets = 2,type=5,std=TRUE),name="/res_caa_5",dim=c(25,20),wd=.wd,type=type)
saveplot(resplot(x,fleets = 2,type=6),name="/res_caa_6",dim=c(10,6),wd=.wd,type=type)
saveplot(resplot(x,fleets = 2,type=7),name="/res_caa_7",dim=c(10,6),wd=.wd,type=type)