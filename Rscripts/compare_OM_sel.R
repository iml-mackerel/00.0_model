#################################################################################################################
#*** Mackerel MSE
#*** Canadian mackerel (DFO, 2018)
#*** COMPARE Different M in operating models
#################################################################################################################

#################################################################################################################
########### READ IN DATA AND FIT ########################################################################################
#################################################################################################################

load(file='Rdata/input/dat.Rdata')
load(file='Rdata/input/conf.Rdata')
load(file='Rdata/input/par.Rdata')

selrange <- 4:10 #flat topped from 4 to 10

# one by one in case there is an error (especially in the lower range)
for(i in selrange){
    sel <- rep(i-1,10)
    sel[0:(i-1)] <- 0:(i-2)
    conf$keySel <- matrix(sel, nrow=nrow(conf$keySel), ncol=ncol(conf$keySel),byrow = T)
    par <- defpar(dat,conf)
    fit <- ccam.fit(dat,conf,par,silent=TRUE)           
    save(fit, file=paste0('Rdata/fit_compare/sel',i,'.Rdata'))
}

#################################################################################################################
########### fit model ###########################################################################################
#################################################################################################################

filenames <- dir('Rdata/fit_compare', pattern = "sel")
files <- paste0('Rdata/fit_compare','/',filenames)
selruns <- lapply(files, function(x) {print(x);get(load(x))})

class(selruns) <- 'ccamset'
names(selruns) <- selrange

save(selruns,file='Rdata/fit_compare/sel.Rdata')
#load(file='Rdata/fit_compare/sel.Rdata')

.wd <- 'img/fit_compare/sel/'
dir.create(.wd, showWarnings = FALSE)

savepng(ssbplot(selruns,ci=FALSE),.wd,"SSB",c(17,10))
savepng(catchplot(selruns,ci=FALSE),.wd,"catch",c(17,10))
savepng(fbarplot(selruns,ci=FALSE,linesize=1),.wd,"F",c(12,8))
savepng(selplot(selruns,ci=FALSE),.wd,"sel",c(17,10))
savepng(fitplot(selruns,type='AIC'),.wd,"AIC",c(14,6))
savepng(fitplot(selruns,type='nll'),.wd,"nll",c(14,6))

p1 <- ggplot(melt(ntable(selruns[[2]])),aes(x=Var1,y=Var2,size=value,col=value))+geom_point(alpha=0.8)+
    scale_size(range = c(1,8)) +
    labs(size="N",y='Age',x='Year')+
    scale_color_viridis()+
    guides(col=FALSE)+
    ggtitle('5')
p2 <- ggplot(melt(ntable(selruns[[5]])),aes(x=Var1,y=Var2,size=value,col=value))+geom_point(alpha=0.8)+
    scale_size(range = c(1,8)) +
    labs(size="N",y='Age',x='Year')+
    scale_color_viridis()+
    guides(col=FALSE)+
    ggtitle('8')

saveplot(grid.arrange(p1,p2),name='n',dim=c(16,10),wd=.wd)


df <- data.frame(Amax=selrange,LRP=unlist(lapply(ypr(selruns),'[','f40ssb'))*0.4)
selLRP <- ggplot(df,aes(x=Amax,y=LRP))+geom_point()+
    labs(x='Amax fishery selectivity')
saveplot(selLRP,name='LRP',dim=c(8,6),wd=.wd)




