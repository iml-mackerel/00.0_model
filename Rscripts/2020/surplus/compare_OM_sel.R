#################################################################################################################
#*** Mackerel MSE
#*** Canadian mackerel (DFO, 2020)
#*** COMPARE Different selectivity in operating models
#################################################################################################################

#################################################################################################################
########### READ IN DATA AND FIT ########################################################################################
#################################################################################################################

load(file='Rdata/input/dat.Rdata')
load(file='Rdata/input/conf.Rdata')
load(file='Rdata/input/par.Rdata')

# different max age
selrange <- 4:10 #flat topped from 4 to 10
for(i in selrange){
    sel <- rep(i-1,10)
    sel[0:(i-1)] <- 0:(i-2)
    conf$keySel <- matrix(sel, nrow=nrow(conf$keySel), ncol=ncol(conf$keySel),byrow = T)
    par <- defpar(dat,conf)
    fit <- ccam.fit(dat,conf,par,silent=TRUE)           
    save(fit, file=paste0('Rdata/fit_compare/sel',i,'.Rdata'))
}

# sel in 2 blocks
nrow(conf$keySel)
block1 <- matrix(c(0,1,2,3,4,4,4,4,4,4), nrow=length(c(1968:2000)), ncol=ncol(conf$keySel),byrow = T)
block2 <- matrix(c(0,1,2,3,4,4,4,4,4,4)+5, nrow=length(c(2001:2020)), ncol=ncol(conf$keySel),byrow = T)
newsel <- rbind(block1,block2)
nrow(newsel)
conf$keySel <- newsel
par$logitSel <- c(par$logitSel,par$logitSel)
block <- ccam.fit(dat,conf,par,silent=TRUE,paracheck = FALSE)           
save(block, file='Rdata/fit_compare/sel_2block.Rdata')

#################################################################################################################
########### fit model ###########################################################################################
#################################################################################################################

filenames <- dir('Rdata/fit_compare', pattern = "sel")
files <- paste0('Rdata/fit_compare','/',filenames[-c(1,2)])
selruns <- lapply(files, function(x) {print(x);get(load(x))})

class(selruns) <- 'ccamset'
names(selruns) <- selrange

save(selruns,file='Rdata/fit_compare/sel.Rdata')


#load(file='Rdata/fit_compare/sel.Rdata')
#load(file='Rdata/fit_compare/sel_2block.Rdata')
#load('Rdata/fit/fit.Rdata')

.wd <- 'img/fit_compare/sel/'
dir.create(.wd, showWarnings = FALSE)

### plot with different top

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


### plot if block
res <- invlogit(tail(partable(block),8)[,1])
res <- data.frame(age=1:10,sel1=c(res[1:4],rep(1,6)),sel2=c(res[5:8],rep(1,6)))
res <- melt(res,id='age')
comp <- c(fit,block)
names(comp) <- c('base','block')

selblock <- ggplot(res,aes(x=age,y=value,col=variable))+geom_line(size=1)+labs(col='')+
            scale_color_viridis_d(labels = c("1968-2000", "2001-2018"))
saveplot(selblock,name='sel_block2000',dim=c(8,6),wd=.wd)
saveplot(ssbplot(comp,linesize=1),name='ssb_block',dim=c(16,10),wd=.wd)
saveplot(recplot(comp,linesize=1),name='rec_block',dim=c(16,10),wd=.wd)
saveplot(resplot(block,fleets = 3,type=4),name='resI_block',dim=c(16,10),wd=.wd)




