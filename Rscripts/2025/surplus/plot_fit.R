#################################################################################################################
#*** Mackerel assessment
#*** Plot model fits
#*** based on CCAM package
#################################################################################################################

#x <- get(load(file=paste0('Rdata/',year,'/fit/fit.Rdata')))
type <- 'png'
res <- TRUE
procres <- TRUE

.wd <- paste0('img/',year,'/fit/',name)
dir.create(.wd, showWarnings = FALSE,recursive = T)

### reference points
refBase <- ypr(x)

### plots
update_geom_defaults("line", list(size = 0.6))
saveplot(srplot(x,curve=TRUE),name='sr',dim=c(16,10),wd=.wd,type=type) 
saveplot(recplot(x),name='rec',dim=c(10,6),wd=.wd,type=type) 
saveplot(recplot(x,trans=function(x)x),name='rec_log',dim=c(10,6),wd=.wd,type=type) 
saveplot(catchplot(x,fleet = 1,ci=FALSE)+scale_y_continuous(limits=c(0,max(exp(x$data$logobs[,2]),na.rm=T)*1.1),expand = c(0,0)),name='catch',dim=c(10,6),wd=.wd,type=type)



saveplot(ssbplot(x)+scale_y_continuous(limits=c(0,max(ssbtable(x)[,3])*1.1),expand = c(0,0)),name='ssb',dim=c(10,6),wd=.wd,type=type)
saveplot(ssb0plot(x)+scale_y_continuous(limits=c(0,max(ssb0table(x)[,3])*1.1),expand = c(0,0)),name='ssb0',dim=c(10,6),wd=.wd,type=type)
saveplot(fbarplot(x)+scale_y_continuous(limits=c(0,4),expand = c(0,0)),name='F',dim=c(10,6),wd=.wd,type=type)
saveplot(plot(refBase),name='rp',dim=c(14,14),wd=.wd,type=type)
saveplot(selplot(x),name='sel',dim=c(6,6),wd=.wd,type=type)
saveplot(expplot(x),name='exp',dim=c(10,6),wd=.wd,type=type)
saveplot(parplot(x),name='par',dim=c(8,7),wd=.wd,type=type)
saveplot(plot(x),name='plot_all',dim=c(17,20),wd=.wd,type=type)
saveplot(prodplot(x),name='prod',dim=c(20,12),wd=.wd,type=type)
saveplot(kobeplot(x) + labs(x="SSB/SSBref", y="F/F40%"),name='kobe',dim=c(14/1.5,12/1.5),wd=.wd,type=type)
saveplot(scplot(x),name='ssb_rel',dim=c(16,10),wd=.wd,type=type)

p1 <- srplot(x,curve=T)+
    geom_vline(xintercept=refBase$ssbmsy,linetype='dashed')+
    geom_vline(xintercept=refBase$ssbmsy*0.4,linetype='dashed',col='red')+
    geom_vline(xintercept=refBase$ssbmsy*0.8,linetype='dashed',col='green')+
    geom_vline(xintercept=refBase$f40ssb,linetype='dotted')+
    geom_vline(xintercept=refBase$f40ssb*0.4,linetype='dotted',col='red')+
    geom_vline(xintercept=refBase$f40ssb*0.8,linetype='dotted',col='green')
saveplot(p1,name='sr_rp',dim=c(16,10),wd=.wd,type=type) 

p1b <- srplot(x,curve=T)+
    geom_vline(xintercept=refBase$f40ssb,linetype='dashed')+
    geom_vline(xintercept=refBase$f40ssb*0.4,linetype='dashed',col='darkred')+
    geom_vline(xintercept=refBase$f40ssb*0.8,linetype='dashed',col='darkgreen')
saveplot(p1b,name='sr_rpf40',dim=c(16,10),wd=.wd,type=type) 

p2 <- ssbplot(x)+scale_y_continuous(limits=c(0,max(ssbtable(x)[,3])*1.1),expand = c(0,0))+
    geom_hline(yintercept = refBase$f40ssb, colour="grey45")+
    geom_hline(yintercept = refBase$f40ssb*0.8,colour="grey45", linetype=2)+
    geom_hline(yintercept = refBase$f40ssb*0.4,colour="grey45", linetype=3)   +
    scale_x_continuous(breaks=seq(1970, year,5))
    
saveplot(p2,name='ssb_rpF40',dim=c(10,6),wd=.wd,type=type)

p3 <- ssbplot(x,years=2000:year)+scale_y_continuous(limits=c(0,max(tail(ssbtable(x)[,3],23))*1.1),expand = c(0,0))+
    geom_hline(yintercept = refBase$f40ssb, colour="grey45")+
    geom_hline(yintercept = refBase$f40ssb*0.8,colour="grey45", linetype=2)+
    geom_hline(yintercept = refBase$f40ssb*0.4,colour="grey45", linetype=3)   +
    scale_x_continuous(breaks=seq(1970, year,5))

saveplot(p3,name='ssb_rpF40end',dim=c(6,6),wd=.wd,type=type)
p3BI<- p3+labs(x="Année | Year", y="BSR | SSB (t)")
saveplot(p3BI,name='ssb_rpF40endBI',dim=c(6,6),wd=.wd,type=type)

p4 <- ssbplot(x)+scale_y_continuous(limits=c(0,max(ssbtable(x)[,3])*1.1),expand = c(0,0))+
    geom_hline(yintercept = refBase$ssbmsy)+
    geom_hline(yintercept = refBase$ssbmsy*0.8,col='darkgreen')+
    geom_hline(yintercept = refBase$ssbmsy*0.4,col='darkred')    

saveplot(p4,name='ssb_rpmsy',dim=c(10,6),wd=.wd,type=type)


p5 <- ssbplot(x,years=2000:year)+scale_y_continuous(limits=c(0,max(tail(ssbtable(x)[,3],23))*1.1),expand = c(0,0))+
    geom_hline(yintercept = refBase$ssbmsy)+
    geom_hline(yintercept = refBase$ssbmsy*0.8,col='darkgreen')+
    geom_hline(yintercept = refBase$ssbmsy*0.4,col='darkred')    

saveplot(p5,name='ssb_rpmsyend',dim=c(6,6),wd=.wd,type=type)

p6 <- ggplot(melt(ntable(fit)),aes(x=Var1,y=Var2))+
    geom_point(alpha=0.8,aes(size=value,col=value))+
    scale_size(range = c(0.5,6)) +
    labs(size="N",y='Age',x='Year')+
    scale_color_gradient(low="grey65", high="black")+
    guides(col='none')  +
    scale_x_continuous(breaks=seq(1970, year,5))
saveplot(p6,name='n',dim=c(16,10),wd=.wd,type=type)

 
pn <- ggplot(melt(ntable(fit)),aes(x=Var1,y=Var2))+geom_point(alpha=0.6,aes(size=value/1000))+
    scale_size(range = c(1,8), breaks=seq(0,1000,250), guide="none") +
    labs(size="N *1000",y='Age',x='Year')+
   # scale_color_gradient(high="black", low="grey65")+ 
    scale_y_continuous(breaks=1:10)
  # guides(col='none')
saveplot(pn,name='n_grey',dim=c(16,8),wd=.wd,type=type)

pn2 <- ggplot(melt(ntable(fit)) %>%  filter(Var1 > 2015),aes(x=Var1,y=Var2))+
    geom_point(alpha=0.6,aes(size=value/1000))+
    scale_size(range = c(1,10), breaks=seq(0,1000,250), guide="none") +
    labs(size="N *1000",y='Age',x='Year')+
    # scale_color_gradient(high="black", low="grey65")+ 
    scale_y_continuous(breaks=1:10) +
    scale_x_continuous(breaks=2015:year) +
    geom_abline(slope=1, intercept=-2015, alpha=0.5, col="grey", linewidth=2)+
    geom_abline(slope=1, intercept=-2020, alpha=0.5, col="grey", linewidth=2)
pn2
# guides(col='none')
saveplot(pn2,name='n2015_y_grey',dim=c(16,8),wd=.wd,type=type)
lm(age ~ year, data=data.frame(age=1:10, year=2016:2025))


pnFR <- pn +
    labs(size="N *1000",y='Âge',x='Année')+
    # scale_color_gradient(high="black", low="grey65")+ 
    scale_y_continuous(breaks=1:10)
# guides(col='none')
saveplot(pnFR,name='n_greyFR',dim=c(16,8),wd=.wd,type=type)


pa <- p2+geom_text(aes(x=-Inf,y=Inf,label='A)'),hjust=-0.5,vjust=2)+labs(y="SSB (t)\n")
pb <- p6+theme(legend.position = 'none')+geom_text(aes(x=-Inf,y=Inf,label='B)'),hjust=-0.5,vjust=2)+scale_y_continuous(breaks = 1:10)+labs(y="Age\n")
pc <- recplot(x)+geom_text(aes(x=-Inf,y=Inf,label='C)'),hjust=-0.5,vjust=2)+
    scale_y_continuous(labels = function(x) format(x/(10^3)),
                       expand=c(0,0),limits=c(0,max(rectable(x)[,3])*1.02))+
    scale_x_continuous(breaks=seq(1970, year,5)) +
    labs(y="Recruitment (x 1000)\n")
pd <- srplot(x,curve=T,text=F,linecol='black')+labs(y='Recruitment')+
    geom_text(aes(x=-Inf,y=Inf,label='D)'),hjust=-0.5,vjust=2)+
    scale_y_continuous(labels = function(x) format(x/(10^3),
                       breaks=as.numeric(na.omit(layer_scales(pc)$y$break_positions())),
                       limits=c(0,max(rectable(x)[,1])*1.05),expand=c(0,0)))+
    labs(y="Recruitment (x 1000)\n")
pe <- fbarplot(x)+scale_y_continuous(limits=c(0,4),expand = c(0,0))+geom_hline(yintercept = refBase$f40, col="grey65")+geom_text(aes(x=-Inf,y=Inf,label='E)'),hjust=-0.5,vjust=2)+
    labs(y="Fbar\n") +
    scale_x_continuous(breaks=seq(1970, year,5))
pf <- catchplot(x,fleet = 1,ci=FALSE)+
    scale_y_continuous(limits=c(0,150000),expand = c(0,0),
                       labels = function(x) format(x/(10^3)))+
    ylab('Catch')+geom_text(aes(x=-Inf,y=Inf,label='F)'),hjust=-0.5,vjust=2)+
    labs(y="Landings (kt)\n") +
    scale_x_continuous(breaks=seq(1970, year,5))
saveplot(grid::grid.draw(rbind(
    cbind(ggplotGrob(pa), ggplotGrob(pb), size="first"),
    cbind(ggplotGrob(pc), ggplotGrob(pd), size="first"),
    cbind(ggplotGrob(pe), ggplotGrob(pf), size="first"),
    size='first')),name='RESDOC2',dim=c(25,22),wd=.wd,type=type)


pafr <- pa + labs(y="BSR (t)", x="Année")
pbfr <- pb + labs(y="Âge", x="Année")
pcfr <-  pc +labs(y="Recrutement (milliers)\n", x="Année")
pdfr <- pd + labs(y="Recrutement (milliers)\n", x="BSR (t)")
pefr <- pe + labs(y="Fbar\n", x="Année")
pffr <- pf + labs(y="Captures (kt)\n", x="Année")
saveplot(grid::grid.draw(rbind(
    cbind(ggplotGrob(pafr), ggplotGrob(pbfr), size="first"),
    cbind(ggplotGrob(pcfr), ggplotGrob(pdfr), size="first"),
    cbind(ggplotGrob(pefr), ggplotGrob(pffr), size="first"),
    size='first')),name='RESDOC2FR',dim=c(25,22),wd=.wd,type=type)


pabi <- pa + labs(y="BSR | SSB (t)\n", x="Année | Year")
pbbi <- pb + labs(y="Âge | Age\n", x="Année")
pcbi <- pc + labs(y="Recrutement | Recruitment (x 1000)\n", x="Année | Year")
pdbi <- pd + labs(y="Recrutement | Recruitment (x 1000)\n", x="BSR | SSB (t)\n")
pebi <- pe + labs(y="Fbar\n", x="Année | Year")
pfbi <- pf + labs(y="Captures | Landings (kt)\n", x="Année | Year")
saveplot(grid::grid.draw(rbind(
    cbind(ggplotGrob(pabi), ggplotGrob(pbbi), size="first"),
    cbind(ggplotGrob(pcbi), ggplotGrob(pdbi), size="first"),
    cbind(ggplotGrob(pebi), ggplotGrob(pfbi), size="first"),
    size='first')),name='RESDOC2BI',dim=c(25,22),wd=.wd,type=type)


if(res){
    slope <- function(x){diff(quantile(x[!is.na(x)], c(0.25, 0.75)))/diff(qnorm(c(0.25, 0.75)))}
    intercept <- function(x){quantile(x[!is.na(x)], c(0.25, 0.75))[1L] - diff(quantile(x[!is.na(x)], c(0.25, 0.75)))/diff(qnorm(c(0.25, 0.75))) * qnorm(c(0.25, 0.75))[1L]}
    
    #myres <- residuals(x) # not working
    #saveplot(plot(myres,fleet=c(2,3),qq=TRUE),name="res",dim=c(20,10),wd=.wd,type=type)
}

if(procres){
    myprocres <- procres(x)
    saveplot(plot(myprocres,qq=FALSE)+scale_y_continuous(breaks=1:10),name="pe",dim=c(20,10),wd=.wd,type=type)
}


# residuals the old way (though they are wrong because of autocorrelation due to random effects)
saveplot(resplot(x,fleets = 3,type=1),name="/res_index_1",dim=c(10,6),wd=.wd,type=type)
saveplot(resplot(x,fleets = 3,type=2,out=1),name="/res_index_2",dim=c(10,6),wd=.wd,type=type)
saveplot(resplot(x,fleets = 3,type=3),name="/res_index_3",dim=c(10,6),wd=.wd,type=type)
saveplot(resplot(x,fleets = 3,type=4),name="/res_index_4",dim=c(10,6),wd=.wd,type=type)
saveplot(resplot(x,fleets = 3,type=4,trans = exp),name="/res_index_5exp",dim=c(10,6),wd=.wd,type=type)

saveplot(resplot(x,fleets = 2,type=1,low=c('red','orange'),high=c('grey','green','darkgreen')),name="/res_caa_1",dim=c(10,6),wd=.wd,type=type)
saveplot(resplot(x,fleets = 2,type=2,out=3),name="/res_caa_2",dim=c(10,6),wd=.wd,type=type)
saveplot(resplot(x,fleets = 2,type=3),name="/res_caa_3",dim=c(10,6),wd=.wd,type=type)
saveplot(resplot(x,fleets = 2,type=4),name="/res_caa_4",dim=c(25,20),wd=.wd,type=type)
saveplot(resplot(x,fleets = 2,type=5,std=TRUE),name="/res_caa_5",dim=c(25,20),wd=.wd,type=type)
saveplot(resplot(x,fleets = 2,type=6),name="/res_caa_6",dim=c(10,6),wd=.wd,type=type)
saveplot(resplot(x,fleets = 2,type=7),name="/res_caa_7",dim=c(10,6),wd=.wd,type=type)

nol <- theme(legend.position = 'none')
saveplot(grid.arrange(
    arrangeGrob(
        resplot(x,fleets = 3,type=1)+ggtitle('TEP'),
        resplot(x,fleets = 3,type=2),
        resplot(x,fleets = 3,type=3),ncol=1),
    arrangeGrob(
        resplot(x,fleets = 2,type=6)+ggtitle('LAA'),
        resplot(x,fleets = 2,type=2)+nol,
        resplot(x,fleets = 2,type=3)+nol,ncol=1),
    ncol=2),
    name="/res_all",dim=c(18,16),wd=.wd,type=type)

saveplot(grid.arrange(
    arrangeGrob(
        resplot(x,fleets = 3,type=1)+ggtitle('PTO')+labs(y="Résidus",x='Année'),
        resplot(x,fleets = 3,type=2)+labs(y="Résidus",x="Prédictions"),
        resplot(x,fleets = 3,type=3)+labs(y="Prédictions",x='Observations'),ncol=1),
    arrangeGrob(
        resplot(x,fleets = 2,type=6)+ggtitle('DAA')+labs(y="Résidus",x='Année'),
        resplot(x,fleets = 2,type=2)+nol+labs(y="Résidus",x="Prédictions"),
        resplot(x,fleets = 2,type=3)+nol+labs(y="Prédictions",x='Observations'),ncol=1),
    ncol=2),
    name="res_all_FR",dim=c(18,16),wd=.wd,type=type)

