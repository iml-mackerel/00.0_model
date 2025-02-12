#################################################################################################################
#*** Mackerel MSE
#*** Canadian mackerel (DFO, 2018)
#*** COMPARE Different M in operating models
#################################################################################################################
type  <- 'png'
#################################################################################################################
########### READ IN DATA AND FIT ########################################################################################
#################################################################################################################
source(paste0('Rscripts/',year, '/surplus/read_data.R'))
load(paste0('Rdata/',my.year,'/input/dat.Rdata'))
load(paste0('Rdata/',my.year,'/input/conf.Rdata'))
load(paste0('Rdata/',my.year,'/input/par.Rdata'))

#cn files cross sensitivity
cn_modif22_23 <- read.ices(paste0(dir,'/sensitivity/modified2022_2023.dat'))  %>%  as.data.frame()
cn_default <- read.ices(paste0(dir,'/sensitivity/cn_default.dat')) %>%  as.data.frame()
cn_modif22_24 <- read.ices(paste0(dir,'/sensitivity/modified2022_2024.dat')) %>%  as.data.frame()

# different max age
#selrange <- 4:10 #flat topped from 4 to 10
#for(i in selrange){
#    sel <- rep(i-1,10)
#    sel[0:(i-1)] <- 0:(i-2)
#    conf$keySel <- matrix(sel, nrow=nrow(conf$keySel), ncol=ncol(conf$keySel),byrow = T)
#    par <- defpar(dat,conf)
#    fit <- ccam.fit(dat,conf,par,silent=TRUE)           
#    save(fit, file=paste0('Rdata/',my.year,'/sensitivity/sel',i,'.Rdata'))
#}
sens=list()
newdat=list()


#sens1
block1 <- matrix(c(0,1,2,3,4,4,4,4,4,4), nrow=length(c(1969:2024)), ncol=ncol(conf$keySel),byrow = T)
sens[[1]] <- rbind(block1)
newdat[[1]] = cn_default

#sens2
sens[[2]] <- rbind(block1)
newdat[[2]] =cn_modif22_23

#sens3
sens[[3]] <- rbind(block1)
newdat[[3]] = cn_modif22_24


#sens4
    block1 <- matrix(c(0,1,2,3,4,4,4,4,4,4), nrow=length(c(1969:2021)), ncol=ncol(conf$keySel),byrow = T)
    block2 <- matrix(c(0,1,2,3,4,4,4,4,4,4)+5, nrow=length(c(2022:2023)), ncol=ncol(conf$keySel),byrow = T)
    block3 <- matrix(c(0,1,2,3,4,4,4,4,4,4), nrow=length(c(2024)), ncol=ncol(conf$keySel),byrow = T)
    sens[[4]] <- rbind(block1,block2, block3)
    newdat[[4]] = cn_default
    
    #sens5
    block1 <- matrix(c(0,1,2,3,4,4,4,4,4,4), nrow=length(c(1969:2021)), ncol=ncol(conf$keySel),byrow = T)
    block2 <- matrix(c(0,1,2,3,4,4,4,4,4,4)+5, nrow=length(c(2022:2024)), ncol=ncol(conf$keySel),byrow = T)
    sens[[5]] <- rbind(block1,block2) #block 3 same param as block1
    newdat[[5]]= cn_default
#sens6 same conf. different CAA
    sens[[6]] =sens[[4]]
    newdat[[6]]=cn_modif22_23
#sens7
      sens[[7]]=sens[[5]]  
      newdat[[7]]= cn_modif22_24
    
          
      #sens8
      block1 <- matrix(c(0,1,2,3,4,4,4,4,4,4), nrow=length(c(1969:1999)), ncol=ncol(conf$keySel),byrow = T)
      block2 <- matrix(c(c(0,1,2,3)+5,4,4,4,4,4,4), nrow=length(c(2000:2021)), ncol=ncol(conf$keySel),byrow = T)
      block3 <- matrix(c(c(0,1,2,3)+10,4,4,4,4,4,4), nrow=length(c(2022:2023)), ncol=ncol(conf$keySel),byrow = T)
      block4 <- matrix(c(c(0,1,2,3)+5,4,4,4,4,4,4), nrow=length(c(2024)), ncol=ncol(conf$keySel),byrow = T) # same as block2
        
      sens[[8]] <- rbind(block1,block2, block3, block4)
      newdat[[8]]=cn_default


    nsens=7 # scenario 8 does not always converge
      
#choose caa file.cross sensitivity
dats <- lapply(1:nsens,function(x){
    dat<- setup.ccam.data(surveys=survey,
                    residual.fleet=newdat[[x]][-1,],
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
})

#################################################################################################################
########### fit model ###########################################################################################
#################################################################################################################

conf =list()
par = list()


for(i in 1:nsens){
    conf[[i]] <- defcon(dats[[i]])
    conf[[i]]$keySel <- sens[[i]]
    conf[[i]]$keyVarObs[1,]=-1                     
    conf[[i]]$keyVarObs[2,1:9]=c(0,1,2,2,2,2,2,2,1) 
    conf[[i]]$keyVarObs[3,1]=3           
    conf[[i]]$stockRecruitmentModelCode=2 #0: RW, 1: ricker, 2: BH, 3:mean
    conf[[i]]$obsLikelihoodFlag[1]='CE'
    conf[[i]]$keyBiomassTreat[3]=5
    conf[[i]]$fbarRange=c(5,10) #fully recruited fish
    par <- defpar(dats[[i]],conf[[i]])
 
    fits_sel<- ccam.fit(dats[[i]],conf[[i]],par[[i]],debug=T)
    save(fits_sel, file=paste0('Rdata/',year,'/sensitivity/fit.sel.',i,'.Rdata'))
}


filenames <- dir(paste0('Rdata/',my.year,'/sensitivity'), pattern = "fit.sel")
files <- paste0('Rdata/',my.year,'/sensitivity','/',filenames)
selruns <- lapply(files, function(x) {print(x);get(load(x))})

class(selruns) <- 'ccamset'
names(selruns) <- c(paste0("sens", 1:nsens))
#names(selruns) <- c("")

save(selruns,file=paste0('Rdata/',my.year,'/sensitivity/sel.Rdata'))


load(file=paste0('Rdata/',my.year,'/sensitivity/sel.Rdata'))
#load('Rdata/fit/fit.Rdata')

.wd <- paste0('img/',my.year,'/sensitivity/sel/')
dir.create(.wd, showWarnings = FALSE)

### plot with different top

ssbplot0(selruns, minyear=2010, year=year, legend=T)
ggsave(paste0(.wd,"ssb_noci2010.png"), width=6, height=5, units="in", bg="white")
ssbplot0(selruns, minyear=1969, year=year, legend=T)
ggsave(paste0(.wd,"ssb_noci1969.png"), width=6, height=5, units="in", bg="white")

#selectivity plots
source("R/patch_table_sel.R")
p = list()
s = list()
df=list()
my.seltable = list()
newtable=list()
cfiles=c("default", "modif. 2022-2023", "modif. 2022-2024",
         "default","default", "modif. 2022-2023", "modif. 2022-2024",
         "default", "default" 
         )


for(i in 1:nsens){
df[[i]] <-  mytableit(selruns[[i]], what="par.logitSel",trans = invlogit)
   
my.seltable[[i]]<- selruns[[i]]$conf$keySel +1
colnames(my.seltable[[i]]) <-  1:10
newtable[[i]]<- left_join(my.seltable[[i]] %>%  as.data.frame() %>% mutate(year = 1969:my.year) %>%  pivot_longer(1:10, names_to="age", values_to="param"),
          df[[i]]) 
 p[[i]]<- ggplot(data=newtable[[i]] %>%  dplyr::filter(age <=5, year >2010) ,aes(x=year, y=as.numeric(age), fill=Estimate)) +geom_tile(col="white") +
     scale_fill_viridis_b(breaks=seq(0,1,0.1)) +
     scale_y_continuous(breaks=seq(1,5,1), labels=c(1:4, "5+"), limits=c(0,6))+
     ggtitle(names(selruns)[i])+
     labs(fill="Sel", x="Année | Year", y="Âge | Age")

s[[i]] <-  newtable[[i]] %>%  mutate(age=as.numeric(age)) %>%  
     group_by(age, param) %>% 
     summarise(Estimate= mean(Estimate), Low=mean(Low), High=mean(High), years = paste(min(year), max(year), sep="-")) %>% 
    mutate(Low=if_else(Low ==0, NA, Low),
           fichier=cfiles[i]) %>%  
    ggplot(aes(x=age, y=Estimate)) +
    geom_line(aes(color=years, group=years)) +geom_ribbon(aes(ymin=Low,ymax=High, fill=years, color=years, group=years), alpha=0.5) +
    scale_color_manual(name="", values=c("black", "grey"))+
    scale_fill_manual(name="", values=c("black", "grey"))+
    labs(y="Selectivity", x="Âge | Age") +
    scale_x_continuous(breaks=1:10) + ggtitle(names(selruns)[i]) +
    theme(legend.position="inside", legend.position.inside =c(0.8,0.2) ) +
    geom_text(aes(x=8,y=0.6, label=unique(fichier)), col="black") 

 
}
ggarrange(plotlist=p, ncol=4,nrow=2, common.legend=T, legend="right", align="hv")
ggsave(paste(.wd,"sel.png"), width=12, height=6, units="in", dpi=600)

ggarrange(plotlist=s, ncol=4,nrow=2, common.legend=F, align="hv")
ggsave(paste(.wd,"sel2.png"), width=12, height=6, units="in", dpi=600, bg="white")

savepng(fitplot(selruns,type='AIC'),.wd,"AIC",c(14,6))


df <- data.frame(Amax=names(selruns),LRP=unlist(lapply(ypr(selruns),'[','f40ssb'))*0.4)
selLRP <- ggplot(df,aes(x=Amax,y=LRP))+geom_point()+
    labs(x='Amax fishery selectivity')
saveplot(selLRP,name='LRP',dim=c(12,6),wd=.wd)




nol <- theme(legend.position = 'none')
saveplot(grid.arrange(
    arrangeGrob(
        resplot(selruns[[1]],fleets = 2,type=6)+ggtitle('sens1'),
        resplot(selruns[[1]],fleets = 2,type=2)+nol,
        resplot(selruns[[1]],fleets = 2,type=3)+nol,ncol=1),
    arrangeGrob(
        resplot(selruns[[2]],fleets = 2,type=6)+ggtitle('sens2'),
        resplot(selruns[[2]],fleets = 2,type=2)+nol,
        resplot(selruns[[2]],fleets = 2,type=3)+nol,ncol=1),
    arrangeGrob(
        resplot(selruns[[3]],fleets = 2,type=6)+ggtitle('sens3'),
        resplot(selruns[[3]],fleets = 2,type=2)+nol,
        resplot(selruns[[3]],fleets = 2,type=3)+nol,ncol=1),
    arrangeGrob(
        resplot(selruns[[4]],fleets = 2,type=6)+ggtitle('sens4'),
        resplot(selruns[[4]],fleets = 2,type=2)+nol,
        resplot(selruns[[4]],fleets = 2,type=3)+nol,ncol=1),
    arrangeGrob(
        resplot(selruns[[5]],fleets = 2,type=6)+ggtitle('sens5'),
        resplot(selruns[[5]],fleets = 2,type=2)+nol,
        resplot(selruns[[5]],fleets = 2,type=3)+nol,ncol=1),
    arrangeGrob(
        resplot(selruns[[6]],fleets = 2,type=6)+ggtitle('sens6'),
        resplot(selruns[[6]],fleets = 2,type=2)+nol,
        resplot(selruns[[6]],fleets = 2,type=3)+nol,ncol=1),
    arrangeGrob(
        resplot(selruns[[7]],fleets = 2,type=6)+ggtitle('sens7'),
        resplot(selruns[[7]],fleets = 2,type=2)+nol,
        resplot(selruns[[7]],fleets = 2,type=3)+nol,ncol=1),
       ncol=nsens),
    name="/res_all",dim=c(40,10),wd=.wd,type=type)

r <- list()
for(i in 1:nsens){
r[[i]]<- resplot(selruns[[i]], fleets = 2,type=1,low=c('red','orange'),high=c('green','darkgreen')) + 
    ggtitle(names(selruns)[[i]]) + scale_fill_gradient2(low=c('red','orange'),high=c('green','darkgreen'), mid="grey", limits=c(-5,3))+
    scale_x_continuous(name="Année | Year", breaks=seq(1970, my.year, 10))+
    scale_y_continuous(name="Âge | Age", breaks=seq(1,10, 2))
}
ggarrange(plotlist=r, common.legend = T, legend="right")
ggsave(paste(.wd,"caa_res.png"), width=10, height=10, units="in", dpi=600)



r <- list()
for(i in 1:nsens){
    r[[i]]<- ggplot(melt(ntable(selruns[[i]])),aes(x=Var1,y=Var2))+geom_point(alpha=0.8,aes(size=value,col=value))+
        scale_size(range = c(1,8)) +
        labs(size="N",y='Âge | Age',x='Année | Year')+
        scale_color_viridis()+
        guides(col='none') +ggtitle(names(selruns)[i])
}
ggarrange(plotlist=r, common.legend = T, legend="right")
ggsave(paste(.wd,"n.png"), width=10, height=10, units="in", dpi=600)



