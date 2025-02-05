#################################################################################################################
#*** Mackerel MSE
#*** Canadian mackerel (DFO, 2018)
#*** COMPARE varObs
#################################################################################################################
type  <- 'png'
#################################################################################################################
########### READ IN DATA AND FIT ########################################################################################
#################################################################################################################
source(paste0('Rscripts/',year, '/surplus/read_data.R'))
load(paste0('Rdata/',my.year,'/input/dat.Rdata'))
load(paste0('Rdata/',my.year,'/input/conf.Rdata'))
load(paste0('Rdata/',my.year,'/input/par.Rdata'))

vobs<- rbind(c(0,	1,	2,	2,	1,	1,	1,	1,	1),
c(0,	1,	2,	2,	2,	1,	1,	1,	1),
c(0,	1,	2,	2,	2,	2,	1,	1,	1),
c(0,	1,	2,	2,	2,	2,	2,	1,	1),
c(0,	1,	2,	2,	2,	2,	2,	2,	1),
c(0,	1,	2,	2,	2,	2,	2,	2,	2),
c(0,	1,	1,	2,	2,	1,	1,	1,	1),
c(0,	1,	1,	2,	2,	2,	1,	1,	1),
c(0,	1,	1,	2,	2,	2,	2,	1,	1),
c(0,	1,	1,	2,	2,	2,	2,	2,	1),
c(0,	1,	1,	2,	2,	2,	2,	2,	2)) 


confs = list()
for(i in 1:nrow(vobs)){
conf$keyVarObs[2,1:9]=vobs[i, ]
confs[[i]]<- conf
}

#########################################################################################################
########### fit model ###########################################################################################
#################################################################################################################

fits.var <- lapply(confs,function(x)ccam.fit(dat,x,par,debug=T))
names(fits.var) <- c(paste0("Sens",1:length(fits.var)))
class(fits.var) <- "ccamset"

save(fits.var, file=paste0('Rdata/',year,'/sensitivity/fits.var.Rdata'))


load(file=paste0('Rdata/',year,'/sensitivity/fits.var.Rdata'))

.wd <- paste0('img/',my.year,'/sensitivity/varObs/')
dir.create(.wd, showWarnings = FALSE)

### plot with different top

ssbplot0(fits.var, minyear=1969, year=year, legend=T)
ggsave(paste0(.wd,"ssb_noci1969.png"), width=6, height=5, units="in", bg="white")
ssbplot0(fits.var, minyear=2010, year=year, legend=T)
ggsave(paste0(.wd,"ssb_noci2010.png"), width=6, height=5, units="in", bg="white")


par.table<- partable(fits.var) %>%  as.matrix()
colnames(par.table)[1] <- "par.name"
par.table<- par.table %>% as.data.frame() %>%  dplyr::filter(grepl(par.name, pattern="LogObs")) %>% 
    mutate(param= as.numeric(gsub(par.name,pattern="logSdLogObs_", replacement=""))-1) %>%  
    dplyr::select(fit, par, param) %>%  dplyr::filter(param !=-1)


vobs<- vobs %>%  as.data.frame()
colnames(vobs) <-  1:9
vobs$fit =paste0("Sens", 1:nrow(vobs))
vobs<- vobs %>%  pivot_longer(1:9, names_to="age", values_to="param")
param.table<- left_join(vobs, par.table) %>%  mutate(par=as.numeric(par),
                                                     fit=as.numeric(gsub(fit, pattern="Sens",replacement=""))) 

aic<- modeltable(fits.var) %>% as.data.frame() %>% dplyr ::select(AIC) %>% rownames_to_column("fit") %>% 
    mutate(AIC=round(AIC),
           fit=as.numeric(gsub(fit, pattern="Sens",replacement="")),
           age=10) 
vobs<- vobs %>% 
    mutate(fit=as.numeric(gsub(fit, pattern="Sens",replacement=""))
          ) 

ggplot(param.table , aes(x=as.numeric(age), y =fit)) +geom_tile(aes(fill=par),color="white")+
    scale_fill_viridis(name="logSdLogObs") +scale_y_reverse(name="",breaks=seq(1,11, 1))+
    scale_x_continuous(name="Âge | Age", breaks=seq(1,10,1), label=c(1:9, "AIC")) +theme(legend.position="bottom")+
    geom_text(data=aic, aes(x=age, y=fit,label=AIC, color=AIC)) +
    scale_color_gradient(low="green", high="red", guide="none") +
    geom_text(aes(label=param), col="white") +
    annotate(geom="rect", xmin=0.5, xmax=11, ymin=2.5, ymax=3.45, alpha=0.1, col="grey55", lwd=1.1)+
annotate(geom="rect", xmin=0.5, xmax=11, ymin=3.55, ymax=4.5, alpha=0.1, col="black", lwd=1.1)
ggsave(paste(.wd,"parObs.png"), width=4, height=4, dpi=600, units="in")


savepng(fitplot(fits.var,type='AIC'),.wd,"AIC",c(14,6))


r <- list()
for(i in 1:length(fits.var)){
    r[[i]]<- resplot(fits.var[[i]], fleets = 2,type=1,low=c('red','orange'),high=c('green','darkgreen')) + 
        ggtitle(names(fits.var)[[i]]) + scale_fill_gradient2(low=c('red','orange'),high=c('green','darkgreen'), mid="grey", limits=c(-5,4))+
        scale_x_continuous(name="Année | Year", breaks=seq(1970, my.year, 10))+
        scale_y_continuous(name="Âge | Age", breaks=seq(1,10, 2))
}
ggarrange(plotlist=r, common.legend = T, legend="right")
ggsave(paste(.wd,"caa_res.png"), width=12, height=8, units="in", dpi=600)



