#################################################################################################################
#*** Mackerel MSE
#*** Canadian mackerel (DFO, 2020)
#*** COMPARE Different M in operating models
#################################################################################################################

#################################################################################################################
########### READ IN DATA ########################################################################################
#################################################################################################################
year <- 2020
dir <- paste0('data/',year,'/')

nm.alv <- read.ices(paste0(dir,"nm_Alverson.dat"))
nm.zhang <- read.ices(paste0(dir,"nm_Zhang.dat"))
nm.gisl <- read.ices(paste0(dir,"nm_Gislason.dat"))
nm.gund <- read.ices(paste0(dir,"nm_Gunderson.dat"))
nm.DFO2017 <- read.ices(paste0(dir,"nm_DFO2017.dat"))

load(file='Rdata/input/dat_2020.Rdata')
load(file='Rdata/input/conf_2020.Rdata')
load(file='Rdata/input/par_2020.Rdata')

Mrange <- seq(0.15,0.5,0.01)
n <- length(Mrange)
ntot <- n # +5

mydats <- replicate(ntot,dat,simplify=FALSE)

for(i in 1:n){
    mydats[[i]]$natMor[] <- Mrange[i]
}
# mydats[[n+1]]$natMor[] <- nm.gund
# mydats[[n+2]]$natMor[] <- nm.alv
# mydats[[n+3]]$natMor[] <- nm.zhang
# mydats[[n+4]]$natMor[] <- nm.gisl
# mydats[[n+5]]$natMor[] <- nm.DFO2017

# Mtypes <- c(Mrange,'Gunderson','Alverson','Zhang','Gislason','DFO2017')
Mtypes <- c(Mrange)

#################################################################################################################
########### fit model ###########################################################################################
#################################################################################################################

Mruns <- lapply(mydats,function(x){ccam.fit(x,conf,par,silent = TRUE) })

class(Mruns) <- 'ccamset'
names(Mruns) <- Mtypes

save(Mruns,file='Rdata/2020/fit_compare/new_M.Rdata')
#load(file='Rdata/fit_compare/M.Rdata')

.wd <- 'img/2020/fit_compare/natural mortality new/'
dir.create(.wd, showWarnings = FALSE)

M_AICs <- Mruns %>% map_dfc(., AIC)
M_AICs <- t(M_AICs)
M_AICs <- as.data.frame(M_AICs) %>% mutate(M = seq(0.15,0.5,0.01))

M_AICs %>% ggplot(aes(M, V1)) + geom_point(size = 3) + theme_minimal(base_size = 14)

savepng(ssbplot(Mruns,ci=FALSE)+ scale_color_viridis_c(),.wd,"SSB",c(21,13))
savepng(catchplot(Mruns,ci=FALSE)+ scale_color_viridis_c(),.wd,"catch",c(21,13))
savepng(recplot(Mruns,ci=FALSE)+ scale_color_viridis_c() + xlim(1969,2020) + ylim(0,800000),.wd,"recruitment",c(21,13))
savepng(fitplot(Mruns,type='AIC',n=FALSE)+ scale_color_viridis_c(),.wd,"AIC",c(16,8))
savepng(fitplot(Mruns,type='nll',n=FALSE)+ scale_color_viridis_c(),.wd,"nll",c(16,8))

# test <-Mruns[c('DFO2017','0.27')]
# class(test) <- 'ccamset'
# savepng(ssbplot(test,ci=FALSE),.wd,"SSB_DFO_17.19",c(21,13))
# 
# 
# df <- data.frame(M=Mtypes,LRP=unlist(lapply(ypr(Mruns),'[','f40ssb'))*0.4)
# mLRP <- ggplot(df[df$M %in% c('0.27','DFO2017'),],aes(x=M,y=LRP))+geom_point()+
#     theme(axis.text.x = element_text(angle = 90, hjust = 1))
# saveplot(mLRP,name='LRP',dim=c(8,6),wd=.wd)
