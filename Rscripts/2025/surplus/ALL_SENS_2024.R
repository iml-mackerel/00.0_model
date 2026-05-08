source("C:/LEHOUX/Maquereau/iml-mackerel/00.0_model/Rscripts/2024/surplus/ssbplot0.R")

year=2024

load("C:/LEHOUX/Maquereau/iml-mackerel/00.0_model/Rdata/2024/sensitivity/sel.Rdata")
selruns

load("C:/LEHOUX/Maquereau/iml-mackerel/00.0_model/Rdata/2024/sensitivity/fits.caa.Rdata")
fits.caa<- fits.caa[2:3]
class(fits.caa) <- 'ccamset'
load("C:/LEHOUX/Maquereau/iml-mackerel/00.0_model/Rdata/2024/sensitivity/fits.usa.Rdata")
fits.usa<- fits.usa[2:3]
class(fits.usa) <- 'ccamset'
load("C:/LEHOUX/Maquereau/iml-mackerel/00.0_model/Rdata/2024/sensitivity/fits.var.Rdata")
fits.var<- fits.var[c(1,3:7)]
class(fits.var) <- 'ccamset'
load("C:/LEHOUX/Maquereau/iml-mackerel/00.0_model/Rdata/2024/sensitivity/fitsC.Rdata")
Cruns<- fits.caa[1:2]
class(Cruns) <- 'ccamset'
s <- list.files(paste0('Rdata/',year,'/sensitivity/'),'.M.0',full.names = TRUE,)
fits.Mct <- lapply(s,function(x)get(load(x)))
names(fits.Mct) <- gsub(".Rdata","",gsub(paste0('Rdata/',year,'/sensitivity/fit.M.'),"",s))
class(fits.Mct) <- "ccamset"
#fits.Mct <- fits.Mct[10:11]


s <- list.files(paste0('Rdata/',year,'/sensitivity/'),'egg',full.names = TRUE,)
fits.egg <- lapply(s,function(x)get(load(x)))
class(fits.egg) <- "ccamset"


allsens<- c(selruns, fits.caa, fits.usa, fits.var, Cruns, fits.Mct, fits.egg)
class(allsens) <- 'ccamset'
names(allsens) <- c(paste0("sens", 1:length(allsens)))
#
res <- cbind(ssbtable(allsens)[,c("Estimate", "year", "fit")],
                  rec=rectable(allsens)[,1],
                  f=fbartable(allsens)[,1]) %>% 
    as.matrix() %>%  as.data.frame() %>%  dplyr::rename(ssb=Estimate)
                  ##  LRP=ypr(fit)$LRP,
                 #ratio=ssbtable(fit)[,1]/ypr(fit)$LRP)
ssbplot0(fit=allsens, ci=F, language="BI", minyear=1969, year, legend=T)

ggsave("img/2024/sensitivity/all_sens.png", width=8, height=5 , dpi=600)

    