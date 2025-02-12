#################################################################################################################
#*** Mackerel assessment
#*** Canadian mackerel (DFO, 2023)
#*** what if 2022-2024 CAA removed, what if alternative CAA
#################################################################################################################

source(paste0('Rscripts/',year, '/surplus/read_data.R'))
.wd <- paste0("img/",year,"/sensitivity/CAA/")
type  <- 'png'

# run with different options of cn in final year (or first year data removed) #################################################################
cn_modif22_23 <- read.ices(paste0(dir,'/sensitivity/modified2022_2023.dat'))  %>%  as.data.frame()
cn_default <- read.ices(paste0(dir,'/sensitivity/cn_default.dat')) %>%  as.data.frame()
cn_modif22_24 <- read.ices(paste0(dir,'/sensitivity/modified2022_2024.dat')) %>%  as.data.frame()

dats <- lapply(list(cn_default,cn_modif22_23,cn_modif22_24),function(x){
    setup.ccam.data(surveys=survey,
                    residual.fleet=x[-1,],
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

fits.caa <- lapply(dats,function(x)ccam.fit(x,conf,par,debug=T))
names(fits.caa) <- c('default','modified 2022-2023','modified 2022-2024')
class(fits.caa) <- "ccamset"

save(fits.caa, file=paste0('Rdata/',year,'/sensitivity/fits.caa.Rdata'))

# compare all three

load(paste0('Rdata/',year,'/sensitivity/fits.caa.Rdata'))
ssbplot0(fits.caa, minyear=2010, year=year, legend=T)
ggsave(paste0(.wd,"ssb_noci2010.png"), width=6, height=5, units="in", bg="white")


saveplot(fitplot(fits.caa,type='AIC', n=F),name='AIC',dim=c(12,10),wd=.wd, type=type)
tab <- as.data.frame(modeltable(fits.caa))
tab$fit <- rownames(tab)
names(tab)[2] <-'par'
tab$AIC=round(tab$AIC)
write.csv(tab,file=paste0("img/",my.year,"/sensitivity/CAA/AIC_CAA.csv"), row.names=F)


nol <- theme(legend.position = 'none')
saveplot(grid.arrange(
    arrangeGrob(
        resplot(fits.caa$default,fleets = 2,type=6)+ggtitle('default'),
        resplot(fits.caa$default,fleets = 2,type=2)+nol,
        resplot(fits.caa$default,fleets = 2,type=3)+nol,ncol=1),
    arrangeGrob(
        resplot(fits.caa$`modified 2022-2023`,fleets = 2,type=6)+ggtitle('`modified 2022-2023`'),
        resplot(fits.caa$`modified 2022-2023`,fleets = 2,type=2)+nol,
        resplot(fits.caa$`modified 2022-2023`,fleets = 2,type=3)+nol,ncol=1),
    arrangeGrob(
        resplot(fits.caa$`modified 2022-2024`,fleets = 2,type=6)+ggtitle('modified 2022-2024'),
        resplot(fits.caa$`modified 2022-2024`,fleets = 2,type=2)+nol,
        resplot(fits.caa$`modified 2022-2024`,fleets = 2,type=3)+nol,ncol=1),
    ncol=3),
    name="/res_all",dim=c(24,16),wd=.wd,type=type)


