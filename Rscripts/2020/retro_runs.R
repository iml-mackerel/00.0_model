# retro runs as SAM::retro and CCAM::retro not working

# load all file first from run_CCAM.R
tep1 <- read.ices("data/2020/tep_2020_retro1.dat") # Total egg production (billions of eggs) = (daily_egg_production (N/m^2) * survey area (m^2)) / spawning fraction at median mission date
tep2 <- read.ices("data/2020/tep_2020_retro2.dat")
tep3 <- read.ices("data/2020/tep_2020_retro3.dat")
tep4 <- read.ices("data/2020/tep_2020_retro4.dat")
tep5 <- read.ices("data/2020/tep_2020_retro5.dat")
tep6 <- read.ices("data/2020/tep_2020_retro6.dat")
tep7 <- read.ices("data/2020/tep_2020_retro7.dat")

tep6[[1]] <- tep6[[1]][!is.na(tep6[[1]]), 1, drop = FALSE] # setup data 
tep6[[1]][,1] <- tep6[[1]][,1] * 1000000000000
attr(tep6[[1]],'time') <- c(0.47)


dat <- setup.ccam.data(surveys = tep6,
                       residual.fleet = cn[1:47,],
                       total.catch = ctwusa[1:47,],
                       prop.mature = mo_smoothed[1:47,],
                       stock.mean.weight = cw_smooth[1:47,],
                       stock.start.weight = cw_smooth[1:47,],
                       catch.mean.weight = cw_smooth[1:47,],
                       dis.mean.weight = cw_smooth[1:47,],
                       land.mean.weight = cw_smooth[1:47,],
                       prop.f = pf[1:47,],
                       prop.m = pm[1:47,],
                       natural.mortality = nm[1:47,],
                       land.frac = lf[1:47,],
                       prop.fem = pfem[1:47,],
                       fec = fec_smooth[1:47,])

# Setup basic minimal configuration for CCAM model. See ?CCAM::defcon for definitions of terms
conf <- defcon(dat)

conf$keySel <- matrix(c(0,1,2,3,4,4,4,4,4,4), nrow = nrow(conf$keySel), ncol = ncol(conf$keySel),byrow = T) # selectivity structure
conf$keyVarObs[1,] = -1 # observation variance structure                    
conf$keyVarObs[2,1:9] = c(0,1,2,2,2,2,2,2,1) 
conf$keyVarObs[3,1] = 3           
conf$stockRecruitmentModelCode = 2 # options: 0 = random walk, 1 = Ricker (i.e. density dependance), 2 = Beverton-Holt, 3 = mean (over n years?)
conf$obsLikelihoodFlag[1] = 'CE'# fleet type 3
conf$keyBiomassTreat[3] = 5
conf$fbarRange = c(5,10) # Fully recruited fish. Based on sensitivity runs of selectivity by EVB in 2017

# model parameterization
par <- defpar(dat,conf)

# save each runs input 
save(dat,file = 'Rdata/2020/input/dat_retro6_2020.Rdata')
save(conf,file = 'Rdata/2020/input/conf_retro6_2020.Rdata')
save(par,file = 'Rdata/2020/input/par_retro6_2020.Rdata')

# Plot input data (automatically stored in "./img/data/)

# source('Rscripts/2020/surplus/plot_data.R')


#####################  Fit Model  ####################


# Run the model and name the object something appropriate
fit <- ccam.fit(dat, conf, par, silent = FALSE, paracheck = FALSE, debug = T)            
fit # check convergence
AIC(fit) # model AIC - Important for comparing different M scenarios only

# save the run and name it appropriately to not overwrite your data
save(fit, file = 'Rdata/2020/fit_compare/fit_retro6_2020.Rdata')


fit1 <-fit
fit2 <-fit
fit3 <-fit
fit4 <-fit
fit5 <-fit
fit6 <-fit
fit7 <-fit


#new figs august 2021
retro_ssb <-ssbplot(c(fit2020,fitr1,fitr2,fitr3,fitr4,fitr5,fitr6,fitr7),ci=FALSE, linesize = 1) + 
    scale_y_continuous(n.breaks=8)+
    scale_x_continuous(n.breaks=10)+
    scale_colour_viridis_d(name = "Peel", labels = c("2020", "2019", "2018", "2017", "2016", "2015", "2014", "2013")) +
    theme_minimal()

retro_ssb2 <-ssbplot(c(fit2020,fitr1,fitr2,fitr3,fitr4,fitr5,fitr6,fitr7),ci=FALSE, linesize = 1) + 
    scale_y_continuous(n.breaks=8)+
    scale_x_continuous(n.breaks=10)+
    coord_cartesian(xlim=c(2010, 2020),ylim=c(0,100000))+
    scale_colour_viridis_d(name = "Peel", labels = c("2020", "2019", "2018", "2017", "2016", "2015", "2014", "2013")) +
    theme_minimal()

retro_ssb3 <-ssbplot(c(fit2020,fitr1,fitr2,fitr3,fitr4,fitr5,fitr6,fitr7),ci=TRUE, linesize = 1) + 
    scale_y_continuous(n.breaks=8)+
    scale_x_continuous(n.breaks=10)+
    coord_cartesian(xlim=c(2010, 2020),ylim=c(0,150000))+
    scale_colour_viridis_d(name = "Peel", labels = c("2020", "2019", "2018", "2017", "2016", "2015", "2014", "2013")) +
    scale_fill_viridis_d(name = "Peel", labels = c("2020", "2019", "2018", "2017", "2016", "2015", "2014", "2013")) +
    theme_minimal()

retro_rec <-recplot(c(fit2020,fitr1,fitr2,fitr3,fitr4,fitr5,fitr6,fitr7),ci=FALSE, linesize = 1) + 
    scale_y_continuous(n.breaks=8, limits = c(0, 600000))+
    scale_x_continuous(n.breaks=10)+
    coord_cartesian(xlim=c(1969, 2020))+
    scale_colour_viridis_d(name = "Peel", labels = c("2020", "2019", "2018", "2017", "2016", "2015", "2014", "2013")) +
    theme_minimal()

retro_rec2 <-recplot(c(fit2020,fitr1,fitr2,fitr3,fitr4,fitr5,fitr6,fitr7),ci=FALSE, linesize = 1) + 
    scale_y_continuous(n.breaks=8, limits = c(0, 3e05))+
    scale_x_continuous(n.breaks=10)+
    coord_cartesian(xlim=c(2010, 2020))+
    scale_colour_viridis_d(name = "Peel", labels = c("2020", "2019", "2018", "2017", "2016", "2015", "2014", "2013")) +
    theme_minimal()

retro_rec3 <-recplot(c(fit2020,fitr1,fitr2,fitr3,fitr4,fitr5,fitr6,fitr7),ci=TRUE, linesize = 1) + 
    scale_y_continuous(n.breaks=8, limits = c(0, 5e05))+
    scale_x_continuous(n.breaks=10)+
    coord_cartesian(xlim=c(2010, 2020))+
    scale_colour_viridis_d(name = "Peel", labels = c("2020", "2019", "2018", "2017", "2016", "2015", "2014", "2013")) +
    scale_fill_viridis_d(name = "Peel", labels = c("2020", "2019", "2018", "2017", "2016", "2015", "2014", "2013")) +
    theme_minimal()

retro_fbar <-fbarplot(c(fit2020,fitr1,fitr2,fitr3,fitr4,fitr5,fitr6,fitr7),ci=FALSE, linesize = 1) + 
    scale_y_continuous(n.breaks = 8, limits = c(0, 3)) + 
    scale_x_continuous(n.breaks=10)+
    # coord_cartesian(xlim=c(1969, 2020))+
    scale_colour_viridis_d(name = "Peel", labels = c("2020", "2019", "2018", "2017", "2016", "2015", "2014", "2013")) +
    theme_minimal()

retro_fbar2 <-fbarplot(c(fit2020,fitr1,fitr2,fitr3,fitr4,fitr5,fitr6,fitr7),ci=F, linesize = 1) + 
    scale_y_continuous(n.breaks = 8) + 
    scale_x_continuous(n.breaks=10)+
    coord_cartesian(xlim=c(2010, 2020))+
    scale_colour_viridis_d(name = "Peel", labels = c("2020", "2019", "2018", "2017", "2016", "2015", "2014", "2013")) +
    theme_minimal()

retro_fbar3 <-fbarplot(c(fit2020,fitr1,fitr2,fitr3,fitr4,fitr5,fitr6,fitr7),ci=TRUE, linesize = 1) + 
    scale_y_continuous(n.breaks = 8) + 
    scale_x_continuous(n.breaks=10)+
    coord_cartesian(xlim=c(2010, 2020))+
    scale_colour_viridis_d(name = "Peel", labels = c("2020", "2019", "2018", "2017", "2016", "2015", "2014", "2013")) +
    scale_fill_viridis_d(name = "Peel", labels = c("2020", "2019", "2018", "2017", "2016", "2015", "2014", "2013")) +
    theme_minimal()

library(cowplot)
retroplot <- plot_grid(retro_ssb, retro_rec, retro_fbar, ncol = 1)
retroplot2 <- plot_grid(retro_ssb2, retro_rec2, retro_fbar2, ncol = 1)
retroplot3 <- plot_grid(retro_ssb3, retro_rec3, retro_fbar3, ncol = 1)


saveplot(retroplot,name = "retroplot_new",dim=c(15,15),wd='img/2020/fit_compare')
saveplot(retroplot2,name = "retroplot2_new",dim=c(15,15),wd='img/2020/fit_compare')
saveplot(retroplot3,name = "retroplot3_new",dim=c(15,15),wd='img/2020/fit_compare')


### older
retro_ssb <- ssbplot(c(fit, fit1, fit2, fit3, fit4, fit5, fit6, fit7), ci = F, linesize = 1) +
    scale_y_continuous(n.breaks = 8) + 
    scale_colour_viridis_d(name = "Peel", labels = c("2020", "2019", "2018", "2017", "2016", "2015", "2014", "2013")) + 
    theme_minimal() +theme(legend.position = "none") + 
    labs(x="", y = "SSB")
saveplot(retro_ssb, name = "retro_ssb",dim=c(15,15),wd='img/2020/fit_compare')

retro_rec <- recplot(c(fit, fit1, fit2, fit3, fit4, fit5, fit6, fit7), ci = F, linesize = 1) +
    scale_x_continuous(limits = c(1969,2020)) +
    scale_y_continuous(n.breaks = 8, limits = c(0, 600000)) + 
    # ylim(0,300000) + 
    scale_colour_viridis_d(name = "Peel", labels = c("2020", "2019", "2018", "2017", "2016", "2015", "2014", "2013")) + theme_minimal() +theme(legend.position = "none") + labs(x="")
saveplot(retro_rec,name = "retro_rec",dim=c(15,15),wd='img/2020/fit_compare')

retro_fbar <- fbarplot(c(fit, fit1, fit2, fit3, fit4, fit5, fit6, fit7), ci = F, linesize = 1) + 
    scale_x_continuous() +
    scale_y_continuous(n.breaks = 8, limits = c(0, 3)) + 
    scale_colour_viridis_d(name = "Peel", labels = c("2020", "2019", "2018", "2017", "2016", "2015", "2014", "2013")) + 
    theme_minimal() +
    theme(legend.position = "none")
saveplot(retro_fbar,name = "retro_fbar",dim=c(15,15),wd='img/2020/fit_compare')


library(cowplot)
retroplot <- plot_grid(retro_ssb, retro_rec, retro_fbar, ncol = 1)
saveplot(retroplot,name = "retroplot",dim=c(15,15),wd='img/2020/fit_compare')
