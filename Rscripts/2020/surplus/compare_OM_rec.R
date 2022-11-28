#################################################################################################################
#*** Mackerel MSE
#*** Canadian mackerel (DFO, 2020)
#*** COMPARE Different recruitment types in operating models
#################################################################################################################

#################################################################################################################
########### READ IN DATA ########################################################################################
#################################################################################################################

load(file = 'Rdata/input/dat_base_2020.Rdata')
load(file = 'Rdata/input/confbase_2020.Rdata')
load(file = 'Rdata/input/parbase_2020.Rdata')

RECrange <- c(0,2) #rw and BH

for (i in RECrange) {
    conf$stockRecruitmentModelCode <- i
    par <- defpar(dat,conf)
    fit <- ccam.fit(dat,conf,par,silent = TRUE)           
    save(fit, file = paste0('Rdata/2020/fit_compare/rec',i,'.Rdata'))
}

#################################################################################################################
########### fit model ###########################################################################################
#################################################################################################################

filenames <- dir('Rdata/2020/fit_compare', pattern = "rec")
files <- paste0('Rdata/2020/fit_compare','/',filenames)
RECruns <- lapply(files, function(x) {print(x);get(load(x))})
class(RECruns) <- 'ccamset'
names(RECruns) <- c('RW','BH')

save(RECruns,file='Rdata/2020/fit_compare/REC.Rdata')
#load(file='Rdata/fit_compare/REC.Rdata')

.wd <- 'img/2020/fit_compare/rec/'
dir.create(.wd, showWarnings = FALSE)

savepng(ssbplot(RECruns,ci = FALSE),.wd,"SSB",c(17,10))
savepng(catchplot(RECruns,ci = FALSE),.wd,"catch",c(17,10))
savepng(recplot(RECruns,ci = FALSE) + xlim(1968,2020) + ylim(0,900000),.wd,"recruitment",c(17,10))
savepng(fitplot(RECruns,type = 'AIC'),.wd,"AIC",c(14,6))
savepng(fitplot(RECruns,type = 'nll'),.wd,"nll",c(14,6))
savepng(srplot(RECruns,curve = TRUE),.wd,"sr",c(18,12))
savepng(recplot(RECruns,ci = FALSE,trans = log),.wd,"recruitment_log",c(10,4))


x <- RECruns[[2]]#BH
### reference points
refBase <- ypr(x,rec.years = 1969:2016)


p <- ssbplot(x) + scale_y_continuous(limits = c(0,10e5),expand = c(0,0)) +
    # geom_hline(yintercept = refBase$f40ssb)+
    # geom_hline(yintercept = refBase$f40ssb*0.8,col='darkgreen')+
    # geom_hline(yintercept = refBase$f40ssb*0.4,col='darkred')+
    geom_hline(yintercept = refBase$ssbmsy) +
    geom_hline(yintercept = refBase$ssbmsy*0.8,col = 'darkgreen') +
    geom_hline(yintercept = refBase$ssbmsy*0.4,col = 'darkred')    

# ssbplot() was showing an error indicating that there was no method for it to plot ccamset object
p2 <- ssbplot(x,years = 2000:2018) + scale_y_continuous(limits = c(0,5e5),expand = c(0,0)) + geom_hline(yintercept = refBase$f40ssb)+
    geom_hline(yintercept = refBase$f40ssb*0.8,col = 'darkgreen') +
    geom_hline(yintercept = refBase$f40ssb*0.4,col = 'darkred')

# saveplot(p,name = 'ssb_rp',dim = c(10,6),wd = .wd,type = type)
# saveplot(p2,name = 'ssb_rp_end',dim = c(10,6),wd = .wd,type = type)

saveplot(p,name = 'ssb_rp',dim = c(10,6),wd = .wd)
saveplot(p2,name = 'ssb_rp_end',dim = c(10,6),wd = .wd)


