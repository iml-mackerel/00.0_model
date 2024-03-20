#################################################################################################################
#*** Mackerel assessment
#*** Canadian mackerel (DFO, 2023)
#*** Compare new to old fit: at request Kiersten
#################################################################################################################

.wd <- "img/2022/fit_compare/oldnew/"
type  <- 'png'

load("C:/Users/VANBE/Desktop/post-doc/DATA/iml-mackerel/00.0_model/Rdata/2020/fit.Rdata")
fit2020 <- fit
load("C:/Users/VANBE/Desktop/post-doc/DATA/iml-mackerel/00.0_model/Rdata/2022/fit.Rdata")
fit2022 <- fit
load("C:/Users/VANBE/Desktop/post-doc/DATA/iml-mackerel/00.0_model/Rdata/2022/fit_retro/fit.2.Rdata")
fit2022ret <- fitr
fits <- c(fit2020,fit2022,fit2022ret)
names(fits) <- c('2020','2022','2022retro')
saveplot(plot(fits,col=c('darkblue','darkred','darkgreen')),name='comp_3',dim=c(12,16),wd=.wd,type=type)
saveplot(plot(fits,col=c('darkblue','darkred','darkgreen'),ci=FALSE),name='comp_3_noci',dim=c(12,16),wd=.wd,type=type)

f <- c(fit2020,fit2022)
names(f) <- c('2020','2022')
saveplot(plot(f,col=c('darkblue','darkred')),name='comp_oldnew',dim=c(12,16),wd=.wd,type=type)

f <- c(fit2020,fit2022ret)
names(f) <- c('2020','2022ret')
saveplot(plot(f,col=c('darkblue','darkgreen')),name='comp_2',dim=c(12,16),wd=.wd,type=type)
saveplot(plot(f,col=c('darkblue','darkgreen'),ci=FALSE),name='comp_2noci',dim=c(12,16),wd=.wd,type=type)


out <- ldply(1:length(fits),function(x){
    data.frame(year=ssb0table(fits[[x]])[,4],
               ssb0=ssb0table(fits[[x]])[,1],
               ssb=ssbtable(fits[[x]])[,1],
               rec=rectable(fits[[x]])[,1],
               f=fbartable(fits[[x]])[,1],
               catch=catchtable(fits[[x]])[,1],
               exploit=exptable(fits[[x]])[,1]*100,
               LRP=ypr(fits[[x]])$LRP,
               ratio=ssbtable(fits[[x]])[,1]/ypr(fits[[x]])$LRP,
               run=names(fits)[x])
})

write.csv(out,paste0("csv/2022/out_oldnew.csv"),row.names = FALSE)


