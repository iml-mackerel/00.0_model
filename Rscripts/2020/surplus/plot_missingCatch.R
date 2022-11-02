#################################################################################################################
#*** Mackerel MSE
#*** Plot missing catch
#*** based on CCAM package
#################################################################################################################
library(zoo)

x <- get(load(file='Rdata/fit/fit.Rdata'))
ct <- read.ices('data/2018/ct.dat')

x <- fitBase_run_2

catch <- catchtable(x)
d <- x$data
ix <- d$idx1[1,]+1
catch[,c('low','high')]<-exp(d$logobs[ix,])
catchm <- melt(catch[,-c(2,3)],id=c('year'))

pc<-ggplot(catchm,aes(x=year))+
    geom_line(aes(size=variable,y=value,col=variable))+
    geom_ribbon(data=catch,aes(ymin=low,ymax=Estimate),fill='darkred',alpha=0.5)+
    scale_color_manual(values=c('black','darkgrey','darkgrey'))+
    labs(col='',linetype='',y='Catch (t)',x='Year')+
    scale_size_manual(values=c(1,0.3,0.3))+
    theme(legend.position = 'none')

catch$missing <- catch$Estimate - ct[,1]
catch$missingmean <- c(rep(NA,5),rollmean(catch$missing,6,align='right')) #resdoc 2017: 2011-2016
pm <- ggplot(catch,aes(x=year,y=missing))+geom_line(size=1)+
    geom_line(data=catch,aes(y=missingmean),col='green',size=1.5)+
    geom_hline(yintercept=6000,linetype='dashed',col='darkgrey')

saveplot(pc,name='catch_missing',dim=c(10,6),'img/resdoc')  # raw data
saveplot(pm,name='catch_missing_Thomasmethod',dim=c(10,6),'img/resdoc')  # raw data


