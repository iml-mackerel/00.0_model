#################################################################################################################
#*** Mackerel MSE
#*** Plot missing catch
#*** based on CCAM package
#################################################################################################################
library(zoo)

ct <- read.ices(paste0('data/',year,'/ct.dat'))

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

saveplot(pc,name='catch_missing',dim=c(10,6),paste0('img/',year,'/fit'))  # raw data

