#################################################################################################################
#*** Mackerel Stock Evaluation
#*** Plot all input data
#################################################################################################################

theme_set(catchR::theme_mackerel())

wd  <-  paste0('img/',year,'/data')
type  <- 'png'

### cn (catch-at-age in numbers)
p1 <- bubble(cn,col=c('black','grey'),scale = 8)+scale_y_continuous(breaks=1:10)
saveplot(p1,name='cn_raw',dim=c(15,8),wd=wd,type=type)  # raw data

p1a <- bubble(sweep(cn,1,rowSums(cn),"/"),col=c('black','grey'),scale = 8)+scale_y_continuous(breaks=1:10)
saveplot(p1a,name='cn_prop',dim=c(15,8),wd=wd,type=type)  # raw data

p2 <- bubble(t(spay(t(cn))),scale = 8)+ggtitle('Standardised by year')+scale_y_continuous(breaks=1:10)
saveplot(p2,name='cn_spay',dim=c(15,8),wd=wd,type=type)  # to check cohorts

p3 <- bubble(t(spya(t(cn))),scale = 8)+ggtitle('Standardised by age')+scale_y_continuous(breaks=1:10)
saveplot(p3,name='cn_spya',dim=c(15,8),wd=wd,type=type)  # to compare between years

p4 <- plotobs(dat,fleets=2,type='bar')+scale_x_continuous(expand=c(0,0))+scale_y_continuous(expand=c(0,0))+ylab('Proportion')
saveplot(p4,name='cn_propbar',dim=c(15,8),wd=wd,type=type)  # proportion in population



## proportion mature
p5 <- heat(mo)
saveplot(p5,name='pm_heat',dim=c(15,8),wd=wd,type=type)  

p6 <- prettymatplot(mo,ylab='Proportion mature',xlab='Year')+scale_x_continuous(expand = c(0,0))+scale_y_continuous(limits=c(0,1),expand=c(0,0))
saveplot(p6,name='pm_line',dim=c(15,8),wd=wd,type=type)  

## stock weight
p7 <- heat(sw)
saveplot(p7,name='sw_heat',dim=c(15,8),wd=wd,type=type)  

p8 <- prettymatplot(sw,ylab='Stock weight (kg)',xlab='Year')+theme(legend.position = 'none')
saveplot(p8,name='sw_line',dim=c(15,8),wd=wd,type=type) 

## catch weight
p9 <- heat(cw)
saveplot(p9,name='cw_heat',dim=c(15,8),wd=wd,type=type)  

p10 <- prettymatplot(cw,ylab='Catch weight (kg)',xlab='Year')+theme(legend.position = 'none')
saveplot(p10,name='cw_line',dim=c(15,8),wd=wd,type=type) 

## survey
update_geom_defaults("line", list(size = 1))
survey[[1]][,1] <- survey[[1]][,1]/10^12
p11 <- surveyplot(survey)+ylab('TEP (billions)')+scale_y_continuous(limits=c(0,max(survey[[1]][,1])),expand = c(0,0))
saveplot(p11,name='survey',dim=c(15,8),wd=wd,type=type) 

d <- data.frame(Year=as.numeric(rownames(survey[[1]])),TEP=survey[[1]][,1])
d$mismatch <- "test"[d$Year %in% c(2006,2017,2019)]
d$uncertainS <- d$Year %in% c(2022)
p11b <- ggplot(d,aes(x=Year,y=TEP))+
    geom_line()+
    geom_point(aes(color=mismatch))+
    scale_y_continuous(limits=c(0,max(survey[[1]][,1])*1.05),expand = c(0,0))+
    labs(y='TEP (billions)')+
    scale_color_manual(values=c('black','red'))+
    theme(legend.position = c(0.8,0.8))

saveplot(p11b,name='survey_mismatch',dim=c(15,8),wd=wd,type=type) 

## total catch
p12 <- prettymatplot(ct,ylab='Catch (t)', xlab='Year',col=c('black','darkgrey'))
saveplot(p12,name='ct',dim=c(15,8),wd=wd,type=type) 

p13 <- prettymatplot(ctwusa,ylab='Catch (t)', xlab='Year',col=c('black','darkgrey'))
saveplot(p13,name='ct_wusa',dim=c(15,8),wd=wd,type=type) 

p14 <- prettymatplot(sweep(ct,1,ct[,1],'/'),col=c('darkgrey','black'),ylab = 'Crel')
saveplot(p14,name='ct_rel',dim=c(15,8),wd=wd,type=type) 

p15 <- prettymatplot(sweep(ctwusa,1,ctwusa[,1],'/'),col=c('darkgrey','black'),ylab = 'Crel')
saveplot(p15,name='ct_rel_wusa',dim=c(15,8),wd=wd,type=type) 

allC <- cbind(Canada=ct[,1],USA=ctUSA[,1],Foreign=ctForeign[,1])
allC <- cbind(allC, Total=rowSums(allC))
p16 <- prettymatplot(allC,ylab='Catch (t)', xlab='Year',col=c('orange','yellowgreen','mediumorchid','black'))
saveplot(p16,name='ct_all',dim=c(15,8),wd=wd,type=type) 


