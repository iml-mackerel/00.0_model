#################################################################################################################
#*** Mackerel Stock Evaluation
#*** Plot all input data
#################################################################################################################

wd  <-  'img/data'
type  <- 'png'

### cn (catch-at-age in numbers)
p1 <- bubble(cn,col='darkblue',scale = 10)+ggtitle('Raw')
saveplot(p1,name='cn_raw',dim=c(15,8),wd=wd,type=type)  # raw data

p2 <- bubble(t(spay(t(cn))),scale = 8)+ggtitle('Standardised by year')
saveplot(p2,name='cn_spay_compCohorts',dim=c(15,8),wd=wd,type=type)  # to check cohorts

p3 <- bubble(t(spya(t(cn))),scale = 8)+ggtitle('Standardised by age')
saveplot(p3,name='cn_spya_compYears',dim=c(15,8),wd=wd,type=type)  # to compare between years

p4 <- plotobs(dat,fleets=2,type='bar')+scale_x_continuous(expand=c(0,0))+scale_y_continuous(expand=c(0,0))+ylab('Proportion')
saveplot(p4,name='cn_prop',dim=c(15,8),wd=wd,type=type)  # proportion in population

## proportion mature
p5 <- heat(mo)
saveplot(p5,name='pm_heat',dim=c(15,8),wd=wd,type=type)  

p6 <- prettymatplot(mo,ylab='Proportion mature',xlab='Year')
saveplot(p6,name='pm_line',dim=c(15,8),wd=wd,type=type)  

## stock weight
p7 <- heat(sw)
saveplot(p7,name='sw_heat',dim=c(15,8),wd=wd,type=type)  

p8 <- prettymatplot(sw,ylab='Stock weight',xlab='Year')
saveplot(p8,name='sw_line',dim=c(15,8),wd=wd,type=type) 

## catch weight
p9 <- heat(cw)
saveplot(p9,name='cw_heat',dim=c(15,8),wd=wd,type=type)  

p10 <- prettymatplot(sw,ylab='Catch weight',xlab='Year')
saveplot(p10,name='cw_line',dim=c(15,8),wd=wd,type=type) 

## survey
update_geom_defaults("line", list(size = 1))
p11 <- surveyplot(surveys)+ylab('SSB')+scale_y_continuous(limits=c(0,2000000),expand = c(0,0))
saveplot(p11,name='survey',dim=c(15,8),wd=wd,type=type) 

## total catch
p12 <- prettymatplot(ct,ylab='Catch (t)', xlab='Year',col=c('black','darkgrey'))
saveplot(p12,name='ct',dim=c(15,8),wd=wd,type=type) 

p13 <- prettymatplot(ctwusa,ylab='Catch (t)', xlab='Year',col=c('black','darkgrey'))
saveplot(p13,name='ct_wusa',dim=c(15,8),wd=wd,type=type) 

p14 <- prettymatplot(sweep(ct,1,ct[,1],'/'),col=c('darkgrey','black'),ylab = 'Crel')
saveplot(p14,name='ct_rel',dim=c(15,8),wd=wd,type=type) 

p15 <- prettymatplot(sweep(ctwusa,1,ctwusa[,1],'/'),col=c('darkgrey','black'),ylab = 'Crel')
saveplot(p15,name='ct_rel_wusa',dim=c(15,8),wd=wd,type=type) 

allC <- cbind(Canada=ct[,1],USA=ctUSA[-c(1:8),1],Foreign=ctForeign[-c(1:8),1])
allC <- cbind(allC, Total=rowSums(allC))
p16 <- prettymatplot(allC,ylab='Catch (t)', xlab='Year',col=c('orange','yellowgreen','mediumorchid','black'))
saveplot(p16,name='ct_all',dim=c(15,8),wd=wd,type=type) 




