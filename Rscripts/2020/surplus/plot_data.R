#################################################################################################################
#*** Mackerel Stock Evaluation
#*** Plot all input data
#################################################################################################################

wd  <-  paste0('img/',year,'/data')
dir.create(wd,showWarnings = F,recursive = T)
type  <- 'png'

update_geom_defaults("line", list(size = 1))

### cn (catch-at-age in numbers)
p1 <- bubble(cn,col=c('black','grey'),scale = 8)
saveplot(p1,name='cn_raw',dim=c(15,8),wd=wd,type=type)  # raw data

p2 <- bubble(t(spay(t(cn))),scale = 8)+ggtitle('Standardised by year')
saveplot(p2,name='cn_spay',dim=c(15,8),wd=wd,type=type)  # to check cohorts

p3 <- bubble(t(spya(t(cn))),scale = 8)+ggtitle('Standardised by age')
saveplot(p3,name='cn_spya',dim=c(15,8),wd=wd,type=type)  # to compare between years

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
p11 <- surveyplot(survey)+ylab('survey')+scale_y_continuous(expand = c(0,0))
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

cprov <- read.csv2('data/2018/raw/ct_prov.csv',sep=',')[,-7]

call <- type.convert(merge(cprov,cbind(data.frame(ct),Year=rownames(ct)),all=T))
call<- melt(call,id=c('Year','min','max'),variable.name = 'province')
p17 <-  prettymatplot(ctwusa,ylab='Catch (t)', xlab='Year',col=c('black','darkgrey'),legend=F)+
    geom_bar(data=call,aes(x=Year,y=value,fill=province),stat = 'identity',alpha=0.8)+
    scale_fill_viridis_d()+
    theme(legend.position = c(0.11,0.7))


### combination
saveplot(grid.arrange(p8+theme(legend.position = 'none'),
                      p6+theme(legend.position = 'none'),
                      p1+theme(legend.position = 'none'),
                      p11+theme(legend.position = 'none')
                      ,ncol=2),name='all',dim=c(18,12),wd=wd,type=type)

saveplot(grid.arrange(p17+ggtitle('Total Catch'),
                      p1+theme(legend.position = 'none')+ggtitle('Catch-at-age'),
                      prettymatplot(surveys[[1]],ylab='TEP',xlab='Year')+ggtitle('Survey index'),ncol=3),
         name='alldat',dim=c(30,8),wd=wd,type=type)

saveplot(grid.arrange(p17+ggtitle('Total Catch'),
                      p1+theme(legend.position = 'none')+ggtitle('Catch-at-age'),
                      prettymatplot(surveys[[1]],ylab='TEP',xlab='Year')+ggtitle('Survey index'),ncol=1),
         name='alldatvertical',dim=c(12,24),wd=wd,type=type)

