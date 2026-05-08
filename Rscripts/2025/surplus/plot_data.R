#################################################################################################################
#*** Mackerel Stock Evaluation
#*** Plot all input data
#################################################################################################################
source("R/saveplot.R")
theme_set(catchR::theme_mackerel())

wd  <-  paste0('img/',year,'/data')
type  <- 'png'

### cn (catch-at-age in numbers)
p1 <- bubble(cn,col=c('black','grey'),scale = 8)+scale_y_continuous(breaks=1:10)
saveplot(p1,name='cn_raw',dim=c(15,8),wd=wd,type=type)  # raw data

p1a <- bubble(sweep(cn,1,rowSums(cn),"/"),col=c('black','grey'),scale = 8)+scale_y_continuous(breaks=1:10) +
    geom_rect(xmin=2021.5, xmax=2023.5, ymin=0.5, ymax=10.5, fill="white", alpha=0, linewidth=0.5)
saveplot(p1a,name='cn_prop',dim=c(15,8),wd=wd,type=type)  # raw data


p1a <- ggplot(sweep(cn,1,rowSums(cn),"/") %>%  as.data.frame() %>% rownames_to_column("year") %>%  
                pivot_longer(`1`:`10`, names_to="age", values_to="caa") %>% 
                mutate(year_cat= if_else(year %in% 2022:2023,"1" ,"0"),
                       shape_cat= if_else(caa <= 0, "1", "0")),
            aes(x = as.numeric(year), y = as.numeric(age))) + 
    geom_point(alpha=0.6,aes(size = caa, col = year_cat, shape=shape_cat)) + scale_size(range = c(1, 8), breaks=seq(0,0.6,0.1)) + 
    scale_color_manual(values = c("black", "#578279"), guide="none") +
    scale_shape_manual(values=c(19, 4), guide="none")+
    scale_y_continuous(breaks=1:10)+
    scale_x_continuous(breaks=seq(1970,year,5))
    
    
    
p1aBI = p1a +  labs(size = "", x="Année | Year", y="Âge | Age", col="") 
saveplot(p1aBI,name='cn_propBI',dim=c(18,8),wd=wd,type=type)  # raw data
p1aEN = p1a +  labs(size = "", x="Year", y="Age", col="") 
saveplot(p1aEN,name='cn_propEN',dim=c(15,8),wd=wd,type=type)  # raw data
p1aEN<- p1aEN + scale_x_continuous(breaks=seq(1968, 2024, 4))
saveplot(p1aEN,name='cn_propwide',dim=c(18,6),wd=wd,type=type)  # raw data

p1aFR = p1a +  labs(size = "", x="Année", y="Âge", col="") 
saveplot(p1aFR,name='cn_propFR',dim=c(15,8),wd=wd,type=type)  # raw data


p2 <- bubble(t(spay(t(cn))),scale = 8)+ggtitle('Standardised by year')+scale_y_continuous(breaks=1:10)
saveplot(p2,name='cn_spay',dim=c(15,8),wd=wd,type=type)  # to check cohorts

p3 <- bubble(t(spya(t(cn))),scale = 8)+ggtitle('Standardised by age')+scale_y_continuous(breaks=1:10)
saveplot(p3,name='cn_spya',dim=c(15,8),wd=wd,type=type)  # to compare between years

p4 <- plotobs(dat,fleets=2,type='bar')+scale_x_continuous(expand=c(0,0))+scale_y_continuous(expand=c(0,0))+ylab('Proportion')
saveplot(p4,name='cn_propbar',dim=c(15,8),wd=wd,type=type)  # proportion in population



## proportion mature
p5 <- heat(mo)
saveplot(p5,name='pm_heat',dim=c(15,8),wd=wd,type=type)  

p6<- mo %>%  as.matrix() %>%  as.data.frame()%>%  rownames_to_column("year") %>%  mutate(year=as.numeric(year)) %>% pivot_longer(2:11)  %>% mutate(age= fct_relevel(name, "10", after=10)) %>% 
    ggplot(aes(x=year, y=value, color=age)) +geom_line() +scale_x_continuous(expand = c(0,0))+scale_y_continuous(limits=c(0,1),expand=c(0,0)) +
               scale_color_viridis_d(name="", guide="none") 
p6EN = p6 +labs(y='Proportion mature',x='Year')
saveplot(p6EN,name='pm_lineEN',dim=c(15,8),wd=wd,type=type)  
p6FR = p6 +labs(y='Proportion mature',x='Année')
saveplot(p6FR,name='pm_lineFR',dim=c(15,8),wd=wd,type=type)  
p6BI = p6 +labs(y='Proportion mature',x='Année |Year')
saveplot(p6BI,name='pm_lineBI',dim=c(15,8),wd=wd,type=type)  



## stock weight
p7 <- heat(sw)
saveplot(p7,name='sw_heat',dim=c(15,8),wd=wd,type=type)  

p8 <- prettymatplot(sw,ylab='Stock weight (kg)',xlab='Year')+theme(legend.position = 'none')
saveplot(p8,name='sw_line',dim=c(15,8),wd=wd,type=type) 

## catch weight
p9 <- heat(cw)
saveplot(p9,name='cw_heat',dim=c(15,8),wd=wd,type=type)  

p10 <- prettymatplot(cw,ylab='Catch weight (kg)',xlab='Year')+theme(legend.position = 'none')  +
    scale_x_continuous(breaks=seq(1970, year,5))
saveplot(p10,name='cw_line',dim=c(15,8),wd=wd,type=type) 
p10 <- prettymatplot(cw,ylab='Poids dans les captures (kg)',xlab='Année')+theme(legend.position = 'none') +
    scale_x_continuous(breaks=seq(1970, year,5))
saveplot(p10,name='cw_lineFR',dim=c(15,8),wd=wd,type=type) 


## survey
update_geom_defaults("line", list(size = 1))
survey[[1]][,1] <- survey[[1]][,1]/10^9
p11 <- surveyplot(survey)+ylab('TEP (billions)')+scale_y_continuous(limits=c(0,max(survey[[1]][,1])),expand = c(0,0))
saveplot(p11,name='survey',dim=c(15,8),wd=wd,type=type) 

eggs <- survey$TEP %>%  as.data.frame() %>%  rownames_to_column("year") %>% rename(TEP=`-1`) %>%  mutate(year=as.numeric(year))
allyears=data.frame(year=1983:year)

eggs<- full_join(eggs, allyears)

p11<- ggplot(data=eggs, aes(x=as.numeric(year), y=TEP))+geom_point()+geom_line()+
    scale_x_continuous(breaks=seq(1980, year, 5))
p11BI= p11+labs(x="Année | Year", y="PTO (Billions) | TEP (Trillions)")
saveplot(p11BI,name='surveyBI',dim=c(15,8),wd=wd,type=type) 

p11FR= p11+labs(x="Année ", y="PTO (Billions)")
saveplot(p11FR,name='surveyFR',dim=c(15,8),wd=wd,type=type) 
p11EN= p11+labs(x="Year", y="TEP (Trillions)")
saveplot(p11EN,name='surveyEN',dim=c(15,8),wd=wd,type=type) 



d <- data.frame(Year=as.numeric(rownames(survey[[1]])),TEP=survey[[1]][,1])
d$mismatch <- d$Year %in% c(2006,2017,2019)
d$uncertainS <- d$Year %in% c(2022)
p11b <- ggplot(d,aes(x=Year,y=TEP))+
    geom_line()+
    geom_point(aes(color=mismatch))+
    scale_y_continuous(limits=c(0,max(survey[[1]][,1])*1.05),expand = c(0,0))+
    labs(y='TEP (billions)')+
    scale_color_manual(values=c('black','red'))+
    theme(legend.position = c(0.8,0.8))

saveplot(p11b,name='survey_mismatch',dim=c(15,8),wd=wd,type=type) 

d <- data.frame(Year=as.numeric(rownames(survey[[1]])),TEP=survey[[1]][,1])
d <- merge(data.frame(Year=min(d$Year):max(d$Year)),d,all.x = T)
p11c <- ggplot(d, aes(x = Year, y = TEP))+ 
    geom_line()+
    geom_point(col = "grey30")+
    ylab('TEP (billions)')+
    scale_y_continuous(limits=c(0,max(survey[[1]][,1])*1.1),expand = c(0,0))
saveplot(p11c,name='survey_discline',dim=c(15,8),wd=wd,type=type) 
    
## total catch
p12 <- prettymatplot(ct,ylab='Catch (t)', xlab='Year',col=c('black','darkgrey'))
saveplot(p12,name='ct',dim=c(15,8),wd=wd,type=type) 

p13 <- prettymatplot(ctwusa,ylab='Catch (kt)', xlab='Year',col=c('black','darkgrey'))+
    scale_x_continuous(breaks=seq(1970,year, 5)) +
    scale_y_continuous(labels = function(x) format(x/(10^3)))
saveplot(p13,name='ct_wusa',dim=c(15,8),wd=wd,type=type) 

p14 <- prettymatplot(sweep(ct,1,ct[,1],'/'),col=c('darkgrey','black'),ylab = 'Crel')
saveplot(p14,name='ct_rel',dim=c(15,8),wd=wd,type=type) 

p15 <- prettymatplot(sweep(ctwusa,1,ctwusa[,1],'/'),col=c('darkgrey','black'),ylab = 'Crel')
saveplot(p15,name='ct_rel_wusa',dim=c(15,8),wd=wd,type=type) 

allC <- cbind(Canada=ct[,1],USA=ctUSA[,1],Foreign=ctForeign[,1])
allC <- cbind(allC, Total=rowSums(allC))
p16 <- prettymatplot(allC,ylab='Catch (t)', xlab='Year',col=c('orange','yellowgreen','mediumorchid','black'))
saveplot(p16,name='ct_all',dim=c(15,8),wd=wd,type=type) 

#########US rec
ctus2<- read.csv(paste0("data/",year,"/raw/USrec_yearly_during_winter.csv"), dec=".")  %>%  dplyr::select(year, perc_rec)
ctus3<- read.csv(paste0("data/",year,"/raw/USrec_decades_during_winter.csv"), dec=".")  %>%  dplyr::select(decade, dec_perc_rec) %>%  distinct()

ctus4<- left_join(full_join(ctus2 ,
                            data.frame(year =seq(1969, year))) %>%
                      mutate(decade= year - year %% 10 ,
                             decade=if_else(decade < 1980, 1980, decade))  , 
                  ctus3) %>%  ungroup() %>% 
    dplyr::mutate(flag.dec= ifelse(is.na(perc_rec), "1", "0"),
                  perc_rec= dplyr::coalesce(perc_rec, dec_perc_rec))


p1<- ggplot(data=ctus4,aes(x=year, y=perc_rec, fill=flag.dec))+geom_bar(stat="identity", col="black") +
    scale_fill_manual(values=c("black", "grey"), guide="none") +
    scale_x_continuous(breaks=seq(1970, year, 5))
p1BI<- p1 + labs(x="Année | Year",
                 y= "% de la pêche récréative avec cont. nord présent  \n % of the recreational fishery with north. cont. ")

ggsave(paste0(wd,"/recfishUS.png" ), width=6, height=4, dpi=600)
