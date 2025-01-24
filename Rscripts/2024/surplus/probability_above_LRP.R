#################################################################################################################
#*** Mackerel assessment
#*** rebuilding target probability
#################################################################################################################

### reference points
refBase <- ypr(x)
LRP <- refBase$LRP

### data frame with ssb and all relevant quantities
ssb <- ssbtable(x)
ssb[,paste0('log',names(ssb)[1:3])] <- log(ssb[,1:3])
ssb$logsd <- apply(ssb,1,function(x) (x['logHigh']-x['logEstimate'])/2) # same as in the sdrep
ssb$LRP <- refBase$LRP # change in initial years starts not in 1968
ssb$logLRP <- log(ssb$LRP)
ssb$ratio <- round(ssb$Estimate/ssb$LRP,2)
ssb$prob <- apply(ssb,1,function(x) 1-pnorm(x['logLRP'],x['logEstimate'],x['logsd'])) # probability being above
ssb$prob <- ssb$prob*100

write.csv(ssb, paste0("csv/", year, "/ssblrp.csv"), row.names=F)

### plot probability
grid.arrange(
    ssbplot(x)+geom_hline(yintercept=refBase$LRP,col='red'),
    ggplot(ssb,aes(x=year,y=prob))+
        geom_line()+
        geom_hline(yintercept=75,col='darkgrey',linetype='dashed')+
        scale_y_continuous(limits=c(0,100),expand=c(0,0))+
        scale_x_continuous(expand=c(0,0))+
        labs(x='Year',y='Probability of being above the LRP (%)')
)

p1 <- ggplot(ssb,aes(x=year,y=prob))+
    geom_line()+
    geom_hline(yintercept=75,col='darkgrey',linetype='dashed')+
    scale_y_continuous(limits=c(0,100),expand=c(0,0))+
    scale_x_continuous(expand=c(0,0))+
    labs(x='Year',y='Probability of being above the LRP (%)')

savepng(p1,paste0('img/',year,'/probLRP/'),"ProbLRP_annual",c(14,8))



#########new method#######
refBase <- ypr(x)
LRP <- refBase$LRP

logssb<- seq(floor(min(ssb$logEstimate)), ceiling(max(ssb$logEstimate)), 0.001) 
ssb$logsd <- apply(ssb,1,function(x) (x['logHigh']-x['logEstimate'])/1.96) # same as in the sdrep

CV =mean(ssb$logsd /ssb$logEstimate)
sdCV =sd(ssb$logsd /ssb$logEstimate)
LCV = CV - (1.96 * sdCV)
HCV = CV + (1.96 * sdCV)

sdlogssb = logssb*CV
Lsdlogssb = logssb*LCV
Hsdlogssb = logssb*HCV
logLRP<- log(LRP)
# CV max  et CV min sd <a remplacer du CV moyen - sdCV sd=  dans pnorm, meoyenne reste pareil

probLRP<- data.frame(logssb, CV, sdlogssb, logLRP, Lsdlogssb, Hsdlogssb) 

#CVs<- rnorm(mean=CV, sd=sdCV, n=nrun) this doesn't work. not a normal distribution
 


#https://www.youtube.com/watch?v=M2v1JCjdrgQs see this very good video for pnorm
probLRP <-   probLRP %>% dplyr::mutate(probLRP =pnorm(logLRP, mean=logssb, sd=sdlogssb, lower.tail=F),
                                        LprobLRP= pnorm(logLRP, mean=logssb, sd=Lsdlogssb, lower.tail=F),
                                        UprobLRP= pnorm(logLRP, mean=logssb, sd=Hsdlogssb, lower.tail=F)) %>% 
                                          mutate(ssb =exp(logssb),
                                                 Lssb = exp(logssb-(1.96 *Lsdlogssb)), ##or sdlogssb
                                                 Hssb = exp(logssb+(1.96 *Hsdlogssb)),
                                                 ratio=ssb/LRP)

probLRP %>% ggplot(aes(x=exp(logssb), y=probLRP)) +geom_point()

probLRP75<- probLRP %>% # lower.tail or 1-pnorm
    dplyr::slice(which(abs(probLRP - 0.75) == min(abs(probLRP - 0.75)) |
                      abs(LprobLRP - 0.75) == min(abs(LprobLRP - 0.75))|
                                abs(UprobLRP - 0.75) == min(abs(UprobLRP - 0.75)))) 
    
problrp<- data.frame(ratio=probLRP75$ratio)

write.csv(problrp, paste0("csv/", year, "/problrp.csv"), row.names=F)

#highlight last 2 years
ssb<- ssb %>% as.matrix() %>% as.data.frame() %>%  mutate(catyear= if_else(year %in% c((my.year-1) : my.year), "1", "0"))

p2 <- ggplot()+
    geom_rect(aes(xmin=0,xmax=LRP/1000,ymin=0,ymax=Inf),fill='darkred',alpha=0.2)+
    geom_rect(aes(xmin=LRP/1000,xmax=LRP*2/1000,ymin=0,ymax=Inf),fill='orange',alpha=0.2)+
    geom_rect(aes(xmin=LRP*2/1000,xmax=Inf,ymin=0,ymax=Inf),fill='darkgreen',alpha=0.2)+
    geom_errorbarh(data=ssb,aes(xmin=Low/1000,xmax=High/1000,y=prob),size=0.2)+
    geom_point(data=probLRP,aes(x=ssb/1000,y=probLRP*100),size=0.5)+
    geom_ribbon(data=probLRP,aes(ymin=LprobLRP*100,ymax=UprobLRP*100,x=ssb/1000),alpha=0.2)+
    geom_rect(data=probLRP75,aes(xmin=min(ssb)/1000,xmax=max(ssb)/1000,ymin=-Inf,ymax=Inf),fill='orange',alpha=0.2)+
    geom_vline(xintercept=probLRP75[2,"ssb"]/1000,col='darkorange4',linetype='dashed',linewidth=0.5)+
    geom_hline(yintercept=75,col='darkgrey',linetype='dashed')+
    labs(y='Probability of being above the LRP (%)',x="SSB (1000' t)")+
    scale_y_continuous(limits=c(0,100),expand=c(0,0))+
    scale_x_continuous(expand=c(0,0))+
    geom_point(data=ssb,aes(x=Estimate/1000,y=prob, fill=catyear),size=1.5, shape=21, color="white", stroke=0.4)+ scale_fill_manual(values=c("grey35", "red"), guide="none")

savepng(p2,paste0('img/',year,'/probLRP/'),"ProbLRP",c(14,8))


gBI<- ggarrange(p1 + labs(x='Année | Year',y="Probabilité d'être supérieur à PRL \n Probability of being above the LRP (%)"), 
                p2 + labs(y="Probabilité d'être supérieur à PRL \n Probability of being above the LRP (%)",x="SSB (1000' t)"), ncol=1)
savepng(gBI,paste0('img/',year,'/probLRP/'),"ProbLRP_BI",c(14*0.9,16))

gEN<- ggarrange(p1 + labs(x='Year',y="Probability of being above the LRP (%)"), 
                p2 + labs(y="Probability of being above the LRP (%)",x="SSB (1000' t)"), ncol=1)
savepng(gEN,paste0('img/',year,'/probLRP/'),"ProbLRP_EN",c(14*0.9,16))


gFR<- ggarrange(p1 + labs(x='Année',y="Probabilité d'être supérieur à PRL (%)"), 
                p2 + labs(y="Probabilité d'être supérieur à PRL (%)",x="SSB (1000' t)"), ncol=1)
savepng(gFR,paste0('img/',year,'/probLRP/'),"ProbLRP_FR",c(14*0.9,16))

write.csv(probLRP, paste0("csv/", year, "/problrp_method2.csv"), row.names=F)

