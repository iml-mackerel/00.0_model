#################################################################################################################
#*** Mackerel assessment
#*** rebuilding target probability
#################################################################################################################

### reference points
refBase <- ypr(x)
RR <- refBase$f40

### data frame with f40 and all relevant quantities
f40 <- fbartable(x)
f40[,paste0('log',names(f40)[1:3])] <- log(f40[,1:3])
f40$logsd <- apply(f40,1,function(x) (x['logHigh']-x['logEstimate'])/2) # same as in the sdrep
f40$RR <- refBase$f40 # change in initial years starts not in 1968
f40$logRR <- log(f40$RR)
f40$ratio <- round(f40$Estimate/f40$RR,2)
f40$prob <- apply(f40,1,function(x) 1-pnorm(x['logRR'],x['logEstimate'],x['logsd'])) # probability being above
f40$prob <- f40$prob*100

write.csv(f40, paste0("csv/", year, "/f40f40.csv"), row.names=F)

### plot probability
p1<- ggarrange(
    fbarplot(x)+geom_hline(yintercept=refBase$f40,col='red'),
    ggplot(f40,aes(x=year,y=prob))+
        geom_line()+
        geom_hline(yintercept=50,col='darkgrey',linetype='dashed')+
        scale_y_continuous(limits=c(0,100),expand=c(0,0))+
        scale_x_continuous(expand=c(0,0))+
        labs(x='Year',y='Probability of being above the RR (%)'), ncol=1
)
ggsave(paste0('img/',year,'/probf40/',"FbarProbf40_annual.png"),dpi=600, units="cm", width=10, height=14)

p1 <- ggplot(f40,aes(x=year,y=prob))+
    geom_line()+
    geom_hline(yintercept=75,col='darkgrey',linetype='dashed')+
    scale_y_continuous(limits=c(0,100),expand=c(0,0))+
    scale_x_continuous(expand=c(0,0))+
    labs(x='Year',y='Probability of being above the RR (%)')

savepng(p1,paste0('img/',year,'/probf40/'),"Probf40_annual",c(14,8))


