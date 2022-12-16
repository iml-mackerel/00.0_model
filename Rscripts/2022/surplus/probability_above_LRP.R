#################################################################################################################
#*** Mackerel assessment
#*** rebuilding target probability
#################################################################################################################

### reference points
refBase <- ypr(x,rec.years=1969:2016)
LRP <- refBase$LRP

### data frame with ssb0 and all relevant quantities
ssb0 <- ssb0table(x)
ssb0[,paste0('log',names(ssb0)[1:3])] <- log(ssb0[,1:3])
ssb0$logsd <- apply(ssb0,1,function(x) (x['logHigh']-x['logEstimate'])/2) # same as in the sdrep
ssb0$LRP <- refBase$LRP # change in initial years starts not in 1968
ssb0$logLRP <- log(ssb0$LRP)
ssb0$ratio <- round(ssb0$Estimate/ssb0$LRP,2)
ssb0$prob <- apply(ssb0,1,function(x) 1-pnorm(x['logLRP'],x['logEstimate'],x['logsd'])) # probability being above

### plot probability
grid.arrange(
    ssb0plot(x)+geom_hline(yintercept=refBase$LRP,col='red'),
    ggplot(ssb0,aes(x=year,y=prob*100))+
        geom_line()+
        geom_hline(yintercept=75,col='darkgrey',linetype='dashed')+
        scale_y_continuous(limits=c(0,100),expand=c(0,0))+
        scale_x_continuous(expand=c(0,0))+
        labs(x='Year',y='Probability of being above the LRP (%)')
)


ggplot(ssb0,aes(x=Estimate,y=prob*100))+
    geom_point()+
    #geom_text(aes(label=year),vjust=1.1,hjust=0)+
    geom_vline(aes(xintercept=LRP))+
    geom_vline(aes(xintercept=LRP*2))+
    labs(y='Probability of being above the LRP (%)',x='SSB0 (t)')


### fit segmented regression 
X <- ssb0$Estimate
Y <- ssb0$prob
df <- data.frame(X=seq(0,max(ssb0$Estimate),10))

#library(nlme)
#modass <- nls(Y ~ SSasymp(X, Asym, R0, lrc))
#df$Yass <- predict(modass,df)*100

library(segmented)   # ideally fix plateau: https://stackoverflow.com/questions/13810607/in-r-package-segmented-how-could-i-set-the-slope-of-one-of-lines-in-the-model
my.lm <- lm(Y~X)
modseg <- segmented(my.lm , 
                    seg.Z = ~ X, 
                    psi = list(X = 100000))

p <- predict(modseg,df,se.fit=T)
df$Yseg <- p$fit*100
df$Ysegsd <-p$se.fit*100

x75 <- df[which.min(abs(df$Yseg - 75)),'X']
x75/refBase$LRP

ggplot(ssb0,aes(x=Estimate,y=prob*100))+
    geom_rect(xmin=0,xmax=LRP,ymin=0,ymax=Inf,fill='darkred',alpha=0.5)+
    geom_rect(xmin=LRP,xmax=LRP*2,ymin=0,ymax=Inf,fill='orange',alpha=0.5)+
    geom_rect(xmin=LRP*2,xmax=Inf,ymin=0,ymax=Inf,fill='darkgreen',alpha=0.5)+
    geom_ribbon(data=df,aes(ymin=Yseg-2*Ysegsd,ymax=Yseg+2*Ysegsd,y=Yseg,x=X),alpha=0.5)+
    geom_line(data=df,aes(x=X,y=Yseg),size=1)+
    #geom_line(data=df,aes(x=X,y=Yass),col='red',size=1)+
    #geom_text(aes(label=year),vjust=1.1,hjust=0)+
    geom_vline(aes(xintercept=LRP))+
    geom_vline(aes(xintercept=LRP*2))+
    geom_vline(xintercept=x75,col='darkgrey',linetype='dashed')+
    geom_hline(yintercept=75,col='darkgrey',linetype='dashed')+
    geom_point()+
    labs(y='Probability of being above the LRP (%)',x='SSB0 (t)')+
    scale_y_continuous(limits=c(0,101),expand=c(0,0))+
    scale_x_continuous(expand=c(0,0))
