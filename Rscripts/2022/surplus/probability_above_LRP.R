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

p <- ggplot(ssb,aes(x=year,y=prob))+
    geom_line()+
    geom_hline(yintercept=75,col='darkgrey',linetype='dashed')+
    scale_y_continuous(limits=c(0,100),expand=c(0,0))+
    scale_x_continuous(expand=c(0,0))+
    labs(x='Year',y='Probability of being above the LRP (%)')

savepng(p,'img/2022/probLRP/',"ProbLRP_annual",c(14,8))

### fit segmented regression (but there is always this not so great slope for prob 100%, while it should just be a horizontal line)
# X <- ssb$Estimate
# Y <- ssb$prob
# df <- data.frame(X=seq(0,max(ssb$Estimate),10))

#library(nlme)
#modass <- nls(Y ~ SSasymp(X, Asym, R0, lrc))
#df$Yass <- predict(modass,df)*100

# library(segmented)   # ideally fix plateau: https://stackoverflow.com/questions/13810607/in-r-package-segmented-how-could-i-set-the-slope-of-one-of-lines-in-the-model
# my.lm <- lm(Y~X)
# modseg <- segmented(my.lm , 
#                     seg.Z = ~ X, 
#                     psi = list(X = 100000))
# 
# p <- predict(modseg,df,se.fit=T)
# df$Yseg <- p$fit*100
# df$Ysegsd <-p$se.fit*100
# 
# x75 <- df[which.min(abs(df$Yseg - 75)),'X']
# x75/refBase$LRP

# simple regression for years <USR


my.lm <- lm(prob~Estimate,data=ssb[ssb$Estimate<(ssb$LRP*2),])
df <- data.frame(Estimate=seq(0,max(ssb$Estimate),10))
df$pred <- predict(my.lm,df)
df$sd <- predict(my.lm,df,se.fit = T)$se.fit
df$low <- with(df,pred-2*sd)
df$high <- with(df,pred+2*sd)
df[df$pred>100,'pred'] <- 100
df[df$high>100,'high'] <- 100
df[df$low>100,'low'] <- 100
df[df$low<0,'low'] <- 0
df[df$pred<0,'pred'] <- 0
df[df$high<0,'high'] <- 0

x75 <- df[which.min(abs(df$pred - 75)),'Estimate']
x75l <- df[which.min(abs(df$low - 75)),'Estimate']
x75h <- df[which.min(abs(df$high - 75)),'Estimate']

round(c(x75h,x75,x75l)/LRP,1)


p <- ggplot()+
    geom_rect(aes(xmin=0,xmax=LRP/1000,ymin=0,ymax=Inf),fill='darkred',alpha=0.2)+
    geom_rect(aes(xmin=LRP/1000,xmax=LRP*2/1000,ymin=0,ymax=Inf),fill='orange',alpha=0.2)+
    geom_rect(aes(xmin=LRP*2/1000,xmax=Inf,ymin=0,ymax=Inf),fill='darkgreen',alpha=0.2)+
    geom_point(data=ssb,aes(x=Estimate/1000,y=prob),size=1)+
    geom_errorbarh(data=ssb,aes(xmin=Low/1000,xmax=High/1000,y=prob),size=0.2)+
    geom_line(data=df,aes(x=Estimate/1000,y=pred),size=0.5)+
    geom_ribbon(data=df,aes(ymin=low,ymax=high,x=Estimate/1000),alpha=0.2)+
    geom_rect(aes(xmin=x75l/1000,xmax=x75h/1000,ymin=-Inf,ymax=Inf),fill='orange',alpha=0.2)+
    geom_vline(xintercept=x75/1000,col='orange',linetype='dashed',linewidth=0.5)+
    geom_hline(yintercept=75,col='darkgrey',linetype='dashed')+
    labs(y='Probability of being above the LRP (%)',x='SSB (t)')+
    scale_y_continuous(limits=c(0,100),expand=c(0,0))+
    scale_x_continuous(expand=c(0,0))

savepng(p,'img/2022/probLRP/',"ProbLRP",c(14,8))

