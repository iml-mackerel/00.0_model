######################################################################
# Figure illustrating the censored likelihood
# the sd value is added as data in the TMB model
######################################################################

.wd <- 'img/resdoc'

### simulate data
cens <- function(x,sd,L,U){
  ZU <- (log(U) - log(x))/sd
  ZL <- (log(L) - log(x))/sd
  nll <- log(pnorm(ZU) - pnorm(ZL))
  return(nll) 
}

low <- 200
up <- 400
av<- mean(c(low,up))

censdf <- data.frame(Cpred=rep(seq(low-100,up+100,1),4),sd=rep(c(0.01,0.05,0.1,0.2),each=1604),
                        Lower=low,Upper=up)
censdf <- mutate(censdf, Likelihood = exp(cens(Cpred, sd,Lower,Upper)))
censdf$Likelihoodnormal <-dnorm(censdf$Cpred,av,sd=50)

### plots 
p1 <- ggplot(censdf,aes(x=Cpred,y=Likelihood,col=as.character(sd)))+geom_line(aes(size=as.character(sd)))+
  theme_classic()+
  geom_vline(xintercept=200)+
  geom_vline(xintercept=400)+
  scale_size_manual(values=c(1,1,1,1))+
  labs(color='sd',size='sd',x='Catch')+
  scale_color_manual(values=c("red","green","blue",'orange'))

p2 <- ggplot(censdf[censdf$sd==0.01,],aes(x=Cpred,y=Likelihood))+geom_line(col='red')+
    theme_classic()+
    geom_vline(xintercept=200)+
    geom_vline(xintercept=400)+
    labs(x='Catch')

p3 <- ggplot(censdf[censdf$sd==0.01,],aes(x=Cpred,y=Likelihoodnormal))+geom_line(col='red')+
    theme_classic()+
    geom_vline(xintercept=300)+
    labs(x='Catch',y='Likelihood')

p4 <- ggplot(censdf[censdf$sd==0.01,],aes(x=Cpred,y=Likelihoodnormal))+geom_line(col='white')+
    theme_classic()+
    geom_vline(xintercept=300,col='red',size=1.5)+
    labs(x='Catch',y='Likelihood')


### plots
saveplot(p1,name='likelihood_cens',dim=c(10,6),wd=.wd) 
saveplot(p2,name='likelihood_cens_1sd',dim=c(10,6),wd=.wd) 
saveplot(p3,name='likelihood_normal',dim=c(10,6),wd=.wd) 
saveplot(p4,name='likelihood_vpa',dim=c(10,6),wd=.wd) 


set.seed(1)
dfc <- data.frame(x=1:50,y=cumsum(c(100,rnorm(49,0,50))))
dfc$y2 <- dfc$y*2


add <- function(p) (p+scale_y_continuous(expand=c(0,0),limits=c(0,800))+
                    scale_x_continuous(expand=c(0,0),limits=c(1,50))+
                        labs(x='Year',y='Catch'))
c1 <- add(ggplot(dfc,aes(x=x,y=y))+geom_line())
c2 <- add(ggplot(dfc,aes(x=x,y=y))+ geom_ribbon(aes(ymin=y*0.7,ymax=y*1.3),alpha=0.5)+
    geom_line())
c3 <- add(ggplot(dfc,aes(x=x))+ geom_ribbon(aes(ymin=y,ymax=y2),alpha=0.5,col='black',size=1.5))
   
saveplot(c1,name='likelihood_catch_vpa',dim=c(10,6),wd=.wd) 
saveplot(c2,name='likelihood_catch_normal',dim=c(10,6),wd=.wd) 
saveplot(c3,name='likelihood_catch_cens',dim=c(10,6),wd=.wd) 
    

