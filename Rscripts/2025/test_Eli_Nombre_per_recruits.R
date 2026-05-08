 ypr2.ccam <- function(fit, Flimit=2, Fdelta=0.01, aveYears=min(15,length(fit$data$years)), ageLimit=100,rec.years=fit$data$years,deterministic=TRUE,simpara=NULL,...){
        
        last.year.used=max(fit$data$years)
        
        idxno<-which(fit$data$years==last.year.used)
        
        extend<-function(x,len=100){
            
            ret<-numeric(len)
            
            ret[1:length(x)]<-x
            
            ret[-c(1:length(x))]<-x[length(x)]
            
            ret
            
        }
        
        if(deterministic){
            
            ave.sl <- fit$pl$logitSel
            
        }else{
            
            if(is.null(simpara)) {
                
                simpara <- rmvnorm(1, mu=fit$sdrep$par.fixed, Sigma=fit$sdrep$cov.fixed)
                
                names(simpara) <- names(fit$sdrep$par.fixed)
                
            }
            
            ave.sl <- simpara[which(names(simpara)=='logitSel')]
            
        }
        
        ave.sl<-c(invlogit(ave.sl),1)[fit$conf$keySel[fit$data$noYears,]-min(fit$conf$keySel[fit$data$noYears,])+1]
        
        ave.sw<-colMeans(fit$data$stockMeanWeight[(idxno-aveYears+1):idxno,,drop=FALSE])
        
        ave.cw<-colMeans(fit$data$catchMeanWeight[(idxno-aveYears+1):(idxno-1),,drop=FALSE])
        
        ave.pm<-colMeans(fit$data$propMat[(idxno-aveYears+1):idxno,,drop=FALSE])
        
        ave.nm<-colMeans(fit$data$natMor[(idxno-aveYears+1):idxno,,drop=FALSE])
        
        ave.lf<-colMeans(fit$data$landFrac[(idxno-aveYears+1):(idxno-1),,drop=FALSE])
        
        ave.cw.land<-colMeans(fit$data$landMeanWeight[(idxno-aveYears+1):(idxno-1),,drop=FALSE])
        
        N<-numeric(ageLimit)
        
        N[1]<-1.0
        
        M<-extend(ave.nm)
        
        sw<-extend(ave.sw)
        
        cw<-extend(ave.cw.land)
        
        pm<-extend(ave.pm)
        
        lf<-extend(ave.lf)
        
        deltafirst <- 0.00001
        
        delta <- Fdelta
        
        scales<-c(0, deltafirst, seq(0.01, Flimit, by=delta))
        
        yields<-numeric(length(scales))
        
        ssbs<-numeric(length(scales))
        
        n<-matrix(NA,nrow=length(scales),ncol=length(N),dimnames = list(f=scales,age=1:length(N)))
        
        for(i in 1:length(scales)){
            
            scale<-scales[i]
            
            F<-extend(ave.sl*scale)
            
            Z<-M+F
            
            for(a in 2:length(N)){
                
                N[a]<-N[a-1]*exp(-Z[a-1])
                
            }
            
            C<-F/Z*(1-exp(-Z))*N*lf
            
            Y<-sum(C*cw)
            
            yields[i]<-Y  #ypr
            
            ssbs[i]<-sum(N*pm*sw)  #spr
            
            n[i,] <- N
            
        }
        
        ret <- n
        
        return(ret)
        
    }
    
    # depends to what age they live
    
    blub <- ypr2.ccam(fit)
    
    blubdf <- melt(blub)
    
    ggplot(blubdf[blubdf$age %in% 2:10,],aes(x=f,y=value,col=as.factor(age)))+
        
        geom_line()+
        
        scale_y_continuous(expand=c(0,0),limits=c(0,1))+
        
        labs(y="NPR",x='F',col="Age")
    
    # average age in pop depends on F... get "equilibrium average age under each F"
    
    df <- data.frame(f=as.numeric(rownames(blub)),
                     
                     meanage=apply(blub[,-1],1,function(x) weighted.mean(as.numeric(colnames(blub))[-1],x)))
    
    df$age <- round(df$meanage,0)
    
    df <- merge(df,blubdf)
    
    ggplot(df,aes(x=f,y=value,col=as.factor(age)))+
        
        geom_line()+
        
        scale_y_continuous(expand=c(0,0),limits=c(0,1))+
        
        labs(y="NPR",x='F',col="Age")
    
       