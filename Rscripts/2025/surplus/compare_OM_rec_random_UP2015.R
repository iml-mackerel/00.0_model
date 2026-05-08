#################################################################################################################
#*** Weight-of-Evidence approach for rec fishery
#*** Sensitivity index / assessment
#################################################################################################################
##' run multiple models
##' @param x list with elements containing fit parameters
##' @param dir directory to save forecasts
##' @param parallel logical
##' @return does not return an object. Save all individual runs as Rdata
##' @import parallel
##' @rdname multifit
##' @export
multifit <- function(x,dir,parallel=FALSE,ncores=NULL){
    if(parallel){
        library(parallel)
        if(is.null(ncores)) ncores <- detectCores()
        cl <- makeCluster(ncores) #set up nodes
        clusterEvalQ(cl, {library(CCAM)}) #load the package to each node
        clusterExport(cl, c('dir','x'), envir=environment())
        empty <- parLapply(cl, 1:length(x), function(y){ 
            run <- do.call(ccam.fit, x[[y]])
            save(run,file=paste0(dir,names(x)[y],'.Rdata'))
        })
        stopCluster(cl) 
    }else{
        empty <- lapply(1:length(x),function(y){
            run <- do.call(ccam.fit, x[[y]])
            save(run,file=paste0(dir,names(x)[y],'.Rdata'))
        }) 
    }
}


#################################################################################################################
########### BASE TEP ############################################################################################
#################################################################################################################

load(file=paste0('Rdata/',my.year,'/fit.Rdata'))
   # quick check
d <- fit$data  
id <- which(d$aux[,2]==1)
idx<-fit$data$aux[,"fleet"]%in%1
max.catches<- fit$data$logobs[idx,"aux2"] # maximum . minimum would be aux1
total_catches<- exp(max.catches)


################################################################################################################
########### ALTERNATIVE REC #####################################################################################
#################################################################################################################

load(file=paste0('Rdata/',my.year,'/fit.Rdata'))
# quick check
d <- fit$data  
id <- which(d$aux[,2]==1)
idx<-fit$data$aux[,"fleet"]%in%1
max.catches<- fit$data$logobs[idx,"aux2"] # maximum . minimum would be aux1
total_catches<- exp(max.catches)
ny <- length(total_catches)



# -------------------------------------------------------------------------
# PARAMÈTRES À AJUSTER
# -------------------------------------------------------------------------
years <- as.numeric(names(total_catches))
# --- portion constante de total catches ---
baseline <- total_catches - 680   # tout sauf la portion exponentielle

# -------------------------------------------------------------------------
# PARAMÈTRES
# -------------------------------------------------------------------------

nsims <- 100
growth_rates <- runif(nsims, 0.02, 0.2)  # taux de croissance exponentielle
noise_sd <- 0.05
anchor_years <- c(2016, 2018, 2020, 2022)       # années où commence l’augmentation

# -------------------------------------------------------------------------
# SIMULATIONS
# -------------------------------------------------------------------------

simrec <- matrix(NA, nrow = ny, ncol = nsims)

for (s in 1:nsims) {
    
    # portion exponentielle simulée
    expo <- rep(680, ny)  # initialement constante à 680
    
    anchor <- sample(anchor_years, 1)
    i_anchor <- which(years == anchor)
    r <- growth_rates[s]
    
    for (t in i_anchor:ny) {
        
        expected <- 680 * exp(r * (t - i_anchor))
        noise <- rlnorm(1, meanlog = 0, sdlog = noise_sd)
        
        expo[t] <- expected * noise
    }
    
    # total = baseline (fixe) + expo (variable)
    simrec[, s] <- baseline + expo
}

dimnames(simrec) <- list(ny = years, nsim = 1:nsims)

# données originales
tc_default <- data.frame(ny = years, total_catches = total_catches)

# -------------------------------------------------------------------------
# GRAPHIQUE
# -------------------------------------------------------------------------

c1<- ggplot() +
    geom_line(data = melt(simrec) %>%  filter(ny > 2015),
              aes(x = ny, y = value, col = factor(nsim)), alpha = 0.4, size = 0.25) +
    scale_color_viridis_d() +
    geom_line(data = tc_default %>%  filter(ny > 2015), aes(x = ny, y = total_catches),
              linewidth = 1.2, color = "black") +
    theme(legend.position = "none") +
    scale_x_continuous(breaks=seq(2015,my.year,1))+
    labs(x = "Year",
         y = "Upper limit catches",
         title = "Simulated upper level of total catches")

saveplot(c1,'Simulated_maximum_catches',paste0('img/',my.year,'/sensitivity/rec2015/'),c(12,8))


c2<- ggplot() +
    geom_line(data = left_join(melt(simrec) %>%  filter(ny > 2015), 
                               as.data.frame(baseline) %>%  rownames_to_column("ny") %>% 
                                   mutate(ny=as.numeric(ny))) %>% 
                               mutate(rec=value-baseline), 
              aes(x = ny, y = rec, col = factor(nsim)), alpha = 0.4, size = 0.25) +
    scale_color_viridis_d() +
    geom_hline(yintercept=680, col="red", lty=2)+
    theme(legend.position = "none") +
    scale_x_continuous(breaks=seq(2015,my.year,1))+
    labs(x = "Year",
         y = "CAN Rec catches",
         title = "Simulated upper level of rec catches") 

saveplot(c2,'Simulated_rec_catches',paste0('img/',my.year,'/sensitivity/rec2015/'),c(12,8))

#################################################################################################################
########### MODEL SENSITIVITY ###################################################################################
#################################################################################################################

########## runs #################################################################################################

inputs <- lapply(1:nsims,function(x){
    dat <- d
    dat$logobs[idx,"aux2"]<- log(simrec[,x])
    conf <- fit$conf
    par <- defpar(d,conf)
    par[!names(par)%in%c("logN", "logF", "logSW", "logCW", "logitMO", "logNM")]<-fit$pl[!names(fit$pl)%in%c("missing", "logN", "logF", "logSW", "logCW", "logitMO", "logNM")]
    
    list(data=dat,conf=conf,par=par,silent=FALSE,paracheck = FALSE)
})
names(inputs) <- 1:nsims

multifit(inputs,dir = paste0('Rdata/',my.year,'/sensitivity/rec2015/'),parallel=FALSE)


#############Check results#########
sens <- lapply(list.files(paste0('Rdata/',my.year,'/sensitivity/rec2015/'),full.names = T),function(x) get(load(x)))
out <- ldply(1:(length(sens)+1),function(x){
    if(x==1) m <- fit else m <- sens[[x-1]]
    d <- data.frame(
        class=ifelse(x==1,"base","sim"),
        model=x,
        AIC=AIC(m),
        ssb=ssbtable(m),
        ssbend=tail(ssbtable(m)[,1],1),
        LRP=ypr(m)$LRP,
        ssbLRP=tail(ssbtable(m)[,1],1)/ypr(m)$LRP
    )
    
    
    sens.results <- data.frame(ssb.year=m$data$aux[id,1],
                      catches=exp(m$data$logobs[id,2])
                     )
    d <- merge(d,sens.results,all.x=T)
})

p1 <- ggplot(melt(out[out$class=='sim' & out$ssb.year==my.year,c('ssbLRP','class')],id=c('class')),aes(x=class,y=value))+
    geom_boxplot(fill='grey')+
    geom_hline(yintercept=1)+
    geom_hline(aes(yintercept=out[out$class=='base'& out$ssb.year==my.year,'ssbLRP']),col='green',size=2)+
    ylab('SSB/LRP') +xlab("") +coord_flip() +theme(axis.text.y  = element_blank())

saveplot(p1,'fits_ssblrp',paste0('img/',my.year,'/sensitivity/rec2015/'),c(8,4))

p2 <- ggplot(out[out$class=='sim',],aes(x=ssb.year,y=ssb.Estimate,group=model))+
    geom_line(col='darkgrey',size=0.2)+
    geom_line(data=out[out$class=='base',],col='red',size=1)+
    labs(y='SSB (t)',x='Année | Year') +scale_x_continuous(breaks=seq(1970, my.year, 5))

saveplot(p2,'fits_ssb',paste0('img/',my.year,'/sensitivity/rec2015/'),c(12,8))

p3 <- ggplot(out[out$class=='sim' & !is.na(out$catches),],aes(x=ssb.year,y=catches,group=model))+
    geom_line(col='darkgrey',size=0.2)+
    geom_line(data=out[out$class=='base'&!is.na(out$catches),],col='red')+
    labs(y='catches',x='Année | Year') +scale_x_continuous(breaks=seq(1970, my.year, 5))

saveplot(p3,'catch_new',paste0('img/',my.year,'/sensitivity/rec2015/'),c(12,8))


g1<- ggarrange(c2+ theme(plot.title = element_blank()), 
               c1+ theme(plot.title = element_blank()), 
               p2,p1, ncol=2, nrow=2, labels=c("A", "B", "C", "D"))

saveplot(g1,'rec_panels',paste0('img/',my.year,'/sensitivity/rec2015/'),c(25,15))
