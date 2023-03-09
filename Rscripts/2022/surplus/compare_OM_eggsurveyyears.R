#################################################################################################################
#*** Mackerel assessment
#*** Canadian mackerel (DFO, 2023)
#*** include vs exclude certain egg survey years
#################################################################################################################


source('Rscripts/2022/surplus/read_data.R')
.wd <- "img/2022/fit_compare/eggs/"
type  <- 'png'

# run with different options of cn in final year (or first year data removed) #################################################################
survey_all <- read.ices(paste0(dir,'/sensitivity/tep_all.dat'))
survey_less <- read.ices(paste0(dir,'/sensitivity/tep_no_badS_mismatch.dat'))
survey_most <- read.ices(paste0(dir,'/sensitivity/tep_no22.dat'))

survey_all[[1]] <- survey_all[[1]][!is.na(survey_all[[1]]),1,drop=FALSE]
survey_all[[1]][,1] <- survey_all[[1]][,1]*10^9
attr(survey_all [[1]],'time') <- c(0.47)

survey_less[[1]] <- survey_less[[1]][!is.na(survey_less[[1]]),1,drop=FALSE]
survey_less[[1]][,1] <- survey_less[[1]][,1]*10^9
attr(survey_less[[1]],'time') <- c(0.47)

survey_most[[1]] <- survey_most[[1]][!is.na(survey_most[[1]]),1,drop=FALSE]
survey_most[[1]][,1] <- survey_most[[1]][,1]*10^9
attr(survey_most[[1]],'time') <- c(0.47)


dats <- lapply(list(survey,survey_all,survey_less,survey_most),function(x){
    setup.ccam.data(surveys=x,
                    residual.fleet=cn[-1,],
                    total.catch=ctwusa[-1,],
                    prop.mature=mo[-1,],
                    stock.mean.weight=sw[-1,],
                    stock.start.weight=sw0[-1,],
                    catch.mean.weight=cw[-1,],
                    prop.f=pf[-1,],
                    prop.m=pm[-1,],
                    natural.mortality=nm[-1,],
                    prop.fem=pfem[-1,],
                    fec=fec[-1,])
})

nam <- c('default','all','NOmismatch','defaultno22')

for(i in 1:4){  # if 2006, 2017 and 2019 are excluded, no convergence.
    fit <- ccam.fit(dats[[i]],conf,par,debug=T)
    save(fit, file=paste0('Rdata/',year,'/fit_compare/fit.egg.',nam[i],'.Rdata'))
}

s <- list.files(paste0('Rdata/',year,'/fit_compare/'),'egg',full.names = TRUE)
fits.eggs <- lapply(s,function(x)get(load(x)))
names(fits.eggs) <- gsub(".Rdata","",gsub(".*egg.","",s))
class(fits.eggs) <- "ccamset"

# make plots of TEP included
out <- ldply(1:length(fits.eggs),function(x){
    m <- fits.eggs[[x]]
    id <- which(m$data$aux[,2]==3)
    data.frame(
        model=names(fits.eggs)[x],
        year=m$data$aux[id,1],
        tep=exp(m$data$logobs[id,1]))
})
out$col <- 0
deleted <- setdiff(out[out$model=='all','year'],out[out$model=='default','year'] )
out[out$model=='all' & out$year %in% deleted,'col'] <- 1
out[out$model=='default' & out$year %in% 2022,'col'] <- 1
mism <- setdiff(out[out$model=='defaultno22','year'],out[out$model=='NOmismatch','year'] )
out[out$model=='defaultno22' & out$year %in% mism,'col'] <- 1
p <- ggplot(out,aes(x=year,y=tep))+
    geom_line()+
    geom_point(aes(col=as.factor(col)))+
    facet_wrap(~model,ncol=1)+
    scale_color_manual(values=c('black','red'))+
    theme(legend.position = 'none')+
    labs(y='TEP',x='Year')
saveplot(p,name='tep_series',dim=c(12,18),wd=.wd,type=type)

# compare basics
saveplot(plot(fits.eggs,ci=FALSE),name='ssb_noci',dim=c(12,10),wd=.wd,type=type)
saveplot(plot(fits.eggs,ci=FALSE,years=2010:2022),name='ssb_noci_short',dim=c(12,10),wd=.wd,type=type)

fits.eggs2 <- fits.eggs[c('default','defaultno22')]
class(fits.eggs2) <- "ccamset"
names(fits.eggs2) <- c('With 2022','Without 22')

saveplot(plot(fits.eggs2,ci=FALSE),name='ssb_noci_comp2',dim=c(12,10),wd=.wd,type=type)
saveplot(plot(fits.eggs2,ci=FALSE,years=2010:2022),name='ssb_noci_short_comp2',dim=c(12,10),wd=.wd,type=type)

# residuals (shit code)
r1 <- residuals(fits.eggs$all,fleets = 3)
r1$fit <- "all"
r2 <- residuals(fits.eggs$default,fleets = 3)
r2$fit <- 'default'

r1$extra <- !r1$year %in% r2$year
r2$extra <- FALSE

r <- rbind(r1,r2)
ggplot(r,aes(x=year,y=res,col=fit,shape=extra))+
    geom_point(size=3)+
    geom_hline(yintercept = 0)+
    scale_color_manual(values=c('red','black'))+
    scale_shape_manual(values=c(16,8))


r1 <- residuals(fits.eggs2$`With 2022`,fleets = 3)
r1$fit <- "With 2022"
r2 <- residuals(fits.eggs2$`Without 2022`,fleets = 3)
r2$fit <- 'Without 2022'

r1$extra <- !r1$year %in% r2$year
r2$extra <- FALSE

r <- rbind(r1,r2)
p<- ggplot(r,aes(x=year,y=res,col=fit,shape=extra))+
    geom_point(size=1.5)+
    geom_hline(yintercept = 0)+
    scale_color_manual(values=c('red','black'))+
    scale_shape_manual(values=c(16,8))+
    labs(y='Standardised residuals',x='Year',col='')+
    guides(shape='none')
saveplot(p,name='res',dim=c(12,6),wd=.wd,type=type)

    
    
residuals <- function(fit,std=TRUE,fleets=1){
    idx<-fit$data$aux[,"fleet"]%in%fleets
    p <- fit$rep$predObs[idx]
    o <- fit$data$logobs[idx,1]
    res <- o-p
    aa <- fit$data$aux[idx,"age"]
    
    neg.age <- (aa < -1.0e-6)
    aa[neg.age] <- NA
    Year <- fit$data$aux[idx,"year"]
    sds <- exp(fit$obj$par[which(names(fit$obj$par)=='logSdLogObs')])
    keysd <- fit$conf$keyVarObs[fleets,]
    keysd <- keysd[keysd>-1]
    sd <- sds[keysd+1]
    if(std) res <- res/sd[ifelse(is.na(aa),1,aa)]
    df <- data.frame(year=Year, p=p,o=o,res=res, age=aa)
    return(df)
}

    
