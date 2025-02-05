#################################################################################################################
#*** Mackerel assessment
#*** Canadian mackerel (DFO, 2023)
#*** include vs exclude certain egg survey years
#################################################################################################################


source(paste0('Rscripts/',my.year,'/surplus/read_data.R'))
.wd <- paste0("img/",my.year,"/sensitivity/eggs/")
type  <- 'png'
 ###############change here 

survey <- read.ices(paste0('data/',my.year,'/tep.dat'))
survey22 <- read.ices(paste0('data/',my.year,'/sensitivity/tep_no22.dat'))
survey23 <- read.ices(paste0('data/',my.year,'/sensitivity/tep_no23.dat'))
surveyflag <- read.ices(paste0('data/',my.year,'/sensitivity/tepnoflag.dat'))


survey[[1]] <- survey[[1]][!is.na(survey[[1]]),1,drop=FALSE]
survey[[1]][,1] <- survey[[1]][,1]*10^9
attr(survey [[1]],'time') <- c(0.47)


survey22[[1]] <- survey22[[1]][!is.na(survey22[[1]]),1,drop=FALSE]
survey22[[1]][,1] <- survey22[[1]][,1]*10^9
attr(survey22 [[1]],'time') <- c(0.47)

survey23[[1]] <- survey23[[1]][!is.na(survey23[[1]]),1,drop=FALSE]
survey23[[1]][,1] <- survey23[[1]][,1]*10^9
attr(survey23 [[1]],'time') <- c(0.47)

surveyflag[[1]] <- surveyflag[[1]][!is.na(surveyflag[[1]]),1,drop=FALSE]
surveyflag[[1]][,1] <- surveyflag[[1]][,1]*10^9
attr(surveyflag [[1]],'time') <- c(0.47)

dats <- lapply(list(survey,survey22,survey23,surveyflag ),function(x){
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

nam <- c('default','no22','no23', 'noflag')

for(i in 1:length(nam)){  # if 2006, 2017 and 2019 are excluded, no convergence.
    fit <- ccam.fit(dats[[i]],conf,par,debug=T)
    save(fit, file=paste0('Rdata/',year,'/sensitivity/fit.egg.',nam[i],'.Rdata'))
}

s <- list.files(paste0('Rdata/',year,'/sensitivity/'),'egg',full.names = TRUE)
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

all_years =expand_grid(year=seq(1979,my.year, 1), model=nam)
out<- left_join(all_years, out)


p <- ggplot(out,aes(x=year,y=tep))+
    geom_point(data=out %>%  dplyr::filter(is.na(tep)),aes(x=year,y=1), shape=4, col="red")+
    geom_line()+
    geom_point()+
    facet_wrap(~model,ncol=1)+
    theme(legend.position = 'none')+
    labs(y='PTO | TEP',x='Année | Year')
saveplot(p,name='tep_series',dim=c(12,12),wd=.wd,type=type)

# compare basics
ssbplot0(fits.eggs, minyear=2010, year=year, legend=T)
ggsave(paste0(.wd,"ssb_noci2010.png"), width=6, height=5, units="in", bg="white")
ssbplot0(fits.eggs, minyear=1969, year=year, legend=T)
ggsave(paste0(.wd,"ssb_noci1969.png"), width=6, height=5, units="in", bg="white")



residuals <- function(fit,std=TRUE,fleets=3){
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

    
ires<- residuals(x, fleets=3)

saveplot(
grid.arrange(
  arrangeGrob(
    resplot(fits.eggs[[1]], fleets = 3, type = 1) + ggtitle(nam[[1]]) + labs(y = "Résidus", x = "Année"),
    resplot(fits.eggs[[1]], fleets = 3, type = 2) + labs(y = "Résidus", x = "Prédictions"),
    resplot(fits.eggs[[1]], fleets = 3, type = 3) + labs(y = "Prédictions", x = "Observations"),
    ncol = 1
  ),
  arrangeGrob(
    resplot(fits.eggs[[2]], fleets = 3, type = 1) + ggtitle(nam[[2]]) + labs(y = "Résidus", x = "Année"),
    resplot(fits.eggs[[2]], fleets = 3, type = 2) + labs(y = "Résidus", x = "Prédictions"),
    resplot(fits.eggs[[2]], fleets = 3, type = 3) + labs(y = "Prédictions", x = "Observations"),
    ncol = 1
  ),
  arrangeGrob(
    resplot(fits.eggs[[3]], fleets = 3, type = 1) + ggtitle(nam[[3]]) + labs(y = "Résidus", x = "Année"),
    resplot(fits.eggs[[3]], fleets = 3, type = 2) + labs(y = "Résidus", x = "Prédictions"),
    resplot(fits.eggs[[3]], fleets = 3, type = 3) + labs(y = "Prédictions", x = "Observations"),
    ncol = 1
  ),
  arrangeGrob(
    resplot(fits.eggs[[4]], fleets = 3, type = 1) + ggtitle(nam[[4]]) + labs(y = "Résidus", x = "Année"),
    resplot(fits.eggs[[4]], fleets = 3, type = 2) + labs(y = "Résidus", x = "Prédictions"),
    resplot(fits.eggs[[4]], fleets = 3, type = 3) + labs(y = "Prédictions", x = "Observations"),
    ncol = 1
  ),
  ncol = length(nam)
)
,
name="res_TEP",dim=c(18,16),wd=.wd,type=type)

savepng(fitplot(fits.eggs,type='AIC'),.wd,"AIC",c(14,6))

aic<- modeltable(fits.eggs) %>% as.data.frame() %>% dplyr ::select(AIC) %>% rownames_to_column("fit") %>% 
    mutate(AIC=round(AIC)) 
