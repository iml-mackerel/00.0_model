#################################################################################################################
#*** Mackerel assessment
#*** Canadian mackerel (DFO, 2023)
#*** what if 2022-2024 CAA removed, what if alternative CAA
#################################################################################################################

source(paste0('Rscripts/',year, '/surplus/read_data.R'))
.wd <- paste0("img/",year,"/sensitivity/USrec/")
type  <- 'png'

# run with different options of cn in final year (or first year data removed) #################################################################
ctUSA <- read.ices(paste0(dir,'/ctUSA.dat'))  %>%  as.data.frame()
ctUSA_norec <- read.ices(paste0(dir,'/sensitivity/ctUSA_norec.dat')) %>%  as.data.frame()
ctUSA_allrec <- read.ices(paste0(dir,'/sensitivity/ctUSA_allrec.dat')) %>%  as.data.frame()

ctwusa <- ct                                   # reported landings
ctwusa[,2] <- ctwusa[,1] + ctunac[,1]          # add max missing in Canada
ctwusa[,1] <- ctwusa[,1]*1.10 + ctUSA[,1]*0.2     # lower bound: increase and add US
ctwusa[,2] <- ctwusa[,2] + ctUSA[,1]*0.8          # add us upper


ctwusa.norec <- ct                                   # reported landings
ctwusa.norec[,2] <- ctwusa.norec[,1] + ctunac[,1]          # add max missing in Canada
ctwusa.norec[,1] <- ctwusa.norec[,1]*1.10 + ctUSA_norec[,1]*0.2     # lower bound: increase and add US
ctwusa.norec[,2] <- ctwusa.norec[,2] + ctUSA_norec[,1]*0.8          # add us upper


ctwusa.allrec <- ct                                   # reported landings
ctwusa.allrec[,2] <- ctwusa.allrec[,1] + ctunac[,1]          # add max missing in Canada
ctwusa.allrec[,1] <- ctwusa.allrec[,1]*1.10 + ctUSA_allrec[,1]*0.2     # lower bound: increase and add US
ctwusa.allrec[,2] <- ctwusa.allrec[,2] + ctUSA_allrec[,1]*0.8          # add us upper


dats <- lapply(list(ctwusa,ctwusa.norec, ctwusa.allrec),function(x){
    setup.ccam.data(surveys=survey,
                    residual.fleet=cn[-1,],
                    total.catch=x[-1,],
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

fits.usa <- lapply(dats,function(x)ccam.fit(x,conf,par,debug=T))
names(fits.usa) <- c('ctUSA','ctUSA_norec','ctUSA_allrec')
class(fits.usa) <- "ccamset"

save(fits.usa, file=paste0('Rdata/',year,'/sensitivity/fits.usa.Rdata'))

# compare all three

load(paste0('Rdata/',year,'/sensitivity/fits.usa.Rdata'))
ssbplot0(fits.usa, minyear=2010, year=year, legend=T)
ggsave(paste0(.wd,"ssb_noci2010.png"), width=6, height=5, units="in", bg="white")


saveplot(fitplot(fits.usa,type='AIC', n=F),name='AIC',dim=c(12,10),wd=.wd, type=type)


catch <- catchtable(fits.usa[[1]])
catch1 <- catchtable(fits.usa[[2]])
catch2 <- catchtable(fits.usa[[3]])

d <- fits.usa[[1]]$data
ix <- d$idx1[1,]+1
catch[,c('lowin','highin')]<-exp(d$logobs[ix,])
#catchm <- melt(catch[,-c(2,3)],id=c('year'))
catch$run="ctUSA"

d1 <- fits.usa[[2]]$data
ix1 <- d1$idx1[1,]+1
catch1[,c('lowin','highin')]<-exp(d1$logobs[ix1,])
#catchm1 <- melt(catch1[,-c(2,3)],id=c('year'))
catch1$run="ctUSA_norec"

d2 <- fits.usa[[3]]$data
ix2 <- d2$idx1[1,]+1
catch2[,c('lowin','highin')]<-exp(d2$logobs[ix2,])
#catchm1 <- melt(catch1[,-c(2,3)],id=c('year'))
catch2$run="ctUSA_allrec"

#catchm3<- bind_rows(catchm, catchm1) %>%  filter(year >2010)

catch3<- bind_rows(catch%>%  as.matrix() %>% data.frame() ,
                   catch1%>%   as.matrix() %>% data.frame() ,
                   catch2%>%   as.matrix() %>% data.frame() )  %>% 
    mutate_at(1:6, as.numeric)



pc<-ggplot(catch3 %>%  dplyr::filter(year >2010),aes(x=year, y=Estimate,col=run, linetype=run))+
    geom_line(lwd=2)+
    geom_ribbon(aes(ymin=lowin, ymax=highin), fill="transparent")+
    geom_ribbon(aes(ymin=lowin,ymax=Estimate, fill=run),alpha=0.3)+
        #scale_color_manual(values=c('black','darkgrey','darkgrey'))+
    labs(col='',linetype='',y='Catch (t)',x='Year')+
    scale_size_manual(values=c(1,0.3,0.3)) +scale_color_viridis_d() +scale_fill_viridis_d()

saveplot(pc,name='catch_missing',dim=c(15,12),.wd)  # raw data


pc<-ggplot(catch3,aes(x=year, y=Estimate,col=run, linetype=run))+
    geom_line(lwd=2)+
    geom_ribbon(aes(ymin=lowin, ymax=highin), fill="transparent")+
    geom_ribbon(aes(ymin=lowin,ymax=Estimate, fill=run),alpha=0.3)+
    #scale_color_manual(values=c('black','darkgrey','darkgrey'))+
    labs(col='',linetype='',y='Catch (t)',x='Year')+
    scale_size_manual(values=c(1,0.3,0.3)) +scale_color_viridis_d() +scale_fill_viridis_d()

saveplot(pc,name='catch_missing_1969',dim=c(15,12),.wd)  # raw data

