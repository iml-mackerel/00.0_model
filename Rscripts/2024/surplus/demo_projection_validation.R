# quick demo script to compare forecasts one assessment with new TEP observations

# 1) basic projection (example) -------------------------------
load('Rdata/2022/fit.Rdata')
ny=3

# 2) with MP hack ----------------------------------------------
# make MP that does the same as a catchval
MPcaro <- function(){
    catchval=c(0,470,500)  # ideally the true future TACs
    rep(catchval,10)
}
class(MPcaro) <- append(class(MPcaro),"MP")
attr(MPcaro,'model') <- FALSE

# either export this to CCAM package, or simply block out line 102 in the forecast function
#if(all(!is.na(MP)) & any(!MP %in% MPs) & !is.numeric(MP)) stop("Undefined MP (see avail('MP'))")

# use this to forecast (same results as catchval=c(0,0,0))
projb <- forecast(MP=rep("MPcaro",ny),
                  fit=fit,
                  nosim=10)

# extract predictions
fut <- data.frame(Estimate=do.call(rbind, lapply(projb, function(xx)median(xx$index))),
                    Low=unlist(lapply(projb, function(xx)quantile(xx$index,0.025))),
                    High=unlist(lapply(projb, function(xx)quantile(xx$index,0.975))),
                    year=as.numeric(rownames(attr(projb,"tab"))),
                    period="Future")
f <- attr(projb,"fit")
pass <- data.frame(cbind(f$data$aux,exp(f$data$logobs)))
pass <- pass[pass$fleet==3,c(1,4)]
names(pass)[2]="Estimate"
pass$period="Passed"

# get new true observations
#à changer pour tep.dat
NEW<- read.ices("data/2024/tep.dat")$TEP %>% as.matrix() %>%  as.data.frame() %>%  rename(TEP=`-1`)  %>%  
    dplyr::mutate(Estimate=TEP* 10^9,
                  period="NEW",
                  year=1979:2024) %>%  
    dplyr::select(year, Estimate, period) %>% 
    dplyr::filter(!year %in% c(1991, 1999))

#NEW <- data.frame(year=c(2023, 2024),Estimate=rep(35860523284,2),period="NEW") # new observations

# compare predictions with reality
this <- rbind.fill(pass,fut[-1,],NEW)

pt<- ggplot(this,aes(x=year,y=Estimate*1000,col=period))+
    geom_point()+
    geom_line()+
    geom_errorbar(aes(ymin=Low*1000,ymax=High*1000))+
    theme(legend.position = "none")+
    labs(x="Year",y="TEP")+
    scale_color_manual(values=c("grey","red","black"))

pt + scale_x_continuous(limits=c(2000,2026), name = "Ann\u00E9e | Year") +
    scale_y_continuous(limits=c(0, 3e14),"Production total d'oeufs (milliard) \n Total egg production ")
ggsave(paste0("img/",my.year,"/projvalid/Projections2022validation.png"), width = 7, height = 4, unit = "in", dpi = 600)

