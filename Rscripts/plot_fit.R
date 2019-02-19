#################################################################################################################
#*** Mackerel MSE
#*** Plot model fits
#*** based on CCAM package
#################################################################################################################

#x <- get(load(file='Rdata/fit/fit.Rdata'))

type <- 'png'
retro <- TRUE
res <- TRUE
procres <- TRUE

.wd <- paste0('img/fit/',name)
dir.create(.wd, showWarnings = FALSE)

### reference points
refBase <- ypr(x,rec.years=1969:2016)
yr <- range(x$data$year)
yr[1] <- yr[1]+1

### plots
saveplot(srplot(x,curve=TRUE),name='sr',dim=c(17,10),wd=.wd,type=type) 
saveplot(recplot(x),name='rec',dim=c(17,10),wd=.wd,type=type) 
saveplot(recplot(x,years=yr[1]:yr[2]),name='rec_1969',dim=c(17,10),wd=.wd,type=type) 
saveplot(recplot(x,trans=function(x)x),name='rec_log',dim=c(17,10),wd=.wd,type=type) 
saveplot(catchplot(x,fleet = 1,ci=FALSE)+scale_y_continuous(limits=c(0,100000),expand = c(0,0)),name='catch',dim=c(17,10),wd=.wd,type=type)
saveplot(ssbplot(x)+scale_y_continuous(limits=c(0,9e5),expand = c(0,0)),name='ssb',dim=c(17,10),wd=.wd,type=type)
saveplot(fbarplot(x)+scale_y_continuous(limits=c(0,4),expand = c(0,0)),name='F',dim=c(17,10),wd=.wd,type=type)
saveplot(plot(refBase),name='RP',dim=c(14,14),wd=.wd,type=type)
saveplot(selplot(x),name='sel',dim=c(14,14),wd=.wd,type=type)
saveplot(expplot(x),name='exp',dim=c(17,10),wd=.wd,type=type)
saveplot(parplot(x),name='par',dim=c(17,17),wd=.wd,type=type)
saveplot(plot(x),name='plot_all',dim=c(17,20),wd=.wd,type=type)
saveplot(scplot(x),name='ssb_rel',dim=c(20,12),wd=.wd,type=type)
saveplot(prodplot(x),name='prod',dim=c(20,12),wd=.wd,type=type)
saveplot(kobeplot(x),name='kobe',dim=c(14,12),wd=.wd,type=type)

update_geom_defaults("line", list(size = 1))
saveplot(scplot(x),name='ssb_rel',dim=c(20,12),wd=.wd,type=type)

if(retro){
    r <- retro(x,year=7,parallell=FALSE,silent=TRUE)  #maybe make plot with relative change
    save(r, file=paste0('Rdata/retro/',name,'_retro.Rdata'))
    saveplot(plot(r,ci=FALSE),name="retro",dim=c(25,20),wd=.wd,type=type)
    saveplot(plot(r,ci=TRUE),name="retro_CI",dim=c(25,20),wd=.wd,type=type)
    m <- round(mohn(r),2)
    write.table(m,paste0(.wd,"/mohn.txt"))
    #plot(unlist(lapply(lapply(r,ypr),'[','f40ssb')),type='l',ylab='SSBF40%',xlab='peel')
    #plot(unlist(lapply(lapply(r,ypr),'[','f40')),type='l',ylab='F40%',xlab='peel')
    
}

if(res){
    myres <- residuals(x)
    saveplot(plot(myres,fleet=c(2,3),qq=FALSE),name="res",dim=c(30,10),wd=.wd,type=type)
}

if(procres){
    myprocres <- procres(x)
    saveplot(plot(myprocres,qq=FALSE),name="pe",dim=c(20,10),wd=.wd,type=type)
}


# residuals the old way (though they are wrong because of autocorrelation due to random effects)
# saveplot(resplot(x,fleets = 3,type=1),name="/res_index_1",dim=c(17,10),wd=.wd,type=type)
# saveplot(resplot(x,fleets = 3,type=2,out=1),name="/res_index_2",dim=c(17,10),wd=.wd,type=type)
# saveplot(resplot(x,fleets = 3,type=3),name="/res_index_3",dim=c(17,10),wd=.wd,type=type)
# saveplot(resplot(x,fleets = 3,type=4),name="/res_index_4",dim=c(17,10),wd=.wd,type=type)
# saveplot(resplot(x,fleets = 3,type=4,trans = exp),name="/res_index_5exp",dim=c(17,10),wd=.wd,type=type)
# 
# saveplot(resplot(x,fleets = 2,type=1,low=c('red','orange'),high=c('grey','green','darkgreen')),name="/res_caa_1",dim=c(17,10),wd=.wd,type=type)
# saveplot(resplot(x,fleets = 2,type=2,out=3),name="/res_caa_2",dim=c(17,10),wd=.wd,type=type)
# saveplot(resplot(x,fleets = 2,type=3),name="/res_caa_3",dim=c(17,10),wd=.wd,type=type)
# saveplot(resplot(x,fleets = 2,type=4),name="/res_caa_4",dim=c(25,20),wd=.wd,type=type)
# saveplot(resplot(x,fleets = 2,type=5,std=TRUE),name="/res_caa_5",dim=c(25,20),wd=.wd,type=type)
# saveplot(resplot(x,fleets = 2,type=6),name="/res_caa_6",dim=c(17,10),wd=.wd,type=type)
# saveplot(resplot(x,fleets = 2,type=7),name="/res_caa_7",dim=c(17,10),wd=.wd,type=type)


