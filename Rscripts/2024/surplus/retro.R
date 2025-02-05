

mohn.ccamset <- function(fits, what=NULL, lag=0){
    if(is.null(what)){
        what <- function(fit)summary(fit)[,c(1,4,7),drop=FALSE]
    }
    ref <- what(attr(fits,"fit"))
    ret <- lapply(fits, what)
    bias <- lapply(ret, function(x){y<-rownames(x)[nrow(x)-lag]; (x[rownames(x)==y,]-ref[rownames(ref)==y,])/ref[rownames(ref)==y,]})
    colMeans(do.call(rbind,bias))
}

    r <-retro(x,year=7,parallell=F)  #maybe make plot with relative change
    save(r, file=paste0('Rdata/',year,'/retro/',name,'_retro.Rdata'))
    saveplot(plot(r,ci=FALSE,legendnames=paste0("peel",1:7)),name="retro",dim=c(16,16),wd=.wd,type=type)
    saveplot(plot(r,ci=FALSE,legendnames=paste0("peel",1:7)),name="FR/retro",dim=c(16,16),wd=.wd,type='pdf')
    saveplot(plot(r,ci=TRUE,legendnames=paste0("peel",1:7)),name="retro_ci",dim=c(16,16),wd=.wd,type=type)
    saveplot(plot(r,ci=TRUE,legendnames=paste0("peel",1:7),year=2010:year)+scale_x_continuous(breaks=2010:year),name="retro_ci_zoom",dim=c(16,16),wd=.wd,type=type)
    saveplot(plot(r,ci=TRUE,legendnames=paste0("peel",1:7),year=2010:year)+scale_x_continuous(breaks=2010:year),name="FR/retro_ci_zoom",dim=c(16,16),wd=.wd,type='pdf')
    m <- round(mohn.ccamset(r),2)
    write.table(m,paste0(.wd,"/mohn.txt"))
    #df <- data.frame(peel=1:7,LRP=unlist(lapply(lapply(r,ypr),'[','f40ssb'))*0.4)
    #rLRP <- ggplot(df,aes(x=peel,y=LRP))+geom_line()
    #saveplot(rLRP,name='retro_LRP',dim=c(8,6),wd=.wd,type=type)
