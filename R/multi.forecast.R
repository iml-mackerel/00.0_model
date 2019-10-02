##' run multiple forecasts
##' @param x list with elements containing forecast parameters
##' @param dir directory to save forecasts
##' @param parallel logical
##' @details perform multiple forecasts at once (for MSE)
##' @return does not return an object. Save all individual forecasts as Rdata
##' @import parallel
##' @rdname multi.forecast
##' @export
multi.forecast <- function(x,dir,parallel=FALSE,ncores=NULL){
    if(parallel){
        library(parallel)
        if(is.null(ncores))ncores <- detectCores()
        cl <- makeCluster(6) #set up nodes
        clusterEvalQ(cl, {library(CCAM)}) #load the package to each node
        clusterExport(cl, c('dir','x','ctUSA'), envir=environment())
        empty <- parLapply(cl, 1:length(x), function(y){ 
            run <- do.call(forecast, x[[y]])
            save(run,file=paste0(dir,names(x)[y],'.Rdata'))
        })
        stopCluster(cl) 
    }else{
        empty <- lapply(1:length(x),function(y){
            run <- do.call(forecast, x[[y]])
            save(run,file=paste0(dir,names(x)[y],'.Rdata'))
        }) 
    }
}