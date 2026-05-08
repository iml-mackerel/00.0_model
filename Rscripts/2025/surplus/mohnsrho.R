mohn.ccamset <- function(fits, what=NULL, lag=0){
    if(is.null(what)){
        what <- function(fit)summary(fit)[,c(1,4,7),drop=FALSE]
    }
    ref <- what(fits$default)
    ret <- lapply(fits, what)
    bias <- lapply(ret, function(x){
    y<-rownames(x)[nrow(x)-lag]
    (x[rownames(x)==y,]-ref[rownames(ref)==y,])/ref[rownames(ref)==y,]})
    colMeans(do.call(rbind,bias))
}


