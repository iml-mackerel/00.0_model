##' smoothmatrix
##' @param x matrix
##' @param subset index of values to take 
##' @param smooth smoothing degree (0-1), as par in smooth.spline
##' @param byrow logical. smooth by row. default=TRUE
##' @param max upper limit
##' @param plot logical. plot smoothing result
##' @details smooths out values in a matrix.
##' @return matrix of same shape as x, but values are smoothed over rows or columns
##' @export
smoothmatrix <- function(x,subset=NULL,smooth=0.99,byrow=TRUE,max=NULL,plot=FALSE){
    for(i in 1:ncol(x)){
        if(byrow){
            y=x[,i] 
            xv=1:nrow(x)
            if(plot & i==1) matplot(x)
        }else{
            y=x[i,]
            xv=1:ncol(x)
            if(plot & i==1) matplot(t(x))
        }
        n <- length(xv)
        
        if(!is.null(subset)){
            y=y[subset]
            xv=xv[subset]
        }
        
        smoothingSpline = smooth.spline(xv, y, spar=smooth)
        smo <- predict(smoothingSpline,x = 1:n)$y
        if(!is.null(max)) smo[smo>max] <- max
        if(plot) lines(smo)
        
        if(byrow) x[,i] <- smo else x[i,] <- smo
    }  
    return(x)
}