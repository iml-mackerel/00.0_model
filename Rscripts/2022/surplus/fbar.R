#################################################################################################################
#*** Mackerel assessment
#*** F values
#*** based on CCAM package
#################################################################################################################

fay <- faytable(fit)
nay <- ntable(fit)

fweighted <- function(f,n,a=ncol(f)){
    rowSums(f[,a,drop=FALSE]*n[,a,drop=FALSE])/rowSums(n[,a,drop=FALSE])
}
f5.10 <- tail(fweighted(fay,nay,5:10),1)
f3.10 <- tail(fweighted(fay,nay,3:10),1)
f1.10 <- tail(fweighted(fay,nay,1:10),1)

exp5.10 <- 1-exp(-f5.10)  # same as unweighted fbar because constant
exp3.10 <- 1-exp(-f3.10)
exp1.10 <- 1-exp(-f3.10)
