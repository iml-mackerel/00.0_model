
mytableit<- function (fit, what, trans=function(x)x,fleet=NULL,...){
  idx<-names(fit$sdrep$value)==what
  y<-fit$sdrep$value[idx]
  ci<-y+fit$sdrep$sd[idx]%o%c(-2,2)
  ret<-trans(cbind(y,ci))
  colnames(ret)<-c("Estimate","Low","High")
     ret <- rbind(ret,matrix(c(1,NA,NA),ncol=3,nrow=1,byrow=TRUE))
      rownames(ret)<-1:nrow(ret)### changed this line only
     #1 at the end of blocks
    #ret<-  ret %>% as.data.frame() %>%  mutate(diff= c(NA,diff(Estimate)))
    #neg<- which(ret$diff <0)
    #neg<- neg[which(round(ret[neg-1, "Estimate"],1)!=1)]
    #ret = as.matrix(ret %>%  dplyr::select(-diff))
    
#    if(length(neg) >0) ret <- rbind(ret[1:(neg-1),],matrix(c(1,NA,NA),ncol=3,nrow=1,byrow=TRUE),
 #                                   ret[neg: nrow(ret),])
     
    
  ret <- data.frame(ret) 
  ret$param = 1:nrow(ret)
  return(ret)
}

