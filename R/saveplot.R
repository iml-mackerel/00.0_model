##' save plots in one line
##' @param plot plot
##' @param name vector sting (without extension)
##' @param wd working directory in which to place image
##' @param dim vector of dimensions (cm)
##' @param type png or pdf
##' @param res resolution (if png)
##' @param ... arguments passed to png
##' @importFrom gridExtra grid.arrange
##' @details Saves plot as png of pdf in one line
##' @rdname saveplot
##' @export
saveplot <- function(plot,name,wd=NULL,dim,type='png',res=300,...){
    if(!is.null(wd)){wd=paste0(wd,'/')}
    switch(type,
           png={
               png(file=paste0(wd,name,".png"),units="cm",width=dim[1],height=dim[2],res=res,...)
               if("ggplot" %in% class(plot)) grid.arrange(plot) else plot
               dev.off() 
           },
           pdf={
               inch <- function(x){x/cm(1)}
               pdf(file=paste0(wd,name,".pdf"),width=inch(dim[1]),height=inch(dim[2]),...)
               if("ggplot" %in% class(plot)) grid.arrange(plot) else plot
               dev.off()    
           },
           {
               print('Type does not exist')
           }
    )
}