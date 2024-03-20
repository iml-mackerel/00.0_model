.First <- function(){
  
  ### load packages CRAN
  list.of.packages <- c('reshape2','ggplot2','gridExtra','viridis','plyr','lubridate')
  new.packages <- list.of.packages[!(list.of.packages %in% utils::installed.packages()[,"Package"])]
  if(length(new.packages)) install.packages(new.packages)
  lapply(list.of.packages, function(x) suppressMessages(require(x, character.only = TRUE)))
  
  ### github
  git.packages <- c('CCAM')
  install.this <- git.packages[!(git.packages %in% utils::installed.packages()[,"Package"])]
  if('CCAM' %in% install.this)  devtools::install_github("elisvb/CCAM")
  
  ### source directory
  invisible(sapply(list.files(pattern="[.]R$", path="R/", full.names=TRUE), source))
  
  ### ggplot layout
  theme_new <- theme_classic()+
                theme(axis.line.x = element_line(color="black"),
                      axis.line.y = element_line(color="black"),
                      legend.background = element_rect(fill=alpha('black', 0)),
                      strip.background = element_blank())
  
  theme_set(theme_new)
}
