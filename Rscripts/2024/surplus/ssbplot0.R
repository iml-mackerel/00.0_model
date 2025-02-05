ssbplot0 <-  function(fit, ci=F, language="BI", minyear=1969, year, legend=T){
    
  p1<-   ssbplot(fit,ci=ci, year=minyear:year)+ scale_y_continuous(limits=c(0,NA), expand=c(0,0)) + scale_x_continuous(breaks=seq(minyear, year, 5))
  p2  <- fbarplot(fit,ci=ci, year=minyear:year)+ scale_y_continuous(limits=c(0,NA), expand=c(0,0)) + scale_x_continuous(breaks=seq(minyear, year, 5))
  p3 <-  recplot(fit,ci=ci, year=minyear:year) + scale_y_continuous( limits=c(0,NA), expand=c(0,0)) + scale_x_continuous(breaks=seq(minyear, year, 5))
    
   if(language=="BI")  pout<- ggarrange(p1 + labs(x="", y="BSR | SSB"),
                                 p2 +labs(x="", y="Fbar"),
                                 p3 + labs(x="Année | Year", y="Recrutement\nRecruitment"),
         ncol=1, nrow=3, common.legend = legend,legend=if_else(legend, "right","none"), align="hv")
  
  if(language=="EN")  pout<- ggarrange(p1 + labs(x="", y="SSB"),
                                p2 +labs(x="", y="Fbar"),
                                p3 + labs(x="Year", y="Recruitment"),
                                ncol=1, nrow=3, common.legend = legend,legend=if_else(legend, "right","none"), align="hv")
  
  if(language=="FR")  pout <- ggarrange(p1 + labs(x="", y="BSR"),
                                p2 +labs(x="", y="Fbar"),
                                p3 + labs(x="Année", y="Recrutement"),
                                ncol=1, nrow=3, common.legend = legend,legend=if_else(legend, "right","none"), align="hv")
  
  return(pout)
  
}


relative_retro_plot <-  function(fits, language="BI", year){
 library(lemon)   
  retrodf<- bind_rows( ssbtable(fits) %>% as.matrix() %>% as.data.frame() %>%  mutate(var="BSR | SSB", varEN="SSB", varFR="BSR") ,  
    rectable(fits) %>% as.matrix() %>% as.data.frame() %>%  mutate(var="Recrutement | Recruitment", varEN ="Recruitment", varFR="Recrutement"),
     fbartable(fits) %>% as.matrix() %>% as.data.frame()  %>%  mutate(var="F", varEN="F", varFR="F"))
       
       
     pr<-   retrodf %>% dplyr::select(-Low, -High) %>%  
       pivot_wider(names_from="fit", values_from="Estimate") %>% 
       pivot_longer(peel2:peel7) %>%  
       mutate(year=as.numeric(year),
              default=as.numeric(default),
              value=as.numeric(value),
              rel.error=(value-default)/default *100,
              varEN=recode_factor(varEN, SSB="SSB",`F`="F",  Recruitment="Recruitment")) %>% 
       ggplot(aes(x=year, y=rel.error, col=name))+
       geom_hline(yintercept=0, col="grey", lty=2)+
       geom_line() +scale_color_viridis_d(guide="none") 
    
   
   if(language=="BI") pr <-  pr + labs(y="Erreur relative au modèle complet \nError relative to the full model (%)", x="Année | Year") +
           scale_x_continuous(breaks=seq(1970, year, 5)) +facet_rep_wrap(~var, ncol=1,repeat.tick.labels = 'bottom')
   
   if(language=="EN")  pr <-  pr+ labs(y="Error relative to the full model (%)", x="Year") +
           scale_x_continuous(breaks=seq(1970, year, 5))+facet_rep_wrap(~varEN, ncol=1,repeat.tick.labels = 'bottom')
   
   if(language=="FR")  pr <-  pr + labs(y="Erreur relative au modèle complet (%)", x="Année") +
           scale_x_continuous(breaks=seq(1970, year, 5))+facet_rep_wrap(~varFR, ncol=1,repeat.tick.labels = 'bottom')
   

return(pr)
    
}
