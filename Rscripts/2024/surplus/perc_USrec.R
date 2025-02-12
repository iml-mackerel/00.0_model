
usrec<- readRDS(paste0("data/",outy,"/raw/USrec.agg.data.yr.RDS")) %>% 
    dplyr::mutate(new.wave= if_else(grepl(Wave, pattern="MARCH")| grepl(Wave, pattern="DEC"),
                                            "Winter", "Summer")) %>%  
    ungroup() %>% dplyr::group_by(new.wave, Year) %>% 
    dplyr::summarize(rec.kg=sum(AB1.kg)) %>%  pivot_wider(names_from="new.wave", values_from=rec.kg) %>% 
    dplyr::mutate(perc_rec= Winter/ (Summer+Winter) *100) %>%  dplyr::rename(year=Year)
quantile(usrec$perc_rec) 

write.csv(usrec, paste0("data/", outy, "/raw/USrec_yearly_during_winter.csv"), row.names=F)


dec.usrec<- usrec %>%  mutate(decade= year - year %% 10 ) %>%  dplyr::group_by(decade) %>% 
    mutate(dec_perc_rec=mean(perc_rec)) 
   

write.csv(dec.usrec, paste0("data/", outy, "/raw/USrec_decades_during_winter.csv"), row.names=F)




#+ggtitle("Decadal %rec")
#+ggtitle("Annual %rec")