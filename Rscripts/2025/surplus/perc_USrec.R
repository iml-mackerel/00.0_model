wave<- data.frame(Wave = c("MARCH/APRIL","MAY/JUNE","JULY/AUGUST","SEPTEMBER/OCTOBER", "NOVEMBER/DECEMBER"),
            WAVE = 2:6)



usrec1 <- readRDS(paste0("data/",outy,"/raw/USrec.agg.data.yr.RDS"))
#A is observed
#B1 is unobserved landings
#B2 is discards
#All of these are in numbers

#AB1.kg is A+B1 transformed with a weight -- something relationship. Discards were not 
usrec2 <-  full_join(read.csv(paste0("data/",outy,"/raw/mackerel_catch_wave_2016-04.csv"))  , wave) %>% 
    rename(Year=YEAR, AB1.kg=Landings_MT)
#discards are ignored because low in wave 2 and 6 higher in summer. + not used in 2025. Assumption to be made on weight if using
usrec <-  full_join(usrec1, usrec2) %>% 
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