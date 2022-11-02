
# fecundity
ssbplot(c(fit, fec_old,fec_raw, fec_25, fec_75),linesize = 2, ci = F) + 
    scale_colour_viridis_d(name = "Fecundity", labels = c("Core model 50%", "Fixed per age","No smoothing", "25%", "75%")) + theme_classic(base_size = 14)

recplot(c(fit, fec_old,fec_raw, fec_25, fec_75), ci = F) + 
    scale_colour_viridis_d(name = "Fecundity", labels = c("Core model 50%", "Fixed per age","No smoothing", "25%", "75%")) + theme_classic(base_size = 14) +
    scale_x_continuous(limits = c(1969,2020))  + scale_y_continuous(limits = c(0,600000))

fbarplot(c(fit, fec_old,fec_raw, fec_25, fec_75), ci = F) + 
    scale_colour_viridis_d(name = "Fecundity", labels = c("Core model 50%", "Fixed per age","No smoothing", "25%", "75%")) + theme_classic(base_size = 14)

# proportion mature

ssbplot(c(fit, mo_raw, mo_25, mo_75),linesize = 2, ci = F) + 
    scale_colour_viridis_d(name = "Proportion mature", labels = c("Core model 50%", "No smoothing", "25%", "75%")) + theme_classic(base_size = 14)

recplot(c(fit, mo_raw, mo_25, mo_75), ci = F) + 
    scale_colour_viridis_d(name = "Proportion mature", labels = c("Core model 50%", "No smoothing", "25%", "75%")) + theme_classic(base_size = 14)+
    scale_x_continuous(limits = c(1969,2020))  + scale_y_continuous(limits = c(0,600000))

fbarplot(c(fit, mo_raw, mo_25, mo_75), ci = F) + 
    scale_colour_viridis_d(name = "Proportion mature", labels = c("Core model 50%", "No smoothing", "25%", "75%")) + theme_classic(base_size = 14)

# Mass
ssbplot(c(fit, cw_raw, cw_25, cw_75),linesize = 2, ci = F) + 
    scale_colour_viridis_d(name = "Mass", labels = c("Core model 50%", "No smoothing", "25%", "75%")) + theme_classic(base_size = 14)

recplot(c(fit, cw_raw, cw_25, cw_75), ci = F) + 
    scale_colour_viridis_d(name = "Mass", labels = c("Core model 50%", "No smoothing", "25%", "75%")) + theme_classic(base_size = 14)+
    scale_x_continuous(limits = c(1969,2020))  + scale_y_continuous(limits = c(0,600000))

fbarplot(c(fit, cw_raw, cw_25, cw_75), ci = F) + 
    scale_colour_viridis_d(name = "Mass", labels = c("Core model 50%", "No smoothing", "25%", "75%")) + theme_classic(base_size = 14)




# raw data plots
mass <- as.data.frame(cw)
fecundity<- as.data.frame(fec)
propmat<-as.data.frame(mo)

mass %<>% mutate(year = seq(1968,2020,1)) %>% pivot_longer(cols = 1:10,names_to = "age") %>%
    mutate(age = fct_relevel(age, "1","2","3","4","5","6","7","8","9",'10'))
                                                                                                                      
fecundity %<>% mutate(year = seq(1968,2020,1)) %>% pivot_longer(cols = 1:10,names_to = "age")%>%
    mutate(age = fct_relevel(age, "1","2","3","4","5","6","7","8","9",'10'))
propmat %<>% mutate(year = seq(1968,2020,1)) %>% pivot_longer(cols = 1:10,names_to = "age")%>%
    mutate(age = fct_relevel(age, "1","2","3","4","5","6","7","8","9",'10'))

mass %>% ggplot(aes(year,value, colour = factor(age)))+geom_point()+ geom_line() + scale_colour_viridis_d()
fecundity %>% ggplot(aes(year,value, colour = factor(age)))+geom_point()+ geom_line() + scale_colour_viridis_d()
propmat %>% ggplot(aes(year,value, colour = factor(age)))+geom_point()+ geom_line() + scale_colour_viridis_d()
