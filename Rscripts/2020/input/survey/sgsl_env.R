sgsl_ichthyo_temp <- read.csv("C:/Users/SmithAND/Documents/Data/Ichthyoplankton/sgsl_ichthyo_temp.csv") %>% as_tibble()
sgsl_ichthyo_temp %<>% dplyr::select(1:33)
sgsl_ichthyo_temp %<>% pivot_longer(cols = starts_with("X"), names_to = "year", values_to = "temp_10m") %>% mutate(year = as.numeric(str_remove(year, "X")),station = as.character(station))
sgsl_ichthyo_env <- read_csv("data/survey/sgsl_ichthyo_env.csv", 
                             col_types = cols(station = col_character(), 
                                              day = col_character()))

sort(unique(sgsl_ichthyo_env$station))

sgsl_ichthyo_env %<>% arrange(year, station, month, day, hour,min)
sgsl_ichthyo_env %<>% mutate(row = rownames(sgsl_ichthyo_env))
sgsl_ichthyo_env %<>% mutate(id = paste(year,station,day, sep = "-"))

dup <- as.data.frame(match(unique(sgsl_ichthyo_env$id), sgsl_ichthyo_env$id)) %>% as_tibble() %>% transmute(row = as.character(`match(unique(sgsl_ichthyo_env$id), sgsl_ichthyo_env$id)`), PASS = 1)
sgsl_ichthyo_env <- left_join(sgsl_ichthyo_env,dup)
glimpse(sgsl_ichthyo_env)
sgsl_ichthyo_env %<>% mutate(PASS = as.character(replace_na(PASS,"2")))

sgsl_ichthyo_env %<>% transmute(year = year, 
                               month = month, 
                               day = day,
                               date = as_date(paste(year, month, day)),
                               doy = yday(date),
                               hour = hour, 
                               min = str_sub(min,1,2), 
                               station = station, 
                               temp_0_10m = temp_0_10m, 
                               sal_0_10m = sal_0_10m, 
                               sigma_t_kg_m3 = sigma_t_kg_m3, 
                               pass = PASS) %>% 
    arrange(year, month, doy, hour, pass) %>% 
    dplyr::filter(year != 17)

sgsl_ichthyo_env %<>% mutate(station = ifelse(station == 5, "5.0",ifelse(station ==6,"6.0",ifelse(station==7,"7.0",ifelse(station==8,"8.0",ifelse(station==9,"9.0",ifelse(station==2,"2.1",station)))))))

df_coords <- mackerel_ichthyo %>% group_by(station) %>% dplyr::summarise(lat = mean(lat,na.rm=T),lon=mean(lon,na.rm=T))
sgsl_ichthyo_env <- left_join(sgsl_ichthyo_env,df_coords)
sgsl_ichthyo_temp %<>% dplyr::select(-lon,-lat)
sgsl_ichthyo_temp <- left_join(sgsl_ichthyo_temp,df_coords)
sgsl_ichthyo_temp %<>% dplyr::filter(!is.na(station),!is.na(year))
sgsl_env <- full_join(sgsl_ichthyo_temp,sgsl_ichthyo_env)
#
mackerel_ichthyo %<>% dplyr::select(-lat,-lon)
mackerel_ichthyo <- left_join(mackerel_ichthyo, df_coords)

df <- full_join(mackerel_ichthyo,sgsl_ichthyo_env)

