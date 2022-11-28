######### Mackerel Ichthyoplankton data from the Southern Gulf of St Lawrence
##
##  Script to subset southern Gulf of St Lawrence spring ichthyoplankton survey data (1979-2020) (sgsl_ichthyo_full)
##  and calculate the empirical daily egg production at each station. Also Figures
##  
##  bY ANDREW D SMITH
##  
##  This script creates a number of variables used as conditional indices to subset the data. They are named either as "pass_x" or "use_x".
##  - To subset the mackerel_ichthyo data to the first pass and to use the pre 2014 published set, use pass_3 == 1 and use_2 %in% c(1,2)
##
##  Change Log:
##  v.0 = pre 2017: all data on annual, differently formatted spreadsheets (.xls or .xlsx) in S/Pélagiques/Plancton/Relevés
##  v.1 = 2017: Baye Mbaye and Elisabeth van Beveren wrote eggSurvey.R but data was messy and only covered years 2012 onwards. published values were used otherwise.
##  v.1.5 = August 2018: Andrew Smith, Jean Martin Chamberland, and Mélanie Boudreau all make attempts to clean and merge the data. The end result, largely rewritten by ADS is SSB_index_egg_survey.R. Still many problems with formatting and bugs. raw data used for quality control and summary statistics. published values used in model with updates from raw for years 2015:2018
##  v.2.0 = November 2020: Following the validation and transfer of the data onto the national BioChem database by the DAISS team at IML (Isabelle St. Pierre and Hélène Dionne), 
##        - ADS writes read_ichthyo.R to merge and format the BioChem data and names the data sgsl_ichthyo_full. Some formatting still needed for station names etc. 1982-1985 hard coded in for the instant (original mission sheets MIA). 
##        - January 2021: ADS writes this script (mackerel_egg_index.R) to read and parse new format
##                          - Added summarised mission metadata as well gsi, krigging, and other varia model params
##                          - Added temperature data from Caroline. Lafleur - Francois Grégoire calculated these a different way as values differ. Decision: years(1979-2014 = published set, 2015-2020 C. Lafleur DAISS)
##                          - Added figures to do quality control
##                          - Did quality control and calculated incubation and daily egg production
##                          - GSI logistic curves and calculation of spawning fraction moved to mackerel_gsi.R
## Last full DFO Research Documents detailing the ichthyoplankton survey were:
#   - Grégoire, F., Girard, L. et Boudreau, M. 2014. Résultats des relevés du programme de monitorage zonal atlantique (PMZA)-maquereau bleu (Scomber scombrus L.) réalisés dans le sud du golfe du Saint-Laurent en 2012 et 2013. Secr. can. de consult. sci. du MPO. Doc. de rech. 2014/075. v + 82 p.
##  and
##  - Grégoire, F., Girard, L., Beaulieu, J.-L., Lussier, J.-F., et Gendron, M.-H. 2014. Détection des tendances communes dans les abondances d’oeufs et de larves de poissons récoltés dans le sud du golfe du Saint-Laurent entre 1983 et 2012. Secr. can. de consult. sci. du MPO. Doc. de rech. 2014/074. v + 34 p.
##  and
##  - Grégoire, F., Girard, L., et Beaulieu, J.-L. 2014. Analyses de similarité appliquées sur les abondances de larves de poissons récoltées dans le sud du golfe du Saint-Laurent entre 1983 et 2012. Secr. can. de consult. sci. du MPO. Doc. de rech. 2014/080. v + 16 p.
## 
#########

#####################  LOAD PACKAGES  ####################
packages = c('ggforce', 'cowplot', 'esquisse', 'magrittr', 'lubridate', 'readr', 'tidyverse', 'mapproj', 'raster','gstat','ggthemes','maptools','mapdata','gridExtra','sp','gstat','rgeos','rgdal','geoR','tmap','sf')
invisible(lapply(packages, function(x) {
    if (!require(x, character.only = T)) {
        install.packages(x)
        require(x)
    }
}))

# source functions
source("Rscripts/bio/mackerel_fun_incubation.R")
source("Rscripts/survey/moy.var.Krigeage.R")

# set wd
project_wd <- getwd()

#####################  LOAD DATA  ####################
# Southern Gulf of Saint Lawrence Ichthyoplankton Data 

load("Rdata/sgsl_ichthyo_full.Rdata") 
figures_path <- file.path("img/")

# Temperature data from Caroline Lafleur
setwd("C:/Users/SmithAND/Documents/Data/Ichthyoplankton")
file_list <- list.files(path = "C:/Users/SmithAND/Documents/Data/Ichthyoplankton", pattern = "Moyenne")

temperature_data <- file_list %>%
    map_dfr(~read.delim(.x, header = FALSE, sep=",",skip = 1, col.names = c("file","station","date","time","lat","lon","temperature","salinity","sigma_t"))%>%
                mutate_all(as.character))

setwd(project_wd)
temperature_data$df <- "DAISS"
temperature_data$year <- str_sub(temperature_data$date, 8,11)
temperature_data %<>% mutate(lat = as.numeric(lat), lon = as.numeric(lon), temperature = as.numeric(temperature), year = as.numeric(year))

# Temperature data from 2014 ResDoc (1979-2013)
sgsl_ichthyo_temp <- read.csv("C:/Users/SmithAND/Documents/Data/Ichthyoplankton/sgsl_ichthyo_temp.csv") %>% as_tibble()
sgsl_ichthyo_temp$df <- "resdoc_2014"

sgsl_ichthyo_temp %<>% dplyr::select(1:33)
sgsl_ichthyo_temp %<>% pivot_longer(cols = starts_with("X"), names_to = "year", values_to = "temp_10m") %>% mutate(year = as.numeric(str_remove(year, "X")),station = as.character(station))

# Merge datasets
temp_df <- bind_rows(sgsl_ichthyo_temp,temperature_data)

# Changed station names in sublime after cleaning below 
temp_df_cor <- read.csv(file = "data/survey/temp_df_cleaned_stations.csv")
temp_df_cor %<>% mutate(ifelse(station == "5", "5.0", 
                              ifelse(station == "6", "6.0", 
                                     ifelse(station == "7", "7.0", 
                                     ifelse(station == "8", "8.0", 
                                            ifelse(station == "9", "9.0",station))))))

#####################  PREPARE DATA  ####################

# Subset database to mackerel
mackerel_ichthyo <- sgsl_ichthyo_full %>% 
    dplyr::filter(TAXONS %in% c(
        "Scomber scombrus (oeuf stade 1)",
        "Scomber scombrus (oeuf stade 2)",
        "Scomber scombrus (oeuf stade 3)",
        "Scomber scombrus (oeuf stade 4)",
        "Scomber scombrus (oeuf stade 5)",
        "Scomber scombrus (larva)"))

# Subset, change names, and calculate concentration and density
mackerel_ichthyo %<>% 
    dplyr::select(year, month, day, doy, EVENT_STIME_HOUR,EVENT_STIME_MIN,
                  EVENT_MIN_LAT, EVENT_MIN_LON,
                  EVENT_COLLECTOR_STN_NAME, extra_station,extra_station_1991,
                  extra_station_1991b, extra_station_1986, extra_station_northumberland, 
                  extra_station_ns_1998,extra_station_st_georges_bay_1994,extra_station_2000,pass,
                  EVENT_COLLECTOR_EVENT_ID,
                  PL_HEADR_START_DEPTH, PL_HEADR_VOLUME, PL_GEN_SPLIT_FRACTION, 
                  TAXONS, PL_GEN_COUNTS) %>%
    transmute(year = year,
              month = as.character(month),
              day = as.character(day),
              doy = doy,
              hour = EVENT_STIME_HOUR,
              min = EVENT_STIME_MIN,
              lat = EVENT_MIN_LAT,
              lon = EVENT_MIN_LON,
              station = EVENT_COLLECTOR_STN_NAME,
              consec = EVENT_COLLECTOR_EVENT_ID,
              extra_station = extra_station, # first pass vs second pass (y/n) = (1/2)            
              extra_station_1991 = extra_station_1991, # extra stations (y/n) = (2,1)
              extra_station_1991b = extra_station_1991b,
              extra_station_1986 = extra_station_1986,
              extra_station_northumberland =extra_station_northumberland,
              extra_station_ns_1998 = extra_station_ns_1998,
              extra_station_st_georges_bay_1994 = extra_station_st_georges_bay_1994,
              extra_station_2000 = extra_station_2000, 
              pass = pass,
              depth = PL_HEADR_START_DEPTH, # bongo depth
              volume = PL_HEADR_VOLUME, # flowmeter or bionet volume depending on the year
              fraction = PL_GEN_SPLIT_FRACTION, # fraction sampled
              stage = TAXONS,
              n = PL_GEN_COUNTS, # count
              N = n/fraction, # corrected count
              N_m3 = N/volume, # number per cubed metre (concentration)
              N_m2 = N_m3 * depth) # number per square metre (density)

# Format variables
mackerel_ichthyo %<>% mutate(consec = str_remove(consec, "consec"),
                            consec = str_remove(consec, "a"),
                            consec = str_remove(consec, "b"),
                            consec = as.numeric(consec))

# Subset and widen data
mackerel_ichthyo %<>%
    dplyr::select(-n, -N, -N_m3) %>% 
    pivot_wider(names_from = stage, values_from = N_m2, values_fn = mean)

# Combine egg stages 1 (recently hatched) & 5 eggs (damaged - mostly stage 1 as determined by JJ Maguire and Francois Grégoire)
mackerel_ichthyo$N_m2 =  mackerel_ichthyo$`Scomber scombrus (oeuf stade 1)` + mackerel_ichthyo$`Scomber scombrus (oeuf stade 5)`

# The way the data is formatted in BioChem, pass number was not included. Using station names to ID extra stations used above and in read_ichthyo.R
# Solution: ID duplicate station per year by rowname to create index then use multiple indices to classify
mackerel_ichthyo %<>% arrange(year, station, doy, consec)
mackerel_ichthyo %<>% mutate(row = rownames(mackerel_ichthyo))
mackerel_ichthyo %<>% mutate(id = paste(year,station, sep = "-"))

dup <- as.data.frame(match(unique(mackerel_ichthyo$id), mackerel_ichthyo$id)) %>% as_tibble() %>% transmute(row = as.character(`match(unique(mackerel_ichthyo$id), mackerel_ichthyo$id)`), pass_2 = 1)
mackerel_ichthyo <- left_join(mackerel_ichthyo,dup)
glimpse(mackerel_ichthyo)
mackerel_ichthyo %<>% mutate(pass_2 = replace_na(pass_2,"2"))
mackerel_ichthyo <- mackerel_ichthyo %>% mutate(pass_3 = ifelse(pass == 1 & pass_2 == 1, "1",
                                          ifelse(pass == 1 & pass_2 == 2, "2", 
                                                 ifelse(pass == 2, "2",
                                                 "extra"))))
# Replace NA with pass = 2
mackerel_ichthyo %<>% 
    mutate(pass_3 = replace_na(pass_3, "2"),
           pass_3 = ifelse(year == 1991 & pass == 2 & station == "8.4", "2",
                           ifelse(year == 2000 & str_detect(station, "supp"), "extra", pass)))
mackerel_ichthyo %<>% mutate(pass_3 = replace_na(pass_3,"1"))
mackerel_ichthyo %<>% mutate(pass_3 = ifelse(year %in% c(1987, 1988) & pass_2 == "2", "2", pass_3))
mackerel_ichthyo %<>% mutate(pass_3 = ifelse(year %in% c(1999) & pass_3 == "2", "1", pass_3))
mackerel_ichthyo %<>% mutate(pass_3 = ifelse(year %in% c(2007) & pass_3 == "2", "extra", pass_3))
# Station without any characters for merging
mackerel_ichthyo %>% mutate(stn = str_sub(station,1,3))

# Check parsing
mackerel_ichthyo %>% ggplot(aes(lon,lat,label = station, colour = pass_3)) +
    geom_text() + facet_wrap(vars(year, pass_3))

# factor year
mackerel_ichthyo$f_year <- as.factor(mackerel_ichthyo$year)

# presence/absence
mackerel_ichthyo %<>% mutate(presence = ifelse(N_m2 > 0, "1", "0"))

# Save
save(mackerel_ichthyo, file = "Rdata/mackerel_ichthyo")
write.csv(mackerel_ichthyo, file = "data/survey/mackerel_ichthyo.csv")
write.csv(sgsl_ichthyo_full, file = "data/survey/sgsl_ichthyo_full.csv")
write.csv(temp_df, file = "data/survey/temp_df.csv")

#####################  COMPARE SUMMARISED DATA WITH PUBLISHED  ####################
# load("Rdata/mackerel_ichthyo.Rdata")
mackerel_ichthyo <- read_csv("data/survey/mackerel_ichthyo.csv", 
                             col_types = cols(station = col_character(), 
                                              extra_station = col_character(), 
                                              extra_station_1991 = col_character(), 
                                              extra_station_1991b = col_character(), 
                                              extra_station_1986 = col_character(), 
                                              extra_station_northumberland = col_character(), 
                                              extra_station_ns_1998 = col_character(), 
                                              extra_station_st_georges_bay_1994 = col_character(), 
                                              extra_station_2000 = col_character(), 
                                              pass = col_character(), pass_2 = col_character(), 
                                              pass_3 = col_character()))

# res doc published data to compare
resdoc_2014 <- read_csv("data/survey/egg_density_res_2014.075.csv", 
                        col_types = cols(`1996` = col_character(), 
                                         `1998` = col_character(), `2006` = col_character(), 
                                         `2007` = col_character(), `2008` = col_character(), 
                                         `2009` = col_character(), `2010` = col_character(), 
                                         `2011` = col_character(), `2012` = col_character(), 
                                         `2013` = col_character()))
resdoc_2014 %<>% pivot_longer(cols = 5:32, names_to = "year") %>%
    transmute(year = as.numeric(year), station = as.character(station), stratum = as.numeric(strata), lon = lon, lat = lat, N_m2 = as.numeric(value)) %>% 
    arrange(year, station)

resdoc_2014$df <- "resdoc_2014"
resdoc_2014$pass_3 <- "1"
resdoc_2014$pass_2 <- "1"
resdoc_2014$use <- 1
mackerel_ichthyo$df <- "mackerel_ichthyo"

# egg_survey_stats <- read_csv("data/survey/egg_survey_stats.csv")
dat <- bind_rows(mackerel_ichthyo,resdoc_2014)

dat %>% dplyr::filter(pass_3 == 1, year < 2014) %>% ggplot(aes(year, N_m2, colour = df)) + geom_point(position = position_dodge(width = 1)) + scale_color_viridis_d(end = 0.8) +
    theme_minimal(base_size = 14) +
    theme(legend.position = c(0.75,0.75), legend.title = element_blank()) 

dat %>% dplyr::filter(pass_3 == 1) %>% ggplot(aes(year, N_m2, colour = df)) + stat_summary(fun.data = "mean_cl_boot",position = position_dodge(width = 1)) + scale_color_viridis_d(end = 0.8) +
    theme_minimal(base_size = 14) +
    theme(legend.position = c(0.75,0.75), legend.title = element_blank()) 


MISSION_MORE_COMMENT
MISSION_COLLECTOR_COMMENT
PL_HEADR_DATA_MANAGER_COMMENT
PL_GEN_COLLECTOR_COMMENT
PL_HEADR_COLLECTOR_COMMENT
PL_GEN_DATA_MANAGER_COMMENT
MISSION_DATA_MANAGER_COMMENT
PL_HEADR_DATA_MANAGER_COMMENT


#####################  MERGE WITH CTD TEMPERATURE DATA  ####################
# NOTE: Either from resdoc 2014 or from Caroline Lafleur-- though the values differ... so choice is choose historic published and from 2014 on use CLs data

# For plotting and merging get mean coords per station
df_coords <- mackerel_ichthyo %>%
    group_by(station) %>%
    dplyr::summarise(lat = mean(lat,na.rm = T),lon = mean(lon,na.rm = T))

# Exchange coords
temp_df_cor %<>% dplyr::select(-lon,-lat)
temp_df_cor <- left_join(temp_df_cor,df_coords)
# temp_df_cor %<>% transmute(year = year, stratum = strata, station = station, date, time, lat = lat, lon = lon, temp_resdoc_2014 = temp_10m, temp_daiss = temperature, salinity = salinity, sigma_t = sigma_t, df = df, file = file)
temp_df_cor %<>% dplyr::filter(!is.na(station),!is.na(year))
dat %<>% dplyr::select(-lat,-lon)
dat <- left_join(dat, df_coords)

# choose published for years prior to 2014
temp_df_cor %<>% mutate(temperature = ifelse(year < 2014, temp_resdoc_2014, temp_daiss))
temp_df_cor %<>% dplyr::select(-stratum,-file,-df)
# If missing temperature data for a station or temperature data for a missing station, merge and calculate mean temperatures per year/day = mean_temp.x or year = mean_temp.y if day is NA Alternative is fill in with EA.data(SST.06 for EAR == 2 i.e. magdallen shallows)
df <- left_join(dat,temp_df_cor)
df2 <- df %>% group_by(year, doy) %>% dplyr::summarise(mean_temp = mean(temperature, na.rm = T))
df3 <- df %>% group_by(year) %>% dplyr::summarise(mean_temp = mean(temperature, na.rm = T))

# Fill in gaps via linear interpolation (i.e. take the mean from values bookending it)
library(zoo)
df3 %<>% mutate(mean_temp = ifelse(is.na(mean_temp), na.approx(df3$mean_temp), mean_temp)) 

# If temperature is NS then use year and day average, if that is na use year average, if that is na then linear interpolate
df <- left_join(df, df2)
df <- left_join(df, df3, by = "year")
df %<>% mutate(temp = ifelse(is.na(temperature),mean_temp.x, temperature),
               temp = ifelse(is.na(temp),mean_temp.y,temp))

# Assign missing values for strata (as per Patrick Oullet 1998)
df %<>% mutate(stn = str_sub(station,1,3))
df %<>% mutate(stratum = ifelse(station %in% c("8.7", "7.7", "5.7","4.9","3.9","3.8","2.6","1.5","2.5","1.4","2.4","1.3","3.5","3.4",'3.3',"3.2",'3.1',"4.2","4.1","2.3","2.2",'2.1',"1.2","1.1"), "1", 
                                             ifelse(station %in% c("10.1","9.4","9.3",'8.2','7.2','7.1',"6.3","6.2","6.1","5.2","5.1",'4.3',"4.4","4.5",'4.6',"3.6","3.7","4.8","5.6",'6.7',"7.5","7.6","6.6"), '2', "3"))) 
df %<>% dplyr::select(-strata)

# rename
mackerel_ichthyo <- df
glimpse(mackerel_ichthyo)
mackerel_ichthyo %<>% dplyr::filter(!is.na(N_m2)) %>% 
    mutate(use_2 = case_when(df == "resdoc_2014" ~ "1", 
                             year > 2013 ~ "2", TRUE ~ "0"))
save(mackerel_ichthyo, file = "Rdata/mackerel_ichthyo")
write.csv(mackerel_ichthyo, file = "data/survey/mackerel_ichthyo.csv")
#####################  INCUBATION AND DAILY EGG PRODUCTION  ####################
# Calculate daily egg production (dep)
mackerel_ichthyo  %<>%  mutate(dep = (N_m2/I_lockwood(temp))*24)

#####################  SAVE  ####################
# Specify official year set rules (1979-2020)
mackerel_ichthyo %<>% mutate(use = ifelse(year %in% c(1982, 1999, 2001, 2006), 0, 1))
# 

# some stations that have NAs will not show up in plots because pass_3 or use == NA
save(mackerel_ichthyo, file = "Rdata/mackerel_ichthyo.Rdata")
write.csv(mackerel_ichthyo, file = "data/survey/mackerel_ichthyo.csv")

load(file = "Rdata/mackerel_ichthyo.Rdata")

#####################  FIGURES  ####################
# stages
a <- mackerel_ichthyo %>%  ggplot(aes(year, `Scomber scombrus (oeuf stade 1)`)) + geom_col() + labs(y = "Stage 1", x = "") + theme_minimal()
b <- mackerel_ichthyo %>% ggplot(aes(year, `Scomber scombrus (oeuf stade 2)`)) + geom_col() + labs(y = "Stage 2", x = "") + theme_minimal()
d <- mackerel_ichthyo %>% ggplot(aes(year, `Scomber scombrus (oeuf stade 3)`)) + geom_col() + labs(y = "Stage 3", x = "") + theme_minimal()
e <- mackerel_ichthyo %>% ggplot(aes(year, `Scomber scombrus (oeuf stade 4)`)) + geom_col() + labs(y = "Stage 4", x = "") + theme_minimal()
f <- mackerel_ichthyo %>% ggplot(aes(year, `Scomber scombrus (oeuf stade 5)`)) + geom_col() + labs(y = "Stage 5", x = "") + theme_minimal()
g <- mackerel_ichthyo %>% ggplot(aes(year, `Scomber scombrus (larva)`)) + geom_col() + labs(y = "Larvae", x = "") + theme_minimal()
plot_grid(a,b,d,e,f,g)

# Maps
# interactive map by year 
canada <- map_data("world", "Canada") %>% mutate(lon = long)
canada_map <- ggplot() + geom_polygon(data = canada,aes(x = lon, y = lat, group = group),fill = "gray20",color = "black") + theme_bw() 
canada_atlantic <- canada_map + 
    coord_map(xlim = c(-69, -57.5),  ylim = c(43.5, 50), projection = "lambert", parameters = c(45,50))       
sGSL <- canada_map + 
    coord_map(xlim = c(-66.5, -59),  ylim = c(45.6, 49), projection = "lambert", parameters = c(46.5 ,48)) 
sGSL_2 <- sGSL + 
    coord_map(xlim = c(-65, -63),  ylim = c(48.5, 50.5), projection = "lambert", parameters = c(46.5 ,48))  
# rm(canada);rm(canada_map);rm(canada_atlantic)

# stage maps
a <- sGSL +  geom_point(data = mackerel_ichthyo %>% dplyr::filter(pass_3 == 1, year > 1979), aes(lon,lat, size = `Scomber scombrus (oeuf stade 1)`),shape = 21, colour = "black",  fill = "red", alpha = 0.8) + labs(y = "Latitude", x = "Longitude") + theme_minimal() + scale_color_viridis_c() + scale_size_area(max_size = 6) + theme(legend.position = "top")
a + facet_wrap_paginate(vars(year), ncol = 5, nrow = 4, page = 1) + ggsave("egg_1_1.png", device = "png", path = figures_path, width = 6.5, height = 7, units = "in", dpi = 300)
a + facet_wrap_paginate(vars(year), ncol = 5, nrow = 4, page = 2)+ ggsave("egg1_2.png", device = "png", path = figures_path, width = 6.5, height = 7, units = "in", dpi = 300)
a <- sGSL +  geom_point(data = mackerel_ichthyo %>% dplyr::filter(pass_3 == 1, pass_2 == 1), aes(lon,lat,size = `Scomber scombrus (oeuf stade 2)`),shape = 21, colour = "black",  fill = "red", alpha = 0.8) + labs(y = "Latitude", x = "Longitude") + theme_minimal() + scale_color_viridis_c() + scale_size_area(max_size = 6) + theme(legend.position = "top")
a + facet_wrap_paginate(vars(year), ncol = 5, nrow = 4, page = 1) + ggsave("egg_2_1.png", device = "png", path = figures_path, width = 6.5, height = 7, units = "in", dpi = 300)
a + facet_wrap_paginate(vars(year), ncol = 5, nrow = 4, page = 2)+ ggsave("egg_2_2.png", device = "png", path = figures_path, width = 6.5, height = 7, units = "in", dpi = 300)
a <- sGSL +  geom_point(data = mackerel_ichthyo %>% dplyr::filter(pass_3 == 1, pass_2 == 1), aes(lon,lat, size = `Scomber scombrus (oeuf stade 3)`),shape = 21, colour = "black",  fill = "red", alpha = 0.8) + labs(y = "Latitude", x = "Longitude") + theme_minimal() + scale_color_viridis_c() + scale_size_area(max_size = 6) + theme(legend.position = "top")
a + facet_wrap_paginate(vars(year), ncol = 5, nrow = 4, page = 1) + ggsave("egg_3_1.png", device = "png", path = figures_path, width = 6.5, height = 7, units = "in", dpi = 300)
a + facet_wrap_paginate(vars(year), ncol = 5, nrow = 4, page = 2)+ ggsave("egg_3_2.png", device = "png", path = figures_path, width = 6.5, height = 7, units = "in", dpi = 300)
a <- sGSL +  geom_point(data = mackerel_ichthyo %>% dplyr::filter(pass_3 == 1, pass_2 == 1), aes(lon,lat, size = `Scomber scombrus (oeuf stade 4)`),shape = 21, colour = "black",  fill = "red", alpha = 0.8) + labs(y = "Latitude", x = "Longitude") + theme_minimal() + scale_color_viridis_c() + scale_size_area(max_size = 6) + theme(legend.position = "top")
a + facet_wrap_paginate(vars(year), ncol = 5, nrow = 4, page = 1) + ggsave("egg_4_1.png", device = "png", path = figures_path, width = 6.5, height = 7, units = "in", dpi = 300)
a + facet_wrap_paginate(vars(year), ncol = 5, nrow = 4, page = 2)+ ggsave("egg_4_2.png", device = "png", path = figures_path, width = 6.5, height = 7, units = "in", dpi = 300)
a <- sGSL +  geom_point(data = mackerel_ichthyo %>% dplyr::filter(pass_3 == 1, pass_2 == 1), aes(lon,lat, size = `Scomber scombrus (oeuf stade 5)`),shape = 21, colour = "black",  fill = "red", alpha = 0.8) + labs(y = "Latitude", x = "Longitude") + theme_minimal() + scale_color_viridis_c() + scale_size_area(max_size = 6) + theme(legend.position = "top") 
a + facet_wrap_paginate(vars(year), ncol = 5, nrow = 4, page = 1) + ggsave("egg_5_1.png", device = "png", path = figures_path, width = 6.5, height = 7, units = "in", dpi = 300)
a + facet_wrap_paginate(vars(year), ncol = 5, nrow = 4, page = 2)+ ggsave("egg_5_2.png", device = "png", path = figures_path, width = 6.5, height = 7, units = "in", dpi = 300)
a <- sGSL +  geom_point(data = mackerel_ichthyo %>% dplyr::filter(pass_3 == 1, pass_2 == 1), aes(lon,lat, size = `Scomber scombrus (larva)`),shape = 21, colour = "black",  fill = "red", alpha = 0.8) + labs(y = "Latitude", x = "Longitude") + theme_minimal() + scale_color_viridis_c() + scale_size_area(max_size = 6) + theme(legend.position = "top")
a + facet_wrap_paginate(vars(year), ncol = 5, nrow = 4, page = 1) + ggsave("larvae_1.png", device = "png", path = figures_path, width = 6.5, height = 7, units = "in", dpi = 300)
a + facet_wrap_paginate(vars(year), ncol = 5, nrow = 4, page = 2)+ ggsave("larvae_2.png", device = "png", path = figures_path, width = 6.5, height = 7, units = "in", dpi = 300)

# by year for established set
sGSL +  geom_point(data = mackerel_ichthyo %>% dplyr::filter(pass_3 ==1), aes(lon,lat, fill = use_2),shape = 21) + scale_fill_viridis_d() + facet_wrap(vars(year))

# Temperature datasets differ
a <- temp_df %>% 
    ggplot(aes(year, temperature)) + 
    stat_summary(fun.data = "mean_cl_boot") +
    labs(title = "DAISS data", x = "year", y = "mean temperature 0-10 m") + xlim(1979,2020) 
b <- temp_df %>% 
    ggplot(aes(year, temp_10m)) + 
    stat_summary(fun.data = "mean_cl_boot", colour = "red") +
    labs(title = "ResDoc 2014", x = "Year", y = "") + xlim(1979,2020)
    
plot_grid(a,b)
temp_df %>% group_by(year) %>% 
    dplyr::summarise(temp_resdoc_2014 = mean(temp_resdoc_2014, na.rm = T), temp_daiss = mean(temp_daiss, na.rm = T)) %>% 
    ggplot(aes(year, temp_resdoc_2014)) + geom_point(shape = 3, size = 4, alpha = 0.6) +
    geom_point(aes(year+0.2,temp_daiss,), colour = "red", size = 4, alpha = 0.6) + ylab("temperature")+ xlab("year")+annotate(geom = "text", x = 1990, y = 13, label = "+ = resdoc, o = daiss", size = 8)

# Trends
mackerel_ichthyo %>% dplyr::filter(pass_3 != "extra", df == "resdoc_2014" | year >2013) %>% 
    ggplot(aes(year, N_m2, colour = pass_3)) + stat_summary(fun.data = "mean_cl_boot", size = 1.2, alpha = 0.8) +
    theme_minimal(base_size = 14) + labs(x = "Year", y = "Daily egg production N/m^2") +
    scale_color_viridis_d(end = 0.9) + 
    ggsave("dep.png", device = "png", path = figures_path, width = 6.5, height = 7, units = "in", dpi = 300)

mackerel_ichthyo %>% dplyr::filter(pass_3 != "extra", df == "resdoc_2014" | year >2013) %>% 
    ggplot(aes(year, dep, colour = stratum)) + stat_summary(fun.data = "mean_cl_boot", size = 1.2, alpha = 0.8) +
    theme_minimal(base_size = 14) + labs(x = "Year", y = "Daily egg production N/m^2") +
    scale_color_viridis_d(end = 0.9) + 
    ggsave("dep_stratum.png", device = "png", path = figures_path, width = 6.5, height = 7, units = "in", dpi = 300)

# stratum map
    sGSL + geom_point(aes(lon, lat, fill = stratum), size = 6, shape = 21, data = mackerel_ichthyo %>% dplyr::filter(pass_3 != "extra", df == "resdoc_2014" | year >2013)) + scale_size_area() +
    theme_minimal(base_size = 14) + labs(x = "Longitude", y = "Latitude") +
    scale_fill_viridis_d(end = 0.9, option = "B") + theme(legend.position = "top") +
        ggsave("dep_stratum_map.png", device = "png", path = figures_path, width = 6.5, height = 7, units = "in", dpi = 300)   

    # station map
    sGSL + geom_point(aes(lon,lat),fill = "white", colour ="black", shape = 21, size = 10, data = mackerel_ichthyo %>% dplyr::filter(pass_3 != "extra", df == "resdoc_2014" | year == 2019)) +
        geom_text(aes(lon, lat, label = station), size = 4, colour = "black", data = mackerel_ichthyo %>% dplyr::filter(pass_3 != "extra", df == "resdoc_2014" | year == 2019)) +
        theme_minimal(base_size = 14) + labs(x = "Longitude", y = "Latitude") +
        scale_fill_viridis_d(end = 0.9, option = "B") + theme(legend.position = "top") +
        ggsave("station_map.png", device = "png", path = figures_path, width = 6.5, height = 7, units = "in", dpi = 300)   
    
    


mackerel_ichthyo %>% ggplot(aes(year, N_m2, colour = pass_3)) + stat_summary(fun.data = "mean_cl_boot", position = position_dodge(preserve = "total"))

# annual proportion of dep by temperature and ecdf
temp <- mackerel_ichthyo %>%
    group_by(year, round(temp)) %>%
    summarise(n = sum(N_m2,na.rm=T)) %>%
    group_by(year) %>% 
    mutate(freq = n / sum(n), temp = `round(temp)`) %>% dplyr::filter(!is.na(temp),!is.na(freq)) %>% as_tibble()



mackerel_ichthyo %>% ggplot(aes(temp,dep))+geom_point() + facet_wrap(vars(year),scales = "free_y")+ xlab("temperature") + ylab("daily egg production")+
    labs(title = "Relationship between temperature and daily egg production", subtitle = "note that y axis scale varies")

mackerel_ichthyo %>% ggplot(aes(temp)) + stat_ecdf() + 
    geom_point(aes(temp, freq),data = temp) +
    facet_wrap(vars(round(year))) + xlab("temperature") + ylab("cumulative distribution (ECDF)") +
    labs(title = "Relationship between temperature and daily egg production", subtitle = "points = % annual dep, line = ecdf of temperature")

# temp depth and dep
mackerel_ichthyo %>% dplyr::filter(pass_3 == 1, depth < 75) %>% droplevels() %>% ggplot(aes(temp_10m, -depth, size = dep, colour = volume)) + geom_point(alpha = 0.6) + facet_wrap(vars(year))+scale_color_viridis_c() +scale_size_binned_area(max_size = 6)


# mean, sd, and CV
summary_df <- dat %>%
    dplyr::filter(pass_2 == 1, pass_3 == 1) %>%
    droplevels() %>% 
    group_by(year, df) %>% 
    dplyr::summarise(mean_nm2 = mean(N_m2, na.rm = T),
                     boot_lower_ci = Hmisc::smean.cl.boot(N_m2)[2],
                     boot_upper_ci = Hmisc::smean.cl.boot(N_m2)[3],
                     sd_nm2 = sd(N_m2, na.rm = T),
                     var_nm2 = var(N_m2, na.rm = T),
                     cv_nm2 = sd_nm2/mean_nm2,
                     n = n(),
                     cv_nm2_unbiased = cv_nm2 * (1 + (1/4*n))) 
    
egg_survey_stats$df <- "published mean from resdoc 2014"
egg_survey_stats$mean_nm2 <- egg_survey_stats$mean_egg_density_nm2
summary_df <- bind_rows(summary_df, egg_survey_stats)
summary_df %>% 
    ggplot(aes(year, mean_nm2, colour = df)) + geom_line() +
    geom_point(aes(year, mean_daily_egg_production), colour = "black") +
    geom_point(aes(year, krigged_mean_dep), colour = "orange")

# which stations are different
dat %>% dplyr::filter(pass_3 ==1, pass_2 == 1, year < 1991, use == 1) %>% 
    ggplot(aes(station, N_m2, colour = df, shape = df)) +
    geom_point(alpha = 0.6, size = 3) + 
    theme_minimal() +
    scale_color_viridis_d(end = 0.8) +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
    facet_wrap(vars(year), scales = "free_y")

dat %>% dplyr::filter(pass_3 ==1, pass_2 == 1, between(year, 1992,2004), use == 1) %>% 
    ggplot(aes(station, N_m2, colour = df, shape = df)) +
    geom_point(alpha = 0.6, size = 3) + 
    scale_color_viridis_d(end = 0.8) +
    theme_minimal()+
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
    facet_wrap(vars(year), scales = "free_y")

dat %>% dplyr::filter(pass_3 ==1, pass_2 == 1, between(year, 2005,2013), use == 1) %>% 
    ggplot(aes(station, N_m2, colour = df, shape = df)) +
    geom_point(alpha = 0.6, size = 3) + 
    scale_color_viridis_d(end = 0.8) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
    facet_wrap(vars(year), scales = "free_y")


# layout of different passes
p1 <- canada_atlantic + geom_point(aes(lon,lat, colour = pass_3), data = mackerel_ichthyo) + 
    scale_colour_viridis_d(option = "D", end = 0.8, name = "Stations", labels = c("Pass 1", "Pass 2", "Extra")) + facet_wrap(vars(pass_3), ncol = 2) +
    theme_minimal(base_size = 12) +
    theme(legend.position = c(0.75,0.25)) +
    labs(title = "Southern Gulf of St. Lawrence Spring Ichthyoplankton Survey", x = "Longitude", y = "Latitude")

sGSL + geom_point(aes(lon,lat,  size = N_m2), shape = 21, fill = presence, alpha = 0.9, data = mack %>% dplyr::filter(pass_3 == 1)) +
    scale_size_binned_area() +
    facet_wrap(vars(year)) +
    labs(title = "Southern GSL mackerel egg survey - first pass", x = "Longitude", y = "Latitude", subtitle = "1982, 1999, 2001, and 2006 not part of official set")


library(plotly)

map_pass_1 <- sGSL + geom_point(aes(lon,lat, colour = dep),alpha = 0.9, data = mackerel_ichthyo %>% dplyr::filter(pass_3 == 1)) + 
    scale_size_binned_area() +
    scale_color_binned(type = "viridis") +
    facet_wrap(vars(year)) +
    labs(title = "Southern GSL mackerel egg survey - first pass", x = "Longitude", y = "Latitude", subtitle = "1982, 1999, 2001, and 2006 not part of official set")
    
map_pass_2 <- sGSL + geom_point(aes(lon,lat, colour = dep),alpha = 0.9, data = mackerel_ichthyo %>% dplyr::filter(pass_3 == 2)) + 
                           scale_size_binned_area() +
                           scale_color_binned(type = "viridis") +
                           facet_wrap(vars(year)) +
                           labs(title = "Southern GSL mackerel egg survey - second pass", x = "Longitude", y = "Latitude", subtitle = "1982, 1999, 2001, and 2006 not part of official set")

# Area
A <- 6.945e+10
A_2 <- 8.21e+10

# Total egg production (simplest form - not official version) - gives abundance estimate in numbers of eggs
eggs <- mackerel_ichthyo %>% 
    dplyr::filter(pass_3 == 1) %>% 
    group_by(year) %>% 
    dplyr::summarise(N = mean(dep,na.rm = T) * A)


#####################  CONSTRUCT VARIOGRAMS (if data already prepared, start here) ####################

# Guide here: https://steveviss.github.io/PR-GSFE01/sp_vario/index.html#1

load(file = "Rdata/mackerel_ichthyo.Rdata")

# function to calculate krigged mean and the correct variance
source("Rscripts/survey/moy.var.Krigeage.R")

# Interpolation grids and shape files
sgsl_grid_pt_classic <- readOGR("data/spatial/sgsl_grid_classic_32198.shp") # classic interpolation grid without northumberland
sgsl_grid_pt_full <- readOGR("data/spatial/sgsl_grid_full_32198.shp") # expanded grid with buffer in the north and includes northumberland strait
# sgsl_grid_pt_full_dense <- readOGR("data/spatial/sgsl_grid_full_dense_32198.shp") # heavy
sgsl_mask <- readOGR("data/spatial/sgsl_mask.shp") # to plot land
sgsl_poly_classic <- readOGR("data/spatial/sgsl_grid_poly_no_northumberland_32198.shp") # without northumberland 110260116404.98943 m2 = 110260 km^2, EPSG = 32198
sgsl_poly_full <- readOGR("data/spatial/sgsl_grid_poly_w_northumberland_32198.shp") # includes northumberland strait. EPSG = 32198. units = m^2. area = 83298936818.23183 m^2 or ~ 83299 km^2

# Subset egg survey data for simplicity
df1 <- mackerel_ichthyo %>% dplyr::filter(df == "resdoc_2014")
df2 <- mackerel_ichthyo %>% dplyr::filter(year > 2013, pass_3 == 1)
egg_survey_sp <- bind_rows(df1,df2)

egg_survey_sp <- egg_survey_sp[,c('year','station','stratum','lon','lat','dep','temp')]
egg_survey_sp_2015 <- egg_survey_sp %>%  dplyr::filter(year == 2015)
egg_survey_sp_2016 <- egg_survey_sp %>%  dplyr::filter(year == 2016)
egg_survey_sp_2017 <- egg_survey_sp %>%  dplyr::filter(year == 2017)
egg_survey_sp_2018 <- egg_survey_sp %>%  dplyr::filter(year == 2018)
egg_survey_sp_2019 <- egg_survey_sp %>%  dplyr::filter(year == 2019)

# Original data in WGS84 so have to assign it that and then change it to QC lambert
# 2015
xy <- subset(egg_survey_sp_2015, select = c("lon", "lat")) # extract xy coordinates. lon first column, lat second column.
names(xy) <- c("lon", "lat") # this is just in case longitude and latitude had a different name. The next line of code requires the names to be lon lat.
egg_survey_sp_2015 <- SpatialPointsDataFrame(coords = xy, data = egg_survey_sp_2015, proj4string = CRS("+init=epsg:4326") )
rm(xy)
egg_survey_sp_2015 <- spTransform(egg_survey_sp_2015, CRS = CRS("+init=epsg:32198")) # this is in NAD 83 Quebec Lambert. Units = meters.

#2016
xy <- subset(egg_survey_sp_2016, select = c("lon", "lat")) # extract xy coordinates. lon first column, lat second column.
names(xy) <- c("lon", "lat") # this is just in case longitude and latitude had a different name. The next line of code requires the names to be lon lat.
egg_survey_sp_2016 <- SpatialPointsDataFrame(coords = xy, data = egg_survey_sp_2016, proj4string = CRS("+init=epsg:4326") )
rm(xy)
egg_survey_sp_2016 <- spTransform(egg_survey_sp_2016, CRS = CRS("+init=epsg:32198"))

# 2017
xy <- subset(egg_survey_sp_2017, select = c("lon", "lat")) # extract xy coordinates. lon first column, lat second column.
names(xy) <- c("lon", "lat") # this is just in case longitude and latitude had a different name. The next line of code requires the names to be lon lat.
egg_survey_sp_2017 <- SpatialPointsDataFrame(coords = xy, data = egg_survey_sp_2017, proj4string = CRS("+init=epsg:4326") )
rm(xy)
egg_survey_sp_2017 <- spTransform(egg_survey_sp_2015, CRS = CRS("+init=epsg:32198"))

#2018
xy <- subset(egg_survey_sp_2018, select = c("lon", "lat")) # extract xy coordinates. lon first column, lat second column.
names(xy) <- c("lon", "lat") # this is just in case longitude and latitude had a different name. The next line of code requires the names to be lon lat.
egg_survey_sp_2018 <- SpatialPointsDataFrame(coords = xy, data = egg_survey_sp_2018, proj4string = CRS("+init=epsg:4326") )
rm(xy)
egg_survey_sp_2018 <- spTransform(egg_survey_sp_2018, CRS = CRS("+init=epsg:32198"))

# 2019
xy <- subset(egg_survey_sp_2019, select = c("lon", "lat")) # extract xy coordinates. lon first column, lat second column.
names(xy) <- c("lon", "lat") # this is just in case longitude and latitude had a different name. The next line of code requires the names to be lon lat.
egg_survey_sp_2019 <- SpatialPointsDataFrame(coords = xy, data = egg_survey_sp_2019, proj4string = CRS("+init=epsg:4326") )
rm(xy)
egg_survey_sp_2019 <- spTransform(egg_survey_sp_2019, CRS = CRS("+init=epsg:32198"))

# check projections
plot(sgsl_grid_pt_classic)
plot(egg_survey_sp_2015, add = T, col = "red", size = 5)

# Build empirical variogram (draw cloud variogram)
# creates gstatVariogram object where np is the number of points in the lag (bin); dist is the mean distance between points in the lag; and gamma is the mean for the lag

vario_ini_2015 <- variogram(dep ~ 1, data = egg_survey_sp_2015)
vario_ini_2016 <- variogram(dep ~ 1, data = egg_survey_sp_2016)
vario_ini_2017 <- variogram(dep ~ 1, data = egg_survey_sp_2017)
vario_ini_2018 <- variogram(dep ~ 1, data = egg_survey_sp_2018)
vario_ini_2019 <- variogram(dep ~ 1, data = egg_survey_sp_2019)
# Visualise variograms 
plot(vario_ini_2015,cex = 2, pch = 19) # some outliers in each
plot(vario_ini_2016,cex = 2, pch = 19)
plot(vario_ini_2017,cex = 2, pch = 19)
plot(vario_ini_2018,cex = 2, pch = 19)
plot(vario_ini_2019,cex = 2, pch = 19)

# Remove outliers to create variogram (variance from outliers added back subsequently after fitting model)
vario_ini_2015 <- variogram(dep ~ 1, data = egg_survey_sp_2015[egg_survey_sp_2015$dep < quantile(egg_survey_sp_2015$dep, .9), ])
vario_ini_2016 <- variogram(dep ~ 1, data = egg_survey_sp_2016[egg_survey_sp_2016$dep < quantile(egg_survey_sp_2016$dep, .9), ])
vario_ini_2017 <- variogram(dep ~ 1, data = egg_survey_sp_2017[egg_survey_sp_2017$dep < quantile(egg_survey_sp_2017$dep, .9), ])
vario_ini_2018 <- variogram(dep ~ 1, data = egg_survey_sp_2018[egg_survey_sp_2018$dep < quantile(egg_survey_sp_2018$dep, .9), ])
vario_ini_2019 <- variogram(dep ~ 1, data = egg_survey_sp_2019[egg_survey_sp_2019$dep < quantile(egg_survey_sp_2019$dep, .9), ])

# Visualise variograms 
 # outlier above 2000, try stronger quantile threshold above
plot(vario_ini_2015,cex = 2, pch = 19) # some outliers in each
plot(vario_ini_2016,cex = 2, pch = 19)
plot(vario_ini_2017,cex = 2, pch = 19)
plot(vario_ini_2018,cex = 2, pch = 19)
plot(vario_ini_2019,cex = 2, pch = 19)

# Normalise the variogram by dividing the data used by the variance (i.e. add the variance back in)
vario_ini_2015$gamma <- vario_ini_2015$gamma / var(egg_survey_sp_2015[egg_survey_sp_2015$dep < quantile(egg_survey_sp_2015$dep,.90),]$dep)
vario_ini_2016$gamma <- vario_ini_2016$gamma / var(egg_survey_sp_2016[egg_survey_sp_2016$dep < quantile(egg_survey_sp_2016$dep,.90),]$dep)
vario_ini_2017$gamma <- vario_ini_2017$gamma / var(egg_survey_sp_2017[egg_survey_sp_2017$dep < quantile(egg_survey_sp_2017$dep,.90),]$dep)
vario_ini_2018$gamma <- vario_ini_2018$gamma / var(egg_survey_sp_2018[egg_survey_sp_2018$dep < quantile(egg_survey_sp_2018$dep,.90),]$dep)
vario_ini_2019$gamma <- vario_ini_2019$gamma / var(egg_survey_sp_2019[egg_survey_sp_2019$dep < quantile(egg_survey_sp_2019$dep,.90),]$dep)

# Check for anisotropy - no major signs in any direction except maybe for 2017 at 45 degrees
TheGStat <- gstat(formula = dep ~ 1, data = egg_survey_sp_2019)
TheVariogram = variogram(TheGStat, map = TRUE, cutoff=500000, width=500000/15)
plot(TheVariogram, threshold = 10)
TheVariogram <- variogram(TheGStat, alpha=c(0,45,90,135))
TheModel = vgm(model = 'Sph' , anis = c(0, 0.5))
FittedModel <- fit.variogram(TheVariogram, model = TheModel, fit.method = 6)
plot(TheVariogram, model = FittedModel, as.table = TRUE)

# Set initial variogram parameters by eye 
model_sph_2015 = vgm(psill = 1.2, model = "Exp", range = 100000, nugget = 0.2)
plot(vario_ini_2015, model = model_sph_2015, plot.numbers = T, pch = 19, cex = 2)

model_sph_2016 = vgm(psill = 1.2, model = "Exp", range = 150000, nugget = 0.2)
plot(vario_ini_2016, model = model_sph_2016, plot.numbers = T, pch = 19, cex = 2)

model_sph_2017 = vgm(psill = 1.2, model = "Exp", range = 100000, nugget = 0.2)
plot(vario_ini_2017, model = model_sph_2017, plot.numbers = T, pch = 19, cex = 2)

model_sph_2018 = vgm(psill = 1.2, model = "Exp", range = 100000, nugget = 0.1)
plot(vario_ini_2018, model = model_sph_2018, plot.numbers = T, pch = 19, cex = 2)

model_sph_2019 = vgm(psill = 1.0, model = "Exp", range = 100000, nugget = 0.1)
plot(vario_ini_2019, model = model_sph_2019, plot.numbers = T, pch = 19, cex = 2)

# Note: gstat will pop a warning if the model does not converge but:
# Donald Myers of The University of Arizona writes: ""convergence" is not a relevant term with respect to estimating and modeling a variogram. In order for a function to be a variogram it must be conditionally negative definite. However actually checking to see if a function satisfies this condition is usually not easy and hence in practice one considers only functions that are known to satisfy the condition. In addition the model usually has one or more parameters, e.g. sill, range.
# see 1987, A. Warrick and D.E. Myers, Optimization of Sampling Locations for Variogram Calculations Water Resources Research 23, 496-500
# and 1991, Myers,D.E., On Variogram Estimation in Proceedings of the First Inter. Conf. Stat. Comp., Cesme, Turkey, 30 Mar.-2 April 1987, Vol II, American Sciences Press, 261-281
# 

# Fit variogram from initial values (If no convergence, check tolerence of outlier threshhold. Above it is set to .99 in the quantile function)
model_f_2015 = fit.variogram(vario_ini_2015, model = model_sph_2015,fit.ranges = T, fit.method = 6) 
plot(model_f_2015, model = model_sph_2015, plot.numbers = T, pch = 16, cutoff = 200000, cex = 2)

# tough to fit. methods 6 and 7 for sph and exp don't converge regardless of starting values
model_f_2016 = fit.variogram(vario_ini_2016, model = model_sph_2016,fit.ranges = T, fit.method = 7) 
plot(model_f_2016, model = model_sph_2016, plot.numbers = T, pch = 16, cutoff = 200000)

model_f_2017 = fit.variogram(vario_ini_2017, model=model_sph_2017,fit.ranges = T, fit.method = 6) 
plot(model_f_2017, model = model_sph_2017, plot.numbers=T, pch = 16, cutoff = 200000)

# tough to fit. generally not suuuuuper important, more important taht the model looks good
model_f_2018 = fit.variogram(vario_ini_2018, model=model_sph_2018,fit.ranges = T, fit.method = 6) 
plot(model_f_2018, model = model_sph_2018, plot.numbers=T, pch = 16, cutoff = 200000)

model_f_2019 = fit.variogram(vario_ini_2019, model=model_sph_2019,fit.ranges = T, fit.method = 6) 
plot(model_f_2019, model = model_sph_2015, plot.numbers=T, pch = 16, cutoff = 200000)

# Once the variogram is fit, rescale the semivariogram to the true values by multiplying by the variance of the values (undoing earlier division)
model_f_2015$psill <- model_f_2015$psill * var(egg_survey_sp_2015$dep)
model_f_2016$psill <- model_f_2016$psill * var(egg_survey_sp_2016$dep)
model_f_2017$psill <- model_f_2017$psill * var(egg_survey_sp_2017$dep)
model_f_2018$psill <- model_f_2018$psill * var(egg_survey_sp_2018$dep)
model_f_2019$psill <- model_f_2019$psill * var(egg_survey_sp_2019$dep)

params <- data.frame(year = seq(2015,2019,1), 
                     model = "Exp", 
                     nug = c(0, 149.9, 0, 2236, 0), 
                     psill = c(2530, 280269.9, 2530, 23790, 7762),
                     range = c(82116, 82116, 82116, 837801, 361447))
save(params, file = "./Rdata/krig_params_2015_2019.Rdata")
write.csv(params, file = "./data/survey/krig_params_2015_2019.csv")
#####################  KRIGGING ####################
krig_df <- array(dim = c(length(year),3),dimnames = list(NULL, c('year','Z','VarKrigeage')))
year = 2018
df_sp <- egg_survey_sp_2018
grid <- sgsl_grid_pt_full
model <- model_f_2018

for(i in seq_along(year)){
    df_sp[which(df_sp@data[,"year"] == year[i]),]
    tmp <- moy.var.Krigeage(
        xCoords = coordinates(df_sp), 
        xValues = df_sp$dep, 
        kCoords = coordinates(grid), 
        vario = model)
    krig_df[i,1]=year[i]
    krig_df[i,2]=tmp$Z
    krig_df[i,3]=tmp$VarKrigeage
}
df <- as.data.frame(krig_df)
df2 <- as.data.frame(krig_df)
df3 <- as.data.frame(krig_df)
df4 <- as.data.frame(krig_df)
df5 <- as.data.frame(krig_df)

df <- bind_rows(df,df2)
df <- bind_rows(df,df3)
df <- bind_rows(df,df4)
df <- bind_rows(df,df5)

df$sd = sqrt(df$VarKrigeage)
df$CV = (df$sd/df$Z)*100
df$lower_ci <- -df$sd*1.96
df$upper_ci <- df$sd*1.96

df_krigged_2015_19 <- df
save(df, file = "./Rdata/df_krigged_19.Rdata")
write.csv(df, file = "./data/survey/df_krigged_19.csv")

moy.var.Krigeage(
    xCoords = coordinates(egg_survey_sp_2019), 
    xValues = egg_survey_sp_2019$dep, 
    kCoords = coordinates(grid), 
    vario = model_f_2019)

#  Figures
# Krig plot
krig_plot1 <- ggplot(data = egg_survey_df_2015, aes(x = lon, y = lat))  + 
    stat_summary_2d(data=krigdf,aes(x = lon, y = lat,z=var1.pred),fun = mean, binwidth = c(0.03, 0.03)) +
    geom_polygon(data = canada, aes(x=long, y = lat, group = group), fill = "ghostwhite",color="black")  +
    coord_map(xlim = c(-66.5, -60),  ylim = c(45.5, 49.5),projection = "lambert", parameters = c(46.5 ,48)) +
    theme_bw() + 
    geom_point(data = egg_survey_df, aes(Long,Lat, size = DEP), pch = 21) +  
    scale_fill_viridis_c(option="viridis", begin = 0.1) + 
    labs(x = "Longitude",y = "Latitude", title = "Interpolated Daily Egg Production")


krig_plot1_var <- ggplot(data = krigdf, aes(x = x, y = y))  + 
    stat_summary_2d(data=krigdf,aes(x = long, y = lat,z=var1.var),fun = mean, binwidth = c(0.4, 0.4))+
    geom_polygon(data = canada, aes(x=long, y = lat, group = group), fill = "ghostwhite",color="black")  +
    coord_map(xlim = c(-66.5, -60),  ylim = c(45.5, 49.5),projection = "lambert", parameters = c(46.5 ,48)) + 
    theme_bw() + 
    geom_point(data = egg_survey_df, aes(Long,Lat, size = DEP), pch = 21) +  
    scale_fill_viridis_c(option="viridis") + 
    labs(x = "Longitude",y = "Latitude", title = "Daily Egg Production Variance", tag = "B") 
krig_compare<-grid.arrange(krig_plot2, krig_plot2_var)




# figures temperature etc



sGSL + 
    geom_point(aes(lon,lat, fill = temperature,), size = 4, shape = 21, alpha = 0.9, data = temperature_data %>% dplyr::filter(year >2015)) +
    scale_size_binned_area() +
    scale_fill_viridis_c(option = "B") +
    facet_wrap(vars(year)) +
    labs(x = "Longitude", y = "Latitude")


sGSL + 
    geom_point(aes(lon,lat, fill = temperature,), size = 4, shape = 21, alpha = 0.9, data = temperature_data %>% dplyr::filter(year >2015)) +
    scale_size_binned_area() +
    scale_fill_viridis_c(option = "B") +
    facet_wrap(vars(year)) +
    labs(x = "Longitude", y = "Latitude")


mackerel_ichthyo %<>% mutate(presence = as.factor(ifelse(dep>0,1,0)))


sGSL + 
    geom_point(aes(lon,lat, fill = presence, size = dep),shape = 21, alpha = 0.9, data = mackerel_ichthyo %>% dplyr::filter(year ==2019)) +
    scale_size_binned_area(name = "Daily Egg Production") +
    scale_fill_manual(values =c("grey","red"), name ="Presence/Absence") +
    facet_wrap(vars(year)) +
    labs(x = "Longitude", y = "Latitude") + theme(legend.position = "top")


sGSL + 
    geom_point(aes(lon,lat, size = dep, fill = presence),shape =21,
               data = mackerel_ichthyo %>% dplyr::filter(year >1999)) +
    scale_size_area(name = "Daily Egg Production") +
    scale_fill_manual(values =c("grey","red"), name ="Presence/Absence") +
    facet_wrap(vars(year)) +
    labs(x = "Longitude", y = "Latitude") + theme(legend.position = "top")


library(ggplot2)
ggplot(mtcars, aes(wt, mpg)) + 
    geom_point(aes(colour = cut(qsec, c(-Inf, 17, 19, Inf))),
               size = 5) +
    scale_color_manual(name = "qsec",
                       values = c("(-Inf,17]" = "black",
                                  "(17,19]" = "yellow",
                                  "(19, Inf]" = "red"),
                       labels = c("<= 17", "17 < qsec <= 19", "> 19"))