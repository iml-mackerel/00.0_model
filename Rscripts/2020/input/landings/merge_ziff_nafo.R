#########
##
## Script to merge mackerel ziff and NAFO landings data (see Res Doc 2000/021 chapters 1-3)
##  for input into mackerel stock assessment model CCAM
##  bY ANDREW SMITH, Dec 2020
##
## NAFO (1960-2019) - USA stopped contributing post ~ 1993
## ZIFF (1985-2020) - though data is sketchy prior to 1995 for mackerel as foreign landings not included
## DFO ResDoc 2000/021  - showed inconsistencies between the two data sets and proposed a standard set
## foss_landings are scraped from noaa website but doesn't include discards. from NOAA's website https://foss.nmfs.noaa.gov/apexfoss/f?p=215:200:3789716835843::NO:::   for validation
## ctusa is what Kiersten Curti (NEFSC-NOAA) gave us for the 2019 assessment
## 
## Goal: create csv for 1) total landings in canadian eez (min = canadian + foreign, max = min + bait estimates), 2) total us landings in the us eez, and 3) foreign landings in the us EEZ
##       format for CCAM
#########

#####################  LOAD PACKAGES  ####################
packages = c('esquisse', 'magrittr', 'lubridate', 'readr', 'tidyverse')
invisible(lapply(packages, function(x) {
    if (!require(x, character.only = T)) {
        install.packages(x)
        require(x)
    }
}))

#####################  LOAD DATA  ####################
# ziff data. Run ziff_exploreR.R, then clean_mackerel_ziff.R to obtain this
load("Rdata/ziff.Rdata") 

# NAFO data. Run clean_mackerel_nafo.R to obtain these
load("Rdata/nafo_can_eez_c.Rdata") 
load("Rdata/nafo_can_eez_f.Rdata")
load("Rdata/nafo_usa_eez_u.Rdata")
load("Rdata/nafo_usa_eez_f.Rdata") 

# Canadian EEZ landings from Francois Grégoire's 2000/021 resdoc data for validation
res_2000 <- read_csv("csv/res_2000_021_ct.csv") 
res_2000 %<>% dplyr::select(year, total) 

# US landings from NOAA/NEFSC (Commercial, recreational, and discards in US EEZ) - Caution, foreign landings not split by EEZ. use NAFO or request split from NOAA
ctusa <- read_csv("csv/ctusa.csv") # from Kiersten Curti in 2018 
ctusa %<>% transmute(year = X1, us_com = US.Commercial, us_rec = US.Recreational, us_disc = US.Comm.discards, total_us = US.Total.Catch)

foss_landings <- read_csv("csv/foss_landings.csv")
foss_landings %<>% mutate(us_landings_t = Pounds/2204.6) %>%
transmute(year = Year, us_landings_t = us_landings_t)

#####################  LOOK AT DATA  ####################
glimpse(ziff)
glimpse(nafo_can_eez_c)
glimpse(res_2000)
glimpse(ctusa)

#####################  SUMMARIZE, MERGE, AND COMPARE DATA  ####################
nafo_can_eez_c %<>% group_by(year) %>% dplyr::summarise(nafo_can_eez_c = sum(landings_t)) 
nafo_can_eez_f %<>% group_by(year) %>% dplyr::summarise(nafo_can_eez_f = sum(landings_t)) 
nafo_can_eez <- full_join(nafo_can_eez_c, nafo_can_eez_f)
nafo_can_eez$nafo_can_eez_f <- replace_na(nafo_can_eez$nafo_can_eez_f, 0)
nafo_can_eez %<>% mutate(nafo_can_eez_total = nafo_can_eez_c + nafo_can_eez_f)

ziff %<>% dplyr::select(year, total_can_ziff)
can_eez <- full_join(nafo_can_eez, ziff)
can_eez$total_can_ziff <- replace_na(can_eez$total_can_ziff, 0)
can_eez$nafo_can_eez_total <- replace_na(can_eez$nafo_can_eez_total, 0)
can_eez$nafo_can_eez_f <- replace_na(can_eez$nafo_can_eez_f, 0)
can_eez %<>% mutate(total_can_eez = ifelse(between(year,1960,1994), nafo_can_eez_total, total_can_ziff + nafo_can_eez_f))

# see if match with FG Res 2000/021 
can_eez <- left_join(can_eez,res_2000)
can_eez$total <- replace_na(can_eez$total, 0)

# Choose whichever larger - Details in ResDoc 2000/021 - includes at sea sales etc.
can_eez %<>% 
    mutate(min = ifelse(total > total_can_eez, total, total_can_eez))

# Format for CCAM
# Add bait estimates as per supp material in Van Beveren et al., 2017a
# NL = 0
# NS = 1000 (crab), 3290 pre 1990, 6580 post 1990 for lobster
# Quebec = 2130 for both crab and lobster
# sGSL = 190 for crab, 900 lobster pre 1990, and 1800 post 1990
# so for can_eez$max, add 3320 to all years, 6320 to years pre 1990, and 5380 >= 1990

can_eez  %<>%  
    mutate(max = min + 3320, 
           max = ifelse(year < 1995, max + 6320, max + 5380))

can_eez %<>% dplyr::select(min, max) %>% as.matrix() 
rownames(can_eez) <- 1960:2020

# save canadian data
save(can_eez, file = "Rdata/can_eez.Rdata")
write.csv(can_eez, file = "csv/can_eez.csv")

# Check nafo usa vs noaa  (what Kiersten sent)
nafo_usa_eez_u <- nafo_usa_eez_u %>% group_by(year) %>% dplyr::summarise(nafo_usa_eez_u = sum(landings_t))
nafo_usa_eez_f <- nafo_usa_eez_f %>% group_by(year) %>% dplyr::summarise(nafo_usa_eez_f = sum(landings_t))
nafo_usa_eez_f %<>% transmute(year = year, nafo_usa_eez_f = nafo_usa_eez_f)
df <- data.frame(year = 1960:2020)
nafo_usa_eez_u <- left_join(df, nafo_usa_eez_u)
nafo_usa_eez_f <- left_join(df, nafo_usa_eez_f)
nafo_usa_eez_u$nafo_usa_eez_u <- replace_na(nafo_usa_eez_u$nafo_usa_eez_u, 0)
nafo_usa_eez_f$nafo_usa_eez_f <- replace_na(nafo_usa_eez_f$nafo_usa_eez_f, 0) # notice that foreign values in recent years are actually from canadian catches in subarea 5 skirting the border


ctusa %<>% dplyr::select(year, total_us)
us_eez_u <- full_join(df, ctusa)

rownames(us_eez_u) <- 1960:2020
us_eez_u <- us_eez_u %>% dplyr::select(total_us) %>% as.matrix()
us_eez_f <- nafo_usa_eez_f

us_eez_f <- us_eez_f %>% dplyr::select(nafo_usa_eez_f)
rownames(us_eez_f) <- 1960:2020

# add missing catch data for 2018:2020 based on noaa website values and expected 2020 catch
us_eez_u[59,1] <- 10784.3
us_eez_u[60,1] <- 6804.7
us_eez_u[61,1] <- 19184

# save
save(us_eez_u, file = "Rdata/us_eez_u.Rdata")  # us catch in us eez
save(us_eez_f, file = "Rdata/us_eez_f.Rdata")  # foreign catch in us eez

write.csv(us_eez_u, file = "csv/us_eez_u.csv")
write.csv(us_eez_f, file = "csv/us_eez_f.csv")
