
####################  Mackerel CARBIO and Length Frequencies  ####################
##
##  Mackerel "caracteristiques biologiques" aka car_bio and length frequencies
##
##  bY ANDREW SMITH, July 2019
##
##  Extracted from SAS program autoexecpeche found in "S:/SAS/Peche/autoexec_peche_64bits.sas"
##
##  Access SAS 9.4 through Citrix connection: https://citrix.dfo-mpo.gc.ca/vpn/index.html username = SmithAND , password = t******!!
##  Choose species, carbio option, f8 to run the script, save the .dat file in I:/ and then copy to new location
##  Same information from the entetes 4, 5 .dat file but easier to parse
##
####################  LOAD LIBRARIES  #################### 
packages = c('tidyverse','magrittr','measurements','forcats','lubridate', "ggthemes", "GGally", "gridExtra", "FSA", "broom", "ggeffects","performance")
invisible(lapply(packages, function(x) {if (!require(x, character.only = T)) {install.packages(x);require(x)}}))

####################  READ RAW CARBIO DATA  #################### 

# Transfer the '.dat' files from I:// drive to here
# data_path = "S:/Pélagiques/Echantillions_Commerciales/Scomber_scombrus_EchComm/Maquereau_carbio.dat"
data_path = "C:/Users/SmithAND/Documents/My Stocks/Mackerel/mackerel_sar_2021/mackerel_model_2021/mackerel_assessment_input/data/bio/Maquereau_carbio.dat"
# define column widths ... this is different for each species. Sylvain H. has the column width keys
carbio_cols <- fwf_cols(
  ZONE_NUM = c(1, 3),
  TRIP_NUM = c(4, 6),
  YEAR = c(7,10),
  MONTH = c(11, 12),
  DAY = c(13, 14),
  TYPE = c(23, 24),
  GEAR_CODE = c(25, 28),
  NUM_SAMPLED = c(29, 32),
  LENGTH = c(33, 35),
  WEIGHT = c(36, 41),
  SEX = c(42),
  MATURITY = c(43),
  WEIGHT_GONAD = c(44, 49),
  AGE = c(53, 54),
  S_ECH = c(55, 60),
  S_CBIOL = c(61, 66),
  S_RCBIOI = c(68, 73),
  AGE_1 = c(75, 76),
  DIVISION = c(78, 81),
  SUBAREA = c(83, 84))

# parse
mackerel_carbio <- read_fwf(data_path, carbio_cols) %>% arrange(YEAR)
glimpse(mackerel_carbio) # check

# make life easier
names(mackerel_carbio) <- tolower(names(mackerel_carbio))

# change variable class
mackerel_carbio$day <- as.numeric(mackerel_carbio$day)
mackerel_carbio$month <- as.numeric(mackerel_carbio$month)
rm(carbio_cols)

## rename variables
mackerel_carbio <-
  mackerel_carbio %>% rename(
    year = year  ,
    month = month  ,
    day = day  ,
    division = subarea , 
    subdivision = division ,
    type = type  ,
    zone_num = zone_num  ,
    trip_num = trip_num  ,
    gear_code = gear_code  ,
    fish_id = num_sampled  ,
    length = length  ,
    mass = weight  ,
    sex = sex  ,
    maturity = maturity  ,
    mass_gonad = weight_gonad  ,
    age =  age  ,
    age_1 = age_1)

####################  READ RAW LF DATA  #################### 

# Stuck with this format for now. Request change to something sensible from DAISS
# ID where file was saved
# The following code was written by Mathieu D. and Thomas D-V and I have not been able to improve it despite many attempts (parsing problems for flat files)
dirInput <- "C:/Users/SmithAND/Documents/My Stocks/Mackerel/mackerel_sar_2021/mackerel_model_2021/mackerel_assessment_input/data/bio/"
fileName <- paste0(dirInput,'sortie.dat')
con <- file(fileName,open="r")
ech.init <- readLines(con)
#lireEchantillonneur <- function(dirInput='input/', dirOutput='output/'){
## le fichier sortie.dat est produit par programme SAS nommé 'peche'

## créer objet ech4, et merger pour obtenir ech5 incluant ech4
index <- 1
for(i in 2:length(ech.init)){
  if(substring(ech.init[i], 1, 1) == '4'){
    index <- c(index,i)
  }   #tous les 5 demeurent des NA
}
index <- c(index,length(ech.init)+1)
ech4 <- array(dim=c(length(index),25), dimnames=list(NULL, c('ech','dist','jr','mois','an','nafo','latDeg','latMin','latSec',
                                                             'longDeg','longMin','longSec','exper','nbpc','engin','mail','prof','etatDeb','pdsEsp',
                                                             'pdsEch','sexe','tMes','nbEng','zonNum','noVoy')))
ech4 <- as.data.frame(ech4)
positionDeb4 <- c(6,9,12,14,16,20,24,26,28,30,31,34,36,38,44,48,52,55,57,63,78,80,81,84,87)
positionFin4 <- c(8,11,13,15,19,23,25,27,29,31,33,35,37,43,47,51,54,56,62,68,78,80,83,86,89)
ech <- NULL
positionDeb5 <- c(2,5,8,11,15)
positionFin5 <- c(4,7,10,13,22)
for(i in head(seq_along(index),-1)){
  for(j in 1:25){
    temp4 <- substring(ech.init[index[i]], positionDeb4[j], positionFin4[j])
    if(j %in% c(6,15,21)){
      ech4[i,j] <- temp4
    }else{
      ech4[i,j] <- as.numeric(temp4)
    }
  }
  temp5 <- array(dim=c(index[i+1]-index[i]-1,5), dimnames=list(NULL, c('dist','ech','long','nmes','sFreq')))
  index2 <- (index[i]+1):(index[i+1]-1)
  for(j in seq_along(index2)){
    for(k in 1:5){
      temp5[j,k] <- as.numeric(substring(ech.init[index2[j]], positionDeb5[k], positionFin5[k]))
    }
  }
  ech <- rbind(ech, merge(temp5, ech4[i,], all=TRUE))
}

ech$opanoDiv <- str_extract(ech$nafo, "^.{2}")
ech$S_DIST_ECG <-str_c(ech$dist, ech$ech, sep = "_")
#save(ech, file=paste0(dirInput,'lectureEchantillonneur.RData'))
#}

# rename
mackerel_lf <- ech
rm(ech);rm(ech4);rm(temp5);rm(con);rm(ech.init);rm(i);rm(fileName);rm(index);rm(index2);rm(j);rm(k);rm(positionDeb4);rm(positionDeb5);rm(positionFin4);rm(positionFin5);rm(temp4)
rm(dirInput);rm(data_path)

names(mackerel_lf)

## rename variables for conveniance
mackerel_lf <-
  mackerel_lf %>% rename(
    district = dist  ,
    sample = ech  ,
    length = long  ,
    freq = nmes  ,
    day = jr  ,
    month = mois  ,
    year = an  ,
    subdivision = nafo  ,
    lat_deg = latDeg ,
    lat_min = latMin ,
    lat_sec = latSec ,
    lon_deg = longDeg ,
    lon_min = longMin ,
    lon_sec = longSec ,
    type = exper,
    cfv = nbpc  ,
    gear_code = engin  ,
    mesh_size = mail  ,
    depth = prof  ,
    landed_state = etatDeb,
    est_mass_landed = pdsEsp  ,
    mass_sampled =  pdsEch  ,
    sex =  sexe  ,
    times_measured = tMes,
    gear_num = nbEng  ,
    zone_num = zonNum  ,
    trip_num =  noVoy  ,
    division = opanoDiv ,
    sample_id = S_DIST_ECG)

# EXPAND AND FILL ROWS OF OBS BASED ON FREQUENCY COUNTS
# eg if line 1 reads freq = 5 then this line is expanded to five lines. Be careful when summarising/aggregating data
mackerel_lf <- mackerel_lf[rep(1:nrow(mackerel_lf), mackerel_lf[["freq"]]), ]
glimpse(mackerel_lf) # check structure

# Coordinates  # there are problems with how the degrees were parsed. results in weird values
library(measurements)

mackerel_lf$lat <- paste(mackerel_lf$lat_deg, mackerel_lf$lat_min, mackerel_lf$lat_sec)
mackerel_lf$lon <- paste("-",mackerel_lf$lon_deg, mackerel_lf$lon_min, mackerel_lf$lon_sec)

# Change "NA NA NA" to NA
mackerel_lf %<>% mutate(lat = na_if(lat, "NA NA NA"), lon = na_if(lon, "NA NA NA"))

# Change from dd.mm.ss to dd.dddd
# doesn't work. find another way
# mackerel_lf %<>%
#   mutate(lat = conv_unit(lat, from = 'deg_min_sec', to = "dec_deg"), 
#          lon = conv_unit(lon, from = 'deg_min_sec', to = "dec_deg"))

# mackerel_lf %<>% mutate(lat = as.numeric(lat), lon = as.numeric(lon))

####################  Merge Carbio and Length Frequency objects  ####################

mackerel_bio <- bind_rows(mackerel_lf, mackerel_carbio) %>% arrange(year, month, day) 

# remove columns with ALL NA
mackerel_bio <- Filter(function(x)!all(is.na(x)), mackerel_bio)

# reorder columns
col_order <- c("year", "month", "day","division","subdivision",
               "type", "sample","sample_id","fish_id","s_ech",
               "s_cbiol","s_rcbioi", "cfv", "gear_code", "mesh_size", 
               "depth", "est_mass_landed", "mass_sampled",
               "lon","lat","freq", "sex", "length", "age","age_1", "mass", "mass_gonad", "maturity")
mackerel_bio <- mackerel_bio[, col_order]
glimpse(mackerel_bio)
rm(mackerel_carbio)
rm(mackerel_lf)

####################  ADD NEW VARIABLES  ####################

# Data source
mackerel_bio %<>% mutate(data_source = ifelse(!is.na(freq), "length_freq", "carbio"))

# Quarter
mackerel_bio <- mackerel_bio %>% mutate(quarter = fct_collapse(
  factor(mackerel_bio$month),
  Q1 = c("1", "2","3"),
  Q2 = c("4", "5","6"),
  Q3 = c("7", "8", "9"),
  Q4 =c("10","11","12")))

# GEAR NAME. Codes found on BD Peches documentation folder 
unique(mackerel_bio$gear_code) # check gear types may change from year to year  
gear_key <-   
  list(
    CPO = "DIVE_HAND_TOOL",
    "CPO " = "DIVE_HAND_TOOL",
    CMA = "DIVE_HAND_TOOL",
    "CMA "  = "DIVE_HAND_TOOL",
    SSC = "SEINE_SCOTTISH",
    "SSC "  = "SEINE_SCOTTISH",
    ST2 = "TRAWL_SHRIMP_REAR_NO_GRID",
    "ST2 " = "TRAWL_SHRIMP_REAR_NO_GRID",
    GND = "GILLNET_DRIFT",
    "GND " = "GILLNET_DRIFT",
    GN = "GILLNET_UNKNOWN",
    "GN  " = "GILLNET_UNKNOWN",
    TXS = "TRAWL_SHRIMP_UNKNOWN",
    "TXS " = "TRAWL_SHRIMP_UNKNOWN",
    GRL = "TRAWL_SHRIMP_UNKNOWN",
    "GRL " = "TRAWL_SHRIMP_UNKNOWN",
    GRL2 = "TRAWL_SHRIMP_REAR_GRID",
    GRL1 = "TRAWL_SHRIMP_SIDE_GRID",
    LHM = "LINE_MECHANICAL",
    GNS = "GILLNET_SET",
    "GNS " = "GILLNET_SET",
    PS = "SEINE_PURSE",
    "PS  " = "SEINE_PURSE",
    FPN = "NET_TRAP",
    "FPN " = "NET_TRAP",
    LHP = "LINE_HAND",
    "LHP " = "LINE_HAND",
    "LHM " = "JIGGER_MECHANIZED",
    LX = "LINE_UNKNOWN",
    NK = "UNKNOWN",
    "NK  " = "UNKNOWN",
    FWR = "WEIRS",
    "FWR " = "WEIRS",
    OTB = "TRAWL_BOTTOM",
    "OTB " = "TRAWL_BOTTOM",
    OTM = "TRAWL_PELAGIC",
    "OTM " = "TRAWL_PELAGIC",
    GN = "GILLNET_UNKNOWN",
    SB = "SEINE_BEACH",
    "SB  " = "SEINE_BEACH",
    SPR = "SEINE_PAIR",
    LA = "NET_LAMPARA",
    "LA  " = "NET_LAMPARA",
    FIX = "TRAP_UNKNOWN",
    "FIX " = "TRAP_UNKNOWN",
    TS = "SEINE_TUCK",
    "TS  " = "SEINE_TUCK",
    MIS = "MISC",
    "MIS " = "MISC",
    SDN = "SEINE_DANISH",
    "SDN " = "SEINE_DANISH",
    LLS = "LONGLINE_ANCHORED",
    "LLS " = "LONGLINE_ANCHORED",
    LX = "LINE_HAND",
    "LX  " = "LINE_HAND",
    SPR = "SEINE_PAIR",
    "SPR " = "SEINE_PAIR")

# Apply key
mackerel_bio <- mackerel_bio %>% mutate(gear_name = recode(gear_code, !!!gear_key))
unique(mackerel_bio$gear_name)

# Simplified and aggregated Gear names by type
mackerel_bio <- mackerel_bio %>% mutate(gear = fct_collapse(
  factor(mackerel_bio$gear_name),
  NETS_TRAPS_WEIRS = c("NET_TRAP", "WEIRS","TRAP_UNKNOWN","NET_LAMPARA"),
  LINES = c("LINE_UNKNOWN", "LINE_HAND","LINE_MECHANICAL","JIGGER_MECHANIZED","LONGLINE_ANCHORED"),
  # SEINERS = c("SEINE_PURSE", "SEINE_TUCK"),
  GILLNETS =c("GILLNET_SET","GILLNET_DRIFT","GILLNET_UNKNOWN"),
  SEINES_OTHER = c("SEINE_BEACH","SEINE_PAIR","SEINE_DANISH"),
  UNKNOWN = c("UNKNOWN","MISC")))

# factor year
mackerel_bio$f_year <- as.factor(mackerel_bio$year)

# date
mackerel_bio$date <- as_date(paste(mackerel_bio$year,mackerel_bio$month,mackerel_bio$day))
mackerel_bio$doy <- yday(mackerel_bio$date)

## ADD COORDINATES OF NAFO SUBDIVISION CENTROIDS
load("Rdata/NAFO_centroids.Rdata")

# it is a mix of NAFO Divisions and subdivisions/unit areas
unique(mackerel_bio$subdivision)
mackerel_bio$subdivision <- trimws(mackerel_bio$subdivision)

# rename variables
NAFO_centroids$subdivision <- NAFO_centroids$UnitArea
NAFO_centroids %<>% transmute(subdivision = subdivision, lon_subdiv = X_Longitude, lat_subdiv = Y_Latitude)

# merge subdivision coordinates
mackerel_bio <- left_join(mackerel_bio, NAFO_centroids, by = "subdivision")

# now same for Divisions
NAFO_centroids$division <- str_sub(NAFO_centroids$subdivision,1,2)
NAFO_centroids %<>% group_by(division) %>% 
  dplyr::summarise(lon_div = mean(lon_subdiv), lat_div = mean(lat_subdiv))

# merge
mackerel_bio <- left_join(mackerel_bio, NAFO_centroids,  by = "division")

# Ages
mackerel_bio$age <- as.factor(mackerel_bio$age)
mackerel_bio <- mackerel_bio %>%  mutate(f_age = fct_collapse(    
  factor(mackerel_bio$age),
  "10" = c("10","11","12","13","14","15","16","17","18","42","63"))) # the last two values (42 and 63) are typos and were validated
 
# This gives missing value an explicit factor level, ensuring that they appear in summaries and on plots.
# mackerel_bio$f_AGE <- forcats::fct_explicit_na(mackerel_bio$f_AGE)

# group some divisions for convenience
mackerel_bio <- mackerel_bio %>% mutate(division_group = fct_collapse(    
  factor(mackerel_bio$division),
  "4WX5YZ" = c("4W","4X","5Y","5Z"),
  "4V3P" = c("4V","3P"),
  "2J3KL" = c("3K","3L","2J"),
  "4RST" = c("4R","4S","4T")))

# turn sample info into factors so that I can find and match them
mackerel_bio %<>% mutate(sample = as.factor(as.character(sample)),
                        sample_id = as.factor(as.character(sample_id)),
                        s_ech = as.factor(as.character(s_ech)),
                        s_cbiol = as.factor(as.character(s_cbiol)),
                        s_rcbioi = as.factor(as.character(s_rcbioi))
                        )


####################  SAVE  ####################
save(mackerel_bio, file = "./Rdata/mackerel_bio.Rdata")
glimpse(mackerel_bio)

