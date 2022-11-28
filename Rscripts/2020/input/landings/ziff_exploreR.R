#######
##  Fisheries and Oceans Canada (DFO) Landings Data (1985-2020)
##
##  Zonal Interchange File Format (ZIFF)
##
##  All Regions, All Species
##
##  Extracted From Regional Statistics Databases (Maritimes, Gulf, Quebec, and Newfoundland)
##  Converted to ZIFF by DAISS
##  Provided by DAISS (contact sylvain.hurtubuise@dfo-mpo.gc.ca) - Retired
##  Contact is now Denis Bernier (denis.bernier@dfo-mpo.gc.ca)
##
##  Accessed from DFO-IML network: "\\dcqcimlna01a\BD_Peches\Ziff\Format CSV\Fichiers de données"
##
##  Goal will be to combine the .csv files into a subset database as the files are too large to work with directly
##  By ANDREW D. SMITH, October 2019
##
##  Change log:
##  - updated/cleaned throughout 2020 but mostly in November
##  - last data update by DAISS on 11/12/2020

#####################  LOAD LIBRARIES  ####################
packages = c('magrittr', 'lubridate', 'readr', 'readxl', 'tidyverse')
invisible(lapply(packages, function(x) {
  if (!require(x, character.only = T)) {
    install.packages(x)
    require(x)
  }
}))

#####################  LOAD DATA  ####################

## Goal will be to do this with a one shot function but an error occurs when using purrr::map for the instant due to columns not matching

Version_totale_19851989 <-
  read_csv(
    "C:/Users/SmithAND/Documents/Data/Ziff/Version_totale_19851989.csv",
    col_types = cols(
      capt_est = col_double(),
      cl_prof = col_double(),
      date_cap = col_date(format = "%Y/%M/%D"),
      latit_GIS = col_double(),
      latit_ori = col_double(),
      longit_GIS = col_double(),
      longit_ori = col_double(),
      pav_eff = col_double(),
      pav_mar = col_double(),
      prof = col_double(),
      quadril = col_double()
    )
  )

# Filter by species of interest using the cod_esp (species code) variable. Use the prespvis variable to filter by species sought either alone or in combination with cod_esp
mack_1 <- Version_totale_19851989 %>% dplyr::filter(cod_esp == 250)
cap_1 <- Version_totale_19851989 %>% dplyr::filter(cod_esp == 360)
rm(Version_totale_19851989)

Version_totale_19901994 <-
  read_csv(
    "C:/Users/SmithAND/Documents/Data/Ziff/Version_totale_19901994.csv",
    col_types = cols(
      capt_est = col_double(),
      cl_prof = col_double(),
      date_cap = col_date(format = "%Y/%M/%D"),
      latit_GIS = col_double(),
      latit_ori = col_double(),
      longit_GIS = col_double(),
      longit_ori = col_double(),
      pav_eff = col_double(),
      pav_mar = col_double(),
      prof = col_double(),
      quadril = col_double()
    )
  )

mack_2 <- Version_totale_19901994 %>%  dplyr::filter(cod_esp == 250)
cap_2 <- Version_totale_19901994 %>%  dplyr::filter(cod_esp == 360)
rm(Version_totale_19901994)

Version_totale_19951999 <-
  read_csv(
    "C:/Users/SmithAND/Documents/Data/Ziff/Version_totale_19951999.csv",
    col_types = cols(
      capt_est = col_double(),
      cl_prof = col_double(),
      date_cap = col_date(format = "%Y/%M/%D"),
      latit_GIS = col_double(),
      latit_ori = col_double(),
      longit_GIS = col_double(),
      longit_ori = col_double(),
      pav_eff = col_double(),
      pav_mar = col_double(),
      prof = col_double(),
      quadril = col_double()
    )
  )

mack_3 <- Version_totale_19951999 %>% dplyr::filter(cod_esp == 250)
cap_3 <- Version_totale_19951999 %>% dplyr::filter(cod_esp == 360)
rm(Version_totale_19951999)

Version_totale_20002004 <-
  read_csv(
    "C:/Users/SmithAND/Documents/Data/Ziff/Version_totale_20002004.csv",
    col_types = cols(
      capt_est = col_double(),
      cl_prof = col_double(),
      date_cap = col_date(format = "%Y/%M/%D"),
      latit_GIS = col_double(),
      latit_ori = col_double(),
      longit_GIS = col_double(),
      longit_ori = col_double(),
      pav_eff = col_double(),
      pav_mar = col_double(),
      prof = col_double(),
      quadril = col_double()
    )
  )

mack_4 <- Version_totale_20002004 %>% dplyr::filter(cod_esp == 250)
cap_4 <- Version_totale_20002004 %>% dplyr::filter(cod_esp == 360)
rm(Version_totale_20002004)

Version_totale_20052009 <-
  read_csv(
    "C:/Users/SmithAND/Documents/Data/Ziff/Version_totale_20052009.csv",
    col_types = cols(
      capt_est = col_double(),
      cl_prof = col_double(),
      date_cap = col_date(format = "%Y/%M/%D"),
      latit_GIS = col_double(),
      latit_ori = col_double(),
      longit_GIS = col_double(),
      longit_ori = col_double(),
      pav_eff = col_double(),
      pav_mar = col_double(),
      prof = col_double(),
      quadril = col_double()
    )
  )

mack_5 <- Version_totale_20052009 %>% dplyr::filter(cod_esp == 250)
cap_5 <- Version_totale_20052009 %>% dplyr::filter(cod_esp == 360)
rm(Version_totale_20052009)

Version_totale_20102014 <-
  read_csv(
    "C:/Users/SmithAND/Documents/Data/Ziff/Version_totale_20102014.csv",
    col_types = cols(
      capt_est = col_double(),
      cl_prof = col_double(),
      date_cap = col_date(format = "%Y/%M/%D"),
      latit_GIS = col_double(),
      latit_ori = col_double(),
      longit_GIS = col_double(),
      longit_ori = col_double(),
      pav_eff = col_double(),
      pav_mar = col_double(),
      prof = col_double(),
      quadril = col_double()
    )
  )



mack_6 <- Version_totale_20102014 %>% dplyr::filter(cod_esp == 250)
cap_6 <- Version_totale_20102014 %>% dplyr::filter(cod_esp == 360)
rm(Version_totale_20102014)

Version_totale_20152019 <-
  read_csv(
    "C:/Users/SmithAND/Documents/Data/Ziff/Version_totale_20152019.csv",
    col_types = cols(
      capt_est = col_double(),
      cl_prof = col_double(),
      date_cap = col_date(format = "%Y/%M/%D"),
      latit_GIS = col_double(),
      latit_ori = col_double(),
      longit_GIS = col_double(),
      longit_ori = col_double(),
      pav_eff = col_double(),
      pav_mar = col_double(),
      prof = col_double(),
      quadril = col_double()
    )
  )

mack_7 <- Version_totale_20152019 %>% dplyr::filter(cod_esp == 250)
cap_7 <- Version_totale_20152019 %>% dplyr::filter(cod_esp == 360)
rm(Version_totale_20152019)

Version_totale_20202024 <-
  read_csv(
    "C:/Users/SmithAND/Documents/Data/Ziff/Version_totale_20202024.csv",
    col_types = cols(
      capt_est = col_double(),
      cl_prof = col_double(),
      date_cap = col_date(format = "%Y/%M/%D"),
      latit_GIS = col_double(),
      latit_ori = col_double(),
      longit_GIS = col_double(),
      longit_ori = col_double(),
      pav_eff = col_double(),
      pav_mar = col_double(),
      prof = col_double(),
      quadril = col_double()
    )
  )

mack_8 <- Version_totale_20202024 %>% dplyr::filter(cod_esp == 250)
cap_8 <- Version_totale_20202024 %>% dplyr::filter(cod_esp == 360)
rm(Version_totale_20202024)

#####################  Merge Data  ####################
mackerel_ziff <-
  bind_rows(mack_1, mack_2, mack_3, mack_4, mack_5, mack_6, mack_7, mack_8)
rm("mack_1",
   "mack_2",
   "mack_3",
   "mack_4",
   "mack_5",
   "mack_6",
   "mack_7",
   "mack_8")

capelin_ziff <-
  bind_rows(cap_1, cap_2, cap_3, cap_4, cap_5, cap_6, cap_7, cap_8)
rm("cap_1",
   "cap_2",
   "cap_3",
   "cap_4",
   "cap_5",
   "cap_6",
   "cap_7",
   "cap_8")

#####################  Add Information  ####################

# Supply more informative names to the variables, recode certain classes, and add supplemental information

## ziff metadata
Documentation_Versiontotale <-
  read_excel(
    "C:/Users/SmithAND/Documents/Data/Ziff/Documentation_Versiontotale.xlsx",
    sheet = "Espèces"
  )

Documentation_Versiontotale %<>% transmute(
  cod_esp = cod_esp,
  Species_en = DA_ESP,
  Species_fr = DF_ESP,
  Species_la = DL_ESP
)

species_targeted <-
  read_csv(
    "C:/Users/SmithAND/Documents/Data/Ziff/species_targeted.csv",
    col_types = cols(Remarques = col_skip())
  )

pr_spec_caught <-
  read_csv(
    "C:/Users/SmithAND/Documents/Data/Ziff/pr_spec_caught.csv",
    col_types = cols(Remarques = col_skip())
  )

engin_codes <-
  read_csv("C:/Users/SmithAND/Documents/Data/Ziff/Codes_engin.csv")

## Merge. Choice to retain original variable names here as opposed to renaming them to help with reproducibility
mackerel_ziff <-
  left_join(mackerel_ziff, Documentation_Versiontotale, by = "cod_esp")

mackerel_ziff <-
  left_join(mackerel_ziff, species_targeted, by = "prespvis")

mackerel_ziff <-
  left_join(mackerel_ziff, pr_spec_caught, by = "prespcap")

mackerel_ziff <- left_join(mackerel_ziff, engin_codes, by = "engin")

capelin_ziff <-
  left_join(capelin_ziff, Documentation_Versiontotale, by = "cod_esp")

capelin_ziff <-
  left_join(capelin_ziff, species_targeted, by = "prespvis")

capelin_ziff <-
  left_join(capelin_ziff, pr_spec_caught, by = "prespcap")

capelin_ziff <- left_join(capelin_ziff, engin_codes, by = "engin")

## Add port information
Ports <-
  read_excel(
    "C:/Users/SmithAND/Documents/Data/Ziff/Ports.xlsx",
    col_types = c("text", "text", "text", "text", "text")
  )

Ports$DISTRICT_CODE <- str_pad(Ports$DISTRICT_CODE, 2, pad = "0")
Ports$COMMUNITY_CODE <- str_pad(Ports$COMMUNITY_CODE, 2, pad = "0")

Ports$port_att <-
  as.numeric(paste(
    Ports$PROVINCE_CODE,
    Ports$DISTRICT_CODE,
    Ports$COMMUNITY_CODE,
    sep = ""
  ))

Ports$port_deb <-
  as.numeric(paste(
    Ports$PROVINCE_CODE,
    Ports$DISTRICT_CODE,
    Ports$COMMUNITY_CODE,
    sep = ""
  ))

## Some more formatting
mackerel_ziff <- left_join(mackerel_ziff, Ports)

mackerel_ziff <- mackerel_ziff %>%
  mutate(
    Year = year(date_deb),
    Month = month(date_deb),
    Day = day(date_deb),
    vessel_length_m = lht / 0.3048,
    DATE = lubridate::ymd(paste(Year, Month, Day)),
    doy = as.numeric(format(DATE, "%j")),
    div = toupper(div)
  )

capelin_ziff <- left_join(capelin_ziff, Ports)

capelin_ziff <- capelin_ziff %>%
  mutate(
    Year = year(date_deb),
    Month = month(date_deb),
    Day = day(date_deb),
    vessel_length_m = lht / 0.3048,
    DATE = lubridate::ymd(paste(Year, Month, Day)),
    doy = as.numeric(format(DATE, "%j")),
    div = toupper(div)
  )

## Make life easier by reducing keystrokes
names(mackerel_ziff) <- tolower(names(mackerel_ziff))
names(capelin_ziff) <- tolower(names(capelin_ziff))

#####################  SAVE DATA  ####################
save(mackerel_ziff, file = "C:/Users/SmithAND/Documents/Data/Ziff/Rdata/mackerel_ziff.Rdata")
save(capelin_ziff, file = "C:/Users/SmithAND/Documents/Data/Ziff/Rdata/capelin_ziff.Rdata")

## Clean work space
rm(Documentation_Versiontotale);rm(engin_codes)rm(engin_codes);rm(Ports);rm(species_targeted);rm(pr_spec_caught)