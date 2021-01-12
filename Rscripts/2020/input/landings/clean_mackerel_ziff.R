#######
##  Clean mackerel ziff
##  - Run ziff_exploreR.R first
##  By ANDREW SMITH, July 2020,
##  Log:
##  - based on same for capelin from cpue_clean.R
##  - just noticed 2019 not entered yet, re extract from bd peches
##  - updating for 2021
##  - 2020 data updated on 22/12/2020
##
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
load("C:/Users/SmithAND/Documents/Data/Ziff/Rdata/mackerel_ziff.Rdata")

# Look at data
summary(mackerel_ziff)
glimpse(mackerel_ziff)
esquisse::esquisser() # interactive visualisation of dataframes and production of ggplot code

# Subset to make sure only mackerel
mackerel_ziff %<>% dplyr::filter(species_en == "Mackerel")

#####################  CLEAN DATA  ####################

# Create variables to match mackerel_bio.R
mackerel_ziff$division <- str_sub(mackerel_ziff$opano, 1, 2)

# new variable : quarter
mackerel_ziff %<>% mutate(quarter = fct_collapse(
  factor(mackerel_ziff$month),
  Q1 = c("1", "2", "3"),
  Q2 = c("4", "5", "6"),
  Q3 = c("7", "8", "9"),
  Q4 = c("10", "11", "12")
))

# Explicit NA for grouping
mackerel_ziff$engin_en <- forcats::fct_explicit_na(mackerel_ziff$engin_en)
mackerel_ziff$opano <- forcats::fct_explicit_na(mackerel_ziff$opano)

# Aggregate gear types for catch at age based off of Francois Grégoire's suggestions of similar gear selectivity
unique(mackerel_ziff$engin_en)

mackerel_ziff %<>% mutate(
  gear_caa = fct_collapse(
    factor(mackerel_ziff$engin_en),
    "Seines_Nets_Traps_Weirs" = c(
      "Trap net",
      "Weir",
      "Box net",
      "Pot",
      "Bag net",
      "Fyke net",
      "Purse seine",
      "Tuck seine",
      "Danish seine",
      "Beach and bar seine",
      "Scottish seine"
    ),
    "Gillnets" = c(
      "Gillnet (set or fixed)",
      "Gillnet (drift)",
      "Gillnet (unspecified)"
    ),
    "Lines" = c(
      "Hand line (baited)",
      "Jigger",
      "Longline",
      "Rod and reel (chumming)",
      "Mechanized squid jigger",
      "Hand and hand held tools",
      "Automated jigger  (hand line)"
    ),
    "Misc" = c(
      "Bottom otter trawl (stern)",
      "Shrimp trawl",
      "Bottom otter trawl (side)",
      "Midwater pair trawl",
      "Midwater trawl (stern)",
      "Unknown",
      "Miscellaneous",
      "Setheared hooks",
      "Null data",
      "(Missing)"
    )
  )
)

unique(mackerel_ziff$gear_caa)

mackerel_ziff %<>% mutate(subdivision = ifelse(is.na(div), opano, div))

# Bin nafo divions to reflect bioregions
# All NAs from Quebec 4S and 4T. Most are from purchase slips and data coded to have been landed Jan. 1st i.e. incomplete logbooks or slips
mackerel_ziff$opano <- toupper(mackerel_ziff$opano)
mackerel_ziff  %<>%
  mutate(
    div_caa = fct_collapse(
      mackerel_ziff$opano,
      "2GJ3KLMNO" = c("2G", "2J", "3K", "3L", "3M", "3N", "3O"),
      "3P4V" = c("3PS", "3PN", "4VN", "4VS"),
      "4RST" = c("4R", "4S", "4T", "(Missing)"),
      "4XW5YZ" = c("4X", "4W", "5ZE", "5Y")
    )
  ) 

unique(mackerel_ziff$div_caa)

save(mackerel_ziff, file = "Rdata/mackerel_ziff.Rdata") # last updated in the second week of December 2020.

# Format for CCAM
ziff <- mackerel_ziff %>% group_by(year) %>% dplyr::summarise(landings_ziff_t = sum(pds_vif)/1000)

# Will have to manually fill in 2017-2020 landings data for ct.dat
# 2017 = 9783.359; 2018 = 12029.13; 2019 = 9254.144; 2020 = 9295.369 or 8373.241 (see below)

save(ziff, file = "Rdata/ziff.Rdata")
write.csv(ziff, file = "csv/ziff.csv", row.names = F)

#####################  QUALITY CHECK  ####################

# Context: - Landings from the Gulf have been slow to come in historically. Gist is that mackerel are lower priority regionally and that the preponderance of purchase slips vs logbooks makes processing slow
#          - At the 2019 mackerel stock assessment, landings data from the Gulf Region was not available at the time of the assessment (impacts on catch-at-age)
#          - Landings data for the Gulf in 2018 was obtained from the Gulf Quota Report page and from communications with their statistics bureau
#          - Landings data for the Gulf region in 2018 and 2020 still not on their quota reports page as of (22/12/2020) but...!
#          - The data are on their Public (drop down menu) page https://inter-j01.dfo-mpo.gc.ca/qr/report/query
#          - 

# Verify ziff landings vs regional quota reports and from NHQ
table(mackerel_ziff$year, mackerel_ziff$division)

ziff_table <- mackerel_ziff %>% group_by(year, region) %>%
  dplyr::summarise(landings_ziff_t = sum(pds_vif)/1000) %>% 
  pivot_wider(names_from = region, values_from = landings_ziff_t)

# ziff 2017 update
# G = 3726.159
# N = 2653.286
# Q = 1347.125
# S = 2056.789
# total = 9783.359 vs 9430 t as reported in last assessment

# ziff 2018 =
# G = NA	        # even after an update 2 years later. Note to self. Mention this to Denis Bernier
# N = 5625.213
# Q = 1426.380
# S = 1521.602

# Gulf quota report 2018: https://inter-j01.dfo-mpo.gc.ca/qr/report/query
# prelim 2018 = NA
# public 2018 = 3455.939	(NB = 260.168; PEI = 1916.675; NS = 38.213; QC = 1240.883)

# 2018 total = 5625.213 + 1426.380 + 1521.602 + 3455.939 =
# 12029.13 t; 120.2913 % over the TAC (not including foreign) vs. 10499 t as reported in the last assessment

# 2019
# G = 2150.688
# N = 4813.750
# Q = 753.976
# S = 907.741

# Gulf quota report 2019: https://inter-j01.dfo-mpo.gc.ca/qr/report/query
# ziff 2019 = 2151
# prelim 2019 = NA
# public 2019 = 2778.677 =(NB = 364; PEI = 1579.499; NS = 198.19; QC = 636.988)

# 2019 total if ziff only = 8626.155
# 2019 total ziff + gulf quota report = 4813.750 + 753.976 + 907.741 + 2778.677 = 9254.144

# 2020
# G = NA
# N = 4013.918
# Q = 679.142
# S = 1128.487

# Gulf quota report 2020: https://inter-j01.dfo-mpo.gc.ca/qr/report/query
# prelim 2020 = NA
# public 2020 = 3473.822	(NB = 318.281; PEI = 1454.224; NS = 126.072; QC = 653.117, Scotia Fundy (why are they there?!) = 875.817; NL (again, wtf?!) = 46.311)

# From Jenness on December 10th:

# 2020 Total landings to date	(10-Dec)
# NL: 4014		ziff says 4013.918
# Mar: 1294		ziff says 1128.48
# Gulf: 1947		ziff = missing
# QC: 679	ziff = 679.142

# total as per ziff file = 5821.547 t whereas,
# total as per Jenness = 7934	t
# total if ziff + gulf quota report (total) = 9295.369
# total if ziff + gulf quota report (w/o scotia-fundy and NL) = 2551.694 + 5821.547 = 8373.241 t
