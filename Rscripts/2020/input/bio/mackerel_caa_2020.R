#####################  Estimation of catch-at-age and its variance for mackerel  #################### 
##  NEED TO UPDATE HEADER COMPLETELY 
## 
##  Adapted from:
##  1) CATCH.exe, a VBA blackbox program that was inspired from equations in Gavaris & Gavaris 1983 in Doubleday and Rivard Can.Spec.Publ.Fish.Aquat.Sci.66
##     - Methodologies in catch.exe are not exactly the same as Gavaris & Gavaris based on the program documentation but there is no way to look under the hood of the program either.
##     - For the 2018 assessment I searched for R packages/functions that did age-length-keys and/or catch at age but there are few in existence. Derek Ogle's FSA package is the only one that I found and I didn't want to have to reinvent the wheel. 
##  2) FSA package in R https://derekogle.com/
##  3) ICCAT. 2006-2016. ICCAT Manual. International Commission for the Conservation of Atlantic Tuna. Puts Gavaris^2 1983 and Baird 1983 (in Doubleday and Rivard 1983) into "plain English" ...at least plain to me. 
##     https://www.iccat.int/Documents/SCRS/Manual/CH4/CH4_3-ENG.pdf
##
# Key References:
#
# Baird, J.W. 1983. A Method to Select Optimum Numbers for Aging in a Stratified Approach. 
# In W. G. Doubleday and/et D. Rivard [ed./ed.] Sampling commercial catches
# of marine fish and invertebrates/L'léchantillonnage des prises commerciales de poissons
# et d'invertebres marins. Can. Spec. Pub!. Fish. Aquat. Sci./Publ. spec. can. sci. halieut. aquat 66.
#
# 
# Goal: The objective of sampling for ages is to obtatin similar
# coefficients of variation (CV) for all ages of fish that contribute significantly to the population
# To this en, here we evaluate number of fish and number of samples needed to have satisfactory precision in the Northwest Atlantic's northern contingent of Atlantic mackerel

# Variables:
# Ni - catch number at length i
# ni - number aged at length i
# pi - proportion at length for a given age
# nipi - number aged at length that are a given age
# Nipi - catch at length for a given age
# k - stratification (year/quarter/division/gear)
# An appropriate age-length-key 

# Methodology: Follow eqns in Baird 1983 for each age.
# N = sum(Nipi) catch at age is the sum catch at each length for that age
# Var(Nipi) = Ni^2*Var(pi) + pi^2*Var(Ni) - Gulland 1955 variance of the catch at each length group
# Second term can be dropped
# Var(Nipi) = Ni^2*Var(pi) where Var(pi) = pi*(1-pi)/ni
# Var(N) = sum(Ni^2)*Var(pi)
# CV = Var(N)/N
#
##  Notes:
##  Port samplers in Nova Scotia and Eastern NL were lacking for years. We managed to get new samples but they do not follow the two stage sampling program due to lack of resources. Random samples are sent directly to IML from these regions and so aged and unaged samples need to be merged here even if it isn't perfect. However, since I make smoothed alks it should be fine
##  
##  By ANDREW SMITH, November 2018
##  Change Log:
##  July 27, 2020 
##  - updating to run all years
##  January 2021
##  - updating for 2015-2020 - noticed that cn.dat was not updated for 2015-2016 so do that


#####################  Load libraries  ####################

packages = c('FSA','magrittr','nnet','broom', 'purrr', 'nnet', 'readxl', 'magrittr','tidyverse')
invisible(lapply(packages, function(x) {if (!require(x, character.only = T)) {install.packages(x);require(x)}}))

#####################  Load data  ####################

# Combined carbio and length frequency data. Made with read_mackerel_bio_lf.R. Contains commercial fish samples and some data from fisheries independant surveys and sporadic research projects. Since 2000, mackerel biological data tagged as "research" has largely been added to an excel sheet. It includes samples from BIOs winter/spring survey in the Gulf of Maine/Georges Bank/Scotian Shelf bottom trawl surveys (1970-present - bio data prior to 2000 in various research documents, manuscripts, & theses or in the Maritimes database - see their github page)
load("Rdata/mackerel_bio.Rdata")

# Canadian landings data. Made with ziffexploreR.R and clean_mackerel_ziff.R. Does not include foreign landings in Canadian EEZ. 1985-present.
load("Rdata/mackerel_ziff.Rdata") 

# Last minute length frequencies from 3KD in 2020. Purse seine, Q3. Being coded into peche database presently.
mack_2020_3kd_lf <- read_csv("data/bio/mack_2020_3kd_lf.csv") # bio now contains this

# Gulf region landings data missing from ZIFF files. Different format than ZIFF but formatted and made with clean_mackerel_ziff.R (bottom of script)
# load("Rdata/gulf_2018_2020_pr.Rdata") # commercial and bait landings for gulf regions because missing in ZIFF for 2018 and 2020. Not as detailed as ZIFF so coded all as gillnet despite lines being more prevalent... may be problematic since lines capture smaller mackerel generally. could use last few years and calculate relative proportions per quarter. very shoddy prior to 2018. gulf region management databases are poor in quality. this data does, however, allow the distinction between commercial licence landings and bait licence landings (!!!) 25% DMP since 2018
# ziff contains this now

# # Gulf region landings data missing from QC ZIFF files. Using the first data set (just above this)
# gulf_ziff_2018_2020 <- read_csv("~/Data/Ziff/ZIFF/gulf_ziff_2018-2020.csv")
# glimpse(gulf_2018_2020_pr)
# Biological samples since 2000 stored in an active excel file. is scheduled to be coded into the main database with the appropriate coding. some tweaking of the excel file column headers, cell formats, and some filling in of NAs to facilitate loading in R (i.e. the .xlsx file here is modified from the original one on the S/Pelagiques/maqueraeau network files) mackerel samples from needler and teleost missions (mostly maritimes)
mackerel_bio_research <- read_excel("data/bio/mackerel_bio_research.xlsx",
                                    col_types = c("text", "text", "text",
                                                  "text", "text", "text", "numeric", 
                                                  "text", "text", "text", "text", "text", 
                                                  "numeric", "numeric", "text", "text", 
                                                  "numeric", "text", "text", "text"), skip = 3)   
                                                                                         

##################### Data wrangling  ####################

# Subset ZIFF data for easier manipulation 
mackerel_ziff %<>%
  dplyr::select(year, month, quarter, division, div_caa, gear_caa, pds_vif) %>%
  # dplyr::filter(year > 2014) %>% 
  mutate(landings_t = pds_vif/1000)


# Make common grouping factors (Francois Grégoire grouped nets, traps, weirs, trawls, and seines together, otherwise lines or gillnets)
# - though, tbh, besides lines (LX) of all sorts, the various gear types are not that selective. lines get the lil tinkers... though this is in large part due to the spatio temporal nature of the fishery and the life cycle/ontology of the various lifecycles of mackerel. gulf switches from gillnets (spring) to lines in the summer. gillnets catch the spawners. lines get the yoy and ages 1-3 mostly. same for the scotian shelf except that since the outer shelf is a nursery for yoy-2 and they move inshore during the summer  (strawberry run) they are more likely to be caught there while the adults are out further abroad feeding. (see Kulka 1977 and Gregoire and Showell 1994 and any recent BIO data)
mackerel_ziff %<>% mutate(division_group = fct_collapse(div_caa,
                                                       "4RST" = c("4RST", "(MISSING)"), # Missing values are from Québec so 4RST
                                                       "2J3KL" = "2GJ3KLMNO",
                                                       "4V3P" = "3P4V",
                                                       "4WX5YZ" = "4XW5YZ"))

mackerel_ziff %<>% mutate(gear_caa = fct_collapse(gear_caa,
                                                        "gillnets" = c("Gillnets"),
                                                        "lines" = "Lines",
                                                        "seines_nets_traps_weirs_misc" = c("Seines_Nets_Traps_Weirs", "Misc")))

# Format missing gulf landings data
# gulf_2018_2020_pr %<>% dplyr::select(1,2,6,5,4,8) %>% mutate(division_group = "4RST")

# Merge ZIFF file with Gulf data
# mackerel_ziff <- bind_rows(mackerel_ziff, gulf_2018_2020_pr %>% dplyr::filter(year != 2019)) # detailed 2019 landings are in the ziff but are slightly smaller than in the other data base possibly due to bait (ziff = 2151; gulf caqr = 2223

mackerel_bio %<>% mutate(gear_caa = fct_collapse(gear_caa,
                                                  "gillnets" = "gillnets",
                                                  "lines" = "lines",
                                                  "seines_nets_traps_weirs_misc" = c("nets_traps_weirs_misc", "seines")))
# Subset bio data for easier manipulation
mackerel_bio <- as_tibble(mackerel_bio %>% 
  dplyr::select(year, month, quarter, length, mass,
                division, division_group,
                f_age, gear, gear_caa, s_ech, sample_id, data_source)) %>% 
  dplyr::filter(year > 2014)

# Format extra 2020 3KD length frequency data and research data
#  - expand based on number of counts
mack_2020_3kd_lf <- mack_2020_3kd_lf[rep(1:nrow(mack_2020_3kd_lf), mack_2020_3kd_lf[["freq"]]), ]
mack_2020_3kd_lf$data_source = "length_freq"
mack_2020_3kd_lf$sample_id <- as.factor(mack_2020_3kd_lf$sample_id)

mackerel_bio <- bind_rows(mackerel_bio, mack_2020_3kd_lf)

# Research data - needs to be standardised. Many of the NAs can be cross referenced with the BIO survey data. - to be done in future. NAs filled in and headers changed from original file for easier import and wrangling in R.
# Create common set of grouping variables
mackerel_bio_research %<>% mutate(quarter = fct_collapse(as.factor(month),
                                                         "Q1" = c("printemps","fev-mars","hiver"),
                                                         "Q2" = c("4","5","6"),
                                                         "Q3" = c("7","8","9","juil-août","juillet","été", ".", "42644"),
                                                         "Q4" = c("10","11","12")))

mackerel_bio_research %<>% mutate(division_group = fct_collapse(as.factor(division),
                                                         "2J3KL" = c("3K","3L",  "3Lb", "3Ki", "3Lf"),
                                                         "4RST" = c("4S", "4T", "4Tf", "4Rc", "4Rd", "4Ra", "4Tm","4Tn"),
                                                         "4WX5YZ" = c("4W"),
                                                         "4V3P" = c("3P","4Vn")))


mackerel_bio_research %<>% dplyr::select(year, quarter, division_group, length, mass, sex, maturity, mass_gonad, age )
mackerel_bio_research %<>% transmute(year = as.numeric(year), quarter = quarter, division_group = division_group, gear_caa = "seines_nets_traps_weirs_misc",length = length,  mass = mass, sex = sex, maturity = as.numeric(maturity), mass_gonad = mass_gonad, f_age = as.factor(age), sample_id = as.factor(paste(year,quarter,division_group)), data_source = "carbio" )
mackerel_bio_research %<>% dplyr::filter(year > 2014) # n.obs = 2045 of 3178 (Jan 2021)

# merge
mackerel_bio <- bind_rows(mackerel_bio, mackerel_bio_research) # n.obs = 69900 (2015-2020; Jan 2021)

# Add length category variable (5mm as per current two stage sampling protocol (i.e. ~ 100-200 fish sampled for length frequencies (sortie.dat) in proportion to landings and 2 fish per 5mm length class sent to IML to be processed for biological trait data (Maquereau_carbio.dat))) 
mackerel_bio %<>% 
  mutate(lcat5 = FSA::lencat(length, w = 5))

# Note, in 2020 there are some 2J3KL length frequency data in the carbio file because of issues with port sampling. We renewed the sampling there but have not implemented a two stage system yet
# There are multiple other instances of this too. Caution must be taken because of the nature of catch at age equations. Specifically, the association of age length keys to length frequency samples can change the weighting of the data and thus final catch at age. 

# Make second index to parse data
# counts of original 
mackerel_bio %>% group_by(data_source) %>% summarise(n = n()) #carbio  = 17285; length_freq = 52615 (Jan 2021)
mackerel_bio %<>% dplyr::mutate(data_source_2 = ifelse(data_source == "carbio" & is.na(f_age), "length_freq", data_source))
mackerel_bio %>% group_by(data_source_2) %>% summarise(n = n()) #carbio 15628; = length_freq  = 54272;   54272-52615 =  1657 good (Jan 2021)

##################### Higher order summaries  ####################

# Number of samples and number of fish sampled per catch-at-age stratification (i.e. year/quarter/division/gear_type)
mackerel_lf_summary <- mackerel_bio %>% dplyr::filter(data_source_2 == "length_freq") %>%  
  mutate(sample_id = ifelse(is.na(sample_id), s_ech, sample_id)) %>% 
  group_by(year, quarter, division_group, gear_caa) %>% 
  dplyr::summarise(n_lf_samples = n_distinct(sample_id), n_lf = n()) 

mackerel_bio_summary <- mackerel_bio %>% dplyr::filter(!is.na(f_age)) %>%  # note here that I do not call data_source == "carbio". Some LF data coded in carbio database and/or some carbio not aged (either because of subsampling, lack of resources and so at least getting some data, undocumented historic choices made etc.)    Context: This is because over the years various biological samples have been entered into the commercial database and only in 2020 was the problem identified and the DAISS SAS program modified so that the variable "type" is included in the extraction. type indicates data type. 99 = commercial. 93 is BIO. all other types are either special projects, random samples not known how to class etc. db from 2000-present.
  mutate(s_ech = ifelse(is.na(s_ech), sample_id, s_ech)) %>%  
  group_by(year, quarter, division_group, gear_caa) %>% 
  dplyr::summarise(n_bio_samples = n_distinct(s_ech), n_bio = n())

mackerel_ziff_summary <- mackerel_ziff %>% 
  group_by(year, quarter, division_group, gear_caa) %>% 
  dplyr::summarise(landings = sum(landings_t, na.rm = T)) %>% 
  mutate(over_1000 = ifelse(landings > 999.999, "over_1000", "under_1000"),
         over_median = ifelse(landings > median(landings), "over_median", "under_median"))
  
mackerel_bio_summary <- full_join(mackerel_lf_summary, mackerel_bio_summary, by = c("year","quarter","division_group","gear_caa")) %>% arrange(year, quarter, division_group, gear_caa)

# With this file I can index groupings
mackerel_caa_summary <- full_join(mackerel_ziff_summary, mackerel_bio_summary, by = c("year","quarter","division_group","gear_caa")) %>% arrange(year, quarter, division_group, gear_caa)

# mackerel_caa_summary moved to spread sheet to make index to assign ALKs to LFs

# save(mackerel_caa_summary, file = "Rdata/mackerel_caa_summary_15-20.Rdata")
# caa_bio_index_2015_2020 <- read_csv("~/My Stocks/Mackerel/mackerel_sar_2021/mackerel_model_2021/mackerel_assessment_input/data/bio/caa_bio_index_2015_2020.csv") # File made with mackerel_caa_summary with added index to group ALK and LF data
caa_bio_index_2015_2020 <- read_csv("./data/bio/caa_bio_index_2015_2020.csv")
#####################  Predict Mass as a function of length (stratification = year * quarter; log-log linear regression lm())  ####################

# Fit model to each year/quarter stratification
mackerel_ml_fits <- mackerel_bio %>% 
  dplyr::filter(!is.na(mass), !is.na(length), mass > 0) %>% # a few weird values
  group_by(year, quarter) %>% 
  nest() %>% 
  mutate(fit = map(data, ~ lm(log(mass) ~ log(length), data = .x)))

# Nest the length frequency data by the same stratification  
mack_lf_list <- mackerel_bio %>% 
  dplyr::filter(data_source_2 == "length_freq") %>%
  group_by(year, quarter) %>% 
  nest() %>% 
  transmute(year = year, quarter = quarter, lf_data = data) 

# Merge the datasets by the common stratification
mackerel_ml_fits <- right_join(mackerel_ml_fits, mack_lf_list, by = c("year", "quarter")) %>%
  arrange(year, quarter)

# Get predictions from original data ie adjusted/fitted values
mackerel_ml_fits <- mackerel_ml_fits %>% 
  mutate(tidied = map(fit, tidy),
         glanced = map(fit, glance),
         predictions = map(fit, augment, type.predict = "response"))

# Predict masses from corresponding stratified length frequency data
mackerel_ml_fits <- mackerel_ml_fits %>%
  mutate(predictions_new = map2(fit, lf_data, ~ broom::augment(.x, newdata = .y, type.predict = "response")))

# Get model coefficients and back-transform log transformed response variable (mass) and then multiply by 1/2 sigma^2 to correct for known bias
mackerel_ml_fits <- mackerel_ml_fits %>% 
  mutate(syx = map_dbl(fit, sigma),
         cf = exp((syx^2)/2))
    
# Pluck the nested length frequency data with their new predicted masses 
mackerel_lf <- mackerel_ml_fits %>% 
  unnest(predictions_new) %>% 
  mutate(mass = exp(.fitted)*cf)
# - can extract other lists too like model coeffs etc mackerel_ml_fits %>% unnest(glanced)

# Subset
mackerel_lf %<>% dplyr::select(1,2,9:29) %>% as_tibble()

save(mackerel_ml_fits, file = "Rdata/mackerel_ml_fits.Rdata")
save(mackerel_lf, file = "Rdata/mackerel_lf.Rdata")
save(mack_bio, file = "Rdata/mack_bio.Rdata")
rm(mack_lf_list); rm(mackerel_ml_fits)
rm(mackerel_bio_summary); 

##################### 3) Create age-length-keys (observed or modeled) and apply to length frequency data  ####################
# load("Rdata/mackerel_lf.Rdata")

# Join indexed landings to aged and unaged data
mackerel_lf <- left_join(mackerel_lf, caa_bio_index_2015_2020 %>% dplyr::select(1:4,12)) %>%
                           dplyr::select(year, quarter,month, division_group, division, gear_caa, lcat5, f_age, mass, index) %>%
  arrange(year, quarter, division_group, division, gear_caa, lcat5)

mackerel_bio <- mackerel_bio %>%
  dplyr::filter(!is.na(f_age), !is.na(lcat5)) %>%
  dplyr::select(year, quarter,month, division_group, division, gear_caa, mass, lcat5, f_age)
mackerel_bio <- left_join(mackerel_bio, caa_bio_index_2015_2020 %>% dplyr::select(1:4,12))


# NEW 2022
# Key for resdoc publication
key_bio <-unique(mackerel_bio[,c('index','year','quarter','month','division_group','division','gear_caa')])
key_bio2 <- mackerel_bio %>% 
    group_by(index, year,quarter,month,division_group,division,gear_caa) %>% dplyr::summarise(n_bio = n()) %>% 
    arrange(year, quarter,division_group,gear_caa) %>% 
    mutate(key = paste(year, "ALK", quarter, division_group , gear_caa, sep = "_"))

key_lf <-unique(mackerel_lf[,c('index','year','quarter','month','division_group','division','gear_caa')])
key_lf2 <- mackerel_lf %>% 
    group_by(index, year,quarter,month,division_group,division,gear_caa) %>% dplyr::summarise(n_lf = n())%>% 
    arrange(year, quarter,division_group,gear_caa) %>% 
    mutate(key = paste(year,"LF", quarter, division_group , gear_caa, sep = "_"))

KEY <- full_join(key_bio2,key_lf2) %>% arrange(year, quarter, gear_caa)

# or just use caa_bio_index
caa_bio_index_2015_2020 %<>% 
    dplyr::select(year, index, quarter, division_group, gear_caa, n_lf, n_bio, landings) 
caa_bio_index_2015_2020$n_bio %<>% replace_na(0) 
caa_bio_index_2015_2020$n_lf %<>% replace_na(0) 
caa_bio_index_2015_2020$landings %<>% replace_na(0) 
caa_bio_index_2015_2020$landings <- round(caa_bio_index_2015_2020$landings)
caa_bio_index_2015_2020 %<>% arrange(year, quarter,gear_caa,index)


caa_bio_index_2015_2020 %<>% group_by(year, quarter,division_group, gear_caa, index) %>% 
    dplyr::summarise(landings = round(sum(landings,na.rm=T),2),
                     n_lf = sum(n_lf, na.rm = T), 
                     n_bio = sum(n_bio, na.rm = T))
# copy paste this for res doc table S4 2022


## METHOD 1 a & b: global observed and smoothed age-length-keys

# # First. Global length/mass/age stats
# ## Construct Observed ALK
# alk.freq <- xtabs(~lcat5+f_age, data = mackerel_bio)
# alk <- prop.table(alk.freq,margin = 1)
# len.n <- xtabs(~lcat5,data = mackerel_bio)
# 
# ## Construct Modelled ALK
# alk_mod <- multinom(f_age~lcat5,data = mackerel_bio %>% dplyr::filter(lcat5 > 5),maxit = 500)
# lens <- seq(54,415,5)
# alk_smoothed <- predict(alk_mod, data.frame(lcat5 = lens), type = "probs")
# row.names(alk_smoothed) <- lens
# alk_smoothed <- as.table(alk_smoothed)
# alkPlot(alk, type = "area", pal = "jet", showLegend = T)
# alkPlot(alk_smoothed, type = "area", pal = "jet", showLegend = T)
# 
# # obs applied alk
# df.obs.unaged <- alkIndivAge(alk, as.numeric(f_age) ~ lcat5, data = mackerel_bio)
# # mod applied alk
# df.mod.unaged <- alkIndivAge(alk_smoothed, as.numeric(f_age) ~ lcat5, data = mackerel_bio %>% dplyr::filter(lcat5 > 0))
# 
# df.obs.unaged %<>% mutate(age = `as.numeric(f_age)`)
# df.mod.unaged %<>% mutate(age = `as.numeric(f_age)`)
# 
# df.obs.unaged %>% group_by(year, quarter, division_group) 
# df.mod.unaged %>% group_by(year, quarter, division_group)
# df_ziff <- mackerel_ziff %>% group_by(year, quarter, division_group) %>% 
#   summarise(landings = sum(landings_t))
# 
# df.mass <- df.obs.unaged %>% group_by(year, quarter, division_group, lcat5, f_age) %>% 
#   summarise(mass = mean(mass))
# 
# # no age zeros
# df.obs.unaged  %<>%  dplyr::filter(f_age != 0) %>% droplevels() 
# df.mod.unaged  %<>%  dplyr::filter(f_age != 0) %>% droplevels() 
# 
# # Get total frequencies per age and length class (Nijk)
# df <- 
#   df.obs.unaged %>% 
#   group_by(year, quarter, division_group) %>% # grouping variables for weighting
#   group_modify(~ as.data.frame(xtabs(~lcat5 + f_age, data = .))) %>% # map the xtabs function to the split data to get frequencies
#   mutate(lcat5 = as.numeric(as.character(lcat5)), f_age = as.numeric(as.character(f_age)))  %>% as_tibble()
# 
# df_mod <- 
#   df.mod.unaged %>% 
#   group_by(year, quarter, division_group) %>% # grouping variables for weighting
#   group_modify(~ as.data.frame(xtabs(~lcat5 + f_age, data = .))) %>% # map the xtabs function to the split data to get frequencies
#   mutate(lcat5 = as.numeric(as.character(lcat5)), f_age = as.numeric(as.character(f_age)))  %>% as_tibble()
# # Get proportions per age and length class (pijk)
# df1 <- 
#   df.obs.unaged %>% 
#   group_by(year, quarter, division_group) %>% # grouping variables for weighting
#   group_modify(~ as.data.frame(prop.table(xtabs(~lcat5 + f_age, data = .), margin = 1))) %>% # map the xtabs and proptable functions to the split data to get proportions
#   mutate(lcat5 = as.numeric(as.character(lcat5)), f_age = as.numeric(as.character(f_age)))  %>% as_tibble() # change classes
# df1_mod <- 
#   df.mod.unaged %>% 
#   group_by(year, quarter, division_group) %>% # grouping variables for weighting
#   group_modify(~ as.data.frame(prop.table(xtabs(~lcat5 + f_age, data = .), margin = 1))) %>% # map the xtabs and proptable functions to the split data to get proportions
#   mutate(lcat5 = as.numeric(as.character(lcat5)), f_age = as.numeric(as.character(f_age)))  %>% as_tibble() # change classes
# 
# ##################### 5) Stratified Numbers at age by length group and precision estimates (Ogle)  ####################
# # change column names 
# colnames(df)[colnames(df) == "Freq"] <- "n.ijk" 
# colnames(df1)[colnames(df1) == "Freq"] <- "p.ijk"
# colnames(df_mod)[colnames(df_mod) == "Freq"] <- "n.ijk" 
# colnames(df1_mod)[colnames(df1_mod) == "Freq"] <- "p.ijk"
# df <- full_join(df, df1) 
# df %<>% dplyr::filter(n.ijk != 0)
# df_mod <- full_join(df_mod, df1_mod) 
# df_mod %<>% dplyr::filter(n.ijk != 0)
# 
# # weight the masses
# df %<>% mutate(raised_numbers = n.ijk * p.ijk) 
# df_mod %<>% mutate(raised_numbers = n.ijk * p.ijk) 
# 
# # mass
# df.mass %<>% 
#   mutate(f_age = as.numeric(as.character(f_age)), mean_mass_t = mass/1e6)
# 
# # merge masses with numbers at age
# df3 <- left_join(df,df.mass)
# df3_mod <- left_join(df_mod,df.mass)
# 
# df3 %<>% mutate(raised_mass = mean_mass_t * raised_numbers)
# df3_mod %<>% mutate(raised_mass = mean_mass_t * raised_numbers)
# 
# # summarise and get stratified numbers at age, their variances, the weighted mean mass of an individual, and the sum of the sample
# df4 <- df3  %>%  group_by(year, quarter, division_group, f_age) %>% 
#   dplyr::summarise(mean_mass_t = mean(mean_mass_t, na.rm = T),
#                    N.jk = sum(raised_numbers, na.rm = T),
#                    var.Njk = var(raised_numbers, na.rm = T),
#                    sample_mass = sum(raised_mass, na.rm = T))
# df4_mod <- df3_mod  %>%  group_by(year, quarter, division_group, f_age) %>% 
#   dplyr::summarise(mean_mass_t = mean(mean_mass_t, na.rm = T),
#                    N.jk = sum(raised_numbers, na.rm = T),
#                    var.Njk = var(raised_numbers, na.rm = T),
#                    sample_mass = sum(raised_mass, na.rm = T))
# df_ziff
# df5 <- full_join(df4, df_ziff)
# df5_mod <- full_join(df4_mod, df_ziff)
# 
# df5 %<>% mutate(var.Njk = replace_na(var.Njk, 0))
# df5_mod %<>% mutate(var.Njk = replace_na(var.Njk, 0))
# 
# # calculate Weigth catch/weight of sample
# mack_caa_obs <- df5 %>% group_by(year, quarter, division_group) %>% 
#   mutate(raising_factor = landings/sum(sample_mass))  
# 
# mack_caa_mod <- df5_mod %>% group_by(year, quarter, division_group) %>% 
#   mutate(raising_factor = landings/sum(sample_mass))  
# 
# # calculate catch at age
# mack_caa_obs %<>% 
#   mutate(Njk = N.jk * raising_factor, 
#          varNjk = var.Njk * raising_factor, 
#          M.jk = Njk * mean_mass_t,
#          sd.N = sqrt(varNjk),
#          CV.N = sd.N/Njk)
# mack_caa_mod %<>% 
#   mutate(Njk = N.jk * raising_factor, 
#          varNjk = var.Njk * raising_factor, 
#          M.jk = Njk * mean_mass_t,
#          sd.N = sqrt(varNjk),
#          CV.N = sd.N/Njk)
# 
# mack_caa_mod %>% ggplot(aes(year, CV.N, group = year)) + geom_violin() + facet_wrap(vars(division_group))
# 
# mack_caa_obs %>% group_by(year) %>% summarise( landings = sum(landings, na.rm = T))
## METHOD 2: Same as 2018 assessment. Hybrid between Ogle 2015 and old catch.exe program. D. Ogle's FSA::alkIndivage() for age assignment based on Knight and Iserman equation. Then following GAVARIS^2 1983 & Baird 1983 for data weighting estimation of catch at age. 

# Nest data
df <- mackerel_bio %>% 
  group_by(index) %>% 
  nest() %>%    # grouping variables for weighting
  transmute(bio_data = data)

# Calculate the frequencies and proportions
df %<>% mutate(alk.freq = map(bio_data, ~ xtabs(~lcat5 + f_age, data = .x)))
df %<>% mutate(alk = map(alk.freq, ~ prop.table(.x, margin = 1)))

# Nest the length frequency data by the same stratification (same procedure as in 2) above)
mackerel_lf <- mackerel_lf %>% dplyr::filter(!is.na(lcat5)) %>% 
  mutate(f_age = as.numeric(f_age)) %>% 
  group_by(index) %>%
  nest() %>% 
  transmute(lf_data = data)

# Merge data frames
df <- full_join(mackerel_lf, df)

# Apply ALK to unaged data

# this (below) doesn't work because of differences in sizes of length frequencies
# df %>% mutate(predictions = map2(alk, lf_data, ~ alkIndivAge(.x, f_age~lcat5, data = .y)))

# So iterate and modify by hand. if null choose closest division in same quarter and year
alk1 <- alkIndivAge(df$alk[[1]], df$lf_data[[1]] , formula = f_age~lcat5) 
alk2 <- alkIndivAge(df$alk[[2]], df$lf_data[[2]] , formula = f_age~lcat5)
alk3 <- alkIndivAge(df$alk[[3]], df$lf_data[[3]], formula = f_age~lcat5)
alk4 <- alkIndivAge(df$alk[[4]], df$lf_data[[4]] , formula = f_age~lcat5)
alk5 <- alkIndivAge(df$alk[[5]], df$lf_data[[5]] , formula = f_age~lcat5)
alk6 <- alkIndivAge(df$alk[[6]], df$lf_data[[6]] , formula = f_age~lcat5)
alk7 <- alkIndivAge(df$alk[[7]], df$lf_data[[7]] , formula = f_age~lcat5)
alk8 <- alkIndivAge(df$alk[[8]], df$lf_data[[8]] , formula = f_age~lcat5) 
alk9 <- alkIndivAge(df$alk[[9]], df$lf_data[[9]] %>% dplyr::filter(lcat5 > 175), formula = f_age~lcat5) # one obs dropped
alk10 <- alkIndivAge(df$alk[[10]], df$lf_data[[10]] , formula = f_age~lcat5) 
alk11 <- alkIndivAge(df$alk[[11]], df$lf_data[[11]] , formula = f_age~lcat5)
alk12 <- alkIndivAge(df$alk[[12]], df$lf_data[[12]] , formula = f_age~lcat5) 
alk13 <- alkIndivAge(df$alk[[13]], df$lf_data[[13]] , formula = f_age~lcat5) 
alk14 <- alkIndivAge(df$alk[[14]], df$lf_data[[14]] , formula = f_age~lcat5)
alk15 <- alkIndivAge(df$alk[[15]], df$lf_data[[15]] , formula = f_age~lcat5)
alk16 <- alkIndivAge(df$alk[[16]], df$lf_data[[16]] , formula = f_age~lcat5)
alk17 <- alkIndivAge(df$alk[[17]], df$lf_data[[17]] %>% dplyr::filter(lcat5 > 235) , formula = f_age~lcat5) # one obs dropped
alk18 <- alkIndivAge(df$alk[[18]], df$lf_data[[18]] , formula = f_age~lcat5) 
alk19 <- alkIndivAge(df$alk[[19]], df$lf_data[[19]] , formula = f_age~lcat5)
alk20 <- alkIndivAge(df$alk[[20]], df$lf_data[[20]] , formula = f_age~lcat5)
alk21 <- alkIndivAge(df$alk[[21]], df$lf_data[[21]] , formula = f_age~lcat5)
alk22 <- alkIndivAge(df$alk[[22]], df$lf_data[[22]] , formula = f_age~lcat5)
alk23 <- alkIndivAge(df$alk[[23]], df$lf_data[[23]] , formula = f_age~lcat5)
alk24 <- alkIndivAge(df$alk[[24]], df$lf_data[[24]] %>% dplyr::filter(lcat5 > 224), formula = f_age~lcat5)
alk25 <- alkIndivAge(df$alk[[25]], df$lf_data[[25]] , formula = f_age~lcat5)
alk26 <- alkIndivAge(df$alk[[26]], df$lf_data[[26]] , formula = f_age~lcat5)
alk27 <- alkIndivAge(df$alk[[27]], df$lf_data[[27]], formula = f_age~lcat5)
alk28 <- alkIndivAge(df$alk[[28]], df$lf_data[[28]] %>% dplyr::filter(lcat5 > 84) , formula = f_age~lcat5)
alk29 <- alkIndivAge(df$alk[[29]], df$lf_data[[29]] , formula = f_age~lcat5)
alk30 <- alkIndivAge(df$alk[[30]], df$lf_data[[30]] , formula = f_age~lcat5)
alk31 <- alkIndivAge(df$alk[[31]], df$lf_data[[31]] , formula = f_age~lcat5)
alk32 <- alkIndivAge(df$alk[[32]], df$lf_data[[32]] %>% dplyr::filter(lcat5 > 259), formula = f_age~lcat5) # 3 obs dropped
alk33 <- alkIndivAge(df$alk[[33]], df$lf_data[[33]] , formula = f_age~lcat5)
alk34 <- alkIndivAge(df$alk[[34]], df$lf_data[[34]] , formula = f_age~lcat5) 
# all age length keys used

# now bind all the rows together 
mackerel_df <- bind_rows(alk1,alk2,alk3,alk4,alk5,alk6,alk7,alk8,alk9,
                         alk10,alk11,alk12,alk13,alk14,alk15,alk16,alk17,
                         alk18,alk19,alk20,alk21,alk22,alk23,alk24,alk25,
                         alk26,alk27,alk28,alk29,alk30,alk31,alk32,alk33,
                         alk34)

save(mackerel_df, file = "Rdata/mackerel_df_caa_15-20.Rdata")
rm(alk1,alk2,alk3,alk4,alk5,alk6,alk7,alk8,alk9,
   alk10,alk11,alk12,alk13,alk14,alk15,alk16,alk17,
   alk18,alk19,alk20,alk21,alk22,alk23,alk24,alk25,
   alk26,alk27,alk28,alk29,alk30,alk31,alk32,alk33,
   alk34)

load("Rdata/mackerel_df_caa_15-20.Rdata")

# Ogle. Ages assigned so now 1) sum Nij across stratifications, calculate the mean mass per Nj, multiply Nj by (Mc/Ms) i.e the stratified total landings/sum(sample mass) where the sum of the sample mass is the summed mean mass per stratification

mackerel_bio$f_age <- as.numeric(mackerel_bio$f_age)
mackerel_bio$data_source_3 = "aged"
mackerel_df$data_source_3 = "unaged"

# option to combine aged and unaged samples however some steps upstream to recode carbio as LF when they don't have age data takes care of this
# mackerel_df <- bind_rows(mackerel_df, mackerel_bio)

# no age zeros
dim(mackerel_df) # 53730    10
mackerel_df  %<>%  dplyr::filter(f_age != 0) %>% droplevels() 
dim(mackerel_df) # 53650    10

# Get total frequencies per age and length class (Nijk)
df <- 
  mackerel_df %>% 
  group_by(year, quarter, division_group, gear_caa) %>% # grouping variables for weighting
  group_modify(~ as.data.frame(xtabs(~lcat5 + f_age, data = .))) %>% # map the xtabs function to the split data to get frequencies
  mutate(lcat5 = as.numeric(as.character(lcat5)), f_age = as.numeric(as.character(f_age)))  %>% as_tibble()

# Get proportions per age and length class (pijk)
df1 <- 
  mackerel_df %>% 
  group_by(year, quarter, division_group, gear_caa) %>% # grouping variables for weighting
  group_modify(~ as.data.frame(prop.table(xtabs(~lcat5 + f_age, data = .), margin = 1))) %>% # map the xtabs and proptable functions to the split data to get proportions
  mutate(lcat5 = as.numeric(as.character(lcat5)), f_age = as.numeric(as.character(f_age)))  %>% as_tibble() # change classes

##################### 5) Stratified Numbers at age by length group and precision estimates (Ogle)  ####################
# change column names 
colnames(df)[colnames(df) == "Freq"] <- "n.ijk" 
colnames(df1)[colnames(df1) == "Freq"] <- "p.ijk"
df <- full_join(df, df1) 
df %<>% dplyr::filter(n.ijk != 0) # no need to keep obs of 0 in df

# scaling factor #### This may be redundant step.... 
df %<>% mutate(raised_numbers = n.ijk * p.ijk) 

# go back and get mean mass per length class in tonnes
df2 <- mackerel_df  %>%  group_by(year, quarter, lcat5, f_age) %>% 
  dplyr::summarise(mean_mass = mean(mass, na.rm = TRUE)) %>% 
  mutate(mean_mass_t = mean_mass/1e6)

# merge the two data sets (numbers and proportions per age and length class with masses for same)
df3 <- left_join(df,df2)

df3 %<>% mutate(raised_mass = mean_mass_t * raised_numbers,
                scaled_mass = mean_mass_t * n.ijk)
df3 <- left_join(df3, caa_bio_index_2015_2020 %>% dplyr::select(1:4,12))
# summarise and get stratified numbers at age, their variances, the weighted mean mass of an individual, and the sum of the sample
df4 <- df3  %>%  group_by(year, quarter, division_group, gear_caa, f_age) %>% 
  dplyr::summarise(mean_mass_t = mean(mean_mass_t, na.rm = T),
                   N.jk = sum(raised_numbers, na.rm = T),
                   var.Njk = var(raised_numbers, na.rm = T),
                   sample_mass = sum(raised_mass, na.rm = T))

DF4 <- df3  %>%  group_by(index, f_age) %>% 
    dplyr::summarise(mean_mass_t = mean(mean_mass_t, na.rm = T),
                     N.jk = sum(n.ijk, na.rm = T),
                     var.a = var(n.ijk, na.rm = T),
                     var.l = var(lcat5, na.rm = T),
                     sample_mass = sum(scaled_mass, na.rm = T))
length(unique(df4$index))# should be 34
  
df4 %<>% as_tibble()

  # summarise  landings data
  mackerel_ziff %<>% group_by(year, quarter, division_group, gear_caa) %>% summarise(landings = sum(landings_t))
  mackerel_ziff %<>% as_tibble()
  landings <- left_join(mackerel_ziff, caa_bio_index_2015_2020 %>% dplyr::select(1:4,12)) %>% dplyr::filter(year>2014)
  landings <- landings %>% group_by(index) %>% summarise(landings = sum(landings,na.rm=T))
  
  # merge
  df5 <- full_join(df4 , landings) 
  df5 %<>% dplyr::filter(!is.na(landings))
  df5 %<>% mutate(var.Njk = replace_na(var.Njk, 0))
  
  DF5 <- full_join(DF4 , landings)
  DF5 %<>% dplyr::filter(!is.na(landings))
  DF5 %<>% mutate(var.a = replace_na(var.a, 0))
  # 
  # d6<-DF5 %>% 
  #     mutate(sample_catch_ratio = landings/sample_mass,
  #            naa = N.jk * sample_catch_ratio,
  #            Var.a = var.a * sample_catch_ratio,
  #            Var.l = var.l * sample_catch_ratio)
  # d6 %<>% group_by(index, f_age) %>% 
  #     dplyr::summarise(caa = sum(naa),
  #                     mean_mass = mean(mean_mass_t),
  #                     var.a = sum(Var.a),
  #                     var.l = sum(var.l))
  index<-caa_bio_index_2015_2020 %>% group_by(index) %>% dplyr::summarise(year = mean(year)) %>% arrange(year)
  # d6<-left_join(d6,index)
  # d7 <- d6 %>% group_by(year,f_age) %>% 
  #     summarise(CAA = sum(caa),
  #               Mean_mass = mean(mean_mass),
  #               Var.a = sum(var.a),
  #               Var.l = sum(var.l)) %>% 
  #     mutate(sd.N = sqrt(Var.a),
  #            sd.L = sqrt(Var.l), 
  #            CV.N = sd.N/CAA,
  #            CV.L = sd.L/CAA,
  #            CAW = CAA * Mean_mass)
  # d8 <- d7 %>% group_by(year) %>% dplyr::summarise(biomass = sum(CAW)) %>% mutate(source = "CAW")
  # ziff<- mackerel_ziff %>% 
  #     group_by(year) %>% 
  #     dplyr::summarise(biomass = sum(landings)) %>%
  #     dplyr::filter(year>2014) %>% 
  #     mutate(source = "landings")
  
  # calculate Weigth catch/weight of sample
  # mack_caa <- df5 %>% group_by(year, quarter, division_group, gear_caa) %>% 
  #   mutate(raising_factor = landings/sum(sample_mass))  
  mack_caa <- df5 %>% group_by(index) %>% 
    mutate(raising_factor = landings/sum(sample_mass))  
  
  MACCK_caa <- DF5 %>% group_by(index) %>% 
      mutate(raising_factor = landings/sum(sample_mass))  
 
  # calculate catch at age
  mack_caa %<>% 
    mutate(Njk = N.jk * raising_factor, 
           varNjk = var.Njk * raising_factor, 
           M.jk = Njk * mean_mass_t,
           sd.N = sqrt(varNjk),
           CV.N = sd.N/Njk)
  
  df6 <- MACCK_caa %>% 
      mutate(Njk = N.jk * raising_factor,
             Var.a = var.a * raising_factor, 
             Var.l = var.l * raising_factor)
  
  df6 <- left_join(df6,index)
  d7 <- df6 %>% group_by(year,f_age) %>% 
      dplyr::summarise(CAA = sum(Njk),
                       var.a = sum(Var.a),
                       var.l = sum(Var.l),
                       mean_mass = mean(mean_mass_t)) %>% 
      mutate(sd.N = sqrt(var.a),
             sd.L = sqrt(var.l), 
             CV.N = sd.N/CAA,
             CV.L = sd.L/CAA,
             CAW = CAA * mean_mass)
 
  
  d8 <-d7 %>% group_by(year) %>% dplyr::summarise(biomass = sum(CAW)) %>% mutate(source = "CAW")# similar
  d9<- bind_rows(d8,ziff)
  d9 %>% ggplot(aes(year,biomass,colour = source)) + geom_point()
  d10 <- d9 %>% pivot_wider(names_from = source,values_from = biomass) %>% mutate(diff = abs(CAW-landings))
  # YAY it works! :)
  # Now just make a simple index to get meta data back
  # index <- unique(caa_bio_index_2015_2020$index)
  # index <-read_csv("data/bio/caa_index.csv", # created in excel
  #          col_types = cols(X4 = col_skip(), X5 = col_skip(), 
  #                           X6 = col_skip(), X7 = col_skip(), 
  #                           X8 = col_skip(), X9 = col_skip()))
  # index<-caa_bio_index_2015_2020
  # merge back with index
  mack_caa <- left_join(mack_caa, index)
  save(mack_caa,file = "Rdata/caa_2015_2020.Rdata")
  
  ##################### 6) Catch at age  ####################
# caw by year
  
  mack_caa %>% group_by(year) %>% summarise(d = sum(M.jk,na.rm=T))
  
  mackerel_caa_2020 <- mack_caa %>% group_by(year, f_age) %>% summarise(N = sum(Njk))
  mackerel_caw_2020 <- mack_caa %>% group_by(year, f_age) %>% summarise(M = sum(M.jk))
  mackerel_cw_2020 <- mack_caa %>% group_by(year, f_age) %>% summarise(cw = mean(mean_mass_t)*1000)
  
 left_join(mackerel_caw_2020, mackerel_ziff %>% group_by(year) %>% summarise(landings = sum(landings))) %>% 
    ggplot(aes(year, M, fill = factor(f_age))) + 
    geom_col(colour = "black") + 
    scale_fill_viridis_d() + 
    geom_point(aes(year,landings), size = 3,shape = 21, fill = "red") + 
    theme_minimal(base_size = 12) + 
    annotate("text", x = 2015, y = 13000, label = "89 t", size = 3) +
    annotate("text", x = 2016, y = 13000, label = "380 t", size = 3) +
    annotate("text", x = 2017, y = 13000, label = "393 t", size = 3) +
    annotate("text", x = 2018, y = 13000, label = "489 t", size = 3) +
    annotate("text", x = 2019, y = 13000, label = "115 t", size = 3) +
    annotate("text", x = 2020, y = 13000, label = "112 t", size = 3) +
    labs(x = "Year", y = "Catch at age (t)")
  
  left_join(mackerel_caa_2020,ziff) %>% 
    ggplot(aes(year, N/1e6, fill = factor(f_age))) + 
    geom_col(colour = "black") + 
    scale_fill_viridis_d() + 
    theme_minimal(base_size = 12) +
    labs(x = "Year", y = "Catch at age (Millions)")

cn <- mackerel_caa_2020 %>% pivot_wider(names_from = f_age, values_from = N)
cn %<>% mutate(`8` = replace_na(`8`,0), `9` = replace_na(`9`,0), `10` = 0)

cw <- mackerel_cw_2020 %>% mutate(cw = round(cw,digits = 3)) %>%  pivot_wider(names_from = f_age, values_from = cw)
# fill in NAs with mean from below and above

cw %<>% mutate(`10` = NA)

cn <-as.matrix(cn)
rownames(cn) <-  seq(2015,2020,1)
cn <- cn[,2:11]
cn <- cn/1000

cw <-as.matrix(cw)
rownames(cw) <-  seq(2015,2020,1)
cw <- cw[,2:11]


save(cn, file = "Rdata/cn.Rdata")
save(cw, file = "Rdata/cw.Rdata")

write.csv(cn, file = "csv/cn.csv")
write.csv(cw, file = "csv/cw.csv")
# As per Baird 1983 & Gavaris^2 1983
# merge frequencies and proportions by stratification, length category, and age
# The proportion at age is calculated as:Number at age for a length group / Number of fish aged in that length group 
# The ALK for the time period is raised to the length distribution for that time period 
# Raised numbers at age by length group = Numbers at length * Proportion at age for that length
mackerel_bio

df2 <- mackerel_df %>% 
  group_by(year, quarter, division_group, gear_caa, f_age, lcat5) %>%
  dplyr::summarise(n.i = n())

df <- full_join(df2, df)
df$n.i <- replace_na(df$n.i, 0)
df %<>% dplyr::filter(N.i>0, n.i>0)

df %<>%  mutate(ni.pi = n.i * p.i, # stratified raised number at age by length group (i.e. number aged at length that are a given age)
               Ni.pi = N.i * p.i, # stratified catch at length for a given age
               var.Ni.pi = N.i^2 * (p.i * (1 - p.i) / n.i))  # Variance as per Gulland 1955. Full eqn is Var(Nipi = Ni^2 * Var(pi) + p^2 * Var(Ni)) ... though the authors note the second term can be dropped because it is negligeable when compared to the first term. Assumes binomial distribution
  dplyr::select(-index)

# Get precision estimates on catch at age. 
# The variance of the total number caught at any age is determined by summing the variance at each length group. nijkm
df_summary <- df %>% 
  group_by(year, f_age) %>% 
  dplyr::summarise(N = sum(Ni.pi, na.rm = T), Var.N = sum(var.Ni.pi,na.rm = T),
                   sd = sqrt(Var.N),
                   CV = (sd/N)*100) # as opposed to sd/mean this is the Bienaymé formula whereby the variance of the sum of uncorrelated random variables is the sum of their variances

df_summary %>% ggplot(aes(year, CV, fill = factor(f_age))) + geom_col(position = position_dodge(width = 2, preserve = "single"), colour = "black") + scale_fill_viridis_d() + scale_y_continuous(n.breaks = 35) + geom_hline(yintercept = 10)
# Very small CVs overall for each age and most are under the recommended 10% !!
# Furthermore, we have a high precision for our estimates of young fish, even age 0! Take that Newfoundland! Tinkers as far as the eyes can see!
df_summary %>% ggplot(aes(year, log(CV), fill = factor(f_age))) + geom_col(position = position_dodge(width = 2, preserve = "single"), colour = "black") + scale_fill_viridis_d() + geom_hline(yintercept = c(log(10), log(20)))

# Where is the variance coming from in recent years?
df_summary2 <- df %>% 
  group_by(quarter, division_group, f_age) %>% 
  dplyr::summarise(N = sum(Ni.pi, na.rm = T), Var.N = sum(var.Ni.pi,na.rm = T), # gavaris says this should be raised by the stratified (landed biomass/sum(sample)))
                   sd = sqrt(Var.N),
                   CV = (sd/N)*100) # as opposed to sd/mean this is the Bienaymé formula whereby the variance of the sum of uncorrelated random variables is the sum of their variances

# Low CVS for most area for all ages
df_summary2 %>% ggplot(aes(factor(f_age), log(CV), fill = factor(division_group))) + 
  geom_col(position = position_dodge(width = 2, preserve = "single"), colour = "black") +
  scale_fill_viridis_d() + scale_y_continuous(n.breaks = 10, limits = c(0,log(12))) + geom_hline(yintercept = log(10))

dt <- df_summary2 %>% expand(quarter, division_group, f_age)

df_summary2<-left_join(dt, df_summary2)
df_summary2 %>% ggplot(aes(factor(f_age), division_group, size = CV, fill = CV)) + 
  geom_point(shape = 21) +
  scale_fill_viridis_c() + 
  scale_size_binned_area(max_size = 10) +
  facet_wrap(vars(quarter))

##################### 6) Catch at age  ####################

# apply same methodology to get mean predicted masses per the defined groupings
# to get all masses for all length categories you have to merge the length frequencies with the bio data. It will just average out 
df2 <- mackerel_df %>% 
  group_by(year, quarter, lcat5, f_age) %>% 
  dplyr::summarise(mean_mass = mean(mass, na.rm = T))

# Merge the mean masses to the frequencies and proportions
df <- left_join(df, df2)

# Mass caught per length and age class raised to their proportions in the sample 
df %<>%  mutate(mass_t = mean_mass/1e6, 
                mass_raised = mass_t * ni.pi)

# Calculate numbers t age for all gears (sum of sampled numbers at age * (landed biomass/sum(sample)))
# Get mean mass, sample mass, and total frequencies by groupings
df  %<>% group_by(year, quarter, division_group, gear_caa, f_age) %>% 
  dplyr::summarise(mean_mass = mean(mass_t, na.rm = T), # mean mass of fish in sample
                   sample_mass = sum(mass_raised, na.rm = T), # mass of sample
                   tot_freq = sum(ni.pi, na.rm = T)) # raised number of fish in sample

mackerel_ziff %<>% as_tibble()
DF <- left_join(df, caa_index_2020)

# get the ratio by landings and sample mass then calculate catch-at-age and weight-at-age
DF %<>% group_by(year, quarter, division_group, gear_caa) %>% 
  dplyr::mutate(raise_factor = landings/sum(sample_mass)) %>% 
  group_by(year, quarter, division_group, f_age) %>% 
  dplyr::summarise(caa = raise_factor * tot_freq,
                caw = caa * mean_mass,
                var_N = var(tot_freq),
                var_N_raised = var_N * raise_factor,
                sd_N = sqrt(var_N_raised), 
                cv_N = sd_N/caa * 100) %>% 
  dplyr::filter(!is.na(caa))

summary(DF) # low CV, yay!

# Figures    

# higer order grouping
DF2 <- DF %>% 
  group_by(year, f_age) %>% 
  dplyr::summarise(caa = sum(caa,na.rm=T),caw = sum(caw,na.rm=T))

# compare caw and total landings to validate
df_land <- mackerel_ziff %>% 
  group_by(year) %>% 
  dplyr::summarise(landings = sum(landings_t, na.rm=T))

df_caw <- DF %>% 
  group_by(year,f_age) %>% 
  dplyr::summarise(caw = sum(caw,na.rm = T)) 

dfland <- left_join(df_caw,df_land)


# figure to compare - 2017 and 2019 off but this might be due to the gulf missing data... 
df_land %>% group_by()
dfland %>% 
  ggplot(aes(year, caw, fill = factor(-f_age))) + 
  geom_col() + 
  scale_fill_viridis_d() +
  # geom_point(aes(year, landings))
ylab("Landed biomass t") + xlab("Year") + theme_minimal(base_size = 16) 


save(DF, file = "Rdata/mackerel_catch_at_age_2020.Rdata")


# observed alk
# alk
alk.freq <- xtabs(~lcat5+f_age,data = maq_aged)
alk <- prop.table(alk.freq,margin = 1)
len <- xtabs(~lcat5, data = maq_aged)
# modelled alk
library(nnet)
mod1 <- multinom(f_age ~ lcat5, data = maq_aged %>% dplyr::filter(!is.na(sex)), maxit = 500)
lens <- seq(55, 595, 5)
alk.smoothed <- predict(mod1, data.frame(lcat5 = lens), type = "probs")
row.names(alk.smoothed) <- lens
round(alk.smoothed,3)
alkPlot(alk.smoothed, type = "area", pal = "jet")
alkPlot(alk, type = "area", pal = "jet")

# mean length at age and sd
mean_sd_obs <- alkMeanVar(alk, length ~ lcat5 + f_age, data = maq_aged, len.n = len) %>% as.data.frame() %>% mutate(CV = sd/mean)
# age dist with std errors
age_dist_obs <- alkAgeDist(alk, lenA.n = rowSums(alk.freq), len.n = len)


# chisquare test to test for differences in length freqs among different groups (Neumann and Allen 2007, p 123 in Ogle 2015)
maq_aged %<>% dplyr::filter(f_age!=0) %>% droplevels()
maq.LF <- xtabs(~year+lcat5, data = maq_aged)
chisq.test(maq.LF)
# Had to align these in a spreadsheet sadly... but the index idea is interesting
# 
# # need to create an index in each data set for merging... 
# # note that generally samples in q1 and q2 are always combined, quarter takes 1st precedent, then division for groupings however, in cases where there are samples for lines in 4RST or 4VWX these should be combined as they catch similar small mackerel (if bio samples not available for each strata)
# test <- mackerel_ziff %>%
#   mutate(index = case_when(year == 2015 & quarter %in% c("Q1", "Q2") & gear_caa %in% c("seines_nets_traps_weirs_misc","gillnets") ~ "a",
#                            year == 2015 & quarter %in% c("Q2") & gear_caa %in% c("lines") ~ "b", 
#                            year == 2015 & quarter %in% c("Q3") & gear_caa %in% c("seines_nets_traps_weirs_misc","gillnets") ~"c",
#                            year == 2015 & quarter %in% c("Q3") & gear_caa %in% c("lines") ~ "d",
#                            year == 2015 & quarter %in% c("Q4") & division %in% c("3K","3L") & gear_caa %in% c("seines_nets_traps_weirs_misc") ~ "e",
#                            year == 2015 & quarter %in% c("Q4") & division %in% c("4R","4T","4V","4W","4X","5Y") & gear_caa %in%  c("seines_nets_traps_weirs_misc","gillnets") ~ "f",
#                            year == 2015 & quarter %in% c("Q4") & gear_caa %in% c("lines") ~ "g",
#                            
#                            year == 2016 & quarter %in% c("Q1", "Q2") & division %in% c("4V","4W","4X") & gear_caa %in% c("seines_nets_traps_weirs_misc","gillnets") ~ "h",
#                            year == 2016 & quarter %in% c("Q2") & gear_caa %in% c("lines") ~ "i",
#                            year == 2016 & quarter %in% c("Q1", "Q2") & division %in% c("4T") & gear_caa %in% c("seines_nets_traps_weirs_misc","gillnets") ~ "j",
#                            year == 2016 & quarter %in% c("Q3") & division %in% c("3K","4R","4S","4T") & gear_caa %in% c("seines_nets_traps_weirs_misc","gillnets") ~ "k",
#                            year == 2016 & quarter %in% c("Q3") & division %in% c("4V","4X","4W", "5Y","4T") & gear_caa %in% c("seines_nets_traps_weirs_misc","gillnets","lines") ~ "l",
#                            year == 2016 & quarter %in% c("Q4") & division %in% c("3K","3L") & gear_caa %in% c("seines_nets_traps_weirs_misc","gillnets") ~ "m",
#                            year == 2016 & quarter %in% c("Q4") & division %in% c("4R","4T","4V","4W","4X") & gear_caa %in% c("seines_nets_traps_weirs_misc","gillnets") ~ "n",
#                            year == 2016 & quarter %in% c("Q4") & gear_caa %in% c("lines") ~ "o",
#                            
#                            year == 2017 & quarter %in% c("Q1", "Q2")  ~ "p",
#                            year == 2017 & quarter %in% c("Q3") & division %in% c("3K", "3P", "4R", "4S", "4T", "4V", "4W", "4X") & gear_caa %in% c("seines_nets_traps_weirs_misc","gillnets")  ~ "q",
#                            year == 2017 & quarter %in% c("Q3") & gear_caa %in% c("lines") ~ "r",
#                            year == 2017 & quarter %in% c("Q4") & gear_caa %in% c("seines_nets_traps_weirs_misc","gillnets") ~ "s",
#                            year == 2017 & quarter %in% c("Q4") & gear_caa %in% c("lines") ~ "t",
#                            
#                            year == 2018 & quarter %in% c("Q1", "Q2") ~ "u",
#                            year == 2018 & quarter %in% c("Q3") & division %in% c("2J","3K","3L","3O") ~ "v",
#                            year == 2018 & quarter %in% c("Q3") & division %in% c("4R","4S","4T","4V","4W", "4X") & gear_caa %in% c("seines_nets_traps_weirs_misc","gillnets") ~ "w",
#                            year == 2018 & quarter %in% c("Q3") & gear_caa %in% c("lines") ~ "x",
#                            year == 2018 & quarter %in% c("Q4") & division %in% c("3K", "4R", "4S", "4T", "4W", "4V", "4X") & gear_caa %in% c("seines_nets_traps_weirs_misc","gillnets") ~ "y",
#                            year == 2018 & quarter %in% c("Q4") & gear_caa %in% c("lines") ~ "z",
#                            
#                            year == 2019 & quarter %in% c("Q1", "Q2") ~ "aa",
#                            year == 2019 & quarter %in% c("Q3") & division %in% c("2J","3K") ~ "bb",
#                            year == 2019 & quarter %in% c("Q3","Q4") & gear_caa %in% c("lines") ~ "cc",
#                            year == 2019 & quarter %in% c("Q3","Q4") & gear_caa %in% c("seines_nets_traps_weirs_misc","gillnets") ~ "dd",
#                            
#                            year == 2020 & quarter %in% c("Q1", "Q2") & gear_caa %in% c("seines_nets_traps_weirs_misc","gillnets") ~ "ee",
#                            year == 2020 & quarter %in% c("Q2") & gear_caa %in% c("lines") ~ "ff",
#                            year == 2020 & quarter %in% c("Q3") & division %in% c("2J", "3K", "3L") ~ "gg",
#                            year == 2020 & quarter %in% c("Q3") & division %in% c("4R", "4S", "4T", "4W", "4V", "4X","5Z") & gear_caa %in% c("seines_nets_traps_weirs_misc","gillnets") ~ "hh",
#                            year == 2020 & quarter %in% c("Q3") & gear_caa %in% c("lines") ~ "ii",
#                            year == 2020 & quarter %in% c("Q4") & division %in% c("3K", "3L", "4T", "4W", "4V", "4X") ~ "jj"))
# 



# # index made from excel spreadsheet corresponding to binned landings that should match "test" above
# caa_bio_index_2015_2020 <- read_csv("data/bio/caa_bio_index_2015_2020.csv", 
#                                     col_types = cols(year = col_character()))
# # merge index with bio data
# caa_bio_index_2015_2020$division <- trimws(caa_bio_index_2015_2020$division)
# df$division <- trimws(df$division)
# aa <- left_join(df, caa_bio_index_2015_2020)
# 
# # merge fails for some reason so hard code.... ughghghgh
# aa[26:30,10] <- "e"
# aa[31:35,10] <- "f"
# aa[36:40,10] <- "g"
# aa[80:85,10] <- "m"
# aa[86:92,10] <- "n"
# aa[93:99,10] <- "o"
# aa[122:127,10] <- "s"
# aa[128:133,10] <- "t"
# aa[168:175,10] <- "z"
# aa[249:250,10] <- "jj"
# aa[221:259,10] <- "ii"
# 
# ### gah this better work now

# # corresponding ziff landings that should match the big case_when thing above
# ziff_df <- data.frame(index = c("a","b","c","d",'e',"f","g","h","i",'j',"k","l","m",'n',"o",'p',"q",'r',"s",'t','u','v','w','x','y',"z","aa","bb","cc","dd",'ee','ff','gg','hh',"ii","jj","kk"), landings = c(679.666, 53.266, 737.841,1505.99, 262.106,814.104,227.306,404.497,5.009,948.076,970.406,1468.549,2384.854,1604.912,269.114,1327.319,4388.567,1371.495,2576.206,119.772,878.345,2870.287,1260.886,832.659,2657.815,73.203,1046.743,4689.953,362.32,2527.139,899.813,21.903,704.487,665.214,231.655,3284.132,14.343))
# 
# # merge datasets
# # OK it works but gulf data still missing.... but let's see if it works first
# DF <- left_join(aa, ziff_df)


# try testing just a year by quarter alk, observed and modelled


us_eez_u
