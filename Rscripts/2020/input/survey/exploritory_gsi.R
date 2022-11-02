#####################  Load libraries  ####################

packages = c('tidyverse','FSA','magrittr','nnet','broom', 'purrr', 'nnet','readxl','lubridate')
invisible(lapply(packages, function(x) {if (!require(x, character.only = T)) {install.packages(x);require(x)}}))

#####################  Load data  ####################

# Combined carbio and length frequency data. Made with read_mackerel_bio_lf.R. Contains commercial fish samples and some data from fisheries independant surveys and sporadic research projects. Since 2000, mackerel biological data tagged as "research" has largely been added to an excel sheet. It includes samples from BIOs winter/spring survey in the Gulf of Maine/Georges Bank/Scotian Shelf bottom trawl surveys (1970-present - bio data prior to 2000 in various research documents, manuscripts, & theses or in the Maritimes database - see their github page)
load("Rdata/mackerel_bio.Rdata")


# Biological samples since 2000 stored in an active excel file. is scheduled to be coded into the main database with the appropriate coding. some tweaking of the excel file column headers, cell formats, and some filling in of NAs to facilitate loading in R (i.e. the .xlsx file here is modified from the original one on the S/Pelagiques/maqueraeau network files) mackerel samples from needler and teleost missions (mostly maritimes)
mackerel_bio_research <- read_excel("data/bio/mackerel_bio_research.xlsx", col_types = c("text", "text", "text", 
                                                                                         "text", "text", "text", "text", "text", 
                                                                                         "text", "text", "text", "numeric", 
                                                                                         "numeric", "numeric", "text", "text", 
                                                                                         "numeric", "text", "text", "text"), skip = 3)

glimpse(mackerel_bio_research)
##################### Data wrangling  ####################

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

mackerel_bio_research %>%
    dplyr::select(year, month, day, mission, set, division, length, mass, sex, maturity, mass_gonad, age, quarter, division_group) 
# Add length category variable (5mm as per current two stage sampling protocol (i.e. ~ 100-200 fish sampled for length frequencies (sortie.dat) in proportion to landings and 2 fish per 5mm length class sent to IML to be processed for biological trait data (Maquereau_carbio.dat))) 
mackerel_bio %<>% 
    mutate(lcat5 = FSA::lencat(length, w = 5))



# calculate GSI and rename maturity variables
mackerel_bio %<>% dplyr::filter(!is.na(mass), !is.na(mass_gonad), mass > 0) %>% 
    mutate(year = as.factor(year), 
           maturity = as.factor(maturity),
           gsi = mass_gonad / mass * 100,
           maturity = recode(maturity, '1' = "1_immature_virgin", '2' = "2_immature", '3' = "3_maturing", '4' = "4_maturing", '5' = "5_ripe", '6' = "6_spawning", '7' = "7_spent", '8' = "8_recovering"), 
           maturité = recode(maturity, '1_immature_virgin' = "_immature_vierge", '2_immature' = "2_immature", '3_maturing' = "3_maturation", '4_maturing' = "4_maturation", '5_ripe' = "5_mature", '6_spawning' = "6_fraie", '7_spent' = "7_fin_de_ponte", '8_recovering' = "8_récupération")) %>% 
    dplyr::select(-lat_deg, -lat_min, -lat_sec, -lon_deg, -lon_min, -lon_sec, -trip_num, -fish_id, -mesh_size, -depth, -times_measured, -district, -zone_num, -landed_state, -freq, -est_mass_landed, -mass_sampled,-sample,sample_id)

mackerel_bio %>% ggplot(aes(doy, gsi, colour = f_age )) + geom_point()+facet_wrap(vars(year), scales = "free_y")


is.outlier <- function(x, coef=1.5, na.rm = TRUE, ...) {
    qnt <- quantile(x, probs = c(.25, .75), na.rm = na.rm, ...)
    H <- coef * IQR(x, na.rm = na.rm)
    x < (qnt[1] - H) | x > (qnt[2] + H)
} #with coef=3 for extremes


mackerel_bio %<>% group_by(year, division_group, doy) %>% mutate(gsi_outlier = is.outlier(gsi))
mackerel_bio %>% ggplot(aes(doy, gsi, colour = gsi_outlier)) + geom_point()


mackerel_bio %>% 
    ggplot(aes(doy, gsi, colour = division_group )) +
    geom_point() + 
    facet_wrap(vars(year), scales = "free_y")+ scale_color_viridis_d() +
    theme_minimal() + scale_x_continuous(limits = c(100, 365))

mackerel_bio %>% dplyr::filter(year %in% c(1982:1995)) %>% 
    ggplot(aes(doy, gsi, colour = division_group )) +
    geom_point() + 
    facet_wrap(vars(year), scales = "free_y") + scale_color_viridis_d() +
    theme_minimal() + scale_x_continuous(limits = c(100, 365))

mackerel_bio %>% dplyr::filter(year %in% c(1982:1995)) %>% 
    ggplot(aes(doy, gsi, colour = division_group )) +
    geom_point() + 
    facet_wrap(vars(year), scales = "free_y") + scale_color_viridis_d() +
    theme_minimal() + scale_x_continuous(limits = c(100, 365))


mackerel_bio %>% dplyr::filter(year %in% c(1982:1995)) %>% 
    ggplot(aes(doy, gsi, colour = division)) +
    geom_point() + 
    facet_wrap(vars(year), scales = "free_y") + scale_color_viridis_d() +
    theme_minimal() + scale_x_continuous(limits = c(100, 365))

mackerel_bio %>% dplyr::filter(year %in% c(1982:1995)) %>% 
    ggplot(aes(doy, gsi, colour = f_age )) +
    geom_point() + 
    facet_wrap(vars(year), scales = "free_y") + scale_color_viridis_d() +
    theme_minimal() + scale_x_continuous(limits = c(100, 365))

mackerel_bio %>% dplyr::filter(year == 1983) %>% 
    ggplot(aes(doy, gsi, colour = gear_name )) +
    geom_point() + 
    facet_wrap(vars(year), scales = "free_y") + scale_color_viridis_d() +
    theme_minimal() + scale_x_continuous(limits = c(100, 300))

