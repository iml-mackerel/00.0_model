#########
##
## Northwest Atlantic (NWA) MACKEREL DATA - NAFO
##  https://www.nafo.int/Data/Catch-Statistics
##  From the NAFO statlant 21A and 21B databases
##
## By Andrew Smith August 2018
##  - Update Fall and Winter 2020  for 2021 assessment
##  - Caution, NAFO changed their website extraction method, values say kg but they are actually metric tonnes
##  - Caution, 21a (annual) and 21b (monthly) databases differ somewhat in later years, particularly for US landings.
### - Caution, NAFO data are not the same as DFO data despite coming from the same source. For an analysis see DFO ResDoc 2000/021 chapters 1-3
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

# Annual data
nafo_21a <- read_csv("C:/Users/SmithAND/Documents/My Stocks/Mackerel/mackerel_sar_2021/mackerel_model_2021/mackerel_assessment_input/csv/nafo_statlant21a.csv")

# Monthly data. compiled from earlier scripts. caution, quality not great in more recent years. USA stopped contributing in the early 1990s
load("~/My Stocks/Mackerel/mackerel_sar_2021/mackerel_model_2021/mackerel_assessment_input/Rdata/nafo_mackerel_21B.Rdata")

nafo_21b <- NAFO_MACKEREL_21B
rm(NAFO_MACKEREL_21B)

#####################  CLEAN DATA  ####################

# 21A database (Annual)

## Subset
nafo_mack_21a <- nafo_21a %>% dplyr::filter(`Species Name` == "ATLANTIC MACKEREL - MAC")
rm(nafo_21a)

## Exclusive Economic Zone (or what would later become that). 
## For simplicity I am assuming the EEZ extends past the 200 nm limit if the bounds of the nafo divsions extend beyond it
sort(unique(nafo_mack_21a$Division))

nafo_mack_21a %<>%
    mutate(eez = ifelse(
        Division %in% c(
            "5NK",
            "5Y",
            "5Z",
            "5ZE",
            "5ZU",
            "5ZW",
            "6NK",
            "6A",
            "6B",
            "6C",
            "6D",
            "6G"
        ),
        "usa_eez",
        ifelse(
            Division %in% c("UNKNOWN",
                            "NOT REPORTED",
                            "OUTSIDE NAFO AREA"),
            "unknown",
            "canada_eez"
        )
    ))

## Simplify country groupings
nafo_mack_21a %<>% mutate(nation = fct_collapse(
    Country,
    "Foreign" = c("FRANCE ST. PIERRE ET MIQUELON - FRA-SP",
                  "RUSSIA - RUS",
                  "CUBA - CUB",
                  "JAPAN - JPN",
                  "LITHUANIA - E/LTU",
                  "NETHERLANDS - E/NLD",
                  "UNION SOVIET SOCIALIST REPUBLICS - SUN",
                  "BULGARIA - BGR",
                  "GERMAN DEMOCRATIC REPUBLIC - DDR",
                  "POLAND - E/POL",
                  "ITALY - E/ITA",
                  "SPAIN - E/ESP",
                  "FRANCE MAINLAND - E/FRA-M",
                  "MEXICO - MEX(NC)",
                  "ROMANIA - ROM",
                  "FEDERAL REPUBLIC OF GERMANY - E/DEU",
                  "IRELAND - E/IRL",
                  "ISRAEL - ISR(NC)"),
    "USA" = "UNITED STATES OF AMERICA - USA",
    "Canada" = c("CANADA MARITIMES - CAN-M",
                 "CANADA NEWFOUNDLAND - CAN-N",
                 "CANADA GULF - CAN-G",
                 "CANADA SCOTIA - FUNDY - CAN-SF",
                 "CANADA QUEBEC - CAN-Q",
                 "CANADA MARITIMES & QUEBEC - CAN-MQ")
))

## Subset and rename variables for simplicity
nafo_mack_21a %<>% 
    transmute(year = Year, country = Country, nation = nation, eez = eez, division = Division, landings_t = Kg)

## Subset to just Canadian EEZ
nafo_mack_can <- nafo_mack_21a %>% dplyr::filter(eez == "canada_eez") %>% 
    dplyr::select(year, nation, landings_t) %>% 
    group_by(year, nation) %>% 
    dplyr::summarise(landings_t = sum(landings_t)) %>% 
    pivot_wider(names_from = nation, values_from = landings_t)

## Calculate lotal landings in Canadian EEZ from all sources
nafo_mack_can %<>% mutate(landings_nafo_t = sum(Canada, Foreign, USA, na.rm = T))

# 21B database (Annual)

## look at distribution of monthly landings
glimpse(nafo_21b)

## make life easier
names(nafo_21b) <- tolower(names(nafo_21b))
sort(unique(nafo_21b$nafo_division))

##  Group by EEZ
nafo_21b %<>%
    mutate(eez = ifelse(
        nafo_division %in% c(
            "5Z",
            "5ZE",
            "5Y",
            "5NK",
            "5ZU",
            "5ZW",
            "6A",
            "6B",
            "6C",
            "6NK",
            "6G",
            "6D"
        ),
        "usa_eez",
        ifelse(nafo_division %in% c("NK"),
               "unknown",
               "canada_eez")
    ))

## Subset
nafo_21b %<>% dplyr::select(year, eez, nation, month, nafo_division, gear_name, landings_t)

## Change variables
nafo_21b %<>% mutate(division = str_sub(nafo_division,1,2))

#####################  SUMMARIZE DATA  ####################

nafo_21b_summary <- nafo_21b %>% 
    group_by(year, eez) %>% 
    dplyr::summarise(landings_t = sum(landings_t,na.rm = T)) %>% 
    pivot_wider(names_from = eez, values_from = landings_t)

nafo_21b %<>% 
    group_by(year, eez, division, nation, month) %>% 
    dplyr::summarise(landings_t = sum(landings_t,na.rm = T))

nafo_21b %<>% 
    mutate(month = fct_relevel(month, "Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec",'NK'))

#####################  FIGURES  ####################

nafo_21b %>% dplyr::filter(eez == "usa_eez",year > 1965, year < 1985) %>% 
    ggplot(aes(year,landings_t)) +
    geom_col(fill = "blue") + facet_wrap(vars(month)) + xlab("Year") + ylab("Landings t") +
    theme_minimal(base_size = 12) + labs(title = "Landings in US EEZ")

nafo_21b %>% dplyr::filter(eez == "canada_eez",year > 1965, year < 1985) %>% 
    ggplot(aes(year,landings_t)) +
    geom_col(fill = "red") + facet_wrap(vars(month)) + xlab("Year") + ylab("Landings t") +
    theme_minimal(base_size = 12) + labs(title = "Landings in Canadian EEZ")

nafo_21b %>% dplyr::filter(year > 1965, year < 1985) %>% 
    ggplot(aes(year, landings_t, fill = eez)) +
    geom_col() + facet_wrap(vars(month)) + xlab("Year") + ylab("Landings t") +
    theme_minimal(base_size = 12) + labs(title = "Landings in the NWA") + scale_fill_manual(values = c("usa_eez"= "blue","canada_eez"= "red"))

#####################  COMPARE 21A AND 21B DATA  ####################

## Unknown EEZ in 21A are a mix of Canadian and US landings
nafo_mack_21a %<>% mutate(eez2 = ifelse(eez == "unknown"&nation == "Canada", "canada_eez",
                                        ifelse(eez == "canada_eez", "canada_eez", "usa_eez"))) %>% 
    transmute(year = year, country = country, nation = nation, eez = eez2, division = division, landings_t = landings_t)

## All unknown EEZ in 21B are landed by Canada, so they are most likely from the canadian eez
nafo_21b %<>% mutate(eez = fct_collapse(eez,
    "canada_eez" = c("canada_eez", "unknown"),
    "usa_eez" = c("usa_eez")))

## Save
save(nafo_mack_21a, file = "./Rdata/nafo_mack_21a.Rdata")
save(nafo_mack_can, file = "./Rdata/nafo_mack_can.Rdata")
write.csv(nafo_mack_can, file = "./csv/nafo_mack_can.csv")

## Summarize
df_21a <- nafo_mack_21a %>% 
    group_by(year, eez) %>% 
    dplyr::summarise(landings_t = sum(landings_t, na.rm = T)) %>% 
    as_tibble() %>% 
    pivot_wider(names_from = eez, values_from = landings_t) 

df_21b <- nafo_21b %>% 
    group_by(year, eez) %>% 
    dplyr::summarise(landings_t = sum(landings_t, na.rm = T)) %>% 
    as_tibble() %>% 
    pivot_wider(names_from = eez, values_from = landings_t) 

df_compare <- full_join(df_21a, df_21b, by = c("year"))

cor.test(df_compare$canada_eez.x,df_compare$canada_eez.y) # 9883524 
plot(df_compare$canada_eez.x,df_compare$canada_eez.y)

cor.test(df_compare$usa_eez.x,df_compare$usa_eez.y) # 1.00

df_compare %<>% mutate(can_match = ifelse(canada_eez.x == canada_eez.y, "yes", "no"),
                      us_match = ifelse(usa_eez.x == usa_eez.y, "yes", "no"))

df_compare %>% ggplot(aes(year, canada_eez.x)) +
    geom_col() +
    geom_point(aes(year, canada_eez.y, colour = can_match)) + 
    theme_minimal(base_size = 16) + 
    xlab("Year") + ylab("landings in Canada's EEZ") + 
    labs(title = "Comparison of NAFO's 21A and 21B databases",
         subtitle = "Columns are annual (21A), points are monthly (21B)")

df_compare %>% ggplot(aes(year, usa_eez.x)) +
    geom_col() +
    geom_point(aes(year, usa_eez.y, colour = us_match)) + 
    theme_minimal(base_size = 16) + 
    xlab("Year") + ylab("landings in the USA's EEZ") + 
    labs(title = "Comparison of NAFO's 21A and 21B databases",
         subtitle = "Columns are annual (21A), points are monthly (21B)")

# Conclusion: Montlhly data unreliable after ~ 2000
