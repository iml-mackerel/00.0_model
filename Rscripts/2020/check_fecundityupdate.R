# 25/02/2021 19:31h day 1 of 2021 assessment

# Fecundity
# Issue: weird drop for younger ages in recent years below time series mean
# Doesn't seem related to gonad mass but number of samples was low

# In the end I figured out why. Fecundity was the first of the input data sets that I completed. 
# Following the intrants in early december. Since then the number of mackerel analysed has increased quite a bit so more data from all ages.
# But also, data from 2018 and 2019 were also being updated (both in terms of being measured but also data entry and integrating research data base into main database - mind you when I say research it is a catch all and many commercial samples were there)
# Also note that Francois Gregoire never just did june july and 4T for fecundity. Just proportion mature. And in the end, the equations would weigh down the existence of any less present ages in the population as well as by their proportion mature


# So. Two strategies: 
# 1) Say the best available science at the time gave these results and in any event sensitivity analyses show that a) not sensitive to fecundity, b) various smoothing and/or using flat lined 2018 fecundity estimates using Pelletier's fec ~ age eqn gives similar results
# 2) Re run model with updated fecundity data and smoothers and show that it gives the same though slightly different results. 
smoothmatrix(fec_old, subset = 0:nrow(fec_old), smooth = 0, max = 8e+5, plot = TRUE) 
af0 <- fec_old
bf1 <- fec
df25 <- smoothmatrix(fec, subset = 0:nrow(fec), smooth = 0.25, max = 8e+5, plot = TRUE) 
ff50 <- smoothmatrix(fec, subset = 0:nrow(fec), smooth = 0.50, max = 8e+5, plot = TRUE) 
hf75 <- smoothmatrix(fec, subset = 0:nrow(fec), smooth = 0.75, max = 8e+5, plot = TRUE) 

cf1_new <- fec_update
ef25_new <- smoothmatrix(fec_update, subset = 0:nrow(fec), smooth = 0.25, max = 8e+5, plot = TRUE) 
gf50_new <- smoothmatrix(fec_update, subset = 0:nrow(fec), smooth = 0.50, max = 8e+5, plot = TRUE) 
if75_new <- smoothmatrix(fec_update, subset = 0:nrow(fec), smooth = 0.75, max = 8e+5, plot = TRUE)

af0 %<>% as.data.frame(f0) %>% mutate(year = seq(1968,2020,1)) %>% pivot_longer(cols = 1:10, names_to = "age") %>% transmute(year = year, age = age, fecundity = value, df = "af0")
bf1 %<>% as.data.frame(f1) %>% mutate(year = seq(1968,2020,1)) %>% pivot_longer(cols = 1:10, names_to = "age") %>% transmute(year = year, age = age, fecundity = value, df = "bf1")
df25 %<>% as.data.frame(f25) %>% mutate(year = seq(1968,2020,1)) %>% pivot_longer(cols = 1:10, names_to = "age") %>% transmute(year = year, age = age, fecundity = value, df = "df25")
ff50 %<>% as.data.frame(f50) %>% mutate(year = seq(1968,2020,1)) %>% pivot_longer(cols = 1:10, names_to = "age") %>% transmute(year = year, age = age, fecundity = value, df = "ff50")
hf75 %<>% as.data.frame(f75) %>% mutate(year = seq(1968,2020,1)) %>% pivot_longer(cols = 1:10, names_to = "age") %>% transmute(year = year, age = age, fecundity = value, df = "hf75")

cf1_new %<>% as.data.frame(f1_new) %>% mutate(year = seq(1968,2020,1)) %>% pivot_longer(cols = 1:10, names_to = "age") %>% transmute(year = year, age = age, fecundity = value, df = "cf1_new")
ef25_new %<>% as.data.frame(f25_new) %>% mutate(year = seq(1968,2020,1)) %>% pivot_longer(cols = 1:10, names_to = "age") %>% transmute(year = year, age = age, fecundity = value, df = "ef25_new")
gf50_new %<>% as.data.frame(f50_new) %>% mutate(year = seq(1968,2020,1)) %>% pivot_longer(cols = 1:10, names_to = "age") %>% transmute(year = year, age = age, fecundity = value, df = "gf50_new")
if75_new %<>% as.data.frame(f75_new) %>% mutate(year = seq(1968,2020,1)) %>% pivot_longer(cols = 1:10, names_to = "age") %>% transmute(year = year, age = age, fecundity = value, df = "if75_new")

fecundity_df_compare <- bind_rows(af0,bf1,df25,ff50,hf75,cf1_new,ef25_new,gf50_new,if75_new)

fecundity_df_compare %<>%
    mutate(age = fct_relevel(age, "1","2","3","4","5","6","7","8","9",'10'))


supp.labs <- c("2019 - last assessment", "2021 0% old figure", "2021 0%", "2021 25% old figure", "2021 25%", "2021 50% old figure", "2021 50%", "2021 75% old figure", "2021 75%")
names(supp.labs) <- c("af0", "bf1", "cf1_new", "df25", "ef25_new", "ff50", "gf50_new", "hf75", "if75_new")


p1 <- fecundity_df_compare %>% 
    ggplot(aes(year, fecundity, colour = age)) +
    geom_line(size = 1) +
    theme_minimal(base_size = 14) +
    scale_colour_viridis_d(name = "Age") +
    labs(y = "fecundity (# of eggs)", x = "Year")

# Create the plot
p1 + facet_wrap(vars(df), 
                labeller = labeller(df = supp.labs))

p2 <- fecundity_df_compare %>% dplyr::filter(df %in% c("af0","bf1","cf1_new","ff50", "gf50_new")) %>% 
    ggplot(aes(year, fecundity, colour = age)) +
    geom_line(size = 1) +
    theme_minimal(base_size = 14) +
    scale_colour_viridis_d(name = "Age") +
    labs(y = "fecundity (# of eggs)", x = "Year")

# Create the plot
p2 + facet_wrap(vars(df), 
                labeller = labeller(df = supp.labs)) +
    theme(legend.position = c(0.75,0.2))

