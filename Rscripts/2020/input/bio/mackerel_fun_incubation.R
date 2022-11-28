# Mackerel egg incubation functions to assist in the stock assessment of Atlantic mackerel (northern contingent)
# by: Andrew D. Smith 2020


# Incubation 

# Atlantic mackerel (Scomber scombrus) egg incubation as functions of sea water temperature (C)

# All incubation equations take the form of power functions: I = a*temperature^b; log(I) = log(a) + b * temperature); I = exp(b * log(temperature) + a)
# These equations are simplifications of equations proposed by Belehradex (1930, 1935) whereby I = a*(T-a)^ b and not I = a*T^b

# For the Northwest Atlantic mackerel, the mean temperature of the first 10 m of the sea surface are used as derived from CTD casts.

## 1) Worley 1933
### Worley, L. G. 1933.  Development of the egg of the mackerel at different constant temperatures. J. Gen. Physiol. 16:841-857.

#' Mackerel egg incubation - Worley 1933 - NWA mackerel
#' The  spawning  season  of  the  mackerel  at  Woods  Hole,  during  1931 and  1932  embraced  the  period  from  about  May  15  to  June  20.  The sea  water  temperature  at  Woods  Hole  during  this  period  increased from  about  11.5  °  to  18°C. Early  June  experiments  demonstrated  that  mackerel  eggs  fail  to cleave  normally  at  temperatures  as  low  as  10  °,  although  they  develop in  a  typical  manner  at  11°C.  Embryos  which  were  held  at  a  thermostat  temperature  of  8  °  displayed  no  development  whatever,  although eggs  were  kept  under  observation  at  this  temperature  for  approximately  48  hours.  The  nuclei  of  eggs  held  at  8.5  °  underwent  division as  many  as  four  times,  so  that  16  nuclei  resulted  in  each  egg,  but  there was  no  cytoplasmic  division.  
#' DOI: 10.1085/jgp.16.5.841 
#' 
#' Abstract:
#' 1. Mackerel egg development was followed to hatching at constant temperatures of 10 degrees , 11 degrees , 12 degrees , 13 degrees , 14 degrees , 15 degrees , 16 degrees , 17 degrees , 18 degrees , 19 degrees , 20 degrees , 21 degrees , 22 degrees , and 24 degrees C. Experiment showed that typical development could be realized only between 11 degrees and 21 degrees . 2. The length of the developmental period increases from 49.5 hours to 207 hours when the temperature is lowered from 21 degrees to 10 degrees C. 3. The calculated micro for the development of the mackerel egg is about 19,000 at temperatures above 15 degrees and approximately 24,900 for temperatures below 15 degrees C. 15 degrees is, apparently, a critical temperature for this process. 4. The calculated values of micro for eight stages of development preceding hatching, i.e. 6 somites, 12 somites, 18 somites, 24 somites, three-quarters circles, four-fifths circles, five-sixths circles, and full circles, are essentially the same as the micro's for hatching, indicating that the rate of differentiation up to hatching is governed by one process throughout. Critical temperatures for these stages approximate 15 degrees . 5. The total mortality during the incubation period was least at 16 degrees C. where it amounted to 43 per cent. At temperatures above and below this there was a steady increase in the percentage of mortality which reached 100 per cent at 10 degrees and 21 degrees .
#' 
#' Equation in days originaly: I = (exp(-1.87*log(t)+9.67))* 0.0417;0.0417 days  = 1 hour
#' @param t temperature in degrees celsisus
#' @return incubation time in hours
#' @export
#' @examples
#'I_worley(13)
I_worley <- function(t){
  I = (exp(-1.87 * log(t) + 9.67)) 
  I
}

### 2) Lockwood et al., 1977
#### Lockwood, S. J., J. H. Nichols, and S. H. Coombs. 1977. The development rates of mackerel (Scomber scombrusL.) eggs over a range of temperatures. ICES CM 1977/J:13, 13 p 

#' Mackerel egg incubation - Lockwood 1977 - NEA mackerel
#' Bay of Biscay
#' @param t t = temperature in degrees Celsisus. 
#' @return incubation time in hours
#' @export
#' @examples I = I_Lockwood(10)
I_lockwood <- function(t){
  I = exp(-1.614 * log(t) + 7.759)
  I
}

### 3) Lanctot 1980
# The development and early growth of embryos and larvae of the Atlantic mackerel, Scomber scombrus L., at different temperatures / by Marthe Lanctot
# Eggs from St. George's Bay Nova Scotia. Laboratory study with temperatures ranging from 8 - 23 C
# Eggs from surface tows on June 21, 1978 - temp = 9.5 C
# Also from artificially fertilized eggs by capturing mature male and female
# claims that lockwood got the eqn wrong from Belehradex (1930, 1935) whereby I = a*(T-a)^ b and not I = a*T^b

#' Mackerel egg incubation - Lanctot 1980 - NWA mackerel - 1
#' Lanctot's fitted incubation equation rearranged from 1/hathing time  = -0.00694 + 0.00108*t, r^2 = 0.974
#' @param t temperature in degrees C
#' @return incubation time in hours
#' @export
#' @examples I_lanctot1(10)
I_lanctot1 <- function(t){
  I  = 1/(-0.00694 + (0.00108 * t))
  I
}

#' Mackerel egg incubation - Lanctot 1980 - NWA mackerel - 2
#' As calculated by Lanctot 1980 - this is a refit of Worley's 1933 data r^2 = 0.999 using the formula proposed by Belehradex (1930, 1935)
#' @param t temperature in degrees Celsisus
#' @return incubation time in hours
#' @export
#' @examples I_lanctot2(10)
I_lanctot2 <- function(t){
  I = 182120.99 * (t + 4.8)^-2.516
  I
}

#' Mackerel egg incubation - Lanctot 1980 - NWA mackerel - 3
#' As calculated by Mendiola et al 2006 to get I at the average age of eggs at hatching at temperature t
#' @param t temperature in degrees C
#' @return incubation time in hours
#' @export
#' @examples I_lanctot3(10)
I_lanctot3 <- function(t){
  I = exp(-2.0318 * log(t) + 10.2280)
  I
}

### 4) Mendiola et al., 2006
### Mendiola, D., P. Alvarez, U. Cotano, E. Etxebeste, and A. M. de Murguia. 2006. Effects of temperature on development and mortality of Atlantic mackerel fish eggs. Fish. Res. 80:158-168.
#' Mackerel egg incubation - Mendiola et al., 2006 - NEA mackerel
#' for egg stage 1b which is roughly equivalent to our stage 1 (see ICES documents and Girard 2000)
#' @param t temperature in degrees Celsius
#' @return incubation time in hours
#' @export
#' @examples 
#' I_mendiola(10)
I_mendiola <- function(t){
  I = exp(-1.313 * log(t) + 6.902)
  I
}

# Mendiola also calculates average age at hatch ~ temperature for previous studies
# average age of eggs at hatching at temp C 
# Worley 1933: I = 9.7151*t^-1.8932
# Lanctot 1980: I = 10.2280*t^-2.0318
# Lockwood et al 1977: I = 9.3797 * t^-1.7647
# Mendiola 2006: I = 8.8767*t^-1.5790

# for a vignette
# Comparison of curves
# library(magrittr);library(tidyverse)
# Temp <- c(seq(5,30,0.5))
# Temp <- as.data.frame(Temp)
# Temp %>% mutate(I_W = I_worley(Temp),
#                 I_L = I_Lockwood(Temp), # notice that this is the same as I_la3
#                 I_la = I_lanctot(Temp),
#                 I_la2 = I_lanctot2(Temp),
#                 I_la3 = I_lanctot3(Temp),
#                 I_M = I_mendiola(Temp)) %>% 
#   pivot_longer(cols = 2:7, names_to = "model", values_to = "Incubation") %>% 
#   ggplot(aes(Temp, Incubation/24, colour = model))+geom_line(size = 4, alpha = 0.7)+geom_point()+
#   facet_wrap(vars(model))
