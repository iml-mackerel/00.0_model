# Daily egg production (DEP)
# DEP = abundance (stage 1 and 5 eggs in Numbers/m^2) * (24/I)
#' Calculate dailey egg production (DEP)
#' 
#'
#' @param Nm2 
#' @param I 
#' I = Incubation period (hours) derived from one of the above incubation functions (i.e. Lockwood, Worley, Mendiola, etc.)
#' @return
#' @export
#'
#' @examples
#' DEP = DEP(Nm2, I) - can be done for single values but generally you will be applying this acrros many observations
DEP <- function(Nm2, I){
  DEP = Nm2 * (24*I)
  DEP
}

## ) Richardson et al., 2020
#### Richardson, D.E., Carter, L., Curti, K.L., Marancik, K.E., Castonguay, M. 2020. Changes in the spawning distribution and biomass of Atlantic mackerel (Scomber scombrus) in the western Atlantic Ocean over 4 decades Fish. Bull. 118:120-134 (2020). doi: 10.7755/FB.118.2.2
#
# PD = (exp(a + (b * D)) / 1 + exp(a + (b * D)))^(1/c)
# 3 parameter skewed logistic funciton representing spawning seasonality of mackerel based on multidecadal data of mackerel larvae abundance at age (Richardson et al., 2020)
# a = -7.3110 = estimated model param
# b = 0.0694 = estimated model param
# c = 0.08282 = estimated model param
# D = doy = day of year (Julian)
# PD = cummulative proportion of spawning at day of year D = average for time series as opposed to year-specific
#' Cummulative proportion of spawning at day of year
#'
#' @param doy 
#' Julian day of year
#' @return
#' @export
#'
#' @examples 
#' mean_cum_prop_spawn(173)
mean_cum_prop_spawn <- function(doy){
  PD = (exp(-7.3110 + (0.0694 * doy)) / 1 + exp(-7.3110 + (0.0694 * doy)))^(1/0.08282)
  PD
}

# Annual (Total) egg production (AEP) aka (TEP)
# AEP = DEP/(P(D+0.5)-P(D-0.5))
# AEP = annual egg production
# DEP = daily egg production
# P = density of eggs (N/m^2)
# For the egg surveys in Canada and the United States, total annual egg production (AEP) is calculated by dividing the daily egg production (DEP) by the proportion of eggs estimated to be spawned on the mean day (D) stations were sampled on the survey:
# hmm Richardson et al did not multiply by the Area of the survey, in their SSB eqn they divide by S (PD for them) first then calculate SSB

# SSB = (AEP * W)/(f*R*10^6)
# SSB = Spawning stock biomass
# AEP = Annual egg production
# W = mean mass (grammes) of spawning fish (should be females only)
# f = mean female fecundity
# R = proportion female (should be by mass)
# 10^6 conversion factor g -> t





# Egg mortality correction factor - Richardson et al 2020
# We used egg mortality estimates of 0.20/d and 0.55/d in our sensitivity analyses. Stage-1 egg abundances were scaled up by using a correction factor that corresponded to the number of eggs spawned 
# (i.e., the start of stage 1) versus the average number of stage-1 eggs at any point in time from spawning to the transition from stage 1 to stage 2
# correction factor = (I/24)/(1/-Z)*(exp(-Z*(I/24))-1)
# Z = daily mortality rate; I = incubation time in hours
# This correction factor is applied on a sample-  by- samplebasis and varies on the basis of temperature- dependent incubation times
