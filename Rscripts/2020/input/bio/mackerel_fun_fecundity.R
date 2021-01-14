# Mackerel fecundity equation functions to assist in the stock assessment of Atlantic mackerel (northern contingent)
# by: Andrew D. Smith 2020

# Fecundity

# Atlantic mackerel (Scomber scombrus) fecundities as functions of biological (length, mass, gonad mass) and physical paramaters (temperature, day of year)

# Fecundity

# 1 Maguire 1981
# JJ's master's thesis

# 2) Pelletier 1986; 2 x 228 fish (4 years, multiple locations) 

# Pelletier, L. 1986. Fecondité du maquereau bleu, Scomber scomobrus L., du golfe du Saint-Laurent. Canadian Technical Report of Fisheries and Aquatic Sciences. No. 1467.
# Study on the relation of NWA mackerel fecundity in relation with other biological parameters 
# 2 ovaries * 228 individuals (1982-1985) from commercial samples taken from the southeast of Nova Scotia Canada  between may and august. 
# only ovary maturity stage 5 (Parish and Saville 1965) were used (n = 214); Age range (2-14)
# of note, this is not absolute fecundity but maximum potential fecundity (mass atresia, skipped spawning, and/or reabsorbtion of ovocytes mean that this is a overestimate)
# Length = Fork Length (mm)

# Note!!!!! - last couple of SARs since Thomas used log base e instead of log base 10... huge difference
# For example: Figure 1 in Pelletier et al. show fecundity (nx10^5)~length (mm). 340 mm ~= about 4*10^5
# but if you solve for y using log base e (i.e. log(), the default in R) you get fec = 56.85421 * 10^5 
# instead of fec = 10^(-2.05+3.02*log10(340)) = 393610.2/100000 = 3.936102...
# Difference of 56.85421/3.936102 = 14.44429x higher estimate.... ! fuck. 
# it is unclear in past resdocs which of Pelletier's equations were used and whether ln or log10 were used. eg. Resdoc 2008 says that mean weight of fish calculated and corresponding fecundity was used...
# Fecundity ~ Length (r = 0.56, n = 214)
# log10(f) = -2.05+3.02*(log10(length))
# rearranged: f = 0.00891251*length^3.02 = 10^(-2.05+3.02*log10(length))
# if it were ln then: 
# log(f) = -2.05+3.02*(log(length))
# f = (1/exp(2.05))*length^3.02


#' Fecundity as a funciton of length - Pelletier 1986
#'
#' @param length female mackerel fork length in mm for stage 5 ovaries
#' @return fecundity in numbers of eggs 
#' @export
#' @examples f_length_pelletier(300)
fec_length_pelletier <- function(length){
  f = 10^(-2.05 + 3.02 * log10(length))
  f
}

# Fecundity ~ total mass (g) (r = 0.65, n = 214)
# linear form = f = 960 * mass - 65.481. Line somehow turned this into log10 form but unclear how
# log10 form: log10(f) = 2.80 + 1.04 * log10(mass) 
# f = 630.957 * mass^1.04
# f = 10^(2.80 + 1.04 * log10(length))

#' Fecundity as a funciton of mass - Pelletier 1986
#' 
#' @param mass in grammes for females stage 5
#' @return fecundity in numbers of eggs 
#' @export
#' @examples fec_mass_pelletier()
fec_mass_pelletier <- function(mass){
  f = 10^(2.80 + 1.04 * log10(mass))
  f
}

# Fecundity ~ age (r = 0.52, n = 185)
# log10(f) = 5.37 + 0.47 * log10(age)
# = 234423*age^0.469999
# = 10^(5.37 + 0.47 * log10(age))

#' Fecundity as a funciton of age - Pelletier 1986
#' @param age age of females for females stage 5
#' @return fecundity in numbers of eggs 
#' @export
#' @examples fec_age_pelletier(4)
fec_age_pelletier <- function(age){
  f = 10^(5.37 + 0.47 * log10(age))
  f
}

# Fecundity ~ ovary mass (g) (r = 0.79, n = 213); 
# best fit of study and concords with Maguire 1981
# log10(f) = 4.32 + 0.75 * log10(mass_gonad)
# = f = 20893*mass_gonad^0.75
# = f = 10^(4.32 + 0.75 * log10(mass_gonad))

#' Fecundity as a funciton of ovary mass - Pelletier 1986
#'
#' @param mass_gonad in grammes for females stage 5
#' mass_gonad = ovary mass
#' @return fecundity in numbers of eggs 
#' @export
#' @examples fec_ovary_pelletier()
fec_ovary_pelletier <- function(mass_gonad){
  f = 10^(4.32 + 0.75 * log10(mass_gonad))
  f
}

# Pelletier found that fecundity decreased linearly as a function of day of year (r = -0.57*)
# If you don't have gonad weight you can use day of year as a predictor with length or mass to get a better fit

# F ~ length and day (n = 214, r = 0.73)
# log10(F) = 2.2*log10(length)-0.007*(doy)+1.21
# F = 10^(2.2*log10(length)-0.007*(doy)+1.21)

# F ~ mass and day (n = 214, r = 0.77)
# log10(F) = 0.8*log10(mass)-0007*(doy)+4.59
# F = 0.8*log10(mass)-0007*(doy)+4.59

# even better fit for ovaries if you account for doy
# log10(F)  = 0.61 * log10(mass_gonad)-0.004 * (doy) + 5.33; (n = 213, r = 0.84)
# F = 10^(0.61 * log10(mass_gonad) - 0.004(doy) + 5.33)
# 

#' Fecundity as a funciton of ovary mass and day of year (julian) - Pelletier 1986
#' Could maybe be used as replacement for DAILY FECUNDITY REDUCTION METHOD (DFRM)?
#' Instead of doing histology, correct fecundity by day of year
#' @param mass_gonad in grammes for females stage 5
#' @param doy Julian day
#' @return fecundity in numbers of eggs
#' @export
#'
#' @examples fec_ovary_doy_pelletier(60, 170)
fec_ovary_doy_pelletier <- function(mass_gonad, doy){
  f = 10^(0.61*log10(mass_gonad) - 0.004(doy) + 5.33)
  f
}

# 3) Morse 1980
# MORSE, W. W. 1980. Spawning and fecundity of Atlantic mackerel, Scomber scombrus, in the middle-Atlantic Bight.Fish. Bull.U.S.,78:103-108.
# Morse's eqn transformed to the following in 1000s eggs by Griswold and Silverman 1992: 
# log10(F) = -11.346 + 5.544 * log10(length); (r = 0.88, n = 126, Sy.x = 0.11882)

#' Fecundity as a funciton of fork length - Morse 1980
#' NWA mackerel (USA)
#' Original formulation: log10(F) = -8.346 + 5.5441109 * log10(length) with f in millions
#' n = 218; r = 0.88; Sy.x = 0.066 (for original formulation - see above)
#' @param length fork length in mm
#' @return fecundity in numbers of eggs (originally in millions)
#' @export
#'
#' @examples
fec_length_morse <- function(length){
  f = 10^(-11.346 + 5.544 * log10(length))
  f = f/1000
  f
}

#' Fecundity as a funciton of gutted mass - Morse 1980
#' NWA mackerel (USA)
#' n = 218; r = 0.81; Sy.x = 0.081
#' @param mass gutted mass in grammes
#' @return fecundity in numbers of eggs (originally in millions)
#' @export
#'
#' @examples
fec_guttedmass_morse <- function(mass){
  f = 10^(1.721 + 1.547 * log10(length))
  f = f/1000000
  f
}

#' Fecundity as a funciton of age - Morse 1980
#' NWA mackerel (USA)
#' n = 197; r = 0.76; Sy.x = 0.084
#' @param age age
#' @return fecundity in numbers of eggs (originally in millions)
#' @export
#'
#' @examples
fec_age_morse <- function(mass){
  f = 10^(5.264 + 0.6401 * log10(length))
  f = f/1000000
  f
}



# 4) Walsh 1983
# WALSH,M. 1983. Investigations on the fecundity of North Sea mackerel. ICES C.M. Doc., No. H:48, 10 p.
# From Griswald and Silverman 1992: Walsh's original equation was stated as:
# F = 0.00311 * total_length^3.169
# = log10(F) = -2.507 + 3.169 * log10(total_length)
# using the eqn. total_length = 1.09985 * fork_length (n = 295, Sy.x = 0.00065, r >= 0.999)
# Walsh's eqn transformed to:
# log10(F) = -2.376 + 3.169 * log10(fork_length)
# or log10(F) = -5.376 + 3.169 * (fork_length) (r =0.52, N = 64, Sy.x = 0.508)

#' Fecundity as a funciton of fork length - Walsh 1982
#' North sea
#' @param length fork length in mm
#' equation modified from original by Griswold and Silverman (1992)
#' @return fecundity in number of eggs
#' @export
#'
#' @examples
fec_length_walsh <- function(length){
  f = 10^(-2.376 + 3.169 * log10(length))
  f
}


# 5) Griswold and Silverman 1992
# Griswold, C.A. and Silverman, M.J. 1992. Fecundity of the Atlantic Mackerel (Scomber scombrus) in the Northwest Atlantic in 1987. J. Northw. Atl. Fish. Sci. Vol. 12: 35-40.
# A total of 295 Atlantic mackerel were examined for fecundity. Of these, 169 pairs of ovaries were from fish capturedin USA watersand126fromCanadianwaters.
# Samples were collected from various areas off the coast of USA from 15 April to 10 May 1987 from Polish midwater trawls. 
# Additional samples were obtained from USAcommercial trawlers and party boats using bottomtrawl and hook and line, respectively. Canadian samples were obtained from trap fisheries off CapeBreton, NovaScotia, between 6 April and 10 June, 1987.
# given the dates perhaps some mixing

#' Fecundity as a funciton of fork length (Canada) - Griswold and Silverman 1992
#' r = 0.66, n = 126, Sy.x = 0.11882
#' @param length fork length in mm
#' @return fecundity in number of eggs
#' @export
#'
#' @examples
fec_length_can_GS <- function(length){
  f = 10^(-6.812 + 3.737 * log10(length))
  f = f/1000
  f
}

#' Fecundity as a funciton of fork length (USA) - Griswold and Silverman 1992
#' r = 0.87 n = 169, Sy.x = 0.11351
#' @param length fork length in mm
#' @return fecundity in number of eggs
#' @export
#' @examples fec_length_usa_GS()
fec_length_usa_GS <- function(length){
  f = 10^(-8.681 + 4.464 * log10(length))
  f = f/1000
  f
}

#' Fecundity as a funciton of age (Canada) - Griswold and Silverman 1992
#' r = 0.62320 n = 126, Sy.x = 0.12300
#' @param age fork length in mm
#' @return fecundity in number of eggs
#' @export
#' @examples fec_length_usa_GS()
fec_age_can_GS <- function(age){
  f = 10^(1.9757 + 1.0040 * log10(age))
  f = f/1000
  f
}

#' Fecundity as a funciton of age (USA) - Griswold and Silverman 1992
#' r = 0.83716 n = 169, Sy.x = 0.12492
#' @param age fork length in mm
#' @return fecundity in number of eggs
#' @export
#' @examples fec_length_usa_GS()
fec_age_usa_GS <- function(age){
  f = 10^(1.9686 + 1.0228 * log10(age))
  f = f/1000
  f
}

#' Fecundity as a funciton of somatic mass (Canada) - Griswold and Silverman 1992
#' r = 0.71208 n = 126, Sy.x = 0.11042
#' @param age fork length in mm
#' @return fecundity in number of eggs
#' @export
#' @examples fec_length_usa_GS()
fec_mass_can_GS <- function(mass_somatic){
  f = 10^(-0.57035 + 1.26023 * log10(mass_somatic))
  f = f/1000
  f
}

#' Fecundity as a funciton of somatic mass (USA) - Griswold and Silverman 1992
#' r = 0.87074 n = 169, Sy.x = 0.11232
#' @param age fork length in mm
#' @return fecundity in number of eggs
#' @export
#' @examples fec_length_usa_GS()
fec_mass_usa_GS <- function(mass_somatic){
  f = 10^(-0.76583 + 1.32778 * log10(mass_somatic))
  f = f/1000
  f
}


# 6) Larraneta 1988
# Larraneta, M. G. 1988. ON THE ESTIMATION OF ACTUAL FECUNDITY OF Scomber scombrus. ICES: Pelagic Fish Comittee. C.M. 1998/H:7
# Institutode Investigaciones Marinas Bouzas. 36208 Vigo. Spain
# Abstract: Ovarian material from mackerel caught off the Galician coast   (NW"Spain) was classified into eight "categories",
# according to the gonadc somatic index and the sampling date. Fecundity (number of oocytes sequal or, 
# greater than 0.25 mm) per gram of gutted and whole fish in each category sugest that the number of eggs shed pergramof, whole, fishcould bebetween 171 and 442,
# an estimate ofthe actual fecundity.

# 2060 fishes were sampled during 1970-1982 from landings of purse seine and bottom otter trawl vessels.
# Ovary lobes from 77 females greater than 33 cm were preserved in Gilson's fluid, and gravimetric estimates of fecundity were carriedout. 

