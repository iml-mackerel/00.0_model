#################################################################################################################
#*** Mackerel helper functions to read and load input data into the stock assessment model
#*** For the northern contingent of the Northwest Atlantic population of Scomber scombrus
#*** uses dplyr (https://dplyr.tidyverse.org/reference/index.html) and CCAM (https://github.com/elisvb/CCAM)
#
# Version 0.1 
# Author: Andrew D. Smith andrew.d.smith@dfo-mpo.gc.ca
# Changelog:
## 24/11/2020
### - created first functions to read .csv s (forthcoming) of input data and change to ices format
### - will require rewriting portions of script.R (nothing major)
#################################################################################################################


# Functions to format mackerel data for the stock assessment
library(dplyr);library(CCAM)

# save working directory
# MainDir = getwd()
MainDir = "C:/Users/SmithAND/Documents/My Stocks/Mackerel/mackerel_sar_2021/mackerel_model_2021/mackerel_assessment_input"

# set values
start.year = 1968
terminal.year = 2020
length.year = terminal.year - start.year + 1

# Make funtions to create headers for ices format

#' make_header
#' Make header for CCAM::read.ices format for the following data (cn, )
#' This function is called internally in other functions and you will not necessarily use it alone
#' @param title 
#' A string of your title, appears as the first line in the ices format
#' @return a data.frame
#' @export
#'
#' @examples header <- make_header("your_title")
make_header <- function(title) {
  header <- data.frame("1" = as.character(c(title,1, start.year, 1, 1)), 
                       "2" = as.character(c("", 2, terminal.year, 10, "")), 
                       "3" = as.character(c("", "", "", "", "")),
                       "4" = as.character(c("", "", "", "", "")),
                       "5" = as.character(c("", "", "", "", "")),
                       "6" = as.character(c("", "", "", "", "")),
                       "7" = as.character(c("", "", "", "", "")),
                       "8" = as.character(c("", "", "", "", "")),
                       "9" = as.character(c("", "", "", "", "")),
                       "10" = as.character(c("", "", "", "", "")))
}

#' make_header2
#' Make header for CCAM::read.ices format for the following data (ct, ct_Foreign, ct_USA )
#' This function is called internally in other functions and you will not necessarily use it alone
#' @param title 
#' A string of your title, appears as the first line in the ices format
#' @return a data.frame
#' @export
#'
#' @examples header <- make_header2("your_title")
make_header2 <- function(title) {
    header <- data.frame("1" = as.character(c(title, 1, start.year, 1, 6)), 
                         "2" = as.character(c("", 2, terminal.year, 10, "")))
}

#' make_header3
#' Make header for CCAM::read.ices format for the following data (cw,dw )
#' This function is called internally in other functions and you will not necessarily use it alone
#' @param title 
#' A string of your title, appears as the first line in the ices format
#' @return a data.frame
#' @export
#'
#' @examples header <- make_header3("your_title")
make_header3 <- function(title) {
    header <- data.frame("1" = as.character(c(title, 1, start.year, 1, 1)), 
                         "2" = as.character(c("", 3, terminal.year, 10, "")), 
                         "3" = as.character(c("", "", "", "", "")),
                         "4" = as.character(c("", "", "", "", "")),
                         "5" = as.character(c("", "", "", "", "")),
                         "6" = as.character(c("", "", "", "", "")),
                         "7" = as.character(c("", "", "", "", "")),
                         "8" = as.character(c("", "", "", "", "")),
                         "9" = as.character(c("", "", "", "", "")),
                         "10" = as.character(c("", "", "", "", "")))
}

#' make_header4
#' Make header for CCAM::read.ices format for the following data (env)
#' This function is called internally in other functions and you will not necessarily use it alone
#' @param title 
#' A string of your title, appears as the first line in the ices format
#' @return a data.frame
#' @export
#'
#' @examples header <- make_header4("your_title")
make_header4 <- function(title) {
    header <- data.frame("1" = as.character(c(title, 1, start.year, 1, 7,0,0,0)), 
                         "2" = as.character(c("", 3, terminal.year, 1, "", "", "", "")))
}

#' make_header5
#' Make header for CCAM::read.ices format for the following data (fl)
#' This function is called internally in other functions and you will not necessarily use it alone
#' @param title 
#' A string of your title, appears as the first line in the ices format
#' @return a data.frame
#' @export
#'
#' @examples header <- make_header5("your_title")
make_header5 <- function(title) {
    header <- data.frame("1" = as.character(c(title, 1, start.year, 1)), 
                         "2" = as.character(c("", 1, terminal.year, 10)))
}

#' make_header6
#' Make header for CCAM::read.ices format for the following data (pm)
#' This function is called internally in other functions and you will not necessarily use it alone
#' @param title 
#' A string of your title, appears as the first line in the ices format
#' @return a data.frame
#' @export
#'
#' @examples header <- make_header6("your_title")
make_header6 <- function(title) {
    header <- data.frame("1" = as.character(c(title, 1, start.year, 1, 1)), 
                         "2" = as.character(c("", 6, terminal.year, 10, "")), 
                         "3" = as.character(c("", "", "", "", "")),
                         "4" = as.character(c("", "", "", "", "")),
                         "5" = as.character(c("", "", "", "", "")),
                         "6" = as.character(c("", "", "", "", "")),
                         "7" = as.character(c("", "", "", "", "")),
                         "8" = as.character(c("", "", "", "", "")),
                         "9" = as.character(c("", "", "", "", "")),
                         "10" = as.character(c("", "", "", "", "")))
}

#' make_header7
#' Make header for CCAM::read.ices format for the following data (nm)
#' This function is called internally in other functions and you will not necessarily use it alone
#' @param title 
#' A string of your title, appears as the first line in the ices format
#' @return a data.frame
#' @export
#'
#' @examples header <- make_header6("your_title")
make_header7 <- function(title) {
    header <- data.frame("1" = as.character(c(title, 1, start.year, 1, 1)), 
                         "2" = as.character(c("", 5, terminal.year, 10, "")), 
                         "3" = as.character(c("", "", "", "", "")),
                         "4" = as.character(c("", "", "", "", "")),
                         "5" = as.character(c("", "", "", "", "")),
                         "6" = as.character(c("", "", "", "", "")),
                         "7" = as.character(c("", "", "", "", "")),
                         "8" = as.character(c("", "", "", "", "")),
                         "9" = as.character(c("", "", "", "", "")),
                         "10" = as.character(c("", "", "", "", "")))
}

#' make_header8
#' Make header for CCAM::read.ices format for the following data (pf)
#' This function is called internally in other functions and you will not necessarily use it alone
#' @param title 
#' A string of your title, appears as the first line in the ices format
#' @return a data.frame
#' @export
#'
#' @examples header <- make_header8("your_title")
make_header8 <- function(title) {
    header <- data.frame("1" = as.character(c(title, 1, start.year, 1, 1)), 
                         "2" = as.character(c("", 7, terminal.year, 10, "")), 
                         "3" = as.character(c("", "", "", "", "")),
                         "4" = as.character(c("", "", "", "", "")),
                         "5" = as.character(c("", "", "", "", "")),
                         "6" = as.character(c("", "", "", "", "")),
                         "7" = as.character(c("", "", "", "", "")),
                         "8" = as.character(c("", "", "", "", "")),
                         "9" = as.character(c("", "", "", "", "")),
                         "10" = as.character(c("", "", "", "", "")))
}

#' make_header9
#' Make header for CCAM::read.ices format for the following data (pf)
#' This function is called internally in other functions and you will not necessarily use it alone
#' @param title 
#' A string of your title, appears as the first line in the ices format
#' @return a data.frame
#' @export
#'
#' @examples header <- make_header8("your_title")
make_header9 <- function(title) {
    header <- data.frame("1" = as.character(c(title, 1, start.year, 1, 1)), 
                         "2" = as.character(c("", 8, terminal.year, 10, "")), 
                         "3" = as.character(c("", "", "", "", "")),
                         "4" = as.character(c("", "", "", "", "")),
                         "5" = as.character(c("", "", "", "", "")),
                         "6" = as.character(c("", "", "", "", "")),
                         "7" = as.character(c("", "", "", "", "")),
                         "8" = as.character(c("", "", "", "", "")),
                         "9" = as.character(c("", "", "", "", "")),
                         "10" = as.character(c("", "", "", "", "")))
}

#' make_header10
#' Make header for CCAM::read.ices format for the following data (survey)
#' This function is called internally in other functions and you will not necessarily use it alone
#' @param title 
#' A string of your title, appears as the first line in the ices format
#' @return a data.frame
#' @export
#'
#' @examples header <- make_header10("your_title")
make_header10 <- function(title, title2) {
    header <- data.frame("1" = as.character(c(title, 103, title2, start.year, 1, -1)), 
                         "2" = as.character(c("", "","", terminal.year, 1, -1)), 
                         "3" = as.character(c("", "", "", "", 0.47, "")),
                         "4" = as.character(c("", "", "", "", 0.47,"")))
}

#' make_header11
#' Make header for CCAM::read.ices format for the following data (sw)
#' This function is called internally in other functions and you will not necessarily use it alone
#' @param title 
#' A string of your title, appears as the first line in the ices format
#' @return a data.frame
#' @export
#'
#' @examples header <- make_header("your_title")
make_header11 <- function(title) {
    header <- data.frame("1" = as.character(c(title,1, start.year, 1, 1)), 
                         "2" = as.character(c("", 4, terminal.year, 10, "")), 
                         "3" = as.character(c("", "", "", "", "")),
                         "4" = as.character(c("", "", "", "", "")),
                         "5" = as.character(c("", "", "", "", "")),
                         "6" = as.character(c("", "", "", "", "")),
                         "7" = as.character(c("", "", "", "", "")),
                         "8" = as.character(c("", "", "", "", "")),
                         "9" = as.character(c("", "", "", "", "")),
                         "10" = as.character(c("", "", "", "", "")))
}

# Make funtions to create headers for ices format

# fake catch at age data for proof of concept
# cn <- data.frame("year" = seq(start.year,terminal.year,1), 
#                  "1" = as.character(round(runif(length.year, 0, 25))),
#                  "2" = as.character(round(runif(length.year, 0, 25))),
#                  "3" = as.character(round(runif(length.year, 0, 25))),
#                  "4" = as.character(round(runif(length.year, 0, 25))),
#                  "5" = as.character(round(runif(length.year, 0, 25))),
#                  "6" = as.character(round(runif(length.year, 0, 25))),
#                  "7" = as.character(round(runif(length.year, 0, 25))),
#                  "8" = as.character(round(runif(length.year, 0, 25))),
#                  "9" = as.character(round(runif(length.year, 0, 25))),
#                  "10" = as.character(round(runif(length.year, 0, 25)))
#                  )
# write.csv(cn, file = "./Rdata/cn.csv", sep = "    ", row.names = F)
# rm(cn)
#path = "./Rdata/cn.csv"


# Functions to read, format, and employ CCAM::read.ices() for each input dataset in "./data/

#' make_cn
#' A function to make a catch-at-age table in thousands of individuals
#' @param path 
#' The file path where the cn.csv file is located
#' @return a catch-at-age matrix
#' @export
#'
#' @examples cn <- make_cn(path)
make_cn <- function(path){
    require(dplyr)
    require(CCAM)
    cn <- read.csv(path, colClasses = "character") # data.frame with year, and ages 1-10 as headers
    cn <- cn[,2:11]
    title = "NWA Mackerel catch-at-age - catch.n (units : Thousands)"
    header = make_header(title)
    cn <- dplyr::bind_rows(header, cn)
    colnames(cn) <- NULL
    rownames(cn) <- NULL
    setwd("./data")
    write.table(cn, file = "cn.dat", quote = FALSE, sep = "    ", row.names = F, col.names = F)
    setwd(MainDir)
    cn = CCAM::read.ices("./data/cn.dat")
    cn
}

#' make_ct
#' A function to create the upper and lower bounds of the censured catch-at-age model
#' @param path 
#' file path for ct.csv
#' @return a censured catch matrix
#' @export
#'
#' @examples
make_ct <- function(path){
    require(dplyr)
    require(CCAM)
    ct <- read.csv(path, colClasses = "character") # data.frame with year, and ages 1-10 as headers
    ct <- ct[,2:3]
    title = "NWA Mackerel catch total lower and upper bounds (unit = metric tonnes)"
    header = make_header2(title)
    ct <- dplyr::bind_rows(header, ct)
    colnames(ct) <- NULL
    rownames(ct) <- NULL
    setwd("./data")
    write.table(ct, file = "ct.dat", quote = FALSE, sep = "    ", row.names = F, col.names = F)
    setwd(MainDir)
    ct = CCAM::read.ices("./data/ct.dat")
    ct
}

#' make_ctForeign
#' A function to create upper and lower bounds of landings for the distant water fleet that fished in what is now Canada's EEZ in the Northwest Atlantic (i.e. NAFO 2-5Ze)
#' @param path 
#' file path for ctForeign.csv
#' @return a matrix
#' @export
#'
#' @examples ctForeign <- make_ctForeign(path)
make_ctForeign <- function(path){
    require(dplyr)
    require(CCAM)
    ctForeign <- read.csv(path, colClasses = "character") # data.frame with year, and ages 1-10 as headers
    ctForeign <- ctForeign[,2:3]
    title = "NWA Mackerel distant water fleet (foreign other than USA) landings in NAFO 0-6 (units = metric tonnes)"
    header = make_header2(title)
    ctForeign <- dplyr::bind_rows(header, ctForeign)
    colnames(ctForeign) <- NULL
    rownames(ctForeign) <- NULL
    setwd("./data")
    write.table(ctForeign, file = "ctForeign.dat", quote = FALSE, sep = "    ", row.names = F, col.names = F)
    setwd(MainDir)
    ctForeign = CCAM::read.ices("./data/ctForeign.dat")
    ctForeign
}

#' make_ctUSA
#'
#' @param path 
#' file path for ctUSA.csv
#' @return a matrix
#' @export
#'
#' @examples ct_USA <- make_ctUSA(path)
    make_ctUSA <- function(path){
        require(dplyr)
        require(CCAM)
        ctUSA <- read.csv(path, colClasses = "character") # data.frame with year, lower bounds and upper bounds as headers
        ctUSA <- ctUSA[,2:3]
        title = "NWA Mackerel landings by the U.S.A. in NAFO 0-6 (units = metric tonnes)"
        header = make_header2(title)
        ctUSA <- dplyr::bind_rows(header, ctUSA)
        colnames(ctUSA) <- NULL
        rownames(ctUSA) <- NULL
        setwd("./data")
        write.table(ctUSA, file = "ctUSA.dat", quote = FALSE, sep = "    ", row.names = F, col.names = F)
        setwd(MainDir)
        ctUSA = CCAM::read.ices("./data/ctUSA.dat")
        ctUSA
}

make_cw <- function(path){
    require(dplyr)
    require(CCAM)
    cw <- read.csv(path, colClasses = "character") # data.frame with year, and ages 1-10 as headers
    cw <- cw[,2:11]
    title = "NWA Mackerel mean mass-at-age weighted by catch- catch.wt (units : kg)"
    header = make_header3(title)
    cw <- dplyr::bind_rows(header, cw)
    colnames(cw) <- NULL
    rownames(cw) <- NULL
    setwd("./data")
    write.table(cw, file = "cw.dat", quote = FALSE, sep = "    ", row.names = F, col.names = F)
    setwd(MainDir)
    cw = CCAM::read.ices("./data/cw.dat")
    cw
}

#' make_dw
#' functonally the same as make_cw but for discards if we ever get that data
#' @param path 
#' file path for dw.csv
#' @return a matrix
#' @export
#'
#' @examples
make_dw <- function(path){
    require(dplyr)
    require(CCAM)
    dw <- read.csv(path, colClasses = "character") # data.frame with year, and ages 1-10 as headers
    dw <- dw[,2:11]
    title = "NWA Mackerel mean mass-at-age weighted by catch- catch.wt (units : kg)"
    header = make_header3(title)
    dw <- dplyr::bind_rows(header, dw)
    colnames(dw) <- NULL
    rownames(dw) <- NULL
    setwd("./data")
    write.table(dw, file = "dw.dat", quote = FALSE, sep = "    ", row.names = F, col.names = F)
    setwd(MainDir)
    dw = CCAM::read.ices("./data/dw.dat")
    dw
}

#' make_env
#' A function to read an annual environmental index and change to ices format
#' @param path 
#' file path for env.csv
#' @return a matrix
#' @export
#'
#' @examples
make_env <- function(path){
    require(dplyr)
    require(CCAM)
    env <- read.csv(path, colClasses = "character") # data.frame with year, and ages 1-10 as headers
    env <- env[,2]
    title = "NWA Mackerel environmental index (A PCA loading or some other standardized (mean-xi)/sd) variable) units = dimensionless"
    header = make_header4(title)
    env <- dplyr::bind_rows(header, env)
    colnames(env) <- NULL
    rownames(env) <- NULL
    setwd("./data")
    write.table(env, file = "env.dat", quote = FALSE, sep = "    ", row.names = F, col.names = F)
    setwd(MainDir)
    env = CCAM::read.ices("./data/env.dat")
    env
}

make_fec <- function(path){ # see mack_fun_fec.R.. EVB coded this as fec~age which isn't the best fit and might be biased due to it being calculated on very strong year classes (1982-1984)
    
}

#' make_lf
#' make matrix of landed fraction
#' currently only a matrix of 1s
#' @param path 
#' file path for lf.csv
#' @return a matrix
#' @export
#'
#' @examples
make_lf <- function(path){
    require(dplyr)
    require(CCAM)
    lf <- read.csv(path, colClasses = "character") # data.frame with year, and ages 1-10 as headers
    lf <- lf[,2:11]
    title = "NWA Mackerel landed fraction"
    header = make_header5(title)
    lf <- dplyr::bind_rows(header, lf)
    colnames(lf) <- NULL
    rownames(lf) <- NULL
    setwd("./data")
    write.table(lf, file = "lf.dat", quote = FALSE, sep = "    ", row.names = F, col.names = F)
    setwd(MainDir)
    lf = CCAM::read.ices("./data/lf.dat")
    lf
}

make_lw <- function(path){
    require(dplyr)
    require(CCAM)
    lw <- read.csv(path, colClasses = "character") # data.frame with year, and ages 1-10 as headers
    lw <- lw[,2:11]
    title = "NWA Mackerel mean mass-at-age weighted by catch- catch.wt (units : kg)"
    header = make_header3(title)
    lw <- dplyr::bind_rows(header, lw)
    colnames(lw) <- NULL
    rownames(lw) <- NULL
    setwd("./data")
    write.table(lw, file = "lw.dat", quote = FALSE, sep = "    ", row.names = F, col.names = F)
    setwd(MainDir)
    lw = CCAM::read.ices("./data/lw.dat")
    lw
}

#' make_mo
#' A function to create the ices format data for proportion mature
#' @param path 
#' file path for mo.csv
#' @return a matrix of proportion mature by ages (1-10)
#' @export
#'
#' @examples
make_mo <- function(path){
    require(dplyr)
    require(CCAM)
    mo <- read.csv(path, colClasses = "character") # data.frame with year, and ages 1-10 as headers
    mo <- mo[,2:11]
    title = "NWA Mackerel proportion mature by age"
    header = make_header6(title)
    mo <- dplyr::bind_rows(header, mo)
    colnames(mo) <- NULL
    rownames(mo) <- NULL
    setwd("./data")
    write.table(mo, file = "mo.dat", quote = FALSE, sep = "    ", row.names = F, col.names = F)
    setwd(MainDir)
    mo = CCAM::read.ices("./data/mo.dat")
    mo
}

#' make_nm
#' A function to create a natural mortality by age matrix
#' @param path 
#' file path to nm.csv
#' @return
#' @export
#'
#' @examples
make_nm <- function(path){
    require(dplyr)
    require(CCAM)
    nm <- read.csv(path, colClasses = "character") # data.frame with year, and ages 1-10 as headers
    nm <- nm[,2:11]
    title = "NWA Mackerel natural mortality by age"
    header = make_header7(title)
    nm <- dplyr::bind_rows(header, nm)
    colnames(nm) <- NULL
    rownames(nm) <- NULL
    setwd("./data")
    write.table(nm, file = "nm.dat", quote = FALSE, sep = "    ", row.names = F, col.names = F)
    setwd(MainDir)
    nm = CCAM::read.ices("./data/nm.dat")
    nm
}

#' make_nm_Alverson
#'
#' @param path 
#'
#' @return
#' @export
#'
#' @examples
make_nm_Alverson <- function(path){
    require(dplyr)
    require(CCAM)
    nm_Alverson <- read.csv(path, colClasses = "character") # data.frame with year, and ages 1-10 as headers
    nm_Alverson <- nm_Alverson[,2:11]
    title = "NWA Mackerel natural mortality by age as per Alverson"
    header = make_header7(title)
    nm_Alverson <- dplyr::bind_rows(header, nm_Alverson)
    colnames(nm_Alverson) <- NULL
    rownames(nm_Alverson) <- NULL
    setwd("./data")
    write.table(nm_Alverson, file = "nm_Alverson.dat", quote = FALSE, sep = "    ", row.names = F, col.names = F)
    setwd(MainDir)
    nm_Alverson = CCAM::read.ices("./data/nm_Alverson.dat")
    nm_Alverson
}

#' make_nm_DFO2017
#'
#' @param path 
#'
#' @return
#' @export
#'
#' @examples
make_nm_DFO2017 <- function(path){
    require(dplyr)
    require(CCAM)
    nm_DFO2017 <- read.csv(path, colClasses = "character") # data.frame with year, and ages 1-10 as headers
    nm_DFO2017 <- nm_DFO2017[,2:11]
    title = "NWA Mackerel natural mortality by age as per DFO 2017"
    header = make_header7(title)
    nm_DFO2017 <- dplyr::bind_rows(header, nm_DFO2017)
    colnames(nm_DFO2017) <- NULL
    rownames(nm_DFO2017) <- NULL
    setwd("./data")
    write.table(nm_DFO2017, file = "nm_DFO2017.dat", quote = FALSE, sep = "    ", row.names = F, col.names = F)
    setwd(MainDir)
    nm_DFO2017 = CCAM::read.ices("./data/nm_DFO2017.dat")
    nm_DFO2017
}

#' make_nm_Gislasson
#'
#' @param path 
#'
#' @return
#' @export
#'
#' @examples
make_nm_Gislasson <- function(path){
    require(dplyr)
    require(CCAM)
    nm_Gislasson <- read.csv(path, colClasses = "character") # data.frame with year, and ages 1-10 as headers
    nm_Gislasson <- nm_Gislasson[,2:11]
    title = "NWA Mackerel natural mortality by age as per Gislasson"
    header = make_header7(title)
    nm_Gislasson <- dplyr::bind_rows(header, nm_Gislasson)
    colnames(nm_Gislasson) <- NULL
    rownames(nm_Gislasson) <- NULL
    setwd("./data")
    write.table(nm_Gislasson, file = "nm_Gislasson.dat", quote = FALSE, sep = "    ", row.names = F, col.names = F)
    setwd(MainDir)
    nm_Gislasson = CCAM::read.ices("./data/nm_Gislasson.dat")
    nm_Gislasson
}

#' make_nm_Gunderson
#'
#' @param path 
#'
#' @return
#' @export
#'
#' @examples
make_nm_Gunderson <- function(path){
    require(dplyr)
    require(CCAM)
    nm_Gunderson <- read.csv(path, colClasses = "character") # data.frame with year, and ages 1-10 as headers
    nm_Gunderson <- nm_Gunderson[,2:11]
    title = "NWA Mackerel natural mortality by age as per Gunderson"
    header = make_header7(title)
    nm_Gunderson <- dplyr::bind_rows(header, nm_Gunderson)
    colnames(nm_Gunderson) <- NULL
    rownames(nm_Gunderson) <- NULL
    setwd("./data")
    write.table(nm_Gunderson, file = "nm_Gunderson.dat", quote = FALSE, sep = "    ", row.names = F, col.names = F)
    setwd(MainDir)
    nm_Gunderson = CCAM::read.ices("./data/nm_Gunderson.dat")
    nm_Gunderson
}

#' make_nm_Zhang
#'
#' @param path 
#'
#' @return
#' @export
#'
#' @examples
make_nm_Zhang <- function(path){
    require(dplyr)
    require(CCAM)
    nm_Zhang <- read.csv(path, colClasses = "character") # data.frame with year, and ages 1-10 as headers
    nm_Zhang <- nm_Zhang[,2:11]
    title = "NWA Mackerel natural mortality by age as per Zhang"
    header = make_header7(title)
    nm_Zhang <- dplyr::bind_rows(header, nm_Zhang)
    colnames(nm_Zhang) <- NULL
    rownames(nm_Zhang) <- NULL
    setwd("./data")
    write.table(nm_Zhang, file = "nm_Zhang.dat", quote = FALSE, sep = "    ", row.names = F, col.names = F)
    setwd(MainDir)
    nm_Zhang = CCAM::read.ices("./data/nm_Zhang.dat")
    nm_Zhang
}

#' make_pf
#'
#' @param path 
#'
#' @return
#' @export
#'
#' @examples
make_pf <- function(path){
    require(dplyr)
    require(CCAM)
    pf <- read.csv(path, colClasses = "character") # data.frame with year, and ages 1-10 as headers
    pf <- pf[,2:11]
    title = "NWA Mackerel proportion of Fishing mortality (F) before peak spawning"
    header = make_header8(title)
    pf <- dplyr::bind_rows(header, pf)
    colnames(pf) <- NULL
    rownames(pf) <- NULL
    setwd("./data")
    write.table(pf, file = "pf.dat", quote = FALSE, sep = "    ", row.names = F, col.names = F)
    setwd(MainDir)
    pf = CCAM::read.ices("./data/pf.dat")
    pf
    
}

#' make_pm
#'
#' @param path 
#'
#' @return
#' @export
#'
#' @examples
make_pm <- function(path){
    require(dplyr)
    require(CCAM)
    pm <- read.csv(path, colClasses = "character") # data.frame with year, and ages 1-10 as headers
    pm <- pm[,2:11]
    title = "NWA Mackerel proportion of natural mortality (M) before peak spawning"
    header = make_header9(title)
    pm <- dplyr::bind_rows(header, pm)
    colnames(pm) <- NULL
    rownames(pm) <- NULL
    setwd("./data")
    write.table(pm, file = "pm.dat", quote = FALSE, sep = "    ", row.names = F, col.names = F)
    setwd(MainDir)
    pm = CCAM::read.ices("./data/pm.dat")
    pm
}

#' make_propFemale
#'
#' @param path 
#'
#' @return
#' @export
#'
#' @examples
make_propFemale <- function(path){
    require(dplyr)
    require(CCAM)
    propFemale <- read.csv(path, colClasses = "character") # data.frame with year, and ages 1-10 as headers
    propFemale <- propFemale[,2:11]
    title = "NWA Mackerel proportion of Females in spawning area during spawning period"
    header = make_header(title)
    propFemale <- dplyr::bind_rows(header, propFemale)
    colnames(propFemale) <- NULL
    rownames(propFemale) <- NULL
    setwd("./data")
    write.table(propFemale, file = "propFemale.dat", quote = FALSE, sep = "    ", row.names = F, col.names = F)
    setwd(MainDir)
    propFemale = CCAM::read.ices("./data/propFemale.dat")
    propFemale
    
}


#' make_survey
#' 
#' @param path 
#'
#' @return
#' @export
#'
#' @examples
make_survey <- function(path){
    require(dplyr)
    require(CCAM)
    survey <- read.csv(path, colClasses = "character") # data.frame with year, and ages 1-10 as headers
    survey <- survey[,2:3]
    title = "SSB indices"
    title2 = "SSB egg survey: SSB = (P*S*A)/(F*W*10^6)"
    survey = make_header10(title,title2)
    survey <- dplyr::bind_rows(header, survey)
    colnames(survey) <- NULL
    rownames(survey) <- NULL
    setwd("./data")
    write.table(survey, file = "survey.dat", quote = FALSE, sep = "    ", row.names = F, col.names = F)
    setwd(MainDir)
    survey = CCAM::read.ices("./data/survey.dat")
    survey
    
}

#' make_sw
#'
#' @param path 
#'
#' @return
#' @export
#'
#' @examples
make_sw <- function(path){
    require(dplyr)
    require(CCAM)
    sw <- read.csv(path, colClasses = "character") # data.frame with year, and ages 1-10 as headers
    sw <- sw[,2:11]
    title = "Mean mass in stock. units = kg"
    sw = make_header11(title)
    sw <- dplyr::bind_rows(header, sw)
    colnames(sw) <- NULL
    rownames(sw) <- NULL
    setwd("./data")
    write.table(sw, file = "sw.dat", quote = FALSE, sep = "    ", row.names = F, col.names = F)
    setwd(MainDir)
    sw = CCAM::read.ices("./data/sw.dat")
    sw
    
}

#' make_sw0
#'
#' @param path 
#'
#' @return
#' @export
#'
#' @examples
make_sw0 <- function(path){
    require(dplyr)
    require(CCAM)
    sw0 <- read.csv(path, colClasses = "character") # data.frame with year, and ages 1-10 as headers
    sw0 <- sw0[,2:11]
    title = "Mean mass in stock on January 1st units = kg"
    sw0 = make_header11(title)
    sw0 <- dplyr::bind_rows(header, sw0)
    colnames(sw0) <- NULL
    rownames(sw0) <- NULL
    setwd("./data")
    write.table(sw0, file = "sw0.dat", quote = FALSE, sep = "    ", row.names = F, col.names = F)
    setwd(MainDir)
    sw0 = CCAM::read.ices("./data/sw0.dat")
    sw0
    
}

#' make_tep
#'
#' @param path 
#'
#' @return
#' @export
#'
#' @examples
make_tep <- function(path){
    require(dplyr)
    require(CCAM)
    tep <- read.csv(path, colClasses = "character") # data.frame with year, and ages 1-10 as headers
    tep <- tep[,2:3]
    title = "Total egg production; TEP = (P*S*A)"
    title2 = "TEP"
    tep = make_header10(title,title2)
    tep <- dplyr::bind_rows(header, tep)
    colnames(tep) <- NULL
    rownames(tep) <- NULL
    setwd("./data")
    write.table(tep, file = "tep.dat", quote = FALSE, sep = "    ", row.names = F, col.names = F)
    setwd(MainDir)
    tep = CCAM::read.ices("./data/tep.dat")
    tep
}
