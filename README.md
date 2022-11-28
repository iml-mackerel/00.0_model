# Northwest Atlantic Mackerel stock assessment

Rproject to perform the stock assessment for the northern contingent of the Northwest Atlantic stock of Atlantic mackerel 

The year refers to the last year of data used in the assessment and not the assessment year.

## Previous assessments

Research documents, Stock Assessment Reports, and meeting proceedings can be found at either:
* The Canadian Science Advice Secratariat (CSAS) website: http://www.isdm-gdsi.gc.ca/csas-sccs/applications/Publications/search-recherche-eng.asp 
* The Federal Science Library: https://science-libraries.canada.ca/eng/home/
* or The North Atlantic Fisheries Organisation's (NAFO) publications site: https://www.nafo.int/Publications

The assessments performed by the U.S.A. evaluates the entire Northwest Atlantic stock (northern and southern spawning contingents). Assessments are done by NOAA Fisheries' Northeast Fisheries Science Centre (NEFSC). Documents can be found here: https://apps-nefsc.fisheries.noaa.gov/saw/reviews_report_options.php

Data and model output for the 2016, and 2018 assessments can be found in the master branch here: https://github.com/iml-mackerel/0.0_model/
* 2016: input data for the 2017 assessment. A different model was used back then and hence no output is provided.
* 2018: assessment of 2019
  - Science Advisory Report: https://www.dfo-mpo.gc.ca/csas-sccs/Publications/SAR-AS/2019/2019_035-eng.pdf
  - Research document: http://www.dfo-mpo.gc.ca/csas-sccs/Publications/ResDocs-DocRech/2020/2020_013-eng.pdf 
* 2020: assessment of 2021
  - Science Advisory Report: https://www.dfo-mpo.gc.ca/csas-sccs/Publications/SAR-AS/2021/2021_029-eng.html
  - Research document: https://www.dfo-mpo.gc.ca/csas-sccs/Publications/ResDocs-DocRech/2022/2022_045-eng.html

# Information

The stock assessment used is a censured catch at age model (CCAM), which requires installation of Rtools and TMB to compile and read C++ code 

* Rtools:	https://cran.r-project.org/bin/windows/Rtools/
* TMB:		install.packages('TMB')
* CCAM:		devtools::install_github("elisvb/CCAM")
* Additional R packages used/needed are detailed in the scripts. 

# Data and model output

* input data : data/assessmentyear/...
* fitted model : Rdata/assessmentyear/fit.Rdata
* model summary : csv/assessmentyear/out.csv
* short-term projections : Rdata/2020/proj/...


