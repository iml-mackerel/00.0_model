# Northwest Atlantic Mackerel stock assessment

Rproject to run the stock assessment model for the northern contingent of Northwest Atlantic mackerel.

Years refer to the last year of data used in the assessment and not the assessment year.

## Previous assessments

Recent research documents, Stock Assessment Reports, and meeting proceedings can be found on the
Canadian Science Advice Secratariat (CSAS) website: http://www.isdm-gdsi.gc.ca/csas-sccs/applications/Publications/search-recherche-eng.asp 

* <2018: only input data.
* 2018: assessment of 2019 (see fit.R)
  - Science Advisory Report: https://www.dfo-mpo.gc.ca/csas-sccs/Publications/SAR-AS/2019/2019_035-eng.pdf
  - Research document: http://www.dfo-mpo.gc.ca/csas-sccs/Publications/ResDocs-DocRech/2020/2020_013-eng.pdf 
* 2020: assessment of 2021 (see fit.R)
  - Science Advisory Report: https://www.dfo-mpo.gc.ca/csas-sccs/Publications/SAR-AS/2021/2021_029-eng.html
  - Research document: https://www.dfo-mpo.gc.ca/csas-sccs/Publications/ResDocs-DocRech/2022/2022_045-eng.html
* 2022: assessment of 2023 (see fit.R)
  - Science Advisory Report: ...
  - Research document: ...

# Information

The stock assessment used is a censured catch at age model (CCAM), which requires installation of Rtools and TMB to compile and read C++ code.

* Rtools:	https://cran.r-project.org/bin/windows/Rtools/
* TMB:		install.packages('TMB')
* CCAM:		devtools::install_github("elisvb/CCAM")
* Additional R packages used/needed are detailed in the scripts. 

# Data and model output

* input data : data/assessmentyear/...
* fitted model : Rdata/assessmentyear/fit.Rdata
* model summary : csv/assessmentyear/out.csv
* short-term projections : Rdata/2020/proj/...


