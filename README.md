# Northwest Atlantic Mackerel stock assessment

Rproject to run the stock assessment model for the northern contingent of Northwest Atlantic mackerel.

Years refer to the last year of data used in the assessment and not the assessment year.

## Previous assessments

Recent research documents, Stock Assessment Reports, and meeting proceedings can be found on the
Canadian Science Advice Secratariat (CSAS) website: http://www.isdm-gdsi.gc.ca/csas-sccs/applications/Publications/search-recherche-eng.asp 

* <2018: only input data.
* 2018: assessment of 2019 (see fit.R)
  - Science Advisory Report: https://csas-scas.dfo-mpo.gc.ca/publications-publications/fe601667-1f47-45e0-89f6-7ae694b0013c?lang=en
  - Research document: https://csas-scas.dfo-mpo.gc.ca/publications-publications/4425643c-58cd-4c65-a2b3-de960fe4229f?lang=en
* 2020: assessment of 2021 (see fit.R)
  - Science Advisory Report: https://csas-scas.dfo-mpo.gc.ca/publications-publications/811dd183-1c1c-4620-9c0a-8edb74184f27?lang=en
  - Research document:  https://csas-scas.dfo-mpo.gc.ca/publications-publications/e3b5f8c7-3b25-4bba-8e93-d5c131db0b03?lang=en
* 2022: assessment of 2023 (see fit.R)
  - Science Advisory Report: https://csas-scas.dfo-mpo.gc.ca/publications-publications/4aa8200f-aa77-4522-842d-41ef36305136?lang=en
  - Research document: https://csas-scas.dfo-mpo.gc.ca/publications-publications/4e12611e-06ef-4f4a-a1b3-1605a994610e?lang=en
* 2024: assessment of 2025 (see fit.R)
  - Science Advisory Report: https://csas-scas.dfo-mpo.gc.ca/publications-publications/2d6007ac-3fe1-4864-b738-7295367248fb?lang=en
  - Research document: https://csas-scas.dfo-mpo.gc.ca/publications-publications/065a7c5e-6e5b-4fdc-8110-efae64ada956?lang=en
* 2025: assessment of 2026 (see fit.R)
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
* short-term projections : Rdata/assessmentyear/proj/...


