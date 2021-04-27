# Northwest Atlantic Mackerel stock assessment

Rproject to perform the stock assessment for the northern contingent of the Northwest Atlantic stock of Atlantic mackerel 

The year refers to the last year of data used in the assessment and not the assessment year.

## Previous assessments

Research documents, Stock Assessment Reports, and meeting proceedings can be found at either:
* The Canadian Science Advice Secratariat (CSAS) website: http://www.isdm-gdsi.gc.ca/csas-sccs/applications/Publications/search-recherche-eng.asp 
* The Federal Science Library: https://science-libraries.canada.ca/eng/home/
* or The North Atlantic Fisheries Organisation's (NAFO) publications site: https://www.nafo.int/Publications

The assessments performed by the U.S.A. evaluates the entire Northwest Atlantic stock (northern and southern spawning contingents). Assessments are done by NOAA Fisheries' Northeast Fisheries Science Centre (NEFSC). Documents can be found here: https://apps-nefsc.fisheries.noaa.gov/saw/reviews_report_options.php

Data and model output for the 2016, and 2018 assessments can be found in the master branch here: https://github.com/iml-assess/mackerel_assessment/
* 2016: input data for the 2017 assessment. A different model was used back then and hence no output is provided.
* 2018: assessment of 2019
  - Science Advisory Report: https://www.dfo-mpo.gc.ca/csas-sccs/Publications/SAR-AS/2019/2019_035-eng.pdf
  - Research document: http://www.dfo-mpo.gc.ca/csas-sccs/Publications/ResDocs-DocRech/2020/2020_013-eng.pdf 

Data and preliminary results for the 2020 assessment can be found in the 2021 branch: https://github.com/iml-assess/mackerel_assessment/tree/mackerel_2021
* The assessment is scheduled for the 25-26th of February, 2021 and will be held virtually. 

# Information

The stock assessment used is a censured catch at age model (CCAM), which requires installation of Rtools and TMB to compile and read C++ code 

* Rtools:	https://cran.r-project.org/bin/windows/Rtools/
* TMB:		install.packages('TMB')
* CCAM:		devtools::install_github("elisvb/CCAM")
* Additional R packages used/needed are detailed in the scripts. 

# Data and model output


* input data : mackerel_assessment/data/2020/
* fitted model : mackerel_assessment/Rdata/2020/
* model summary : mackerel_assessment/csv/2020/
* short-term projections : mackerel_assessment/Rdata/2020/proj/

# Contacts

Andrew D. Smith Ph.D. : andrew.d.smith@dfo-mpo.gc.ca

