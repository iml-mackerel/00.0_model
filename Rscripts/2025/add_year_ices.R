#remotes::install_github("fishfollower/SAM/stockassessment")
#ADD lines to ICES files automatically
#library(stockassessment)#always before CCAM 
source("R/writing_ices.R")
library(tidyverse)
library(CCAM)

#nyears= number of year to update. For new ziff files
add_rows_ices<- function(datname, csvfile, input_year, output_year,  name, nyears= 0){
    
    datfile=paste0("data/",input_year,"/",datname,".dat") 
    fileout=paste0("data/",output_year,"/",datname,".dat") 
    
      if(max(csvfile$year)==output_year){
        #nothing to do just add rows.
    } 
    
    if(max(csvfile$year) == as.numeric(output_year)-1){
        csvfile[nrow(csvfile)+1,]  <- csvfile[nrow(csvfile),]
        csvfile[nrow(csvfile),"year"] <- output_year
    }
    
    if(max(csvfile$year) == as.numeric(output_year)-2){
        csvfile[nrow(csvfile)+1,]  <- csvfile[nrow(csvfile),]
        csvfile[nrow(csvfile),"year"] <- as.numeric(output_year)-1
        csvfile[nrow(csvfile)+1,]  <- csvfile[nrow(csvfile),]
        csvfile[nrow(csvfile),"year"] <- output_year
    }
    
    datf<- read.ices(datfile) 
    
    if(nyears!=0){
        nr<- nrow(datf)
        datf <-  datf[-((nr-nyears)+1 : nr),]  
        year_start= as.numeric(input_year)  -nyears+ 1
    } 
    
    if(nyears==0)year_start= as.numeric(input_year) + 1     
    
    dat_to_write<- rbind(datf,
               csvfile %>%   dplyr::filter(year %in% (year_start : as.numeric(output_year))) %>% 
                   remove_rownames() %>% 
                   column_to_rownames("year")) 
    
    if(datname=="nm"){ dat_to_write <- csvfile %>%   
                                                remove_rownames() %>% 
                                                column_to_rownames("year")
    write.ices(as.matrix(dat_to_write), fileout=fileout, name=name)
    } else{
    if(ncol(dat_to_write) > 2) write.ices(as.matrix(dat_to_write), fileout=fileout, name=name)
    
    if(ncol(dat_to_write) == 2){
    header <- readLines(datfile, n=5)
    header<- gsub(header, pattern=input_year, replacement=output_year)
    writeLines(header, fileout)
    write.table(dat_to_write, fileout, sep="\t", 
                quote=FALSE, col.names=FALSE, row.names = FALSE, append=TRUE)
    }
    
    if(ncol(dat_to_write) == 1){
        header <- readLines(datfile, n=6)
        header<- gsub(header, pattern=input_year, replacement=output_year)
        writeLines(header, fileout)
        write.table(as.list(dat_to_write), fileout, sep="\t", 
                    quote=FALSE, col.names=FALSE, row.names = FALSE, append=TRUE)
    }
  }  
    return(paste0(datfile, " updated to ", fileout)) 
}

iny="2024"
list_of_files = list.files(paste0("data/",iny), full.names=T, pattern=".dat")

openy ="2025"  # file to open for interim files using last year data
outy="2025"
year_to_remove=3# update landings and caa from previous assessment.
#check if available otherwise copies from previous year

# l'entête ct ctForeign unrorted pas à l'âge., write ices change l'entête, est-ce que ça va marcher quand même &&&?????
#caa

#sensitivity

#caa with prior landings distribution
default<- read.csv(paste0("../02.0_catch-at-age/csv/",openy,"/caa_interpol.csv"))[1:3] %>% 
    pivot_wider(names_from=age, values_from="caan") %>%  as.data.frame() %>%  filter(!year %in% c(2022,2023))

caain2223 <- read.csv(paste0("../02.0_catch-at-age/csv/2024/caa_modified2022_2023.csv"), row.names=1)[1:3] %>% 
    pivot_wider(names_from=age, values_from="caan") %>%  as.data.frame() %>%  filter(year %in% c(2022,2023))
caanew<- bind_rows(default, caain2223) %>%  arrange(year)



add_rows_ices(datname="cn" , csvfile=caanew, nyears=year_to_remove,
              input_year=iny, output_year=outy, 
              name="NWA Mackerel catch-at-age - catch.n (units : Thousands )")


#Landings no yet calculated for 2024 # copied from 2023
#changer min et max pour 1 et 10 dans les années futures.
ctin<- read.csv(paste0("../01.0_landings/csv/",openy,"/total.csv")) %>% dplyr::select(year, total)%>%  as.data.frame() %>%  mutate(max=NA) %>%  rename(min=total)
add_rows_ices(datname="ct" , csvfile=ctin, input_year=iny, output_year=outy,nyears=3,
              name="NWA Mackerel catch total lower and upper bounds (units : metric tonnes)")


## catch foreign
ctfin<- read.csv(paste0("../01.0_landings/csv/",openy,"/total.csv")) %>% dplyr::select(year, Foreign)%>%  as.data.frame() %>%  mutate(max=NA) %>%  rename(min=Foreign)
add_rows_ices(datname="ctForeign" , csvfile=ctfin, input_year=iny, output_year=outy,
              name="NWA Mackerel, catch by foreign countries in US EEZ total lower and upper bounds (units : metric tonnes)")


## catch unreported
testval<- read.ices(paste0("data/",outy,"/ct.dat"))
testval = testval[as.character((as.numeric(iny) +1 -year_to_remove) : as.numeric(outy)),] %>%  as.data.frame() %>%
          rownames_to_column("year") %>%  mutate(min= if_else(min> 680, min *0.4, 680)) 

add_rows_ices(datname="ctUnaccounted" , csvfile=testval, input_year=iny, output_year=outy,nyears=year_to_remove,
              name="NWA Mackerel, Maximum amount of missing catch")

#catch USA
#copy landings to 00.0_model
US<- read.csv2("../01.0_landings/data/US/Landings_fromKiersten.csv")
write.csv2(US, file= paste0("data/",outy,"/raw/from_Kiersten.csv"))

source(paste0("Rscripts/",outy,"/surplus/perc_USrec.R"))

ctus1<- read.csv2(paste0("data/",openy,"/raw/from_Kiersten.csv"), dec=".") %>%  dplyr::rename(year=X)
ctus2<- read.csv(paste0("data/",openy,"/raw/USrec_yearly_during_winter.csv"), dec=".")  %>%  dplyr::select(year, perc_rec)
ctus3<- read.csv(paste0("data/",openy,"/raw/USrec_decades_during_winter.csv"), dec=".")  %>%  dplyr::select(decade, dec_perc_rec) %>%  distinct()

means<- ctus1 %>%  summarize(disc=mean(US.Comm.discards, na.rm=T), rec=mean(US.Recreational))


ctus4<- left_join(full_join(
    left_join(ctus1, ctus2) ,
                data.frame(year =seq(1969, outy))) %>%
        mutate(decade= year - year %% 10 ,
               decade=if_else(decade < 1980, 1980, decade))  , 
    ctus3) %>%  ungroup() %>% 
    dplyr::mutate(perc_rec= dplyr::coalesce(perc_rec, dec_perc_rec))

ctus <-  ctus4 %>% mutate(US.Total.Catch =US.Commercial + US.Comm.discards + (US.Recreational *perc_rec/100)) %>% dplyr::select(year, US.Total.Catch) %>%  as.data.frame() %>%  mutate(max=NA) %>%  
    rename(min=US.Total.Catch)
add_rows_ices(datname="ctUSA" , csvfile=ctus, input_year=iny, output_year=outy,nyears=34,
              name="NWA Mackerel USA catch - us vessels only. total lower and upper bounds (units : metric tonnes)")

#caa weight
caawin<- bind_rows(read.csv(paste0("../02.0_catch-at-age/csv/",openy,"/caa_interpol.csv")) %>%  
                       filter(!year %in% c(2022,2023)) %>%  
                       dplyr::select(-zero),
                   read.csv(paste0("../02.0_catch-at-age/csv/2024/caa_modified2022_2023.csv"), header=T, check.names =F)[,-1]%>% # column with rownames
                     filter(year !=2024)
                   ) %>% 
   as.data.frame()  %>%  dplyr::select(year, age, waa.interpol) %>%  
    mutate(waa.interpol=round(waa.interpol,3)) %>% 
    pivot_wider(names_from=age, values_from=waa.interpol) %>% arrange(year) %>% 
    mutate(year=as.character(year)) 
add_rows_ices(datname="cw" , csvfile=caawin, 
              input_year=iny, output_year=outy, nyears=year_to_remove,
              name="NWA Mackerel catch weight-at-age - catch.wt (units : Kg )")
#discarded weight
add_rows_ices(datname="dw" , csvfile=caawin, 
              input_year=iny, output_year=outy, nyears=year_to_remove, 
              name="NWA Mackerel discarded catch weight-at-age - catch.wt (units : Kg )")
#landed weight
add_rows_ices(datname="lw" , csvfile=caawin, 
              input_year=iny, output_year=outy, nyears=year_to_remove, 
              name="NWA Mackerel discarded catch weight-at-age - catch.wt (units : Kg )")


#stock weight
swin<- read.csv(paste0("../04.0_weight-at-age/csv/waa_",openy,"_base_cv1shrink0.csv"), header=T, check.names =F) %>% 
    as.data.frame()
add_rows_ices(datname="sw" , csvfile=swin, 
              input_year=iny, output_year=outy, 
              name="Mean Weight in Stock (kilograms)")

#stock weight january 1st
add_rows_ices(datname="sw0" , csvfile=swin, 
              input_year=iny, output_year=outy, 
              name="January 1 Weight in Stock (kilograms)		")


#faa 
faa<- read.csv(paste0("../05.0_fecundity-at-age/csv/",openy,"/faa_",openy,"_base_cv1shrink0.5.csv"), header=T, check.names =F) %>% 
    as.data.frame()
add_rows_ices(datname="fec" , csvfile=faa, 
              input_year=iny, output_year=outy, 
              name="NWA Mackerel fecundity (Pelletier 1986 reanalysed F ~ age * gsi for each year)")

#landed fraction
#just copy row
land<- read.ices(paste0("data/",iny,"/lf.dat")) %>%  as.data.frame() %>%  rownames_to_column("year")

add_rows_ices(datname="lf" , csvfile=land, input_year=iny, output_year=outy,
              name="Landed fraction")

#maa 
maa<- read.csv(paste0("../06.0_maturity-at-age/csv/",openy,"/maa",openy,"_base_smooth0.5.csv"), header=T, check.names =F) %>% 
    as.data.frame()
add_rows_ices(datname="mo" , csvfile=maa, 
              input_year=iny, output_year=outy, 
              name="Proportion Mature")

#natural mortality
#just copy row
#warning will only change new data
mort<- read.ices(paste0("data/",iny,"/nm.dat")) %>%  as.data.frame() %>%  rownames_to_column("year")
#adjsut natural mortality value if wish to change
mort[,2:11] <- 0.28 #0.28 starting 2025, 0.3 starting 2024, before was changed in 
add_rows_ices(datname="nm" , csvfile=mort, input_year=iny, output_year=outy,
              name="Natural Mortality")


#proportion F before spawning
#just copy row
propF<- read.ices(paste0("data/",iny,"/pf.dat")) %>%  as.data.frame() %>%  rownames_to_column("year")

add_rows_ices(datname="pf" , csvfile=propF, input_year=iny, output_year=outy,
              name="Proportion of F before Spawning	")

#proportion m before spawning
#just copy row
propm<- read.ices(paste0("data/",iny,"/pm.dat")) %>%  as.data.frame() %>%  rownames_to_column("year")

add_rows_ices(datname="pm" , csvfile=propm, input_year=iny, output_year=outy,
              name="Proportion of M before Spawning	")


#proportion Females before spawning
#just copy row
propfem<- read.ices(paste0("data/",iny,"/propFemale.dat")) %>%  as.data.frame() %>%  rownames_to_column("year")

add_rows_ices(datname="propFemale" , csvfile=propfem, input_year=iny, output_year=outy,
              name="NEA Mackerel proportion of females")



#TEP 
#do manually, did not find ways to do it.....attribute time
#or change skip oin writeLines
#tep<- read.ices(paste0("data/",outy,"/tep.dat"))

#ne fonctionne pas tout à fait
#tepp<- read.csv(paste0("../03.0_egg-index/csv/",outy,"/TEP_",outy,".csv")) %>%
 #   dplyr::select(year, TEP) %>%  remove_rownames() %>% 
 #   mutate(TEP = TEP/10^12) %>% 
 #   as.data.frame() %>%  arrange(year)
#tepp

#add_rows_ices(datname="tep" , csvfile=tepp, 
#              input_year=iny, output_year=outy, 
 #             name="Total egg production")

detach(package:tidyverse)
