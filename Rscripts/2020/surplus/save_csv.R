#################################################################################################################
#*** Mackerel assessment
#*** get CSV files with output
#*** based on CCAM package
#################################################################################################################
year <-2021
wdcsv <- paste0('csv/',year,'/')
dir.create(wdcsv, showWarnings = FALSE,recursive = T)

res <- data.frame(ssb=ssbtable(fit)[,1],
                  rec=rectable(fit)[,1],
                  f=fbartable(fit)[,1],
                  catch=catchtable(fit)[,1],
                  # exploit=exptable(fit)[,1]*100,   # gives weird value???? old SAM throwback?
                  age=rowSums(sweep(ntable(fit),2,fit$conf$minAge:fit$conf$maxAge,'*'))/rowSums(ntable(fit)))
res$exploit<-1-exp(-res$f)
res <- round(res,2)
write.csv(res,paste0(wdcsv,"/out.csv"))

n <- ntable(fit)
n <- round(n/1000,2)
write.csv(n,paste0(wdcsv,"/n.csv"))

nprop <- ntable(fit)
nprop <- prop.table(nprop, margin = 1)
nprop <- round(nprop,2)
write.csv(nprop,paste0(wdcsv,"/nprop.csv"))

f <- faytable(fit)
f <- round(f,2)
write.csv(f,paste0(wdcsv,"/f.csv"))

expl <- 1-exp(-faytable(fit))
expl <- round(expl,2)
write.csv(expl,paste0(wdcsv,"/expl.csv"))

pars <- partable(fit)
pars <- round(pars,2)
write.csv(pars,paste0(wdcsv,"/par.csv"))
