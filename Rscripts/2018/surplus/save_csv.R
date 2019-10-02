res <- data.frame(ssb=ssbtable(fit)[,1],
                  rec=rectable(fit)[,1],
                  f=fbartable(fit)[,1],
                  catch=catchtable(fit)[,1],
                  exploit=exptable(fit)[,1]*100,
                  age=rowSums(sweep(ntable(fit),2,fit$conf$minAge:fit$conf$maxAge,'*'))/rowSums(ntable(fit)))
res <- round(res,2)
write.csv(res,paste0("csv/",year,"/out.csv"))

n <- ntable(fit)
n <- round(n/1000,2)
write.csv(n,paste0("csv/",year,"/n.csv"))

f <- faytable(fit)
f <- round(f,2)
write.csv(f,paste0("csv/",year,"/f.csv"))

expl <- 1-exp(-faytable(fit))
expl <- round(exp,2)
write.csv(expl,paste0("csv/",year,"/expl.csv"))

pars <- partable(fit)
pars <- round(pars,2)
write.csv(pars,paste0("csv/",year,"/par.csv"))
