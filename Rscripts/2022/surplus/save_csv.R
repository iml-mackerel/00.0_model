res <- data.frame(ssb0=ssb0table(fit)[,1],
                  ssb=ssbtable(fit)[,1],
                  rec=rectable(fit)[,1],
                  f=fbartable(fit)[,1],
                  catch=catchtable(fit)[,1],
                  exploit=exptable(fit)[,1]*100,
                  age=rowSums(sweep(ntable(fit),2,fit$conf$minAge:fit$conf$maxAge,'*'))/rowSums(ntable(fit)),
                  LRP=ypr(fit)$LRP,
                  ratio=ssbtable(fit)[,1]/ypr(fit)$LRP)
res <- round(res,2)
write.csv(res,paste0("csv/",year,"/out.csv"))

n <- ntable(fit)
nrel <- round(sweep(n,1,rowSums(n),'/')*100,0)
n <- round(n/1000,2)
write.csv(n,paste0("csv/",year,"/n.csv"))
write.csv(nrel,paste0("csv/",year,"/nrel.csv"))

f <- faytable(fit)
f <- round(f,2)
write.csv(f,paste0("csv/",year,"/f.csv"))

expl <- 1-exp(-faytable(fit))
expl <- round(expl,2)
write.csv(expl,paste0("csv/",year,"/expl.csv"))

pars <- partable(fit)
pars <- round(pars,2)
write.csv(pars,paste0("csv/",year,"/par.csv"))

ssb <- ssbtable(fit)
ssb <- round(ssb,0)
write.csv(ssb,paste0("csv/",year,"/ssb.csv"),row.names = F)

ssb0 <- ssb0table(fit)
ssb0 <- round(ssb0,0)
write.csv(ssb0,paste0("csv/",year,"/ssb0.csv"),row.names = F)

rec <- rectable(fit)
rec <- round(rec,0)
write.csv(rec,paste0("csv/",year,"/rec.csv"),row.names = F)

tsb <- tsbtable(fit)
tsb <- round(tsb,0)
write.csv(tsb,paste0("csv/",year,"/tsb.csv"),row.names = F)
