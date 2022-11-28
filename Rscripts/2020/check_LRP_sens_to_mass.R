fitbase<-fit

refBase <- ypr(fit)
REF <- refBase$f40ssb
LRP <- REF*0.40
USR <- REF*0.80

refcwraw <- ypr(cw_raw)
refcw25 <- ypr(cw_25)
refcw75 <- ypr(cw_75)

reffecold <- ypr(fec_old)
reffec25 <- ypr(fec_25)
reffec75 <- ypr(fec_75)
reffecraw <- ypr(fec_raw)

refmoraw <- ypr(mo_raw)
refmo25 <- ypr(mo_25)
refmo75 <- ypr(mo_75)
 
REFcwraw <- refcwraw$f40ssb
LRPcwraw <- REFcwraw*0.40

REFcw25 <- refcw25$f40ssb
LRPcw25 <- REFcw25*0.40

REFcw75 <- refcw75$f40ssb
LRPcw75 <- REFcw75*0.40


df <- data.frame(LRP = c(LRP, LRPcwraw, LRPcw25, LRPcw75), 
                 name = c("core model 50% mass smooth", "no smooth", "25% smooth", "75% smooth"))
df %>% ggplot(aes(name,LRP)) + geom_point()

