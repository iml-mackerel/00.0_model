ggl <- list(
    theme(legend.position = 'none'),
    scale_y_continuous(expand = c(0,0))
)
p1a <- prettymatplot(sim*100)+geom_point()+labs(y='% missing')+ggl
p2tot <- pbase+
    geom_line(data=na.omit(melt(simtep)),aes(x=ny,y=value/tepscale,col=factor(nsim)),size=0.5)+
    scale_color_viridis_d()+
    theme(legend.position = 'none')

p2new <- pbase+
    geom_line(data=na.omit(melt(simtepnl)),aes(x=ny,y=value/tepscale,col=factor(nsim)),size=0.5)+
    scale_color_viridis_d()+
    theme(legend.position = 'none')

p2newobs <- pbase+
    geom_line(data=na.omit(melt(simnew)),aes(x=ny,y=value/tepscale,col=factor(nsim)),size=0.1)+
    scale_color_viridis_d()+
    theme(legend.position = 'none')

p2c <- ggplot()+
    geom_line(data=tep,aes(x=y,y=tep/tepscale),size=0.3,col='red')+
    geom_line(data=na.omit(melt(simnew)),aes(x=ny,y=value/tepscale),size=0.3)+
    scale_color_viridis_d()+
    facet_wrap(~nsim,scale='free')+
    labs(y='TEP outside sgulf')+
    theme(axis.line=element_blank(),axis.text.x=element_blank(),
          axis.text.y=element_blank(),axis.ticks=element_blank(),
          axis.title.x=element_blank(),
          axis.title.y=element_blank())


saveplot(p1a,'perc','img/2022/TEP',c(15,8))
saveplot(p2tot,'prod_tot','img/2022/TEP/',c(15,8))
saveplot(p2new,'prod_new','img/2022/TEP/',c(15,8))
saveplot(p2newobs,'prod_newobs','img/2022/TEP/',c(15,8))
saveplot(p2c,'prod_facet','img/2022/TEP/',c(50,50))

saveplot(grid.arrange(p1a,p2tot,p2newobs),'all','img/2022/TEP/',c(14,20))
