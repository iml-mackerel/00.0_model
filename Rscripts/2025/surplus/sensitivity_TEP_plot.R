ggl <- list(
    theme(legend.position = 'none'),
    scale_y_continuous(expand = c(0,0))
)
tep$year

p1a <- prettymatplot(sim*100)+geom_point()+
    scale_y_continuous(name='% PTO manquant \n % missing TEP', breaks=seq(0, 100,20)) + 
    theme(legend.position = 'none')+ scale_x_continuous(name="Année | Year", labels=as.character(seq(1980, my.year,5)), breaks=seq(2,46, 5))
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


saveplot(p1a,'perc',paste0('img/',my.year,'/sensitivity/TEP'),c(15,8))
saveplot(p2tot,'prod_tot',paste0('img/',my.year,'/sensitivity/TEP/'),c(15,8))
saveplot(p2new,'prod_new',paste0('img/',my.year,'/sensitivity/TEP/'),c(15,8))
saveplot(p2newobs,'prod_newobs',paste0('img/',my.year,'/sensitivity/TEP/'),c(15,8))
saveplot(p2c,'prod_facet',paste0('img/',my.year,'/sensitivity/TEP/'),c(50,50))

saveplot(grid.arrange(p1a,p2tot,p2newobs),'all',paste0('img/',my.year,'/sensitivity/TEP/'),c(14,20))
