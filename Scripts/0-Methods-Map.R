myLocation <- c(lon = -111.42, lat = 56.732)
NYCMap <- get_map("New York", zoom = 11)
F<-get_map(location=myLocation,zoom = 12, maptype = "satellite")
ggmap(F) + 
  geom_point(data=dt, aes(x=lo,y=la,color=dt$season),size=dt$OP,alpha=0.6) + 
  theme_bw() + 
  xlab("Longitude") + 
  ylab("Latitude") + 
  theme(axis.text=element_text(size=12,face="bold"),
        axis.title=element_text(size=18,face="bold",family = "serif")) + 
  scale_color_manual(values=c("#FF0000", "#33ffff")) + 
  guides(shape = guide_legend(override.aes = list(size = 8))) + 
  theme(legend.title=element_blank()) + 
  theme(legend.text=element_text(size=15,face="bold")) + 
  theme(legend.position=c(0.85,0.85))