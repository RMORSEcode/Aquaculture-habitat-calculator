# Plotting EFH for habitat manuscript
# Single plot with 4 life stages for BSB
library(dplyr)
library(ggplot2)
library(ggExtra)
library(patchwork)
library(maps)
library(mapdata)
library(terra)
library(sf); sf_use_s2(FALSE)

wd1='C:/Users/ryan.morse/Documents/Aquaculture/Shellfish permitting and ecosystem services/Shellfish Calculators/Habitat/Manuscript/'
#_________________________________________________________________
### Figure 1 ### Map of region and samples
data(stateMapEnv)
wd='C:/Users/ryan.morse/Documents/Aquaculture/Shellfish permitting and ecosystem services/Shellfish Calculators/Habitat/'
stations=readxl::read_xlsx(paste(wd, "FarmLocation.xlsx", sep=''),sheet='Farms')
## plot map
tiff(paste0(wd1,"Fig1map_Morse_et_al_12_17.tiff"), height = 12, width = 17, units = 'cm', 
     compression = "lzw", res = 300)
par(mar = c(2,2,0,0)) #(b,l,t,r)
par(oma = c(2,2,0,0))
map("worldHires", xlim=c(-79,-68),ylim=c(33,45), fill=T,border=0,col="gray70", xlab="Lon")
map('lakes', add=TRUE, fill=TRUE, col='white', boundary='black')
map.axes(las=1)
mtext(c("Longitude", "Latitude"), side=c(1,2), line = 2.5)
map('state', fill = F, add=T) # add state lines
points(stations$Longitude, stations$Latitude, pch=23, col='black', bg=ifelse(stations$GoPro=='yes','red','yellow'), cex=1.15)
legend(-76,36, pch=c(23,23), col='black', pt.bg=c('red', 'yellow'),legend=c('GoPro video', 'Observation'), bty='n', )
dev.off()


w <- map_data("world")


### EFH maps

ggplot() + 
  geom_polygon(data = w, aes(x=long, y = lat, group = group), fill = "grey80") + 
  geom_sf(data = BSBefh, aes(fill=factor(LIFESTAGE))) +
  coord_sf(1.3, xlim = c(-78,-68), ylim = c(36.5,45)) +
  theme_bw() +
  ggtitle("BSB EFH")

## Plots of individual stages as multiplot
p1=ggplot() + 
  geom_polygon(data = w, aes(x=long, y = lat, group = group), fill = "grey80") + 
  geom_sf(data = BSBefh %>% dplyr::filter(LIFESTAGE=="Adult"), fill='#F8766D') +
  coord_sf(1.3, xlim = c(-78,-68), ylim = c(36.5,45)) +
  theme_bw()+
  labs(x = "Longitude", y='Latitude')+
  ggtitle("Adult")
p2=ggplot() + 
  geom_polygon(data = w, aes(x=long, y = lat, group = group), fill = "grey80") + 
  geom_sf(data = BSBefh %>% dplyr::filter(LIFESTAGE=="Juvenile"), fill='#00BFC4') +
  coord_sf(1.3, xlim = c(-78,-68), ylim = c(36.5,45)) +
  theme_bw()+ 
  labs(x = "Longitude", y='Latitude')+
  ggtitle("Juvenile")
p3=ggplot() + 
  geom_polygon(data = w, aes(x=long, y = lat, group = group),fill = "grey80") + 
  # geom_sf(data = BSBefh %>% dplyr::filter(LIFESTAGE=="Larvae" | LIFESTAGE=="Eggs"), fill=factor(LIFESTAGE)) +
  geom_sf(data = BSBefh %>% dplyr::filter(LIFESTAGE=="Larvae"), aes(color='#C77CFF', fill='#C77CFF'))+ #aes(color=factor(LIFESTAGE)), fill=factor(LIFESTAGE)) +#fill='#C77CFF') +
  geom_sf(data = BSBefh %>% dplyr::filter(LIFESTAGE=="Eggs"), aes(color='#7CAE00', fill='#7CAE00'))+#'fill=#7CAE00') +
  coord_sf(1.3, xlim = c(-78,-68), ylim = c(36.5,45)) +
  theme_bw()+ 
  theme(legend.position.inside = c(.7, .95))+
    # legend.justification = c("right", "top"),
    # legend.box.just = "right",
    # legend.margin = margin(6, 6, 6, 6)
  # )+
  # scale_color_manual(values=Col) +
  # theme(legend.position='bottom')+
  ggtitle("Eggs and Larvae")

p3=ggplot() + 
  geom_polygon(data = w, aes(x=long, y = lat, group = group),fill = "grey80") + 
  geom_sf(data = BSBefh %>% dplyr::filter(LIFESTAGE=="Larvae"), aes(color=factor(LIFESTAGE)), fill='#C77CFF') +
  geom_sf(data = BSBefh %>% dplyr::filter(LIFESTAGE=="Eggs"), aes(color=factor(LIFESTAGE)), fill='#7CAE00') +
  coord_sf(1.3, xlim = c(-78,-68), ylim = c(36.5,45)) +
  theme_bw()+ 
  ggtitle("Eggs and Larvae")

tiff(paste0(wd1,"//Figx_Morse_et_al_15_15.tiff"), height = 15, width = 15, units = 'cm', 
     compression = "lzw", res = 300)
# (p1 + p2 ) / (p3 + p4)  + plot_annotation(tag_levels = 'A')
p3
p1 / p2  / p3  + plot_annotation(tag_levels = 'A')
p1 + p2  +  p3 + plot_annotation(tag_levels = 'A',tag_suffix = ')', title = 'Black sea bass Essential Fish Habitat',
                                 theme = theme(plot.title = element_text(size = 16)))
p1 + p2  + plot_annotation(tag_levels = 'A', tag_suffix = ')', title = 'Black sea bass Essential Fish Habitat',
                           theme = theme(plot.title = element_text(size = 16)))

dev.off()

p3  + theme(legend.position.inside = c(.7, .95))

# Plots of individual stages as multiplot
p4=ggplot() + 
  geom_polygon(data = w, aes(x=long, y = lat, group = group), fill = "grey80") + 
  geom_sf(data = SCUPefh %>% dplyr::filter(LIFESTAGE=="Adult"), fill='#F8766D') +
  coord_sf(1.3, xlim = c(-78,-68), ylim = c(36.5,45)) +
  theme_bw()+
  labs(x = "Longitude", y='Latitude')+
  ggtitle("Adult")
p5=ggplot() + 
  geom_polygon(data = w, aes(x=long, y = lat, group = group), fill = "grey80") + 
  geom_sf(data = SCUPefh %>% dplyr::filter(LIFESTAGE=="Juvenile"), fill='#00BFC4') +
  coord_sf(1.3, xlim = c(-78,-68), ylim = c(36.5,45)) +
  theme_bw()+ 
  labs(x = "Longitude", y='Latitude')+
  ggtitle("Juvenile")
p6=ggplot() + 
  geom_polygon(data = w, aes(x=long, y = lat, group = group),fill = "grey80") + 
  # geom_sf(data = SCUPefh %>% dplyr::filter(LIFESTAGE=="Larvae" | LIFESTAGE=="Eggs"), fill=factor(LIFESTAGE)) +
  geom_sf(data = SCUPefh %>% dplyr::filter(LIFESTAGE=="Larvae"), aes(color=factor(LIFESTAGE)), fill='#C77CFF') +
  geom_sf(data = SCUPefh %>% dplyr::filter(LIFESTAGE=="Eggs"), aes(color=factor(LIFESTAGE)), fill='#7CAE00') +
  coord_sf(1.3, xlim = c(-78,-68), ylim = c(36.5,45)) +
  theme_bw()+ 
  ggtitle("Eggs and Larvae")

tiff(paste0(wd1,"//Figx2_Morse_et_al_15_15.tiff"), height = 15, width = 15, units = 'cm', 
     compression = "lzw", res = 300)
p4 + p5  + plot_annotation(tag_levels = 'A', tag_suffix = ')', title = 'Black sea bass Essential Fish Habitat',
                           theme = theme(plot.title = element_text(size = 16)))
dev.off()

# Both BSB and Scup
## adult + juv
tiff(paste0(wd1,"//Figx3_Morse_et_al_15_15.tiff"), height = 15, width = 15, units = 'cm', 
     compression = "lzw", res = 300)
(p1 | p2 ) / (p4 | p5 ) +plot_annotation(tag_levels = 'A', tag_suffix = ')', title = 'Essential Fish Habitat',
                                                 theme = theme(plot.title = element_text(size = 16)))
dev.off()

#Adt, Jv, Lrv+Egg
(p1 | p2 | p3) / (p4 | p5 | p6) +plot_annotation(tag_levels = 'A', tag_suffix = ')', title = 'Essential Fish Habitat',
                                    theme = theme(plot.title = element_text(size = 16)))

dev.off()


### Plot for NRHA survey abundance ###
## Annual
tiff(paste0(wd1,"//Figx1a_Morse_et_al_15_15.tiff"), height = 15, width = 15, units = 'cm', 
     compression = "lzw", res = 300)
par(mfrow = c(1, 1))
maps::map("worldHires", xlim=c(-77,-65),ylim=c(35,45), fill=T,border=0,col="gray90")
map.axes(las=1)
plot(NRHA.val["bsbYRhab"], breaks=c(0,1,2,3),at=c(0,1,2,3), add=T, col=c('azure', 'lightskyblue', 'steelblue', 'royalblue4')[as.numeric(NRHA.val$bsbYRhab)])
# plot(NRHA.val["bsbYRhab"], breaks=c(0,1,2,3),at=c(0,1,2,3), add=T, col=c('white','lightgoldenrodyellow' , 'goldenrod', 'tomato2')[as.numeric(NRHA.val$bsbYRhab)])
map('state', fill = F, add=T)
legend(-72, 39, bty='n', legend=c('Not observed', 'Low', 'Med', 'High'), pch=21, cex=1.5, col='black', pt.bg=c('azure', 'lightskyblue', 'steelblue', 'royalblue4'))
text(-69.5, 39,"Survey Presence", cex=1.5)
dev.off()


tiff(paste0(wd1,"//Figx1_Morse_et_al_15_15.tiff"), height = 30, width = 30, units = 'cm', 
     compression = "lzw", res = 300)
par(mfrow = c(2, 2)) 
## Spring
maps::map("worldHires", xlim=c(-77,-65),ylim=c(35,45), fill=T,border=0,col="gray90")
map.axes(las=1)
plot(NRHA.val["bsbSPhab"], breaks=c(0,1,2,3),at=c(0,1,2,3), add=T, col=c('azure', 'lightskyblue', 'steelblue', 'royalblue4')[as.numeric(NRHA.val$bsbSPhab)])
map('state', fill = F, add=T)
legend(-72, 39, bty='n', legend=c('Not observed', 'Low', 'Med', 'High'), pch=21, cex=1.5, col='black', pt.bg=c('azure', 'lightskyblue', 'steelblue', 'royalblue4'))
text(-69.5, 39,"Spring Presence", cex=1.5)
## Summer
maps::map("worldHires", xlim=c(-77,-65),ylim=c(35,45), fill=T,border=0,col="gray90")
map.axes(las=1)
plot(NRHA.val["bsbSMhab"], breaks=c(0,1,2,3),at=c(0,1,2,3), add=T, col=c('azure', 'lightskyblue', 'steelblue', 'royalblue4')[as.numeric(NRHA.val$bsbSMhab)])
map('state', fill = F, add=T)
legend(-72, 39, bty='n', legend=c('Not observed', 'Low', 'Med', 'High'), pch=21, cex=1.5, col='black', pt.bg=c('azure', 'lightskyblue', 'steelblue', 'royalblue4'))
text(-69.5, 39,"Summer Presence", cex=1.5)
## Fall
maps::map("worldHires", xlim=c(-77,-65),ylim=c(35,45), fill=T,border=0,col="gray90")
map.axes(las=1)
plot(NRHA.val["bsbFLhab"], breaks=c(0,1,2,3),at=c(0,1,2,3), add=T, col=c('azure', 'lightskyblue', 'steelblue', 'royalblue4')[as.numeric(NRHA.val$bsbFLhab)])
map('state', fill = F, add=T)
legend(-72, 39, bty='n', legend=c('Not observed', 'Low', 'Med', 'High'), pch=21, cex=1.5, col='black', pt.bg=c('azure', 'lightskyblue', 'steelblue', 'royalblue4'))
text(-69.5, 39,"Fall Presence", cex=1.5)
## Winter
maps::map("worldHires", xlim=c(-77,-65),ylim=c(35,45), fill=T,border=0,col="gray90")
map.axes(las=1)
plot(NRHA.val["bsbWThab"], breaks=c(0,1,2,3),at=c(0,1,2,3), add=T, col=c('azure', 'lightskyblue', 'steelblue', 'royalblue4')[as.numeric(NRHA.val$bsbWThab)])
map('state', fill = F, add=T)
legend(-72, 39, bty='n', legend=c('Not observed', 'Low', 'Med', 'High'), pch=21, cex=1.5, col='black', pt.bg=c('azure', 'lightskyblue', 'steelblue', 'royalblue4'))
text(-69.5, 39,"Winter Presence", cex=1.5)
dev.off()

tiff(paste0(wd1,"//Figx1b_Morse_et_al_15_15.tiff"), height = 15, width = 15, units = 'cm', 
     compression = "lzw", res = 300)
par(mfrow = c(1, 1))
maps::map("worldHires", xlim=c(-77,-65),ylim=c(35,45), fill=T,border=0,col="gray90")
map.axes(las=1)
plot(NRHA.val["scupYRhab"], breaks=c(0,1,2,3),at=c(0,1,2,3), add=T, col=c('azure', 'lightskyblue', 'steelblue', 'royalblue4')[as.numeric(NRHA.val$scupYRhab)])
# plot(NRHA.val["scupYRhab"], breaks=c(0,1,2,3),at=c(0,1,2,3), add=T, col=c('white', 'gold', 'darkgoldenrod','tomato2')[as.numeric(NRHA.val$scupYRhab)])
map('state', fill = F, add=T)
# legend(-72, 39, bty='n', legend=c('Not observed', 'Low', 'Med', 'High'), pch=21, cex=1.5, col='black', pt.bg=c('white', 'gold', 'darkgoldenrod','tomato2'))
legend(-72, 39, bty='n', legend=c('Not observed', 'Low', 'Med', 'High'), pch=21, cex=1.5, col='black', pt.bg=c('azure', 'lightskyblue', 'steelblue', 'royalblue4'))
text(-69.5, 39,"Survey Presence", cex=1.5)
dev.off()

tiff(paste0(wd1,"//Figx1c_Morse_et_al_15_15.tiff"), height = 30, width = 30, units = 'cm', 
     compression = "lzw", res = 300)
par(mfrow = c(2, 2)) 
## Spring
maps::map("worldHires", xlim=c(-77,-65),ylim=c(35,45), fill=T,border=0,col="gray90")
map.axes(las=1)
plot(NRHA.val["scupSPhab"], breaks=c(0,1,2,3),at=c(0,1,2,3), add=T, col=c('azure', 'lightskyblue', 'steelblue', 'royalblue4')[as.numeric(NRHA.val$scupSPhab)])
map('state', fill = F, add=T)
legend(-72, 39, bty='n', legend=c('Not observed', 'Low', 'Med', 'High'), pch=21, cex=1.5, col='black', pt.bg=c('azure', 'lightskyblue', 'steelblue', 'royalblue4'))
text(-69.5, 39,"Spring Presence", cex=1.5)
## Summer
maps::map("worldHires", xlim=c(-77,-65),ylim=c(35,45), fill=T,border=0,col="gray90")
map.axes(las=1)
plot(NRHA.val["scupSMhab"], breaks=c(0,1,2,3),at=c(0,1,2,3), add=T, col=c('azure', 'lightskyblue', 'steelblue', 'royalblue4')[as.numeric(NRHA.val$scupSMhab)])
map('state', fill = F, add=T)
legend(-72, 39, bty='n', legend=c('Not observed', 'Low', 'Med', 'High'), pch=21, cex=1.5, col='black', pt.bg=c('azure', 'lightskyblue', 'steelblue', 'royalblue4'))
text(-69.5, 39,"Summer Presence", cex=1.5)
## Fall
maps::map("worldHires", xlim=c(-77,-65),ylim=c(35,45), fill=T,border=0,col="gray90")
map.axes(las=1)
plot(NRHA.val["scupFLhab"], breaks=c(0,1,2,3),at=c(0,1,2,3), add=T, col=c('azure', 'lightskyblue', 'steelblue', 'royalblue4')[as.numeric(NRHA.val$scupFLhab)])
map('state', fill = F, add=T)
legend(-72, 39, bty='n', legend=c('Not observed', 'Low', 'Med', 'High'), pch=21, cex=1.5, col='black', pt.bg=c('azure', 'lightskyblue', 'steelblue', 'royalblue4'))
text(-69.5, 39,"Fall Presence", cex=1.5)
## Winter
maps::map("worldHires", xlim=c(-77,-65),ylim=c(35,45), fill=T,border=0,col="gray90")
map.axes(las=1)
plot(NRHA.val["scupWThab"], breaks=c(0,1,2,3),at=c(0,1,2,3), add=T, col=c('azure', 'lightskyblue', 'steelblue', 'royalblue4')[as.numeric(NRHA.val$scupWThab)])
map('state', fill = F, add=T)
legend(-72, 39, bty='n', legend=c('Not observed', 'Low', 'Med', 'High'), pch=21, cex=1.5, col='black', pt.bg=c('azure', 'lightskyblue', 'steelblue', 'royalblue4'))
text(-69.5, 39,"Winter Presence", cex=1.5)
dev.off()

## Tidal range (m)
maps::map("worldHires", xlim=c(-77,-65),ylim=c(35,45), fill=T,border=0,col="gray90")
map.axes(las=1)
map('state', fill = F, add=T)
plot(tidalRangeM, add=T)
#convert the raster to points for plotting
map.p <- raster::rasterToPoints(tidalRangeM)
#Make the points a dataframe for ggplot
df <- data.frame(map.p)
#Make appropriate column headings
colnames(df) <- c("Longitude", "Latitude", "MAP")
# ggplot(data=df, aes(y=Latitude, x=Longitude)) +
#   geom_raster(aes(fill=MAP)) +
#   theme_bw() +
#   coord_equal() 
w <- map_data("worldHires", xlim=c(-78,-68),ylim=c(36.5,45))
p1=ggplot() + 
  geom_polygon(data = w, aes(x=long, y = lat, group = group), fill = "grey80") + 
  geom_tile(data=df, aes(x=Longitude, y=Latitude, fill=MAP)) +
  coord_sf(1.3, xlim = c(-78,-68), ylim = c(36.5,45)) +
  theme_bw() +
  # ggtitle("Tidal Range")+
  labs(fill=c("Tidal Range (m)"))+
  xlab("Longitude")+
  ylab("Latitude")+
  scale_fill_gradientn(colours = terrain.colors(7), limits=c(0,5))
  # scale_fill_gradient(low = "gray70", high = "black", limits=c(0,6), na.value = NA)
tiff(paste0(wd1,"Figx4_Morse_et_al_15_15.tiff"), height = 15, width = 15, units = 'cm', 
     compression = "lzw", res = 300)
p1
dev.off()

### CONNMAP Sediments ###
Conmap=sf::st_read(paste0(wd,"Habitat/Continental_Margin_Mapping_Program_(CONMAP)/Continental_Margin_Mapping_Program_(CONMAP).shp"))
cmap=sf::st_crop(Conmap, xmin=-77, ymin=35, xmax=-66.98481, ymax=60.83483)
w <- map_data("worldHires", xlim=c(-78,-68),ylim=c(36.5,45))
p2=ggplot() + 
  geom_polygon(data = w, aes(x=long, y = lat, group = group), fill = "grey80") + 
  geom_sf(data = cmap, aes(fill=factor(SEDNUM))) +
  coord_sf(1.3, xlim = c(-78,-68), ylim = c(36.5,45)) +
  theme_bw() +
  # ggtitle("CONMAP Sediments")+
  scale_fill_discrete(labels = c("Bedrock","Gravel","Gravel-sand","Sand","Clay-silt/sand","Sand-clay/silt","Clay","Sand-silt/clay","Sand/silt/clay"))+
  labs(fill=c("Sediment Class"))+
  xlab("Longitude")+
  ylab("Latitude")
tiff(paste0(wd1,"Figx5_Morse_et_al_15_15.tiff"), height = 15, width = 15, units = 'cm', 
     compression = "lzw", res = 300)
p2
dev.off()




### plotting OISST for milford
netCDF_file <- "sst.mon.mean.nc.nc4"
nc1 <- nc_open(netCDF_file)
time <- ncvar_get(nc1, "time")
nc_close(nc1)
time <- as.Date(time, origin="1800-01-01")
YM=format(time, "%Y-%m")
r <- brick(netCDF_file, varname = "sst")
r2=terra::rotate(r, left=T) 
vals <- extract(r2, matrix(c(lonx, latx), ncol = 2))
valx=data.frame(vals[,1:length(vals)])
valx$time=time
colnames(valx)=c('sst', 'time')
valx$month=as.numeric(format(time, "%m"))
valx$year=as.numeric(format(time, "%Y"))
monT=valx %>% 
  dplyr::filter(year!=2025) %>%
  group_by(month) %>%
  summarize(minT=min(sst),
            maxT=max(sst),
            meanT=mean(sst))
yearT=valx %>% 
  dplyr::filter(year!=2025) %>%
  group_by(year) %>%
  summarize(minT=min(sst),
            maxT=max(sst),
            meanT=mean(sst))

## Plot monthly
  P=ggplot(monT, aes(x=month, y=meanT))+
    geom_line(stat = "identity", linetype=1, linewidth=2)+
    # theme_minimal()+
    theme_bw() +
    theme(panel.border = element_blank(), 
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.line = element_line(colour = "black"))+
    ylim(-5,30)+
    ylab('Monthly Surface Temperature (°C)')+
    xlab("Month")+
    scale_x_continuous(breaks=c(2,4,6,8,10,12)) +
    theme(axis.title.x = element_text(size = 16),
          axis.text.x = element_text(size = 14),
          axis.text.y = element_text(size = 14),
          axis.title.y = element_text(size = 16))
  P=P+geom_ribbon(aes(ymin=`minT`, ymax=`maxT`), linetype=2, alpha=0.1)
  P=P+geom_ribbon(aes(ymin=9, ymax=27), linetype=2, alpha=0.1, fill='#F8766D')
  P=P+geom_ribbon(aes(ymin=10, ymax=22), linetype=2, alpha=0.1, fill='#619CFF')
  tiff(paste0(wd1,"//Figx6_Morse_et_al_15_15.tiff"), height = 15, width = 15, units = 'cm', 
       compression = "lzw", res = 300)
  P
  dev.off()

plot(monT$minT~monT$month, type='l', ylim=c(0,30), lty=1, lwd=1, col='blue', main='Black Sea Bass',ylab='Monthly Temperature (°C)', xlab='Month', las=1, bty='l')
lines(monT$maxT, lty=1, lwd=1, col='red')
lines(monT$meanT, lty=1, col='gray30', lwd=2)
legend('topleft', lty=c(1,1,1), lwd=c(1,2,1), legend=c('Max Temp', 'Avg Temp', 'Min Temp'), col=c('red', 'gray30', 'blue'), bty='n')
abline(h=11, lty=3)
abline(h=22, lty=3)
## Plot annual
plot(yearT$minT~yearT$year, type='l', ylim=c(0,30), lty=1, lwd=1, col='blue', ylab='Annual Temperature (°C)', xlab='Month', las=1, bty='l')
lines(yearT$maxT~yearT$year, lty=1, lwd=1, col='red')
lines(yearT$meanT~yearT$year, lty=1, col='gray30', lwd=2)
legend('topleft', lty=c(1,1,1), lwd=c(1,2,1), legend=c('Max Temp', 'Avg Temp', 'Min Temp'), col=c('red', 'gray30', 'blue'), bty='n')
# plot(valx$sst~valx$time, type='l', ylab='SST', xaxt = "n")
# axis(1, valx$date, format(valx$time, "%Y-%m"),cex.axis = .7)
plot(valx$sst~valx$time, type='l', ylab='SST', xlab='Date')
