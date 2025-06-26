# Plotting EFH for habitat manuscript
# Single plot with 4 life stages for BSB
library(ggplot2)
library(ggExtra)
library(patchwork)
wd1='C:/Users/ryan.morse/Documents/Aquaculture/Shellfish permitting and ecosystem services/Shellfish Calculators/Habitat/Manuscript'
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
Tplot <- reactive({
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
  
  # plot(monT$minT~monT$month, type='l', ylim=c(0,30), lty=1, lwd=1, col='blue', ylab='Monthly Surface Temperature (°C)', xlab='Month', las=1, bty='l')
  # lines(monT$maxT, lty=1, lwd=1, col='red')
  # lines(monT$meanT, lty=1, col='gray30', lwd=2)
  # legend('topleft', horiz=T, lty=c(1,1,1), lwd=c(1,2,1), legend=c('Max', 'Avg', 'Min'), col=c('red', 'gray30', 'blue'), bty='n')
  # abline(h=9, lty=3)#scup 9-27 (16-22 preferred) EFH NMFS-NE-149 1999 Table 1 p.14
  # abline(h=27, lty=3)
  # abline(h=10, lty=2)#BSB 10-22 (17-21 preferred) EFH NMFS-NE-200 2007, pp.8-9
  # abline(h=22, lty=2)
  # legend('bottomright', horiz=T, lty=c(3,2), lwd=c(1,1), legend=c('Scup', 'Black sea bass'), bty='n')
})

output$SSTplot <- 
  renderPlot({
    Tplot()
  })

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
