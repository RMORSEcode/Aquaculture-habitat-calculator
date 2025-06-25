# Plotting EFH for habitat manuscript
# Single plot with 4 life stages for BSB
library(ggplot2)
library(ggExtra)
library(patchwork)

ggplot() + 
  geom_polygon(data = w, aes(x=long, y = lat, group = group), fill = "grey80") + 
  geom_sf(data = BSBefh, aes(fill=factor(LIFESTAGE))) +
  coord_sf(1.3, xlim = c(-78,-68), ylim = c(36.5,45)) +
  theme_bw() +
  ggtitle("BSB EFH")


# Plots of individual stages as multiplot
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
  geom_sf(data = BSBefh %>% dplyr::filter(LIFESTAGE=="Larvae"), aes(color=factor(LIFESTAGE)), fill='#C77CFF') +
  geom_sf(data = BSBefh %>% dplyr::filter(LIFESTAGE=="Eggs"), aes(color=factor(LIFESTAGE)), fill='#7CAE00') +
  coord_sf(1.3, xlim = c(-78,-68), ylim = c(36.5,45)) +
  theme_bw()+ 
  # theme(legend.position.inside = c(.7, .95)
    # legend.justification = c("right", "top"),
    # legend.box.just = "right",
    # legend.margin = margin(6, 6, 6, 6)
  # )+
  scale_color_manual(values=Col) +
  theme(legend.position='bottom')+
  ggtitle("Eggs and Larvae")

tiff(paste0(wd,"/manuscript/Phosphorus/Fig2AB_Morse_et_al_15_10.tiff"), height = 15, width = 15, units = 'cm', 
     compression = "lzw", res = 300)
# (p1 + p2 ) / (p3 + p4)  + plot_annotation(tag_levels = 'A')
p1 / p2  / p3  + plot_annotation(tag_levels = 'A')
p1 + p2  +  p3 + plot_annotation(tag_levels = 'A')
p1 + p2  + plot_annotation(tag_levels = 'A', tag_suffix = ')', title = 'Black sea bass Essential Fish Habitat',
                           theme = theme(plot.title = element_text(size = 16)))

dev.off()

p3  + theme(legend.position.inside = c(.7, .95))
