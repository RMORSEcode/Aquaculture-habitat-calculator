library(leaflet)
library(leaflet.extras)
library(shiny)
library(shinyBS)
library(ggplot2)
library(ggmap)
library(maps)
library(mapdata)
library(sf); sf_use_s2(FALSE)

wd="C:/Users/ryan.morse/Documents/Aquaculture/Shellfish permitting and ecosystem services/Shellfish Calculators/"

# OISST high res monthly mean
# https://psl.noaa.gov/thredds/ncss/grid/Datasets/noaa.oisst.v2.highres/sst.mon.mean.nc/dataset.html

# New England Only, missing RI, no area
sf::st_layers(paste0(wd,"Habitat/Aquaculture/Aquaculture.gdb"))
test.shellmgt=sf::st_read(paste0(wd,"Habitat/Aquaculture/Aquaculture.gdb", layer="ShellfishManagementAreas"))
test.aq=sf::st_read(paste0(wd,"Habitat/Aquaculture/Aquaculture.gdb", layer="Aquaculture"))

sf::st_layers(paste0(wd,"Habitat/Restoration/Restoration.gdb"))
test.res.eel=sf::st_read(paste0(wd,"Habitat/Restoration/Restoration.gdb", layer="EelgrassMeadows"))
test.res.wet=sf::st_read(paste0(wd,"Habitat/Restoration/Restoration.gdb", layer="CoastalWetlands"))
test.res.pot=sf::st_read(paste0(wd,"Habitat/Restoration/Restoration.gdb", layer="PotentialRestorationProjects"))

# plot(test.aq["allSpecies"])
test.aq %>%
  leaflet() %>%
  addTiles() %>%
  setView(lng = -70, lat = 40, zoom = 5) %>%
  addPolygons(popup = paste("Site:", test.aq$siteName, "<br>", 
                            "Type:", test.aq$allSpecies, "<br>",
                            "Lease status:", test.aq$status, "<br>",
                            "Area:", test.aq$Shape__Area))
plot(test.res.eel["sav"])
plot(test.shellmgt[[9]])
plot(sf::st_geometry(test.shellmgt), axes=T)
plot(test.shellmgt["regClass"])

sf::st_layers(paste0(wd,"Habitat/Marine Cadastre/Aquaculture.shp"))
aquaculture=sf::st_read(paste0(wd,"Habitat/Marine Cadastre/Aquaculture.shp"))
NESaquaculture=sf::st_crop(aquaculture, xmin=-77, ymin=35, xmax=-66.98481, ymax=60.83483) %>% 
  dplyr::filter(biotype=="Shellfish") %>% dplyr::mutate(centroid=sf::st_centroid(geometry))
# sf::st_write(NESaquaculture, file="C:/Users/ryan.morse/Documents/GitHub/Aquaculture-habitat-calculator/NESaquaculture.shp")


## CONMAP sediment from USGS (via NY GIS)
# https://opdgig.dos.ny.gov/maps/NYSDOS::continental-margin-mapping-program-conmap
# https://woodshole.er.usgs.gov/openfile/of2005-1001/data/conmapsg/conmapsg-faq.htm
Conmap=sf::st_read(paste0(wd,"Habitat/Continental_Margin_Mapping_Program_(CONMAP)/Continental_Margin_Mapping_Program_(CONMAP).shp"))
cmap=sf::st_crop(Conmap, xmin=-77, ymin=35, xmax=-66.98481, ymax=60.83483)
w <- map_data("worldHires", xlim=c(-78,-68),ylim=c(36.5,45))
ggplot() + 
  geom_polygon(data = w, aes(x=long, y = lat, group = group), fill = "grey80") + 
  geom_sf(data = cmap, aes(fill=factor(SEDNUM))) +
  coord_sf(1.3, xlim = c(-78,-68), ylim = c(36.5,45)) +
  theme_bw() +
  ggtitle("CONMAP")
# with(Conmap, table(SEDIMENT, SEDNUM))
            # SEDNUM
#SEDIMENT     1   2   3   4   5   6   7   8   9
 # br         4   0   0   0   0   0   0   0   0 # Bedrock
 # cl         0   0   0   0   0   0   3   0   0 # Clay
 # cl-st/sd   0   0   0   0  51   0   0   0   0 # Clay-silt/sand
 # gr         0  42   0   0   0   0   0   0   0 # Gravel
 # gr-sd      0   0 102   0   0   0   0   0   0 # Gravel-sand
 # sd         0   0   0  39   0   0   0   0   0 # Sand
 # sd-cl/st   0   0   0   0   0  46   0   0   0 # Sand-clay/silt
 # sd-st/cl   0   0   0   0   0   0   0  23   0 # Sand-silt/clay
 # sd/st/cl   0   0   0   0   0   0   0   0  66 # Sand/silt/clay
# SEDNUM
# 1 Bedrock
# 2 Clay
# 3 Clay-silt/sand
# 4 Gravel
# 5 Gravel-sand
# 6 Sand
# 7 Sand-clay/silt
# 8 Sand-silt/clay
# 9 Sand/silt/clay
Conmap$SEDNAME=NA
Conmap$SEDNAME[which(Conmap$SEDIMENT=='br')]='Bedrock'
Conmap$SEDNAME[which(Conmap$SEDIMENT=='cl')]='Clay'
Conmap$SEDNAME[which(Conmap$SEDIMENT=='cl-st/sd')]='Clay-silt/sand'
Conmap$SEDNAME[which(Conmap$SEDIMENT=='gr')]='Gravel'
Conmap$SEDNAME[which(Conmap$SEDIMENT=='gr-sd')]='Gravel-sand'
Conmap$SEDNAME[which(Conmap$SEDIMENT=='sd')]='Sand'
Conmap$SEDNAME[which(Conmap$SEDIMENT=='sd-cl/st')]='Sand-clay/silt'
Conmap$SEDNAME[which(Conmap$SEDIMENT=='sd-st/cl')]='Sand-silt/clay'
Conmap$SEDNAME[which(Conmap$SEDIMENT=='sd/st/cl')]='Sand/silt/clay'

RIaqua=sf::st_crop(aquaculture, xmin=-71.82, ymin=41.12, xmax=-71.11, ymax=41.74)
plot(RIaqua["biotype"])
RIaqua %>%
  leaflet() %>%
  addTiles() %>%
  addPolygons(popup = paste("Site:", RIaqua$sitename, "<br>", 
                          "Type:", RIaqua$biotype, "<br>",
                          "Lease status:", RIaqua$leasestatu, "<br>",
                          "Area:", round(RIaqua$SHAPE__Are, 0)))
              
# NESaqua=sf::st_filter(NESaquaculture, biotype="Shellfish")
NESaquaculture %>%
  leaflet() %>%
  addTiles() %>%
  setView(lng = -70, lat = 40, zoom = 5) %>%
  addPolygons(popup = paste("Site:", NESaquaculture$sitename, "<br>", 
                            "ID:", NESaquaculture$objectid, "<br>",
                            "Type:", NESaquaculture$biotype, "<br>",
                            "Lease status:", NESaquaculture$leasestatu, "<br>",
                            "Area:", round(NESaquaculture$SHAPE__Are,0), "<br>",
                            "Lon, Lat:", NESaquaculture$centroid))

# NEStest=NESaquaculture %>%
#   dplyr::filter(biotype=="Shellfish")
  
## join NESaquaculture and test.aq
# newAq=sf::st_join(NESaquaculture, test.aq, join = st_nearest_feature, left = T)
NEWENGefh=sf::read_sf(paste(wd,"/Habitat/EFH/neweng_efh/neweng_efh.shp",sep=''))
MIDATLefh=sf::read_sf(paste(wd,"/Habitat/EFH/midatl_efh/midatl_efh.shp",sep=''))
MIDefh=sf::st_transform(MIDATLefh, "WGS84")
NEWefh=sf::st_transform(NEWENGefh, "WGS84")
BSBefh=MIDefh %>% dplyr::filter(SITENAME_L=="Black Sea Bass")
SCUPefh=MIDefh %>% dplyr::filter(SITENAME_L=="Scup")

BSBefh %>%
  dplyr::filter(LIFESTAGE=="Larvae") %>%
  leaflet() %>%
  addTiles() %>%
  setView(lng = -70, lat = 40, zoom = 5) %>%
  addPolygons(popup = paste("Site:", NESaquaculture$sitename, "<br>", 
                            "ID:", NESaquaculture$objectid, "<br>",
                            "Type:", NESaquaculture$biotype, "<br>",
                            "Lease status:", NESaquaculture$leasestatu, "<br>",
                            "Area:", round(NESaquaculture$SHAPE__Are,0), "<br>",
                            "Lon, Lat:", NESaquaculture$centroid))

map("worldHires", xlim=c(-78,-68),ylim=c(36.5,45), fill=T,border=0,col="gray70")
map.axes(las=1)
BSBefh %>%
  dplyr::filter(LIFESTAGE=="Larvae") %>%
  ggplot(., geometry, col='green', add=T) #BSB Juv

plot(MIDefh$geometry[40], col='blue', add=T) #BSB adult
plot(MIDefh$geometry[3], col='red', add=T) # BSB eggs

# bbox <- make_bbox(lon=c(-78,-68),lat=c(36.5,45))
# map_big <- get_map(location = bbox, maptype = "terrain", source = "google")
w <- map_data("worldHires", xlim=c(-78,-68),ylim=c(36.5,45))
ggplot() + 
  geom_polygon(data = w, aes(x=long, y = lat, group = group), fill = "grey80") + 
  geom_sf(data = BSBefh, aes(fill=factor(LIFESTAGE))) +
  coord_sf(1.3, xlim = c(-78,-68), ylim = c(36.5,45)) +
  theme_bw() +
  ggtitle("BSB EFH")

ggplot() + 
  geom_polygon(data = w, aes(x=long, y = lat, group = group), fill = "grey80") + 
  geom_sf(data = SCUPefh, aes(fill=factor(LIFESTAGE))) +
  coord_sf(1.3, xlim = c(-78,-68), ylim = c(36.5,45)) +
  theme_bw() +
  ggtitle("Scup EFH")

ggplot() + 
geom_polygon(data = w, aes(x=long, y = lat, group = group), fill = "grey80") + 
geom_sf(data = BSBefh %>% dplyr::filter(LIFESTAGE=="Adult"), fill='green') +
coord_sf(1.3, xlim = c(-78,-68), ylim = c(36.5,45)) +
theme_bw()  

ggplot() + 
  geom_polygon(data = w, aes(x=long, y = lat, group = group), fill = "grey80") + 
  geom_sf(data = BSBefh %>% dplyr::filter(LIFESTAGE=="Juvenile"), fill='lightblue') +
  coord_sf(1.3, xlim = c(-78,-68), ylim = c(36.5,45)) +
  theme_bw()+ 
  ggtitle("BSB Juv EFH")

ggplot() + 
  geom_polygon(data = w, aes(x=long, y = lat, group = group), fill = "grey80") + 
  geom_sf(data = SCUPefh %>% dplyr::filter(LIFESTAGE=="Juvenile"), fill='lightblue') +
  coord_sf(1.3, xlim = c(-78,-68), ylim = c(36.5,45)) +
  theme_bw()+ 
  ggtitle("Scup Juv EFH")

# checking on EFH at a point
check=sf::st_point(c(-73.062097, 41.189598)) # Milford dense cage farm
check=sf::st_point(c(-74.302611, 39.603333)) # Rose Cove
int <- sf::st_intersects(check, MIDefh$geometry)
x=MIDefh$SITENAME_L[int[[1]]]
y=MIDefh$LIFESTAGE[int[[1]]]
for(i in 1:length(int[[1]])){
  print(paste(y[i],x[i], sep=' '))
}


intc <- sf::st_intersects(check, Conmap$geometry)
Conmap$SEDIMENT[intc[[1]]]


## Northeast Regional Marine Fish Habitat Assessment
## https://www.mafmc.org/nrha (below from Tori Kentner):
# Count Column:
#   The "mean" column represents a mean catch/abundance across all surveys in each hexbin from 2000-2019, split into spring and fall seasons.
#   A value of -999 in the mean column indicates absent species unless recorded as true zeros by the survey.
#   NAs in the SOURCES column represents hexbins where no surveys were conducted.
# Other details: I included the number of tows recorded by each survey and a grand tow total per hexbin, as well as a list of surveys in each hexbin (SOURCES)

# NRHAbsb=sf::st_read(paste0(wd,"Habitat/NHRA/abun_7_11_24/Black sea bass_abunSEASON.shp")) # spring and fall only
NRHAbsb=sf::st_read(paste0(wd,"Habitat/NRHA/abun_7_31_24/Black sea bass_abunSEASON.shp")) # all seasons
NRHAscup=sf::st_read(paste0(wd,"Habitat/NRHA/abun_7_31_24/Scup_abunSEASON.shp")) # all seasons

NRHAbsb$mean[which(NRHAbsb$mean==-999)]=NA
NRHAbsb.sp=NRHAbsb %>% 
  dplyr::filter(SEASON=="Spring") %>% 
  dplyr::arrange(h3_ddrs) %>%
  dplyr::mutate(cpue=NRHAbsb.sp$mean/NRHAbsb.sp$grnd_t_, ID=seq(1:length(NRHAbsb.sp$mean)))
NRHAbsb.sm=NRHAbsb %>% 
  dplyr::filter(SEASON=="Summer") %>% 
  dplyr::arrange(h3_ddrs) %>%
  dplyr::mutate(cpue=NRHAbsb.sm$mean/NRHAbsb.sm$grnd_t_, ID=seq(1:length(NRHAbsb.sm$mean)))
NRHAbsb.fl=NRHAbsb %>% 
  dplyr::filter(SEASON=="Fall") %>% 
  dplyr::arrange(h3_ddrs) %>%
  dplyr::mutate(cpue=NRHAbsb.fl$mean/NRHAbsb.fl$grnd_t_, ID=seq(1:length(NRHAbsb.fl$mean)))
NRHAbsb.wt=NRHAbsb %>% 
  dplyr::filter(SEASON=="Winter") %>% 
  dplyr::arrange(h3_ddrs) %>%
  dplyr::mutate(cpue=NRHAbsb.wt$mean/NRHAbsb.wt$grnd_t_, ID=seq(1:length(NRHAbsb.wt$mean)))

NRHAscup$mean[which(NRHAscup$mean==-999)]=NA
NRHAscup.sp=NRHAscup %>% 
  dplyr::filter(SEASON=="Spring") 
NRHAscup.sp=NRHAscup.sp %>%
  dplyr::arrange(h3_ddrs) %>%
  dplyr::mutate(cpue=NRHAscup.sp$mean/NRHAscup.sp$grnd_t_, ID=seq(1:length(NRHAscup.sp$mean)))
NRHAscup.sm=NRHAscup %>% 
  dplyr::filter(SEASON=="Summer")
NRHAscup.sm=NRHAscup.sm %>% 
  dplyr::arrange(h3_ddrs) %>%
  dplyr::mutate(cpue=NRHAscup.sm$mean/NRHAscup.sm$grnd_t_, ID=seq(1:length(NRHAscup.sm$mean)))
NRHAscup.fl=NRHAscup %>% 
  dplyr::filter(SEASON=="Fall")
NRHAscup.fl=NRHAscup.fl %>% 
  dplyr::arrange(h3_ddrs) %>%
  dplyr::mutate(cpue=NRHAscup.fl$mean/NRHAscup.fl$grnd_t_, ID=seq(1:length(NRHAscup.fl$mean)))
NRHAscup.wt=NRHAscup %>% 
  dplyr::filter(SEASON=="Winter")
NRHAscup.wt=NRHAscup.wt %>% 
  dplyr::arrange(h3_ddrs) %>%
  dplyr::mutate(cpue=NRHAscup.wt$mean/NRHAscup.wt$grnd_t_, ID=seq(1:length(NRHAscup.wt$mean)))


plot(NRHAbsb.sp["mean"], main='BSB Spring', breaks=c(0,10,20,40,80,160))
plot(NRHAbsb.fl["mean"], main='BSB Fall',breaks=c(0,10,20,40,80,160))
plot(NRHAbsb.sm["mean"], main='BSB Summer', breaks=c(0,10,20,40,80,160))
plot(NRHAbsb.wt["mean"], main='BSB Winter', breaks=c(0,10,20,40,80,160))

plot(NRHAbsb.sp["cpue"], main='BSB Spring', breaks=c(0,1,2,5,10,20))
plot(NRHAbsb.sm["cpue"], main='BSB Summer', breaks=c(0,1,2,5,10,20))
plot(NRHAbsb.fl["cpue"], main='BSB Fall',breaks=c(0,1,2,5,10,20))
plot(NRHAbsb.wt["cpue"], main='BSB Winter', breaks=c(0,1,2,5,10,20))

## counts of tows within hexbin
barplot(table(NRHAbsb.sp$grnd_t_), main='Spring')
barplot(table(NRHAbsb.sm$grnd_t_), main='Summer')
barplot(table(NRHAbsb.wt$grnd_t_), main='Winter')
barplot(table(NRHAbsb.fl$grnd_t_), main='Fall')

## mean cpue per hexbin over 2000-2019
quantile(NRHAbsb.sp$cpue,na.rm=T)
quantile(NRHAbsb.sm$cpue,na.rm=T)
quantile(NRHAbsb.fl$cpue,na.rm=T)
quantile(NRHAbsb.wt$cpue,na.rm=T)

## visualize distribution of catch density
bp=barplot(table(floor(NRHAbsb.fl$mean)))
abline(v = quantile(NRHAbsb.fl$mean,na.rm=T), col = "red")
bp=barplot(table(floor(NRHAbsb.sp$mean)))
abline(v = quantile(NRHAbsb.sp$mean,na.rm=T), col = "red")

## using CPUE instead of mean
NRHAbsb.sp$habitat=0 #initialize as numeric
NRHAbsb.sp$habitat[which(NRHAbsb.sp$cpue<=quantile(NRHAbsb.sp$cpue,na.rm=T)[2])]=1
NRHAbsb.sp$habitat[which(NRHAbsb.sp$cpue>quantile(NRHAbsb.sp$cpue,na.rm=T)[2])]=2
NRHAbsb.sp$habitat[which(NRHAbsb.sp$cpue>quantile(NRHAbsb.sp$cpue,na.rm=T)[3])]=3
NRHAbsb.sp$habitat[which(NRHAbsb.sp$cpue==quantile(NRHAbsb.sp$cpue,na.rm=T)[1])]=0
NRHAbsb.sm$habitat=0 #initialize as numeric
NRHAbsb.sm$habitat[which(NRHAbsb.sm$cpue<=quantile(NRHAbsb.sm$cpue,na.rm=T)[2])]=1
NRHAbsb.sm$habitat[which(NRHAbsb.sm$cpue>quantile(NRHAbsb.sm$cpue,na.rm=T)[2])]=2
NRHAbsb.sm$habitat[which(NRHAbsb.sm$cpue>quantile(NRHAbsb.sm$cpue,na.rm=T)[3])]=3
NRHAbsb.sm$habitat[which(NRHAbsb.sm$cpue==quantile(NRHAbsb.sm$cpue,na.rm=T)[1])]=0
NRHAbsb.fl$habitat=0 #initialize as numeric
NRHAbsb.fl$habitat[which(NRHAbsb.fl$cpue<=quantile(NRHAbsb.fl$cpue,na.rm=T)[2])]=1
NRHAbsb.fl$habitat[which(NRHAbsb.fl$cpue>quantile(NRHAbsb.fl$cpue,na.rm=T)[2])]=2
NRHAbsb.fl$habitat[which(NRHAbsb.fl$cpue>quantile(NRHAbsb.fl$cpue,na.rm=T)[3])]=3
NRHAbsb.fl$habitat[which(NRHAbsb.fl$cpue==quantile(NRHAbsb.fl$cpue,na.rm=T)[1])]=0
NRHAbsb.wt$habitat=0 #initialize as numeric
NRHAbsb.wt$habitat[which(NRHAbsb.wt$cpue<=quantile(NRHAbsb.wt$cpue,na.rm=T)[2])]=1
NRHAbsb.wt$habitat[which(NRHAbsb.wt$cpue>quantile(NRHAbsb.wt$cpue,na.rm=T)[2])]=2
NRHAbsb.wt$habitat[which(NRHAbsb.wt$cpue>quantile(NRHAbsb.wt$cpue,na.rm=T)[3])]=3
NRHAbsb.wt$habitat[which(NRHAbsb.wt$cpue==quantile(NRHAbsb.wt$cpue,na.rm=T)[1])]=0

## using CPUE instead of mean
NRHAscup.sp$habitat=0 #initialize as numeric
NRHAscup.sp$habitat[which(NRHAscup.sp$cpue<=quantile(NRHAscup.sp$cpue,na.rm=T)[2])]=1
NRHAscup.sp$habitat[which(NRHAscup.sp$cpue>quantile(NRHAscup.sp$cpue,na.rm=T)[2])]=2
NRHAscup.sp$habitat[which(NRHAscup.sp$cpue>quantile(NRHAscup.sp$cpue,na.rm=T)[3])]=3
NRHAscup.sp$habitat[which(NRHAscup.sp$cpue==quantile(NRHAscup.sp$cpue,na.rm=T)[1])]=0
NRHAscup.sm$habitat=0 #initialize as numeric
NRHAscup.sm$habitat[which(NRHAscup.sm$cpue<=quantile(NRHAscup.sm$cpue,na.rm=T)[2])]=1
NRHAscup.sm$habitat[which(NRHAscup.sm$cpue>quantile(NRHAscup.sm$cpue,na.rm=T)[2])]=2
NRHAscup.sm$habitat[which(NRHAscup.sm$cpue>quantile(NRHAscup.sm$cpue,na.rm=T)[3])]=3
NRHAscup.sm$habitat[which(NRHAscup.sm$cpue==quantile(NRHAscup.sm$cpue,na.rm=T)[1])]=0
NRHAscup.fl$habitat=0 #initialize as numeric
NRHAscup.fl$habitat[which(NRHAscup.fl$cpue<=quantile(NRHAscup.fl$cpue,na.rm=T)[2])]=1
NRHAscup.fl$habitat[which(NRHAscup.fl$cpue>quantile(NRHAscup.fl$cpue,na.rm=T)[2])]=2
NRHAscup.fl$habitat[which(NRHAscup.fl$cpue>quantile(NRHAscup.fl$cpue,na.rm=T)[3])]=3
NRHAscup.fl$habitat[which(NRHAscup.fl$cpue==quantile(NRHAscup.fl$cpue,na.rm=T)[1])]=0
NRHAscup.wt$habitat=0 #initialize as numeric
NRHAscup.wt$habitat[which(NRHAscup.wt$cpue<=quantile(NRHAscup.wt$cpue,na.rm=T)[2])]=1
NRHAscup.wt$habitat[which(NRHAscup.wt$cpue>quantile(NRHAscup.wt$cpue,na.rm=T)[2])]=2
NRHAscup.wt$habitat[which(NRHAscup.wt$cpue>quantile(NRHAscup.wt$cpue,na.rm=T)[3])]=3
NRHAscup.wt$habitat[which(NRHAscup.wt$cpue==quantile(NRHAscup.wt$cpue,na.rm=T)[1])]=0


barplot(table(NRHAbsb.sp$habitat), main='Sp')
barplot(table(NRHAbsb.sm$habitat), main='Sm')
barplot(table(NRHAbsb.fl$habitat), main='Fl')
barplot(table(NRHAbsb.wt$habitat), main='Wt')

colbin=c('white', 'blue', 'yellow', 'red')

sppX="Black sea bass"
seasonX="Spring"; habX=NRHAbsb.sp
seasonX="Summer"; habX=NRHAbsb.sm
seasonX="Fall"; habX=NRHAbsb.fl
seasonX="Winter"; habX=NRHAbsb.wt

sppX="Scup"
seasonX="Spring"; habX=NRHAscup.sp
seasonX="Summer"; habX=NRHAscup.sm
seasonX="Fall"; habX=NRHAscup.fl
seasonX="Winter"; habX=NRHAscup.wt

habX["habitat"] %>%
  leaflet() %>%
  addTiles() %>%
  setView(lng = -70, lat = 40, zoom = 5) %>%
  addPolygons(fillColor = colbin[habX$habitat+1],
              stroke = TRUE, 
              col = 'black',
              opacity = 0.5,
              weight = 0.25,
              fillOpacity = 0.4,
              popup = paste(sppX, "<br>",
                            "Season:", seasonX, "<br>",
                            "Habitat:", habX$habitat, "<br>",
                            "ID:", habX$ID))

### take average of habitat quantiles as annual value

## BSB
test=data.frame(NRHAbsb.sp$cpue)
colnames(test)='sp'
test$sm=NRHAbsb.sm$cpue
test$fl=NRHAbsb.fl$cpue
test$wt=NRHAbsb.wt$cpue
test$yr=rowMeans(test, na.rm=T)
## more strict classification ##
test$habitat=0 #initialize as numeric
test$habitat[which(test$yr<quantile(test$yr,na.rm=T)[3])]=1 # < 50%
test$habitat[which(test$yr>=quantile(test$yr,na.rm=T)[3])]=2 # 50:75%
test$habitat[which(test$yr>quantile(test$yr,na.rm=T)[4])]=3 # > 75%
test$habitat[which(test$yr==quantile(test$yr,na.rm=T)[1])]=0 # 0%
## scup
test=data.frame(NRHAscup.sp$cpue)
colnames(test)='sp'
test$sm=NRHAscup.sm$cpue
test$fl=NRHAscup.fl$cpue
test$wt=NRHAscup.wt$cpue
test$yr=rowMeans(test, na.rm=T)
## more strict classification ##
test$habitat=0 #initialize as numeric
test$habitat[which(test$yr<quantile(test$yr,na.rm=T)[3])]=1 # < 50%
test$habitat[which(test$yr>=quantile(test$yr,na.rm=T)[3])]=2 # 50:75%
test$habitat[which(test$yr>quantile(test$yr,na.rm=T)[4])]=3 # > 75%
test$habitat[which(test$yr==quantile(test$yr,na.rm=T)[1])]=0 # 0%



# add individual values from scup and bsb to annual
NRHA.yr=NRHAbsb.sp %>% dplyr::select(geometry)
NRHA.yr$bsbYRhab=test$habitat
NRHA.yr$bsbSPhab=NRHAbsb.sp$habitat
NRHA.yr$bsbSMhab=NRHAbsb.sm$habitat
NRHA.yr$bsbFLhab=NRHAbsb.fl$habitat
NRHA.yr$bsbWThab=NRHAbsb.wt$habitat
NRHA.yr$scupYRhab=test$habitat
NRHA.yr$scupSPhab=NRHAscup.sp$habitat
NRHA.yr$scupSMhab=NRHAscup.sm$habitat
NRHA.yr$scupFLhab=NRHAscup.fl$habitat
NRHA.yr$scupWThab=NRHAscup.wt$habitat

NRHA.val=NRHA.yr %>% dplyr::select(geometry)
NRHA.val$bsbYRhab=cut(NRHA.yr$bsbYRhab, breaks=c(-Inf,0,1,2,3), labels = c("None","Low","Medium","High"))
NRHA.val$bsbSPhab=cut(NRHA.yr$bsbSPhab, breaks=c(-Inf,0,1,2,3), labels = c("None","Low","Medium","High"))
NRHA.val$bsbSMhab=cut(NRHA.yr$bsbSMhab, breaks=c(-Inf,0,1,2,3), labels = c("None","Low","Medium","High"))
NRHA.val$bsbFLhab=cut(NRHA.yr$bsbFLhab, breaks=c(-Inf,0,1,2,3), labels = c("None","Low","Medium","High"))
NRHA.val$bsbWThab=cut(NRHA.yr$bsbWThab, breaks=c(-Inf,0,1,2,3), labels = c("None","Low","Medium","High"))
NRHA.val$scupYRhab=cut(NRHA.yr$scupYRhab, breaks=c(-Inf,0,1,2,3), labels = c("None","Low","Medium","High"))
NRHA.val$scupSPhab=cut(NRHA.yr$scupSPhab, breaks=c(-Inf,0,1,2,3), labels = c("None","Low","Medium","High"))
NRHA.val$scupSMhab=cut(NRHA.yr$scupSMhab, breaks=c(-Inf,0,1,2,3), labels = c("None","Low","Medium","High"))
NRHA.val$scupFLhab=cut(NRHA.yr$scupFLhab, breaks=c(-Inf,0,1,2,3), labels = c("None","Low","Medium","High"))
NRHA.val$scupWThab=cut(NRHA.yr$scupWThab, breaks=c(-Inf,0,1,2,3), labels = c("None","Low","Medium","High"))
save(NRHA.val, file=paste0(wd,'NRHA.val.rdata'))

# NRHA.yr2=NRHA.yr %>% 
#   dplyr::mutate(cut( breaks=c(-Inf,0,1,2,3), labels = c("None","Low","Medium","High")))
# 
# t2=cut(test, breaks=c(-Inf,0,1,2,3), labels = c("None","Low","Medium","High"))

sppX="Black sea bass"; habX=NRHA.yr %>% dplyr::select(bsbYRhab, bsbSPhab, bsbSMhab, bsbFLhab, bsbWThab, geometry)
sppX="Scup"; habX=NRHA.yr %>% dplyr::select(scupYRhab, scupSPhab, scupSMhab, scupFLhab, scupWThab, geometry)
habX %>%
  leaflet() %>%
  addTiles() %>%
  setView(lng = -70, lat = 40, zoom = 5) %>%
  addPolygons(fillColor = colbin[habX[[1]]+1],
              stroke = TRUE, 
              col = 'black',
              opacity = 0.5,
              weight = 0.25,
              fillOpacity = 0.4,
              popup = paste(sppX, "habitat", "<br>",
                            "Annual:", habX[[1]], "<br>",
                            "Spring:", habX[[2]],"<br>",
                            "Summer:", habX[[3]],"<br>",
                            "Fall:", habX[[4]],"<br>",
                            "Winter:", habX[[5]]))

# popup = paste(sppX, "habitat", "<br>",
#               "Annual:", ifelse(sppX=="Black sea bass", NRHA.yr$bsbYRhab, NRHA.yr$scupYRhab),"<br>",
#               "Spring:", ifelse(sppX=="Black sea bass", NRHA.yr$bsbSPhab, NRHA.yr$scupSPhab),"<br>",
#               "Summer:", ifelse(sppX=="Black sea bass", NRHA.yr$bsbSMhab, NRHA.yr$scupSMhab),"<br>",
#               "Fall:", ifelse(sppX=="Black sea bass", NRHA.yr$bsbFLhab, NRHA.yr$scupFLhab),"<br>",
#               "Winter:", ifelse(sppX=="Black sea bass", NRHA.yr$bsbWThab, NRHA.yr$scupWThab)))


hist(test$cpue)
hist(test$cpue, breaks = quantile(test$cpue, na.rm=T))
qcut=quantile(test$cpue, na.rm=T)
hist(test$cpue, br=qcut)
hist(test$habitat)
barplot(table(test$habitat))

NRHAbsb.yr=NRHAbsb.sp %>% dplyr::select(geometry)
NRHAbsb.yr$avghab=test$habitat

NRHAbsb.yr["avghab"] %>%
  leaflet() %>%
  addTiles() %>%
  setView(lng = -70, lat = 40, zoom = 5) %>%
  addPolygons(fillColor = colbin[NRHAbsb.yr$avghab+1],
              stroke = TRUE, 
              col = 'black',
              opacity = 0.5,
              weight = 0.25,
              fillOpacity = 0.4,
              popup = paste("Black sea bass habitat", "<br>",
                            "Annual:", NRHAbsb.yr$avghab, "<br>",
                            "Spring:", NRHAbsb.sp$habitat, "<br>",
                            "Summer:", NRHAbsb.sm$habitat, "<br>",
                            "Fall:", NRHAbsb.fl$habitat, "<br>",
                            "Winter:", NRHAbsb.wt$habitat))

boxplot(NRHAbsb.sp$cpue,na.rm=T, ylim=c(0,20),col='Orange',ylab='Values',xlab='CPUE',border = 'brown',horizontal = T, main="BSB Spring")
boxplot(NRHAbsb.sm$cpue,na.rm=T, ylim=c(0,40),col='Orange',ylab='Values',xlab='CPUE',border = 'brown',horizontal = T, main="BSB Summer")
boxplot(NRHAbsb.fl$cpue,na.rm=T, ylim=c(0,20),col='Orange',ylab='Values',xlab='CPUE',border = 'brown',horizontal = T, main="BSB Fall")
boxplot(NRHAbsb.wt$cpue,na.rm=T, ylim=c(0,60),col='Orange',ylab='Values',xlab='CPUE',border = 'brown',horizontal = T, main="BSB Winter")

quantile(NRHAbsb.sp$mean,na.rm=T)

maps::map("worldHires", xlim=c(-77,-65),ylim=c(35,45), fill=T,border=0,col="gray70")
map.axes(las=1)
map('state', fill = F, add=T)
# plot(NRHAbsb.sp["mean"], breaks=c(0,1,2,5,10,150), add=T)
plot(NRHAbsb.sp["mean"], breaks=c(0,1,2,5,10,15),at=c(0,1,2,5,10,15), main="BSB Spring CPUE", add=T)
text(-72,38, pos=4, labels = "BSB Spring")
# ColorBar(breaks=c(0,1,2,5,10,15),at=c(0,1,2,5,10,15))



## testing for point in polygon
# check=sf::st_point(c(-73.3630,41.1032)) # western CT
check=sf::st_point(c(-73.062097, 41.189598)) # Milford dense cage farm
# t=mgcv::in.out(NRHAbsb.sp$geometry[[1]],check)
# t=sapply(NRHAbsb.sp, FUN=mgcv::in.out(NRHAbsb.sp$geometry,check))
int <- sf::st_intersects(check, NRHAbsb.yr$geometry)
plot(NRHAbsb.sp$geometry[[int[[1]]]], add=T, col='red')
# points(check, col='green')
NRHAbsb.sp$ID[[int[[1]]]]
NRHAbsb.sp$habitat[int[[1]]]
NRHAbsb.sp$cpue[int[[1]]]

hablist=data.frame("Scup"=c(NRHA.val$scupYRhab[int[[1]]], 
                       NRHA.val$scupSPhab[int[[1]]], 
                       NRHA.val$scupSMhab[int[[1]]], 
                       NRHA.val$scupFLhab[int[[1]]], 
                       NRHA.val$scupWThab[int[[1]]]),
"Black sea bass"=c(NRHA.val$bsbYRhab[int[[1]]], 
                      NRHA.val$bsbSPhab[int[[1]]], 
                      NRHA.val$bsbSMhab[int[[1]]], 
                      NRHA.val$bsbFLhab[int[[1]]], 
                      NRHA.val$bsbWThab[int[[1]]]))
rownames(hablist)=c("Annual", "Spring", "Summer", "Fall", "Winter")

