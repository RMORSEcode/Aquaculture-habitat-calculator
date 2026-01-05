# C:/Users/ryan.morse/Documents/GitHub/Aquaculture-habitat-calculator
# https://test-connect.fisheries.noaa.gov/connect/#/apps/364
# https://test-connect.fisheries.noaa.gov/Habitat/
# https://connect.fisheries.noaa.gov/AHC/

library(shiny)
library(bslib)
library(leaflet)
library(leaflet.extras)
library(sf); sf_use_s2(FALSE)
library(gh)
library(shinycssloaders)
library(shinyWidgets)
library(raster)
library(terra)
library(ncdf4)
library(lubridate)
library(dplyr)
library(ggplot2)

ui <- fluidPage(style = 'margin-left: 10%; margin-right: 10%;',
                theme = bslib::bs_theme(bootswatch = "cerulean"),
                helpText(strong("Aquaculture Habitat Calculator Version:", style = "font-size:18px;")),
                textOutput("githubversion"),
                helpText(br()),
                # setBackgroundImage(src='background1.png'),
                
                mainPanel(
                  tabsetPanel(
                    type = "tabs",
                    tabPanel("Habitat Calculator", 
                             # tags$img(src='meatball_BSB_gear_500pxH_1050pxW.png', width = "100%", alt="NOAA branding, NOAA Fisheries Logo, and multiple oysters on and in cage"),
                             tags$img(src='white_swoosh_fish3_500pxH_1650pxW.png', width = "100%", alt="NOAA branding, NOAA Fisheries Logo, and multiple oysters on and in cage"),
                             titlePanel(h1("Aquaculture Habitat Calculator"), windowTitle = "Aquaculture Habitat Calculator"),
                             helpText(br()),
                             
                             ### add text box with black border ### #5761C0  style = "border-style: solid; border-color: #C6E6F0#5EB6D9; background-color: #5EB6D9;",
                             div( style = "border-style: solid; border-radius: 5px; border-color: #0085CA; background-color: #0085CA;",
                                  p("This calculator predicts the habitat provided by aquaculture gear to scup and black sea bass. This tool applies to oyster farms located within the geographic range of Virginia to New Hampshire, USA.", style="text-align:justify; padding-left:10px; padding-right:10px; font-size:18px;color: white"),
                                  p("To use the tool, please fill in information about your farm in sections 1-2 below. Note that gear in intertidal zones does not count toward habitat creation.", style="text-align:justify; padding-left:10px; padding-right:10px; font-size:18px; color: white")),
                             #p("To download a report, click on ",strong("Generate PDF Report")," at the bottom", style="text-align:justify; padding-left:10px; padding-right:10px; font-size:18px; color: white")),
                             helpText(br()),
                             
                             ### 1 FARM INFORMATION ###
                             ## Farm Site
                             helpText(h3("1) Farm Information")),
                             textAreaInput("farmname", div(strong("Farm Name:"), " Please enter the name of the oyster farm"),value = "", width="100%", rows=1, placeholder = NULL),
                             textAreaInput("projloc", div(strong("Location:"),"Please enter the name of the water body where the farm is located"), value = "", width ="100%", rows=1, placeholder = NULL),
                             
                             ## Tides and Depth
                             # selectInput("tides", div(strong("Tidal zone information:"),"Please select the appropriate tidal zone at the farm site - if the gear is always submerged please select 'subtidal'"), c("Subtidal", "Intertidal"), width ="100%"),
                             prettyRadioButtons( 
                               inputId = "tidalx",
                               label = div(strong("Tidal zone information:"),"Please select the appropriate tidal zone at the farm site - if the gear is always submerged please select 'subtidal'. Habitat provisioning is not currently credited for intertidal gear."),
                               choices = c("Intertidal","Subtidal"),
                               outline = TRUE,
                               plain = TRUE,
                               shape = "square",
                               status = "primary",
                               icon = icon("check"),
                               width = "100%"
                             ),
                             # textAreaInput("depth", div(strong("Average depth at farm site during low tide (MLLW):"),"Please enter the average water depth during mean lower low water (MLLW) at the farm site"), value = "", width ="100%", rows=1, placeholder = NULL),
                             numericInput("depth", div(strong("Average depth in feet at farm site during low tide (ft):"),"Please enter the average water depth in feet during mean lower low water (MLLW) at the farm site"), value = "", width ="100%", min=0, max=100),
                             
                             ## Gear - Culture Method, removal, and dimensions
                             prettyCheckboxGroup(
                               inputId = "gearGroup",
                               label = div(strong("Culture Method:"), "Select the gear type primarily used for growing oysters (select all that apply)"),
                               choices = list("Off-bottom cages", "Floating cages", "On-Bottom/No gear"),
                               # choices = list("Off-bottom cages" = 1, "Floating cages" = 2, "On-Bottom/No gear" = 3),
                               outline = TRUE,
                               plain = TRUE,
                               shape = "square",
                               status = "primary",
                               icon = icon("check"),
                               width = "100%",
                               selected = NULL,
                             ),
                             input_switch("switch", "Gear is removed from the water sesaonally at this site", value=F, width = "100%"), 
                             conditionalPanel(
                               condition = "input.switch == true",
                               selectInput("gearIn", div(strong("Select month gear typically goes into the water:")), 
                                           # choices = list("Jan"=1, "Feb"=2, "Mar"=3, "Apr"=4, "May"=5, "Jun"=6, "Jul"=7, "Aug"=8, "Sep"=9, "Oct"=10, "Nov"=11, "Dec"=12), 
                                           choices = list("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"), 
                                           selected="Jan", 
                                           width ="100%"),
                               selectInput("gearOut", div(strong("Select month gear is typically removed from the water:")), 
                                           # choices = list("Jan"=1, "Feb"=2, "Mar"=3, "Apr"=4, "May"=5, "Jun"=6, "Jul"=7, "Aug"=8, "Sep"=9, "Oct"=10, "Nov"=11, "Dec"=12),
                                           choices = list("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"), 
                                           selected="Dec", 
                                           width ="100%")
                             ),
                             # dateRangeInput("daterange", startview = "decade", div(strong("Date range:"), "Select months gear is in the water")),
                             helpText(br()),
                             helpText(h5("Gear Quantity and Dimensions")),
                             helpText("Please enter the average number of gear typically in the water, followed by the gear dimentions length, width, and height in feet"),
                             fluidRow(
                               column(3, numericInput("cageN", strong("Number of Cages"), 0, min=0, max=1000)),
                               column(3, numericInput("cageL", strong("Length of Gear (ft)"), 3, min=0, max=15)),
                               column(3, numericInput("cageW", strong("Width of Gear(ft)"), 3, min=0, max=10)),
                               column(3, numericInput("cageH", strong("Height of Gear (ft)"), 3, min=0, max=10)),
                             ),
                             
                             ### 2 LOCATION ###
                             helpText(h3("2) Farm Location")),
                             helpText(h4("Farm Location: "),"Please scroll or pinch to zoom in to the farm location on the map, then click on the appropriate polygon for your site to record the coordinates.
                                      If your site is not shown, you can drop a marker pin by clicking once on the marker pin icon and and then once again on your site to record the coordinates. To remove a marker, 
                                      click on the trash icon and then the errant marker", style = "font-size:18px;"),
                             # helpText(h4("Approximate Coordinates")),
                             # helpText(h6("Please scroll or pinch to zoom to the farm location, then click once on the marker pin and select the site to record the coordinates. To remove a marker, click on the trash icon and then the errant marker", style = "font-size:18px;"),
                             leafletOutput("mymap", width="100%", height=400) %>% withSpinner(type=1, size=2, color='#0085CA'), #0085CA #0dc5c1 color.background='white'
                             ## Location table
                             tableOutput('loctable'),
                             helpText(h4("Habitat Information")),
                             helpText(h5("Tidal range at selected site:")),
                             tableOutput('tideTable'),
                             helpText(h5("Average sediment size class at selected site:")),
                             tableOutput('SEDTable'),
                             helpText(h5("Essential Fish Habitat (EFH) at selected site:")),
                             tableOutput('EFHTable'),
                             
                             # verbatimTextOutput('efhc1', placeholder = T),
                             # verbatimTextOutput('efhc2', placeholder = T),
                             # verbatimTextOutput('areamod2', placeholder = T),
                             # verbatimTextOutput('surveyscup', placeholder = T),
                             # verbatimTextOutput('surveybsb', placeholder = T),
                             # verbatimTextOutput('efhareamod', placeholder = T),
                             
                             helpText(h5("Survey-based habitat quality for black sea bass and scup at selected site:")),
                             tableOutput('habTable'),
                             helpText(h5("Average surface water temperature by month (black line) with minimum and maximum monthly values (gray shading) at selected site. Also shown are preferred temperature ranges for black sea bass (blue shading) and scup (red shading). Note that some nearshore coastal areas may not have data coverage for water temperature:")),
                             plotOutput("SSTplot", width="100%"), 
                             helpText(h5("Additional Structured Habitat Provided:")),
                             tableOutput('AreaTable'),
                             downloadButton(
                               outputId = "downloader",
                               label = "Generate PDF Report"
                             ),
                    ),
                    
                    tabPanel("About", 
                             tags$img(src='meatball_BSB_scup_gear_500pxH_1050pxW.png', width = "100%", alt="NOAA branding, NOAA Fisheries Logo, and five oysters being held in palm, with additional oysters in the background."),
                             # tags$img(src='meatball_BSB_gear_500pxH_1050pxW.png', width = "100%", alt="NOAA branding, NOAA Fisheries Logo, and five oysters being held in palm, with additional oysters in the background."),
                             titlePanel(h1("Aquaculture Habitat Calculator"), windowTitle = "Aquaculture Habitat Calculator"),
                             helpText(br()),
                             div( style = "border-style: solid; border-radius: 5px; border-color: #0085CA; background-color: #0085CA;",
                                  p("About the Calculator:", style="text-align:justify; padding-left:10px; padding-right:10px; font-size:20px; color: white;"),
                                  p("The Aquaculture Habitat Calculator can be used for new permit applications under consideratoin of the environmental benefits provided by farms, as well as to increase social license for farms. By providing information on:", style="text-align:justify; padding-left:10px; padding-right:10px; font-size:18px; color: white;"),
                                  p(strong("- Farm location"), style="text-align:justify; padding-left:10px; padding-right:10px; font-size:18px; color: white;"),
                                  p(strong("- Culture method (floating gear vs. off-bottom gear vs. on-bottom)"), style="text-align:justify; padding-left:10px; padding-right:10px; font-size:18px; color: white;"),
                                  p(strong("- Tidal information (sub-tidal vs tidal)"), style="text-align:justify; padding-left:10px; padding-right:10px; font-size:18px; color: white;"),
                                  p("And by selecting the location of a farm on a GIS layer, information is provided on:", style="text-align:justify; padding-left:10px; padding-right:10px; font-size:18px; color: white;"),
                                  p(strong("- Essential Fish Habitat for scup and black sea bass"), style="text-align:justify; padding-left:10px; padding-right:10px; font-size:18px; color: white;"),
                                  p(strong("- Bottom (sediment type) classification"), style="text-align:justify; padding-left:10px; padding-right:10px; font-size:18px; color: white;"),
                                  p(strong("- Survey-based habitat classification for black sea bass and scup"), style="text-align:justify; padding-left:10px; padding-right:10px; font-size:18px; color: white;"),
                             ),
                             helpText(br()),
                             tags$p(
                               h4("Background"),
                               helpText(strong("Oyster Farms Create Fish Habitat"), style = "font-size:18px;"),
                               p("Oyster farms create habitat for local fish species, serving an ecological role similar to oyster reefs. Some of these fish are commercially and recreationally important species. Fish exhibit natural behaviors on oyster farms, including feeding, hiding from predators, looking for mates, spawning, and schooling. Oyster farms may also serve as a nursery habitat for young fish."),
                               tags$img(src='FarmHabitat_1500_1000.png', width = "100%", alt="This illustration shows an oyster cage filled with oysters and growing algae and other fouling organisms in the foreground. Surrounding the cage are fish displaying behaviors of habitat usage like feeding, courtship and spawning, use as shelter, and use as a nursery. There are multiple cages in the background as typically found on an oyster farm."),
                               helpText(br()),
                               h6(tags$a(target="_blank", href="https://www.fisheries.noaa.gov/new-england-mid-atlantic/aquaculture/milford-labs-gopro-aquaculture-project",
                                         "Learn more about the NOAA Milford Lab's GoPro Aquaculture Project here >")
                               ),
                               br(),
                               tags$p(
                                 h4("Project Team"),
                                 tags$a(target="_blank", href="https://www.fisheries.noaa.gov/contact/renee-mercaldo-allen", "Renee Mercaldo-Allen,"),
                                 tags$a(target="_blank", href="https://www.fisheries.noaa.gov/contact/ryan-morse-phd","Ryan Morse"),
                                 tags$a(target="_blank", href="https://www.linkedin.com/in/julie-m-rose/", "Julie Rose,"),
                                 tags$a(target="_blank", href="https://coastalscience.noaa.gov/staff/christopher-schillaci/", "Chris Schillaci,"), #https://www.linkedin.com/in/chris-schillaci/
                               ),
                               div( style = "border-style: solid; border-radius: 10px; border-color: #0085CA; background-color: #0085CA;",
                                    p("Send questions or comments to:",style="text-align:center; padding-left:10px; padding-right:10px; font-size:16px; color: white"),
                                    p("ES.Tools@noaa.gov",style="text-align:center; padding-left:10px; padding-right:10px; font-size:16px; color: white"),
                               ),
                               tags$p(
                                 h4("References:"),
                                 p("Ambrose A, Munroe D. Habitat usage on an oyster aquaculture farm: Impacts of farm activities and biological fouling on marine communities. J Shellfish Res 43(3): 389-397. 2025. https://doi.org/10.2983/035.043.0310"
                                   ),
                                 p("Luke T. Barrett, Seth J. Theuerkauf, Julie M. Rose, Heidi K. Alleway, Suzanne B. Bricker, Matt Parker, Daniel R. Petrolia, Robert C. Jones, Sustainable growth of non-fed aquaculture can generate valuable ecosystem benefits, Ecosystem Services, Volume 53, 2022, 101396, https://doi.org/10.1016/j.ecoser.2021.101396.
                                   "),
                                 p("Mercaldo-Allen R, Auster PJ, Clark P, Dixon MS, Estela E, Liu Y, Milke L, Phillips G, Redman, D, Smith BC, Verkade A, Rose JM. Oyster aquaculture cages provide fish habitat similar to natural structure with minimal differences based on farm location. Front. Mar. Sci., Sec. Marine Fisheries, Aquaculture and Living Resources 10.3389/fmars.2023.1058709. 2023. https://doi.org/10.3389/fmars.2023.1058709"
                                   ),
                                 p("Renee Mercaldo-Allen, Ryan Morse, Christopher Schillaci, Peter J Auster, Adrianna Bourget, Paul Clark, Mark Dixon, Kenneth Oliveira, Gillian Phillips, Dylan H Redman, Barry Smith, Julie M Rose, Measures of habitat quality for Black Sea Bass using oyster aquaculture cages, North American Journal of Aquaculture, 2025;, vraf019, https://doi.org/10.1093/naaqua/vraf019"
                                   ),
                                 p("Theuerkauf SJ, Barrett LT, Alleway HK, Costa-Pierce BA, St. Gelais A, Jones RC. Habitat value of bivalve shellfish and seaweed aquaculture for fish and invertebrates: Pathways, synthesis and next steps. Rev Aquac. 2021; 14: 54–72. https://doi.org/10.1111/raq.12584"
                                   ),
                                 h4("Disclaimer:"),
                                 p("This is a scientific product and is not an official communication of the National Oceanic and Atmospheric Administration, or the United States Department of Commerce. All NOAA GitHub project code is provided on an ‘as is’ basis and the user assumes responsibility for its use. Any claims against the Department of Commerce or Department of Commerce bureaus stemming from the use of this GitHub project will be governed by all applicable Federal law. Any reference to specific commercial products, processes, or services by service mark, trademark, manufacturer, or otherwise, does not constitute or imply their endorsement, recommendation or favoring by the Department of Commerce. The Department of Commerce seal and logo, or the seal and logo of a DOC bureau, shall not be used in any manner to imply endorsement of any commercial product or activity by DOC or the United States Government."
                                 ),
                                 ),
                               )
                    )
                    
                  )
                )
)




server <- function(input, output, session) {
  session$onSessionEnded(function() {
    stopApp()
  })
  ### Add github version to top of page
  output$githubversion <- renderText({
    releases <- gh("GET /repos/{owner}/{repo}/releases", 
                   owner = "RMORSEcode",
                   repo = "Aquaculture-habitat-calculator")
    releases[[1]][["name"]]
  })
  
  # ### Calculate area and volume of gear, modify by EFH overlap and survey presence
  # ## this can be modified with ifelse to check on EFH and survey presence at site
  # Rarea<-reactive({
  #   AreaN=data.frame(input$cageN * input$cageL * input$cageW)
  #   colnames(AreaN)='Square feet added'
  #   AreaN$`Cubic feet added`=AreaN*input$cageH
  #   AreaN
  # })
  # output$AreaTable <- 
  #   renderTable({
  #     Rarea()
  #   })
  ### Read in OISST netcdf file for surface temperature calculations
  netCDF_file <- "sst.mon.mean.nc.nc4"
  nc1 <- nc_open(netCDF_file)
  ssttime <- ncvar_get(nc1, "time")
  nc_close(nc1)
  Ttime <- as.Date(ssttime, origin="1800-01-01")
  # YM=format(Ttime, "%Y-%m")
  r <- raster::brick(netCDF_file, varname = "sst")
  r2=terra::rotate(r, left=T) 
  # sstr <- raster(netCDF_file,varname = "sst", layer=1)
  # xr <- raster::rotate(sstr, left=T)
  
  ### Read in Northeast Regional Habitat Assessment pre-processed hexgrid quantiles for black sea bass and scup
  load("NRHA.val.rdata")
  
  ### read in tidal range tif file, cropped to NES extent (depths are in meters)
  tidalRangeM=raster('cropGlobalTidalRange3.tif')
  
  ### read in aquaculture shapefile for northeast USA
  aquaculture=sf::st_read("Aquaculture.shp")
  NESaquaculture=sf::st_crop(aquaculture, xmin=-77, ymin=35, xmax=-66.98481, ymax=60.83483) %>% 
    # st_transform(., +proj=laea +lon_0=-71.8945313 +lat_0=40.5845014 +datum=WGS84 +units=m +no_defs) %>%
    dplyr::filter(biotype=="Shellfish") %>%
    dplyr::mutate(centroid=sf::st_centroid(geometry))
  
  ### read in EFH shapefile and filter to just scup and black sea bass
  MIDATLefh=sf::read_sf("midatl_efh.shp")
  MIDefh=sf::st_transform(MIDATLefh, "WGS84") %>% 
    # st_transform(., +proj=laea +lon_0=-71.8945313 +lat_0=40.5845014 +datum=WGS84 +units=m +no_defs) %>%
    dplyr::filter(SITENAME_L=="Black Sea Bass"| SITENAME_L=="Scup")
  
  ### read in ConMap sediment classification shapefile
  Conmap=sf::st_read("Continental_Margin_Mapping_Program_(CONMAP).shp")
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
  
  ### set layerId for leaflet map for click to add lat/lon
  objectID=NESaquaculture$objectid
  
  output$mymap <- renderLeaflet({
    leaflet(height="50%") %>%
      addTiles() %>%
      setView(lng = -70, lat = 40, zoom = 5) %>%
      addPolygons(data=NESaquaculture,
                  layerId = objectID,
                  popup = paste("Site:", NESaquaculture$sitename, "<br>",
                                "ID:", NESaquaculture$objectid, "<br>",
                                "Type:", NESaquaculture$biotype, "<br>",
                                "Lease status:", NESaquaculture$leasestatu, "<br>",
                                "Area:", round(NESaquaculture$SHAPE__Are,0), "<br>",
                                "Lon, Lat:", NESaquaculture$centroid)) %>%
      addDrawToolbar(
        targetGroup='Selected',
        polylineOptions=FALSE,
        polygonOptions=FALSE,
        markerOptions = T,
        rectangleOptions =F,
        circleOptions = F,
        circleMarkerOptions = F,
        editOptions = editToolbarOptions(edit = FALSE, selectedPathOptions = selectedPathOptions()))
  })
  #### ____________________________________________________ Click on shape to set Latx Lonx
  RV<-reactiveValues(Clicks=list())
  
  observeEvent(input$mymap_shape_click,{
    click=input$mymap_shape_click
    if (is.null(click))
      return()
    RV$Clicks<-click$id
    ## parse to lat/long based on LayerId
    Lonx=NESaquaculture$centroid[[which(NESaquaculture$objectid==RV$Clicks)]][1]
    Latx=NESaquaculture$centroid[[which(NESaquaculture$objectid==RV$Clicks)]][2]
    Areax=NESaquaculture$SHAPE__Are[[which(NESaquaculture$objectid==RV$Clicks)]][1]
    clickloc=reactive({
      df=data.frame("Lon"=Lonx,
                    "Lat"=Latx,
                    "Area"=Areax)
      df
    })
    # clickloc=data.frame("Lon"=Lonx,
    #            "Lat"=Latx,
    #            "Area"=Areax)
    ### Lat Long and area of lease site from GIS
    # output$loctable <- renderTable(
    #   data.frame("Lon"=Lonx,
    #              "Lat"=Latx,
    #              "Area"=Areax),
    #   # clickloc(),
    #   striped = T,
    #   hover = F,
    #   bordered = T,
    #   spacing = c("s", "xs", "m", "l"),
    #   width = "auto",
    #   align = NULL,
    #   rownames = FALSE,
    #   colnames = TRUE,
    #   digits = 4,
    #   na = "NA",
    #   quoted = FALSE
    # )
    LOCtable <- reactive({
      loc=data.frame("Lon"=Lonx,
                     "Lat"=Latx,
                     "Area"=Areax)
      loc
    })
    output$loctable <-
      renderTable(
        LOCtable(),
        striped = T,
        hover = F,
        bordered = T,
        spacing = c("s", "xs", "m", "l"),
        width = "auto",
        align = NULL,
        rownames = FALSE,
        colnames = TRUE,
        digits = 4,
        na = "NA",
        quoted = FALSE
      )
    ### extract OISST temperature data at site
    nearestLand <- function (points, raster, max_distance) {
      nearest <- function (lis, raster) {
        neighbours <- matrix(lis[[1]], ncol = 2)
        point <- lis[[2]]
        land <- !is.na(neighbours[, 2])
        if (!any(land)) {
          return (c(NA, NA))
        } else{
          coords <- xyFromCell(raster, neighbours[land, 1])   
          if (nrow(coords) == 1) {
            return (coords[1, ])
          }
          dists <- sqrt((coords[, 1] - point[1]) ^ 2 +
                          (coords[, 2] - point[2]) ^ 2)
          return (coords[which.min(dists), ])
        }
      }
      neighbour_list <- extract(raster, points,
                                buffer = max_distance,
                                cellnumbers = TRUE)
      neighbour_list <- lapply(1:nrow(points),
                               function(i) {
                                 list(neighbours = neighbour_list[[i]],
                                      point = as.numeric(points[i, ]))
                               })
      return (t(sapply(neighbour_list, nearest, raster)))
    }
    Tplot <- reactive({
      ## add check for NA and get nearest coords if yes
      vals <- extract(r2, matrix(c(Lonx, Latx), ncol = 2))
      if(is.na(vals[1])){
        vals <- extract(r2, nearestLand(matrix(c(Lonx, Latx), ncol = 2), r2, 25000))
      }
      
      valx=data.frame(vals[,1:length(vals)])
      valx$time=Ttime
      colnames(valx)=c('sst', 'time')
      valx$month=as.numeric(format(Ttime, "%m"))
      valx$year=as.numeric(format(Ttime, "%Y"))
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
      # From EFH descriptions MAFMC - referenced 8/2025
      #scup 9-27 (16-22 preferred) EFH NMFS-NE-149 1999 Table 1 p.14
      #BSB 10-22 (17-21 preferred) EFH NMFS-NE-200 2007, pp.8-9
      P=ggplot(monT, aes(x=month, y=meanT))+
        geom_line(stat = "identity", linetype=1, linewidth=2)+
        theme_bw() +
        theme(panel.border = element_blank(), 
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              axis.line = element_line(colour = "black"))+
        ylim(-5,30)+
        ylab('Water Temperature (°C)')+
        xlab("Month")+
        scale_x_continuous(breaks=c(2,4,6,8,10,12)) +
        theme(axis.title.x = element_text(size = 16),
              axis.text.x = element_text(size = 14),
              axis.text.y = element_text(size = 14),
              axis.title.y = element_text(size = 16))
      P=P+geom_ribbon(aes(ymin=`minT`, ymax=`maxT`), linetype=2, alpha=0.1)
      P=P+geom_ribbon(aes(ymin=9, ymax=27), linetype=2, alpha=0.1, fill='#DF536B')
      P=P+geom_ribbon(aes(ymin=10, ymax=22), linetype=2, alpha=0.1, fill='#2297E6')
      P
    })
    
    output$SSTplot <- 
      renderPlot({
        Tplot()
      })
    
    ### Survey presence at selected coordinates
    # output$habTable = renderTable({
    #   check=sf::st_point(c(Lonx, Latx))
    #   int <- sf::st_intersects(check, NRHA.val$geometry)
    #   zs1=NRHA.val$scupYRhab[int[[1]]]
    #   # zs2=NRHA.val$scupSPhab[int[[1]]]
    #   # zs3=NRHA.val$scupSMhab[int[[1]]]
    #   # zs4=NRHA.val$scupFLhab[int[[1]]]
    #   # zs5=NRHA.val$scupWThab[int[[1]]]
    #   zsall= zs1=="None"
    #   zb1=NRHA.val$bsbYRhab[int[[1]]]
    #   zball = zb1=="None"
    #   # output$surveyscup= renderText({
    #   #   paste("Scup survey modification = ", zsall)
    #   # })
    #   # output$surveybsb= renderText({
    #   #   paste("Black sea bass survey modification = ", zball)
    #   # })
    #   habt=data.frame("Scup"=c(NRHA.val$scupYRhab[int[[1]]],
    #                            NRHA.val$scupSPhab[int[[1]]],
    #                            NRHA.val$scupSMhab[int[[1]]],
    #                            NRHA.val$scupFLhab[int[[1]]],
    #                            NRHA.val$scupWThab[int[[1]]]),
    #                   "Black.sea.bass"=c(NRHA.val$bsbYRhab[int[[1]]],
    #                                      NRHA.val$bsbSPhab[int[[1]]],
    #                                      NRHA.val$bsbSMhab[int[[1]]],
    #                                      NRHA.val$bsbFLhab[int[[1]]],
    #                                      NRHA.val$bsbWThab[int[[1]]]))
    #   rownames(habt)=c("Annual", "Spring", "Summer", "Fall", "Winter")
    #   colnames(habt)=c("Scup", "Black sea bass")
    #   habt
    # },
    # rownames = TRUE,
    # colnames = TRUE,
    # )
    survtable <- reactive({
      
      check=sf::st_point(c(Lonx, Latx))
      int <- sf::st_intersects(check, NRHA.val$geometry)
      habt=data.frame("Scup"=c(NRHA.val$scupYRhab[int[[1]]],
                               NRHA.val$scupSPhab[int[[1]]],
                               NRHA.val$scupSMhab[int[[1]]],
                               NRHA.val$scupFLhab[int[[1]]],
                               NRHA.val$scupWThab[int[[1]]]),
                      "Black.sea.bass"=c(NRHA.val$bsbYRhab[int[[1]]],
                                         NRHA.val$bsbSPhab[int[[1]]],
                                         NRHA.val$bsbSMhab[int[[1]]],
                                         NRHA.val$bsbFLhab[int[[1]]],
                                         NRHA.val$bsbWThab[int[[1]]]))
      rownames(habt)=c("Annual", "Spring", "Summer", "Fall", "Winter")
      colnames(habt)=c("Scup", "Black sea bass")
      habt
    })
    
    output$habTable <-
      renderTable(
        survtable(),
        rownames = TRUE,
        colnames = TRUE
      )
    ### Essential Fish Habitat at selected coordinates
    efh=reactive({
      check=sf::st_point(c(Lonx, Latx))
      intefh <- sf::st_intersects(check, MIDefh$geometry)
      x=MIDefh$SITENAME_L[intefh[[1]]]
      y=MIDefh$LIFESTAGE[intefh[[1]]]
      df=data.frame(matrix(nrow=length(intefh[[1]]), ncol=1))
      for(i in 1:length(intefh[[1]])){
        df[i,1]=paste(y[i],x[i], sep=' ')
      }
      # c1=!("Black Sea Bass" %in% x) #df$EFH #output$EFHTable #MIDefh$SITENAME_L[int[[1]]]
      # c2=!("Scup" %in% x) #df$EFH #output$EFHTable  #MIDefh$SITENAME_L[int[[1]]]
      # areamod=ifelse((c1==T| c2==T), TRUE, FALSE) # if area is EFH for either species count it as habitat
      # output$efhc1= renderText({
      #   paste("EFH black sea bass modification = ", c1)
      # })
      # output$efhc2= renderText({
      #   paste("EFH Scup modification = ", c2)
      # })
      # output$areamod2= renderText({
      #   paste("Area modification = ", areamod)
      # })
      colnames(df)="Essential Fish Habitat"
      df
    })
    
    # Areamod <- reactive(
    #   x=observe({
    #     efh$areamod
    #   }),
    #   print(Areamod$x)
    # )

    # output$efhareamod <- renderText({
      # paste("EFH modification = ", efh$areamod)
      # paste("EFH modification = ", efh$areamod)
    # })
    
    output$EFHTable = 
      renderTable(
        efh(),
        colnames=T
      )
    
    #  output$EFHTable = renderTable({
    #   check=sf::st_point(c(Lonx, Latx))
    #   intefh <- sf::st_intersects(check, MIDefh$geometry)
    #   x=MIDefh$SITENAME_L[intefh[[1]]]
    #   y=MIDefh$LIFESTAGE[intefh[[1]]]
    #   efh=data.frame(matrix(nrow=length(intefh[[1]]), ncol=1))
    #   for(i in 1:length(intefh[[1]])){
    #     efh[i,1]=paste(y[i],x[i], sep=' ')
    #     # print(paste(y[i],x[i], sep=' '))
    #   }
    #   colnames(efh)="Essential Fish Habitat"
    #   efh
    # },
    # rownames = F,
    # colnames = TRUE,
    # )
    
    ### Sediment type and Tides at selected coordinates
    # output$SEDTable = renderTable({
    #   check=sf::st_point(c(Lonx, Latx))
    #   intc <- sf::st_intersects(check, Conmap$geometry)
    #   sed=data.frame(Conmap$SEDNAME[intc[[1]]])
    #   colnames(sed)="Sediment Classification"
    #   sed
    # },
    # rownames = F,
    # colnames = TRUE,
    # )
    # 
    # output$tideTable=renderTable({
    #   trangem=raster::extract(tidalRangeM, matrix(c(Lonx, Latx), ncol = 2))
    #   trangeft=trangem*3.28084
    #   tides=data.frame(trangeft)
    #   colnames(tides)="Tidal Range (ft)"
    #   tides
    #   # check=sf::st_point(c(Lonx, Latx))
    #   # intrange <- sf::st_intersects(check, tidalRangeM)
    # },
    # rownames = F,
    # colnames = TRUE,
    # )
    sedtable <- reactive({
      check=sf::st_point(c(Lonx, Latx))
      intc <- sf::st_intersects(check, Conmap$geometry)
      sed=data.frame(Conmap$SEDNAME[intc[[1]]])
      colnames(sed)="Sediment Classification"
      sed
    })
    output$SEDTable <-
      renderTable(
        sedtable(),
        rownames = F,
        colnames = TRUE
      )
    
    tidedata <- reactive({
      trangem=raster::extract(tidalRangeM, matrix(c(Lonx, Latx), ncol = 2))
      trangeft=trangem*3.28084
      tides=data.frame(trangeft)
      colnames(tides)="Tidal Range (ft)"
      tides
    })
    output$tideTable=
      renderTable(
        tidedata(),
        rownames = F,
        colnames = TRUE
      )
    
    ### Calculate area and volume of gear, modify by EFH overlap and survey presence
    ## this can be modified with ifelse to check on EFH and survey presence at site
    Rarea <- reactive({
      # check intertidal - no credit for intertidal sites, only for subtidal
      tidemod=ifelse(input$tidalx == 'Subtidal', 1, 0)
      # AreaN <- reactive()
      # observe({
      #   EFHTable
      # c1="Black Sea Bass" %in% output$EFHTable #MIDefh$SITENAME_L[int[[1]]]
      # c2="Scup" %in% output$EFHTable  #MIDefh$SITENAME_L[int[[1]]]
      # })
      # print(c1)
      # print(c2)
      # areamod=ifelse((c1==T| c2==T), 1, 0) # if area is EFH for either species count it as habitat
      # AreaN=data.frame(input$cageN * input$cageL * input$cageW) # * areamod)
      # colnames(AreaN)='Square feet added'
      # AreaN$`Cubic feet added`=AreaN*input$cageH
      # AreaN
      ## check EFH
      check=sf::st_point(c(Lonx, Latx))
      intefh <- sf::st_intersects(check, MIDefh$geometry)
      x=MIDefh$SITENAME_L[intefh[[1]]]
      y=MIDefh$LIFESTAGE[intefh[[1]]]
      df=data.frame(matrix(nrow=length(intefh[[1]]), ncol=1))
      for(i in 1:length(intefh[[1]])){
        df[i,1]=paste(y[i],x[i], sep=' ')
      }
      c1="Black Sea Bass" %in% x
      c2="Scup" %in% x
      # check Survey
      check=sf::st_point(c(Lonx, Latx))
      int <- sf::st_intersects(check, NRHA.val$geometry)
      zs1=NRHA.val$scupYRhab[int[[1]]]
      zsall= zs1=="None"
      zb1=NRHA.val$bsbYRhab[int[[1]]]
      zball = zb1=="None"
      ## compare to credit as habitat
      scupmod=ifelse((c2==F | zsall==T), 0, 1) # NO credit if area not EFH for scup OR scup absent from survey
      bsbmod=ifelse((c1==F| zball==T), 0, 1) # NO credit if area not EFH for bsb OR bsb absent from survey
      AreaN=data.frame(input$cageN * input$cageL * input$cageW * bsbmod * tidemod)
      colnames(AreaN)='Square feet added'
      AreaN$`Cubic feet added`=AreaN$`Square feet added`*input$cageH
      scupAreaN=data.frame(input$cageN * input$cageL * input$cageW * scupmod * tidemod)
      colnames(scupAreaN)='Square feet added'
      scupAreaN$`Cubic feet added`=scupAreaN$`Square feet added`*input$cageH
      AreaN2=rbind(AreaN, scupAreaN)
      rownames(AreaN2)=c('Black sea bass', 'Scup')
      AreaN2
    })
    
    output$AreaTable <- 
      renderTable(
        Rarea(),
        rownames=T
      )
    output$downloader <- 
      downloadHandler(
        paste0(Sys.Date(),"_Oyster_Farm_Habitat_Report.pdf"),
        content = 
          function(file)
          {
            rmarkdown::render(
              input = "habitatReport.Rmd",
              output_file = "built_report.pdf",
              params = list(
                tableEFH = efh(),
                tableSed = sedtable(),
                tableSurvey = survtable(),
                tableTide = tidedata(),
                loctable = LOCtable(),
                # table5 =clickloc(),
                tableArea = Rarea(),
                plot = Tplot(),
                tidal=input$tidalx,
                Location=input$projloc, 
                Depth=input$depth,
                gear=input$gearGroup,
                gearIn=input$gearIn,
                gearOut=input$gearOut,
                Farm=input$farmname,
                Number=input$cageN,
                Length=input$cageL,
                Width=input$cageW,
                Height=input$cageH,
                Lonx=NESaquaculture$centroid[[which(NESaquaculture$objectid==RV$Clicks)]][1],
                Latx=NESaquaculture$centroid[[which(NESaquaculture$objectid==RV$Clicks)]][2])#,
              # Lat=Latx,
              # Lon=Lonx)
            ) 
            readBin(con = "built_report.pdf", 
                    what = "raw",
                    n = file.info("built_report.pdf")[, "size"]) %>%
              writeBin(con = file)
          }
      )
    # })
  })
  #####_____________________________________________________________________________________________
  ### Drop a marker to use for coordinates instead of Click if GIS aquaculture layer does not include farm
  observeEvent(input$mymap_draw_new_feature,{
    feature <- input$mymap_draw_new_feature
    Lonx=feature$geometry$coordinates[[1]]
    Latx=feature$geometry$coordinates[[2]]
    Areax=NA
    clickloc=reactive({
      df=data.frame("Lon"=Lonx,
                    "Lat"=Latx,
                    "Area"=Areax)
      df
    })
    output$loctable <- renderTable(
      # data.frame("Lon"=Lonx,
      #            "Lat"=Latx,
      #            "Area"=Areax),
      clickloc(),
      striped = T,
      hover = F,
      bordered = T,
      spacing = c("s", "xs", "m", "l"),
      width = "auto",
      align = NULL,
      rownames = FALSE,
      colnames = TRUE,
      digits = 4,
      na = "NA",
      quoted = FALSE
    )
    
    ### extract OISST temperature data at site
    nearestLand <- function (points, raster, max_distance) {
      nearest <- function (lis, raster) {
        neighbours <- matrix(lis[[1]], ncol = 2)
        point <- lis[[2]]
        land <- !is.na(neighbours[, 2])
        if (!any(land)) {
          return (c(NA, NA))
        } else{
          coords <- xyFromCell(raster, neighbours[land, 1])   
          if (nrow(coords) == 1) {
            return (coords[1, ])
          }
          dists <- sqrt((coords[, 1] - point[1]) ^ 2 +
                          (coords[, 2] - point[2]) ^ 2)
          return (coords[which.min(dists), ])
        }
      }
      neighbour_list <- extract(raster, points,
                                buffer = max_distance,
                                cellnumbers = TRUE)
      neighbour_list <- lapply(1:nrow(points),
                               function(i) {
                                 list(neighbours = neighbour_list[[i]],
                                      point = as.numeric(points[i, ]))
                               })
      return (t(sapply(neighbour_list, nearest, raster)))
    }
    Tplot <- reactive({
      ## add check for NA and get nearest coords if yes
      vals <- extract(r2, matrix(c(Lonx, Latx), ncol = 2))
      if(is.na(vals[1])){
        vals <- extract(r2, nearestLand(matrix(c(Lonx, Latx), ncol = 2), r2, 25000))
      }
      
      valx=data.frame(vals[,1:length(vals)])
      valx$time=Ttime
      colnames(valx)=c('sst', 'time')
      valx$month=as.numeric(format(Ttime, "%m"))
      valx$year=as.numeric(format(Ttime, "%Y"))
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
      
      P=ggplot(monT, aes(x=month, y=meanT))+
        geom_line(stat = "identity", linetype=1, linewidth=2)+
        theme_bw() +
        theme(panel.border = element_blank(), 
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              axis.line = element_line(colour = "black"))+
        ylim(-5,30)+
        ylab('Water Temperature (°C)')+
        xlab("Month")+
        scale_x_continuous(breaks=c(2,4,6,8,10,12)) +
        theme(axis.title.x = element_text(size = 16),
              axis.text.x = element_text(size = 14),
              axis.text.y = element_text(size = 14),
              axis.title.y = element_text(size = 16))
      P=P+geom_ribbon(aes(ymin=`minT`, ymax=`maxT`), linetype=2, alpha=0.1)
      P=P+geom_ribbon(aes(ymin=9, ymax=27), linetype=2, alpha=0.1, fill='#DF536B')
      P=P+geom_ribbon(aes(ymin=10, ymax=22), linetype=2, alpha=0.1, fill='#2297E6')
      P
    })
    
    output$SSTplot <- 
      renderPlot({
        Tplot()
      })
    
    
    ### marker based coordinates for survey presence (supersedes click)
    # output$habTable = renderTable({
    #   check=sf::st_point(c(Lonx, Latx))
    #   int <- sf::st_intersects(check, NRHA.val$geometry)
    #   zs1=NRHA.val$scupYRhab[int[[1]]]
    #   # zs2=NRHA.val$scupSPhab[int[[1]]]
    #   # zs3=NRHA.val$scupSMhab[int[[1]]]
    #   # zs4=NRHA.val$scupFLhab[int[[1]]]
    #   # zs5=NRHA.val$scupWThab[int[[1]]]
    #   zsall= zs1=="None"
    #   zb1=NRHA.val$bsbYRhab[int[[1]]]
    #   zball = zb1=="None"
    #   output$surveyscup= renderText({
    #     paste("Scup survey modification = ", zsall)
    #   })
    #   output$surveybsb= renderText({
    #     paste("Black sea bass survey modification = ", zball)
    #   })
    #   habt=data.frame("Scup"=c(NRHA.val$scupYRhab[int[[1]]],
    #                            NRHA.val$scupSPhab[int[[1]]],
    #                            NRHA.val$scupSMhab[int[[1]]],
    #                            NRHA.val$scupFLhab[int[[1]]],
    #                            NRHA.val$scupWThab[int[[1]]]),
    #                   "Black.sea.bass"=c(NRHA.val$bsbYRhab[int[[1]]],
    #                                      NRHA.val$bsbSPhab[int[[1]]],
    #                                      NRHA.val$bsbSMhab[int[[1]]],
    #                                      NRHA.val$bsbFLhab[int[[1]]],
    #                                      NRHA.val$bsbWThab[int[[1]]]))
    #   rownames(habt)=c("Annual", "Spring", "Summer", "Fall", "Winter")
    #   colnames(habt)=c("Scup", "Black sea bass")
    #   habt
    # },
    # rownames = TRUE,
    # colnames = TRUE,
    # )
    survtable <- reactive({
      
      check=sf::st_point(c(Lonx, Latx))
      int <- sf::st_intersects(check, NRHA.val$geometry)
      habt=data.frame("Scup"=c(NRHA.val$scupYRhab[int[[1]]],
                               NRHA.val$scupSPhab[int[[1]]],
                               NRHA.val$scupSMhab[int[[1]]],
                               NRHA.val$scupFLhab[int[[1]]],
                               NRHA.val$scupWThab[int[[1]]]),
                      "Black.sea.bass"=c(NRHA.val$bsbYRhab[int[[1]]],
                                         NRHA.val$bsbSPhab[int[[1]]],
                                         NRHA.val$bsbSMhab[int[[1]]],
                                         NRHA.val$bsbFLhab[int[[1]]],
                                         NRHA.val$bsbWThab[int[[1]]]))
      rownames(habt)=c("Annual", "Spring", "Summer", "Fall", "Winter")
      colnames(habt)=c("Scup", "Black sea bass")
      habt
    })
    
    output$habTable <-
      renderTable(
        survtable(),
        rownames = TRUE,
        colnames = TRUE
      )
    ### marker based coordinates for EFH (supersedes click)
    
    ### Essential Fish Habitat at selected coordinates
    efh=reactive({
      check=sf::st_point(c(Lonx, Latx))
      intefh <- sf::st_intersects(check, MIDefh$geometry)
      x=MIDefh$SITENAME_L[intefh[[1]]]
      y=MIDefh$LIFESTAGE[intefh[[1]]]
      df=data.frame(matrix(nrow=length(intefh[[1]]), ncol=1))
      for(i in 1:length(intefh[[1]])){
        df[i,1]=paste(y[i],x[i], sep=' ')
      }
      # c1="Black Sea Bass" %in% x #df$EFH #output$EFHTable #MIDefh$SITENAME_L[int[[1]]]
      # c2="Scup" %in% x #df$EFH #output$EFHTable  #MIDefh$SITENAME_L[int[[1]]]
      # areamod=ifelse((c1==T| c2==T), TRUE, FALSE) # if area is EFH for either species count it as habitat
      # output$efhc1= renderText({
      #   paste("EFH BSB modification = ", c1)
      # })
      # output$efhc2= renderText({
      #   paste("EFH Scup modification = ", c2)
      # })
      # output$areamod2= renderText({
      #   paste("Area modification = ", areamod)
      # })
      colnames(df)="Essential Fish Habitat"
      df
    })
    
    # Areamod <- reactive(
    #   x=observe({
    #     efh$areamod
    #   }),
    #   print(Areamod$x)
    # )
    # 
    
    # output$efhareamod <- renderText({
    #   paste("EFH modification = ", efh$areamod)
    # })
    
    output$EFHTable = 
      renderTable(
        efh(),
        colnames=T
      )
    # output$EFHTable = renderTable({
    #   check=sf::st_point(c(Lonx, Latx))
    #   intefh <- sf::st_intersects(check, MIDefh$geometry)
    #   x=MIDefh$SITENAME_L[intefh[[1]]]
    #   y=MIDefh$LIFESTAGE[intefh[[1]]]
    #   efh=data.frame(matrix(nrow=length(intefh[[1]]), ncol=1))
    #   for(i in 1:length(intefh[[1]])){
    #     efh[i,1]=paste(y[i],x[i], sep=' ')
    #     # print(paste(y[i],x[i], sep=' '))
    #   }
    #   colnames(efh)="Essential Fish Habitat"
    #   efh
    # },
    # rownames = F,
    # colnames = TRUE,
    # )
    
    ### marker based coordinates for sediments and tides (supersedes click)
    # output$SEDTable = renderTable({
    #   check=sf::st_point(c(Lonx, Latx))
    #   intc <- sf::st_intersects(check, Conmap$geometry)
    #   sed=data.frame(Conmap$SEDNAME[intc[[1]]])
    #   colnames(sed)="Sediment Classification"
    #   sed
    # },
    # rownames = F,
    # colnames = TRUE,
    # )
    # output$tideTable=renderTable({
    #   trangem=raster::extract(tidalRangeM, matrix(c(Lonx, Latx), ncol = 2))
    #   trangeft=trangem*3.28084
    #   tides=data.frame(trangeft)
    #   colnames(tides)="Tidal Range (ft)"
    #   tides
    #   # check=sf::st_point(c(Lonx, Latx))
    #   # intrange <- sf::st_intersects(check, tidalRangeM)
    # },
    # rownames = F,
    # colnames = TRUE,
    # )
    sedtable <- reactive({
      check=sf::st_point(c(Lonx, Latx))
      intc <- sf::st_intersects(check, Conmap$geometry)
      sed=data.frame(Conmap$SEDNAME[intc[[1]]])
      colnames(sed)="Sediment Classification"
      sed
    })
    output$SEDTable <-
      renderTable(
        sedtable(),
        rownames = F,
        colnames = TRUE
      )
    
    tidedata <- reactive({
      trangem=raster::extract(tidalRangeM, matrix(c(Lonx, Latx), ncol = 2))
      trangeft=trangem*3.28084
      tides=data.frame(trangeft)
      colnames(tides)="Tidal Range (ft)"
      tides
    })
    output$tideTable=
      renderTable(
        tidedata(),
        rownames = F,
        colnames = TRUE
      )
    
    Rarea <- reactive({
      # check intertidal - no credit for intertidal sites, only for subtidal
      tidemod=ifelse(input$tidalx == 'Subtidal', 1, 0)
      # check EFH
      check=sf::st_point(c(Lonx, Latx))
      intefh <- sf::st_intersects(check, MIDefh$geometry)
      x=MIDefh$SITENAME_L[intefh[[1]]]
      y=MIDefh$LIFESTAGE[intefh[[1]]]
      df=data.frame(matrix(nrow=length(intefh[[1]]), ncol=1))
      for(i in 1:length(intefh[[1]])){
        df[i,1]=paste(y[i],x[i], sep=' ')
      }
      c1="Black Sea Bass" %in% x
      c2="Scup" %in% x
      # check Survey
      check=sf::st_point(c(Lonx, Latx))
      int <- sf::st_intersects(check, NRHA.val$geometry)
      zs1=NRHA.val$scupYRhab[int[[1]]]
      zsall= zs1=="None"
      zb1=NRHA.val$bsbYRhab[int[[1]]]
      zball = zb1=="None"
      ## compare to credit as habitat
      scupmod=ifelse((c2==F | zsall==T), 0, 1) # NO credit if area not EFH for scup OR scup absent from survey
      bsbmod=ifelse((c1==F| zball==T), 0, 1) # NO credit if area not EFH for bsb OR bsb absent from survey
      AreaN=data.frame(input$cageN * input$cageL * input$cageW * bsbmod * tidemod)
      colnames(AreaN)='Square feet added'
      AreaN$`Cubic feet added`=AreaN$`Square feet added`*input$cageH
      scupAreaN=data.frame(input$cageN * input$cageL * input$cageW * scupmod * tidemod)
      colnames(scupAreaN)='Square feet added'
      scupAreaN$`Cubic feet added`=scupAreaN$`Square feet added`*input$cageH
      AreaN2=rbind(AreaN, scupAreaN)
      rownames(AreaN2)=c('Black sea bass', 'Scup')
      AreaN2
    })
    
    output$AreaTable <- 
      renderTable(
        Rarea(),
        rownames=T
      )
    output$downloader <- 
      downloadHandler(
        paste0(Sys.Date(),"_Oyster_Farm_Habitat_Report.pdf"),
        content = 
          function(file)
          {
            rmarkdown::render(
              input = "habitatReport.Rmd",
              output_file = "built_report.pdf",
              params = list(
                tableEFH = efh(),
                tableSed = sedtable(),
                tableSurvey = survtable(),
                tableTide = tidedata(),
                #table5 = loctable(),
                loctable =clickloc(),
                tableArea = Rarea(),
                plot = Tplot(),
                tidal=input$tidalx,
                Location=input$projloc, 
                Depth=input$depth,
                gear=input$gearGroup,
                gearIn=input$gearIn,
                gearOut=input$gearOut,
                Farm=input$farmname,
                Number=input$cageN,
                Length=input$cageL,
                Width=input$cageW,
                Height=input$cageH,
                Lonx=NESaquaculture$centroid[[which(NESaquaculture$objectid==RV$Clicks)]][1],
                Latx=NESaquaculture$centroid[[which(NESaquaculture$objectid==RV$Clicks)]][2])#,
              # Lat=Latx,
              # Lon=Lonx)
            ) 
            readBin(con = "built_report.pdf", 
                    what = "raw",
                    n = file.info("built_report.pdf")[, "size"]) %>%
              writeBin(con = file)
          }
      )
  })
  ####_________________________________________ end of map input section
  # output$downloader <- 
  #   downloadHandler(
  #     paste0(Sys.Date(),"_Oyster_Farm_Habitat_Report.pdf"),
  #     content = 
  #       function(file)
  #       {
  #         rmarkdown::render(
  #           input = "habitatReport.Rmd",
  #           output_file = "built_report.pdf",
  #           params = list(
  #             # tableEFH = efh(),
  #             # tableSed = SEDtable(),
  #             # tableHab = habTable(),
  #             #tableTide = tideTable(),
  #             #table5 = loctable(),
  #             # table5 =clickloc(),
  #             tableArea = Rarea(),
  #             plot = Tplot(),
  #             tidal=input$tidalx,
  #             Location=input$projloc, 
  #             Depth=input$depth,
  #             gear=input$gearGroup,
  #             gearIn=input$gearIn,
  #             gearOut=input$gearOut,
  #             Farm=input$farmname,
  #             Number=input$cageN,
  #             Length=input$cageL,
  #             Width=input$cageW,
  #             Height=input$cageH,
  #             Lonx=NESaquaculture$centroid[[which(NESaquaculture$objectid==RV$Clicks)]][1],
  #             Latx=NESaquaculture$centroid[[which(NESaquaculture$objectid==RV$Clicks)]][2])#,
  #           # Lat=Latx,
  #           # Lon=Lonx)
  #         ) 
  #         readBin(con = "built_report.pdf", 
  #                 what = "raw",
  #                 n = file.info("built_report.pdf")[, "size"]) %>%
  #           writeBin(con = file)
  #       }
  #   )
}

shinyApp(ui = ui, server = server)

