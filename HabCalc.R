library(shiny)
library(bslib)
library(leaflet)
library(leaflet.extras)
library(sf); sf_use_s2(FALSE)
library(gh)

# ui <- page_fluid(
#   navset_pill( 
#     nav_panel("A", "Page A content"), 
#     nav_panel("B", "Page B content"), 
#     nav_panel("C", "Page C content"), 
#     nav_menu( 
#       "Other links", 
#       nav_panel("D", "Panel D content"), 
#       "----", 
#       "Description:", 
#       nav_item( 
#         a("Milford GoPro Habitat Project", href = "https://www.fisheries.noaa.gov/new-england-mid-atlantic/aquaculture/milford-labs-gopro-aquaculture-project", target = "_blank") 
#       ), 
#     ), 
#   ), 
#   id = "tab" ,
ui <- fluidPage(style = 'margin-left: 10%; margin-right: 10%;',
                theme = bslib::bs_theme(bootswatch = "cerulean"),
                helpText(strong("Habitat Calculator Version:", style = "font-size:18px;")),
                textOutput("githubversion"),
                helpText(br()),
                # setBackgroundImage(src='background1.png'),
                
                mainPanel(
                  tabsetPanel(
                    type = "tabs",
                    tabPanel("Habitat Calculator", 
                             # tags$img(src='meatball_BSB_gear_500pxH_1050pxW.png', width = "100%", alt="NOAA branding, NOAA Fisheries Logo, and multiple oysters on and in cage"),
                             tags$img(src='white_swoosh_fish_500pxH_1650pxW.png', width = "100%", alt="NOAA branding, NOAA Fisheries Logo, and multiple oysters on and in cage"),
                             titlePanel(h1("Habitat Calculator"), windowTitle = "Aquaculture Nutrient Removal Calculator"),
                             helpText(br()),
                             
                             ### add text box with black border ### #5761C0  style = "border-style: solid; border-color: #C6E6F0#5EB6D9; background-color: #5EB6D9;",
                             div( style = "border-style: solid; border-radius: 5px; border-color: #0085CA; background-color: #0085CA;",
                                  p("This calculator predicts the habitat provided by aquaculture gear to scup and black sea bass. This tool applies to oyster farms located within the geographic range of North Carolina to Maine, USA.", style="text-align:justify; padding-left:10px; padding-right:10px; font-size:18px;color: white"),
                                  p("To use the tool, please fill in information about your farm in sections 1-2 below.", style="text-align:justify; padding-left:10px; padding-right:10px; font-size:18px; color: white")),
                             #p("To download a report, click on ",strong("Generate PDF Report")," at the bottom", style="text-align:justify; padding-left:10px; padding-right:10px; font-size:18px; color: white")),
                             helpText(br()),
                             
                             ### 1 FARM INFORMATION ###
                             ## Farm Site
                             helpText(h3("1) Farm Information")),
                             textAreaInput("farmname", div(strong("Farm Name:"), " Please enter the name of the oyster farm"),value = "", width="100%", rows=1, placeholder = NULL),
                             # helpText(br()),
                             textAreaInput("projloc", div(strong("Location:"),"Please enter the name of the water body where the farm is located"), value = "", width ="100%", rows=1, placeholder = NULL),
                             # helpText(br()),
                             ## Tides and Depth
                             selectInput("tides", div(strong("Tidal zone information:"),"Please select the appropriate tidal zone at the farm site - if the gear is always submerged please select 'subtidal'"), c("Subtidal", "Intertidal"), width ="100%"),
                             # helpText(br()),
                             textAreaInput("depth", div(strong("Average depth at farm site during low tide (MLLW):"),"Please enter the average water depth during mean lower low water (MLLW) at the farm site"), value = "", width ="100%", rows=1, placeholder = NULL),
                             # helpText(br()), 
                             ## Gear - Culture Method
                             selectInput("gear", div(strong("Culture Method:")," Select the gear type primarily used for growing oysters, or select 'On-Bottom' for no gear"), c("Off-bottom cages", "Floating cages", "On-Bottom"), width="100%"),
                             # helpText(br()),
                             # numericInput("ncages", div(strong("Quantity of gear:")," Please enter the average number of gear typically in the water"), 0, min=0, max=10000, width="100%"),
                             # helpText(br()),
                             # numericInput("cageL", div(strong("Gear dimensions: Length (ft):")," Please enter the length of the gear in feet"), 0, min=0, max=30, width="100%"),
                             # helpText(br()),
                             # numericInput("cageW", div(strong("Gear dimensions: Width (ft):")," Please enter the width of the gear in feet"), 0, min=0, max=30, width="100%"),
                             # helpText(br()),
                             # numericInput("cageH", div(strong("Gear dimensions: Height (ft):")," Please enter the height of the gear in feet"), 0, min=0, max=30, width="100%"),
                             helpText(br()),
                             helpText(h5("Gear Quantity and Dimensions")),
                             helpText("Please enter the average number of gear typically in the water, followed by the gear dimentions length, width, and height in feet"),
                             fluidRow(
                               column(3, numericInput("cageN", strong("Number of Cages"), 0, min=0, max=1000)),
                               column(3, numericInput("cageL", strong("Length of Gear (ft)"), 0, min=0, max=10)),
                               column(3, numericInput("cageW", strong("Width of Gear(ft)"), 0, min=0, max=10)),
                               column(3, numericInput("cageH", strong("Height of Gear (ft)"), 0, min=0, max=10)),
                             ),
                             ### 2 LOCATION ###
                             helpText(h3("2) Farm Location")),
                             helpText(h4("Farm Location: "),"Please scroll or pinch to zoom in to the farm location on the map, then click on the appropriate polygon for your site to record the coordinates.
                                      If your site is not shown, you can drop a marker pin by clicking once on the marker pin icon and and then once again on your site to record the coordinates. To remove a marker, 
                                      click on the trash icon and then the errant marker", style = "font-size:18px;"),
                             # helpText(h4("Approximate Coordinates")),
                             # helpText(h6("Please scroll or pinch to zoom to the farm location, then click once on the marker pin and select the site to record the coordinates. To remove a marker, click on the trash icon and then the errant marker", style = "font-size:18px;"),
                             leafletOutput("mymap", width="100%", height=400),
                             ## Location table
                             tableOutput('loctable'),
                             helpText(h4("Habitat Information")),
                             helpText(h5("Average sediment size class at selected site:")),
                             tableOutput('SEDTable'),
                             helpText(h5("Essential Fish Habitat (EFH) at selected site:")),
                             tableOutput('EFHTable'),
                             helpText(h5("Survey-based habitat quality for black sea bass and scup at selected site:")),
                             tableOutput('habTable'),
                             helpText(h5("Additional Structured Habitat Provided:")),
                             tableOutput('AreaTable'),
                    ),
                    
                    tabPanel("About", 
                             tags$img(src='meatball_BSB_gear_500pxH_1050pxW.png', width = "100%", alt="NOAA branding, NOAA Fisheries Logo, and five oysters being held in palm, with additional oysters in the background."),
                             titlePanel(h1("Habitat Calculator"), windowTitle = "Habitat Calculator"),
                             helpText(br()),
                             div( style = "border-style: solid; border-radius: 5px; border-color: #0085CA; background-color: #0085CA;",
                                  p("About the Calculator:", style="text-align:justify; padding-left:10px; padding-right:10px; font-size:20px; color: white;"),
                                  p("The Habitat Calculator can be used for new permit applications under consideratoin of the environmental benefits provided by farms, as well as to increase social license for farms. By providing information on:", style="text-align:justify; padding-left:10px; padding-right:10px; font-size:18px; color: white;"),
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
  
  ### Calculate area and volume of gear, modify by EFH overlap and survey presence
  Rarea<-reactive({
    AreaN=data.frame(input$cageN * input$cageL * input$cageW)
    colnames(AreaN)='Square feet added'
    AreaN$`Cubic feet added`=AreaN*input$cageH
    AreaN
  })
  output$AreaTable <- 
    renderTable({
      Rarea()
    })
  
  ### Read in Northeast Regional Habitat Assessment pre-processed hexgrid quantiles for black sea bass and scup
  load("NRHA.val.rdata")
  
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
    
    ### Lat Long and area of lease site from GIS
    output$loctable <- renderTable(
      data.frame("Lon"=Lonx,
                 "Lat"=Latx,
                 "Area"=Areax),
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
    
    ### Survey presence at selected coordinates
    output$habTable = renderTable({
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
      habt
    },
    rownames = TRUE,
    colnames = TRUE,
    )
    
    ### Essential Fish Habitat at selected coordinates
    output$EFHTable = renderTable({
      check=sf::st_point(c(Lonx, Latx))
      intefh <- sf::st_intersects(check, MIDefh$geometry)
      x=MIDefh$SITENAME_L[intefh[[1]]]
      y=MIDefh$LIFESTAGE[intefh[[1]]]
      efh=data.frame(matrix(nrow=length(intefh[[1]]), ncol=1))
      for(i in 1:length(intefh[[1]])){
        efh[i,1]=paste(y[i],x[i], sep=' ')
        # print(paste(y[i],x[i], sep=' '))
      }
      colnames(efh)="Essential Fish Habitat"
      efh
    },
    rownames = F,
    colnames = TRUE,
    )
    
    ### Sediment type at selected coordinates
    output$SEDTable = renderTable({
      check=sf::st_point(c(Lonx, Latx))
      intc <- sf::st_intersects(check, Conmap$geometry)
      sed=data.frame(Conmap$SEDNAME[intc[[1]]])
      colnames(sed)="Sediment Classification"
      sed
    },
    rownames = F,
    colnames = TRUE,
    )
  })
  
  ### Drop a marker to use for coordinates instead of Click if GIS aquaculture layer does not include farm
  observeEvent(input$mymap_draw_new_feature,{
    feature <- input$mymap_draw_new_feature
    Lonx=feature$geometry$coordinates[[1]]
    Latx=feature$geometry$coordinates[[2]]
    Areax=NA
    output$loctable <- renderTable(
      data.frame("Lon"=Lonx,
                 "Lat"=Latx,
                 "Area"=Areax),
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
    ### marker based coordinates for survey presence (supersedes click)
    output$habTable = renderTable({
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
      habt
    },
    rownames = TRUE,
    colnames = TRUE,
    )
    ### marker based coordinates for EFH (supersedes click)
    output$EFHTable = renderTable({
      check=sf::st_point(c(Lonx, Latx))
      intefh <- sf::st_intersects(check, MIDefh$geometry)
      x=MIDefh$SITENAME_L[intefh[[1]]]
      y=MIDefh$LIFESTAGE[intefh[[1]]]
      efh=data.frame(matrix(nrow=length(intefh[[1]]), ncol=1))
      for(i in 1:length(intefh[[1]])){
        efh[i,1]=paste(y[i],x[i], sep=' ')
        # print(paste(y[i],x[i], sep=' '))
      }
      colnames(efh)="Essential Fish Habitat"
      efh
    },
    rownames = F,
    colnames = TRUE,
    )
    ### marker based coordinates for sediments (supersedes click)
    output$SEDTable = renderTable({
      check=sf::st_point(c(Lonx, Latx))
      intc <- sf::st_intersects(check, Conmap$geometry)
      sed=data.frame(Conmap$SEDNAME[intc[[1]]])
      colnames(sed)="Sediment Classification"
      sed
    },
    rownames = F,
    colnames = TRUE,
    )

  })
}

shinyApp(ui = ui, server = server)

