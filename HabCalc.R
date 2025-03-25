library(shiny)
library(bslib)
library(leaflet)
library(leaflet.extras)
library(sf); sf_use_s2(FALSE)

ui <- page_fluid(
  navset_pill( 
    nav_panel("A", "Page A content"), 
    nav_panel("B", "Page B content"), 
    nav_panel("C", "Page C content"), 
    nav_menu( 
      "Other links", 
      nav_panel("D", "Panel D content"), 
      "----", 
      "Description:", 
      nav_item( 
        a("Milford GoPro Habitat Project", href = "https://www.fisheries.noaa.gov/new-england-mid-atlantic/aquaculture/milford-labs-gopro-aquaculture-project", target = "_blank") 
      ), 
    ), 
  ), 
  id = "tab" ,
  
  ### 1 LOCATION ###
  helpText(h4("Farm Location")),
  textAreaInput("projloc", div("Please enter the name of the water body where the farm is located"), value = "", width ="100%", rows=2, placeholder = NULL),
  helpText(br()),
  helpText(h4("Approximate Coordinates: "),"Please scroll or pinch to zoom to the farm location, then click once on the marker pin and select the site to record the coordinates. To remove a marker, click on the trash icon and then the errant marker", style = "font-size:18px;"),
  # helpText(h4("Approximate Coordinates")),
  # helpText(h6("Please scroll or pinch to zoom to the farm location, then click once on the marker pin and select the site to record the coordinates. To remove a marker, click on the trash icon and then the errant marker", style = "font-size:18px;"),
  leafletOutput("mymap", width="100%", height=400),
  ## Location table
  tableOutput('loctable'),
  helpText(h4("Average sediment size class at selected site:")),
  tableOutput('SEDTable'),
  helpText(h4("Essential Fish Habitat (EFH) at selected site:")),
  tableOutput('EFHTable'),
  helpText(h4("Habitat quality for black sea bass and scup at selected site:")),
  tableOutput('habTable')
)

server <- function(input, output) {
  ## Read in Northeast Regional Habitat Assessment pre-processed hexgrid quantiles for black sea bass and scup
  load("C:/Users/ryan.morse/Documents/GitHub/Aquaculture-habitat-calculator/NRHA.val.rdata")
  
  ## read in aquaculture shapefile for northeast USA
  aquaculture=sf::st_read(paste0(wd,"Habitat/Marine Cadastre/Aquaculture.shp"))
  NESaquaculture=sf::st_crop(aquaculture, xmin=-77, ymin=35, xmax=-66.98481, ymax=60.83483) %>% 
    dplyr::filter(biotype=="Shellfish") %>%
    dplyr::mutate(centroid=sf::st_centroid(geometry))
  
  # read in EFH shapefile and filter to just scup and black sea bass
  MIDATLefh=sf::read_sf(paste(wd,"/Habitat/EFH/midatl_efh/midatl_efh.shp",sep=''))
  MIDefh=sf::st_transform(MIDATLefh, "WGS84") %>% dplyr::filter(SITENAME_L=="Black Sea Bass"| SITENAME_L=="Scup")

  ## set layerId for leaflet map
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
    # print(paste("Shapefile ID is: ", RV$Clicks))
    
    ## parse to lat/long based on LayerId
    Lonx=NESaquaculture$centroid[[which(NESaquaculture$objectid==RV$Clicks)]][1]
    Latx=NESaquaculture$centroid[[which(NESaquaculture$objectid==RV$Clicks)]][2]
    Areax=NESaquaculture$SHAPE__Are[[which(NESaquaculture$objectid==RV$Clicks)]][1]
    
    
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
    
    output$SEDTable = renderTable({
      check=sf::st_point(c(Lonx, Latx))
      intc <- sf::st_intersects(check, Conmap$geometry)
      sed=data.frame(Conmap$SEDIMENT[intc[[1]]])
      colnames(sed)="Sediment Classification"
      sed
    },
    rownames = F,
    colnames = TRUE,
    )
  })
  
  # ## add point to map if shapefile does not show location of farm (need switch)
  # observeEvent(input$mymap_draw_new_feature,{
  #   feature <- input$mymap_draw_new_feature
  #   
  #   output$loctable <- renderTable(
  #     data.frame("Lon"=feature$geometry$coordinates[[1]],"Lat"=feature$geometry$coordinates[[2]]),
  #     striped = T,
  #     hover = F,
  #     bordered = T,
  #     spacing = c("s", "xs", "m", "l"),
  #     width = "auto",
  #     align = NULL,
  #     rownames = FALSE,
  #     colnames = TRUE,
  #     digits = 4,
  #     na = "NA",
  #     quoted = FALSE
  #   )
  #   
  #   output$habTable = renderTable({
  #     check=sf::st_point(c(feature$geometry$coordinates[[1]], feature$geometry$coordinates[[2]]))
  #     int <- sf::st_intersects(check, NRHA.val$geometry)
  #     habt=data.frame("Scup"=c(NRHA.val$scupYRhab[int[[1]]],
  #                              NRHA.val$scupSPhab[int[[1]]],
  #                              NRHA.val$scupSMhab[int[[1]]],
  #                              NRHA.val$scupFLhab[int[[1]]],
  #                              NRHA.val$scupWThab[int[[1]]]),
  #                     "Black.sea.bass"=c(NRHA.val$bsbYRhab[int[[1]]],
  #                                        NRHA.val$bsbSPhab[int[[1]]],
  #                                        NRHA.val$bsbSMhab[int[[1]]],
  #                                        NRHA.val$bsbFLhab[int[[1]]],
  #                                        NRHA.val$bsbWThab[int[[1]]]))
  #     rownames(habt)=c("Annual", "Spring", "Summer", "Fall", "Winter")
  #     habt
  #   },
  #   rownames = TRUE,
  #   colnames = TRUE,
  #   )
  #   
  #   output$EFHTable = renderTable({
  #     check=sf::st_point(c(feature$geometry$coordinates[[1]], feature$geometry$coordinates[[2]]))
  #     intefh <- sf::st_intersects(check, MIDefh$geometry)
  #     x=MIDefh$SITENAME_L[intefh[[1]]]
  #     y=MIDefh$LIFESTAGE[intefh[[1]]]
  #     efh=data.frame(matrix(nrow=length(intefh[[1]]), ncol=1))
  #     for(i in 1:length(intefh[[1]])){
  #       efh[i,1]=paste(y[i],x[i], sep=' ')
  #       # print(paste(y[i],x[i], sep=' '))
  #     }
  #     colnames(efh)="Essential Fish Habitat"
  #     efh
  #   },
  #   rownames = F,
  #   colnames = TRUE,
  #   )
  #   
  #   output$SEDTable = renderTable({
  #     check=sf::st_point(c(feature$geometry$coordinates[[1]], feature$geometry$coordinates[[2]]))
  #     intc <- sf::st_intersects(check, Conmap$geometry)
  #     sed=data.frame(Conmap$SEDIMENT[intc[[1]]])
  #     colnames(sed)="Sediment Classification"
  #     sed
  #   },
  #   rownames = F,
  #   colnames = TRUE,
  #   )
  # })
}

shinyApp(ui = ui, server = server)

