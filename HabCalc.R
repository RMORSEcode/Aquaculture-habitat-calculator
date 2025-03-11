library(shiny)
library(bslib)
library(leaflet)
library(leaflet.extras)


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
  helpText(h4("Approximate Coordinates: "),"Please scroll or pinch to zoom to the harvest location, then click once on the marker pin and select the site to record the coordinates. To remove a marker, click on the trash icon and then the errant marker", style = "font-size:18px;"),
  # helpText(h4("Approximate Coordinates")),
  # helpText(h6("Please scroll or pinch to zoom to the farm location, then click once on the marker pin and select the site to record the coordinates. To remove a marker, click on the trash icon and then the errant marker", style = "font-size:18px;"),
  leafletOutput("mymap", width="100%", height=400),
  ## Location table
  tableOutput('loctable'),
  helpText(h6("Fish habitat quality at selected site:")),
  tableOutput('habTable')
  )

server <- function(input, output) {
  load("C:/Users/ryan.morse/Documents/GitHub/Aquaculture-habitat-calculator/NRHA.val.rdata")
  
  aquaculture=sf::st_read(paste0(wd,"Habitat/Marine Cadastre/Aquaculture.shp"))
  NESaquaculture=sf::st_crop(aquaculture, xmin=-77, ymin=35, xmax=-66.98481, ymax=60.83483)
  
  output$mymap <- renderLeaflet({
      NESaquaculture %>%
      leaflet(height="50%") %>%
      addTiles() %>%
      setView(lng = -70, lat = 40, zoom = 5) %>%
      addPolygons(popup = paste("Site:", NESaquaculture$sitename, "<br>", 
                                "Type:", NESaquaculture$biotype, "<br>",
                                "Lease status:", NESaquaculture$leasestatu, "<br>",
                                "Area:", round(NESaquaculture$SHAPE__Are,0))) %>%
      setView(lng = -70, lat = 40, zoom = 5) %>%
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
  
  observeEvent(input$mymap_draw_new_feature,{
    feature <- input$mymap_draw_new_feature
    
    output$loctable <- renderTable(
      data.frame("Lon"=feature$geometry$coordinates[[1]],"Lat"=feature$geometry$coordinates[[2]]),
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
    
    # check=sf::st_point(c(feature$geometry$coordinates[[1]], feature$geometry$coordinates[[2]]))
    # int <- sf::st_intersects(check, NRHA.val$geometry)
    output$habTable = renderTable({
      check=sf::st_point(c(feature$geometry$coordinates[[1]], feature$geometry$coordinates[[2]]))
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
      # habt=data.frame("Scup"=c("Low", "Med", "Low", "None", "High"),
      #            "Black.sea.bass"=c("Med","Low", "Low", "None", "High"))
      rownames(habt)=c("Annual", "Spring", "Summer", "Fall", "Winter")
      habt
      },
      rownames = TRUE,
      colnames = TRUE,
      )
  })
}

shinyApp(ui = ui, server = server)

