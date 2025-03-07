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
        a("Shiny", href = "https://shiny.posit.co", target = "_blank") 
      ), 
    ), 
  ), 
  id = "tab" ,
  
  ### 1 LOCATION ###
  helpText(h3("2) Farm Location")),
  textAreaInput("projloc", div(strong("Harvest Location:"), " Please enter the name of the water body where the oysters were harvested from", em("(will not affect calculation)")), value = "", width ="100%", rows=2, placeholder = NULL),
  helpText(br()),
  helpText(h6("Approximate Coordinates: "),"Please scroll or pinch to zoom to the harvest location, then click once on the marker pin and select the site to record the coordinates. To remove a marker, click on the trash icon and then the errant marker", style = "font-size:18px;"),
  leafletOutput("mymap", width="100%", height=400),
  ## Location table
  tableOutput('loctable'),
  helpText(br())
  
  )

server <- function(input, output) {
  
  output$mymap <- renderLeaflet({
    leaflet(height="50%") %>%
      # addProviderTiles("Esri.OceanBasemap",group = "Ocean Basemap") %>%
      addTiles() %>%
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
  })
  
  # NRHAbsb=sf::st_read("C:/Users/ryan.morse/Documents/Aquaculture/Shellfish permitting and ecosystem services/Shellfish Calculators/Habitat/NHRA/abun_7_31_24/Black sea bass_abunSEASON.shp")
  # NRHAbsb$mean[which(NRHAbsb$mean==-999)]=NA
  # NRHAbsb.sp=NRHAbsb %>% filter(SEASON=="Spring")
  # NRHAbsb.sm=NRHAbsb %>% filter(SEASON=="Summer")
  # NRHAbsb.fl=NRHAbsb %>% filter(SEASON=="Fall")
  # NRHAbsb.wt=NRHAbsb %>% filter(SEASON=="Winter")
  # plot(NRHAbsb.sp["mean"], main='BSB Spring', breaks=c(0,10,20,40,80,160))
  # plot(NRHAbsb.fl["mean"], main='BSB Fall',breaks=c(0,10,20,40,80,160))
  # plot(NRHAbsb.sm["mean"], main='BSB Summer', breaks=c(0,10,20,40,80,160))
  # plot(NRHAbsb.wt["mean"], main='BSB Winter', breaks=c(0,10,20,40,80,160))
  
}

shinyApp(ui = ui, server = server)

