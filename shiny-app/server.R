# server --------------------------------------

server <- function(input, output){
  output$menu <- renderMenu({
    sidebarMenu(
      id = "tabs",
      menuItem("Owners", tabName="owners", icon=icon("user")),
      menuItem("Properties", tabName="properties", icon=icon("chart-area")),
      menuItem("Data", tabName="data", icon=icon("table")),
      menuItem("Map", tabName="map", icon=icon("map"))
    )
  })
  output$barplot <- renderPlot({
    barplot(table(scrape[[input$features]]), xlab="Owner Type or Occupation", ylab="Count")
  })
  output$boxplot <- renderPlot({
    plot(scrape[[input$features]],scrape$area, xlab="Owner Type or Occupation", ylab="Property Area (ha)")
  })
  output$proptable <- renderDataTable(scrape)
  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles(group = "OSM") %>%
      addProviderTiles(providers$Esri.WorldImagery, group = "Esri World Imagery") %>%
      setView(lat = -4.984035409464784, lng = -52.614175513495034, zoom=6) %>%
      addMarkers(lat = -4.984035409464784, lng = -52.614175513495034, popup = "State of Para, Brazil", group = "state label") %>%
      addLayersControl(baseGroups = c("OSM", "Esri World Imagery"), 
                       overlayGroups = c("state label"),
                       options = layersControlOptions(collapsed = FALSE)) %>% 
      addMiniMap(zoomLevelOffset = -4) %>% 
      addScaleBar()
  })
}
