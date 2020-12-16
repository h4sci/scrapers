library(shiny)
library(shinydashboard)

library(leaflet)
library(spData)
library(dplyr)

## Data
setwd("~/Dropbox/PhD/Courses/hack4sci/scrapers")

# ## anonymize data
# scrape <- read.csv("small_data.csv", encoding = "UTF-8")
# head(scrape)
# attach(scrape)
# scrape <- scrape[,c("id","area","Muni","UF","cadastrante_Profissao")]
# colnames(scrape) <- c("id","area","municipality","state","owner_profession")
# write.csv(scrape, "small_data_anon.csv")

scrape <- read.csv("small_data_anon.csv", encoding="UTF-8")

## Shiny
ui <- dashboardPage(skin="red",
                    dashboardHeader(title = "Brazil Property Statistics"),
                    dashboardSidebar(
                      # put the menuItems in the server function based on another tutorial; 
                      # wasn't working here
                      sidebarMenuOutput("menu")
                    ),
                    dashboardBody(
                      tabItems(
                        tabItem("iris",
                                box(plotOutput("correlation_plot"), width=8),
                                box(
                                  selectInput("features", "Features:",c("Sepal.Width","Petal.Length","Petal.Width")), width=4
                                )
                        ),
                        tabItem("brazil",
                                fluidPage(
                                  h1("Brazil"),
                                  dataTableOutput("proptable")
                                )
                        ),
                        tabItem("map",
                                fluidPage(
                                  h1("Map"),
                                  # slider input for year, numeric input for population 
                                  leafletOutput("map", height = 800),
                                  absolutePanel(top = 100, right = 10, draggable = TRUE,
                                                dateInput("dateinput", "Imagery Date", value = "2018-03-28"
                                                )
                                  )
                                ))
                      )
                    )
)

server <- function(input, output){
  output$correlation_plot <- renderPlot({
    plot(iris$Sepal.Length,iris[[input$features]],
         xlab="Sepal length", ylab="Feature")
  })
  output$menu <- renderMenu({
    sidebarMenu(
      id = "tabs",
      menuItem("Iris", tabName="iris", icon=icon("flower")),
      menuItem("Brazil", tabName="brazil", icon=icon("tree")),
      menuItem("Map", tabName="map", icon=icon("map"))
    )
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

shinyApp(ui, server)

#### Tutorial Links
# https://www.youtube.com/watch?v=41jmGq7ALMY
# https://www.youtube.com/watch?v=Gyrfsrd4zK0
# https://www.youtube.com/watch?v=tfN10IUX9Lo
# https://rstudio.github.io/shinydashboard/behavior.html

#### Dope applications
# https://demo.appsilon.ai/apps/visuarisk/
# https://demo.appsilon.ai/
# https://appsilon.com/video-tutorial-create-and-customize-a-simple-shiny-dashboard/

#### WMS
# https://inbo.github.io/tutorials/tutorials/spatial_wfs_services/
# https://stackoverflow.com/questions/39815494/how-to-use-wms-in-r
# http://manpages.ubuntu.com/manpages/cosmic/man1/r.in.wms.1grass.html