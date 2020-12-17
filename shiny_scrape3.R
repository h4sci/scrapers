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

scrape <- read.csv("scraped_data_format.csv", encoding="UTF-8")
scrape$domino_tipo <- as.factor(scrape$domino_tipo)
scrape$cadastrante_Profissao <- as.factor(scrape$cadastrante_Profissao)

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
                        tabItem("owners",
                                box(plotOutput("barplot"), width=8),
                                box(
                                  selectInput("features", "Features:",c("cadastrante_Profissao","domino_tipo")), width=4
                                )
                        ),
                        tabItem("properties",
                                box(plotOutput("boxplot"), width=8),
                                box(
                                  selectInput("features", "Features:",c("cadastrante_Profissao","domino_tipo")), width=4
                                )
                        ),
                        tabItem("data",
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

#### Ideas
# Could incorporate municipal boundaries and other data from this website:
# https://www.citypopulation.de/en/brazil/para/



#### Retired script
# tabItem("iris",
#         box(plotOutput("correlation_plot"), width=8),
#         box(
#           selectInput("features", "Features:",c("Sepal.Width","Petal.Length","Petal.Width")), width=4
#         )
# ),

# output$barplot <- renderPlot({
#   plot(scrape$domino_tipo,median(scrape[[input$features]]),
#        xlab="Owner Type", ylab="Feature")

# which(scrape$domino_tipo=="Pessoa fisica",)
# which(scrape$domino_tipo=="Pessoa juridica",)

# barplot(table(scrape$cadastrante_Profissao))
# barplot(table(scrape$domino_tipo))


## Produces medians of each level of domino_tipo (owner type)
# scrape$medians=ave(
#   scrape$area,
#   scrape$domino_tipo,
#   FUN=median)

# plot(scrape$domino_tipo,scrape$area, xlab="Owner type", ylab="Property Area (ha)")
# plot(scrape$cadastrante_Profissao, scrape$area, xlab="Occupation", ylab="Property Area (ha)")