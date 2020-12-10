library(shiny)
library(shinydashboard)


## Data
setwd("~/Dropbox/PhD/Courses/hack4sci/test_repo/")
scrape <- read.csv("small_data.csv", encoding = "UTF-8")
head(scrape)
attach(scrape)
scrape <- scrape[,c("id","area","Muni","UF","cadastrante_Profissao")]
colnames(scrape) <- c("id","area","municipality","state","owner_profession")
write.csv(scrape, "small_data_anon.csv")

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
                        )
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
      menuItem("Brazil", tabName="brazil", icon=icon("tree"))
    )
  })
  output$proptable <- renderDataTable(scrape)
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