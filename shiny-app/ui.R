# Shiny app: scraping Brazilian property registry data from public government website

# user interface ----------------------------------
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
