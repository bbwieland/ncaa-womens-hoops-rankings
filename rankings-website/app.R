library(shiny)
library(readr)
library(reactable)
library(reactablefmtr)

landing_page <- read_csv("https://raw.githubusercontent.com/bbwieland/ncaa-womens-hoops-rankings/main/landing_page.csv")

ui <- fluidPage(

  reactableOutput("homepage")
  
)

ratings_formatter <- function(x) sprintf("%+.2f", x * 100)

server <- function(input, output) {

  homepage <- reactable(landing_page, theme = fivethirtyeight(),
                        columns = list(
                          net_eff = colDef(name = "Net Eff",
                                           cell = ratings_formatter),
                          off_eff = colDef(name = "Off Eff",
                                           cell = ratings_formatter),
                          def_eff = colDef(name = "Def Eff",
                                           cell = ratings_formatter)
                        ))
  
  output$homepage <- renderReactable(homepage)
  
}

shinyApp(ui = ui, server = server)
