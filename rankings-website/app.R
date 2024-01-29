library(shiny)
library(readr)
library(reactable)
library(reactablefmtr)

landing_page <- read_csv("landing_page.csv")

ui <- fluidPage(

)

server <- function(input, output) {

}

shinyApp(ui = ui, server = server)
