library(shiny)
library(readr)
library(reactable)
library(reactablefmtr)

landing_page <-
  read_csv(
    "https://raw.githubusercontent.com/bbwieland/ncaa-womens-hoops-rankings/main/landing_page.csv"
  )

ui <- fluidPage(reactableOutput("homepage"))

net_rating_format <- function(x)
  sprintf("%+.2f", x * 100)
off_def_format <- function(x)
  sprintf("%.1f", x * 100)
poss_format <- function(x)
  sprintf("%.1f", x)

rating_width <- 125
rank_width <- 40

green_red_scale <- c()
tempo_scale <- c("#5552fa", "white","#fa493c")
rating_scale <- c("#c266ff","white","#64ff30")

server <- function(input, output) {
  homepage <- reactable(
    landing_page,
    theme = fivethirtyeight(font_size = 20, header_font_size = 16),
    pagination = FALSE,
    searchable = TRUE,
    columns = list(
      net_eff = colDef(
        name = "Net Eff",
        cell = net_rating_format,
        align = "center",
        width = rating_width,
        defaultSortOrder = "desc",
        style = color_scales(landing_page,
                             colors = rating_scale)
      ),
      off_eff = colDef(
        name = "Off Eff",
        cell = off_def_format,
        align = "center",
        width = rating_width,
        defaultSortOrder = "desc",
        style = color_scales(landing_page,
                             colors = rating_scale)
      ),
      def_eff = colDef(
        name = "Def Eff",
        cell = off_def_format,
        align = "center",
        width = rating_width,
        style = color_scales(landing_page,
                             colors = rev(rating_scale))
      ),
      poss = colDef(
        name = "Poss",
        cell = poss_format,
        width = rating_width,
        align = "center",
        defaultSortOrder = "desc",
        style = color_scales(landing_page,
                             colors = tempo_scale)
      ),
      off_rk = colDef(
        name = "",
        align = "center",
        width = rank_width,
        sortable = FALSE,
        style = color_scales(landing_page,
                             color_by = "off_eff",
                             text_size = 14,
                             colors = rating_scale),
        vAlign = "center"
      ),
      def_rk = colDef(
        name = "",
        align = "center",
        width = rank_width,
        style = color_scales(landing_page,
                             color_by = "def_eff",
                             text_size = 14,
                             colors = rev(rating_scale)),
        vAlign = "center",
        sortable = FALSE
      ),
      net_rk = colDef(
        name = "",
        align = "center",
        width = rank_width,
        sortable = FALSE,
        style = color_scales(landing_page,
                             color_by = "net_eff",
                             text_size = 14,
                             colors = rating_scale),
        vAlign = "center"
      ),
      conf_record = colDef(
        name = "Conf W-L",
        width = 100,
        align = "center",
        sortable = FALSE
      ),
      poss_rk = colDef(
        name = "",
        align = "center",
        width = rank_width,
        sortable = FALSE,
        style = color_scales(landing_page,
                             color_by = "poss",
                             text_size = 14,
                             colors = tempo_scale),
        vAlign = "center"
      ),
      conf = colDef(
        name = "Conference",
        width = 150,
        align = "center",
        sortable = FALSE,
        searchable = TRUE
      ),
      team = colDef(width = 250,
                    sortable = FALSE,
                    searchable = FALSE),
      record = colDef(
        name = "W-L",
        width = 150,
        align = "center",
        sortable = FALSE
      )
    )
  )
  
  output$homepage <- renderReactable(homepage)
  
}

shinyApp(ui = ui, server = server)
