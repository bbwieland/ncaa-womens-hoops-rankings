library(shiny)
library(shinythemes)
library(readr)
library(reactable)
library(reactablefmtr)
library(markdown)
library(dplyr)

landing_page <- readr::read_csv(
  "https://raw.githubusercontent.com/bbwieland/ncaa-womens-hoops-rankings/main/landing_page.csv"
)

fanmatch <- readr::read_csv(
  "https://raw.githubusercontent.com/bbwieland/ncaa-womens-hoops-rankings/main/fanmatch.csv"
)

methodology <- readr::read_file("https://raw.githubusercontent.com/bbwieland/ncaa-womens-hoops-rankings/main/methodology.md")
dictionary <- readr::read_file("https://raw.githubusercontent.com/bbwieland/ncaa-womens-hoops-rankings/main/dictionary.md")

net_rating_format <- function(x)
  sprintf("%+.2f", x * 100)

off_def_format <- function(x)
  sprintf("%.1f", x * 100)

poss_format <- function(x)
  sprintf("%.1f", x)

fanmatch <- fanmatch %>%
  select(game_time, net_rk_home, team_home, net_rk_away, team_away, 
         final_score_proj, poss_est, quality, competitiveness)

rating_width <- 125
rank_width <- 40

green_red_scale <- c()
tempo_scale <- c("#5552fa", "white", "#fa493c")
rating_scale <- c("#c266ff", "white", "#64ff30")

table_theme <- function() {
  font_size = 20
  font_color = "#222222"
  header_font_size = 16
  header_font_color = "#000000"
  cell_padding = 5
  centered_content = NULL
  
  reactableTheme(
    cellStyle = centered_content,
    color = font_color,
    backgroundColor = "#ffffff",
    borderWidth = "1px",
    borderColor = "#dddddd",
    stripedColor = "#dddddd",
    highlightColor = "#f0f0f0",
    cellPadding = cell_padding,
    tableStyle = list(fontSize = font_size,
                      borderBottom = "3px solid #222222"),
    headerStyle = list(
      borderWidth = "3px",
      paddingTop = "12px",
      verticalAlign = "bottom",
      textAlign = "bottom",
      background = "#ffffff",
      textTransform = "uppercase",
      borderColor = "#222222",
      color = header_font_color,
      `&:hover` = list(background = "#dddddd"),
      `&[aria-sort='ascending'], &[aria-sort='descending']` = list(background = "#5b5e5f",
                                                                   color = "#ffffff"),
      borderColor = "#333",
      fontSize = header_font_size
    ),
    groupHeaderStyle = list(
      `&:not(:empty)` = list(
        paddingBottom = "3px",
        verticalAlign = "bottom",
        textAlign = "bottom",
        backgroundColor = "#ffffff",
        textTransform = "uppercase",
        fontSize = header_font_size,
        color = font_color
      )
    ),
    inputStyle = list(
      backgroundColor = "#ffffff",
      color = "#222222"
    ),
    rowSelectedStyle = list(backgroundColor = "#dddddd"),
    pageButtonStyle = list(textTransform = "uppercase",
                           fontSize = "14px"),
    paginationStyle = list(textTransform = "uppercase",
                           fontSize = "14px"),
    searchInputStyle = list(
      paddingLeft = "0.5rem",
      paddingTop = "0.5rem",
      paddingBottom = "0.5rem",
      width = "100%",
      border = "none",
      backgroundColor = "white",
      backgroundSize = "1rem",
      backgroundPosition = "left 0.5rem center",
      backgroundRepeat = "no-repeat",
      "&:focus" = list(backgroundColor = "rgba(255, 255, 255, 0.1)", border = "none"),
      "&:hover::placeholder, &:focus::placeholder" = list(color = "#222222"),
      fontSize = font_size
    )
  )
}


# Begin UI ----------------------------------------------------------------

ui <- fluidPage(
  theme = shinytheme("yeti"),
  titlePanel("Women's College Basketball Efficiency Rankings",
             windowTitle = "WBB Rankings"),
  tabsetPanel(
    tabPanel(title = "Rankings",
             reactableOutput("homepage")),
    tabPanel(title = "Matchups",
             reactableOutput("matchups")),
    tabPanel(title = "Methodology",
             includeMarkdown(methodology)),
    tabPanel(title = "Dictionary",
             includeMarkdown(dictionary))
  ),
  tags$head(tags$style(type = 'text/css',
                       ".nav-tabs {font-size: 20px}
                       p {font-size: 20px"))
)

# Begin Server ------------------------------------------------------------

server <- function(input, output) {
  
  ## Homepage Table ----
  
  homepage <- reactable(
    landing_page %>% select(-team_id),
    theme = table_theme(),
    pagination = FALSE,
    searchable = TRUE,
    language = reactableLang(searchPlaceholder = "Filter by conference...",
                             noData = "No conference found."),
    columns = list(
      net_eff = colDef(
        name = "Net Eff",
        cell = net_rating_format,
        align = "center",
        width = rating_width,
        defaultSortOrder = "desc"
      ),
      off_eff = colDef(
        name = "Off Eff",
        cell = off_def_format,
        align = "center",
        width = rating_width,
        defaultSortOrder = "desc"
      ),
      def_eff = colDef(
        name = "Def Eff",
        cell = off_def_format,
        align = "center",
        width = rating_width
      ),
      poss = colDef(
        name = "Poss",
        cell = poss_format,
        width = rating_width,
        align = "center",
        defaultSortOrder = "desc"
      ),
      off_rk = colDef(
        name = "",
        align = "center",
        width = rank_width,
        sortable = FALSE,
        style = cell_style(
          #landing_page,
          #color_by = "off_eff",
          font_size = 14,
          vertical_align = "center"
          #colors = rating_scale
        )
      ),
      def_rk = colDef(
        name = "",
        align = "center",
        width = rank_width,
        style = cell_style(
          #landing_page,
          #color_by = "def_eff",
          font_size = 14,
          vertical_align = "center"
          #colors = rev(rating_scale)
        ),
        sortable = FALSE
      ),
      net_rk = colDef(
        name = "",
        align = "center",
        width = rank_width,
        sortable = FALSE,
        style = cell_style(
          #landing_page,
          #color_by = "net_eff",
          font_size = 14,
          vertical_align = "center"
          #colors = rating_scale
        )
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
        style = cell_style(
          # landing_page,
          # color_by = "poss",
          font_size = 14,
          vertical_align = "center"
          # colors = tempo_scale
        )
      ),
      conf = colDef(
        name = "Conference",
        width = 150,
        align = "center",
        sortable = FALSE,
        searchable = TRUE
      ),
      team = colDef(
        width = 250,
        sortable = FALSE,
        searchable = FALSE
      ),
      record = colDef(
        name = "W-L",
        width = 150,
        align = "center",
        sortable = FALSE
      )
    )
  )
  
  output$homepage <- renderReactable(homepage)
  
  ## FanMatch Table ----
  
  matchups <- reactable(
    fanmatch, 
    theme = table_theme(),
    pagination = FALSE,
    searchable = TRUE,
    language = reactableLang(searchPlaceholder = "Filter by team...",
                             noData = "No team found."),
    columns = list(
      net_rk_home = colDef(
        name = "",
        width = rank_width, sortable = FALSE,
        style = cell_style(font_size = 14,
                           vertical_align = "center"),
      ),
      net_rk_away = colDef(
        name = "",
        width = rank_width, sortable = FALSE,
        style = cell_style(font_size = 14,
                           vertical_align = "center"),
        vAlign = "center"
      ), 
      team_home = colDef(
        name = "Home Team",
        width = 250,
        sortable = FALSE
      ),
      team_away = colDef(
        name = "Road Team",
        width = 250,
        sortable = FALSE
      ),
      game_time = colDef(
        name = "Time",
        width = 150
      ),
      final_score_proj = colDef(
        name = "Proj. Score",
        align = "center",
        width = 200,
        sortable = FALSE
      ), 
      poss_est = colDef(
        name = "Poss",
        align = "center",
        width = 80,
        cell = poss_format,
        defaultSortOrder = "desc"
      ),
      quality = colDef(
        name = "Quality",
        align = "center",
        width = 140,
        cell = poss_format,
        defaultSortOrder = "desc"
      ),
      competitiveness = colDef(
        name = "Closeness",
        align = "center",
        width = 140,
        cell = poss_format,
        defaultSortOrder = "desc"
      )
    )
    
  )
  
  output$matchups <- renderReactable(matchups)
  
}

shinyApp(ui = ui, server = server)
