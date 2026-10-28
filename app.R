# app.R ------------------------------------------------------------------------
# NBA Shot Explorer: a Shiny app for exploring every field-goal attempt from
# the 2015-16 through 2019-20 NBA seasons.
#
# Data: data/nba_shots.rds, created from the raw Kaggle files by data_prep.R
# ------------------------------------------------------------------------------

library(shiny)
library(bslib)
library(readr)

# ---- Data ---------------------------------------------------------------------

shots <- read_rds("data/nba_shots.rds")

# ---- UI -----------------------------------------------------------------------

app_sidebar <- sidebar(
  width = 300,
  h5("Subset the data"),
  p("Filters")
)

about_tab <- nav_panel(
  "About",
  p(" ")
)

download_tab <- nav_panel(
  "Data Download",
  p(" ")
)

exploration_tab <- nav_panel(
  "Data Exploration",
  p(" ")
)

ui <- page_sidebar(
  title = "NBA Shot Explorer",
  theme = bs_theme(version = 5, primary = "darkblue"),
  sidebar = app_sidebar,
  navset_tab(
    id = "main_tabs",
    about_tab,
    download_tab,
    exploration_tab
  )
)

# ---- Server -------------------------------------------------------------------

server <- function(input, output, session) {
  
}

shinyApp(ui, server)