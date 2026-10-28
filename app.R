# app.R ------------------------------------------------------------------------
# NBA Shot Explorer: a Shiny app for exploring every field-goal attempt from
# the 2015-16 through 2019-20 NBA seasons.
#
# Data: data/nba_shots.rds, created from the raw Kaggle files by data_prep.R
# ------------------------------------------------------------------------------

library(shiny)
library(bslib)
library(dplyr)
library(readr)
library(scales)

# ---- Data ---------------------------------------------------------------------

shots <- read_rds("data/nba_shots.rds")

# ---- Variable lookups ---------------------------------------------------------
# Named vectors: names are the labels shown to the user, values are columns.

num_vars <- c(
  "Shot distance (ft)"     = "shot_dist",
  "Score margin (pts)"     = "score_margin",
  "Seconds left in period" = "sec_left_period"
)

none_choice <- c("None" = "none")

# Return the display label for a column name
var_label <- function(var) {
  all_labels <- num_vars
  names(all_labels)[match(var, all_labels)]
}

# Lower-case version of a label for use mid-sentence (keeps "FG%" intact)
var_label_lower <- function(var) {
  sub("fg%", "FG%", tolower(var_label(var)), fixed = TRUE)
}

# ---- Helper functions ---------------------------------------------------------

# Keep rows where `var` is within `range`; skip if no variable/range chosen
filter_numeric <- function(data, var, range) {
  if (is.null(var) || var == "none" || is.null(range)) {
    return(data)
  }
  filter(data, between(.data[[var]], range[1], range[2]))
}

# Range slider for a chosen numeric variable (used by the dynamic UI)
make_range_slider <- function(id, var) {
  if (is.null(var) || var == "none") {
    return(NULL)
  }
  var_range <- range(shots[[var]], na.rm = TRUE)
  sliderInput(
    id,
    label = paste("Range of", var_label_lower(var)),
    min = var_range[1], max = var_range[2],
    value = var_range, step = 1
  )
}

# ---- UI -----------------------------------------------------------------------

app_sidebar <- sidebar(
  width = 300,
  h5("Subset the data"),
  selectInput("season", "Season",
              choices = c("All seasons" = "All", levels(shots$season))),
  selectInput("team", "Shooting team",
              choices = c("All teams" = "All", levels(shots$shooting_team))),
  radioButtons("game_type", "Game type",
               choices = c("All", levels(shots$game_type)), inline = TRUE),
  radioButtons("shot_value", "Shot value",
               choices = c("All", levels(shots$shot_value)), inline = TRUE),
  hr(),
  selectInput("num_var1", "Numeric filter 1",
              choices = c(none_choice, num_vars), selected = "shot_dist"),
  uiOutput("num_slider1"),
  selectInput("num_var2", "Numeric filter 2",
              choices = c(none_choice, num_vars), selected = "score_margin"),
  uiOutput("num_slider2"),
  actionButton("apply", "Apply filters", icon = icon("filter"),
               class = "btn-primary"),
  textOutput("n_shots")
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
  
  # -- Dynamic sliders for the two numeric filters ------------------------------
  
  output$num_slider1 <- renderUI(make_range_slider("num_range1", input$num_var1))
  output$num_slider2 <- renderUI(make_range_slider("num_range2", input$num_var2))
  
  # Stop the two numeric filters from using the same variable
  observeEvent(input$num_var1, {
    choices2 <- c(none_choice, num_vars[num_vars != input$num_var1])
    selected2 <- if (input$num_var2 %in% choices2) input$num_var2 else "none"
    updateSelectInput(session, "num_var2", choices = choices2,
                      selected = selected2)
  })
  
  # -- Subset the data only when the Apply button is pressed --------------------
  
  shots_subset <- eventReactive(input$apply, {
    data <- shots
    
    if (input$season != "All") {
      data <- filter(data, season == input$season)
    }
    if (input$team != "All") {
      data <- filter(data, shooting_team == input$team)
    }
    if (input$game_type != "All") {
      data <- filter(data, game_type == input$game_type)
    }
    if (input$shot_value != "All") {
      data <- filter(data, shot_value == input$shot_value)
    }
    
    data |>
      filter_numeric(input$num_var1, input$num_range1) |>
      filter_numeric(input$num_var2, input$num_range2)
  }, ignoreNULL = FALSE)
  
  output$n_shots <- renderText({
    paste("Current data:", comma(nrow(shots_subset())), "shots")
  })
}

shinyApp(ui, server)