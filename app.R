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
library(DT)
library(shinycssloaders)

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
  layout_columns(
    col_widths = c(7, 5),
    card(
      card_header("About this app"),
      h4("Purpose"),
      p("NBA Shot Explorer lets you dig into every field-goal attempt from",
        "five NBA seasons (2015-16 through 2019-20), a little over one million",
        "shots. You can see how the league's shot mix changed during the",
        "three-point boom, how shooting accuracy depends on distance and shot",
        "type, and how teams differ from one another."),
      h4("The data"),
      p("The data come from the",
        a("NBA Play-by-Play Data",
          href = "https://www.kaggle.com/datasets/schmadam97/nba-playbyplay-data-20182019",
          target = "_blank"),
        "set on Kaggle, which was scraped from",
        a("Basketball-Reference", href = "https://www.basketball-reference.com",
          target = "_blank"),
        "and records every play of every game. The raw play-by-play was",
        "reduced to one row per field-goal attempt (free throws are not",
        "included), with variables describing the shot (distance, 2 or 3",
        "points, kind of shot, make or miss, assisted) and the game situation",
        "(season, regular season or playoffs, period, time left, and the",
        "shooting team's lead or deficit before the shot)."),
      h4("How to use the app"),
      tags$ul(
        tags$li(strong("Sidebar: "),
                "choose a season, team, game type and shot value, then pick",
                "up to two numeric variables and drag their sliders to set a",
                "range. Nothing changes until you press", em("Apply filters"),
                ". Every other tab uses the filtered data."),
        tags$li(strong("About: "), "this page."),
        tags$li(strong("Data Download: "),
                "browse the filtered shots in a table and save them as a",
                "CSV file."),
        tags$li(strong("Data Exploration: "),
                "build your own summaries. The",
                em("Categorical"), "view gives one- and two-way contingency",
                "tables with bar charts; the", em("Numeric"),
                "view gives summary statistics with histograms, density",
                "plots or box plots; the", em("Shooting by Distance"),
                "view plots accuracy against distance; and the",
                em("Heatmap"), "view compares any two categorical variables",
                "on a metric such as FG% or three-point rate.")
      )
    ),
    card(
      card_header("Shooting accuracy across the court"),
      tags$img(src = "court_fg.png", alt = "Half court shaded by FG% at each distance",
               style = "width: 100%; height: auto;"),
      p(class = "text-muted small",
        "Each ring is a one-foot band of distance from the hoop, shaded by",
        "the league-wide field-goal percentage from that distance.")
    )
  )
)

download_tab <- nav_panel(
  "Data Download",
  card(
    card_header(
      class = "d-flex justify-content-between align-items-center",
      "Filtered shot data",
      downloadButton("download_data", "Download CSV", class = "btn-sm")
    ),
    DT::dataTableOutput("data_table") |> withSpinner()
  )
)

exploration_tab <- nav_panel(
  "Data Exploration",
  p("Coming soon.")
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
  
  # -- Data Download tab ----------------------------------------------------------
  
  output$data_table <- DT::renderDataTable({
    shots_subset()
  }, rownames = FALSE, class = "compact stripe nowrap",
  options = list(pageLength = 10, scrollX = TRUE))
  
  output$download_data <- downloadHandler(
    filename = function() {
      paste0("nba_shots_subset_", Sys.Date(), ".csv")
    },
    content = function(file) {
      write_csv(shots_subset(), file)
    }
  )
}

shinyApp(ui, server)