# app.R ------------------------------------------------------------------------
# NBA Shot Explorer: a Shiny app for exploring every field-goal attempt from
# the 2015-16 through 2019-20 NBA seasons.
#
# Data: data/nba_shots.rds, created from the raw Kaggle files by data_prep.R
# ------------------------------------------------------------------------------

library(shiny)
library(bslib)
library(dplyr)
library(tidyr)
library(ggplot2)
library(readr)
library(scales)
library(DT)
library(shinycssloaders)

# ---- Data ---------------------------------------------------------------------

shots <- read_rds("data/nba_shots.rds")

# ---- Variable lookups ---------------------------------------------------------
# Named vectors: names are the labels shown to the user, values are columns.

cat_vars <- c(
  "Season"       = "season",
  "Game type"    = "game_type",
  "Period"       = "period",
  "Home / away"  = "shooting_side",
  "Shot value"   = "shot_value",
  "Shot kind"    = "shot_kind",
  "Outcome"      = "outcome",
  "Assisted"     = "assisted"
)

num_vars <- c(
  "Shot distance (ft)"     = "shot_dist",
  "Score margin (pts)"     = "score_margin",
  "Seconds left in period" = "sec_left_period"
)

none_choice <- c("None" = "none")

# Return the display label for a column name
var_label <- function(var) {
  all_labels <- c(cat_vars, num_vars)
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

plot_theme <- theme_minimal(base_size = 14) +
  theme(legend.position = "bottom", plot.title = element_text(face = "bold"))

empty_data_msg <- "No shots match the current filters. Widen the sidebar filters and press Apply."

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

categorical_panel <- nav_panel(
  "Categorical",
  layout_sidebar(
    sidebar = sidebar(
      width = 260,
      selectInput("cat_var1", "Variable", choices = cat_vars,
                  selected = "season"),
      selectInput("cat_var2", "Second variable (two-way table)",
                  choices = c(none_choice, cat_vars), selected = "shot_value"),
      radioButtons("cat_table_type", "Table shows",
                   choices = c("Counts" = "count",
                               "Row proportions" = "row_prop")),
      radioButtons("cat_position", "Bar position",
                   choices = c("Side-by-side" = "dodge",
                               "Stacked" = "stack",
                               "Proportion (filled)" = "fill"))
    ),
    plotOutput("cat_plot", height = "420px") |> withSpinner(),
    h5("Contingency table"),
    tableOutput("cat_table")
  )
)

exploration_tab <- nav_panel(
  "Data Exploration",
  navset_card_pill(
    categorical_panel
  )
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
  
  # -- Data Exploration: categorical ---------------------------------------------
  
  output$cat_table <- renderTable({
    data <- shots_subset()
    validate(need(nrow(data) > 0, empty_data_msg))
    var1 <- input$cat_var1
    var2 <- input$cat_var2
    validate(need(var1 != var2, "Choose two different variables."))
    
    if (var2 == "none") {
      # one-way table
      data |>
        count(.data[[var1]], name = "Count") |>
        mutate(Proportion = percent(Count / sum(Count), accuracy = 0.1)) |>
        rename(!!var_label(var1) := 1)
    } else {
      # two-way table: rows = var1, columns = levels of var2
      two_way <- data |>
        count(.data[[var1]], .data[[var2]]) |>
        pivot_wider(names_from = all_of(var2), values_from = n,
                    values_fill = 0)
      
      if (input$cat_table_type == "row_prop") {
        totals <- rowSums(two_way[, -1])
        two_way <- two_way |>
          mutate(across(-1, \(x) percent(x / totals, accuracy = 0.1)))
      } else {
        two_way <- two_way |>
          mutate(across(-1, comma))
      }
      rename(two_way, !!paste(var_label(var1), "/", var_label(var2)) := 1)
    }
  }, striped = TRUE, align = "l")
  
  output$cat_plot <- renderPlot({
    data <- shots_subset()
    validate(need(nrow(data) > 0, empty_data_msg))
    var1 <- input$cat_var1
    var2 <- input$cat_var2
    validate(need(var1 != var2, "Choose two different variables."))
    
    if (var2 == "none") {
      p <- ggplot(data, aes(x = .data[[var1]])) +
        geom_bar(fill = "darkblue") +
        labs(title = paste("Shot attempts by", var_label_lower(var1)))
    } else {
      p <- ggplot(data, aes(x = .data[[var1]], fill = .data[[var2]])) +
        geom_bar(position = input$cat_position) +
        scale_fill_brewer(palette = "Dark2") +
        labs(
          title = paste("Shot attempts by", var_label_lower(var1), "and",
                        var_label_lower(var2)),
          fill = var_label(var2)
        )
    }
    
    y_scale <- if (var2 != "none" && input$cat_position == "fill") {
      scale_y_continuous(labels = percent)
    } else {
      scale_y_continuous(labels = comma)
    }
    y_label <- if (var2 != "none" && input$cat_position == "fill") {
      "Proportion of attempts"
    } else {
      "Attempts"
    }
    
    p + y_scale + labs(x = var_label(var1), y = y_label) + plot_theme
  })
}

shinyApp(ui, server)