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
library(forcats)
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

# The heatmap can also use team, which has too many levels for the other plots
heat_cat_vars <- c("Shooting team" = "shooting_team", cat_vars)

num_vars <- c(
  "Shot distance (ft)"     = "shot_dist",
  "Score margin (pts)"     = "score_margin",
  "Seconds left in period" = "sec_left_period"
)

heat_metrics <- c(
  "3-pt attempt rate"          = "three_rate",
  "FG%"                        = "fg_pct",
  "Points per shot"            = "pts_per_shot",
  "Average shot distance (ft)" = "avg_dist",
  "Assisted rate (made shots)" = "ast_rate",
  "Attempts"                   = "attempts"
)

none_choice <- c("None" = "none")

# Return the display label for a column name
var_label <- function(var) {
  all_labels <- c(heat_cat_vars, num_vars, heat_metrics)
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

# Summary statistics of one numeric variable at each level of a categorical one
summarize_numeric <- function(data, num_var, group_var) {
  data |>
    group_by(.data[[group_var]]) |>
    summarise(
      n      = n(),
      mean   = mean(.data[[num_var]], na.rm = TRUE),
      median = median(.data[[num_var]], na.rm = TRUE),
      sd     = sd(.data[[num_var]], na.rm = TRUE),
      min    = min(.data[[num_var]], na.rm = TRUE),
      q1     = quantile(.data[[num_var]], 0.25, na.rm = TRUE),
      q3     = quantile(.data[[num_var]], 0.75, na.rm = TRUE),
      max    = max(.data[[num_var]], na.rm = TRUE),
      .groups = "drop"
    ) |>
    rename(!!var_label(group_var) := 1)
}

# Points scored on each attempt (0 for a miss)
shot_points <- function(shot_value, made) {
  if_else(shot_value == "3-pt", 3, 2) * made
}

# Format a heatmap metric for tile labels
format_metric <- function(x, metric) {
  labels <- switch(
    metric,
    attempts     = comma(x),
    avg_dist     = number(x, accuracy = 0.1),
    pts_per_shot = number(x, accuracy = 0.01),
    percent(x, accuracy = 1)
  )
  if_else(is.na(x), "", labels)
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

numeric_panel <- nav_panel(
  "Numeric",
  layout_sidebar(
    sidebar = sidebar(
      width = 260,
      selectInput("num_var", "Numeric variable", choices = num_vars,
                  selected = "shot_dist"),
      selectInput("num_group", "Summarize across", choices = cat_vars,
                  selected = "season"),
      radioButtons("num_plot_type", "Plot type",
                   choices = c("Histogram" = "hist", "Density" = "density",
                               "Box plot" = "box")),
      selectInput("num_fill", "Color by", choices = c(none_choice, cat_vars),
                  selected = "shot_value"),
      selectInput("num_facet", "Facet by", choices = c(none_choice, cat_vars),
                  selected = "season"),
      checkboxInput("num_zoom", "Zoom to the middle 99% of values",
                    value = TRUE)
    ),
    plotOutput("num_plot", height = "520px") |> withSpinner(),
    h5("Summary statistics"),
    tableOutput("num_table")
  )
)

distance_panel <- nav_panel(
  "Shooting by Distance",
  layout_sidebar(
    sidebar = sidebar(
      width = 260,
      radioButtons("dist_metric", "Y-axis",
                   choices = c("FG%" = "fg_pct",
                               "Points per shot" = "pts_per_shot")),
      selectInput("dist_color", "Color by",
                  choices = c(none_choice, cat_vars), selected = "shot_value"),
      sliderInput("dist_max", "Maximum distance (ft)",
                  min = 10, max = 60, value = 35, step = 1),
      sliderInput("dist_min_n", "Minimum attempts per point",
                  min = 10, max = 1000, value = 100, step = 10)
    ),
    plotOutput("dist_plot", height = "520px") |> withSpinner(),
    p(class = "text-muted small",
      "Each point is one distance (rounded to the foot) for one group, sized by",
      "the number of attempts. Points with fewer attempts than the minimum",
      "are hidden because their percentages are too noisy.")
  )
)

heatmap_panel <- nav_panel(
  "Heatmap",
  layout_sidebar(
    sidebar = sidebar(
      width = 260,
      selectInput("heat_rows", "Rows", choices = heat_cat_vars,
                  selected = "shooting_team"),
      selectInput("heat_cols", "Columns", choices = heat_cat_vars,
                  selected = "season"),
      selectInput("heat_metric", "Fill", choices = heat_metrics,
                  selected = "three_rate"),
      checkboxInput("heat_labels", "Show values on tiles", value = TRUE)
    ),
    plotOutput("heat_plot", height = "720px") |> withSpinner()
  )
)

exploration_tab <- nav_panel(
  "Data Exploration",
  navset_card_pill(
    categorical_panel,
    numeric_panel,
    distance_panel,
    heatmap_panel
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
  
  # -- Data Exploration: numeric --------------------------------------------------
  
  output$num_table <- renderTable({
    data <- shots_subset()
    validate(need(nrow(data) > 0, empty_data_msg))
    summarize_numeric(data, input$num_var, input$num_group)
  }, striped = TRUE, digits = 2)
  
  output$num_plot <- renderPlot({
    data <- shots_subset()
    validate(need(nrow(data) > 0, empty_data_msg))
    num_var <- input$num_var
    fill_var <- input$num_fill
    facet_var <- input$num_facet
    plot_type <- input$num_plot_type
    validate(need(
      fill_var == "none" || facet_var == "none" || fill_var != facet_var,
      "Choose different variables for color and facet."
    ))
    
    if (plot_type == "box") {
      # box plot: numeric variable across the "Summarize across" groups
      group_var <- input$num_group
      p <- ggplot(data, aes(x = .data[[group_var]], y = .data[[num_var]])) +
        labs(x = var_label(group_var), y = var_label(num_var),
             title = paste(var_label(num_var), "by", var_label_lower(group_var)))
      if (fill_var == "none") {
        p <- p + geom_boxplot(fill = "darkblue", alpha = 0.6, outlier.alpha = 0.05)
      } else {
        p <- p + geom_boxplot(aes(fill = .data[[fill_var]]), outlier.alpha = 0.05) +
          labs(fill = var_label(fill_var))
      }
      zoom <- coord_cartesian(
        ylim = if (input$num_zoom) quantile(data[[num_var]], c(0.005, 0.995)) else NULL
      )
    } else {
      # x-axis limits (optionally the middle 99%) and a matching bin width
      x_limits <- if (input$num_zoom) {
        quantile(data[[num_var]], c(0.005, 0.995))
      } else {
        range(data[[num_var]])
      }
      bin_width <- max(1, round(diff(x_limits) / 40))
      
      p <- ggplot(data, aes(x = .data[[num_var]])) +
        labs(x = var_label(num_var),
             title = paste("Distribution of", var_label_lower(num_var)))
      fill_aes <- if (fill_var == "none") NULL else aes(fill = .data[[fill_var]])
      
      if (plot_type == "hist") {
        layer <- if (fill_var == "none") {
          geom_histogram(binwidth = bin_width, boundary = 0,
                         fill = "darkblue", color = "white", linewidth = 0.1)
        } else {
          geom_histogram(fill_aes, binwidth = bin_width, boundary = 0,
                         color = "white", linewidth = 0.1)
        }
        p <- p + layer + labs(y = "Attempts") + scale_y_continuous(labels = comma)
      } else {
        layer <- if (fill_var == "none") {
          geom_density(fill = "darkblue", alpha = 0.6, adjust = 1.5)
        } else {
          geom_density(fill_aes, alpha = 0.5, adjust = 1.5)
        }
        p <- p + layer + labs(y = "Density")
      }
      if (fill_var != "none") {
        p <- p + labs(fill = var_label(fill_var))
      }
      zoom <- coord_cartesian(xlim = x_limits)
    }
    
    if (facet_var != "none") {
      p <- p + facet_wrap(vars(.data[[facet_var]]))
    }
    
    p + zoom + scale_fill_brewer(palette = "Dark2") + plot_theme
  })
  
  # -- Data Exploration: shooting by distance ------------------------------------
  
  output$dist_plot <- renderPlot({
    data <- shots_subset()
    validate(need(nrow(data) > 0, empty_data_msg))
    color_var <- input$dist_color
    metric <- input$dist_metric
    
    group_vars <- c("shot_dist", if (color_var != "none") color_var)
    by_dist <- data |>
      filter(shot_dist <= input$dist_max) |>
      mutate(points = shot_points(shot_value, made)) |>
      group_by(across(all_of(group_vars))) |>
      summarise(
        attempts = n(),
        fg_pct = mean(made),
        pts_per_shot = mean(points),
        .groups = "drop"
      ) |>
      filter(attempts >= input$dist_min_n)
    
    validate(need(
      nrow(by_dist) > 0,
      "No distance has enough attempts. Lower the minimum attempts or widen the filters."
    ))
    
    p <- ggplot(by_dist, aes(x = shot_dist, y = .data[[metric]]))
    if (color_var == "none") {
      p <- p + geom_point(aes(size = attempts), color = "darkblue", alpha = 0.8)
    } else {
      p <- p + geom_point(aes(size = attempts, color = .data[[color_var]]),
                          alpha = 0.8) +
        scale_color_brewer(palette = "Dark2") +
        labs(color = var_label(color_var))
    }
    
    y_scale <- if (metric == "fg_pct") {
      scale_y_continuous(labels = percent)
    } else {
      scale_y_continuous(labels = number_format(accuracy = 0.1))
    }
    y_label <- if (metric == "fg_pct") "FG%" else "Points per shot"
    
    p + y_scale +
      scale_size_continuous(labels = comma, range = c(1, 8)) +
      labs(
        title = paste(y_label, "by shot distance"),
        x = "Shot distance (ft)", y = y_label, size = "Attempts"
      ) +
      plot_theme +
      theme(legend.box = "vertical")
  })
  
  # -- Data Exploration: heatmap ---------------------------------------------------
  
  output$heat_plot <- renderPlot({
    data <- shots_subset()
    validate(need(nrow(data) > 0, empty_data_msg))
    row_var <- input$heat_rows
    col_var <- input$heat_cols
    metric <- input$heat_metric
    validate(need(row_var != col_var, "Choose different variables for rows and columns."))
    
    heat_data <- data |>
      mutate(points = shot_points(shot_value, made)) |>
      group_by(.data[[row_var]], .data[[col_var]]) |>
      summarise(
        attempts     = n(),
        three_rate   = mean(shot_value == "3-pt"),
        fg_pct       = mean(made),
        pts_per_shot = mean(points),
        avg_dist     = mean(shot_dist),
        ast_rate     = if (any(made)) mean(assisted[made] == "Assisted") else NA_real_,
        .groups = "drop"
      ) |>
      mutate(row_level = fct_reorder(.data[[row_var]], .data[[metric]],
                                     .fun = mean, na.rm = TRUE))
    
    fill_labels <- if (metric %in% c("attempts", "avg_dist", "pts_per_shot")) {
      label_number(accuracy = if (metric == "attempts") 1 else 0.1, big.mark = ",")
    } else {
      label_percent(accuracy = 1)
    }
    
    p <- ggplot(heat_data, aes(x = .data[[col_var]], y = row_level,
                               fill = .data[[metric]])) +
      geom_tile(color = "white") +
      scale_fill_gradient(low = "grey", high = "red",
                          labels = fill_labels, na.value = "grey") +
      labs(
        title = paste(var_label(metric), "by", var_label_lower(row_var),
                      "and", var_label_lower(col_var)),
        subtitle = paste("Rows ordered by their average", var_label_lower(metric)),
        x = var_label(col_var), y = var_label(row_var),
        fill = var_label(metric)
      ) +
      plot_theme +
      theme(panel.grid = element_blank(), legend.key.width = unit(1.5, "cm"))
    
    if (input$heat_labels) {
      p <- p + geom_text(aes(label = format_metric(.data[[metric]], metric)),
                         size = 3.5)
    }
    p
  })
}

shinyApp(ui, server)