# make_court_image.R -----------------------------------------------------------
# Purpose: Create the picture shown on the app's About tab: an NBA half court
#          shaded by field-goal percentage at each shot distance.
# ------------------------------------------------------------------------------

library(dplyr)
library(ggplot2)
library(readr)
library(scales)

shots <- read_rds("data/nba_shots.rds")

hoop_y <- 5.25  # hoop center, in feet from the baseline

# FG% for each 1-ft distance band (bands with few attempts are left blank)
fg_by_dist <- shots |>
  filter(shot_dist <= 30) |>
  group_by(shot_dist) |>
  summarise(fg_pct = mean(made), attempts = n(), .groups = "drop") |>
  filter(attempts >= 100)

# Build a ring (annulus) polygon around the hoop for one distance band
make_ring <- function(dist, fg_pct, n_points = 200) {
  angles <- seq(0, 2 * pi, length.out = n_points)
  outer  <- dist + 1
  inner  <- dist
  tibble(
    x = c(outer * cos(angles), rev(inner * cos(angles))),
    y = hoop_y + c(outer * sin(angles), rev(inner * sin(angles))),
    dist = dist,
    fg_pct = fg_pct
  )
}

rings <- purrr::map2(fg_by_dist$shot_dist, fg_by_dist$fg_pct, make_ring) |>
  purrr::list_rbind()

# Helper for drawing circle arcs (court lines)
arc <- function(x0, y0, r, from, to, n = 200) {
  angles <- seq(from, to, length.out = n)
  tibble(x = x0 + r * cos(angles), y = y0 + r * sin(angles))
}

corner_y  <- hoop_y + sqrt(23.75^2 - 22^2)          # where the arc meets the corners
arc_angle <- asin((corner_y - hoop_y) / 23.75)

three_line <- bind_rows(
  tibble(x = -22, y = c(0, corner_y)),
  arc(0, hoop_y, 23.75, pi - arc_angle, arc_angle),
  tibble(x = 22, y = c(corner_y, 0))
)

line_col <- "white"

court <- ggplot() +
  geom_polygon(
    data = rings,
    aes(x = x, y = y, group = dist, fill = fg_pct),
    color = NA
  ) +
  # paint, free-throw circle, restricted area, backboard, hoop
  annotate("rect", xmin = -8, xmax = 8, ymin = 0, ymax = 19,
           fill = NA, color = line_col, linewidth = 0.8) +
  geom_path(data = arc(0, 19, 6, 0, pi), aes(x, y),
            color = line_col, linewidth = 0.8) +
  geom_path(data = arc(0, hoop_y, 4, 0, pi), aes(x, y),
            color = line_col, linewidth = 0.8) +
  annotate("segment", x = -3, xend = 3, y = 4, yend = 4,
           color = line_col, linewidth = 1.2) +
  geom_path(data = arc(0, hoop_y, 0.75, 0, 2 * pi), aes(x, y),
            color = "red", linewidth = 1) +
  # three-point line, half-court line and center circle
  geom_path(data = three_line, aes(x, y), color = line_col, linewidth = 0.8) +
  annotate("segment", x = -25, xend = 25, y = 47, yend = 47,
           color = line_col, linewidth = 1.6) +
  geom_path(data = arc(0, 47, 6, pi, 2 * pi), aes(x, y),
            color = line_col, linewidth = 0.8) +
  scale_fill_gradient(
    low = "darkblue", high = "gold",
    labels = percent, name = "FG%"
  ) +
  coord_fixed(xlim = c(-25, 25), ylim = c(0, 47), expand = FALSE) +
  labs(
    title = "Field-Goal Percentage by Shot Distance",
    subtitle = "NBA, 2015-16 to 2019-20 (shots within 30 ft)"
  ) +
  theme_void(base_size = 13) +
  theme(
    panel.background = element_rect(fill = "tan", color = NA),
    plot.background  = element_rect(fill = "white", color = NA),
    plot.title       = element_text(face = "bold", hjust = 0.5),
    plot.subtitle    = element_text(hjust = 0.5, margin = margin(b = 8)),
    legend.position  = "bottom",
    legend.key.width = unit(1.5, "cm"),
    plot.margin      = margin(10, 10, 10, 10)
  )

ggsave("www/court_fg.png", court, width = 6, height = 6.6, dpi = 150)