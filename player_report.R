# player_report.R --------------------------------------------------------------
# Helper functions for the app's Shooting Report tab. Everything is built from
# the existing shot-level data (data/nba_shots.rds), so no extra data prep is
# needed. Sourced by app.R.
#
# Unit of analysis: one player's regular-season shots for one team in one
# season (a "stint"). Traded players have one stint per team.
# ------------------------------------------------------------------------------

# ---- Settings -----------------------------------------------------------------

zone_levels <- c("Rim (0-3 ft)", "Short (4-9 ft)", "Mid-range (10+ ft)", "Three")

min_player_fga <- 25    # smallest stint that can be selected in the app
qualify_fga    <- 200   # stints with this many FGA form the comparison pool

# Each rating: the statistic it ranks, the attempts column it depends on, and
# the minimum attempts needed before the rating is shown
ratings_def <- tibble::tribble(
  ~key,         ~label,              ~stat,        ~n_col,    ~min_n,
  "rim",        "Rim finishing",     "rim_pct",    "rim_a",   40,
  "short",      "Short range",       "short_pct",  "short_a", 30,
  "mid",        "Mid-range",         "mid_pct",    "mid_a",   30,
  "three",      "Three-point",       "fg3_pct",    "fg3a",    50,
  "efficiency", "Efficiency (eFG%)", "efg",        "fga",     0,
  "volume",     "Volume (FGA/game)", "fga_pg",     "fga",     0,
  "creation",   "Shot creation",     "unast_rate", "fgm",     50
)

tier_labels <- c("Below Average", "Average", "Above Average", "Elite")

# ---- Building the player table ------------------------------------------------

# Assign every shot to one of four zones
add_zone <- function(data) {
  data |>
    mutate(
      zone = case_when(
        shot_value == "3-pt" ~ zone_levels[4],
        shot_dist <= 3       ~ zone_levels[1],
        shot_dist <= 9       ~ zone_levels[2],
        TRUE                 ~ zone_levels[3]
      ),
      zone = factor(zone, levels = zone_levels)
    )
}

# Divide, returning NA instead of NaN/Inf when the denominator is 0
safe_divide <- function(x, y) {
  if_else(y > 0, x / y, NA_real_)
}

# One row per player-team-season with shooting totals and rates
build_player_stints <- function(shots) {
  shots |>
    filter(game_type == "Regular") |>
    add_zone() |>
    group_by(season, shooting_team, shooter_id) |>
    summarise(
      shooter  = first(shooter),
      games    = n_distinct(game_id),
      fga      = n(),
      fgm      = sum(made),
      fg3a     = sum(zone == zone_levels[4]),
      fg3m     = sum(made & zone == zone_levels[4]),
      rim_a    = sum(zone == zone_levels[1]),
      rim_m    = sum(made & zone == zone_levels[1]),
      short_a  = sum(zone == zone_levels[2]),
      short_m  = sum(made & zone == zone_levels[2]),
      mid_a    = sum(zone == zone_levels[3]),
      mid_m    = sum(made & zone == zone_levels[3]),
      dunks    = sum(shot_kind == "Dunk"),
      unast_m  = sum(made & assisted == "Unassisted"),
      avg_dist = mean(shot_dist),
      .groups  = "drop"
    ) |>
    filter(fga >= min_player_fga) |>
    mutate(
      fg_pct      = fgm / fga,
      fg3_pct     = safe_divide(fg3m, fg3a),
      efg         = (fgm + 0.5 * fg3m) / fga,
      rim_pct     = safe_divide(rim_m, rim_a),
      short_pct   = safe_divide(short_m, short_a),
      mid_pct     = safe_divide(mid_m, mid_a),
      rim_share   = rim_a / fga,
      short_share = short_a / fga,
      mid_share   = mid_a / fga,
      three_rate  = fg3a / fga,
      unast_rate  = safe_divide(unast_m, fgm),
      fga_pg      = fga / games,
      fg_pts_pg   = (2 * (fgm - fg3m) + 3 * fg3m) / games,
      qualified   = fga >= qualify_fga
    )
}

# League-wide share of attempts and FG% by zone in each regular season
build_league_zones <- function(shots) {
  shots |>
    filter(game_type == "Regular") |>
    add_zone() |>
    group_by(season, zone) |>
    summarise(attempts = n(), fg_pct = mean(made), .groups = "drop_last") |>
    mutate(share = attempts / sum(attempts)) |>
    ungroup()
}

# ---- Ratings, tiers and playstyle -------------------------------------------------

# Percentile (1-99) of each value within the qualified pool. Values without
# enough attempts (eligible = FALSE) get NA.
percentile_in_pool <- function(x, eligible, in_pool) {
  pool <- x[eligible & in_pool & !is.na(x)]
  pct <- vapply(x, function(value) mean(pool <= value), numeric(1))
  pct <- pmin(pmax(round(100 * pct), 1), 99)
  if_else(eligible & !is.na(x), pct, NA_real_)
}

# Map percentiles to the four tiers
percentile_tier <- function(pct) {
  cut(pct, breaks = c(-Inf, 30, 65, 90, Inf), labels = tier_labels,
      right = FALSE)
}

# Add a percentile column (pct_<key>) for each rating, ranked within season,
# then an overall rating
add_ratings <- function(stints) {
  stints <- group_by(stints, season)
  
  for (i in seq_len(nrow(ratings_def))) {
    r <- ratings_def[i, ]
    stints <- stints |>
      mutate(!!paste0("pct_", r$key) := percentile_in_pool(
        .data[[r$stat]], .data[[r$n_col]] >= r$min_n, qualified
      ))
  }
  
  # Accuracy = zone ratings weighted by how often the player shoots there
  zone_pcts   <- c("pct_rim", "pct_short", "pct_mid", "pct_three")
  zone_shares <- c("rim_share", "short_share", "mid_share", "three_rate")
  
  stints |>
    mutate(
      accuracy = weighted_zone_rating(pick(all_of(zone_pcts)),
                                      pick(all_of(zone_shares))),
      overall_raw = rowMeans(
        cbind(pct_efficiency, pct_volume, pct_creation, accuracy),
        na.rm = TRUE
      ),
      pct_overall = percentile_in_pool(overall_raw, TRUE, qualified),
      playstyle = classify_playstyle(rim_share, short_share, mid_share,
                                     three_rate, unast_rate, fga_pg)
    ) |>
    ungroup()
}

# Row-wise weighted mean of zone percentiles, skipping zones with no rating
weighted_zone_rating <- function(pcts, shares) {
  pcts <- as.matrix(pcts)
  shares <- as.matrix(shares)
  shares[is.na(pcts)] <- 0
  pcts[is.na(pcts)] <- 0
  safe_divide(rowSums(pcts * shares), rowSums(shares))
}

# Playstyle label from the player's shot mix (first matching rule wins)
classify_playstyle <- function(rim, short, mid, three, unast, fga_pg) {
  case_when(
    rim >= 0.65 & three < 0.05                         ~ "Rim Runner",
    rim + short >= 0.60 & three < 0.15                 ~ "Interior Scorer",
    rim >= 0.45 & three < 0.35                         ~ "Slasher",
    three >= 0.50 & unast >= 0.50                      ~ "Pull-Up Shooter",
    three >= 0.50 & unast < 0.25                       ~ "Spot-Up Shooter",
    three >= 0.50                                      ~ "Floor Spacer",
    mid >= 0.35 & three < 0.20                         ~ "Mid-Range Maestro",
    rim >= 0.15 & mid >= 0.15 & three >= 0.20 &
      fga_pg >= 14                                     ~ "Three-Level Scorer",
    rim >= 0.35 & three >= 0.25                        ~ "Inside-Out Scorer",
    three >= 0.40                                      ~ "Perimeter Shooter",
    TRUE                                               ~ "Balanced Shooter"
  )
}

# Short description shown under each playstyle label
playstyle_blurb <- c(
  "Rim Runner"         = "Lives at the basket on dunks, lobs and putbacks; rarely shoots from outside.",
  "Interior Scorer"    = "Scores mostly inside 10 feet with layups, hooks and short jumpers.",
  "Slasher"            = "Attacks the rim often but will also take some outside shots.",
  "Pull-Up Shooter"    = "Takes mostly threes and creates many of them off the dribble.",
  "Spot-Up Shooter"    = "Takes mostly threes, nearly all set up by a teammate's pass.",
  "Floor Spacer"       = "Takes mostly threes, mixing catch-and-shoot and self-created looks.",
  "Mid-Range Maestro"  = "Does much of his damage from mid-range, between the paint and the arc.",
  "Three-Level Scorer" = "High-volume scorer who gets shots at the rim, in the mid-range and from three.",
  "Inside-Out Scorer"  = "Splits his game between attacking the rim and shooting threes.",
  "Perimeter Shooter"  = "Leans toward the three-point line but still takes a fair share of twos.",
  "Balanced Shooter"   = "Spreads his attempts across the floor without one dominant zone."
)

# Extra trait badges based on volume, shot creation and efficiency
player_traits <- function(player) {
  traits <- c(
    "High Volume"     = player$fga_pg >= 15,
    "Shot Creator"    = !is.na(player$unast_rate) && player$unast_rate >= 0.60,
    "Catch-and-Shoot" = !is.na(player$unast_rate) && player$unast_rate <= 0.20 &&
      player$three_rate >= 0.35,
    "Efficient"       = !is.na(player$pct_efficiency) && player$pct_efficiency >= 80,
    "Small Sample"    = !player$qualified
  )
  names(traits)[traits]
}

# ---- Card and chart ---------------------------------------------------------------

# CSS class for a tier (e.g., "Above Average" -> "tier-above-average")
tier_class <- function(tier) {
  paste0("tier-", gsub(" ", "-", tolower(as.character(tier))))
}

# One row of the ratings list: label, number, tier pill and bar
rating_row <- function(label, pct) {
  if (is.na(pct)) {
    return(div(
      class = "rating-row",
      span(class = "rating-label", label),
      span(class = "rating-value", "--"),
      span(class = "tier-pill tier-na", "Not enough attempts"),
      div(class = "rating-bar")
    ))
  }
  tier <- percentile_tier(pct)
  div(
    class = "rating-row",
    span(class = "rating-label", label),
    span(class = "rating-value", pct),
    span(class = paste("tier-pill", tier_class(tier)), as.character(tier)),
    div(class = "rating-bar",
        div(class = paste("rating-fill", tier_class(tier)),
            style = paste0("width: ", pct, "%;")))
  )
}

# A single summary statistic box
stat_box <- function(value, label) {
  div(class = "stat-box", div(class = "stat-value", value),
      div(class = "stat-label", label))
}

# The full player card (returns HTML tags)
player_card <- function(player) {
  traits <- player_traits(player)
  
  # Overall is only shown for players in the qualified pool
  if (player$qualified) {
    overall_number <- player$pct_overall
    overall_tier   <- as.character(percentile_tier(player$pct_overall))
    card_class     <- tier_class(overall_tier)
  } else {
    overall_number <- "--"
    overall_tier   <- paste("Under", qualify_fga, "FGA")
    card_class     <- "tier-small-sample"
  }
  fmt_pct <- function(x) if (is.na(x)) "--" else percent(x, accuracy = 0.1)
  
  rating_rows <- purrr::map2(
    ratings_def$label, ratings_def$key,
    function(label, key) rating_row(label, player[[paste0("pct_", key)]])
  )
  
  div(
    class = paste("player-card", card_class),
    div(
      class = "card-top",
      div(
        class = "card-overall",
        div(class = "overall-number", overall_number),
        div(class = "overall-label", "OVERALL"),
        div(class = "overall-tier", overall_tier)
      ),
      div(
        class = "card-identity",
        h3(class = "card-name", player$shooter),
        div(class = "card-sub", paste(player$shooting_team, "|", player$season,
                                      "regular season")),
        div(class = "playstyle", player$playstyle),
        p(class = "playstyle-blurb", playstyle_blurb[[player$playstyle]]),
        div(class = "trait-list",
            lapply(traits, function(t) span(class = "trait", t)))
      )
    ),
    div(class = "card-ratings", rating_rows),
    div(
      class = "card-stats",
      stat_box(player$games, "Games with a shot"),
      stat_box(number(player$fga_pg, accuracy = 0.1), "FGA per game"),
      stat_box(number(player$fg_pts_pg, accuracy = 0.1), "FG points per game"),
      stat_box(fmt_pct(player$fg_pct), "FG%"),
      stat_box(fmt_pct(player$efg), "eFG%"),
      stat_box(fmt_pct(player$fg3_pct), paste0("3P% (", player$fg3a, " att.)")),
      stat_box(fmt_pct(player$three_rate), "3-pt attempt rate"),
      stat_box(number(player$avg_dist, accuracy = 0.1), "Avg. shot distance (ft)"),
      stat_box(fmt_pct(player$unast_rate), "Makes unassisted"),
      stat_box(comma(player$dunks), "Dunk attempts")
    )
  )
}

# Bar chart comparing the player's shot mix and FG% by zone to the league
profile_plot <- function(player, league_zones) {
  player_zones <- tibble(
    zone = factor(zone_levels, levels = zone_levels),
    attempts = c(player$rim_a, player$short_a, player$mid_a, player$fg3a),
    makes = c(player$rim_m, player$short_m, player$mid_m, player$fg3m)
  ) |>
    mutate(
      share = attempts / sum(attempts),
      # FG% from fewer than 10 attempts is too noisy to show
      fg_pct = if_else(attempts >= 10, makes / attempts, NA_real_)
    )
  
  league <- filter(league_zones, season == player$season)
  
  plot_data <- bind_rows(
    mutate(player_zones, group = player$shooter),
    mutate(league, group = "League average")
  ) |>
    select(zone, group, share, fg_pct) |>
    pivot_longer(c(share, fg_pct), names_to = "measure", values_to = "value") |>
    mutate(
      measure = factor(measure, levels = c("share", "fg_pct"),
                       labels = c("Share of attempts", "FG%")),
      group = factor(group, levels = c(player$shooter, "League average"))
    )
  
  ggplot(plot_data, aes(x = zone, y = value, fill = group)) +
    geom_col(position = position_dodge(width = 0.75), width = 0.7,
             na.rm = TRUE) +
    geom_text(aes(label = percent(value, accuracy = 1)),
              position = position_dodge(width = 0.75), vjust = -0.4,
              size = 3.6, na.rm = TRUE) +
    facet_wrap(~ measure, ncol = 2) +
    scale_y_continuous(labels = percent, limits = c(0, 1),
                       expand = expansion(mult = c(0, 0.02))) +
    scale_fill_manual(values = c("darkblue", "grey70")) +
    labs(
      title = "Where he shoots and how well, compared with the league",
      caption = "FG% is hidden for zones with fewer than 10 attempts.",
      x = NULL, y = NULL, fill = NULL
    ) +
    theme_minimal(base_size = 14) +
    theme(
      legend.position = "bottom",
      plot.title = element_text(face = "bold"),
      axis.text.x = element_text(size = 10),
      panel.grid.major.x = element_blank()
    )
}

# ---- Card styling ---------------------------------------------------------------

player_card_css <- "
.player-card { border-radius: 18px; padding: 22px; max-width: 760px;
  box-shadow: 0 6px 18px rgba(0,0,0,0.18); color: #1b1b1b; }
.player-card.tier-elite { background: linear-gradient(135deg, #0b1d3a, #1d428a);
  color: #fdf6e3; }
.player-card.tier-above-average { background: linear-gradient(135deg, #fbe7a1, #d9a520); }
.player-card.tier-average, .player-card.tier-small-sample {
  background: linear-gradient(135deg, #f1f3f6, #b9c1cc); }
.player-card.tier-below-average { background: linear-gradient(135deg, #f0cfb2, #b07a4f); }
.card-top { display: flex; gap: 22px; align-items: flex-start; }
.card-overall { text-align: center; min-width: 96px; }
.overall-number { font-size: 64px; font-weight: 800; line-height: 1; }
.overall-label { font-size: 12px; letter-spacing: 2px; font-weight: 700; }
.overall-tier { font-size: 13px; font-weight: 600; margin-top: 4px; }
.card-name { margin: 0; font-weight: 800; }
.card-sub { font-size: 14px; opacity: 0.85; margin-bottom: 8px; }
.playstyle { display: inline-block; font-weight: 700; font-size: 18px;
  padding: 2px 12px; border-radius: 999px; background: rgba(255,255,255,0.55);
  color: #1b1b1b; }
.playstyle-blurb { font-size: 14px; margin: 6px 0; }
.trait { display: inline-block; font-size: 12px; font-weight: 600;
  padding: 2px 9px; margin: 2px 4px 2px 0; border-radius: 999px;
  border: 1px solid currentColor; }
.card-ratings { margin-top: 16px; background: rgba(255,255,255,0.75);
  border-radius: 12px; padding: 10px 14px; color: #1b1b1b; }
.rating-row { display: grid; grid-template-columns: 150px 36px 150px 1fr;
  align-items: center; gap: 8px; padding: 4px 0; font-size: 14px; }
.rating-value { font-weight: 800; text-align: right; }
.tier-pill { font-size: 12px; font-weight: 600; text-align: center;
  border-radius: 999px; padding: 1px 8px; color: white; }
.rating-bar { height: 8px; background: #e3e6ea; border-radius: 4px; overflow: hidden; }
.rating-fill { height: 100%; }
.tier-pill.tier-elite, .rating-fill.tier-elite { background: #6b3fa0; }
.tier-pill.tier-above-average, .rating-fill.tier-above-average { background: #1a7f4b; }
.tier-pill.tier-average, .rating-fill.tier-average { background: #5c6b7a; }
.tier-pill.tier-below-average, .rating-fill.tier-below-average { background: #b4472c; }
.tier-pill.tier-na { background: #9aa3ad; }
.card-stats { display: grid; grid-template-columns: repeat(5, 1fr); gap: 8px;
  margin-top: 14px; }
.stat-box { background: rgba(255,255,255,0.75); border-radius: 10px;
  padding: 8px; text-align: center; color: #1b1b1b; }
.stat-value { font-size: 20px; font-weight: 800; }
.stat-label { font-size: 11px; opacity: 0.8; }
@media (max-width: 700px) {
  .card-stats { grid-template-columns: repeat(2, 1fr); }
  .rating-row { grid-template-columns: 110px 30px 1fr; }
  .rating-bar { display: none; }
}
"