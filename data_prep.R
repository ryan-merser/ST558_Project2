# data_prep.R 
# Purpose: Reduce the raw NBA play-by-play CSVs (one per season) to a single,
# small, shot-level dataset for the Shiny app and the static EDA file.
#
# Usage: Run once locally. Raw CSVs live in data/raw/ (git-ignored);
# the output data/nba_shots.rds is small and IS committed to the repo.

library(tidyverse)
library(lubridate)

raw_dir  <- "data/raw"
out_file <- "data/nba_shots.rds"

# Seasons to include. 2020-21 is left out because that file holds only part
# of the season
seasons <- c("2015-16", "2016-17", "2017-18", "2018-19", "2019-20")
raw_files <- file.path(raw_dir, paste0("NBA_PBP_", seasons, ".csv"))

# Columns we keep from the raw files, with explicit types so every season
# reads identically (prevents bind errors from mismatched guesses)
keep_cols <- c(
  "URL", "GameType", "Date", "Quarter", "SecLeft",
  "AwayTeam", "AwayPlay", "AwayScore",
  "HomeTeam", "HomePlay", "HomeScore",
  "Shooter", "ShotType", "ShotOutcome", "ShotDist",
  "Assister", "Blocker"
)
col_spec <- cols(
  .default  = col_character(),
  Quarter   = col_integer(),
  SecLeft   = col_double(),
  AwayScore = col_integer(),
  HomeScore = col_integer(),
  ShotDist  = col_double()
)

# Function: read one season and reduce it to field-goal attempts
read_season_shots <- function(file) {
  season_label <- str_extract(basename(file), "\\d{4}-\\d{2}")
  message("Reading ", season_label, " ...")
  
  read_csv(file, col_select = all_of(keep_cols), col_types = col_spec) |>
    # keep only field-goal attempts (free throws are recorded separately)
    filter(!is.na(Shooter), !is.na(ShotType)) |>
    mutate(season = season_label)
}

# Read every season and stack them 
shots_raw <- map(raw_files, read_season_shots) |>
  list_rbind()

# Sanity checks: confirm category labels before relying on them
glimpse(shots_raw)
count(shots_raw, season)
count(shots_raw, ShotType, sort = TRUE)
count(shots_raw, ShotOutcome)
count(shots_raw, GameType)

# Build derived variables 
shots <- shots_raw |>
  mutate(
    # which side the shot came from: the play text is in HomePlay or AwayPlay
    shooting_side = if_else(!is.na(HomePlay), "Home", "Away"),
    shooting_team = if_else(shooting_side == "Home", HomeTeam, AwayTeam),
    opponent      = if_else(shooting_side == "Home", AwayTeam, HomeTeam),
    
    # shot attributes
    shot_value = if_else(str_detect(ShotType, "^3"), "3-pt", "2-pt"),
    points     = if_else(shot_value == "3-pt", 3L, 2L),
    shot_kind  = ShotType |> str_remove("^[23]-pt ") |> str_to_title(),
    made       = ShotOutcome == "make",
    outcome    = if_else(made, "Make", "Miss"),
    assisted   = if_else(!is.na(Assister), "Assisted", "Unassisted"),
    blocked    = !is.na(Blocker),
    
    # game situation; overtime periods are collapsed into one "OT" level
    period = if_else(Quarter <= 4, paste0("Q", Quarter), "OT"),
    sec_left_period = SecLeft,
    
    # score margin from the shooting team's perspective BEFORE the shot
    # (scores in the PBP file reflect the state after the play)
    margin_after = if_else(
      shooting_side == "Home",
      HomeScore - AwayScore,
      AwayScore - HomeScore
    ),
    score_margin = margin_after - if_else(made, points, 0L),
    
    game_type = str_to_title(GameType),
    game_date = mdy(Date)
  ) |>
  select(
    season, game_id = URL, game_date, game_type, period, sec_left_period,
    shooting_team, opponent, shooting_side, shooter = Shooter,
    shot_value, shot_kind, shot_dist = ShotDist, outcome, made,
    assisted, blocked, score_margin
  ) |>
  mutate(
    across(c(season, game_type, shooting_team, opponent, shooting_side,
             shot_value, shot_kind, outcome, assisted), as.factor),
    period = factor(period, levels = c("Q1", "Q2", "Q3", "Q4", "OT"))
  )

# ---- Save a compressed copy for the app --------------------------------------
write_rds(shots, out_file, compress = "xz")

cat("Rows:", nrow(shots), "\n")
cat("File size (MB):", round(file.size(out_file) / 1e6, 1), "\n")