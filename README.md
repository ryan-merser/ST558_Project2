# ST558_Project2
NBA Shot Explorer

ST558 Project 2: a Shiny app for exploring every field-goal attempt from five NBA seasons (2015-16 through 2019-20), a little over one million shots.

Live app: 

Purpose

The app lets you filter the shot data and build your own tables and plots. You can look at how the league's shot mix changed during the three-point boom, how accuracy depends on distance and shot type, and how teams and players compare.

Data

The data come from the NBA Play-by-Play Data set on Kaggle, scraped from Basketball-Reference. data_prep.R reduces the raw play-by-play files (about 130 MB per season) to one row per field-goal attempt. The result is saved as data/nba_shots.rds (about 6 MB). Free throws are not included.

App layout

Sidebar: subset the data by season, team, game type and shot value, plus two numeric variables with range sliders. Changes take effect when you press Apply filters.
About: the app's purpose, the data source and how to use each tab.
Data Download: view the filtered data and save it as a CSV.
Data Exploration: contingency tables and bar charts, numeric summaries with histograms, density plots and box plots, FG% by shot distance, and a heatmap comparing any two categorical variables.
Shooting Report: pick a season, team and player to get a player card. It rates their shooting against the rest of the league, gives a playstyle label based on where they shoot, and compares their shot mix with the league average.

Files

app.R:	The Shiny app
player_report.R:	Helper functions for the Shooting Report tab
data_prep.R:	Reduces the raw Kaggle CSVs to data/nba_shots.rds
eda.qmd:	Static exploratory analysis done before building the app
make_court_image.R:	Creates the court image on the About tab (www/court_fg.png)

Running locally

install.packages(c("shiny", "bslib", "dplyr", "tidyr", "forcats", "ggplot2",
                   "readr", "scales", "DT", "shinycssloaders", "purrr", "tibble"))
shiny::runApp()

The processed data are included in the repo, so you don't need the raw files to run the app. To rebuild data/nba_shots.rds, download the season CSVs from Kaggle into data/raw/ and run data_prep.R.

AI use

AI was used as a coding assistant for part of this project (the Experimental Player shooting report tab, which was outside the scope of the project but I just wanted to include for fun). All code was reviewed, run and tested by me.
