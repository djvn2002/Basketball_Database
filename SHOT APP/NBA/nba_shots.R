# Author: David Vialpando-Nielsen
# Date Made: 8/25/2024
# Latest Update: 9/4/2024

# This file is used to grab ESPN pbp data for shot attempts since 2014, you can grab more
# years if you would like but I recommend keeping it at 2014 and later for performance.
# This file takes a while to run so keep your eyes on it

library(tidyverse)
library(hoopR)

# Data Collection of WNBA Data from 2014 to 2024
nba_shots <- load_nba_pbp(seasons = 2014:most_recent_nba_season()) %>%
  filter(shooting_play == T,
         coordinate_x <= 47.5 & coordinate_x >= -47.5,
         coordinate_y <= 25 & coordinate_y >= -25) %>%
  mutate(
    team_name = if_else(team_id == home_team_id, home_team_name, away_team_name),
    team_mascot = if_else(team_id == home_team_id, home_team_mascot, away_team_mascot),
    team_name = paste0(team_name, " ", team_mascot),
    team_abbreviation = if_else(team_id == home_team_id, home_team_abbrev, away_team_abbrev),
    team_id = case_when(
      team_name == "Charlotte Bobcats" ~ 33,
      TRUE ~ team_id
    ),
    home_team_id = case_when(
      home_team_abbrev == "CHA" & team_name == "Charlotte Bobcats" ~ 33,
      TRUE ~ home_team_id
    ),
    away_team_id = case_when(
      away_team_abbrev == "CHA" & team_name == "Charlotte Bobcats" ~ 33,
      TRUE ~ away_team_id
      ),
    home_team_abbrev = case_when(
      team_name == "Charlotte Bobcats" ~ "CHB",
      TRUE ~ home_team_abbrev
    ),
    away_team_abbrev = case_when(
      team_name == "Charlotte Bobcats" ~ "CHB",
      TRUE ~ away_team_abbrev))

# Creating Shot Type for nba_shots to track attempts
type_shots <- nba_shots %>%
  group_by(type_text,score_value) %>%
  distinct(type_text)

# Separate shots into home and away teams
home_nba_shots <- nba_shots %>%
  filter(team_id == home_team_id)

away_nba_shots <- nba_shots %>%
  filter(team_id == away_team_id)

away_nba_shots <- away_nba_shots %>%
  mutate(coordinate_x = ifelse(id == 401031412365, -coordinate_x, coordinate_x),
         coordinate_y = ifelse(id == 401031412365, -coordinate_y, coordinate_y))

# Finding obscure 3 point shot locations
obscure_3PA_away <- away_nba_shots %>%
  filter(score_value == 3) %>%
  select(coordinate_x, coordinate_y, score_value) %>%
  group_by(coordinate_x, coordinate_y, score_value) %>%
  summarise(count = n()) %>%
  distinct()

obscure_3PA_home <- home_nba_shots %>%
  filter(score_value == 3) %>%
  select(coordinate_x, coordinate_y, score_value) %>%
  group_by(coordinate_x, coordinate_y, score_value) %>%
  summarise(count = n()) %>%
  distinct()

# Calculate arc boundaries for away and home shots
arc_boundary_away <- obscure_3PA_away %>%
  group_by(coordinate_y) %>%
  summarise(min_x = min(coordinate_x)) %>%
  filter(coordinate_y > -22,
         coordinate_y < 22) %>%
  ungroup()

arc_boundary_home <- obscure_3PA_home %>%
  group_by(coordinate_y) %>%
  summarise(max_x = max(coordinate_x)) %>%
  filter(coordinate_y > -22,
         coordinate_y < 22) %>%
  ungroup()

# Function to check if a point is within the arc (2PA zone) for away shots
is_within_arc_away <- function(x, y, arc_boundary) {
  arc_x_limit <- arc_boundary %>%
    filter(coordinate_y == y) %>%
    pull(min_x)
  
  if (length(arc_x_limit) == 0) {
    return(FALSE)
  } else {
    return(x < arc_x_limit)
  }
}

# Function to check if a point is within the arc (2PA zone) for home shots
is_within_arc_home <- function(x, y, arc_boundary) {
  arc_x_limit <- arc_boundary %>%
    filter(coordinate_y == y) %>%
    pull(max_x)
  
  if (length(arc_x_limit) == 0) {
    return(FALSE)
  } else {
    return(x > arc_x_limit)
  }
}

# Classify shots for away_nba_shots
away_nba_shots <- away_nba_shots %>%
  mutate(
    shot_type = case_when(
      # Free Throw Identification
      score_value == 1 ~ "FTA",
      grepl("free throw", text, ignore.case = TRUE) & score_value == 1 ~ "FTA",
      grepl("free_throw", text, ignore.case = TRUE) & score_value == 1 ~ "FTA",
      grepl("free throw", type_text, ignore.case = TRUE) & score_value == 1 ~ "FTA",
      grepl("free_throw", type_text, ignore.case = TRUE) & score_value == 1 ~ "FTA",
      score_value == 0 & grepl("free throw", text, ignore.case = TRUE) ~ "FTA",
      score_value == 0 & grepl("free_throw", text, ignore.case = TRUE) ~ "FTA",
      score_value == 0 & grepl("free throw", type_text, ignore.case = TRUE) ~ "FTA",
      score_value == 0 & grepl("free_throw", type_text, ignore.case = TRUE) ~ "FTA",
      
      # Explicit score-based identification
      score_value == 3 ~ "3PA",
      score_value == 2 ~ "2PA",
      
      # Check if the shot is within the arc (should be 2PA)
      mapply(is_within_arc_away, coordinate_x, coordinate_y, MoreArgs = list(arc_boundary = arc_boundary_away)) ~ "2PA",
      
      # Coordinate-based identification for 3PA
      (coordinate_y <= -22 | coordinate_y >= 22) ~ "3PA",
      
      # Default to 3PA if not classified by above
      TRUE ~ "3PA"
    )
  )

# Classify shots for home_nba_shots
home_nba_shots <- home_nba_shots %>%
  mutate(
    shot_type = case_when(
      # Free Throw Identification
      score_value == 1 ~ "FTA",
      grepl("free throw", text, ignore.case = TRUE) & score_value == 1 ~ "FTA",
      grepl("free_throw", text, ignore.case = TRUE) & score_value == 1 ~ "FTA",
      grepl("free throw", type_text, ignore.case = TRUE) & score_value == 1 ~ "FTA",
      grepl("free_throw", type_text, ignore.case = TRUE) & score_value == 1 ~ "FTA",
      score_value == 0 & grepl("free throw", text, ignore.case = TRUE) ~ "FTA",
      score_value == 0 & grepl("free_throw", text, ignore.case = TRUE) ~ "FTA",
      score_value == 0 & grepl("free throw", type_text, ignore.case = TRUE) ~ "FTA",
      score_value == 0 & grepl("free_throw", type_text, ignore.case = TRUE) ~ "FTA",
      
      # Explicit score-based identification
      score_value == 3 ~ "3PA",
      score_value == 2 ~ "2PA",
      
      # Check if the shot is within the arc (should be 2PA)
      mapply(is_within_arc_home, coordinate_x, coordinate_y, MoreArgs = list(arc_boundary = arc_boundary_home)) ~ "2PA",
      
      # Coordinate-based identification for 3PA
      (coordinate_y <= -22 | coordinate_y >= 22) ~ "3PA",
      
      # Default to 3PA if not classified by above
      TRUE ~ "3PA"
    )
  ) %>%
  mutate(coordinate_x = -coordinate_x,
         coordinate_y = -coordinate_y)

# Bind rows back together
nba_shots <- bind_rows(home_nba_shots, away_nba_shots)

# Counting of each shot type
shot_count <- nba_shots %>%
  group_by(shot_type) %>%
  summarise(count = n())

# Directory path for csv files (MAKE SURE TO CHANGE THE DIRECTORY ACCORDINGLY!!!)
conf_path <- "C:/Users/djvia/OneDrive/Documents/Blog Website/Basketball_Database/SHOT APP/NBA"

if (!dir.exists(conf_path)) {
  dir.create(conf_path, recursive = TRUE)
}

nba_conf <- read.csv(file.path(conf_path,"nba_conf.csv"))

# Grabs the identity for every nba team, will be helpful once we aggregate scoring
team_identity <- nba_shots %>%
  select(team_id, team_name, team_abbreviation) %>%
  distinct() %>%
  filter(team_id >= 1 & team_id <= 30 | team_id == 33) %>%
  mutate(team_abbreviation = if_else(team_name == "Charlotte Bobcats", "CHB", team_abbreviation)) %>%
  left_join(nba_conf) %>%
  arrange(team_id)

# This identifies that there are minimal exceptions due to lack of data for specific
# scoring plays
team_w_NA <- nba_shots %>%
  filter(is.na(team_id))

# Check for duplicates in the team_identity dataset
team_identity_duplicates <- team_identity %>%
  group_by(team_id) %>%
  filter(n() > 1)

# Remove duplicate rows based on join keys if any
team_identity <- team_identity %>%
  distinct(team_id, .keep_all = TRUE)

# Team Season Aggregation
nba_season_team <- nba_shots %>%
  group_by(season,team_id) %>%
  summarise(tot_scoring = sum(if_else(scoring_play == TRUE, score_value, 0), na.rm = TRUE),
            tot_scoring_no_ft = sum(if_else(scoring_play == TRUE & score_value %in% c(2, 3), score_value, 0), na.rm = TRUE),
            `2PM` = sum(score_value == 2 & scoring_play == TRUE, na.rm = TRUE),
            `2PA` = sum((score_value == 2 & scoring_play == TRUE) | (score_value == 0 & shot_type == "2PA"), na.rm = TRUE),
            `3PM` = sum(score_value == 3 & scoring_play == TRUE, na.rm = TRUE),
            `3PA` = sum((score_value == 3 & scoring_play == TRUE) | (score_value == 0 & shot_type == "3PA"), na.rm = TRUE),
            FTM = sum(shot_type == "FTA" & scoring_play == TRUE, na.rm = TRUE),
            FTA = sum((score_value == 1 & scoring_play == TRUE) | (score_value == 0 & shot_type == "FTA"), na.rm = TRUE),
            shots_made = sum(`2PM` + `3PM`, na.rm = TRUE),
            shots_attempted = sum(`2PA` + `3PA`, na.rm = TRUE)) %>%
  right_join(team_identity)

# Player Season Aggregation
players <- hoopR::load_nba_player_box(seasons = 2014:most_recent_nba_season())

players <- players %>%
  select(athlete_id, athlete_display_name) %>%
  distinct()

# Check for duplicates in the players dataset
players_duplicates <- players %>%
  group_by(athlete_id) %>%
  filter(n() > 1)

# Remove duplicate rows based on join keys if any
players <- players %>%
  distinct(athlete_id, .keep_all = TRUE)

nba_season_player <- nba_shots %>%
  group_by(season,athlete_id_1) %>%
  rename(athlete_id = athlete_id_1) %>%
  summarise(tot_scoring = sum(if_else(scoring_play == TRUE, score_value, 0), na.rm = TRUE),
            tot_scoring_no_ft = sum(if_else(scoring_play == TRUE & score_value %in% c(2, 3), score_value, 0), na.rm = TRUE),
            `2PM` = sum(score_value == 2 & scoring_play == TRUE, na.rm = TRUE),
            `2PA` = sum((score_value == 2 & scoring_play == TRUE) | (score_value == 0 & shot_type == "2PA"), na.rm = TRUE),
            `3PM` = sum(score_value == 3 & scoring_play == TRUE, na.rm = TRUE),
            `3PA` = sum((score_value == 3 & scoring_play == TRUE) | (score_value == 0 & shot_type == "3PA"), na.rm = TRUE),
            FTM = sum(shot_type == "FTA" & scoring_play == TRUE, na.rm = TRUE),
            FTA = sum((score_value == 1 & scoring_play == TRUE) | (score_value == 0 & shot_type == "FTA"), na.rm = TRUE),
            shots_made = sum(`2PM` + `3PM`, na.rm = TRUE),
            shots_attempted = sum(`2PA` + `3PA`, na.rm = TRUE)) %>%
  right_join(players)

# Acknowledging that there are some NA values
nba_season_player_NA <- nba_season_player %>%
  filter(is.na(season))

# Final Set for NBA during 2014 season and after
nba_shots <- nba_shots %>%
  select(id,season,season_type,team_id,athlete_id_1,coordinate_x,coordinate_y,shooting_play,
         scoring_play,score_value,shot_type) %>%
  rename(athlete_id = athlete_id_1) %>%
  right_join(players) %>%
  right_join(team_identity) %>%
  mutate(season_type = if_else(season_type == 3, "Postseason", "Regular Season"),
         League = "Professional",
         Gender = "Men",
         league_name = "NBA") %>%
  rename(athlete_name = athlete_display_name) %>%
  na.omit()

# Make sure to change this directory path when making rda files
# (MAKE SURE TO CHANGE THE DIRECTORY ACCORDINGLY!!!)
dir_path <- "C:/Users/djvia/OneDrive/Documents/Blog Website/Basketball_Database/SHOT APP/RDA Files"

if (!dir.exists(dir_path)) {
  dir.create(dir_path, recursive = TRUE)
}

# Write a rda of the final dataset
save(nba_shots,file = file.path(dir_path,"NBA_Shots.rda"))