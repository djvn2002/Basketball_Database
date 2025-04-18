# Author: David Vialpando-Nielsen
# Date Made: 9/28/2024
# Latest Update: 9/28/2024

# This is an update file to the WNBA_Shots.RDA file

library(tidyverse)
library(wehoop)

# Function to load WNBA shot data with a fallback season
load_wnba_shots_with_fallback <- function(first_season, fallback_season) {
  wnba_shots <- NULL
  
  # Try to load the most recent season
  wnba_shots <- tryCatch({
    suppressWarnings({
      message(paste("Trying to load WNBA shot data for season:", first_season))
      load_wnba_pbp(seasons = first_season) %>%
        filter(shooting_play == TRUE,
               coordinate_x <= 47.5 & coordinate_x >= -47.5,
               coordinate_y <= 25 & coordinate_y >= -25)
    })
  }, error = function(e) {
    message(paste("WNBA shot data not available for season:", first_season))
    return(NULL)
  })
  
  # If the first season is NULL or has 0 rows, try the fallback season
  if (is.null(wnba_shots) || nrow(wnba_shots) == 0) {
    message(paste("Trying to load WNBA shot data for fallback season:", fallback_season))
    wnba_shots <- tryCatch({
      suppressWarnings({
        load_wnba_pbp(seasons = fallback_season) %>%
          filter(shooting_play == TRUE,
                 coordinate_x <= 47.5 & coordinate_x >= -47.5,
                 coordinate_y <= 25 & coordinate_y >= -25)
      })
    }, error = function(e) {
      message(paste("WNBA shot data not available for fallback season:", fallback_season))
      return(NULL)
    })
  }
  
  return(wnba_shots)
}

# Function to load WNBA player data with a fallback season
load_wnba_player_data_with_fallback <- function(first_season, fallback_season) {
  players <- NULL
  
  # Try to load player data for the most recent season
  players <- tryCatch({
    suppressWarnings({
      message(paste("Trying to load WNBA player data for season:", first_season))
      wehoop::load_wnba_player_box(seasons = first_season)
    })
  }, error = function(e) {
    message(paste("WNBA player data not available for season:", first_season))
    return(NULL)
  })
  
  # If the first season is NULL or has 0 rows, try the fallback season
  if (is.null(players) || nrow(players) == 0) {
    message(paste("Trying to load WNBA player data for fallback season:", fallback_season))
    players <- tryCatch({
      suppressWarnings({
        wehoop::load_wnba_player_box(seasons = fallback_season)
      })
    }, error = function(e) {
      message(paste("WNBA player data not available for fallback season:", fallback_season))
      return(NULL)
    })
  }
  
  return(players)
}

# Define the most recent season and a fallback season
first_season <- most_recent_wnba_season()
fallback_season <- first_season - 1  # Example fallback season: the previous year

# Load WNBA shot data with fallback mechanism
wnba_shots <- load_wnba_shots_with_fallback(first_season, fallback_season)

# Check if shot data was successfully loaded or not
if (!is.null(wnba_shots)) {
  # If data was loaded, process it as usual
  wnba_shots <- wnba_shots %>%
    mutate(
      team_name = if_else(team_id == home_team_id, home_team_name, away_team_name),
      team_mascot = if_else(team_id == home_team_id, home_team_mascot, away_team_mascot),
      team_name = paste0(team_name, " ", team_mascot),
      team_abbreviation = if_else(team_id == home_team_id, home_team_abbrev, away_team_abbrev),
      team_id = case_when(
        team_name == "Detroit Shock" ~ 7,
        team_name == "Tulsa Shock" ~ 10,
        team_name == "San Antonio Silver Stars" ~ 12,
        team_name == "San Antonio Stars" ~ 15,
        TRUE ~ team_id
      ),
      home_team_id = case_when(
        home_team_abbrev == "DET" & team_name == "Detroit Shock" ~ 7,
        home_team_abbrev == "TUL" & team_name == "Tulsa Shock" ~ 10,
        home_team_abbrev == "SAS" & team_name == "San Antonio Silver Stars" ~ 12,
        home_team_abbrev == "SA" & team_name == "San Antonio Stars" ~ 15,
        TRUE ~ home_team_id
      ),
      away_team_id = case_when(
        away_team_abbrev == "DET" & team_name == "Detroit Shock" ~ 7,
        away_team_abbrev == "TUL" & team_name == "Tulsa Shock" ~ 10,
        away_team_abbrev == "SAS" & team_name == "San Antonio Silver Stars" ~ 12,
        away_team_abbrev == "SA" & team_name == "San Antonio Stars" ~ 15,
        TRUE ~ away_team_id
      )
    )
  
  message("Shot data successfully processed for the most recent WNBA season.")
} else {
  message("No shot data processed for either the most recent or fallback WNBA season.")
}

# Creating Shot Type for wnba_shots to track attempts
type_shots <- wnba_shots %>%
  group_by(type_text,score_value) %>%
  distinct(type_text)

# Separate shots into home and away teams
home_wnba_shots <- wnba_shots %>%
  filter(team_id == home_team_id)

away_wnba_shots <- wnba_shots %>%
  filter(team_id == away_team_id)

# Finding obscure 3 point shot locations for away and home shots
obscure_3PA_away <- away_wnba_shots %>%
  filter(score_value == 3) %>%
  select(coordinate_x, coordinate_y, score_value) %>%
  group_by(coordinate_x, coordinate_y, score_value) %>%
  summarise(count = n()) %>%
  distinct()

obscure_3PA_home <- home_wnba_shots %>%
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

# Classify shots for away_wnba_shots
away_wnba_shots <- away_wnba_shots %>%
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
  ) %>%
  mutate(
    # Set coordinates for FTAs to the specified location
    coordinate_x = if_else(shot_type == "FTA", -28.00, coordinate_x),
    coordinate_y = if_else(shot_type == "FTA", 0, coordinate_y)
  )

# Classify shots for home_wnba_shots
home_wnba_shots <- home_wnba_shots %>%
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
  mutate(
    # Set coordinates for FTAs to the specified location
    coordinate_x = if_else(shot_type == "FTA", 28.00, coordinate_x),
    coordinate_y = if_else(shot_type == "FTA", 0, coordinate_y),
    coordinate_x = -coordinate_x,
    coordinate_y = -coordinate_y
  )

# Bind rows back together
wnba_shots <- bind_rows(home_wnba_shots, away_wnba_shots)

# Counting of each shot type
shot_count <- wnba_shots %>%
  group_by(shot_type) %>%
  summarise(count = n())

# Directory path for csv files (MAKE SURE TO CHANGE THE DIRECTORY ACCORDINGLY!!!)
conf_path <- "C:/Users/djvia/OneDrive/Documents/Blog Website/Basketball_Database/SHOT APP/WNBA"

if (!dir.exists(conf_path)) {
  dir.create(conf_path, recursive = TRUE)
}

wnba_conf <- read.csv(file.path(conf_path,"wnba_conf.csv"))

# Grabs the identity for every wnba team, will be helpful once we aggregate scoring
team_identity <- wnba_shots %>%
  select(team_id, team_name, team_abbreviation) %>%
  distinct() %>%
  filter(team_id >= 1 & team_id <= 20) %>%
  left_join(wnba_conf) %>%
  arrange(team_id)

# This identifies that there are minimal exceptions due to lack of data for specific
# scoring plays
team_w_NA <- wnba_shots %>%
  filter(is.na(team_id))

# Check for duplicates in the team_identity dataset
team_identity_duplicates <- team_identity %>%
  group_by(team_id) %>%
  filter(n() > 1)

# Remove duplicate rows based on join keys if any
team_identity <- team_identity %>%
  distinct(team_id, .keep_all = TRUE)

# Aggregating scoring value each team made
# Adding new columns for the count of actual made shots and shots attempted by team
wnba_season_team <- wnba_shots %>%
  group_by(season,team_id) %>%
  summarise(tot_scoring = sum(if_else(scoring_play == TRUE, score_value, 0), na.rm = TRUE),
            tot_scoring_no_ft = sum(if_else(scoring_play == TRUE & score_value %in% c(2, 3), score_value, 0), na.rm = TRUE),
            `2PM` = sum(score_value == 2 & scoring_play == TRUE, na.rm = TRUE),
            `2PA` = sum((score_value == 2 & scoring_play == TRUE) | (score_value == 0 & shot_type == "2PA"), na.rm = TRUE),
            `3PM` = sum(score_value == 3 & scoring_play == TRUE, na.rm = TRUE),
            `3PA` = sum((score_value == 3 & scoring_play == TRUE) | (score_value == 0 & shot_type == "3PA"), na.rm = TRUE),
            FTM = sum(score_value == 1 & scoring_play == TRUE, na.rm = TRUE),
            FTA = sum((score_value == 1 & scoring_play == TRUE) | (score_value == 0 & shot_type == "FTA"), na.rm = TRUE),
            shots_made = sum(`2PM` + `3PM`, na.rm = TRUE),
            shots_attempted = sum(`2PA` + `3PA`, na.rm = TRUE)) %>%
  right_join(team_identity)

# Load WNBA player data with fallback mechanism
players <- load_wnba_player_data_with_fallback(first_season, fallback_season)

# Check if player data was successfully loaded or not
if (!is.null(players)) {
  # If player data was loaded, process it as usual
  players <- players %>%
    select(athlete_id, athlete_display_name) %>%
    distinct()
  
  message("Player data successfully processed for the most recent WNBA season.")
} else {
  message("No player data processed for either the most recent or fallback WNBA season.")
}

# Check for duplicates in the players dataset
players_duplicates <- players %>%
  group_by(athlete_id) %>%
  filter(n() > 1)

# Remove duplicate rows based on join keys if any
players <- players %>%
  distinct(athlete_id, .keep_all = TRUE)

wnba_season_player <- wnba_shots %>%
  group_by(season,athlete_id_1) %>%
  rename(athlete_id = athlete_id_1) %>%
  summarise(tot_scoring = sum(if_else(scoring_play == TRUE, score_value, 0), na.rm = TRUE),
            tot_scoring_no_ft = sum(if_else(scoring_play == TRUE & score_value %in% c(2, 3), score_value, 0), na.rm = TRUE),
            `2PM` = sum(score_value == 2 & scoring_play == TRUE, na.rm = TRUE),
            `2PA` = sum((score_value == 2 & scoring_play == TRUE) | (score_value == 0 & shot_type == "2PA"), na.rm = TRUE),
            `3PM` = sum(score_value == 3 & scoring_play == TRUE, na.rm = TRUE),
            `3PA` = sum((score_value == 3 & scoring_play == TRUE) | (score_value == 0 & shot_type == "3PA"), na.rm = TRUE),
            FTM = sum(score_value == 1 & scoring_play == TRUE, na.rm = TRUE),
            FTA = sum((score_value == 1 & scoring_play == TRUE) | (score_value == 0 & shot_type == "FTA"), na.rm = TRUE),
            shots_made = sum(`2PM` + `3PM`, na.rm = TRUE),
            shots_attempted = sum(`2PA` + `3PA`, na.rm = TRUE)) %>%
  right_join(players)

# Acknowledging that there are some NA values
wnba_season_player_NA <- wnba_season_player %>%
  filter(is.na(season))

# Final Set for WNBA during 2014 season and after
recent_wnba_shots <- wnba_shots %>%
  select(id,season,season_type,team_id,athlete_id_1,coordinate_x,coordinate_y,shooting_play,
         scoring_play,score_value,shot_type) %>%
  rename(athlete_id = athlete_id_1) %>%
  right_join(players) %>%
  right_join(team_identity) %>%
  mutate(season_type = if_else(season_type == 3, "Postseason", "Regular Season"),
         League = "Professional",
         Gender = "Women",
         league_name = "WNBA") %>%
  rename(athlete_name = athlete_display_name) %>%
  na.omit()

# Make sure to change this directory path when making rda files
# (MAKE SURE TO CHANGE THE DIRECTORY ACCORDINGLY!!!)
dir_path <- "C:/Users/djvia/OneDrive/Documents/Blog Website/Basketball_Database/SHOT APP/RDA Files"

if (!dir.exists(dir_path)) {
  dir.create(dir_path, recursive = TRUE)
}

# Load in shot data and filter out most recent to add in newly recent data
load(file.path(dir_path,"WNBA_Shots.rda"))

wnba_shots <- wnba_shots %>%
  filter(season != most_recent_wnba_season() &
           season != max(wnba_shots$season))

wnba_shots <- bind_rows(wnba_shots,recent_wnba_shots) %>%
  arrange(desc(season))

# Write a rda of the final dataset
save(wnba_shots,file = file.path(dir_path,"WNBA_Shots.rda"))