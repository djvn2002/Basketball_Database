# Author: David Vialpando-Nielsen
# Date Made: 9/28/2024
# Latest Update: 9/28/2024

# This an update file to this RDA file: WNCAA_Shots.RDA

library(tidyverse)
library(wehoop)

# Directory path for csv files (MAKE SURE TO CHANGE THE DIRECTORY ACCORDINGLY!!!)
conf_path <- "C:/Users/djvia/OneDrive/Documents/Blog Website/Basketball_Database/SHOT APP/NCAA"

if (!dir.exists(conf_path)) {
  dir.create(conf_path, recursive = TRUE)
}

# Load the mncaa_conf data
wncaa_conf <- read.csv(file.path(conf_path,"wncaa_conf.csv"))

# Extract the team abbreviations
women_valid_teams_abbrev <- wncaa_conf %>%
  select(team_abbreviation) %>%
  distinct() %>%
  pull()

women_identity <- load_wbb_player_box(seasons = most_recent_wbb_season()) %>%
  mutate(team_abbreviation = if_else(team_id == 275 & team_abbreviation == "WISC",
                                     "WIS", team_abbreviation),
         team_abbreviation = if_else(team_id == 152 & team_abbreviation == "NCSU", 
                                     "NCST", team_abbreviation),
         team_abbreviation = if_else(team_id == 2429 & team_abbreviation == "CLT", 
                                     "CHAR", team_abbreviation),
         team_abbreviation = if_else(team_id == 292 & team_abbreviation == "UTRGV", 
                                     "RGV", team_abbreviation),
         team_abbreviation = if_else(team_id == 2241 & team_abbreviation == "WEBB", 
                                     "GWEB", team_abbreviation),
         team_abbreviation = if_else(team_id == 2908 & team_abbreviation == "SCUP",
                                     "UPST", team_abbreviation),
         team_abbreviation = if_else(team_id == 2097 & team_abbreviation == "CAM", 
                                     "CAMP", team_abbreviation),
         team_abbreviation = if_else(team_id == 2113 & team_abbreviation == "CENT", 
                                     "CENTLA", team_abbreviation),
         team_abbreviation = if_else(team_id == 311 & team_abbreviation == "MAINE", 
                                     "ME", team_abbreviation),
         team_display_name = if_else(team_id == 45,"George Washington Revolutionaries", 
                                     team_display_name),
         team_display_name = if_else(team_id == 113,"Massachusetts Minutewomen", 
                                     team_display_name),
         team_display_name = if_else(team_id == 2572,"Southern Miss Lady Eagles", 
                                     team_display_name),
         team_display_name = if_else(team_id == 2193, "East Tennessee State Buccaneers",
                                     team_display_name),
         team_display_name = if_else(team_id == 2110, "Central Arkansas Sugar Bears",
                                     team_display_name),
         team_display_name = if_else(team_id == 2113, "Centenary (LA) Ladies",
                                     team_display_name),
         team_display_name = if_else(team_id == 2900, "St. Thomas - Minnesota Tommies",
                                     team_display_name),
         team_display_name = if_else(team_id == 399, "Albany Great Danes",
                                     team_display_name),
         team_display_name = if_else(team_id == 2379, "Maryland-Eastern Shore Lady Hawks",
                                     team_display_name),
         team_display_name = if_else(team_id == 2169, "Delaware State Lady Hornets",
                                     team_display_name),
         team_display_name = if_else(team_id == 193, "Miami (OH) RedHawks",
                                     team_display_name)) %>%
  filter(team_abbreviation %in% women_valid_teams_abbrev)

# Identification for teams
women_teams <- women_identity %>%
  inner_join(wncaa_conf, relationship = "many-to-many") %>%
  filter((season >= join_season1 & (is.na(left_season1) | season < left_season1)) |
           (season >= join_season2 & (is.na(left_season2) | season < left_season2)) |
           (season >= join_season3 & (is.na(left_season3) | season < left_season3))) %>%
  select(team_id,team_display_name,team_abbreviation, season, conference) %>%
  distinct()

# Identification for players
women_players <- women_identity %>%
  inner_join(wncaa_conf, relationship = "many-to-many") %>%
  filter((season >= join_season1 & (is.na(left_season1) | season < left_season1)) |
           (season >= join_season2 & (is.na(left_season2) | season < left_season2)) |
           (season >= join_season3 & (is.na(left_season3) | season < left_season3))) %>%
  select(athlete_id, athlete_display_name) %>%
  distinct()

# Define the updated list of athlete_ids to remove
women_athlete_ids_to_remove <- c(3351418, 3093948, 3351793, 3166221, 3166222, 3166223,
                                 3952933, 3957692, 3961535, 3965773, 3965774, 3973906,
                                 3974224, 4073512, 4079645, 4086326, 4610002,
                                 3084921, 3085346, 4074483, 3346435, 3955581,
                                 4189461, 4297349, 4295601, 4297350, 4295602,
                                 4297351, 4295603, 3956346, 3281572, 3952930,
                                 3953366, 3952130, 4076016, 4074042, 4073463,
                                 4073464, 4708087, 4604639, 4604640, 3095497,
                                 4408657, 4402853, 4599261, 4706519, 3161520,
                                 4188258, 4188259, 4711716, 4710787, 4903088,
                                 5114801, 5113372, 4711750, 4711749)

# Define the updated list of athlete_display_names to remove
women_athlete_names_to_remove <- c("- 01", "- 22", "- 25", "- 32", "- 42", 
                                   "- 20", "Null", "Smith", "- 05", "- 35", 
                                   "- 99", "- Null", "Richards-Harris",
                                   "- Team", "- 02", "- 10", "- 23", 
                                   "- 97", "- 98", "- Julia Buehler", 
                                   "- Rag", "- Ragheiour", "- Team2", 
                                   "- Umkc", "- Utrgv", "2 2", "3 3")

# Filter out the unwanted entries from women_players
women_players <- women_players %>%
  filter(!athlete_id %in% women_athlete_ids_to_remove,
         !athlete_display_name %in% women_athlete_names_to_remove,
         athlete_display_name != "- ",
         athlete_id > 0)

# Find players that share ids
players_duplicates <- women_players %>%
  select(athlete_id,athlete_display_name) %>%
  group_by(athlete_id) %>%
  filter(n() > 1)

# Unique ids to players
women_players <- women_players %>%
  distinct(athlete_id, .keep_all = TRUE)

# Load in college from 2014 and onwards
womens_college <- load_wbb_pbp(seasons = most_recent_wbb_season()) %>%
  filter(shooting_play == T) %>%
  mutate(shot_type = ifelse(score_value == 1, "FTA",
                            ifelse(score_value == 2, "2PA", "3PA")),
         coordinate_x = ifelse(shot_type == "FTA", -28.00, coordinate_x),
         coordinate_y = ifelse(shot_type == "FTA", 0, coordinate_y),
         season_type = if_else(season_type == 3, "Postseason", "Regular Season")) %>%
  inner_join(women_teams %>% select(team_id, season), by = c("team_id", "season")) %>%
  select(id, season, season_type,team_id, athlete_id_1, coordinate_x, coordinate_y, shooting_play,
         scoring_play, score_value, shot_type)

# Player Aggregation
player_agg_womens <- womens_college %>%
  group_by(season,athlete_id_1) %>%
  rename(athlete_id = athlete_id_1) %>%
  summarise(tot_scoring = sum(if_else(scoring_play == TRUE, score_value, 0), na.rm = TRUE),
            tot_scoring_no_ft = sum(if_else(scoring_play == TRUE & score_value %in% c(2, 3), score_value, 0), na.rm = TRUE),
            `2PM` = sum(score_value == 2 & scoring_play == TRUE, na.rm = TRUE),
            `2PA` = sum(score_value == 2, na.rm = TRUE),
            `3PM` = sum(score_value == 3 & scoring_play == TRUE, na.rm = TRUE),
            `3PA` = sum(score_value == 3, na.rm = TRUE),
            `FTM` = sum(score_value == 1 & scoring_play == TRUE, na.rm = TRUE),
            `FTA` = sum(score_value == 1, na.rm = TRUE),
            shots_made = sum(`2PM` + `3PM`, na.rm = TRUE),
            shots_attempted = sum(`2PA` + `3PA`, na.rm = TRUE)) %>%
  right_join(women_players)

# Team Aggregation
team_agg_womens <- womens_college %>%
  group_by(season,team_id) %>%
  summarise(tot_scoring = sum(if_else(scoring_play == TRUE, score_value, 0), na.rm = TRUE),
            tot_scoring_no_ft = sum(if_else(scoring_play == TRUE & score_value %in% c(2, 3), score_value, 0), na.rm = TRUE),
            `2PM` = sum(score_value == 2 & scoring_play == TRUE, na.rm = TRUE),
            `2PA` = sum(score_value == 2, na.rm = TRUE),
            `3PM` = sum(score_value == 3 & scoring_play == TRUE, na.rm = TRUE),
            `3PA` = sum(score_value == 3, na.rm = TRUE),
            `FTM` = sum(score_value == 1 & scoring_play == TRUE, na.rm = TRUE),
            `FTA` = sum(score_value == 1, na.rm = TRUE),
            shots_made = sum(`2PM` + `3PM`, na.rm = TRUE),
            shots_attempted = sum(`2PA` + `3PA`, na.rm = TRUE)) %>%
  right_join(women_teams)

# Final Set for wncaa_shots
recent_wncaa_shots <- womens_college %>%
  rename(athlete_id = athlete_id_1) %>%
  filter(coordinate_x <= 47.5 & coordinate_x >= -47.5,
         coordinate_y <= 25 & coordinate_y >= -25) %>%
  mutate(coordinate_y = if_else(coordinate_x > 0, -coordinate_y, coordinate_y),
         coordinate_x = if_else(coordinate_x > 0, -coordinate_x, coordinate_x)) %>%
  right_join(women_players) %>%
  right_join(women_teams) %>%
  mutate(League = "College",
         Gender = "Women",
         league_name = "NCAA") %>%
  rename(athlete_name = athlete_display_name,
         team_name = team_display_name) %>%
  na.omit()

# Make sure to change this directory path when making csv files
# (MAKE SURE TO CHANGE THE DIRECTORY ACCORDINGLY!!!)
dir_path <- "C:/Users/djvia/OneDrive/Documents/Blog Website/Basketball_Database/SHOT APP/RDA Files"

if (!dir.exists(dir_path)) {
  dir.create(dir_path, recursive = TRUE)
}

# Load in shot data and filter out most recent to add in newly recent data
load(file.path(dir_path,"WNCAA_Shots.rda"))

wncaa_shots <- wncaa_shots %>%
  filter(season != most_recent_wbb_season())

wncaa_shots <- bind_rows(wncaa_shots,recent_wncaa_shots) %>%
  arrange(desc(season))

# Write a csv of the final dataset
save(wncaa_shots,file = file.path(dir_path,"WNCAA_Shots.rda"))

# To find duplicate seasons for troubleshooting
duplicates_in_women_teams <- women_teams %>%
  group_by(team_id, season) %>%
  filter(n() > 1)