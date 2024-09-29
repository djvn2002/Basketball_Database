# Author: David Vialpando-Nielsen
# Date Made: 9/28/2024
# Latest Update: 9/28/2024

# This an update file to this RDA file: MNCAA_Shots.RDA

library(tidyverse)
library(hoopR)

# Directory path for csv files (MAKE SURE TO CHANGE THE DIRECTORY ACCORDINGLY!!!)
conf_path <- "C:/Users/djvia/OneDrive/Documents/Blog Website/Basketball_Database/SHOT APP/NCAA"

if (!dir.exists(conf_path)) {
  dir.create(conf_path, recursive = TRUE)
}

# Load the mncaa_conf data
mncaa_conf <- read.csv(file.path(conf_path,"mncaa_conf.csv"))

# Extract the team abbreviations
men_valid_teams_abbrev <- mncaa_conf %>%
  select(team_abbreviation) %>%
  distinct() %>%
  pull()

# Loading data to establish ids to teams and players
men_identity <- load_mbb_player_box(seasons = most_recent_mbb_season()) %>%
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
         team_abbreviation = if_else(team_id == 2506 & team_abbreviation == "PRES", 
                                     "PRE", team_abbreviation),
         team_abbreviation = if_else(team_id == 2272 & team_abbreviation == "HP", 
                                     "HPU", team_abbreviation),
         team_abbreviation = if_else(team_id == 2097 & team_abbreviation == "CAM", 
                                     "CAMP", team_abbreviation),
         team_abbreviation = if_else(team_id == 2113 & team_abbreviation == "CENT", 
                                     "CENTLA", team_abbreviation),
         team_abbreviation = if_else(team_id == 2571 & team_abbreviation == "SDSU", 
                                     "SDST", team_abbreviation),
         team_abbreviation = if_else(team_id == 2597 & team_abbreviation == "SFBK", 
                                     "SFNY", team_abbreviation),
         team_display_name = if_else(team_id == 45 & season <= 2022, 
                                     "George Washington Revolutionaries", 
                                     team_display_name),
         team_display_name = if_else(team_id == 113 & season <= 2022, 
                                     "Massachusetts Minutemen", 
                                     team_display_name),
         team_display_name = if_else(team_id == 2110,
                                     "Central Arkansas Sugar Bears",
                                     team_display_name),
         team_display_name = if_else(team_id == 2113, 
                                     "Centenary (LA) Gentlemen",
                                     team_display_name),
         team_display_name = if_else(team_id == 2900, 
                                     "St. Thomas - Minnesota Tommies",
                                     team_display_name),
         team_display_name = if_else(team_id == 399, 
                                     "Albany Great Danes",
                                     team_display_name),
         team_display_name = if_else(team_id == 2597, 
                                     "St. Francis Brooklyn Terriers",
                                     team_display_name),
         team_display_name = if_else(team_id == 193, 
                                     "Miami (OH) RedHawks",
                                     team_display_name)) %>%
  filter(team_abbreviation %in% men_valid_teams_abbrev)

# Identification for teams
men_teams <- men_identity %>%
  inner_join(mncaa_conf, relationship = "many-to-many") %>%
  filter((season >= join_season1 & (is.na(left_season1) | season < left_season1)) |
           (season >= join_season2 & (is.na(left_season2) | season < left_season2)) |
           (season >= join_season3 & (is.na(left_season3) | season < left_season3))) %>%
  select(team_id,team_display_name,team_abbreviation, season, conference) %>%
  distinct()

# Identification for players
men_players <- men_identity %>%
  inner_join(mncaa_conf, relationship = "many-to-many") %>%
  filter((season >= join_season1 & (is.na(left_season1) | season < left_season1)) |
           (season >= join_season2 & (is.na(left_season2) | season < left_season2)) |
           (season >= join_season3 & (is.na(left_season3) | season < left_season3))) %>%
  select(athlete_id, athlete_display_name) %>%
  distinct()

# Define the updated list of athlete_ids to remove
men_athlete_ids_to_remove <- c(3081466, 3080113, 3081464, 3081465, 3081467, 3527494, 
                               3166110, 3166109, 3164695, 3960373, 3960372, 4081684, 
                               4076030, 4076031, 3173728, 3082157, 3081681, 3080660, 
                               3079623, 3165439, 4073511, 4295628, 4295629, 4304077, 
                               4297358, 4295630, 4596605, 3968536, 4083841, 3165538, 
                               3390447, 3165462, 3163531, 3161467, 3158826, 3965654, 
                               3965578, 3963158, 3961918, 3961493, 3961405, 3961404, 
                               3960731, 3960474, 3960355, 3954616, 3953640, 3952568, 
                               3953058, 3951589, 3951555, 4165845, 4107665, 4087503, 
                               4082123, 4080624, 4080542, 4080211, 4079699, 4076579, 
                               4418432, 4403373, 4405523, 4599818, 4598015, 4708512, 
                               4706536, 3957792, 4903943, 5113375, 5113398, 4917714, 
                               4911096, 4714790, 4710047)

# Define the updated list of athlete_display_names to remove
men_athlete_names_to_remove <- c("- 01", "- 22", "- 25", "- 32", "- 42", 
                                 "- 20", "Null", "Smith", "- Team", "- 21", 
                                 "- 23", "- 97", "- 98", "- 99", "- Fitzpatrick-Dorsey", 
                                 "- Laross", "- Lightfoot", "- Player", "- Unknown", 
                                 " X", "3 3")

# Filter out the unwanted entries
men_players <- men_players %>%
  filter(!athlete_id %in% men_athlete_ids_to_remove,
         !athlete_display_name %in% men_athlete_names_to_remove,
         athlete_display_name != "- ",
         athlete_id > 0)

# Find players that share ids
players_duplicates <- men_players %>%
  select(athlete_id,athlete_display_name) %>%
  group_by(athlete_id) %>%
  filter(n() > 1)

# Unique ids to players
men_players <- men_players %>%
  distinct(athlete_id, .keep_all = TRUE)

# Load in college from 2014 and onwards
mens_college <- load_mbb_pbp(seasons = most_recent_mbb_season()) %>%
  filter(shooting_play == T) %>%
  mutate(shot_type = ifelse(score_value == 1, "FTA",
                            ifelse(score_value == 2, "2PA", "3PA")),
         coordinate_x = ifelse(shot_type == "FTA", -28.00, coordinate_x),
         coordinate_y = ifelse(shot_type == "FTA", 0, coordinate_y),
         season_type = if_else(season_type == 3, "Postseason", "Regular Season")) %>%
  inner_join(men_teams %>% select(team_id, season), by = c("team_id", "season")) %>%
  select(id, season, season_type, team_id, athlete_id_1, coordinate_x, coordinate_y, shooting_play,
         scoring_play, score_value, shot_type)

# Player Aggregation
player_agg_mens <- mens_college %>%
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
  right_join(men_players)

# Team Aggregation
team_agg_mens <- mens_college %>%
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
  right_join(men_teams)

# Final Set for mncaa_shots
recent_mncaa_shots <- mens_college %>%
  rename(athlete_id = athlete_id_1) %>%
  filter(coordinate_x <= 47.5 & coordinate_x >= -47.5,
         coordinate_y <= 25 & coordinate_y >= -25) %>%
  mutate(coordinate_y = if_else(coordinate_x > 0, -coordinate_y, coordinate_y),
         coordinate_x = if_else(coordinate_x > 0, -coordinate_x, coordinate_x)) %>%
  right_join(men_players) %>%
  right_join(men_teams) %>%
  mutate(League = "College",
         Gender = "Men",
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
load(file.path(dir_path,"MNCAA_Shots.rda"))

mncaa_shots <- mncaa_shots %>%
  filter(season != most_recent_mbb_season())

mncaa_shots <- bind_rows(mncaa_shots,recent_mncaa_shots) %>%
  arrange(desc(season))

# Write a rda of the final dataset
save(mncaa_shots,file = file.path(dir_path,"MNCAA_Shots.rda"))

# To find duplicate seasons for troubleshooting
duplicates_in_men_teams <- men_teams %>%
  group_by(team_id, season) %>%
  filter(n() > 1)