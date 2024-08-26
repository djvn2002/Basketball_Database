# Author: David Vialpando-Nielsen
# Date Made: 8/25/2024
# Latest Update: 8/25/2024

# The purpose of this file is to create conference data frames for college basketball data
# Serves as a reference for updating the csv for men's and women's NCAA conferences

library(tidyverse)
library(hoopR)
library(wehoop)

men_reference <- load_mbb_team_box(seasons = 2014:most_recent_mbb_season()) %>%
  select(team_id, team_display_name, team_abbreviation) %>%
  distinct()

women_reference <- load_wbb_team_box(seasons = 2014:most_recent_wbb_season()) %>%
  select(team_id, team_display_name, team_abbreviation) %>%
  distinct()

# Directory path for csv files (MAKE SURE TO CHANGE THE DIRECTORY ACCORDINGLY!!!)
conf_path <- "C:/Users/djvia/OneDrive/Documents/Blog Website/Basketball_Database/SHOT APP/NCAA"

if (!dir.exists(conf_path)) {
  dir.create(conf_path, recursive = TRUE)
}

# Load the mncaa_conf and wncaa_conf data
wncaa_conf <- read.csv(file.path(conf_path,"wncaa_conf.csv"))
mncaa_conf <- read.csv(file.path(conf_path,"mncaa_conf.csv"))

mvt <- mncaa_conf %>%
  select(team_display_name,team_abbreviation) %>%
  distinct() %>%
  arrange(team_abbreviation)

wvt <- wncaa_conf %>%
  select(team_display_name,team_abbreviation) %>%
  distinct() %>%
  arrange(team_abbreviation)

mvt_missing <- mvt %>%
  anti_join(men_reference, by = c("team_abbreviation","team_display_name"))

wvt_missing <- wvt %>%
  anti_join(women_reference, by = c("team_abbreviation","team_display_name"))
