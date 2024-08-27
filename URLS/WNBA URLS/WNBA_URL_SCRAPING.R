# Author: David Vialpando-Nielsen
# Date Made: 8/26/2024
# Latest Update: 8/26/2024

# This file will be scraping for WNBA URLs from Basketball-Reference.com including
# Each league season, every WNBA team from the inaugural 1997 season to today,
# And every playoff team from 1997 to today.

# NOTE: You will need to manually download 2 CSVs to run everything
# WNBA_LEAGUE_INFO.csv & WNBA_STANDINGS.csv
# These files are hard-written and will be updated on a weekly-basis during the season

# Install and library Packages - refer to 'PACKAGES' file for installation

library(tidyverse)
library(httr)
library(wehoop)

# Generate URLs
generate_team_urls <- function(teams, seasons) {
  expand_grid(team = teams, season = seasons) %>%
    mutate(url = paste0("https://www.basketball-reference.com/wnba/teams/", team, "/", season, ".html"))
}

# List of teams and seasons with updated team names
teams <- c('ATL','CHI','CON','ORL','IND','LAS','MIN','NYL','PHO','LVA','SAS',
           'UTA','SEA','DAL','TUL','DET','WAS','CHA','CLE','HOU','MIA','POR',
           'SAC')

seasons <- 1997:most_recent_wnba_season()  # Adjusted range of seasons

# Generate the URLs
team_urls <- generate_team_urls(teams, seasons)

# Read the league history data from the CSV file (Change the directory accordingly)
wnba_league_path <- "C:/Users/djvia/OneDrive/Documents/Blog Website/Basketball_Database/WNBA/LEAGUE/"
league_his <- read_csv(file.path(wnba_league_path,"WNBA_LEAGUE_INFO.csv"))

# Function to filter league_his for the relevant row based on the season
get_relevant_team_info <- function(team, season, league_info) {
  league_info %>%
    filter(Team == team) %>%
    mutate(To = ifelse(is.na(To), most_recent_wnba_season(), To)) %>%
    filter(season >= From & season <= To) %>%
    slice(1)  # Take the first matching row, should only be one
}

# Apply the function to each row in team_urls
team_urls <- team_urls %>%
  rowwise() %>%
  mutate(
    team_info = list(get_relevant_team_info(team, season, league_his))
  ) %>%
  unnest(cols = c(team_info))

# Rename and select relevant columns
valid_team_urls <- team_urls %>%
  rename(`Team Abbr.` = team,
         Season = season,
         URL = url) %>%
  select(`Team Name`, `Team Abbr.`, Season, League, URL)

# URL path to write CSVs (Change the directory accordingly)
url_path <- "C:/Users/djvia/OneDrive/Documents/Blog Website/Basketball_Database/URLS/WNBA URLS/"
write_csv(valid_team_urls, file.path(url_path,"WNBA_TEAM_URLS.csv"))

# League Urls
# This portion will grab valid urls that are based around each season of the WNBA

# Function to generate URLs based on the league and year
generate_league_urls <- function(start_year, end_year) {
  years <- start_year:end_year
  urls <- tibble(
    Year = years,
    League = "wnba",
    URL = paste0("https://www.basketball-reference.com/",League,"/years/", years, ".html")
  )
  return(urls)
}

# Generate URLs from 1997 to present day
wnba_league_urls <- generate_league_urls(1997, most_recent_wnba_season()) %>%
  mutate(League = "WNBA")

# Creating a csv for league csvs
write_csv(wnba_league_urls,file.path(url_path,"WNBA_LEAGUE_URLS.csv"))

# Playoff URLs
# This is separate from the WNBA Team URLs as these URLs are exclusive to only teams that have'
# made the playoffs, we are going to take advantage of the valid_team_urls we made

# Read in standings csv
standings <- read_csv(file.path(wnba_league_path,"WNBA_STANDINGS.csv")) %>%
  select(`Team Name`, `Team Abbr.`, Season, League, `Made Playoffs`) %>%
  filter(`Made Playoffs` == "Yes")

# Generate playoff URLs
generate_playoff_urls <- function(standings) {
  standings %>%
    mutate(URL = paste0("https://www.basketball-reference.com/wnba/teams/",`Team Abbr.`, "/", Season, ".html"))
}

# Get the playoff URLs
playoff_urls <- generate_playoff_urls(standings) %>%
  select(-`Made Playoffs`)

# Creating a csv for all playoff teams
write_csv(playoff_urls,file.path(url_path,"WNBA_PLAYOFFS_URLS.csv"))

# Now you are able to scrape wnba data from Basketball-Reference.com