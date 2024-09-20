# Author: David Vialpando-Nielsen
# Date Made: 9/19/2024
# Latest Update: 9/19/2024

# This file will be scraping for G LEAGUE URLs from Basketball-Reference.com including
# Each league season, every NBA team from the inaugural 2002 season to today,
# And every playoff team from 2002 to today.

# NOTE: You will need to manually download 2 CSVs to run everything
# G_LEAGUE_LEAGUE_INFO.csv & G_LEAGUE_STANDINGS.csv
# These files are hard-written and will be updated on a weekly-basis during the season

# Install and library Packages - refer to 'PACKAGES' file for installation

library(tidyverse)
library(httr)
library(hoopR)

# Generate URLs function
generate_team_urls <- function(teams, seasons) {
  expand_grid(team = teams, season = seasons) %>%
    mutate(url = paste0("https://www.basketball-reference.com/gleague/teams/", team, "/", season, ".html"))
}

# List of teams
teams <- c('AUS', 'AST', 'CLB', 'BIR', 'EBH', 'CCG', 'MXC', 'CLC', 'CAN', 'NMX', 
           'ABQ', 'HUN', 'CPS', 'HAW', 'DEL', 'DLW', 'UTA', 'GLI', 'GRG', 'GRR', 
           'SPR', 'ANA', 'GBO', 'IND', 'FWN', 'IWA', 'IWE', 'LIN', 'MNE', 'MAI', 
           'MHU', 'MCC', 'NAS', 'BAK', 'OKL', 'TUL', 'ASH', 'ONT', 'ACC', 'OSC', 
           'LAK', 'ERI', 'RAP', 'RGV', 'RIP', 'SLC', 'IDA', 'SCW', 'DAK', 'SXF', 
           'SBL', 'LOS', 'STO', 'REN', 'TEX', 'COL', 'WES', 'WCB', 'WIS', 'ARK', 
           'FAY', 'FLO', 'CHS', 'NCH', 'FTW', 'GRE', 'MOB', 'ROA')

# List of seasons
seasons <- 2002:most_recent_nba_season()

# Generate the URLs
team_urls <- generate_team_urls(teams, seasons)

# Read the league history data from the CSV file (Change the directory accordingly)
gleague_league_path <- "C:/Users/djvia/OneDrive/Documents/Blog Website/Basketball_Database/G LEAGUE/LEAGUE/"
league_his <- read_csv(file.path(gleague_league_path,"G_LEAGUE_LEAGUE_INFO.csv"))

# Function to filter URLs based on the CSV data
filter_valid_urls <- function(team, season, league_info) {
  team_info <- league_info %>% filter(Team == team)
  
  if (nrow(team_info) == 0) {
    return(FALSE)
  }
  
  # If the team has multiple rows (e.g., different periods), check all of them
  for (i in 1:nrow(team_info)) {
    from <- team_info$From[i]
    to <- ifelse(is.na(team_info$To[i]), most_recent_nba_season(), team_info$To[i])
    
    if (season >= from & season <= to) {
      return(TRUE)
    }
  }
  
  return(FALSE)
}

# Filter valid URLs, add the League column, and join with team information
valid_team_urls <- team_urls %>%
  rowwise() %>%
  filter(filter_valid_urls(team, season, league_his)) %>%
  mutate(League = "G League") %>%
  left_join(league_his, by = c("team" = "Team", "League")) %>%
  rename(`Team Abbr.` = team,
         Season = season,
         URL = url) %>%
  select(`Team Name`,`Team Abbr.`,Season, League, URL)

# URL path to write CSVs (Change the directory accordingly)
url_path <- "C:/Users/djvia/OneDrive/Documents/Blog Website/Basketball_Database/URLS/G LEAGUE URLS/"
write_csv(valid_team_urls,file.path(url_path,"G_LEAGUE_TEAM_URLS.csv"))

# League Urls
# This portion will grab valid urls that are based around each season of the NBA

# Function to generate URLs based on the league and year
generate_league_urls <- function(start_year, end_year) {
  years <- start_year:end_year
  urls <- tibble(
    Year = years,
    League = "G League",
    URL = paste0("https://www.basketball-reference.com/gleague/years/", years, ".html")
  )
  return(urls)
}

# Generate URLs from 1947 to 2024
g_league_urls <- generate_league_urls(2002, most_recent_nba_season())

# Creating a csv for league csvs
write_csv(nba_league_urls,file.path(url_path,"G_LEAGUE_LEAGUE_URLS.csv"))

# Playoff URLs
# This is separate from the G League Team URLs as these URLs are exclusive to only teams that have'
# made the playoffs, we are going to take advantage of the valid_team_urls we made

# Read in standings csv
standings <- read_csv(file.path(gleague_league_path,"G_LEAGUE_STANDINGS.csv")) %>%
  select(`Team Name`, `Team Abbr.`, Season, League, `Made Playoffs`) %>%
  filter(`Made Playoffs` == "Yes")

# Generate playoff URLs
generate_playoff_urls <- function(standings) {
  standings %>%
    mutate(URL = paste0("https://www.basketball-reference.com/gleague/teams/", `Team Abbr.`, "/", Season, ".html"))
}

# Get the playoff URLs
playoff_urls <- generate_playoff_urls(standings) %>%
  select(-`Made Playoffs`)

# Creating a csv for all playoff teams
write_csv(playoff_urls,file.path(url_path,"G_LEAGUE_PLAYOFFS_URLS.csv"))

# Now you are able to scrape nba data from Basketball-Reference.com