# Author: David Vialpando-Nielsen
# Date Made: 9/18/2024
# Latest Update: 9/18/2024

# This file will be scraping for NBL URLs from Basketball-Reference.com including
# Each league season, every NBL team from 1938 to 1949 and every playoff team 
# from those seasons.

# NOTE: You will need to manually download 2 CSVs to run everything
# NBL_LEAGUE_INFO.csv & NBL_STANDINGS.csv
# These files are hard-written and will be updated on a weekly-basis during the season

# Install and library Packages - refer to 'PACKAGES' file for installation

library(tidyverse)
library(httr)

# Generate URLs function
generate_team_urls <- function(teams, seasons) {
  expand_grid(team = teams, season = seasons) %>%
    mutate(url = paste0("https://www.basketball-reference.com/nbl/teams/", team, "/", season, ".html"))
}

# List of teams
teams <- c('TCB','FWZ','MNN','DTG','SYN','RCR','ADP','DNV','INK','SHR','WTH',
           'AFN','AGW','BFB','CAG','CSF','CHB','CNC','CAT','CCB','CAS','DYM',
           'DYR','DTE','CWH','WRP','DVK','FDA','FWG','HCB','HCA','WCA','KGT',
           'OAS','PTS','PTR','TLJ','TJW','YNB')

# List of seasons
seasons <- 1938:1949

# Generate the URLs
team_urls <- generate_team_urls(teams, seasons)

# Read the league history data from the CSV file (Change the directory accordingly)
nbl_league_path <- "C:/Users/djvia/OneDrive/Documents/Blog Website/Basketball_Database/NBL/LEAGUE/"
league_his <- read_csv(file.path(nbl_league_path,"NBL_LEAGUE_INFO.csv"))

# Function to filter URLs based on the CSV data without checking for NA values
filter_valid_urls <- function(team, season, league_info) {
  team_info <- league_info %>% filter(Team == team)
  
  if (nrow(team_info) == 0) {
    return(FALSE)
  }
  
  # Check if the season falls within the 'From' and 'To' range for each row in team_info
  for (i in 1:nrow(team_info)) {
    from <- team_info$From[i]
    to <- team_info$To[i]
    
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
  mutate(League = "NBL") %>%
  left_join(league_his, by = c("team" = "Team", "League")) %>%
  rename(`Team Abbr.` = team,
         Season = season,
         URL = url) %>%
  select( `Team Name`,`Team Abbr.`,Season, League, URL)

# URL path to write CSVs (Change the directory accordingly)
url_path <- "C:/Users/djvia/OneDrive/Documents/Blog Website/Basketball_Database/URLS/NBL URLS/"
write_csv(valid_team_urls,file.path(url_path,"NBL_TEAM_URLS.csv"))

# League Urls
# This portion will grab valid urls that are based around each season of the NBL

# Function to generate URLs based on the league and year
generate_league_urls <- function(start_year, end_year) {
  years <- start_year:end_year
  urls <- tibble(
    Year = years,
    League = "NBL",
    URL = paste0("https://www.basketball-reference.com/nbl/seasons/",years,".html")
  )
  return(urls)
}

# Generate URLs from 1968 to 1976
nbl_league_urls <- generate_league_urls(1938, 1949)

# Creating a csv for league csvs
write_csv(nbl_league_urls,file.path(url_path,"NBL_LEAGUE_URLS.csv"))

# Playoff URLs
# This is separate from the NBL Team URLs as these URLs are exclusive to only teams that have'
# made the playoffs, we are going to take advantage of the valid_team_urls we made

# Read in standings csv
standings <- read_csv(file.path(nbl_league_path,"NBL_STANDINGS.csv")) %>%
  select(`Team Name`, `Team Abbr.`, Season, League, `Made Playoffs`) %>%
  filter(`Made Playoffs` == "Yes")

# Generate playoff URLs
generate_playoff_urls <- function(standings) {
  standings %>%
    mutate(URL = paste0("https://www.basketball-reference.com/nbl/teams/", `Team Abbr.`, "/", Season, ".html"))
}

# Get the playoff URLs
playoff_urls <- generate_playoff_urls(standings) %>%
  select(-`Made Playoffs`)

# Creating a csv for all playoff teams
write_csv(playoff_urls,file.path(url_path,"NBL_PLAYOFFS_URLS.csv"))

# Now you are able to scrape nba data from Basketball-Reference.com