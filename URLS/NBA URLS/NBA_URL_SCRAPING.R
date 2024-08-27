# Author: David Vialpando-Nielsen
# Date Made: 8/23/2024
# Latest Update: 8/24/2024

# This file will be scraping for NBA URLs from Basketball-Reference.com including
  # Each league season, every NBA team from the inaugural 1947 season to today,
  # And every playoff team from 1947 to today.

# NOTE: You will need to manually download 2 CSVs to run everything
  # NBA_LEAGUE_INFO.csv & NBA_STANDINGS.csv
  # These files are hard-written and will be updated on a weekly-basis during the season

# Install and library Packages - refer to 'PACKAGES' file for installation

library(tidyverse)
library(httr)
library(hoopR)

# Generate URLs function
generate_team_urls <- function(teams, seasons) {
  expand_grid(team = teams, season = seasons) %>%
    mutate(url = paste0("https://www.basketball-reference.com/teams/", team, "/", season, ".html"))
}

# List of teams
teams <- c('AND','ATL','BAL','BLB','BOS','BRK','BUF','CAP','CHA','CHH','CHI',
           'CHO','CHP','CHS','CHZ','CIN','CLE','CLR','DAL','DEN','DET','DNN',
           'DTF','FTW','GSW','HOU','IND','INJ','INO','KCK','KCO','LAC','LAL',
           'MEM','MIA','MIL','MIN','MLH','MNL','NJN','NOH','NOJ','NOK','NOP',
           'NYK','NYN','OKC','ORL','PHI','PHO','PHW','PIT','POR','PRO','ROC',
           'SAC','SAS','SDC','SDR','SEA','SFW','SHE','STB','STL','SYR','TOR',
           'TRH','TRI','UTA','VAN','WAS','WAT','WSB','WSC')

# List of seasons
seasons <- 1947:most_recent_nba_season()

# Generate the URLs
team_urls <- generate_team_urls(teams, seasons)

# Read the league history data from the CSV file (Change the directory accordingly)
nba_league_path <- "C:/Users/djvia/OneDrive/Documents/Blog Website/Basketball_Database/NBA/LEAGUE/"
league_his <- read_csv(file.path(nba_league_path,"NBA_LEAGUE_INFO.csv"))

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
  mutate(League = ifelse(season < 1950, "BAA", "NBA")) %>%
  left_join(league_his, by = c("team" = "Team", "League")) %>%
  rename(`Team Abbr.` = team,
         Season = season,
         URL = url) %>%
  select( `Team Name`,`Team Abbr.`,Season, League, URL) %>%
  filter(!(`Team Abbr.` == "NOH" & Season %in% c(2006, 2007)))

# URL path to write CSVs (Change the directory accordingly)
url_path <- "C:/Users/djvia/OneDrive/Documents/Blog Website/Basketball_Database/URLS/NBA URLS/"
write_csv(valid_team_urls,file.path(url_path,"NBA_TEAM_URLS.csv"))

# League Urls
# This portion will grab valid urls that are based around each season of the NBA

# Function to generate URLs based on the league and year
generate_league_urls <- function(start_year, end_year) {
  years <- start_year:end_year
  urls <- tibble(
    Year = years,
    League = ifelse(years < 1950, "BAA", "NBA"),
    URL = paste0("https://www.basketball-reference.com/leagues/", ifelse(years < 1950, "BAA", "NBA"), "_", years, ".html")
  )
  return(urls)
}

# Generate URLs from 1947 to 2024
nba_league_urls <- generate_league_urls(1947, 2024)

# Creating a csv for league csvs
write_csv(nba_league_urls,file.path(url_path,"NBA_LEAGUE_URLS.csv"))

# Playoff URLs
# This is separate from the NBA Team URLs as these URLs are exclusive to only teams that have'
# made the playoffs, we are going to take advantage of the valid_team_urls we made

# Read in standings csv
standings <- read_csv(file.path(nba_league_path,"NBA_STANDINGS.csv")) %>%
  select(`Team Name`, `Team Abbr.`, Season, League, `Made Playoffs`) %>%
  filter(`Made Playoffs` == "Yes")

# Generate playoff URLs
generate_playoff_urls <- function(standings) {
  standings %>%
    mutate(URL = paste0("https://www.basketball-reference.com/teams/", `Team Abbr.`, "/", Season, ".html"))
}

# Get the playoff URLs
playoff_urls <- generate_playoff_urls(standings) %>%
  select(-`Made Playoffs`)

# Creating a csv for all playoff teams
write_csv(playoff_urls,file.path(url_path,"NBA_PLAYOFFS_URLS.csv"))

# Now you are able to scrape nba data from Basketball-Reference.com