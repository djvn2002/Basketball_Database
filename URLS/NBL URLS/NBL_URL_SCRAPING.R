# Author: David Vialpando-Nielsen
# Date Made: 9/18/2024
# Latest Update: 9/18/2024

# This file will be scraping for NBL URLs from Basketball-Reference.com including
# Each league season, every NBA team from 1938 to 1949 and every playoff team 
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

# League Urls
# This portion will grab valid urls that are based around each season of the ABA

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
