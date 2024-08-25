# Author: David Vialpando-Nielsen
# Date Made: 8/24/2024
# Latest Update: 8/24/2024

# This file will contain scrape code for player total traditional stats throughout NBA history

# Library and install necessary packages

library(tidyverse)
library(stringr)
library(lubridate)
library(readr)
library(rvest)

# Directory of where the rda file will reside in
player_fp <- "C:/Users/djvia/OneDrive/Documents/Blog Website/Basketball_Database/NBA/PLAYER/REGULAR SEASON/"

# Load the valid URLs from the CSV file
nba_urls <- read_csv("C:/Users/djvia/OneDrive/Documents/Blog Website/Basketball_Database/URLS/NBA URLS/NBA_TEAM_URLS.csv")

