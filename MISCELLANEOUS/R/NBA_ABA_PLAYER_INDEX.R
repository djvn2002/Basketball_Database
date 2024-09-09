# Author: David Vialpando-Nielsen
# Date Made: 9/5/2024
# Latest Update: 9/7/2024

# The usage of this file is to pinpoint players in the NBA and ABA with unqiue indexes

# Load necessary libraries
library(rvest)
library(purrr)
library(stringr)
library(lubridate)
library(tidyverse)
library(hoopR)

# Base URL structure
base_url <- "https://www.basketball-reference.com/players/"

# Function to scrape the table from a single letter page
scrape_player_table <- function(letter) {
  # Construct the URL
  url <- paste0(base_url, letter, "/")
  
  # Read the webpage
  webpage <- read_html(url)
  
  # Extract table rows for each player
  players_table <- webpage %>%
    html_nodes("#div_players") %>%
    html_table(fill = TRUE)
  
  # The table is in list format, we extract the first item
  players_df <- players_table[[1]]
  
  # Ensure only the relevant columns are character type
  players_df <- players_df %>%
    mutate(Player = as.character(Player),
           Pos = as.character(Pos),
           Ht = as.character(Ht),
           `Birth Date` = as.character(`Birth Date`),
           Colleges = as.character(Colleges))
  
  # Add the letter scraped as a column for tracking (optional)
  players_df <- players_df %>%
    mutate(Letter = letter)
  
  return(players_df)
}

# Letters of the alphabet (A-Z)
letters <- letters

# Scrape player tables for each letter and combine into one data frame
all_players_df <- map_df(letters, scrape_player_table)

# Create HOF column
all_players_df <- all_players_df %>%
  mutate(HOF = if_else(str_detect(Player,"\\*"), "Yes", "No"))

# Removing asterisks and adding NA to empty rows in Colleges
all_players_df <- all_players_df %>%
  mutate(Player = str_replace(Player,"\\*","")) %>%
  mutate(Colleges = na_if(Colleges,"")) %>%
  mutate(`Birth Date` = na_if(`Birth Date`,""))

# Add Luca Vildoza - because he is a very special occasion
all_players_df <- all_players_df %>%
  add_row(
    Player = "Luca Vildoza",
    From = 2022,  # Year Luca Vildoza entered the NBA
    To = 2022,  # Current season
    Pos = "G",  # Position: Guard
    Ht = "6-3",  # Height
    Wt = 190,  # Weight (adjust as needed)
    `Birth Date` = "August 11, 1995",  # Birth Date
    Colleges = NA,  # Colleges information (if applicable, otherwise NA)
    HOF = "No",  # Hall of Fame status
  )

# Resorting player df and applying an ID
all_players_df <- all_players_df %>%
  arrange(as.numeric(From, Player)) %>%
  mutate(`Player ID` = 1000 + row_number()) %>%
  mutate(Active = if_else(To == most_recent_nba_season(), "Yes", "No")) %>%
  select(`Player ID`, everything(), -Letter)

# View the first few rows of the combined dataframe
print(head(all_players_df))

# Print to csv for later use
write_csv(all_players_df,"C:/Users/djvia/OneDrive/Documents/Blog Website/Basketball_Database/MISCELLANEOUS/NBA_ABA_PLAYER_INDEX.csv")
