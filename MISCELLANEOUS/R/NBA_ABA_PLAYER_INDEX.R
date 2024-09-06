# Load necessary libraries
library(rvest)
library(purrr)
library(stringr)
library(lubridate)
library(tidyverse)

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

# Resorting player df and applying an ID
all_players_df <- all_players_df %>%
  arrange(as.numeric(From, Player)) %>%
  mutate(`Player ID` = 1000 + row_number()) %>%
  select(`Player ID`, everything(), -Letter)

# View the first few rows of the combined dataframe
print(head(all_players_df))

# Print to csv for later use
write_csv(all_players_df,"C:/Users/djvia/OneDrive/Documents/Blog Website/Basketball_Database/MISCELLANEOUS/NBA_ABA_PLAYER_INDEX.csv")
