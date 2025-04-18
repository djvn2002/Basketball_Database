# Author: David Vialpando-Nielsen
# Date Made: 9/12/2024
# Latest Update: 9/13/2024

# This file will contain scrape code for player stats based by shooting statistics

# Library and install necessary packages

library(furrr)
library(RSelenium)
library(tidyverse)
library(netstat)
library(wdman)
library(stringr)
library(lubridate)
library(readr)
library(rvest)
library(progressr)

# Directory for the saved CSV file
player_fp <- "C:/Users/djvia/OneDrive/Documents/Blog Website/Basketball_Database/NBA/PLAYER/REGULAR SEASON/"

# Load the valid URLs from the CSV file
nba_urls <- read_csv("C:/Users/djvia/OneDrive/Documents/Blog Website/Basketball_Database/URLS/NBA URLS/NBA_TEAM_URLS.csv") %>%
  filter(Season >= 1997)

# Function to clean column names and ensure specific column types are consistent
clean_colnames <- function(df) {
  colnames(df) <- ifelse(colnames(df) == "" | is.na(colnames(df)), paste0("V", seq_along(colnames(df))), colnames(df))
  
  # Convert all columns to character to avoid type mismatches
  df <- df %>% mutate(across(everything(), as.character))
  
  return(df)
}

# Function to clear cache and cookies
clear_cache_and_cookies <- function(remDr) {
  remDr$deleteAllCookies()  # Clear cookies
  remDr$executeScript("window.localStorage.clear();")  # Clear local storage
  remDr$executeScript("window.sessionStorage.clear();")  # Clear session storage
}

# Function to scrape the specific table using RSelenium
scrape_table_selenium <- function(remDr, url, max_retries = 3) {
  for (i in 1:max_retries) {
    tryCatch({
      # Navigate to the URL
      remDr$navigate(url)
      
      # Wait for the table to appear
      Sys.sleep(3)  # Wait for the page to load
      
      # Find the table element
      webElem <- remDr$findElement(using = "css", "#div_shooting")
      table_html <- webElem$getElementAttribute("outerHTML")[[1]]
      
      # Parse the HTML table using rvest
      table <- read_html(table_html) %>%
        html_table(fill = TRUE) %>%
        as.data.frame()
      
      # Clean column names and ensure specific column types are consistent
      table <- clean_colnames(table)
      
      # Add additional metadata
      season <- str_extract(url, "\\d{4}") %>% as.numeric()
      season_formatted <- paste(season - 1, season, sep = "-")
      team <- str_extract(url, "(?<=teams/)[A-Z]{3}")
      table <- table %>%
        mutate(URL = url, Team = team, Season = season_formatted)
      
      return(table)
      
    }, error = function(e) {
      message(paste("Error scraping URL:", url, "-", e$message))
      
      # Retry mechanism
      if (i == max_retries) {
        return(NULL)  # After retries, return NULL if it keeps failing
      }
      
      # Wait before retrying
      Sys.sleep(5)
    })
  }
}

# Function to restart the Selenium driver
restart_selenium <- function() {
  remDr$close()
  rs_driver_object$server$stop()
  Sys.sleep(3)  # Allow the system to close processes
  rs_driver_object <<- rsDriver(browser = 'chrome', extraCapabilities = eCaps, verbose = FALSE, port = free_port())
  remDr <<- rs_driver_object$client
}

# Function to scrape all URLs in batches, clear cache/cookies periodically, and restart Selenium
scrape_all_urls_selenium <- function(nba_urls, remDr, batch_size = 100, restart_every = 500) {
  results <- list()
  
  # Initialize the progress bar
  p <- progressr::progressor(steps = nrow(nba_urls))
  
  # Scrape each URL one at a time in batches
  for (i in seq(1, nrow(nba_urls), by = batch_size)) {
    batch_urls <- nba_urls$URL[i:min(i + batch_size - 1, nrow(nba_urls))]
    
    for (url in batch_urls) {
      p(message = paste("Scraping URL:", url))  # Update the progress bar
      
      table <- scrape_table_selenium(remDr, url)
      
      if (!is.null(table)) {
        results <- bind_rows(results, table)
      }
    }
    
    # Clear cache and cookies periodically
    clear_cache_and_cookies(remDr)
    
    # Restart Selenium periodically
    if (i %% restart_every == 0) {
      restart_selenium()
    }
    
    Sys.sleep(2)  # Small delay to ease memory usage
  }
  
  # Save the final result after scraping all URLs
  write_csv(bind_rows(results), file.path(player_fp, "NBA_PLAYER_REG_SHOOTING_partial.csv"))
  
  return(results)
}

# RSelenium setup with headless browsing enabled
eCaps <- list(chromeOptions = list(args = c('--headless', '--disable-gpu', '--window-size=1280,800')))
rs_driver_object <- rsDriver(browser = 'chrome', extraCapabilities = eCaps, verbose = FALSE, port = free_port())
remDr <- rs_driver_object$client

# Start progress handling
handlers(global = TRUE)  # Enable global progress handlers

# Measure start time
start_time <- Sys.time()

# Run the scraping function with progress bar and batch processing
nba_reg_shooting_parallel <- with_progress({
  scrape_all_urls_selenium(nba_urls, remDr)
})

# Measure end time and calculate total time taken
end_time <- Sys.time()
total_time <- end_time - start_time
total_minutes <- as.numeric(total_time, units = "mins")

# Print the total time
message(paste("Total scraping time:", round(total_minutes, 2), "minutes"))

# Close the RSelenium session
remDr$close()
rs_driver_object$server$stop()

convert_to_numeric <- function(df) {
  # Specify columns to keep as character
  character_cols <- c("Var.2", "URL", "Team", "Season")
  
  # Convert all other columns to numeric, leaving the specified columns as character
  df <- df %>% 
    mutate(across(!all_of(character_cols), ~ suppressWarnings(as.numeric(.))))  # Suppress warnings for NAs
  
  return(df)
}

# Cleaning up tables
nba_reg_shooting <- convert_to_numeric(nba_reg_shooting_parallel) %>%
  select(-Var.8, -Var.15, -Var.22, -Var.25, -Var.28, -Var.31)

# Rename the columns and drop Rk
nba_reg_shooting <- nba_reg_shooting %>%
  rename(Rk = Var.1,
         Player = Var.2,
         Age = Var.3,
         G = Var.4,
         MP = Var.5,
         `FG%` = Var.6,
         `Avg. Distance` = Var.7,
         `% of FGA by Distance: 2P` = X..of.FGA.by.Distance,
         `% of FGA by Distance: 0-3ft` = X..of.FGA.by.Distance.1,
         `% of FGA by Distance: 3-10ft` = X..of.FGA.by.Distance.2,
         `% of FGA by Distance: 10-16ft` = X..of.FGA.by.Distance.3,
         `% of FGA by Distance: 16ft-3P` = X..of.FGA.by.Distance.4,
         `% of FGA by Distance: 3P` = X..of.FGA.by.Distance.5,
         `FG% by Distance: 2P` = FG..by.Distance,
         `FG% by Distance: 0-3ft` = FG..by.Distance.1,
         `FG% by Distance: 3-10ft` = FG..by.Distance.2,
         `FG% by Distance: 10-16ft` = FG..by.Distance.3,
         `FG% by Distance: 16ft-3P` = FG..by.Distance.4,
         `FG% by Distance: 3P` = FG..by.Distance.5,
         `% of FG Ast'd: 2P` = X..of.FG.Ast.d,
         `% of FG Ast'd: 3P` = X..of.FG.Ast.d.1,
         `%FGA of Dunks` = Dunks,
         `Made Dunk Attempts` = Dunks.1,
         `%3PA Corner 3s` = Corner.3s,
         `3P% Corner 3s` = Corner.3s.1,
         `Heaves Attempted` = Heaves,
         `Heaves Made` = Heaves.1) %>%
  filter(!is.na(Rk)) %>%
  select(-Rk)

# Load in player reg roster and league info for ids
load(file.path(player_fp, "NBA_PLAYER_REG_ROSTER.rda"))

league_info <- read_csv("C:/Users/djvia/OneDrive/Documents/Blog Website/Basketball_Database/NBA/LEAGUE/NBA_LEAGUE_INFO.csv")

# Assigning Player IDs to players
nba_reg_shooting <- nba_reg_shooting %>%
  rename(`Team Abbr.` = Team) %>%
  left_join(nba_reg_roster %>% select(`Player ID`, Player, `Team Abbr.`, Season),
            by = c("Player", "Team Abbr.", "Season"),
            relationship = "many-to-many")

# Handling duplicate players that have played on the same team in the same season
player_index <- read_csv("C:/Users/djvia/OneDrive/Documents/Blog Website/Basketball_Database/MISCELLANEOUS/NBA_ABA_PLAYER_INDEX.csv") %>%
  rename(College = Colleges)

nba_duplicates <- nba_reg_shooting %>%
  group_by(Player, `Team Abbr.`, Season) %>%
  filter(n() > 1)

nba_duplicates <- nba_duplicates %>%
  left_join(player_index %>% select(`Player ID`,`Birth Date`, Player),
            by = c("Player", "Player ID")) %>%
  mutate(`Birth Date` = as.Date(`Birth Date`, format = "%B %d, %Y"),
         Season_End_Year = as.numeric(str_sub(Season, 6, 9)),
         Calculated_Age = Season_End_Year - year(as.Date(`Birth Date`, "%B %d, %Y")))

nba_duplicates_filtered <- nba_duplicates %>%
  filter(abs(Age - Calculated_Age) <= 1) %>%
  distinct() %>%
  select(-`Birth Date`, -Season_End_Year, -Calculated_Age)

nba_reg_shooting <- nba_reg_shooting %>%
  anti_join(nba_duplicates, by = c("Player", "Team Abbr.", "Season")) %>%
  bind_rows(nba_duplicates_filtered)

# Assigning Franchise IDs to teams
nba_reg_shooting <- nba_reg_shooting %>%
  left_join(league_info %>% select(`Franchise ID`, Team, `Team Name`),
            by = c("Team Abbr." = "Team"), relationship = "many-to-many") %>%
  distinct()

# Arranging columns and dropping URL for final data frame
nba_reg_shooting <- nba_reg_shooting %>%
  select(`Player ID`, Player,`Franchise ID`,`Team Abbr.`, `Team Name`, 
         Season, everything()) %>%
  select(-URL) %>%
  arrange(`Team Name`,desc(Season), Player)

# Save the final nba_reg_shooting table to a RDA file
save(nba_reg_shooting,file = file.path(player_fp,"NBA_PLAYER_REG_SHOOTING.rda"))

# Display message to confirm save
print("nba_reg_shooting table has been saved to NBA_PLAYER_REG_SHOOTING.rda")

# Delete the partial RDA file
file.remove(file.path(player_fp,"NBA_PLAYER_REG_SHOOTING_partial.csv"))