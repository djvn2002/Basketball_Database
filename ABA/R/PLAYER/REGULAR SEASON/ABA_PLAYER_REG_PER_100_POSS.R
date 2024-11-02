# Author: David Vialpando-Nielsen
# Date Made: 11/1/2024
# Latest Update: 11/1/2024

# This file will contain scrape code for player stats based by 100 possessions

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
player_fp <- "C:/Users/djvia/OneDrive/Documents/Blog Website/Basketball_Database/ABA/PLAYER/REGULAR SEASON/"

# Load the valid URLs from the CSV file
aba_urls <- read_csv("C:/Users/djvia/OneDrive/Documents/Blog Website/Basketball_Database/URLS/ABA URLS/ABA_TEAM_URLS.csv")

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
      webElem <- remDr$findElement(using = "css", "#div_per_poss")
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
scrape_all_urls_selenium <- function(aba_urls, remDr, batch_size = 100, restart_every = 500) {
  results <- list()
  
  # Initialize the progress bar
  p <- progressr::progressor(steps = nrow(aba_urls))
  
  # Scrape each URL one at a time in batches
  for (i in seq(1, nrow(aba_urls), by = batch_size)) {
    batch_urls <- aba_urls$URL[i:min(i + batch_size - 1, nrow(aba_urls))]
    
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
  write_csv(bind_rows(results), file.path(player_fp, "ABA_PLAYER_REG_PER_100_POSS_partial.csv"))
  
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
aba_reg_per_100_parallel <- with_progress({
  scrape_all_urls_selenium(aba_urls, remDr)
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
  character_cols <- c("Player", "URL", "Team", "Season")
  
  # Convert all other columns to numeric, leaving the specified columns as character
  df <- df %>% 
    mutate(across(!all_of(character_cols), ~ suppressWarnings(as.numeric(.))))  # Suppress warnings for NAs
  
  return(df)
}

# Cleaning up tables
aba_reg_per_100_poss <- convert_to_numeric(aba_reg_per_100_parallel) %>%
  filter(Rk != "Rk") %>%
  select(-Rk)

# Load in player reg roster and league info for ids
load(file.path(player_fp, "ABA_PLAYER_REG_ROSTER.rda"))

league_info <- read_csv("C:/Users/djvia/OneDrive/Documents/Blog Website/Basketball_Database/ABA/LEAGUE/ABA_LEAGUE_INFO.csv")

# Assigning Player IDs to players
aba_reg_per_100_poss <- aba_reg_per_100_poss %>%
  rename(`Team Abbr.` = Team) %>%
  left_join(aba_reg_roster %>% select(`Player ID`, Player, `Team Abbr.`, Season),
            by = c("Player", "Team Abbr.", "Season"),
            relationship = "many-to-many")

# Assigning Franchise IDs to teams
aba_reg_per_100_poss <- aba_reg_per_100_poss %>%
  left_join(league_info %>% select(`Franchise ID`, Team, `Team Name`),
            by = c("Team Abbr." = "Team"), relationship = "many-to-many") %>%
  distinct()

# Arranging columns and dropping URL for final data frame
aba_reg_per_100_poss <- aba_reg_per_100_poss %>%
  select(`Player ID`, Player,`Franchise ID`,`Team Abbr.`, `Team Name`, 
         Season, everything()) %>%
  select(-URL, -`Var.28`) %>%
  arrange(`Team Name`,desc(Season), Player) %>%
  rename(`FG%` = FG.,`3P` = `X3P`,`3PA` = `X3PA`,`3P%` = `X3P.`,`2P` = `X2P`,
         `2PA` = `X2PA`,`2P%` = `X2P.`,`FT%` = FT.)

# Save the final aba_reg_per_100_poss table to a RDA file
save(aba_reg_per_100_poss,file = file.path(player_fp,"ABA_PLAYER_REG_PER_100_POSS.rda"))

# Display message to confirm save
print("aba_reg_per_100_poss table has been saved to ABA_PLAYER_REG_PER_100_POSS.rda")

# Delete the partial RDA file
file.remove(file.path(player_fp,"ABA_PLAYER_REG_PER_100_POSS_partial.csv"))