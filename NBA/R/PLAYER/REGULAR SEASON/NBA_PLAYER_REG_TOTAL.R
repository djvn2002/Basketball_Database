# Author: David Vialpando-Nielsen
# Date Made: 8/24/2024
# Latest Update: 8/25/2024

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

# Function to clean column names and ensure specific column types are consistent
clean_colnames <- function(df) {
  colnames(df) <- ifelse(colnames(df) == "" | is.na(colnames(df)), paste0("V", seq_along(colnames(df))), colnames(df))
  
  # Convert No. column to character
  if ("No." %in% colnames(df)) {
    df <- df %>% mutate(`No.` = as.character(`No.`))
  }
  
  # Convert Exp column to integer, with "R" replaced by 0
  if ("Exp" %in% colnames(df)) {
    df <- df %>% mutate(Exp = as.integer(replace(Exp, Exp == "R", 0)))
  }
  
  return(df)
}

# Function to scrape the specific table from a URL with enhanced error handling
scrape_table <- function(url) {
  max_retries <- 5
  wait_time <- 30  # Initial wait time set to 30 seconds
  
  for (i in 1:max_retries) {
    tryCatch({
      webpage <- read_html(url)
      
      # Extract the table with ID #div_totals
      table <- webpage %>%
        html_node("#div_totals") %>%
        html_table()
      
      # Clean column names and ensure specific column types are consistent
      table <- clean_colnames(table)
      
      # Extract the season from the URL and format it as "YYYY-YYYY"
      season <- str_extract(url, "\\d{4}")
      season <- as.numeric(season)
      season_formatted <- paste(season - 1, season, sep = "-")
      
      # Extract the team abbreviation from the URL
      team <- str_extract(url, "(?<=teams/)[A-Z]{3}")
      
      # Add the URL, Team, and Season to the table for reference
      table <- table %>%
        mutate(URL = url, Team = team, Season = season_formatted)
      
      return(table)
      
    }, error = function(e) {
      if (grepl("HTTP error 429", e$message)) {
        jitter <- runif(1, 0, wait_time / 2)
        message(paste("Rate limited. Retrying in", wait_time + jitter, "seconds... (Attempt", i, "of", max_retries, ")"))
        Sys.sleep(wait_time + jitter)
        wait_time <- wait_time * 2  # More aggressive exponential backoff
      } else {
        message(paste("Error scraping URL:", url, "-", e$message))
        return(NULL)
      }
    })
  }
  
  # If we reach here, it means we exhausted retries for 429 errors
  message(paste("Exhausted retries for URL:", url))
  return(NULL)
}

# Function to scrape data for a batch of URLs
scrape_batch <- function(urls) {
  results <- list()
  
  for (url in urls) {
    table <- scrape_table(url)
    if (!is.null(table)) {
      results <- bind_rows(results, table)
    }
  }
  
  return(results)
}

# Function to scrape data in batches of 30 URLs and save progress
scrape_data_in_batches <- function(nba_urls, batch_size = 30) {
  total_urls <- nrow(nba_urls)
  num_batches <- ceiling(total_urls / batch_size)
  results <- list()
  
  # Define wait_time here to avoid scope issues
  wait_time <- 30  # Initial wait time set to 30 seconds
  
  # Measure the start time
  start_time <- Sys.time()
  
  for (i in 1:num_batches) {
    start_index <- (i - 1) * batch_size + 1
    end_index <- min(i * batch_size, total_urls)
    batch_urls <- nba_urls$URL[start_index:end_index]
    
    timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
    message(paste("Processing batch", i, "of", num_batches, "at", timestamp))
    batch_results <- scrape_batch(batch_urls)
    
    if (length(batch_results) > 0) {
      results <- bind_rows(results, batch_results)
    }
    
    # Save progress after each batch
    write_csv(bind_rows(results), file.path(player_fp,"NBA_PLAYER_REG_TOTAL_partial.csv"))
    
    # Pause before the next batch
    Sys.sleep(120)  # Wait for 2 minutes before the next batch
  }
  
  # Measure the end time
  end_time <- Sys.time()
  total_time <- end_time - start_time
  total_minutes <- as.numeric(total_time, units = "mins")
  
  message(paste("Total processing time:", round(total_minutes, 2), "minutes"))
  
  return(results)
}

# Scrape data in batches of 30 URLs
nba_reg_total <- scrape_data_in_batches(nba_urls)

# Filter out rows where Rk column is NA
nba_reg_total <- nba_reg_total %>%
  filter(!is.na(Rk))

# Define a function to parse dates robustly
parse_birth_date <- function(birth_date) {
  parse_date_time(birth_date, orders = c("mdy", "dmy", "ymd"))
}

# Load the NBA roster data
load(file.path(player_fp, "NBA_PLAYER_REG_ROSTER.rda"))

nba_reg_roster <- nba_reg_roster %>%  
  mutate(Player = str_remove(Player, " \\(.*\\)"),
         Birth_Year = year(parse_birth_date(`Birth Date`)))

# Extract the start year from the season
nba_reg_total <- nba_reg_total %>%
  mutate(Start_Year = as.numeric(str_sub(Season, 1, 4)),
         End_Year = as.numeric(str_sub(Season, 6, 9)))

# Left join nba_reg_total with nba_reg_player_roster_updated to get Birth Date
nba_reg_total <- nba_reg_total %>%
  left_join(nba_reg_player_roster_updated %>% select(Player, Birth_Year, URL, Team, Season, `Birth Date`),
            by = c("Player", "Team", "Season", "URL"))

# Calculate possible ages and verify
nba_reg_total <- nba_reg_total %>%
  mutate(Calculated_Age_Start = Start_Year - Birth_Year,
         Calculated_Age_End = End_Year - Birth_Year)

# Handling different players with the same name
player_dupes_list <- read_delim("C:/Users/djvia/OneDrive/Documents/Blog Website/Basketball_Database/MISCELLANEOUS/NBA_Player_Dupes.txt", delim = "\n", col_names = FALSE) %>%
  rename(Player = X1) %>%
  filter(Player != "")

player_dupes <- nba_reg_total %>%
  filter(Player %in% player_dupes_list$Player) %>%
  arrange(Player)

player_dupes <- player_dupes %>%
  filter(Calculated_Age_Start == Age | Calculated_Age_End == Age)

# Function to assign suffixes based on calculated ages
assign_suffixes <- function(df) {
  df <- df %>%
    group_by(Player, `Birth Date`) %>%
    mutate(Suffix = "") %>%
    ungroup() %>%
    arrange(Player, desc(`Birth Date`)) %>%
    group_by(Player) %>%
    mutate(Suffix = paste0(" (", cumsum(!duplicated(`Birth Date`)), ")")) %>%
    ungroup() %>%
    mutate(Player = paste0(Player, Suffix)) %>%
    select(-Suffix)
  return(df)
}

# Assign suffixes to players with duplicate names
player_dupes_with_suffixes <- assign_suffixes(player_dupes)

# Remove duplicate players from the original dataframe
nba_reg_total_without_dupes <- nba_reg_total %>%
  filter(!Player %in% player_dupes_list$Player)

# Combine the updated player names back to the original dataframe
nba_reg_total_updated <- bind_rows(nba_reg_total_without_dupes, player_dupes_with_suffixes) %>%
  arrange(Season, Team, Player) %>%
  select(-Start_Year,-End_Year,-Birth_Year,-`Birth Date`,-Calculated_Age_Start,-Calculated_Age_End) %>%
  rename(`Team Abbr.` = Team)

# Add the correct 'Team Name' and 'League' from the 'nba_urls' data frame
nba_reg_total <- nba_reg_total_updated %>%
  left_join(nba_urls %>% select(URL, `Team Abbr.`, `Team Name`, League),
            by = c("URL", "Team Abbr.")) %>%
  select(-Rk)

# Save the final nba_reg_total table to a RDA file
save(nba_reg_total,file = file.path(player_fp,"NBA_PLAYER_REG_TOTAL.rda"))

# Display message to confirm save
print("nba_reg_total table has been saved to NBA_PLAYER_REG_TOTAL.rda")

# Delete the partial RDA file
file.remove(file.path(player_fp,"NBA_PLAYER_REG_TOTAL_partial.csv"))
