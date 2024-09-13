# Author: David Vialpando-Nielsen
# Date Made: 9/11/2024
# Latest Update: 9/11/2024

# This is an update file!
# This will update: NBA_PLAYER_REG_ADVANCED.rda
# Based on the most recent season

# Install and Library Packages

library(tidyverse)
library(rvest)
library(hoopR)

# Directory of where the rda file will reside in
player_fp <- "C:/Users/djvia/OneDrive/Documents/Blog Website/Basketball_Database/NBA/PLAYER/REGULAR SEASON/"

# Load the valid URLs from the CSV file
nba_urls <- read_csv("C:/Users/djvia/OneDrive/Documents/Blog Website/Basketball_Database/URLS/NBA URLS/NBA_TEAM_URLS.csv") %>%
  filter(Season == most_recent_nba_season())

# Load in regular season roster
load(file.path(player_fp,"NBA_PLAYER_REG_ADVANCED.rda"))

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
        html_node("#div_advanced") %>%
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
    write_csv(bind_rows(results), file.path(player_fp,"NBA_PLAYER_REG_ADVANCED_partial.csv"))
    
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
latest_advanced <- scrape_data_in_batches(nba_urls)

# Filter out Rk and then drop unnecessary columns
latest_advanced <- latest_advanced %>%
  filter(!is.na(Rk)) %>%
  select(-Rk, -URL, -V18, -V23)

# Load the NBA roster data and league info
load(file.path(player_fp, "NBA_PLAYER_REG_ROSTER.rda"))

league_info <- read_csv("C:/Users/djvia/OneDrive/Documents/Blog Website/Basketball_Database/NBA/LEAGUE/NBA_LEAGUE_INFO.csv")

# Assigning Player IDs to players
latest_advanced <- latest_advanced %>%
  rename(`Team Abbr.` = Team) %>%
  left_join(nba_reg_roster %>% select(`Player ID`, Player, `Team Abbr.`, Season),
            by = c("Player", "Team Abbr.", "Season"))

# Handling duplicate players that have played on the same team in the same season
player_index <- read_csv("C:/Users/djvia/OneDrive/Documents/Blog Website/Basketball_Database/MISCELLANEOUS/NBA_ABA_PLAYER_INDEX.csv") %>%
  rename(College = Colleges)

nba_duplicates <- latest_advanced %>%
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

latest_advanced <- latest_advanced %>%
  anti_join(nba_duplicates, by = c("Player", "Team Abbr.", "Season")) %>%
  bind_rows(nba_duplicates_filtered)

# Assigning Franchise IDs to teams
latest_advanced <- latest_advanced %>%
  left_join(league_info %>% select(`Franchise ID`, Team, `Team Name`),
            by = c("Team Abbr." = "Team"), relationship = "many-to-many") %>%
  distinct()

# Arranging columns and dropping URL
latest_advanced <- latest_advanced %>%
  select(`Player ID`, Player,`Franchise ID`,`Team Abbr.`, `Team Name`, 
         Season, everything()) %>%
  arrange(`Team Name`,desc(Season), Player)

# Get the most recent NBA season using hoopR as a number (e.g., 2024)
most_recent_season <- most_recent_nba_season()

# Convert to "YYYY-YYYY" format for filtering
most_recent_season_formatted <- paste(most_recent_season - 1, most_recent_season, sep = "-")

# Filter out the most recent season's data from nba_reg_advanced
nba_reg_advanced <- nba_reg_advanced %>%
  filter(Season != most_recent_season_formatted)

# Bind the new latest season data with the filtered nba_reg_advanced
nba_reg_advanced <- bind_rows(nba_reg_advanced, latest_advanced) %>%
  arrange(`Team Name`,desc(Season), Player)

# Save the final nba_reg_advanced table to a RDA file
save(nba_reg_advanced,file = file.path(player_fp,"NBA_PLAYER_REG_ADVANCED.rda"))

# Display message to confirm save
print("nba_reg_advanced table has been saved to NBA_PLAYER_REG_ADVANCED.rda")

# Delete the partial RDA file
file.remove(file.path(player_fp,"NBA_PLAYER_REG_ADVANCED_partial.csv"))