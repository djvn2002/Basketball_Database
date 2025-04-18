# Author: David Vialpando-Nielsen
# Date Made: 9/17/2024
# Latest Update: 9/17/2024

# This file will contain scrape code for opponent team per 100 statsistics
# Library Packages
library(tidyverse)
library(stringr)
library(lubridate)
library(readr)
library(rvest)

# File path for the team file
team_fp <- "C:/Users/djvia/OneDrive/Documents/Blog Website/Basketball_Database/NBA/TEAM/REGULAR SEASON"

# Load the valid URLs from the CSV file
nba_urls <- read_csv("C:/Users/djvia/OneDrive/Documents/Blog Website/Basketball_Database/URLS/NBA URLS/NBA_LEAGUE_URLS.csv") %>%
  filter(Year >= 1974)

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
      
      # Extract the table with ID #div_per_poss-opponent
      table <- webpage %>%
        html_node("#div_per_poss-opponent") %>%
        html_table()
      
      # Clean column names and ensure specific column types are consistent
      table <- clean_colnames(table)
      
      # Extract the season from the URL and format it as "YYYY-YYYY"
      season <- str_extract(url, "\\d{4}")
      season <- as.numeric(season)
      season_formatted <- paste(season - 1, season, sep = "-")
      
      # Add the URL and Season to the table for reference
      table <- table %>%
        mutate(URL = url, Season = season_formatted)
      
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
    write_csv(bind_rows(results), file.path(team_fp,"NBA_TEAM_REG_OPP_100_POSS_partial.csv"))
    
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
nba_team_reg_opp_100_poss <- scrape_data_in_batches(nba_urls)

# Drops rows that have Rk that contain NA and drop the entire column afterwards
nba_team_reg_opp_100_poss <- nba_team_reg_opp_100_poss %>%
  filter(!is.na(Rk)) %>%
  select(-Rk)

# Remove '*' from the 'Team' column
nba_team_reg_opp_100_poss$Team <- gsub("\\*", "", nba_team_reg_opp_100_poss$Team)

# Read in csv for League Info
league_info <- read_csv("C:/Users/djvia/OneDrive/Documents/Blog Website/Basketball_Database/NBA/LEAGUE/NBA_LEAGUE_INFO.csv")

# Arrange by earliest season and joining to have Team Abbr.
nba_team_reg_opp_100_poss <- nba_team_reg_opp_100_poss %>%
  rename(`Team Name` = Team) %>%
  mutate( From = as.numeric(substr(nba_team_reg_opp_100_poss$Season,1,4)),
          To = as.numeric(substr(nba_team_reg_opp_100_poss$Season,6,9))) %>%
  left_join(league_info %>%
              select(`Franchise ID`, Team, `Team Name`, From, To), 
            by = c('Team Name'), relationship = 'many-to-many') %>%
  filter(To.x >= From.y & (is.na(To.y) | To.x <= To.y)) %>%
  select(-To.x, -To.y, -From.x,-From.y) %>%
  rename(`Team Abbr.` = Team) %>%
  select(`Franchise ID`,`Team Name`, `Team Abbr.`, Season, everything()) %>%
  select(-URL)                                    

# Read in standings data to join into nba_team_reg_opp_100_poss
nba_standings <- read_csv("C:/Users/djvia/OneDrive/Documents/Blog Website/Basketball_Database/NBA/LEAGUE/NBA_STANDINGS.csv")

# Make Season End and joining to nba_standings
nba_team_reg_opp_100_poss <- nba_team_reg_opp_100_poss %>%
  mutate(Season_End = as.numeric(str_extract(Season, "\\d{4}$"))) %>%
  left_join(nba_standings %>% select(`Team Abbr.`, Season, W, L, `W/L%`, GB, SRS, 
                                     Division, `Division Rank`, Conference, 
                                     `Conference Rank`, `Made Playoffs`),
            by = c("Team Abbr.", "Season_End" = "Season")) %>%
  select(-Season_End)

# Rearrange columns for final dataframe %>%
nba_team_reg_opp_100_poss <- nba_team_reg_opp_100_poss %>%
  select(`Franchise ID`,`Team Name`, `Team Abbr.`, Season, G, W, L, `W/L%`, GB, SRS,
         Division, `Division Rank`, Conference, `Conference Rank`, everything()) %>%
  arrange(`Team Name`, desc(Season))

# Save per game data frame to a rda file
save(nba_team_reg_opp_100_poss,file = file.path(team_fp,"NBA_TEAM_REG_OPP_100_POSS.rda"))

# Display message to confirm save
print("nba_team_reg_opp_100_poss table has been saved to NBA_TEAM_REG_OPP_100_POSS.rda")

# Delete the partial RDA file
file.remove(file.path(team_fp,"NBA_TEAM_REG_OPP_100_POSS_partial.csv"))