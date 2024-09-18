# Author: David Vialpando-Nielsen
# Date Made: 9/17/2024
# Latest Update: 9/17/2024

# This is an update file!
# This will update: NBA_TEAM_REG_OPP_SHOOTING.rda
# Based on the most recent season

# Install and Library Packages
library(tidyverse)
library(stringr)
library(lubridate)
library(readr)
library(rvest)
library(hoopR)

# File path for the team file
team_fp <- "C:/Users/djvia/OneDrive/Documents/Blog Website/Basketball_Database/NBA/TEAM/REGULAR SEASON"

# Load the valid URLs from the CSV file
nba_urls <- read_csv("C:/Users/djvia/OneDrive/Documents/Blog Website/Basketball_Database/URLS/NBA URLS/NBA_LEAGUE_URLS.csv") %>%
  filter(Year == most_recent_nba_season())

# Load in regular season roster
load(file.path(team_fp,"NBA_TEAM_REG_OPP_SHOOTING.rda"))

# Function to clean column names and handle duplicates
clean_colnames <- function(df) {
  # Ensure no empty or NA column names
  colnames(df) <- ifelse(colnames(df) == "" | is.na(colnames(df)), paste0("V", seq_along(colnames(df))), colnames(df))
  
  # Handle duplicate column names by appending unique suffix
  colnames(df) <- make.names(colnames(df), unique = TRUE)
  
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
        html_node("#div_shooting-opponent") %>%
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
    write_csv(bind_rows(results), file.path(team_fp,"NBA_TEAM_REG_OPP_SHOOTING_partial.csv"))
    
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
latest_team_opp_shooting <- scrape_data_in_batches(nba_urls)

# Rename columns and cleaning them for consistency
latest_team_opp_shooting <- latest_team_opp_shooting %>%
  rename(Rk = V1,
         Team = V2,
         G = V3,
         MP = V4,
         `FG%` = V5,
         `Avg. Distance` = V6,
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
         `%FGA of Dunks`= Dunks,
         `Made Dunk Attempts` = Dunks.1,
         `%3PA Corner 3s` = Corner,
         `3P% Corner 3s` = Corner.1) %>%
  select(-V7, -V14, -V21, -V24, -V27) %>%
  filter(Rk != "Rk" & Team != "League Average") %>%
  select(-Rk) %>%
  mutate(across(-c(Team, URL, Season), as.numeric))

# Remove '*' from the 'Team' column
latest_team_opp_shooting$Team <- gsub("\\*", "", latest_team_opp_shooting$Team)

# Read in csv for League Info
league_info <- read_csv("C:/Users/djvia/OneDrive/Documents/Blog Website/Basketball_Database/NBA/LEAGUE/NBA_LEAGUE_INFO.csv")

# Arrange by earliest season and joining to have Team Abbr.
latest_team_opp_shooting <- latest_team_opp_shooting %>%
  rename(`Team Name` = Team) %>%
  mutate( From = as.numeric(substr(latest_team_opp_shooting$Season,1,4)),
          To = as.numeric(substr(latest_team_opp_shooting$Season,6,9))) %>%
  left_join(league_info %>%
              select(`Franchise ID`, Team, `Team Name`, From, To), 
            by = c('Team Name'), relationship = 'many-to-many') %>%
  filter(To.x >= From.y & (is.na(To.y) | To.x <= To.y)) %>%
  select(-To.x, -To.y, -From.x,-From.y) %>%
  rename(`Team Abbr.` = Team) %>%
  select(`Franchise ID`,`Team Name`, `Team Abbr.`, Season, everything()) %>%
  select(-URL)                                    

# Read in standings data to join into latest_team_opp_shooting
nba_standings <- read_csv("C:/Users/djvia/OneDrive/Documents/Blog Website/Basketball_Database/NBA/LEAGUE/NBA_STANDINGS.csv")

# Make Season End and joining to nba_standings
latest_team_opp_shooting <- latest_team_opp_shooting %>%
  mutate(Season_End = as.numeric(str_extract(Season, "\\d{4}$"))) %>%
  left_join(nba_standings %>% select(`Team Abbr.`, Season, W, L, `W/L%`, GB, SRS, 
                                     Division, `Division Rank`, Conference, 
                                     `Conference Rank`, `Made Playoffs`),
            by = c("Team Abbr.", "Season_End" = "Season")) %>%
  select(-Season_End)

# Rearrange columns for final dataframe %>%
latest_team_opp_shooting <- latest_team_opp_shooting %>%
  select(`Franchise ID`,`Team Name`, `Team Abbr.`, Season, G, W, L, `W/L%`, GB, SRS,
         Division, `Division Rank`, Conference, `Conference Rank`, everything()) %>%
  arrange(`Team Name`, desc(Season))

# Get the most recent NBA season using hoopR as a number (e.g., 2024)
most_recent_season <- most_recent_nba_season()

# Convert to "YYYY-YYYY" format for filtering
most_recent_season_formatted <- paste(most_recent_season - 1, most_recent_season, sep = "-")

# Filter out the most recent season's data from nba_team_reg_opp_shooting
nba_team_reg_opp_shooting <- nba_team_reg_opp_shooting %>%
  filter(Season != most_recent_season_formatted)

# Bind the new latest season data with the filtered nba_team_reg_opp_shooting
nba_team_reg_opp_shooting <- bind_rows(nba_team_reg_opp_shooting, latest_team_opp_shooting) %>%
  arrange(`Team Name`,desc(Season))

# Save per game data frame to a rda file
save(nba_team_reg_opp_shooting,file = file.path(team_fp,"NBA_TEAM_REG_OPP_SHOOTING.rda"))

# Display message to confirm save
print("nba_team_reg_opp_shooting table has been saved to NBA_TEAM_REG_OPP_SHOOTING.rda")

# Delete the partial RDA file
file.remove(file.path(team_fp,"NBA_TEAM_REG_OPP_SHOOTING_partial.csv"))