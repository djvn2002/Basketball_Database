# Author: David Vialpando-Nielsen
# Date Made: 9/6/2024
# Latest Update: 9/7/2024

# This file will contain draft data from 1947 to concurrent season

# Library Packages
library(tidyverse)
library(rvest)
library(hoopR)
library(zoo)

# NBA seasons
seasons <- 1947:most_recent_nba_season()

# Constructing draft urls
draft_urls <- tibble(Season = seasons) %>%
  mutate(URL = if_else(Season < 1950,
                       paste0("https://www.basketball-reference.com/draft/BAA_", Season, ".html"),
                       paste0("https://www.basketball-reference.com/draft/NBA_", Season, ".html")))

# File path for the league file
league_fp <- "C:/Users/djvia/OneDrive/Documents/Blog Website/Basketball_Database/NBA/LEAGUE/DRAFT"

# Function to clean column names and ensure specific column types are consistent
clean_colnames <- function(df) {
  # Ensure all column names are unique using make.names
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
      
      # Extract the table with ID #div_totals-team
      table <- webpage %>%
        html_node("#div_stats") %>%
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
scrape_data_in_batches <- function(draft_urls, batch_size = 30) {
  total_urls <- nrow(draft_urls)
  num_batches <- ceiling(total_urls / batch_size)
  results <- list()
  
  # Define wait_time here to avoid scope issues
  wait_time <- 30  # Initial wait time set to 30 seconds
  
  # Measure the start time
  start_time <- Sys.time()
  
  for (i in 1:num_batches) {
    start_index <- (i - 1) * batch_size + 1
    end_index <- min(i * batch_size, total_urls)
    batch_urls <- draft_urls$URL[start_index:end_index]
    
    timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
    message(paste("Processing batch", i, "of", num_batches, "at", timestamp))
    batch_results <- scrape_batch(batch_urls)
    
    if (length(batch_results) > 0) {
      results <- bind_rows(results, batch_results)
    }
    
    # Save progress after each batch
    write_csv(bind_rows(results), file.path(league_fp,"NBA_DRAFT_partial.csv"))
    
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
nba_draft <- scrape_data_in_batches(draft_urls)

# Rename columns and grabbing only necessary columns for data frame
nba_draft_test <- nba_draft %>%
  rename(Rk = X,
         Pick = X.1,
         `Team Abbr.` = X.2,
         Player = Round.1,
         College = Round.1.1) %>%
  select(-X.3, -Totals, -Totals.1, -Totals.2, -Totals.3, -Totals.4, -Shooting,
         -Shooting.1, -Shooting.2, -Per.Game, -Per.Game.1, -Per.Game.2, -Per.Game.3,
         -Advanced, -Advanced.1, -Advanced.2, -Advanced.3)

# Rename 'Season' to 'Draft Year' and extract the year on the right-hand side of "YYYY-YYYY"
nba_draft_test <- nba_draft_test %>%
  rename(`Draft Year` = Season) %>%
  mutate(`Draft Year` = as.numeric(str_extract(`Draft Year`, "\\d{4}$")))

# Add the 'Round' column, initialize with NA
nba_draft_test <- nba_draft_test %>%
  mutate(Round = NA_integer_)

# Create a column that tracks the current round within each season
nba_draft_test <- nba_draft_test %>%
  group_by(`Draft Year`) %>%
  mutate(
    # Detect where "Round" appears in the 'Player' column
    Round = if_else(str_detect(Player, "Round"), as.integer(str_extract(Player, "\\d+")), NA_integer_),
    
    # Fill down the 'Round' column and backfill any NA values
    Round = zoo::na.locf(Round, na.rm = FALSE)
  ) %>%
  ungroup()

# Replace all NA values in the 'Round' column with 1
nba_draft_test <- nba_draft_test %>%
  mutate(Round = replace_na(Round, 1))

# Remove rows where "Round" is mentioned in the 'Player' column (these rows contain no player data)
nba_draft_test <- nba_draft_test %>%
  filter(!str_detect(Player, "Round"))

# Remove rows containing RK and dropping the Rk column
nba_draft_test <- nba_draft_test %>%
  filter(Rk != "Rk") %>%
  select(-Rk) %>%
  mutate( Pick = as.numeric(Pick),
          Round = as.numeric(Round)) %>%
  arrange(`Draft Year`, Round, Pick)

# House cleaning for Player and College columns
nba_draft_test <- nba_draft_test %>%
  filter(Player != "") %>%
  mutate(College = na_if(College, ""))

# Read in player index
player_index <- read_csv("C:/Users/djvia/OneDrive/Documents/Blog Website/Basketball_Database/MISCELLANEOUS/NBA_ABA_PLAYER_INDEX.csv") %>%
  rename(College = Colleges) %>%
  mutate(College = str_trim(College),  # Remove any leading/trailing whitespace
         College = if_else(!is.na(College),  # Only perform the split if 'College' is not NA
                           sapply(str_split(College, ","), function(x) str_trim(tail(x, 1))),
                           College))  # Extract rightmost college and trim whitespace

# Perform a left join to transfer Player ID from player_index to nba_draft_test
nba_draft_test <- nba_draft_test %>%
  left_join(player_index %>% select(`Player`, `College`, `Player ID`, `From`), 
            by = c("Player", "College"), relationship = 'many-to-many')

# Identify duplicates based on Player and College
duplicates_df <- nba_draft_test %>%
  group_by(Player, College) %>%
  filter(n_distinct(`Player ID`) > 1) %>%  # Keep players with more than 1 distinct Player ID per Player and College
  ungroup()

# Retain unaffected rows
valid_rows <- nba_draft_test %>%
  group_by(Player, College) %>%
  filter(n_distinct(`Player ID`) == 1) %>%  # Keep players with only one distinct Player ID per Player and College
  ungroup()

# Filter out rows that are inconsistent and duplicates
filtered_duplicates <- duplicates_df %>%
  filter(is.na(From) | (From >= `Draft Year` & From <= `Draft Year` + 3)) %>%  # Retain rows with valid From range or NA
  distinct(`Draft Year`, `Player ID`, .keep_all = TRUE) 

# Combine rows together, drop URL and From
nba_draft_cleaned <- bind_rows(valid_rows, filtered_duplicates) %>%
  arrange(`Draft Year`, Round, Pick) %>%
  select(-URL, -From)

# Assign Franchise IDs
league_info <- read_csv("C:/Users/djvia/OneDrive/Documents/Blog Website/Basketball_Database/NBA/LEAGUE/NBA_LEAGUE_INFO.csv")

nba_draft_cleaned <- nba_draft_cleaned %>%
  left_join(league_info %>% select(`Franchise ID`, Team, `Team Name`),
            by = c("Team Abbr." = "Team"), relationship = "many-to-many") %>%
  distinct()

# Create the 'Overall Pick' column, numbering each pick sequentially within each draft year
nba_draft_cleaned <- nba_draft_cleaned %>%
  group_by(`Draft Year`) %>%
  mutate(`Overall Pick` = row_number(),
         `Overall Pick` = as.numeric(`Overall Pick`)) %>%
  ungroup()

# Final NBA Draft data frame
nba_draft <- nba_draft_cleaned %>%
  mutate(`Draft ID` = row_number()) %>%
  select(`Draft ID`, `Draft Year`, `Player ID`, Player, College, Pick, Round, 
         `Overall Pick`, `Franchise ID`, `Team Abbr.`, `Team Name`)

# Save the final nba_draft table to a RDA file
save(nba_draft,file = file.path(league_fp,"NBA_LEAGUE_DRAFT.rda"))

# Display message to confirm save
print("nba_draft table has been saved to NBA_LEAGUE_DRAFT.rda")

# Delete the partial RDA file
file.remove(file.path(league_fp,"NBA_DRAFT_partial.csv"))
