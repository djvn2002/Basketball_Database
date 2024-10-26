# Author: David Vialpando-Nielsen
# Date Made: 10/25/2024
# Latest Update: 10/25/2024

# This is an update file to update the following rda file:
# NBA_PLAYER_PLAYOFF_ROSTER.rda

# Install and Library Packages

library(tidyverse)
library(rvest)
library(hoopR)

# Directory of where the rda file will reside in
player_fp <- "C:/Users/djvia/OneDrive/Documents/Blog Website/Basketball_Database/NBA/PLAYER/PLAYOFFS/"

# Load the valid URLs from the CSV file
nba_urls <- read_csv("C:/Users/djvia/OneDrive/Documents/Blog Website/Basketball_Database/URLS/NBA URLS/NBA_PLAYOFFS_URLS.csv") %>%
  filter(Season == most_recent_nba_season())

# Load in playoff roster
load(file.path(player_fp,"NBA_PLAYER_PLAYOFF_ROSTER.rda"))

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
      
      # Extract the table with ID #div_roster
      table <- webpage %>%
        html_node("#div_roster") %>%
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
    write_csv(bind_rows(results), file.path(player_fp,"NBA_PLAYER_PLAYOFF_ROSTER_partial.csv"))
    
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
recent_playoff_roster <- scrape_data_in_batches(nba_urls)

# Rename column V7 to Birthplace if it exists
recent_playoff_roster <- recent_playoff_roster %>%
  rename(Birthplace = Birth) %>%
  mutate(Birthplace = str_sub(Birthplace,1,2))

# Load the Nationality.txt file and create a tibble
birthplace_data <- read_lines("C:/Users/djvia/OneDrive/Documents/Blog Website/Basketball_Database/MISCELLANEOUS/NBA_Nationality.txt") %>%
  str_trim() %>%
  str_split_fixed(" - ", 2) %>%
  as.data.frame() %>%
  as_tibble(.name_repair = "minimal") %>%
  rename(Birthplace = V1, Country = V2)

# Display the nationality_data tibble to verify
print(birthplace_data)

# Replace the abbreviations in the Nationality_Abbr column with the actual country names
recent_playoff_roster <- recent_playoff_roster %>%
  left_join(birthplace_data) %>%
  mutate(Birthplace = ifelse(is.na(Country), Birthplace, Country),
         Birthplace = ifelse(is.na(Birthplace) | Birthplace == "", "United States", Birthplace),
         College = ifelse(is.na(Birthplace) | College == "", NA, College),
         `Birth Date` = if_else(is.na(`Birth Date`) | `Birth Date` == "", NA, `Birth Date`),
         No. = if_else(is.na(No.) | No. == "", NA, No.)) %>%
  select(-Country)

# Read in player index and league info
player_index <- read_csv("C:/Users/djvia/OneDrive/Documents/Blog Website/Basketball_Database/MISCELLANEOUS/NBA_ABA_PLAYER_INDEX.csv") %>%
  rename(College = Colleges)

league_info <- read_csv("C:/Users/djvia/OneDrive/Documents/Blog Website/Basketball_Database/NBA/LEAGUE/NBA_LEAGUE_INFO.csv")

# Assigning Player IDs to players
recent_playoff_roster <- recent_playoff_roster %>%
  left_join(player_index %>% select(`Player ID`, Player, College,`Birth Date`), 
            by = c("Player", "College","Birth Date"))

# Assigning Franchise IDs to teams
recent_playoff_roster <- recent_playoff_roster %>%
  left_join(league_info %>% select(`Franchise ID`, Team, `Team Name`),
            by = c("Team"), relationship = "many-to-many") %>%
  distinct() %>%
  rename(`Team Abbr.` = Team)

# Reorganizing dataframe and dropping url columns
recent_playoff_roster <- recent_playoff_roster %>%
  select(`Player ID`, No., Player,`Franchise ID`,`Team Abbr.`, `Team Name`, 
         Season, everything()) %>%
  select(-URL) %>%
  arrange(`Team Name`,desc(Season), Player)

# Get the most recent NBA season using hoopR as a number (e.g., 2024)
most_recent_season <- most_recent_nba_season()

# Convert to "YYYY-YYYY" format for filtering
most_recent_season_formatted <- paste(most_recent_season - 1, most_recent_season, sep = "-")

# Filter out the most recent season's data from nba_playoff_roster
nba_playoff_roster <- nba_playoff_roster %>%
  filter(Season != most_recent_season_formatted)

# Bind the new latest season data with the filtered nba_playoff_roster
nba_playoff_roster <- bind_rows(nba_playoff_roster, recent_playoff_roster) %>%
  arrange(`Team Name`,desc(Season), Player)

# Save the final nba_playoff_roster table to a RDA file
save(nba_playoff_roster,file = file.path(player_fp,"NBA_PLAYER_PLAYOFF_ROSTER.rda"))

# Display message to confirm save
print("nba_playoff_roster table has been saved to NBA_PLAYER_PLAYOFF_ROSTER.rda")

# Delete the partial RDA file
file.remove(file.path(player_fp,"NBA_PLAYER_PLAYOFF_ROSTER_partial.csv"))