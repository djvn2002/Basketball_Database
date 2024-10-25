# Author: David Vialpando-Nielsen
# Date Made: 9/14/2024
# Latest Update: 9/14/2024

# This is an update file!
# This will update: NBA_STANDINGS.csv
# Based on the most recent season

# Install and Library Packages

library(tidyverse)
library(rvest)
library(hoopR)

# Directory of where the rda file will reside in
league_fp <- "C:/Users/djvia/OneDrive/Documents/Blog Website/Basketball_Database/NBA/LEAGUE"

# Load the valid URLs from the CSV file
nba_urls <- read_csv("C:/Users/djvia/OneDrive/Documents/Blog Website/Basketball_Database/URLS/NBA URLS/NBA_LEAGUE_URLS.csv") %>%
  filter(Year == most_recent_nba_season())

# Load in standings data
standings <- read_csv("C:/Users/djvia/OneDrive/Documents/Blog Website/Basketball_Database/NBA/LEAGUE/NBA_STANDINGS.csv") %>%
  filter(Season != most_recent_nba_season()) %>%
  mutate(GB = as.numeric(GB),
         GB = ifelse(!is.na(GB), round(GB, 1), GB))
  

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

# Function to clean and restructure the table to have Team, Conference, and Division columns
restructure_table <- function(df) {
  # Define conference and division mappings
  eastern_conference_divisions <- c("Atlantic Division", "Central Division", "Southeast Division")
  western_conference_divisions <- c("Northwest Division", "Pacific Division", "Southwest Division")
  
  # Create Conference and Division columns
  df <- df %>%
    mutate(
      Conference = case_when(
        str_detect(df$`Eastern Conference`, "Division") ~ "Eastern",
        str_detect(df$`Western Conference`, "Division") ~ "Western",
        TRUE ~ NA_character_
      ),
      Division = case_when(
        df$`Eastern Conference` %in% eastern_conference_divisions ~ str_replace(df$`Eastern Conference`, " Division", ""),
        df$`Western Conference` %in% western_conference_divisions ~ str_replace(df$`Western Conference`, " Division", ""),
        TRUE ~ NA_character_
      )
    ) %>%
    fill(Conference, Division, .direction = "down")  # Fill down Conference and Division values
  
  # Combine the team names into a single "Team" column and remove unnecessary columns
  df <- df %>%
    mutate(
      `Team Name` = coalesce(df$`Eastern Conference`, df$`Western Conference`)
    ) %>%
    select(`Team Name`, Conference, Division, W, L, `W/L%`, GB, `PS/G`, `PA/G`, SRS, URL, Season)
  
  return(df)
}

# Function to scrape multiple tables from a URL and bind them on top of each other
scrape_tables <- function(url) {
  max_retries <- 5
  wait_time <- 30  # Initial wait time set to 30 seconds
  
  for (i in 1:max_retries) {
    tryCatch({
      webpage <- read_html(url)
      
      # Extract both tables (Eastern Conference and Western Conference standings)
      table_east <- webpage %>%
        html_node("#divs_standings_E") %>%
        html_table()
      
      table_west <- webpage %>%
        html_node("#divs_standings_W") %>%
        html_table()
      
      # Clean column names and ensure specific column types are consistent
      table_east <- clean_colnames(table_east)
      table_west <- clean_colnames(table_west)
      
      # Combine both tables by binding rows (Eastern and Western standings together)
      combined_table <- bind_rows(table_east, table_west)
      
      # Extract the season from the URL and format it as "YYYY"
      season <- str_extract(url, "\\d{4}")
      season_formatted <- as.numeric(season)
      
      # Add the URL and Season to the table for reference
      combined_table <- combined_table %>%
        mutate(URL = url, Season = season_formatted)
      
      # Restructure the table to add Team, Conference, and Division columns
      combined_table <- restructure_table(combined_table)
      
      return(combined_table)
      
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
    table <- scrape_tables(url)
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
    write_csv(bind_rows(results), file.path(league_fp,"NBA_STANDINGS_partial.csv"))
    
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
nba_standings <- scrape_data_in_batches(nba_urls)

nba_standings <- nba_standings %>%
  mutate(`Made Playoffs` = ifelse(str_detect(`Team Name`, "\\*"), "Yes", "No"),
         `Team Name` = str_replace_all(`Team Name`, "\\s*\\(\\d+\\)", ""),
         `Team Name` = str_trim(`Team Name`),  # Ensure no extra spaces are left
         League = "NBA") %>%
  filter(!str_detect(`Team Name`, "Division")) %>%
  select(-`PS/G`, -`PA/G`, -URL)

# Ensure numeric conversion for W, L, W/L%, GB, and SRS
nba_standings <- nba_standings %>%
  mutate(
    W = as.numeric(W),
    L = as.numeric(L),
    `W/L%` = as.numeric(`W/L%`),
    GB = as.numeric(replace(GB, GB == "—", 0)),  # Replace '—' with 0 and convert to numeric
    GB = ifelse(!is.na(GB), round(GB, 1), GB),  # Round to 1 decimal place, keep as numeric
    SRS = as.numeric(SRS),
    G = W + L
  )

# Read in csv for League Info
league_info <- read_csv("C:/Users/djvia/OneDrive/Documents/Blog Website/Basketball_Database/NBA/LEAGUE/NBA_LEAGUE_INFO.csv")

# Joining to grab Team Abbreviations
nba_standings <- nba_standings %>%
  left_join(league_info %>%
              select(Team, `Team Name`, League, To) %>%
              filter(is.na(To)), 
            by = c("Team Name", "League")) %>%
  select(-To) %>%
  rename(`Team Abbr.` = Team)

# Add Division Rank (rank by W/L% within each Division, resolve ties by row order)
nba_standings <- nba_standings %>%
  group_by(Division) %>%
  mutate(`Division Rank` = rank(-`W/L%`, ties.method = "first")) %>%  # Rank in descending order, breaking ties by row order
  ungroup()

# Add Conference Rank (rank by W/L% within each Conference, resolve ties by row order)
nba_standings <- nba_standings %>%
  group_by(Conference) %>%
  mutate(`Conference Rank` = rank(-`W/L%`, ties.method = "first")) %>%  # Rank in descending order, breaking ties by row order
  ungroup()

# Reorganize so we can rebind the rows
nba_standings <- nba_standings %>%
  select(`Team Name`, `Team Abbr.`, G, W, L, `W/L%`, GB, SRS, League, Season, 
         Division, `Division Rank`, Conference, `Conference Rank`, `Made Playoffs`) %>%
  arrange(Conference,`Conference Rank`)

# Binding rows
nba_standings <- bind_rows(standings,nba_standings)

# Write csv and delete partial csv file
write_csv(nba_standings, file.path(league_fp,"NBA_STANDINGS.csv"))

file.remove(file.path(league_fp,"NBA_STANDINGS_partial.csv"))
