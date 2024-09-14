# Author: David Vialpando-Nielsen
# Date Made: 9/7/2024
# Latest Update: 9/7/2024

# This file will contain scrape code for team total traditional stats throughout NBA history

# Library Packages
library(tidyverse)
library(hoopR)
library(stringr)
library(lubridate)
library(readr)
library(rvest)

# File path for the team file
team_fp <- "C:/Users/djvia/OneDrive/Documents/Blog Website/Basketball_Database/NBA/TEAM/PLAYOFFS"

# Create nba urls since the playoff summary is different from the league urls
Year <- 1947:most_recent_nba_season()

nba_urls <- tibble(Year = Year) %>%
  mutate(League = if_else(Year < 1950, "BAA", "NBA"),
         URL = paste0("https://www.basketball-reference.com/playoffs/", 
                      League, "_", Year, ".html"))


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
      
      # Extract the table with ID #div_totals-team
      table <- webpage %>%
        html_node("#div_totals-team") %>%
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
    write_csv(bind_rows(results), file.path(team_fp,"NBA_TEAM_PLAYOFFS_TOTAL_partial.csv"))
    
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
nba_team_ply_total <- scrape_data_in_batches(nba_urls)

# Drops rows that have Rk that contain NA and drop the entire column afterwards
nba_team_ply_total <- nba_team_ply_total %>%
  filter(!is.na(Rk)) %>%
  select(-Rk)

# Dropping rows with literally nothing to do with the playoffs (for some reason in 2023)
# Also rename hornets in 2014 to bobcats

nba_team_ply <- nba_team_ply_total %>%
mutate(Team = if_else(is.na(Team), Tm, Team),
       Team = if_else(Team == "Seattle Supersonics", "Seattle SuperSonics", Team),
       Team = if_else(Team == "Charlotte Hornets" & Season == "2013-2014", "Charlotte Bobcats", Team)) %>%
  select(-Tm) %>%
  filter(!is.na(PTS))

# Read in csv for League Info
league_info <- read_csv("C:/Users/djvia/OneDrive/Documents/Blog Website/Basketball_Database/NBA/LEAGUE/NBA_LEAGUE_INFO.csv")

# Arrange by earliest season and joining to have Team Abbr.
nba_team_ply <- nba_team_ply %>%
  rename(`Team Name` = Team) %>%
  mutate( From = as.numeric(substr(nba_team_ply$Season,1,4)),
          To = as.numeric(substr(nba_team_ply$Season,6,9))) %>%
  left_join(league_info %>%
              select(`Franchise ID`, Team, `Team Name`, From, To), 
            by = c('Team Name'), relationship = 'many-to-many') %>%
  filter(To.x >= From.y & (is.na(To.y) | To.x <= To.y)) %>%
  select(-To.x, -To.y, -From.x,-From.y) %>%
  rename(`Team Abbr.` = Team) %>%
  select(`Franchise ID`,`Team Name`, `Team Abbr.`, Season, everything()) %>%
  select(-URL)

# Read in standings data to join into nba_team_ply
nba_standings <- read_csv("C:/Users/djvia/OneDrive/Documents/Blog Website/Basketball_Database/NBA/LEAGUE/NBA_PLAYOFF_SERIES.csv")

# Make Season End and joining to nba_standings
nba_team_ply <- nba_team_ply %>%
  mutate(Season_End = as.numeric(str_extract(Season, "\\d{4}$"))) %>%
  left_join(nba_standings %>% select(`Team Abbr.`, Season, Division, 
                                     `Division Rank`, Conference, `Conference Rank`, 
                                     `Play-In`, `Win Play-In`,`First Round Wins`,
                                     `First Round Losses`,`First Round Opp.`,
                                     `Won First Round`,`Second Round Wins`,
                                     `Second Round Losses`,`Second Round Opp.`,
                                     `Won Second Round`,`Semifinals Wins`,
                                     `Semifinals Losses`,`Semifinals Opp.`,
                                     `Won Semifinals`,`Finals Wins`,`Finals Losses`,
                                     `Finals Opp.`,`Won Finals`),
            by = c("Team Abbr.", "Season_End" = "Season")) %>%
  select(-Season_End) %>%
  mutate(W = rowSums(select(., `First Round Wins`, `Second Round Wins`, 
                       `Semifinals Wins`, `Finals Wins`), na.rm = TRUE),
         L = rowSums(select(., `First Round Losses`, `Second Round Losses`, 
                       `Semifinals Losses`, `Finals Losses`), na.rm = TRUE),
         `W/L%` = round(W / (W + L),3))

# Rearrange columns for final dataframe %>%
nba_team_ply_total <- nba_team_ply %>%
  select(`Franchise ID`,`Team Name`, `Team Abbr.`, Season, G, W, L, `W/L%`,
         Division, `Division Rank`, Conference, `Conference Rank`, everything()) %>%
  arrange(`Team Name`, desc(Season))

# Save per game data frame to a rda file
save(nba_team_ply_total,file = file.path(team_fp,"NBA_TEAM_PLAYOFF_TOTAL.rda"))

# Display message to confirm save
print("nba_team_ply_total table has been saved to NBA_TEAM_PLAYOFF_TOTAL.rda")

# Delete the partial RDA file
file.remove(file.path(team_fp,"NBA_TEAM_PLAYOFFS_TOTAL_partial.csv"))
