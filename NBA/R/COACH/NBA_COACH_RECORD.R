# Author: David Vialpando-Nielsen
# Date Made: 9/8/2024
# Latest Update: 9/8/2024

# This file will contain scrape code for coach records across regular season, playoffs, and combined

# Library Packages
library(tidyverse)
library(stringr)
library(lubridate)
library(readr)
library(rvest)
library(hoopR)

# File directory for the coach file
coach_fp <- "C:/Users/djvia/OneDrive/Documents/Blog Website/Basketball_Database/NBA/COACH"

# Function to generate the URLs for coaches from 1947 (BAA) to the most recent NBA season
generate_coach_urls <- function() {
  
  # Initialize the start and end seasons
  start_season <- 1947  # BAA starts in 1947
  end_season <- most_recent_nba_season()  # Get the most recent NBA season
  
  # Create an empty list to store the URLs
  coach_urls <- c()
  
  # Loop through each season year
  for (season_year in start_season:end_season) {
    
    # Determine if it's BAA or NBA based on the year
    if (season_year < 1950) {
      league <- "BAA"
    } else {
      league <- "NBA"
    }
    
    # Construct the URL
    url <- paste0("https://www.basketball-reference.com/leagues/", league, "_", season_year, "_coaches.html")
    
    # Add the URL to the list
    coach_urls <- c(coach_urls, url)
  }
  
  return(coach_urls)
}

# Generate the list of URLs
coach_urls <- generate_coach_urls()

# Function to clean column names and ensure specific column types are consistent
clean_colnames <- function(df) {
  # Replace empty or NA column names
  colnames(df) <- ifelse(colnames(df) == "" | is.na(colnames(df)), paste0("V", seq_along(colnames(df))), colnames(df))
  
  # Make column names unique to avoid duplicates
  colnames(df) <- make.unique(colnames(df))
  
  return(df)
}

# Function to scrape the specific table from a URL
scrape_table <- function(url) {
  max_retries <- 5
  wait_time <- 30  # Initial wait time set to 30 seconds
  
  for (i in 1:max_retries) {
    tryCatch({
      webpage <- read_html(url)
      
      # Determine if it's a BAA or NBA URL and select the correct node
      node <- ifelse(grepl("BAA", url), "#div_BAA_coaches", "#div_NBA_coaches")
      
      # Extract the table
      table <- webpage %>%
        html_node(node) %>%
        html_table(fill = TRUE)
      
      # Clean column names
      table <- clean_colnames(table)
      
      # Extract the season from the URL and format it as "YYYY-YYYY"
      season <- str_extract(url, "\\d{4}")
      season <- as.numeric(season)
      season_formatted <- paste(season - 1, season, sep = "-")
      
      # Add the Season and URL to the table for reference
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
  
  # If we reach here, it means retries have been exhausted
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

# Function to scrape data in batches and save progress to a partial CSV
scrape_data_in_batches <- function(coach_urls, coach_fp, batch_size = 30) {
  total_urls <- length(coach_urls)
  num_batches <- ceiling(total_urls / batch_size)
  results <- list()
  
  # Measure the start time
  start_time <- Sys.time()
  
  for (i in 1:num_batches) {
    start_index <- (i - 1) * batch_size + 1
    end_index <- min(i * batch_size, total_urls)
    batch_urls <- coach_urls[start_index:end_index]
    
    timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
    message(paste("Processing batch", i, "of", num_batches, "at", timestamp))
    
    # Scrape the current batch
    batch_results <- scrape_batch(batch_urls)
    
    if (length(batch_results) > 0) {
      results <- bind_rows(results, batch_results)
    }
    
    # Save progress after each batch
    write_csv(bind_rows(results), file.path(coach_fp, "NBA_COACH_RECORD_partial.csv"))
    
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

# Scrape the data in batches
coach_reg <- scrape_data_in_batches(coach_urls, coach_fp, batch_size = 30)

# Clean up data frame
coach_reg <- coach_reg %>%
  select(-V3, -V6, -V17) %>%
  rename(`Coach` = V1,
         `Team Abbr.` = V2,
         `Seasons w/ Franchise` = Seasons,
         `Seasons Overall` = Seasons.1,
         `Current Reg. Season Games` = `Regular Season`,
         `Current Reg. Season Wins` = `Regular Season.1`,
         `Current Reg. Season Losses` = `Regular Season.2`,
         `Franchise Reg. Season Games` = `Regular Season.3`,
         `Franchise Reg. Season Wins` = `Regular Season.4`,
         `Franchise Reg. Season Losses` = `Regular Season.5`,
         `Career Reg. Season Games` = `Regular Season.6`,
         `Career Reg. Season Wins` = `Regular Season.7`,
         `Career Reg. Season Losses` = `Regular Season.8`,
         `Career Reg. Season W%` = `Regular Season.9`,
         `Current Playoffs Games` = `Playoffs`,
         `Current Playoffs Wins` = `Playoffs.1`,
         `Current Playoffs Losses` = `Playoffs.2`,
         `Franchise Playoffs Games` = `Playoffs.3`,
         `Franchise Playoffs Wins` = `Playoffs.4`,
         `Franchise Playoffs Losses` = `Playoffs.5`,
         `Career Playoffs Games` = `Playoffs.6`,
         `Career Playoffs Wins` = `Playoffs.7`,
         `Career Playoffs Losses` = `Playoffs.8`) %>%
  filter(Coach != "" & Coach != "Coach")

# List of columns to exclude from conversion
exclude_columns <- c('Coach', 'Team Abbr.', 'URL', 'Season')

# Convert all columns except the excluded ones to numeric
coach_reg <- coach_reg %>%
  mutate(across(-all_of(exclude_columns), ~ as.numeric(replace_na(replace(., . == "", 0), 0))))

# Modify the 'Coach' column with specific name changes
coach_reg <- coach_reg %>%
  mutate(Coach = case_when(
    Coach == "Honey Russell" ~ "John Russell",
    Coach == "Charley Eckman" ~ "Charles Eckman",
    Coach == "Jordi Fernandez" ~ "Jordi Fernández Torres",
    TRUE ~ Coach  # Keep other names unchanged
  ))

# This column will tell us if the coach was the coach by the end of the season
coach_reg <- coach_reg %>%
  group_by(`Team Abbr.`, Season) %>%
  mutate(`EOS Coach` = ifelse(row_number() == n(), "Yes", "No")) %>%
  ungroup()

# Grabbing only regular season for dataframe and dropping URL, make season end column
nba_coach_reg <- coach_reg %>%
  select(-matches("Playoffs"), -URL) %>%
  mutate(`Season End` = as.numeric(str_extract(Season, "\\d{4}$")))

# Load in csv for Coach Index and League Info
coach_index <- read_csv("C:/Users/djvia/OneDrive/Documents/Blog Website/Basketball_Database/MISCELLANEOUS/NBA_ABA_COACH_INDEX.csv")

league_info <- read_csv("C:/Users/djvia/OneDrive/Documents/Blog Website/Basketball_Database/NBA/LEAGUE/NBA_LEAGUE_INFO.csv")

# Perform the left join on 'Coach' and filter rows where 'Season End' is between 'From' and 'To' in the index
nba_coach_reg <- nba_coach_reg %>%
  left_join(coach_index %>%
              select(`Coach ID`, Coach, To, From), by = "Coach",
            relationship = 'many-to-many') %>%
  filter(`Season End` >= From & `Season End` <= To) %>%
  select(`Coach ID`, everything(), -To, -From, -`Season End`)

# Perform the left join with league_info on 'Team Abbr.'
nba_coach_reg <- nba_coach_reg %>%
  left_join(league_info %>% select(Team, `Franchise ID`, `Team Name`),
            by = c("Team Abbr." = "Team"),
            relationship = "many-to-many") %>%
  distinct()

# Make final reg season data frame and last Win % columns
nba_coach_reg <- nba_coach_reg %>%
  select(`Coach ID`, Coach, `Franchise ID`, `Team Abbr.`, `Team Name`, Season,
         everything()) %>%
  mutate(`Current Reg. Season W%` = round(`Current Reg. Season Wins` / `Current Reg. Season Games`, 3),
         `Franchise Reg. Season W%` = round(`Franchise Reg. Season Wins` / `Franchise Reg. Season Games`, 3)) %>%
  relocate(`Current Reg. Season W%`, .after =`Current Reg. Season Losses`) %>%
  relocate(`Franchise Reg. Season W%`, .after =`Franchise Reg. Season Losses`) %>%
  arrange(`Team Name`, desc(Season))

# Save regular season data frame to a rda file
save(nba_coach_reg,file = file.path(coach_fp,"NBA_COACH_REG_RECORD.rda"))

# Display message to confirm save
print("nba_coach_reg table has been saved to NBA_COACH_REG_RECORD.rda")

# Playoff Records for Coaches
nba_coach_playoff <- coach_reg %>%
  select(-matches("Reg. Season"), -URL) %>%
  mutate(`Season End` = as.numeric(str_extract(Season, "\\d{4}$")))

# Perform the left join on 'Coach' and filter rows where 'Season End' is between 'From' and 'To' in the index
nba_coach_playoff <- nba_coach_playoff %>%
  left_join(coach_index %>%
              select(`Coach ID`, Coach, To, From), by = "Coach",
            relationship = 'many-to-many') %>%
  filter(`Season End` >= From & `Season End` <= To) %>%
  select(`Coach ID`, everything(), -To, -From, -`Season End`)

# Perform the left join with league_info on 'Team Abbr.'
nba_coach_playoff <- nba_coach_playoff %>%
  left_join(league_info %>% select(Team, `Franchise ID`, `Team Name`),
            by = c("Team Abbr." = "Team"),
            relationship = "many-to-many") %>%
  distinct()

# Make final reg season data frame and last Win % columns
nba_coach_playoff <- nba_coach_playoff %>%
  select(`Coach ID`, Coach, `Franchise ID`, `Team Abbr.`, `Team Name`, Season,
         everything()) %>%
  mutate(`Current Playoffs W%` = round(`Current Playoffs Wins` / `Current Playoffs Games`, 3),
         `Franchise Playoffs W%` = round(`Franchise Playoffs Wins` / `Franchise Playoffs Games`, 3),
         `Career Playoffs W%` = round(`Career Playoffs Wins` / `Career Playoffs Games`, 3)) %>%
  relocate(`Current Playoffs W%`, .after =`Current Playoffs Losses`) %>%
  relocate(`Franchise Playoffs W%`, .after =`Franchise Playoffs Losses`) %>%
  relocate(`Career Playoffs W%`, .after =`Career Playoffs Losses`) %>%
  arrange(`Team Name`, desc(Season))

# Save playoff data frame to a rda file
save(nba_coach_playoff,file = file.path(coach_fp,"NBA_COACH_PLAYOFF_RECORD.rda"))

# Display message to confirm save
print("nba_coach_playoff table has been saved to NBA_COACH_PLAYOFF_RECORD.rda")

# Career Records
nba_coach_career <- coach_reg %>%
  select(-matches("Current|Franchise"), -URL) %>%
  mutate(`Season End` = as.numeric(str_extract(Season, "\\d{4}$")),
         `Career Playoffs W%` = round(`Career Playoffs Wins` / `Career Playoffs Games`, 3)) %>%
  relocate(`Career Playoffs W%`, .after =`Career Playoffs Losses`)

# Perform the left join on 'Coach' and filter rows where 'Season End' is between 'From' and 'To' in the index
nba_coach_career <- nba_coach_career %>%
  left_join(coach_index %>%
              select(`Coach ID`, Coach, To, From), by = "Coach",
            relationship = 'many-to-many') %>%
  filter(`Season End` >= From & `Season End` <= To) %>%
  select(`Coach ID`, everything(), -To, -From, -`Season End`)

# Perform the left join with league_info on 'Team Abbr.'
nba_coach_career <- nba_coach_career %>%
  left_join(league_info %>% select(Team, `Franchise ID`, `Team Name`),
            by = c("Team Abbr." = "Team"),
            relationship = "many-to-many") %>%
  distinct()

# All time columns
nba_coach_career <- nba_coach_career %>%
  select(`Coach ID`, Coach, `Franchise ID`, `Team Abbr.`, `Team Name`, Season,
         everything(), -`EOS Coach`) %>%
  group_by(`Coach ID`, Coach) %>%
  summarize(
    # Count unique seasons
    Seasons = n_distinct(Season),
    
    # Taking the max values for career records
    `Career Reg. Season Games` = max(`Career Reg. Season Games`, na.rm = TRUE),
    `Career Reg. Season Wins` = max(`Career Reg. Season Wins`, na.rm = TRUE),
    `Career Reg. Season Losses` = max(`Career Reg. Season Losses`, na.rm = TRUE),
    
    # Recalculate career regular season win percentage
    `Career Reg. Season W%` = round(sum(`Career Reg. Season Wins`, na.rm = TRUE) / sum(`Career Reg. Season Games`, na.rm = TRUE), 3),
    
    # Taking the max values for playoff records
    `Career Playoffs Games` = max(`Career Playoffs Games`, na.rm = TRUE),
    `Career Playoffs Wins` = max(`Career Playoffs Wins`, na.rm = TRUE),
    `Career Playoffs Losses` = max(`Career Playoffs Losses`, na.rm = TRUE),
    
    # Recalculate career playoffs win percentage
    `Career Playoffs W%` = round(sum(`Career Playoffs Wins`, na.rm = TRUE) / sum(`Career Playoffs Games`, na.rm = TRUE), 3)
  ) %>%
  ungroup()

# Save career data frame to a rda file
save(nba_coach_career,file = file.path(coach_fp,"NBA_COACH_CAREER_RECORD.rda"))

# Display message to confirm save
print("nba_coach_playoff table has been saved to NBA_COACH_CAREER_RECORD.rda")

# Last Career file but by franchise
nba_coach_franchise_career <- coach_reg %>%
  select(-matches("Current|Franchise"), -URL) %>%
  mutate(`Season End` = as.numeric(str_extract(Season, "\\d{4}$")),
         `Career Playoffs W%` = round(`Career Playoffs Wins` / `Career Playoffs Games`, 3)) %>%
  relocate(`Career Playoffs W%`, .after =`Career Playoffs Losses`)

# Perform the left join on 'Coach' and filter rows where 'Season End' is between 'From' and 'To' in the index
nba_coach_franchise_career <- nba_coach_franchise_career %>%
  left_join(coach_index %>%
              select(`Coach ID`, Coach, To, From), by = "Coach",
            relationship = 'many-to-many') %>%
  filter(`Season End` >= From & `Season End` <= To) %>%
  select(`Coach ID`, everything(), -To, -From, -`Season End`)

# Perform the left join with league_info on 'Team Abbr.'
nba_coach_franchise_career <- nba_coach_franchise_career %>%
  left_join(league_info %>% select(Team, `Franchise ID`, `Team Name`),
            by = c("Team Abbr." = "Team"),
            relationship = "many-to-many") %>%
  distinct()

# All time columns
nba_coach_franchise_career <- nba_coach_franchise_career %>%
  select(`Coach ID`, Coach, `Franchise ID`, `Team Abbr.`, `Team Name`, Season,
         everything(), -`EOS Coach`) %>%
  group_by(`Coach ID`, Coach, `Franchise ID`, `Team Abbr.`, `Team Name`) %>%
  summarize(
    # Count unique seasons based on a coach's tenure with the franchise
    Seasons = n_distinct(Season),
    
    # Taking the max values for career records
    `Career Reg. Season Games` = max(`Career Reg. Season Games`, na.rm = TRUE),
    `Career Reg. Season Wins` = max(`Career Reg. Season Wins`, na.rm = TRUE),
    `Career Reg. Season Losses` = max(`Career Reg. Season Losses`, na.rm = TRUE),
    
    # Recalculate career regular season win percentage
    `Career Reg. Season W%` = round(sum(`Career Reg. Season Wins`, na.rm = TRUE) / sum(`Career Reg. Season Games`, na.rm = TRUE), 3),
    
    # Taking the max values for playoff records
    `Career Playoffs Games` = max(`Career Playoffs Games`, na.rm = TRUE),
    `Career Playoffs Wins` = max(`Career Playoffs Wins`, na.rm = TRUE),
    `Career Playoffs Losses` = max(`Career Playoffs Losses`, na.rm = TRUE),
    
    # Recalculate career playoffs win percentage
    `Career Playoffs W%` = round(sum(`Career Playoffs Wins`, na.rm = TRUE) / sum(`Career Playoffs Games`, na.rm = TRUE), 3)
  ) %>%
  ungroup()

# Save career by franchise data frame to a rda file
save(nba_coach_franchise_career,file = file.path(coach_fp,"NBA_COACH_CAREER_FRANCHISE_RECORD.rda"))

# Display message to confirm save
print("nba_coach_franchise_career table has been saved to NBA_COACH_CAREER_FRANCHISE_RECORD.rda")

# Delete the partial RDA file
file.remove(file.path(coach_fp,"NBA_COACH_RECORD_partial.csv"))