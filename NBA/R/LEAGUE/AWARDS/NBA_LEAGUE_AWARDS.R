# Author: David Vialpando-Nielsen
# Date Made: 8/27/2024
# Latest Update: 9/8/2024

# This file will contain scrape code for NBA awards

# Load necessary packages
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

# Define the file path for the saved CSV output
award_fp <- "C:/Users/djvia/OneDrive/Documents/Blog Website/Basketball_Database/NBA/LEAGUE/AWARDS"

# Other useful filepaths
player_fp <- "C:/Users/djvia/OneDrive/Documents/Blog Website/Basketball_Database/NBA/PLAYER/REGULAR SEASON/"

# Define the award URLs and corresponding HTML elements and award initials
award_urls <- list(
  list(url = "https://www.basketball-reference.com/awards/mvp.html", element = "#div_mvp_NBA", award = "MVP"),
  list(url = "https://www.basketball-reference.com/awards/roy.html", element = "#div_roy_NBA", award = "ROY"),
  list(url = "https://www.basketball-reference.com/awards/dpoy.html", element = "#div_dpoy_NBA", award = "DPOY"),
  list(url = "https://www.basketball-reference.com/awards/smoy.html", element = "#div_smoy_NBA", award = "6MOTY"),
  list(url = "https://www.basketball-reference.com/awards/mip.html", element = "#div_mip_NBA", award = "MIP"),
  list(url = "https://www.basketball-reference.com/awards/finals_mvp.html", element = "#div_finals_mvp_NBA", award = "FMVP"),
  list(url = "https://www.basketball-reference.com/awards/all_star_mvp.html", element = "#div_all_star_mvp_NBA", award = "ASMVP"),
  list(url = "https://www.basketball-reference.com/awards/tmoy.html", element = "#div_tmoy_NBA", award = "TSTY"),
  list(url = "https://www.basketball-reference.com/awards/citizenship.html", element = "#div_citizenship_NBA", award = "JWKCA"),
  list(url = "https://www.basketball-reference.com/awards/social_justice.html", element = "#div_social_justice_NBA", award = "SJC"),
  list(url = "https://www.basketball-reference.com/awards/sportsmanship.html", element = "#div_sportsmanship_NBA", award = "SMSA")
)

# Function to clean column names and ensure specific column types are consistent
clean_colnames <- function(df) {
  colnames(df) <- ifelse(colnames(df) == "" | is.na(colnames(df)), paste0("V", seq_along(colnames(df))), colnames(df))
  df <- df %>% mutate(across(everything(), as.character))
  return(df)
}

# Function to clear cache and cookies
clear_cache_and_cookies <- function(remDr) {
  remDr$deleteAllCookies()
  remDr$executeScript("window.localStorage.clear();")
  remDr$executeScript("window.sessionStorage.clear();")
}

# Function to scrape award data using RSelenium
scrape_award_data_selenium <- function(remDr, url, element, award, max_retries = 3) {
  for (i in 1:max_retries) {
    tryCatch({
      remDr$navigate(url)
      Sys.sleep(3)  # Wait for the page to load
      
      # Find the table element
      webElem <- remDr$findElement(using = "css", element)
      table_html <- webElem$getElementAttribute("outerHTML")[[1]]
      
      # Parse the HTML table using rvest
      table <- read_html(table_html) %>%
        html_table(fill = TRUE) %>%
        as.data.frame()
      
      # Clean column names
      table <- clean_colnames(table)
      
      # Ensure the Season column is retained
      if ("Season" %in% colnames(table)) {
        table <- table %>%
          mutate(Season = table$Season)
      }
      
      # Add Award and URL metadata
      table <- table %>%
        mutate(Award = award, URL = url)
      
      return(table)
      
    }, error = function(e) {
      message(paste("Error scraping URL:", url, "-", e$message))
      
      if (i == max_retries) {
        return(NULL)
      }
      
      Sys.sleep(5)  # Wait before retrying
    })
  }
}

# Function to restart Selenium
restart_selenium <- function() {
  remDr$close()
  rs_driver_object$server$stop()
  Sys.sleep(3)
  rs_driver_object <<- rsDriver(browser = 'chrome', extraCapabilities = eCaps, verbose = FALSE, port = free_port(), chromever = "128.0.6613.119")
  remDr <<- rs_driver_object$client
}

# Function to scrape all awards in batches
scrape_all_awards_selenium <- function(award_urls, remDr, batch_size = 5, restart_every = 10) {
  results <- list()
  p <- progressr::progressor(steps = length(award_urls))
  
  for (i in seq(1, length(award_urls), by = batch_size)) {
    batch_urls <- award_urls[i:min(i + batch_size - 1, length(award_urls))]
    
    for (award_info in batch_urls) {
      p(message = paste("Scraping URL:", award_info$url))
      table <- scrape_award_data_selenium(remDr, award_info$url, award_info$element, award_info$award)
      
      if (!is.null(table)) {
        results <- bind_rows(results, table)
      }
    }
    
    clear_cache_and_cookies(remDr)
    
    if (i %% restart_every == 0) {
      restart_selenium()
    }
    
    Sys.sleep(2)
  }
  
  write_csv(bind_rows(results), file.path(award_fp, "NBA_Awards_Data.csv"))
  
  return(results)
}

# Setup RSelenium with headless browsing enabled
eCaps <- list(chromeOptions = list(args = c('--headless', '--disable-gpu', '--window-size=1280,800')))
rs_driver_object <- rsDriver(browser = 'chrome', extraCapabilities = eCaps, verbose = FALSE, port = free_port(), chromever = "128.0.6613.119")
remDr <- rs_driver_object$client

# Enable global progress handlers
handlers(global = TRUE)

# Measure start time
start_time <- Sys.time()

# Run the scraping function
nba_awards_data <- with_progress({
  scrape_all_awards_selenium(award_urls, remDr)
})

# Measure end time and calculate total time taken
end_time <- Sys.time()
total_time <- end_time - start_time
total_minutes <- as.numeric(total_time, units = "mins")

message(paste("Total scraping time:", round(total_minutes, 2), "minutes"))

# Close the RSelenium session
remDr$close()
rs_driver_object$server$stop()

# Function to adjust specific columns for certain awards
adjust_column_content_by_award <- function(df) {
  df <- df %>%
    mutate(
      # For FMVP, move Var.5 content to Var.6
      Var.6 = ifelse(Award == "FMVP", Var.5, Var.6),
      # For ASMVP, move Var.4 content to Var.6
      Var.6 = ifelse(Award == "ASMVP", Var.4, Var.6),
      # For SJC, move Var.5 content to Var.6
      Var.6 = ifelse(Award == "SJC", Var.5, Var.6),
      # For SMSA, move Var.5 content to Var.6
      Var.6 = ifelse(Award == "SMSA", Var.5, Var.6)
    )
  return(df)
}

# Apply the adjust_column_content_by_award function to the entire dataframe
nba_awards_data <- nba_awards_data %>%
  adjust_column_content_by_award()

# Rename columns and grabbing only necessary columns
nba_awards_data <- nba_awards_data %>%
  rename(Season = Var.1,
         Player = Var.3,
         `Team Abbr.` = Var.6) %>%
  select(Season, Player, Award)

# Filtering out unnecessary columns
nba_awards_data <- nba_awards_data %>%
  filter(Season != "" & Season != "Season")

# Reformatting Player and Season columns
nba_awards_data <- nba_awards_data %>%
  mutate(Player = str_replace_all(Player, "\\*", ""),
         Player = str_replace_all(Player, "\\(Tie\\)", ""),
         Player = str_trim(Player),
         Season = str_extract(Season, "\\d{4}"),
         Season = paste0(Season, "-", as.numeric(Season) + 1))

# Load in player reg roster and league info for ids
player_index <- read_csv("C:/Users/djvia/OneDrive/Documents/Blog Website/Basketball_Database/MISCELLANEOUS/NBA_ABA_PLAYER_INDEX.csv") %>%
  rename(College = Colleges)

league_info <- read_csv("C:/Users/djvia/OneDrive/Documents/Blog Website/Basketball_Database/NBA/LEAGUE/NBA_LEAGUE_INFO.csv")

load(file.path(player_fp, "NBA_PLAYER_REG_ROSTER.rda"))

# Assigning Player IDs to players
nba_awards_data <- nba_awards_data %>%
  left_join(player_index %>% select(`Player ID`, Player),
            by = c("Player"),
            relationship = "many-to-many")

# List of correct player IDs for specific players
correct_player_ids <- tibble(
  Player = c("Larry Johnson", "Patrick Ewing", "Eddie Johnson", "Bobby Jones"),
  `Player ID` = c(3662, 3231, 3000, 2578)
)

# Ensure specific players have the correct Player IDs
nba_awards_data <- nba_awards_data %>%
  left_join(correct_player_ids, by = "Player", suffix = c("", "_correct")) %>%
  mutate(`Player ID` = ifelse(!is.na(`Player ID_correct`), `Player ID_correct`, `Player ID`)) %>%
  select(-`Player ID_correct`)

# Identify duplicates based on Player, Season, and Award
nba_duplicates <- nba_awards_data %>%
  group_by(Player, Season, Award) %>%
  filter(n() > 1)

# Filter duplicates, keeping rows with the correct Player ID for specific players
nba_duplicates_filtered <- nba_duplicates %>%
  filter(`Player ID` %in% correct_player_ids$`Player ID` | !duplicated(paste(Player, Season, Award))) %>%
  distinct()

# Define the custom order for the Award column
award_order <- c("MVP", "ROY", "DPOY", "6MOTY", "MIP", "FMVP", "ASMVP", "TSTY", "JWKCA", "SJC", "SMSA")

# Remove the original duplicates from nba_awards_data and bind back the filtered duplicates
nba_awards_data <- nba_awards_data %>%
  anti_join(nba_duplicates, by = c("Player", "Season", "Award")) %>%
  bind_rows(nba_duplicates_filtered) %>%
  mutate(Award = factor(Award, levels = award_order)) %>%
  arrange(desc(Season), Award)

# Join through roster now since player id is valid now
nba_awards_data <- nba_awards_data %>%
  left_join(nba_reg_roster %>%
              select(`Player ID`, Player, Season, `Franchise ID`,
                     `Team Abbr.`, `Team Name`),
            by = c("Player ID", "Player", "Season"),
            relationship = "many-to-many")

# Save Player Awards
save(nba_awards_data,file = file.path(award_fp,"NBA_LEAGUE_PLAYER_AWARDS.rda"))

# Display message to confirm save
print("nba_awards_data table has been saved to NBA_LEAGUE_PLAYER_AWARDS.rda")

# Delete the partial RDA file
file.remove(file.path(award_fp,"NBA_Awards_Data.csv"))

# COY Specifically

# Define the COY URL and HTML element
coach_url <- "https://www.basketball-reference.com/awards/coy.html"
element <- "#coyNBA"  # HTML element for the COY table

# Function to clean column names and ensure specific column types are consistent
clean_colnames <- function(df) {
  colnames(df) <- ifelse(colnames(df) == "" | is.na(colnames(df)), paste0("V", seq_along(colnames(df))), colnames(df))
  
  # Convert all columns to character to avoid type mismatches
  df <- df %>% mutate(across(everything(), as.character))
  
  return(df)
}

# Function to scrape the COY table using RSelenium
scrape_coach_data_selenium <- function(remDr, url, element, max_retries = 3) {
  for (i in 1:max_retries) {
    tryCatch({
      # Navigate to the URL
      remDr$navigate(url)
      
      # Wait for the table to appear
      Sys.sleep(3)  # Wait for the page to load
      
      # Find the table element
      webElem <- remDr$findElement(using = "css", element)
      table_html <- webElem$getElementAttribute("outerHTML")[[1]]
      
      # Parse the HTML table using rvest
      table <- read_html(table_html) %>%
        html_table(fill = TRUE) %>%
        as.data.frame()
      
      # Clean column names and ensure specific column types are consistent
      table <- clean_colnames(table)
      
      # Format the Season column if it exists by adding 1 to the year on the left
      if ("Season" %in% colnames(table)) {
        table <- table %>%
          mutate(Season = str_extract(Season, "\\d{4}") %>% as.numeric(),
                 Season = paste0(Season, "-", Season + 1))
      }
      
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

# RSelenium setup with headless browsing enabled
eCaps <- list(chromeOptions = list(args = c('--headless', '--disable-gpu', '--window-size=1280,800')))
rs_driver_object <- rsDriver(browser = 'chrome', extraCapabilities = eCaps, verbose = FALSE, port = free_port(), chromever = "128.0.6613.119")
remDr <- rs_driver_object$client

# Start progress handling
handlers(global = TRUE)  # Enable global progress handlers

# Measure start time
start_time <- Sys.time()

# Scrape the COY data
coach_data <- with_progress({
  scrape_coach_data_selenium(remDr, coach_url, element)
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

coach_award_data <- coach_data %>%
  filter(Lg != "Lg") %>%
  select(Season, Coach, Tm) %>%
  rename(`Team Abbr.` = Tm)

# Coach IDs
coach_index <- read_csv("C:/Users/djvia/OneDrive/Documents/Blog Website/Basketball_Database/MISCELLANEOUS/NBA_ABA_COACH_INDEX.csv")

coach_award_data <- coach_award_data %>%
  left_join(coach_index %>%
              select(`Coach ID`, Coach),
            by = "Coach") %>%
  mutate(Award = "COY")

# Save Coach Awards
save(coach_award_data,file = file.path(award_fp,"NBA_LEAGUE_COACH_AWARDS.rda"))

# Display message to confirm save
print("coach_award_data table has been saved to NBA_LEAGUE_COACH_AWARDS.rda")
