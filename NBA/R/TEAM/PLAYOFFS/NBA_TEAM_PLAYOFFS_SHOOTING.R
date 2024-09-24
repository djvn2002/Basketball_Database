# Author: David Vialpando-Nielsen
# Date Made: 9/23/2024
# Latest Update: 9/23/2024

# This file will contain scrape code for team shooting data in the playoffs
# Library Packages
library(tidyverse)
library(stringr)
library(lubridate)
library(readr)
library(rvest)
library(hoopR)
library(furrr)
library(RSelenium)
library(progressr)
library(netstat)
library(wdman)

# File path for the team file
team_fp <- "C:/Users/djvia/OneDrive/Documents/Blog Website/Basketball_Database/NBA/TEAM/PLAYOFFS"

# Create nba urls since the playoff summary is different from the league urls
Year <- 1947:most_recent_nba_season()

nba_urls <- tibble(Year = Year) %>%
  mutate(League = if_else(Year < 1950, "BAA", "NBA"),
         URL = paste0("https://www.basketball-reference.com/playoffs/", 
                      League, "_", Year, ".html")) %>%
  filter(Year >= 1997)

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

# Function to clear cache and cookies
clear_cache_and_cookies <- function(remDr) {
  remDr$deleteAllCookies()  # Clear cookies
  remDr$executeScript("window.localStorage.clear();")  # Clear local storage
  remDr$executeScript("window.sessionStorage.clear();")  # Clear session storage
}

# Function to scrape the specific table using RSelenium
scrape_table_selenium <- function(remDr, url, max_retries = 3) {
  for (i in 1:max_retries) {
    tryCatch({
      # Navigate to the URL
      remDr$navigate(url)
      
      # Wait for the table to appear
      Sys.sleep(3)  # Wait for the page to load
      
      # Find the table element
      webElem <- remDr$findElement(using = "css", "#div_shooting-team")
      table_html <- webElem$getElementAttribute("outerHTML")[[1]]
      
      # Parse the HTML table using rvest
      table <- read_html(table_html) %>%
        html_table(fill = TRUE) %>%
        as.data.frame()
      
      # Clean column names and ensure specific column types are consistent
      table <- clean_colnames(table)
      
      # Add additional metadata
      season <- str_extract(url, "\\d{4}") %>% as.numeric()
      season_formatted <- paste(season - 1, season, sep = "-")
      table <- table %>%
        mutate(URL = url, Season = season_formatted)
      
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

# Function to restart the Selenium driver
restart_selenium <- function() {
  remDr$close()
  rs_driver_object$server$stop()
  Sys.sleep(3)  # Allow the system to close processes
  rs_driver_object <<- rsDriver(browser = 'chrome', extraCapabilities = eCaps, verbose = FALSE, port = free_port(), chromever = "128.0.6613.119")
  remDr <<- rs_driver_object$client
}

# Function to scrape all URLs in batches, clear cache/cookies periodically, and restart Selenium
scrape_all_urls_selenium <- function(nba_urls, remDr, batch_size = 100, restart_every = 500) {
  results <- list()
  
  # Initialize the progress bar
  p <- progressr::progressor(steps = nrow(nba_urls))
  
  # Scrape each URL one at a time in batches
  for (i in seq(1, nrow(nba_urls), by = batch_size)) {
    batch_urls <- nba_urls$URL[i:min(i + batch_size - 1, nrow(nba_urls))]
    
    for (url in batch_urls) {
      p(message = paste("Scraping URL:", url))  # Update the progress bar
      
      table <- scrape_table_selenium(remDr, url)
      
      if (!is.null(table)) {
        results <- bind_rows(results, table)
      }
    }
    
    # Clear cache and cookies periodically
    clear_cache_and_cookies(remDr)
    
    # Restart Selenium periodically
    if (i %% restart_every == 0) {
      restart_selenium()
    }
    
    Sys.sleep(2)  # Small delay to ease memory usage
  }
  
  # Save the final result after scraping all URLs
  write_csv(bind_rows(results), file.path(team_fp, "NBA_TEAM_PLAYOFF_SHOOTING_partial.csv"))
  
  return(results)
}

# RSelenium setup with headless browsing enabled
eCaps <- list(chromeOptions = list(args = c('--headless', '--disable-gpu', '--window-size=1280,800')))
rs_driver_object <- rsDriver(browser = 'chrome', extraCapabilities = eCaps, verbose = FALSE, port = free_port(), chromever = "128.0.6613.119")
remDr <- rs_driver_object$client

# Start progress handling
handlers(global = TRUE)  # Enable global progress handlers

# Measure start time
start_time <- Sys.time()

# Run the scraping function with progress bar and batch processing
nba_ply_shooting_parallel <- with_progress({
  scrape_all_urls_selenium(nba_urls, remDr)
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

# Rename columns and cleaning them for consistency
nba_team_ply_shooting <- nba_ply_shooting_parallel %>%
  rename(Rk = Var.1,
         Team = Var.2,
         G = Var.3,
         MP = Var.4,
         `FG%` = Var.5,
         `Avg. Distance` = Var.6,
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
         `3P% Corner 3s` = Corner.1,
         `Heaves Attempted` = Heaves,
         `Heaves Made` = Heaves.1) %>%
  select(-Var.7, -Var.14, -Var.21, -Var.24, -Var.27, 
         -Var.30, -Var.8, -Var.13, -Var.15, -Var.20) %>%
  filter(Rk != "Rk" & Team != "League Average") %>%
  select(-Rk) %>%
  mutate(across(-c(Team, URL, Season), as.numeric),
         Team = if_else(Team == "Seattle Supersonics", "Seattle SuperSonics", Team),
         Team = if_else(Team == "Charlotte Hornets" & Season == "2013-2014", "Charlotte Bobcats", Team))

# Read in csv for League Info
league_info <- read_csv("C:/Users/djvia/OneDrive/Documents/Blog Website/Basketball_Database/NBA/LEAGUE/NBA_LEAGUE_INFO.csv")

# Arrange by earliest season and joining to have Team Abbr.
nba_team_ply_shooting <- nba_team_ply_shooting %>%
  rename(`Team Name` = Team) %>%
  mutate( From = as.numeric(substr(nba_team_ply_shooting$Season,1,4)),
          To = as.numeric(substr(nba_team_ply_shooting$Season,6,9))) %>%
  left_join(league_info %>%
              select(`Franchise ID`, Team, `Team Name`, From, To), 
            by = c('Team Name'), relationship = 'many-to-many') %>%
  filter(To.x >= From.y & (is.na(To.y) | To.x <= To.y)) %>%
  select(-To.x, -To.y, -From.x,-From.y) %>%
  rename(`Team Abbr.` = Team) %>%
  select(`Franchise ID`,`Team Name`, `Team Abbr.`, Season, everything()) %>%
  select(-URL)                                    

# Read in standings data to join into nba_team_ply_shooting
nba_standings <- read_csv("C:/Users/djvia/OneDrive/Documents/Blog Website/Basketball_Database/NBA/LEAGUE/NBA_PLAYOFF_SERIES.csv")

# Make Season End and joining to nba_standings
nba_team_ply_shooting <- nba_team_ply_shooting %>%
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
nba_team_ply_shooting <- nba_team_ply_shooting %>%
  select(`Franchise ID`,`Team Name`, `Team Abbr.`, Season, G, W, L, `W/L%`,
         Division, `Division Rank`, Conference, `Conference Rank`, everything()) %>%
  arrange(`Team Name`, desc(Season))

# Save per game data frame to a rda file
save(nba_team_ply_shooting,file = file.path(team_fp,"NBA_TEAM_PLAYOFF_SHOOTING.rda"))

# Display message to confirm save
print("nba_team_ply_shooting table has been saved to NBA_TEAM_PLAYOFF_SHOOTING.rda")

# Delete the partial RDA file
file.remove(file.path(team_fp,"NBA_TEAM_PLAYOFF_SHOOTING_partial.csv"))