# Author: David Vialpando-Nielsen
# Date Made: 9/15/2024
# Latest Update: 9/15/2024

# This file will contain scrape code for all star season selections

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


# Define file paths (adjust these as necessary)
award_fp <- "C:/Users/djvia/OneDrive/Documents/Blog Website/Basketball_Database/NBA/LEAGUE/AWARDS"

player_fp <- "C:/Users/djvia/OneDrive/Documents/Blog Website/Basketball_Database/NBA/PLAYER/REGULAR SEASON/"

# Define file path for the league URLs
league_urls <- read_csv("C:/Users/djvia/OneDrive/Documents/Blog Website/Basketball_Database/URLS/NBA URLS/NBA_LEAGUE_URLS.csv") %>%
  filter(Year >= 1951 & Year != 1999)

# Function to clean column names and ensure specific column types are consistent
clean_colnames <- function(df) {
  colnames(df) <- ifelse(colnames(df) == "" | is.na(colnames(df)), paste0("V", seq_along(colnames(df))), colnames(df))
  
  # Convert all columns to character to avoid type mismatches
  df <- df %>% mutate(across(everything(), as.character))
  
  return(df)
}

# Function to clear cache and cookies
clear_cache_and_cookies <- function(remDr) {
  remDr$deleteAllCookies()  # Clear cookies
  remDr$executeScript("window.localStorage.clear();")  # Clear local storage
  remDr$executeScript("window.sessionStorage.clear();")  # Clear session storage
}

# Function to scrape All-Star rosters using RSelenium
scrape_all_star_rosters <- function(remDr, url, max_retries = 3) {
  for (i in 1:max_retries) {
    tryCatch({
      # Navigate to the URL
      remDr$navigate(url)
      Sys.sleep(3)  # Wait for the page to load
      
      # Get the season from the URL
      season <- str_extract(url, "\\d{4}") %>% as.numeric()
      season_formatted <- paste(season - 1, season, sep = "-")
      
      # Extract the main All-Star rosters container
      webElem <- remDr$findElement(using = "css", "#div_all_star_game_rosters")
      page_html <- webElem$getElementAttribute("outerHTML")[[1]]
      parsed_html <- read_html(page_html)
      
      # Extract the captions for both teams
      team1_caption <- parsed_html %>% html_node("#all_star_game_rosters_1 caption") %>% html_text()  # Giannis
      team2_caption <- parsed_html %>% html_node("#all_star_game_rosters_2 caption") %>% html_text()  # LeBron
      
      # Extract player names for each team
      players_team1 <- parsed_html %>% html_nodes("#all_star_game_rosters_1 td a") %>% html_text()  # First team players
      players_team2 <- parsed_html %>% html_nodes("#all_star_game_rosters_2 td a") %>% html_text()  # Second team players
      
      # Create a dataframe with players, team names, and season
      roster_data <- tibble(
        Player = c(players_team1, players_team2),
        Team = c(rep(team1_caption, length(players_team1)), rep(team2_caption, length(players_team2))),
        Season = season_formatted
      )
      
      return(roster_data)
      
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

# Set up RSelenium with headless browsing
eCaps <- list(chromeOptions = list(args = c('--headless', '--disable-gpu', '--window-size=1280,800')))
rs_driver_object <- rsDriver(browser = 'chrome', extraCapabilities = eCaps, verbose = FALSE, port = free_port(), chromever = "128.0.6613.119")
remDr <- rs_driver_object$client

# Loop through all the URLs in `league_urls` and scrape All-Star rosters
all_rosters <- list()

for (url in league_urls$URL) {
  message(paste("Scraping All-Star data for URL:", url))
  all_star_data <- scrape_all_star_rosters(remDr, url)
  if (!is.null(all_star_data)) {
    all_rosters[[url]] <- all_star_data
  }
}

# Combine all scraped rosters into a single dataframe
combined_rosters <- bind_rows(all_rosters)

# Close the RSelenium session
remDr$close()
rs_driver_object$server$stop()

# Rename columns for joins later
nba_all_star <- combined_rosters %>%
  rename(`All Star Team` = Team)

# Player Roster for IDs
load(file.path(player_fp, "NBA_PLAYER_REG_ROSTER.rda"))

player_index <- read_csv("C:/Users/djvia/OneDrive/Documents/Blog Website/Basketball_Database/MISCELLANEOUS/NBA_ABA_PLAYER_INDEX.csv")

league_info <- read_csv("C:/Users/djvia/OneDrive/Documents/Blog Website/Basketball_Database/NBA/LEAGUE/NBA_LEAGUE_INFO.csv")

# Assign IDs to All Star data frame
nba_all_star <- nba_all_star %>%
  left_join(nba_reg_roster %>%
              select(`Player ID`, Player, `Franchise ID`, `Team Abbr.`, 
                     `Team Name`, Season),
            by = c("Player", "Season"))

missing <- nba_all_star %>%
  filter(is.na(`Player ID`)) %>%
  select(-`Player ID`, -`Team Name`, -`Franchise ID`) %>%
  left_join(player_index %>%
              select(`Player ID`, Player),
            by = c("Player")) %>%
  mutate(`Team Abbr.` = if_else(Player == "Magic Johnson","LAL",`Team Abbr.`)) %>%
  left_join(league_info %>%
              select(Team, `Team Name`, `Franchise ID`),
            by = c("Team Abbr." = "Team")) %>%
  select(Player, `All Star Team`, Season, `Player ID`, `Franchise ID`, 
         `Team Abbr.`, `Team Name`)

duplicates <- nba_all_star %>%
  group_by(Player, Season) %>%
  filter(n() > 1)

duplicates_filtered <- duplicates %>%
  filter(!(Player == "Dike Eddleman" & `Team Abbr.` == "FTW"),
         !(Player == "Fred Scolari" & `Team Abbr.` == "FTW"),
         !(Player == "Andy Phillip" & `Team Abbr.` == "PHW"),
         !(Player == "Frank Selvy" & `Team Abbr.` == "MLH"),
         !(Player == "Slater Martin" & `Team Abbr.` == "NYK"),
         !(Player == "George Yardley" & `Team Abbr.` == "SYR"),
         !(Player == "Dick Garmaker" & `Team Abbr.` == "NYK"),
         !(Player == "Tom Gola" & `Team Abbr.` == "SFW"),
         !(Player == "Len Chappell" & `Team Abbr.` == "PHI"),
         !(Player == "Wilt Chamberlain" & `Team Abbr.` == "PHI"),
         !(Player == "Archie Clark" & `Team Abbr.` == "PHI"),
         !(Player == "John Block" & `Team Abbr.` == "KCO"),
         !(Player == "Jim Price" & `Team Abbr.` == "LAL"),
         !(Player == "Bob McAdoo" & `Team Abbr.` == "BUF"),
         !(Player == "Ricky Pierce" & `Team Abbr.` == "SEA"),
         !(Player == "Dominique Wilkins" & `Team Abbr.` == "LAC"),
         !(Player == "Danny Manning" & `Team Abbr.` == "ATL"),
         !(Player == "Chris Gatling" & `Team Abbr.` == "NJN"),
         !(Player == "Dikembe Mutombo" & `Team Abbr.` == "PHI"),
         !(Player == "Gary Payton" & `Team Abbr.` == "MIL"),
         !(Player == "Vince Carter" & `Team Abbr.` == "TOR"),
         !(Player == "Allen Iverson" & `Team Abbr.` == "PHI" & Season == "2006-2007"),
         !(Player == "Jason Kidd" & `Team Abbr.` == "DAL"),
         !(Player == "Allen Iverson" & `Team Abbr.` == "DEN" & Season == "2008-2009"),
         !(Player == "Chauncey Billups" & `Team Abbr.` == "DET"),
         !(Player == "Allen Iverson" & `Team Abbr.` == "MEM"),
         !(Player == "Carmelo Anthony" & `Team Abbr.` == "DEN"),
         !(Player == "Deron Williams" & `Team Abbr.` == "NJN"),
         !(Player == "DeMarcus Cousins" & `Team Abbr.` == "NOP"),
         !(Player == "James Harden" & `Team Abbr.` == "HOU"),
         !(Player == "Nikola Vučević" & `Team Abbr.` == "CHI"),
         !(Player == "James Harden" & `Team Abbr.` == "BRK" & Season == "2021-2022"),
         !(Player == "Kevin Durant" & `Team Abbr.` == "BRK"),
         !(Player == "Kyrie Irving" & `Team Abbr.` == "BRK"))

# Drop the specific row with Magic Johnson, West, and 1991-1992
nba_all_star_cleaned <- nba_all_star %>%
  filter(!(Player == "Magic Johnson" & `All Star Team` == "West" & Season == "1991-1992"))

# Drop rows from nba_all_star that are in the `duplicates` dataframe
nba_all_star_cleaned <- nba_all_star_cleaned %>%
  anti_join(duplicates, by = c("Player", "Season", "Team Abbr."))

# Add rows from the `missing` dataframe and the filtered `duplicates_filtered` dataframe
# Creates Appearance column
nba_all_star <- nba_all_star_cleaned %>%
  bind_rows(missing) %>%
  bind_rows(duplicates_filtered) %>%
  group_by(Player) %>%
  arrange(Player, Season) %>%
  mutate(Appearances = row_number()) %>%
  ungroup() %>%
  arrange(desc(Season), `All Star Team`, Player)

# Save the final nba_all_star table to a rda file
save(nba_all_star,file = file.path(award_fp, "NBA_LEAGUE_ALL_STAR.rda"))

# Display message to confirm save
print("nba_all_star table has been saved to NBA_LEAGUE_ALL_STAR.rda")

# Delete the partial CSV file
file.remove(file.path(award_fp, "NBA_PLAYER_ALL-STAR_partial.csv"))
