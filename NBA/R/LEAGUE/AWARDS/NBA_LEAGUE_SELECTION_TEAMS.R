# Author: David Vialpando-Nielsen
# Date Made: 9/16/2024
# Latest Update: 9/16/2024

# This file will contain scrape code for all-nba, all-defensive, and all-rookie team selections

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

# Load league URLs without filtering
league_urls <- read_csv("C:/Users/djvia/OneDrive/Documents/Blog Website/Basketball_Database/URLS/NBA URLS/NBA_LEAGUE_URLS.csv")

# Function to scrape team rosters using RSelenium and capture team captions
scrape_team_rosters_with_captions <- function(remDr, url, element_prefix, max_teams, team_type, max_retries = 3) {
  for (i in 1:max_retries) {
    tryCatch({
      # Navigate to the URL
      remDr$navigate(url)
      Sys.sleep(3)  # Wait for the page to load
      
      # Get the season from the URL
      season <- str_extract(url, "\\d{4}") %>% as.numeric()
      season_formatted <- paste(season - 1, season, sep = "-")
      
      # Determine the correct element_id based on the season
      if (season <= 1949) {
        element_prefix <- "all-baa"  # For BAA years (1947-1949)
      }
      
      # Extract player names and team captions
      players <- list()
      team_names <- list()
      
      # Extract captions and players for the number of teams based on element prefix
      for (team_num in 1:max_teams) {
        caption_selector <- paste0("#", element_prefix, "_", team_num, " caption")
        players_selector <- paste0("#", element_prefix, "_", team_num, " td a")
        
        # Extract the caption and players for each team
        team_names[[team_num]] <- remDr$findElement(using = "css selector", caption_selector)$getElementText()[[1]]
        players[[team_num]] <- remDr$findElements(using = "css selector", players_selector) %>% 
          map_chr(~ .x$getElementText()[[1]])
      }
      
      # Modify team names by appending the selection type (e.g., All-NBA, All-Defensive)
      team_names <- map_chr(team_names, ~ paste0(team_type, " ", .x))
      
      # Flatten the lists into vectors and create the dataframe
      roster_data <- tibble(
        Player = unlist(players),
        Team = rep(unlist(team_names), times = sapply(players, length)),
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

# Set up RSelenium with headless browsing and increased timeout
eCaps <- list(chromeOptions = list(args = c('--headless', '--disable-gpu', '--window-size=1280,800')))
rs_driver_object <- rsDriver(browser = 'chrome', extraCapabilities = eCaps, verbose = FALSE, port = free_port(), chromever = "128.0.6613.119")
remDr <- rs_driver_object$client

# Set script, page load, and implicit timeouts to 5 minutes (300,000 milliseconds)
remDr$setTimeout(type = "script", milliseconds = 300000)
remDr$setTimeout(type = "page load", milliseconds = 300000)
remDr$setTimeout(type = "implicit", milliseconds = 300000)

# Define the output CSV file path for periodic saving
output_csv <- file.path(award_fp, "nba_selection_teams_partial.csv")

# Initialize an empty list to store all combined data for later use
all_rosters_combined <- list()

# Loop through all the URLs in `league_urls` and scrape All-NBA, All-Defensive, and All-Rookie rosters
for (url in league_urls$URL) {
  season_year <- str_extract(url, "\\d{4}") %>% as.numeric()
  
  # Scraping All-NBA (BAA) data
  if (season_year <= 1988) {  # 2 teams for All-NBA/BAA before 1989
    message(paste("Scraping 2-team All-NBA/BAA data for URL:", url))
    all_nba_data <- scrape_team_rosters_with_captions(remDr, url, "all-nba", max_teams = 2, team_type = "All-NBA")
  } else {  # 3 teams for All-NBA starting from 1989
    message(paste("Scraping 3-team All-NBA data for URL:", url))
    all_nba_data <- scrape_team_rosters_with_captions(remDr, url, "all-nba", max_teams = 3, team_type = "All-NBA")
  }
  
  # Scraping All-Defensive data (always 2 teams, started in 1969)
  if (season_year >= 1969) {
    message(paste("Scraping All-Defensive data for URL:", url))
    all_defensive_data <- scrape_team_rosters_with_captions(remDr, url, "all-defensive", max_teams = 2, team_type = "All-Defensive")
  } else {
    all_defensive_data <- NULL
  }
  
  # Scraping All-Rookie data (started in 1963, 1 team until 1989, 2 teams from 1990)
  if (season_year >= 1963 & season_year < 1990) {  # 1 team between 1963 and 1989
    message(paste("Scraping 1-team All-Rookie data for URL:", url))
    all_rookie_data <- scrape_team_rosters_with_captions(remDr, url, "all-rookie", max_teams = 1, team_type = "All-Rookie")
  } else if (season_year >= 1990) {  # 2 teams from 1990 onwards
    message(paste("Scraping 2-team All-Rookie data for URL:", url))
    all_rookie_data <- scrape_team_rosters_with_captions(remDr, url, "all-rookie", max_teams = 2, team_type = "All-Rookie")
  } else {
    all_rookie_data <- NULL  # No All-Rookie teams before 1963
  }
  
  # Combine the scraped data into a single dataframe for the current URL
  combined_data <- bind_rows(all_nba_data, all_defensive_data, all_rookie_data)
  
  # Append to the CSV after processing each URL
  if (nrow(combined_data) > 0) {
    write_csv(combined_data, output_csv, append = TRUE)
  }
  
  # Store the combined data in the list for later use
  all_rosters_combined[[url]] <- combined_data
}

# Combine all scraped rosters into a single dataframe
nba_selection_teams <- bind_rows(all_rosters_combined)

# Close the RSelenium session
remDr$close()
rs_driver_object$server$stop()

# Load in files for IDs
load(file.path(player_fp, "NBA_PLAYER_REG_ROSTER.rda"))

# Assign Player ID through roster data
nba_selection_teams <- nba_selection_teams %>%
  left_join(nba_reg_roster %>% select(`Player ID`, Player, `Franchise ID`, 
                                      `Team Abbr.`, `Team Name`, Season),
            by = c("Player", "Season"),
            relationship = "many-to-many") %>%
  distinct()

nba_duplicates <- nba_selection_teams %>%
  group_by(Player, Season, Team) %>%
  filter(n() > 1)

nba_duplicates_filtered <- nba_duplicates %>%
  filter(!(Player == "Andy Phillip" & `Team Abbr.` == "PHW"),
         !(Player == "Slater Martin" & `Team Abbr.` == "NYK"),
         !(Player == "Wilt Chamberlain" & `Team Abbr.` == "SFW"),
         !(Player == "Dave DeBusschere" & `Team Abbr.` == "DET"),
         !(Player == "Archie Clark" & `Team Abbr.` == "PHI"),
         !(Player == "Norm Van Lier" & `Team Abbr.` == "CIN"),
         !(Player == "John Shumate" & `Team Abbr.` == "PHO"),
         !(Player == "Calvin Natt" & `Team Abbr.` == "NJN"),
         !(Player == "George Johnson" & `Player ID` == 2817),
         !(Player == "Dominique Wilkins" & `Team Abbr.` == "ATL"),
         !(Player == "Clyde Drexler" & `Team Abbr.` == "POR"),
         !(Player == "Donyell Marshall" & `Team Abbr.` == "MIN"),
         !(Player == "Eddie Jones" & `Team Abbr.` == "LAL"),
         !(Player == "Dikembe Mutombo" & `Team Abbr.` == "ATL"),
         !(Player == "Courtney Alexander" & `Team Abbr.` == "DAL"),
         !(Player == "Joe Johnson" & `Team Abbr.` == "BOS"),
         !(Player == "Slater Martin" & `Team Abbr.` == "NYK"),
         !(Player == "Drew Gooden" & `Team Abbr.` == "MEM"),
         !(Player == "Gordan Giriček" & `Team Abbr.` == "MEM"),
         !(Player == "Theo Ratliff" & `Team Abbr.` == "ATL"),
         !(Player == "Metta World Peace" & `Team Abbr.` == "IND"),
         !(Player == "Chauncey Billups" & `Team Abbr.` == "DET"),
         !(Player == "Derrick Favors" & `Team Abbr.` == "NJN"),
         !(Player == "Buddy Hield" & `Team Abbr.` == "NOP"),
         !(Player == "Yogi Ferrell" & `Team Abbr.` == "BRK"),
         !(Player == "Landry Shamet" & `Team Abbr.` == "PHI"))

# Define the custom order for `Selection Team`
selection_team_order <- c(
  "All-NBA 1st Team", "All-NBA 2nd Team", "All-NBA 3rd Team",
  "All-Defensive 1st Team", "All-Defensive 2nd Team",
  "All-Rookie 1st Team", "All-Rookie 2nd Team"
)


# Remove the original duplicates and bind the filtered duplicates back
nba_selection_teams <- nba_selection_teams %>%
  anti_join(nba_duplicates, by = c("Player", "Season", "Team")) %>%
  bind_rows(nba_duplicates_filtered) %>%
  rename(`Selection Team` = Team) %>%
  mutate(`Selection Team` = factor(`Selection Team`, levels = selection_team_order)) %>%
  arrange(desc(Season), `Selection Team`, Player)

# Save Player Selection Teams
save(nba_selection_teams,file = file.path(award_fp,"NBA_LEAGUE_SELECTION_TEAMS.rda"))

# Display message to confirm save
print("nba_selection_teams table has been saved to NBA_LEAGUE_SELECTION_TEAMS.rda")

# Delete the partial RDA file
file.remove(file.path(award_fp,"nba_selection_teams_partial.csv"))