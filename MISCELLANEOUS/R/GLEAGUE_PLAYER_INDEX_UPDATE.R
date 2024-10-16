# Required Libraries
library(RSelenium)
library(tidyverse)
library(netstat)
library(wdman)
library(stringr)
library(lubridate)
library(readr)
library(rvest)
library(progressr)
library(stringi)
library(hoopR)

# Read in the GLEAGUE_PLAYER_INDEX.csv
gleague_player_index <- read_csv("C:/Users/djvia/OneDrive/Documents/Blog Website/Basketball_Database/MISCELLANEOUS/GLEAGUE_PLAYER_INDEX.csv")
nba_index <- read_csv("C:/Users/djvia/OneDrive/Documents/Blog Website/Basketball_Database/MISCELLANEOUS/NBA_ABA_PLAYER_INDEX.csv")

# Initialize Progress Handling
handlers(global = TRUE)

# Function to Start RSelenium Driver
start_rselenium <- function() {
  eCaps <- list(chromeOptions = list(args = c('--headless', '--disable-gpu', '--window-size=1280,800')))
  rs_driver_object <- rsDriver(browser = 'chrome', extraCapabilities = eCaps, verbose = FALSE, port = free_port(), chromever = "128.0.6613.119")
  return(rs_driver_object)
}

# Function to Stop RSelenium Driver
stop_rselenium <- function(rs_driver_object) {
  rs_driver_object$client$close()
  rs_driver_object$server$stop()
}

# Function to Clear Cache and Cookies
clear_cache_and_cookies <- function(remDr) {
  remDr$deleteAllCookies()
  remDr$executeScript("window.localStorage.clear();")
  remDr$executeScript("window.sessionStorage.clear();")
}

# Function to extract 'From' and 'To' years with improved logic, and handle player names with hyphens
extract_from_to <- function(seasons, player_name) {
  # Remove the player's name from the season string to avoid confusion with hyphens or other characters
  clean_season <- str_replace_all(seasons, fixed(player_name), "")
  clean_season <- gsub("[^0-9to-]", "", clean_season)  # Keep only relevant characters
  
  from <- to <- NA
  
  # Handle cases with "to" (e.g., "2010-11 to 2016-17")
  if (str_count(clean_season, "-") > 2 || grepl("\\bto\\b", clean_season, ignore.case = FALSE)) {
    years <- str_extract_all(clean_season, "\\d{4}")[[1]]
    if (length(years) >= 2) {
      from <- as.numeric(years[1]) + 1
      to <- as.numeric(years[length(years)]) + 1
    }
  }
  # Handle cases with two years (e.g., "2010-11")
  else if (grepl("-", clean_season)) {
    years <- str_extract_all(clean_season, "\\d{4}")[[1]]
    if (length(years) == 2) {
      from <- as.numeric(years[1]) + 1
      to <- as.numeric(years[2]) + 1
    } else if (length(years) == 1) {
      from <- as.numeric(years[1]) + 1
      to <- from
    }
  }
  # Handle single-year cases (e.g., "2024")
  else {
    single_year <- as.numeric(clean_season)
    if (!is.na(single_year)) {
      from <- single_year + 1
      to <- from
    }
  }
  
  return(c(from, to))
}

# Function to Scrape Player Names, Seasons, and Player Links for Each Letter Page
scrape_players <- function(letter, remDr) {
  url <- paste0("https://www.basketball-reference.com/gleague/players/", letter, "/")
  remDr$navigate(url)
  Sys.sleep(3) 
  
  player_elements <- remDr$findElements("css selector", "p a")
  player_names <- lapply(player_elements, function(x) x$getElementText()[[1]])
  
  player_links <- lapply(player_elements, function(x) x$getElementAttribute("href")[[1]])
  
  season_elements <- remDr$findElements("css selector", "p")
  season_texts <- lapply(season_elements, function(x) x$getElementText()[[1]])
  
  player_seasons <- season_texts[grep("\\d{4}-\\d{2}|\\d{4}-\\d{4}|\\d{4}", season_texts)]
  
  min_length <- min(length(player_names), length(player_seasons), length(player_links))
  player_data <- data.frame(
    Player_Name = unlist(player_names)[1:min_length],
    Seasons = unlist(player_seasons)[1:min_length],
    Player_Link = unlist(player_links)[1:min_length],
    stringsAsFactors = FALSE
  )
  
  # Clean and extract From and To years, removing player names from the season string
  player_data <- player_data %>%
    filter(!grepl("G-League|Sports Reference|Copyright", Player_Name)) %>%
    filter(!grepl("Sports Reference", Seasons)) %>%
    mutate(
      From_To = map2(Seasons, Player_Name, extract_from_to),  # Pass player_name to remove it from season string
      From = map_int(From_To, 1),
      To = map_int(From_To, 2)
    ) %>%
    select(-From_To, -Seasons)
  
  return(player_data)
}

# Retry function to handle errors
retry <- function(expr, retries = 3, silent = TRUE) {
  for (i in seq_len(retries)) {
    result <- tryCatch(expr, error = function(e) {
      if (!silent) message("Error: ", e$message, " - Retrying (", i, "/", retries, ")")
      NULL
    })
    if (!is.null(result)) return(result)
    Sys.sleep(2)  # Wait before retrying
  }
  return(NA)
}

# List of all letters to scrape (including 'x')
letters_to_scrape <- letters

# Dataframe to hold combined player data
all_player_data_combined <- data.frame()

# Loop through each letter, reset RSelenium after each letter
for (letter in letters_to_scrape) {
  message(paste("Starting to scrape players from letter:", letter))
  
  # Start RSelenium
  rs_driver_object <- start_rselenium()
  remDr <- rs_driver_object$client
  
  # Scrape players for the current letter
  player_data <- with_progress({
    scrape_players(letter, remDr)
  })
  
  # Remove rows with empty Player_Name or invalid Player_Link
  player_data <- player_data %>%
    filter(Player_Name != "" & !grepl("email|javascript:", Player_Link))
  
  # Combine current player data with the overall dataframe
  all_player_data_combined <- bind_rows(all_player_data_combined, player_data)
  
  message(paste("Completed scraping players from letter:", letter))
  
  # Stop RSelenium and reset for the next letter
  stop_rselenium(rs_driver_object)
  
  # Add a larger delay between letters
  Sys.sleep(60)
}

# Create a dupes list specifically for Luke Martinez and Ra'Shad James
dupes_list <- all_player_data_combined %>%
  filter(toupper(Player_Name) %in% c("LUKE MARTINEZ", "RA'SHAD JAMES")) %>%
  mutate(Upper_Name = toupper(Player_Name))  # Create the Upper_Name column for grouping

# Clean the duplicates by keeping relevant information
cleaned_duplicates <- dupes_list %>%
  group_by(Upper_Name) %>%
  summarise(
    Player_Name = first(Player_Name),  # Keep the first occurrence of Player_Name
    Player_Link = first(Player_Link),  # Keep the first Player_Link
    From = min(From, na.rm = TRUE),  # Keep the earliest 'From' year
    To = max(To, na.rm = TRUE),  # Keep the latest 'To' year
    .groups = 'drop'
  ) %>%
  select(-Upper_Name)  # Drop the Upper_Name column after cleaning

# Remove all instances of Luke Martinez and Ra'Shad James from the original dataset
all_player_data_combined <- all_player_data_combined %>%
  filter(!toupper(Player_Name) %in% c("LUKE MARTINEZ", "RA'SHAD JAMES"))

# Add the cleaned Luke Martinez and Ra'Shad James data back into the dataset
all_player_data_combined <- bind_rows(all_player_data_combined, cleaned_duplicates)

# Function to Scrape NBA Status for Each Player (check for "NBA Stats")
scrape_nba_status <- function(player_link, remDr) {
  retry({
    remDr$navigate(player_link)
    Sys.sleep(2)  # Allow page to load
    
    # Check if the text "NBA Stats" exists on the page
    nba_stats_elements <- remDr$findElements(using = "xpath", "//a[contains(text(), 'NBA Stats')]")
    
    # Return 'yes' if "NBA Stats" link is found, otherwise 'no'
    if (length(nba_stats_elements) > 0) {
      return("yes")
    } else {
      return("no")
    }
  }, silent = TRUE)
}

# Filtering for the most recent season after scraping all players
most_recent_season <- most_recent_mbb_season()  # Get the most recent season using hoopR
filtered_player_data <- all_player_data_combined %>%
  filter(To == most_recent_season)

# Start Selenium session
rs_driver_object <- start_rselenium()
remDr <- rs_driver_object$client

# Set up the progress bar
with_progress({
  p <- progressor(along = seq_along(filtered_player_data$Player_Link))  # Ensure progress aligns with the number of Player_Link entries
  
  # Scrape NBA status for each player with progress bar
  filtered_player_data <- filtered_player_data %>%
    mutate(
      NBA = map_chr(Player_Link, ~ {
        p(message = paste("Scraping NBA status for", .x))  # Update progress bar for each player
        scrape_nba_status(.x, remDr)  # Scrape NBA status
      })
    )
})

# Stop Selenium session after scraping
stop_rselenium(rs_driver_object)

# Rename columns for simplicity
filtered_player_data <- filtered_player_data %>%
  rename(Player = Player_Name)

# Separate data into 'Yes' and 'No' based on NBA status
nba_yes_data <- filtered_player_data %>%
  filter(NBA == "yes")

nba_no_data <- filtered_player_data %>%
  filter(NBA == "no") %>%
  mutate(`NBA ID` = NA) %>%
  select(-NBA)

# Perform a left join for 'Yes' NBA players with 'nba_index' by 'Player'
# Select only `Player ID` from 'nba_index' and rename it to 'NBA ID'
nba_yes_joined <- nba_yes_data %>%
  left_join(nba_index, by = "Player", relationship = "many-to-many") %>%
  rename(`NBA ID` = `Player ID`)

# Remove duplicates based on 'Player' and 'NBA ID' columns (if applicable)
# Keep players where 'From' is greater than or equal to 2002
nba_yes_duplicates <- nba_yes_joined %>%
  group_by(Player) %>%
  filter(n() > 1) %>%
  filter(!From.y >= 2002)

# Remove the duplicate rows from nba_yes_joined
nba_yes_cleaned <- nba_yes_joined %>%
  anti_join(nba_yes_duplicates)

# Select only the required columns: Player, Player_Link, From, To, NBA, and NBA ID
# Rename To.x and From.x to To and From
nba_yes_cleaned <- nba_yes_cleaned %>%
  select(Player, Player_Link, From = From.x, To = To.x, `NBA ID`)

# Combine cleaned 'Yes' NBA data and 'No' NBA data
filtered_player_data <- bind_rows(nba_yes_cleaned, nba_no_data)

# Create a left join to find matches by Player and From
matched_players <- gleague_player_index %>%
  left_join(filtered_player_data, by = c("Player", "From"), suffix = c("_gleague", "_new"))

# For matching players, update the 'To' column with the higher value from the new data
gleague_player_index_updated <- matched_players %>%
  mutate(
    To = ifelse(!is.na(To_new) & To_new > To_gleague, To_new, To_gleague),  # Update 'To' with the higher value
    `NBA ID` = coalesce(`NBA ID_new`, `NBA ID_gleague`)  # Ensure correct NBA ID is retained
  ) %>%
  select(-To_new, -To_gleague, -Player_Link, -`NBA ID_new`, -`NBA ID_gleague`) %>%  # Remove temporary columns
  select(`GLEAGUE ID`, `NBA ID`, Player, From, To, everything())  # Re-arrange columns

# Identify Dupes
dupes <- gleague_player_index_updated %>%
  group_by(`NBA ID`) %>%
  filter(n() > 1 & !is.na(`NBA ID`))

# Perform a left join with 'nba_index' using 'Player' and 'Birth Date'
# Only for duplicates (dupes) based on Player and Birth Date
dupes_joined <- dupes %>%
  left_join(nba_index %>%
              select(Player, `Birth Date`, `Player ID`) %>%
              rename(`NBA ID_new` = `Player ID`), 
            by = c("Player", "Birth Date")) %>%
  select(`GLEAGUE ID`, `NBA ID_new`, everything()) %>%
  ungroup() %>%
  select(-`NBA ID`) %>%
  rename(`NBA ID` = `NBA ID_new`)

# Drop everything from 'dupes' in 'gleague_player_index_updated' and add 'dupes_joined'
gleague_player_index_updated <- gleague_player_index_updated %>%
  filter(!`GLEAGUE ID` %in% dupes$`GLEAGUE ID`)  # Drop the rows matching the dupes

# Add the 'dupes_joined' back into 'gleague_player_index_updated'
gleague_player_index_updated <- bind_rows(gleague_player_index_updated, dupes_joined) %>%
  arrange(`GLEAGUE ID`)

# Filter players with NA in 'NBA ID'
players_with_na_nba_id <- gleague_player_index_updated %>%
  filter(is.na(`NBA ID`))

# Perform a left join with 'nba_index' using 'Player' and 'Birth Date'
# Assuming that both 'gleague_player_index_updated' and 'nba_index' have a 'Birth Date' column
players_with_na_nba_id_joined <- players_with_na_nba_id %>%
  left_join(nba_index %>%
              select(Player, `Birth Date`, `Player ID`) %>%
              rename(`NBA ID_new` = `Player ID`), 
            by = c("Player", "Birth Date"))

# Check if 'NBA ID_new' in 'players_with_na_nba_id_joined' contains any numeric values
if (any(!is.na(as.numeric(players_with_na_nba_id_joined$`NBA ID_new`)))) {
  
  # Update the NBA ID where applicable and clean up the columns
  gleague_player_index_updated <- gleague_player_index_updated %>%
    left_join(players_with_na_nba_id_joined %>%
                select(Player, `NBA ID_new`), 
              by = "Player", "Birth Date") %>%
    mutate(`NBA ID` = coalesce(`NBA ID_new`, `NBA ID`)) %>%  # Use coalesce to fill missing NBA IDs
    select(-`NBA ID_new`)  # Remove temporary NBA ID column
  
} else {
  # If 'NBA ID_new' is entirely NA, don't perform any updates
  message("No valid numeric NBA IDs found in 'players_with_na_nba_id_joined'. No updates made to the NBA ID column.")
}

# Identify leftover players where there was no match by Player and From
leftover_players <- filtered_player_data %>%
  anti_join(gleague_player_index, by = c("Player", "From"))

# This portion will be updated later to test with new incoming players to the gleague
# For now,  it will be left like this.

# Write to a csv for G League Player Index
write_csv(gleague_player_index_updated,"C:/Users/djvia/OneDrive/Documents/Blog Website/Basketball_Database/MISCELLANEOUS/GLEAGUE_PLAYER_INDEX.csv")