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
library(wehoop)

# Read in the GLEAGUE_PLAYER_INDEX.csv
wnba_player_index <- read_csv("C:/Users/djvia/OneDrive/Documents/Blog Website/Basketball_Database/MISCELLANEOUS/WNBA_PLAYER_INDEX.csv")

# Initialize Progress Handling
handlers(global = TRUE)

# RSelenium Setup
eCaps <- list(chromeOptions = list(args = c('--headless', '--disable-gpu', '--window-size=1280,800')))
rs_driver_object <- rsDriver(browser = 'chrome', extraCapabilities = eCaps, verbose = FALSE, port = free_port())
remDr <- rs_driver_object$client

# Function to Clear Cache and Cookies
clear_cache_and_cookies <- function(remDr) {
  remDr$deleteAllCookies()
  remDr$executeScript("window.localStorage.clear();")
  remDr$executeScript("window.sessionStorage.clear();")
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

# Suppress warnings function
suppress_warnings <- function(expr) {
  suppressWarnings(suppressMessages(expr))
}

# Function to extract 'From' and 'To' years (WNBA case)
extract_from_to_wnba <- function(seasons) {
  clean_season <- gsub("[^0-9]", "", seasons)
  from <- to <- NA
  
  if (nchar(clean_season) == 8) {
    from <- as.numeric(substr(clean_season, 1, 4))
    to <- as.numeric(substr(clean_season, 5, 8))
  } else if (nchar(clean_season) == 4) {
    from <- as.numeric(clean_season)
    to <- from
  }
  
  return(c(from, to))
}

# Function to scrape valid player URLs and 'From'/'To' years from the letter page
scrape_players_by_letter <- function(letter, remDr) {
  url <- paste0("https://www.basketball-reference.com/wnba/players/", letter, "/")
  remDr$navigate(url)
  Sys.sleep(3)  # Allow the page to load
  
  # Find all player elements by targeting the correct HTML structure
  player_elements <- remDr$findElements("css selector", "p a")
  season_elements <- remDr$findElements("css selector", "p")
  
  # Extract player names and links from valid player elements
  player_names <- lapply(player_elements, function(x) x$getElementText()[[1]])
  player_links <- lapply(player_elements, function(x) x$getElementAttribute("href")[[1]])
  
  # Extract 'From' and 'To' seasons from the same 'p' elements
  season_texts <- lapply(season_elements, function(x) x$getElementText()[[1]])
  player_seasons <- season_texts[grep("\\d{4}", season_texts)]
  
  # Ensure all extracted data has the same length
  min_length <- min(length(player_names), length(player_links), length(player_seasons))
  player_names <- player_names[1:min_length]
  player_links <- player_links[1:min_length]
  player_seasons <- player_seasons[1:min_length]
  
  # Create a dataframe with player names, links, and seasons
  player_data <- data.frame(
    Player_Name = unlist(player_names),
    Player_Link = unlist(player_links),
    Seasons = unlist(player_seasons),
    Letter = letter,  # Add the letter column here
    stringsAsFactors = FALSE
  )
  
  # Filter to only keep valid player URLs that match the expected pattern
  player_data <- player_data %>%
    filter(grepl("/wnba/players/[a-z]/[a-z]+\\d{2}w\\.html$", Player_Link))  # Keep valid player links only
  
  # Extract 'From' and 'To' years using the extract_from_to_wnba function
  player_data <- player_data %>%
    mutate(From_To = map(Seasons, extract_from_to_wnba),
           From = map_int(From_To, 1),
           To = map_int(From_To, 2)) %>%
    select(-From_To, -Seasons)  # Remove intermediate columns
  
  # Filter players for the most recent WNBA season using the wehoop function
  player_data <- player_data %>%
    filter(To == most_recent_wnba_season())
  
  return(player_data)
}

# Function to scrape player info using individual player links
scrape_info_section <- function(remDr, url) {
  remDr$navigate(url)
  Sys.sleep(3)  # Allow the page to load
  
  # Try to grab player name
  player_name <- tryCatch({
    player_name_element <- remDr$findElement(using = "css selector", "h1")
    player_name_element$getElementText()[[1]]
  }, error = function(e) { NA })  # Set to NA if element not found
  
  # Try to grab the 'info' section
  info_text <- tryCatch({
    info_element <- remDr$findElement(using = "xpath", "//*[(@id = 'info')]")
    info_element$getElementText()[[1]]
  }, error = function(e) { NA })  # Set to NA if element not found
  
  if (is.na(info_text)) {
    return(data.frame(
      Player_Name = player_name,
      Birth_Date = NA,
      Position = NA,
      Height = NA,
      Weight = NA,
      College = NA,
      stringsAsFactors = FALSE
    ))
  }
  
  # Extract useful data using regex patterns
  birth_date <- str_extract(info_text, "Born:\\s[A-Za-z]+\\s\\d{1,2},\\s\\d{4}") %>% 
    str_replace("Born:\\s", "")  # Remove 'Born: ' part
  if (is.na(birth_date)) { birth_date <- NA }
  
  position <- str_extract(info_text, "Position:\\s\\S+") %>% str_replace("Position:\\s", "")  
  if (is.na(position)) { position <- NA }
  
  height <- str_extract(info_text, "\\d+-\\d+")  
  if (is.na(height)) { height <- NA }
  
  weight <- str_extract(info_text, "\\d+lb") %>% str_replace("lb", "")
  # Handle missing or empty weight
  if (is.na(weight) || weight == "") { weight <- NA }
  weight <- as.numeric(weight)  # Ensure Weight is always numeric
  
  college <- str_extract_all(info_text, "Colleges?:\\s.*") %>%
    str_replace_all("Colleges?:\\s", "") %>%
    paste(collapse = ", ")
  # Replace 'character(0)' with NA
  if (college == "" || college == "character(0)") { college <- NA }
  
  return(data.frame(
    Player_Name = player_name,
    Birth_Date = birth_date,
    Position = position,
    Height = height,
    Weight = weight,
    College = college,
    Player_Link = url,  # Add the Player_Link back to the dataframe
    stringsAsFactors = FALSE
  ))
}

# Directory to save partial CSV files
save_directory <- "C:/Users/djvia/OneDrive/Documents/Blog Website/Basketball_Database/MISCELLANEOUS"
partial_csv_file <- file.path(save_directory, "WNBA_partial.csv")

# Function to append new data to the partial CSV after a letter is fully processed
write_or_append_partial_csv <- function(player_data, file_path) {
  if (file.exists(file_path)) {
    existing_data <- read_csv(file_path, show_col_types = FALSE)
    combined_data <- bind_rows(existing_data, player_data)
  } else {
    combined_data <- player_data
  }
  write_csv(combined_data, file_path)
}

# Read the partial CSV if it exists
if (file.exists(partial_csv_file)) {
  all_player_data_combined <- read_csv(partial_csv_file, show_col_types = FALSE)
} else {
  all_player_data_combined <- data.frame()
}

# Function to determine completed letters based on the 'Letter' column
get_completed_letters_from_partial <- function(file_path) {
  if (file.exists(file_path)) {
    player_data <- read_csv(file_path, show_col_types = FALSE)
    
    # Get unique letters from the 'Letter' column
    completed_letters <- unique(player_data$Letter)
    
    return(completed_letters)
  } else {
    return(character(0))
  }
}

# Get completed letters from partial file
completed_letters <- get_completed_letters_from_partial(partial_csv_file)

# Loop over each letter and scrape player info
letters_to_scrape <- setdiff(letters, completed_letters)  # Exclude completed letters
for (letter in letters_to_scrape) {
  message(paste("Starting to scrape players from letter:", letter))
  
  # Suppress warnings for scraping process
  suppressWarnings({
    # Scrape player URLs and seasons for the current letter
    player_urls <- scrape_players_by_letter(letter, remDr)
    
    if (nrow(player_urls) == 0) {
      message(paste("No players found for letter:", letter))
      next  # Skip to the next letter if no players are found
    }
    
    # Initialize an empty dataframe to store data for the current letter
    letter_data <- data.frame()
    
    # Scrape info for each player
    for (i in 1:nrow(player_urls)) {
      player_link <- player_urls$Player_Link[i]
      
      # Scrape player info
      player_data <- with_progress({
        scrape_info_section(remDr, player_link)
      })
      
      # Add the 'From' and 'To' columns to player_data before combining
      player_data <- player_data %>%
        mutate(From = player_urls$From[i], To = player_urls$To[i], Letter = letter)  # Add 'From', 'To', and 'Letter'
      
      # Add player data to the dataframe for the current letter
      letter_data <- bind_rows(letter_data, player_data)
      
      # Progress message after each player is scraped
      message(paste("Finished scraping player:", player_urls$Player_Name[i]))
    }
    
    # Save all data for the current letter to the partial CSV
    write_or_append_partial_csv(letter_data, partial_csv_file)
    
    # Combine with overall data after writing the letter's data
    all_player_data_combined <- bind_rows(all_player_data_combined, letter_data)
  })
  
  # Reset RSelenium session to prevent memory overload
  remDr$close()
  rs_driver_object$server$stop()
  Sys.sleep(10)
  
  # Restart RSelenium session after reset
  rs_driver_object <- rsDriver(browser = 'chrome', extraCapabilities = eCaps, verbose = FALSE, port = free_port())
  remDr <- rs_driver_object$client
}

# Final cleanup
remDr$close()
rs_driver_object$server$stop()

# Reformmating to fit the wnba index
all_player_data_combined <- all_player_data_combined %>%
  rename(Player = Player_Name,`Birth Date` = Birth_Date, Pos = Position, Ht = Height,
         Wt = Weight, Colleges = College) %>%
  select(Player, From, To, Pos, Ht, Wt, `Birth Date`, Colleges)

# Cross-validation and merging logic between wnba_player_index and all_player_data_combined
# Merge the data on Player and From columns
updated_wnba_data <- wnba_player_index %>%
  left_join(all_player_data_combined, by = c("Player", "From"), suffix = c("_index", "_combined")) %>%
  
  # For overlapping data, take the most recent 'To', and update the columns if they are NA in the index
  mutate(
    To = coalesce(To_combined, To_index),
    Pos = coalesce(Pos_combined, Pos_index),
    Ht = coalesce(Ht_combined, Ht_index),
    Wt = coalesce(Wt_combined, Wt_index),
    `Birth Date` = coalesce(`Birth Date_combined`, `Birth Date_index`),
    Colleges = coalesce(Colleges_combined, Colleges_index),
    Active = if_else(To == most_recent_wnba_season(), "Yes", "No")
  ) %>%
  
  # Select the necessary columns to retain the original structure
  select(`Player ID`, Player, From, To, Pos, Ht, Wt, `Birth Date`, Colleges, Active)

# Identify new players that are in all_player_data_combined but not in the wnba_player_index
new_players <- anti_join(all_player_data_combined, wnba_player_index, by = c("Player", "From"))

# Assign new Player IDs for new players
new_player_id_start <- max(wnba_player_index$`Player ID`, na.rm = TRUE) + 1
new_players <- new_players %>%
  mutate(
    `Player ID` = row_number() + new_player_id_start,
    Active = if_else(To == most_recent_wnba_season(), "Yes", "No")
  ) %>%
  arrange(From, Player) %>%
  select(`Player ID`, Player, From, To, Pos, Ht, Wt, `Birth Date`, Colleges, Active)

# Combine the updated index with new players
final_wnba_player_index <- bind_rows(updated_wnba_data, new_players) %>%
  arrange(`Player ID`)  # Ensure the final table is sorted by From and Player

# Write the final WNBA Player Index back to a CSV
write_csv(final_wnba_player_index, "C:/Users/djvia/OneDrive/Documents/Blog Website/Basketball_Database/MISCELLANEOUS/WNBA_PLAYER_INDEX.csv")

# Delete the partial CSV file after successful processing
if (file.exists(partial_csv_file)) {
  file.remove(partial_csv_file)
  message("Partial CSV file has been deleted.")
} else {
  message("No partial CSV file found to delete.")
}