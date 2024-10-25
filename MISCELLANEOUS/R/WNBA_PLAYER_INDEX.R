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
  
  return(player_data)
}

scrape_info_section <- function(remDr, url) {
  # Navigate to the URL
  remDr$navigate(url)
  Sys.sleep(3)  # Allow the page to load
  
  # Initialize all fields as NA
  player_name <- NA
  birth_date <- NA
  position <- NA
  height <- NA
  weight <- NA
  college <- NA
  
  # Get page source using rvest
  page_source <- read_html(remDr$getPageSource()[[1]])
  
  # Try to extract player name from the <h1> tag
  player_name <- tryCatch({
    page_source %>%
      html_element("h1 span") %>%
      html_text(trim = TRUE)
  }, error = function(e) { NA })
  
  # Try to extract birth date
  birth_date <- tryCatch({
    page_source %>%
      html_element("span#necro-birth") %>%
      html_text(trim = TRUE)
  }, error = function(e) { NA })
  
  # Try to extract position
  position <- tryCatch({
    page_source %>%
      html_nodes(xpath = "//p[strong[contains(text(), 'Position')]]") %>%
      html_text(trim = TRUE) %>%
      str_extract("Position:\\s*([A-Za-z]+)") %>%
      str_replace("Position:\\s*", "")
  }, error = function(e) { NA })
  
  # Refined height extraction logic to exclude 'Also known as' spans
  height <- tryCatch({
    page_source %>%
      html_elements("p span") %>%  # Target all spans in <p> elements
      html_text(trim = TRUE) %>%
      .[str_detect(., "^\\d+-\\d+$|^\\d+-\\d{2}$")] %>%  # Match pattern #-# or #-##
      .[1]  # Take the first valid match
  }, error = function(e) { NA })
  
  # Try to extract weight and suppress warnings during coercion
  suppressWarnings({
    weight <- tryCatch({
      page_source %>%
        html_element("p span:nth-child(2)") %>%
        html_text(trim = TRUE) %>%
        str_replace("lb", "") %>%
        as.numeric()
    }, error = function(e) { NA })
  })
  
  # Try to extract college(s) information, if none found set to NA
  college <- tryCatch({
    page_source %>%
      html_nodes(xpath = "//p[strong[contains(text(), 'College') or contains(text(), 'Colleges')]]/a") %>%
      html_text(trim = TRUE) %>%
      paste(collapse = ", ") %>%
      ifelse(. == "", NA, .)  # Set to NA if the value is blank
  }, error = function(e) { NA })
  
  # Return the scraped player info as a dataframe
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
    
    # If no players found, move to the next letter
    if (nrow(player_urls) == 0) {
      message(paste("No players found for letter:", letter))
      next  # Skip to the next letter if no players are found
    }
    
    # Initialize an empty dataframe to store data for the current letter
    letter_data <- data.frame()
    
    # Scrape info for each player
    for (i in 1:nrow(player_urls)) {
      player_link <- player_urls$Player_Link[i]
      
      # Check if the player is Ezinne Kalu and update the URL
      if (player_urls$Player_Name[i] == "Ezinne Kalu") {
        player_link <- "https://www.basketball-reference.com/wnba/players/k/kaluez01w.html"  # Corrected link
      }
      
      # Scrape player info
      player_data <- with_progress({
        scrape_info_section(remDr, player_link)
      })
      
      # Add the 'From' and 'To' columns to player_data before combining
      player_data <- player_data %>%
        mutate(From = player_urls$From[i], To = player_urls$To[i], Letter = letter)  # Add 'From', 'To', and 'Letter'
      
      # Add player data to the dataframe for the current letter
      letter_data <- bind_rows(letter_data, player_data)
      
      message(paste("Finished scraping player:", player_urls$Player_Name[i]))
      
      # Periodically refresh the Selenium session to keep it alive (every 25 players)
      if (i %% 25 == 0) {
        message("Refreshing session...")
        remDr$refresh()
        Sys.sleep(3)  # Give the page time to reload after the refresh
      }
    }
    
    # Save all data for the current letter to the partial CSV
    write_or_append_partial_csv(letter_data, partial_csv_file)
    
    # Combine with overall data after writing the letter's data
    all_player_data_combined <- bind_rows(all_player_data_combined, letter_data)
    
    # Reset RSelenium session to prevent memory overload
    remDr$close()
    rs_driver_object$server$stop()
    Sys.sleep(10)
    
    # Restart RSelenium session after reset
    rs_driver_object <- rsDriver(browser = 'chrome', extraCapabilities = eCaps, verbose = FALSE, port = free_port())
    remDr <- rs_driver_object$client
  }) # End of suppressWarnings
}

# Final cleanup
remDr$close()
rs_driver_object$server$stop()

# Function to remove non-breaking spaces from the dataframe
clean_non_breaking_spaces <- function(df) {
  df %>%
    mutate(across(everything(), ~ str_replace_all(., "\\u00A0", " ")))  # Replace non-breaking space with regular space
}

# Clean the non-breaking spaces in the dataframe
all_player_data_combined <- clean_non_breaking_spaces(all_player_data_combined)

# Rearrange and assign Player IDs for WNBA players
all_player_data_combined <- all_player_data_combined %>%
  arrange(From, Player_Name) %>%
  mutate(`Player ID` = 1000 + row_number(),
         Active = if_else(To == most_recent_wnba_season(), "Yes", "No")) %>%
  rename(Player = Player_Name,`Birth Date` = Birth_Date, Pos = Position, Ht = Height,
         Wt = Weight, Colleges = College) %>%
  select(`Player ID`, Player, From, To, Pos, Ht, Wt, `Birth Date`, Colleges, Active)

# Rework Position so that it fits other player index syntax
wnba_player_index <- all_player_data_combined %>%
  mutate(Pos = case_when(
    Pos == "Guard-Forward" ~ "G-F",
    Pos == "Forward-Guard" ~ "F-G",
    Pos == "Forward-Center" ~ "F-C",
    Pos == "Center-Forward" ~ "C-F",
    Pos == "Guard" ~ "G",
    Pos == "Forward" ~ "F",
    Pos == "Center" ~ "C",
    TRUE ~ Pos  # Keep other positions as is
  ))

# Write to a csv for WNBA Player Index
write_csv(wnba_player_index,"C:/Users/djvia/OneDrive/Documents/Blog Website/Basketball_Database/MISCELLANEOUS/WNBA_PLAYER_INDEX.csv")

# Delete the partial CSV file after successful processing
if (file.exists(partial_csv_file)) {
  file.remove(partial_csv_file)
  message("Partial CSV file has been deleted.")
} else {
  message("No partial CSV file found to delete.")
}
