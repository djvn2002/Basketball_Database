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

# Function to extract 'From' and 'To' years with improved logic, handling hyphenated player names
extract_from_to <- function(seasons, player_name) {
  clean_season <- str_replace_all(seasons, fixed(player_name), "")
  clean_season <- gsub("[^0-9to-]", "", clean_season)  # Keep only relevant characters
  
  from <- to <- NA
  
  if (str_count(clean_season, "-") > 2 || grepl("\\bto\\b", clean_season, ignore.case = FALSE)) {
    years <- str_extract_all(clean_season, "\\d{4}")[[1]]
    if (length(years) >= 2) {
      from <- as.numeric(years[1]) + 1
      to <- as.numeric(years[length(years)]) + 1
    }
  } else if (grepl("-", clean_season)) {
    years <- str_extract_all(clean_season, "\\d{4}")[[1]]
    if (length(years) == 2) {
      from <- as.numeric(years[1]) + 1
      to <- as.numeric(years[2]) + 1
    } else if (length(years) == 1) {
      from <- as.numeric(years[1]) + 1
      to <- from
    }
  } else {
    single_year <- as.numeric(clean_season)
    if (!is.na(single_year)) {
      from <- single_year + 1
      to <- from
    }
  }
  
  return(c(from, to))
}

# Function to scrape player info along with College Stats
scrape_player_info_with_college <- function(remDr, url) {
  remDr$navigate(url)
  Sys.sleep(3)  # Allow page to load
  
  # Try to grab player name using css selector and exclude 'G-League Stats' text
  player_name <- tryCatch({
    suppressMessages({
      player_name_element <- remDr$findElement(using = "css selector", "h1")
      player_name <- player_name_element$getElementText()[[1]]
      player_name <- str_replace(player_name, "\\sG-League Stats", "")  # Remove 'G-League Stats' text
      player_name
    })
  }, error = function(e) { NA })
  
  # Scrape the 'info' section for other player data
  info_text <- remDr$findElement(using = "xpath", "//*[@id='info']")$getElementText()[[1]]
  
  # Extract relevant details
  birth_date <- str_extract(info_text, "(?<=Born:\\s)[A-Za-z]+\\s\\d{1,2},\\s\\d{4}")  # Extract birth date
  position <- str_extract(info_text, "(?<=Position:\\s)\\w+")  # Extract position
  height <- str_extract(info_text, "\\d+-\\d+")  # Extract height
  weight <- str_extract(info_text, "(?<=\\d{1,2}-\\d{1,2}\\D{1,3})\\d{1,3}")  # Extract weight
  weight <- as.numeric(weight)  # Convert weight to numeric
  
  # Check if NBA Stats link exists (use tryCatch to avoid interruption, and suppress the error message)
  nba_stats <- tryCatch({
    suppressMessages({
      remDr$findElement(using = "link text", "NBA Stats")$getElementAttribute("href")[[1]]
    })
  }, error = function(e) { NA })
  
  # Check for College Stats link
  college_stats <- tryCatch({
    suppressMessages({
      remDr$findElement(using = "link text", "College Stats")$getElementAttribute("href")[[1]]
    })
  }, error = function(e) { NA })
  
  college_info <- NA
  if (!is.na(college_stats)) {
    # Navigate to the College Stats page
    remDr$navigate(college_stats)
    Sys.sleep(3)
    
    # Extract college info based on both "School" and "Schools" cases
    college_info <- tryCatch({
      page_source <- remDr$getPageSource()[[1]]
      page_html <- read_html(page_source)
      
      # Check for both 'School' and 'Schools' cases in the page source
      college_schools <- page_html %>%
        html_nodes(xpath = "//p[strong[contains(text(),'Schools:') or contains(text(),'School:')]]/a") %>%
        html_text(trim = TRUE) %>%
        paste(collapse = ", ")
      
      # Clean up any '(Men)' text and extra spaces around commas
      college_schools <- gsub("\\(Men\\)", "", college_schools)
      college_schools <- str_squish(gsub("\\s*,\\s*", ", ", college_schools))  # Ensure correct comma spacing
      college_schools
    }, error = function(e) { NA })
  }
  
  # Determine if player has NBA stats
  nba_presence <- if (!is.na(nba_stats)) "Yes" else "No"
  
  # Return as a data frame
  return(data.frame(
    Player_Name = player_name,
    Birth_Date = birth_date,
    Position = position,
    Height = height,
    Weight = weight,  # Ensure Weight is numeric
    NBA = nba_presence,
    NBA_Stats_Link = nba_stats,
    College_Schools = college_info,
    stringsAsFactors = FALSE
  ))
}

# Function to scrape NBA info using the page source and custom birth date format
scrape_nba_info_from_source <- function(nba_stats_link) {
  if (is.na(nba_stats_link)) {
    return(data.frame(NBA_Player_Name = NA, NBA_Birth_Date = NA, stringsAsFactors = FALSE))
  }
  
  # Read the page source using rvest
  page_source <- read_html(nba_stats_link)
  
  # Extract NBA Player Name from the <h1> tag
  nba_player_name <- page_source %>%
    html_element("h1") %>%
    html_text(trim = TRUE)
  
  # Extract birth month and day from the appropriate <a> tag
  birth_month_day <- page_source %>%
    html_element("span#necro-birth") %>%
    html_elements("a") %>%
    html_text2() %>%
    .[1]  # Select the first match for month and day
  
  # Extract birth year from the appropriate <a> tag
  birth_year <- page_source %>%
    html_element("span#necro-birth") %>%
    html_elements("a") %>%
    html_text2() %>%
    .[2]  # Select the second match for year
  
  # Combine the extracted birth date components
  nba_birth_date <- paste(birth_month_day, birth_year, sep = ", ")
  
  # Return the results as a dataframe
  return(data.frame(
    NBA_Player_Name = nba_player_name,
    NBA_Birth_Date = nba_birth_date,
    stringsAsFactors = FALSE
  ))
}

# Modify the function to exclude unwanted rows like "G League Index"
scrape_players_by_letter <- function(letter, remDr) {
  url <- paste0("https://www.basketball-reference.com/gleague/players/", letter, "/")
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
  
  # Filter out rows like "G League Index" or any other invalid names
  player_data <- player_data %>%
    filter(!grepl("Index", Player_Name, ignore.case = TRUE))  # Exclude rows containing "Index"
  
  # Extract 'From' and 'To' years using the extract_from_to function
  player_data <- player_data %>%
    mutate(From_To = map2(Seasons, Player_Name, extract_from_to),
           From = map_int(From_To, 1),
           To = map_int(From_To, 2)) %>%
    select(-From_To, -Seasons)  # Remove intermediate columns
  
  return(player_data)
}

# Directory to save partial CSV files
save_directory <- "C:/Users/djvia/OneDrive/Documents/Blog Website/Basketball_Database/MISCELLANEOUS"
partial_csv_file <- file.path(save_directory, "GLEAGUE_partial.csv")

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

# Completed letters from the letters to scrape
letters_to_scrape <- setdiff(letters, completed_letters)

# Loop over each letter and scrape player info
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
      
      # Scrape player info with NBA stats and College info check
      player_data <- retry(with_progress({ scrape_player_info_with_college(remDr, player_link) }))
      
      # Scrape NBA info if NBA Stats Link is available
      nba_info <- scrape_nba_info_from_source(player_data$NBA_Stats_Link)
      
      # Combine player info and NBA info
      full_player_data <- bind_cols(player_data, nba_info)
      
      # Add the 'From' and 'To' columns to player_data before combining
      full_player_data <- full_player_data %>%
        mutate(From = player_urls$From[i], To = player_urls$To[i], Letter = letter)
      
      # Add player data to the dataframe for the current letter
      letter_data <- bind_rows(letter_data, full_player_data)
      
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

# Create a dupes list specifically for Luke Martinez and Ra'Shad James
dupes_list <- all_player_data_combined %>%
  filter(toupper(Player_Name) %in% c("LUKE MARTINEZ", "RA'SHAD JAMES")) %>%
  mutate(Upper_Name = toupper(Player_Name))  # Create the Upper_Name column for grouping

# Clean the duplicates by keeping relevant information
cleaned_duplicates <- dupes_list %>%
  group_by(Upper_Name) %>%
  summarise(
    Player_Name = first(na.omit(Player_Name)),  # Keep the first non-NA occurrence of Player_Name
    Birth_Date = first(na.omit(Birth_Date)),  # Keep the first non-NA occurrence of Birth_Date
    Position = first(na.omit(Position)),  # Keep the first non-NA occurrence of Position
    Height = first(na.omit(Height)),  # Keep the first non-NA occurrence of Height
    Weight = first(na.omit(Weight)),  # Keep the first non-NA occurrence of Weight
    NBA = first(na.omit(NBA)),  # Keep the first non-NA occurrence of NBA
    NBA_Stats_Link = first(na.omit(NBA_Stats_Link)),  # Keep the first non-NA occurrence of NBA_Stats_Link
    College_Schools = first(na.omit(College_Schools)), # Keep the first non-NA occurrence of College_Schools
    NBA_Player_Name = first(na.omit(NBA_Player_Name)),  # Keep the first non-NA occurrence of NBA_Player_Name
    NBA_Birth_Date = first(na.omit(NBA_Birth_Date)),  # Keep the first non-NA occurrence of NBA_Birth_Date
    From = min(From, na.rm = TRUE),  # Keep the earliest 'From' year
    To = max(To, na.rm = TRUE),  # Keep the latest 'To' year
    Letter = first(na.omit(Letter)),  # Keep the first non-NA occurrence of Letter
    .groups = 'drop'
  ) %>%
  select(-Upper_Name)  # Drop the Upper_Name column after cleaning

# Remove all instances of Luke Martinez and Ra'Shad James from the original dataset
all_player_data_combined <- all_player_data_combined %>%
  filter(!toupper(Player_Name) %in% c("LUKE MARTINEZ", "RA'SHAD JAMES"))

# Add the cleaned Luke Martinez and Ra'Shad James data back into the dataset
all_player_data_combined <- bind_rows(all_player_data_combined, cleaned_duplicates)

# Updating to remove links and updating Sheldon Mac specifically
all_player_data_combined <- all_player_data_combined %>%
  select(-NBA_Stats_Link, -Letter) %>%
  mutate(NBA = ifelse(Player_Name == "Sheldon Mac", "yes", NBA),
         From = ifelse(Player_Name == "Sheldon Mac", 2017, From),
         Position = ifelse(Player_Name == "Sheldon Mac", "G", Position),
         College_Schools = ifelse(Player_Name == "Sheldon Mac", "Texas, Miami (FL)", College_Schools),
         NBA_Player_Name = ifelse(Player_Name == "Sheldon Mac", "Sheldon Mac", NBA_Player_Name),
         NBA_Birth_Date = ifelse(Player_Name == "Sheldon Mac", "December 21, 1992 ", NBA_Birth_Date))

# Rename columns and update birth date
all_player_data_combined <- all_player_data_combined %>%
  rename(Player = Player_Name, `Birth Date` = Birth_Date,
         Pos = Position, Ht = Height, Wt = Weight, Colleges = College_Schools) %>%
  mutate(`Birth Date` = ifelse(!is.na(NBA_Birth_Date) & NBA_Birth_Date != "", NBA_Birth_Date, `Birth Date`)) %>%
  select(-NBA_Birth_Date)

# Load in nba player index
nba_index <- read_csv("C:/Users/djvia/OneDrive/Documents/Blog Website/Basketball_Database/MISCELLANEOUS/NBA_ABA_PLAYER_INDEX.csv")

# Perform a left join on 'NBA_Player_Name' and 'Player', and also 'Birth Date'
player_data_joined <- all_player_data_combined %>%
  left_join(nba_index %>%
              select(`Player ID`, Player, `Birth Date`), 
            by = c("NBA_Player_Name" = "Player", "Birth Date" = "Birth Date")) %>%
  rename(`NBA ID` = `Player ID`) %>% # Rename 'Player ID' to 'NBA ID'
  select(-NBA_Player_Name)

# Making Player ID for gleague players
gleague_player_index <- player_data_joined %>%
  arrange(From, Player) %>%
  mutate(`Player ID` = 1000 + row_number(),
         Active = if_else(To == most_recent_nba_season() |
                            To == max(player_data_joined$To, na.rm = TRUE), "Yes", "No"))

# Coalesce to overwrite only NA values in gleague_player_index with nba_index values
gleague_player_index <- gleague_player_index %>%
  left_join(nba_index %>%
              select(`Player ID`, Pos, Ht, Wt, `Birth Date`, Colleges), 
            by = c("NBA ID" = "Player ID")) %>%
  mutate(
    Pos = coalesce(Pos.x, Pos.y),   # Overwrite NA in Pos
    Ht = coalesce(Ht.x, Ht.y),      # Overwrite NA in Ht
    Wt = coalesce(Wt.x, Wt.y),      # Overwrite NA in Wt
    `Birth Date` = coalesce(`Birth Date.x`, `Birth Date.y`),  # Overwrite NA in Birth Date
    Colleges = coalesce(Colleges.x, Colleges.y),
    .keep = "unused"                # Keep only the updated columns
  )

# Modify the position column in gleague_player_index using mutate
gleague_player_index <- gleague_player_index %>%
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

# After coalescing, drop any extra columns created during the join
gleague_player_index <- gleague_player_index %>%
  select(`Player ID`, `NBA ID`, Player, From, To, Pos, Ht, Wt, `Birth Date`, Colleges, Active)

# Write to a csv for G League Player Index
write_csv(gleague_player_index,"C:/Users/djvia/OneDrive/Documents/Blog Website/Basketball_Database/MISCELLANEOUS/GLEAGUE_PLAYER_INDEX.csv")

# Delete the partial CSV file after successful processing
if (file.exists(partial_csv_file)) {
  file.remove(partial_csv_file)
  message("Partial CSV file has been deleted.")
} else {
  message("No partial CSV file found to delete.")
}
