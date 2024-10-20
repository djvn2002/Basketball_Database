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

# Function to extract 'From' and 'To' years properly with improved logic
extract_from_to <- function(seasons) {
  clean_season <- gsub("[^0-9to-]", "", seasons)
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

# Function to Scrape Player Names, Seasons, and Player Links for Each Letter Page
scrape_players <- function(letter, remDr) {
  url <- paste0("https://www.basketball-reference.com/nbl/players/", letter, "/")
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
  
  player_data <- player_data %>%
    filter(!grepl("NBL|Sports Reference|Copyright", Player_Name)) %>%
    filter(!grepl("Sports Reference", Seasons)) 
  
  player_data <- player_data %>%
    mutate(
      From_To = map(Seasons, extract_from_to),
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

# Function to Scrape Birth Date for Each Player
scrape_birth_date <- function(player_link, remDr) {
  retry({
    remDr$navigate(player_link)
    Sys.sleep(2)  # Allow page to load
    
    birth_element <- remDr$findElement(using = "xpath", "//div[@id='meta']//p[strong[text()='Born:']]/span")
    birth_date <- birth_element$getElementText()[[1]]
    birth_date
  }, silent = TRUE)
}

# Function to Scrape Position for Each Player
scrape_position <- function(player_link, remDr) {
  retry({
    remDr$navigate(player_link)
    Sys.sleep(2)  # Allow page to load
    
    position_element <- remDr$findElement(using = "xpath", "//div[@id='meta']//p[strong[contains(text(),'Position:')]]")
    position_text <- position_element$getElementText()[[1]]
    position <- str_extract(position_text, "(Guard-Forward|Forward-Center|Center-Forward|Forward-Guard|Guard|Forward|Center)")
    if (position == "") return(NA) else return(position)
  }, silent = TRUE)
}

# Function to Scrape Height for Each Player
scrape_height <- function(player_link, remDr) {
  retry({
    remDr$navigate(player_link)
    Sys.sleep(2)  # Allow page to load
    
    height_element <- remDr$findElement(using = "xpath", "//div[@id='meta']//p/span[contains(text(), '-') and not(contains(text(), 'cm'))]")
    height_text <- height_element$getElementText()[[1]]
    height <- str_extract(height_text, "\\d+-\\d+")  # Extract height in '5-10' format
    if (height == "") return(NA) else return(height)
  }, silent = TRUE)
}

# Function to Scrape Weight for Each Player
scrape_weight <- function(player_link, remDr) {
  retry({
    remDr$navigate(player_link)
    Sys.sleep(2)  # Allow page to load
    
    weight_element <- remDr$findElement(using = "xpath", "//div[@id='meta']//p/span[contains(text(), 'lb') and not(contains(text(), 'kg'))]")
    weight_text <- weight_element$getElementText()[[1]]
    weight <- str_extract(weight_text, "\\d+")  # Extract weight number only
    if (weight == "") return(NA) else return(weight)
  }, silent = TRUE)
}

# Scrape All Players From A to Z (Excluding X)
all_player_data <- data.frame()

# Exclude 'x' from the letters
for (letter in setdiff(letters, 'x')) {
  message(paste("Starting to scrape players from letter:", letter))
  
  # Step 1: Scrape players for the current letter
  player_data <- with_progress({
    scrape_players(letter, remDr)
  })
  
  # Step 2: Remove rows with empty Player_Name or invalid Player_Link
  player_data <- player_data %>%
    filter(Player_Name != "" & !grepl("email|javascript:", Player_Link))
  
  # Step 3: Scrape additional information for each player in the current letter
  player_data <- player_data %>%
    mutate(
      Birth_Date = map_chr(Player_Link, ~ scrape_birth_date(.x, remDr), .default = NA_character_),
      Position = map_chr(Player_Link, ~ scrape_position(.x, remDr)),
      Height = map_chr(Player_Link, ~ scrape_height(.x, remDr)),
      Weight = map_chr(Player_Link, ~ scrape_weight(.x, remDr))
    ) %>%
    mutate(across(c(Birth_Date, Position, Height, Weight), ~ ifelse(. == "", NA, .)))
  
  # Step 4: Combine current letter data with overall data
  all_player_data <- bind_rows(all_player_data, player_data)
  
  message(paste("Completed scraping players from letter:", letter))
}

# Print the combined player data
print(all_player_data)

# Close RSelenium Session
remDr$close()
rs_driver_object$server$stop()

# Rename and organize columns
nbl_player_index <- all_player_data %>%
  rename(Player = Player_Name,
         `Birth Date` = Birth_Date,
         Ht = Height,
         Wt = Weight,
         Pos = Position) %>%
  select(-Player_Link) %>%
  select(Player, From, To, Pos, Ht, Wt, `Birth Date`)

# Making Player ID
nbl_player_index <- nbl_player_index %>%
  arrange(as.numeric(From, Player)) %>%
  mutate(`NBL ID` = 1000 + row_number())

# Read in csv for NBA ID for players
nba_index <- read_csv("C:/Users/djvia/OneDrive/Documents/Blog Website/Basketball_Database/MISCELLANEOUS/NBA_ABA_PLAYER_INDEX.csv")

# Perform the join to add NBA ID to NBL player index
nbl_player_index <- nbl_player_index %>%
  left_join(nba_index %>% select(Player,`Player ID`), 
            by = c("Player")) %>%
  rename(`NBA ID` = `Player ID`) %>%
  select(`NBL ID`, `NBA ID`, everything())

# Correct the `Don Smith` with NBL ID 1112
nbl_player_index <- nbl_player_index %>%
  mutate(`NBA ID` = if_else(`NBL ID` == 1112, NA_real_, `NBA ID`))

# Removing a second instance of Don Smith
nbl_player_index <- nbl_player_index %>% 
  slice(-112)

# Identify duplicates
duplicates <- nbl_player_index %>%
  group_by(Player) %>%
  filter(n() > 1) %>%
  arrange(Player) %>%
  filter(!(`NBA ID` == 1281 & `NBL ID` == 1390) &
           !(`NBA ID` == 1248 & `NBL ID` == 1567) & 
           !(`NBL ID` == 1112))

# Remove duplicates from the original nbl_player_index
nbl_player_index <- nbl_player_index %>%
  anti_join(duplicates)

# Modify the position column in nbl_player_index using mutate
nbl_player_index <- nbl_player_index %>%
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

# Perform a left join on NBA ID
merged_df <- merge(nbl_player_index, nba_index, by.x = "NBA ID", by.y = "Player ID", all.x = TRUE, suffixes = c("_nbl", "_nba"))

# Use mutate and coalesce to only replace NA values in NBL with NBA data
updated_nbl_player_index <- merged_df %>%
  mutate(
    Pos_nbl = coalesce(Pos_nbl, Pos_nba),
    Ht_nbl = coalesce(Ht_nbl, Ht_nba),
    Wt_nbl = coalesce(Wt_nbl, Wt_nba),
    Birth_Date_nbl = coalesce(`Birth Date_nbl`, `Birth Date_nba`)
  )

# Drop the extra NBA columns if you don't need them anymore and rename columns for clarity
nbl_player_index <- updated_nbl_player_index %>%
  select(`NBL ID`, `NBA ID`, Player_nbl, From_nbl, To_nbl, Pos_nbl, Ht_nbl, Wt_nbl, Birth_Date_nbl) %>%
  rename(
    `NBL ID` = `NBL ID`,
    `NBA ID` = `NBA ID`,
    Player = Player_nbl,
    From = From_nbl,
    To = To_nbl,
    Pos = Pos_nbl,
    Ht = Ht_nbl,
    Wt = Wt_nbl,
    `Birth Date` = Birth_Date_nbl
  ) %>%
  arrange(`NBL ID`)

# Write to a csv for NBL Player Index
write_csv(nbl_player_index,"C:/Users/djvia/OneDrive/Documents/Blog Website/Basketball_Database/MISCELLANEOUS/NBL_PLAYER_INDEX.csv")