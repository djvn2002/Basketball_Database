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
rs_driver_object <- rsDriver(browser = 'chrome', extraCapabilities = eCaps, verbose = FALSE, port = free_port(), chromever = "128.0.6613.119")
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
  
  # Scrape player URLs and seasons for the current letter
  player_urls <- scrape_players_by_letter(letter, remDr)
  
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
    
    message(paste("Finished scraping player:", player_urls$Player_Name[i]))
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
  rs_driver_object <- rsDriver(browser = 'chrome', extraCapabilities = eCaps, verbose = FALSE, port = free_port(), chromever = "128.0.6613.119")
  remDr <- rs_driver_object$client
}

# Final cleanup
remDr$close()
rs_driver_object$server$stop()

# Rearrange and assign Player IDs for WNBA players
all_player_data_combined <- all_player_data_combined %>%
  arrange(From, Player_Name) %>%
  mutate(`Player ID` = 1000 + row_number(),
         Active = if_else(To == most_recent_wnba_season(), "Yes", "No")) %>%
  rename(Player = Player_Name,`Birth Date` = Birth_Date, Pos = Position, Ht = Height,
         Wt = Weight, Colleges = College) %>%
  select(`Player ID`, Player, From, To, Pos, Ht, Wt, `Birth Date`, Colleges, Active)

# Players with no NAs in any columns
players_no_na <- all_player_data_combined[complete.cases(all_player_data_combined), ]

# Players with NAs in at least one column
players_with_na <- all_player_data_combined[!complete.cases(all_player_data_combined), ]

# Update player specifcs that have na in them
na_players_updated <- players_with_na %>%
  mutate(Pos = case_when(
    `Player ID` == 1183 ~ "Forward",`Player ID` == 1803 ~ "Guard-Forward",`Player ID` == 2080 ~ "Guard",
    TRUE ~ Pos
    ),
    Ht = case_when(
      `Player ID` == 1004 ~ NA,`Player ID` == 1056 ~ NA,`Player ID` == 1070 ~ "5-9",
      `Player ID` == 1077 ~ "6-2",`Player ID` == 1089 ~ NA,`Player ID` == 1103 ~ "5-11",
      `Player ID` == 1108 ~ "5-10",`Player ID` == 1143 ~ "6-5",`Player ID` == 1151 ~ NA,
      `Player ID` == 1158 ~ "5-4",`Player ID` == 1167 ~ "6-1",`Player ID` == 1169 ~ "6-3",
      `Player ID` == 1177 ~ NA,`Player ID` == 1183 ~ "6-1",`Player ID` == 1199 ~ "5-8",
      `Player ID` == 1231 ~ "6-2",`Player ID` == 1284 ~ "6-3",`Player ID` == 1299 ~ "6-4",
      `Player ID` == 1443 ~ "6-0",`Player ID` == 1543 ~ "6-0",`Player ID` == 1558 ~ "6-2",
      `Player ID` == 1625 ~ "6-6",`Player ID` == 1642 ~ "6-4",`Player ID` == 1687 ~ "5-8",
      `Player ID` == 1699 ~ "6-0",`Player ID` == 1702 ~ "6-8",`Player ID` == 1724 ~ "6-2",
      `Player ID` == 1725 ~ "5-7",`Player ID` == 1738 ~ "6-9",`Player ID` == 1760 ~ "5-9",
      `Player ID` == 1767 ~ "6-2",`Player ID` == 1803 ~ "6-0",`Player ID` == 1822 ~ "5-11",
      `Player ID` == 1828 ~ "5-10",`Player ID` == 1842 ~ "6-4",`Player ID` == 1849 ~ "6-7",
      `Player ID` == 1855 ~ "6-1",`Player ID` == 1859 ~ "5-6",`Player ID` == 1870 ~ "6-0",
      `Player ID` == 1896 ~ "6-4",`Player ID` == 1928 ~ "5-8",`Player ID` == 1937 ~ "6-7",
      `Player ID` == 1947 ~ "6-1",`Player ID` == 1948 ~ "5-8",`Player ID` == 1953 ~ "6-7",
      `Player ID` == 1975 ~ "6-2",`Player ID` == 1977 ~ "5-11",`Player ID` == 1982 ~ "5-10",
      `Player ID` == 2011 ~ "6-0",`Player ID` == 2018 ~ "6-1",`Player ID` == 2033 ~ "6-1",
      `Player ID` == 2080 ~ "5-8",`Player ID` == 2092 ~ "6-0",
      TRUE ~ Ht
      ),
    Wt = case_when(
      `Player ID` == 1070 ~ 150,`Player ID` == 1077 ~ 190,`Player ID` == 1103 ~ 168,
      `Player ID` == 1108 ~ 170,`Player ID` == 1143 ~ 185,`Player ID` == 1158 ~ 125,
      `Player ID` == 1167 ~ 191,`Player ID` == 1169 ~ 182,`Player ID` == 1183 ~ 185,
      `Player ID` == 1199 ~ 129,`Player ID` == 1231 ~ 199,`Player ID` == 1299 ~ 170,
      `Player ID` == 1443 ~ 163,`Player ID` == 1543 ~ 174,`Player ID` == 1558 ~ 180,
      `Player ID` == 1625 ~ 219,`Player ID` == 1642 ~ 140,`Player ID` == 1687 ~ 137,
      `Player ID` == 1699 ~ 209,`Player ID` == 1702 ~ 216,`Player ID` == 1724 ~ 190,
      `Player ID` == 1725 ~ 165,`Player ID` == 1738 ~ 205,`Player ID` == 1760 ~ 140,
      `Player ID` == 1767 ~ 203,`Player ID` == 1803 ~ 166,`Player ID` == 1822 ~ 180,
      `Player ID` == 1828 ~ 160,`Player ID` == 1842 ~ 170,`Player ID` == 1849 ~ 215,
      `Player ID` == 1855 ~ 165,`Player ID` == 1859 ~ 130,`Player ID` == 1870 ~ 167,
      `Player ID` == 1896 ~ 195,`Player ID` == 1928 ~ 165,`Player ID` == 1937 ~ 245,
      `Player ID` == 1947 ~ 173,`Player ID` == 1948 ~ 128,`Player ID` == 1953 ~ 239,
      `Player ID` == 1975 ~ 152,`Player ID` == 1977 ~ 165,`Player ID` == 1982 ~ 152,
      `Player ID` == 2011 ~ 158,`Player ID` == 2018 ~ 176,`Player ID` == 2033 ~ 170,
      `Player ID` == 2092 ~ 156,
      TRUE ~ Wt
      ),
    `Birth Date` = case_when(
      `Player ID` == 1011 ~ "April 14, 1963",`Player ID` == 1012 ~ "November 22, 1968",
      `Player ID` == 1023 ~ "November 12, 1971",`Player ID` == 1029 ~ "June 13, 1964",
      `Player ID` == 1032 ~ "March 17, 1968",`Player ID` == 1059 ~ "July 1, 1958",
      `Player ID` == 1066 ~ "October 6, 1973",`Player ID` == 1070 ~ "May 27, 1967",
      `Player ID` == 1073 ~ "March 25, 1971",`Player ID` == 1077 ~ "September 27, 1975",
      `Player ID` == 1082 ~ "December 8, 1965",`Player ID` == 1093 ~ "October 4, 1967",
      `Player ID` == 1098 ~ "February 3, 1977",`Player ID` == 1103 ~ "February 11, 1976",
      `Player ID` == 1108 ~ "September 30, 1976",`Player ID` == 1113 ~ "March 16, 1965",
      `Player ID` == 1134 ~ "October 11, 1964",`Player ID` == 1137 ~ "December 17, 1971",
      `Player ID` == 1143 ~ "September 26, 1976",`Player ID` == 1158 ~ "February 26, 1973",
      `Player ID` == 1167 ~ "June 12, 1974",`Player ID` == 1169 ~ "November 5, 1973",
      `Player ID` == 1170 ~ "March 11, 1977",`Player ID` == 1176 ~ "May 3, 1976",
      `Player ID` == 1183 ~ "September 11, 1974",`Player ID` == 1194 ~ "May 15, 1974",
      `Player ID` == 1196 ~ "August 15, 1975",`Player ID` == 1198 ~ "June 4, 1974",
      `Player ID` == 1199 ~ "November 2, 1974",`Player ID` == 1219 ~ "August 18, 1974",
      `Player ID` == 1220 ~ "May 5, 1974",`Player ID` == 1221 ~ "October 4, 1969",
      `Player ID` == 1224 ~ "October 20, 1970",`Player ID` == 1226 ~ "March 6, 1970",
      `Player ID` == 1227 ~ "March 13, 1968",`Player ID` == 1231 ~ "April 9, 1972",
      `Player ID` == 1235 ~ "March 1, 1970",`Player ID` == 1236 ~ "February 15, 1977",
      `Player ID` == 1243 ~ "December 4, 1976",`Player ID` == 1249 ~ "November 28, 1977",
      `Player ID` == 1284 ~ "March 6, 1976",`Player ID` == 1299 ~ "December 5, 1974",
      `Player ID` == 1324 ~ "August 25, 1979",`Player ID` == 1325 ~ "June 5, 1979",
      `Player ID` == 1340 ~ "October 14, 1977",`Player ID` == 1343 ~ "May 11, 1981",
      `Player ID` == 1347 ~ "October 13, 1978",`Player ID` == 1352 ~ "September 24, 1977",
      `Player ID` == 1356 ~ "August 28, 1979",`Player ID` == 1362 ~ "January 27, 1978",
      `Player ID` == 1393 ~ "March 22, 1979",`Player ID` == 1397 ~ "October 16, 1980",
      `Player ID` == 1399 ~ "September 22, 1979",`Player ID` == 1402 ~ "July 21, 1979",
      `Player ID` == 1403 ~ "April 12, 1980",`Player ID` == 1435 ~ "May 14, 1982",
      `Player ID` == 1443 ~ "June 11, 1982",`Player ID` == 1444 ~ "July 19, 1982",
      `Player ID` == 1462 ~ "March 13, 1982",`Player ID` == 1465 ~ "December 11, 1981",
      `Player ID` == 1489 ~ "November 17, 1982",`Player ID` == 1500 ~ "July 19, 1982",
      `Player ID` == 1508 ~ "November 29, 1983", `Player ID` == 1538 ~ "December 2, 1983",
      `Player ID` == 1543 ~ "April 30, 1984",`Player ID` == 1547 ~ "December 15, 1983",
      `Player ID` == 1555 ~ "April 3, 1985",`Player ID` == 1558 ~ "January 18, 1985",
      `Player ID` == 1577 ~ "January 3, 1985",`Player ID` == 1579 ~ "April 15, 1985",
      `Player ID` == 1582 ~ "April 26, 1985",`Player ID` == 1590 ~ "April 19, 1986",
      `Player ID` == 1611 ~ "September 11, 1986",`Player ID` == 1618 ~ "February 27, 1986",
      `Player ID` == 1625 ~ "October 6, 1985",`Player ID` == 1633 ~ "September 10, 1986",
      `Player ID` == 1636 ~ "January 11, 1987",`Player ID` == 1642 ~ "August 21, 1987",
      `Player ID` == 1643 ~ "January 24, 1987",`Player ID` == 1654 ~ "August 28, 1986",
      `Player ID` == 1659 ~ "December 26, 1987",`Player ID` == 1670 ~ "May 14, 1988",
      `Player ID` == 1677 ~ "July 15, 1988",`Player ID` == 1685 ~ "July 19, 1989",
      `Player ID` == 1687 ~ "February 8, 1989",`Player ID` == 1694 ~ "November 12, 1988",
      `Player ID` == 1696 ~ "May 2, 1989",`Player ID` == 1698 ~ "February 23, 1988",
      `Player ID` == 1699 ~ "September 11, 1989",`Player ID` == 1702 ~ "August 18, 1991",
      `Player ID` == 1709 ~ "July 7, 1987",`Player ID` == 1724 ~ "July 2, 1990",
      `Player ID` == 1725 ~ "May 28, 1990",`Player ID` == 1732 ~ "September 20, 1989",
      `Player ID` == 1738 ~ "October 18, 1990",`Player ID` == 1745 ~ "September 5, 1989",
      `Player ID` == 1755 ~ "May 2, 1991",`Player ID` == 1760 ~ "August 2, 1990",
      `Player ID` == 1763 ~ "March 2, 1991",`Player ID` == 1767 ~ "April 12, 1992",
      `Player ID` == 1774 ~ "March 21, 1992",`Player ID` == 1783 ~ "June 25, 1992",
      `Player ID` == 1787 ~ "September 2, 1991",`Player ID` == 1792 ~ "January 8, 1992",
      `Player ID` == 1793 ~ "May 18, 1992",`Player ID` == 1800 ~ "September 8, 1993",
      `Player ID` == 1803 ~ "October 29, 1993",`Player ID` == 1805 ~ "June 11, 1993",
      `Player ID` == 1809 ~ "October 8, 1992",`Player ID` == 1810 ~ "August 22, 1992",
      `Player ID` == 1814 ~ "November 6, 1993",`Player ID` == 1821 ~ "October 5, 1993",
      `Player ID` == 1822 ~ "November 3, 1993",`Player ID` == 1827 ~ "November 22, 1992",
      `Player ID` == 1828 ~ "February 22, 1992",`Player ID` == 1838 ~ "January 17, 1994",
      `Player ID` == 1842 ~ "August 27, 1994",`Player ID` == 1849 ~ "October 11, 1994",
      `Player ID` == 1850 ~ "September 27, 1993",`Player ID` == 1854 ~ "January 5, 1994",
      `Player ID` == 1855 ~ "August 28, 1994",`Player ID` == 1859 ~ "March 8, 1994",
      `Player ID` == 1870 ~ "January 12, 1995",`Player ID` == 1872 ~ "December 18, 1995",
      `Player ID` == 1873 ~ "February 7, 1994",`Player ID` == 1880 ~ "March 15, 1995",
      `Player ID` == 1881 ~ "August 24, 1994",`Player ID` == 1890 ~ "May 18, 1995",
      `Player ID` == 1894 ~ "September 8, 1992",`Player ID` == 1896 ~ "August 8, 1996",
      `Player ID` == 1898 ~ "April 7, 1995",`Player ID` == 1899 ~ "July 30, 1996",
      `Player ID` == 1900 ~ "February 1, 1996",`Player ID` == 1903 ~ "September 9, 1996",
      `Player ID` == 1911 ~ "February 22, 1996",`Player ID` == 1919 ~ "May 30, 1995",
      `Player ID` == 1921 ~ "February 24, 1995",`Player ID` == 1925 ~ "September 10, 1996",
      `Player ID` == 1928 ~ "March 2, 1997",`Player ID` == 1929 ~ "July 5, 1996",
      `Player ID` == 1934 ~ "September 16, 1997",`Player ID` == 1937 ~ "March 21, 1997",
      `Player ID` == 1947 ~ "September 23, 1996",`Player ID` == 1948 ~ "February 10, 1997",
      `Player ID` == 1953 ~ "September 28, 1996",`Player ID` == 1958 ~ "November 14, 1998",
      `Player ID` == 1969 ~ "December 6, 1995",`Player ID` == 1975 ~ "August 21, 1998",
      `Player ID` == 1976 ~ "April 28, 1998",`Player ID` == 1977 ~ "December 6, 1997",
      `Player ID` == 1978 ~ "April 25, 1998",`Player ID` == 1982 ~ "May 1, 1998",
      `Player ID` == 1984 ~ "August 20, 1998",`Player ID` == 1995 ~ "January 8, 1998",
      `Player ID` == 2000 ~ "August 10, 1999",`Player ID` == 2005 ~ "November 13, 1998",
      `Player ID` == 2011 ~ "September 28, 1998",`Player ID` == 2018 ~ "March 16, 2000",
      `Player ID` == 2024 ~ "August 8, 2000",`Player ID` == 2028 ~ "August 17, 2000",
      `Player ID` == 2029 ~ "June 29, 2000",`Player ID` == 2033 ~ "February 24, 1999",
      `Player ID` == 2034 ~ "April 29, 2000",`Player ID` == 2037 ~ "July 25, 2000",
      `Player ID` == 2053 ~ "May 21, 2001",`Player ID` == 2064 ~ "February 26, 2000",
      `Player ID` == 2073 ~ "May 6, 2002",`Player ID` == 2075 ~ "January 22, 2002",
      `Player ID` == 2080 ~ "June 26, 1992",`Player ID` == 2087 ~ "April 30, 2001",
      `Player ID` == 2092 ~ "April 9, 2001",`Player ID` == 2094 ~ "March 16, 2001",
      TRUE ~ `Birth Date`
      ),
    Colleges = case_when(
      `Player ID` == 1011 ~ "USC",`Player ID` == 1012 ~ "Tennessee",`Player ID` == 1023 ~ "Virginia",
      `Player ID` == 1029 ~ "Ole Miss",`Player ID` == 1032 ~ "Hawaii",`Player ID` == 1045 ~ "USC",
      `Player ID` == 1059 ~ "Old Dominion",`Player ID` == 1066 ~ "UConn",`Player ID` == 1070 ~ "Auburn",
      `Player ID` == 1073 ~ "Texas Tech",`Player ID` == 1077 ~ "George Washington",`Player ID` == 1082 ~ "Louisiana Tech",
      `Player ID` == 1086 ~ "USC",`Player ID` == 1093 ~ "Maryland",`Player ID` == 1098 ~ "Arizona",
      `Player ID` == 1103 ~ "Harvard University",`Player ID` == 1108 ~ "Iowa",`Player ID` == 1113 ~ "Cal State Long Beach",
      `Player ID` == 1134 ~ "USC",`Player ID` == 1137 ~ "Tennessee",`Player ID` == 1143 ~ "Indiana",
      `Player ID` == 1155 ~ "Old Dominion",`Player ID` == 1158 ~ "Boise State",`Player ID` == 1167 ~ "Penn State",
      `Player ID` == 1169 ~ "Southern Nazarene University",`Player ID` == 1170 ~ "Colorado State",`Player ID` == 1173 ~ "Tennesse",
      `Player ID` == 1176 ~ "NC State",`Player ID` == 1182 ~ "Virginia",`Player ID` == 1183 ~ "Florida",
      `Player ID` == 1194 ~ "UConn",`Player ID` == 1196 ~ "UConn",`Player ID` == 1198 ~ "Ohio State",
      `Player ID` == 1199 ~ "Georgia",`Player ID` == 1219 ~ "South Carolina",`Player ID` == 1220 ~ "Vanderbilt",
      `Player ID` == 1221 ~ "Stanford",`Player ID` == 1224 ~ "Georgia State University, St. Edward's University",
      `Player ID` == 1226 ~ "Georgia, Central Florida",`Player ID` == 1227 ~ "Tennessee",`Player ID` == 1231 ~ "Stanford",
      `Player ID` == 1235 ~ "Florida Atlantic University",`Player ID` == 1236 ~ "USC",`Player ID` == 1243 ~ "Louisiana Tech",
      `Player ID` == 1249 ~ "Virginia",`Player ID` == 1284 ~ "Stanford",`Player ID` == 1299 ~ "Purdue",
      `Player ID` == 1322 ~ "Georgia",`Player ID` == 1324 ~ "Georgia",`Player ID` == 1325 ~ "Master's College",
      `Player ID` == 1337 ~ "Purdue",`Player ID` == 1338 ~ "Georgia",`Player ID` == 1340 ~ "UConn",
      `Player ID` == 1347 ~ "LSU",`Player ID` == 1352 ~ "Notre Dame",`Player ID` == 1356 ~ "Notre Dame",
      `Player ID` == 1362 ~ "Rutgers University",`Player ID` == 1393 ~ "UNC",`Player ID` == 1397 ~ "UConn",
      `Player ID` == 1399 ~ "UConn",`Player ID` == 1402 ~ "Tennessee",`Player ID` == 1403 ~ "UConn",
      `Player ID` == 1409 ~ "Louisiana Tech",`Player ID` == 1421 ~ "Tennessee", `Player ID` == 1435 ~ "Duke",
      `Player ID` == 1443 ~ "UConn",`Player ID` == 1444 ~ "LSU", `Player ID` == 1458 ~ "Minnesota",
      `Player ID` == 1462 ~ "Kansas State",`Player ID` == 1465 ~ "Georgetown",`Player ID` == 1489 ~ "Liberty",
      `Player ID` == 1500 ~ "Florida State",`Player ID` == 1508 ~ "Penn State",`Player ID` == 1509 ~ "LSU",
      `Player ID` == 1518 ~ "Temple",`Player ID` == 1519 ~ "Rutgers University",`Player ID` == 1538 ~ "Duke",
      `Player ID` == 1543 ~ "LSU",`Player ID` == 1547 ~ "Baylor",`Player ID` == 1555 ~ "Ole Miss",
      `Player ID` == 1558 ~ "UNC",`Player ID` == 1573 ~ "Duke",`Player ID` == 1577 ~ "UCLA",
      `Player ID` == 1579 ~ "USC",`Player ID` == 1582 ~ "Texas",`Player ID` == 1587 ~ "DePaul",
      `Player ID` == 1590 ~ "Tennessee",`Player ID` == 1591 ~ "Stanford",`Player ID` == 1597 ~ "Maryland",
      `Player ID` == 1611 ~ "UNC", `Player ID` == 1618 ~ "Tennessee",`Player ID` == 1625 ~ "LSU",
      `Player ID` == 1633 ~ "Louisville",`Player ID` == 1636 ~ "Arizona State",`Player ID` == 1641 ~ "Oklahoma",
      `Player ID` == 1642 ~ "Auburn",`Player ID` == 1643 ~ "Rutgers University",`Player ID` == 1645 ~ "Maryland",
      `Player ID` == 1652 ~ "UConn",`Player ID` == 1654 ~ "Pitt",`Player ID` == 1659 ~ "Iowa State",
      `Player ID` == 1666 ~ "Rutgers University",`Player ID` == 1670 ~ "Stanford",`Player ID` == 1677 ~ "Virginia",
      `Player ID` == 1680 ~ "UConn",`Player ID` == 1685 ~ "Boston College",`Player ID` == 1687 ~ "Gonzaga",
      `Player ID` == 1690 ~ "Oklahoma",`Player ID` == 1694 ~ "Ohio State",`Player ID` == 1696 ~ "Stanford",
      `Player ID` == 1698 ~ "UNC",`Player ID` == 1699 ~ "Duke",`Player ID` == 1703 ~ "UConn",
      `Player ID` == 1709 ~ "Middle Tennessee",`Player ID` == 1724 ~ "Stanford",`Player ID` == 1725 ~ "Miami (FL)",
      `Player ID` == 1732 ~ "UConn",`Player ID` == 1738 ~ "Baylor",`Player ID` == 1745 ~ "University of Delaware",
      `Player ID` == 1755 ~ "California",`Player ID` == 1760 ~ "Notre Dame",`Player ID` == 1763 ~ "Maryland",
      `Player ID` == 1767 ~ "Maryland",`Player ID` == 1774 ~ "Stanford",`Player ID` == 1783 ~ "Notre Dame",
      `Player ID` == 1787 ~ "Florida State",`Player ID` == 1792 ~ "UConn",`Player ID` == 1793 ~ "LSU",
      `Player ID` == 1800 ~ "Minnesota",`Player ID` == 1803 ~ "Rutgers University",`Player ID` == 1805 ~ "California",
      `Player ID` == 1809 ~ "Duke",`Player ID` == 1810 ~ "Middle Tennessee",`Player ID` == 1814 ~ "Wake Forest",
      `Player ID` == 1815 ~ "Duke",`Player ID` == 1821 ~ "Notre Dame",`Player ID` == 1822 ~ "UConn",
      `Player ID` == 1827 ~ "Notre Dame",`Player ID` == 1828 ~ "Maryland, Saint Joseph's",`Player ID` == 1838 ~ "Michigan State",
      `Player ID` == 1842 ~ "UConn",`Player ID` == 1849 ~ "Texas",`Player ID` == 1850 ~ "Tennessee",
      `Player ID` == 1854 ~ "Clemson, George Washington",`Player ID` == 1855 ~ "Rutgers University",`Player ID` == 1859 ~ "UConn",
      `Player ID` == 1870 ~ "UNC, South Carolina",`Player ID` == 1872 ~ "Maryland",`Player ID` == 1873 ~ "Syracuse",
      `Player ID` == 1880 ~ "Georgia Tech, South Carolina",`Player ID` == 1881 ~ "Washington",`Player ID` == 1890 ~ "Maryland",
      `Player ID` == 1894 ~ "Harvard University, USC",`Player ID` == 1896 ~ "South Carolina",`Player ID` == 1898 ~ "South Carolina",
      `Player ID` == 1899 ~ "Texas",`Player ID` == 1900 ~ "Duke, UConn",`Player ID` == 1901 ~ "Tennessee",
      `Player ID` == 1903 ~ "UConn",`Player ID` == 1911 ~ "UConn",`Player ID` == 1919 ~ "Louisville",
      `Player ID` == 1921 ~ "Ohio State",`Player ID` == 1925 ~ "Stanford",`Player ID` == 1928 ~ "Notre Dame",
      `Player ID` == 1929 ~ "Notre Dame",`Player ID` == 1934 ~ "Notre Dame",`Player ID` == 1937 ~ "Baylor",
      `Player ID` == 1947 ~ "UConn",`Player ID` == 1948 ~ "Marquette",`Player ID` == 1953 ~ "Mississippi State",
      `Player ID` == 1958 ~ "Texas A&M",`Player ID` == 1969 ~ "NC State",`Player ID` == 1975 ~ "South Carolina",
      `Player ID` == 1976 ~ "Oregon",`Player ID` == 1977 ~ "Oregon",`Player ID` == 1978 ~ "Oregon",
      `Player ID` == 1982 ~ "South Carolina",`Player ID` == 1984 ~ "Washington, Arizona",`Player ID` == 1995 ~ "Stanford, Baylor",
      `Player ID` == 2000 ~ "UCLA",`Player ID` == 2005 ~ "Virginia Tech",`Player ID` == 2011 ~ "Tennessee, UConn",
      `Player ID` == 2018 ~ "Ohio State, Florida Gulf Coast",`Player ID` == 2024 ~ "Baylor",`Player ID` == 2028 ~ "UConn",
      `Player ID` == 2029 ~ "Baylor",`Player ID` == 2033 ~ "Tennessee",`Player ID` == 2034 ~ "Kentucky",
      `Player ID` == 2037 ~ "Maryland, Ole Miss",`Player ID` == 2053 ~ "Tennessee",`Player ID` == 2064 ~ "Oregon",
      `Player ID` == 2073 ~ "Maryland, LSU",`Player ID` == 2075 ~ "Iowa",`Player ID` == 2080 ~ "Savannah State University",
      `Player ID` == 2087 ~ "Syracuse, South Carolina",`Player ID` == 2092 ~ "UConn",`Player ID` == 2094 ~ "Mississippi State, Tennessee",
      TRUE ~ Colleges
      )
    )

# Bind back updated na players with no na players and arrange by player id
wnba_player_index <- bind_rows(players_no_na,na_players_updated) %>%
  arrange(`Player ID`)

# Rework Position so that it fits other player index syntax
wnba_player_index <- wnba_player_index %>%
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