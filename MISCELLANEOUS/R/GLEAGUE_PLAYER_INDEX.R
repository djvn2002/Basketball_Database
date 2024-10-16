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
rs_driver_object <- rsDriver(browser = 'chrome', extraCapabilities = eCaps, verbose = FALSE, port = free_port(), chromever = "128.0.6613.119")
remDr <- rs_driver_object$client

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
  
  player_data <- player_data %>%
    filter(!grepl("G-League|Sports Reference|Copyright", Player_Name)) %>%
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

# Function to Scrape Birth Date for Each Player (excluding any text after 'in')
scrape_birth_date <- function(player_link, remDr) {
  retry({
    remDr$navigate(player_link)
    Sys.sleep(2)  # Allow page to load
    
    birth_element <- remDr$findElement(using = "xpath", "//div[@id='meta']//p[strong[text()='Born:']]/span")
    birth_date <- birth_element$getElementText()[[1]]
    
    # Check if the birth_date contains the word 'in' followed by a location
    if (grepl("in ", birth_date, ignore.case = TRUE)) {
      # Return NA if the birth_date contains location information
      return(NA_character_)
    } else {
      # Return the valid birth date (no 'in' keyword)
      return(birth_date)
    }
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

# Directory to save partial CSV files
save_directory <- "C:/Users/djvia/OneDrive/Documents/Blog Website/Basketball_Database/MISCELLANEOUS"
partial_csv_file <- file.path(save_directory, "GLEAGUE_partial.csv")

# Read the GLEAGUE partial CSV into a single dataframe
if (file.exists(partial_csv_file)) {
  all_player_data_combined <- read_csv(partial_csv_file)
} else {
  all_player_data_combined <- data.frame()  # Create empty dataframe if no partial exists
}

# Function to write or append data to the partial CSV
write_or_append_partial_csv <- function(player_data, file_path) {
  if (file.exists(file_path)) {
    # If the file exists, read the existing data and append the new data
    existing_data <- read_csv(file_path)
    combined_data <- bind_rows(existing_data, player_data)
  } else {
    # If the file doesn't exist (first letter), create the new data
    combined_data <- player_data
  }
  
  # Write the combined data back to the CSV file
  write_csv(combined_data, file_path)
}

# Function to get completed letters based on GLEAGUE_partial.csv
get_completed_letters_from_partial <- function(file_path) {
  if (file.exists(file_path)) {
    player_data <- read_csv(file_path)
    
    # Use regular expression to extract the first letter after /gleague/players/
    completed_letters <- unique(tolower(str_extract(player_data$Player_Link, "(?<=/gleague/players/)[a-z]")))
    
    return(completed_letters)
  } else {
    # If file doesn't exist, return empty set to start from 'a'
    return(character(0))
  }
}

# Determine completed letters
completed_letters <- get_completed_letters_from_partial(partial_csv_file)

# Exclude 'x' and already completed letters from the letters to scrape
letters_to_scrape <- setdiff(letters, c('x', completed_letters))

if (length(completed_letters) > 0) {
  message(paste("Resuming scrape from letter:", letters_to_scrape[1]))
} else {
  message("Starting scrape from the beginning ('a').")
}

# Function to convert Weight to numeric safely
convert_weight_to_numeric <- function(df) {
  df %>%
    mutate(Weight = as.numeric(Weight))  # Convert Weight to numeric
}

# Continue scraping from the next unprocessed letter
for (letter in letters_to_scrape) {
  message(paste("Starting to scrape players from letter:", letter))
  
  # Scrape players for the current letter
  player_data <- with_progress({
    scrape_players(letter, remDr)
  })
  
  # Remove rows with empty Player_Name or invalid Player_Link
  player_data <- player_data %>%
    filter(Player_Name != "" & !grepl("email|javascript:", Player_Link))
  
  # Scrape additional information for each player in the current letter
  player_data <- player_data %>%
    mutate(
      Birth_Date = map_chr(Player_Link, ~ scrape_birth_date(.x, remDr), .default = NA_character_),
      Position = map_chr(Player_Link, ~ scrape_position(.x, remDr)),
      Height = map_chr(Player_Link, ~ scrape_height(.x, remDr)),
      Weight = map_chr(Player_Link, ~ scrape_weight(.x, remDr)),
      NBA = map_chr(Player_Link, ~ scrape_nba_status(.x, remDr))  # Scrape NBA Status
    ) %>%
    mutate(across(c(Birth_Date, Position, Height, Weight, NBA), ~ ifelse(. == "", NA, .))) %>%
    convert_weight_to_numeric()  # Ensure Weight is numeric
  
  # Read the existing GLEAGUE_partial.csv and ensure consistency in types
  if (file.exists(partial_csv_file)) {
    existing_data <- read_csv(partial_csv_file)
    existing_data <- convert_weight_to_numeric(existing_data)  # Ensure Weight is numeric
  } else {
    existing_data <- data.frame()
  }
  
  # Combine current player data with existing data
  combined_data <- bind_rows(existing_data, player_data)
  
  # Write the combined data back to the partial CSV file
  write_csv(combined_data, partial_csv_file)
  
  message(paste("Completed scraping players from letter:", letter))
}

# Close RSelenium Session
remDr$close()
rs_driver_object$server$stop()

# Read the GLEAGUE_partial.csv after closing the session
all_player_data_combined <- read_csv(partial_csv_file)

# Create a new column Upper_Name that uppercases the Player_Name and another column Gleague Display Name
all_player_data_combined <- all_player_data_combined %>%
  mutate(Upper_Name = toupper(Player_Name),
         `Gleague Display Name` = Player_Name)

# Define a regular expression pattern to match common suffixes (Jr., Sr., II, III, IV, etc.)
suffixes_pattern <- "\\b(Jr\\.|Sr\\.|II|III|IV|V|VI|VII|VIII|IX|X)\\b"

# Filter for players with suffixes in their names
players_with_suffixes <- all_player_data_combined %>%
  filter(grepl(suffixes_pattern, Player_Name, ignore.case = TRUE))

# Updating certain player names to match names in the NBA
all_player_data_combined <- all_player_data_combined %>%
  mutate(Player_Name = ifelse(Player_Name == "James Blackmon Jr.", "James Blackmon", Player_Name),
         Player_Name = ifelse(Player_Name == "Anthony Lawrence II", "Anthony Lawrence", Player_Name),
         Player_Name = ifelse(Player_Name == "Barry Brown Jr.", "Barry Brown", Player_Name),
         Player_Name = ifelse(Player_Name == "Derek Cooke Jr.", "Derek Cooke", Player_Name),
         Player_Name = ifelse(Player_Name == "Gregory Jackson II", "GG Jackson II", Player_Name),
         Player_Name = ifelse(Player_Name == "James Palmer Jr.", "James Palmer", Player_Name),
         Player_Name = ifelse(Player_Name == "Johnny O'Bryant III", "Johnny O'Bryant", Player_Name),
         Player_Name = ifelse(Player_Name == "Kenyon Martin Jr.", "KJ Martin", Player_Name),
         Player_Name = ifelse(Player_Name == "Melvin Frazier Jr.", "Melvin Frazier", Player_Name),
         Player_Name = ifelse(Player_Name == "Terrell Brown Jr.", "Terrell Brown", Player_Name),
         Player_Name = ifelse(Player_Name == "Zach Norvell Jr.", "Zach Norvell", Player_Name),
         Player_Name = ifelse(Player_Name == "Brian Bowen II", "Brian Bowen", Player_Name),
         Player_Name = ifelse(Player_Name == "James Palmer Jr.", "James Palmer", Player_Name),
         Player_Name = ifelse(Player_Name == "Travis Trice III", "Travis Trice", Player_Name),
         Player_Name = ifelse(Player_Name == "Derrick Walton Jr.", "Derrick Walton", Player_Name))

# Identify duplicates based on Upper_Name
duplicates <- all_player_data_combined %>%
  filter(Upper_Name != "CHRIS ANDERSON",
         Upper_Name != "MIKE ANDERSON",
         Upper_Name != "RYAN ANDERSON",
         Upper_Name != "JOHN BRYANT",
         Upper_Name != "DEONTE BURTON",
         Upper_Name != "A.J. DAVIS",
         Upper_Name != "JOSH DAVIS",
         Upper_Name != "KYLE DAVIS",
         Upper_Name != "MIKE DAVIS",
         Upper_Name != "JUSTIN HAMILTON",
         Upper_Name != "MARCUS HILL",
         Upper_Name != "JUSTIN JACKSON",
         Upper_Name != "ANTHONY JOHNSON",
         Upper_Name != "CHRIS JOHNSON",
         Upper_Name != "JUSTIN JOHNSON",
         Upper_Name != "MALCOLM MILLER",
         Upper_Name != "TONY MITCHELL",
         Upper_Name != "AUSTIN NICHOLS",
         Upper_Name != "DAVID PALMER",
         Upper_Name != "ANTHONY ROBERSON",
         Upper_Name != "COREY SANDERS",
         Upper_Name != "MIKE SCOTT",
         Upper_Name != "CHRIS SMITH",
         Upper_Name != "JAMAR SMITH",
         Upper_Name != "TYLER SMITH",
         Upper_Name != "MARCUS THORNTON",
         Upper_Name != "BRANDON WILLIAMS",
         Upper_Name != "MARCUS WILLIAMS",
         Upper_Name != "CHRIS WRIGHT") %>%
  group_by(Upper_Name) %>%
  filter(n() > 1)

# Merge duplicate rows and clean the data by taking the relevant information, keeping Gleague Display Name and handling NBA column
cleaned_duplicates <- duplicates %>%
  group_by(Upper_Name) %>%
  summarise(
    Player_Name = first(Player_Name),  # Take the first occurrence of Player_Name
    `Gleague Display Name` = first(`Gleague Display Name`),  # Keep Gleague Display Name
    Player_Link = first(Player_Link),  # Take the first Player_Link
    From = min(From, na.rm = TRUE),  # Take the earliest 'From' year
    To = max(To, na.rm = TRUE),  # Take the latest 'To' year
    Birth_Date = coalesce(first(Birth_Date[!is.na(Birth_Date)]), NA_character_),  # Take the first non-NA Birth Date
    Position = coalesce(first(Position[!is.na(Position)]), NA_character_),  # Take the first non-NA Position
    Height = coalesce(first(Height[!is.na(Height)]), NA_character_),  # Take the first non-NA Height
    Weight = coalesce(first(Weight[!is.na(Weight)]), NA_real_),  # Use NA_real_ for numeric columns like Weight
    NBA = if_else(any(NBA == "yes"), "yes", "no"),  # Keep "yes" if any row has "yes", otherwise "no"
    Upper_Name = first(Upper_Name)  # Keep Upper_Name for consistency
  )

# Remove original duplicates from all_player_data_combined
non_duplicates <- anti_join(all_player_data_combined, duplicates, by = c("Upper_Name"))

# Combine non-duplicate data with cleaned duplicate data, ensuring Gleague Display Name stays
player_data <- bind_rows(non_duplicates, cleaned_duplicates) %>%
  rename(Player = Player_Name, `Birth Date` = Birth_Date, Pos = Position,
         Ht = Height, Wt = Weight) %>%
  select(-Player_Link)

# Separate players who played and not played in the nba
# This data frame will serve to update player names and birth dates
gleague_nba <- player_data %>%
  filter(NBA == "yes") %>%
  mutate(Player = ifelse(Player == "Nicolas Claxton", "Nic Claxton", Player),
         Player = ifelse(Player == "Jose Barea", "J.J. Barea", Player),
         Player = ifelse(Player == "Louis Amundson", "Lou Amundson", Player),
         Player = ifelse(Player == "DJ Carton", "D.J. Carton", Player),
         Player = ifelse(Player == "Matt Coleman", "Matt Coleman III", Player),
         Player = ifelse(Player == "Mitchell Creek", "Mitch Creek", Player),
         Player = ifelse(Player == "Larry Drew", "Larry Drew II", Player),
         Player = ifelse(Player == "Vincent Edwards", "Vince Edwards", Player),
         Player = ifelse(Player == "James Ennis", "James Ennis III", Player),
         Player = ifelse(Player == "Vitor Faverani", "Vitor Luiz Faverani", Player),
         Player = ifelse(Player == "LJ Figueroa", "L.J. Figueroa", Player),
         Player = ifelse(Player == "AJ Green", "A.J. Green", Player),
         Player = ifelse(Player == "PJ Hairston", "P.J. Hairston", Player),
         Player = ifelse(Player == "Elijah Harkless", "EJ Harkless", Player),
         Player = ifelse(Player == "Max Heidegger", "Maxwell Heidegger", Player),
         Player = ifelse(Player == "Juan Hernangomez", "Juancho Hernangomez", Player),
         Player = ifelse(Player == "D.J. Hogg", "DJ Hogg", Player),
         Player = ifelse(Player == "Danuel House", "Danuel House Jr.", Player),
         Player = ifelse(Player == "Vincent Hunter", "Vince Hunter", Player),
         Player = ifelse(Player == "Pooh Jeter", "Eugene Jeter", Player),
         Player = ifelse(Player == "BJ Johnson", "B.J. Johnson", Player),
         Player = ifelse(Player == "Nikola Jović", "Nikola Jovic", Player),
         Player = ifelse(Player == "Walter Lemon Jr.", "Walt Lemon Jr.", Player),
         Player = ifelse(Player == "Timothe Luwawu", "Timothe Luwawu-Cabarrot", Player),
         Player = ifelse(Player == "John Lucas", "John Lucas III", Player),
         Player = ifelse(Player == "Joirdon Nicholas", "Joirdon Karl Nicholas", Player),
         Player = ifelse(Player == "CJ Massinburg", "C.J. Massinburg", Player),
         Player = ifelse(Player == "Karlo Matković", "Karlo Matkovic", Player),
         Player = ifelse(Player == "C.J. McCollum", "CJ McCollum", Player),
         Player = ifelse(Player == "KJ McDaniels", "K.J. McDaniels", Player),
         Player = ifelse(Player == "Patrick Mills", "Patty Mills", Player),
         Player = ifelse(Player == "Sviatoslav Mykhailiuk", "Svi Mykhailiuk", Player),
         Player = ifelse(Player == "Daniel Nwaelele", "Dan Nwaelele", Player),
         Player = ifelse(Player == "EJ Onu", "E.J. Onu", Player),
         Player = ifelse(Player == "Eli Pemberton", "Elijah Pemberton", Player),
         Player = ifelse(Player == "JR Pinnock", "J.R. Pinnock", Player),
         Player = ifelse(Player == "Glen Rice", "Glen Rice Jr.", Player),
         Player = ifelse(Player == "Dennis Schröder", "Dennis Schroder", Player),
         Player = ifelse(Player == "EJ Singler", "E.J. Singler", Player),
         Player = ifelse(Player == "Ja’Vonte Smart", "Javonte Smart", Player),
         Player = ifelse(Player == "Ishmael Smith", "Ish Smith", Player),
         Player = ifelse(Player == "Walter Tavares", "Edy Tavares", Player),
         Player = ifelse(Player == "Jeffrey Taylor", "Jeff Taylor", Player),
         Player = ifelse(Player == "Lonnie Walker", "Lonnie Walker IV", Player),
         Player = ifelse(Player == "Louis Williams", "Lou Williams", Player),
         Player = ifelse(Player == "Mike Young", "Michael Young", Player),
         `Birth Date` = ifelse(Player == "Vander Blue", "July 17, 1992", `Birth Date`),
         `Birth Date` = ifelse(Player == "Kevin Burleson", "September 4, 1979", `Birth Date`),
         `Birth Date` = ifelse(Player == "Isaiah Canaan", "May 21, 1991", `Birth Date`),
         `Birth Date` = ifelse(Player == "Rodney Carney", "April 15, 1984", `Birth Date`),
         `Birth Date` = ifelse(Player == "Jack Cooley", "April 12, 1991", `Birth Date`),
         `Birth Date` = ifelse(Player == "Allen Crabbe", "April 9, 1992", `Birth Date`),
         `Birth Date` = ifelse(Player == "Eric Dawson", "July 7, 1984", `Birth Date`),
         `Birth Date` = ifelse(Player == "Vince Edwards", "April 5, 1996", `Birth Date`),
         `Birth Date` = ifelse(Player == "Joe Harris", "September 6, 1991", `Birth Date`),
         `Birth Date` = ifelse(Player == "Keldon Johnson", "October 11, 1999", `Birth Date`),
         `Birth Date` = ifelse(Player == "Shelvin Mack", "April 22, 1990", `Birth Date`),
         `Birth Date` = ifelse(Player == "Trey McKinney-Jones", "August 27, 1990", `Birth Date`),
         `Birth Date` = ifelse(Player == "C.J. Miles", "March 18, 1987", `Birth Date`),
         `Birth Date` = ifelse(Player == "Matt Mooney", "February 7, 1996", `Birth Date`),
         `Birth Date` = ifelse(Player == "Elfrid Payton", "February 22, 1994", `Birth Date`),
         `Birth Date` = ifelse(Player == "Jeremy Richardson", "March 1, 1984", `Birth Date`),
         `Birth Date` = ifelse(Player == "Gabe Vincent", "June 14, 1996", `Birth Date`),
         `Birth Date` = ifelse(Player == "Greg Whittington", "February 7, 1993", `Birth Date`),
         NBA = ifelse(Player == "Sheldon Mac", "yes", NBA),
         From = ifelse(Player == "Sheldon Mac", 2017, From),
         Pos = ifelse(Player == "Sheldon Mac", "G", From))


# IMPORTANT!! This portion of code is specifically made for separating the player data
# By separating them we can identify NBA IDs to players and then make GLEAGUE IDs

# Normalize names by removing special characters (diacritics)
normalize_name <- function(name) {
  stri_trans_general(name, "Latin-ASCII")
}

# Load in nba player index
nba_index <- read_csv("C:/Users/djvia/OneDrive/Documents/Blog Website/Basketball_Database/MISCELLANEOUS/NBA_ABA_PLAYER_INDEX.csv") %>%
  mutate(Player = normalize_name(Player))

# Players with no birth dates
player_data_no_bday <- gleague_nba %>%
  filter(is.na(`Birth Date`))

# Assign IDs
player_data_no_bday <- player_data_no_bday %>%
  left_join(nba_index %>% select(Player,`Player ID`), 
            by = c("Player"),
            relationship = "many-to-many") %>%
  rename(`NBA ID` = `Player ID`)

# Duplicates
no_bday_dupes <- player_data_no_bday %>%
  group_by(Player) %>%
  filter(n() > 1) %>%
  filter(`NBA ID` != 6043 & `NBA ID` != 5207)

# Perform anti join to remove the duplicates from player_data_no_bday
player_data_no_bday <- anti_join(player_data_no_bday, no_bday_dupes, by = c("Player", "NBA ID")) %>%
                                   mutate(`NBA ID` = ifelse(Player == "Kenny Williams" &
                                                              `NBA ID` == 3626, NA, `NBA ID`),
                                          `NBA ID` = ifelse(Player == "Chris Smith" &
                                                              `NBA ID` == 5207, NA, `NBA ID`),
                                          `NBA ID` = ifelse(Player == "Justin Hamilton" &
                                                              `NBA ID` == 5172, NA, `NBA ID`))

# Players with birth date
player_data_with_bday <- gleague_nba %>%
  filter(!is.na(`Birth Date`))

# Assign IDs
player_data_with_bday <- player_data_with_bday %>%
  left_join(nba_index %>% select(Player,`Player ID`, `Birth Date`), 
            by = c("Player", "Birth Date"),
            relationship = "many-to-many") %>%
  rename(`NBA ID` = `Player ID`)

# Duplicates
bday_dupes <- player_data_with_bday %>%
  group_by(`NBA ID`) %>%
  filter(n() > 1 & !is.na(`NBA ID`))

# Bind data frames together to make the gleague player index
gleague_player_index <- player_data %>%
  filter(NBA == "no") %>% 
  bind_rows(player_data_no_bday,player_data_with_bday) %>%
  arrange(From, Player) %>%
  mutate(`GLEAGUE ID` = 1000 + row_number()) # MAKE SURE this dataframe matches the # of obs in player_data!

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

# Perform a left join on NBA ID
merged_df <- merge(gleague_player_index, nba_index, by.x = "NBA ID", by.y = "Player ID", all.x = TRUE, suffixes = c("_gleague", "_nba"))

# Use mutate and coalesce to only replace NA values in G-League with NBA data
updated_gleague_player_index <- merged_df %>%
  mutate(
    Pos_gleague = coalesce(Pos_gleague, Pos_nba),
    Ht_gleague = coalesce(Ht_gleague, Ht_nba),
    Wt_gleague = coalesce(Wt_gleague, Wt_nba),
    Birth_Date_gleague = coalesce(`Birth Date_gleague`, `Birth Date_nba`)
  )

# After merging with NBA data, ensure Gleague Display Name is preserved
gleague_player_index <- updated_gleague_player_index %>%
  select(`GLEAGUE ID`, `NBA ID`, Player_gleague, `Gleague Display Name`, From_gleague, To_gleague, Pos_gleague, Ht_gleague, Wt_gleague, Birth_Date_gleague) %>%
  rename(
    `GLEAGUE ID` = `GLEAGUE ID`,
    `NBA ID` = `NBA ID`,
    Player = Player_gleague,
    `Gleague Display Name` = `Gleague Display Name`,
    From = From_gleague,
    To = To_gleague,
    Pos = Pos_gleague,
    Ht = Ht_gleague,
    Wt = Wt_gleague,
    `Birth Date` = Birth_Date_gleague
  ) %>%
  arrange(`GLEAGUE ID`) %>%
  mutate(Active = if_else(To == 2024, "Yes", "No"),
         Player = `Gleague Display Name`) %>%
  select(-`Gleague Display Name`)

# Last check of duplicates
gleague_dupes <- gleague_player_index %>%
  group_by(`NBA ID`) %>%
  filter(n() > 1 & !is.na(`NBA ID`))

# Write to a csv for G League Player Index
write_csv(gleague_player_index,"C:/Users/djvia/OneDrive/Documents/Blog Website/Basketball_Database/MISCELLANEOUS/GLEAGUE_PLAYER_INDEX.csv")