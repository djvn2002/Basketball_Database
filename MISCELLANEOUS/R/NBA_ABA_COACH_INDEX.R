# Author: David Vialpando-Nielsen
# Date Made: 9/8/2024
# Latest Update: 9/8/2024

# The usage of this file is to pinpoint coaches in the NBA and ABA with unqiue indexes

# Load necessary libraries
library(rvest)
library(tidyverse)
library(hoopR)

# URL for the coaches page
url <- "https://www.basketball-reference.com/coaches/"

# Function to scrape the coach table, replace column names by the first row, and filter out unwanted rows
scrape_coach_table <- function(url) {
  # Read the webpage
  webpage <- read_html(url)
  
  # Extract the table
  coaches_table <- webpage %>%
    html_nodes("#div_coaches") %>%
    html_table(fill = TRUE)
  
  # The table is in list format, we extract the first item
  coaches_df <- coaches_table[[1]]
  
  # Replace column names with the first row
  colnames(coaches_df) <- as.character(coaches_df[1, ])
  
  # Remove the first row from the data frame
  coaches_df <- coaches_df[-1, ]
  
  # Filter out rows where 'Coach' contains only a single uppercase letter
  coaches_df <- coaches_df %>%
    filter(!str_detect(Coach, "^[A-Z]$"))
  
  return(coaches_df)
}

# Scrape the coach table
coaches_df <- scrape_coach_table(url) %>%
  filter(Coach != "Coach")

# Fill in NA for empty rows in Birth Date and College
coaches_index <- coaches_df %>%
  mutate(`Birth Date` = ifelse(`Birth Date` == "", NA, `Birth Date`),
         College = ifelse(College == "", NA, College))

# New columns 'HOF' & 'Active' and drop '*' in Coach column
coaches_index <- coaches_index %>%
  mutate(HOF = ifelse(str_detect(Coach, "\\*"), "Yes", "No"),
         Active = ifelse(To == most_recent_nba_season() + 1, "Yes", "No"),
         Coach = str_replace(Coach, "\\*", ""))

# Rearrange rows and add 'Coach ID'
coaches_index <- coaches_index %>%
  arrange(`From`, Coach) %>%
  mutate(`Coach ID` = row_number() + 100) %>%
  select(`Coach ID`, everything())

# Print to csv for later use
write_csv(coaches_index,"C:/Users/djvia/OneDrive/Documents/Blog Website/Basketball_Database/MISCELLANEOUS/NBA_ABA_COACH_INDEX.csv")