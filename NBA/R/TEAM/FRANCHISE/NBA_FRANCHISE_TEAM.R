# Author: David Vialpando-Nielsen
# Date Made: 9/20/2024
# Latest Update: 9/20/2024

# The purpose of file is to create a couple of rda files of franchise per game

# Install and Library Packages

library(tidyverse)
library(rvest)

# Directory of where the rda file will reside in
franchise_fp <- "C:/Users/djvia/OneDrive/Documents/Blog Website/Basketball_Database/NBA/TEAM/FRANCHISE/"

# League Info for distinction of Active and Inactive Franchises
league_info <- read_csv("C:/Users/djvia/OneDrive/Documents/Blog Website/Basketball_Database/NBA/LEAGUE/NBA_LEAGUE_INFO.csv") %>%
  rename(`Team Abbr.` = Team)

# REGULAR SEASON

# Directory and loading regular season totals
team_fp <- "C:/Users/djvia/OneDrive/Documents/Blog Website/Basketball_Database/NBA/TEAM/REGULAR SEASON"
load(file = file.path(team_fp,"NBA_TEAM_REG_TOTAL.rda"))

# Aggregating based on individual team totals
nba_franchise_team_reg_total <- nba_team_reg_total %>%
  group_by(`Franchise ID`,`Team Abbr.`, `Team Name`) %>%
  summarise(
    G = sum(G, na.rm = TRUE),
    MP = round(sum(MP, na.rm = TRUE), 2),
    FG = round(sum(FG, na.rm = TRUE), 2),
    FGA = round(sum(FGA, na.rm = TRUE), 2),
    `FG%` = round(sum(FG, na.rm = TRUE) / sum(FGA, na.rm = TRUE), 3),
    `3P` = round(sum(`3P`, na.rm = TRUE), 2),
    `3PA` = round(sum(`3PA`, na.rm = TRUE), 2),
    `3P%` = round(sum(`3P`, na.rm = TRUE) / sum(`3PA`, na.rm = TRUE), 3),
    `2P` = round(sum(`2P`, na.rm = TRUE), 2),
    `2PA` = round(sum(`2PA`, na.rm = TRUE), 2),
    `2P%` = round(sum(`2P`, na.rm = TRUE) / sum(`2PA`, na.rm = TRUE), 3),
    `eFG%` = round((sum(FG, na.rm = TRUE) + (0.5 * sum(`3P`, na.rm = TRUE))) / sum(FGA, na.rm = TRUE), 3),
    FT = round(sum(FT, na.rm = TRUE), 2),
    FTA = round(sum(FTA, na.rm = TRUE), 2),
    `FT%` = round(sum(FT, na.rm = TRUE) / sum(FTA, na.rm = TRUE), 3),
    ORB = round(sum(ORB, na.rm = TRUE), 2),
    DRB = round(sum(DRB, na.rm = TRUE), 2),
    TRB = round(sum(TRB, na.rm = TRUE), 2),
    AST = round(sum(AST, na.rm = TRUE), 2),
    STL = round(sum(STL, na.rm = TRUE), 2),
    BLK = round(sum(BLK, na.rm = TRUE), 2),
    TOV = round(sum(TOV, na.rm = TRUE), 2),
    PF = round(sum(PF, na.rm = TRUE), 2),
    PTS = round(sum(PTS, na.rm = TRUE), 2),
    `Playoff Appearances` = sum(`Made Playoffs` == "Yes", na.rm = TRUE)
  ) %>%
  left_join(league_info |>
              select(`Franchise ID`, `Team Abbr.`, `To`), by = c("Franchise ID", "Team Abbr.")) %>%
  mutate(Active = ifelse(is.na(To), "Yes", "No")) %>%
  distinct(`Franchise ID`, `Team Name`, .keep_all = TRUE) %>%
  select(-To) %>%
  arrange(`Franchise ID`) %>%
  select(`Franchise ID`, `Team Name`, everything())

# Save franchise data frame to a rda file
save(nba_franchise_team_reg_total,file = file.path(franchise_fp,"NBA_FRANCHISE_TEAM_REG_TOTAL.rda"))

# Aggregating based on individual team per game
nba_franchise_team_reg_per_game <- nba_team_reg_total %>%
  group_by(`Franchise ID`,`Team Abbr.`, `Team Name`) %>%
  summarise(
    G = sum(G, na.rm = TRUE),
    `MP/G` = round(sum(MP, na.rm = TRUE) / sum(G, na.rm = TRUE), 2),
    `FG/G` = round(sum(FG, na.rm = TRUE) / sum(G, na.rm = TRUE), 2),
    `FGA/G` = round(sum(FGA, na.rm = TRUE) / sum(G, na.rm = TRUE), 2),
    `FG%` = round(sum(FG, na.rm = TRUE) / sum(FGA, na.rm = TRUE), 3),
    `3P/G` = round(sum(`3P`, na.rm = TRUE) / sum(G, na.rm = TRUE), 2),
    `3PA/G` = round(sum(`3PA`, na.rm = TRUE) / sum(G, na.rm = TRUE), 2),
    `3P%` = round(sum(`3P`, na.rm = TRUE) / sum(`3PA`, na.rm = TRUE), 3),
    `2P/G` = round(sum(`2P`, na.rm = TRUE) / sum(G, na.rm = TRUE), 2),
    `2PA/G` = round(sum(`2PA`, na.rm = TRUE) / sum(G, na.rm = TRUE), 2),
    `2P%` = round(sum(`2P`, na.rm = TRUE) / sum(`2PA`, na.rm = TRUE), 3),
    `eFG%` = round((sum(FG, na.rm = TRUE) + (0.5 * sum(`3P`, na.rm = TRUE))) / sum(FGA, na.rm = TRUE), 3),
    `FT/G` = round(sum(FT, na.rm = TRUE) / sum(G, na.rm = TRUE), 2),
    `FTA/G` = round(sum(FTA, na.rm = TRUE) / sum(G, na.rm = TRUE), 2),
    `FT%` = round(sum(FT, na.rm = TRUE) / sum(FTA, na.rm = TRUE), 3),
    `ORB/G` = round(sum(ORB, na.rm = TRUE) / sum(G, na.rm = TRUE), 2),
    `DRB/G` = round(sum(DRB, na.rm = TRUE) / sum(G, na.rm = TRUE), 2),
    `TRB/G` = round(sum(TRB, na.rm = TRUE) / sum(G, na.rm = TRUE), 2),
    `AST/G` = round(sum(AST, na.rm = TRUE) / sum(G, na.rm = TRUE), 2),
    `STL/G` = round(sum(STL, na.rm = TRUE) / sum(G, na.rm = TRUE), 2),
    `BLK/G` = round(sum(BLK, na.rm = TRUE) / sum(G, na.rm = TRUE), 2),
    `TOV/G` = round(sum(TOV, na.rm = TRUE) / sum(G, na.rm = TRUE), 2),
    `PF/G` = round(sum(PF, na.rm = TRUE) / sum(G, na.rm = TRUE), 2),
    `PTS/G` = round(sum(PTS, na.rm = TRUE) / sum(G, na.rm = TRUE), 2),
    `Playoff Appearances` = sum(`Made Playoffs` == "Yes", na.rm = TRUE)
  ) %>%
  left_join(league_info |>
              select(`Franchise ID`, `Team Abbr.`, `To`), by = c("Franchise ID", "Team Abbr.")) %>%
  mutate(Active = ifelse(is.na(To), "Yes", "No")) %>%
  distinct(`Franchise ID`, `Team Name`, .keep_all = TRUE) %>%
  select(-To) %>%
  arrange(`Franchise ID`) %>%
  select(`Franchise ID`, `Team Name`, everything())

# Save franchise data frame to a rda file
save(nba_franchise_team_reg_per_game,file = file.path(franchise_fp,"NBA_FRANCHISE_TEAM_REG_PER_GAME.rda"))

# PLAYOFFS

# Directory and loading regular season totals
playoff_fp <- "C:/Users/djvia/OneDrive/Documents/Blog Website/Basketball_Database/NBA/TEAM/PLAYOFFS"
load(file = file.path(playoff_fp,"NBA_TEAM_PLAYOFF_TOTAL.rda"))

# Aggregating based on individual team totals
nba_franchise_team_ply_total <- nba_team_ply_total %>%
  group_by(`Franchise ID`,`Team Abbr.`, `Team Name`) %>%
  summarise(
    G = sum(G, na.rm = TRUE),
    MP = round(sum(MP, na.rm = TRUE), 2),
    FG = round(sum(FG, na.rm = TRUE), 2),
    FGA = round(sum(FGA, na.rm = TRUE), 2),
    `FG%` = round(sum(FG, na.rm = TRUE) / sum(FGA, na.rm = TRUE), 3),
    `3P` = round(sum(`3P`, na.rm = TRUE), 2),
    `3PA` = round(sum(`3PA`, na.rm = TRUE), 2),
    `3P%` = round(sum(`3P`, na.rm = TRUE) / sum(`3PA`, na.rm = TRUE), 3),
    `2P` = round(sum(`2P`, na.rm = TRUE), 2),
    `2PA` = round(sum(`2PA`, na.rm = TRUE), 2),
    `2P%` = round(sum(`2P`, na.rm = TRUE) / sum(`2PA`, na.rm = TRUE), 3),
    `eFG%` = round((sum(FG, na.rm = TRUE) + (0.5 * sum(`3P`, na.rm = TRUE))) / sum(FGA, na.rm = TRUE), 3),
    FT = round(sum(FT, na.rm = TRUE), 2),
    FTA = round(sum(FTA, na.rm = TRUE), 2),
    `FT%` = round(sum(FT, na.rm = TRUE) / sum(FTA, na.rm = TRUE), 3),
    ORB = round(sum(ORB, na.rm = TRUE), 2),
    DRB = round(sum(DRB, na.rm = TRUE), 2),
    TRB = round(sum(TRB, na.rm = TRUE), 2),
    AST = round(sum(AST, na.rm = TRUE), 2),
    STL = round(sum(STL, na.rm = TRUE), 2),
    BLK = round(sum(BLK, na.rm = TRUE), 2),
    TOV = round(sum(TOV, na.rm = TRUE), 2),
    PF = round(sum(PF, na.rm = TRUE), 2),
    PTS = round(sum(PTS, na.rm = TRUE), 2),
    `Won Finals` = sum(`Won Finals` == "Yes", na.rm = TRUE)
  ) %>%
  left_join(league_info |>
              select(`Franchise ID`, `Team Abbr.`, `To`), by = c("Franchise ID", "Team Abbr.")) %>%
  mutate(Active = ifelse(is.na(To), "Yes", "No")) %>%
  distinct(`Franchise ID`, `Team Name`, .keep_all = TRUE) %>%
  select(-To) %>%
  arrange(`Franchise ID`) %>%
  select(`Franchise ID`, `Team Name`, everything())

# Save franchise data frame to a rda file
save(nba_franchise_team_ply_total,file = file.path(franchise_fp,"NBA_FRANCHISE_TEAM_PLY_TOTAL.rda"))

# Aggregating based on individual team per game
nba_franchise_team_ply_per_game <- nba_team_ply_total %>%
  group_by(`Franchise ID`,`Team Abbr.`, `Team Name`) %>%
  summarise(
    G = sum(G, na.rm = TRUE),
    `MP/G` = round(sum(MP, na.rm = TRUE) / sum(G, na.rm = TRUE), 2),
    `FG/G` = round(sum(FG, na.rm = TRUE) / sum(G, na.rm = TRUE), 2),
    `FGA/G` = round(sum(FGA, na.rm = TRUE) / sum(G, na.rm = TRUE), 2),
    `FG%` = round(sum(FG, na.rm = TRUE) / sum(FGA, na.rm = TRUE), 3),
    `3P/G` = round(sum(`3P`, na.rm = TRUE) / sum(G, na.rm = TRUE), 2),
    `3PA/G` = round(sum(`3PA`, na.rm = TRUE) / sum(G, na.rm = TRUE), 2),
    `3P%` = round(sum(`3P`, na.rm = TRUE) / sum(`3PA`, na.rm = TRUE), 3),
    `2P/G` = round(sum(`2P`, na.rm = TRUE) / sum(G, na.rm = TRUE), 2),
    `2PA/G` = round(sum(`2PA`, na.rm = TRUE) / sum(G, na.rm = TRUE), 2),
    `2P%` = round(sum(`2P`, na.rm = TRUE) / sum(`2PA`, na.rm = TRUE), 3),
    `eFG%` = round((sum(FG, na.rm = TRUE) + (0.5 * sum(`3P`, na.rm = TRUE))) / sum(FGA, na.rm = TRUE), 3),
    `FT/G` = round(sum(FT, na.rm = TRUE) / sum(G, na.rm = TRUE), 2),
    `FTA/G` = round(sum(FTA, na.rm = TRUE) / sum(G, na.rm = TRUE), 2),
    `FT%` = round(sum(FT, na.rm = TRUE) / sum(FTA, na.rm = TRUE), 3),
    `ORB/G` = round(sum(ORB, na.rm = TRUE) / sum(G, na.rm = TRUE), 2),
    `DRB/G` = round(sum(DRB, na.rm = TRUE) / sum(G, na.rm = TRUE), 2),
    `TRB/G` = round(sum(TRB, na.rm = TRUE) / sum(G, na.rm = TRUE), 2),
    `AST/G` = round(sum(AST, na.rm = TRUE) / sum(G, na.rm = TRUE), 2),
    `STL/G` = round(sum(STL, na.rm = TRUE) / sum(G, na.rm = TRUE), 2),
    `BLK/G` = round(sum(BLK, na.rm = TRUE) / sum(G, na.rm = TRUE), 2),
    `TOV/G` = round(sum(TOV, na.rm = TRUE) / sum(G, na.rm = TRUE), 2),
    `PF/G` = round(sum(PF, na.rm = TRUE) / sum(G, na.rm = TRUE), 2),
    `PTS/G` = round(sum(PTS, na.rm = TRUE) / sum(G, na.rm = TRUE), 2),
    `Won Finals` = sum(`Won Finals` == "Yes", na.rm = TRUE)
  )%>%
  left_join(league_info |>
              select(`Franchise ID`, `Team Abbr.`, `To`), by = c("Franchise ID", "Team Abbr.")) %>%
  mutate(Active = ifelse(is.na(To), "Yes", "No")) %>%
  distinct(`Franchise ID`, `Team Name`, .keep_all = TRUE) %>%
  select(-To) %>%
  arrange(`Franchise ID`) %>%
  select(`Franchise ID`, `Team Name`, everything())

# Save franchise data frame to a rda file
save(nba_franchise_team_ply_per_game,file = file.path(franchise_fp,"NBA_FRANCHISE_TEAM_PLY_PER_GAME.rda"))

# Full join the two dataframes on Player ID and Player
nba_franchise_all_time_total <- full_join(nba_team_reg_total, nba_team_ply_total, 
                                          by = c("Franchise ID", "Team Name","Team Abbr.","Season"), suffix = c("_reg", "_playoff"))

# Replace NA values with 0 in numeric columns
nba_franchise_all_time_total <- nba_franchise_all_time_total %>%
  mutate(across(where(is.numeric), ~ replace_na(., 0)))

# Combine regular season and playoff statistics total
nba_franchise_team_all_time_total <- nba_franchise_all_time_total %>%
  group_by(`Franchise ID`,`Team Abbr.`, `Team Name`) %>%
  summarise(
    G = sum(G_reg + G_playoff, na.rm = TRUE),
    MP = sum(MP_reg + MP_playoff, na.rm = TRUE),
    FG = sum(FG_reg + FG_playoff, na.rm = TRUE),
    FGA = sum(FGA_reg + FGA_playoff, na.rm = TRUE),
    `FG%` = round(sum(FG_reg + FG_playoff, na.rm = TRUE) / sum(FGA_reg + FGA_playoff, na.rm = TRUE), 3),
    `3P` = sum(`3P_reg` + `3P_playoff`, na.rm = TRUE),
    `3PA` = sum(`3PA_reg` + `3PA_playoff`, na.rm = TRUE),
    `3P%` = round(sum(`3P_reg` + `3P_playoff`, na.rm = TRUE) / sum(`3PA_reg` + `3PA_playoff`, na.rm = TRUE), 3),
    `2P` = sum(`2P_reg` + `2P_playoff`, na.rm = TRUE),
    `2PA` = sum(`2PA_reg` + `2PA_playoff`, na.rm = TRUE),
    `2P%` = round(sum(`2P_reg` + `2P_playoff`, na.rm = TRUE) / sum(`2PA_reg` + `2PA_playoff`, na.rm = TRUE), 3),
    `eFG%` = round((sum(FG_reg + FG_playoff, na.rm = TRUE) + 0.5 * sum(`3P_reg` + `3P_playoff`, na.rm = TRUE)) / sum(FGA_reg + FGA_playoff, na.rm = TRUE), 3),
    FT = sum(FT_reg + FT_playoff, na.rm = TRUE),
    FTA = sum(FTA_reg + FTA_playoff, na.rm = TRUE),
    `FT%` = round(sum(FT_reg + FT_playoff, na.rm = TRUE) / sum(FTA_reg + FTA_playoff, na.rm = TRUE), 3),
    ORB = sum(ORB_reg + ORB_playoff, na.rm = TRUE),
    DRB = sum(DRB_reg + DRB_playoff, na.rm = TRUE),
    TRB = sum(TRB_reg + TRB_playoff, na.rm = TRUE),
    AST = sum(AST_reg + AST_playoff, na.rm = TRUE),
    STL = sum(STL_reg + STL_playoff, na.rm = TRUE),
    BLK = sum(BLK_reg + BLK_playoff, na.rm = TRUE),
    TOV = sum(TOV_reg + TOV_playoff, na.rm = TRUE),
    PF = sum(PF_reg + PF_playoff, na.rm = TRUE),
    PTS = sum(PTS_reg + PTS_playoff, na.rm = TRUE),
    `Playoff Appearances` = sum(`Made Playoffs` == "Yes", na.rm = TRUE),
    `Won Finals` = sum(`Won Finals` == "Yes", na.rm = TRUE),
    `Finals-to-Appearances` = ifelse(`Playoff Appearances` == 0, 0, round(`Won Finals` / `Playoff Appearances`, 3))
  ) %>%
  left_join(league_info |>
              select(`Franchise ID`, `Team Abbr.`, `To`), by = c("Franchise ID", "Team Abbr.")) %>%
  mutate(Active = ifelse(is.na(To), "Yes", "No")) %>%
  distinct(`Franchise ID`, `Team Name`, .keep_all = TRUE) %>%
  select(-To) %>%
  arrange(`Franchise ID`) %>%
  select(`Franchise ID`, `Team Name`, everything())

# Save franchise data frame to a rda file
save(nba_franchise_team_all_time_total,file = file.path(franchise_fp,"NBA_FRANCHISE_TEAM_ALL_TIME_TOTAL.rda"))

# Combine regular season and playoff statistics per game
nba_franchise_team_all_time_per_game <- nba_franchise_all_time_total %>%
  group_by(`Franchise ID`,`Team Abbr.`, `Team Name`) %>%
  summarise(
    G = sum(G_reg + G_playoff, na.rm = TRUE),
    `MP/G` = round(sum(MP_reg + MP_playoff, na.rm = TRUE) / sum(G, na.rm = TRUE), 2),
    `FG/G` = round(sum(FG_reg + FG_playoff, na.rm = TRUE) / sum(G, na.rm = TRUE), 2),
    `FGA/G` = round(sum(FGA_reg + FGA_playoff, na.rm = TRUE) / sum(G, na.rm = TRUE), 2),
    `FG%` = round(sum(FG_reg + FG_playoff, na.rm = TRUE) / sum(FGA_reg + FGA_playoff, na.rm = TRUE), 3),
    `3P/G` = round(sum(`3P_reg` + `3P_playoff`, na.rm = TRUE) / sum(G, na.rm = TRUE), 2),
    `3PA/G` = round(sum(`3PA_reg` + `3PA_playoff`, na.rm = TRUE) / sum(G, na.rm = TRUE), 2),
    `3P%` = round(sum(`3P_reg` + `3P_playoff`, na.rm = TRUE) / sum(`3PA_reg` + `3PA_playoff`, na.rm = TRUE), 3),
    `2P/G` = round(sum(`2P_reg` + `2P_playoff`, na.rm = TRUE) / sum(G, na.rm = TRUE), 2),
    `2PA/G` = round(sum(`2PA_reg` + `2PA_playoff`, na.rm = TRUE) / sum(G, na.rm = TRUE), 2),
    `2P%` = round(sum(`2P_reg` + `2P_playoff`, na.rm = TRUE) / sum(`2PA_reg` + `2PA_playoff`, na.rm = TRUE), 3),
    `eFG%` = round((sum(FG_reg + FG_playoff, na.rm = TRUE) + 0.5 * sum(`3P_reg` + `3P_playoff`, na.rm = TRUE)) / sum(FGA_reg + FGA_playoff, na.rm = TRUE), 3),
    `FT/G` = round(sum(FT_reg + FT_playoff, na.rm = TRUE) / sum(G, na.rm = TRUE), 2),
    `FTA/G` = round(sum(FTA_reg + FTA_playoff, na.rm = TRUE) / sum(G, na.rm = TRUE), 2),
    `FT%` = round(sum(FT_reg + FT_playoff, na.rm = TRUE) / sum(FTA_reg + FTA_playoff, na.rm = TRUE), 3),
    `ORB/G` = round(sum(ORB_reg + ORB_playoff, na.rm = TRUE) / sum(G, na.rm = TRUE), 2),
    `DRB/G` = round(sum(DRB_reg + DRB_playoff, na.rm = TRUE) / sum(G, na.rm = TRUE), 2),
    `TRB/G` = round(sum(TRB_reg + TRB_playoff, na.rm = TRUE) / sum(G, na.rm = TRUE), 2),
    `AST/G` = round(sum(AST_reg + AST_playoff, na.rm = TRUE) / sum(G, na.rm = TRUE), 2),
    `STL/G` = round(sum(STL_reg + STL_playoff, na.rm = TRUE) / sum(G, na.rm = TRUE), 2),
    `BLK/G` = round(sum(BLK_reg + BLK_playoff, na.rm = TRUE) / sum(G, na.rm = TRUE), 2),
    `TOV/G` = round(sum(TOV_reg + TOV_playoff, na.rm = TRUE) / sum(G, na.rm = TRUE), 2),
    `PF/G` = round(sum(PF_reg + PF_playoff, na.rm = TRUE) / sum(G, na.rm = TRUE), 2),
    `PTS/G` = round(sum(PTS_reg + PTS_playoff, na.rm = TRUE) / sum(G, na.rm = TRUE), 2),
    `Playoff Appearances` = sum(`Made Playoffs` == "Yes", na.rm = TRUE),
    `Won Finals` = sum(`Won Finals` == "Yes", na.rm = TRUE),
    `Finals-to-Appearances` = ifelse(`Playoff Appearances` == 0, 0, round(`Won Finals` / `Playoff Appearances`, 3))
  ) %>%
  left_join(league_info |>
              select(`Franchise ID`, `Team Abbr.`, `To`), by = c("Franchise ID", "Team Abbr.")) %>%
  mutate(Active = ifelse(is.na(To), "Yes", "No")) %>%
  distinct(`Franchise ID`, `Team Name`, .keep_all = TRUE) %>%
  select(-To) %>%
  arrange(`Franchise ID`) %>%
  select(`Franchise ID`, `Team Name`, everything())

# Save franchise data frame to a rda file
save(nba_franchise_team_all_time_per_game,file = file.path(franchise_fp,"NBA_FRANCHISE_TEAM_ALL_TIME_PER_GAME.rda"))