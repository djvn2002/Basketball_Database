# Author: David Vialpando-Nielsen
# Date Made: 9/7/2024
# Latest Update: 9/7/2024

# The purpose of file is to create a couple of rda files of player career totals

# Install and Library Packages

library(tidyverse)
library(rvest)

# Directory of where the rda file will reside in
career_fp <- "C:/Users/djvia/OneDrive/Documents/Blog Website/Basketball_Database/NBA/PLAYER/CAREER/"

# REGULAR SEASON

# Directory and loading regular season totals
player_fp <- "C:/Users/djvia/OneDrive/Documents/Blog Website/Basketball_Database/NBA/PLAYER/REGULAR SEASON"
load(file = file.path(player_fp,"NBA_PLAYER_REG_TOTAL.rda"))

# Aggregating based by player
nba_reg_career_total <- nba_reg_total %>%
  group_by(`Player ID`, Player) %>%
  summarise( G = sum(G, na.rm = TRUE),
             GS = sum(GS, na.rm = TRUE),
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
             PTS = round(sum(PTS, na.rm = TRUE), 2)) %>%
arrange(`Player ID`)

# Save career data frame to a rda file
save(nba_reg_career_total,file = file.path(career_fp,"NBA_PLAYER_CAREER_REG_TOTAL.rda"))

# PLAYOFFS

# Directory and loading regular season totals
playoff_fp <- "C:/Users/djvia/OneDrive/Documents/Blog Website/Basketball_Database/NBA/PLAYER/PLAYOFFS"
load(file = file.path(playoff_fp,"NBA_PLAYER_PLAYOFF_TOTAL.rda"))

# Aggregating based by player
nba_playoff_career_total <- nba_playoff_total %>%
  group_by(`Player ID`, Player) %>%
  summarise( G = sum(G, na.rm = TRUE),
             GS = sum(GS, na.rm = TRUE),
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
             PTS = round(sum(PTS, na.rm = TRUE), 2)) %>%
arrange(`Player ID`)

# Save career data frame to a rda file
save(nba_playoff_career_total,file = file.path(career_fp,"NBA_PLAYER_CAREER_PLAYOFF_TOTAL.rda"))

# REGULAR SEASON AND PLAYOFFS

# Full join the two dataframes on Player ID and Player
nba_career_total <- full_join(nba_reg_career_total, nba_playoff_career_total, 
                              by = c("Player ID", "Player"), suffix = c("_reg", "_playoff"))

# Replace NA values with 0 in numeric columns
nba_career_total <- nba_career_total %>%
  mutate(across(where(is.numeric), ~ replace_na(., 0)))

# Combine regular season and playoff statistics under the same name
nba_career_total <- nba_career_total %>%
  mutate(
    G = G_reg + G_playoff,
    GS = GS_reg + GS_playoff,
    MP = MP_reg + MP_playoff,
    FG = FG_reg + FG_playoff,
    FGA = FGA_reg + FGA_playoff,
    `FG%` = round(FG / FGA, 3),
    `3P` = `3P_reg` + `3P_playoff`,
    `3PA` = `3PA_reg` + `3PA_playoff`,
    `3P%` = round(`3P` / `3PA`, 3),
    `2P` = `2P_reg` + `2P_playoff`,
    `2PA` = `2PA_reg` + `2PA_playoff`,
    `2P%` = round(`2P` / `2PA`, 3),
    `eFG%` = round((FG + 0.5 * `3P`) / FGA, 3),
    FT = FT_reg + FT_playoff,
    FTA = FTA_reg + FTA_playoff,
    `FT%` = round(FT / FTA, 3),
    ORB = ORB_reg + ORB_playoff,
    DRB = DRB_reg + DRB_playoff,
    TRB = TRB_reg + TRB_playoff,
    AST = AST_reg + AST_playoff,
    STL = STL_reg + STL_playoff,
    BLK = BLK_reg + BLK_playoff,
    TOV = TOV_reg + TOV_playoff,
    PF = PF_reg + PF_playoff,
    PTS = PTS_reg + PTS_playoff) %>%
  select(`Player ID`, Player, G, GS, MP, FG, FGA, `FG%`, `3P`, `3PA`, `3P%`, `2P`, `2PA`, `2P%`, `eFG%`, 
         FT, FTA, `FT%`, ORB, DRB, TRB, AST, STL, BLK, TOV, PF, PTS) %>%
  arrange(`Player ID`)

# Save career data frame to a rda file
save(nba_career_total,file = file.path(career_fp,"NBA_PLAYER_CAREER_ALL_TOTAL.rda"))
