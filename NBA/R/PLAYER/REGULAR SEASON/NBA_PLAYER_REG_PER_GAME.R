# Author: David Vialpando-Nielsen
# Date Made: 8/25/2024
# Latest Update: 8/26/2024

# This file will run based on the assumption that you have already ran this file:
# NBA_PLAYER_REG_TOTAL.R

# The purpose of this file is to grab an aggregate of seasonal per game data by player.

# Library Packages
library(tidyverse)

# Load NBA_PLAYER_REG_TOTAL
player_fp <- "C:/Users/djvia/OneDrive/Documents/Blog Website/Basketball_Database/NBA/PLAYER/REGULAR SEASON"
load(file = file.path(player_fp,"NBA_PLAYER_REG_TOTAL.rda"))

# Aggregating based by dividing games played for each player by season
nba_reg_per_game <- nba_reg_total %>%
  group_by(Player, Age, Season, `Team Name`, `Team Abbr.`, League, URL) %>%
  summarise( G = sum(G, na.rm = TRUE),
             GS = sum(GS, na.rm = TRUE),
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
             `PTS/G` = round(sum(PTS, na.rm = TRUE) / sum(G, na.rm = TRUE), 2)) %>%
  select(Player, Age, G, GS, `MP/G`, `FG/G`, `FGA/G`, `FG%`, `3P/G`, `3PA/G`, `3P%`, `2P/G`, `2PA/G`, `2P%`, `eFG%`,
         `FT/G`, `FTA/G`, `FT%`, `ORB/G`, `DRB/G`, `TRB/G`, `AST/G`, `STL/G`, `BLK/G`, `TOV/G`, `PF/G`, `PTS/G`,
         URL, `Team Abbr.`, Season, `Team Name`, League)

# Save per game data frame to a rda file
save(nba_reg_per_game,file = file.path(player_fp,"NBA_PLAYER_REG_PER_GAME.rda"))
