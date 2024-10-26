# Author: David Vialpando-Nielsen
# Date Made: 10/25/2024
# Latest Update: 10/25/2024

# This is an update file to update the following rda file:
# NBA_TEAM_PLAYOFF_PER_GAME.rda

# Library Packages
library(tidyverse)

# Load NBA_PLAYER_REG_TOTAL
team_fp <- "C:/Users/djvia/OneDrive/Documents/Blog Website/Basketball_Database/NBA/TEAM/PLAYOFFS"
load(file = file.path(team_fp,"NBA_TEAM_PLAYOFF_TOTAL.rda"))

# Aggregating based by dividing games played for each player by season
nba_team_ply_per_game <- nba_team_ply_total %>%
  group_by(`Franchise ID`, `Team Name` , `Team Abbr.`, Season, G, W, L, `W/L%`,
           Division, `Division Rank`, Conference, `Conference Rank`, `Play-In`, 
           `Win Play-In`,`First Round Wins`, `First Round Losses`,`First Round Opp.`,
           `Won First Round`,`Second Round Wins`,`Second Round Losses`,`Second Round Opp.`,
           `Won Second Round`,`Semifinals Wins`,`Semifinals Losses`,`Semifinals Opp.`,
           `Won Semifinals`,`Finals Wins`,`Finals Losses`,`Finals Opp.`,`Won Finals`) %>%
  summarise( G = sum(G, na.rm = TRUE),
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
  arrange(`Team Name`,desc(Season)) %>%
  select(-`Play-In`, -`Win Play-In`,-`First Round Wins`, -`First Round Losses`,-`First Round Opp.`,
         -`Won First Round`,-`Second Round Wins`,-`Second Round Losses`,-`Second Round Opp.`,
         -`Won Second Round`,-`Semifinals Wins`,-`Semifinals Losses`,-`Semifinals Opp.`,
         -`Won Semifinals`,-`Finals Wins`,-`Finals Losses`,-`Finals Opp.`,-`Won Finals`, 
         everything(), `Play-In`, `Win Play-In`,`First Round Wins`, `First Round Losses`,`First Round Opp.`,
         `Won First Round`,`Second Round Wins`,`Second Round Losses`,`Second Round Opp.`,
         `Won Second Round`,`Semifinals Wins`,`Semifinals Losses`,`Semifinals Opp.`,
         `Won Semifinals`,`Finals Wins`,`Finals Losses`,`Finals Opp.`,`Won Finals`)

# Save per game data frame to a rda file
save(nba_team_ply_per_game,file = file.path(team_fp,"NBA_TEAM_PLAYOFF_PER_GAME.rda"))