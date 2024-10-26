@echo off

:: Clear log file at the start of each run
echo. > "C:\Users\djvia\OneDrive\Documents\Blog Website\Basketball_Database\NBA\BATCH FILES\nba_reg_update.txt"

:: Log file location
set log_file="C:\Users\djvia\OneDrive\Documents\Blog Website\Basketball_Database\NBA\BATCH FILES\nba_reg_update.txt"

:: PLAYER FILES

:: NBA Roster Update Script
echo Running NBA Roster Update Script >> %log_file%
"C:\Program Files\R\R-4.4.1\bin\Rscript.exe" "C:\Users\djvia\OneDrive\Documents\Blog Website\Basketball_Database\NBA\BATCH FILES\R\REGULAR SEASON\NBA_REG_ROSTER_UPDATE.R" >> %log_file% 2>&1
if %ERRORLEVEL% neq 0 (
    echo NBA Roster Update Script failed at %time% on %date% >> %log_file%
    exit /b 1
)
echo NBA Roster Update Script completed at %time% on %date% >> %log_file%

:: Wait for 2 minutes (120 seconds)
timeout /t 120 /nobreak

:: NBA Player Total Update Script
echo Running NBA Player Total Update Script >> %log_file%
"C:\Program Files\R\R-4.4.1\bin\Rscript.exe" "C:\Users\djvia\OneDrive\Documents\Blog Website\Basketball_Database\NBA\BATCH FILES\R\REGULAR SEASON\NBA_REG_PLAYER_TOTAL_UPDATE.R" >> %log_file% 2>&1
if %ERRORLEVEL% neq 0 (
    echo NBA Player Total Update Script failed at %time% on %date% >> %log_file%
    exit /b 1
)
echo NBA Player Total Update Script completed at %time% on %date% >> %log_file%

:: Wait for 2 minutes (120 seconds)
timeout /t 120 /nobreak

:: NBA Player Per Game Update Script
echo Running NBA Player Per Game Update Script >> %log_file%
"C:\Program Files\R\R-4.4.1\bin\Rscript.exe" "C:\Users\djvia\OneDrive\Documents\Blog Website\Basketball_Database\NBA\BATCH FILES\R\REGULAR SEASON\NBA_REG_PLAYER_PER_GAME_UPDATE.R" >> %log_file% 2>&1
if %ERRORLEVEL% neq 0 (
    echo NBA Player Per Game Update Script failed at %time% on %date% >> %log_file%
    exit /b 1
)
echo NBA Player Per Game Update Script completed at %time% on %date% >> %log_file%

:: Wait for 2 minutes (120 seconds)
timeout /t 120 /nobreak

:: NBA Player Per 36 Minutes Update Script
echo Running NBA Player Per 36 Minutes Update Script >> %log_file%
"C:\Program Files\R\R-4.4.1\bin\Rscript.exe" "C:\Users\djvia\OneDrive\Documents\Blog Website\Basketball_Database\NBA\BATCH FILES\R\REGULAR SEASON\NBA_REG_PLAYER_PER_36_MIN_UPDATE.R" >> %log_file% 2>&1
if %ERRORLEVEL% neq 0 (
    echo NBA Player Per 36 Minutes Update Script failed at %time% on %date% >> %log_file%
    exit /b 1
)
echo NBA Player Per 36 Minutes Update Script completed at %time% on %date% >> %log_file%

:: Wait for 2 minutes (120 seconds)
timeout /t 120 /nobreak

:: NBA Player Per 100 Possessions Update Script
echo Running NBA Player Per 100 Possessions Update Script >> %log_file%
"C:\Program Files\R\R-4.4.1\bin\Rscript.exe" "C:\Users\djvia\OneDrive\Documents\Blog Website\Basketball_Database\NBA\BATCH FILES\R\REGULAR SEASON\NBA_REG_PLAYER_PER_100_POSS_UPDATE.R" >> %log_file% 2>&1
if %ERRORLEVEL% neq 0 (
    echo NBA Player Per 100 Possessions Update Script failed at %time% on %date% >> %log_file%
    exit /b 1
)
echo NBA Player Per 100 Possessions Update Script completed at %time% on %date% >> %log_file%

:: Wait for 2 minutes (120 seconds)
timeout /t 120 /nobreak

:: NBA Player Advanced Stats Update Script
echo Running NBA Player Advanced Stats Update Script >> %log_file%
"C:\Program Files\R\Rscript.exe" "C:\Users\djvia\OneDrive\Documents\Blog Website\Basketball_Database\NBA\BATCH FILES\R\REGULAR SEASON\NBA_REG_PLAYER_ADVANCED_UPDATE.R" >> %log_file% 2>&1
if %ERRORLEVEL% neq 0 (
    echo NBA Player Advanced Stats Update Script failed at %time% on %date% >> %log_file%
    exit /b 1
)
echo NBA Player Advanced Stats Update Script completed at %time% on %date% >> %log_file%

:: Wait for 2 minutes (120 seconds)
timeout /t 120 /nobreak

:: NBA Player Adjusted Shooting Update Script
echo Running NBA Player Adjusted Shooting Update Script >> %log_file%
"C:\Program Files\R\Rscript.exe" "C:\Users\djvia\OneDrive\Documents\Blog Website\Basketball_Database\NBA\BATCH FILES\R\REGULAR SEASON\NBA_REG_PLAYER_ADJ_SHOOTING_UPDATE.R" >> %log_file% 2>&1
if %ERRORLEVEL% neq 0 (
    echo NBA Player Adjusted Shooting Update Script failed at %time% on %date% >> %log_file%
    exit /b 1
)
echo NBA Player Adjusted Shooting Update Script completed at %time% on %date% >> %log_file%

:: Wait for 2 minutes (120 seconds)
timeout /t 120 /nobreak

:: NBA Player Shooting Update Script
echo Running NBA Player Shooting Update Script >> %log_file%
"C:\Program Files\R\Rscript.exe" "C:\Users\djvia\OneDrive\Documents\Blog Website\Basketball_Database\NBA\BATCH FILES\R\REGULAR SEASON\NBA_REG_PLAYER_SHOOTING_UPDATE.R" >> %log_file% 2>&1
if %ERRORLEVEL% neq 0 (
    echo NBA Player Shooting Update Script failed at %time% on %date% >> %log_file%
    exit /b 1
)
echo NBA Player Shooting Update Script completed at %time% on %date% >> %log_file%

:: Wait for 2 minutes (120 seconds)
timeout /t 120 /nobreak

:: NBA Player Play-by-Play Update Script
echo Running NBA Player Play-by-Play Update Script >> %log_file%
"C:\Program Files\R\Rscript.exe" "C:\Users\djvia\OneDrive\Documents\Blog Website\Basketball_Database\NBA\BATCH FILES\R\REGULAR SEASON\NBA_REG_PLAYER_PBP_UPDATE.R" >> %log_file% 2>&1
if %ERRORLEVEL% neq 0 (
    echo NBA Player Play-by-Play Update Script failed at %time% on %date% >> %log_file%
    exit /b 1
)
echo NBA Player Play-by-Play Update Script completed at %time% on %date% >> %log_file%

:: TEAM FILES

:: NBA Team Total Update Script
echo Running NBA Team Total Update Script >> %log_file%
"C:\Program Files\R\Rscript.exe" "C:\Users\djvia\OneDrive\Documents\Blog Website\Basketball_Database\NBA\BATCH FILES\R\REGULAR SEASON\NBA_REG_TEAM_TOTAL_UPDATE.R" >> %log_file% 2>&1
if %ERRORLEVEL% neq 0 (
    echo NBA Team Total Update Script failed at %time% on %date% >> %log_file%
    exit /b 1
)
echo NBA Team Total Update Script completed at %time% on %date% >> %log_file%

:: Wait for 2 minutes (120 seconds)
timeout /t 120 /nobreak

:: NBA Team Per Game Update Script
echo Running NBA Team Per Game Update Script >> %log_file%
"C:\Program Files\R\Rscript.exe" "C:\Users\djvia\OneDrive\Documents\Blog Website\Basketball_Database\NBA\BATCH FILES\R\REGULAR SEASON\NBA_REG_TEAM_PER_GAME_UPDATE.R" >> %log_file% 2>&1
if %ERRORLEVEL% neq 0 (
    echo NBA Team Per Game Update Script failed at %time% on %date% >> %log_file%
    exit /b 1
)
echo NBA Team Per Game Update Script completed at %time% on %date% >> %log_file%

:: Wait for 2 minutes (120 seconds)
timeout /t 120 /nobreak

:: NBA Team Per 100 Possessions Update Script
echo Running NBA Team Per 100 Possessions Update Script >> %log_file%
"C:\Program Files\R\Rscript.exe" "C:\Users\djvia\OneDrive\Documents\Blog Website\Basketball_Database\NBA\BATCH FILES\R\REGULAR SEASON\NBA_REG_TEAM_100_POSS_UPDATE.R" >> %log_file% 2>&1
if %ERRORLEVEL% neq 0 (
    echo NBA Team Per 100 Possessions Update Script failed at %time% on %date% >> %log_file%
    exit /b 1
)
echo NBA Team Per 100 Possessions Update Script completed at %time% on %date% >> %log_file%

:: Wait for 2 minutes (120 seconds)
timeout /t 120 /nobreak

:: NBA Team Shooting Update Script
echo Running NBA Team Shooting Update Script >> %log_file%
"C:\Program Files\R\Rscript.exe" "C:\Users\djvia\OneDrive\Documents\Blog Website\Basketball_Database\NBA\BATCH FILES\R\REGULAR SEASON\NBA_REG_TEAM_SHOOTING_UPDATE.R" >> %log_file% 2>&1
if %ERRORLEVEL% neq 0 (
    echo NBA Team Shooting Update Script failed at %time% on %date% >> %log_file%
    exit /b 1
)
echo NBA Team Shooting Update Script completed at %time% on %date% >> %log_file%

:: Wait for 2 minutes (120 seconds)
timeout /t 120 /nobreak

:: NBA Team Advanced Stats Update Script
echo Running NBA Team Advanced Stats Update Script >> %log_file%
"C:\Program Files\R\Rscript.exe" "C:\Users\djvia\OneDrive\Documents\Blog Website\Basketball_Database\NBA\BATCH FILES\R\REGULAR SEASON\NBA_REG_TEAM_ADVANCED_UPDATE.R" >> %log_file% 2>&1
if %ERRORLEVEL% neq 0 (
    echo NBA Team Advanced Stats Update Script failed at %time% on %date% >> %log_file%
    exit /b 1
)
echo NBA Team Advanced Stats Update Script completed at %time% on %date% >> %log_file%

:: Wait for 2 minutes (120 seconds)
timeout /t 120 /nobreak

:: NBA Team Opponent Total Update Script
echo Running NBA Team Opponent Total Update Script >> %log_file%
"C:\Program Files\R\Rscript.exe" "C:\Users\djvia\OneDrive\Documents\Blog Website\Basketball_Database\NBA\BATCH FILES\R\REGULAR SEASON\NBA_REG_TEAM_OPP_TOTAL_UPDATE.R" >> %log_file% 2>&1
if %ERRORLEVEL% neq 0 (
    echo NBA Team Opponent Total Update Script failed at %time% on %date% >> %log_file%
    exit /b 1
)
echo NBA Team Opponent Total Update Script completed at %time% on %date% >> %log_file%

:: Wait for 2 minutes (120 seconds)
timeout /t 120 /nobreak

:: NBA Team Opponent Per Game Update Script
echo Running NBA Team Opponent Per Game Update Script >> %log_file%
"C:\Program Files\R\Rscript.exe" "C:\Users\djvia\OneDrive\Documents\Blog Website\Basketball_Database\NBA\BATCH FILES\R\REGULAR SEASON\NBA_REG_TEAM_OPP_PER_GAME_UPDATE.R" >> %log_file% 2>&1
if %ERRORLEVEL% neq 0 (
    echo NBA Team Opponent Per Game Update Script failed at %time% on %date% >> %log_file%
    exit /b 1
)
echo NBA Team Opponent Per Game Update Script completed at %time% on %date% >> %log_file%

:: Wait for 2 minutes (120 seconds)
timeout /t 120 /nobreak

:: NBA Team Opponent Per 100 Possessions Update Script
echo Running NBA Team Opponent Per 100 Possessions Update Script >> %log_file%
"C:\Program Files\R\Rscript.exe" "C:\Users\djvia\OneDrive\Documents\Blog Website\Basketball_Database\NBA\BATCH FILES\R\REGULAR SEASON\NBA_REG_TEAM_OPP_100_POSS_UPDATE.R" >> %log_file% 2>&1
if %ERRORLEVEL% neq 0 (
    echo NBA Team Opponent Per 100 Possessions Update Script failed at %time% on %date% >> %log_file%
    exit /b 1
)
echo NBA Team Opponent Per 100 Possessions Update Script completed at %time% on %date% >> %log_file%

:: Wait for 2 minutes (120 seconds)
timeout /t 120 /nobreak

:: NBA Team Opponent Shooting Update Script
echo Running NBA Team Opponent Shooting Update Script >> %log_file%
"C:\Program Files\R\Rscript.exe" "C:\Users\djvia\OneDrive\Documents\Blog Website\Basketball_Database\NBA\BATCH FILES\R\REGULAR SEASON\NBA_REG_TEAM_OPP_SHOOTING_UPDATE.R" >> %log_file% 2>&1
if %ERRORLEVEL% neq 0 (
    echo NBA Team Opponent Shooting Update Script failed at %time% on %date% >> %log_file%
    exit /b 1
)
echo NBA Team Opponent Shooting Update Script completed at %time% on %date% >> %log_file%

:: COACH FILE

:: NBA Coach Record Update Script
echo Running NBA Coach Record Update Script >> %log_file%
"C:\Program Files\R\Rscript.exe" "C:\Users\djvia\OneDrive\Documents\Blog Website\Basketball_Database\NBA\BATCH FILES\R\REGULAR SEASON\NBA_REG_COACH_RECORD_UPDATE.R" >> %log_file% 2>&1
if %ERRORLEVEL% neq 0 (
    echo NBA Coach Record Update Script failed at %time% on %date% >> %log_file%
    exit /b 1
)
echo NBA Coach Record Update Script completed at %time% on %date% >> %log_file%

:: All scripts completed
echo All scripts have completed successfully at %time% on %date% >> %log_file%

:: Git commands for commit and push
cd "C:\Users\djvia\OneDrive\Documents\Blog Website\Basketball_Database\NBA"
git add .
git commit -m "NBA Data Updated at %date% %time%"
git push origin main

:: Pull latest changes
git pull origin main