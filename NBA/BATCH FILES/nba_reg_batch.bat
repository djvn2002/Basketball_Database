@echo off

:: Clear log file at the start of each run
echo. > "C:\Users\djvia\OneDrive\Documents\Blog Website\Basketball_Database\NBA\BATCH FILES\nba_reg_update.txt"

:: Log file location
set log_file="C:\Users\djvia\OneDrive\Documents\Blog Website\Basketball_Database\NBA\BATCH FILES\nba_reg_update.txt"

:: NBA Roster Update Script
echo Running NBA Roster Update Script >> %log_file%
"C:\Program Files\R\R-4.4.1\bin\Rscript.exe" "C:\Users\djvia\OneDrive\Documents\Blog Website\Basketball_Database\NBA\BATCH FILES\R\NBA_REG_ROSTER_UPDATE.R" >> %log_file% 2>&1
if %ERRORLEVEL% neq 0 (
    echo NBA Roster Update Script failed at %time% on %date% >> %log_file%
    exit /b 1
)
echo NBA Roster Update Script completed at %time% on %date% >> %log_file%

:: Wait for 2 minutes (120 seconds)
timeout /t 120 /nobreak

:: NBA Player Total Update Script
echo Running NBA Player Total Update Script >> %log_file%
"C:\Program Files\R\R-4.4.1\bin\Rscript.exe" "C:\Users\djvia\OneDrive\Documents\Blog Website\Basketball_Database\NBA\BATCH FILES\R\NBA_REG_PLAYER_TOTAL_UPDATE.R" >> %log_file% 2>&1
if %ERRORLEVEL% neq 0 (
    echo NBA Player Total Update Script failed at %time% on %date% >> %log_file%
    exit /b 1
)
echo NBA Player Total Update Script completed at %time% on %date% >> %log_file%

:: Wait for 2 minutes (120 seconds)
timeout /t 120 /nobreak

:: NBA Player Per Game Update Script
echo Running NBA Player Per Game Update Script >> %log_file%
"C:\Program Files\R\R-4.4.1\bin\Rscript.exe" "C:\Users\djvia\OneDrive\Documents\Blog Website\Basketball_Database\NBA\BATCH FILES\R\NBA_REG_PLAYER_PER_GAME_UPDATE.R" >> %log_file% 2>&1
if %ERRORLEVEL% neq 0 (
    echo NBA Player Per Game Update Script failed at %time% on %date% >> %log_file%
    exit /b 1
)
echo NBA Player Per Game Update Script completed at %time% on %date% >> %log_file%

:: Wait for 2 minutes (120 seconds)
timeout /t 120 /nobreak

:: NBA Player Per 36 Minutes Update Script
echo Running NBA Player Per 36 Minutes Update Script >> %log_file%
"C:\Program Files\R\R-4.4.1\bin\Rscript.exe" "C:\Users\djvia\OneDrive\Documents\Blog Website\Basketball_Database\NBA\BATCH FILES\R\NBA_REG_PLAYER_PER_36_MIN_UPDATE.R" >> %log_file% 2>&1
if %ERRORLEVEL% neq 0 (
    echo NBA Player Per 36 Minutes Update Script failed at %time% on %date% >> %log_file%
    exit /b 1
)
echo NBA Player Per 36 Minutes Update Script completed at %time% on %date% >> %log_file%

:: Wait for 2 minutes (120 seconds)
timeout /t 120 /nobreak

:: NBA Player Per 100 Possessions Update Script
echo Running NBA Player Per 100 Possessions Update Script >> %log_file%
"C:\Program Files\R\R-4.4.1\bin\Rscript.exe" "C:\Users\djvia\OneDrive\Documents\Blog Website\Basketball_Database\NBA\BATCH FILES\R\NBA_REG_PLAYER_PER_100_POSS_UPDATE.R" >> %log_file% 2>&1
if %ERRORLEVEL% neq 0 (
    echo NBA Player Per 100 Possessions Update Script failed at %time% on %date% >> %log_file%
    exit /b 1
)
echo NBA Player Per 100 Possessions Update Script completed at %time% on %date% >> %log_file%

:: Wait for 2 minutes (120 seconds)
timeout /t 120 /nobreak

:: NBA Player Advanced Stats Update Script
echo Running NBA Player Advanced Stats Update Script >> %log_file%
"C:\Program Files\R\R-4.4.1\bin\Rscript.exe" "C:\Users\djvia\OneDrive\Documents\Blog Website\Basketball_Database\NBA\BATCH FILES\R\NBA_REG_PLAYER_ADVANCED_UPDATE.R" >> %log_file% 2>&1
if %ERRORLEVEL% neq 0 (
    echo NBA Player Advanced Stats Update Script failed at %time% on %date% >> %log_file%
    exit /b 1
)
echo NBA Player Advanced Stats Update Script completed at %time% on %date% >> %log_file%

:: Wait for 2 minutes (120 seconds)
timeout /t 120 /nobreak

:: NBA Player Adjusted Shooting Update Script
echo Running NBA Player Adjusted Shooting Update Script >> %log_file%
"C:\Program Files\R\R-4.4.1\bin\Rscript.exe" "C:\Users\djvia\OneDrive\Documents\Blog Website\Basketball_Database\NBA\BATCH FILES\R\NBA_REG_PLAYER_ADJ_SHOOTING_UPDATE.R" >> %log_file% 2>&1
if %ERRORLEVEL% neq 0 (
    echo NBA Player Adjusted Shooting Update Script failed at %time% on %date% >> %log_file%
    exit /b 1
)
echo NBA Player Adjusted Shooting Update Script completed at %time% on %date% >> %log_file%

:: Wait for 2 minutes (120 seconds)
timeout /t 120 /nobreak

:: NBA Player Shooting Update Script
echo Running NBA Player Shooting Update Script >> %log_file%
"C:\Program Files\R\R-4.4.1\bin\Rscript.exe" "C:\Users\djvia\OneDrive\Documents\Blog Website\Basketball_Database\NBA\BATCH FILES\R\NBA_REG_PLAYER_SHOOTING_UPDATE.R" >> %log_file% 2>&1
if %ERRORLEVEL% neq 0 (
    echo NBA Player Shooting Update Script failed at %time% on %date% >> %log_file%
    exit /b 1
)
echo NBA Player Shooting Update Script completed at %time% on %date% >> %log_file%

:: Wait for 2 minutes (120 seconds)
timeout /t 120 /nobreak

:: NBA Player Play-by-Play Update Script
echo Running NBA Player Play-by-Play Update Script >> %log_file%
"C:\Program Files\R\R-4.4.1\bin\Rscript.exe" "C:\Users\djvia\OneDrive\Documents\Blog Website\Basketball_Database\NBA\BATCH FILES\R\NBA_REG_PLAYER_PBP_UPDATE.R" >> %log_file% 2>&1
if %ERRORLEVEL% neq 0 (
    echo NBA Player Play-by-Play Update Script failed at %time% on %date% >> %log_file%
    exit /b 1
)
echo NBA Player Play-by-Play Update Script completed at %time% on %date% >> %log_file%

echo All scripts have completed successfully at %time% on %date% >> %log_file%

:: Git commands for commit and push
cd "C:\Users\djvia\OneDrive\Documents\Blog Website\Basketball_Database\NBA"
git add .
git commit -m "NBA Data Updated at %date% %time%"
git push origin main

:: Pull latest changes
git pull origin main