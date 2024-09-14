@echo off

:: Clear log file at the start of each run
echo. > "C:\Users\djvia\OneDrive\Documents\Blog Website\Basketball_Database\MISCELLANEOUS\nba_indexes_log.txt"

:: Log file location
set log_file="C:\Users\djvia\OneDrive\Documents\Blog Website\Basketball_Database\MISCELLANEOUS\nba_indexes_log.txt"

:: NBA ABA Coach Index Script
echo Running NBA ABA Coach Index Script >> %log_file%
"C:\Program Files\R\R-4.4.1\bin\Rscript.exe" "C:\Users\djvia\OneDrive\Documents\Blog Website\Basketball_Database\MISCELLANEOUS\R\NBA_ABA_COACH_INDEX.R" >> %log_file% 2>&1
if %ERRORLEVEL% neq 0 (
    echo NBA ABA Coach Index Script failed at %time% on %date% >> %log_file%
    exit /b 1
)
echo NBA ABA Coach Index Script completed at %time% on %date% >> %log_file%

:: Wait for 2 minutes (120 seconds)
timeout /t 120 /nobreak

:: NBA ABA Player Index Script
echo Running NBA ABA Player Index Script >> %log_file%
"C:\Program Files\R\R-4.4.1\bin\Rscript.exe" "C:\Users\djvia\OneDrive\Documents\Blog Website\Basketball_Database\MISCELLANEOUS\R\NBA_ABA_PLAYER_INDEX.R" >> %log_file% 2>&1
if %ERRORLEVEL% neq 0 (
    echo NBA ABA Player Index Script failed at %time% on %date% >> %log_file%
    exit /b 1
)
echo NBA ABA Player Index Script completed at %time% on %date% >> %log_file%

echo All scripts have completed successfully at %time% on %date% >> %log_file%

:: Git commands for commit and push
cd "C:\Users\djvia\OneDrive\Documents\Blog Website\Basketball_Database"
git add .
git commit -m "NBA ABA Indexes Updated at %date% %time%"
git push origin main

:: Pull latest changes
git pull origin main