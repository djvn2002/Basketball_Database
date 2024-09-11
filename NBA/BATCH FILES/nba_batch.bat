@echo off

:: Clear log file at the start of each run
echo. > "C:\Users\djvia\OneDrive\Documents\Blog Website\Basketball_Database\NBA\BATCH FILES\log.txt"

:: Log file location
set log_file="C:\Users\djvia\OneDrive\Documents\Blog Website\Basketball_Database\NBA\BATCH FILES\log.txt"

:: NBA Roster Update Script
echo Running NBA Roster Update Script >> %log_file%
"C:\Program Files\R\R-4.4.1\bin\Rscript.exe" "C:\Users\djvia\OneDrive\Documents\Blog Website\Basketball_Database\NBA\BATCH FILES\R\NBA_REG_ROSTER_UPDATE.R" >> %log_file% 2>&1
if %ERRORLEVEL% neq 0 (
    echo NBA Roster Update Script failed at %time% on %date% >> %log_file%
    exit /b 1
)
echo NBA Roster Update Script completed at %time% on %date% >> %log_file%

echo All scripts have completed successfully at %time% on %date% >> %log_file%

:: Git commands for commit and push
cd "C:\Users\djvia\OneDrive\Documents\Blog Website\Basketball_Database\NBA"
git add .
git commit -m "NBA Roster Data Updated at %date% %time%"
git push origin main

:: Pull latest changes
git pull origin main