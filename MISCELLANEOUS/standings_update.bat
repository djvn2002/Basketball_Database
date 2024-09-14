@echo off

:: Log file location
set log_file="C:\Users\djvia\OneDrive\Documents\Blog Website\Basketball_Database\MISCELLANEOUS\standings_update_log.txt"

:: Clear the log file at the start of each run
echo. > %log_file%

:: NBA Standings Update Script
echo Running NBA Standings Update Script >> %log_file%
"C:\Program Files\R\R-4.4.1\bin\Rscript.exe" "C:\Users\djvia\OneDrive\Documents\Blog Website\Basketball_Database\MISCELLANEOUS\R\NBA_STANDINGS_UPDATE.R" >> %log_file% 2>&1
if %ERRORLEVEL% neq 0 (
    echo NBA Standings Update Script failed at %time% on %date% >> %log_file%
    exit /b 1
)
echo NBA Standings Update Script completed at %time% on %date% >> %log_file%

:: Optional Git Commands if you need to push the updates
cd "C:\Users\djvia\OneDrive\Documents\Blog Website\Basketball_Database\MISCELLANEOUS"
git add .
git commit -m "Standings Data Updated at %date% %time%"
git push origin main