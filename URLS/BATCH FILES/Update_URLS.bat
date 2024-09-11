@echo off

:: Clear log file at the start of each run
echo. > "C:\Users\djvia\OneDrive\Documents\Blog Website\Basketball_Database\URLS\BATCH FILES\log.txt"

:: Log file location
set log_file="C:\Users\djvia\OneDrive\Documents\Blog Website\Basketball_Database\URLS\BATCH FILES\log.txt"

:: NBA URL Scraping Script
echo Running NBA URL Scraping Script >> %log_file%
"C:\Program Files\R\R-4.4.1\bin\Rscript.exe" "C:\Users\djvia\OneDrive\Documents\Blog Website\Basketball_Database\URLS\NBA URLS\NBA_URL_SCRAPING.R" >> %log_file% 2>&1
if %ERRORLEVEL% neq 0 (
    echo NBA URL Scraping Script failed at %time% on %date% >> %log_file%
    exit /b 1
)
echo NBA URL Scraping Script completed at %time% on %date% >> %log_file%

:: ABA URL Scraping Script
echo Running ABA URL Scraping Script >> %log_file%
"C:\Program Files\R\R-4.4.1\bin\Rscript.exe" "C:\Users\djvia\OneDrive\Documents\Blog Website\Basketball_Database\URLS\ABA URLS\ABA_URL_SCRAPING.R" >> %log_file% 2>&1
if %ERRORLEVEL% neq 0 (
    echo ABA URL Scraping Script failed at %time% on %date% >> %log_file%
    exit /b 1
)
echo ABA URL Scraping Script completed at %time% on %date% >> %log_file%

:: WNBA URL Scraping Script
echo Running WNBA URL Scraping Script >> %log_file%
"C:\Program Files\R\R-4.4.1\bin\Rscript.exe" "C:\Users\djvia\OneDrive\Documents\Blog Website\Basketball_Database\URLS\WNBA URLS\WNBA_URL_SCRAPING.R" >> %log_file% 2>&1
if %ERRORLEVEL% neq 0 (
    echo WNBA URL Scraping Script failed at %time% on %date% >> %log_file%
    exit /b 1
)
echo WNBA URL Scraping Script completed at %time% on %date% >> %log_file%

echo All scripts have completed successfully at %time% on %date% >> %log_file%

:: Git commands for commit and push
cd "C:\Users\djvia\OneDrive\Documents\Blog Website\Basketball_Database\URLS"
git add .
git commit -m "URL Data Updated at %date% %time%"
git push origin main

:: Pull latest changes
git pull origin main