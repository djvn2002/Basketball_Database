@echo off

:: Clear log file at the start of each run
echo. > "C:\Users\djvia\OneDrive\Documents\Blog Website\Basketball_Database\SHOT APP\BATCH FILES\log.txt"

:: Log file location
set log_file="C:\Users\djvia\OneDrive\Documents\Blog Website\Basketball_Database\SHOT APP\BATCH FILES\log.txt"

:: WNBA Shots Script
echo Running WNBA Shots Script >> %log_file%
"C:\Program Files\R\R-4.4.1\bin\Rscript.exe" "C:\Users\djvia\OneDrive\Documents\Blog Website\Basketball_Database\SHOT APP\BATCH FILES\wnba_shots_recent.R" >> %log_file% 2>&1
if %ERRORLEVEL% neq 0 (
    echo WNBA Shots Script failed at %time% on %date% >> %log_file%
    exit /b 1
)
echo WNBA Shots Script completed at %time% on %date% >> %log_file%

:: NBA Shots Script
echo Running NBA Shots Script >> %log_file%
"C:\Program Files\R\R-4.4.1\bin\Rscript.exe" "C:\Users\djvia\OneDrive\Documents\Blog Website\Basketball_Database\SHOT APP\BATCH FILES\nba_shots_recent.R" >> %log_file% 2>&1
if %ERRORLEVEL% neq 0 (
    echo NBA Shots Script failed at %time% on %date% >> %log_file%
    exit /b 1
)
echo NBA Shots Script completed at %time% on %date% >> %log_file%

:: MNCAA Shots Script (Men's College)
echo Running MNCAA Shots Script >> %log_file%
"C:\Program Files\R\R-4.4.1\bin\Rscript.exe" "C:\Users\djvia\OneDrive\Documents\Blog Website\Basketball_Database\SHOT APP\BATCH FILES\mens_college_shot_recent.R" >> %log_file% 2>&1
if %ERRORLEVEL% neq 0 (
    echo MNCAA Shots Script failed at %time% on %date% >> %log_file%
    exit /b 1
)
echo MNCAA Shots Script completed at %time% on %date% >> %log_file%

:: WNCAA Shots Script (Women's College)
echo Running WNCAA Shots Script >> %log_file%
"C:\Program Files\R\R-4.4.1\bin\Rscript.exe" "C:\Users\djvia\OneDrive\Documents\Blog Website\Basketball_Database\SHOT APP\BATCH FILES\womens_college_shot_recent.R" >> %log_file% 2>&1
if %ERRORLEVEL% neq 0 (
    echo WNCAA Shots Script failed at %time% on %date% >> %log_file%
    exit /b 1
)
echo WNCAA Shots Script completed at %time% on %date% >> %log_file%

echo All scripts have completed successfully at %time% on %date% >> %log_file%

cd "C:\Users\djvia\OneDrive\Documents\Blog Website\Basketball_Database\SHOT APP"
git add .
git commit -m "Shot RDA Files Updated at %date% %time%"
git push origin main
git pull origin main