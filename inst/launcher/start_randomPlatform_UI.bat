@echo off
setlocal
cd /d "%~dp0"
Rscript "%~dp0start_randomPlatform_UI.R"
if errorlevel 1 pause
