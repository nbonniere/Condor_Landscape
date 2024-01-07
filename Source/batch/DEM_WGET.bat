@echo off
setlocal EnableDelayedExpansion

echo Will first download terrain data from USGS.
echo.
timeout 3 /nobreak > /nul

set PATH=%PATH%;c:\Programs\wget
where wget

IF %ERRORLEVEL% NEQ 0 (
   echo ERROR: You need to install the wget program to unpack the data
   echo Copy this link to your browser: https://www.SoaringTools.org/downloads/wget.exe
   echo and download.  You should save wget.exe in the folder C:\Program Files\Tools\
   echo If that folder doesn't exist, please create it first.
   echo. 
   echo If you get a permission error trying to download the .exe, you can download a zip version
   echo using this: https://gnuwin32.sourceforge.net/packages/wget_1_212_3.zip
   echo You must then unpack the zip, and move the wget.exe to C:\Program Files\Tools\
   echo.
   echo Then you can try again.
   pause
   exit /b 9
)

rem goto directory where batch file is
cd /d %~dp0
echo USGS SRTM data download
set uuuu=user
set pppp=password
if %uuuu% EQU user (set /p uuuu= User Name: )
if %pppp% EQU password (set /p pppp= Password: )

rem create a fresh file to store zip names
set ZIPS=ZIPs.txt
if exist %ZIPS% del /f %ZIPS%

rem loop thru URLs.txt files to download, store names, check for existence
set /a count=0
set /a fail=0

for /f %%G in (URLs.txt) DO (
  timeout 2 /nobreak > /nul

  rem get the zip name, check if downloaded
  for /F "tokens=6 delims=/" %%a in ("%%G") do set "zipFile=%%a"
  if exist !zipFile! (
    echo.
    echo Skipping !zipFile!, already exists
    echo !zipFile! >> %ZIPS%
    set /a count=count+1
  ) else (
    echo.
    echo Now retrieving !zipFile!
    wget  -nv --http-user=%uuuu% --http-password=%pppp%  %%G
    rem status check below doesn't work?
    if exist !zipFile! (
       echo Download OK
       echo !zipFile! >> %ZIPS%
       set /a count=count+1
    ) else (
       echo.
       echo ERROR: Unable to retrieve: !zipFile!
       set /a fail=fail+1
    )
  )
)

echo.
echo.

if !fail! GTR 0 (
   echo ERROR: Failed to retrieve !fail! files, !count! downloaded OK. You may retry the command again. > wget.log
   type wget.log
   echo Looks like an error occured.  If you need help from us, please copy all the text displayed and email it to us.
   echo The program is paused to allow you to read/copy the text.
   pause
   exit /B 7

) else (
   echo All files present/downloaded OK! > wget.log
   type wget.log
   timeout 3 /nobreak > /nul
) 

endlocal
