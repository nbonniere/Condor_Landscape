@echo off
setlocal

echo Will now unpack the downloaded data
echo.
timeout 3 /nobreak > /nul

rem make sure needed programs exist
set PATH=%PATH%;c:\Programs\7-Zip;c:\Program Files\7-Zip
where 7z
IF %ERRORLEVEL% NEQ 0 (
   echo ERROR: You need to install the 7z program to retrieve files.
   echo Copy this link to your browser: https://www.SoaringTools.org/downloads/7z2201-x64.msi
   echo and download. You can run the install, choose the "Repair" option for a new install.
   echo.
   echo Then you can try again.
   pause
   exit /b 9
)

rem set PATH=%PATH%;"C:\Program Files\Allmapsoft\gmid\geotiff"
rem goto directory where batch file is
cd /d %~dp0

rem loop thru ZIPs.txt file and unzip
set ZIPS=ZIPs.txt
if not exist %ZIPS% (
   echo ERROR - can't find list of zip file, %ZIPS%, in DEM folder, please start over.
   pause
   exit /b 9
)   

for /f %%G in (ZIPs.txt) DO (

   7z e -y %%G
   IF %ERRORLEVEL% NEQ 0 (
      echo Looks like an error occured.  If you need help from us, please copy all the text displayed and email it to us.
      echo The program is paused to allow you to read/copy the text.
      pause
      exit /B 9
   )
   timeout 2 /nobreak > /nul
)

echo.
echo.
echo All files extracted OK! 
timeout 3 /nobreak > /nul

endlocal
