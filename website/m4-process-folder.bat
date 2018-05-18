@ECHO OFF
SETLOCAL

REM %1 Quellordner
REM %2 Zielordner
REM %3 relativer Pfad zum Ordner
REM %4 relativer Pfad zum Root

SET Source=%1
SET Target=%2
SET SUBDIR=%4

IF NOT EXIST %Target%\NUL MKDIR "%Target%"

REM ECHO %Source%
REM ECHO %Target%

FOR %%F IN (%Source%*.html.m4) DO CALL m4-process-file.bat %%~fF %Target%%%~nF %3%%~nF %SUBDIR%

IF NOT "%SUBDIR%" == "" SET SUBDIR=%SUBDIR%/
FOR /D %%F IN (%Source%*) DO CALL m4-process-folder.bat %Source%%%~nF\ %Target%%%~nF\ %3%%~nF/ %SUBDIR%..

ENDLOCAL