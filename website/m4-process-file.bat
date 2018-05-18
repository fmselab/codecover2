ECHO OFF
SETLOCAL

REM %1 Quelldatei
REM %2 Zieldatei
REM %3 relativer Pfad zur Datei
REM %4 relativer Pfad zum Root

ECHO %3

SET RELATIVE=%4
IF "%RELATIVE%" == "" SET RELATIVE=.

m4 -P -I . -D m4_web_pagename="%~3" -D m4_web_rootdir="%RELATIVE%" %1 > %2

ENDLOCAL