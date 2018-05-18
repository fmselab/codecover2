@ECHO OFF
SETLOCAL
SET WebsiteHome=%~d0%~sp0

REM %1 -refresh > only the m4 files are newly created

REM Are we in the correct folder?
IF NOT EXIST "%WebsiteHome%website.inc.m4" GOTO FOLDER
SET TargetHome=%WebsiteHome%..\website-complete\

REM we delete the target, if we do not refresh
IF NOT "%1" == "-refresh" IF EXIST "%TargetHome%" RMDIR /S /Q "%TargetHome%"

ECHO m4-Makros aufl”sen
CALL m4-process-folder.bat %WebsiteHome% %TargetHome%

REM In this case, we skip to copy the other files
IF "%1" == "-refresh" GOTO END
ECHO.
ECHO Copy all non ignored files
ECHO .bak > ignored
ECHO .bat >> ignored
ECHO .m4 >> ignored
ECHO .sh >> ignored
ECHO ignored >> ignored
XCOPY /S /Y /Q /EXCLUDE:ignored *.* %TargetHome%
DEL ignored

GOTO END

:FOLDER
ECHO The folder "%WebsiteHome%" is not the website folder.
GOTO ENDE

:END
ENDLOCAL