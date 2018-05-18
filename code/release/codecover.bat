@echo off

rem $Id: codecover.bat 2317 2007-11-04 16:09:15Z muellecr $
setlocal
set CodeCoverHome=%~d0%~p0
java -Xmx512M -cp "%CodeCoverHome%lib\codecover-core.jar";"%CodeCoverHome%lib\codecover-batch.jar" org.codecover.batch.Batch %*
endlocal