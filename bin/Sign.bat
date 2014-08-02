@echo off
title PM Sign

if "%1"=="" (
	echo %cd%
	set /p INPUT=Dateiname eingeben:
) else (
	set INPUT=%1
	echo %INPUT%
)

C:\Users\Phil\Informatik\PMCW\Signieren\signtool.exe sign /v /s "PM Store" /n "PM Code Works" "%cd%\%INPUT%"
C:\Users\Phil\Informatik\PMCW\Signieren\signtool.exe timestamp /t http://timestamp.globalsign.com/scripts/timstamp.dll "%cd%\%INPUT%"
pause
