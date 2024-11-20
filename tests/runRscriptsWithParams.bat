@echo off
REM Running R scripts in batch mode with parameters

REM Path to Rscript.exe
set RscriptPath="C:\Program Files\R\R-4.4.1\bin\x64\Rscript.exe"

REM Path to your R scripts directory
set ScriptDir="."

REM Parameters for the scripts
set param1=value1
set param2=value2

cd %ScriptDir%

REM Run each R script with parameters
%RscriptPath% "%ScriptDir%\script1.R" %param1% %param2%
%RscriptPath% "%ScriptDir%\script2.R" %param1% %param2%
%RscriptPath% "%ScriptDir%\script3.R" %param1% %param2%

REM Add more scripts with parameters as needed