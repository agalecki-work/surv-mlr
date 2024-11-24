@echo off
setlocal enabledelayedexpansion
REM Set project, analysis, and script names 
REM Ex: prj_name=CRIC_prj, scriptBaseName=30-cric-cv.coxnet, anl_name=lin1
set prj_name=CRIC_prj
set scriptBaseName=30-cric-cv.coxnet
set anl_name=test
set args=%prj_name%:%anl_name%:%scriptBaseName%

REM Set tvar_ids to hold the sequence of parameters. Ex: tvar_ids=01 02 03 31 32
set tvar_ids=01 02 03 10 31 32

REM Path to Rscript.exe
set RscriptPath=C:\Program Files\R\R-4.4.1\bin\x64\Rscript.exe


REM Construct paths
REM Get the current directory path
set CDir=%CD%

set ourScriptPath=%CDir%\%scriptBaseName%
set outPath=%CDir%\%prj_name%\%anl_name%
set log=%outPath%\_%scriptBaseName%.log
set logx=%ourScriptPath%x.log
set logmap=%outPath%\_map.log

echo Current Directory: %CDir%
echo Args: %args%
echo tvar_ids: %tvar_ids%
echo log: %log%
echo logx: %logx%


REM Log the beginning of script execution
echo ===== %scriptBaseName%.R execution started on %DATE% at %TIME% >> %logx%
echo. > %logmap%
echo ============ %scriptBaseName%.R executed > %log%
echo Running script from the current directory: %CDir% >> %log%


REM Run R scripts with specified parameters in tvar_ids
for %%i in (%tvar_ids%) do (
    REM Call the timestamp function to update the cur_timestamp variable
    call :timestamp
    
    REM Log timestamp for each iteration
    echo --- Running iteration %%i started on !cur_timestamp! >> %logx%
    echo --- Running iteration %%i started on !cur_timestamp! >> %log%
    REM Log timestamp for each iteration
    "%RscriptPath%" --no-save --no-restore --verbose "%ourScriptPath%.R" %args% %%i >> %log% 2>&1
)
REM Log the end of script execution
call :timestamp
echo ===== %scriptBaseName%.R execution ended on %DATE% at %TIME% >> %logx%

REM Function to get the current timestamp
:timestamp
set cur_timestamp=%DATE% %TIME%
exit /b
