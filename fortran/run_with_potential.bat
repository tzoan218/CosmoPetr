@echo off
REM Wrapper script to run Fortran program with user-provided potential expression

if "%1"=="" (
    echo Usage: %0 ^<potential_expression_file^>
    echo Example: %0 my_potential.inc
    exit /b 1
)

set POTENTIAL_FILE=%1

if not exist "%POTENTIAL_FILE%" (
    echo Error: File '%POTENTIAL_FILE%' not found!
    exit /b 1
)

echo Using potential expression from: %POTENTIAL_FILE%
copy "%POTENTIAL_FILE%" potential_expression.inc >nul
echo Copied to potential_expression.inc

echo Compiling...
gfortran multifix.f -o m.exe
if errorlevel 1 (
    echo Compilation failed!
    exit /b 1
)

echo Running program...
m.exe

