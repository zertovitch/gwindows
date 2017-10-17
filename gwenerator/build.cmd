@echo off

echo ====================================
echo Building the code generator
echo  - GWenerator, the GUI version
echo  - RC2GW, the command-line version
echo ====================================

cd src
call make
cd..

echo.
echo =================================================================
echo GWenerator and RC2GW should be built and appear in this directory
echo =================================================================
dir *.exe

REM echo.
REM echo ==================================================================
REM echo Now we test the code generator. Press Ctrl-Break or Ctrl-C to stop
REM echo at this point, or any other key to go on.
REM echo ==================================================================
REM pause

REM cd test
REM call make_all
REM cd..
