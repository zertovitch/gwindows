@echo off

echo ====================================
echo Building the code generator
echo  - GWenerator, the GUI version
echo  - RC2GW, the command-line version
echo ====================================

cd src
call make
cd..

echo ========================================
echo GWenerator and RC2GW should be built now
echo ========================================
dir *.exe

echo ================================================
echo Test the code generator - Crtl-Break to stop now
echo ================================================
pause

cd test
call make_all
cd..
