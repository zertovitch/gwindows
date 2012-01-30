@echo off

echo ====================================
echo Building the code generator
echo  - GWenerator, the GUI version
echo  - RC2GW, the command-line version
echo ====================================

cd src
call make
cd..

echo ====================================
echo Test the code generator
echo ====================================

cd test
call make_all
cd..
