@ECHO OFF

ECHO Installing GWindows.....

rem copy obsolete\mak* .
rem make ANSI=1 install

gnatmake -P gwindows
ECHO Install Completed