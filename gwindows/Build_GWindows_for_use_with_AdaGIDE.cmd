@ECHO OFF

ECHO Installing GWindows.....

rem copy obsolete\mak* .
rem make ANSI=1 install

cd tools\gnatreg
windres gnatreg.rc ..\..\obj\gnatreg.coff
cd ..\gbmanager
windres gbmanager.rc ..\..\obj\gbmanager.coff
cd ..\..

gnatmake -P gwindows
gnatmake -P gwindows_tools

ECHO Install Completed