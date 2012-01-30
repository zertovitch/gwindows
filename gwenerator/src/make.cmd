@echo off
echo.
echo make: option -r   recreates rc2gw's and gwenerator's parser sources
echo              -gen recreates gwenerator's own GUI Ada sources with rc2gw
echo              -res recompiles the resource for getting bitmaps / icon actualized
echo.

if exist ..\..\gwenerator\windows_stuff\gwindows.ads goto stuff_ok

echo --------------------------------------------------------
echo We put a fresh, ANSI, copy of the GWindows framework,
echo plus a copy of GNATCOM, into windows_stuff.
echo --------------------------------------------------------

mkdir ..\windows_stuff
copy ..\..\gwindows\framework\*.ad* ..\windows_stuff
copy ..\..\gwindows\framework\coding\gwindows_ansi.ads ..\windows_stuff\gwindows.ads
copy ..\..\gwindows\framework\coding\gwindows-gstrings_ansi.adb ..\windows_stuff\gwindows-gstrings.adb
copy ..\..\gwindows\framework\coding\gwindows-gstrings-handling_ansi.ads ..\windows_stuff\gwindows-gstrings-handling.ads
copy ..\..\gwindows\framework\coding\gwindows-gstrings-maps_ansi.ads ..\windows_stuff\gwindows-gstrings-maps.ads
copy ..\..\gwindows\framework\coding\gwindows-gstrings-maps_constants_ansi.ads ..\windows_stuff\gwindows-gstrings-maps_constants.ads
copy ..\..\gwindows\framework\coding\gwindows-gstrings-unbounded_ansi.ads ..\windows_stuff\gwindows-gstrings-unbounded.ads
copy ..\..\gnatcom\framework\*.ad* ..\windows_stuff

:stuff_ok

if "%1"=="-h" goto fin
if "%1"=="--help" goto fin
if not "%1"=="-r" goto comp

rem
echo ** Compile AFLEX (.l) file to Ada sources
aflex.exe -i -E rc.l
echo.
gnatchop -w *.a
del *.a
rem
echo ** Compile AYACC (.y) file to Ada sources
ayacc.exe rc.y off off on on >ayacc.log
type ayacc.log
echo. >ayacc.log
type rc.verbose>>ayacc.log
del rc.verbose
rem
if exist yyparse.adb del yyparse.adb
ren rc.a yyparse.adb
shift
rem

:comp

rem **** Here the GWenerator generates a part of itself!... ****

if "%1"=="-gen" rc2gw GWenerator.rc
if "%1"=="-gen" shift
if "%1"=="-res" windres GWenerator.rc GWenerator.rbj
if "%1"=="-res" shift
if not exist GWenerator.rbj windres GWenerator.rc GWenerator.rbj

gnatmake %1 %2 gwenerator -PGWenerator.gpr -XBuild_Mode=Small
gnatmake %1 %2 rc2gw      -PGWenerator.gpr -XBuild_Mode=Small
copy /b GWenerator.exe ..\GWenerator.exe
copy /b rc2gw.exe ..\RC2GW.exe

:fin
