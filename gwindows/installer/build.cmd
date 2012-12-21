@echo off

rem Version should match the field VALUE "FileVersion" in GW_Install.rc
set version=22-Dec-2012

if exist ..\..\gwenerator\windows_stuff\gwindows.ads goto stuff_ok

echo --------------------------------------------------------
echo We put a fresh, ANSI, copy of the GWindows framework,
echo plus a copy of GNATCOM, into windows_stuff.
echo --------------------------------------------------------

mkdir ..\..\gwenerator\windows_stuff
copy ..\framework\*.ad* ..\..\gwenerator\windows_stuff
copy ..\framework\coding\gwindows_ansi.ads ..\..\gwenerator\windows_stuff\gwindows.ads
copy ..\framework\coding\gwindows-gstrings_ansi.adb ..\..\gwenerator\windows_stuff\gwindows-gstrings.adb
copy ..\framework\coding\gwindows-gstrings-handling_ansi.ads ..\..\gwenerator\windows_stuff\gwindows-gstrings-handling.ads
copy ..\framework\coding\gwindows-gstrings-maps_ansi.ads ..\..\gwenerator\windows_stuff\gwindows-gstrings-maps.ads
copy ..\framework\coding\gwindows-gstrings-maps_constants_ansi.ads ..\..\gwenerator\windows_stuff\gwindows-gstrings-maps_constants.ads
copy ..\framework\coding\gwindows-gstrings-unbounded_ansi.ads ..\..\gwenerator\windows_stuff\gwindows-gstrings-unbounded.ads
copy ..\..\gnatcom\framework\*.ad* ..\..\gwenerator\windows_stuff

:stuff_ok

if not exist GW_Install_Resource_GUI.ads goto gen
if not exist GW_Install_Resource_GUI.adb goto gen
if "%1"=="-gen" goto gen

goto no_gen

:gen
cd ..\..\gwenerator\src
call make
cd ..\..\gwindows\installer
..\..\gwenerator\rc2gw GW_Install.rc
if "%1"=="-gen" shift

:no_gen

gnatmake -P GW_Install.gpr -XBuild_Mode=Debug
copy /B gw_inst_tb.exe + gwin.zip "Setup_Test.exe"

gnatmake -P GW_Install.gpr -XBuild_Mode=Small
upx --ultra-brute gw_install.exe
if exist *.upx del *.upx

copy /B gw_install.exe + gwin.zip "GWindows Setup %version%.exe"
