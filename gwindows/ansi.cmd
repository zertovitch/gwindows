@echo off

echo Setting the ANSI (8 bit, String) version for following source files...
echo.
echo     framework\gwindows.ads
echo     framework\gwindows-gstrings.adb
echo     framework\gwindows-gstrings-handling.ads
echo     framework\gwindows-gstrings-maps.ads
echo     framework\gwindows-gstrings-maps_constants.ads
echo     framework\gwindows-gstrings-unbounded.ads
echo.

rem  ======================================
rem  This sets the ANSI version of GWindows
rem  ======================================

copy framework\coding\gwindows_ansi.ads                         framework\gwindows.ads
copy framework\coding\gwindows-gstrings_ansi.adb                framework\gwindows-gstrings.adb
copy framework\coding\gwindows-gstrings-handling_ansi.ads       framework\gwindows-gstrings-handling.ads
copy framework\coding\gwindows-gstrings-maps_ansi.ads           framework\gwindows-gstrings-maps.ads
copy framework\coding\gwindows-gstrings-maps_constants_ansi.ads framework\gwindows-gstrings-maps_constants.ads
copy framework\coding\gwindows-gstrings-unbounded_ansi.ads      framework\gwindows-gstrings-unbounded.ads

rem  This is for forcing recompilation.
rem  Both versions (ansi or unicode) of the above files may have the same time stamp, fooling gprbuild.
if exist obj\gwindows*.o del obj\gwindows*.o
