@echo off

rem Version should match the field VALUE "FileVersion" in GW_Install.rc
set version=31-Jul-2020

if not exist GW_Install_Resource_GUI.ads goto gen
if not exist GW_Install_Resource_GUI.adb goto gen
if "%1"=="-gen" goto gen

goto no_gen

:gen
cd ..\..\gwenerator\src
call make
cd ..\..\gwindows\extractor
..\..\gwenerator\rc2gw GW_Install.rc
if "%1"=="-gen" shift

:no_gen

if not exist ..\..\gwin.zip echo The archive gwin.zip is missing.
if not exist ..\..\gwin.zip echo Consequence, the new installer executable can't work.
if not exist ..\..\gwin.zip echo You can retrieve gwin.zip from the SVN repository or 
if not exist ..\..\gwin.zip echo from a previous installer executable.
if not exist ..\..\gwin.zip echo.
if not exist ..\..\gwin.zip echo Press any key to continue, or Ctrl-Break or Ctrl-C to stop.
if not exist ..\..\gwin.zip pause

gnatmake -P GW_Install.gpr -XBuild_Mode=Debug
copy /B gw_extract.exe + ..\..\gwin.zip "Extract Test (Debug mode).exe"
del gw_extract_deb.exe
ren gw_extract.exe gw_extract_deb.exe

if not exist obj\small\libwin32ada.a copy /B obj\debug\libwin32ada.a obj\small\libwin32ada.a

gnatmake -P GW_Install.gpr -XBuild_Mode=Small
copy /B gw_extract.exe gw_extract_small_no_upx.exe

upx --ultra-brute gw_extract.exe
if exist *.upx del *.upx
if exist *.000 del *.000
if exist *.001 del *.001

copy /B gw_extract.exe + ..\..\gwin.zip "GWindows %version%.exe"
copy /B                  ..\..\gwin.zip "GWindows Archive %version%.zip"

