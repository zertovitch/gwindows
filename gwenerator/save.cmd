@echo off

echo.
echo save: option -f makes a full standalone, usable, publishable archive of GWenerator / rc2gw
echo.

call clean
cd src
call make
cd..

set archive=GWenerator_%date%
if "%1"=="-f" set archive=%archive%_full
set archive=%archive%_.zip

set files=src/*.gpr src/*.ad* src/*.ago src/*.cmd src/*.y src/*.l
set files=%files% src/*.bmp src/*.ico src/*.cur src/*.png src/*.txt src/*.rc src/*.h src/*.gwen src/*.rbj src/*.xml
set files=%files% *.cmd gwenerator_info.html *.txt
set files=%files% test/*.res test/*.h test/*.dlg test/*.rc test/*.ad*
set files=%files% obj/gnatdebg/*.pra obj/gnatsmal/*.txt
set files=%files% test/*.ago test/*.cmd test/*.gwen test/mani*.xml

if "%1"=="-f" set files=%files% *.exe
if "%1"=="-f" set files=%files% img/*
if "%1"=="-f" set files=%files% ayacc/*.ad* aflex/*.ad* ayacc/*.gpr aflex/*.gpr
if "%1"=="-f" upx --ultra-brute *.exe

rem Zip tools @ http://unzip-ada.sf.net/

zipada -ed3 %archive% %files%
rezip -defl -comp %archive%

:fin

pause
