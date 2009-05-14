@echo off

echo.
echo save: option -f makes a full standalone, usable, publishable archive of GWenerator / rc2gw
echo.

call clean

set archive=GWenerator_%date%
if "%1"=="-f" set archive=%archive%_full
set archive=%archive%_.zip

zipada %archive% src/*.gpr src/*.ad* src/*.ago src/*.cmd src/*.y src/*.l
zip -9 %archive% src/*.bmp src/*.ico src/*.cur src/*.png src/*.txt src/*.rc src/*.h src/*.gwen
zip -9 %archive% *.cmd *.html *.txt
zip -9 %archive% test/*.res test/*.h test/*.dlg test/*.rc test/*.ad*
zip -9 %archive% test/*.ago test/*.cmd test/*.gwen test/*.xml
zip -9 %archive% obj/gnatdebg/*.pra obj/gnatsmal/*.txt

if not "%1"=="-f" goto fin

zip -9 %archive% *.exe
zip -9 %archive% windows_stuff/*.ad*
zip -9 %archive% img/*
cd..
zip -9 GWenerator/%archive% ayacc95/*.ad* aflex95/*.ad*
cd GWenerator

:fin

pause
