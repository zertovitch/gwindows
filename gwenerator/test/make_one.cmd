set opt=

if "%1"=="-s" set rcopt72341251337=%1
if "%1"=="-s" shift

if "%1"=="-t" set rcopt72341251337=%rcopt72341251337% %1
if "%1"=="-t" shift

..\rc2gw %rcopt72341251337% -c %1.rc

set gnatopt928554631=-i -g -gnat05 -I..\..\gwindows\framework -I..\..\gnatcom\framework -aO..\obj\gnatdebg

gnatmake %1_Resource_GUI %gnatopt928554631% 
if exist %1.adb gnatmake %1 %gnatopt928554631% 
if exist %1_Test_app.adb gnatmake %1_Test_app %gnatopt928554631%

set rcopt72341251337=
set gnatopt928554631=