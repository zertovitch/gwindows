set opt=

if "%1"=="-s" set opt=%1
if "%1"=="-s" shift

if "%1"=="-t" set opt=%opt% %1
if "%1"=="-t" shift

..\rc2gw %opt% -c %1.rc

gnatmake %1_Resource_GUI    -i -g -I../windows_stuff -aO..\obj\gnatdebg
if exist %1.adb gnatmake %1 -i -g -I../windows_stuff -aO..\obj\gnatdebg
if exist %1_Test_app.adb gnatmake %1_Test_app -i -g -I../windows_stuff -aO..\obj\gnatdebg
