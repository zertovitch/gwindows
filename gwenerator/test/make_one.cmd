if     "%1"=="-s" ..\rc2gw -s %2.rc
if not "%1"=="-s" ..\rc2gw %1.rc
if     "%1"=="-s" shift

gnatmake %1_Resource_GUI    -i -g -I../windows_stuff -aO..\obj\gnatdebg
if exist %1.adb gnatmake %1 -i -g -I../windows_stuff -aO..\obj\gnatdebg
