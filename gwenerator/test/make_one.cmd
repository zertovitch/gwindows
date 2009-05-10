if     "%1"=="-s" ..\rc2gw -s %2.rc
if not "%1"=="-s" ..\rc2gw %1.rc
if     "%1"=="-s" shift

gnatmake %1_Resource_GUI    -i -I../windows_stuff
if exist %1.adb gnatmake %1 -i -I../windows_stuff
