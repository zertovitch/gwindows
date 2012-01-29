if "%1"=="-gen" ..\..\gwenerator\rc2gw GW_Install.rc
if "%1"=="-gen" shift

gnatmake -P GW_Install.gpr
copy /B gw_install.exe + gwin.zip "GWindows Setup.exe"
