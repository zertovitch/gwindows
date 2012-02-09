rem ==================================================
rem Freshen the archive file with latest file versions
rem ==================================================

cd ..\..
copy /B gwindows\installer\gwin.zip gwindows\installer\gwin_ok.zip
zip -f -9 -t 2012-01-31 gwindows\installer\gwin.zip
cd gwindows\installer

pause
