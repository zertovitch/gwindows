rem ==================================================
rem Freshen the archive file with latest file versions
rem ==================================================

cd ..\..
copy /B gwindows\installer\gwin.zip gwindows\installer\gwin_ok.zip
zip -f -9 -t 2012-07-04 gwindows\installer\gwin.zip
cd gwindows\installer

pause

rem Comparison old - new archive (comp_zip is a tool from Zip-Ada)
comp_zip gwin_ok.zip gwin.zip 

pause
