rem ==================================================
rem Freshen the archive file with latest file versions
rem ==================================================

cd ..\..
copy /B gwin.zip gwin_ok.zip
zip -f -9 -t 2014-03-01 gwin.zip
cd gwindows\installer

pause

rem Comparison old - new archive (comp_zip is a tool from Zip-Ada)
comp_zip ..\..\gwin_ok.zip ..\..\gwin.zip 

pause
