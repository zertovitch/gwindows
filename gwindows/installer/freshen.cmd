rem ==================================================
rem Freshen the archive file with latest file versions
rem ==================================================

cd ..\..
copy /B gwin.zip gwin_ok.zip
zip -f -9 -t 2014-10-26 gwin.zip
cd gwindows\installer

rem Zip is fooled by DST time stamps, should use AZip instead

pause

rem Comparison old - new archive (comp_zip is a tool from Zip-Ada)
comp_zip ..\..\gwin_ok.zip ..\..\gwin.zip 

pause
