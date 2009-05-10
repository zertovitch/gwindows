@echo off

rem Build the tool if not done...
cd..
cd src
call make
cd..
cd test

echo.
echo *** Creator: ResEdit.
call make_one GW_RE_App

echo.
echo *** Creator: Visual Studio 2008. We choose the separate packages option.
call make_one -s VS_2008

echo.
echo *** Creator: DlgEdit 3.10.200 1990-1992...
copy GW_DlgEdit_App.dlg GW_DlgEdit_App.rc
call make_one GW_DlgEdit_App
del GW_DlgEdit_App.rc

rem Just test parsing some famous .rc files

call make_one TeXCAD
call make_one AdaGIDE

rem Just test parsing some .rc google'd on the web using keywords

for %%w in (Web*.rc) do call make_one %%~nw

pause