gnatmake -gnatg initialize_test.adb
gnatmake -gnatg bstr_test.adb
gnatmake -gnatg guid_test.adb
gnatmake -gnatg variant_test.adb
gnatmake -gnatg safearray_test.adb
cd cpp\ada
call make.bat
cd ..\..
cd ada
call make.bat
cd ..