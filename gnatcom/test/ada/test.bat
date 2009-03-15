call make.bat
gnatmake beep_test
cd test2
call make.bat
cd ..
regsvr32 /s beep-dll.dll
beep_test
test2\beep_test
regsvr32 /s /u beep-dll.dll
beep-exe -RegServer
beep_test
test2\beep_test
beep-exe -UnregServer
