call omake.bat
gnatmake -I../../src beep_test
regsvr32 /s beep-dll.dll
beep_test
regsvr32 /s /u ../release/cpp.dll
beep-exe -RegServer
beep_test
beep-exe -UnregServer