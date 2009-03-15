regsvr32 /s ../release/cpp.dll
call make.bat
beep_test
regsvr32 /s /u ../release/cpp.dll