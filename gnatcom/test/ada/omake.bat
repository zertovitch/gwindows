windres beeprc.rc beeprc.coff
gnatmake -O2 Beep-exe -largs -s
gnatmake -O2 Beep-remote -largs -s
gnatmake -O2 Beep-dll -largs -s
gnatdll -n -e beep.def -dBeep-dll.dll Beep-dll.ali -largs -s

@ECHO See http://www.AdaPower.com/links.html for link to UPX
@ECHO UPX majorly reduces the size of EXEs and DLLs
upx beep-exe.exe
upx beep-remote.exe
upx beep-dll.dll