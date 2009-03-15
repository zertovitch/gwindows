windres beeprc.rc beeprc.coff
gnatmake beep-exe
gnatmake beep-remote
gnatmake beep-dll
gnatdll -n -e beep.def -dbeep-dll.dll beep-dll.ali
