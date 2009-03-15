all:
	windres vbtoadarc.rc vbtoadarc.coff
	gnatmake vbtoada-exe
	gnatmake vbtoada-remote
	gnatmake vbtoada-dll
	gnatdll -n -e vbtoada.def -dvbtoada-dll.dll vbtoada-dll.ali

clean:
	-del b~*.*
	-del *.ali
	-del *.o
	-del *~
	-del *.dll
	-del *.exe
