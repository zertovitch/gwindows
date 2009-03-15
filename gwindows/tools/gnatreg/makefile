PACKAGES = wgnatreg.exe

ADAOPTS = -I../../framework/

include ../../makefile.inc

gnatreg.exe: gnatreg.coff
	gnatmake ${ADAOPTS} gnatreg ${POSTOPTS}

wgnatreg.exe: gnatreg.exe
	$(COPYTOOL) gnatreg.exe wgnatreg.exe

gnatreg.coff:
	windres gnatreg.rc gnatreg.coff