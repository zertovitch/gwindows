PACKAGES = getdir dialog_example

ADAOPTS = -I../../framework/
WININCLUDE = C:/gnatpro/pentium-mingw32msv/include/windows32

include ../../makefile.inc

dialog_example: dialog_example.coff
	gnatmake ${ADAOPTS} dialog_example ${POSTOPTS}


dialog_example.coff: dialog_example.rc
	windres -i dialog_example.rc -o dialog_example.coff

getdir:
	gnatmake $(ADAOPTS) getdir $(POSTOPTS)
