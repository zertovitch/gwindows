PACKAGES = tcl_example

ADAOPTS = -I../../framework/

include ../../makefile.inc

tcl_example: tclcontrol.ads
	gnatmake ${ADAOPTS} tcl_example ${POSTOPTS}

tclcontrol.ads:
	bindcom ..\\..\\redist\\tclcontrol82.dll tclcontrol

cleanall: clean
	-del tclcontrol*.*
