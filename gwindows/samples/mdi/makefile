PACKAGES = mdi_example

ADAOPTS = -I../../framework/

include ../../makefile.inc

mdi_example: mdi_example.coff
	gnatmake ${ADAOPTS} mdi_example ${POSTOPTS}

mdi_example.coff: mdi_example.rc
	windres mdi_example.rc mdi_example.coff

