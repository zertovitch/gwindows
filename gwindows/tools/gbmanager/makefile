PACKAGES = gbmanager

ADAOPTS = -I../../framework/ -I../../../gnatcom/tools -I../gnatreg

include ../../makefile.inc

gbmanager.coff:
	windres gbmanager.rc gbmanager.coff

gbmanager: gbmanager.coff
	gnatmake ${ADAOPTS} gbmanager ${POSTOPTS}