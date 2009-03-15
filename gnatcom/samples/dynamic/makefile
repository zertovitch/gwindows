PACKAGES = dynamic
ADAOPTS = -I../ -I../bindings/

include ../../makefile.inc

dynamic:
	gnatmake ${ADAOPTS} nt_gnatfind ${POSTOPTS}
	gnatmake ${ADAOPTS} spin ${POSTOPTS}
