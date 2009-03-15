PACKAGES = dispatch
ADAOPTS = -I../ -I../bindings/

include ../../makefile.inc

dispatch:
	gnatmake ${ADAOPTS} nt_gnatfind ${POSTOPTS}
	gnatmake ${ADAOPTS} spin ${POSTOPTS}
