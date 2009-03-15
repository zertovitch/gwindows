PACKAGES = example

ADAOPTS = -I../../framework/ -I../../contrib/

include ../../makefile.inc

example: example.coff
	gnatmake ${ADAOPTS} example ${POSTOPTS}

example.coff: example.rc
	windres -i example.rc -o example.coff
