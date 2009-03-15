PACKAGES = tools
ADAOPTS = -I../bindings

include ../makefile.inc

tools:
	gnatmake ${ADAOPTS} makeguid ${POSTOPTS}
	gnatmake ${ADAOPTS} comscope ${POSTOPTS}
	gnatmake ${ADAOPTS} bindcom ${POSTOPTS}
	gnatmake ${ADAOPTS} createcom ${POSTOPTS}
