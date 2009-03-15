PACKAGES = scribble scribble2 splash_screen text_spin

ADAOPTS = -I../../framework/

include ../../makefile.inc

scribble:
	gnatmake ${ADAOPTS} scribble ${POSTOPTS}

scribble2:
	gnatmake ${ADAOPTS} scribble2 ${POSTOPTS}

splash_screen: splash_screen.coff
	gnatmake ${ADAOPTS} splash_screen ${POSTOPTS}

splash_screen.coff: splash_screen.rc
	windres splash_screen.rc splash_screen.coff

text_spin:
	gnatmake ${ADAOPTS} text_spin ${POSTOPTS}
