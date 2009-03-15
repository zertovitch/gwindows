PACKAGES = hello_world form_example dock_test point_test key_test \
		menu_example splitters

ADAOPTS = -I../../framework/

include ../../makefile.inc

hello_world:
	gnatmake ${ADAOPTS} hello_world ${POSTOPTS}

form_example: form_example.coff
	gnatmake ${ADAOPTS} form_example ${POSTOPTS}

form_example.coff:
	windres form_example.rc form_example.coff

dock_test:
	gnatmake ${ADAOPTS} dock_test ${POSTOPTS}

point_test:
	gnatmake ${ADAOPTS} point_test ${POSTOPTS}

key_test:
	gnatmake ${ADAOPTS} key_test ${POSTOPTS}

menu_example:
	gnatmake ${ADAOPTS} menu_example ${POSTOPTS}

splitters:
	gnatmake ${ADAOPTS} splitters ${POSTOPTS}