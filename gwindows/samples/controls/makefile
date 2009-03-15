PACKAGES = tab_test control_test win_controls gtoolbar

ADAOPTS = -I../../framework/

include ../../makefile.inc

tab_test:
	gnatmake ${ADAOPTS} tab_test ${POSTOPTS}

control_test: control_test.coff
	gnatmake ${ADAOPTS} control_test ${POSTOPTS}

control_test.coff: control_test.rc
	windres -i control_test.rc -o control_test.coff

win_controls:
	gnatmake $(ADAOPTS) win_controls $(POSTOPTS)

gtoolbar:
	gnatmake ${ADAOPTS} gtoolbar ${POSTOPTS}