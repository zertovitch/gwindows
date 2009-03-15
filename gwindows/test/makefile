PACKAGES = compile
ADAOPTS = -I../framework -I../../third_party/aunit/framework -I../../third_party/aunit/text_reporter

include ../makefile.inc

compile: test_application.coff
	gnatmake ${ADAOPTS} test_gstrings ${POSTOPTS}
	gnatmake ${ADAOPTS} test_errors ${POSTOPTS}
	gnatmake ${ADAOPTS} test_application ${POSTOPTS}
	gnatmake ${ADAOPTS} test_resources ${POSTOPTS}
	gnatmake ${ADAOPTS} test_all_harness ${POSTOPTS}

test_application.coff: test_application.rc
	windres test_application.rc test_application.coff

test: compile
	test_gstrings
	test_errors
	test_application
	test_all_harness

test_gui: compile
	test_resources