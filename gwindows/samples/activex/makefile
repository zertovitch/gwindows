PACKAGES = mschart_demo ie_example pdf_example

ADAOPTS = -I../../framework/

include ../../makefile.inc

mschart_demo: mschart20.ads
	gnatmake ${ADAOPTS} mschart_demo ${POSTOPTS}

ie_example:
	gnatmake ${ADAOPTS} ie_example ${ADAOPTS}

pdf_example:
	gnatmake ${ADAOPTS} pdf_example ${ADAOPTS}

mschart20.ads:
	bindcom mschrt20.ocx mschart20

cleanall: clean
	-del mschart20*.*
