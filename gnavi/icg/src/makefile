PACKAGES = icg
ADAOPTS = -I../../xml

include ../../makefile.inc

icg:
	gnatmake ${ADAOPTS} icg -largs -Xlinker --stack=0x2000000,0x20000

cleanall: clean
