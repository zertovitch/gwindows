PACKAGES = gw_in_a_dll call_gw_dll

ADAOPTS = -I../../framework/

include ../../makefile.inc

gw_in_a_dll:
	gnatmake ${ADAOPTS} gw_in_a_dll ${POSTOPTS}
	gnatdll -egw_in_a_dll.def -dgw_in_a_dll.dll gw_in_a_dll.ali

call_gw_dll:
	gnatmake ${ADAOPTS} call_gw_dll ${POSTOPTS}

cleanall: clean
	-del *.a