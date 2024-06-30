PACKAGES = gnavi_ide
ADAOPTS = -I../../xml -I../../icg/src -I../../../gwindows/framework -I../../../gnatcom/framework

include ../../makefile.inc

gnavi_ide: control_creators.ads icg gnavi_ide.coff
	gnatmake ${ADAOPTS} gnavi_ide ${POSTOPTS}

gnavi_ide.coff: gnavi_ide.rc
	windres gnavi_ide.rc gnavi_ide.coff

.PHONY: icg
icg:
	../../icg/src/icg.exe gnavi_ide.gnp
#   Will be one day: ../../icg.exe gnavi_ide.gnp

control_generator.exe: control_generator.adb
	gnatmake ${ADAOPTS} control_generator ${POSTOPTS}

control_creators.ads: control_generator.exe
	Control_Generator.exe

cleanall: clean
	-del control_creators.ad?
	-del gnavi_layout_view-controls-dispatch.adb
	-del *-on_create.adb
