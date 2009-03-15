PACKAGES = packages
ADAOPTS = -I../framework

include ../makefile.inc

packages:
	gnatmake ${ADAOPTS} gwindows-colored_control_window
	gnatmake ${ADAOPTS} gwindows-buttons-ex_buttons
	gnatmake ${ADAOPTS} gwindows-common_controls-ex_tb
	gnatmake ${ADAOPTS} gwindows-common_controls-ex_lv
	gnatmake ${ADAOPTS} gwindows-common_controls-ex_lv_generic
	gnatmake ${ADAOPTS} gwindows-common_controls-ex_tv
	gnatmake ${ADAOPTS} gwindows-image_lists-ex_image_lists
	gnatmake ${ADAOPTS} gwindows-windows-ex_windows
	gnatmake ${ADAOPTS} gwindows-windows-ex_windows-ex_main
	gnatmake ${ADAOPTS} splitbar

lock:
	$(LOCKTOOL) *.ali

unlock:
	-$(UNLOCKTOOL) *.ali

register:
	./gnatreg set GWCONTRIB

cleanall: unlock clean
	-$(DELTOOL) files.txt
	-$(DELTOOL) *.a

