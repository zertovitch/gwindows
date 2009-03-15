PACKAGES = gwindows

include ../makefile.inc

gwindows: tom.ads ado.ads

ifdef UNICODE
	make unicode
endif

ifdef ANSI
	make ansi
endif
	gnatmake ${ADAOPTS} gwindows
	gnatmake ${ADAOPTS} gwindows-gstrings
	gnatmake ${ADAOPTS} gwindows-gstrings-io
	gnatmake ${ADAOPTS} gwindows-gstrings-unbounded
	gnatmake ${ADAOPTS} gwindows-gstrings-constants
	gnatmake ${ADAOPTS} gwindows-gstrings-handling
	gnatmake ${ADAOPTS} gwindows-gstrings-maps
	gnatmake ${ADAOPTS} gwindows-gstrings-maps_constants
	gnatmake ${ADAOPTS} gwindows-utilities
	gnatmake ${ADAOPTS} gwindows-errors
	gnatmake ${ADAOPTS} gwindows-internal
	gnatmake ${ADAOPTS} gwindows-base
	gnatmake ${ADAOPTS} gwindows-application
	gnatmake ${ADAOPTS} gwindows-key_states
	gnatmake ${ADAOPTS} gwindows-message_boxes
	gnatmake ${ADAOPTS} gwindows-windows
	gnatmake ${ADAOPTS} gwindows-windows-main
	gnatmake ${ADAOPTS} gwindows-windows-mdi
	gnatmake ${ADAOPTS} gwindows-events
	gnatmake ${ADAOPTS} gwindows-colors
	gnatmake ${ADAOPTS} gwindows-cursors
	gnatmake ${ADAOPTS} gwindows-drawing
	gnatmake ${ADAOPTS} gwindows-drawing-capabilities
	gnatmake ${ADAOPTS} gwindows-drawing_objects
	gnatmake ${ADAOPTS} gwindows-drawing_panels
	gnatmake ${ADAOPTS} gwindows-menus
	gnatmake ${ADAOPTS} gwindows-registry
	gnatmake ${ADAOPTS} gwindows-buttons
	gnatmake ${ADAOPTS} gwindows-carets
	gnatmake ${ADAOPTS} gwindows-image_lists
	gnatmake ${ADAOPTS} gwindows-combo_boxes
	gnatmake ${ADAOPTS} gwindows-common_controls
	gnatmake ${ADAOPTS} gwindows-common_dialogs
	gnatmake ${ADAOPTS} gwindows-edit_boxes
	gnatmake ${ADAOPTS} gwindows-scintilla
	gnatmake gwindows-edit_boxes-rich
	gnatmake ${ADAOPTS} gwindows-list_boxes
	gnatmake ${ADAOPTS} gwindows-metrics
	gnatmake ${ADAOPTS} gwindows-panels
	gnatmake ${ADAOPTS} gwindows-packing_boxes
	gnatmake ${ADAOPTS} gwindows-scroll_bars
	gnatmake ${ADAOPTS} gwindows-scroll_panels
	gnatmake ${ADAOPTS} gwindows-static_controls
	gnatmake ${ADAOPTS} gwindows-multi_media
	gnatmake gwindows-activex
	gnatmake gwindows-databases
	gnatmake gwindows-databases-controls
	gnatmake ${ADAOPTS} gwindows-gcontrols
	gnatmake ${ADAOPTS} gwindows-gcontrols-gsize_bars
	gnatmake ${ADAOPTS} gwindows-gcontrols-duration
	gnatmake ${ADAOPTS} gwindows-gcontrols-duration-edit
	gnatmake standard_ids.ads

ado.ads:
	bindcom {00000200-0000-0010-8000-00AA006D2EA4} 2 0 ADO

tom.ads:
#	TOM is the Text Object Model specified by Microsoft
#	the tom.tlb is located in riched20.dll, but is not
#	always registered on the system. Which would allow:
#	bindcom {8CC497C9-A1DF-11CE-8098-00AA0047BE5D} 1 0 TOM
#	Therefore, we include the tlb for TOM in the redist dir.
#	It should be noted that tom.tlb does not need to be
#	registered on the OS in order to develop or use TOM
	bindcom ..\\redist\\tom.tlb TOM

unicode:
	$(UNLOCKTOOL) gwindows.ads
	$(UNLOCKTOOL) gwindows-gstrings.adb
	$(UNLOCKTOOL) gwindows-gstrings-unbounded.ads
	$(UNLOCKTOOL) gwindows-gstrings-handling.ads
	$(UNLOCKTOOL) gwindows-gstrings-maps.ads
	$(UNLOCKTOOL) gwindows-gstrings-maps_constants.ads
	$(COPYTOOL) coding$(DIRSEP)gwindows_unicode.ads gwindows.ads
	$(COPYTOOL) coding$(DIRSEP)gwindows-gstrings_unicode.adb \
		gwindows-gstrings.adb
	$(COPYTOOL) coding$(DIRSEP)gwindows-gstrings-unbounded_unicode.ads \
		gwindows-gstrings-unbounded.ads
	$(COPYTOOL) coding$(DIRSEP)gwindows-gstrings-handling_unicode.ads \
		gwindows-gstrings-handling.ads
	$(COPYTOOL) coding$(DIRSEP)gwindows-gstrings-maps_unicode.ads \
		gwindows-gstrings-maps.ads
	$(COPYTOOL) \
		coding$(DIRSEP)gwindows-gstrings-maps_constants_unicode.ads \
		gwindows-gstrings-maps_constants.ads
	$(LOCKTOOL) gwindows.ads
	$(LOCKTOOL) gwindows-gstrings.adb
	$(LOCKTOOL) gwindows-gstrings-unbounded.ads
	$(LOCKTOOL) gwindows-gstrings-handling.ads
	$(LOCKTOOL) gwindows-gstrings-maps.ads
	$(LOCKTOOL) gwindows-gstrings-maps_constants.ads

ansi:
	$(UNLOCKTOOL) gwindows.ads
	$(UNLOCKTOOL) gwindows-gstrings.adb
	$(UNLOCKTOOL) gwindows-gstrings-unbounded.ads
	$(UNLOCKTOOL) gwindows-gstrings-handling.ads
	$(UNLOCKTOOL) gwindows-gstrings-maps.ads
	$(UNLOCKTOOL) gwindows-gstrings-maps_constants.ads
	$(COPYTOOL) coding$(DIRSEP)gwindows_ansi.ads gwindows.ads
	$(COPYTOOL) coding$(DIRSEP)gwindows-gstrings_ansi.adb \
		gwindows-gstrings.adb
	$(COPYTOOL) coding$(DIRSEP)gwindows-gstrings-unbounded_ansi.ads \
		gwindows-gstrings-unbounded.ads
	$(COPYTOOL) coding$(DIRSEP)gwindows-gstrings-handling_ansi.ads \
		gwindows-gstrings-handling.ads
	$(COPYTOOL) coding$(DIRSEP)gwindows-gstrings-maps_ansi.ads \
		gwindows-gstrings-maps.ads
	$(COPYTOOL) coding$(DIRSEP)gwindows-gstrings-maps_constants_ansi.ads \
		gwindows-gstrings-maps_constants.ads
	$(LOCKTOOL) gwindows.ads
	$(LOCKTOOL) gwindows-gstrings.adb
	$(LOCKTOOL) gwindows-gstrings-unbounded.ads
	$(LOCKTOOL) gwindows-gstrings-handling.ads
	$(LOCKTOOL) gwindows-gstrings-maps.ads
	$(LOCKTOOL) gwindows-gstrings-maps_constants.ads

both:
	make clean
	make ANSI=1
	make clean
	make UNICODE=1

lock:
	$(LOCKTOOL) *.ali

unlock:
	-$(UNLOCKTOOL) *.*

register:
	./gnatreg set GWINDOWS

.PHONY: html
html:
	-mkdir html
	make unicode
	dir /b gwindows*.ads >files.txt
	adabrowse -ipackages.html -psubprogs.html \
		-tindex.html -f@files.txt -ohtml\\

cleanall: unlock clean
	-$(DELTOOL) gwindows.ads
	-$(DELTOOL) gwindows-gstrings.adb
	-$(DELTOOL) gwindows-gstrings-unbounded.ads
	-$(DELTOOL) gwindows-gstrings-handling.ads
	-$(DELTOOL) gwindows-gstrings-maps.ads
	-$(DELTOOL) gwindows-gstrings-maps_constants.ads
	-$(DELTOOL) files.txt
	-$(DELDIRTOOL) html
	-cmd /c "rmdir /s /q html"
	-$(DELTOOL) *.a
	-$(DELTOOL) tom*.*
	-$(DELTOOL) ado*.*
	-$(DELTOOL) coding$(DIRSEP)*~