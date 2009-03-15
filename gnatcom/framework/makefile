PACKAGES = gnatcom reggnatcom gnatocx gnatocx_site

include ../makefile.inc

gnatcom:
	gnatmake ${ADAOPTS} gnatcom
	gnatmake ${ADAOPTS} gnatcom-utility
	gnatmake ${ADAOPTS} gnatcom-types
	gnatmake ${ADAOPTS} gnatcom-errors
	gnatmake ${ADAOPTS} gnatcom-guid
	gnatmake ${ADAOPTS} gnatcom-initialize
	gnatmake ${ADAOPTS} gnatcom-bstr
	gnatmake ${ADAOPTS} gnatcom-variant
	gnatmake ${ADAOPTS} gnatcom-iinterface
	gnatmake ${ADAOPTS} gnatcom-dispinterface
	gnatmake ${ADAOPTS} gnatcom-safearray
	gnatmake ${ADAOPTS} gnatcom-register
	gnatmake ${ADAOPTS} gnatcom-events
	gnatmake ${ADAOPTS} gnatcom-events-event_object
	gnatmake ${ADAOPTS} gnatcom-itypelib_interface
	gnatmake ${ADAOPTS} gnatcom-itypeinfo_interface
	gnatmake ${ADAOPTS} gnatcom-icreatetypelib_interface
	gnatmake ${ADAOPTS} gnatcom-icreatetypeinfo_interface
	gnatmake ${ADAOPTS} gnatcom-ienumvariant_interface
	gnatmake ${ADAOPTS} gnatcom-ierrorinfo
	gnatmake ${ADAOPTS} gnatcom-create
	gnatmake ${ADAOPTS} gnatcom-create-com_interface
	gnatmake ${ADAOPTS} gnatcom-create-factory
	gnatmake ${ADAOPTS} gnatcom-create-inproc
	gnatmake ${ADAOPTS} gnatcom-create-local_server
	gnatmake ${ADAOPTS} gnatcom-create-remote_register
	gnatmake ${ADAOPTS} gnatcom-create-idispatch

gnatocx:
	gnatmake ${ADAOPTS} gnatocx
	gnatmake ${ADAOPTS} gnatocx-iadvisesink_interface.adb
	gnatmake ${ADAOPTS} gnatocx-ibindctx_interface.adb
	gnatmake ${ADAOPTS} gnatocx-idataobject_interface.adb
	gnatmake ${ADAOPTS} gnatocx-ienumformatetc_interface.adb
	gnatmake ${ADAOPTS} gnatocx-ienummoniker_interface.adb
	gnatmake ${ADAOPTS} gnatocx-ienumoleverb_interface.adb
	gnatmake ${ADAOPTS} gnatocx-ienumstatdata_interface.adb
	gnatmake ${ADAOPTS} gnatocx-ienumstring_interface.adb
	gnatmake ${ADAOPTS} gnatocx-ienumunknown_interface.adb
	gnatmake ${ADAOPTS} gnatocx-imoniker_interface.adb
	gnatmake ${ADAOPTS} gnatocx-ioleclientsite_interface.adb
	gnatmake ${ADAOPTS} gnatocx-iolecontainer_interface.adb
	gnatmake ${ADAOPTS} gnatocx-iparsedisplayname_interface.adb
	gnatmake ${ADAOPTS} gnatocx-ipersiststream_interface.adb
	gnatmake ${ADAOPTS} gnatocx-ipersist_interface.adb
	gnatmake ${ADAOPTS} gnatocx-irunningobjecttable_interface.adb
	gnatmake ${ADAOPTS} gnatocx-isequentialstream_interface.adb
	gnatmake ${ADAOPTS} gnatocx-istream_interface.adb
	gnatmake ${ADAOPTS} gnatocx-iolewindow_interface.adb
	gnatmake ${ADAOPTS} gnatocx-ioleobject_interface.adb
	gnatmake ${ADAOPTS} gnatocx-ioleinplaceobject_interface.adb

gnatocx_site:
	gnatmake ${ADAOPTS} gnatocx_site.ads
	gnatmake gnatocx_site-class.adb

reggnatcom:
	gnatmake ${ADAOPTS} reggnatcom ${POSTOPTS}

register: reggnatcom
	reggnatcom

lock:
	$(LOCKTOOL) *.ali

unlock:
	-$(UNLOCKTOOL) *.ali

.PHONY: html
html:
	-mkdir html
	dir /b gnatcom*.ad? >files.txt
	adabrowse -ipackages.html -psubprogs.html \
		-tindex.html -f@files.txt -ohtml$(DIRSEP)


cleanall: unlock clean
	-$(DELTOOL) files.txt
	-$(DELTOOL) /s /q html
	-$(DELDIRTOOL) html
	-cmd /c "rmdir /s /q html"
