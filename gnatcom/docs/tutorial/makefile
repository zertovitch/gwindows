PACKAGES = local inproc remote client
CLEAN_ALSO = gnatclient*.*

include ../../makefile.inc

local: gnatexamplerc.coff
	gnatmake ${ADAOPTS} gnatexample-exe ${POSTOPTS}

inproc: gnatexamplerc.coff
	gnatmake ${ADAOPTS} gnatexample-dll
	gnatdll -n -e GNATExample.def -dgnatexample-dll.dll \
		gnatexample-dll.ali ${POSTOPTS}

remote: gnatexamplerc.coff
	gnatmake ${ADAOPTS} gnatexample-remote ${POSTOPTS}

gnatexamplerc.coff: gnatexamplerc.rc
	windres gnatexamplerc.rc gnatexamplerc.coff

client:
	../../tools/bindcom gnatexample.tlb gnatclient
	gnatmake ${ADAOPTS} client ${POSTOPTS}