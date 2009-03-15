PACKAGES = nt_gnatfind spin javacall ado
SUB_PACKAGES = dispatch dynamic vb GIT

ADAOPTS = -Ibindings/

include ../makefile.inc

nt_gnatfind: nt_gnatfind.coff
	gnatmake ${ADAOPTS} nt_gnatfind ${POSTOPTS}

spin: spin.coff
	gnatmake ${ADAOPTS} spin ${POSTOPTS}

javacall:
	gnatmake ${ADAOPTS} javacall ${POSTOPTS}

ado: ado1 ado2 ado3

ado1:
	gnatmake ${ADAOPTS} ado1 ${POSTOPTS}

ado2:
	gnatmake ${ADAOPTS} ado2 ${POSTOPTS}

ado3:
	gnatmake ${ADAOPTS} ado3 ${POSTOPTS}

msmq: msmq_client msmq_server

msmq_client:
	gnatmake ${ADAOPTS} msmq_client ${POSTOPTS}

msmq_server:
	gnatmake ${ADAOPTS} msmq_server ${POSTOPTS}

nt_gnatfind.coff: nt_gnatfind.rc res/nt_gnatfind.html
	windres nt_gnatfind.rc nt_gnatfind.coff

spin.coff: spin.rc res/spin.html
	windres spin.rc spin.coff
