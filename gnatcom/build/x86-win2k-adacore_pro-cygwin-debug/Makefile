# gnu make file for debug version using AdaCore Pro compiler, Cygwin shell

all : docs register tools tests 

install :
	cp bindcom.exe $(INSTALL_BIN)

include ../gnu_common_rules_bash.make
include ../gnat_project_rules.make
include ../texinfo_rules.make
include ../gnu_common_targets.make

VPATH += ../../test ../../tools

GNAT_CURDIR := $(shell cygpath -w $(CURDIR))
ADA_PROJECT_PATH := $(dir $(GNAT_CURDIR))
export ADA_PROJECT_PATH

GNAT_PROJECT := gnatcom_test.gpr

#Local Variables:
#eval: (ada-parse-prj-file "gnatcom.prj")
#mode: makefile
#End:
# end of file
