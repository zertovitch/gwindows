# gnu make file for debug version using AdaCore public compiler, DOS shell

all : tools tests-out

include ..\gnu_common_rules_dos.make
include ..\gnat_project_rules.make
include ..\gnu_common_targets.make

VPATH := ..\..\test ..\..\tools

ADA_PROJECT_PATH := $(dir $(CURDIR))
export ADA_PROJECT_PATH

GNAT_PROJECT := gnatcom.gpr

#Local Variables:
#eval: (ada-parse-prj-file "gnatcom.prj")
#mode: makefile
#End:
# end of file
