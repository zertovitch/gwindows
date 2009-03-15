# gnu make file for debug version using AdaCore Pro compiler, Cygwin shell

all : tests

#../../test/test_one_harness.adb
#t_one : RUN_ARGS := 2
t_one : test_one_harness.run

t_all : test_all_harness.diff

trace :
	addr2line -e bug_dock_packing_boxes_harness.exe 0x494f5b 0x46118e 0x77d48732 0x77d48814 0x77d4b4be 0x77d4b50a 0x7c90eae1 0x77d4b3f7 0x77d4b391 0x77d48732 0x77d48814 0x77d4c63d 0x77d4c663 0x460174 0x477928 0x4610a6 0x77d48732 0x77d48814 0x77d4b899 0x77d4b901 0x45e83f 0x45af08 0x499433 0x4993e7

include ../../../gnatcom/build/gnu_common_rules_bash.make
include ../../../gnatcom/build/gnat_project_rules.make
include ../gnu_common_targets.make

VPATH := ../../samples/simple ../../test ../../tools/gbmanager ../../tools/gnatreg

GNAVI_DIR := $(shell cygpath -m $(CURDIR))
GNAVI_DIR := $(dir $(GNAVI_DIR))
GNAVI_DIR := $(subst gwindows/build/,,$(GNAVI_DIR))

ADA_PROJECT_PATH := $(GNAVI_DIR)/gwindows/build
ADA_PROJECT_PATH := $(ADA_PROJECT_PATH);$(GNAVI_DIR)/third_party/aunit/build/x86-win2k-adacore_pro-cygwin-debug
ADA_PROJECT_PATH := $(ADA_PROJECT_PATH);$(GNAVI_DIR)/gnatcom/build
ADA_PROJECT_PATH := $(ADA_PROJECT_PATH);$(GNAVI_DIR)/gnatcom/build/x86-win2k-adacore_pro-cygwin-debug
export ADA_PROJECT_PATH

GNAT_PROJECT := gwindows_test.gpr

# useful when changing compiler versions
recursive-clean :
	gnatclean -r -q -P$(GNAT_PROJECT)

#Local Variables:
#eval: (ada-parse-prj-file "gwindows.prj")
#mode: makefile
# eval: (setq ada-gnatstub-opts "-q -hs -t -I${src_dir} -gnatecc:/Projects/gnavi/gwindows/build/x86-win2k-adacore_pro-cygwin-debug/gnatstub.conf")
#End:
# end of file
