# gnu make rules for GNAT, using project files.

# We use GNAT project files; standard settings are in
# 'gnavi_common.gpr'. Objects, executables and test outputs go in the
# directory with the Makefile and project file.

.PHONY : force

# The C compilation results are considered intermediate and deleted,
# which we don't want, so declare them precious.
.SECONDARY : %.o

# Main compile and link rule Note that there is no way to specify the
# project search path on the gnatmake command line; it must be in the
# environment variable ADA_PROJECT_PATH
%.exe : %.adb force; gnatmake -k -C -P$(GNAT_PROJECT) $(GNATMAKE_ARGS) $*

# Compile individual files
%.o : %.adb force; gnatmake -k -C -c -P$(GNAT_PROJECT) $*
%.o : %.ads force; gnatmake -k -C -c -P$(GNAT_PROJECT) $*

# we occasionally link C code with Ada code. Provide the C compile and
# clean rules here, rather than forcing "include gnu_c_rules", since
# that would declare conflicting link rules.
%.o : %.c ; gcc -c $(CFLAGS) $(INCLUDES) $<
%.dep : %.c ; gcc -M $(CFLAGS) $(INCLUDES) $< > $*.dep
lib%.a : ; ar rc lib$*.a $?

# end of file
