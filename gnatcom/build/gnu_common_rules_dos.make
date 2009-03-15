# gnu make rules common to all compilers; assumes DOS shell

# gnu make deletes intermediate files created by chained rules;
# label the ones we want to keep precious
.PRECIOUS : %.exe %.out

.PHONY : clean force
# Separate from clean, since that will often be added to.
# FIXME: should delete *.diff also; see %.diff below
test-clean ::
	-del /F /Q *.out
	-del /F /Q *.a *.ali *.dep *.exe *.o b~* 

clean :: test-clean

# Don't include source-clean in release-clean, because
# release-clean is often done from several different build
# directories, and we don't want to repeat source-clean.
release-clean :: clean

maintainer-clean :: release-clean

%.out : %.exe ;	$(*F).exe $(RUN_ARGS) > $(*F).out

%.run : %.exe ;	$(*F).exe $(RUN_ARGS)

# FIXME: make.exe from gnat-3.15p produces commands like:
#    comp  ..\..\test/guid_test.good_out guid_test.out > guid_test.diff
# that '/' is a bug.
%.diff : %.good_out %.out ; comp $(COMP_OPT) $^ > $@

# end of file
