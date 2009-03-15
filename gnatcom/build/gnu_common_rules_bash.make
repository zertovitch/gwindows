# gnu make rules common to all compilers; assumes bash shell

# some defaults for user options

DIFF_OPT := -u -w

# gnu make deletes intermediate files created by chained rules;
# label the ones we want to keep precious
.PRECIOUS : %.exe %.out

.PHONY : clean force

# Separate from clean, since that will often be added to.
test-clean ::
	rm -f *.out *.diff

clean :: test-clean
	rm -f *.a *.ali *.coff *.dep *.exe *.o b~* 

source-clean ::
	-find $(SOURCE_ROOT) -name "*~" -print | xargs rm -v
	-find $(SOURCE_ROOT) -name ".#*" -print | xargs rm -v
	-find $(SOURCE_ROOT) -name "*,t" -print | xargs rm -v

# Don't include source-clean in release-clean, because
# release-clean is often done from several different build
# directories, and we don't want to repeat source-clean.
release-clean :: clean

maintainer-clean :: release-clean

%.out : %.exe ;	./$(*F).exe $(RUN_ARGS) > $(*F).out

%.run : %.exe ;	./$(*F).exe $(RUN_ARGS)

%.diff : %.good_out %.out ; diff $(DIFF_OPT) $^ > $@

# end of file
