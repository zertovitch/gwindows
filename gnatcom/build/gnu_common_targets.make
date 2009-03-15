# common targets for gnu makefiles

tools : bindcom.exe
tools : comscope.exe
tools : createcom.exe
tools : makeguid.exe

docs : com_with_gnat.html
docs : gnatcom.html

VPATH := ../../docs

TESTS := bstr_test.diff
TESTS += guid_test.diff
TESTS += initialize_test.diff
TESTS += safearray_test.diff
TESTS += variant_test.diff

tests : $(TESTS)

# FIXME: work-around for gnat-3.15p make (see gnu_common_rules_dos.make %.diff rule)
TESTS_OUT := $(subst .diff,.out, $(TESTS))

tests-out : $(TESTS_OUT)
# end of file
