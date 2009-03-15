# gnu makefile fragment for common targets

tools : gbmanager.exe
tools : gnatreg.exe

%.coff : %.rc
	windres $< $@

gbmanager.exe : gbmanager.o gbmanager.coff

gnatreg.exe : gnatreg.o gnatreg.coff

tests : test_all_harness.diff
tests : test_gstrings.diff
tests : test_errors.diff
tests : test_application.diff
# tests : test_resources.diff # need shorter runtime, better output.

test_application.exe : test_application.o test_application.coff

form_example.exe : form_example.o form_example.coff

# end of file
