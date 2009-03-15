all:
	make -C icg
	make -C gnavi_ide

debug:
	make debug -C icg
	make debug -C gnavi_ide


release:
	make release -C icg
	make release -C gnavi_ide


maxcheck:
	make maxcheck -C icg
	make maxcheck -C gnavi_ide

gnatcheck:
	make gnatcheck -C icg
	make gnatcheck -C gnavi_ide

clean:
	-del *~
	make clean -C icg
	make clean -C gnavi_ide

cleanall:
	-del *~
	make cleanall -C icg
	make cleanall -C gnavi_ide

.PHONY: install
install: all
