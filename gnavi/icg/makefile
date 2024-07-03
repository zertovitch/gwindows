all:
	make -C src

debug:
	make debug -C src


release:
	make release -C src


maxcheck:
	make maxcheck -C src

gnatcheck:
	make gnatcheck -C src

clean:
	-del *~
	make clean -C src
	gnatclean -P icg.gpr

cleanall:
	-del *~
	make cleanall -C src
	gnatclean -r -P icg.gpr

.PHONY: install
install: all
