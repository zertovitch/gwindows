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

cleanall:
	-del *~
	make cleanall -C src

.PHONY: install
install: all
