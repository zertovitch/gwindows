include makefile.inc

all:
	make -C framework
	make register -C framework
	make -C tools

debug:
	make debug -C framework
	make register -C framework
	make debug -C tools

release:
	make release -C framework
	make release -C tools

maxcheck:
	make maxcheck -C framework
	make maxcheck -C tools

gnatcheck:
	make gnatcheck -C framework
	make gnatcheck -C tools

clean:
	-$(DELTOOL) *~
	make clean -C framework
	make clean -C tools
	make clean -C samples

cleanall: clean
	make cleanall -C framework
	make clean -C samples$(DIRSEP)framework
	make clean -C docs
	cd test & call clean.bat

html: all
	make html -C framework

.PHONY: docs
docs:
	cd docs & call make-docs.bat

.PHONY: install
install: all
	make lock -C framework

.PHONY: samples
samples:
	make -C samples$(DIRSEP)framework
	make -C samples

.PHONY: test
test: all
	cd test & call test.bat