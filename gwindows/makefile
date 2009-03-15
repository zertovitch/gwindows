include makefile.inc

all:
	make -C framework
	make -C contrib

debug:
	make debug -C framework
	make debug -C contrib

release:
	make release -C framework
	make release -C contrib

maxcheck:
	make maxcheck -C framework
	make maxcheck -C contrib

gnatcheck:
	make gnatcheck -C framework
	make gnatcheck -C contrib

.PHONY: tutorials
tutorials:
	make -C tutorials

clean:
	-$(DELTOOL) *~
	make clean -C framework
	make clean -C contrib
	make clean -C samples
	make clean -C tools
	make clean -C tutorials

cleanall:
	-$(DELTOOL) *~
	make cleanall -C framework
	make cleanall -C contrib
	make cleanall -C samples
	make clean -C tools
	make clean -C tutorials
	make clean -C test

html: all
	make html -C framework

tools: all
	make -C tools

register: tools
	$(COPYTOOL) tools$(DIRSEP)gnatreg$(DIRSEP)gnatreg.exe \
		framework$(DIRSEP)gnatreg.exe
	copy tools$(DIRSEP)gnatreg$(DIRSEP)gnatreg.exe \
		contrib$(DIRSEP)gnatreg.exe
	make register -C framework
	make register -C contrib

.PHONY: install
install: all tools register
	make lock -C framework
	make lock -C contrib

.PHONY: samples
samples:
	make -C samples

.PHONY: test
test: all
	make -C test
	make test -C test

.PHONY: fulltest
fulltest:
	make test ANSI=1
	make test UNICODE=1