all:
	make -C COMObject
	make -C client
	make -C tutorial

clean:
	-del *.info
	-del *.rtf
	-del *~
	-del *.hpj
	-del *.hlp
	-del *.html
	make clean -C client
	make clean -C COMObject
	make clean -C tutorial

