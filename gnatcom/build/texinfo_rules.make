# gnu make rules for processing texinfo files

%.dvi : %.texi
	latex $(TEXI_INCLUDE) --c-style-errors $(TEXI_DVI_OPTS) '\nonstopmode\input{$<}'

%.view : %.dvi
	cmd /c start $<

%.pdf : %.texi %.out %.aux
	pdflatex --c-style-errors $(TEXI_INCLUDE) $(TEXI_PDF_OPTS) '\nonstopmode\input{$<}'

%.out : %.texi
	pdflatex --c-style-errors $(TEXI_INCLUDE) $(TEXI_PDF_OPTS) '\nonstopmode\input{$<}'

%.aux : %.texi
	pdflatex --c-style-errors $(TEXI_INCLUDE) $(TEXI_PDF_OPTS) '\nonstopmode\input{$<}'

%.info : %.texi
	makeinfo $(TEXI_INFO_OPTS) $<

%.html : %.texi
	makeinfo --html --no-split $(TEXI_HTML_OPTS) -o $*.html $<

%.hpj  : %.texi
	makeinfo --hpj-output $*.hpj -o $*.rtf  $(TEXI_HPJ_OPTS) $<

clean ::
	rm -f *.aux *.dvi *.html *.info *.log *.out *.pdf

release-clean ::
	rm -f *.aux *.dvi *.log *.out

maintainer-clean :: clean

# end of file
