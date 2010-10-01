these.pdf: *.tex *.cls pre/*.tex trans/*.tex artin/*.tex isogeny/*.tex *.bib
	pdflatex these;
	bibtex these;
	makeindex these;
	makeindex these.symb.glo -s symb.gst -o these.symb.gls;
	pdflatex these; pdflatex these
