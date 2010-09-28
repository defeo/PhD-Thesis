these.pdf: *.tex *.cls pre/*.tex trans/*.tex artin/*.tex isogeny/*.tex *.bib
	pdflatex these;
	bibtex these;
	makeindex these;
	makeindex these.nlo -s nomencl.ist -o these.nls;
	pdflatex these; pdflatex these
