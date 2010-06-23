these.pdf: *.tex pre/*.tex trans/*.tex artin/*.tex isogeny/*.tex *.bib
	pdflatex these; bibtex these; makeindex these; pdflatex these; pdflatex these
