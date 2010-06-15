these.pdf: *.tex trans/*.tex artin/*.tex isogeny/*.tex
	pdflatex these; bibtex these; makeindex these; pdflatex these; pdflatex these
