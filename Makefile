these.pdf: *.tex *.cls *.sty pre/*.tex trans/*.tex artin/*.tex isogeny/*.tex defeo.bib
	pdflatex "\def\options{$(THESEOPT)}\input{these}";
	bibtex these;
	makeindex these;
	makeindex these.symb.glo -s symb.gst -o these.symb.gls;
	pdflatex "\def\options{$(THESEOPT)}\input{these}";
	pdflatex "\def\options{$(THESEOPT)}\input{these}";

defeo.bib:
	wget -O defeo.bib "http://www.citeulike.org/bibtex/user/defeo?key_type=0&clean_urls=0";
	echo "%%% Local Variables:" >> defeo.bib;
	echo "%%% mode:auto-revert" >> defeo.bib;
	echo "%%% End:" >> defeo.bib;
