all:
	pdflatex local.tex
	bibtex local
	pdflatex local.tex
	pdflatex local.tex

clean:
	rm -f *~ *.bbl *.blg *.log *.aux local.pdf

