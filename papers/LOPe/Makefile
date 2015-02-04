all:
	pdflatex local.tex

	bibtex local
	pdflatex local.tex
	pdflatex local.tex

check: all
	open local.pdf

clean:
	rm -f *~ *.bbl *.blg *.log *.aux local.pdf

