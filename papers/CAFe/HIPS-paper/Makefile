all:
	pdflatex cafe.tex

	bibtex cafe
	pdflatex cafe.tex
	pdflatex cafe.tex

check: all
	open cafe.pdf

clean:
	rm -f *~ *.bbl *.blg *.log *.aux cafe.pdf

