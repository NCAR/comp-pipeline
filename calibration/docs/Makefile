.PHONY: all clean

LATEX=pdflatex

all: writeup.pdf

writeup.pdf: writeup.tex
	$(LATEX) writeup.tex
	$(LATEX) writeup.tex
	$(LATEX) writeup.tex

clean:
	rm -f writeup.{pdf,aux,log}
