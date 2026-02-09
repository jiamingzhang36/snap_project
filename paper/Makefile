# Compile paper and slides from project root: make -C paper
# Or from paper/: make

LATEX = pdflatex -file-line-error -interaction=nonstopmode

all: paper.pdf slides.pdf

paper.pdf: paper.tex
	$(LATEX) paper
	$(LATEX) paper

slides.pdf: slides.tex
	$(LATEX) slides
	$(LATEX) slides

clean:
	rm -f *.aux *.log *.out *.bbl *.blg *.nav *.snm *.toc *.vrb
