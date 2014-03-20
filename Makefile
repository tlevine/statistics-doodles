.PHONY: doodles.pdf formulae.pdf

doodles.pdf:
	Rscript doodles.r

formulae.pdf:
	pdflatex formulae.tex
