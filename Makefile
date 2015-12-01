# Compile bibtex enough times, and ensure that we have the correct amount of compilations
#

NAME=general_outline

all: tex bib tex_final

bib: $(NAME).bib
	bibtex $(NAME)
	bibtex $(NAME)

tex: $(NAME).tex
	pdflatex $(NAME).tex

tex_final: $(NAME).tex
	pdflatex $(NAME).tex
	pdflatex $(NAME).tex

