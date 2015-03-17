paper.pdf: paper.tex
	pdflatex -shell-escape paper.tex
	pdflatex -shell-escape paper.tex

paper.tex: paper.md Makefile
	pandoc paper.md -o paper.tex -s --tab-stop=2 --template=latex.template --bibliography=feldspar.bib
