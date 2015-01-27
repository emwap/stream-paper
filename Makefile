paper.pdf: paper.tex
	pdflatex paper.tex
	pdflatex paper.tex

paper.tex: paper.md Makefile
	pandoc paper.md -o paper.tex -s --tab-stop=2
