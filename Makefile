paper.pdf: paper.tex
	pdflatex -shell-escape paper.tex
	pdflatex -shell-escape paper.tex

paper.tex: paper.md Makefile benchmark/benchmark.csv
	pandoc paper.md -o paper.tex -s --tab-stop=2 --template=latex.template --bibliography=feldspar.bib

bench benchmark/benchmark.csv: benchmark/Main.hs
	cabal configure --enable-benchmark
	cabal build benchmark
	@-$(RM) benchmark/benchmark.csv
	./dist/build/benchmark/benchmark --csv=benchmark/benchmark.csv

examples: Code/PaperExamples.hs
	cabal run examples
