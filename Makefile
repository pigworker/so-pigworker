default: so-pigworker.pdf

so-pigworker.pdf: latex so-pigworker.tex
	latex so-pigworker.tex
	pdflatex so-pigworker.tex

latex: made-latex.txt

made-latex.txt: markdown/*.md
	./all-pandoc
	touch made-latex.txt

upload: so-pigworker.pdf
	scp so-pigworker.pdf cafe:WWW/so-pigworker.pdf
