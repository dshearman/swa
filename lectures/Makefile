TEX = xelatex --shell-escape
R_PROG := /opt/local/bin/R
R_SCRIPT := /opt/local/bin/Rscript

# Rules for construction
%.md.png:	%.Rmd
	Rscript -e "library(knitr); opts_chunk[['set']](dev = 'png'); knit('$<', output='$*.md')"

%.md.pdf:	%.Rmd
	Rscript -e "library(knitr); opts_chunk[['set']](dev = 'pdf'); knit('$<', output='$*.md')"

%.md:	%.Rmd
	Rscript -e "library(knitr); knit('$<', output='$*.md')"

%.tex:	%.md figure/*
	pandoc -f markdown+grid_tables -t beamer --slide-level=2 $< -o $*.tex

%.web: 	%.md
	pandoc --slide-level=2 -s -c buttondown.css -f markdown -t html $< -o ${OUTPUT_DIR}/$*.html


