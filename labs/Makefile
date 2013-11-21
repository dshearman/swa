# Makefile for lab notes
# To generate lab notes, type make and the name of the html notes.

R_PROG := /opt/local/bin/R
R_SCRIPT := /opt/local/bin/Rscript
PANDOC := /opt/local/bin/pandoc
STYLESHEET := buttondown.css

%.md:	%.Rmd
	${R_SCRIPT} -e "library(knitr); knit('$<', output='$*.md')"

%.html: %.md
	${PANDOC} --mathjax --table-of-contents --toc-depth=1 --number-sections \
	 --section-divs --slide-level=2 -s -c ${STYLESHEET} -f markdown -t html \
	$< -o $*.html

%.tex:  %.md figure/*
	pandoc -f markdown+grid_tables -t latex $< -o $*.tex
