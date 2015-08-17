# Makefile for lab notes
# To generate lab notes, type make and the name of the html notes.

R_PROG := /usr/bin/R
R_SCRIPT := /usr/bin/Rscript
#PANDOC := pandoc -f markdown+grid_tables
PANDOC := pandoc -f markdown
STYLESHEET := buttondown.css

%.md:	%.Rmd
	${R_SCRIPT} -e "library(knitr); knit('$<', output='$*.md')"

%.html: %.md
	${PANDOC} --mathjax --table-of-contents --toc-depth=1 --number-sections \
	 --section-divs --slide-level=2 -s -c ${STYLESHEET} -f markdown -t html \
	$< -o $*.html

%.tex:  %.md 
	${PANDOC} -t latex $< -o $*.tex
