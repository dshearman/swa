#R_PROG := /opt/local/bin/R
R_PROG := /usr/bin/R
TEX := xelatex
#R_SCRIPT := /opt/local/bin/Rscript
R_SCRIPT := /usr/bin/Rscript
PANDOC := /opt/local/bin/pandoc
STYLESHEET := buttondown.css

build:	$(PROJ)

bib:	
	bibtex ${PROJ}

%.solutions.pdf: %.tex FORCE 
	${TEX} -jobname "$*.solutions" $<

%.pdf:	%.tex FORCE 
	${TEX} '\PassOptionsToClass{hideanswers}{uwsexamTFS} \input{$<}'

%.AIP.pdf: %.tex FORCE
	${TEX} -jobname "$*.AIP" '\PassOptionsToClass{hideanswers,fontsize=17pt}{uwsexamTFS} \input{$<}'

%.tex:	%.Rtex exam_question.pool.m4 uwsexam.cls
	${R_SCRIPT} -e "library(knitr); knit('$<', output='$*.tex')"

%.R:	%.Rtex
	echo "library(tools); Stangle('$<')" | ${R_PROG} --vanilla --silent 

%.Rtex:	%.skel exam_question.pool.m4 uwsexam.cls
	m4 -P $< > $@

%.md:	%.Rmd
	${R_SCRIPT} -e "library(knitr); knit('$<', output='$*.md')"

%.html: %.md
	${PANDOC} --mathjax \
	 --section-divs --slide-level=2 -s -c ${STYLESHEET} -f markdown -t html \
	$< -o $*.html

.PRECIOUS: %.tex

FORCE:
