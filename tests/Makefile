#PROJ := final-exam-2010
PROJ := exam-2010A-final.pdf exam-2010A-deferred.pdf
DOC := 	${PROJ}.Rtex
OUT := 	${PROJ}.pdf
R_PROG := /usr/bin/R
TEX := xelatex

build:	$(PROJ)

bib:	
	bibtex ${PROJ}

%.pdf:	%.tex
	xelatex $<

%.tex:	%.Rtex exam_question.pool.m4 uwsexam.cls
	echo "library(tools); Sweave('$<')" | ${R_PROG} --vanilla --silent

%.R:	%.Rtex
	echo "library(tools); Stangle('$<')" | ${R_PROG} --vanilla --silent 

%.Rtex:	%.skel
	m4 -P $< > $@
