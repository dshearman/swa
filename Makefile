TEX = xelatex --shell-escape
R_PROG := /opt/local/bin/R
CODE = 300958
TEMPLATE_DIR = templates
INCLUDE_DIR = includes
LECTURE_DIR = lectures
TUTORIAL_DIR = tutorials
PROJECT_DIR = projects
INFORMATION_DIR = information
TEST_DIR = tests
OUTPUT_DIR = bin


COMPILE_SOURCE = README Makefile

TEST_SOURCE = $(addprefix ${TEST_DIR}/, Makefile \
	exam-2010A-deferred.skel exam-2010A-final.skel \
	exam-question-pool.Rtex final-exam-2010-details.tex \
	mid-semester.2010.tex question.pool.m4 test-cover.tex)


PROJECT_SOURCE = $(addprefix ${PROJECT_DIR}/, Makefile \
	credit-sample.csv credit.Rtex crx.data.txt \
	crx.names.txt sample.data.r)


INFORMATION_SOURCE = $(addprefix ${INFORMATION_DIR}/, uws_cymk.pdf \
	Computer_File_008.gif Computer_File_017.gif Computer_File_042.gif \
	Computer_File_056.gif Computer_File_064.gif Computer_File_076.gif \
	Computer_File_077.gif Computer_File_128.gif Die_bone.jpg \
	icon.url learning-guide.tex onlinenetiquette.pdf question-plot-1990.pdf \
	question-plot-1998.pdf unit-outline.tex uws-logo-transparent-small.png \
	uws-logo-transparent.png uws-logo-white.pdf uws-logo.pdf uws-logo.png)


LECTURE_SOURCE = $(addprefix ${LECTURE_DIR}/, week01/week01.beamer.tex \
	week02/week02.beamer.tex \
	week03/week03.beamer.tex \
	week04/week04.beamer.tex \
	week05/week05.beamer.tex \
	week06/week06.beamer.tex \
	week07/week07.beamer.tex \
	week08/week08.beamer.tex \
	week09/week09.beamer.tex \
	week10/week10.beamer.tex \
	week11/week11.beamer.tex \
	week12/week12.beamer.tex \
	week13/week13.beamer.tex)

TEMPLATES = $(addprefix ${TEMPLATE_DIR}/, all.tex week01.tex week06.tex week10.tex \
	 week07.tex week12.tex week11.tex week02.tex \
	 week09.tex week03.tex week04.tex week05.tex week13.tex week14.tex)

INCLUDES = $(addprefix ${INCLUDE_DIR}/, article.tex beamer.tex \
	includes.tex)

IMAGES = $(addprefix ${LECTURE_DIR}/, week1/donor_bar.pdf \
	week1/donor_multibar.pdf \
	week1/donor_pie.pdf \
	week1/milk_polygon.pdf \
	week1/milk_ogive.pdf \
	week1/milk_histogram.pdf \
	week5/normal-plot-prog.pdf \
	week5/statistics-marks.pdf \
	week5/maths-marks.pdf \
	week5/prog-marks.pdf \
	week5/normal-plot-statistics.pdf \
	week5/normal-plot-maths.pdf \
	week6/t-distribution.pdf \
	week11/no-relationship.pdf \
	week11/bird-oxygen.pdf \
	week11/annual-sales.pdf \
	week11/neg-linear-relationship.pdf \
	week11/fitted-annual-sales.pdf \
	week11/linear-relationship.pdf \
	week11/non-linear-relationship.pdf \
	week2/left-skew.pdf \
	week2/mean-variance.pdf \
	week2/symmetric.pdf \
	week2/right-skew.pdf \
	week12/chi2-distribution.pdf)

SCRIPTS = $(addprefix ${LECTURE_DIR}/, week1/blood_donors.r \
	week1/histogram.r \
	week2/distributions.r \
	week5/student-marks.r \
	week6/t-distribution.r \
	week11/fitting-lines.r)


# Dependancies for lecture tex files (needed if written in markdown or sweave)
${TEMPLATE_DIR}/week01.tex:	${LECTURE_DIR}/introduction/intro.tex
${TEMPLATE_DIR}/week02.tex:	${LECTURE_DIR}/Rprogramming/Rprogram.tex ${LECTURE_DIR}/api/api.tex
${TEMPLATE_DIR}/week03.tex:	${LECTURE_DIR}/exposure/Exposure.tex
${TEMPLATE_DIR}/week04.tex:	${LECTURE_DIR}/text_analysis/text_index.tex
${TEMPLATE_DIR}/week05.tex:	${LECTURE_DIR}/graphs/graphs_intro.tex
${TEMPLATE_DIR}/week06.tex:	${LECTURE_DIR}/visualization/Viz.tex
${TEMPLATE_DIR}/week07.tex:	${LECTURE_DIR}/text_analysis/clustering.tex
${TEMPLATE_DIR}/week08.tex:	${LECTURE_DIR}/graphs/link_analysis.tex
${TEMPLATE_DIR}/week10.tex:	${LECTURE_DIR}/trends/Trend.tex

# Rules for construction
%.md.png:	%.Rmd
	Rscript -e "library(knitr); opts_chunk[['set']](dev = 'png'); knit('$<', output='$*.md')"

%.md.pdf:	%.Rmd
	Rscript -e "library(knitr); opts_chunk[['set']](dev = 'pdf'); knit('$<', output='$*.md')"

%.md:	%.Rmd
	Rscript -e "library(knitr); knit('$<', output='$*.md')"

%.yaml: %.Ryaml
	Rscript -e "library(knitr); knit('$<', output='$*.yaml')"

%.Ryaml: %.R
	Rscript --vanilla -e 'source("${TEST_DIR}/rlist.r"); yaml.header(); source("$<"); yaml.footer();' > "$*.Ryaml"


%.tex:	%.md
	pandoc -f markdown+grid_tables -t beamer --slide-level=2 $< -o $*.tex

%.tex:	%.Rtex
	echo "library(tools); Sweave('$<')" | ${R_PROG} --vanilla --silent


%.html: %.md
	pandoc --mathjax --table-of-contents --toc-depth=1 --number-sections --section-divs --slide-level=2 -s -c buttondown.css -f markdown -t html $< -o $*.html

%.web: 	%.md
	pandoc --slide-level=2 -s -c buttondown.css -f markdown -t html $< -o ${OUTPUT_DIR}/$*.html

%.notes:	${INCLUDE_DIR}/article.tex ${TEMPLATE_DIR}/%.tex
	${TEX}  -jobname "${CODE}.$*.notes" -output-directory ${OUTPUT_DIR} "\input{${INCLUDE_DIR}/article.tex}\input{${TEMPLATE_DIR}/$*.tex}"

%.slides:	${INCLUDE_DIR}/beamer.tex ${TEMPLATE_DIR}/%.tex
	${TEX} -jobname "${CODE}.$*.slides" -output-directory ${OUTPUT_DIR} "\input{${INCLUDE_DIR}/beamer.tex}\input{${TEMPLATE_DIR}/$*.tex}"

each.notes:	week01.notes week02.notes week03.notes week04.notes week05.notes week06.notes \
	week07.notes week08.notes week09.notes week10.notes week12.notes

each.slides:	week01.slides week02.slides week03.slides week04.slides week05.slides week06.slides \
	week07.slides week08.slides week09.slides week10.slides week12.slides

%.tute:	522.tutorial.week.tex ${TUTORIAL_SOURCE}
	${TEX} -jobname "$*.tutorial" "\input{${INCLUDE_DIR}/article.tex}\input{${INCLUDE_DIR}/includes.tex}\newcommand{\week}{$*}\input{522.tutorial.week.tex}"


tar:	$(TUTORIAL_SOURCE) $(LECTURE_SOURCE) $(TEMPLATES) $(SCRIPTS) \
	$(INCLUDES) $(COMPILE_SOURCE) $(IMAGES) $(INFORMATION_SOURCE) $(TEST_SOURCE) $(PROJECT_SOURCE)
	tar -cvzf ${CODE}.tgz $(TUTORIAL_SOURCE) $(LECTURE_SOURCE) $(TEMPLATES) $(SCRIPTS) \
	$(INCLUDES) $(COMPILE_SOURCE) $(IMAGES) $(INFORMATION_SOURCE) $(TEST_SOURCE) $(PROJECT_SOURCE)
