#XELATEX = lualatex --shell-escape
XELATEX = xelatex --shell-escape
PDFLATEX := pdflatex
R_PROG := /usr/bin/R
CODE = 300958
TEMPLATE_DIR = templates
INCLUDE_DIR = includes
LECTURE_DIR = lectures
TUTORIAL_DIR = tutorials
LAB_DIR = labs
PROJECT_DIR = projects
INFORMATION_DIR = information
TEST_DIR = tests
OUTPUT_DIR = bin

ifneq ($(cd ${LECTURE_DIR}/figures; pwd -P),$(cd figures; pwd -P))
    $(error The ${LECTURE_DIR}/figures directory is not a symbolic link of the figures directory!)
endif

ifneq ($(cd ${LAB_DIR}/figures; pwd -P),$(cd figures; pwd -P))
    $(error The ${LAB_DIR}/figures directory is not a symbolic link of the figures directory!)
endif

MAKESTYLE = slides
# NO NEED TO ADD THESE, THEY ARE NOW AUTOMATICALLY GENERATED.
# THESE WILL BE REMOVED SOON.
# Dependancies for lecture tex files (needed if written in markdown or sweave)
#${TEMPLATE_DIR}/week01.tex:	${LECTURE_DIR}/introduction/intro.tex
#${TEMPLATE_DIR}/week02.tex:	${LECTURE_DIR}/Rprogramming/Rprogram.tex ${LECTURE_DIR}/api/api.tex
#${TEMPLATE_DIR}/week03.tex:	${LECTURE_DIR}/exposure/Exposure.tex
#${TEMPLATE_DIR}/week04.tex:	${LECTURE_DIR}/text_analysis/text_index.tex
#${TEMPLATE_DIR}/week05.tex:	${LECTURE_DIR}/graphs/graphs_intro.tex
#${TEMPLATE_DIR}/week06.tex:	${LECTURE_DIR}/visualization/Viz.tex
#${TEMPLATE_DIR}/week07.tex:	${LECTURE_DIR}/text_analysis/clustering.tex
#${TEMPLATE_DIR}/week08.tex:	${LECTURE_DIR}/graphs/link_analysis.tex
#${TEMPLATE_DIR}/week10.tex:	${LECTURE_DIR}/trends/Trend.tex
#${TEMPLATE_DIR}/week11.tex:	${LECTURE_DIR}/events/Events.tex
#${TEMPLATE_DIR}/week12.tex:	${LECTURE_DIR}/sentiment/classification.tex

# Rules for construction
$(LECTURE_DIR)/%.tex: FORCE 
	$(MAKE) -C ${LECTURE_DIR} -f makefile.$(MAKESTYLE) $*.tex	


$(LAB_DIR)/%: FORCE
	$(MAKE) -C $(LAB_DIR) -f makefile.$(MAKESTYLE) $*

$(INFORMATION_DIR)/%: FORCE
	$(MAKE) -C $(INFORMATION_DIR) $*

$(TEMPLATE_DIR)/%: FORCE
	$(MAKE) -C $(TEMPLATE_DIR) $*

%.handout:	${INCLUDE_DIR}/handout.tex ${TEMPLATE_DIR}/%.handout.tex
	${PDFLATEX}  -jobname "${CODE}.$*.handout" -output-directory ${OUTPUT_DIR} "\input{${INCLUDE_DIR}/handout.tex}\input{${TEMPLATE_DIR}/$*.handout.tex}"

%.notes:	MAKESTYLE = notes
%.notes:	${INCLUDE_DIR}/article.tex ${TEMPLATE_DIR}/%.tex
	${XELATEX}  -jobname "${CODE}.$*.notes" -output-directory ${OUTPUT_DIR} "\input{${INCLUDE_DIR}/article.tex}\input{${TEMPLATE_DIR}/$*.tex}"

%.slides:	MAKESTYLE = slides
%.slides:	${INCLUDE_DIR}/beamer.tex ${TEMPLATE_DIR}/%.tex
	${XELATEX} -jobname "${CODE}.$*.slides" -output-directory ${OUTPUT_DIR} "\input{${INCLUDE_DIR}/beamer.tex}\input{${TEMPLATE_DIR}/$*.tex}"

each.notes:	week01.notes week02.notes week03.notes week04.notes week05.notes week06.notes \
	week07.notes week08.notes week10.notes week11.notes week12.notes week13.notes week14.notes

each.slides:	week01.slides week02.slides week03.slides week04.slides week05.slides week06.slides \
	week07.slides week08.slides week10.slides week11.slides week12.slides week13.slides week14.slides


FORCE:

include $(TEMPLATE_DIR)/dependencies.mak
include $(TEMPLATE_DIR)/dependencies.notes.mak

