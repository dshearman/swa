TEX = xelatex --shell-escape
R_PROG := /usr/bin/R
R_SCRIPT := /usr/bin/Rscript
CODE = 300958
TEMPLATE_DIR = templates
INCLUDE_DIR = includes
LECTURE_DIR = lectures
TUTORIAL_DIR = tutorials
PROJECT_DIR = projects
INFORMATION_DIR = information
TEST_DIR = tests
OUTPUT_DIR = bin
YAMLTOCSV := python ./yamltocsv.py


# Rules for construction

%.yaml: %.Ryaml
	${R_SCRIPT} -e "library(knitr); knit('$<', output='$*.yaml')"

%.Ryaml: %.R
	${R_SCRIPT} --vanilla -e 'source("rlist.r"); yaml.header(); source("$<"); yaml.footer();' > "$*.Ryaml"

%.csv: %.yaml
	${YAMLTOCSV} $< > $*.csv

%.txt: %.yaml
	${YAMLTOCSV} $< > $*.txt

