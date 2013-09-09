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
YAMLTOCSV := python ./yamltocsv.py


# Rules for construction

%.yaml: %.Ryaml
	Rscript -e "library(knitr); knit('$<', output='$*.yaml')"

%.Ryaml: %.R
	Rscript --vanilla -e 'source("rlist.r"); yaml.header(); source("$<"); yaml.footer();' > "$*.Ryaml"

%.csv: %.yaml
	${YAMLTOCSV} $< > $*.csv

