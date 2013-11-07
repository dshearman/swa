TEX = xelatex --shell-escape
R_PROG := /opt/local/bin/R
R_SCRIPT := /opt/local/bin/Rscript

all: learning-guide.pdf

# Rules for construction
%.pdf:  %.tex
	$(TEX) $*
