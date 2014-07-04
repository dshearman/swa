# Make dependencies for lecture slides.

dependencies.mak: *.tex
	grep -v ^% *.tex | grep input | sed -e 's/\\input{/ /' -e 's/}\s*//' -e 's/^/templates\//' > dependencies.mak

dependencies.notes.mak: *.tex
	grep -v ^% *.tex | grep input | sed -e 's/\\input{/ /' -e 's/}\s*//' -e 's/^/templates\//' -e 's/\.tex/.notes.tex/g' > dependencies.notes.mak
