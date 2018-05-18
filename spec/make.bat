del *.aux
del *.glg
del *.glo
del *.gls
del *.ist
del *.lof
del *.log
del *.out
del *.pdf
del *.svn
del *.toc
pdflatex -quiet -interaction=nonstopmode Specification
makeindex -s Specification.ist -t Specification.glg -o Specification.gls Specification.glo
pdflatex -quiet -interaction=nonstopmode Specification
pdflatex -quiet -interaction=nonstopmode Specification
pdflatex -quiet -time-statistics -interaction=nonstopmode Specification
PAUSE