#!/bin/sh

set -e
set -x

DOCUMENT="DetailedDesign"
MAXTRIES=10
CALL_MAKEINDEX=0

cd "`dirname \"$0\"`"

rm -f *.aux "$DOCUMENT".glg "$DOCUMENT".glo "$DOCUMENT".gls "$DOCUMENT".ist "$DOCUMENT".lof "$DOCUMENT".log "$DOCUMENT".out "$DOCUMENT".pdf "$DOCUMENT".svn "$DOCUMENT".toc

COUNT=0

CallLaTeX() {
    pdflatex -quiet -interaction=errorstopmode -file-line-error -halt-on-error "$DOCUMENT"
}

CallLaTeX
NEED_CALL_LATEX=0
if [ "$CALL_MAKEINDEX" = "1" ]; then
    makeindex -s "$DOCUMENT".ist -t "$DOCUMENT".glg -o "$DOCUMENT".gls "$DOCUMENT".glo
    NEED_CALL_LATEX=1
fi
if [ "$NEED_CALL_LATEX" = "1" ]; then
    CallLaTeX
fi
while grep -q 'Label.s. may have changed' "$DOCUMENT.log"; do
    COUNT=$(($COUNT + 1))
    if [ "$COUNT" -gt "$MAXTRIES" ]; then
	echo "Labels are still incorrect after $MAXTRIES executions."
	false
    fi
    CallLaTeX
done

echo "Compiled after $COUNT executions."

#if grep 'LaTeX Warning: Label .* multiply defined.' "$DOCUMENT.log"; then
## Doesn't work because LaTeX breaks lines in its logs

if grep -q 'LaTeX Warning: There were multiply-defined labels.' "$DOCUMENT.log"; then
    grep -A 2 'LaTeX Warning: Label .*' "$DOCUMENT.log"
    echo 'ERROR: Label(s) multiply defined; labels may (and probably will) be incorrect'
    false
fi

echo 'Success.'
