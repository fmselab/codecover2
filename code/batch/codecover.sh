#!/bin/sh

# $Id: codecover.sh 1 2007-12-12 17:37:26Z t-scheller $

MAINCLASS="org.codecover.batch.Batch"

EXEC="$0"
POS="${EXEC%/*}"
PAR="$POS/.."

java -classpath "$PAR/model/model.jar:$PAR/metrics/metrics.jar:$PAR/report/report.jar:$PAR/instrumentation/instrumentation.jar:$PAR/utils/utils.jar:$POS/batch.jar" "$MAINCLASS" "$@"
