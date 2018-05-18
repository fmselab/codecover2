#!/bin/sh

# $Id: codecover.sh 1 2007-12-12 17:37:26Z t-scheller $

EXEC="$0"
POS="${EXEC%/*}"

exec java -Xmx512M -jar "$POS/lib/codecover-batch.jar" "$@"
