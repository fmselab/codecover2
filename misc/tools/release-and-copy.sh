#!/bin/sh

# $Id: release-and-copy.sh 1 2007-12-12 17:37:26Z t-scheller $

set -e

EXEC="$0"
POS="${EXEC%/*}"

cd "$POS"

if [ "$1" = "" -o "$2" != "" ]; then
    echo "Usage: $0 <target directory>" >&2
    echo "e.g.:" >&2
    echo "$0 /sftp/bruessel/home/export/bruessel/projects/stupro06/abgabe_`date '+%Y-%m-%d'`"
    exit 1
fi

set -x

umask 022

test ! -e "$1"

ant clean jar javadoc

rm -rf tmp
cp -R release tmp

find tmp -name ".svn" -a -type d -print0 | xargs -0 rm -rf

test ! -e "$1"
cp -R tmp "$1"

rm -rf tmp
