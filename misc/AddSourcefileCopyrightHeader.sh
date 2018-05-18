#!/bin/sh

set -e
set -x

test -z "$2"
test -f "$1"

MyPath="${0%/*}"
HeaderFile="$MyPath/SourcefileCopyrightHeader"

test -f "$HeaderFile"

cat "$HeaderFile" "$1" > "$1.new"
mv "$1.new" "$1"
