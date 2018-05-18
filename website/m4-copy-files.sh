#!/bin/sh

set -e

DEST="$1"
shift

for file in "$@"; do
    D="$DEST/${file%.m4}"
    DN="$D.new"
    rm -f -- "$DN"
    cp -- "$file" "$DN" || { rm -f -- "$DN"; exit 1; }
    mv -- "$DN" "$D" || { rm -f -- "$DN"; exit 1; }
done
