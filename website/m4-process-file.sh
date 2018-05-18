#!/bin/sh

set -e
#set -x

DEST="$1"
shift

for file in "$@"; do
    DESTINATION_FILE="$DEST/${file%.m4}"
    NEW_DESTINATION_FILE="$DESTINATION_FILE.new"
    PLAIN_FILE_NAME="${file%.m4}"
    PLAIN_FILE_NAME="${PLAIN_FILE_NAME#./}"
    PLAIN_DIRECTORY_NAME="${file%/*}"
    ROOT_DIR="`echo -n \"$PLAIN_DIRECTORY_NAME\" | sed 's,[^/][^/]*,..,g'`"
    if [ "$ROOT_DIR" = ".." ]; then
        ROOT_DIR=.
    else
        ROOT_DIR="${ROOT_DIR%/..}"
    fi
    rm -f -- "$NEW_DESTINATION_FILE"
    m4 -P -I . -D m4_web_pagename="$PLAIN_FILE_NAME" -D m4_web_rootdir="$ROOT_DIR" -- "$file" > "$NEW_DESTINATION_FILE" || { rm -f -- "$NEW_DESTINATION_FILE"; exit 1; }
    mv -- "$NEW_DESTINATION_FILE" "$DESTINATION_FILE" || { rm -f -- "$NEW_DESTINATION_FILE"; exit 1; }
done
