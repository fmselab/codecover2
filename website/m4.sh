#!/bin/sh
#
#DESCRIPTION: builds the website for local testing and to prepare deployment

set -e
set -x

EXEC="$0"
POS="${EXEC%/*}"

SOURCE=
TARGET=

if [ "$#" = "0" ]; then
    SOURCE="$POS"
    TARGET="$POS/../website-complete"
else
    if [ "$#" = "1" ]; then
        SOURCE="$POS"
        TARGET="$1"
    else
        if [ "$#" = "2" ]; then
            SOURCE="$1"
            TARGET="$2"
        else
            echo "Usage: $0 [[InputDirectory] OutputDirectory]"
        fi
    fi
fi

SOURCE="`readlink -f \"$SOURCE\"`"
TARGET="`readlink -f \"$TARGET\"`"
POS="`readlink -f \"$POS\"`"

test -d "$TARGET" || mkdir "$TARGET"

cd "$SOURCE"

find -type d -print0 | ( cd "$TARGET"; xargs -0 mkdir -p -- )

find -type f -name "*.m4" ! -name "*.inc.m4" -print0 | xargs -0 "$POS/m4-process-file.sh" "$TARGET"

find -type f ! \( -name "*.m4" -o -path "./m4.sh" -o -path "./m4-process-file.sh" -o -path "./m4-copy-files.sh" \) -print0 | xargs -0 "$POS/m4-copy-files.sh" "$TARGET"
