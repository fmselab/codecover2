#!/bin/sh

# $Id: uninstall.sh 1 2007-12-12 17:37:26Z t-scheller $

set -ex

EXEC="$0"
POS="${EXEC%/*}"
cd "$POS"

umask 022

rm -rf /usr/local/share/java/codecover

rm -f /etc/bash_completion.d/codecover

# Has to be in /usr, not /usr/local :-(
rm -f /usr/share/ant/lib/codecover-*.jar

rm -f /usr/local/bin/codecover
