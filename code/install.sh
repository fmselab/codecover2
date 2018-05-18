#!/bin/sh

# $Id: install.sh 1 2007-12-12 17:37:26Z t-scheller $

set -ex

EXEC="$0"
POS="${EXEC%/*}"
cd "$POS"

umask 022

./uninstall.sh

mkdir -p /usr/local/share/java/codecover/plugins
cp release/lib/*.jar /usr/local/share/java/codecover
cp release/lib/plugins/*.jar /usr/local/share/java/codecover/plugins

mkdir -p /etc/bash_completion.d
cp release/bash_completion-codecover /etc/bash_completion.d/codecover

# Has to be in /usr, not /usr/local :-(
CODECOVER_ANT_JARS="codecover-core.jar codecover-ant.jar"
CODECOVER_ANT_ADDITIONAL=""
mkdir -p /usr/share/ant/lib
for i in $CODECOVER_ANT_JARS; do
    ln -s "../../../local/share/java/codecover/$i" "/usr/share/ant/lib/$i"
done
for i in $CODECOVER_ANT_ADDITIONAL; do
    ln -s "../../../local/share/java/codecover/$i" "/usr/share/ant/lib/codecover-$i"
done

cat <<'EOF' > /usr/local/bin/codecover
#!/bin/sh

# $Id: install.sh 1 2007-12-12 17:37:26Z t-scheller $

EXEC="$0"
POS="${EXEC%/*}"

exec java -Xmx512M -jar "$POS/../share/java/codecover/codecover-batch.jar" "$@"

EOF
chmod 755 /usr/local/bin/codecover
