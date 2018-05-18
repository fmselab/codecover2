#!/bin/sh

# $Id: put-website-to-sourceforge.sh 3 2007-12-12 21:10:39Z kiess $

# This script puts the website and the site.xml to sourceforge.
# Ideally, we would have a script which is run on shell.sourceforge.net and
# which pulls the stuff from the SVN, but until the SVN is at sourceforge,
# we'll have to do it this way.

# This script MUST NOT be run multiple times at once. (Otherwise you'll end up
# with a messed up website).

# Usage: $0 [trunkDirectory]

# To install the needed packages for this script:
# aptitude install wdg-html-validator m4

set -e
set -x

#SSH_OPTIONS="-l codecover -i $HOME/.ssh/id_codecover_sourceforge"
SSH_OPTIONS=""



EXEC="$0"
POS="${EXEC%/*}"

if [ "$#" = "0" ]; then
    DIR="$POS/../.."
else
    if [ "$#" = "1" ]; then
        DIR="$1"
    else
        echo "Usage: $0 [trunkDirectory]" >&2
        echo "trunkDirectory defaults to `readlink -f "$POS/../.."`"
        false
    fi
fi

cd -- "$DIR"

# Test that we're really in trunk

test -d website
test -d code
test -d misc


# create tmp dir
TMPDIR="`mktemp -td codecoverwebsite.XXXXXX`"
delTmpDir() {
    rm -rf -- "$TMPDIR"
}
sigHandler() {
    delTmpDir
    trap - $1
    kill -$1 $$
    false
    kill -9 $$
}
handleSigs() {
    for sig in "$@"; do
        trap "sigHandler $sig" $sig
    done
}
trap delTmpDir EXIT
handleSigs INT QUIT TERM PIPE
chmod 700 -R "$TMPDIR"
# tmp dir created (and will be deleted on exit)

# Ok, copy website/ to the tmpdir. Use svn export to remove .svn dirs and other
# garbage.
# Won't work if we're not in an SVN, doesn't matter.
svn export website "$TMPDIR/website-m4"

mkdir "$TMPDIR/website"

# We've our own site.xml for now (this has to contain absolute paths).
cp misc/tools/sf-site.xml "$TMPDIR/website/site.xml"


cd "$TMPDIR"

# Process website
website-m4/m4.sh website

# Validate website
find website \( \( -name "*.html" -o -name "*.xhtml" \) -type f \) -print0 | xargs -0 validate --

# Clean up permissions (they will end up at sourceforge)
chmod u+rwX -R website
chmod g+rwX -R website
chmod o+rX -R website
chmod o-w -R website

# Create a setup.sh which will be run later on sourceforge in
# /home/groups/c/co/codecover/tmp
cat > setup.sh <<'EOF'
#!/bin/sh
set -ex
umask 002
delalldirs() { # delete all empty directories from . on
    set +x # This would contain the entire directory listings
    while DIRS="`find -type d -empty -mindepth 1`"; test "$DIRS" != ""; do
        find \( -type d -empty -mindepth 1 \) -print0 | xargs -0 rmdir --
    done
    set -x
}
# Delete old .new and .isnew files
cd ../htdocs
find \( -type f -a \( -name "*.new" -o -name "*.isnew" -o -name ".*.new" -o -name ".*.isnew" \) \) -print0 | xargs -0 rm -f --
cd ../tmp
# Move all files into htdocs
cd website
find -type f -print0 | xargs -0 ../move-files.sh
delalldirs
cd ..
rmdir website
# Rename new files
cd ../htdocs
find \( -type f -a \( -name "*.new" -o -name ".*.new" \) \) -print0 | xargs -0 ../tmp/rename-files.sh
# Delete old files
find \( -type f -a ! \( -name "*.isnew" -o -name ".*.isnew" \) \) -print0 | xargs -0 ../tmp/cleanup-old-files.sh
# Delete isnew files
find \( -type f -a \( -name "*.isnew" -o -name ".*.isnew" \) \) -print0 | xargs -0 rm -f --
# Delete old directories
delalldirs
# Clean up permissions
chmod u+rwX -R . 2> /dev/null || :
chmod g+rwX -R . 2> /dev/null || :
chmod o+rX -R . 2> /dev/null || :
chmod o-w -R . 2> /dev/null || :
cd ..
echo "Ok, I (setup.sh) am done."
EOF
chmod 755 setup.sh

cat > move-files.sh <<'EOF'
#!/bin/sh
set -e
#set -x
for file in "$@"; do
    TARGET="/home/groups/c/co/codecover/htdocs/$file"
    TARGET_DIR="${TARGET%/*}"
    mkdir -p -- "$TARGET_DIR"
    test ! -d "$TARGET"
    test ! -e "$TARGET.new"
    mv -- "$file" "$TARGET.new"
done
EOF
chmod 755 move-files.sh

cat > rename-files.sh <<'EOF'
#!/bin/sh
set -e
#set -x
for file in "$@"; do
    TARGET="${file%.new}"
    test "$TARGET" != "$file"
    if [ ! -e "$file" ]; then # new file
        mv -- "$file" "$target"
    else
        if cmp -- "$file" "$TARGET" > /dev/null 2> /dev/null; then
            # The new file is equal to the old one. Don't replace the
            # old one so that Apaches ETag doesn't change.
            rm -f -- "$file"
        else
            mv -- "$file" "$TARGET"
        fi
    fi
    touch "$TARGET.isnew" # To see later that it is a new file
done
EOF
chmod 755 rename-files.sh

cat > cleanup-old-files.sh <<'EOF'
#!/bin/sh
set -e
#set -x
for file in "$@"; do
    if test ! -f "$file.isnew"; then
        rm -f -- "$file"
    fi
done
EOF
chmod 755 cleanup-old-files.sh



# Check, whether we have a .new file. We don't like these.
NEWFILES="`find -name "*.new" -o -name ".*.new"`"
if [ "$NEWFILES" != "" ]; then
    echo "Found *.new files:" >&2
    echo "$NEWFILES" >&2
    false
fi
# Check, whether we have a .isnew file. We don't like these either.
NEWFILES="`find -name "*.isnew" -o -name ".*.isnew"`"
if [ "$NEWFILES" != "" ]; then
    echo "Found *.isnew files:" >&2
    echo "$NEWFILES" >&2
    false
fi

# Pack it.
tar cjf website.tar.bz2 website setup.sh move-files.sh rename-files.sh cleanup-old-files.sh
rm -rf website

# Ok. Next will put it to sourceforge and continue there.
ssh -ax $SSH_OPTIONS -- shell.sourceforge.net '
set -ex

cd /home/groups/c/co/codecover

rm -rf tmp
mkdir tmp
cd tmp
tar xj
./setup.sh < /dev/null
cd ..
rm tmp/*.sh
rmdir tmp
' < website.tar.bz2

echo "Success, we really did it! (would you have believed that?)"
