#!/bin/sh
### update-autoloads.sh --- update loaddefs.el as necessary

set -eu

# This means we're running in a Sun workspace
test -d ../era-specific && cd ../editor

# get to the right directory
test ! -d ./lisp -a -d ../lisp && cd ..
if test ! -d ./lisp ; then
  echo $0: neither ./lisp/ nor ../lisp/ exist
  exit 1
fi

EMACS="./src/xemacs"
echo " (using $EMACS)"

export EMACS

REAL=`cd \`dirname $EMACS\` ; pwd | sed 's|^/tmp_mnt||'`/`basename $EMACS`

echo "Recompiling in `pwd|sed 's|^/tmp_mnt||'`"
echo "          with $REAL..."

$EMACS -batch -q -f batch-update-autoloads \
  `ls lisp | egrep -v \
  "ChangeLog|CVS|COPYING|README|SCCS|egg|eterm|its|mule|paths.el|quail|version.el|vms" \
  | xargs -i echo lisp/\{\}`
