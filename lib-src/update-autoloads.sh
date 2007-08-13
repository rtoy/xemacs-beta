#!/bin/sh
### update-autoloads.sh --- update auto-autoloads.el as necessary

# Author: Jamie Zawinski, Ben Wing, Martin Buchholz, Steve Baur
# Maintainer: Steve Baur
# Keywords: internal

### This file is part of XEmacs

### Commentary:

### Code:

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

echo "Rebuilding autoloads/custom-loads in `pwd|sed 's|^/tmp_mnt||'`"
echo "          with $REAL..."

if [ "`uname -r | sed 's/\(.\).*/\1/'`" -gt 4 ]; then
  echon()
  {    
    /bin/echo $* '\c'
  }
else
  echon()
  {
    echo -n $*
  }
fi

# Compute patterns to ignore when searching for files
ignore_dirs=""

# Only use Mule XEmacs to build Mule-specific autoloads & custom-loads.
echon "Checking for Mule support..."
lisp_prog='(princ (featurep (quote mule)))'
mule_p="`$EMACS -batch -no-site-file -eval \"$lisp_prog\"`"
if test "$mule_p" = nil ; then
	echo No
	ignore_dirs="$ignore_dirs its egg mule leim"
else
	echo Yes
fi

dirs=
for dir in lisp/*; do
	if test -d $dir \
		-a $dir != lisp/CVS \
		-a $dir != lisp/SCCS \
		-a $dir != lisp/language; then
		for ignore in $ignore_dirs; do
			if test $dir = lisp/$ignore; then
				continue 2
			fi
		done
		dirs="$dirs $dir"
	fi
done

set -x
for dir in $dirs; do
	$EMACS -batch -q -l autoload -f batch-update-directory $dir
done
