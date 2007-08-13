#!/bin/sh
### update-elc.sh --- recompile all missing or out-or-date .elc files

# Author:	Jamie Zawinski <jwz@lucid.com>
# Maintainer:	Steve Baur <steve@altair.xemacs.org>
# Created:	?
# Version:	1.0
# Keywords:	recompile .el .elc

### Commentary:
##  Recompile all .elc files that need recompilation.  Requires a working
##  version of 'xemacs'.  Correctly handles the case where the .elc files
##  are missing; thus you can execute 'rm lisp/*/*.elc' before running
##  this script.  Run this from the parent directory of 'src', 'lisp',
##  and 'etc'.  (If this is a Sun workspace, you can run it from
##  'era-specific' instead.)

set -e

# This means we're running in a Sun workspace
if [ -d ../era-specific ]; then
  cd ../editor
fi

# get to the right directory
if [ ! -d ./lisp ]; then
  if [ -d ../lisp ]; then
    cd ..
  else
    echo $0: neither ./lisp/ nor ../lisp/ exist
    exit 1
  fi
fi

EMACS="./src/xemacs"
export EMACS

echo " (using $EMACS)"

# fuckin' sysv, man...
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

REAL=`cd \`dirname $EMACS\` ; pwd | sed 's|^/tmp_mnt||'`/`basename $EMACS`
BYTECOMP="$REAL -batch -q -no-site-file "
echo "Recompiling in `pwd|sed 's|^/tmp_mnt||'`"
echo "          with $REAL..."

$EMACS -batch -q -l `pwd`/lisp/prim/cleantree -f batch-remove-old-elc lisp

prune_vc="( -name SCCS -o -name RCS -o -name CVS ) -prune -o"

tmp1=/tmp/rcl1.$$
tmp2=/tmp/rcl2.$$
rm -f $tmp1 $tmp2

# tmp1 is a list of all .el files
# tmp2 is a list of all .elc files
find lisp/. $prune_vc -name '*.el'  -print | sort > $tmp1
find lisp/. $prune_vc -name '*.elc' -print | sed 's/elc$/el/' | sort > $tmp2

echon "Deleting .elc files without .el files... "
# (except for vm/vm.elc)
comm -13 $tmp1 $tmp2 | sed 's/\(.*\)\.el$/echo \1.elc ; rm \1.elc/' | sh
echo done.

# first recompile the byte-compiler, so that the other compiles take place
# with the latest version (assuming we're compiling the lisp dir of the emacs
# we're running, which might not be the case, but often is.)
#
echon "Checking the byte compiler... "
$BYTECOMP -f batch-byte-recompile-directory lisp/bytecomp

# vm is hard, and must be done first ...
#
echon "Compiling VM... "
( cd lisp/vm ; ${MAKE:-make} EMACS=$REAL autoload)
echo done.

echo Compiling files without .elc...

# Isn't it wonderful the number of different ways you can
# iterate over a list of files?

#
# Second compile all files which don't have a .elc version, except for these:
#

NUMTOCOMPILE=20			# compile up to 20 files with each invocation

comm -23 $tmp1 $tmp2 | sed '
\!/,!d
\!/edebug/edebug-test.el$!d
\!/energize/energize-load.el$!d
\!/energize/write-file.el$!d
\!/eos/!d
\!/gnus/!d
\!/efs/!d
\!/ilisp/!d
\!/paths.el$!d
\!/prim/loadup.el$!d
\!/prim/loadup-el.el$!d
\!/prim/update-elc.el$!d
\!/site-start.el$!d
\!/site-load.el$!d
\!/site-init.el$!d
\!/version.el$!d
\!/sunpro/sunpro-load.el$!d
\!/vm/!d
\!/w3/!d
\!/hyperbole/!d
\!/auctex/!d
\!/oobr/!d
\!/egg/!d
\!/its/!d
\!/mule/!d
\!/quail/!d
' | xargs -t -n$NUMTOCOMPILE $BYTECOMP -f batch-byte-compile

rm -f $tmp1 $tmp2
echo Done.

if [ -d lisp/ediff ]; then
  echo Compiling EDIFF...
  ( cd lisp/ediff ; ${MAKE:-make} EMACS=$REAL elc )
  echo EDIFF done.
fi

if [ -d lisp/viper ]; then
  echo Compiling Viper...
  ( cd lisp/viper ; ${MAKE:-make} EMACS=$REAL elc )
  echo Viper done.
fi

if [ -d lisp/efs ]; then
  echo Compiling efs...
  ( cd lisp/efs ; ${MAKE:-make} EMACS=$REAL )
  echo efs done.
fi

# Gnus now has a makefile...
echo Compiling Gnus...
( cd lisp/gnus ; ${MAKE:-make} EMACS=$REAL some )
echo Gnus done.

# and gee w3 has its own makefile as well
# (no especial need to use it, though)
echo Compiling W3...
( cd lisp/w3 ; ${MAKE:-make} EMACS=$REAL )
echo W3 done.

# Hyperbole has to be different as well.  What is it with these big packages?
echo Compiling Hyperbole...
( cd lisp/hyperbole ; ${MAKE:-make} EMACS=$REAL elc )
echo Hyperbole done.

# OO-Browser too
echo Compiling OO-Browser...
( cd lisp/oobr ; ${MAKE:-make} EMACS=$REAL HYPB_ELC='' elc )
echo OO-Browser done.

# this is not strictly necessary but there are some special dependencies
echo Compiling EOS...
( cd lisp/eos ; ${MAKE:-make} -k EMACS=$REAL )
echo EOS done.

# ilisp would seem to take a little extra now as well
# previously this was up top, but it requires that comint.elc exists.

echo Compiling Ilisp...
( cd lisp/ilisp ; ${MAKE:-make} elc -f Makefile EMACS=$REAL )
echo Ilisp done.

# AUC TeX requires special treatment
echo Compiling AUC TeX...
( cd lisp/auctex ; ${MAKE:-make} some -f Makefile EMACS=$REAL )
echo AUC TeX done.
