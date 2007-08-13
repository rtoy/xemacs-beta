#!/bin/sh
### update-elc.sh --- recompile all missing or out-or-date .elc files

# Author:	Jamie Zawinski <jwz@lucid.com>
# Maintainer:	Ben Wing <ben.wing@Eng.Sun.COM>
# Created:	?
# Version:	1.0
# Modified:     94/07/13 16:18:44
# Keywords:	recompile .el .elc

### Commentary:
##  Recompile all .elc files that need recompilation.  Requires a working
##  version of 'xemacs'.  Correctly handles the case where the .elc files
##  are missing; thus you can execute 'rm lisp/*/*.elc' before running
##  this script.  Run this from the parent directory of 'src', 'lisp',
##  and 'etc'.  (If this is a Sun workspace, you can run it from
##  'era-specific' instead.)

set -eu
unset MAKEFLAGS   # GNU make sets MAKEFLAGS to -w; confuses non-GNU make

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

EMACS=${XEMACS:-./src/xemacs}
echo " (using $EMACS)"

export EMACS

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

echo "Recompiling in `pwd|sed 's|^/tmp_mnt||'`"
echo "          with $REAL..."


tmp1=/tmp/rcl1.$$
tmp2=/tmp/rcl2.$$
rm -f $tmp1 $tmp2

# tmp1 is a list of all .el files
# tmp2 is a list of all .elc files
find lisp/. -name SCCS -prune -o -name '*.el'  -print | sort > $tmp1
find lisp/. -name SCCS -prune -o -name 'w3.elc' -prune -o -name 'vm.elc' \
	-prune -o -name '*.elc' -print | sed 's/elc$/el/' | sort > $tmp2

echon "Deleting .elc files without .el files... "
# (except for vm/vm.elc)
comm -13 $tmp1 $tmp2 | sed 's/\(.*\)\.el$/echo \1.elc ; rm \1.elc/' | sh
echo done.


# first recompile the byte-compiler, so that the other compiles take place
# with the latest version (assuming we're compiling the lisp dir of the emacs
# we're running, which might not be the case, but often is.)
#
echon "Checking the byte compiler... "
$REAL -batch -q -no-site-file -f batch-byte-recompile-directory lisp/bytecomp

echo Compiling files without .elc...

# Isn't it wonderful the number of different ways you can
# iterate over a list of files?

#
# First compile all files which don't have a .elc version, except for these:
#

NUMTOCOMPILE=20			# compile up to 20 files with each invocation

comm -23 $tmp1 $tmp2 | sed '
\!/,!d
\!/edebug/edebug-test.el$!d
\!/emulators/edt.el$!d
\!/energize/energize-load.el$!d
\!/energize/write-file.el$!d
\!/eos/!d
\!/gnus/!d
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
\!/tm/!d
\!/tl/!d
\!/mel/!d
\!/url/!d
\!/viper/!d
\!/vm/!d
\!/w3/!d
\!/hyperbole/!d
\!/oobr/!d
\!/ediff/!d
\!/egg/!d
\!/its/!d
\!/mule/!d
\!/quail/!d
' | xargs -t -n$NUMTOCOMPILE $REAL -batch -q -no-site-file -f batch-byte-compile

rm -f $tmp1 $tmp2
echo Done.

# vm is hard...
#
echon "Compiling VM... "
( cd lisp/vm ; make EMACS=$REAL )
echo done.

if [ -d lisp/ediff ]; then
  echo Compiling EDIFF...
  ( cd lisp/ediff ; make EMACS=$REAL elc )
  echo EDIFF done.
fi

if [ -d lisp/viper ]; then
  echo Compiling Viper...
  ( cd lisp/viper ; make EMACS=$REAL elc )
  echo Viper done.
fi

# Gnus now has a makefile...
echo Compiling Gnus...
( cd lisp/gnus ; make EMACS=$REAL some )
echo Gnus done.

# This is really part of w3.
echo Compiling URL...
( cd lisp/url ; make EMACS=$REAL )
echo URL done.

# and gee w3 has its own makefile as well
# (no especial need to use it, though)
echo Compiling W3...
( cd lisp/w3 ; make EMACS=$REAL )
echo W3 done.

# Hyperbole has to be different as well.  What is it with these big packages?
echo Compiling Hyperbole...
( cd lisp/hyperbole ; make EMACS=$REAL elc )
echo Hyperbole done.

# OO-Browser too
echo Compiling OO-Browser...
( cd lisp/oobr ; make EMACS=$REAL HYPB_ELC= elc )
echo OO-Browser done.

# this is not strictly necessary but there are some special dependencies
echo Compiling EOS...
( cd lisp/eos ; make -k EMACS=$REAL )
echo EOS done.

# ilisp would seem to take a little extra now as well
# previously this was up top, but it requires that comint.elc exists.

echo Compiling Ilisp...
( cd lisp/ilisp ; make compile -f Makefile EMACS=$REAL )
echo Ilisp done.

#
# Now get the files whose .el is newer than .elc
#
echo Compiling files with out-of-date .elc...
$REAL -batch -q -no-site-file -f batch-byte-recompile-directory lisp
