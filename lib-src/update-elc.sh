#!/bin/sh
# update-elc.sh --- recompile all missing or out-or-date .elc files

# Author:	Jamie Zawinski, Ben Wing, Martin Buchholz
# Maintainer:	Martin Buchholz
# Keywords:	recompile .el .elc

### Commentary:
##  Recompile all .elc files that need recompilation.  Requires a
##  working version of 'xemacs'.  Correctly handles the case where the
##  .elc files are missing; thus you can execute 'rm lisp/*/*.elc'
##  before running this script.  Run this from the parent of the
##  `lisp' directory, or another nearby directory.

set -eu

# Try to find the lisp directory in several places.
# (Sun workspaces have an `editor' directory)
for dir in  .  ..  ../..  editor  ../editor  ; do
  if test -d $dir ; then cd $dir ; break ; fi
done

if test ! -d lisp/. ; then
  echo "$0: Cannot find the \`lisp' directory."
  exit 1
fi


EMACS=${XEMACS:-./src/xemacs}; export EMACS
REAL=`cd \`dirname $EMACS\` ; pwd | sed 's:^/tmp_mnt::'`/`basename $EMACS`
BYTECOMP="$REAL -batch -q -no-site-file -l bytecomp"
echo "Recompiling in `pwd|sed 's:^/tmp_mnt::'`"
echo "    with $REAL..."

prune_vc="( -name SCCS -o -name RCS -o -name CVS ) -prune -o"

# $els  is a list of all .el  files
# $elcs is a list of all .elc files
els=/tmp/rcl1.$$ ; elcs=/tmp/rcl2.$$
rm -f $els $elcs
trap "rm -f $els $elcs" 0 1 2 3 15
find lisp/. $prune_vc -name '*.el'  -print                    | sort > $els
find lisp/. $prune_vc -name '*.elc' -print | sed 's/elc$/el/' | sort > $elcs


echo "Deleting .elc files without .el files..."
comm -13 $els $elcs | sed -e '\!/vm.el!d' -e '\!/w3.el!d' -e 's/el$/elc/' | \
 while read file ; do echo rm "$file" ; rm "$file" ; done
echo "Deleting .elc files without .el files... Done"


# Compute patterns to ignore when searching for files
ignore_dirs="egg its quail"	# ### Not ported yet...

# Only use Mule XEmacs to compile Mule-specific elisp dirs
echo "Checking for Mule support..."
lisp_prog='(when (featurep (quote mule)) (message "yes"))'
if test -z `$REAL -batch -no-site-file -eval "$lisp_prog" 2>&1` ; then
  ignore_dirs="$ignore_dirs mule"
fi

# first recompile the byte-compiler, so that the other compiles take place
# with the latest version (assuming we're compiling the lisp dir of the emacs
# we're running, which might not be the case, but often is.)
echo "Checking the byte compiler..."
$BYTECOMP -f batch-byte-recompile-directory lisp/bytecomp

# Prepare for byte-compiling directories with directory-specific instructions
make_special_commands=''
make_special () {
  dir="$1"; shift;
  ignore_dirs="$ignore_dirs $dir"
  make_special_commands="$make_special_commands \
echo \"Compiling in lisp/$dir\"; \
(cd \"lisp/$dir\"; \
${MAKE:-make} EMACS=$REAL ${1+$*}); \
echo \"lisp/$dir done.\";"
}

make_special vm
#make_special ediff elc
#make_special viper elc
make_special gnus  some
make_special w3
make_special hyperbole elc
make_special oobr HYPB_ELC='' elc
make_special eos -k		# not stricly necessary...
make_special ilisp elc

ignore_pattern=''
for dir in $ignore_dirs ; do
  ignore_pattern="${ignore_pattern}/\\/$dir\\//d
/\\/$dir\$/d
"
done

# Other special-case filenames that don't get byte-compiled
ignore_pattern="$ignore_pattern"'
\!/,!d
\!/edebug/edebug-test.el$!d
\!/emulators/edt.el$!d
\!/energize/energize-load.el$!d
\!/energize/write-file.el$!d
\!/paths.el$!d
\!/prim/loadup.el$!d
\!/prim/loadup-el.el$!d
\!/prim/update-elc.el$!d
\!/site-start.el$!d
\!/site-load.el$!d
\!/site-init.el$!d
\!/version.el$!d
\!/sunpro/sunpro-load.el$!d
'

echo "Compiling files without .elc..."
NUMTOCOMPILE=20			# compile this many files with each invocation
comm -23 $els $elcs | \
 sed "$ignore_pattern" | \
 xargs -t -n$NUMTOCOMPILE $BYTECOMP -f batch-byte-compile
echo "Compiling files without .elc... Done"


echo "Compiling files with out-of-date .elc..."
find lisp/* $prune_vc -type d -print | \
 sed "$ignore_pattern" | \
 xargs -t $BYTECOMP -f batch-byte-recompile-directory
echo "Compiling files with out-of-date .elc... Done"


eval "$make_special_commands"
