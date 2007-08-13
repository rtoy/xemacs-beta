#!/bin/sh
# update-elc.sh --- recompile all missing or out-of-date .elc files

# Author:	Jamie Zawinski, Ben Wing, Martin Buchholz
# Maintainer:	Martin Buchholz
# Keywords:	recompile byte-compile .el .elc

### Commentary:
##  Recompile all .elc files that need recompilation.  Requires a
##  working version of "xemacs".  Correctly handles the case where the
##  .elc files are missing; thus you can execute "rm lisp/*/*.elc"
##  before running this script.  Run this from the parent of the
##  "lisp" directory, or another nearby directory.

set -e

# Try to find the lisp directory in several places.
# (Sun workspaces have an "editor" directory)
for dir in  .  ..  ../..  editor  ../editor  ; do
  if test -d $dir/lisp/. ; then cd $dir ; break ; fi
done

if test ! -d lisp/. ; then
  echo "$0: Cannot find the \"lisp\" directory."
  exit 1
fi

# Determine xemacs executable to use for compilation.
if test -n "$XEMACS" ; then
  EMACS="$XEMACS"
elif test -x ./src/xemacs ; then
  EMACS="./src/xemacs"
elif test -x "$EMACS" ; then
  :
else
  EMACS=xemacs
fi
case "$EMACS" in
  */* ) : ;; # Pathname specified
  *) # Need to find executable on PATH
     for dir in `echo $PATH | sed 's/:/ /g'` ; do
       if test -x "dir/xemacs" ; then
	 EMACS="$dir/$EMACS"
	 break
       fi
     done ;;
esac
# Canonicalize
EMACS=`cd \`dirname $EMACS\` ; pwd | sed 's:^/tmp_mnt::'`/`basename $EMACS`
export EMACS

echo "Recompiling in `pwd|sed 's:^/tmp_mnt::'`"
echo "    with $EMACS..."

prune_vc="( -name SCCS -o -name RCS -o -name CVS ) -prune -o"

$EMACS -batch -q -l `pwd`/lisp/prim/cleantree -f batch-remove-old-elc lisp

# $els  is a list of all .el  files
# $elcs is a list of all .elc files
els=/tmp/update-elc-1.$$ elcs=/tmp/update-elc-2.$$
rm -f $els $elcs
trap "rm -f $els $elcs" 0 1 2 3 15
find lisp/. $prune_vc -name '*.el'  -print                    | sort > $els
find lisp/. $prune_vc -name '*.elc' -print | sed 's/elc$/el/' | sort > $elcs


echo "Deleting .elc files without .el files..."
comm -13 $els $elcs | sed -e '\!/vm.el!d' -e 's/el$/elc/' | \
 while read file ; do echo rm "$file" ; rm "$file" ; done
echo "Deleting .elc files without .el files... Done"


# Compute patterns to ignore when searching for files
ignore_dirs="its quail"	# ### Not ported yet...

# Only use Mule XEmacs to compile Mule-specific elisp dirs
echo "Checking for Mule support..."
lisp_prog='(princ (featurep (quote mule)))'
mule_p="`$EMACS -batch -no-site-file -eval \"$lisp_prog\"`"
if test "$mule_p" = nil ; then
  echo No
  ignore_dirs="$ignore_dirs mule"
elif test "$mule_p" = t; then
  echo Yes
else
  echo "Error determining presence of mule support"
  exit 1;
fi

# first recompile the byte-compiler, so that the other compiles take place
# with the latest version (assuming we're compiling the lisp dir of the emacs
# we're running, which might not be the case, but often is.)
echo "Checking the byte compiler..."
BYTECOMP="$EMACS -batch -q -no-site-file -l bytecomp"
$BYTECOMP -f batch-byte-recompile-directory lisp/bytecomp

# Byte-compile VM first, because other packages depend on it,
# but it depends on nothing (Kyle is like that).
ignore_dirs="$ignore_dirs vm"
echo "Compiling in lisp/vm";
(cd lisp/vm && ${MAKE:-make} EMACS=$EMACS autoload)
echo "lisp/vm done."

# Prepare for byte-compiling directories with directory-specific instructions
make_special_commands=''
make_special () {
  dir="$1"; shift;
  ignore_dirs="$ignore_dirs $dir"
  make_special_commands="$make_special_commands \
echo \"Compiling in lisp/$dir\"; \
(cd \"lisp/$dir\" && ${MAKE:-make} EMACS=$EMACS ${1+$*}); \
echo \"lisp/$dir done.\";"
}

#make_special vm
#make_special ediff elc
#make_special viper elc
make_special efs
make_special gnus  some
make_special w3
make_special hyperbole elc
make_special oobr HYPB_ELC='' elc
make_special eos -k		# not stricly necessary...
make_special ilisp elc
make_special auctex some

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

eval "$make_special_commands"
