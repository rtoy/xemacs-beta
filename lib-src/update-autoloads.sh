#!/bin/sh
### update-autoloads.sh --- update loaddefs.el as necessary

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

EMACS=./src/xemacs
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

src/xemacs -batch -q -f batch-update-autoloads \
  `ls lisp | egrep -v \
  "COPYING|README|SCCS|egg|eterm|its|mule|paths.el|quail|version.el|vms" \
  | xargs -i echo lisp/\{\}`
