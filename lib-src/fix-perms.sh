#!/bin/sh
### fix-perms.sh --- Correct the permissions on XEmacs source/build files

# Copyright (C) 2010 Ben Wing.

# Author: Ben Wing
# Keywords: internal

# This file is part of XEmacs.

# XEmacs is free software; you can redistribute it and/or modify it
# under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2, or (at your option)
# any later version.

# XEmacs is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
# General Public License for more details.

# You should have received a copy of the GNU General Public License
# along with XEmacs; see the file COPYING.  If not, write to
# the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
# Boston, MA 02111-1307, USA.

### Commentary:

# This program sets the executable bit on all scripts and executable files
# in the XEmacs source tree, including those that are built.

### Code:

# List of executable source files in various directories (root, lib-src, etc)
# other than .sh files.  Note that we are free to list files in
# subdirectories here rather than creating a separate item list"
ROOT_EXES="configure config.guess install-sh move-if-change \
            modules/canna/configure"
LIB_SRC_EXES="ad2c gnuattach gnudoit rcs2log vcdiff *.pl"
ETC_EXES=""

LIB_SRC_BUILT_EXES="`cd lib-src; ls -1 *.c | sed 's/\.c$//g'`"
LIB_SRC_BUILT_EXES="minitar ctags $LIB_SRC_BUILT_EXES"
SRC_BUILT_EXES="temacs xemacs"

find . -type f -print0 | xargs -0 chmod a-x

for dir in . lib-src etc ; do
  if [ "$dir" = "." ]; then
    exes="$ROOT_EXES"
  elif [ "$dir" = "lib-src" ]; then
    exes="$LIB_SRC_EXES"
  elif [ "$dir" = "etc" ]; then
    exes="$ETC_EXES"
  else
    echo "Error! Don't know how to handle directory '$dir'"; exit 2
  fi
  pwd=`pwd`
  cd $dir
  for x in $exes *.sh ; do
    if [ ! -f $x ]; then
      echo "Warning: file '$dir/$x' doesn't exist"
    else
      chmod a+x $x
    fi
  done
  cd "$pwd"
done

# Now do the built executables.  Don't warn or anything if we don't find
# anything, since they may not be built.
for dir in lib-src src ; do
  if [ "$dir" = "src" ]; then
    exes="$SRC_BUILT_EXES"
  elif [ "$dir" = "lib-src" ]; then
    exes="$LIB_SRC_BUILT_EXES"
  else
    echo "Error! Don't know how to handle directory '$dir'"; exit 2
  fi
  pwd=`pwd`
  cd $dir
  for x in $exes *.exe ; do
    if [ -f $x ]; then
      chmod a+x $x
    fi
  done
  cd "$pwd"
done
