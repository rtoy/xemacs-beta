#! /bin/sh
# add-little-package.sh --- Add single file package to Package Lisp Hierarchy
# Copyright (C) 1997 Free Software Foundation, Inc.

# Author:	SL Baur <steve@altair.xemacs.org>
# Maintainer:	SL Baur <steve@altair.xemacs.org>
# Keywords:	packages internal

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

## This file copies a single lisp file into an XEmacs package hierarchy and
## performs the necessary magic so that it will be autoloaded at the next
## dump.

## Parameters:
##	$1 -- Full path to an XEmacsen later than 20.3
##	$2 -- Full path to a lisp file to install
##	$3 -- Full path to a lisp directory in an XEmacs package hierarchy
##	      This directory will be created if it does not exist.
##	      NOTE: the directory name should *not* end in a trailing slash


### Code:

XEMACS="$1"
LISP_FILE="$2"
DEST_DIR="$3"

# Test for valid XEmacs executable and valid input file
if [ ! -f "${LISP_FILE}" -o ! -x "${XEMACS}" ]; then
	exit 1
fi

# Test for destination directory, creating if necessary
test -d "${DEST_DIR}" || mkdir "${DEST_DIR}"
test -d "${DEST_DIR}" || exit 1;

cp -p "${LISP_FILE}" "${DEST_DIR}" || exit 1;
"${XEMACS}" -batch -no-site-file -f batch-byte-compile \
		"${DEST_DIR}/"`basename ${LISP_FILE}`

# recompute autoloads ...
"${XEMACS}" -batch -no-site-file -l autoload \
		-f batch-update-directory "${DEST_DIR}"
# and bytecompile if one was created
if [ -f "${DEST_DIR}/auto-autoloads.el" ]; then
	"${XEMACS}" -batch -no-site-file -f batch-byte-compile \
		"${DEST_DIR}"/auto-autoloads.el
fi

# recompute custom-loads
"${XEMACS}" -batch -no-site-file -l cus-dep \
		-f Custom-make-dependencies "${DEST_DIR}"
# and bytecompile if one was created
if [ -f "${DEST_DIR}/custom-load.el" ]; then
	"${XEMACS}" -batch -no-site-file -f batch-byte-compile \
		"${DEST_DIR}"/custom-load.el
fi

exit 0

### add-little-package.sh ends here
