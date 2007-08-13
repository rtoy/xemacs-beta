#! /bin/sh
### gzip-el.sh --- compress superfluous installed source lisp

# Author:	Jeff Miller <jmiller@bay1.bayserve.net>
# Author:	Hrvoje Niksic <hniksic@srce.hr>
# Maintainer:	Steve Baur <steve@altair.xemacs.org>
# Created:	13 Feb 1997
# Version:	1.0
# Keywords:	internal


#
#
echo Compressing .el files in "$1"...

find "$1" -type f -name "*.el" -print |
	while read file; do
		[ -s "${file}c" ] && echo "$file" && gzip -f9 "$file"
	done

echo Compressing .el files in "$1"...done.
