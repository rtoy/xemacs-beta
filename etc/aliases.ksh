### aliases.ksh --- Useful shortcuts for XEmacs source hackers

# Copyright (C) 1998 Free Software Foundation, Inc.

# Author: Steve Baur
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

# Shortcuts for sh-derived Unix shells (ksh, zsh, bash)

function runtemacs
{
	if [ ! -x temacs ]; then
		echo "Must be in temacs source directory to run temacs."
		return 1;
	fi

	./temacs -batch -l loadup.el run-temacs "$@"
}
