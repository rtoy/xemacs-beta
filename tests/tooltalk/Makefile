### Makefile - Makefile for ToolTalk testing executables

# Author:	Vladimir Ivanovic <vladimir@Eng.Sun.COM>
# Maintainer:	Vladimir Ivanovic <vladimir@Eng.Sun.COM>
# Created:	08 March 1995
# Keywords:	ToolTalk, testing

# Copyright (C) 1995 Sun Microsystems, Inc

# This file is part of XEmacs.

# XEmacs is free software: you can redistribute it and/or modify it
# under the terms of the GNU General Public License as published by the
# Free Software Foundation, either version 3 of the License, or (at your
# option) any later version.

# XEmacs is distributed in the hope that it will be useful, but WITHOUT
# ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
# FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
# for more details.

# You should have received a copy of the GNU General Public License
# along with XEmacs.  If not, see <http://www.gnu.org/licenses/>

### Commentary:
#   This is SPARCworks & Solaris 2.x-centric.

### Code:

DESTDIR=

CC       = cc -Xc
CPPFLAGS = -I/usr/openwin/include
CFLAGS   = -g -v -DNeedFunctionPrototypes
LDFLAGS  = -L/usr/openwin/lib -R/usr/openwin/lib -ltt

all:	emacs-eval load-file make-client-frame

clean:
	rm -f *.o emacs-eval load-file make-client-frame

### Makefile ends here.
