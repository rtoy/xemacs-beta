##   Compface Makefile for Microsoft NMAKE
##   Based on xpm.mak
##   Copyright (C) 2001 Ben Wing.

## This file is part of XEmacs.

## XEmacs is free software; you can redistribute it and/or modify it
## under the terms of the GNU General Public License as published by the
## Free Software Foundation; either version 2, or (at your option) any
## later version.

## XEmacs is distributed in the hope that it will be useful, but WITHOUT
## ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
## FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
## for more details.

## You should have received a copy of the GNU General Public License
## along with XEmacs; see the file COPYING.  If not, write to
## the Free Software Foundation, Inc., 51 Franklin Street - Fifth Floor,
## Boston, MA 02110-1301, USA.

!if !defined(DEBUG_XEMACS)
DEBUG_XEMACS=0
!endif

!if $(DEBUG_XEMACS)
OPT=-Od -Zi
!else
OPT=-O2 -G5 -Zi
!endif

WARN_CPP_FLAGS = -W3

#MSVC uses string.h, not strings.h
DEFINES=-DSYSV32

CC=cl
CFLAGS=-nologo -DFOR_MSW $(WARN_CPP_FLAGS) $(OPT) $(INCLUDES) $(DEFINES) -Fo$@ -c

OBJS= arith.obj file.obj compress.obj gen.obj uncompface.obj

# nmake rule

.SUFFIXES:
.SUFFIXES:	.c

.c.obj:
	$(CC) $(CFLAGS) $< -Fo$@


# targets

all: libcompface.lib

libcompface.lib: $(OBJS)
	link.exe -lib -nologo -out:$@ $(OBJS)

clean: $(OBJS)
	-rm -f $(OBJS) libcompface.lib
