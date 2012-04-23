##   Makefile for external widget test code.
##   Copyright (C) 1993 Ben Wing.

## This file is part of XEmacs.

## XEmacs is free software: you can redistribute it and/or modify it
## under the terms of the GNU General Public License as published by the
## Free Software Foundation, either version 3 of the License, or (at your
## option) any later version.

## XEmacs is distributed in the hope that it will be useful, but WITHOUT
## ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
## FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
## for more details.

## You should have received a copy of the GNU General Public License
## along with XEmacs.  If not, see <http://www.gnu.org/licenses/>.

DESTDIR=

CFLAGS += -Xc -g -DTOOLTALK
EMACSHOME = ../../..
EMACSLIBDIR = $(EMACSHOME)/editor/src
EMACSINCLUDEDIR = $(EMACSHOME)/editor/src
MOTIFHOME = /opt/SUNWmotif

CPPFLAGS += -I$(OPENWINHOME)/include
LDFLAGS +=  -L$(OPENWINHOME)/lib -R$(OPENWINHOME)/lib

CPPFLAGS += -I$(MOTIFHOME)/include
LDFLAGS += -L$(MOTIFHOME)/lib -R$(MOTIFHOME)/lib

CPPFLAGS += -I$(EMACSINCLUDEDIR)
LDFLAGS += -L$(EMACSLIBDIR)

LDFLAGS += -R$(OPENWINHOME)/lib:$(EMACSLIBDIR) -g

TM_OBJS = test-ew-motif.o
TXL_OBJS = test-ew-xlib.o
MOTIF_LIBS = -Bstatic -lextcli_Xm -Bdynamic -lXm -lXt -ltt -lX11 -lgen -lm 
XLIB_LIBS = -Bstatic -lextcli_Xlib -Bdynamic -ltt -lX11 -lgen -lm 

all: test-ew-motif

test-ew-motif: $(TM_OBJS)
	$(CC) -o $@ $(LDFLAGS) $(TM_OBJS) $(MOTIF_LIBS)

test-ew-xlib: $(TXL_OBJS)
	$(CC) -o $@ $(LDFLAGS) $(TXL_OBJS) $(XLIB_LIBS)

