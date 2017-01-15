#
# XPM Makefile for Microsoft NMAKE without X libraries
#
# Copyright (C) 1997 Free Software Foundation, Inc.
#
# This file is part of XEmacs.
# 
# XEmacs is free software: you can redistribute it and/or modify it
# under the terms of the GNU General Public License as published by the
# Free Software Foundation, either version 3 of the License, or (at your
# option) any later version.
# 
# XEmacs is distributed in the hope that it will be useful, but WITHOUT
# ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
# FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
# for more details.
# 
# You should have received a copy of the GNU General Public License
# along with XEmacs.  If not, see <http://www.gnu.org/licenses/>.
#
!if !defined(DEBUG)
!if defined(DEBUG_XEMACS)
DEBUG=$(DEBUG_XEMACS)
!else
DEBUG=0
!endif
!endif

!if !defined(USE_CRTDLL)
USE_CRTDLL=1
!endif

!if $(DEBUG)
OPT=-Od -Zi
LINK_DEBUG=-debug
!else
OPT=-Ox
!endif

!if $(USE_CRTDLL)
!if $(DEBUG)
C_LIBFLAG=-MDd
!else
C_LIBFLAG=-MD
!endif
!else
!if $(DEBUG)
C_LIBFLAG=-MLd
!else
C_LIBFLAG=-ML
!endif
!endif

WARN_CPP_FLAGS = -W3

CC=cl
CFLAGS=-nologo -DFOR_MSW $(C_LIBFLAG) $(WARN_CPP_FLAGS) \
	$(OPT) $(INCLUDES) -c

OBJS= data.obj create.obj misc.obj rgb.obj scan.obj parse.obj hashtab.obj \
      WrFFrI.obj RdFToI.obj CrIFrDat.obj CrDatFrI.obj \
      CrIFrBuf.obj CrBufFrI.obj \
      RdFToDat.obj WrFFrDat.obj \
      Attrib.obj Image.obj Info.obj RdFToBuf.obj WrFFrBuf.obj \
      simx.obj

# nmake rule

.SUFFIXES:
.SUFFIXES:	.c

.c.obj::
	$(CC) $(CFLAGS) $<


# targets

all: ..\X11\xpm.h Xpm.lib

..\X11\xpm.h: ..\X11\NUL xpm.h
	copy xpm.h ..\X11

..\X11\NUL:
	mkdir ..\X11

Xpm.lib: $(OBJS)
	lib -nologo -out:$@ $(OBJS)
