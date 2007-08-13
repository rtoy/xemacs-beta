#
# SUMMARY:      Makefile for the OO-Browser
#
# AUTHOR:       Bob Weiner / Mark Stern
# ORG:          InfoDock Associates / Brown U.
#
# ORIG-DATE:    15-Oct-90
# LAST-MOD:     19-Oct-96 at 00:53:29 by Bob Weiner
#
# Copyright (C) 1990-1996  Free Software Foundation, Inc.
# See the file BR-COPY for license information.
#
# This file is part of the OO-Browser.

# If the usleep () system call exists on your system, uncomment the following line.
DEFINES         = -DHAVE_USLEEP

# Root directory of your X distribution.  Note how this is used in succeeding
# variables.
XROOT           = /usr/X11
LD_LIBRARY_PATH = $(XROOT)/lib
INCLUDEDIRS     = -I$(XROOT)/include
LIBDIRS         = -L$(XROOT)/lib

# Use this for the standard Athena widgets.
XAW = -lXaw
# Use this instead for the 3D Athena widget library.
XAW3D = -lXaw3d
# Set this to one of the 2 above variable names.
DEFXAW = XAW
# Set this to one of the 2 above variable values.
LIBXAW = $(XAW)

# C compiler
CC		= gcc
# Compiler flags
CFLAGS		= -g -O $(INCLUDEDIRS) $(DEFINES) -D$(DEFXAW)

#   Set XVERSION to the proper relase of X11 for your system.
#   Use X11R5 if you system is not on X11R6 or above.
X11R5 = 
X11R6 = -lSM -lICE
XVERSION = $(X11R6)

#   X11 libraries used by all system configurations.
XCOMMON = -lXmu -lXt -lX11 -lXext

#   This is used for static linking on a generic platform such as SunOS 4.
LD_STATIC   	 = -static $(LIBDIRS) $(LIBXAW) $(XCOMMON) $(XVERSION)
#   This is used for dynamic linking under Solaris.
LD_SOLARIS       = $(LIBDIRS) $(LIBXAW) $(XCOMMON) -lsocket $(XVERSION)
#   This is used for dynamic linking under Linux.
LD_LINUX         = $(LIBDIRS) $(LIBXAW) $(XCOMMON) $(XVERSION)

# Set this to be the directory in which to install the X OO-Browser
# executable.  This should be a directory in all users' search paths or the
# value of the Emacs variable, exec-directory.
# Use 'make install' to install the binary.
BINDIR          = /home/infodock/lib-bin-linux-elf

NAME		= xoobr
C_FILES 	= draw.c tree.c input.c dissolve.c dbl.c intf.c usleep.c
O_FILES		= draw.o tree.o input.o dissolve.o dbl.o intf.o usleep.o
INC_FILES	= defs.h tree.h input.h dissolve.h dbl.h rsrc.h intf.h help.h
SRC_FILES	= $(INC_FILES) $(C_FILES)

$(NAME):	linux

static:		$(O_FILES)
		$(CC) $(CFLAGS) -o $(NAME) $(O_FILES) $(LD_STATIC)

solaris:	$(O_FILES)
		$(CC) $(CFLAGS) -DSYSV -o $(NAME) $(O_FILES) $(LD_SOLARIS)

linux:		$(O_FILES)
		$(CC) $(CFLAGS) -o $(NAME) $(O_FILES) $(LD_LINUX)


install:	$(NAME)
		chmod 755 $(NAME)
		mv $(NAME) $(BINDIR)/$(NAME)

clean:
		rm -f *.o *.orig *.rej $(NAME) core *~

size:
		wc $(SRC_FILES)

dbl.o: 		dbl.h 
dissolve.o:	dissolve.h
draw.o: 	defs.h tree.h dbl.h intf.h
input.o: 	defs.h tree.h input.h
intf.o: 	defs.h tree.h dbl.h intf.h rsrc.h input.h help.h
tree.o: 	defs.h tree.h intf.h
