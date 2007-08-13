#   Makefile for Microsoft NMAKE
#   Copyright (C) 1995 Board of Trustees, University of Illinois.
#   Copyright (C) 1995, 1996 Ben Wing.
#   Copyright (C) 1995 Sun Microsystems, Inc.
#   Copyright (C) 1998 Free Software Foundation, Inc.
#
# This file is part of XEmacs.
#
# XEmacs is free software; you can redistribute it and/or modify it
# under the terms of the GNU General Public License as published by the
# Free Software Foundation; either version 2, or (at your option) any
# later version.
#
# XEmacs is distributed in the hope that it will be useful, but WITHOUT
# ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
# FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
# for more details.
#
# You should have received a copy of the GNU General Public License
# along with XEmacs; see the file COPYING.  If not, write to
# the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
# Boston, MA 02111-1307, USA.
#
# Synched up with: Not in FSF.
#

XEMACS=..
LISP=$(XEMACS)\lisp

#
# Command line options defaults
#
!if !defined(PATH_PACKAGEPATH)
PATH_PACKAGEPATH="~/.xemacs"
!endif
!if !defined(HAVE_MSW)
HAVE_MSW=0
!endif
!if !defined(HAVE_X)
HAVE_X=0
!endif
!if !defined(HAVE_MULE)
HAVE_MULE=0
!endif
!if !defined(HAVE_XPM)
HAVE_XPM=0
!endif
!if !defined(HAVE_TOOLBARS)
HAVE_TOOLBARS=$(HAVE_XPM)
!endif
!if !defined(HAVE_MSW_C_DIRED)
HAVE_MSW_C_DIRED=1
!endif
!if !defined(DEBUG_XEMACS)
DEBUG_XEMACS=1
!endif
!if !defined(USE_UNION_TYPE)
USE_UNION_TYPE=0
!endif
!if !defined(USE_MINIMAL_TAGBITS)
USE_MINIMAL_TAGBITS=0
!endif
!if !defined(USE_INDEXED_LRECORD_IMPLEMENTATION)
USE_INDEXED_LRECORD_IMPLEMENTATION=0
!endif

#
# Conf error checks
#
!if !$(HAVE_MSW) && !$(HAVE_X)
!error Please specify at least one HAVE_MSW=1 and/or HAVE_X=1
!endif
!if $(HAVE_X) && !defined(X11_DIR)
!error Please specify root directory for your X11 installation: X11_DIR=path
!endif
!if $(HAVE_MSW) && $(HAVE_XPM) && !defined(XPM_DIR)
!error Please specify root directory for your XPM installation: XPM_DIR=path
!endif
!if $(HAVE_MSW) && $(HAVE_TOOLBARS) && !$(HAVE_XPM)
!error Toolbars require XPM support
!endif

#
# Handle GUNG_HO
#
!if defined(GUNG_HO)
USE_MINIMAL_TAGBITS=$(GUNG_HO)
USE_INDEXED_LRECORD_IMPLEMENTATION=$(GUNG_HO)
!endif

#
# Small configuration report
#
!if !defined(CONF_REPORT_ALREADY_PRINTED)
!if [set CONF_REPORT_ALREADY_PRINTED=1]
!endif
!message ------------------------------------------------
!if $(HAVE_MSW)
!message Compiling in support for native GUI.
!endif
!if $(HAVE_X)
!message Compiling in support for X-Windows.
!endif
!if $(HAVE_MULE)
!message Compiling in MULE.
!endif
!if $(HAVE_XPM)
!message Compiling in support for XPM images.
!endif
!if $(HAVE_TOOLBARS)
!message Compiling in support for toolbars.
!endif
!if $(HAVE_MSW_C_DIRED)
# Define HAVE_MSW_C_DIRED to be non-zero if you want Xemacs to use C
# primitives to significantly speed up dired, at the expense of an
# additional ~4KB of code.
!message Compiling in fast dired implementation.
!endif
!if $(USE_MINIMAL_TAGBITS)
!message Using minimal tagbits.
!endif
!if $(USE_INDEXED_LRECORD_IMPLEMENTATION)
!message Using indexed lrecord implementation.
!endif
!if $(USE_UNION_TYPE)
!message Using union type for Lisp object storage.
!endif
!if $(DEBUG_XEMACS)
!message Compiling in extra debug checks. XEmacs will be slow!
!endif
!message ------------------------------------------------
!message 
!endif # !defined(CONF_REPORT_ALREADY_PRINTED)

!if $(DEBUG_XEMACS)
OPT=-Od -Zi
!else
OPT=-O2 -G5 -Zi
!endif

WARN_CPP_FLAGS = -W3

!if $(HAVE_X)
X_DEFINES=-DHAVE_X_WINDOWS
X_INCLUDES=-I$(X11_DIR)\include
X_LIBS=-libpath:$(X11_DIR)\lib Xaw.lib Xmu.lib Xt.lib SM.lib ICE.lib Xext.lib X11.lib
!endif

!if $(HAVE_MSW)
MSW_DEFINES=-DHAVE_MS_WINDOWS -DHAVE_SCROLLBARS -DHAVE_MENUBARS
MSW_INCLUDES=
MSW_LIBS=
!if $(HAVE_MSW_C_DIRED)
MSW_DEFINES=$(MSW_DEFINES) -DHAVE_MSW_C_DIRED
MSW_C_DIRED_SRC=$(XEMACS)\src\dired-msw.c
MSW_C_DIRED_OBJ=$(OUTDIR)\dired-msw.obj
!endif
!if $(HAVE_XPM)
MSW_DEFINES=$(MSW_DEFINES) -DHAVE_XPM -DFOR_MSW
MSW_INCLUDES=$(MSW_INCLUDES) -I$(XPM_DIR) -I$(XPM_DIR)\lib
MSW_LIBS=$(MSW_LIBS) $(XPM_DIR)\lib\Xpm.lib
!endif
!if $(HAVE_TOOLBARS)
MSW_DEFINES=$(MSW_DEFINES) -DHAVE_TOOLBARS
MSW_TOOLBAR_SRC=$(XEMACS)\src\toolbar.c $(XEMACS)\src\toolbar-msw.c
MSW_TOOLBAR_OBJ=$(OUTDIR)\toolbar.obj $(OUTDIR)\toolbar-msw.obj
MSW_LIBS=$(MSW_LIBS) comctl32.lib
!endif
!endif

!if $(HAVE_MULE)
MULE_DEFINES=-DMULE
!endif

!if $(DEBUG_XEMACS)
DEBUG_DEFINES=-DDEBUG_XEMACS
DEBUG_FLAGS= -debugtype:both -debug:full
!endif

!include "..\version.sh"

# Generic variables

INCLUDES=$(X_INCLUDES) $(MSW_INCLUDES) -I$(XEMACS)\nt\inc -I$(XEMACS)\src -I$(XEMACS)\lwlib -I"$(MSVCDIR)\include"

DEFINES=$(X_DEFINES) $(MSW_DEFINES) $(MULE_DEFINES) \
	-DWIN32 -D_WIN32 -DWIN32_LEAN_AND_MEAN -DWINDOWSNT -Demacs \
	-DHAVE_CONFIG_H -D_DEBUG

OUTDIR=obj

#------------------------------------------------------------------------------

default: $(OUTDIR)\nul all 

$(OUTDIR)\nul:
	-@mkdir $(OUTDIR)

XEMACS_INCLUDES=\
 $(XEMACS)\src\config.h \
 $(XEMACS)\src\Emacs.ad.h \
 $(XEMACS)\src\paths.h \
 $(XEMACS)\src\puresize-adjust.h

$(XEMACS)\src\config.h:	config.h
	!copy config.h $(XEMACS)\src

$(XEMACS)\src\Emacs.ad.h:	Emacs.ad.h
	!copy Emacs.ad.h $(XEMACS)\src

$(XEMACS)\src\paths.h:	paths.h
	!copy paths.h $(XEMACS)\src

$(XEMACS)\src\puresize-adjust.h:	puresize-adjust.h
	!copy puresize-adjust.h $(XEMACS)\src

#------------------------------------------------------------------------------

# LASTFILE Library

LASTFILE=$(OUTDIR)\lastfile.lib
LASTFILE_SRC=$(XEMACS)\src
LASTFILE_FLAGS=-nologo $(WARN_CPP_FLAGS) $(OPT) $(INCLUDES) -Fo$@ -c
LASTFILE_OBJS= \
	$(OUTDIR)\lastfile.obj

$(LASTFILE): $(XEMACS_INCLUDES) $(LASTFILE_OBJS)
	link.exe -lib -nologo -out:$@ $(LASTFILE_OBJS)

$(OUTDIR)\lastfile.obj:	$(LASTFILE_SRC)\lastfile.c
	 $(CC) $(LASTFILE_FLAGS) $**

#------------------------------------------------------------------------------

!if $(HAVE_X)

# LWLIB Library

LWLIB=$(OUTDIR)\lwlib.lib
LWLIB_SRC=$(XEMACS)\lwlib
LWLIB_FLAGS=-nologo $(WARN_CPP_FLAGS) $(OPT) $(INCLUDES) $(DEFINES) \
 -DNEED_ATHENA -DNEED_LUCID \
 -D_WINDOWS -DMENUBARS_LUCID -DSCROLLBARS_LUCID -DDIALOGS_ATHENA \
 -Fo$@ -c
LWLIB_OBJS= \
	$(OUTDIR)\lwlib-config.obj \
	$(OUTDIR)\lwlib-utils.obj \
	$(OUTDIR)\lwlib-Xaw.obj \
	$(OUTDIR)\lwlib-Xlw.obj \
	$(OUTDIR)\lwlib.obj \
	$(OUTDIR)\xlwmenu.obj \
	$(OUTDIR)\xlwscrollbar.obj

$(LWLIB): $(LWLIB_OBJS)
	link.exe -lib -nologo $(DEBUG_FLAGS) -out:$@ $(LWLIB_OBJS)

$(OUTDIR)\lwlib-config.obj:	$(LWLIB_SRC)\lwlib-config.c
	 $(CC) $(LWLIB_FLAGS) $**

$(OUTDIR)\lwlib-utils.obj:	$(LWLIB_SRC)\lwlib-utils.c
	 $(CC) $(LWLIB_FLAGS) $**

$(OUTDIR)\lwlib-Xaw.obj:	$(LWLIB_SRC)\lwlib-Xaw.c
	 $(CC) $(LWLIB_FLAGS) $**

$(OUTDIR)\lwlib-Xlw.obj:	$(LWLIB_SRC)\lwlib-Xlw.c
	 $(CC) $(LWLIB_FLAGS) $**

$(OUTDIR)\lwlib.obj:		$(LWLIB_SRC)\lwlib.c
	 $(CC) $(LWLIB_FLAGS) $**

$(OUTDIR)\xlwmenu.obj:		$(LWLIB_SRC)\xlwmenu.c
	 $(CC) $(LWLIB_FLAGS) $**

$(OUTDIR)\xlwscrollbar.obj:	$(LWLIB_SRC)\xlwscrollbar.c
	 $(CC) $(LWLIB_FLAGS) $**

!endif
#------------------------------------------------------------------------------

# lib-src programs

LIB_SRC=$(XEMACS)\lib-src
LIB_SRC_FLAGS=$(INCLUDES) $(DEFINES) -ML
LIB_SRC_LIBS= kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib\
 advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib libc.lib
LIB_SRC_LFLAGS=-nologo $(LIB_SRC_LIBS) -base:0x1000000\
 -subsystem:console -pdb:none $(DEBUG_FLAGS) -machine:I386\
 -nodefaultlib -out:$@

DOC=$(LIB_SRC)\DOC
DOC_SRC1=\
 $(XEMACS)\src\abbrev.c \
 $(XEMACS)\src\alloc.c \
 $(XEMACS)\src\alloca.c \
 $(XEMACS)\src\blocktype.c \
 $(XEMACS)\src\buffer.c \
 $(XEMACS)\src\bytecode.c \
 $(XEMACS)\src\callint.c \
 $(XEMACS)\src\callproc.c \
 $(XEMACS)\src\casefiddle.c \
 $(XEMACS)\src\casetab.c \
 $(XEMACS)\src\chartab.c \
 $(XEMACS)\src\cmdloop.c \
 $(XEMACS)\src\cmds.c \
 $(XEMACS)\src\console-stream.c \
 $(XEMACS)\src\console.c \
 $(XEMACS)\src\data.c \
 $(XEMACS)\src\device.c
DOC_SRC2=\
 $(XEMACS)\src\dialog.c \
 $(XEMACS)\src\dired.c \
 $(XEMACS)\src\doc.c \
 $(XEMACS)\src\doprnt.c \
 $(XEMACS)\src\dynarr.c \
 $(XEMACS)\src\editfns.c \
 $(XEMACS)\src\elhash.c \
 $(XEMACS)\src\emacs.c \
 $(XEMACS)\src\eval.c \
 $(XEMACS)\src\event-stream.c \
 $(XEMACS)\src\events.c \
 $(XEMACS)\src\extents.c \
 $(XEMACS)\src\faces.c \
 $(XEMACS)\src\file-coding.c \
 $(XEMACS)\src\fileio.c \
 $(XEMACS)\src\filelock.c \
 $(XEMACS)\src\filemode.c \
 $(XEMACS)\src\floatfns.c \
 $(XEMACS)\src\fns.c 
DOC_SRC3=\
 $(XEMACS)\src\font-lock.c \
 $(XEMACS)\src\frame.c \
 $(XEMACS)\src\free-hook.c \
 $(XEMACS)\src\general.c \
 $(XEMACS)\src\glyphs.c \
 $(XEMACS)\src\gmalloc.c \
 $(XEMACS)\src\gui.c  \
 $(XEMACS)\src\hash.c \
 $(XEMACS)\src\imgproc.c \
 $(XEMACS)\src\indent.c \
 $(XEMACS)\src\inline.c \
 $(XEMACS)\src\insdel.c \
 $(XEMACS)\src\intl.c \
 $(XEMACS)\src\keymap.c \
 $(XEMACS)\src\line-number.c \
 $(XEMACS)\src\lread.c \
 $(XEMACS)\src\lstream.c \
 $(XEMACS)\src\macros.c \
 $(XEMACS)\src\marker.c
DOC_SRC4=\
 $(XEMACS)\src\md5.c \
 $(XEMACS)\src\menubar.c \
 $(XEMACS)\src\minibuf.c \
 $(XEMACS)\src\nt.c \
 $(XEMACS)\src\ntheap.c \
 $(XEMACS)\src\ntproc.c \
 $(XEMACS)\src\objects.c \
 $(XEMACS)\src\opaque.c \
 $(XEMACS)\src\print.c \
 $(XEMACS)\src\process.c \
 $(XEMACS)\src\process-nt.c \
 $(XEMACS)\src\profile.c \
 $(XEMACS)\src\pure.c \
 $(XEMACS)\src\rangetab.c \
 $(XEMACS)\src\realpath.c \
 $(XEMACS)\src\redisplay-output.c \
 $(XEMACS)\src\redisplay.c \
 $(XEMACS)\src\regex.c \
 $(XEMACS)\src\scrollbar.c \
 $(XEMACS)\src\scrollbar-msw.c \
 $(XEMACS)\src\search.c \
 $(XEMACS)\src\signal.c \
 $(XEMACS)\src\sound.c 
DOC_SRC5=\
 $(XEMACS)\src\specifier.c \
 $(XEMACS)\src\strftime.c \
 $(XEMACS)\src\symbols.c \
 $(XEMACS)\src\syntax.c \
 $(XEMACS)\src\sysdep.c \
 $(XEMACS)\src\termcap.c  \
 $(XEMACS)\src\tparam.c \
 $(XEMACS)\src\undo.c \
 $(XEMACS)\src\unexnt.c \
 $(XEMACS)\src\vm-limit.c \
 $(XEMACS)\src\window.c \
 $(XEMACS)\src\widget.c

!if $(HAVE_X)
DOC_SRC6=\
 $(XEMACS)\src\balloon_help.c \
 $(XEMACS)\src\console-x.c \
 $(XEMACS)\src\device-x.c  \
 $(XEMACS)\src\dialog-x.c \
 $(XEMACS)\src\EmacsFrame.c \
 $(XEMACS)\src\EmacsManager.c \
 $(XEMACS)\src\EmacsShell-sub.c\
 $(XEMACS)\src\EmacsShell.c \
 $(XEMACS)\src\event-Xt.c  \
 $(XEMACS)\src\frame-x.c \
 $(XEMACS)\src\glyphs-x.c \
 $(XEMACS)\src\gui-x.c \
 $(XEMACS)\src\menubar.c \
 $(XEMACS)\src\menubar-x.c \
 $(XEMACS)\src\objects-x.c \
 $(XEMACS)\src\redisplay-x.c \
 $(XEMACS)\src\scrollbar-x.c \
 $(XEMACS)\src\balloon-x.c \
 $(XEMACS)\src\xgccache.c \
 $(XEMACS)\src\xmu.c \
 $(XEMACS)\src\xselect.c 
!endif

!if $(HAVE_MSW)
DOC_SRC7=\
 $(XEMACS)\src\console-msw.c \
 $(XEMACS)\src\device-msw.c  \
 $(XEMACS)\src\event-msw.c  \
 $(XEMACS)\src\frame-msw.c \
 $(XEMACS)\src\glyphs-msw.c \
 $(XEMACS)\src\menubar-msw.c \
 $(XEMACS)\src\objects-msw.c \
 $(XEMACS)\src\redisplay-msw.c \
 $(XEMACS)\src\scrollbar-msw.c \
 $(XEMACS)\src\select-msw.c \
 $(MSW_C_DIRED_SRC) \
 $(MSW_TOOLBAR_SRC)
!endif

!if $(HAVE_MULE)
DOC_SRC8=\
 $(XEMACS)\src\input-method-xlib.c \
 $(XEMACS)\src\mule.c \
 $(XEMACS)\src\mule-charset.c \
 $(XEMACS)\src\mule-ccl.c \
 $(XEMACS)\src\mule-coding.c
!endif

!if $(DEBUG_XEMACS)
DOC_SRC9=\
 $(XEMACS)\src\debug.c
!endif

MAKE_DOCFILE=$(LIB_SRC)\make-docfile.exe

$(MAKE_DOCFILE): $(OUTDIR)\make-docfile.obj
	link.exe -out:$@ $(LIB_SRC_LFLAGS) $** $(LIB_SRC_LIBS)

$(OUTDIR)\make-docfile.obj:	$(LIB_SRC)\make-docfile.c
	 $(CC) -nologo $(LIB_SRC_FLAGS) -c $** -Fo$@

RUNEMACS=$(XEMACS)\src\runemacs.exe

$(RUNEMACS): $(OUTDIR)\runemacs.obj
	link.exe -nologo -out:$@ -subsystem:windows -entry:WinMainCRTStartup \
	-pdb:none -release -incremental:no $** \
	kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib \
	advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib libc.lib

$(OUTDIR)\runemacs.obj:	$(XEMACS)\nt\runemacs.c
	$(CC) -nologo -ML $(WARN_CPP_FLAGS) $(OPT) -c \
	-D_DEBUG -DWIN32 -D_WIN32 -DWIN32_LEAN_AND_MEAN \
	-D_X86_ -Demacs -DHAVE_CONFIG_H \
	$** -Fo$@

SUPPORT_PROGS=$(MAKE_DOCFILE) $(RUNEMACS)

#------------------------------------------------------------------------------

# TEMACS Executable

TEMACS_DIR=$(XEMACS)\src
TEMACS=$(TEMACS_DIR)\temacs.exe
TEMACS_BROWSE=$(TEMACS_DIR)\temacs.bsc
TEMACS_SRC=$(XEMACS)\src
TEMACS_LIBS=$(LASTFILE) $(LWLIB) $(X_LIBS) $(MSW_LIBS) \
 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib \
 shell32.lib ole32.lib oleaut32.lib uuid.lib wsock32.lib winmm.lib libc.lib
TEMACS_LFLAGS=-nologo $(LIBRARIES) $(DEBUG_FLAGS) -base:0x1000000\
 -stack:0x800000 -entry:_start -subsystem:console\
 -pdb:$(TEMACS_DIR)\temacs.pdb -map:$(TEMACS_DIR)\temacs.map \
 -heap:0x00100000 -out:$@
TEMACS_CPP_FLAGS= $(WARN_CPP_FLAGS) $(INCLUDES) $(DEFINES) $(DEBUG_DEFINES) \
 -DEMACS_MAJOR_VERSION=$(emacs_major_version) \
 -DEMACS_MINOR_VERSION=$(emacs_minor_version) \
 -DEMACS_BETA_VERSION=$(emacs_beta_version) \
 -DXEMACS_CODENAME=\"$(xemacs_codename)\" \
 -DPATH_PREFIX=\"$(XEMACS)\" \
 -DPATH_PACKAGEPATH=\"$(PATH_PACKAGEPATH)\"

TEMACS_FLAGS=-nologo -ML $(WARN_CPP_FALGS) $(OPT) -c $(TEMACS_CPP_FLAGS)

!if $(HAVE_X)
TEMACS_X_OBJS=\
	$(OUTDIR)\balloon-x.obj \
	$(OUTDIR)\balloon_help.obj \
	$(OUTDIR)\console-x.obj \
	$(OUTDIR)\device-x.obj \
	$(OUTDIR)\dialog-x.obj \
	$(OUTDIR)\EmacsFrame.obj \
	$(OUTDIR)\EmacsManager.obj \
	$(OUTDIR)\EmacsShell.obj \
	$(OUTDIR)\TopLevelEmacsShell.obj\
	$(OUTDIR)\TransientEmacsShell.obj\
	$(OUTDIR)\event-Xt.obj \
	$(OUTDIR)\frame-x.obj \
	$(OUTDIR)\glyphs-x.obj \
	$(OUTDIR)\gui-x.obj \
	$(OUTDIR)\menubar-x.obj \
	$(OUTDIR)\objects-x.obj \
	$(OUTDIR)\redisplay-x.obj \
	$(OUTDIR)\scrollbar-x.obj \
	$(OUTDIR)\xgccache.obj \
	$(OUTDIR)\xmu.obj \
	$(OUTDIR)\xselect.obj
!endif

!if $(HAVE_MSW)
TEMACS_MSW_OBJS=\
	$(OUTDIR)\console-msw.obj \
	$(OUTDIR)\device-msw.obj \
	$(OUTDIR)\event-msw.obj \
	$(OUTDIR)\frame-msw.obj \
	$(OUTDIR)\glyphs-msw.obj \
	$(OUTDIR)\menubar-msw.obj \
	$(OUTDIR)\objects-msw.obj \
	$(OUTDIR)\redisplay-msw.obj \
	$(OUTDIR)\scrollbar-msw.obj \
	$(OUTDIR)\select-msw.obj \
	$(MSW_C_DIRED_OBJ) \
	$(MSW_TOOLBAR_OBJ)
!endif


!if $(HAVE_MULE)
TEMACS_MULE_OBJS=\
	$(OUTDIR)\input-method-xlib.obj \
	$(OUTDIR)\mule.obj \
	$(OUTDIR)\mule-charset.obj \
	$(OUTDIR)\mule-ccl.obj \
	$(OUTDIR)\mule-coding.obj
!endif

!if $(DEBUG_XEMACS)
TEMACS_DEBUG_OBJS=\
	$(OUTDIR)\debug.obj
!endif

TEMACS_OBJS= \
	$(TEMACS_X_OBJS)\
	$(TEMACS_MSW_OBJS)\
	$(TEMACS_CODING_OBJS)\
	$(TEMACS_MULE_OBJS)\
	$(TEMACS_DEBUG_OBJS)\
	$(OUTDIR)\abbrev.obj \
	$(OUTDIR)\alloc.obj \
	$(OUTDIR)\alloca.obj \
	$(OUTDIR)\blocktype.obj \
	$(OUTDIR)\buffer.obj \
	$(OUTDIR)\bytecode.obj \
	$(OUTDIR)\callint.obj \
	$(OUTDIR)\callproc.obj \
	$(OUTDIR)\casefiddle.obj \
	$(OUTDIR)\casetab.obj \
	$(OUTDIR)\chartab.obj \
	$(OUTDIR)\cmdloop.obj \
	$(OUTDIR)\cmds.obj \
	$(OUTDIR)\console-stream.obj \
	$(OUTDIR)\console.obj \
	$(OUTDIR)\data.obj \
	$(OUTDIR)\device.obj \
	$(OUTDIR)\dialog.obj \
	$(OUTDIR)\dired.obj \
	$(OUTDIR)\doc.obj \
	$(OUTDIR)\doprnt.obj \
	$(OUTDIR)\dynarr.obj \
	$(OUTDIR)\editfns.obj \
	$(OUTDIR)\elhash.obj \
	$(OUTDIR)\emacs.obj \
	$(OUTDIR)\eval.obj \
	$(OUTDIR)\event-stream.obj \
	$(OUTDIR)\events.obj \
	$(OUTDIR)\extents.obj \
	$(OUTDIR)\faces.obj \
	$(OUTDIR)\file-coding.obj \
	$(OUTDIR)\fileio.obj \
	$(OUTDIR)\filelock.obj \
	$(OUTDIR)\filemode.obj \
	$(OUTDIR)\floatfns.obj \
	$(OUTDIR)\fns.obj \
	$(OUTDIR)\font-lock.obj \
	$(OUTDIR)\frame.obj \
	$(OUTDIR)\free-hook.obj \
	$(OUTDIR)\general.obj \
	$(OUTDIR)\glyphs.obj \
	$(OUTDIR)\gmalloc.obj \
	$(OUTDIR)\gui.obj \
	$(OUTDIR)\hash.obj \
	$(OUTDIR)\indent.obj \
	$(OUTDIR)\imgproc.obj \
	$(OUTDIR)\inline.obj \
	$(OUTDIR)\insdel.obj \
	$(OUTDIR)\intl.obj \
	$(OUTDIR)\keymap.obj \
	$(OUTDIR)\line-number.obj \
	$(OUTDIR)\lread.obj \
	$(OUTDIR)\lstream.obj \
	$(OUTDIR)\macros.obj \
	$(OUTDIR)\menubar.obj \
	$(OUTDIR)\marker.obj \
	$(OUTDIR)\md5.obj \
	$(OUTDIR)\minibuf.obj \
	$(OUTDIR)\nt.obj \
	$(OUTDIR)\ntheap.obj \
	$(OUTDIR)\ntproc.obj \
	$(OUTDIR)\objects.obj \
	$(OUTDIR)\opaque.obj \
	$(OUTDIR)\print.obj \
	$(OUTDIR)\process.obj \
	$(OUTDIR)\process-nt.obj \
	$(OUTDIR)\profile.obj \
	$(OUTDIR)\pure.obj \
	$(OUTDIR)\rangetab.obj \
	$(OUTDIR)\realpath.obj \
	$(OUTDIR)\redisplay-output.obj \
	$(OUTDIR)\redisplay.obj \
	$(OUTDIR)\regex.obj \
	$(OUTDIR)\scrollbar.obj \
	$(OUTDIR)\search.obj \
	$(OUTDIR)\signal.obj \
	$(OUTDIR)\sound.obj \
	$(OUTDIR)\specifier.obj \
	$(OUTDIR)\strftime.obj \
	$(OUTDIR)\symbols.obj \
	$(OUTDIR)\syntax.obj \
	$(OUTDIR)\sysdep.obj \
	$(OUTDIR)\tparam.obj \
	$(OUTDIR)\undo.obj \
	$(OUTDIR)\unexnt.obj \
	$(OUTDIR)\vm-limit.obj \
	$(OUTDIR)\widget.obj \
	$(OUTDIR)\window.obj \
	xemacs.res

# Rules

.SUFFIXES:
.SUFFIXES:	.c

# nmake rule
{$(TEMACS_SRC)}.c{$(OUTDIR)}.obj:
	$(CC) $(TEMACS_FLAGS) $< -Fo$@ -Fr$*.sbr

$(OUTDIR)\TopLevelEmacsShell.obj:	$(TEMACS_SRC)\EmacsShell-sub.c
	$(CC) $(TEMACS_FLAGS) -DDEFINE_TOP_LEVEL_EMACS_SHELL $** -Fo$@

$(OUTDIR)\TransientEmacsShell.obj: $(TEMACS_SRC)\EmacsShell-sub.c
	$(CC) $(TEMACS_FLAGS) -DDEFINE_TRANSIENT_EMACS_SHELL $** -Fo$@

$(OUTDIR)\pure.obj: $(TEMACS_SRC)\pure.c $(TEMACS_SRC)\puresize-adjust.h

#$(TEMACS_SRC)\Emacs.ad.h: $(XEMACS)\etc\Emacs.ad
#	!"sed -f ad2c.sed < $(XEMACS)\etc\Emacs.ad > $(TEMACS_SRC)\Emacs.ad.h"

#$(TEMACS_SRC)\paths.h: $(TEMACS_SRC)\paths.h.in
#	!"cd $(TEMACS_SRC); cp paths.h.in paths.h"

$(TEMACS): $(TEMACS_INCLUDES) $(TEMACS_OBJS)
	link.exe @<<
  $(TEMACS_LFLAGS) $(TEMACS_OBJS) $(TEMACS_LIBS)
<<

xemacs.res: xemacs.rc
	rc xemacs.rc

# MSDEV Source Broswer file. "*.sbr" is too inclusive but this is harmless
$(TEMACS_BROWSE): $(TEMACS_OBJS)
	@dir /b/s $(OUTDIR)\*.sbr > bscmake.tmp
	bscmake /nologo -o$@ @bscmake.tmp
	@del bscmake.tmp

#------------------------------------------------------------------------------

# LISP bits 'n bobs

LOADPATH=$(LISP)

$(DOC): $(LIB_SRC)\make-docfile.exe
	-del $(DOC)
	!$(TEMACS) -batch -l $(TEMACS_DIR)\..\lisp\make-docfile.el -- -o $(DOC) -i $(XEMACS)\site-packages
	!$(LIB_SRC)\make-docfile.exe -a $(DOC) -d $(TEMACS_SRC) $(DOC_SRC1)
	!$(LIB_SRC)\make-docfile.exe -a $(DOC) -d $(TEMACS_SRC) $(DOC_SRC2)
	!$(LIB_SRC)\make-docfile.exe -a $(DOC) -d $(TEMACS_SRC) $(DOC_SRC3)
	!$(LIB_SRC)\make-docfile.exe -a $(DOC) -d $(TEMACS_SRC) $(DOC_SRC4)
	!$(LIB_SRC)\make-docfile.exe -a $(DOC) -d $(TEMACS_SRC) $(DOC_SRC5)
	!$(LIB_SRC)\make-docfile.exe -a $(DOC) -d $(TEMACS_SRC) $(DOC_SRC6)
	!$(LIB_SRC)\make-docfile.exe -a $(DOC) -d $(TEMACS_SRC) $(DOC_SRC7)
	!$(LIB_SRC)\make-docfile.exe -a $(DOC) -d $(TEMACS_SRC) $(DOC_SRC8)
	!$(LIB_SRC)\make-docfile.exe -a $(DOC) -d $(TEMACS_SRC) $(DOC_SRC9)

$(LISP)\Installation.el: Installation.el
	copy Installation.el $(LISP)

update-elc: $(LISP)\Installation.el
	set EMACSBOOTSTRAPLOADPATH=$(LISP)
	!$(TEMACS) -batch -l $(TEMACS_DIR)\..\lisp\update-elc.el

rebuild: $(TEMACS_DIR)\puresize-adjust.h
        !nmake -nologo -f xemacs.mak dump-xemacs

# This rule dumps xemacs and then checks to see if a rebuild is required due
# to changing PURESPACE requirements.
dump-xemacs: $(TEMACS)
	!echo >rebuild
	cd $(TEMACS_DIR)
	set EMACSBOOTSTRAPLOADPATH=$(LISP)
	!$(TEMACS) -batch -l $(TEMACS_DIR)\..\lisp\loadup.el dump
	cd $(XEMACS)\nt
	!nmake -nologo -f xemacs.mak rebuild

#------------------------------------------------------------------------------

# use this rule to build the complete system
all: $(LASTFILE) $(LWLIB) $(SUPPORT_PROGS) $(TEMACS) $(TEMACS_BROWSE) update-elc $(DOC) dump-xemacs
	-del rebuild

temacs:  $(TEMACS)

# use this rule to install the system
install:
	echo Not yet implemented.

distclean:
	del *.bak
	del *.orig
	del *.rej
	del *.pdb
	del *.tmp
	del puresize-adjust.h
	cd $(OUTDIR)
	del *.obj
	del *.sbr
	del *.lib
	cd ..\$(TEMACS_DIR)
	del config.h
	del paths.h
	del Emacs.ad.h
	del *.bak
	del *.orig
	del *.rej
	del *.exe
	del *.map
	del *.bsc
	del *.pdb
	cd $(LIB_SRC)
	del DOC
	del *.bak
	del *.orig
	del *.rej
	del *.exe
	cd $(LISP)
	-del /s /q *.bak *.elc *.orig *.rej

depend:
	mkdepend -f xemacs.mak -p$(OUTDIR)\ -o.obj -w9999 -- $(TEMACS_CPP_FLAGS) --  $(DOC_SRC1) $(DOC_SRC2) $(DOC_SRC3) $(DOC_SRC4) $(DOC_SRC5) $(DOC_SRC6) $(DOC_SRC7) $(DOC_SRC8) $(DOC_SRC9) $(LASTFILE_SRC)\lastfile.c $(LIB_SRC)\make-docfile.c .\runemacs.c

# DO NOT DELETE THIS LINE -- make depend depends on it.

