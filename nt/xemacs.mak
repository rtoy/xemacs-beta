# Makefile for Microsoft NMAKE	-*- Makefile -*-
#
#   Copyright (C) 1995 Board of Trustees, University of Illinois.
#   Copyright (C) 1995, 1996, 2000, 2001, 2002, 2003 Ben Wing.
#   Copyright (C) 1997, 1998, 2000 Jonathan Harris.
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

default: all

# APA: Since there seems to be no way to determine the directory where
# xemacs.mak is located (from within nmake) we just insist on the user
# to invoke nmake in the directory where xemacs.mak is.
!if !exist("$(MAKEDIR)\xemacs.mak")
!error Please run nmake from the directory of this makefile (xemacs\nt).
!endif

MAKEROOT=$(MAKEDIR:\nt=)

########################### Common commands.

# Put these before including config.inc so they can be overridden there.
# Note that some versions of some commands are deficient.

# Define the 'del' command to use
# WinME's DEL command can only handle one argument and only has the /P flag.
# So only delete one glob at a time.  Override flags in config.inc.
DEL=del

# Tell COPY, MOVE, and XCOPY to suppress confirmation for overwriting
# files.
# set COPYCMD=/y
# Define the 'copy' command to use.
# Use /r (instead of /y), which exists on Windows NT 4 and 5.
COPY=xcopy /q /r
COPYDIR=xcopy /q /r /e

########################### Includes, and source and build tree determination.

!include "config.inc"

!if defined(BUILD_DIR)
SEPARATE_BUILD=1
SRCROOT=$(MAKEROOT)
BLDROOT=$(BUILD_DIR)
!else
!if defined(SOURCE_DIR)
SEPARATE_BUILD=1
SRCROOT=$(SOURCE_DIR)
BLDROOT=$(MAKEROOT)
!else
SEPARATE_BUILD=0
SRCROOT=$(MAKEROOT)
BLDROOT=$(MAKEROOT)
!endif
!endif

# Program name and version
!include "$(SRCROOT)\version.sh"

########################### Basic vars referring to directories, both in
########################### the source and build trees.

LISP=$(SRCROOT)\lisp
LIB_SRC=$(SRCROOT)\lib-src
NT=$(SRCROOT)\nt
SRC=$(SRCROOT)\src
ETC=$(SRCROOT)\etc

BLDLIB_SRC=$(BLDROOT)\lib-src
BLDNT=$(BLDROOT)\nt
OUTDIR=$(BLDNT)\obj
BLDSRC=$(BLDROOT)\src

# This appears in the dependency file
LWLIB_SRCDIR=$(SRCROOT)\lwlib

########################### Process the config.inc options.

!if !defined(INFODOCK)
INFODOCK=0
!endif
!if !defined(MULE)
MULE=0
!endif
!if !defined(HAVE_MS_WINDOWS)
HAVE_MS_WINDOWS=1
!endif
!if !defined(HAVE_XPM)
HAVE_XPM=0
!endif
!if !defined(HAVE_PNG)
HAVE_PNG=0
!endif
!if !defined(HAVE_ZLIB)
HAVE_ZLIB=$(HAVE_PNG)
!endif
!if !defined(HAVE_TIFF)
HAVE_TIFF=0
!endif
!if !defined(HAVE_JPEG)
HAVE_JPEG=0
!endif
!if !defined(HAVE_XFACE)
HAVE_XFACE=0
!endif
!if !defined(HAVE_GIF)
HAVE_GIF=1
!endif
!if !defined(HAVE_GTK)
HAVE_GTK=0
!endif
!if !defined(HAVE_MENUBARS)
HAVE_MENUBARS=1
!endif
!if !defined(HAVE_SCROLLBARS)
HAVE_SCROLLBARS=1
!endif
!if !defined(HAVE_TOOLBARS)
HAVE_TOOLBARS=$(HAVE_XPM)
!endif
!if !defined(HAVE_DIALOGS)
HAVE_DIALOGS=1
!endif
!if !defined(HAVE_NATIVE_SOUND)
HAVE_NATIVE_SOUND=1
!endif
!if !defined(HAVE_WIDGETS)
HAVE_WIDGETS=1
!endif
!if !defined(OPTIMIZED_BUILD)
OPTIMIZED_BUILD=1
!endif
!if !defined(USE_FASTCALL)
# #### Change to 1 when I check in the ws with support for fastcall
USE_FASTCALL=0
!endif
!if !defined(PROFILE_SUPPORT)
PROFILE_SUPPORT=0
!endif
!if !defined(DEBUG_XEMACS)
DEBUG_XEMACS=0
!endif
!if !defined(SUPPORT_EDIT_AND_CONTINUE)
SUPPORT_EDIT_AND_CONTINUE=0
!endif

!if !defined(ERROR_CHECK_ALL)
!if "$(emacs_is_beta)" != ""
ERROR_CHECK_ALL=1
!else
ERROR_CHECK_ALL=0
!endif
!endif

!if !defined(CPLUSPLUS_COMPILE)
!if $(ERROR_CHECK_ALL)
CPLUSPLUS_COMPILE=1
!else
CPLUSPLUS_COMPILE=0
!endif
!endif

!if !defined(USE_KKCC)
USE_KKCC=0
!endif
!if !defined(USE_UNION_TYPE)
USE_UNION_TYPE=0
!endif
!if !defined(QUICK_BUILD)
QUICK_BUILD=0
!endif
!if !defined(VERBOSECC)
VERBOSECC=0
!endif
!if !defined(DEPEND)
DEPEND=0
!endif
!if !defined(USE_PORTABLE_DUMPER)
USE_PORTABLE_DUMPER=1
!endif
!if !defined(USE_MINITAR)
USE_MINITAR=$(HAVE_ZLIB)
!endif

# A little bit of adhockery. Default to use system malloc and
# DLL version of the C runtime library when using portable
# dumping. These are the optimal settings.
!if !defined(USE_SYSTEM_MALLOC)
USE_SYSTEM_MALLOC=$(USE_PORTABLE_DUMPER)
!endif
!if !defined(USE_CRTDLL)
USE_CRTDLL=$(USE_PORTABLE_DUMPER)
!endif

########################### Check for incompatible options.

CONFIG_ERROR=0
!if $(INFODOCK) && !exist("..\..\Infodock.rules")
!message Cannot build InfoDock without InfoDock sources
CONFIG_ERROR=1
!endif
!if !$(USE_PORTABLE_DUMPER) && $(USE_SYSTEM_MALLOC)
!message Cannot use system allocator when dumping old way, use portable dumper.
CONFIG_ERROR=1
!endif
!if !$(USE_PORTABLE_DUMPER) && $(USE_CRTDLL)
!message Cannot use C runtime DLL when dumping old way, use portable dumper.
CONFIG_ERROR=1
!endif
!if !$(USE_SYSTEM_MALLOC) && $(USE_CRTDLL)
!message GNU malloc currently cannot be used with CRT DLL.
!message [[[Developer note: If you want to fix it, read Q112297 first]]]  ####
CONFIG_ERROR=1
!endif
!if !$(HAVE_MS_WINDOWS) && !$(HAVE_GTK)
!message Please specify at least one HAVE_MS_WINDOWS=1 and/or HAVE_GTK=1
CONFIG_ERROR=1
!endif
!if $(HAVE_MS_WINDOWS) && $(HAVE_GTK) && !defined(GTK_DIR)
!message Please specify root directory for your GTK installation: GTK_DIR=path
CONFIG_ERROR=1
!endif
!if $(HAVE_MS_WINDOWS) && $(HAVE_XPM) && !defined(XPM_DIR)
!message Please specify root directory for your XPM installation: XPM_DIR=path
CONFIG_ERROR=1
!endif
!if $(HAVE_MS_WINDOWS) && $(HAVE_XPM) && defined(XPM_DIR) && !exist("$(XPM_DIR)\lib\Xpm.lib")
!message Specified XPM directory does not contain "$(XPM_DIR)\lib\Xpm.lib"
CONFIG_ERROR=1
!endif
!if $(HAVE_MS_WINDOWS) && $(HAVE_PNG) && !defined(PNG_DIR)
!message Please specify root directory for your PNG installation: PNG_DIR=path
CONFIG_ERROR=1
!endif
!if $(HAVE_MS_WINDOWS) && $(HAVE_PNG) && defined(PNG_DIR) && !exist("$(PNG_DIR)\libpng.lib")
!message Specified PNG directory does not contain "$(PNG_DIR)\libpng.lib"
CONFIG_ERROR=1
!endif
!if $(HAVE_MS_WINDOWS) && $(HAVE_PNG) && !defined(ZLIB_DIR)
!message Please specify root directory for your ZLIB installation: ZLIB_DIR=path
CONFIG_ERROR=1
!endif
!if $(HAVE_MS_WINDOWS) && $(HAVE_PNG) && defined(ZLIB_DIR) && !exist("$(ZLIB_DIR)\zlib.lib")
!message Specified ZLIB directory does not contain "$(ZLIB_DIR)\zlib.lib"
CONFIG_ERROR=1
!endif
!if $(HAVE_MS_WINDOWS) && $(HAVE_TIFF) && !defined(TIFF_DIR)
!message Please specify root directory for your TIFF installation: TIFF_DIR=path
CONFIG_ERROR=1
!endif
!if $(HAVE_MS_WINDOWS) && $(HAVE_TIFF) && !exist("$(TIFF_DIR)\libtiff\libtiff.lib")
!message Specified TIFF directory does not contain "$(TIFF_DIR)\libtiff\libtiff.lib"
CONFIG_ERROR=1
!endif
!if $(HAVE_MS_WINDOWS) && $(HAVE_JPEG) && !defined(JPEG_DIR)
!message Please specify root directory for your JPEG installation: JPEG_DIR=path
CONFIG_ERROR=1
!endif
!if $(HAVE_MS_WINDOWS) && $(HAVE_JPEG) && !exist("$(JPEG_DIR)\libjpeg.lib")
!message Specified JPEG directory does not contain "$(JPEG_DIR)\libjpeg.lib"
CONFIG_ERROR=1
!endif
!if $(HAVE_MS_WINDOWS) && $(HAVE_XFACE) && !defined(COMPFACE_DIR)
!message Please specify root directory for your COMPFACE installation: COMPFACE_DIR=path
CONFIG_ERROR=1
!endif
!if $(HAVE_MS_WINDOWS) && $(HAVE_XFACE) && !exist("$(COMPFACE_DIR)\libcompface.lib")
!message Specified COMPFACE directory does not contain "$(COMPFACE_DIR)\libcompface.lib"
CONFIG_ERROR=1
!endif
!if $(HAVE_MS_WINDOWS) && $(HAVE_TOOLBARS) && !$(HAVE_XPM)
!message Toolbars require XPM support
CONFIG_ERROR=1
!endif
!if $(CONFIG_ERROR)
!error Configuration error(s) found
!endif

########################### Set version strings.

!if $(INFODOCK)
INFODOCK_VERSION_STRING=$(infodock_major_version).$(infodock_minor_version).$(infodock_build_version)
PROGRAM_DEFINES=-DINFODOCK 					\
	-DPATH_VERSION=\"$(INFODOCK_VERSION_STRING)\"		\
	-DPATH_PROGNAME=\"infodock\" 				\
	-DEMACS_PROGNAME=\"infodock\"				\
	-DEMACS_VERSION=\"$(INFODOCK_VERSION_STRING)\"		\
	-DINFODOCK_MAJOR_VERSION=$(infodock_major_version)	\
	-DINFODOCK_MINOR_VERSION=$(infodock_minor_version)	\
	-DINFODOCK_BUILD_VERSION=$(infodock_build_version)
!else
XEMACS_VERSION_STRING=$(emacs_major_version).$(emacs_minor_version)
!if "$(emacs_beta_version)" != ""
!if "$(emacs_is_beta)" != ""
XEMACS_VERSION_STRING=$(XEMACS_VERSION_STRING)-b$(emacs_beta_version)
!else
XEMACS_VERSION_STRING=$(XEMACS_VERSION_STRING).$(emacs_beta_version)
!endif
!endif
PROGRAM_DEFINES=						\
	-DPATH_VERSION=\"$(XEMACS_VERSION_STRING)\"		\
	-DPATH_PROGNAME=\"xemacs\"				\
	-DEMACS_VERSION=\"$(XEMACS_VERSION_STRING)\"		\
	-DEMACS_PROGNAME=\"xemacs\"
!endif

########################### Set up installation and package directories.

!if !defined(INSTALL_DIR)
! if $(INFODOCK)
INSTALL_DIR=c:\Program Files\Infodock\Infodock-$(INFODOCK_VERSION_STRING)
! else
INSTALL_DIR=c:\Program Files\XEmacs\XEmacs-$(XEMACS_VERSION_STRING)
! endif
!endif
!if !defined(PACKAGE_PATH)
! if !defined(PACKAGE_PREFIX)
PACKAGE_PREFIX=c:\Program Files\XEmacs
! endif
! if $(MULE)
PACKAGE_PATH=~\.xemacs;;$(PACKAGE_PREFIX)\site-packages;$(PACKAGE_PREFIX)\mule-packages;$(PACKAGE_PREFIX)\xemacs-packages
! else
PACKAGE_PATH=~\.xemacs;;$(PACKAGE_PREFIX)\site-packages;$(PACKAGE_PREFIX)\xemacs-packages
! endif
!endif
PATH_PACKAGEPATH="$(PACKAGE_PATH:\=\\)"

!if $(INFODOCK)
PATH_PREFIX=../..
!else
PATH_PREFIX=..
!endif

PATH_DEFINES=-DPATH_PREFIX=\"$(PATH_PREFIX)\"

!if $(SEPARATE_BUILD)
PATH_DEFINES=-DPATH_LOADSEARCH=\"$(LISP:\=\\)\" -DPATH_DATA=\"$(ETC:\=\\)\"
!endif

########################### Determine system configuration.

!if !defined(OS)
OS=Windows_95/98
EMACS_CONFIGURATION=i586-pc-win32
!else if "$(PROCESSOR_ARCHITECTURE)" == "x86"
EMACS_CONFIGURATION=i586-pc-win32
!else if "$(PROCESSOR_ARCHITECTURE)" == "MIPS"
EMACS_CONFIGURATION=mips-pc-win32
!else if "$(PROCESSOR_ARCHITECTURE)" == "ALPHA"
EMACS_CONFIGURATION=alpha-pc-win32
!else if "$(PROCESSOR_ARCHITECTURE)" == "PPC"
EMACS_CONFIGURATION=ppc-pc-win32
!else
! error Unknown processor architecture type $(PROCESSOR_ARCHITECTURE)
!endif
STACK_TRACE_EYE_CATCHER=$(XEMACS_VERSION_STRING:.=_)
STACK_TRACE_EYE_CATCHER=xemacs_$(STACK_TRACE_EYE_CATCHER:-=_)_$(EMACS_CONFIGURATION:-=_)
PROGRAM_DEFINES=$(PROGRAM_DEFINES) -DSTACK_TRACE_EYE_CATCHER=$(STACK_TRACE_EYE_CATCHER)

########################### Determine includes/defines/object file for
########################### various options.

!if $(HAVE_MS_WINDOWS)
MSW_DEFINES=-DHAVE_MS_WINDOWS
MSW_INCLUDES=
MSW_LIBS=comctl32.lib

!if $(HAVE_XPM)
MSW_DEFINES=$(MSW_DEFINES) -DHAVE_XPM -DFOR_MSW
MSW_INCLUDES=$(MSW_INCLUDES) -I"$(XPM_DIR)" -I"$(XPM_DIR)\lib"
MSW_LIBS=$(MSW_LIBS) "$(XPM_DIR)\lib\Xpm.lib"
!endif
!if $(HAVE_GIF)
MSW_DEFINES=$(MSW_DEFINES) -DHAVE_GIF
MSW_GIF_OBJ=$(OUTDIR)\dgif_lib.obj $(OUTDIR)\gif_io.obj
!endif
!if $(HAVE_PNG)
MSW_DEFINES=$(MSW_DEFINES) -DHAVE_PNG
MSW_INCLUDES=$(MSW_INCLUDES) -I"$(PNG_DIR)" -I"$(ZLIB_DIR)"
MSW_LIBS=$(MSW_LIBS) "$(PNG_DIR)\libpng.lib" "$(ZLIB_DIR)\zlib.lib"
!endif
!if $(HAVE_TIFF)
MSW_DEFINES=$(MSW_DEFINES) -DHAVE_TIFF
MSW_INCLUDES=$(MSW_INCLUDES) -I"$(TIFF_DIR)\libtiff"
MSW_LIBS=$(MSW_LIBS) "$(TIFF_DIR)\libtiff\libtiff.lib"
!endif
!if $(HAVE_JPEG)
MSW_DEFINES=$(MSW_DEFINES) -DHAVE_JPEG
MSW_INCLUDES=$(MSW_INCLUDES) -I"$(JPEG_DIR)"
MSW_LIBS=$(MSW_LIBS) "$(JPEG_DIR)\libjpeg.lib"
!endif
!if $(HAVE_XFACE)
MSW_DEFINES=$(MSW_DEFINES) -DHAVE_XFACE
MSW_INCLUDES=$(MSW_INCLUDES) -I"$(COMPFACE_DIR)"
MSW_LIBS=$(MSW_LIBS) "$(COMPFACE_DIR)\libcompface.lib"
!endif
!if $(HAVE_ZLIB)
MSW_DEFINES=$(MSW_DEFINES) -DHAVE_ZLIB
MSW_INCLUDES=$(MSW_INCLUDES) -I"$(ZLIB_DIR)"
MSW_LIBS=$(MSW_LIBS) "$(ZLIB_DIR)\zlib.lib"
!endif
!if $(HAVE_MENUBARS)
MSW_DEFINES=$(MSW_DEFINES) -DHAVE_MENUBARS
MSW_MENUBAR_OBJ=$(OUTDIR)\menubar.obj $(OUTDIR)\menubar-msw.obj
!endif
!if $(HAVE_SCROLLBARS)
MSW_DEFINES=$(MSW_DEFINES) -DHAVE_SCROLLBARS
MSW_SCROLLBAR_OBJ=$(OUTDIR)\scrollbar.obj $(OUTDIR)\scrollbar-msw.obj
!endif
!if $(HAVE_TOOLBARS)
MSW_DEFINES=$(MSW_DEFINES) -DHAVE_TOOLBARS
MSW_TOOLBAR_OBJ=$(OUTDIR)\toolbar.obj $(OUTDIR)\toolbar-msw.obj
!endif
!if $(HAVE_WIDGETS)
MSW_DEFINES=$(MSW_DEFINES) -DHAVE_WIDGETS
!endif
!if $(HAVE_DIALOGS)
MSW_DEFINES=$(MSW_DEFINES) -DHAVE_DIALOGS
MSW_DIALOG_OBJ=$(OUTDIR)\dialog.obj $(OUTDIR)\dialog-msw.obj
!endif
!if $(HAVE_NATIVE_SOUND)
MSW_DEFINES=$(MSW_DEFINES) -DHAVE_NATIVE_SOUND
!endif

TEMACS_MSW_OBJS=\
	$(OUTDIR)\console-msw.obj \
	$(OUTDIR)\device-msw.obj \
	$(OUTDIR)\event-msw.obj \
	$(OUTDIR)\frame-msw.obj \
	$(OUTDIR)\glyphs-msw.obj \
	$(OUTDIR)\gui-msw.obj \
	$(OUTDIR)\objects-msw.obj \
	$(OUTDIR)\redisplay-msw.obj \
	$(OUTDIR)\select-msw.obj \
	$(OUTDIR)\dired-msw.obj \
	$(MSW_MENUBAR_OBJ) \
	$(MSW_SCROLLBAR_OBJ) \
	$(MSW_TOOLBAR_OBJ) \
	$(MSW_DIALOG_OBJ) \
	$(MSW_GIF_OBJ)

# end !if $(HAVE_MS_WINDOWS)
!endif

!if $(MULE)
MULE_DEFINES=-DMULE
TEMACS_MULE_OBJS=\
	$(OUTDIR)\mule-ccl.obj \
	$(OUTDIR)\mule-charset.obj \
	$(OUTDIR)\mule-coding.obj
!endif

!if $(DEBUG_XEMACS)
TEMACS_DEBUG_OBJS=$(OUTDIR)\debug.obj $(OUTDIR)\tests.obj
!endif

!if $(QUICK_BUILD)
QUICK_DEFINES=-DQUICK_BUILD
!endif

!if $(ERROR_CHECK_ALL)
ERROR_CHECK_DEFINES=-DERROR_CHECK_ALL
!endif

!if $(USE_UNION_TYPE)
UNION_DEFINES=-DUSE_UNION_TYPE
!endif

!if $(USE_PORTABLE_DUMPER)
DUMPER_DEFINES=-DPDUMP
TEMACS_DUMP_OBJS=$(OUTDIR)\dumper.obj
!else
TEMACS_DUMP_OBJS=$(OUTDIR)\unexnt.obj
!endif

!if $(USE_KKCC)
KKCC_DEFINES=-DUSE_KKCC
!endif

!if $(USE_SYSTEM_MALLOC)
MALLOC_DEFINES=-DSYSTEM_MALLOC
!else
MALLOC_DEFINES=-DGNU_MALLOC
TEMACS_ALLOC_OBJS=$(OUTDIR)\free-hook.obj $(OUTDIR)\gmalloc.obj \
	$(OUTDIR)\ntheap.obj $(OUTDIR)\vm-limit.obj
!endif

########################### Process options related to compilation.

#
# Compiler command echo control. Define VERBOSECC=1 to get verbose compilation.
#
!if $(VERBOSECC)
CCV=$(CC)
!else
CCV=@$(CC)
!endif

!if $(DEBUG_XEMACS)

# ---- Debugging support ----
! if $(SUPPORT_EDIT_AND_CONTINUE)
# support edit-and-continue
DEBUG_FLAGS_COMPILE=-ZI
# WARNING: There is a very good reason for -incremental:no, as it can cause
# all sorts of weird crashes in or after a pdump load.  We must allow
# incremental linking for edit-and-continue to work, however.
DEBUG_FLAGS_LINK=-debug:full
! else
DEBUG_FLAGS_COMPILE=-Zi
DEBUG_FLAGS_LINK=-debug:full -incremental:no
! endif
DEBUG_DEFINES=-DDEBUG_XEMACS -D_DEBUG 
#BROWSERFLAGS=-Fr -Fd$(OUTDIR)\temacs.pdb
BROWSERFLAGS=-Fr$*.sbr -Fd$(OUTDIR)\temacs.pdb

!else

# ---- No debugging support ----
DEBUG_FLAGS_COMPILE=
DEBUG_FLAGS_LINK=-incremental:no
DEBUG_DEFINES=
BROWSERFLAGS=

!endif

!if $(USE_CRTDLL)
! if $(DEBUG_XEMACS)
C_LIBFLAG=-MDd
LIBC_LIB=msvcrtd.lib
! else
C_LIBFLAG=-MD
LIBC_LIB=msvcrt.lib
! endif
!else
C_LIBFLAG=-ML
LIBC_LIB=libc.lib
!endif

!if $(OPTIMIZED_BUILD)
# -G5 means optimize for Pentium. (According to the code-optimization
# article, -GB is the same as -G5, and -G6, i.e. optimze for Pentium Pro,
# gains you little over -G5 for PPro's but causes big slowdowns for
# Pentiums.) -GF means put strings in read-only memory; -Gr means use
# fastcall convention.  Another possible addition: -Ob2 -- allows inlining
# of any function, not just those declared inline.  Potential code size
# increase, though.
#
# #### Note: fastcall doesn't actually seem to make any difference, at least
# not using the (admittedly unscientific) test of (hanoi 6).  The
# optimization article claims 1-2% improvement in both speed and size.
OPTFLAGS_FASTCALL =-O2 -G5 -Gr -Ob2
OPTFLAGS_CDECL    =-O2 -G5 -Ob2
!else
OPTFLAGS_FASTCALL =-Od
OPTFLAGS_CDECL    =-Od
!endif

!if $(USE_FASTCALL)
OPTFLAGS = $(OPTFLAGS_FASTCALL)
!else
OPTFLAGS = $(OPTFLAGS_CDECL)
!endif

!if $(PROFILE_SUPPORT)
PROFILE_FLAGS=-profile
!else
PROFILE_FLAGS=
!endif

!if $(CPLUSPLUS_COMPILE)
CPLUSPLUS_COMPILE_FLAGS=-TP
!else
CPLUSPLUS_COMPILE_FLAGS=
!endif

########################### Determine generic includes/defines/flags.

INCLUDES=$(MSW_INCLUDES) -I$(NT)\inc -I$(SRC)

DEFINES=$(MSW_DEFINES) $(MULE_DEFINES) $(UNION_DEFINES) \
	$(DUMPER_DEFINES) $(KKCC_DEFINES) $(MALLOC_DEFINES) \
	$(QUICK_DEFINES) $(ERROR_CHECK_DEFINES) $(DEBUG_DEFINES) \
	-DWIN32_LEAN_AND_MEAN -DWIN32_NATIVE -Demacs \
	-DHAVE_CONFIG_H $(PROGRAM_DEFINES) $(PATH_DEFINES)

CFLAGS_NO_OPT=-nologo -W3 -DSTRICT $(DEBUG_FLAGS_COMPILE)

CFLAGS_NO_LIB=$(CFLAGS_NO_OPT) $(OPTFLAGS)
CFLAGS=$(CFLAGS_NO_LIB) $(C_LIBFLAG)

CFLAGS_CDECL_NO_LIB=$(CFLAGS_NO_OPT) $(OPTFLAGS_CDECL)
CFLAGS_CDECL=$(CFLAGS_CDECL_NO_LIB) $(C_LIBFLAG)

########################### Determine flags for XEmacs object files.

# This may not exist
!if "$(emacs_beta_version)" != ""
!if "$(emacs_is_beta)" != ""
EMACS_BETA_VERSION=-DEMACS_BETA_VERSION=$(emacs_beta_version)
!else
EMACS_PATCH_LEVEL=-DEMACS_PATCH_LEVEL=$(emacs_beta_version)
!endif
!endif

TEMACS_CPP_FLAGS_NO_CFLAGS=-c $(CPLUSPLUS_COMPILE_FLAGS) \
 $(INCLUDES) $(DEFINES) \
 -DEMACS_MAJOR_VERSION=$(emacs_major_version) \
 -DEMACS_MINOR_VERSION=$(emacs_minor_version) \
 $(EMACS_BETA_VERSION) $(EMACS_PATCH_LEVEL) \
 -DXEMACS_CODENAME=\"$(xemacs_codename:&=and)\" \
 -DEMACS_CONFIGURATION=\"$(EMACS_CONFIGURATION)\" \
 -DPATH_PACKAGEPATH=\"$(PATH_PACKAGEPATH)\"
TEMACS_CPP_FLAGS=$(CFLAGS) $(TEMACS_CPP_FLAGS_NO_CFLAGS)
TEMACS_CPP_CDECL_FLAGS=$(CFLAGS_CDECL) $(TEMACS_CPP_FLAGS_NO_CFLAGS)

########################### Determine XEmacs object files.

TEMACS_OBJS= \
	$(TEMACS_MSW_OBJS)\
	$(TEMACS_MULE_OBJS)\
	$(TEMACS_DEBUG_OBJS)\
	$(TEMACS_ALLOC_OBJS)\
	$(TEMACS_DUMP_OBJS)\
	$(OUTDIR)\abbrev.obj \
	$(OUTDIR)\alloc.obj \
	$(OUTDIR)\alloca.obj \
	$(OUTDIR)\blocktype.obj \
	$(OUTDIR)\buffer.obj \
	$(OUTDIR)\bytecode.obj \
	$(OUTDIR)\callint.obj \
	$(OUTDIR)\casefiddle.obj \
	$(OUTDIR)\casetab.obj \
	$(OUTDIR)\chartab.obj \
	$(OUTDIR)\cmdloop.obj \
	$(OUTDIR)\cmds.obj \
	$(OUTDIR)\console-stream.obj \
	$(OUTDIR)\console.obj \
	$(OUTDIR)\data.obj \
	$(OUTDIR)\device.obj \
	$(OUTDIR)\dired.obj \
	$(OUTDIR)\doc.obj \
	$(OUTDIR)\doprnt.obj \
	$(OUTDIR)\dragdrop.obj \
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
	$(OUTDIR)\filemode.obj \
	$(OUTDIR)\floatfns.obj \
	$(OUTDIR)\fns.obj \
	$(OUTDIR)\font-lock.obj \
	$(OUTDIR)\frame.obj \
	$(OUTDIR)\general.obj \
	$(OUTDIR)\getloadavg.obj \
	$(OUTDIR)\glyphs.obj \
	$(OUTDIR)\glyphs-eimage.obj \
	$(OUTDIR)\glyphs-shared.obj \
	$(OUTDIR)\glyphs-widget.obj \
	$(OUTDIR)\gui.obj \
	$(OUTDIR)\gutter.obj \
	$(OUTDIR)\hash.obj \
	$(OUTDIR)\indent.obj \
	$(OUTDIR)\imgproc.obj \
	$(OUTDIR)\insdel.obj \
	$(OUTDIR)\intl.obj \
	$(OUTDIR)\intl-win32.obj \
	$(OUTDIR)\intl-encap-win32.obj \
	$(OUTDIR)\intl-auto-encap-win32.obj \
	$(OUTDIR)\keymap.obj \
	$(OUTDIR)\libinterface.obj \
	$(OUTDIR)\line-number.obj \
	$(OUTDIR)\lread.obj \
	$(OUTDIR)\lstream.obj \
	$(OUTDIR)\macros.obj \
	$(OUTDIR)\marker.obj \
	$(OUTDIR)\md5.obj \
	$(OUTDIR)\minibuf.obj \
#	#### Leave the next one out when integrating my working ws
	$(OUTDIR)\nt.obj \
	$(OUTDIR)\ntplay.obj \
	$(OUTDIR)\objects.obj \
	$(OUTDIR)\opaque.obj \
	$(OUTDIR)\print.obj \
	$(OUTDIR)\process.obj \
	$(OUTDIR)\process-nt.obj \
	$(OUTDIR)\profile.obj \
	$(OUTDIR)\rangetab.obj \
	$(OUTDIR)\realpath.obj \
	$(OUTDIR)\redisplay-output.obj \
	$(OUTDIR)\redisplay.obj \
	$(OUTDIR)\regex.obj \
	$(OUTDIR)\search.obj \
	$(OUTDIR)\select.obj \
	$(OUTDIR)\signal.obj \
	$(OUTDIR)\sound.obj \
	$(OUTDIR)\specifier.obj \
	$(OUTDIR)\strftime.obj \
	$(OUTDIR)\symbols.obj \
	$(OUTDIR)\syntax.obj \
	$(OUTDIR)\sysdep.obj \
	$(OUTDIR)\text.obj \
	$(OUTDIR)\tparam.obj \
	$(OUTDIR)\undo.obj \
	$(OUTDIR)\unicode.obj \
	$(OUTDIR)\widget.obj \
	$(OUTDIR)\window.obj \
	$(OUTDIR)\win32.obj

#########################################################################
##                           Implicit rules                            ##
#########################################################################

# Rules

.SUFFIXES:
.SUFFIXES:	.c .obj .texi .info

# nmake rule with batching:
#{$(SRC)}.c{$(OUTDIR)}.obj:
#	echo $< >> $(OUTDIR)\listfile.tmp

{$(SRC)}.c{$(OUTDIR)}.obj:
	$(CCV) $(TEMACS_CPP_FLAGS) $< -Fo$@ $(BROWSERFLAGS)

#########################################################################
##                     Subsidiary dependency rules                     ##
#########################################################################

###################### Include auto-generated dependencies.

#
# Whether to use dependency information generated by make-src-depend
#
!if $(DEPEND) && exist("$(SRC)\depend")
! if [if not exist $(OUTDIR)\nul mkdir "$(OUTDIR)"]
! endif
# This perl script used to be inline but that caused too many quoting problems
! if [perl $(NT)\make-nt-depend -s=$(SRC) -c=$(NT) -o=$(OUTDIR) < $(SRC)\depend > $(OUTDIR)\depend.tmp]
! endif
! include "$(OUTDIR)\depend.tmp"
!else
! if [echo   WARNING: Compiling without dependency information.]
! endif
!endif

###################### Build the output directory structure if not same as
###################### source.

!if $(SEPARATE_BUILD)
# #### `if not exist' does not like the quotes around file names.
# But what if one of them has spaces?  Fucking Microsoft!
! if [if not exist $(BLDROOT)\nul mkdir "$(BLDROOT)"]
! endif
! if [if not exist $(BLDLIB_SRC)\nul mkdir "$(BLDLIB_SRC)"]
! endif
! if [if not exist $(BLDNT)\nul mkdir "$(BLDNT)"]
! endif
! if [if not exist $(OUTDIR)\nul mkdir "$(OUTDIR)"]
! endif
! if [if not exist $(BLDSRC)\nul mkdir "$(BLDSRC)"]
! endif
# No point.
# ! if [if not exist "$(BLDROOT)\nul" mkdir "$(BLDROOT)"]
# ! endif
!endif

###################### Random .obj dependencies

# An explicit rule looks like this ($< works only in implicit rules):
# $(OUTDIR)\foo.obj: $(SRC)\foo.c
#	$(CCV) $(TEMACS_CPP_FLAGS) $(SRC)\$(@B).c -Fo$@ $(BROWSERFLAGS)

$(OUTDIR)\emacs.obj: $(SRCROOT)\version.sh

$(OUTDIR)\libinterface.obj: $(SRC)\libinterface.c
	$(CCV) $(TEMACS_CPP_CDECL_FLAGS) $(SRC)\$(@B).c -Fo$@ $(BROWSERFLAGS)

###################### Generated source files

$(OUTDIR)\nul:
	-@mkdir $(OUTDIR)

XEMACS_INCLUDES=\
 $(SRC)\config.h \
 $(SRC)\Emacs.ad.h \
 $(SRC)\paths.h

# #### Copying is cheap, we should just force these
$(SRC)\config.h:	$(SRC)\config.h.in
# #### ms must have hired monkeys to design their shell commands.  if
# #### you use xcopy to copy a file from one name to another, it
# #### PROMPTS you to see if you meant the second as a directory!  and
# #### no switch to mean "no of course, you idiots, it's a file!"
	@copy $(SRC)\config.h.in $(SRC)\config.h

#$(SRC)\Emacs.ad.h: $(SRCROOT)\etc\Emacs.ad
#	!"sed -f ad2c.sed < $(SRCROOT)\etc\Emacs.ad > $(SRC)\Emacs.ad.h"

#$(SRC)\paths.h: $(SRC)\paths.h.in
#	!"cd $(SRC); cp paths.h.in paths.h"

$(SRC)\Emacs.ad.h:	$(NT)\Emacs.ad.h
	set COPYCMD=/y
	@$(COPY) $(NT)\Emacs.ad.h $(SRC)

$(SRC)\paths.h:	$(NT)\paths.h
	set COPYCMD=/y
	@$(COPY) $(NT)\paths.h $(SRC)


###################### lastfile.lib

!if !$(USE_SYSTEM_MALLOC) || !$(USE_PORTABLE_DUMPER)

LASTFILE=$(OUTDIR)\lastfile.lib
LASTFILE_SRC=$(SRC)
LASTFILE_FLAGS=$(CFLAGS) $(CPLUSPLUS_COMPILE_FLAGS) $(INCLUDES) -Fo$@ -Fd$* -c
LASTFILE_OBJS= \
	$(OUTDIR)\lastfile.obj

$(LASTFILE): $(XEMACS_INCLUDES) $(LASTFILE_OBJS)
	link.exe -lib -nologo -out:$@ $(LASTFILE_OBJS)

$(OUTDIR)\lastfile.obj:	$(LASTFILE_SRC)\lastfile.c
	 $(CCV) $(LASTFILE_FLAGS) $(LASTFILE_SRC)\$(@B).c

!endif

###################### lib-src programs

LIB_SRC_DEFINES = -DHAVE_CONFIG_H -DWIN32_NATIVE

#
# Creating config.values to be used by config.el
#
CONFIG_VALUES = $(BLDLIB_SRC)\config.values
!if [echo Creating $(CONFIG_VALUES) && echo ;;; Do not edit this file!>$(CONFIG_VALUES)]
!endif
!if [echo blddir>>$(CONFIG_VALUES) && echo "$(BLDROOT:\=\\)">>$(CONFIG_VALUES)]
!endif
!if [echo srcdir>>$(CONFIG_VALUES) && echo "$(SRCROOT:\=\\)">>$(CONFIG_VALUES)]
!endif
!if [echo CC>>$(CONFIG_VALUES) && echo "$(CC:\=\\)">>$(CONFIG_VALUES)]
!endif
!if [echo CFLAGS>>$(CONFIG_VALUES) && echo "$(CFLAGS:\=\\)">>$(CONFIG_VALUES)]
!endif
!if [echo CPP>>$(CONFIG_VALUES) && echo "$(CPP:\=\\)">>$(CONFIG_VALUES)]
!endif
!if [echo CPPFLAGS>>$(CONFIG_VALUES) && echo "$(CPPFLAGS:\=\\)">>$(CONFIG_VALUES)]
!endif
!if [echo LISPDIR>>$(CONFIG_VALUES) && echo "\\$(LISP:\=\\)">>$(CONFIG_VALUES)]
!endif
# PATH_PACKAGEPATH is already a quoted string.
!if [echo PACKAGE_PATH>>$(CONFIG_VALUES) && echo $(PATH_PACKAGEPATH)>>$(CONFIG_VALUES)]
!endif

LINK_DEPENDENCY_ARGS = -Fe$@ -Fd$* $** -link $(DEBUG_FLAGS_LINK)
LINK_STANDARD_LIBRARY_ARGS = setargv.obj user32.lib wsock32.lib

LIB_SRC_CFLAGS = $(CFLAGS) -I$(LIB_SRC) -I$(SRC) $(LIB_SRC_DEFINES)

# Inferred rule
{$(LIB_SRC)}.c{$(BLDLIB_SRC)}.exe :
	$(CCV) $(LIB_SRC_CFLAGS) $(LINK_DEPENDENCY_ARGS) $(LINK_STANDARD_LIBRARY_ARGS)

# Individual dependencies
ETAGS_DEPS = $(LIB_SRC)/getopt.c $(LIB_SRC)/getopt1.c $(SRC)/regex.c
$(BLDLIB_SRC)/etags.exe : $(LIB_SRC)/etags.c $(ETAGS_DEPS)
	$(CCV) $(LIB_SRC_CFLAGS) $(LINK_DEPENDENCY_ARGS) -stack:0x800000 $(LINK_STANDARD_LIBRARY_ARGS)

$(BLDLIB_SRC)/movemail.exe : $(LIB_SRC)/movemail.c $(LIB_SRC)/pop.c $(ETAGS_DEPS)

# Minitar uses zlib so just use cdecl to simplify things
$(BLDLIB_SRC)/minitar.exe : $(NT)/minitar.c
	$(CCV) -I"$(ZLIB_DIR)" $(LIB_SRC_DEFINES) $(CFLAGS_CDECL_NO_LIB) $(LINK_DEPENDENCY_ARGS) "$(ZLIB_DIR)\zlib.lib"

LIB_SRC_TOOLS = \
	$(BLDLIB_SRC)/etags.exe		\
	$(BLDLIB_SRC)/hexl.exe		\
	$(BLDLIB_SRC)/i.exe		\
	$(BLDLIB_SRC)/winclient.exe	\
	$(BLDLIB_SRC)/make-docfile.exe	\
	$(BLDLIB_SRC)/mmencode.exe	\
	$(BLDLIB_SRC)/movemail.exe	\
	$(BLDLIB_SRC)/sorted-doc.exe	\
	$(BLDLIB_SRC)/wakeup.exe
!if $(USE_MINITAR)
LIB_SRC_TOOLS = \
	$(LIB_SRC_TOOLS) \
	$(BLDLIB_SRC)/minitar.exe
!endif
!if $(USE_PORTABLE_DUMPER)
LIB_SRC_TOOLS = \
	$(XEMACS_INCLUDES) \
	$(BLDLIB_SRC)/make-dump-id.exe \
	$(LIB_SRC_TOOLS)
!endif

###################### Building the info files

!if !defined(MAKEINFO)
MAKEINFO=$(XEMACS_BATCH_PACKAGES) -l texinfmt -f batch-texinfo-format
!endif

MANDIR = $(SRCROOT)\man
INFODIR = $(SRCROOT)\info
INFO_FILES= \
	$(INFODIR)\cl.info \
	$(INFODIR)\custom.info \
	$(INFODIR)\emodules.info \
	$(INFODIR)\external-widget.info \
	$(INFODIR)\info.info \
	$(INFODIR)\standards.info \
	$(INFODIR)\term.info \
	$(INFODIR)\termcap.info \
	$(INFODIR)\texinfo.info \
	$(INFODIR)\widget.info \
	$(INFODIR)\xemacs-faq.info \
	$(INFODIR)\xemacs.info \
	$(INFODIR)\lispref.info \
	$(INFODIR)\new-users-guide.info \
	$(INFODIR)\internals.info

{$(MANDIR)}.texi{$(INFODIR)}.info:
	cd $(MANDIR)
	$(MAKEINFO) $**

XEMACS_SRCS = \
	$(MANDIR)\xemacs\abbrevs.texi \
	$(MANDIR)\xemacs\basic.texi \
	$(MANDIR)\xemacs\buffers.texi \
	$(MANDIR)\xemacs\building.texi \
	$(MANDIR)\xemacs\calendar.texi \
	$(MANDIR)\xemacs\cmdargs.texi \
	$(MANDIR)\xemacs\custom.texi \
	$(MANDIR)\xemacs\display.texi \
	$(MANDIR)\xemacs\entering.texi \
	$(MANDIR)\xemacs\files.texi \
	$(MANDIR)\xemacs\fixit.texi \
	$(MANDIR)\xemacs\frame.texi \
	$(MANDIR)\xemacs\glossary.texi \
	$(MANDIR)\xemacs\gnu.texi \
	$(MANDIR)\xemacs\help.texi \
	$(MANDIR)\xemacs\indent.texi \
	$(MANDIR)\xemacs\keystrokes.texi \
	$(MANDIR)\xemacs\killing.texi \
	$(MANDIR)\xemacs\m-x.texi \
	$(MANDIR)\xemacs\major.texi \
	$(MANDIR)\xemacs\mark.texi \
	$(MANDIR)\xemacs\menus.texi \
	$(MANDIR)\xemacs\mini.texi \
	$(MANDIR)\xemacs\misc.texi \
	$(MANDIR)\xemacs\mouse.texi \
	$(MANDIR)\xemacs\mule.texi \
	$(MANDIR)\xemacs\new.texi \
	$(MANDIR)\xemacs\packages.texi \
	$(MANDIR)\xemacs\picture.texi \
	$(MANDIR)\xemacs\programs.texi \
	$(MANDIR)\xemacs\reading.texi \
	$(MANDIR)\xemacs\regs.texi \
	$(MANDIR)\xemacs\search.texi \
	$(MANDIR)\xemacs\sending.texi \
	$(MANDIR)\xemacs\startup.texi \
	$(MANDIR)\xemacs\text.texi \
	$(MANDIR)\xemacs\trouble.texi \
	$(MANDIR)\xemacs\undo.texi \
	$(MANDIR)\xemacs\windows.texi \
	$(MANDIR)\xemacs\xemacs.texi

LISPREF_SRCS = \
	$(MANDIR)\lispref\abbrevs.texi \
	$(MANDIR)\lispref\annotations.texi \
	$(MANDIR)\lispref\back.texi \
	$(MANDIR)\lispref\backups.texi \
	$(MANDIR)\lispref\buffers.texi \
	$(MANDIR)\lispref\building.texi \
	$(MANDIR)\lispref\commands.texi \
	$(MANDIR)\lispref\compile.texi \
	$(MANDIR)\lispref\consoles-devices.texi \
	$(MANDIR)\lispref\control.texi \
	$(MANDIR)\lispref\customize.texi \
	$(MANDIR)\lispref\databases.texi \
	$(MANDIR)\lispref\debugging.texi \
	$(MANDIR)\lispref\dialog.texi \
	$(MANDIR)\lispref\display.texi \
	$(MANDIR)\lispref\dragndrop.texi \
	$(MANDIR)\lispref\edebug-inc.texi \
	$(MANDIR)\lispref\edebug.texi \
	$(MANDIR)\lispref\errors.texi \
	$(MANDIR)\lispref\eval.texi \
	$(MANDIR)\lispref\extents.texi \
	$(MANDIR)\lispref\faces.texi \
	$(MANDIR)\lispref\files.texi \
	$(MANDIR)\lispref\frames.texi \
	$(MANDIR)\lispref\functions.texi \
	$(MANDIR)\lispref\glyphs.texi \
	$(MANDIR)\lispref\hash-tables.texi \
	$(MANDIR)\lispref\help.texi \
	$(MANDIR)\lispref\hooks.texi \
	$(MANDIR)\lispref\index.texi \
	$(MANDIR)\lispref\internationalization.texi \
	$(MANDIR)\lispref\intro.texi \
	$(MANDIR)\lispref\keymaps.texi \
	$(MANDIR)\lispref\ldap.texi \
	$(MANDIR)\lispref\lispref.texi \
	$(MANDIR)\lispref\lists.texi \
	$(MANDIR)\lispref\loading.texi \
	$(MANDIR)\lispref\locals.texi \
	$(MANDIR)\lispref\macros.texi \
	$(MANDIR)\lispref\maps.texi \
	$(MANDIR)\lispref\markers.texi \
	$(MANDIR)\lispref\menus.texi \
	$(MANDIR)\lispref\minibuf.texi \
	$(MANDIR)\lispref\modes.texi \
	$(MANDIR)\lispref\mouse.texi \
	$(MANDIR)\lispref\mule.texi \
	$(MANDIR)\lispref\numbers.texi \
	$(MANDIR)\lispref\objects.texi \
	$(MANDIR)\lispref\os.texi \
	$(MANDIR)\lispref\positions.texi \
	$(MANDIR)\lispref\processes.texi \
	$(MANDIR)\lispref\range-tables.texi \
	$(MANDIR)\lispref\scrollbars.texi \
	$(MANDIR)\lispref\searching.texi \
	$(MANDIR)\lispref\sequences.texi \
	$(MANDIR)\lispref\specifiers.texi \
	$(MANDIR)\lispref\streams.texi \
	$(MANDIR)\lispref\strings.texi \
	$(MANDIR)\lispref\symbols.texi \
	$(MANDIR)\lispref\syntax.texi \
	$(MANDIR)\lispref\text.texi \
	$(MANDIR)\lispref\tips.texi \
	$(MANDIR)\lispref\toolbar.texi \
	$(MANDIR)\lispref\tooltalk.texi \
	$(MANDIR)\lispref\variables.texi \
	$(MANDIR)\lispref\windows.texi \
	$(MANDIR)\lispref\x-windows.texi

INTERNALS_SRCS = \
	$(MANDIR)\internals\internals.texi \
	$(MANDIR)\internals\index.texi

NEW_USERS_GUIDE_SRCS = \
	$(MANDIR)\new-users-guide\custom1.texi \
	$(MANDIR)\new-users-guide\custom2.texi \
	$(MANDIR)\new-users-guide\edit.texi \
	$(MANDIR)\new-users-guide\enter.texi \
	$(MANDIR)\new-users-guide\files.texi \
	$(MANDIR)\new-users-guide\help.texi \
	$(MANDIR)\new-users-guide\modes.texi \
	$(MANDIR)\new-users-guide\new-users-guide.texi \
	$(MANDIR)\new-users-guide\region.texi \
	$(MANDIR)\new-users-guide\search.texi \
	$(MANDIR)\new-users-guide\xmenu.texi

$(INFODIR)\xemacs.info: $(XEMACS_SRCS)
	cd $(MANDIR)\xemacs
	$(MAKEINFO) xemacs.texi


$(INFODIR)\lispref.info: $(LISPREF_SRCS)
	cd $(MANDIR)\lispref
	$(MAKEINFO) lispref.texi

$(INFODIR)\internals.info: $(INTERNALS_SRCS)
	cd $(MANDIR)\internals
	$(MAKEINFO) internals.texi

$(INFODIR)\new-users-guide.info: $(NEW_USERS_GUIDE_SRCS)
	cd $(MANDIR)\new-users-guide
	$(MAKEINFO) new-users-guide.texi

info:	makeinfo-test $(INFO_FILES)

makeinfo-test:
	@<<makeinfo_test.bat
@echo off
if exist "$(MAKEINFO)" goto test_done
@$(XEMACS_BATCH_PACKAGES) -eval "(condition-case nil (require (quote texinfo)) (t (kill-emacs 1)))"
@if not errorlevel 1 goto suggest_makeinfo
@echo XEmacs 'info' cannot be built!
@echo Install XEmacs package 'texinfo' (see README.packages).
:suggest_makeinfo
@echo Consider specifying path to 'makeinfo' in config.inc.
@echo as this will build the info docs much faster than XEmacs using 'texinfo'.
@if errorlevel 1 exit 1
:test_done
<<NOKEEP

########################### Create the Installation file

$(BLDROOT)\Installation::	installation

installation::
	@echo OS version:>$(BLDROOT)\Installation
	@ver >> $(BLDROOT)\Installation
	@type >> $(BLDROOT)\Installation <<
!if defined(OS)
OS: $(OS)
!endif

XEmacs $(XEMACS_VERSION_STRING) $(xemacs_codename:"=) $(xemacs_extra_name:"=) configured for '$(EMACS_CONFIGURATION)'.

  Building XEmacs using "$(MAKE:\=\\)".
  Building XEmacs using make flags "$(MAKEFLAGS)".
  Building XEmacs in source tree "$(SRCROOT:\=\\)".
!if $(SEPARATE_BUILD)
  Building XEmacs into compiled tree "$(BLDROOT:\=\\)".
!endif
!if defined(CCV)
  For src, using compiler "$(CC) $(TEMACS_CPP_FLAGS)".
  For lib-src, using compiler "$(CC) $(LIB_SRC_CFLAGS)".
!endif
!if $(CPLUSPLUS_COMPILE)
  Compiling as C++.
!endif
  Installing XEmacs in "$(INSTALL_DIR:\=\\)".
  Package path is $(PATH_PACKAGEPATH).
!if $(INFODOCK)
  Building InfoDock.
!endif
!if $(HAVE_MS_WINDOWS)
  Compiling in support for Microsoft Windows native GUI.
!endif
!if $(MULE)
  Compiling in international (MULE) support.
!endif
!if $(HAVE_GTK)
  --------------------------------------------------------------------
  NOTE: You specified HAVE_GTK=1, but we are compiling WITHOUT GTK support.
  NOTE: gtk-xemacs is not currently supported on MS Windows (mingw or msvc).
  NOTE: Yes, we know that gtk has been ported to native MS Windows, but
  NOTE: XEmacs is not yet ready to use that port.
  --------------------------------------------------------------------
!endif
!if $(HAVE_XPM)
  Compiling in support for XPM images.
!else
  --------------------------------------------------------------------
  NOTE: Compiling without XPM support.
  NOTE: You should strongly consider installing XPM.
  NOTE: Otherwise toolbars and other graphics will look suboptimal.
  NOTE: (a copy may be found in ftp://ftp.xemacs.org/pub/xemacs/aux)
  --------------------------------------------------------------------
!endif
!if $(HAVE_GIF)
  Compiling in support for GIF images.
!endif
!if $(HAVE_PNG)
  Compiling in support for PNG images.
!else
  --------------------------------------------------------------------
  NOTE: Compiling without PNG image support.
  NOTE: You should strongly consider installing the PNG libraries.
  NOTE: Otherwise certain images and glyphs may not display.
  NOTE: (a copy may be found in ftp://ftp.xemacs.org/pub/xemacs/aux
  --------------------------------------------------------------------
!endif
!if $(HAVE_TIFF)
  Compiling in support for TIFF images.
!endif
!if $(HAVE_JPEG)
  Compiling in support for JPEG images.
!endif
!if $(HAVE_XFACE)
  Compiling in support for X-Face message headers.
!endif
!if $(HAVE_ZLIB)
  Compiling in support for GZIP compression/decompression.
!endif
!if $(HAVE_TOOLBARS)
  Compiling in support for toolbars.
!endif
!if $(HAVE_DIALOGS)
  Compiling in support for dialogs.
!endif
!if $(HAVE_WIDGETS)
  Compiling in support for widgets.
!endif
!if $(HAVE_NATIVE_SOUND)
  Compiling in support for native sounds.
!endif
!if $(USE_UNION_TYPE)
  Using union type for Lisp object storage.
  NOTE: ---------------------------------------------------------
  NOTE: This tends to trigger compiler bugs, especially when combined
  NOTE: with MULE and ERROR_CHECKING.  Crashes in pdump have recently
  NOTE: been observed using Visual C++ in combination with union type,
  NOTE: MULE, and ERROR_CHECKING.
  NOTE: ---------------------------------------------------------
!endif
!if $(USE_PORTABLE_DUMPER)
  Using portable dumper.
!endif
!if $(USE_SYSTEM_MALLOC)
  Using system malloc.
!endif
!if $(USE_CRTDLL)
  Using DLL version of C runtime library.
!endif
!if $(ERROR_CHECK_ALL)
  Compiling in extra internal error-checking.
  NOTE: ---------------------------------------------------------
  NOTE: Compiling in support for runtime error-checking.
  NOTE: XEmacs will run noticeably more slowly as a result.
  NOTE: Error-checking is on by default for XEmacs beta releases.
  NOTE: ---------------------------------------------------------
!endif
!if $(DEBUG_XEMACS)
  Compiling in debugging support (no slowdown).
!endif
!if $(OPTIMIZED_BUILD)
  Compiling with optimization.
!endif
!if $(QUICK_BUILD)
  Disabling non-essential build actions.  Use with care!
!endif
!if $(USE_KKCC)
  Using new experimental GC algorithms.
!endif
<<NOKEEP
	@echo --------------------------------------------------------------------
	@type $(BLDROOT)\Installation
	@echo --------------------------------------------------------------------

#########################################################################
##                     Primary rebuilding process                      ##
#########################################################################

########################### Definitions for linking temacs.exe

!if !$(USE_PORTABLE_DUMPER)
TEMACS_ENTRYPOINT=-entry:_start
!else
TEMACS_ENTRYPOINT=-entry:mainCRTStartup
!endif

TEMACS_BROWSE=$(BLDSRC)\temacs.bsc
TEMACS_LIBS=$(LASTFILE) $(MSW_LIBS) \
 oldnames.lib kernel32.lib user32.lib gdi32.lib comdlg32.lib advapi32.lib \
 shell32.lib wsock32.lib netapi32.lib winmm.lib winspool.lib ole32.lib \
 mpr.lib uuid.lib imm32.lib $(LIBC_LIB)
TEMACS_LFLAGS=-nologo $(LIBRARIES) $(DEBUG_FLAGS_LINK) \
 -base:0x1000000 -stack:0x800000 $(TEMACS_ENTRYPOINT) -subsystem:windows \
 -pdb:$(BLDSRC)\temacs.pdb -map:$(BLDSRC)\temacs.map \
 -heap:0x00100000 -nodefaultlib $(PROFILE_FLAGS) setargv.obj

########################### Definitions for running temacs.exe/xemacs.exe

RAW_EXE=$(BLDSRC)\temacs.exe
DUMP_TARGET = $(BLDSRC)\xemacs.exe
DO_TEMACS = "$(BLDLIB_SRC)\i" "$(RAW_EXE)"
DO_XEMACS = "$(BLDLIB_SRC)\i" "$(DUMP_TARGET)"

BATCH = -no-packages -batch
BATCH_PACKAGES = -vanilla -batch
TEMACS_BATCH = $(DO_TEMACS) -nd $(BATCH)
XEMACS_BATCH = $(DO_XEMACS) $(BATCH)
XEMACS_BATCH_PACKAGES = $(DO_XEMACS) $(BATCH_PACKAGES)
temacs_loadup_args = -l $(LISP)/loadup.el
dump_temacs_args   = $(temacs_loadup_args) dump
run_temacs_args = $(temacs_loadup_args) run-temacs
dump_temacs = $(TEMACS_BATCH) $(dump_temacs_args)

########################### Build rules

# use this rule to build the complete system
all:	installation $(OUTDIR)\nul $(LASTFILE) \
	$(LIB_SRC_TOOLS) update-elc $(DUMP_TARGET) \
	update-elc-2 $(LISP)/finder-inf.el load-shadows info

$(TEMACS_BROWSE): $(TEMACS_OBJS)
	@dir /b/s $(OUTDIR)\*.sbr > $(OUTDIR)\bscmake.tmp
	bscmake -nologo -o$(TEMACS_BROWSE) @$(OUTDIR)\bscmake.tmp
	-$(DEL) $(OUTDIR)\bscmake.tmp

# dump-id.c file that contains the dump id

$(OUTDIR)\dump-id.obj : $(BLDSRC)\dump-id.c
	$(CCV) $(TEMACS_CPP_FLAGS) $(BLDSRC)\$(@B).c -Fo$@ $(BROWSERFLAGS)

$(BLDSRC)\dump-id.c : $(BLDLIB_SRC)/make-dump-id.exe $(TEMACS_OBJS)
	cd $(BLDSRC)
	$(BLDLIB_SRC)\make-dump-id.exe 

$(OUTDIR)\temacs.res: $(NT)\xemacs.rc
	cd $(NT)
	rc -Fo$@ xemacs.rc

!if $(USE_PORTABLE_DUMPER)
TEMACS_DUMP_DEP = $(OUTDIR)\dump-id.obj
!else
TEMACS_DUMP_DEP = $(OUTDIR)\temacs.res
!endif

$(RAW_EXE): $(TEMACS_OBJS) $(LASTFILE) $(TEMACS_DUMP_DEP)
	link.exe @<<
  $(TEMACS_LFLAGS) -out:$@ $(TEMACS_OBJS) $(TEMACS_DUMP_DEP) $(TEMACS_LIBS)
<<

!if $(DEBUG_XEMACS)
$(RAW_EXE): $(TEMACS_BROWSE)
!endif

# Rebuild docfile target

DOC=$(BLDLIB_SRC)\DOC

docfile ::
	if exist $(DOC) $(DEL) $(DOC)
docfile :: $(DOC)

# We need to write the QUICK_BUILD stuff as-is (and not just have no
# dependencies for DOC) because DOC needs TEMACS_OBJS as dependencies to
# get $(**) right.  The `touch' is needed because of the way nmake
# calculates dependencies; see comments in src/Makefile.in.in.
$(DOC): $(BLDLIB_SRC)\make-docfile.exe $(BLDSRC)\NEEDTODUMP $(TEMACS_OBJS)
!if $(QUICK_BUILD)
	if not exist $(DOC) $(TEMACS_BATCH) -l $(LISP)\make-docfile.el -- -o $(DOC) -i $(SRCROOT)\site-packages @<<
$(**)
<<
	-touch $(DOC)
!else
	$(TEMACS_BATCH) -l $(LISP)\make-docfile.el -- -o $(DOC) -i $(SRCROOT)\site-packages @<<
$(**)
<<
!endif

update-elc: $(RAW_EXE)
	$(TEMACS_BATCH) -l $(LISP)\update-elc.el

## This file is touched by update-elc.el when redumping is necessary.
$(BLDSRC)\NEEDTODUMP:
	@echo >$(BLDSRC)\NEEDTODUMP

!if $(USE_PORTABLE_DUMPER)
$(DUMP_TARGET): $(NT)\xemacs.rc
!endif

# This rule dumps xemacs and then possibly spawns sub-make if PURESPACE
# requirements have changed.

$(DUMP_TARGET): $(DOC) $(RAW_EXE) $(BLDSRC)\NEEDTODUMP
	$(TEMACS_BATCH) -l $(LISP)\loadup.el dump
!if $(USE_PORTABLE_DUMPER)
	cd $(BLDSRC)
	rc -d INCLUDE_DUMP -Fo $(OUTDIR)\xemacs.res $(NT)\xemacs.rc
# Make the resource section read/write since almost all of it is the dump
# data which needs to be writable.  This avoids having to copy it.
	link.exe @<<
  $(TEMACS_LFLAGS) -section:.rsrc,rw -out:$(BLDSRC)\xemacs.exe $(TEMACS_OBJS) $(OUTDIR)\xemacs.res $(TEMACS_LIBS) $(OUTDIR)\dump-id.obj
<<
	-$(DEL) $(BLDSRC)\xemacs.dmp
!endif

## Update out-of-date .elcs, other than needed for dumping.
update-elc-2:
	$(XEMACS_BATCH) -no-autoloads -l update-elc-2.el -f batch-update-elc-2 $(LISP)

$(LISP)/finder-inf.el:
!if !$(QUICK_BUILD)
	@echo Building finder database ...
	$(XEMACS_BATCH)	-eval "(setq finder-compile-keywords-quiet t)" \
		-l finder -f finder-compile-keywords
	@echo Building finder database ...(done)
!endif

load-shadows:
!if !$(QUICK_BUILD)
	@echo Testing for Lisp shadows ...
	@$(XEMACS_BATCH) -f list-load-path-shadows
!endif

#########################################################################
##                          Other random crap                          ##
#########################################################################

########################### Automated tests

testdir = ../tests/automated
batch_test_emacs = $(BATCH_PACKAGES) -l $(testdir)/test-harness.el -f batch-test-emacs $(testdir)

check:
	cd $(BLDSRC)
	$(DO_XEMACS) $(batch_test_emacs)

check-temacs:
	cd $(BLDSRC)
	$(TEMACS_BATCH) $(run_temacs_args) $(batch_test_emacs)

check-features: all
	cd $(BLDSRC)
	$(XEMACS_BATCH) -l check-features.el


########################### Rebuilding TAGS

tags:
	@echo If you do not have a copy of etags around, then do 'make lib-src' first.
	@echo To make use of the tags file, put the following in your .emacs:
	@echo	(setq tag-table-alist
	@echo	  '(("$(SRCROOT:\=\\)\\" . "$(SRCROOT:\=\\)\\")))
	cd $(SRCROOT)
	-$(DEL) TAGS
	set PATH=lib-src;%PATH%
# we need to double ^, but only in one place, because (according to the
# nmake manual), a ^ is used to quote certain special characters such as
# backslash, but is treated literally within double quotes -- and notice
# carefully the occurrences of double quotes in the first line below!
	etags -a -r "/[ 	]*DEF\(VAR\|INE\)_[A-Z_]+[ 	]*([ 	]*\"\([^^\"]+\)\"/\2/" src\*.c src\*.h lwlib\*.c lwlib\*.h lib-src\*.c lib-src\*.h
	etags -a -l none -r "/^(def\(var\|un\|alias\|const\|macro\|subst\|struct\|face\|group\|custom\|ine-\(function\|compiler-macro\|[a-z-]+alias\)\)[ 	]+'?\([^ 	]+\)/\3/" $(LISP)\*.el $(LISP)\mule\*.el

########################### Install the system

# use this rule to install the system
install:	all
	cd $(NT)
	set COPYCMD=/y
	@echo Installing in $(INSTALL_DIR) ...
	@echo PlaceHolder > PlaceHolder
	@$(COPY) PROBLEMS "$(INSTALL_DIR)\"
	@$(COPY) PlaceHolder "$(INSTALL_DIR)\lock\"
	-$(DEL) "$(INSTALL_DIR)\lock\PlaceHolder"
	@$(COPY) $(BLDLIB_SRC)\*.exe "$(INSTALL_DIR)\$(EMACS_CONFIGURATION)\"
	@$(COPY) $(BLDLIB_SRC)\DOC "$(INSTALL_DIR)\$(EMACS_CONFIGURATION)"
	@$(COPY) $(CONFIG_VALUES) "$(INSTALL_DIR)\$(EMACS_CONFIGURATION)"
	@$(COPY) $(BLDSRC)\xemacs.exe "$(INSTALL_DIR)\$(EMACS_CONFIGURATION)"
	@$(COPYDIR) $(SRCROOT)\etc  "$(INSTALL_DIR)\etc\"
	@$(COPYDIR) $(SRCROOT)\info "$(INSTALL_DIR)\info\"
	@$(COPYDIR) $(SRCROOT)\lisp "$(INSTALL_DIR)\lisp\"
	@echo Making skeleton package tree in $(PACKAGE_PREFIX) ...
	@$(COPY) PlaceHolder "$(PACKAGE_PREFIX)\site-packages\"
	-$(DEL) "$(PACKAGE_PREFIX)\site-packages\PlaceHolder"
	@$(COPY) PlaceHolder "$(PACKAGE_PREFIX)\mule-packages\"
	-$(DEL) "$(PACKAGE_PREFIX)\mule-packages\PlaceHolder"
	@$(COPY) PlaceHolder "$(PACKAGE_PREFIX)\xemacs-packages\"
	-$(DEL) "$(PACKAGE_PREFIX)\xemacs-packages\PlaceHolder"
	-$(DEL) PlaceHolder

########################### clean

mostlyclean:
	-$(DEL) $(BLDROOT)\Installation
	-$(DEL) $(OUTDIR)\*.lib
	-$(DEL) $(OUTDIR)\*.obj
	-$(DEL) $(OUTDIR)\*.pdb
	-$(DEL) $(OUTDIR)\*.res
	-$(DEL) $(OUTDIR)\*.sbr
	-$(DEL) $(BLDSRC)\*.exe
	-$(DEL) $(BLDSRC)\*.dmp
	-$(DEL) $(BLDSRC)\*.map
	-$(DEL) $(BLDSRC)\*.pdb
	-$(DEL) $(BLDSRC)\NEEDTODUMP
	-$(DEL) $(BLDSRC)\dump-id.c
	-$(DEL) $(SRC)\*.bsc
	-$(DEL) $(BLDLIB_SRC)\*.exe
	-$(DEL) $(BLDLIB_SRC)\*.obj
	-$(DEL) $(BLDLIB_SRC)\*.pdb
	-$(DEL) $(BLDLIB_SRC)\*.res

versionclean:
	-$(DEL) $(BLDSRC)\xemacs.exe
	-$(DEL) $(BLDLIB_SRC)\DOC

clean: mostlyclean versionclean
	-$(DEL) $(SRCROOT)\TAGS
	-$(DEL) $(LISP)\auto-autoloads.el*
	-$(DEL) $(LISP)\mule\auto-autoloads.el*
	-$(DEL) $(LISP)\custom-load.el*
	-$(DEL) $(LISP)\mule\custom-load.el*

nicenclean: clean
	-$(DEL) $(NT)\*.bak
	-$(DEL) $(NT)\*.orig
	-$(DEL) $(NT)\*.rej
	-$(DEL) $(NT)\*.tmp
	-$(DEL) $(LIB_SRC)\*.bak
	-$(DEL) $(LIB_SRC)\*.orig
	-$(DEL) $(LIB_SRC)\*.rej
	-$(DEL) $(LIB_SRC)\*.tmp
	-$(DEL) $(SRC)\*.bak
	-$(DEL) $(SRC)\*.orig
	-$(DEL) $(SRC)\*.rej
	-$(DEL) $(SRC)\*.tmp
	-$(DEL) $(LISP)\*.bak
	-$(DEL) $(LISP)\*.orig
	-$(DEL) $(LISP)\*.rej
	-$(DEL) $(LISP)\*.tmp

# Convenience target.
# Reproducing the configuration is just a matter of copying, and if
# we use the same directory for Cygwin builds these must go.  We don't
# want to use distclean.
configclean:
	-$(DEL) $(SRC)\config.h
	-$(DEL) $(SRC)\paths.h
	-$(DEL) $(SRC)\Emacs.ad.h

## This is used in making a distribution.
## Do not use it on development directories!
distclean: nicenclean configclean
	-$(DEL) $(BLDLIB_SRC)\$(CONFIG_VALUES)
	-$(DEL) $(INFODIR)\*.info*
	-$(DEL) $(LISP)\*.elc
	-$(DEL) $(LISP)\mule\*.elc
	-$(DEL) $(LISP)\term\*.elc

realclean: distclean

#not sure about those wildcards.  DOS wildcards are stupid compared to Unix,
#and could end up deleting *everything* instead of just backup files or
#whatever.  So just leave it at "realclean"
extraclean: realclean
#	-$(DEL) *~
#	-$(DEL)  *.*~
#	-$(DEL)  #*
#	-$(DEL)  m\*~
#	-$(DEL)  m\#*
#	-$(DEL)  s\*~
#	-$(DEL)  s\#*

########################### Rebuild source dependency file

depend:
	cd $(SRC)
	perl ./make-src-depend > depend.tmp
	perl -MFile::Compare -e "compare('depend.tmp', 'depend') && rename('depend.tmp', 'depend') or unlink('depend.tmp')"

########################### Redo Unicode-Encapsulation

unicode-encapsulate:
	cd $(SRC)
	perl ../lib-src/make-mswin-unicode.pl --c-output intl-auto-encap-win32.c --h-output intl-auto-encap-win32.h intl-encap-win32.c

