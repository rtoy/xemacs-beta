XEMACS=..
LISP=$(XEMACS)\lisp
CC=cl

OPT=-Od -Zi
#OPT=-O2 -G5 -Zi

include ..\version.sh

#------------------------------------------------------------------------------

# Generic variables

INCLUDES=-I$(X11R6)\include -I.\inc -I$(XEMACS)\src\
 -I$(XEMACS)\lwlib -I"$(MSVCDIR)\include"
LIBRARIES=

OUTDIR=obj

#------------------------------------------------------------------------------

default: $(OUTDIR)\nul all 

$(OUTDIR)\nul:
	-@mkdir $(OUTDIR)

XEMACS_INCLUDES=\
 $(XEMACS)\src\config.h \
 $(XEMACS)\src\Emacs.ad.h \
 $(XEMACS)\src\paths.h

$(XEMACS_INCLUDES):
	!"copy *.h $(XEMACS)\src"

#------------------------------------------------------------------------------

# LASTFILE Library

LASTFILE=$(OUTDIR)\lastfile.lib
LASTFILE_SRC=$(XEMACS)\src
LASTFILE_FLAGS=-nologo -w $(OPT) $(INCLUDES) -Fo$@ -c
LASTFILE_OBJS= \
	$(OUTDIR)\lastfile.obj

$(LASTFILE): $(LASTFILE_OBJS)
	link.exe -lib -nologo -out:$@ $(LASTFILE_OBJS)

$(OUTDIR)\lastfile.obj:	$(LASTFILE_SRC)\lastfile.c
	 $(CC) $(LASTFILE_FLAGS) $**

#------------------------------------------------------------------------------

# LWLIB Library

LWLIB=$(OUTDIR)\lwlib.lib
LWLIB_SRC=$(XEMACS)\lwlib
LWLIB_FLAGS=-nologo -w $(OPT) $(INCLUDES) -D "WIN32" -D "_DEBUG" \
 -D "_NTSDK" -D "_M_IX86" -D "_X86_" \
 -D "NEED_ATHENA" -D "NEED_LUCID" \
 -D "_WINDOWS" -D "MENUBARS_LUCID" -D "SCROLLBARS_LUCID" -D "DIALOGS_ATHENA" \
 -D "WINDOWSNT" -Fo$@ -c
LWLIB_OBJS= \
        $(OUTDIR)\lwlib-config.obj \
        $(OUTDIR)\lwlib-utils.obj \
        $(OUTDIR)\lwlib-Xaw.obj \
        $(OUTDIR)\lwlib-Xlw.obj \
        $(OUTDIR)\lwlib.obj \
        $(OUTDIR)\xlwmenu.obj \
        $(OUTDIR)\xlwscrollbar.obj

$(LWLIB): $(XEMACS_INCLUDES) $(LWLIB_OBJS)
	link.exe -lib -nologo -debug -debugtype:both -out:$@ $(LWLIB_OBJS)

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

#------------------------------------------------------------------------------

# lib-src programs

LIB_SRC=$(XEMACS)\lib-src
LIB_SRC_FLAGS=$(INCLUDES) -D_DEBUG -DWIN32 -D_WIN32 -DWIN32_LEAN_AND_MEAN \
 -D_NTSDK -D_M_IX86 -ML -D_X86_ -Demacs -DHAVE_CONFIG_H -D_MSC_VER=999
LIB_SRC_LIBS= kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib\
 advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib libc.lib
LIB_SRC_LFLAGS=-nologo $(LIB_SRC_LIBS) -base:0x1000000\
 -subsystem:console -pdb:none -debugtype:both -machine:I386\
 -nodefaultlib -out:$@ -debug:full


DOC=$(LIB_SRC)\DOC
DOC_SRCS=\
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
 $(XEMACS)\src\console-w32.c \
 $(XEMACS)\src\console.c \
 $(XEMACS)\src\data.c \
 $(XEMACS)\src\debug.c \
 $(XEMACS)\src\device-w32.c
DOC_SRC2=\
 $(XEMACS)\src\device.c \
 $(XEMACS)\src\dgif_lib.c \
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
 $(XEMACS)\src\event-unixoid.c \
 $(XEMACS)\src\event-w32.c
DOC_SRC3=\
 $(XEMACS)\src\events.c \
 $(XEMACS)\src\extents.c \
 $(XEMACS)\src\faces.c \
 $(XEMACS)\src\fileio.c \
 $(XEMACS)\src\filelock.c \
 $(XEMACS)\src\filemode.c \
 $(XEMACS)\src\floatfns.c \
 $(XEMACS)\src\fns.c \
 $(XEMACS)\src\font-lock.c \
 $(XEMACS)\src\frame-w32.c \
 $(XEMACS)\src\frame.c \
 $(XEMACS)\src\free-hook.c \
 $(XEMACS)\src\general.c \
 $(XEMACS)\src\gif_err.c \
 $(XEMACS)\src\gifalloc.c \
 $(XEMACS)\src\glyphs.c \
 $(XEMACS)\src\gmalloc.c \
 $(XEMACS)\src\gui.c
DOC_SRC4=\
 $(XEMACS)\src\hash.c \
 $(XEMACS)\src\indent.c \
 $(XEMACS)\src\inline.c \
 $(XEMACS)\src\insdel.c \
 $(XEMACS)\src\intl.c \
 $(XEMACS)\src\keymap.c \
 $(XEMACS)\src\lread.c \
 $(XEMACS)\src\lstream.c \
 $(XEMACS)\src\macros.c \
 $(XEMACS)\src\marker.c \
 $(XEMACS)\src\md5.c \
 $(XEMACS)\src\minibuf.c \
 $(XEMACS)\src\nt.c \
 $(XEMACS)\src\ntheap.c \
 $(XEMACS)\src\ntproc.c \
 $(XEMACS)\src\objects.c \
 $(XEMACS)\src\objects-w32.c \
 $(XEMACS)\src\opaque.c
DOC_SRC5=\
 $(XEMACS)\src\print.c \
 $(XEMACS)\src\process.c \
 $(XEMACS)\src\pure.c \
 $(XEMACS)\src\rangetab.c \
 $(XEMACS)\src\realpath.c \
 $(XEMACS)\src\redisplay-output.c \
 $(XEMACS)\src\redisplay-w32.c \
 $(XEMACS)\src\redisplay.c \
 $(XEMACS)\src\regex.c \
 $(XEMACS)\src\search.c \
 $(XEMACS)\src\signal.c \
 $(XEMACS)\src\sound.c \
 $(XEMACS)\src\specifier.c \
 $(XEMACS)\src\strftime.c \
 $(XEMACS)\src\symbols.c \
 $(XEMACS)\src\syntax.c \
 $(XEMACS)\src\sysdep.c
DOC_SRC6=\
 $(XEMACS)\src\tparam.c \
 $(XEMACS)\src\undo.c \
 $(XEMACS)\src\unexnt.c \
 $(XEMACS)\src\vm-limit.c \
 $(XEMACS)\src\w32-proc.c \
 $(XEMACS)\src\widget.c \
 $(XEMACS)\src\window.c 

MAKE_DOCFILE=$(LIB_SRC)\make-docfile.exe

$(MAKE_DOCFILE): $(OUTDIR)\make-docfile.obj
	link.exe -out:$@ $(LIB_SRC_LFLAGS) $** $(LIB_SRC_LIBS)

$(OUTDIR)\make-docfile.obj:	$(LIB_SRC)\make-docfile.c
	 $(CC) $(LIB_SRC_FLAGS) -c $** -Fo$@

RUNEMACS=$(XEMACS)\src\runemacs.exe

$(RUNEMACS): $(OUTDIR)\runemacs.obj
	link.exe -out:$@ -subsystem:windows -entry:WinMainCRTStartup \
	-pdb:none -release -incremental:no $** \
	kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib \
	advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib libc.lib

$(OUTDIR)\runemacs.obj:	.\runemacs.c
	$(CC) -nologo -ML -w $(OPT) -c \
	-D_DEBUG -DWIN32 -D_WIN32 -DWIN32_LEAN_AND_MEAN \
	-D_NTSDK -D_M_IX86 -D_X86_ -Demacs -DHAVE_CONFIG_H -D_MSC_VER=999 \
	$** -Fo$@

SUPPORT_PROGS=$(MAKE_DOCFILE) $(RUNEMACS)

#------------------------------------------------------------------------------

# TEMACS Executable

TEMACS_DIR=$(XEMACS)\src
TEMACS=$(TEMACS_DIR)\temacs.exe
TEMACS_SRC=$(XEMACS)\src
TEMACS_LIBS=$(LASTFILE) kernel32.lib user32.lib gdi32.lib \
 winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib \
 uuid.lib wsock32.lib libc.lib
TEMACS_LFLAGS=-nologo $(LIBRARIES) -base:0x1000000\
 -stack:0x800000 -entry:_start -subsystem:console\
 -pdb:$(TEMACS_DIR)\temacs.pdb -map:$(TEMACS_DIR)\temacs.map -debug:full\
 -heap:0x00100000 -out:$@

TEMACS_CPP_FLAGS= $(INCLUDES) -D_DEBUG -DWIN32 -D_WIN32 -DWIN32_LEAN_AND_MEAN \
 -D_NTSDK -D_M_IX86 -D_X86_ -Demacs -DHAVE_CONFIG_H -D_MSC_VER=999 \
 -DEMACS_MAJOR_VERSION=$(emacs_major_version) \
 -DEMACS_MINOR_VERSION=$(emacs_minor_version) \
 -DXEMACS_CODENAME=\"$(xemacs_codename)\" \
 -DPATH_PREFIX=\"$(XEMACS)\"
TEMACS_FLAGS=-nologo -ML $(OPT) -c $(TEMACS_CPP_FLAGS)

TEMACS_OBJS= \
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
	$(OUTDIR)\console-w32.obj \
	$(OUTDIR)\console.obj \
	$(OUTDIR)\data.obj \
	$(OUTDIR)\debug.obj \
	$(OUTDIR)\device-w32.obj \
	$(OUTDIR)\device.obj \
	$(OUTDIR)\dgif_lib.obj \
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
	$(OUTDIR)\event-unixoid.obj \
	$(OUTDIR)\event-w32.obj \
	$(OUTDIR)\events.obj \
	$(OUTDIR)\extents.obj \
	$(OUTDIR)\faces.obj \
	$(OUTDIR)\fileio.obj \
	$(OUTDIR)\filelock.obj \
	$(OUTDIR)\filemode.obj \
	$(OUTDIR)\floatfns.obj \
	$(OUTDIR)\fns.obj \
	$(OUTDIR)\font-lock.obj \
	$(OUTDIR)\frame-w32.obj \
	$(OUTDIR)\frame.obj \
	$(OUTDIR)\free-hook.obj \
	$(OUTDIR)\general.obj \
	$(OUTDIR)\gif_err.obj \
	$(OUTDIR)\gifalloc.obj \
	$(OUTDIR)\glyphs.obj \
	$(OUTDIR)\gmalloc.obj \
	$(OUTDIR)\gui.obj \
	$(OUTDIR)\hash.obj \
	$(OUTDIR)\indent.obj \
	$(OUTDIR)\inline.obj \
	$(OUTDIR)\insdel.obj \
	$(OUTDIR)\intl.obj \
	$(OUTDIR)\keymap.obj \
	$(OUTDIR)\lread.obj \
	$(OUTDIR)\lstream.obj \
	$(OUTDIR)\macros.obj \
	$(OUTDIR)\marker.obj \
	$(OUTDIR)\md5.obj \
	$(OUTDIR)\minibuf.obj \
	$(OUTDIR)\nt.obj \
	$(OUTDIR)\ntheap.obj \
	$(OUTDIR)\ntproc.obj \
	$(OUTDIR)\objects-w32.obj \
	$(OUTDIR)\objects.obj \
	$(OUTDIR)\opaque.obj \
	$(OUTDIR)\print.obj \
	$(OUTDIR)\process.obj \
	$(OUTDIR)\pure.obj \
	$(OUTDIR)\rangetab.obj \
	$(OUTDIR)\realpath.obj \
	$(OUTDIR)\redisplay-output.obj \
	$(OUTDIR)\redisplay-w32.obj \
	$(OUTDIR)\redisplay.obj \
	$(OUTDIR)\regex.obj \
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
	$(OUTDIR)\w32-proc.obj \
	$(OUTDIR)\widget.obj \
	$(OUTDIR)\window.obj 

#------------------------------------------------------------------------------

# Rules

.SUFFIXES:
.SUFFIXES:	.c

# nmake rule
{$(TEMACS_SRC)}.c{$(OUTDIR)}.obj:	
	$(CC) $(TEMACS_FLAGS) $< -Fo$@ -Fr$*.sbr

# Specific builds

$(OUTDIR)\TopLevelEmacsShell.obj:	$(TEMACS_SRC)\EmacsShell-sub.c
	$(CC) $(TEMACS_FLAGS) -DDEFINE_TOP_LEVEL_EMACS_SHELL $** -Fo$@

$(OUTDIR)\TransientEmacsShell.obj: $(TEMACS_SRC)\EmacsShell-sub.c
	$(CC) $(TEMACS_FLAGS) -DDEFINE_TRANSIENT_EMACS_SHELL $** -Fo$@

#$(TEMACS_SRC)\Emacs.ad.h: $(XEMACS)\etc\Emacs.ad
#	!"sed -f ad2c.sed < $(XEMACS)\etc\Emacs.ad > $(TEMACS_SRC)\Emacs.ad.h"

#$(TEMACS_SRC)\paths.h: $(TEMACS_SRC)\paths.h.in
#	!"cd $(TEMACS_SRC); cp paths.h.in paths.h"

$(TEMACS): $(TEMACS_INCLUDES) $(TEMACS_OBJS) $(LASTFILE)
	link.exe $(TEMACS_LFLAGS) @<<
$(TEMACS_OBJS) $(TEMACS_LIBS)
<<
	dir /b/s obj\*.sbr > bscmake.tmp
	bscmake -o$*.bsc @bscmake.tmp


#------------------------------------------------------------------------------

# LISP bits 'n bobs

$(DOC): $(MAKE_DOCFILE) $(DOC_SRCS) $(DOC_SRC1) $(DOC_SRC2) $(DOC_SRC3) $(DOC_SRC4) $(DOC_SRC5) $(DOC_SRC6)
	cd $(TEMACS_DIR)
	del $(DOC)
	!$(TEMACS) -batch -l make-docfile.el -- -o $(DOC) -i $(XEMACS)\site-packages
	!$(MAKE_DOCFILE) -a $(DOC) -d $(TEMACS_SRC) $(DOC_SRCS)
	!$(MAKE_DOCFILE) -a $(DOC) -d $(TEMACS_SRC) $(DOC_SRC1)
	!$(MAKE_DOCFILE) -a $(DOC) -d $(TEMACS_SRC) $(DOC_SRC2)
	!$(MAKE_DOCFILE) -a $(DOC) -d $(TEMACS_SRC) $(DOC_SRC3)
	!$(MAKE_DOCFILE) -a $(DOC) -d $(TEMACS_SRC) $(DOC_SRC4)
	!$(MAKE_DOCFILE) -a $(DOC) -d $(TEMACS_SRC) $(DOC_SRC4)
	!$(MAKE_DOCFILE) -a $(DOC) -d $(TEMACS_SRC) $(DOC_SRC5)

LOADPATH=$(LISP)\prim

dump-elcs:	$(TEMACS)
	cd $(TEMACS_DIR)
	!$(TEMACS) -batch -l update-elc.el

dump-xemacs:	$(TEMACS) $(SUPPORT_PROGS) $(DOC)
	cd $(TEMACS_DIR)
	!$(TEMACS) -batch -l loadup.el dump

#------------------------------------------------------------------------------

all: $(LASTFILE) $(TEMACS) $(SUPPORT_PROGS)

# use this rule to install the system
install:

# The last line demands that you have a semi-decent shell
distclean:	$(OUTDIR)\nul
	del *.bak
	del *.orig
	del *.rej
	del *.pdb
	del *.tmp
	cd $(OUTDIR)
	del *.obj
	del *.sbr
	del *.lib
	cd ..\$(TEMACS_DIR)
	del config.h
	del paths.h
	del puresize-adjust.h
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
	del *.exe
	cd $(LISP)
	-del /s /q *.bak *.elc *.orig *.rej

depend:
	mkdepend -f xemacs.mak -p$(OUTDIR)\ -o.obj -w9999 -- $(TEMACS_CPP_FLAGS) --  $(DOC_SRCS) $(DOC_SRC1) $(DOC_SRC2) $(DOC_SRC3) $(DOC_SRC4) $(DOC_SRC5) $(DOC_SRC6) $(LASTFILE_SRC)\lastfile.c $(LIB_SRC)\make-docfile.c .\runemacs.c

# DO NOT DELETE THIS LINE -- make depend depends on it.
