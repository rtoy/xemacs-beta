XEMACS=..
LISP=$(XEMACS)\lisp
PACKAGE_PATH="~/.xemacs;f:/src/xemacs/packages"
HAVE_X=0
HAVE_MSW=1

HAVE_MULE=0
HAVE_IMAGEMAGICK=0

DEBUG_XEMACS=1

!if $(DEBUG_XEMACS)
OPT=-Od -Zi
!else
OPT=-O2 -G5 -Zi
!endif

#------------------------------------------------------------------------------

!if $(HAVE_X)

X11R6=h:\utils\X11R6

!if $(HAVE_IMAGEMAGICK)
MAGICK=e:\utils\ImageMagick

MAGICK_INCLUDES=-I$(MAGICK)\Magick
MAGICK_LIBS=Magick.dll.lib
!endif

X_DEFINES=-DHAVE_X_WINDOWS
X_INCLUDES=-I$(X11R6)\include $(MAGICK_INCLUDES)
X_LIBS=$(MAGICK_LIBS) Xaw.lib Xmu.lib Xt.lib SM.lib ICE.lib Xext.lib X11.lib
!endif

!if $(HAVE_MSW)
MSW_DEFINES=-DHAVE_MS_WINDOWS
!endif

!if $(HAVE_MULE)
MULE_DEFINES=-DMULE
!endif

!if $(DEBUG_XEMACS)
DEBUG_DEFINES=-DDEBUG_XEMACS
DEBUG_FLAGS= -debugtype:both -debug:full
!endif

!include "..\version.sh"

# Nothing should need to be edited below this point.
#------------------------------------------------------------------------------

# Generic variables

INCLUDES=$(X_INCLUDES) -I$(XEMACS)\nt\inc -I$(XEMACS)\src -I$(XEMACS)\lwlib -I"$(MSVCDIR)\include"

DEFINES=$(X_DEFINES) $(MSW_DEFINES) $(MULE_DEFINES) -DWIN32 -D_WIN32 \
	-DWIN32_LEAN_AND_MEAN -DWINDOWSNT -Demacs -DHAVE_CONFIG_H \
	-D_DEBUG

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
LASTFILE_FLAGS=-nologo -w $(OPT) $(INCLUDES) -Fo$@ -c
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
LWLIB_FLAGS=-nologo -w $(OPT) $(INCLUDES) $(DEFINES) \
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
 $(XEMACS)\src\device.c \
 $(XEMACS)\src\dgif_lib.c 
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
 $(XEMACS)\src\gif_err.c \
 $(XEMACS)\src\gifalloc.c \
 $(XEMACS)\src\glyphs.c \
 $(XEMACS)\src\gmalloc.c \
 $(XEMACS)\src\gui.c  \
 $(XEMACS)\src\hash.c \
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
 $(XEMACS)\src\minibuf.c \
 $(XEMACS)\src\nt.c \
 $(XEMACS)\src\ntheap.c \
 $(XEMACS)\src\ntproc.c \
 $(XEMACS)\src\objects.c \
 $(XEMACS)\src\opaque.c \
 $(XEMACS)\src\print.c \
 $(XEMACS)\src\process.c \
 $(XEMACS)\src\profile.c \
 $(XEMACS)\src\pure.c \
 $(XEMACS)\src\rangetab.c \
 $(XEMACS)\src\realpath.c \
 $(XEMACS)\src\redisplay-output.c \
 $(XEMACS)\src\redisplay.c \
 $(XEMACS)\src\regex.c \
 $(XEMACS)\src\scrollbar.c \
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
 $(XEMACS)\src\objects-msw.c \
 $(XEMACS)\src\redisplay-msw.c \
 $(XEMACS)\src\select-msw.c \
 $(XEMACS)\src\msw-proc.c
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
DOC_SRC_9=\
 $(XEMACS)\src\debug.c
!endif

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

$(OUTDIR)\runemacs.obj:	$(XEMACS)\nt\runemacs.c
	$(CC) -nologo -ML -w $(OPT) -c \
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
TEMACS_LIBS=$(LASTFILE) $(LWLIB) $(X_LIBS) kernel32.lib user32.lib gdi32.lib \
 winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib \
 uuid.lib wsock32.lib winmm.lib libc.lib
TEMACS_LFLAGS=-nologo $(LIBRARIES) $(DEBUG_FLAGS) -base:0x1000000\
 -stack:0x800000 -entry:_start -subsystem:console\
 -pdb:$(TEMACS_DIR)\temacs.pdb -map:$(TEMACS_DIR)\temacs.map \
 -heap:0x00100000 -out:$@
TEMACS_CPP_FLAGS= $(INCLUDES) $(DEFINES) $(DEBUG_DEFINES) \
 -DEMACS_MAJOR_VERSION=$(emacs_major_version) \
 -DEMACS_MINOR_VERSION=$(emacs_minor_version) \
 -DEMACS_BETA_VERSION=$(emacs_beta_version) \
 -DXEMACS_CODENAME=\"$(xemacs_codename)\" \
 -DPATH_PREFIX=\"$(XEMACS)\" \
 -DPACKAGE_PATH=\"$(PACKAGE_PATH)\"

TEMACS_FLAGS=-nologo -ML -w $(OPT) -c $(TEMACS_CPP_FLAGS)

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
	$(OUTDIR)\menubar.obj \
	$(OUTDIR)\menubar-x.obj \
	$(OUTDIR)\objects-x.obj \
	$(OUTDIR)\redisplay-x.obj \
	$(OUTDIR)\scrollbar.obj \
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
	$(OUTDIR)\objects-msw.obj \
	$(OUTDIR)\redisplay-msw.obj \
	$(OUTDIR)\select-msw.obj \
	$(OUTDIR)\msw-proc.obj
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
	$(OUTDIR)\events.obj \
	$(OUTDIR)\extents.obj \
	$(OUTDIR)\faces.obj \
	$(OUTDIR)\fileio.obj \
	$(OUTDIR)\filelock.obj \
	$(OUTDIR)\filemode.obj \
	$(OUTDIR)\floatfns.obj \
	$(OUTDIR)\fns.obj \
	$(OUTDIR)\font-lock.obj \
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
	$(OUTDIR)\line-number.obj \
	$(OUTDIR)\lread.obj \
	$(OUTDIR)\lstream.obj \
	$(OUTDIR)\macros.obj \
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
	$(OUTDIR)\profile.obj \
	$(OUTDIR)\pure.obj \
	$(OUTDIR)\rangetab.obj \
	$(OUTDIR)\realpath.obj \
	$(OUTDIR)\redisplay-output.obj \
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
	$(OUTDIR)\widget.obj \
	$(OUTDIR)\window.obj 

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
	!$(TEMACS) -batch -l update-elc.el

# MSDEV Source Broswer file. "*.sbr" is too inclusive but this is harmless
$(TEMACS_BROWSE): $(TEMACS_OBJS)
	dir /b/s $(OUTDIR)\*.sbr > bscmake.tmp
	bscmake -o$@ @bscmake.tmp
	del bscmake.tmp

#------------------------------------------------------------------------------

# LISP bits 'n bobs

LOADPATH=$(LISP)

$(DOC): $(LIB_SRC)\make-docfile.exe
	-del $(DOC)
	!$(TEMACS) -batch -l make-docfile.el -- -o $(DOC) -i $(XEMACS)\site-packages
	!$(LIB_SRC)\make-docfile.exe -a $(DOC) -d $(TEMACS_SRC) $(DOC_SRC1)
	!$(LIB_SRC)\make-docfile.exe -a $(DOC) -d $(TEMACS_SRC) $(DOC_SRC2)
	!$(LIB_SRC)\make-docfile.exe -a $(DOC) -d $(TEMACS_SRC) $(DOC_SRC3)
	!$(LIB_SRC)\make-docfile.exe -a $(DOC) -d $(TEMACS_SRC) $(DOC_SRC4)
	!$(LIB_SRC)\make-docfile.exe -a $(DOC) -d $(TEMACS_SRC) $(DOC_SRC5)
	!$(LIB_SRC)\make-docfile.exe -a $(DOC) -d $(TEMACS_SRC) $(DOC_SRC6)
	!$(LIB_SRC)\make-docfile.exe -a $(DOC) -d $(TEMACS_SRC) $(DOC_SRC7)
	!$(LIB_SRC)\make-docfile.exe -a $(DOC) -d $(TEMACS_SRC) $(DOC_SRC8)
	!$(LIB_SRC)\make-docfile.exe -a $(DOC) -d $(TEMACS_SRC) $(DOC_SRC9)

update-elc: $(LOADPATH)\startup.el
	!$(TEMACS) -batch -l update-elc.el

rebuild: $(TEMACS_DIR)\puresize-adjust.h
        !nmake -f xemacs.mak dump-xemacs

# This rule dumps xemacs and then checks to see if a rebuild is required due
# to changing PURESPACE requirements.
dump-xemacs: $(TEMACS)
        !touch rebuild
        cd $(TEMACS_DIR)
        !$(TEMACS) -batch -l loadup.el dump
        cd $(XEMACS)\nt
        !nmake -f xemacs.mak rebuild

#------------------------------------------------------------------------------

# use this rule to build the complete system
all: $(LASTFILE) $(LWLIB) $(SUPPORT_PROGS) $(TEMACS) $(TEMACS_BROWSE) $(DOC) dump-xemacs
	-del rebuild

# use this rule to install the system
install:

# The last line demands that you have a semi-decent shell
distclean:
	-mkdepend -f xemacs.mak
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
	mkdepend -f xemacs.mak -p$(OUTDIR)\ -o.obj -w9999 -- $(TEMACS_CPP_FLAGS) --  $(DOC_SRC1) $(DOC_SRC2) $(DOC_SRC3) $(DOC_SRC4) $(DOC_SRC5) $(DOC_SRC6) $(DOC_SRC7) $(DOC_SRC8) $(LASTFILE_SRC)\lastfile.c $(LIB_SRC)\make-docfile.c .\runemacs.c

# DO NOT DELETE THIS LINE -- make depend depends on it.
	mkdepend -f xemacs.mak -p$(OUTDIR)\ -o.obj -w9999 -- $(TEMACS_CPP_FLAGS) --  $(DOC_SRC1) $(DOC_SRC2) $(DOC_SRC3) $(DOC_SRC4) $(DOC_SRC5) $(DOC_SRC6) $(DOC_SRC7) $(DOC_SRC8) $(DOC_SRC9) $(LASTFILE_SRC)\lastfile.c $(LIB_SRC)\make-docfile.c .\runemacs.c
