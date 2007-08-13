MSDEV=F:/msdev
X11R6=F:/utils/X11R6
XEMACS=..
LISP=$(XEMACS)/lisp

#------------------------------------------------------------------------------

# Generic variables

INCLUDES=-I$(X11R6)/include -I$(XEMACS)/nt/inc -I$(XEMACS)/src\
 -I$(XEMACS)/lwlib -I$(MSDEV)/include
#INCLUDES=-I$(X11R6)/include -I$(XEMACS)/src\
# -I$(XEMACS)/lwlib -I$(MSDEV)/include
LIBRARIES=

OUTDIR=obj

#------------------------------------------------------------------------------

default: objdir all 

objdir:
	@echo "Ignore error message if $(OUTDIR) subdirectory already exists."
	-@mkdir $(OUTDIR)

# LASTFILE Library

LASTFILE=$(OUTDIR)/lastfile.lib
LASTFILE_SRC=$(XEMACS)/src
LASTFILE_FLAGS=-nologo -w -Od $(INCLUDES) -Fo$@ -c
LASTFILE_OBJS= \
	$(OUTDIR)/lastfile.obj
	
$(LASTFILE): $(LASTFILE_OBJS)
	link.exe -lib /nologo /out:$@ $(LASTFILE_OBJS)

$(OUTDIR)/lastfile.obj:	$(LASTFILE_SRC)/lastfile.c
	 $(CC) $(LASTFILE_FLAGS) $**

#------------------------------------------------------------------------------

# LWLIB Library

LWLIB=$(OUTDIR)/lwlib.lib
LWLIB_SRC=$(XEMACS)/lwlib
LWLIB_FLAGS=/nologo /w /Od /Zi $(INCLUDES) /D "WIN32" /D "_DEBUG" \
 /D "NEED_ATHENA" /D "NEED_LUCID" \
 /D "_WINDOWS" /D "MENUBARS_LUCID" /D "SCROLLBARS_LUCID" /D "DIALOGS_ATHENA" \
 /D "WINDOWSNT" /Fo$@ /c
LWLIB_OBJS= \
        $(OUTDIR)/lwlib-config.obj \
        $(OUTDIR)/lwlib-utils.obj \
        $(OUTDIR)/lwlib-Xaw.obj \
        $(OUTDIR)/lwlib-Xlw.obj \
        $(OUTDIR)/lwlib.obj \
        $(OUTDIR)/xlwmenu.obj \
        $(OUTDIR)/xlwscrollbar.obj

$(LWLIB): $(LWLIB_OBJS)
	link.exe -lib -nologo -debug -debugtype:both -out:$@ $(LWLIB_OBJS)

$(OUTDIR)/lwlib-config.obj:	$(LWLIB_SRC)/lwlib-config.c
	 $(CC) $(LWLIB_FLAGS) $**

$(OUTDIR)/lwlib-utils.obj:	$(LWLIB_SRC)/lwlib-utils.c
	 $(CC) $(LWLIB_FLAGS) $**

$(OUTDIR)/lwlib-Xaw.obj:	$(LWLIB_SRC)/lwlib-Xaw.c
	 $(CC) $(LWLIB_FLAGS) $**

$(OUTDIR)/lwlib-Xlw.obj:	$(LWLIB_SRC)/lwlib-Xlw.c
	 $(CC) $(LWLIB_FLAGS) $**

$(OUTDIR)/lwlib.obj:		$(LWLIB_SRC)/lwlib.c
	 $(CC) $(LWLIB_FLAGS) $**

$(OUTDIR)/xlwmenu.obj:		$(LWLIB_SRC)/xlwmenu.c
	 $(CC) $(LWLIB_FLAGS) $**

$(OUTDIR)/xlwscrollbar.obj:	$(LWLIB_SRC)/xlwscrollbar.c
	 $(CC) $(LWLIB_FLAGS) $**

#------------------------------------------------------------------------------

# lib-src programs

LIB_SRC=$(XEMACS)/lib-src
LIB_SRC_FLAGS=$(INCLUDES) -D_DEBUG -DWIN32 -D_WIN32 -DWIN32_LEAN_AND_MEAN \
 -D_NTSDK -D_M_IX86 -ML -D_X86_ -Demacs -DHAVE_CONFIG_H -D_MSC_VER=999
LIB_SRC_LIBS= kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib\
 advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib \
 odbccp32.lib libc.lib
LIB_SRC_LFLAGS=/nologo $(LIB_SRC_LIBS) /base:0x1000000\
 /subsystem:console /pdb:none /debugtype:both /machine:I386\
 /nodefaultlib /out:$@ /debug:full

DOC=$(LIB_SRC)/DOC
DOC_SRCS_1=\
 $(XEMACS)/src/abbrev.c \
 $(XEMACS)/src/alloc.c \
 $(XEMACS)/src/alloca.c \
 $(XEMACS)/src/blocktype.c \
 $(XEMACS)/src/buffer.c \
 $(XEMACS)/src/bytecode.c \
 $(XEMACS)/src/callint.c \
 $(XEMACS)/src/callproc.c \
 $(XEMACS)/src/casefiddle.c \
 $(XEMACS)/src/casetab.c \
 $(XEMACS)/src/chartab.c \
 $(XEMACS)/src/cmdloop.c \
 $(XEMACS)/src/cmds.c \
 $(XEMACS)/src/console-stream.c \
 $(XEMACS)/src/console-x.c \
 $(XEMACS)/src/console.c \
 $(XEMACS)/src/data.c \
 $(XEMACS)/src/database.c \
 $(XEMACS)/src/debug.c \
 $(XEMACS)/src/device-x.c \
 $(XEMACS)/src/device.c \
 $(XEMACS)/src/dgif_lib.c \
 $(XEMACS)/src/dialog-x.c \
 $(XEMACS)/src/dialog.c \
 $(XEMACS)/src/dired.c \
 $(XEMACS)/src/doc.c \
 $(XEMACS)/src/doprnt.c \
 $(XEMACS)/src/dynarr.c \
 $(XEMACS)/src/editfns.c \
 $(XEMACS)/src/elhash.c \
 $(XEMACS)/src/emacs.c \
 $(XEMACS)/src/EmacsFrame.c \
 $(XEMACS)/src/EmacsManager.c \
 $(XEMACS)/src/EmacsShell-sub.c\
 $(XEMACS)/src/EmacsShell.c \
 $(XEMACS)/src/energize.c \
 $(XEMACS)/src/eval.c \
 $(XEMACS)/src/event-stream.c \
 $(XEMACS)/src/event-unixoid.c \
 $(XEMACS)/src/event-Xt.c \
 $(XEMACS)/src/events.c \
 $(XEMACS)/src/extents.c \
 $(XEMACS)/src/faces.c \
 $(XEMACS)/src/fileio.c \
 $(XEMACS)/src/filelock.c \
 $(XEMACS)/src/filemode.c \
 $(XEMACS)/src/floatfns.c \
 $(XEMACS)/src/fns.c \
 $(XEMACS)/src/font-lock.c \
 $(XEMACS)/src/frame-x.c \
 $(XEMACS)/src/frame.c \
 $(XEMACS)/src/free-hook.c \
 $(XEMACS)/src/general.c \
 $(XEMACS)/src/gif_err.c \
 $(XEMACS)/src/gifalloc.c \
 $(XEMACS)/src/glyphs-x.c \
 $(XEMACS)/src/glyphs.c \
 $(XEMACS)/src/gmalloc.c \
 $(XEMACS)/src/gui-x.c \
 $(XEMACS)/src/gui.c \
 $(XEMACS)/src/hash.c \
 $(XEMACS)/src/indent.c \
 $(XEMACS)/src/inline.c \
 $(XEMACS)/src/insdel.c \
 $(XEMACS)/src/intl.c \
 $(XEMACS)/src/keymap.c \
 $(XEMACS)/src/lread.c \
 $(XEMACS)/src/lstream.c \
 $(XEMACS)/src/macros.c \
 $(XEMACS)/src/marker.c \
 $(XEMACS)/src/md5.c \
 $(XEMACS)/src/menubar-x.c \
 $(XEMACS)/src/menubar.c \
 $(XEMACS)/src/minibuf.c \
 $(XEMACS)/src/mocklisp.c \
 $(XEMACS)/src/nt.c \
 $(XEMACS)/src/ntheap.c \
 $(XEMACS)/src/ntproc.c \
 $(XEMACS)/src/objects-x.c \
 $(XEMACS)/src/objects.c \
 $(XEMACS)/src/opaque.c \
 $(XEMACS)/src/print.c \
 $(XEMACS)/src/process.c \
 $(XEMACS)/src/pure.c \
 $(XEMACS)/src/rangetab.c \
 $(XEMACS)/src/realpath.c \
 $(XEMACS)/src/redisplay-output.c \
 $(XEMACS)/src/redisplay-x.c \
 $(XEMACS)/src/redisplay.c \
 $(XEMACS)/src/regex.c \
 $(XEMACS)/src/scrollbar-x.c \
 $(XEMACS)/src/scrollbar.c \
 $(XEMACS)/src/search.c \
 $(XEMACS)/src/signal.c \
 $(XEMACS)/src/sound.c \
 $(XEMACS)/src/specifier.c \
 $(XEMACS)/src/strftime.c \
 $(XEMACS)/src/symbols.c \
 $(XEMACS)/src/syntax.c \
 $(XEMACS)/src/sysdep.c \
 $(XEMACS)/src/termcap.c \
 $(XEMACS)/src/tparam.c \
 $(XEMACS)/src/undo.c \
 $(XEMACS)/src/unexnt.c \
 $(XEMACS)/src/vm-limit.c \
 $(XEMACS)/src/window.c \
 $(XEMACS)/src/xgccache.c \
 $(XEMACS)/src/xmu.c \
 $(XEMACS)/src/xselect.c
DOC_SRCS_2=\
 $(LISP)/version.el \
 $(LISP)/paths.el \
 $(LISP)/prim/loaddefs.elc \
 $(LISP)/prim/auto-autoloads.elc \
 $(LISP)/prim/loadup.el \
 $(LISP)/prim/subr.elc \
 $(LISP)/prim/cmdloop.elc \
 $(LISP)/utils/text-props.elc \
 $(LISP)/prim/gui.elc \
 $(LISP)/prim/mouse.elc \
 $(LISP)/prim/mode-motion.elc \
 $(LISP)/prim/keymap.elc \
 $(LISP)/prim/syntax.elc \
 $(LISP)/prim/minibuf.elc \
 $(LISP)/prim/faces.elc \
 $(LISP)/prim/objects.elc \
 $(LISP)/prim/process.elc \
 $(LISP)/prim/keydefs.elc \
 $(LISP)/prim/device.elc \
 $(LISP)/prim/obsolete.elc \
 $(LISP)/prim/glyphs.elc \
 $(LISP)/prim/extents.elc \
 $(LISP)/prim/backquote.elc \
 $(LISP)/prim/events.elc \
 $(LISP)/prim/console.elc \
 $(LISP)/utils/map-ynp.elc \
 $(LISP)/prim/modeline.elc \
 $(LISP)/prim/profile.elc \
 $(LISP)/modes/list-mode.elc \
 $(LISP)/utils/derived.elc \
 $(LISP)/cl/cl.elc \
 $(LISP)/cl/cl-defs.elc \
 $(LISP)/prim/undo-stack.elc \
 $(LISP)/prim/simple.elc \
 $(LISP)/prim/help.elc \
 $(LISP)/prim/files.elc \
 $(LISP)/utils/lib-complete.elc \
 $(LISP)/prim/indent.elc \
 $(LISP)/prim/frame.elc \
 $(LISP)/prim/format.elc \
 $(LISP)/prim/window.elc \
 $(LISP)/prim/startup.elc \
 $(LISP)/prim/lisp.elc \
 $(LISP)/prim/page.elc \
 $(LISP)/prim/register.elc \
 $(LISP)/iso/iso8859-1.elc \
 $(LISP)/prim/paragraphs.elc \
 $(LISP)/modes/lisp-mode.elc \
 $(LISP)/modes/text-mode.elc \
 $(LISP)/prim/fill.elc \
 $(LISP)/prim/isearch-mode.elc \
 $(LISP)/prim/misc.elc \
 $(LISP)/packages/vc-hooks.elc \
 $(LISP)/prim/replace.elc \
 $(LISP)/prim/specifier.elc \
 $(LISP)/modes/auto-show.elc \
 $(LISP)/bytecomp/bytecomp-runtime.elc \
 $(LISP)/prim/float-sup.elc \
 $(LISP)/prim/itimer.elc \
 $(LISP)/ediff/ediff-hook.elc \
 $(LISP)/packages/fontl-hooks.elc \
 $(LISP)/prim/scrollbar.elc \
 $(LISP)/prim/buffer.elc \
 $(LISP)/prim/menubar.elc \
 $(LISP)/packages/buff-menu.elc  \
 $(LISP)/modes/abbrev.elc 
# X11_LISP NS_LISP ENERGIZE_LISP TOOLTALK_LISP DIALOG_LISP MULE_LISP NOMULE_LISP

MAKE_DOCFILE=$(OUTDIR)\make-docfile.exe
SUPPORT_PROGS=$(MAKE_DOCFILE)

$(MAKE_DOCFILE): $(OUTDIR)\make-docfile.obj
	link.exe $(LIB_SRC_LFLAGS) $** $(LIB_SRC_LIBS)

$(OUTDIR)/make-docfile.obj:	$(LIB_SRC)\make-docfile.c
	 $(CC) $(LIB_SRC_FLAGS) $** /Fo$@

$(DOC): $(OUTDIR)/make-docfile.exe
	!$(MAKE_DOCFILE) -o $@ $(DOC_SRCS_1) 
	!$(MAKE_DOCFILE) -a $@ $(DOC_SRCS_2)

#------------------------------------------------------------------------------

# TEMACS Executable

TEMACS_DIR=$(XEMACS)/src
TEMACS=$(TEMACS_DIR)/temacs.exe
TEMACS_SRC=$(XEMACS)/src
TEMACS_LIBS=$(LASTFILE) $(LWLIB) Xaw.lib Xmu.lib Xt.lib SM.lib ICE.lib \
 Xext.lib X11.lib kernel32.lib user32.lib gdi32.lib \
 winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib \
 uuid.lib odbc32.lib odbccp32.lib wsock32.lib libc.lib
TEMACS_LFLAGS=/nologo $(LIBRARIES) /base:0x1000000\
 /stack:0x800000 /entry:_start /subsystem:console /pdb:none\
 /map:$(TEMACS_DIR)/temacs.map /debug:full /debugtype:both /machine:I386\
 /nodefaultlib /force /out:$@\
 /heap:0x00100000

#TEMACS_CPP=$(XEMACS)/nt/cpp/cpp.exe
TEMACS_CPP=c:/usr/local/bin/cpp.exe
TEMACS_CPP_FLAGS= $(INCLUDES) -D_DEBUG -DWIN32 -D_WIN32 -DWIN32_LEAN_AND_MEAN \
 -D_NTSDK -D_M_IX86 -D_X86_ -Demacs -DHAVE_CONFIG_H -D_MSC_VER=999
TEMACS_FLAGS=-nologo -ML -w -Od -Zi -c $(TEMACS_CPP_FLAGS)

TEMACS_OBJS= \
	$(OUTDIR)/abbrev.obj \
	$(OUTDIR)/alloc.obj \
	$(OUTDIR)/alloca.obj \
	$(OUTDIR)/blocktype.obj \
	$(OUTDIR)/buffer.obj \
	$(OUTDIR)/bytecode.obj \
	$(OUTDIR)/callint.obj \
	$(OUTDIR)/callproc.obj \
	$(OUTDIR)/casefiddle.obj \
	$(OUTDIR)/casetab.obj \
	$(OUTDIR)/chartab.obj \
	$(OUTDIR)/cmdloop.obj \
	$(OUTDIR)/cmds.obj \
	$(OUTDIR)/console-stream.obj \
	$(OUTDIR)/console-x.obj \
	$(OUTDIR)/console.obj \
	$(OUTDIR)/data.obj \
	$(OUTDIR)/database.obj \
	$(OUTDIR)/debug.obj \
	$(OUTDIR)/device-x.obj \
	$(OUTDIR)/device.obj \
	$(OUTDIR)/dgif_lib.obj \
	$(OUTDIR)/dialog-x.obj \
	$(OUTDIR)/dialog.obj \
	$(OUTDIR)/dired.obj \
	$(OUTDIR)/doc.obj \
	$(OUTDIR)/doprnt.obj \
	$(OUTDIR)/dynarr.obj \
	$(OUTDIR)/editfns.obj \
	$(OUTDIR)/elhash.obj \
	$(OUTDIR)/emacs.obj \
	$(OUTDIR)/EmacsFrame.obj \
	$(OUTDIR)/EmacsManager.obj \
	$(OUTDIR)/TopLevelEmacsShell.obj\
	$(OUTDIR)/TransientEmacsShell.obj\
	$(OUTDIR)/EmacsShell.obj \
	$(OUTDIR)/energize.obj \
	$(OUTDIR)/eval.obj \
	$(OUTDIR)/event-stream.obj \
	$(OUTDIR)/event-unixoid.obj \
	$(OUTDIR)/event-Xt.obj \
	$(OUTDIR)/events.obj \
	$(OUTDIR)/extents.obj \
	$(OUTDIR)/faces.obj \
	$(OUTDIR)/fileio.obj \
	$(OUTDIR)/filelock.obj \
	$(OUTDIR)/filemode.obj \
	$(OUTDIR)/floatfns.obj \
	$(OUTDIR)/fns.obj \
	$(OUTDIR)/font-lock.obj \
	$(OUTDIR)/frame-x.obj \
	$(OUTDIR)/frame.obj \
	$(OUTDIR)/free-hook.obj \
	$(OUTDIR)/general.obj \
	$(OUTDIR)/gif_err.obj \
	$(OUTDIR)/gifalloc.obj \
	$(OUTDIR)/glyphs-x.obj \
	$(OUTDIR)/glyphs.obj \
	$(OUTDIR)/gmalloc.obj \
	$(OUTDIR)/gui-x.obj \
	$(OUTDIR)/gui.obj \
	$(OUTDIR)/hash.obj \
	$(OUTDIR)/indent.obj \
	$(OUTDIR)/inline.obj \
	$(OUTDIR)/insdel.obj \
	$(OUTDIR)/intl.obj \
	$(OUTDIR)/keymap.obj \
	$(OUTDIR)/lread.obj \
	$(OUTDIR)/lstream.obj \
	$(OUTDIR)/macros.obj \
	$(OUTDIR)/marker.obj \
	$(OUTDIR)/md5.obj \
	$(OUTDIR)/menubar-x.obj \
	$(OUTDIR)/menubar.obj \
	$(OUTDIR)/minibuf.obj \
	$(OUTDIR)/mocklisp.obj \
	$(OUTDIR)/nt.obj \
	$(OUTDIR)/ntheap.obj \
	$(OUTDIR)/ntproc.obj \
	$(OUTDIR)/objects-x.obj \
	$(OUTDIR)/objects.obj \
	$(OUTDIR)/opaque.obj \
	$(OUTDIR)/print.obj \
	$(OUTDIR)/process.obj \
	$(OUTDIR)/pure.obj \
	$(OUTDIR)/rangetab.obj \
	$(OUTDIR)/realpath.obj \
	$(OUTDIR)/redisplay-output.obj \
	$(OUTDIR)/redisplay-x.obj \
	$(OUTDIR)/redisplay.obj \
	$(OUTDIR)/regex.obj \
	$(OUTDIR)/scrollbar-x.obj \
	$(OUTDIR)/scrollbar.obj \
	$(OUTDIR)/search.obj \
	$(OUTDIR)/signal.obj \
	$(OUTDIR)/sound.obj \
	$(OUTDIR)/specifier.obj \
	$(OUTDIR)/strftime.obj \
	$(OUTDIR)/symbols.obj \
	$(OUTDIR)/syntax.obj \
	$(OUTDIR)/sysdep.obj \
	$(OUTDIR)/termcap.obj \
	$(OUTDIR)/tparam.obj \
	$(OUTDIR)/undo.obj \
	$(OUTDIR)/unexnt.obj \
	$(OUTDIR)/vm-limit.obj \
	$(OUTDIR)/window.obj \
	$(OUTDIR)/xgccache.obj \
	$(OUTDIR)/xmu.obj \
	$(OUTDIR)/xselect.obj

#$(TEMACS): $(TEMACS_SRC)/Emacs.ad.h $(TEMACS_SRC)/paths.h $(TEMACS_OBJS)
#	link.exe $(TEMACS_LFLAGS) $(TEMACS_OBJS) $(TEMACS_LIBS)

$(TEMACS): $(TEMACS_SRC)/Emacs.ad.h $(TEMACS_SRC)/paths.h $(TEMACS_OBJS)
	link.exe @<<
  $(TEMACS_LFLAGS) $(TEMACS_OBJS) $(TEMACS_LIBS)
<<

$(OUTDIR)/abbrev.obj:	$(TEMACS_SRC)/abbrev.c
#	!"$(TEMACS_CPP) $(TEMACS_CPP_FLAGS) $** > $(OUTDIR)/foo.c"
	$(CC) $(TEMACS_FLAGS) $** /Fo$@
#        !"del $(OUTDIR)\\foo.c"

$(OUTDIR)/alloc.obj:	$(TEMACS_SRC)/alloc.c
#	!"$(TEMACS_CPP) $(TEMACS_CPP_FLAGS) $** > $(OUTDIR)/foo.c"
	$(CC) $(TEMACS_FLAGS) $** /Fo$@
#	!"del $(OUTDIR)\\foo.c"

$(OUTDIR)/alloca.obj:	$(TEMACS_SRC)/alloca.c
#	!"$(TEMACS_CPP) $(TEMACS_CPP_FLAGS) $** > $(OUTDIR)/foo.c"
	$(CC) $(TEMACS_FLAGS) $** /Fo$@
#	!"del $(OUTDIR)\\foo.c"

$(OUTDIR)/blocktype.obj:	$(TEMACS_SRC)/blocktype.c
#	!"$(TEMACS_CPP) $(TEMACS_CPP_FLAGS) $** > $(OUTDIR)/foo.c"
	$(CC) $(TEMACS_FLAGS) $** /Fo$@
#	!"del $(OUTDIR)\\foo.c"

$(OUTDIR)/buffer.obj:	$(TEMACS_SRC)/buffer.c
#	!"$(TEMACS_CPP) $(TEMACS_CPP_FLAGS) $** > $(OUTDIR)/foo.c"
	$(CC) $(TEMACS_FLAGS) $** /Fo$@
#	!"del $(OUTDIR)\\foo.c"

$(OUTDIR)/bytecode.obj:	$(TEMACS_SRC)/bytecode.c
#	!"$(TEMACS_CPP) $(TEMACS_CPP_FLAGS) $** > $(OUTDIR)/foo.c"
	$(CC) $(TEMACS_FLAGS) $** /Fo$@
#	!"del $(OUTDIR)\\foo.c"

$(OUTDIR)/callint.obj:	$(TEMACS_SRC)/callint.c
#	!"$(TEMACS_CPP) $(TEMACS_CPP_FLAGS) $** > $(OUTDIR)/foo.c"
	$(CC) $(TEMACS_FLAGS) $** /Fo$@
#	!"del $(OUTDIR)\\foo.c"

$(OUTDIR)/callproc.obj:	$(TEMACS_SRC)/callproc.c
#	!"$(TEMACS_CPP) $(TEMACS_CPP_FLAGS) $** > $(OUTDIR)/foo.c"
	$(CC) $(TEMACS_FLAGS) $** /Fo$@
#	!"del $(OUTDIR)\\foo.c"

$(OUTDIR)/casefiddle.obj:	$(TEMACS_SRC)/casefiddle.c
#	!"$(TEMACS_CPP) $(TEMACS_CPP_FLAGS) $** > $(OUTDIR)/foo.c"
	$(CC) $(TEMACS_FLAGS) $** /Fo$@
#	!"del $(OUTDIR)\\foo.c"

$(OUTDIR)/casetab.obj:	$(TEMACS_SRC)/casetab.c
#	!"$(TEMACS_CPP) $(TEMACS_CPP_FLAGS) $** > $(OUTDIR)/foo.c"
	$(CC) $(TEMACS_FLAGS) $** /Fo$@
#	!"del $(OUTDIR)\\foo.c"

$(OUTDIR)/chartab.obj:	$(TEMACS_SRC)/chartab.c
#	!"$(TEMACS_CPP) $(TEMACS_CPP_FLAGS) $** > $(OUTDIR)/foo.c"
	$(CC) $(TEMACS_FLAGS) $** /Fo$@
#	!"del $(OUTDIR)\\foo.c"

$(OUTDIR)/cmdloop.obj:	$(TEMACS_SRC)/cmdloop.c
#	!"$(TEMACS_CPP) $(TEMACS_CPP_FLAGS) $** > $(OUTDIR)/foo.c"
	$(CC) $(TEMACS_FLAGS) $** /Fo$@
#	!"del $(OUTDIR)\\foo.c"

$(OUTDIR)/cmds.obj:	$(TEMACS_SRC)/cmds.c
#	!"$(TEMACS_CPP) $(TEMACS_CPP_FLAGS) $** > $(OUTDIR)/foo.c"
	$(CC) $(TEMACS_FLAGS) $** /Fo$@
#	!"del $(OUTDIR)\\foo.c"

$(OUTDIR)/console-stream.obj:	$(TEMACS_SRC)/console-stream.c
#	!"$(TEMACS_CPP) $(TEMACS_CPP_FLAGS) $** > $(OUTDIR)/foo.c"
	$(CC) $(TEMACS_FLAGS) $** /Fo$@
#	!"del $(OUTDIR)\\foo.c"

$(OUTDIR)/console-x.obj:	$(TEMACS_SRC)/console-x.c
#	!"$(TEMACS_CPP) $(TEMACS_CPP_FLAGS) $** > $(OUTDIR)/foo.c"
	$(CC) $(TEMACS_FLAGS) $** /Fo$@
#	!"del $(OUTDIR)\\foo.c"

$(OUTDIR)/console.obj:	$(TEMACS_SRC)/console.c
#	!"$(TEMACS_CPP) $(TEMACS_CPP_FLAGS) $** > $(OUTDIR)/foo.c"
	$(CC) $(TEMACS_FLAGS) $** /Fo$@
#	!"del $(OUTDIR)\\foo.c"

$(OUTDIR)/data.obj:	$(TEMACS_SRC)/data.c
#	!"$(TEMACS_CPP) $(TEMACS_CPP_FLAGS) $** > $(OUTDIR)/foo.c"
	$(CC) $(TEMACS_FLAGS) $** /Fo$@
#	!"del $(OUTDIR)\\foo.c"

$(OUTDIR)/database.obj:	$(TEMACS_SRC)/database.c
#	!"$(TEMACS_CPP) $(TEMACS_CPP_FLAGS) $** > $(OUTDIR)/foo.c"
	$(CC) $(TEMACS_FLAGS) $** /Fo$@
#	!"del $(OUTDIR)\\foo.c"

$(OUTDIR)/debug.obj:	$(TEMACS_SRC)/debug.c
#	!"$(TEMACS_CPP) $(TEMACS_CPP_FLAGS) $** > $(OUTDIR)/foo.c"
	$(CC) $(TEMACS_FLAGS) $** /Fo$@
#	!"del $(OUTDIR)\\foo.c"

$(OUTDIR)/device-x.obj:	$(TEMACS_SRC)/device-x.c
#	!"$(TEMACS_CPP) $(TEMACS_CPP_FLAGS) $** > $(OUTDIR)/foo.c"
	$(CC) $(TEMACS_FLAGS) $** /Fo$@
#	!"del $(OUTDIR)\\foo.c"

$(OUTDIR)/device.obj:	$(TEMACS_SRC)/device.c
#	!"$(TEMACS_CPP) $(TEMACS_CPP_FLAGS) $** > $(OUTDIR)/foo.c"
	$(CC) $(TEMACS_FLAGS) $** /Fo$@
#	!"del $(OUTDIR)\\foo.c"

$(OUTDIR)/dgif_lib.obj:	$(TEMACS_SRC)/dgif_lib.c
#	!"$(TEMACS_CPP) $(TEMACS_CPP_FLAGS) $** > $(OUTDIR)/foo.c"
	$(CC) $(TEMACS_FLAGS) $** /Fo$@
#	!"del $(OUTDIR)\\foo.c"

$(OUTDIR)/dialog-x.obj:	$(TEMACS_SRC)/dialog-x.c
#	!"$(TEMACS_CPP) $(TEMACS_CPP_FLAGS) $** > $(OUTDIR)/foo.c"
	$(CC) $(TEMACS_FLAGS) $** /Fo$@
#	!"del $(OUTDIR)\\foo.c"

$(OUTDIR)/dialog.obj:	$(TEMACS_SRC)/dialog.c
#	!"$(TEMACS_CPP) $(TEMACS_CPP_FLAGS) $** > $(OUTDIR)/foo.c"
	$(CC) $(TEMACS_FLAGS) $** /Fo$@
#	!"del $(OUTDIR)\\foo.c"

$(OUTDIR)/dired.obj:	$(TEMACS_SRC)/dired.c
#	!"$(TEMACS_CPP) $(TEMACS_CPP_FLAGS) $** > $(OUTDIR)/foo.c"
	$(CC) $(TEMACS_FLAGS) $** /Fo$@
#	!"del $(OUTDIR)\\foo.c"

$(OUTDIR)/doc.obj:	$(TEMACS_SRC)/doc.c
#	!"$(TEMACS_CPP) $(TEMACS_CPP_FLAGS) $** > $(OUTDIR)/foo.c"
	$(CC) $(TEMACS_FLAGS) $** /Fo$@
#	!"del $(OUTDIR)\\foo.c"

$(OUTDIR)/doprnt.obj:	$(TEMACS_SRC)/doprnt.c
#	!"$(TEMACS_CPP) $(TEMACS_CPP_FLAGS) $** > $(OUTDIR)/foo.c"
	$(CC) $(TEMACS_FLAGS) $** /Fo$@
#	!"del $(OUTDIR)\\foo.c"

$(OUTDIR)/dynarr.obj:	$(TEMACS_SRC)/dynarr.c
#	!"$(TEMACS_CPP) $(TEMACS_CPP_FLAGS) $** > $(OUTDIR)/foo.c"
	$(CC) $(TEMACS_FLAGS) $** /Fo$@
#	!"del $(OUTDIR)\\foo.c"

$(OUTDIR)/editfns.obj:	$(TEMACS_SRC)/editfns.c
#	!"$(TEMACS_CPP) $(TEMACS_CPP_FLAGS) $** > $(OUTDIR)/foo.c"
	$(CC) $(TEMACS_FLAGS) $** /Fo$@
#	!"del $(OUTDIR)\\foo.c"

$(OUTDIR)/elhash.obj:	$(TEMACS_SRC)/elhash.c
#	!"$(TEMACS_CPP) $(TEMACS_CPP_FLAGS) $** > $(OUTDIR)/foo.c"
	$(CC) $(TEMACS_FLAGS) $** /Fo$@
#	!"del $(OUTDIR)\\foo.c"

$(OUTDIR)/emacs.obj:	$(TEMACS_SRC)/emacs.c
#	!"$(TEMACS_CPP) $(TEMACS_CPP_FLAGS) $** > $(OUTDIR)/foo.c"
	$(CC) $(TEMACS_FLAGS) $** /Fo$@
#	!"del $(OUTDIR)\\foo.c"

$(OUTDIR)/EmacsFrame.obj:	$(TEMACS_SRC)/EmacsFrame.c
#	!"$(TEMACS_CPP) $(TEMACS_CPP_FLAGS) $** > $(OUTDIR)/foo.c"
	$(CC) $(TEMACS_FLAGS) $** /Fo$@
#	!"del $(OUTDIR)\\foo.c"

$(OUTDIR)/EmacsManager.obj:	$(TEMACS_SRC)/EmacsManager.c
#	!"$(TEMACS_CPP) $(TEMACS_CPP_FLAGS) $** > $(OUTDIR)/foo.c"
	$(CC) $(TEMACS_FLAGS) $** /Fo$@
#	!"del $(OUTDIR)\\foo.c"

$(OUTDIR)/TopLevelEmacsShell.obj:	$(TEMACS_SRC)/EmacsShell-sub.c
#	!"$(TEMACS_CPP) $(TEMACS_CPP_FLAGS) -DDEFINE_TOP_LEVEL_EMACS_SHELL $** > $(OUTDIR)/foo.c"
	$(CC) $(TEMACS_FLAGS) -DDEFINE_TOP_LEVEL_EMACS_SHELL $** /Fo$@
#	!"del $(OUTDIR)\\foo.c"

$(OUTDIR)/TransientEmacsShell.obj: $(TEMACS_SRC)/EmacsShell-sub.c
#	!"$(TEMACS_CPP) $(TEMACS_CPP_FLAGS) -DDEFINE_TRANSIENT_EMACS_SHELL $** > $(OUTDIR)/foo.c"
	$(CC) $(TEMACS_FLAGS) -DDEFINE_TRANSIENT_EMACS_SHELL $** /Fo$@
#	!"del $(OUTDIR)\\foo.c"

$(OUTDIR)/EmacsShell.obj:	$(TEMACS_SRC)/EmacsShell.c
#	!"$(TEMACS_CPP) $(TEMACS_CPP_FLAGS) $** > $(OUTDIR)/foo.c"
	$(CC) $(TEMACS_FLAGS) $** /Fo$@
#	!"del $(OUTDIR)\\foo.c"

$(OUTDIR)/energize.obj:	$(TEMACS_SRC)/energize.c
#	!"$(TEMACS_CPP) $(TEMACS_CPP_FLAGS) $** > $(OUTDIR)/foo.c"
	$(CC) $(TEMACS_FLAGS) $** /Fo$@
#	!"del $(OUTDIR)\\foo.c"

$(OUTDIR)/eval.obj:	$(TEMACS_SRC)/eval.c
#	!"$(TEMACS_CPP) $(TEMACS_CPP_FLAGS) $** > $(OUTDIR)/foo.c"
	$(CC) $(TEMACS_FLAGS) $** /Fo$@
#	!"del $(OUTDIR)\\foo.c"

$(OUTDIR)/event-stream.obj:	$(TEMACS_SRC)/event-stream.c
#	!"$(TEMACS_CPP) $(TEMACS_CPP_FLAGS) $** > $(OUTDIR)/foo.c"
	$(CC) $(TEMACS_FLAGS) $** /Fo$@
#	!"del $(OUTDIR)\\foo.c"

$(OUTDIR)/event-unixoid.obj:	$(TEMACS_SRC)/event-unixoid.c
#	!"$(TEMACS_CPP) $(TEMACS_CPP_FLAGS) $** > $(OUTDIR)/foo.c"
	$(CC) $(TEMACS_FLAGS) $** /Fo$@
#	!"del $(OUTDIR)\\foo.c"

$(OUTDIR)/event-Xt.obj:	$(TEMACS_SRC)/event-Xt.c
#	!"$(TEMACS_CPP) $(TEMACS_CPP_FLAGS) $** > $(OUTDIR)/foo.c"
	$(CC) $(TEMACS_FLAGS) $** /Fo$@
#	!"del $(OUTDIR)\\foo.c"

$(OUTDIR)/events.obj:	$(TEMACS_SRC)/events.c
#	!"$(TEMACS_CPP) $(TEMACS_CPP_FLAGS) $** > $(OUTDIR)/foo.c"
	$(CC) $(TEMACS_FLAGS) $** /Fo$@
#	!"del $(OUTDIR)\\foo.c"

$(OUTDIR)/extents.obj:	$(TEMACS_SRC)/extents.c
#	!"$(TEMACS_CPP) $(TEMACS_CPP_FLAGS) $** > $(OUTDIR)/foo.c"
	$(CC) $(TEMACS_FLAGS) $** /Fo$@
#	!"del $(OUTDIR)\\foo.c"

$(OUTDIR)/faces.obj:	$(TEMACS_SRC)/faces.c
#	!"$(TEMACS_CPP) $(TEMACS_CPP_FLAGS) $** > $(OUTDIR)/foo.c"
	$(CC) $(TEMACS_FLAGS) $** /Fo$@
#	!"del $(OUTDIR)\\foo.c"

$(OUTDIR)/fileio.obj:	$(TEMACS_SRC)/fileio.c
#	!"$(TEMACS_CPP) $(TEMACS_CPP_FLAGS) $** > $(OUTDIR)/foo.c"
	$(CC) $(TEMACS_FLAGS) $** /Fo$@
#	!"del $(OUTDIR)\\foo.c"

$(OUTDIR)/filelock.obj:	$(TEMACS_SRC)/filelock.c
#	!"$(TEMACS_CPP) $(TEMACS_CPP_FLAGS) $** > $(OUTDIR)/foo.c"
	$(CC) $(TEMACS_FLAGS) $** /Fo$@
#	!"del $(OUTDIR)\\foo.c"

$(OUTDIR)/filemode.obj:	$(TEMACS_SRC)/filemode.c
#	!"$(TEMACS_CPP) $(TEMACS_CPP_FLAGS) $** > $(OUTDIR)/foo.c"
	$(CC) $(TEMACS_FLAGS) $** /Fo$@
#	!"del $(OUTDIR)\\foo.c"

$(OUTDIR)/floatfns.obj:	$(TEMACS_SRC)/floatfns.c
#	!"$(TEMACS_CPP) $(TEMACS_CPP_FLAGS) $** > $(OUTDIR)/foo.c"
	$(CC) $(TEMACS_FLAGS) $** /Fo$@
#	!"del $(OUTDIR)\\foo.c"

$(OUTDIR)/fns.obj:	$(TEMACS_SRC)/fns.c
#	!"$(TEMACS_CPP) $(TEMACS_CPP_FLAGS) $** > $(OUTDIR)/foo.c"
	$(CC) $(TEMACS_FLAGS) $** /Fo$@
#	!"del $(OUTDIR)\\foo.c"

$(OUTDIR)/font-lock.obj:	$(TEMACS_SRC)/font-lock.c
#	!"$(TEMACS_CPP) $(TEMACS_CPP_FLAGS) $** > $(OUTDIR)/foo.c"
	$(CC) $(TEMACS_FLAGS) $** /Fo$@
#	!"del $(OUTDIR)\\foo.c"

$(OUTDIR)/frame-x.obj:	$(TEMACS_SRC)/frame-x.c
#	!"$(TEMACS_CPP) $(TEMACS_CPP_FLAGS) $** > $(OUTDIR)/foo.c"
	$(CC) $(TEMACS_FLAGS) $** /Fo$@
#	!"del $(OUTDIR)\\foo.c"

$(OUTDIR)/frame.obj:	$(TEMACS_SRC)/frame.c
#	!"$(TEMACS_CPP) $(TEMACS_CPP_FLAGS) $** > $(OUTDIR)/foo.c"
	$(CC) $(TEMACS_FLAGS) $** /Fo$@
#	!"del $(OUTDIR)\\foo.c"

$(OUTDIR)/free-hook.obj:	$(TEMACS_SRC)/free-hook.c
#	!"$(TEMACS_CPP) $(TEMACS_CPP_FLAGS) $** > $(OUTDIR)/foo.c"
	$(CC) $(TEMACS_FLAGS) $** /Fo$@
#	!"del $(OUTDIR)\\foo.c"

$(OUTDIR)/general.obj:	$(TEMACS_SRC)/general.c
#	!"$(TEMACS_CPP) $(TEMACS_CPP_FLAGS) $** > $(OUTDIR)/foo.c"
	$(CC) $(TEMACS_FLAGS) $** /Fo$@
#	!"del $(OUTDIR)\\foo.c"

$(OUTDIR)/gif_err.obj:	$(TEMACS_SRC)/gif_err.c
#	!"$(TEMACS_CPP) $(TEMACS_CPP_FLAGS) $** > $(OUTDIR)/foo.c"
	$(CC) $(TEMACS_FLAGS) $** /Fo$@
#	!"del $(OUTDIR)\\foo.c"

$(OUTDIR)/gifalloc.obj:	$(TEMACS_SRC)/gifalloc.c
#	!"$(TEMACS_CPP) $(TEMACS_CPP_FLAGS) $** > $(OUTDIR)/foo.c"
	$(CC) $(TEMACS_FLAGS) $** /Fo$@
#	!"del $(OUTDIR)\\foo.c"

$(OUTDIR)/glyphs-x.obj:	$(TEMACS_SRC)/glyphs-x.c
#	!"$(TEMACS_CPP) $(TEMACS_CPP_FLAGS) $** > $(OUTDIR)/foo.c"
	$(CC) $(TEMACS_FLAGS) $** /Fo$@
#	!"del $(OUTDIR)\\foo.c"

$(OUTDIR)/glyphs.obj:	$(TEMACS_SRC)/glyphs.c
#	!"$(TEMACS_CPP) $(TEMACS_CPP_FLAGS) $** > $(OUTDIR)/foo.c"
	$(CC) $(TEMACS_FLAGS) $** /Fo$@
#	!"del $(OUTDIR)\\foo.c"

$(OUTDIR)/gmalloc.obj:	$(TEMACS_SRC)/gmalloc.c
#	!"$(TEMACS_CPP) $(TEMACS_CPP_FLAGS) $** > $(OUTDIR)/foo.c"
	$(CC) $(TEMACS_FLAGS) $** /Fo$@
#	!"del $(OUTDIR)\\foo.c"

$(OUTDIR)/gui-x.obj:	$(TEMACS_SRC)/gui-x.c
#	!"$(TEMACS_CPP) $(TEMACS_CPP_FLAGS) $** > $(OUTDIR)/foo.c"
	$(CC) $(TEMACS_FLAGS) $** /Fo$@
#	!"del $(OUTDIR)\\foo.c"

$(OUTDIR)/gui.obj:	$(TEMACS_SRC)/gui.c
#	!"$(TEMACS_CPP) $(TEMACS_CPP_FLAGS) $** > $(OUTDIR)/foo.c"
	$(CC) $(TEMACS_FLAGS) $** /Fo$@
#	!"del $(OUTDIR)\\foo.c"

$(OUTDIR)/hash.obj:	$(TEMACS_SRC)/hash.c
#	!"$(TEMACS_CPP) $(TEMACS_CPP_FLAGS) $** > $(OUTDIR)/foo.c"
	$(CC) $(TEMACS_FLAGS) $** /Fo$@
#	!"del $(OUTDIR)\\foo.c"

$(OUTDIR)/indent.obj:	$(TEMACS_SRC)/indent.c
#	!"$(TEMACS_CPP) $(TEMACS_CPP_FLAGS) $** > $(OUTDIR)/foo.c"
	$(CC) $(TEMACS_FLAGS) $** /Fo$@
#	!"del $(OUTDIR)\\foo.c"

$(OUTDIR)/inline.obj:	$(TEMACS_SRC)/inline.c
#	!"$(TEMACS_CPP) $(TEMACS_CPP_FLAGS) $** > $(OUTDIR)/foo.c"
	$(CC) $(TEMACS_FLAGS) $** /Fo$@
#	!"del $(OUTDIR)\\foo.c"

$(OUTDIR)/insdel.obj:	$(TEMACS_SRC)/insdel.c
#	!"$(TEMACS_CPP) $(TEMACS_CPP_FLAGS) $** > $(OUTDIR)/foo.c"
	$(CC) $(TEMACS_FLAGS) $** /Fo$@
#	!"del $(OUTDIR)\\foo.c"

$(OUTDIR)/intl.obj:	$(TEMACS_SRC)/intl.c
#	!"$(TEMACS_CPP) $(TEMACS_CPP_FLAGS) $** > $(OUTDIR)/foo.c"
	$(CC) $(TEMACS_FLAGS) $** /Fo$@
#	!"del $(OUTDIR)\\foo.c"

$(OUTDIR)/keymap.obj:	$(TEMACS_SRC)/keymap.c
#	!"$(TEMACS_CPP) $(TEMACS_CPP_FLAGS) $** > $(OUTDIR)/foo.c"
	$(CC) $(TEMACS_FLAGS) $** /Fo$@
#	!"del $(OUTDIR)\\foo.c"

$(OUTDIR)/lread.obj:	$(TEMACS_SRC)/lread.c
#	!"$(TEMACS_CPP) $(TEMACS_CPP_FLAGS) $** > $(OUTDIR)/foo.c"
	$(CC) $(TEMACS_FLAGS) $** /Fo$@
#	!"del $(OUTDIR)\\foo.c"

$(OUTDIR)/lstream.obj:	$(TEMACS_SRC)/lstream.c
#	!"$(TEMACS_CPP) $(TEMACS_CPP_FLAGS) $** > $(OUTDIR)/foo.c"
	$(CC) $(TEMACS_FLAGS) $** /Fo$@
#	!"del $(OUTDIR)\\foo.c"

$(OUTDIR)/macros.obj:	$(TEMACS_SRC)/macros.c
#	!"$(TEMACS_CPP) $(TEMACS_CPP_FLAGS) $** > $(OUTDIR)/foo.c"
	$(CC) $(TEMACS_FLAGS) $** /Fo$@
#	!"del $(OUTDIR)\\foo.c"

$(OUTDIR)/marker.obj:	$(TEMACS_SRC)/marker.c
#	!"$(TEMACS_CPP) $(TEMACS_CPP_FLAGS) $** > $(OUTDIR)/foo.c"
	$(CC) $(TEMACS_FLAGS) $** /Fo$@
#	!"del $(OUTDIR)\\foo.c"

$(OUTDIR)/md5.obj:	$(TEMACS_SRC)/md5.c
#	!"$(TEMACS_CPP) $(TEMACS_CPP_FLAGS) $** > $(OUTDIR)/foo.c"
	$(CC) $(TEMACS_FLAGS) $** /Fo$@
#	!"del $(OUTDIR)\\foo.c"

$(OUTDIR)/menubar-x.obj:	$(TEMACS_SRC)/menubar-x.c
#	!"$(TEMACS_CPP) $(TEMACS_CPP_FLAGS) $** > $(OUTDIR)/foo.c"
	$(CC) $(TEMACS_FLAGS) $** /Fo$@
#	!"del $(OUTDIR)\\foo.c"

$(OUTDIR)/menubar.obj:	$(TEMACS_SRC)/menubar.c
#	!"$(TEMACS_CPP) $(TEMACS_CPP_FLAGS) $** > $(OUTDIR)/foo.c"
	$(CC) $(TEMACS_FLAGS) $** /Fo$@
#	!"del $(OUTDIR)\\foo.c"

$(OUTDIR)/minibuf.obj:	$(TEMACS_SRC)/minibuf.c
#	!"$(TEMACS_CPP) $(TEMACS_CPP_FLAGS) $** > $(OUTDIR)/foo.c"
	$(CC) $(TEMACS_FLAGS) $** /Fo$@
#	!"del $(OUTDIR)\\foo.c"

$(OUTDIR)/mocklisp.obj:	$(TEMACS_SRC)/mocklisp.c
#	!"$(TEMACS_CPP) $(TEMACS_CPP_FLAGS) $** > $(OUTDIR)/foo.c"
	$(CC) $(TEMACS_FLAGS) $** /Fo$@
#	!"del $(OUTDIR)\\foo.c"

$(OUTDIR)/msdos.obj:	$(TEMACS_SRC)/msdos.c
#	!"$(TEMACS_CPP) $(TEMACS_CPP_FLAGS) $** > $(OUTDIR)/foo.c"
	$(CC) $(TEMACS_FLAGS) $** /Fo$@
#	!"del $(OUTDIR)\\foo.c"

$(OUTDIR)/nt.obj:	$(TEMACS_SRC)/nt.c
#	!"$(TEMACS_CPP) $(TEMACS_CPP_FLAGS) $** > $(OUTDIR)/foo.c"
	$(CC) $(TEMACS_FLAGS) $** /Fo$@
#	!"del $(OUTDIR)\\foo.c"

$(OUTDIR)/ntheap.obj:	$(TEMACS_SRC)/ntheap.c
#	!"$(TEMACS_CPP) $(TEMACS_CPP_FLAGS) $** > $(OUTDIR)/foo.c"
	$(CC) $(TEMACS_FLAGS) $** /Fo$@
#	!"del $(OUTDIR)\\foo.c"

$(OUTDIR)/ntproc.obj:	$(TEMACS_SRC)/ntproc.c
#	!"$(TEMACS_CPP) $(TEMACS_CPP_FLAGS) $** > $(OUTDIR)/foo.c"
	$(CC) $(TEMACS_FLAGS) $** /Fo$@
#	!"del $(OUTDIR)\\foo.c"

$(OUTDIR)/objects-x.obj:	$(TEMACS_SRC)/objects-x.c
#	!"$(TEMACS_CPP) $(TEMACS_CPP_FLAGS) $** > $(OUTDIR)/foo.c"
	$(CC) $(TEMACS_FLAGS) $** /Fo$@
#	!"del $(OUTDIR)\\foo.c"

$(OUTDIR)/objects.obj:	$(TEMACS_SRC)/objects.c
#	!"$(TEMACS_CPP) $(TEMACS_CPP_FLAGS) $** > $(OUTDIR)/foo.c"
	$(CC) $(TEMACS_FLAGS) $** /Fo$@
#	!"del $(OUTDIR)\\foo.c"

$(OUTDIR)/opaque.obj:	$(TEMACS_SRC)/opaque.c
#	!"$(TEMACS_CPP) $(TEMACS_CPP_FLAGS) $** > $(OUTDIR)/foo.c"
	$(CC) $(TEMACS_FLAGS) $** /Fo$@
#	!"del $(OUTDIR)\\foo.c"

$(OUTDIR)/print.obj:	$(TEMACS_SRC)/print.c
#	!"$(TEMACS_CPP) $(TEMACS_CPP_FLAGS) $** > $(OUTDIR)/foo.c"
	$(CC) $(TEMACS_FLAGS) $** /Fo$@
#	!"del $(OUTDIR)\\foo.c"

$(OUTDIR)/process.obj:	$(TEMACS_SRC)/process.c
#	!"$(TEMACS_CPP) $(TEMACS_CPP_FLAGS) $** > $(OUTDIR)/foo.c"
	$(CC) $(TEMACS_FLAGS)  $** /Fo$@
#	!"del $(OUTDIR)\\foo.c"

$(OUTDIR)/pure.obj:	$(TEMACS_SRC)/pure.c
#	!"$(TEMACS_CPP) $(TEMACS_CPP_FLAGS) $** > $(OUTDIR)/foo.c"
	$(CC) $(TEMACS_FLAGS) $** /Fo$@
#	!"del $(OUTDIR)\\foo.c"

$(OUTDIR)/rangetab.obj:	$(TEMACS_SRC)/rangetab.c
#	!"$(TEMACS_CPP) $(TEMACS_CPP_FLAGS) $** > $(OUTDIR)/foo.c"
	$(CC) $(TEMACS_FLAGS) $** /Fo$@
#	!"del $(OUTDIR)\\foo.c"

$(OUTDIR)/realpath.obj:	$(TEMACS_SRC)/realpath.c
#	!"$(TEMACS_CPP) $(TEMACS_CPP_FLAGS) $** > $(OUTDIR)/foo.c"
	$(CC) $(TEMACS_FLAGS) $** /Fo$@
#	!"del $(OUTDIR)\\foo.c"

$(OUTDIR)/redisplay-output.obj:	$(TEMACS_SRC)/redisplay-output.c
#	!"$(TEMACS_CPP) $(TEMACS_CPP_FLAGS) $** > $(OUTDIR)/foo.c"
	$(CC) $(TEMACS_FLAGS) $** /Fo$@
#	!"del $(OUTDIR)\\foo.c"

$(OUTDIR)/redisplay-x.obj:	$(TEMACS_SRC)/redisplay-x.c
#	!"$(TEMACS_CPP) $(TEMACS_CPP_FLAGS) $** > $(OUTDIR)/foo.c"
	$(CC) $(TEMACS_FLAGS) $** /Fo$@
#	!"del $(OUTDIR)\\foo.c"

$(OUTDIR)/redisplay.obj:	$(TEMACS_SRC)/redisplay.c
#	!"$(TEMACS_CPP) $(TEMACS_CPP_FLAGS) $** > $(OUTDIR)/foo.c"
	$(CC) $(TEMACS_FLAGS) $** /Fo$@
#	!"del $(OUTDIR)\\foo.c"

$(OUTDIR)/regex.obj:	$(TEMACS_SRC)/regex.c
#	!"$(TEMACS_CPP) $(TEMACS_CPP_FLAGS) $** > $(OUTDIR)/foo.c"
	$(CC) $(TEMACS_FLAGS) $** /Fo$@
#	!"del $(OUTDIR)\\foo.c"

$(OUTDIR)/scrollbar-x.obj:	$(TEMACS_SRC)/scrollbar-x.c
#	!"$(TEMACS_CPP) $(TEMACS_CPP_FLAGS) $** > $(OUTDIR)/foo.c"
	$(CC) $(TEMACS_FLAGS) $** /Fo$@
#	!"del $(OUTDIR)\\foo.c"

$(OUTDIR)/scrollbar.obj:	$(TEMACS_SRC)/scrollbar.c
#	!"$(TEMACS_CPP) $(TEMACS_CPP_FLAGS) $** > $(OUTDIR)/foo.c"
	$(CC) $(TEMACS_FLAGS) $** /Fo$@
#	!"del $(OUTDIR)\\foo.c"

$(OUTDIR)/search.obj:	$(TEMACS_SRC)/search.c
#	!"$(TEMACS_CPP) $(TEMACS_CPP_FLAGS) $** > $(OUTDIR)/foo.c"
	$(CC) $(TEMACS_FLAGS) $** /Fo$@
#	!"del $(OUTDIR)\\foo.c"

$(OUTDIR)/signal.obj:	$(TEMACS_SRC)/signal.c
#	!"$(TEMACS_CPP) $(TEMACS_CPP_FLAGS) $** > $(OUTDIR)/foo.c"
	$(CC) $(TEMACS_FLAGS) $** /Fo$@
#	!"del $(OUTDIR)\\foo.c"

$(OUTDIR)/sound.obj:	$(TEMACS_SRC)/sound.c
#	!"$(TEMACS_CPP) $(TEMACS_CPP_FLAGS) $** > $(OUTDIR)/foo.c"
	$(CC) $(TEMACS_FLAGS) $** /Fo$@
#	!"del $(OUTDIR)\\foo.c"

$(OUTDIR)/specifier.obj:	$(TEMACS_SRC)/specifier.c
#	!"$(TEMACS_CPP) $(TEMACS_CPP_FLAGS) $** > $(OUTDIR)/foo.c"
	$(CC) $(TEMACS_FLAGS) $** /Fo$@
#	!"del $(OUTDIR)\\foo.c"

$(OUTDIR)/strftime.obj:	$(TEMACS_SRC)/strftime.c
#	!"$(TEMACS_CPP) $(TEMACS_CPP_FLAGS) $** > $(OUTDIR)/foo.c"
	$(CC) $(TEMACS_FLAGS) $** /Fo$@
#	!"del $(OUTDIR)\\foo.c"

$(OUTDIR)/symbols.obj:	$(TEMACS_SRC)/symbols.c
#	!"$(TEMACS_CPP) $(TEMACS_CPP_FLAGS) $** > $(OUTDIR)/foo.c"
	$(CC) $(TEMACS_FLAGS) $** /Fo$@
#	!"del $(OUTDIR)\\foo.c"

$(OUTDIR)/syntax.obj:	$(TEMACS_SRC)/syntax.c
#	!"$(TEMACS_CPP) $(TEMACS_CPP_FLAGS) $** > $(OUTDIR)/foo.c"
	$(CC) $(TEMACS_FLAGS) $** /Fo$@
#	!"del $(OUTDIR)\\foo.c"

$(OUTDIR)/sysdep.obj:	$(TEMACS_SRC)/sysdep.c
#	!"$(TEMACS_CPP) $(TEMACS_CPP_FLAGS) $** > $(OUTDIR)/foo.c"
	$(CC) $(TEMACS_FLAGS) $** /Fo$@
#	!"del $(OUTDIR)\\foo.c"

$(OUTDIR)/termcap.obj:	$(TEMACS_SRC)/termcap.c
#	!"$(TEMACS_CPP) $(TEMACS_CPP_FLAGS) $** > $(OUTDIR)/foo.c"
	$(CC) $(TEMACS_FLAGS) $** /Fo$@
#	!"del $(OUTDIR)\\foo.c"

$(OUTDIR)/tparam.obj:	$(TEMACS_SRC)/tparam.c
#	!"$(TEMACS_CPP) $(TEMACS_CPP_FLAGS) $** > $(OUTDIR)/foo.c"
	$(CC) $(TEMACS_FLAGS) $** /Fo$@
#	!"del $(OUTDIR)\\foo.c"

$(OUTDIR)/undo.obj:	$(TEMACS_SRC)/undo.c
#	!"$(TEMACS_CPP) $(TEMACS_CPP_FLAGS) $** > $(OUTDIR)/foo.c"
	$(CC) $(TEMACS_FLAGS) $** /Fo$@
#	!"del $(OUTDIR)\\foo.c"

$(OUTDIR)/unexnt.obj:	$(TEMACS_SRC)/unexnt.c
#	!"$(TEMACS_CPP) $(TEMACS_CPP_FLAGS) $** > $(OUTDIR)/foo.c"
	$(CC) $(TEMACS_FLAGS) $** /Fo$@
#	!"del $(OUTDIR)\\foo.c"

$(OUTDIR)/vm-limit.obj:	$(TEMACS_SRC)/vm-limit.c
#	!"$(TEMACS_CPP) $(TEMACS_CPP_FLAGS) $** > $(OUTDIR)/foo.c"
	$(CC) $(TEMACS_FLAGS) $** /Fo$@
#	!"del $(OUTDIR)\\foo.c"

$(OUTDIR)/window.obj:	$(TEMACS_SRC)/window.c
#	!"$(TEMACS_CPP) $(TEMACS_CPP_FLAGS) $** > $(OUTDIR)/foo.c"
	$(CC) $(TEMACS_FLAGS)  $** /Fo$@
#	!"del $(OUTDIR)\\foo.c"

$(OUTDIR)/xgccache.obj:	$(TEMACS_SRC)/xgccache.c
#	!"$(TEMACS_CPP) $(TEMACS_CPP_FLAGS) $** > $(OUTDIR)/foo.c"
	$(CC) $(TEMACS_FLAGS) $** /Fo$@
#	!"del $(OUTDIR)\\foo.c"

$(OUTDIR)/xmu.obj:	$(TEMACS_SRC)/xmu.c
#	!"$(TEMACS_CPP) $(TEMACS_CPP_FLAGS) $** > $(OUTDIR)/foo.c"
	$(CC) $(TEMACS_FLAGS) $** /Fo$@
#	!"del $(OUTDIR)\\foo.c"

$(OUTDIR)/xselect.obj:	$(TEMACS_SRC)/xselect.c
#	!"$(TEMACS_CPP) $(TEMACS_CPP_FLAGS) $** > $(OUTDIR)/foo.c"
	$(CC) $(TEMACS_FLAGS) $** /Fo$@
#	!"del $(OUTDIR)\\foo.c"

$(TEMACS_SRC)/Emacs.ad.h: $(XEMACS)/etc/Emacs.ad
	!"sed -f ad2c.sed < $(XEMACS)/etc/Emacs.ad > $(TEMACS_SRC)/Emacs.ad.h"

$(TEMACS_SRC)/paths.h: $(TEMACS_SRC)/paths.h.in
	!"cd $(TEMACS_SRC); cp paths.h.in paths.h"

#------------------------------------------------------------------------------

# LISP bits 'n bobs

dump-elcs: $(TEMACS)
        $(LOADPATH) $(TEMACS) -batch -l ../prim/update-elc.el $(LISP)

dump-xemacs: $(TEMACS)
	$(TEMACS) -batch -l loadup.el dump

#------------------------------------------------------------------------------

# use this rule to build the complete system
all: $(LASTFILE) $(LWLIB) $(TEMACS) $(SUPPORT_PROGS) $(DOC)

# use this rule to install the system
install:
