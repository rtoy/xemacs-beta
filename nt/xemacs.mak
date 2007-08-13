MSDEV=G:/msdev
X11R6=G:/utils/X11R6
XEMACS=..
LISP=$(XEMACS)/lisp

EMACS_MAJOR_VERSION=20
EMACS_MINOR_VERSION=3
XEMACS_CODENAME=\"Stockholm\"

#------------------------------------------------------------------------------

# Generic variables

INCLUDES=-I$(X11R6)/include -I$(XEMACS)/nt/inc -I$(XEMACS)/src\
 -I$(XEMACS)/lwlib -I$(MSDEV)/include
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
 /D "_NTSDK" /D "_M_IX86" /D "_X86_" \
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
 $(XEMACS)/src/xselect.c \
 $(XEMACS)/src/balloon-x.c \
 $(XEMACS)/src/balloon_help.c

DOC_SRCS_2=\
 $(XEMACS)/src/input-method-xlib.c \
 $(XEMACS)/src/mule.c \
 $(XEMACS)/src/mule-charset.c \
 $(XEMACS)/src/mule-ccl.c \
 $(XEMACS)/src/mule-coding.c

DOC_SRCS_3=\
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
 $(LISP)/prim/itimer-autosave.elc \
 $(LISP)/ediff/ediff-hook.elc \
 $(LISP)/packages/fontl-hooks.elc 

DOC_SRCS_4= \
 $(LISP)/prim/scrollbar.elc \
 $(LISP)/prim/buffer.elc \
 $(LISP)/prim/menubar.elc \
 $(LISP)/packages/buff-menu.elc  \
 $(LISP)/modes/abbrev.elc  \
 $(LISP)/x11/x-menubar.elc \
 $(LISP)/x11/x-faces.elc \
 $(LISP)/x11/x-iso8859-1.elc \
 $(LISP)/x11/x-mouse.elc \
 $(LISP)/x11/x-select.elc \
 $(LISP)/x11/x-scrollbar.elc \
 $(LISP)/x11/x-toolbar.elc \
 $(LISP)/x11/x-misc.elc \
 $(LISP)/x11/x-init.elc \
 $(LISP)/prim/dialog.elc \
 $(LISP)/mule/mule-load.elc \
 $(LISP)/mule/mule-coding.elc \
 $(LISP)/mule/mule-charset.elc \
 $(LISP)/mule/mule-files.elc \
 $(LISP)/mule/mule-category.elc \
 $(LISP)/mule/mule-misc.elc \
 $(LISP)/mule/mule-ccl.elc \
 $(LISP)/mule/mule-init.elc \
 $(LISP)/mule/mule-cmds.elc \
 $(LISP)/mule/kinsoku.elc \
 $(LISP)/mule/mule-x-init.elc \
 $(LISP)/mule/arabic-hooks.elc \
 $(LISP)/mule/chinese-hooks.elc \
 $(LISP)/mule/cyrillic-hooks.elc \
 $(LISP)/mule/ethiopic-hooks.elc \
 $(LISP)/mule/european-hooks.elc \
 $(LISP)/mule/greek-hooks.elc \
 $(LISP)/mule/hebrew-hooks.elc \
 $(LISP)/mule/ipa-hooks.elc \
 $(LISP)/mule/japanese-hooks.elc \
 $(LISP)/mule/korean-hooks.elc \
 $(LISP)/mule/thai-hooks.elc \
 $(LISP)/mule/vietnamese-hooks-1.elc \
 $(LISP)/mule/vietnamese-hooks-2.elc \
 $(LISP)/prim/winnt.el \
 $(LISP)/custom/widget.el \
 $(LISP)/custom/custom.el \
 $(LISP)/prim/cus-start.el

MAKE_DOCFILE=$(OUTDIR)\make-docfile.exe

$(MAKE_DOCFILE): $(OUTDIR)\make-docfile.obj
	link.exe $(LIB_SRC_LFLAGS) $** $(LIB_SRC_LIBS)

$(OUTDIR)/make-docfile.obj:	$(LIB_SRC)\make-docfile.c
	 $(CC) $(LIB_SRC_FLAGS) $** /Fo$@

RUNEMACS=$(XEMACS)/src/runemacs.exe

$(RUNEMACS): $(OUTDIR)\runemacs.obj
	link.exe /out:$@ /subsystem:windows /entry:WinMainCRTStartup \
	/pdb:none /release /incremental:no $** \
	kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib \
	advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib \
	odbccp32.lib libc.lib

$(OUTDIR)\runemacs.obj:	$(XEMACS)/nt/runemacs.c
	$(CC) /nologo -ML -w -Od -Zi -c \
	-D_DEBUG -DWIN32 -D_WIN32 -DWIN32_LEAN_AND_MEAN \
	-D_NTSDK -D_M_IX86 -D_X86_ -Demacs -DHAVE_CONFIG_H -D_MSC_VER=999 \
	$** /Fo$@

SUPPORT_PROGS=$(MAKE_DOCFILE) $(RUNEMACS)

$(DOC): $(OUTDIR)/make-docfile.exe
	!$(MAKE_DOCFILE) -o $@ $(DOC_SRCS_1) 
	!$(MAKE_DOCFILE) -a $@ $(DOC_SRCS_2)
	!$(MAKE_DOCFILE) -a $@ $(DOC_SRCS_3)
	!$(MAKE_DOCFILE) -a $@ $(DOC_SRCS_4)

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
 /nodefaultlib /out:$@\
 /heap:0x00100000

TEMACS_CPP=c:/usr/local/bin/cpp.exe
TEMACS_CPP_FLAGS= $(INCLUDES) -D_DEBUG -DWIN32 -D_WIN32 -DWIN32_LEAN_AND_MEAN \
 -D_NTSDK -D_M_IX86 -D_X86_ -Demacs -DHAVE_CONFIG_H -D_MSC_VER=999 \
 -DEMACS_MAJOR_VERSION=$(EMACS_MAJOR_VERSION) \
 -DEMACS_MINOR_VERSION=$(EMACS_MINOR_VERSION) \
 -DXEMACS_CODENAME=$(XEMACS_CODENAME)
TEMACS_FLAGS=-nologo -ML -w -Od -Zi -c $(TEMACS_CPP_FLAGS)

TEMACS_OBJS= \
	$(OUTDIR)/abbrev.obj \
	$(OUTDIR)/alloc.obj \
	$(OUTDIR)/alloca.obj \
	$(OUTDIR)/balloon-x.obj \
	$(OUTDIR)/balloon_help.obj \
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
        $(OUTDIR)/input-method-xlib.obj \
        $(OUTDIR)/mule.obj \
        $(OUTDIR)/mule-charset.obj \
        $(OUTDIR)/mule-ccl.obj \
        $(OUTDIR)/mule-coding.obj \
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

$(TEMACS): $(TEMACS_SRC)/Emacs.ad.h $(TEMACS_SRC)/paths.h $(TEMACS_OBJS)
	link.exe @<<
  $(TEMACS_LFLAGS) $(TEMACS_OBJS) $(TEMACS_LIBS)
<<

$(OUTDIR)/abbrev.obj:	$(TEMACS_SRC)/abbrev.c
	$(CC) $(TEMACS_FLAGS) $** /Fo$@

$(OUTDIR)/alloc.obj:	$(TEMACS_SRC)/alloc.c
	$(CC) $(TEMACS_FLAGS) $** /Fo$@

$(OUTDIR)/alloca.obj:	$(TEMACS_SRC)/alloca.c
	$(CC) $(TEMACS_FLAGS) $** /Fo$@

$(OUTDIR)/balloon-x.obj:	$(TEMACS_SRC)/balloon-x.c
	$(CC) $(TEMACS_FLAGS) $** /Fo$@

$(OUTDIR)/balloon_help.obj:	$(TEMACS_SRC)/balloon_help.c
	$(CC) $(TEMACS_FLAGS) $** /Fo$@

$(OUTDIR)/blocktype.obj:	$(TEMACS_SRC)/blocktype.c
	$(CC) $(TEMACS_FLAGS) $** /Fo$@

$(OUTDIR)/buffer.obj:	$(TEMACS_SRC)/buffer.c
	$(CC) $(TEMACS_FLAGS) $** /Fo$@

$(OUTDIR)/bytecode.obj:	$(TEMACS_SRC)/bytecode.c
	$(CC) $(TEMACS_FLAGS) $** /Fo$@

$(OUTDIR)/callint.obj:	$(TEMACS_SRC)/callint.c
	$(CC) $(TEMACS_FLAGS) $** /Fo$@

$(OUTDIR)/callproc.obj:	$(TEMACS_SRC)/callproc.c
	$(CC) $(TEMACS_FLAGS) $** /Fo$@

$(OUTDIR)/casefiddle.obj:	$(TEMACS_SRC)/casefiddle.c
	$(CC) $(TEMACS_FLAGS) $** /Fo$@

$(OUTDIR)/casetab.obj:	$(TEMACS_SRC)/casetab.c
	$(CC) $(TEMACS_FLAGS) $** /Fo$@

$(OUTDIR)/chartab.obj:	$(TEMACS_SRC)/chartab.c
	$(CC) $(TEMACS_FLAGS) $** /Fo$@

$(OUTDIR)/cmdloop.obj:	$(TEMACS_SRC)/cmdloop.c
	$(CC) $(TEMACS_FLAGS) $** /Fo$@

$(OUTDIR)/cmds.obj:	$(TEMACS_SRC)/cmds.c
	$(CC) $(TEMACS_FLAGS) $** /Fo$@

$(OUTDIR)/console-stream.obj:	$(TEMACS_SRC)/console-stream.c
	$(CC) $(TEMACS_FLAGS) $** /Fo$@

$(OUTDIR)/console-x.obj:	$(TEMACS_SRC)/console-x.c
	$(CC) $(TEMACS_FLAGS) $** /Fo$@

$(OUTDIR)/console.obj:	$(TEMACS_SRC)/console.c
	$(CC) $(TEMACS_FLAGS) $** /Fo$@

$(OUTDIR)/data.obj:	$(TEMACS_SRC)/data.c
	$(CC) $(TEMACS_FLAGS) $** /Fo$@

$(OUTDIR)/debug.obj:	$(TEMACS_SRC)/debug.c
	$(CC) $(TEMACS_FLAGS) $** /Fo$@

$(OUTDIR)/device-x.obj:	$(TEMACS_SRC)/device-x.c
	$(CC) $(TEMACS_FLAGS) $** /Fo$@

$(OUTDIR)/device.obj:	$(TEMACS_SRC)/device.c
	$(CC) $(TEMACS_FLAGS) $** /Fo$@

$(OUTDIR)/dgif_lib.obj:	$(TEMACS_SRC)/dgif_lib.c
	$(CC) $(TEMACS_FLAGS) $** /Fo$@

$(OUTDIR)/dialog-x.obj:	$(TEMACS_SRC)/dialog-x.c
	$(CC) $(TEMACS_FLAGS) $** /Fo$@

$(OUTDIR)/dialog.obj:	$(TEMACS_SRC)/dialog.c
	$(CC) $(TEMACS_FLAGS) $** /Fo$@

$(OUTDIR)/dired.obj:	$(TEMACS_SRC)/dired.c
	$(CC) $(TEMACS_FLAGS) $** /Fo$@

$(OUTDIR)/doc.obj:	$(TEMACS_SRC)/doc.c
	$(CC) $(TEMACS_FLAGS) $** /Fo$@

$(OUTDIR)/doprnt.obj:	$(TEMACS_SRC)/doprnt.c
	$(CC) $(TEMACS_FLAGS) $** /Fo$@

$(OUTDIR)/dynarr.obj:	$(TEMACS_SRC)/dynarr.c
	$(CC) $(TEMACS_FLAGS) $** /Fo$@

$(OUTDIR)/editfns.obj:	$(TEMACS_SRC)/editfns.c
	$(CC) $(TEMACS_FLAGS) $** /Fo$@

$(OUTDIR)/elhash.obj:	$(TEMACS_SRC)/elhash.c
	$(CC) $(TEMACS_FLAGS) $** /Fo$@

$(OUTDIR)/emacs.obj:	$(TEMACS_SRC)/emacs.c
	$(CC) $(TEMACS_FLAGS) $** /Fo$@

$(OUTDIR)/EmacsFrame.obj:	$(TEMACS_SRC)/EmacsFrame.c
	$(CC) $(TEMACS_FLAGS) $** /Fo$@

$(OUTDIR)/EmacsManager.obj:	$(TEMACS_SRC)/EmacsManager.c
	$(CC) $(TEMACS_FLAGS) $** /Fo$@

$(OUTDIR)/TopLevelEmacsShell.obj:	$(TEMACS_SRC)/EmacsShell-sub.c
	$(CC) $(TEMACS_FLAGS) -DDEFINE_TOP_LEVEL_EMACS_SHELL $** /Fo$@

$(OUTDIR)/TransientEmacsShell.obj: $(TEMACS_SRC)/EmacsShell-sub.c
	$(CC) $(TEMACS_FLAGS) -DDEFINE_TRANSIENT_EMACS_SHELL $** /Fo$@

$(OUTDIR)/EmacsShell.obj:	$(TEMACS_SRC)/EmacsShell.c
	$(CC) $(TEMACS_FLAGS) $** /Fo$@

$(OUTDIR)/energize.obj:	$(TEMACS_SRC)/energize.c
	$(CC) $(TEMACS_FLAGS) $** /Fo$@

$(OUTDIR)/eval.obj:	$(TEMACS_SRC)/eval.c
	$(CC) $(TEMACS_FLAGS) $** /Fo$@

$(OUTDIR)/event-stream.obj:	$(TEMACS_SRC)/event-stream.c
	$(CC) $(TEMACS_FLAGS) $** /Fo$@

$(OUTDIR)/event-unixoid.obj:	$(TEMACS_SRC)/event-unixoid.c
	$(CC) $(TEMACS_FLAGS) $** /Fo$@

$(OUTDIR)/event-Xt.obj:	$(TEMACS_SRC)/event-Xt.c
	$(CC) $(TEMACS_FLAGS) $** /Fo$@

$(OUTDIR)/events.obj:	$(TEMACS_SRC)/events.c
	$(CC) $(TEMACS_FLAGS) $** /Fo$@

$(OUTDIR)/extents.obj:	$(TEMACS_SRC)/extents.c
	$(CC) $(TEMACS_FLAGS) $** /Fo$@

$(OUTDIR)/faces.obj:	$(TEMACS_SRC)/faces.c
	$(CC) $(TEMACS_FLAGS) $** /Fo$@

$(OUTDIR)/fileio.obj:	$(TEMACS_SRC)/fileio.c
	$(CC) $(TEMACS_FLAGS) $** /Fo$@

$(OUTDIR)/filelock.obj:	$(TEMACS_SRC)/filelock.c
	$(CC) $(TEMACS_FLAGS) $** /Fo$@

$(OUTDIR)/filemode.obj:	$(TEMACS_SRC)/filemode.c
	$(CC) $(TEMACS_FLAGS) $** /Fo$@

$(OUTDIR)/floatfns.obj:	$(TEMACS_SRC)/floatfns.c
	$(CC) $(TEMACS_FLAGS) $** /Fo$@

$(OUTDIR)/fns.obj:	$(TEMACS_SRC)/fns.c
	$(CC) $(TEMACS_FLAGS) $** /Fo$@

$(OUTDIR)/font-lock.obj:	$(TEMACS_SRC)/font-lock.c
	$(CC) $(TEMACS_FLAGS) $** /Fo$@

$(OUTDIR)/frame-x.obj:	$(TEMACS_SRC)/frame-x.c
	$(CC) $(TEMACS_FLAGS) $** /Fo$@

$(OUTDIR)/frame.obj:	$(TEMACS_SRC)/frame.c
	$(CC) $(TEMACS_FLAGS) $** /Fo$@

$(OUTDIR)/free-hook.obj:	$(TEMACS_SRC)/free-hook.c
	$(CC) $(TEMACS_FLAGS) $** /Fo$@

$(OUTDIR)/general.obj:	$(TEMACS_SRC)/general.c
	$(CC) $(TEMACS_FLAGS) $** /Fo$@

$(OUTDIR)/gif_err.obj:	$(TEMACS_SRC)/gif_err.c
	$(CC) $(TEMACS_FLAGS) $** /Fo$@

$(OUTDIR)/gifalloc.obj:	$(TEMACS_SRC)/gifalloc.c
	$(CC) $(TEMACS_FLAGS) $** /Fo$@

$(OUTDIR)/glyphs-x.obj:	$(TEMACS_SRC)/glyphs-x.c
	$(CC) $(TEMACS_FLAGS) $** /Fo$@

$(OUTDIR)/glyphs.obj:	$(TEMACS_SRC)/glyphs.c
	$(CC) $(TEMACS_FLAGS) $** /Fo$@

$(OUTDIR)/gmalloc.obj:	$(TEMACS_SRC)/gmalloc.c
	$(CC) $(TEMACS_FLAGS) $** /Fo$@

$(OUTDIR)/gui-x.obj:	$(TEMACS_SRC)/gui-x.c
	$(CC) $(TEMACS_FLAGS) $** /Fo$@

$(OUTDIR)/gui.obj:	$(TEMACS_SRC)/gui.c
	$(CC) $(TEMACS_FLAGS) $** /Fo$@

$(OUTDIR)/hash.obj:	$(TEMACS_SRC)/hash.c
	$(CC) $(TEMACS_FLAGS) $** /Fo$@

$(OUTDIR)/indent.obj:	$(TEMACS_SRC)/indent.c
	$(CC) $(TEMACS_FLAGS) $** /Fo$@

$(OUTDIR)/inline.obj:	$(TEMACS_SRC)/inline.c
	$(CC) $(TEMACS_FLAGS) $** /Fo$@

$(OUTDIR)/insdel.obj:	$(TEMACS_SRC)/insdel.c
	$(CC) $(TEMACS_FLAGS) $** /Fo$@

$(OUTDIR)/intl.obj:	$(TEMACS_SRC)/intl.c
	$(CC) $(TEMACS_FLAGS) $** /Fo$@

$(OUTDIR)/keymap.obj:	$(TEMACS_SRC)/keymap.c
	$(CC) $(TEMACS_FLAGS) $** /Fo$@

$(OUTDIR)/lread.obj:	$(TEMACS_SRC)/lread.c
	$(CC) $(TEMACS_FLAGS) $** /Fo$@

$(OUTDIR)/lstream.obj:	$(TEMACS_SRC)/lstream.c
	$(CC) $(TEMACS_FLAGS) $** /Fo$@

$(OUTDIR)/macros.obj:	$(TEMACS_SRC)/macros.c
	$(CC) $(TEMACS_FLAGS) $** /Fo$@

$(OUTDIR)/marker.obj:	$(TEMACS_SRC)/marker.c
	$(CC) $(TEMACS_FLAGS) $** /Fo$@

$(OUTDIR)/md5.obj:	$(TEMACS_SRC)/md5.c
	$(CC) $(TEMACS_FLAGS) $** /Fo$@

$(OUTDIR)/menubar-x.obj:	$(TEMACS_SRC)/menubar-x.c
	$(CC) $(TEMACS_FLAGS) $** /Fo$@

$(OUTDIR)/menubar.obj:	$(TEMACS_SRC)/menubar.c
	$(CC) $(TEMACS_FLAGS) $** /Fo$@

$(OUTDIR)/minibuf.obj:	$(TEMACS_SRC)/minibuf.c
	$(CC) $(TEMACS_FLAGS) $** /Fo$@

$(OUTDIR)/msdos.obj:	$(TEMACS_SRC)/msdos.c
	$(CC) $(TEMACS_FLAGS) $** /Fo$@

$(OUTDIR)/input-method-xlib.obj:        $(TEMACS_SRC)/input-method-xlib.c
        $(CC) $(TEMACS_FLAGS) $** /Fo$@

$(OUTDIR)/mule.obj:     $(TEMACS_SRC)/mule.c
        $(CC) $(TEMACS_FLAGS) $** /Fo$@

$(OUTDIR)/mule-charset.obj:     $(TEMACS_SRC)/mule-charset.c
        $(CC) $(TEMACS_FLAGS) $** /Fo$@

$(OUTDIR)/mule-ccl.obj: $(TEMACS_SRC)/mule-ccl.c
        $(CC) $(TEMACS_FLAGS) $** /Fo$@

$(OUTDIR)/mule-coding.obj:      $(TEMACS_SRC)/mule-coding.c
        $(CC) $(TEMACS_FLAGS) $** /Fo$@

$(OUTDIR)/nt.obj:	$(TEMACS_SRC)/nt.c
	$(CC) $(TEMACS_FLAGS) $** /Fo$@

$(OUTDIR)/ntheap.obj:	$(TEMACS_SRC)/ntheap.c
	$(CC) $(TEMACS_FLAGS) $** /Fo$@

$(OUTDIR)/ntproc.obj:	$(TEMACS_SRC)/ntproc.c
	$(CC) $(TEMACS_FLAGS) $** /Fo$@

$(OUTDIR)/objects-x.obj:	$(TEMACS_SRC)/objects-x.c
	$(CC) $(TEMACS_FLAGS) $** /Fo$@

$(OUTDIR)/objects.obj:	$(TEMACS_SRC)/objects.c
	$(CC) $(TEMACS_FLAGS) $** /Fo$@

$(OUTDIR)/opaque.obj:	$(TEMACS_SRC)/opaque.c
	$(CC) $(TEMACS_FLAGS) $** /Fo$@

$(OUTDIR)/print.obj:	$(TEMACS_SRC)/print.c
	$(CC) $(TEMACS_FLAGS) $** /Fo$@

$(OUTDIR)/process.obj:	$(TEMACS_SRC)/process.c
	$(CC) $(TEMACS_FLAGS) $** /Fo$@

$(OUTDIR)/pure.obj:	$(TEMACS_SRC)/pure.c
	$(CC) $(TEMACS_FLAGS) $** /Fo$@

$(OUTDIR)/rangetab.obj:	$(TEMACS_SRC)/rangetab.c
	$(CC) $(TEMACS_FLAGS) $** /Fo$@

$(OUTDIR)/realpath.obj:	$(TEMACS_SRC)/realpath.c
	$(CC) $(TEMACS_FLAGS) $** /Fo$@

$(OUTDIR)/redisplay-output.obj:	$(TEMACS_SRC)/redisplay-output.c
	$(CC) $(TEMACS_FLAGS) $** /Fo$@

$(OUTDIR)/redisplay-x.obj:	$(TEMACS_SRC)/redisplay-x.c
	$(CC) $(TEMACS_FLAGS) $** /Fo$@

$(OUTDIR)/redisplay.obj:	$(TEMACS_SRC)/redisplay.c
	$(CC) $(TEMACS_FLAGS) $** /Fo$@

$(OUTDIR)/regex.obj:	$(TEMACS_SRC)/regex.c
	$(CC) $(TEMACS_FLAGS) $** /Fo$@

$(OUTDIR)/scrollbar-x.obj:	$(TEMACS_SRC)/scrollbar-x.c
	$(CC) $(TEMACS_FLAGS) $** /Fo$@

$(OUTDIR)/scrollbar.obj:	$(TEMACS_SRC)/scrollbar.c
	$(CC) $(TEMACS_FLAGS) $** /Fo$@

$(OUTDIR)/search.obj:	$(TEMACS_SRC)/search.c
	$(CC) $(TEMACS_FLAGS) $** /Fo$@

$(OUTDIR)/signal.obj:	$(TEMACS_SRC)/signal.c
	$(CC) $(TEMACS_FLAGS) $** /Fo$@

$(OUTDIR)/sound.obj:	$(TEMACS_SRC)/sound.c
	$(CC) $(TEMACS_FLAGS) $** /Fo$@

$(OUTDIR)/specifier.obj:	$(TEMACS_SRC)/specifier.c
	$(CC) $(TEMACS_FLAGS) $** /Fo$@

$(OUTDIR)/strftime.obj:	$(TEMACS_SRC)/strftime.c
	$(CC) $(TEMACS_FLAGS) $** /Fo$@

$(OUTDIR)/symbols.obj:	$(TEMACS_SRC)/symbols.c
	$(CC) $(TEMACS_FLAGS) $** /Fo$@

$(OUTDIR)/syntax.obj:	$(TEMACS_SRC)/syntax.c
	$(CC) $(TEMACS_FLAGS) $** /Fo$@

$(OUTDIR)/sysdep.obj:	$(TEMACS_SRC)/sysdep.c
	$(CC) $(TEMACS_FLAGS) $** /Fo$@

$(OUTDIR)/termcap.obj:	$(TEMACS_SRC)/termcap.c
	$(CC) $(TEMACS_FLAGS) $** /Fo$@

$(OUTDIR)/tparam.obj:	$(TEMACS_SRC)/tparam.c
	$(CC) $(TEMACS_FLAGS) $** /Fo$@

$(OUTDIR)/undo.obj:	$(TEMACS_SRC)/undo.c
	$(CC) $(TEMACS_FLAGS) $** /Fo$@

$(OUTDIR)/unexnt.obj:	$(TEMACS_SRC)/unexnt.c
	$(CC) $(TEMACS_FLAGS) $** /Fo$@

$(OUTDIR)/vm-limit.obj:	$(TEMACS_SRC)/vm-limit.c
	$(CC) $(TEMACS_FLAGS) $** /Fo$@

$(OUTDIR)/window.obj:	$(TEMACS_SRC)/window.c
	$(CC) $(TEMACS_FLAGS)  $** /Fo$@

$(OUTDIR)/xgccache.obj:	$(TEMACS_SRC)/xgccache.c
	$(CC) $(TEMACS_FLAGS) $** /Fo$@

$(OUTDIR)/xmu.obj:	$(TEMACS_SRC)/xmu.c
	$(CC) $(TEMACS_FLAGS) $** /Fo$@

$(OUTDIR)/xselect.obj:	$(TEMACS_SRC)/xselect.c
	$(CC) $(TEMACS_FLAGS) $** /Fo$@

$(TEMACS_SRC)/Emacs.ad.h: $(XEMACS)/etc/Emacs.ad
	!"sed -f ad2c.sed < $(XEMACS)/etc/Emacs.ad > $(TEMACS_SRC)/Emacs.ad.h"

$(TEMACS_SRC)/paths.h: $(TEMACS_SRC)/paths.h.in
	!"cd $(TEMACS_SRC); cp paths.h.in paths.h"

#------------------------------------------------------------------------------

# LISP bits 'n bobs

dump-elcs:
	!"$(TEMACS) -batch -l update-elc.el $(LISP)"

dump-xemacs:
	!"$(TEMACS) -batch -l loadup.el dump"

#------------------------------------------------------------------------------

# use this rule to build the complete system
all: $(LASTFILE) $(LWLIB) $(TEMACS) $(SUPPORT_PROGS) $(DOC)

# use this rule to install the system
install:
