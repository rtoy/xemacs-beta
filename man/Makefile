#  Makefile for man subdirectory in XEmacs
#   Copyright (C) 1995 Board of Trustees, University of Illinois
#   Copyright (C) 1994, 1995 Sun Microsystems.
#   Copyright (C) 2005 Ben Wing.

# This file is part of XEmacs.

# XEmacs is free software; you can redistribute it and/or modify it
# under the terms of the GNU General Public License as published by the
# Free Software Foundation; either version 2, or (at your option) any
# later version.

# XEmacs is distributed in the hope that it will be useful, but WITHOUT
# ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
# FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
# for more details.

# You should have received a copy of the GNU General Public License
# along with XEmacs; see the file COPYING.  If not, write to
# the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
# Boston, MA 02111-1307, USA.

SHELL    = /bin/sh
MAKEINFO = makeinfo
TEXI2DVI = texi2dvi
TEXI2HTML = texi2html
TEXI2HTML_NOSPLIT = $(TEXI2HTML) -verbose -subdir $(HTMLDIR)
TEXI2HTML_SPLIT = $(TEXI2HTML) -verbose -subdir $(HTMLDIR) -split chapter

RM = rm -f
CP = cp

.SUFFIXES:
.SUFFIXES: .html .info .texi .dvi .pdf

RECURSIVE_MAKE_ARGS = $(MFLAGS) MAKEINFO='$(MAKEINFO)' TEXI2DVI='$(TEXI2DVI)'

all : info

# Ughhh!  The things we do to have portable makefiles...

INFODIR = ../info
HTMLDIR = ../html
PHOTODIR = ../etc/photos

info_files = \
	$(INFODIR)/beta.info \
	$(INFODIR)/cl.info \
	$(INFODIR)/custom.info \
	$(INFODIR)/emodules.info \
	$(INFODIR)/external-widget.info \
	$(INFODIR)/info.info \
	$(INFODIR)/internals.info \
	$(INFODIR)/lispref.info \
	$(INFODIR)/new-users-guide.info \
	$(INFODIR)/standards.info \
	$(INFODIR)/term.info \
	$(INFODIR)/termcap.info \
	$(INFODIR)/texinfo.info \
	$(INFODIR)/widget.info \
	$(INFODIR)/xemacs-faq.info \
	$(INFODIR)/xemacs.info

html_files = \
	$(HTMLDIR)/beta.html \
	$(HTMLDIR)/cl.html \
	$(HTMLDIR)/custom.html \
	$(HTMLDIR)/emodules.html \
	$(HTMLDIR)/external-widget.html \
	$(HTMLDIR)/info.html \
	$(HTMLDIR)/lispref.html \
	$(HTMLDIR)/internals.html \
	$(HTMLDIR)/new-users-guide.html \
	$(HTMLDIR)/standards.html \
	$(HTMLDIR)/term.html \
	$(HTMLDIR)/termcap.html \
	$(HTMLDIR)/texinfo.html \
	$(HTMLDIR)/widget.html \
	$(HTMLDIR)/xemacs.html \
	$(HTMLDIR)/xemacs-faq.html

dvi_files = \
	beta.dvi \
	cl.dvi \
	custom.dvi \
	emodules.dvi \
	external-widget.dvi \
	info.dvi \
	lispref.dvi \
	internals.dvi \
	new-users-guide.dvi \
	standards.dvi \
	term.dvi \
	termcap.dvi \
	texinfo.dvi \
	widget.dvi \
	xemacs.dvi \
	xemacs-faq.dvi

pdf_files = \
       beta.pdf \
       cl.pdf \
       custom.pdf \
       emodules.pdf \
       external-widget.pdf \
       info.pdf \
       lispref.pdf \
       internals.pdf \
       new-users-guide.pdf \
       standards.pdf \
       term.pdf \
       termcap.pdf \
       texinfo.pdf \
       widget.pdf \
       xemacs.pdf \
       xemacs-faq.pdf

xemacs-srcs = \
	xemacs/abbrevs.texi \
	xemacs/basic.texi \
	xemacs/buffers.texi \
	xemacs/building.texi \
	xemacs/calendar.texi \
	xemacs/cmdargs.texi \
	xemacs/custom.texi \
	xemacs/display.texi \
	xemacs/entering.texi \
	xemacs/files.texi \
	xemacs/fixit.texi \
	xemacs/frame.texi \
	xemacs/glossary.texi \
	xemacs/gnu.texi \
	xemacs/help.texi \
	xemacs/indent.texi \
	xemacs/keystrokes.texi \
	xemacs/killing.texi \
	xemacs/m-x.texi \
	xemacs/major.texi \
	xemacs/mark.texi \
	xemacs/menus.texi \
	xemacs/mini.texi \
	xemacs/misc.texi \
	xemacs/mouse.texi \
	xemacs/mule.texi \
	xemacs/new.texi \
	xemacs/packages.texi \
	xemacs/picture.texi \
	xemacs/programs.texi \
	xemacs/reading.texi \
	xemacs/regs.texi \
	xemacs/search.texi \
	xemacs/sending.texi \
	xemacs/startup.texi \
	xemacs/text.texi \
	xemacs/trouble.texi \
	xemacs/undo.texi \
	xemacs/windows.texi \
	xemacs/xemacs.texi

lispref-srcs = \
	lispref/abbrevs.texi \
	lispref/annotations.texi \
	lispref/back.texi \
	lispref/backups.texi \
	lispref/buffers.texi \
	lispref/building.texi \
	lispref/commands.texi \
	lispref/compile.texi \
	lispref/consoles-devices.texi \
	lispref/control.texi \
	lispref/customize.texi \
	lispref/databases.texi \
	lispref/debugging.texi \
	lispref/dialog.texi \
	lispref/display.texi \
	lispref/dragndrop.texi \
	lispref/edebug-inc.texi \
	lispref/edebug.texi \
	lispref/errors.texi \
	lispref/eval.texi \
	lispref/extents.texi \
	lispref/faces.texi \
	lispref/files.texi \
	lispref/frames.texi \
	lispref/functions.texi \
	lispref/glyphs.texi \
	lispref/hash-tables.texi \
	lispref/help.texi \
	lispref/hooks.texi \
	lispref/index.texi \
	lispref/internationalization.texi \
	lispref/intro.texi \
	lispref/keymaps.texi \
	lispref/ldap.texi \
	lispref/lispref.texi \
	lispref/lists.texi \
	lispref/loading.texi \
	lispref/locals.texi \
	lispref/macros.texi \
	lispref/maps.texi \
	lispref/markers.texi \
	lispref/menus.texi \
	lispref/minibuf.texi \
	lispref/modes.texi \
	lispref/mouse.texi \
	lispref/mule.texi \
	lispref/numbers.texi \
	lispref/objects.texi \
	lispref/os.texi \
	lispref/packaging.texi \
	lispref/positions.texi \
	lispref/processes.texi \
	lispref/range-tables.texi \
	lispref/scrollbars.texi \
	lispref/searching.texi \
	lispref/sequences.texi \
	lispref/specifiers.texi \
	lispref/streams.texi \
	lispref/strings.texi \
	lispref/symbols.texi \
	lispref/syntax.texi \
	lispref/text.texi \
	lispref/tips.texi \
	lispref/toolbar.texi \
	lispref/tooltalk.texi \
	lispref/variables.texi \
	lispref/windows.texi \
	lispref/x-windows.texi

internals-srcs = \
	internals/internals.texi

new-users-guide-srcs = \
	new-users-guide/custom1.texi \
	new-users-guide/custom2.texi \
	new-users-guide/edit.texi \
	new-users-guide/enter.texi \
	new-users-guide/files.texi \
	new-users-guide/help.texi \
	new-users-guide/modes.texi \
	new-users-guide/new-users-guide.texi \
	new-users-guide/region.texi \
	new-users-guide/search.texi \
	new-users-guide/xmenu.texi

$(INFODIR)/beta.info : beta.texi
	$(MAKEINFO) -o $(INFODIR)/beta.info beta.texi

$(INFODIR)/cl.info : cl.texi
	$(MAKEINFO) -o $(INFODIR)/cl.info cl.texi

$(INFODIR)/custom.info : custom.texi
	$(MAKEINFO) -o $(INFODIR)/custom.info custom.texi

$(INFODIR)/emodules.info : emodules.texi
	$(MAKEINFO) -o $(INFODIR)/emodules.info emodules.texi

$(INFODIR)/external-widget.info : external-widget.texi
	$(MAKEINFO) -o $(INFODIR)/external-widget.info external-widget.texi

$(INFODIR)/info.info : info.texi
	$(MAKEINFO) -o $(INFODIR)/info.info info.texi

$(INFODIR)/standards.info : standards.texi
	$(MAKEINFO) -o $(INFODIR)/standards.info standards.texi

$(INFODIR)/term.info : term.texi
	$(MAKEINFO) -o $(INFODIR)/term.info term.texi

$(INFODIR)/termcap.info : termcap.texi
	$(MAKEINFO) -o $(INFODIR)/termcap.info termcap.texi

$(INFODIR)/texinfo.info : texinfo.texi
	$(MAKEINFO) -o $(INFODIR)/texinfo.info texinfo.texi

$(INFODIR)/widget.info : widget.texi
	$(MAKEINFO) -o $(INFODIR)/widget.info widget.texi

$(INFODIR)/xemacs-faq.info : xemacs-faq.texi
	$(MAKEINFO) -o $(INFODIR)/xemacs-faq.info xemacs-faq.texi

# Manuals with their own subdirectory
$(INFODIR)/xemacs.info : $(xemacs-srcs)
	$(MAKEINFO) -P xemacs -o $(INFODIR)/xemacs.info xemacs/xemacs.texi

$(INFODIR)/lispref.info : $(lispref-srcs)
	$(MAKEINFO) -P lispref -o $(INFODIR)/lispref.info lispref/lispref.texi

$(INFODIR)/internals.info : $(internals-srcs)
	$(MAKEINFO) -P internals -o $(INFODIR)/internals.info internals/internals.texi

$(INFODIR)/new-users-guide.info : $(new-users-guide-srcs)
	$(MAKEINFO) -P new-users-guide -o $(INFODIR)/new-users-guide.info new-users-guide/new-users-guide.texi

xemacs : $(INFODIR)/xemacs.info
lispref : $(INFODIR)/lispref.info
internals : $(INFODIR)/internals.info
new-users-guide.info : $(INFODIR)/new-users-guide.info

.PHONY : xemacs lispref internals new-users-guide info dvi pdf

info : $(info_files)
html : $(html_files)

.texi.dvi:
	-$(TEXI2DVI) $<

.texi.pdf:
	-$(TEXI2DVI) --pdf $<

xemacs.dvi : $(xemacs-srcs)
	$(TEXI2DVI) -I xemacs xemacs/xemacs.texi

lispref.dvi : $(lispref-srcs)
	$(TEXI2DVI) -I lispref lispref/lispref.texi

internals.dvi : $(internals-srcs)
	$(TEXI2DVI) -I internals internals/internals.texi

new-users-guide.dvi : $(new-users-guide-srcs)
	$(TEXI2DVI) -I new-users-guide new-users-guide/new-users-guide.texi

dvi : $(dvi_files)

xemacs.pdf: $(xemacs-srcs)
	$(TEXI2DVI) --pdf -I xemacs xemacs/xemacs.texi

lispref.pdf: $(lispref-srcs)
	$(TEXI2DVI) --pdf -I lispref lispref/lispref.texi

internals.pdf: $(internals-srcs)
	$(TEXI2DVI) --pdf -I internals internals/internals.texi

new-users-guide.pdf: $(new-users-guide-srcs)
	$(TEXI2DVI) --pdf -I new-users-guide new-users-guide/new-users-guide.texi

pdf: $(pdf_files)

.PHONY: mostlyclean clean distclean realclean extraclean
.PHONY: distclean-noconfig realclean-noconfig extraclean-noconfig
mostlyclean:
	$(RM) *.toc *.aux *.log *.op *.cp *.cps *.fn *.fns
	$(RM) *.ky *.kys *.pg *.pgs *.tp *.tps *.vr *.vrs
clean: mostlyclean
	$(RM) core *.dvi *.pdf
distclean-noconfig: clean
distclean: distclean-noconfig
realclean-noconfig: distclean-noconfig
realclean: distclean
extraclean-noconfig: realclean-noconfig
	$(RM) *~ \#* */*~ */\#*
extraclean: realclean
	$(RM) *~ \#* */*~ */\#*

############################################################################

$(HTMLDIR)/beta.html : beta.texi
	$(TEXI2HTML_NOSPLIT) beta.texi

$(HTMLDIR)/cl.html : cl.texi
	$(TEXI2HTML_SPLIT) cl.texi

$(HTMLDIR)/custom.html : custom.texi
	$(TEXI2HTML_SPLIT) custom.texi

$(HTMLDIR)/emodules.html : emodules.texi
	$(TEXI2HTML_SPLIT) emodules.texi

$(HTMLDIR)/external-widget.html : external-widget.texi
	$(TEXI2HTML_SPLIT) external-widget.texi

$(HTMLDIR)/info.html : info.texi
	$(TEXI2HTML_SPLIT) info.texi

$(HTMLDIR)/standards.html : standards.texi
	$(TEXI2HTML_SPLIT) standards.texi

$(HTMLDIR)/term.html : term.texi
	$(TEXI2HTML_SPLIT) term.texi

$(HTMLDIR)/termcap.html : termcap.texi
	$(TEXI2HTML_SPLIT) termcap.texi

$(HTMLDIR)/texinfo.html : texinfo.texi
	$(TEXI2HTML_SPLIT) texinfo.texi

$(HTMLDIR)/widget.html : widget.texi
	$(TEXI2HTML_SPLIT) widget.texi

$(HTMLDIR)/xemacs-faq.html : xemacs-faq.texi
	$(TEXI2HTML_NOSPLIT) xemacs-faq.texi
	$(TEXI2HTML_SPLIT) -top_file xemacs-faq_1.html xemacs-faq.texi
	$(CP) $(PHOTODIR)/ben.png $(HTMLDIR)
	$(CP) $(PHOTODIR)/cthomp.png $(HTMLDIR)
	$(CP) $(PHOTODIR)/hniksic.png $(HTMLDIR)
	$(CP) $(PHOTODIR)/jwz.png $(HTMLDIR)
	$(CP) $(PHOTODIR)/martin.png $(HTMLDIR)
	$(CP) $(PHOTODIR)/mly.png $(HTMLDIR)
	$(CP) $(PHOTODIR)/piper.png $(HTMLDIR)
	$(CP) $(PHOTODIR)/slb.png $(HTMLDIR)

# Manuals with their own subdirectory
$(HTMLDIR)/xemacs.html : $(xemacs-srcs)
	$(TEXI2HTML_SPLIT) xemacs/xemacs.texi

$(HTMLDIR)/lispref.html : $(lispref-srcs)
	$(TEXI2HTML_SPLIT) lispref/lispref.texi

$(HTMLDIR)/internals.html : $(internals-srcs)
	$(TEXI2HTML_SPLIT) internals/internals.texi

$(HTMLDIR)/new-users-guide.html : $(new-users-guide-srcs)
	$(TEXI2HTML_SPLIT) new-users-guide/new-users-guide.texi

xemacs : $(HTMLDIR)/xemacs.html
lispref : $(HTMLDIR)/lispref.html
internals : $(HTMLDIR)/internals.html
new-users-guide.html : $(HTMLDIR)/new-users-guide.html

html : $(html_files)
