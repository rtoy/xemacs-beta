# Makefile for the XEmacs Reference Manual.
#
# based on FSF Makefile, 11 August 1990
# Modified by Ben Wing, February 1994

# Redefine `TEX' if `tex' does not invoke plain TeX. For example:
# TEX=platex

TEX=tex

# Where the TeX macros are kept:
texmacrodir = /usr/local/lib/tex/macros

# Where the Emacs hierarchy lives ($EMACS in the INSTALL document for Emacs.)
# For example: 
# emacslibdir = /usr/local/gnu/lib/emacs

# Directory where Emacs is installed, by default:
emacslibdir = /usr/local/emacs

# Unless you have a nonstandard Emacs installation, these shouldn't have to 
# be changed.
prefix = /usr/local
infodir = ${prefix}/info

# The name of the manual:

NAME=xemacs

# List of all the texinfo files in the manual:

srcs = xemacs.texi abbrevs.texi basic.texi buffers.texi building.texi \
  calendar.texi cmdargs.texi custom.texi display.texi entering.texi \
  files.texi fixit.texi glossary.texi gnu.texi help.texi indent.texi \
  keystrokes.texi killing.texi xemacs.texi m-x.texi major.texi mark.texi \
  menus.texi mini.texi misc.texi mouse.texi new.texi picture.texi \
  programs.texi reading.texi regs.texi frame.texi search.texi sending.texi \
  text.texi trouble.texi undo.texi windows.texi

all: ../../info/$(NAME).info

$(NAME).dvi: $(srcs) ../texindex
	# Avoid losing old contents of aux file entirely.
	-mv $(NAME).aux $(NAME).oaux
	# First shot to define xrefs:
	$(TEX) $(NAME).texi
	../texindex $(NAME).??
	$(TEX) $(NAME).texi

../../info/$(NAME).info: $(srcs) ../makeinfo
	../makeinfo $(NAME).texi

mostlyclean:
	rm -f *.toc *.aux *.oaux *.log *.cp *.cps *.fn *.fns *.tp *.tps \
              *.vr *.vrs *.pg *.pgs *.ky *.kys

clean: mostlyclean
	rm -f *.dvi *.ps
	rm -f make.out core

distclean: clean
realclean: distclean
extraclean: distclean
	-rm -f *~ \#*

../texindex:
	(cd ..; make texindex)

../makeinfo:
	(cd ..; make makeinfo)
