# Makefile for the XEmacs New Users Guide
#
# based on XEmacs Makefile

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

NAME=new-users-guide

# List of all the texinfo files in the manual:

srcs = new-users-guide.texi custom1.texi files.texi region.texi \
  custom2.texi help.texi search.texi edit.texi modes.texi \
  xmenu.texi enter.texi

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
