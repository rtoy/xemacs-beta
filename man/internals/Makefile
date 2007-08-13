# Makefile for the XEmacs Internals Manual.
#
# based on the Lisp Reference Manual.

# Redefine `TEX' if `tex' does not invoke plain TeX. For example:
# TEX=platex

TEX=tex

# Where the TeX macros are kept:
texmacrodir = /usr/local/lib/tex/macros

# Where the Emacs hierarchy lives ($EMACS in the INSTALL document for Emacs.)
# For example: 
# emacslibdir = /usr/local/gnu/lib/emacs

# Directory where Emacs is installed, by default:
emacslibdir = /usr/local/xemacs

# Unless you have a nonstandard Emacs installation, these shouldn't have to 
# be changed.
prefix = /usr/local
infodir = ${prefix}/info

# The name of the manual:

VERSION=1.0
NAME=internals
manual = xemacs-internals-manual-19-$(VERSION)

# Uncomment this line for permuted index.
# permuted_index = 1 

# List of all the texinfo files in the manual:

srcs = internals.texi index.unperm index.perm

all: ../../info/$(NAME).info

$(NAME).dvi: $(srcs) index.texi ../texindex
	# Avoid losing old contents of aux file entirely.
	-mv $(NAME).aux $(NAME).oaux
	# First shot to define xrefs:
	$(TEX) $(NAME).texi
	if [ a${permuted_index} != a ]; \
	then \
	  ./permute-index; \
	  mv permuted.fns $(NAME).fns; \
	else \
	  ../texindex $(NAME).??; \
	fi
	$(TEX) $(NAME).texi

index.texi:
	if [ a${permuted_index} != a ]; \
	then \
	  ln -s index.perm index.texi; \
	else \
	  ln -s index.unperm index.texi; \
	fi

../../info/$(NAME).info: $(srcs) index.texi ../makeinfo
	../makeinfo $(NAME).texi

mostlyclean:
	rm -f *.toc *.aux *.log *.cp *.cps *.fn *.fns *.tp *.tps \
              *.vr *.vrs *.pg *.pgs *.ky *.kys

clean: mostlyclean
	rm -f *.dvi *.ps
	rm -f make.out core 
	rm -f index.texi

# not working.
#install: elisp elisp.dvi
#	mv elisp elisp-* $(infodir)
#	@echo also add the line for elisp to $(infodir)/dir.

# not working.
#installall: install
#	install -c texinfo.tex $(texmacrodir)

distclean: clean
realclean: distclean
extraclean: distclean
	-rm -f *~ \#*

../texindex:
	(cd ..; make texindex)

# not working.
#maintainer-clean: clean
#	rm -f elisp elisp-*

# not working.
# dist:
#	-mkdir temp
#	-mkdir temp/$(manual)
#	-ln README Makefile permute-index $(srcs) \
#   texinfo.tex getopt.c getopt1.c getopt.h \
#   elisp.dvi elisp.aux elisp.??s elisp elisp-[0-9] elisp-[0-9][0-9] temp/$(manual)
#	-rm -f temp/$(manual)/texindex.c temp/$(manual)/makeinfo.c
#	cp texindex.c makeinfo.c temp/$(manual)
#	(cd temp/$(manual); rm -f *~)
#	(cd temp; tar chf - $(manual)) | gzip > $(manual).tar.gz
#	-rm -rf temp

../makeinfo:
	(cd ..; make makeinfo)
