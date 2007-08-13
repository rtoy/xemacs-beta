# Makefile for the edebug manual, and distribution packaging.
#
# March 1994

# The version of edebug.
VERSION = 3.4

# Redefine `TEX' if `tex' does not invoke plain TeX. For example:
# TEX=platex
TEX=tex

# Redefine `DVIPS' if it is not `dvips'.  The command line may need
# to be changed anyway.
DVIPS=dvips

FILES = README Makefile edebug.el cust-print.el edebug-history \
	eval-region.el \
	edebug-test.el \
	cl-specs.el cl-read.el edebug-cl-read.el \
	edebug.tex edebug.texi

# I include the cl package for now.
CLFILES = cl.el cl-extra.el cl-macs.el cl-seq.el cl-compat.el cl.texinfo

SUBDIR = edebug-${VERSION}

all:	edebug.dvi

# First shot to define xrefs and produce permuted index.
edebug.cp:	edebug.tex edebug.texi
	$(TEX) edebug.tex

edebug.cps:	edebug.cp
	./permute-index edebug cp
	mv permuted.cps edebug.cps

# Produce the final dvi.
edebug.dvi:	edebug.cps
	$(TEX) edebug.tex  # This modifies edebug.cp again.
	touch edebug.cps   # This one is OK.

# Produce a postscript file
edebug.ps:	edebug.dvi
	$(DVIPS) edebug.dvi -o edebug.ps

# Produce edebug.info
edebug.info:	edebug.tex edebug.texi
	makeinfo edebug.tex

dist edebug.tar.Z:	${FILES}
	rm -rf edebug.tar edebug.tar.Z ${SUBDIR}
	mkdir ${SUBDIR}
	mv ${FILES} ${SUBDIR}
	tar chf edebug.tar ${SUBDIR}
	mv ${SUBDIR}/* .
	compress edebug.tar
	rm -r ${SUBDIR}

cl-dist:	${CLFILES}
	rm -rf cl.tar cl.tar.Z
	tar cf cl.tar ${CLFILES}
	compress cl.tar

unpack:
	uncompress edebug.tar.Z
	tar xf edebug.tar

mostlyclean clean:
	rm -f edebug.dvi edebug.log edebug.toc
	rm -f edebug.cp edebug.fn edebug.ky edebug.pg edebug.tp edebug.vr
	rm -rf edebug.tar edebug.tar.Z ${SUBDIR}

distclean realclean: clean
	rm -f edebug.??s edebug.aux # tex files
	rm -f *.elc
