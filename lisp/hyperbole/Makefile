#
# SUMMARY:      Build Hyperbole directories and distributions.
#
# AUTHOR:       Bob Weiner
#
# ORIG-DATE:    15-Jun-94 at 03:42:38
# LAST-MOD:     21-Feb-97 at 18:04:02 by Bob Weiner
#
# This file is part of Hyperbole.
# Available for use and distribution under the same terms as GNU Emacs.
#
# Copyright (C) 1994-1995, Free Software Foundation, Inc.
# Developed with support from Motorola Inc.
#
# DESCRIPTION:  
#
# USAGE:        Emacs V19, configure Hyperbole for use in current directory:
#                  make
#               Rebuild out of date Emacs V19 Lisp .elc files:
#                  make
#               If you really want to rebuild all .elc files under Emacs 19:
#                  make all-elc
#               Emacs V18 and Epoch, rebuild Hyperbole in current directory:
#                  make all-elc-v18
#               Build out of date Emacs V18 Lisp .elc files:
#                  make elc-v18
#
# DESCRIP-END.

##########################################################################
#                         CONFIGURABLE SECTION                           #
##########################################################################

# Executables for various versions of Emacs.  Ensure that the name following
# the equals sign is correct for the version of emacs to which you set the
# EMACS variable.
EMACS19 = emacs19
EMACS18 = emacs
EPOCH = epoch
INFODOCK = infodock.bin
XEMACS = xemacs

# Emacs version used to byte-compile .el files into .elc's.
EMACS = $(XEMACS)

# Where to install the Hyperbole mouse key help file.
datadir = ../../etc
# Where to install the Info version of the Hyperbole manual.
infodir = ../../info
# Where to find the .texi source for the Hyperbole manual.
mandir = ../../man
# Where to install the Texinfo version of the Hyperbole manual.
texidir = ../../man

# Site-specific Emacs Lisp libraries to load before byte-compiling any files
# from this package.  Typically the only reason to set this is to get Emacs
# to include the directory of this package into its load-path variable, which
# determines where it will find Lisp library files to load.
#
# InfoDock and XEmacs 19.12 or higher include this package and automatically
# add its directory to load-path.  Under Emacs 19, if you add this directory
# to load-path in your site-lisp/site-start.el file, then you need not change
# this setting since site-start is automatically loaded whenever Emacs starts
# up.  If, however, you set load-path in your personal ~/.emacs file, you
# must add that to this setting.
#
# You must include the .el or .elc file suffix on each library name and each
# must be preceded by the `-l ' command-line flag.  If the directory in which
# the library is stored will not be in your Emacs load-path when Emacs
# attempts to load the library, you must include the full pathname to the
# library.  Here is an example setting.
#
# SITE_PRELOADS = -l ~/.emacs -l set-load-path.el
#
SITE_PRELOADS =

# Temp file to use to build .elc files.
ELISP_TO_COMPILE = /tmp/elc-${USER}

# Shell used to process this Makefile.  Bourne shell syntax is required.
SHELL = /bin/sh

# UNIX commands you may want to change for your particular system.
CP = \cp -p
DVIPS = \dvips
ETAGS = etags
INSTALL = install -c
MAKE = make
MAKEINFO = \makeinfo --fill-column 74 
MV = \mv -f
RM = \rm -f
TAR = tar

# Directory in which to create new distributions of Hyperbole.
DIST_DIR = /tmp

##########################################################################
#                     NO CHANGES REQUIRED BELOW HERE.                    #
##########################################################################

HYPB_VERSION = 4.021

# Libraries that must be pre-loaded before trying to byte-compile anything.
PRELOADS = $(SITE_PRELOADS) -l ./hversion.el -l ./hyperbole.el -l ./hsite.el

# Compile in batch mode. Under Emacs 19 and XEmacs, load
# site-lisp/site-start.el, which may set load-path.
BATCHFLAGS = -batch

# Directories other than the current directory in which to find files.
# This doesn't seem to work in all versions of make, so we also add kotl/
# explicitly to those files which need it.
VPATH = kotl

EL_SRC = hsite.el hui-em19-b.el hui-ep-but.el hui-epV4-b.el hui-xe-but.el

EL_COMPILE = hact.el hactypes.el hargs.el hbdata.el hbmap.el hbut.el \
	     hgnus.el hhist.el hib-doc-id.el hib-kbd.el hibtypes.el \
	     hinit.el hlvar.el hmail.el hmh.el hmoccur.el hmous-info.el \
	     hmouse-drv.el hmouse-key.el hmouse-mod.el hmouse-reg.el \
	     hmouse-sh.el hmouse-tag.el hpath.el hrmail.el hsmail.el \
	     hsys-hbase.el hsys-w3.el hsys-wais.el \
	     htz.el hui-menu.el hui-mini.el hui-mouse.el hui-window.el \
	     hui.el hvar.el hversion.el hvm.el hypb.el hyperbole.el \
	     set.el wconfig.el wrolo-logic.el wrolo-menu.el wrolo.el

EL_KOTL = kotl/kfile.el kotl/kfill.el kotl/kimport.el kotl/klabel.el \
	  kotl/klink.el kotl/kmenu.el kotl/knode.el kotl/kotl-mode.el \
          kotl/kotl.el kotl/kproperty.el kotl/kprop-em.el \
	  kotl/kprop-xe.el kotl/kview.el kotl/kvspec.el

ELC_COMPILE = hactypes.elc hibtypes.elc hib-kbd.elc hib-doc-id.elc hact.elc \
	     hargs.elc hbdata.elc hbmap.elc hbut.elc hgnus.elc hhist.elc \
	     hinit.elc hlvar.elc hmail.elc hmh.elc hmoccur.elc hmous-info.elc \
	     hmouse-drv.elc hmouse-key.elc hmouse-mod.elc hmouse-reg.elc \
	     hmouse-sh.elc hmouse-tag.elc hpath.elc hrmail.elc hsmail.elc \
	     hsys-hbase.elc hsys-w3.elc hsys-wais.elc \
	     htz.elc hui-menu.elc hui-mini.elc hui-mouse.elc hui-window.elc \
	     hui.elc hvar.elc hversion.elc hvm.elc hypb.elc hyperbole.elc \
	     set.elc wconfig.elc wrolo-logic.elc wrolo-menu.elc wrolo.elc

ELC_KOTL = kotl/kfile.elc kotl/kfill.elc kotl/kimport.elc kotl/klabel.elc \
	   kotl/klink.elc kotl/kmenu.elc kotl/knode.elc kotl/kotl-mode.elc \
           kotl/kotl.elc kotl/kproperty.elc \
	   kotl/kprop-xe.elc kotl/kview.el kotl/kvspec.elc

EL_TAGS = $(EL_SRC) $(EL_COMPILE) $(EL_KOTL)

.SUFFIXES:            # Delete the default suffixes
.SUFFIXES: .el .elc   # Define the list of file suffixes to match to rules

# Install Hyperbole for use in current directory under Emacs 19.
all: elc

doc: info ps

install: elc doc
	cd $(mandir); $(INSTALL) hypb-mouse.txt $(datadir); \
	  $(INSTALL) hyperbole.info* $(infodir); \
	  $(INSTALL) hyperbole.texi $(texidir)

install-v18:
	$(MAKE) EMACS="$(EMACS18)" VPATH="" EL_KOTL="" ELC_KOTL="" install

# Record any .el files that need to be compiled.
.el.elc:
	@ echo $< >> $(ELISP_TO_COMPILE)

# Compile all recorded .el files.
elc: elc-init hsite.el $(ELC_KOTL) $(ELC_COMPILE)
	@- \test ! -f $(ELISP_TO_COMPILE) \
            || (echo "These files will be compiled: " \
                 && echo "`cat $(ELISP_TO_COMPILE)`" \
                 && $(EMACS) $(BATCHFLAGS) $(PRELOADS) \
                       -f batch-byte-compile `cat $(ELISP_TO_COMPILE)`)
	@ $(RM) $(ELISP_TO_COMPILE)

elc-init:
	@ $(RM) $(ELISP_TO_COMPILE)

elc-v18:
	$(MAKE) EMACS="$(EMACS18)" VPATH="" EL_KOTL="" ELC_KOTL="" elc

all-v19: all-elc install

all-v18: all-elc-v18 install-v18

# Remove and then rebuild all byte-compiled .elc files for V19 Emacs, even
# those .elc files which do not yet exist.
all-elc: hsite.el
	$(RM) *.elc kotl/*.elc
	$(EMACS) $(BATCHFLAGS) $(PRELOADS) -f batch-byte-compile $(EL_KOTL) $(EL_COMPILE)

# Remove and then rebuild all byte-compiled .elc files for V18 Emacs.
# The kotl/ files are not used under V18 so they are not rebuilt.
all-elc-v18:
	$(MAKE) EMACS="$(EMACS18)" VPATH="" EL_KOTL="" ELC_KOTL="" all-elc

# This creates the site-specific hsite.el file from hsite-ex.el if necessary.
hsite.el: hsite-ex.el
	\test -f hsite.el || $(CP) hsite-ex.el hsite.el

dvi: $(mandir)/hyperbole.dvi

info: $(infodir)/hyperbole.info

ps: $(mandir)/hyperbole.ps

$(mandir)/hyperbole.dvi: info
	cd $(mandir); tex hyperbole.texi; \
	   texindex hyperbole.??; tex hyperbole.texi

$(infodir)/hyperbole.info: $(mandir)/hyperbole.texi
	cd $(mandir); $(MAKEINFO) hyperbole.texi

$(mandir)/hyperbole.ps: $(mandir)/hyperbole.texi
	make $(mandir)/hyperbole.dvi
	-$(RM) $(mandir)/hyperbole.ps
	cd $(mandir); $(DVIPS) -o hyperbole.ps hyperbole.dvi

tags: $(EL_TAGS)
	$(ETAGS) $(EL_TAGS) h-skip-bytec.lsp

clean:  
	-$(RM) core .place* .*~ *~ *\# *.orig *.rej .nfs*
	-cd kotl; $(RM) core .*~ *~ *\# *.orig *.rej .place* .nfs*
	-cd $(mandir); $(RM) core .*~ *~ *\# *.orig *.rej .place* .nfs*

version:
	fgrep $(HYPB_VERSION) Makefile README hversion.el \
	  $(mandir)/hyperbole.texi; echo ""

# Don't use this target.
bw-install:
	cd /home/infodock/lisp/; \
	tar zxvf /tmp/hyperbole-$(HYPB_VERSION).tar.gz; \
	cd hyperbole/; \
	mv man/hyperbole.info* ../../info/; mv man/hyperbole.texi ../../man/; \
	mv man/hypb-mouse.txt ../../etc/; rm -rf man

dist: version elc info
	echo "Ensure version # in README, hversion.el, Makefile and man/hyperbole.texi is up to date."
	cd ..; $(TAR) clf $(DIST_DIR)/h.tar hyperbole; \
	  $(CP) ../etc/ida-logo.xpm $(DIST_DIR); \
	  $(RM) -r $(DIST_DIR)/hman; mkdir $(DIST_DIR)/hman; \
	  $(CP) ../man/hyperbole.texi ../info/hyperbole.info* \
             ../etc/hypb-mouse.txt $(DIST_DIR)/hman
	$(RM) -r $(DIST_DIR)/hyperbole
	cd $(DIST_DIR); $(TAR) xf h.tar; \
	  $(MV) hman hyperbole/man; $(MV) ida-logo.xpm hyperbole
	cd $(DIST_DIR)/hyperbole; make distclean
# This ver setup won't work under SunOS for some reason.
#	ver="`head -3 hyperbole/hversion.el | tail -1 | sed -e 's/.*|//'`"
	cd $(DIST_DIR); $(RM) h.tar ver; \
	  $(TAR) --gzip -clf hyperbole-$(HYPB_VERSION).tar.gz hyperbole

# Don't run this target manually.  Use 'make dist' instead.
distclean: clean
	-$(RM) TAGS hsite.el* kotl/TO-DO* $(mandir)/hyperbole.dvi \
          $(mandir)/hyperbole.??
