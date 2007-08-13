# -*- Mode: Makefile -*-

# Makefile --
# This file is part of ILISP.
# Version: 5.8
#
# Copyright (C) 1990, 1991, 1992, 1993 Chris McConnell
#               1993, 1994 Ivan Vasquez
#               1994, 1995, 1996 Marco Antoniotti and Rick Busdiecker
#               1996 Marco Antoniotti and Rick Campbell
#
# Send mail to 'ilisp-request@naggum.no' to be included in the
# ILISP mailing list.

# Note: this makefile assumes GNU make

# Updated for XEmacs autoloads builds, but unused at present since no
#  autoloads are generated, and this package hasn't been customized.

#==============================================================================
# Various Variables

Version = 5.8

# Use whichever you like most
#EMACS = xemacs
#EMACS = /usr/local/bin/emacs
EMACS = emacs

# The SHELL variable is used only for making the distribution.
#SHELL = /bin/csh
# Tsk, tsk, Though shalt not use csh in distributed scripts.
SHELL = /bin/sh

# These are used mostly for packaging the distribution
Ilisp_src_dir = $(shell pwd)
Ilisp_tar_dir = ilisp-$(Version)

OtherFiles = README         \
             HISTORY        \
             Makefile       \
             ilisp.emacs    \
             INSTALLATION   \
             COPYING        \
             GETTING-ILISP  \
             Welcome

DocFiles = ilisp.texi

LoadFiles = ilisp-def.elc ilisp-el.elc ilisp-sym.elc \
 ilisp-inp.elc ilisp-ind.elc ilisp-prc.elc ilisp-val.elc ilisp-out.elc \
 ilisp-mov.elc ilisp-key.elc ilisp-prn.elc ilisp-low.elc ilisp-doc.elc \
 ilisp-ext.elc ilisp-mod.elc ilisp-dia.elc ilisp-cmt.elc ilisp-rng.elc \
 ilisp-hnd.elc ilisp-utl.elc ilisp-cmp.elc ilisp-kil.elc ilisp-snd.elc \
 ilisp-xfr.elc ilisp-hi.elc ilisp-aut.elc ilisp-cl.elc ilisp-cmu.elc \
 ilisp-acl.elc ilisp-kcl.elc ilisp-luc.elc ilisp-sch.elc ilisp-hlw.elc \
 ilisp-xls.elc


#==============================================================================
# Rules

compile:
	$(EMACS) -batch -l ilisp-mak.el

elc: $(LoadFiles) $(XEmacsELC)

$(LoadFiles) :
	$(EMACS) -batch -l ilisp-mak.el

tags:
	etags *.el

clean: 
	$(RM) *.elc

loadfile:
	touch ilisp-all.elc
	cat $(LoadFiles) > ilisp-all.elc
	rm $(LoadFiles)
# Note that the redirection is done by a Bourne Shell.

compress:
	gzip *.el $(OtherFiles) $(DocFiles)

#==============================================================================
# The following targets are used only to create a distribution file.

dist: tarring dist_compressing

tarring:
	(cd $(Ilisp_src_dir)/..;                                        \
         if ( $(notdir $(Ilisp_src_dir)) != $(Ilisp_tar_dir) )          \
            ln -s $(notdir $(Ilisp_src_dir)) $(Ilisp_tar_dir) ;         \
         tar cvf $(Ilisp_tar_dir).tar                                   \
            $(patsubst %,$(Ilisp_tar_dir)/%,$(OtherFiles))              \
            $(Ilisp_tar_dir)/*.el                                       \
            $(Ilisp_tar_dir)/*.lisp                                     \
            $(patsubst %,$(Ilisp_tar_dir)/%,$(DocFiles))                \
            $(Ilisp_tar_dir)/*.mail)

dist_compressing:
	(cd $(Ilisp_src_dir)/.. ; gzip $(Ilisp_tar_dir).tar)

uuencoding: ../$(Ilisp_tar_dir).tar.gz
	(cd $(Ilisp_src_dir)/.. ;                                           \
         uuencode $(Ilisp_tar_dir).tar.gz $(Ilisp_tar_dir).tar.gz > il.uue)

#autoloads: auto-autoloads.el

#custom-load.elc: custom-load.el
#	$(EMACS) -batch -q -no-site-file -f batch-byte-compile $?

#auto-autoloads.el: $(LoadFiles:.elc=.el)
#	$(EMACS) -batch -q -no-site-file \
#		-eval '(setq autoload-target-directory "'`pwd`'/")' \
#		-l autoload \
#		-f batch-update-autoloads $?

# end of file -- Makefile --
