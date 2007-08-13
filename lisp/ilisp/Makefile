# -*- Mode: Makefile -*-

# Makefile --
# This file is part of ILISP.
# Version: 5.7
#
# Copyright (C) 1990, 1991, 1992, 1993 Chris McConnell
#               1993, 1994 Ivan Vasquez
#               1994, 1995 Marco Antoniotti and Rick Busdiecker
#
# Send mail to 'ilisp-request@lehman.com' to be included in the
# ILISP mailing list.

#------------------------------------------------------------------------------
# Various Variables

#EMACS=xemacs
EMACS=emacs

OtherFiles=README HISTORY Makefile ilisp.emacs

DocFiles=ilisp.texi ilisp.ps.gz ilisp.info ilisp.info-1 ilisp.info-2

Version=5.7

LoadFiles=ilisp-def.elc ilisp-el.elc ilisp-sym.elc \
 ilisp-inp.elc ilisp-ind.elc ilisp-prc.elc ilisp-val.elc ilisp-out.elc \
 ilisp-mov.elc ilisp-key.elc ilisp-prn.elc ilisp-low.elc ilisp-doc.elc \
 ilisp-ext.elc ilisp-mod.elc ilisp-dia.elc ilisp-cmt.elc ilisp-rng.elc \
 ilisp-hnd.elc ilisp-utl.elc ilisp-cmp.elc ilisp-kil.elc ilisp-snd.elc \
 ilisp-xfr.elc ilisp-hi.elc ilisp-aut.elc ilisp-cl.elc ilisp-cmu.elc \
 ilisp-acl.elc ilisp-kcl.elc ilisp-luc.elc ilisp-sch.elc ilisp-hlw.elc


#------------------------------------------------------------------------------
# Rules

compile:
	$(EMACS) -batch -l ilisp-mak.el

id:
	mkid -S.el=lisp *.el

tags:
	etags *.el

dist: tarring compressing

tarring:
	tar cvf ilisp-$(Version).tar $(OtherFiles) *.el *.lisp *.lcd $(DocFiles) *.mail

compressing:
	gzip ilisp-$(Version).tar

clean: 
	$(RM) *.elc

loadfile:
	touch ilisp-all.elc
	cat $(LoadFiles) > ilisp-all.elc
	rm $(LoadFiles)
# Note that the redirection is done by a Bourne Shell.

# end of file -- Makefile --
