###############################################################################
#
# File:         Makefile
# Release:      $efs release: 1.15 $
# Version:      $Revision: 1.4 $
# RCS:
# Description:  Makefile for byte-compiling efs and dired.
# Author:       Andy Norman, HPLabs, Bristol, UK.
# Created:      Sat Jan 30 00:18:56 1993
# Language:     Text (make script)
#
###############################################################################

## Installation Instructions
############################
# 1. Edit the configuration variables below.
#    EMACS should be the name of the emacs program on your system.
#    VERSION should be the emacs version. This must be one of:
#      18     for all versions of Emacs 18.
#      19     for all versions of the original GNU Emacs from FSF between
#             19.1 and 19.22, inclusive.
#      19.23  for version 19.23 and later of the original GNU Emacs from FSF
#      l19.11 for XEmacs 19.11 trhu 19.14
#      x19.15 for XEmacs 19.15 and later
#    LISPDIR should be the directory in which you want the .elc
#      files installed.
#    BDIR should be the directory containing the .elc files for the
#      byte-compiler.  Although efs byte-compiles and works with the
#      Emacs V18 byte-compiler, it is strongly recommended to use
#      Jamie Zawinski's V19 byte-compiler. This byte-compiler is
#      standard with Lucid Emacs, XEmacs, and GNU Emacs V19, so in this
#      case you can set BDIR to nothing.
#    VMDIR should be set to the directory containing the .elc files for
#      VM. If you aren't using VM, then set this to nothing.
#
# 2. To byte-compile the entire package, except for VM support (efs-vm.el),
#    run make VERSION, where VERSION is the emacs version that you are
#    compiling for.  It must be one of:
#    18     for Emacs 18
#    19     for the original GNU Emacs from FSF, versions 19.1 through
#             19.22, inclusive
#    19.23  for the original GNU Emacs from FSF, version 19.23 and later.
#    l19.11 for Lucid XEmacs 19.11 thru 19.14
#    x19.15 for XEmacs 19.15 and later
#
#    If you have set the VERSION variable correctly, then typing just    
#    make will suffice.
#
# 3. To byte-compile everything, including VM support, run make all.
#
# 4. To byte-compile all the efs files, except for VM support,
#    run make efs.
#
# 5. To byte-compile only the core efs files run make core.
#
# 6. To byte compile an efs-XXX.el file, run make XXX.
#    This means that VM support can be compiled by running make vm.
#
# 7. To byte compile only dired, run make dired.
#
# 8. To byte-compile only efs-auto.el, for autoloading efs, run make auto.
#

## Edit these variables according to your configuration.

# Name of Emacs program
EMACS=xemacs
# Emacs version. This must be set to one of 18, 19, 19.23,
# l19.11, x19.15
VERSION=x19.15
# Current working directory
CWD=`pwd`
# Directory in which to install the lisp files
LISPDIR=
# Directory containing byte-compiler.  This is used by fixup.el
BDIR=
# Directory containing VM's .elc files.
VMDIR=
# Bourne shell executable, please.
SHELL=/bin/sh

###### It should not be necessary to edit anything below this line. ######

COREOBJS = efs-defun.elc efs-ovwrt.elc efs-fnh.elc efs-cu.elc efs-netrc.elc \
           efs.elc efs-dired.elc efs-report.elc \
           efs-cp-p.elc
CORESRC = efs-defun.el efs-ovwrt.el efs-fnh.el efs-cu.el efs-netrc.el \
          efs.el efs-dired.el efs-report.el \
          efs-cp-p.el \
	  efs-dump.el
DOBJS = default-dir.elc dired.elc dired-mob.elc dired-oas.elc \
        dired-rgxp.elc dired-shell.elc dired-vir.elc dired-xy.elc \
        dired-grep.elc dired-uu.elc \
        dired-cmpr.elc dired-diff.elc dired-help.elc dired-sex.elc
DSRC = default-dir.el dired.el dired-mob.el dired-oas.el \
       dired-rgxp.el dired-shell.el dired-vir.el dired-xy.el \
       dired-grep.el dired-uu.el \
       dired-cmpr.el dired-diff.el dired-help.el dired-sex.el
EFSOBJS = $(COREOBJS) efs-auto.elc \
          efs-cms.elc efs-cms-knet.elc efs-dos-distinct.elc efs-nos-ve.elc \
          efs-gwp.elc efs-kerberos.elc efs-hell.elc efs-ka9q.elc \
          efs-mpe.elc efs-mts.elc efs-mvs.elc efs-netware.elc \
          efs-pc.elc efs-ti-explorer.elc efs-ti-twenex.elc \
          efs-tops-20.elc efs-dl.elc efs-guardian.elc efs-coke.elc \
          efs-vms.elc efs-vos.elc efs-plan9.elc efs-ms-unix.elc
EFSSRC = $(CORESRC) efs-auto.el \
          efs-cms.el efs-cms-knet.el efs-dos-distinct.el efs-nos-ve.el \
          efs-gwp.el efs-kerberos.el efs-hell.el efs-ka9q.el \
          efs-mpe.el efs-mts.el efs-mvs.el efs-netware.el \
          efs-pc.el efs-ti-explorer.el efs-ti-twenex.el \
          efs-tops-20.el efs-dl.el efs-guardian.el efs-coke.el \
          efs-vms.el efs-vos.el efs-plan9.el efs-ms-unix.el
VMOBJS = efs-vm.elc
VMSRC = efs-vm.el
DUMPSRC = efs-dump.el
GEOBJS = dired-fsf.elc dired-mule.elc efs-dired-mule.elc \
	 passwd.elc diff.elc auto-save.elc
GESRC = dired-fsf.el dired-mule.el efs-dired-mule.el \
	passwd.el diff.el auto-save.el
XEOBJS = dired-faces.elc dired-xemacs.elc
XESRC = dired-faces.el dired-xemacs.el
OBJS = $(DOBJS) $(EFSOBJS) $(VMOBJS) $(GEOBJS) $(XEOBJS) \
       efs-18.elc efs-19.elc efs-19.23.elc \
       efs-l19.11.elc efs-x19.15.elc \
       emacs-19.elc fn-handler.elc \
       reporter.elc

# fixup.el is never byte-compiled.  It would do no harm, but be a waste
# of time.

## Specify new rules.

.SUFFIXES: .elc .el .texi .info

.el.elc:
	BDIR=$(BDIR) CWD=$(CWD) VMDIR=$(VMDIR) \
  $(EMACS) -batch -l $(CWD)/fixup -f batch-byte-compile $(CWD)/$<

.texi.info:
	$(EMACS) -batch -f batch-texinfo-format $(CWD)/$<

## targets

# What lazy fingers buys you
default: $(VERSION) dired

# .elc files depend on .el source
# Do this in this brain-dead way because different makes do pattern
# rules differently. grumble grumble...
#
# dired
dired.elc: dired.el
dired-mob.elc: dired-mob.el
dired-oas.elc: dired-oas.el
dired-rgxp.elc: dired-rgxp.el
dired-shell.elc: dired-shell.el
dired-vir.elc: dired-vir.el
dired-xy.elc: dired-xy.el
dired-grep.elc: dired-grep.el
dired-uu.elc: dired-uu.el
dired-fsf.elc: dired-fsf.el
dired-cmpr.elc: dired-cmpr.el
dired-help.elc: dired-help.el
dired-diff.elc: dired-diff.el
dired-sex.elc: dired-sex.el
dired-mule.elc: dired-mule.el
dired-xemacs.elc: dired-xemacs.el
dired-faces.elc: dired-faces.el
default-dir.elc: default-dir.el
diff.elc: diff.el
# efs core files
efs.elc: efs.el
efs-defun.elc: efs-defun.el
efs-cp-p.elc: efs-cp-p.el
efs-cu.elc: efs-cu.el
efs-netrc.elc: efs-netrc.el
efs-auto.elc: efs-auto.el
efs-dired.elc: efs-dired.el
efs-dired-mule.elc: efs-dired-mule.el
efs-report.elc: efs-report.el
efs-ovwrt.elc: efs-ovwrt.el
efs-fnh.elc: efs-fnh.el
# efs multi-OS and FTP server support
efs-cms.elc: efs-cms.el
efs-cms-knet.elc: efs-cms-knet.el
efs-coke.elc: efs-coke.el
efs-dos-distinct.elc: efs-dos-distinct.el
efs-nos-ve.elc: efs-nos-ve.el
efs-gwp.elc:  efs-gwp.el
efs-hell.elc: efs-hell.el
efs-ka9q.elc: efs-ka9q.el
efs-kerberos.elc: efs-kerberos.el
efs-mpe.elc: efs-mpe.el
efs-mts.elc: efs-mts.el
efs-mvs.elc: efs-mvs.el
efs-netware.elc: efs-netware.el
efs-pc.elc: efs-pc.el
efs-ti-explorer.elc: efs-ti-explorer.el
efs-ti-twenex.elc: efs-ti-twenex.el
efs-tops-20.elc: efs-tops-20.el
efs-dl.elc: efs-dl.el
efs-vms.elc: efs-vms.el
efs-vos.elc: efs-vos.el
efs-guardian.elc: efs-guardian.el
efs-plan9.elc: efs-plan9.el
efs-ms-unix.elc: efs-ms-unix.el
# efs support for different Emacs versions
efs-18.elc: efs-18.el
efs-19.elc: efs-19.el
efs-19.23.elc: efs-19.23.el
efs-l19.11.elc: efs-l19.11.el
efs-x19.15.elc: efs-x19.15.el
# efs vm support
efs-vm.elc: efs-vm.el
# backward compatibility files
fn-handler.elc: fn-handler.el
emacs-19.elc: emacs-19.el
# auto-save package
auto-save.elc: auto-save.el

# Core targets
core: $(COREOBJS)

# Extra perks
auto: core efs-auto.elc
cms: core efs-cms.elc
cms-knet: core efs-cms-knet.elc
dos-distinct: core efs-dos-distinct.elc
nos-ve: core efs-nos-ve.elc
gwp: core efs-gwp.elc
hell: core efs-hell.elc
ka9q: core efs-ka9q.elc
kerberos: core efs-kerberos.elc
mpe: core efs-mpe.elc
mts: core efs-mts.elc
mvs: core efs-mvs.elc
netware: core efs-netware.elc
pc: core efs-pc.elc
ti-explorer: core efs-ti-explorer.elc
ti-twenex: core efs-ti-twenex.elc
tops-20: core efs-tops-20.elc
dl: core efs-dl.elc
vms: core efs-vms.elc
vos: core efs-vos.elc
guardian: core efs-guardian.elc
plan9: core efs-plan9.elc
coke: core efs-coke.elc
vm: core $(VMOBJS)

# The grand tour
efs: $(EFSOBJS)
dired: $(DOBJS)
all: $(OBJS)
info: efs.info

# Making for a specific emacs version
18: emacs-19.elc fn-handler.elc efs dired efs-18.elc dired-mule.elc \
    efs-dired-mule.elc reporter.elc passwd.elc diff.elc auto-save.elc
19: fn-handler.elc efs dired efs-19.elc $(GEOBJS)
19.23: efs dired efs-19.23.elc $(GEOBJS)
l19.11: efs dired efs-l19.11.elc $(XEOBJS)
x19.15: efs dired efs-x19.15.elc $(XEOBJS)

# Installation
install:
	@echo "Installing in $(LISPDIR)..."
	@ls -C *.elc
	cp *.elc $(LISPDIR)
install_src:
	@echo "Installing in $(LISPDIR)..."
	@ls -C `ls *.el 2>&1 | grep -v "fixup"` 2> /dev/null
	cp `ls *.el | grep -v "fixup"` $(LISPDIR)
install_xemacs:
	@echo "Installing source in $(LISPDIR)..."
	cp $(EFSSRC) $(DSRC) $(XESRC) efs-x19.15.el $(LISPDIR)
	cp $(DUMPSRC) $(LISPDIR)
	cp Makefile README fixup.el $(LISPDIR)
diff_xemacs:
	for f in $(EFSSRC) $(DSRC) $(XESRC) efs-x19.15.el \
	         $(DUMPSRC) Makefile README fixup.el; do \
		if [ -e $(LISPDIR)/$$f ]; \
		then\
		  diff -c $(LISPDIR)/$$f $$f; \
		else \
		  diff -c /dev/null $$f; \
		fi \
	done
install_all: install_src install
clean:
	rm -f $(OBJS)

## end of Makefile ##
