# Process this file with autoconf to produce a configure script.
# This is only used for independent module building.
AC_INIT([Sample module], [1.0], [xemacs-beta@xemacs.org])
AC_PREREQ(2.53)
AC_REVISION($Revision: 1.2 $)
AC_COPYRIGHT([Configuration script for the sample module.
Copyright (C) 2002 Jerry James.

This file is part of XEmacs.

XEmacs is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your
option) any later version.

XEmacs is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with XEmacs; see the file COPYING.  If not, write to the Free
Software Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
02111-1307, USA.])

AC_CONFIG_SRCDIR([sample.c])
AC_PROG_CC
AC_PROG_INSTALL
AC_SUBST(CFLAGS)
AC_SUBST(LDFLAGS)

# Find ELLCC
AC_ARG_VAR([ELLCC], [The path to the ellcc module compiler])
AC_PATH_PROG([ELLCC], [ellcc], ["FAIL"])
AS_IF([test "$ELLCC" = "FAIL"], [AS_ERROR([Cannot find ellcc])])

# Insert autoconf macros here to find the headers, libraries, other
# programs, etc. needed by your module.  If other libraries will be linked
# with your module, then after detecting them, use a line of this form last:

# detect canna
canna_includes_found=no
if test "$with_canna" != "no"; then
  AC_CHECK_HEADER(canna/jrkanji.h,canna_includes_found=yes)
fi
if test "$canna_includes_found" = "no" -a "$with_canna" != "no" -a \
    -d "/usr/local/canna/include"; then
  save_c_switch_site="$c_switch_site"
  c_switch_site="$c_switch_site -I/usr/local/canna/include"
  AC_CHECK_HEADER(canna/jrkanji.h,canna_includes_found=yes)
  if test "$canna_includes_found" != "yes"; then
    c_switch_site="$save_c_switch_site"
    with_canna="no"
  fi
fi

test -z "$with_canna" && { AC_CHECK_HEADER(canna/RK.h,         , with_canna=no) }
test -z "$with_canna" && { AC_CHECK_LIB(RKC, RkBgnBun,       [:],with_canna=no) }
test -z "$with_canna" && { AC_CHECK_LIB(canna,jrKanjiControl,[:],with_canna=no) }
test -z "$with_canna" && with_canna=yes
if test "$with_canna" = "yes"; then
  AC_DEFINE(HAVE_CANNA)
  LIBS="-lcanna -lRKC"
fi

AC_SUBST(canna_libs, "$LIBS")

# This part should appear unchanged in every module configure.ac
AC_SUBST(PROGNAME, "module")
AC_SUBST(MOD_CC, "@ELLCC@")
AC_SUBST(MODARCHDIR, "\$(shell @ELLCC@ --mod-archdir)")
AC_SUBST(MAKE_DOCFILE, "\$(MODARCHDIR)/make-docfile")
AC_SUBST(MODCFLAGS, "\$(CFLAGS) --mode=compile --mod-output=\$@ -I\$(MODARCHDIR)/include")
AC_SUBST(INSTALLPATH, "\$(shell @ELLCC@ --mod-site-location)")
AC_SUBST(MOD_INSTALL_PROGRAM, "@INSTALL_PROGRAM@")
AC_SUBST(OBJECT_TO_BUILD, "\$(MODNAME).ell")

AC_CONFIG_FILES([Makefile.in Makefile])
AC_OUTPUT
