# Process this file with autoconf to produce a configure script.
# This is only used for independent module building.
AC_INIT([LDAP module], [1.0], [xemacs-beta@xemacs.org])
AC_PREREQ(2.53)
AC_REVISION($Revision: 1.2 $)
AC_COPYRIGHT([Configuration script for the LDAP module.
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

AC_CONFIG_SRCDIR([eldap.c])

m4_include([../common/configure-pre.ac])

# Find the LDAP headers and libraries
have_ldap="yes"
AC_CHECK_HEADERS([ldap.h lber.h], , [have_ldap="no"])
AS_IF([test "$have_ldap" = "yes"],
  [AC_CHECK_LIB([ldap], [ldap_search], , [have_ldap="no"; LIBS="$LIBS -llber"],
                [-llber])
   test "$have_ldap" = "no" && {
     AC_CHECK_LIB([ldap], [ldap_open], [have_ldap="yes"], [LIBS="$LIBS -lkrb"],
                  [-lkrb]) }
   test "$have_ldap" = "no" && {
     AC_CHECK_LIB([ldap], [ldap_open], [have_ldap="yes"], [LIBS="$LIBS -ldes"],
                  [-ldes]) }],
  [AS_ERROR([Cannot find the LDAP header file])])
AS_IF([test "$have_ldap" = "yes"],
  [AC_CHECK_FUNCS([ldap_set_option ldap_get_lderrno ldap_result2error ldap_parse_result])],
  [AS_ERROR([Cannot link with the LDAP library])])
AC_SUBST(ldap_libs, "$LIBS")

m4_include([../common/configure-post.ac])
