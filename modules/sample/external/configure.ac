# Process this file with autoconf to produce a configure script.
AC_INIT([Sample module], [1.0], [xemacs-beta@xemacs.org])
AC_PREREQ(2.53)
AC_REVISION($Revision: 1.1 $)
AC_COPYRIGHT([Configuration script for the sample module.
Copyright (C) 2002 Jerry James.

This file is not part of XEmacs.])

AC_CONFIG_SRCDIR([sample.c])
AC_PROG_CC
AC_PROG_INSTALL
AC_SUBST(CFLAGS)
AC_SUBST(LDFLAGS)

# Find ELLCC
AC_ARG_VAR([ELLCC], [The path to the ellcc module compiler])
AC_PATH_PROG([ELLCC], [ellcc], ["FAIL"])
AS_IF([test "$ELLCC" = "FAIL"], [AS_ERROR([Cannot find ellcc])])
AC_SUBST(ELLCC)

# Insert autoconf macros here to find the headers, libraries, other
# programs, etc. needed by your module.

AC_CONFIG_FILES([Makefile.in Makefile])
AC_OUTPUT
