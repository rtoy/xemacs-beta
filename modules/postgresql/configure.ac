# Process this file with autoconf to produce a configure script.
# This is only used for independent module building.
AC_INIT([PostgreSQL module], [1.0], [xemacs-beta@xemacs.org])
AC_PREREQ(2.53)
AC_REVISION($Revision: 1.2 $)
AC_COPYRIGHT([Configuration script for the PostgreSQL module.
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

AC_CONFIG_SRCDIR([postgresql.c])

m4_include([../common/configure-pre.ac])

dnl On many Linux systems, PostgreSQL is packaged to be installed in /usr;
dnl in this case, configure will easily detect it there.
dnl
dnl If PostgreSQL is installed into a different prefix,
dnl (such as the default /usr/local/pgsql when building from source),
dnl then specify CPPFLAGS and LDFLAGS when configuring.
dnl
dnl Look for these standard header file locations, known to be used on Linux
for header_dir in "" "pgsql/" "postgresql/"; do
  AC_CHECK_HEADER([${header_dir}libpq-fe.h],
                  [libpq_fe_h_file=${header_dir}libpq-fe.h; break])
done

test -z "$libpq_fe_h_file" &&
  AS_ERROR([Cannot find the PostgresQL header files; try specifying CPPFLAGS.])
AC_DEFINE_UNQUOTED(LIBPQ_FE_H_FILE, "$libpq_fe_h_file")

AC_CHECK_LIB(pq,PQconnectdb,,
  AS_ERROR([Cannot find the PostgresQL library.  Try specifying LDFLAGS.]))
AC_CHECK_LIB(pq,PQconnectStart, [
             with_postgresqlv7=yes;
             AC_DEFINE(HAVE_POSTGRESQLV7)])
AC_SUBST(postgresql_libs, "$LIBS")

m4_include([../common/configure-post.ac])
