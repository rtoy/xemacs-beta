/*
   Copyright (C) 1995 Free Software Foundation, Inc.

This file is part of XEmacs.

XEmacs is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 2, or (at your option) any
later version.

XEmacs is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with XEmacs; see the file COPYING.  If not, write to
the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  */

/* Synched up with: Not really in FSF. */

#include <errno.h>

#ifndef WINDOWSNT
#include <sys/errno.h>          /* <errno.h> does not always imply this */
#endif

/* Load sys/types.h if not already loaded.
   In some systems loading it twice is suicidal.  */
#ifndef makedev
#include <sys/types.h>		/* some typedefs are used in sys/file.h */
#endif
#include <sys/file.h>
#include <sys/stat.h>
#include <sys/param.h>

#if defined (NeXT) || defined(__CYGWIN32__)
/* what is needed from here?  Do others need it too? 
 O_BINARY is in here under cygwin. */
# include <sys/fcntl.h>
#endif /* NeXT */

#ifdef WINDOWSNT
#include <io.h>
#endif

#if 0
#ifdef WINDOWSNT
#include <windows.h>
#include <stdlib.h>	/* for proper declaration of environ */
#include <fcntl.h>
#include "nt.h"
#define _P_NOWAIT 1	/* from process.h */
#endif
#endif

#ifdef MSDOS
#include <dos.h>
#include "msdos.h"
#endif /* MSDOS */

#ifndef O_RDONLY
#define O_RDONLY 0
#endif

#ifndef O_WRONLY
#define O_WRONLY 1
#endif

#ifndef O_RDWR
#define O_RDWR 2
#endif

/* file opening defaults */
#ifndef OPEN_BINARY
#ifdef O_BINARY
#define OPEN_BINARY	O_BINARY
#else
#define OPEN_BINARY	(0)
#endif
#endif

#ifndef OPEN_TEXT
#ifdef O_TEXT
#define OPEN_TEXT	O_TEXT
#else
#define OPEN_TEXT	(0)
#endif
#endif

#ifndef CREAT_MODE
#ifdef WINDOWSNT
#define CREAT_MODE	(S_IREAD | S_IWRITE)
#else
#define CREAT_MODE	(0666)
#endif
#endif

#ifndef READ_TEXT
#ifdef O_TEXT
#define READ_TEXT "rt"
#else
#define READ_TEXT "r"
#endif
#endif

#ifndef READ_BINARY
#ifdef O_BINARY
#define READ_BINARY "rb"
#else
#define READ_BINARY "r"
#endif
#endif

#ifndef WRITE_BINARY
#ifdef O_BINARY
#define WRITE_BINARY "wb"
#else
#define WRITE_BINARY "w"
#endif
#endif

/* if system does not have symbolic links, it does not have lstat.
   In that case, use ordinary stat instead.  */

#ifndef S_IFLNK
#define lstat stat
#endif

#if !S_IRUSR
# if S_IREAD
#  define S_IRUSR S_IREAD
# else
#  define S_IRUSR 00400
# endif
#endif

#if !S_IWUSR
# if S_IWRITE
#  define S_IWUSR S_IWRITE
# else
#  define S_IWUSR 00200
# endif
#endif

#if !S_IXUSR
# if S_IEXEC
#  define S_IXUSR S_IEXEC
# else
#  define S_IXUSR 00100
# endif
#endif

#ifdef STAT_MACROS_BROKEN
#undef S_ISBLK
#undef S_ISCHR
#undef S_ISDIR
#undef S_ISFIFO
#undef S_ISLNK
#undef S_ISMPB
#undef S_ISMPC
#undef S_ISNWK
#undef S_ISREG
#undef S_ISSOCK
#endif /* STAT_MACROS_BROKEN.  */

#if !defined(S_ISBLK) && defined(S_IFBLK)
#define S_ISBLK(m) (((m) & S_IFMT) == S_IFBLK)
#endif
#if !defined(S_ISCHR) && defined(S_IFCHR)
#define S_ISCHR(m) (((m) & S_IFMT) == S_IFCHR)
#endif
#if !defined(S_ISDIR) && defined(S_IFDIR)
#define S_ISDIR(m) (((m) & S_IFMT) == S_IFDIR)
#endif
#if !defined(S_ISREG) && defined(S_IFREG)
#define S_ISREG(m) (((m) & S_IFMT) == S_IFREG)
#endif
#if !defined(S_ISFIFO) && defined(S_IFIFO)
#define S_ISFIFO(m) (((m) & S_IFMT) == S_IFIFO)
#endif
#if !defined(S_ISLNK) && defined(S_IFLNK)
#define S_ISLNK(m) (((m) & S_IFMT) == S_IFLNK)
#endif
#if !defined(S_ISSOCK) && defined(S_IFSOCK)
#define S_ISSOCK(m) (((m) & S_IFMT) == S_IFSOCK)
#endif
#if !defined(S_ISMPB) && defined(S_IFMPB) /* V7 */
#define S_ISMPB(m) (((m) & S_IFMT) == S_IFMPB)
#define S_ISMPC(m) (((m) & S_IFMT) == S_IFMPC)
#endif
#if !defined(S_ISNWK) && defined(S_IFNWK) /* HP/UX */
#define S_ISNWK(m) (((m) & S_IFMT) == S_IFNWK)
#endif

#if !defined (USG)  && !defined (WINDOWSNT)
# define HAVE_FSYNC
#endif

#ifndef MAXPATHLEN
/* in 4.1, param.h fails to define this. */
#define MAXPATHLEN 1024
#endif /* not MAXPATHLEN */

#ifndef X_OK
# define X_OK 01
#endif

#ifndef FD_CLOEXEC
# define FD_CLOEXEC 1
#endif

/* encapsulations: file-information calls */

#ifdef ENCAPSULATE_ACCESS
extern int sys_access (CONST char *path, int mode);
#endif
#if defined (ENCAPSULATE_ACCESS) && !defined (DONT_ENCAPSULATE)
# undef access
# define access sys_access
#endif
#if !defined (ENCAPSULATE_ACCESS) && defined (DONT_ENCAPSULATE)
# define sys_access access
#endif

#ifdef ENCAPSULATE_EACCESS
extern int sys_eaccess (CONST char *path, int mode);
#endif
#if defined (ENCAPSULATE_EACCESS) && !defined (DONT_ENCAPSULATE)
# undef eaccess
# define eaccess sys_eaccess
#endif
#if !defined (ENCAPSULATE_EACCESS) && defined (DONT_ENCAPSULATE)
# define sys_eaccess eaccess
#endif

#ifdef ENCAPSULATE_LSTAT
extern int sys_lstat (CONST char *path, struct stat *buf);
#endif
#if defined (ENCAPSULATE_LSTAT) && !defined (DONT_ENCAPSULATE)
# undef lstat
# define lstat sys_lstat
#endif
#if !defined (ENCAPSULATE_LSTAT) && defined (DONT_ENCAPSULATE)
# define sys_lstat lstat
#endif

#ifdef ENCAPSULATE_READLINK
extern int sys_readlink (CONST char *path, char *buf, int bufsiz);
#endif
#if defined (ENCAPSULATE_READLINK) && !defined (DONT_ENCAPSULATE)
# undef readlink
# define readlink sys_readlink
#endif
#if !defined (ENCAPSULATE_READLINK) && defined (DONT_ENCAPSULATE)
# define sys_readlink readlink
#endif

#ifdef ENCAPSULATE_STAT
extern int sys_stat (CONST char *path, struct stat *buf);
#endif
#if defined (ENCAPSULATE_STAT) && !defined (DONT_ENCAPSULATE)
# undef stat
/* Need to use arguments to avoid messing with struct stat */
# define stat(path, buf) sys_stat (path, buf)
#endif
#if !defined (ENCAPSULATE_STAT) && defined (DONT_ENCAPSULATE)
# define sys_stat stat
#endif

/* encapsulations: file-manipulation calls */

#ifdef ENCAPSULATE_CHMOD
extern int sys_chmod (CONST char *path, int mode);
#endif
#if defined (ENCAPSULATE_CHMOD) && !defined (DONT_ENCAPSULATE)
# undef chmod
# define chmod sys_chmod
#endif
#if !defined (ENCAPSULATE_CHMOD) && defined (DONT_ENCAPSULATE)
# define sys_chmod chmod
#endif

#ifdef ENCAPSULATE_CREAT
extern int sys_creat (CONST char *path, int mode);
#endif
#if defined (ENCAPSULATE_CREAT) && !defined (DONT_ENCAPSULATE)
# undef creat
# define creat sys_creat
#endif
#if !defined (ENCAPSULATE_CREAT) && defined (DONT_ENCAPSULATE)
# define sys_creat creat
#endif

#ifdef ENCAPSULATE_LINK
extern int sys_link (CONST char *existing, CONST char *new);
#endif
#if defined (ENCAPSULATE_LINK) && !defined (DONT_ENCAPSULATE)
# undef link
# define link sys_link
#endif
#if !defined (ENCAPSULATE_LINK) && defined (DONT_ENCAPSULATE)
# define sys_link link
#endif

#ifdef ENCAPSULATE_RENAME
extern int sys_rename (CONST char *old, CONST char *new);
#endif
#if defined (ENCAPSULATE_RENAME) && !defined (DONT_ENCAPSULATE)
# undef rename
# define rename sys_rename
#endif
#if !defined (ENCAPSULATE_RENAME) && defined (DONT_ENCAPSULATE)
# define sys_rename rename
#endif

#ifdef ENCAPSULATE_SYMLINK
extern int sys_symlink (CONST char *name1, CONST char *name2);
#endif
#if defined (ENCAPSULATE_SYMLINK) && !defined (DONT_ENCAPSULATE)
# undef symlink
# define symlink sys_symlink
#endif
#if !defined (ENCAPSULATE_SYMLINK) && defined (DONT_ENCAPSULATE)
# define sys_symlink symlink
#endif

#ifdef ENCAPSULATE_UNLINK
extern int sys_unlink (CONST char *path);
#endif
#if defined (ENCAPSULATE_UNLINK) && !defined (DONT_ENCAPSULATE)
# undef unlink
# define unlink sys_unlink
#endif
#if !defined (ENCAPSULATE_UNLINK) && defined (DONT_ENCAPSULATE)
# define sys_unlink unlink
#endif

#ifdef ENCAPSULATE_EXECVP
extern int sys_execvp (CONST char *, char * CONST *);
#endif
#if defined (ENCAPSULATE_EXECVP) && !defined (DONT_ENCAPSULATE)
# undef execvp
# define execvp sys_execvp
#endif
#if !defined (ENCAPSULATE_EXECVP) && defined (DONT_ENCAPSULATE)
# define sys_execvp execvp
#endif
