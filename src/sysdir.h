/*
   Copyright (C) 1995 Free Software Foundation, Inc.
   Copyright (C) 2000 Ben Wing.

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

#ifndef INCLUDED_sysdir_h_
#define INCLUDED_sysdir_h_

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

#ifdef SYSV_SYSTEM_DIR
# define select select_ /* Shadowing yuck */
# include <dirent.h>
# undef select
#elif defined (WIN32_NATIVE)
# include <direct.h>
# include "ndir.h"
#elif defined (NONSYSTEM_DIR_LIBRARY)
# include "ndir.h"
#else
# include <sys/dir.h>
#endif /* not NONSYSTEM_DIR_LIBRARY */

#ifdef SYSV_SYSTEM_DIR
# define DIRENTRY struct dirent
#else /* not SYSV_SYSTEM_DIR */
# define DIRENTRY struct direct
#endif

/* The d_nameln member of a struct dirent includes the '\0' character
   on some systems, but not on others.  What's worse, you can't tell
   at compile-time which one it will be, since it really depends on
   the sort of system providing the filesystem you're reading from,
   not the system you are running on.  Paul Eggert
   <eggert@bi.twinsun.com> says this occurs when Emacs is running on a
   SunOS 4.1.2 host, reading a directory that is remote-mounted from a
   Solaris 2.1 host and is in a native Solaris 2.1 filesystem.

   (and Solaris 2 doesn't have a d_nameln member at all!  Posix.1
   doesn't specify it -- mrb)

   Since applying strlen to the name always works, we'll just do that.  */
#define NAMLEN(p) strlen (p->d_name)

# define DIRENTRY_NONEMPTY(p) ((p)->d_ino)

/* encapsulation: directory calls */

int qxe_chdir (const Ibyte *path);
int qxe_mkdir (const Ibyte *path, mode_t mode);
DIR *qxe_opendir (const Ibyte *filename);
DIRENTRY *qxe_readdir (DIR *dirp);
int qxe_closedir (DIR *dirp);
int qxe_rmdir (const Ibyte *path);

Ibyte *qxe_allocating_getcwd (void);

#endif /* INCLUDED_sysdir_h_ */
