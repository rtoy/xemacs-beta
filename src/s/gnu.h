/* Definitions file for GNU Emacs running on the GNU Hurd.
   Copyright (C) 1994, 1995, 1996 Free Software Foundation, Inc.

This file is part of GNU Emacs.

GNU Emacs is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

GNU Emacs is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with XEmacs; see the file COPYING.  If not, write to
the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  */

/* Synched up with: FSF 19.31. */

/* Get most of the stuff from bsd4.3 */
#include "bsd4-3.h"

/* For mem-limits.h.  */
#define BSD4_2

#undef SYSTEM_TYPE
#define SYSTEM_TYPE "gnu"

/* XXX should getloadavg be in libc?  Should we have a libutil?
#define HAVE_GETLOADAVG */

#define LIBS_DEBUG

/* XXX emacs should not expect TAB3 to be defined.  */
#define TABDLY OXTABS
#define TAB3 OXTABS

/* GNU needs its own crt0, and libc defines data_start.  */
#define ORDINARY_LINK
#define DATA_START ({ extern int data_start; (char *) &data_start; })

/* GNU now always uses the ELF format.  */
#define UNEXEC "unexelf.o"

/* Some losing code fails to include this and then assumes
   that because it is braindead that O_RDONLY==0.  */
#ifndef NOT_C_CODE
#include <fcntl.h>
#endif

#if defined(HAVE_GRANTPT) && defined(HAVE_UNLOCKPT) && defined(HAVE_PTSNAME)
/* UNIX98 PTYs are available.
   Added by Florian Weimer <Florian.Weimer@RUS.Uni-Stuttgart.DE>,
   RUS-CERT, University of Stuttgart.  Based on Emacs code for DGUX. */

#define PTY_ITERATION for (i = 0; i < 1; i++)
/* no iteration at all */

/* Use getpt() if it's available, because it provides Unix98 PTY
   emulation for kernels which doesn't support it natively. */

#ifdef HAVE_GETPT
#define PTY_OPEN                                 \
  do {                                           \
    fd = getpt();                             \
    if (fcntl (fd, F_SETFL, O_NDELAY) == -1)  \
      fatal ("could not set master PTY to non-block mode"); \
  } while (0)

#else
/* the master PTY device */
#define PTY_NAME_SPRINTF strcpy (pty_name, "/dev/ptmx");
#endif

/* This sets the name of the slave side of the PTY.  grantpt(3) and
   unlockpt(3) may fork a subprocess, so keep sigchld_handler() from
   intercepting that death. */

#define PTY_TTY_NAME_SPRINTF			\
  {						\
    char *ptsname(), *ptyname;			\
						\
    sigblock(sigmask(SIGCHLD));			\
    if (grantpt(fd) == -1)			\
      fatal("could not grant slave pty");	\
    if (unlockpt(fd) == -1)			\
      fatal("could not unlock slave pty");	\
    if (!(ptyname = ptsname(fd)))		\
      fatal ("could not enable slave pty");	\
    strncpy(pty_name, ptyname, sizeof(pty_name)); \
    pty_name[sizeof(pty_name) - 1] = 0;		\
    sigsetmask(siggetmask() & ~sigmask(SIGCHLD));	\
  }

#endif
