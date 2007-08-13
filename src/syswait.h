/* Define wait system call interface for Emacs.
   Copyright (C) 1993, 1994, 1995 Free Software Foundation, Inc.
   Copyright (C) 1995 Sun Microsystems, Inc.

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

/* Synched up with: FSF 19.30. */

/* Cleaned up by Ben Wing. */

/* Define the structure that the wait system call stores.
   On many systems, there is a structure defined for this.
   But on vanilla-ish USG systems there is not.

   NOTE: POSIX specifies that int, rather than union wait,
   be used.  BSD systems based on BSD 4.3+ or newer generally
   have int, but those based on BSD 4.3 or older have union wait.
 */

#ifndef VMS

#  ifdef HAVE_SYS_WAIT_H
#    include <sys/wait.h>
#  endif

#  if !defined (HAVE_UNION_WAIT) /* the POSIX / SYSV way */

#    define WAITTYPE int
#    ifndef WIFSTOPPED
#      define WIFSTOPPED(w) (((w) & 0377) == 0177)
#    endif
#    ifndef WIFSIGNALED
#      define WIFSIGNALED(w) (((w) & 0377) != 0177 && ((w) & ~0377) == 0)
#    endif
#    ifndef WIFEXITED
#      define WIFEXITED(w) (((w) & 0377) == 0)
#    endif
#    ifndef WRETCODE
#      ifdef WEXITSTATUS
#        define WRETCODE(w) WEXITSTATUS (w)
#      else
#        define WRETCODE(w) ((w) >> 8)
#      endif
#    endif
#    ifndef WSTOPSIG
#      define WSTOPSIG(w) ((w) >> 8)
#    endif
#    ifndef WTERMSIG
#      define WTERMSIG(w) ((w) & 0377)
#    endif
#    ifndef WCOREDUMP
#      define WCOREDUMP(w) (((w) & 0200) != 0)
#    endif

#  else /* the older BSD way */

#    define WAITTYPE union wait

#    ifndef WRETCODE
#      ifdef WEXITSTATUS
#        define WRETCODE(w) WEXITSTATUS(w)
#      else
#        define WRETCODE(w) w.w_retcode
#      endif
#    endif

#    undef WCOREDUMP		/* Later BSDs define this name differently.  */
#    define WCOREDUMP(w) w.w_coredump

#    if defined (HPUX) || defined (convex)
/* HPUX version 7 has broken definitions of these.  */
/* pvogel@convex.com says the convex does too.  */
#      undef WTERMSIG
#      undef WSTOPSIG
#      undef WIFSTOPPED
#      undef WIFSIGNALED
#      undef WIFEXITED
#    endif /* HPUX | convex */

#    ifndef WTERMSIG
#      define WTERMSIG(w) w.w_termsig
#    endif
#    ifndef WSTOPSIG
#      define WSTOPSIG(w) w.w_stopsig
#    endif
#    ifndef WIFSTOPPED
#      define WIFSTOPPED(w) (WTERMSIG (w) == 0177)
#    endif
#    ifndef WIFSIGNALED
#      define WIFSIGNALED(w) (WTERMSIG (w) != 0177 && (WSTOPSIG (w)) == 0)
#    endif
#    ifndef WIFEXITED
#      define WIFEXITED(w) (WTERMSIG (w) == 0)
#    endif

#  endif /* HAVE_UNION_WAIT */

#else /* VMS */

#  define WAITTYPE int
#  define WIFSTOPPED(w) 0
#  define WIFSIGNALED(w) 0
#  define WIFEXITED(w) ((w) != -1)
#  define WRETCODE(w) (w)
#  define WSTOPSIG(w) (w)
#  define WCOREDUMP(w) 0
#  define WTERMSIG(w) (w)
#  include <ssdef.h>
#  include <iodef.h>
#  include <clidef.h>
#  include "vmsproc.h"

#endif /* VMS */
