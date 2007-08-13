/* Synched up with: FSF 19.31. */

#include "usg5-4.h"

#define IRIX5

#ifdef LIBS_SYSTEM
#undef LIBS_SYSTEM
#endif

#ifdef LIB_STANDARD
#undef LIB_STANDARD
#endif

#ifdef SYSTEM_TYPE
#undef SYSTEM_TYPE
#endif
#define SYSTEM_TYPE "irix"

#ifdef SETUP_SLAVE_PTY
#undef SETUP_SLAVE_PTY
#endif

/* thomas@mathematik.uni-bremen.de says this is needed.  */
/* Make process_send_signal work by "typing" a signal character on the pty.  */
#define SIGNALS_VIA_CHARACTERS

/* Define HAVE_ALLOCA to say that the system provides a properly
   working alloca function and it should be used. */
#define HAVE_ALLOCA
#undef C_ALLOCA

#ifndef NOT_C_CODE
#ifndef __GNUC__
#include <alloca.h>
#endif
#endif

/* SGI has all the fancy wait stuff, but we can't include sys/wait.h
   because it defines BIG_ENDIAN and LITTLE_ENDIAN (ugh!.)  Instead
   we'll just define WNOHANG right here.
   (An implicit decl is good enough for wait3.)  */
/* [XEmacs: Now that we don't use BIG_ENDIAN/LITTLE_ENDIAN, it's safe
   to include wait.h.  Should something change here?] */

/* #define WNOHANG		0x1 */

/* No need to use sprintf to get the tty name--we get that from _getpty.  */
#ifdef PTY_TTY_NAME_SPRINTF
#undef PTY_TTY_NAME_SPRINTF
#endif
#define PTY_TTY_NAME_SPRINTF
/* No need to get the pty name at all.  */
#ifdef PTY_NAME_SPRINTF
#undef PTY_NAME_SPRINTF
#endif
#define PTY_NAME_SPRINTF
#ifdef emacs
char *_getpty ();
#endif
/* We need only try once to open a pty.  */
#define PTY_ITERATION
/* Here is how to do it.  */
/* It is necessary to prevent SIGCHLD signals within _getpty.
   So we block them. */
#define PTY_OPEN						\
{								\
  char *name;							\
  EMACS_BLOCK_SIGCHLD;						\
  name = _getpty (&fd, O_RDWR | O_NDELAY, 0600, 0);		\
  EMACS_UNBLOCK_SIGCHLD;					\
  if (name == 0)						\
    return -1;							\
  if (fd < 0)							\
    return -1;							\
  if (fstat (fd, &stb) < 0)					\
    return -1;							\
  strcpy (pty_name, name);					\
}

/* jpff@maths.bath.ac.uk reports `struct exception' is not defined
   on this system, so inhibit use of matherr.  */
#define NO_MATHERR

/* Info from simon@lia.di.epfl.ch (Simon Leinen) suggests this is needed.  */
#define GETPGRP_NO_ARG

/* Ulimit(UL_GMEMLIM) is busted...  */
#define ULIMIT_BREAK_VALUE 0x14000000

/* Tell process_send_signal to use VSUSP instead of VSWTCH.  */
#define PREFER_VSUSP

/* Because unexelfsgi.c cannot handle a ".sbss" section yet, we must
   tell the linker to avoid making one.  SGI's cc does this by
   default, but GCC (at least 2.5.8 and 2.6.0) doesn't. */
#ifdef __GNUC__
#define LD_SWITCH_SYSTEM -G 0
#endif

/* define MAIL_USE_FLOCK if the mailer uses flock
   to interlock access to /usr/spool/mail/$USER.
   The alternative is that a lock file named
   /usr/spool/mail/$USER.lock.  */

#define MAIL_USE_FLOCK

/* use K&R C */
/* XEmacs change -- use ANSI, not K&R */
#ifndef __GNUC__
#define C_SWITCH_SYSTEM -xansi
#endif

/* jackr@engr.sgi.com says that you can't mix different kinds of
   signal-handling functions under IRIX 5.3.  I'm going to assume
   that that was the reason this got broken.  Now that the
   signal routines are fixed up, maybe this will work. --ben */
/* Nope, it doesn't.  I've tried lots of things; it must be
   genuinely broken. */
/* XEmacs addition: People on IRIX 5.2 and IRIX 5.3 systems have
   reported that they can't break out of (while t) using C-g or C-G.
   This does not occur on other systems, so let's assume that SIGIO
   is broken on these systems. */
#define BROKEN_SIGIO
