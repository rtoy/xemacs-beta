/* Synched up with: FSF 19.31. */

#define __EXTENSIONS__
#include "usg5-4-2.h"		/* XEmacs change from 5-4 to 5-4-2 */

#define SOLARIS2

/* SIGIO seems to be working under Solaris and it makes ^G work better... */
#undef BROKEN_SIGIO

/* eggert@twinsun.com said these work in Solaris.
   Perhaps they work in all kinds of SVR4, but this is more conservative.  */
#undef BROKEN_TIOCGETC
#undef BROKEN_TIOCGWINSZ

/* This triggers a conditional in xfaces.c.  */
#define XOS_NEEDS_TIME_H

#define POSIX

#ifndef NOT_C_CODE
/* The standard Solaris library nsl has this function in it which is
   supposed to only be in the BSD compat stuff.  Yuck.  Of course,
   there isn't a prototype for it other than in /usr/ucbinclude. */
int gethostname (char *, size_t);

/* Get non-ANSI functions from ANSI header files in cc -Xc mode.
   Sun has promised to fix setjmp.h */
#if __STDC__ == 1
#ifndef __GNUC__
#define _POSIX_C_SOURCE 1
#include <setjmp.h>
#undef _POSIX_C_SOURCE
#endif /* __GNUC__ */
#endif /* __STDC__ */

/* XEmacs: Solaris include files miss this. */
struct timeval;
int utimes (char *file, struct timeval *tvp);

/* XEmacs addition: to this to avoid having problems when we later
   define INT_MAX etc. */
#include <limits.h>
#endif /* C_CODE */

/* XEmacs change -- removed flags to force K & R compilation */

/*
 * XEmacs change -- some Motif packages need -lgen to get regex and regcmp
 */

#undef LIBS_SYSTEM
#define LIBS_SYSTEM -lsocket -lnsl -lelf -lgen -ldl

/* XEmacs change -- Mark Borges says this is needed. */
#define LIBS_TERMCAP -ltermlib 

/* #### XEmacs change: until we've gotten the Energize builds converted
   over to use configure instead of ymakefile, we still need this.
 */
#if defined (NOT_C_CODE) && defined (ENERGIZE)

# define LIBS_DEBUG
# undef LIBS_SYSTEM
# define LIBS_SYSTEM -lsocket -lnsl -lelf -lgen -ldl
# define START_FILES
# define LD_CMD $(CC)

#endif

/* SYSTEM_MALLOC must be defined if dbx/RTC is going to be used.  dbx/RTC does
   not work with a static definition of malloc(). */
/* We want to be able to test out ralloc.c. */
/* #define SYSTEM_MALLOC */

/* XEmacs: there used to be a special definition of
   PTY_TTY_NAME_SPRINTF here that was identical to the
   other SYSV R4 definitions except that it didn't
   block SIGCHLD around the call to grantpt().  This
   is *not* in 19.29 and is almost certainly incorrect.
 */

/* XEmacs change from Georg.Nikodym@Canada.Sun.COM. */
#ifdef UNEXEC 
#undef UNEXEC 
#endif
#define UNEXEC unexsol2.o

/* XEmacs: Solaris has sigsetjmp but using it leads to core dumps at
   least under 2.4 */
#undef _setjmp
#define _setjmp setjmp
