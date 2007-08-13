/* Synched up with: FSF 19.31. */

#define __EXTENSIONS__ 1
#define SOLARIS2 1
#define POSIX 1
#if 1
#ifndef USG
#define USG
#endif
#ifndef USG5_4
#define USG5_4
#endif
#if 0
#undef  SYSTEM_TYPE
#define SYSTEM_TYPE "solaris"
#endif
#undef  _POSIX_C_SOURCE
#if 0
#define _POSIX_C_SOURCE 199506L
#endif
#undef  _XOPEN_SOURCE
#undef  _XOPEN_SOURCE_EXTENDED
#if OS_RELEASE >= 55
#define _XOPEN_SOURCE 1
#define _XOPEN_SOURCE_EXTENDED 1
#endif /* >= SunOS 5.5 */
#endif

#if 1 /* mrb */
#include "usg5-4-2.h"	/* XEmacs change from 5-4 to 5-4-2 */
#endif

/* SIGIO seems to be working under Solaris and it makes ^G work better... */
#undef BROKEN_SIGIO

/* eggert@twinsun.com said these work in Solaris.
   Perhaps they work in all kinds of SVR4, but this is more conservative.  */
#undef BROKEN_TIOCGETC
#undef BROKEN_TIOCGWINSZ

#ifdef NOT_C_CODE
#define ORDINARY_LINK
/* XEmacs change -- some Motif packages need -lgen to get regex and regcmp */

#undef LIBS_SYSTEM
#define LIBS_SYSTEM "-lsocket -lnsl -lelf -lgen -ldl"

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

#undef UNEXEC
#if OS_RELEASE < 56
#define UNEXEC "unexsol2.o"
#else
#define UNEXEC "unexsol2-6.o"
#endif

#else /* C_CODE */

#if OS_RELEASE <= 53
/* Solaris 2.3 has a bug in XListFontsWithInfo.  */
#define BROKEN_XLISTFONTSWITHINFO
#endif

/* 2.5 now has random back in libc but we don't want to use it. */
#if OS_RELEASE >= 55
#undef HAVE_RANDOM
/* Apparently not necessary here, and it causes 10% CPU chewage. */
#undef BROKEN_SIGCHLD
#endif /* >= SunOS 5.5 */

/* XEmacs addition: Raymond Toy says XEmacs completely misses SIGCHLD
   when compiled with GCC 2.7.0 (but not, apparently, with SunPro C?),
   X11R6, and Solaris 2.4.

   Someone else submitted a simple test program that duplicates this
   behavior, and says it has something to do with the fact that X11R6
   links with the threads library. */

#ifdef THIS_IS_X11R6
#define BROKEN_SIGCHLD
#endif

#if OS_RELEASE == 55
/* Solaris 2.5 is the first Solaris that has getpagesize(), srandom()
   and random(), but they forgot to add prototypes to the header
   files. */
int getpagesize (void);
long random (void);
void srandom (unsigned int seed);
#endif /* SunOS 5.5 */

#if OS_RELEASE < 55
/* Missing prototype, added in Solaris 2.5 */
extern void *__builtin_alloca(size_t);
#endif /* before SunOS 5.5 */

#if OS_RELEASE < 56
/* Missing prototypes, added in Solaris 2.6 */
struct timeval;
int utimes (char *file, struct timeval *tvp);
int gethostname(char *name, int namelen);
int usleep(unsigned int useconds);
#endif /* before SunOS 5.6 */

#include <sys/utsname.h> /* warning: macro redefined: SYS_NMLN */

/* Get non-ANSI functions from ANSI header files in cc -Xc mode.
   Sun has promised to fix setjmp.h */
#if __STDC__ == 1 && defined(__SUNPRO_C)
#define _POSIX_C_SOURCE 1
#include <setjmp.h>
#undef _POSIX_C_SOURCE
#endif /* cc -Xc */

/* XEmacs: Solaris has sigsetjmp but using it leads to core dumps at
   least under 2.4 */
#undef _setjmp
#define _setjmp setjmp

#endif /* C_CODE */
