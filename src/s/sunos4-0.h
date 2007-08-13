/* Synched up with: FSF 19.31. */

/* For building XEmacs under SunOS 4.1.* with static libraries. */

#ifndef _S_SUNOS4_H_
#define _S_SUNOS4_H_

#include "bsd4-2.h"

#ifndef SUNOS4
#define SUNOS4
#endif

/* XEmacs addition: */
#ifndef HAVE_SYS_TIME_H
#define HAVE_SYS_TIME_H
#endif

#if 0  /* This may have been needed for an earlier version of Sun OS 4.
	  It seems to cause warnings in 4.0.3 and 4.1.  */
#define O_NDELAY        FNDELAY /* Non-blocking I/O (4.2 style) */
#endif

#ifdef NOT_C_CODE
  /* The new sunOS unexec eliminates the need for a custom crt0.o, so we
     can just let the compiler invoke the linker and don't have to guess
     what options it might have passed it. */
# define ORDINARY_LINK
# define START_FILES
# define LD_CMD $(CC)
# ifndef LD_SWITCH_SYSTEM
# define LD_SWITCH_SYSTEM "-Bstatic"
# endif
# define UNEXEC "unexsunos4.o"
#endif /* NOT_C_CODE */

#define RUN_TIME_REMAP

/* these don't matter, but we have to define something to keep
   sysdep.c from introducing bogus symbols */
#define TEXT_START 0
#define DATA_START 0

/* XEmacs change -- Sun CC needs this to default to ANSI */
#if __SUNPRO_C
#define C_SWITCH_SYSTEM "-Xa"
#endif

/* #### XEmacs: #define of SYSTEM_MALLOC removed.  Is this OK?  FSF says:

   In SunOS 4.1, a static function called by tzsetwall reportedly
   clears the byte just past an eight byte region it mallocs, corrupting
   GNU malloc's memory pool.  But Sun's malloc doesn't seem to mind. */

/* XEmacs: additions for proper prototyping. */
#ifndef NOT_C_CODE
#ifdef __STDC__
/* Sun's headers are categorically losing.
   Mly uses broken-sun.h to get the protos for this, but lcc provides all
   of the prototypes for the ANSI routines.  So I'm just going to put the
   protos of the non-ANSI routines that we use here (I guess that would
   be things that are Posix but not ANSI?)  You're in a maze of twisty
   little standards, all alike...
 */
/* Since lcc is not going to be heavily used anymore if it ever was, I'm
   putting broken-sun.h back in. */
#include "../broken-sun.h"
extern char *strdup ();
extern char *ttyname (int);
extern void tzsetwall (void);
extern int getpagesize (void);

#ifndef __SUNPRO_C
/* Bother! Sun can't even get the arg types right. */
#include <string.h> /* But we need to include this first because
		       *sometimes* (i.e. when using SparcWorks) the
		       correct prototypes are provided. */
#define memset(ptr, val, size) memset ((char *) ptr, val, size)
#define memcpy(dest, src, size) \
  memcpy ((char *) dest, (CONST char *) src, size)
#define memcmp(src1, src2, size) \
  memcmp ((CONST char *) src1, (CONST char *) src2, size)
#endif /* !__SUNPRO_C */

#endif /* __STDC__ */

# ifdef __GNUC__
  /* XEmacs addition: */
  /* gcc has the bug that it claims to conform to the ANSI C standard
     (which is what setting __STDC__ to 1 means) but does not necessarily
     provide all of the library routines which the standard requires of a
     conforming compiler -- such as memmove.  The other Sun ANSI compilers
     (Sun's acc and Lucid's lcc) do not have this bug. */
#  define memmove(to, from, size) bcopy ((char *) (from), (char *) (to), (size))
/* We must define mkdir with this arg prototype
   to match GCC's fixed stat.h.  */
#  define MKDIR_PROTOTYPE \
  int mkdir (const char *dpath, unsigned short dmode)
# endif /* __GNUC__ */

#endif /* !NOT_C_CODE */

#endif /* _S_SUNOS4_H_ */
