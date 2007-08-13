/* Synched up with: FSF 19.31. */

/* s/ file for freebsd system.  */

/* '__FreeBSD__' is defined by the preprocessor on FreeBSD-1.1 and up.
   Earlier versions do not have shared libraries, so inhibit them.
   You can inhibit them on newer systems if you wish
   by defining NO_SHARED_LIBS.  */
#ifndef __FreeBSD__
#define NO_SHARED_LIBS
#endif

/* Get most of the stuff from bsd4.3 */
#include "bsd4-3.h"

/* For mem-limits.h. */
#define BSD4_2

/* These aren't needed, since we have getloadavg.  */
#undef KERNEL_FILE
#undef LDAV_SYMBOL

#define PENDING_OUTPUT_COUNT(FILE) ((FILE)->_p - (FILE)->_bf._base)

#define LIBS_DEBUG
/* FreeBSD 2.2 or later */
#ifndef __FreeBSD_version
#include <osreldate.h>
#endif
#if __FreeBSD_version >= 199701
#define LIBS_SYSTEM -lutil -lxpg4
#else
#define LIBS_SYSTEM -lutil
#endif

#define LIBS_TERMCAP -ltermcap
#define LIB_GCC -lgcc

/* freebsd has POSIX-style pgrp behavior. */
#define GETPGRP_NO_ARG

#ifndef NO_SHARED_LIBS
#define LD_SWITCH_SYSTEM -dc -dp -e start
#define HAVE_TEXT_START		/* No need to define `start_of_text'. */
#define START_FILES pre-crt0.o /usr/lib/crt0.o
#define UNEXEC unexfreebsd.o
#define RUN_TIME_REMAP

#ifndef N_TRELOFF
#define N_PAGSIZ(x) __LDPGSZ
#define N_BSSADDR(x) (N_ALIGN(x, N_DATADDR(x)+x.a_data))
#define N_TRELOFF(x) N_RELOFF(x)
#endif
#else /* NO_SHARED_LIBS */
#ifdef __FreeBSD__  /* shared libs are available, but the user prefers
                     not to use them.  */
#define LD_SWITCH_SYSTEM -Bstatic
#define A_TEXT_OFFSET(x) (sizeof (struct exec))
#define A_TEXT_SEEK(hdr) (N_TXTOFF(hdr) + A_TEXT_OFFSET(hdr))
#endif /* __FreeBSD__ */
#endif /* NO_SHARED_LIBS */

#define HAVE_GETLOADAVG
#define NO_TERMIO
#define DECLARE_GETPWUID_WITH_UID_T

/* freebsd uses OXTABS instead of the expected TAB3. */
#define TABDLY OXTABS
#define TAB3 OXTABS

/* this silences a few compilation warnings */
#undef BSD
#if __FreeBSD__ == 1
#define BSD 199103
#elif __FreeBSD__ == 2
#if __FreeBSD_version < 199701
# define BSD 199306
#else
# define BSD 199506
#endif
#elif __FreeBSD__ == 3
#define BSD 199506
#endif

/* FreeBSD defines INT_MAX in /usr/include/limits.h.
   Unless INT_MAX is already defined in lisp.h, XEmacs goes right
   ahead and rolls its own.
   We make sure that the system's definition is used throughout. */
#ifdef emacs
#include <limits.h>
#endif

/* Needed to avoid hanging when child process writes an error message
   and exits -- enami tsugutomo <enami@ba2.so-net.or.jp>.  */
#define vfork fork
