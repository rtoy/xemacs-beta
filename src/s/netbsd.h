/* Synched up with: FSF 19.31. */

/* s/ file for netbsd system.  */

/* Get most of the stuff from bsd4.3 */
#include "bsd4-3.h"

/* For mem-limits.h.  */
#define BSD4_2

#undef KERNEL_FILE
#undef LDAV_SYMBOL
#define HAVE_GETLOADAVG

#define SIGNALS_VIA_CHARACTERS

#define PENDING_OUTPUT_COUNT(FILE) ((FILE)->_p - (FILE)->_bf._base)

/* netbsd uses OXTABS instead of the expected TAB3.  */
#define TABDLY OXTABS
#define TAB3 OXTABS

#define A_TEXT_OFFSET(x) (sizeof (struct exec))
#define A_TEXT_SEEK(hdr) (N_TXTOFF(hdr) + A_TEXT_OFFSET(hdr))

#define NO_TERMIO

#define LIBS_DEBUG
/* -lutil is not needed for NetBSD >0.9.  */
/* #define LIBS_SYSTEM -lutil */
/* XEmacs change */
#ifdef HAVE_NCURSES
#define LIBS_TERMCAP -lncurses -ltermcap
#else
#define LIBS_TERMCAP -ltermcap
#endif

#define NEED_ERRNO

#define GETPGRP_NO_ARG

#ifndef NO_SHARED_LIBS
/* These definitions should work for either dynamic or static linking,
   whichever is the default for `cc -nostdlib'.  */
/* but they probably don't, and life's too short - jrg@doc.ic.ac.uk 
   ask for no shared libs if you have 0.9 */
#define HAVE_TEXT_START		/* No need to define `start_of_text'.  */
#define LD_SWITCH_SYSTEM -e start
#define START_FILES pre-crt0.o /usr/lib/crt0.o
#define UNEXEC unexfreebsd.o    /* ironic, considering history of unexfreebsd */
#define RUN_TIME_REMAP

/* Try to make this work for both 0.9 and >0.9.  */
#define N_PAGSIZ(x) __LDPGSZ
#define N_BSSADDR(x) (N_ALIGN(x, N_DATADDR(x)+x.a_data))
/* #define N_TRELOFF(x) N_RELOFF(x) */
/* the 1.0 way.. */
#define N_RELOFF(x) N_TRELOFF(x)
#else
#define START_FILES crt0.o
#endif /* not NO_SHARED_LIBS */

#define NO_MATHERR

#define AMPERSAND_FULL_NAME
