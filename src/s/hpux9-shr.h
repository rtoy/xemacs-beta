/* Synched up with: FSF 19.31. */

/* For building XEmacs under HPUX 9.0 with dynamic libraries. */

#define ORDINARY_LINK

/* XEmacs change */
/* Only support for hp9000s700 currently */
#if !defined(__hp9000s300)
/* #ifndef USE_GCC */
#define HPUX_USE_SHLIBS
/* #endif */
#endif /* !hp9000s300 */

/* XEmacs: */
/* Don't tell the linker to link statically */
#ifdef NOT_C_CODE
#define START_FILES
#define LINKER $(CC)
/* now done in hpux8.h */
/* #define LD_SWITCH_SYSTEM -L/usr/lib/X11R5 -L/usr/lib/Motif1.2 */
#endif /* THIS IS YMAKEFILE */

/* get call to brk() when rerunning XEmacs */
/* #ifndef USE_GCC */
#define RUN_TIME_REMAP
/* #endif */

#include "hpux9.h"

/* The curses library seems to have a badly broken version of select(2)
   that makes "poll: interrupted system call" messages to appear and
   Emacs suprocesses to hang (e.g. TeX compilation w/ AUCTeX).
   Althought this is only for hpux 10, linking termcap instead of curses
   on 9.X ensures a 9.X binary will still run properly on 10.X. */
#undef LIBS_TERMCAP
#define LIBS_TERMCAP -ltermcap

#if 0 /* No longer needed, since in current GCC -g no longer does that.  */
/* We must turn off -g since it forces -static.  */
#ifdef __GNUC__
#undef C_DEBUG_SWITCH
#define C_DEBUG_SWITCH
#endif
#endif
