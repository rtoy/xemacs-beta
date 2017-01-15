/* s/ file for openbsd systems.  */

/* Synced up with: FSF 23.1.92. */
/* Synced by: Ben Wing, 2-18-10. */

/* Mostly the same as NetBSD.  */
#include "netbsd.h"

#if 0 /* Following mrb, this stuff is probably unneeded for XEmacs */
/*  This very-badly named symbol is conditionally defined in netbsd.h.
    Better would be either to not need it in the first place, or to choose
    a more descriptive name.  */
#ifndef LD_SWITCH_SYSTEM_tmp
#define LD_SWITCH_SYSTEM_tmp /* empty */
#endif
#endif /* 0 */

/* TERMINFO, LIBS_TERMCAP deleted */

#if 0 /* Following mrb, this stuff is probably unneeded for XEmacs */
#undef LD_SWITCH_SYSTEM_TEMACS
#undef LD_SWITCH_SYSTEM
#ifdef __ELF__

  /*  Han Boetes <han@mijncomputer.nl> says this
      is necessary,  otherwise Emacs dumps core on elf systems.  */
#define LD_SWITCH_SYSTEM LD_SWITCH_SYSTEM_tmp "-Z"

/* The version of gcc on OpenBSD doesn't search /usr/local/lib by
   default.  */
#define LD_SWITCH_X_DEFAULT "-L/usr/local/lib"

#else
  
#define LD_SWITCH_SYSTEM LD_SWITCH_SYSTEM_tmp
#define LD_SWITCH_X_DEFAULT "-L/usr/local/lib"

#endif
#endif /* 0 */

/* arch-tag: 7e3f65ca-3f48-4237-933f-2b208b21e8e2
   (do not change this comment) */
