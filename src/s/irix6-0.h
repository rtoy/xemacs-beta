/* Synched up with: FSF 19.31. */

#include "irix5-3.h"

/* Irix 6.2 doesn't need -lw */
#undef NEED_LIBW

/* Irix 5 has this defined in inttypes.h, but you can't include
 * both inttypes.h and sys/types.h.  This is fixed by Irix 6.2. 
 * This should probably be set by configure.
 */
#define HAVE_UINTPTR_T 1

/* R. Cognot 09/24/97
 * This may be needed for other ABIs, but at least I'm sure it is
 * is needed on n32, as purify reports UMRs in siglongjmp and 
 * xemacs dumps core every once in a while...
 */
#if (_MIPS_SIM==_MIPS_SIM_ABIN32)
#undef HAVE_SIGSETJMP
#endif
