/* Synched up with: Not in FSF. */

#include "decosf3-1.h"

#ifdef NOT_C_CODE
/* This to get rid of the def in decosf3-1 forcing dynamic linking. */
#undef LD_SWITCH_CALL_SHARED
#define LD_SWITCH_CALL_SHARED -non_shared
#endif
