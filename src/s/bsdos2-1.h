/* Synched up with: not in FSF. */

/* s/ file for BSDI BSD/OS 2.1 system. */

#include "bsdos2.h"

/* -lX11 needs shmat and shmdt from -lipc. */
#define LIBX11_SYSTEM "-lipc"
