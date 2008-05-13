/* This is the configuration file for the Apple MacOS X operating system.
   It's descended from NeXTstep and Freebsd.
   The system is popularly known as "Darwin".  */

/* This defines the symbol `BSD', which is required for fakemail to
   compile correctly. */
#ifndef NOT_C_CODE
#include <sys/param.h>
#endif

/* If you don't use the system malloc, you get an obscure link error. */
#define SYSTEM_MALLOC
