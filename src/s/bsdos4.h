#include "bsdos3.h"

/* BSD/OS seems to have switched to ELF format for executables. */
#ifdef __ELF__

#undef ORDINARY_LINK
#define ORDINARY_LINK 1
#define UNEXEC unexelf.o

#endif /* ELF */

/* The declaration for openpty is missing.  Where is libutil.h? */
struct termios;
struct winsize;
int openpty (int *, int *, char *, struct termios *, struct winsize *);
