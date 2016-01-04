/* `alloca' standard 4.2 subroutine for 68000's and 16000's and others.
   Copyright (C) 1985, 1986, 1988 Free Software Foundation, Inc.

This file is part of XEmacs.

XEmacs is free software: you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation, either version 3 of the License, or (at your
option) any later version.

XEmacs is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with XEmacs.  If not, see <http://www.gnu.org/licenses/>. */

/* Synched up with: FSF 19.30. */

/* Both 68000 systems I have run this on have had broken versions of alloca.
   Also, I am told that non-berkeley systems do not have it at all.
   So replace whatever system-provided alloca there may be
   on all 68000 systems.  */

#define NOT_C_CODE
#ifdef emacs
#include <config.h>
#else
#include "config.h"
#endif

#ifndef HAVE_ALLOCA  /* define this to use system's alloca */

#ifndef m68k
#ifndef m68000
you
lose!!
#endif /* m68000 */
#endif /* m68k */


#ifdef m68k			/* SGS assembler totally different */
	file	"alloca.s"
	global	alloca
alloca:
	mov.l	(%sp)+,%a1	# pop return addr from top of stack
	mov.l	(%sp)+,%d0	# pop size in bytes from top of stack
	add.l	&R%1,%d0	# round size up to long word
	and.l	&-4,%d0		# mask out lower two bits of size
	sub.l	%d0,%sp		# allocate by moving stack pointer
	tst.b	P%1(%sp)	# stack probe to allocate pages
	mov.l	%sp,%a0		# return pointer as pointer
	mov.l	%sp,%d0		# return pointer as int to avoid disaster
	add.l	&-4,%sp		# new top of stack
	jmp	(%a1)		# not a normal return
	set	S%1,64		# safety factor for C compiler scratch
	set	R%1,3+S%1	# add to size for rounding
	set	P%1,-132	# probe this far below current top of stack

#else /* not m68k */

#ifdef m68000

/* Some systems want the _, some do not.  Win with both kinds.  */
.globl	_alloca
_alloca:
.globl	alloca
alloca:
	movl	sp@+,a0
	movl	a7,d0
	subl	sp@,d0
	andl	#~3,d0
	movl	d0,sp
	tstb	sp@(0)		/* Make stack pages exist  */
				/* Needed on certain systems
				   that lack true demand paging */
	addql	#4,d0
	jmp	a0@

#endif /* m68000 */
#endif /* not m68k */

#endif /* not HAVE_ALLOCA */
