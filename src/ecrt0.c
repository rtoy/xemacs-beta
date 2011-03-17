/* C code startup routine.
   Copyright (C) 1985, 1986, 1992, 1993 Free Software Foundation, Inc.

This file is part of XEmacs.

XEmacs is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 2, or (at your option) any
later version.

XEmacs is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with XEmacs; see the file COPYING.  If not, write to
the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  */

/* Synched up with: FSF 19.30. */


/* [[The standard Vax 4.2 Unix crt0.c cannot be used for Emacs
   because it makes `environ' an initialized variable.
   It is easiest to have a special crt0.c on all machines
   though I don't know whether other machines actually need it.]]
   This is insane!  We DO NOT want to be doing this crap. */

/* On the vax and 68000, in BSD4.2 and USG5.2,
   this is the data format on startup:
  (vax) ap and fp are unpredictable as far as I know; don't use them.
  sp ->  word containing argc
         word pointing to first arg string
	 [word pointing to next arg string]... 0 or more times
	 0
Optionally:
	 [word pointing to environment variable]... 1 or more times
	 ...
	 0
And always:
	 first arg string
	 [next arg string]... 0 or more times
*/

/* On the 16000, at least in the one 4.2 system I know about,
  the initial data format is
  sp ->  word containing argc
         word containing argp
         word pointing to first arg string, and so on as above
*/

#ifdef emacs
#include <config.h>
#endif

#ifdef __GNUC__
#define asm __asm
#endif

/* Workaround for Sun cc 3.0, which doesn't handle asm's outside a fn. */
#if __SUNPRO_C >= 0x300
#define no_toplevel_asm
#endif

/*		********  WARNING ********
    Do not insert any data definitions before data_start!
    Since this is the first file linked, the address of the following
    variable should correspond to the start of initialized data space.
    On some systems this is a constant that is independent of the text
    size for shared executables.  On others, it is a function of the
    text size. In short, this seems to be the most portable way to
    discover the start of initialized data space dynamically at runtime,
    for either shared or unshared executables, on either swapping or
    virtual systems.  It only requires that the linker allocate objects
    in the order encountered, a reasonable model for most Unix systems.
    Similarly, note that the address of _start() should be the start
    of text space.   Fred Fish, UniSoft Systems Inc.  */

int data_start = 0;

#ifdef NEED_ERRNO
int errno;
#endif

char **environ;

#ifndef static
/* On systems where the static storage class is usable, this function
   should be declared as static.  Otherwise, the static keyword has
   been defined to be something else, and code for those systems must
   take care of this declaration appropriately.  */
static start1 ();
#endif

#ifdef CRT0_DUMMIES

/* Define symbol "start": here; some systems want that symbol.  */
#ifdef DOT_GLOBAL_START
asm("	.text		");
asm("	.globl start	");
asm("	start:		");
#endif /* DOT_GLOBAL_START */

#ifdef NODOT_GLOBAL_START
asm("	text		");
asm("	global start	");
asm("	start:		");
#endif /* NODOT_GLOBAL_START */

#ifdef m68000

/* GCC 2.1, when optimization is turned off, seems to want to push a
   word of garbage on the stack, which screws up the CRT0_DUMMIES
   hack.  So we hand-code _start in assembly language.  */
asm(".text			");
#ifndef sony_news
  asm("	.even			");
#else /* sony_news (not gas) */
+ asm("	.align 2		");
#endif /* sony_news (not gas) */
asm(".globl __start		");
asm("__start:			");
asm("	link a6,#0		");
asm("	jbsr _start1		");
asm("	unlk a6			");
asm("	rts			");

#else /* not m68000 */

int
_start ()
{
  start1 ();
}

#endif /* possibly m68000 */

#ifdef __bsdi__ /* for version number */
#include <sys/param.h>
#endif
#if defined(_BSDI_VERSION) && (_BSDI_VERSION >= 199501)
char *__progname;
#endif
static int
start1 (CRT0_DUMMIES int argc, char *xargv)
{
  char **argv = &xargv;
  environ = argv + argc + 1;
#if defined(_BSDI_VERSION) && (_BSDI_VERSION >= 199501)
  __progname = argv[0];
#endif

  if ((char *)environ == xargv)
    environ--;
  exit (main (argc, argv, environ));

  /* Refer to `start1' so GCC will not think it is never called
     and optimize it out.  */
  (void) &start1;
}
#else /* not CRT0_DUMMIES */

/* "m68k" and "m68000" both stand for m68000 processors,
   but with different program-entry conventions.
   This is a kludge.  Now that the CRT0_DUMMIES mechanism above exists,
   most of these machines could use the vax code above
   with some suitable definition of CRT0_DUMMIES.
   Then the symbol m68k could be flushed.
   But I don't want to risk breaking these machines
   in a version 17 patch release, so that change is being put off.  */

#ifdef m68k			/* Can't do it all from C */
	asm ("	global	_start");
	asm ("	text");
	asm ("_start:");
#ifndef NU
#ifdef STRIDE
	asm ("	comm	havefpu%,2");
#else /* m68k, not STRIDE */
	asm ("  comm	splimit%,4");
#endif /* STRIDE */
	asm ("	global	exit");
	asm ("	text");
#ifdef STRIDE
	asm ("	trap	&3");
	asm ("	mov.w	%d0,havefpu%");
#else /* m68k, not STRIDE */
  	asm ("	mov.l	%d0,splimit%");
#endif /* STRIDE */
#endif /* not NU */
	asm ("	jsr	start1");
	asm ("	mov.l	%d0,(%sp)");
	asm ("	jsr	exit");
	asm ("	mov.l	&1,%d0");	/* d0 = 1 => exit */
	asm ("	trap	&0");
#else /* m68000, not m68k */

#ifdef m68000
_start ()
{
#ifdef sun
  finitfp_();
#endif     
/* On 68000, _start pushes a6 onto stack  */
  start1 ();
}
#endif /* m68000 */
#endif /* m68k */

#if defined(m68k) || defined(m68000)
/* ignore takes care of skipping the a6 value pushed in start.  */
static
#if defined(m68k)
start1 (argc, xargv)
#else
start1 (ignore, argc, xargv)
#endif
     int argc;
     char *xargv;
{
  char **argv = &xargv;
  environ = argv + argc + 1;

  if ((char *)environ == xargv)
    environ--;
#ifdef sun_68881
  asm("    jsr     f68881_used");
#endif
#ifdef sun_fpa
  asm("    jsr     ffpa_used");
#endif
#ifdef sun_soft
  asm("    jsr     start_float");
#endif
  exit (main (argc, argv, environ));
}

#endif /* m68k or m68000 */

#endif /* not CRT0_DUMMIES */

#ifdef sparc
#ifdef no_toplevel_asm
static no_toplevel_asm_wrapper() {
#ifdef USG5_4
asm (".pushsection \".text\"");
#endif /* USG5_4 */
#endif /* no_toplevel_asm */
#ifdef USG5_4
asm (".global _start");
asm (".text");
asm ("_start:");
asm ("	mov	0, %fp");
asm ("	ld	[%sp + 64], %o0");
asm ("	add	%sp, 68, %o1");
asm ("	sll	%o0, 2,	%o2");
asm ("	add	%o2, 4,	%o2");
asm ("	add	%o1, %o2, %o2");
asm ("	sethi	%hi(_environ), %o3");
asm ("	st	%o2, [%o3+%lo(_environ)]");
asm ("	andn	%sp, 7,	%sp");
asm ("	call	main");
asm ("	sub	%sp, 24, %sp");
asm ("	call	_exit");
asm ("	nop");
#else
asm (".global __start");
asm (".text");
asm ("__start:");
asm ("	mov	0, %fp");
asm ("	ld	[%sp + 64], %o0");
asm ("	add	%sp, 68, %o1");
asm ("	sll	%o0, 2,	%o2");
asm ("	add	%o2, 4,	%o2");
asm ("	add	%o1, %o2, %o2");
asm ("	sethi	%hi(_environ), %o3");
asm ("	st	%o2, [%o3+%lo(_environ)]");
asm ("	andn	%sp, 7,	%sp");
asm ("	call	_main");
asm ("	sub	%sp, 24, %sp");
asm ("	call	__exit");
asm ("	nop");
#endif /* USG5_4 */
#ifdef no_toplevel_asm
#ifdef USG5_4
asm (".popsection");
#endif /* USG5_4 */
} /* no_toplevel_asm_wrapper() */
#endif /* no_toplevel_asm */
#endif /* sparc */

#if __FreeBSD__ == 2
char *__progname;
#endif
#ifdef __bsdi__
#include <sys/param.h> /* for version number */
#if defined(_BSDI_VERSION) && (_BSDI_VERSION >= 199501)
char *__progname;
#endif
#endif /* __bsdi__ */
