/* Virtual diry bit implementation for XEmacs.
   Copyright (C) 2005 Marcus Crestani.

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

/* Synched up with: Not in FSF. */

#include <config.h>
#include "lisp.h"
#include "gc.h"
#include "mc-alloc.h"
#include "vdb.h"

#include <errno.h>
#include <signal.h>
#include <sys/mman.h>

#if defined (HAVE_SIGACTION)
# if defined (HAVE_STRUCT_SIGINFO_SI_ADDR)
#  define FAULT_HANDLER_ARGUMENTS \
     int signum, struct siginfo *siginfo, void *UNUSED (ctx)
#  define GET_FAULT_ADDRESS siginfo->si_addr
# elif defined (HAVE_SIGINFO_T_SI_ADDR)
#  define FAULT_HANDLER_ARGUMENTS \
     int signum, siginfo_t *siginfo, void *UNUSED (ctx)
#  define GET_FAULT_ADDRESS siginfo->si_addr
# endif
# define USE_SIGACTION
# define FAULT_HANDLER_REMOVE_HANDLER
#elif defined (HAVE_SIGNAL)
# define FAULT_HANDLER_ARGUMENTS int signum, struct sigcontext sc
# define GET_FAULT_ADDRESS (void *) sc.cr2
# define USE_SIGNAL
#endif


#ifdef USE_SIGACTION
struct sigaction act, segv_oact, bus_oact;
#endif /* USE_SIGACTION */

#ifdef USE_SIGNAL
sighandler_t segv_oact, bus_oact;
#endif /* USE_SIGNAL */

void vdb_remove_signal_handler (void);

void 
vdb_fault_handler (FAULT_HANDLER_ARGUMENTS)
{
  if (write_barrier_enabled
      && (fault_on_protected_page (GET_FAULT_ADDRESS)))
    {
      vdb_designate_modified (GET_FAULT_ADDRESS);
      unprotect_page_and_mark_dirty (GET_FAULT_ADDRESS);
#ifdef FAULT_HANDLER_REINSTALL_HANDLER
      vdb_install_signal_handler ();
#endif /* FAULT_HANDLER_REINSTALL_HANDLER */
    }
  else  /* default sigsegv handler */
    {
      char *signal_name = "";
      if (signum == SIGSEGV)
	signal_name = "SIGSEGV";
      else if (signum == SIGBUS)
	signal_name = "SIGBUS";
      else 
	ABORT (); /* something weird happened: wrong signal caught */
      fprintf (stderr, "\n\nFatal Error: Received %s (%d) for address 0x%x\n",
	       signal_name, signum, (int) GET_FAULT_ADDRESS);
#ifdef FAULT_HANDLER_CALL_PREVIOUS_HANDLER
      if (signum == SIGSEGV)
	segv_oact (signum);
      else if (signum == SIGBUS)
	bus_oact (signum);
#endif /* FAULT_HANDLER_CALL_PREVIOUS_HANDLER */
#ifdef FAULT_HANDLER_REMOVE_HANDLER
      vdb_remove_signal_handler ();
#endif /* FAULT_HANDLER_REMOVE_HANDLER */
    }
}

void 
vdb_remove_signal_handler (void)
{
#ifdef USE_SIGACTION
  sigaction(SIGSEGV, &segv_oact, 0);
  sigaction(SIGBUS, &bus_oact, 0);
#endif /* USE_SIGACTION */
#ifdef USE_SIGNAL
  signal (SIGSEGV, segv_oact);
  signal (SIGBUS, bus_oact);
#endif
}

void
vdb_install_signal_handler (void)
{
  /* See init_signals_very_early () in signal.c. */
  if (noninteractive && !initialized)
    {
      allow_incremental_gc = 0;
      return;
    }

#ifdef USE_SIGACTION
  memset(&act, 0, sizeof(struct sigaction));
  act.sa_sigaction = vdb_fault_handler;
  sigemptyset (&act.sa_mask);
  act.sa_flags = SA_SIGINFO;
  sigaction (SIGSEGV, &act, &segv_oact);
  sigaction (SIGBUS, &act, &bus_oact);
  allow_incremental_gc = 1;
#endif /* USE_SIGACTION */
#ifdef USE_SIGNAL
  segv_oact = signal (SIGSEGV, (void (*)(int)) vdb_fault_handler);
  bus_oact = signal (SIGBUS, (void (*)(int)) vdb_fault_handler);
#endif /* USE_SIGNAL */
}

void
vdb_protect (void *ptr, EMACS_INT len)
{
  if (mprotect (ptr, len, PROT_READ))
    {
      perror ("Couldn't mprotect");
      ABORT ();
    }
}

void
vdb_unprotect (void *ptr, EMACS_INT len)
{
  if (mprotect (ptr, len, PROT_READ | PROT_WRITE))
    {
      perror ("Couldn't mprotect");
      ABORT ();
    }
}
