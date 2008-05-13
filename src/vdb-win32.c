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

#include "syswindows.h"


DWORD WINAPI 
win32_fault_handler (LPEXCEPTION_POINTERS e)
{
#define GET_FAULT_ADDRESS (void *) e->ExceptionRecord->ExceptionInformation[1]
  if ((e->ExceptionRecord->ExceptionCode == EXCEPTION_ACCESS_VIOLATION)
      && (e->ExceptionRecord->ExceptionInformation[0] == 1)
      && write_barrier_enabled
      && (fault_on_protected_page (GET_FAULT_ADDRESS)))
    {
      vdb_designate_modified (GET_FAULT_ADDRESS);
      unprotect_page_and_mark_dirty (GET_FAULT_ADDRESS);
      return EXCEPTION_CONTINUE_EXECUTION;
    } 
  else
    return EXCEPTION_CONTINUE_SEARCH;
}

typedef DWORD (WINAPI *gcPVECTORED_EXCEPTION_HANDLER) (LPEXCEPTION_POINTERS e);


void
vdb_install_signal_handler (void)
{
  HMODULE hm;
  PVOID (WINAPI *aveh) (ULONG, gcPVECTORED_EXCEPTION_HANDLER);
  
  /* See init_signals_very_early () in signal.c. */
  if (noninteractive && !initialized)
    {
      allow_incremental_gc = 0;
      return;
    }

  hm = qxeGetModuleHandle (XETEXT ("kernel32"));
  if (hm)
    aveh = (PVOID (WINAPI *) (ULONG, gcPVECTORED_EXCEPTION_HANDLER))
      GetProcAddress (hm, "AddVectoredExceptionHandler");
  else
    {
      fprintf (stderr, "\nFAILED TO LOAD LIBRARY\n");
      aveh = NULL;
    }
  if (aveh)
    {
      allow_incremental_gc = 1;
      aveh (TRUE, win32_fault_handler);
    }
  else
    {
      fprintf (stderr, "\nFAILED TO INSTALL SIGNAL HANDLER\n");
      ABORT ();
    }
}


void
vdb_protect (void *ptr, EMACS_INT len)
{
  DWORD old;
  VirtualProtect (ptr, len, PAGE_READONLY, &old);
}


void
vdb_unprotect (void *ptr, EMACS_INT len)
{
  DWORD old;
  VirtualProtect (ptr, len, PAGE_READWRITE, &old);
}
