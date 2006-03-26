/* Virtual diry bit implementation (platform independent) for XEmacs.
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


typedef struct
{
  Dynarr_declare (void *);
} void_ptr_dynarr;

void_ptr_dynarr *page_fault_table;

/* Init page fault table and protect heap. */
void
vdb_start_dirty_bits_recording (void)
{
  Elemcount protected_pages = (Elemcount) protect_heap_pages ();
  page_fault_table = Dynarr_new2 (void_ptr_dynarr, void *);
  Dynarr_resize (page_fault_table, protected_pages);
}

/* Remove heap protection. */
void
vdb_stop_dirty_bits_recording (void)
{
  unprotect_heap_pages ();
}

/* Read page fault table and pass page faults to garbage collector. */
int 
vdb_read_dirty_bits (void)
{
  int repushed_objects = 0;
  Elemcount count;
  for (count = Dynarr_length (page_fault_table); count; count--)
    repushed_objects += 
      repush_all_objects_on_page (Dynarr_at (page_fault_table, count - 1));
  Dynarr_free (page_fault_table);
  page_fault_table = 0;
  return repushed_objects;
}

/* Called by the page fault handler: add address to page fault table. */
void 
vdb_designate_modified (void *addr)
{
  Dynarr_add (page_fault_table, addr);
}


/* For testing and debugging... */

DEFUN ("test-vdb", Ftest_vdb, 0, 0, "", /*
Test virtual dirty bit implementation. Prints results to stderr.
*/
       ())
{
  Rawbyte *p;
  char c;
  Elemcount count;
  
  /* Wrap up gc (if currently running). */
  gc_full ();

  /* Allocate a buffer; it will have the default
     protection of PROT_READ|PROT_WRITE. */
  p = (Rawbyte *) mc_alloc (mc_get_page_size());
  set_lheader_implementation ((struct lrecord_header *) p, &lrecord_cons);
  fprintf (stderr, "Allocate p: [%x ... %x], length %d\n", 
	   (int) p, (int) (p + mc_get_page_size ()), 
	   (int) mc_get_page_size ());

  /* Test read. */
  fprintf (stderr, "Attempt to read p[666]... ");
  c = p[666];
  fprintf (stderr, "read ok.\n");

  /* Test write. */
  fprintf (stderr, "Attempt to write 42 to p[666]... ");
  p[666] = 42;
  fprintf (stderr, "write ok, p[666] = %d\n", p[666]);

  /* Mark the buffer read-only and set environemnt for write-barrier. */
  fprintf (stderr, "Write-protect the page.\n");
  MARK_BLACK (p);
  vdb_start_dirty_bits_recording ();
  write_barrier_enabled = 1;

  /* Test write-barrier read. */
  fprintf (stderr, "Attempt to read p[666]... ");
  c = p[666];
  fprintf (stderr, "read ok.\n");
 
  /* Test write-barrier write, program receives SIGSEGV. */
  fprintf (stderr, "Attempt to write 23 to p[666]... ");
  p[666] = 23;
  fprintf (stderr, "Written p[666] = %d\n", p[666]);

  /* Stop write-barrier mode. */
  write_barrier_enabled = 0;
  MARK_WHITE (p);
  vdb_unprotect (p, mc_get_page_size ());
  for (count = Dynarr_length (page_fault_table); count; count--)
    if (Dynarr_at (page_fault_table, count - 1) == &p[666])
      fprintf (stderr, "VALID page fault at %x\n",
	       (int) Dynarr_at (page_fault_table, count - 1));
    else
      fprintf (stderr, "WRONG page fault at %x\n",
	       (int) Dynarr_at (page_fault_table, count - 1));
  Dynarr_free (page_fault_table);
  mc_free (p);
  return Qnil;
}

DEFUN ("test-segfault", Ftest_segfault, 0, 0, "", /*
Test virtual dirty bit implementation: provoke a segfault on purpose.
WARNING: this function causes a SEGFAULT on purpose and thus crashes
XEmacs!  This is only used for debbugging, e.g. for testing how the
debugger behaves when XEmacs segfaults and the write barrier is
enabled.
*/
       ())
{
  Rawbyte *q = 0;
  q[0] = 23;
  return Qnil;
}

void
syms_of_vdb (void)
{
  DEFSUBR (Ftest_vdb);
  DEFSUBR (Ftest_segfault);
}
