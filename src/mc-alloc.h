/* New allocator for XEmacs.
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

#ifndef INCLUDED_mc_alloc_h_
#define INCLUDED_mc_alloc_h_

/*--- prototypes -------------------------------------------------------*/

BEGIN_C_DECLS



/* Internal Allocator Functions: */

/* Initialize the allocator.  This has to be called prior to
   requesting memory. */
void init_mc_allocator (void);

/* Allocate a block of memory of given size and return the pointer to
   it. */
void *mc_alloc (size_t size);

/* Allocate a block of memory as an array with elemcount elements of
   given size and return the pointer to it.  Arrays contain several
   objects that are allocated in one consecutive block of memory with
   each element being a fully qualified object---that is, it has a
   Lisp object header and a mark bit.  Objects like hash tables and
   dynamic arrays use this function. */
void *mc_alloc_array (size_t size, EMACS_INT elemcount);

/* Free the object pointed to by ptr and make its memory re-usable
   again.  The memory must have been returned by a previous call to
   mc_alloc().  This can be used to free memory explicitly, outside a
   garbage collection. */
void mc_free (void *ptr);

/* Modify the size of the memory block pointed to by ptr. Return the
   address of the new block of given size.  The content of the memory
   block will be unchanged to the minimum of the old and new sizes: if
   the new size is smaller, the overlaying data is cut off; if the new
   size is bigger, the newly allocated memory will be uninitialized.*/
void *mc_realloc (void *ptr, size_t size); 

/* Modify the size of the array pointed to by ptr. Return the address
   of the new array block with elemcount elements of given size.  The
   content of the memory block will be unchanged to the minimum of the
   old and new sizes: if the new size is smaller, the overlaying data
   is cut off; if the new size is bigger, the newly allocated memory
   will be uninitialized.*/
void *mc_realloc_array (void *ptr, size_t size, EMACS_INT elemcount);



/* Garbage collection related functions and macros: */

#ifdef NEW_GC
enum mark_bit_colors
{
  WHITE = 0,
  BLACK = 1,
  GREY = 2
};

/* Set the mark bit of the object pointed to by ptr to value.*/
void set_mark_bit (void *ptr, EMACS_INT value);

/* Return the mark bit of the object pointed to by ptr. */
EMACS_INT get_mark_bit (void *ptr);

/* mark bit macros */
/* Returns true if the mark bit of the object pointed to by ptr is set. */
#define MARKED_P(ptr) (get_mark_bit (ptr) != WHITE)

/* Marks the object pointed to by ptr (sets the mark bit to 1). */
#define MARK(ptr)     set_mark_bit (ptr, BLACK)

/* Unmarks the object pointed to by ptr (sets the mark bit to 0). */
#define UNMARK(ptr)   set_mark_bit (ptr, WHITE)

#define MARK_WHITE(ptr) set_mark_bit (ptr, WHITE)
#define MARK_GREY(ptr) set_mark_bit (ptr, GREY)
#define MARK_BLACK(ptr) set_mark_bit (ptr, BLACK)

#define MARKED_WHITE_P(ptr) (get_mark_bit (ptr) == WHITE)
#define MARKED_GREY_P(ptr) (get_mark_bit (ptr) == GREY)
#define MARKED_BLACK_P(ptr) (get_mark_bit (ptr) == BLACK)
#else /* not NEW_GC */
/* Set the mark bit of the object pointed to by ptr to value.*/
void set_mark_bit (void *ptr, EMACS_INT value);

/* Return the mark bit of the object pointed to by ptr. */
EMACS_INT get_mark_bit (void *ptr);

/* mark bit macros */
/* Returns true if the mark bit of the object pointed to by ptr is set. */
#define MARKED_P(ptr) (get_mark_bit (ptr) == 1)

/* Marks the object pointed to by ptr (sets the mark bit to 1). */
#define MARK(ptr)     set_mark_bit (ptr, 1)

/* Unmarks the object pointed to by ptr (sets the mark bit to 0). */
#define UNMARK(ptr)   set_mark_bit (ptr, 0)
#endif /* not NEW_GC */

/* The finalizer of every not marked object is called.  The macro
   MC_ALLOC_CALL_FINALIZER has to be defined and call the finalizer of
   the object. */
void mc_finalize (void);

/* All not marked objects of the used heap are freed. */
void mc_sweep (void);



/* Portable dumper related functions and macros: */

/* The finalizer for disksave of every object is called to shrink the
   dump image.  The macro MC_ALLOC_CALL_FINALIZER_FOR_DISKSAVE has to
   be defined and call the finalizer for disksave of the object. */
void mc_finalize_for_disksave (void);



/* Functions and macros related with allocation statistics: */

#ifdef MEMORY_USAGE_STATS
/* Returns the real size, including overhead, which is actually alloced
   for an object with given claimed_size. */
Bytecount mc_alloced_storage_size (Bytecount claimed_size,
				   struct overhead_stats *stats);
#endif /* MEMORY_USAGE_STATS */


#ifdef NEW_GC
/* Incremental Garbage Collector / Write Barrier Support: */

/* Return the PAGESIZE the allocator uses.  Generally equals to the
   system's PAGESIZE. */
EMACS_INT mc_get_page_size (void);

/* Is the fault at ptr on a protected page? */
EMACS_INT fault_on_protected_page (void *ptr);

/* Remove protection (if there) of heap page of given page header
   ph. */
void protect_heap_pages (void);

/* Remove protection for all heap pages which are protected. */
void unprotect_heap_pages (void);

/* Remove protection and mark page dirty. */
void unprotect_page_and_mark_dirty (void *ptr);

/* Repush all objects on dirty pages onto the mark stack. Return
   number of repushed objects. */
int repush_all_objects_on_page (void *ptr);

/* Mark black if object is currently grey. */
EMACS_INT maybe_mark_black (void *ptr);

/* Only for debugging---not used anywhere in the sources. */
EMACS_INT object_on_heap_p (void *ptr);

#endif /* NEW_GC */

END_C_DECLS

#endif /* INCLUDED_mc_alloc_h_ */
