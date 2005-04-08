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


/* This is moved here from alloc.c. */
#ifndef MALLOC_OVERHEAD
# ifdef GNU_MALLOC
#  define MALLOC_OVERHEAD 0
# elif defined (rcheck)
#  define MALLOC_OVERHEAD 20
# else
#  define MALLOC_OVERHEAD 8
# endif
#endif /* MALLOC_OVERHEAD */


/* This enables type based information (updated during gc). The output
   is used by show-memory-usage to print memory information for each
   type. Since the new allocator does not distinguish between types
   anymore, this functionality is additionally implemented and
   consumes a lot of time.  That is why it is kept conditioned on a
   separate flag called MC_ALLOC_TYPE_STATS. */
#if 1 
# define MC_ALLOC_TYPE_STATS 1 
#endif


/*--- prototypes -------------------------------------------------------*/

BEGIN_C_DECLS



/* Allocation related functions and macros: */

/* Builds and initializes all needed datastructures of the new allocator. */
void init_mc_allocator (void);

/* Returns a pointer to a block of memory of given size on the used heap. */
void *mc_alloc (size_t size);

/* Frees the object pointed to by pointer. */
void mc_free (void *ptr);

/* Modifies the size of the memory block pointed to by ptr. The
   Address of the new block of given size is returned. */
void *mc_realloc (void *ptr, size_t size); 



/* Garbage collection related functions and macros: */

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

/* The finalizer of every not marked object is called. The macro
   MC_ALLOC_CALL_FINALIZER has to be defined and call the finalizer of
   the object. */
void mc_finalize (void);

/* All not marked objects of the used heap are freed. */
void mc_sweep (void);



/* Portable dumper related functions and macros: */

/* The finalizer for disksave of every object is called to shrink the
   dump image. The macro MC_ALLOC_CALL_FINALIZER_FOR_DISKSAVE has to
   be defined and call the finalizer for disksave of the object. */
void mc_finalize_for_disksave (void);



/* Allocation function for the unmanaged heap: */

/* Returns a pointer to a block of memory of given size on the
   unmanaged heap. */
void *mc_alloc_unmanaged (size_t size);

/* Modifies the size of the memory block pointed to by ptr. The
   Address of the new block of given size is returned. */
void *mc_realloc_unmanaged (void *ptr, size_t size);



/* Functions and macros related with allocation statistics: */

#ifdef MEMORY_USAGE_STATS
/* Returns the real size, including overhead, which is actually alloced
   for an object with given claimed_size. */
Bytecount mc_alloced_storage_size (Bytecount claimed_size,
				   struct overhead_stats *stats);
#endif /* MEMORY_USAGE_STATS */

END_C_DECLS

#endif /* INCLUDED_mc_alloc_h_ */
