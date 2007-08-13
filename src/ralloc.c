/* Block-relocating memory allocator. 
   Copyright (C) 1992, 1993, 1994, 1995 Free Software Foundation, Inc.

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
along with GNU Emacs; see the file COPYING.  If not, write to
the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  */

/* NOTES:

   Only relocate the blocs necessary for SIZE in r_alloc_sbrk,
   rather than all of them.  This means allowing for a possible
   hole between the first bloc and the end of malloc storage. */

#ifdef emacs

#include <config.h>
#include "lisp.h"		/* Needed for VALBITS.  */

#undef NULL

/* The important properties of this type are that 1) it's a pointer, and
   2) arithmetic on it should work as if the size of the object pointed
   to has a size of 1.  */
#if 0 /* Arithmetic on void* is a GCC extension.  */
#ifdef __STDC__
typedef void *POINTER;
#else
typedef unsigned char *POINTER;
#endif
#endif /* 0 */

/* Unconditionally use unsigned char * for this.  */
typedef unsigned char *POINTER;

typedef unsigned long SIZE;

#include "getpagesize.h"

#include <string.h>

#else	/* Not emacs.  */

#include <stddef.h>

typedef size_t SIZE;
typedef void *POINTER;

#include <unistd.h>
#include <malloc.h>
#include <string.h>

#endif	/* emacs.  */

#define safe_bcopy(x, y, z) memmove (y, x, z)

#define NIL ((POINTER) 0)


#ifndef HAVE_MMAP

/* A flag to indicate whether we have initialized ralloc yet.  For
   Emacs's sake, please do not make this local to malloc_init; on some
   machines, the dumping procedure makes all static variables
   read-only.  On these machines, the word static is #defined to be
   the empty string, meaning that r_alloc_initialized becomes an
   automatic variable, and loses its value each time Emacs is started up.  */
static int r_alloc_initialized = 0;


/* Declarations for working with the malloc, ralloc, and system breaks.  */

/* Function to set the real break value. */
static POINTER (*real_morecore) ();

/* The break value, as seen by malloc (). */
static POINTER virtual_break_value;

/* The break value, viewed by the relocatable blocs. */
static POINTER break_value;

/* The REAL (i.e., page aligned) break value of the process. */
static POINTER page_break_value;

/* This is the size of a page.  We round memory requests to this boundary.  */
static int page_size;

/* Whenever we get memory from the system, get this many extra bytes.  This 
   must be a multiple of page_size.  */
static int extra_bytes;

/* Macros for rounding.  Note that rounding to any value is possible
   by changing the definition of PAGE. */
#define PAGE (getpagesize ())
#define ALIGNED(addr) (((unsigned long int) (addr) & (page_size - 1)) == 0)
#define ROUNDUP(size) (((unsigned long int) (size) + page_size - 1) \
                       & ~(page_size - 1))
#define ROUND_TO_PAGE(addr) (addr & (~(page_size - 1)))

/* Functions to get and return memory from the system.  */

/* Obtain SIZE bytes of space.  If enough space is not presently available
   in our process reserve, (i.e., (page_break_value - break_value)),
   this means getting more page-aligned space from the system.

   Return non-zero if all went well, or zero if we couldn't allocate
   the memory.  */
static int
obtain (SIZE size)
{
  SIZE already_available = page_break_value - break_value;

  if (already_available < size)
    {
      SIZE get = ROUNDUP (size - already_available);
      /* Get some extra, so we can come here less often.  */
      get += extra_bytes;

      if ((*real_morecore) (get) == 0)
	return 0;

      page_break_value += get;
    }

  break_value += size;

  return 1;
}

/* Obtain SIZE bytes of space and return a pointer to the new area.
   If we could not allocate the space, return zero.  */

static POINTER
get_more_space (SIZE size)
{
  POINTER ptr = break_value;
  if (obtain (size))
    return ptr;
  else
    return 0;
}

/* Note that SIZE bytes of space have been relinquished by the process.
   If SIZE is more than a page, return the space to the system. */

static void
relinquish (SIZE size)
{
  POINTER new_page_break;
  int excess;

  break_value -= size;
  new_page_break = (POINTER) ROUNDUP (break_value);
  excess = (char *) page_break_value - (char *) new_page_break;
  
  if (excess > extra_bytes * 2)
    {
      /* Keep extra_bytes worth of empty space.
	 And don't free anything unless we can free at least extra_bytes.  */
      if ((*real_morecore) (extra_bytes - excess) == 0)
	abort ();

      page_break_value += extra_bytes - excess;
    }

  /* Zero the space from the end of the "official" break to the actual
     break, so that bugs show up faster.  */
  memset (break_value, 0, ((char *) page_break_value - (char *) break_value));
}

/* The meat - allocating, freeing, and relocating blocs.  */

/* These structures are allocated in the malloc arena.
   The linked list is kept in order of increasing '.data' members.
   The data blocks abut each other; if b->next is non-nil, then
   b->data + b->size == b->next->data.  */
typedef struct bp
{
  struct bp *next;
  struct bp *prev;
  POINTER *variable;
  POINTER data;
  SIZE size;
} *bloc_ptr;

#define NIL_BLOC ((bloc_ptr) 0)
#define BLOC_PTR_SIZE (sizeof (struct bp))

/* Head and tail of the list of relocatable blocs. */
static bloc_ptr first_bloc, last_bloc;

/* Find the bloc referenced by the address in PTR.  Returns a pointer
   to that block. */

static bloc_ptr
find_bloc (POINTER *ptr)
{
  bloc_ptr p = first_bloc;

  while (p != NIL_BLOC)
    {
      if (p->variable == ptr && p->data == *ptr)
	return p;

      p = p->next;
    }

  return p;
}

/* Allocate a bloc of SIZE bytes and append it to the chain of blocs.
   Returns a pointer to the new bloc, or zero if we couldn't allocate
   memory for the new block.  */

static bloc_ptr
get_bloc (SIZE size)
{
  bloc_ptr new_bloc;

  if (! (new_bloc = (bloc_ptr) malloc (BLOC_PTR_SIZE))
      || ! (new_bloc->data = get_more_space (size)))
    {
      if (new_bloc)
	free (new_bloc);

      return 0;
    }

  new_bloc->size = size;
  new_bloc->next = NIL_BLOC;
  new_bloc->variable = (POINTER *) NIL;

  if (first_bloc)
    {
      new_bloc->prev = last_bloc;
      last_bloc->next = new_bloc;
      last_bloc = new_bloc;
    }
  else
    {
      first_bloc = last_bloc = new_bloc;
      new_bloc->prev = NIL_BLOC;
    }

  return new_bloc;
}

/* Relocate all blocs from BLOC on upward in the list to the zone
   indicated by ADDRESS.  Direction of relocation is determined by
   the position of ADDRESS relative to BLOC->data.

   If BLOC is NIL_BLOC, nothing is done.

   Note that ordering of blocs is not affected by this function. */

static void
relocate_some_blocs (bloc_ptr bloc, POINTER address)
{
  if (bloc != NIL_BLOC)
    {
      SIZE offset = address - bloc->data;
      SIZE data_size = 0;
      bloc_ptr b;
      
      for (b = bloc; b != NIL_BLOC; b = b->next)
	{
	  data_size += b->size;
	  b->data += offset;
	  *b->variable = b->data;
	}

      memmove (address, address - offset, data_size);
    }
}

/* Free BLOC from the chain of blocs, relocating any blocs above it
   and returning BLOC->size bytes to the free area. */

static void
free_bloc (bloc_ptr bloc)
{
  if (bloc == first_bloc && bloc == last_bloc)
    {
      first_bloc = last_bloc = NIL_BLOC;
    }
  else if (bloc == last_bloc)
    {
      last_bloc = bloc->prev;
      last_bloc->next = NIL_BLOC;
    }
  else if (bloc == first_bloc)
    {
      first_bloc = bloc->next;
      first_bloc->prev = NIL_BLOC;
    }
  else
    {
      bloc->next->prev = bloc->prev;
      bloc->prev->next = bloc->next;
    }

  relocate_some_blocs (bloc->next, bloc->data);
  relinquish (bloc->size);
  free (bloc);
}

/* Interface routines.  */

static int use_relocatable_buffers;

/* Obtain SIZE bytes of storage from the free pool, or the system, as
   necessary.  If relocatable blocs are in use, this means relocating
   them.  This function gets plugged into the GNU malloc's __morecore
   hook.

   We provide hysteresis, never relocating by less than extra_bytes.

   If we're out of memory, we should return zero, to imitate the other
   __morecore hook values - in particular, __default_morecore in the
   GNU malloc package.  */

POINTER 
r_alloc_sbrk (long size)
{
  /* This is the first address not currently available for the heap.  */
  POINTER top;
  /* Amount of empty space below that.  */
  /* It is not correct to use SIZE here, because that is usually unsigned.
     ptrdiff_t would be okay, but is not always available.
     `long' will work in all cases, in practice.  */
  long already_available;
  POINTER ptr;

  if (! use_relocatable_buffers)
    return (*real_morecore) (size);

  top = first_bloc ? first_bloc->data : page_break_value;
  already_available = (char *) top - (char *) virtual_break_value;

  /* Do we not have enough gap already?  */
  if (size > 0 && already_available < size)
    {
      /* Get what we need, plus some extra so we can come here less often.  */
      SIZE get = size - already_available + extra_bytes;

      if (! obtain (get))
	return 0;

      if (first_bloc)
        relocate_some_blocs (first_bloc, first_bloc->data + get);

      /* Zero out the space we just allocated, to help catch bugs
         quickly.  */
      memset (virtual_break_value, 0, get);
    }
  /* Can we keep extra_bytes of gap while freeing at least extra_bytes?  */
  else if (size < 0 && already_available - size > 2 * extra_bytes)
    {
      /* Ok, do so.  This is how many to free.  */
      SIZE give_back = already_available - size - extra_bytes;

      if (first_bloc)
	relocate_some_blocs (first_bloc, first_bloc->data - give_back);
      relinquish (give_back);
    }

  ptr = virtual_break_value;
  virtual_break_value += size;

  return ptr;
}

/* Allocate a relocatable bloc of storage of size SIZE.  A pointer to
   the data is returned in *PTR.  PTR is thus the address of some variable
   which will use the data area.

   If we can't allocate the necessary memory, set *PTR to zero, and
   return zero.  */

POINTER
r_alloc (POINTER *ptr, SIZE size)
{
  bloc_ptr new_bloc;

  if (! r_alloc_initialized)
    init_ralloc ();

  new_bloc = get_bloc (size);
  if (new_bloc)
    {
      new_bloc->variable = ptr;
      *ptr = new_bloc->data;
    }
  else
    *ptr = 0;

  return *ptr;
}

/* Free a bloc of relocatable storage whose data is pointed to by PTR.
   Store 0 in *PTR to show there's no block allocated.  */

void
r_alloc_free (POINTER *ptr)
{
  bloc_ptr dead_bloc;

  dead_bloc = find_bloc (ptr);
  if (dead_bloc == NIL_BLOC)
    abort ();

  free_bloc (dead_bloc);
  *ptr = 0;
}

/* Given a pointer at address PTR to relocatable data, resize it to SIZE.
   Do this by shifting all blocks above this one up in memory, unless
   SIZE is less than or equal to the current bloc size, in which case
   do nothing.

   Change *PTR to reflect the new bloc, and return this value.

   If more memory cannot be allocated, then leave *PTR unchanged, and
   return zero.  */

POINTER
r_re_alloc (POINTER *ptr, SIZE size)
{
  bloc_ptr bloc;

  bloc = find_bloc (ptr);
  if (bloc == NIL_BLOC)
    abort ();

  if (size <= bloc->size)
    /* Wouldn't it be useful to actually resize the bloc here?  */
    return *ptr;

  if (! obtain (size - bloc->size))
    return 0;

  relocate_some_blocs (bloc->next, bloc->data + size);

  /* Zero out the new space in the bloc, to help catch bugs faster.  */
  memset (bloc->data + bloc->size, 0, size - bloc->size);

  /* Indicate that this block has a new size.  */
  bloc->size = size;

  return *ptr;
}

/* The hook `malloc' uses for the function which gets more space
   from the system.  */
extern POINTER (*__morecore) ();

/* Initialize various things for memory allocation. */

void
init_ralloc (void)
{
  if (r_alloc_initialized)
    return;

  r_alloc_initialized = 1;
  real_morecore = __morecore;
  __morecore = r_alloc_sbrk;

  virtual_break_value = break_value = (*real_morecore) (0);
  if (break_value == NIL)
    abort ();

  page_size = PAGE;
  extra_bytes = ROUNDUP (50000);

  page_break_value = (POINTER) ROUNDUP (break_value);

  /* From eirik@elf.IThaca.ny.US (Eirik Fuller):
     The extra call to real_morecore guarantees that the end of the
     address space is a multiple of page_size, even if page_size is
     not really the page size of the system running the binary in
     which page_size is stored.  This allows a binary to be built on a
     system with one page size and run on a system with a smaller page
     size.   (Such as compiling on a Sun 4/260 4.1.3 and running on a
     Sun 4/65 4.1.3: 8k pages at compile time, 4k pages at run time.)
   */
  (*real_morecore) (page_break_value - break_value);

  /* Clear the rest of the last page; this memory is in our address space
     even though it is after the sbrk value.  */

  /* Doubly true, with the additional call that explicitly adds the
     rest of that page to the address space.  */
  memset (break_value, 0, (page_break_value - break_value));
  /* Also from eirik@elf.IThaca.ny.US */
  virtual_break_value = break_value = page_break_value;
  use_relocatable_buffers = 1;
}
#else /* HAVE_MMAP */

/* 
   A relocating allocator built using the mmap(2) facility available
   in some OSes.  Based on another version written by Paul Flinders,
   from which code (and comments) are snarfed.

   The OS should support mmap() with MAP_ANONYMOUS attribute, or have
   /dev/zero.  It should support private memory mapping.

   Paul Flinders wrote a version which works well for systems that
   allow callers to specify (virtual) addresses to mmap().
   Unfortunately, such a scheme doesn't work for certain systems like
   HP-UX that have a system-wide virtual->real address map, and
   consequently impose restrictions on the virtual address values
   permitted.  

   NB: The mapping scheme in HP-UX is motivated by the inverted page
   table design in some HP processors.

   This alternate implementation allows for the addresses to be
   optionally chosen by the system.  Fortunately, buffer allocation
   doesn't insist upon contiguous memory which Flinders' scheme
   provides, and this one doesn't.

   We don't really provide for hysteresis here, but add some metering
   to monitor how poorly the allocator actually works.  See the
   documentation for `mmap-hysteresis'.

   This implementation actually cycles through the blocks allocated
   via mmap() and only sends it to free() if it wasn't one of them.
   Unfortunately, this is O(n) in the number of mmapped blocks.  (Not
   really, as we have a hash table which tries to reduce the cost.)
   Also, this dereferences the pointer passed, so it would cause a
   segfault if garbage was passed to it.  */

#include <fcntl.h>
#include <sys/mman.h>
#include <stdio.h>

typedef void *VM_ADDR;		/* VM addresses */
static CONST VM_ADDR VM_FAILURE_ADDR = (VM_ADDR) -1; /* mmap returns this when it fails. */

/* Configuration for relocating allocator. */

/* #define MMAP_GENERATE_ADDRESSES */
/* Define this if you want Emacs to manage the address table.
   It is not recommended unless you have major problems with the
   default scheme, which allows the OS to pick addresses. */

/* USELESS_LOWER_ADDRESS_BITS defines the number of bits which can be
   discarded while computing the hash, as they're always zero.  The
   default is appropriate for a page size of 4096 bytes. */

#define USELESS_LOWER_ADDRESS_BITS 12


/* Size of hash table for inverted VM_ADDR->MMAP_HANDLE lookup */

#define MHASH_PRIME 89


/* Whether we want to enable metering of some ralloc performance.
   This incurs a constant penalty for each mmap operation. */

#define MMAP_METERING


/* Rename the following to protect against a some smartness elsewhere.
   We need access to the allocator used for non-mmap allocation
   elsewhere, in case we get passed a handle that we didn't allocate
   ourselves.  Currently, this default allocator is also used to
   maintain local structures for relocatable blocks. */

#define UNDERLYING_MALLOC   malloc
#define UNDERLYING_FREE     free
#define UNDERLYING_REALLOC  realloc

/* MAP_ADDRCHOICE_FLAG is set to MAP_FIXED if MMAP_GENERATE_ADDRESSES
   is defined, and MAP_VARIABLE otherwise.  Some losing systems don't
   define the _FIXED/_VARIABLE flags, in which case it is set to 0 */

#ifdef MMAP_GENERATE_ADDRESSES
# ifdef MAP_FIXED
#    define MAP_ADDRCHOICE_FLAG MAP_FIXED
# endif
#else /* !MMAP_GENERATE_ADDRESSES */
# ifdef MAP_VARIABLE
#    define MAP_ADDRCHOICE_FLAG MAP_VARIABLE
# endif
#endif /* MMAP_GENERATE_ADDRESSES */

/* Default case. */
#ifndef MAP_ADDRCHOICE_FLAG
#  define MAP_ADDRCHOICE_FLAG 0
#endif /* MAP_ADDRCHOICE_FLAG */

#ifdef MAP_ANONYMOUS
#  define MAP_FLAGS (MAP_PRIVATE | MAP_ADDRCHOICE_FLAG | MAP_ANONYMOUS)
#else
#  define MAP_FLAGS (MAP_PRIVATE | MAP_ADDRCHOICE_FLAG)
#endif /* MAP_ANONYMOUS */


/* (ptf): A flag to indicate whether we have initialized ralloc yet.  For
   Emacs's sake, please do not make this local to malloc_init; on some
   machines, the dumping procedure makes all static variables
   read-only.  On these machines, the word static is #defined to be
   the empty string, meaning that r_alloc_initialized becomes an
   automatic variable, and loses its value each time Emacs is started up.

   If we're using mmap this flag has three possible values
   0 - initial value
   1 - Normal value when running temacs. In this case buffers
       are allocated using malloc so that any data that they
       contain becomes part of the undumped executable.
   2 - Normal value when running emacs */
static int r_alloc_initialized = 0;

/* (ptf): Macros for rounding.  Note that rounding to any value is possible
   by changing the definition of PAGE. */
#define PAGE (getpagesize ())
#define PAGES_FOR(size) (((unsigned long int) (size) + page_size - 1)/page_size)
#define ROUNDUP(size) ((unsigned long int)PAGES_FOR(size)*page_size)


/* DEV_ZERO_FD is -1 normally, but for systems without MAP_ANONYMOUS
   points to a file descriptor opened on /dev/zero */

static int DEV_ZERO_FD = -1;


/* We actually need a datastructure that can be usefully structured
   based on the VM address, and allows an ~O(1) lookup on an arbitrary
   address, ie a hash-table.  Maybe the XEmacs hash table can be
   coaxed enough.  At the moment, we use lookup on a hash-table to
   decide whether to do an O(n) search on the malloced block list.
   Addresses are hashed to a bucket modulo MHASH_PRIME */


/* We settle for a standard doubly-linked-list.  The dynarr type isn't
   very amenable to deletion of items in the middle, so we conjure up
   yet another stupid datastructure.  The structure is maintained as a
   ring, and the singleton ring has the sole element as it's left and
   right neighbours. */

static void init_MHASH_table (void); /* Forward reference */

typedef struct alloc_dll
{
  size_t size;			/* #bytes currently in use */
  size_t space_for;		/* #bytes we really have */
  POINTER* aliased_address;	/* Address of aliased variable, to tweak if relocating */
  VM_ADDR vm_addr;		/* VM address returned by mmap */
  struct alloc_dll *left;	/* Left link in circular doubly linked list */
  struct alloc_dll *right;
} *MMAP_HANDLE;

static MMAP_HANDLE mmap_start = 0; /* Head of linked list */
static size_t page_size = 0;	/* Size of VM pages */
static int mmap_hysteresis;	/* Should be size_t, really. */

/* Get a new handle for a fresh block. */
static MMAP_HANDLE
new_mmap_handle (size_t nsiz)
{
  MMAP_HANDLE h = (MMAP_HANDLE) UNDERLYING_MALLOC( sizeof (struct alloc_dll));
  if ( h == 0) return 0;
  h->size = nsiz;
  if (mmap_start == 0)
    {
      init_MHASH_table ();
      mmap_start = h; mmap_start->left = h; mmap_start->right = h;
    }
  {
    MMAP_HANDLE prev = mmap_start->left;
    MMAP_HANDLE nex = mmap_start;

    /* Four pointers need fixing. */
    h->right = nex;
    h->left = prev;
    prev->right = h;
    nex->left = h;
  }
  return h;
}

/* Find a handle given the aliased address using linear search. */
static MMAP_HANDLE
find_mmap_handle_lsearch (POINTER *alias)
{
  MMAP_HANDLE h = mmap_start;
  if (h == 0) return 0;
  do {
    if (h->aliased_address == alias && *alias == h->vm_addr)
      return h;
    h = h->right;
  } while( h != mmap_start );
  return 0;			/* Bogus alias passed. */
}

/* Free a handle. */
static void
free_mmap_handle (MMAP_HANDLE h)
{
  MMAP_HANDLE prev = h->left;
  MMAP_HANDLE nex = h->right;
  if (prev == h || nex == h)	/* In fact, this should be && */
    {				/* We're the singleton dll */
      UNDERLYING_FREE( h );		/* Free the sole item */
      mmap_start = 0; return;
    }
  else if (h == mmap_start) 
    {
      mmap_start = nex;		/* Make sure mmap_start isn't bogus. */
    }
  prev->right = nex;
  nex->left = prev;
  UNDERLYING_FREE( h );
}

/* A simple hash table to speed up the inverted lookup of
   VM_ADDR->MMAP_HANDLE. We maintain the number of hits for a
   particular bucket.  We invalidate a hash table entry during block
   deletion if the hash has cached the deleted block's address. */

/* Simple hash check. */
struct {
  int n_hits;			/* How many addresses map to this? */
  MMAP_HANDLE handle;		/* What is the current handle? */
  VM_ADDR addr;			/* What is it's VM address? */
} MHASH_HITS[ MHASH_PRIME ];

static void
init_MHASH_table (void)
{
  int i = 0;
  for (; i < MHASH_PRIME; i++) 
    {
      MHASH_HITS[i].n_hits = 0;
      MHASH_HITS[i].addr = 0;
      MHASH_HITS[i].handle = 0;
    }
}

/* Compute the hash value for an address. */
static int
MHASH (VM_ADDR addr)
{
#if (LONGBITS == 64)
  unsigned long int addr_shift = (unsigned long int)(addr) >> USELESS_LOWER_ADDRESS_BITS;
#else
  unsigned int addr_shift = (unsigned int)(addr) >> USELESS_LOWER_ADDRESS_BITS;
#endif
  int hval = addr_shift % MHASH_PRIME; /* We could have addresses which are -ve 
					  when converted to signed ints */
  return ((hval >= 0) ? hval : MHASH_PRIME + hval);
}

/* Add a VM address with it's corresponding handle to the table. */
static void
MHASH_ADD (VM_ADDR addr, MMAP_HANDLE h)
{
  int kVal = MHASH( addr );
  if (MHASH_HITS[kVal].n_hits++ == 0)
    { /* Only overwrite the table if there were no hits so far. */
      MHASH_HITS[kVal].addr = addr;
      MHASH_HITS[kVal].handle = h;
    }
}

/* Delete a VM address entry from the hash table. */
static void
MHASH_DEL (VM_ADDR addr)
{
  int kVal = MHASH( addr );
  MHASH_HITS[kVal].n_hits--;
  if (addr == MHASH_HITS[kVal].addr)
    {
      MHASH_HITS[kVal].addr = 0; /* Invalidate cache. */
      MHASH_HITS[kVal].handle = 0;
    }
}

/* End of hash buckets */

/* Metering malloc performance. */
#ifdef MMAP_METERING
/* If we're metering, we introduce some extra symbols to aid the noble
   cause of bloating XEmacs core size. */

Lisp_Object Qmm_times_mapped;
Lisp_Object Qmm_pages_mapped;
Lisp_Object Qmm_times_unmapped;
Lisp_Object Qmm_times_remapped;
Lisp_Object Qmm_didnt_copy;
Lisp_Object Qmm_pages_copied;
Lisp_Object Qmm_average_bumpval;
Lisp_Object Qmm_wastage;
Lisp_Object Qmm_live_pages;
Lisp_Object Qmm_addr_looked_up;
Lisp_Object Qmm_hash_worked;
Lisp_Object Qmm_addrlist_size;

#define M_Map 0			/* How many times allocated? */
#define M_Pages_Map 1		/* How many pages allocated? */
#define M_Unmap 2		/* How many times freed? */
#define M_Remap 3		/* How many times increased in size? */
#define M_Didnt_Copy 4		/* How many times didn't need to copy? */
#define M_Copy_Pages 5		/* Total # pages copied */
#define M_Average_Bumpval 6	/* Average bump value */
#define M_Wastage 7		/* Remaining (unused space) */
#define M_Live_Pages 8		/* #live pages */
#define M_Address_Lookup 9	/* How many times did we need to check if an addr is in the block? */
#define M_Hash_Worked   10      /* How many times did the simple hash check work? */
#define M_Addrlist_Size 11	/* What is the size of the XEmacs memory map? */

#define N_Meterables 12		/* Total number of meterables */
#define MEMMETER(x) {x;}
#define MVAL(x) (meter[x])
#define MLVAL(x) (make_int (meter[x]))
static int meter[N_Meterables];

DEFUN ("mmap-allocator-status", Fmmap_allocator_status, 0, 0, 0, /*
Return some information about mmap-based allocator.

  mmap-addrlist-size: number of entries in address picking list. 
  mmap-times-mapped: number of times r_alloc was called. 
  mmap-pages-mapped: number of pages mapped by r_alloc calls only. 
  mmap-times-unmapped: number of times r_free was called. 
  mmap-times-remapped: number of times r_re_alloc was called.
  mmap-didnt-copy: number of times  re-alloc didn\'t have to move the block.
  mmap-pages-copied: total number of pages copied.
  mmap-average-bumpval: average increase in size demanded to re-alloc.
  mmap-wastage: total number of bytes allocated, but not currently in use.
  mmap-live-pages: total number of pages live.
*/
       ())
{
  Lisp_Object result;

  result = Fcons (Fcons (Qmm_addrlist_size, MLVAL (M_Addrlist_Size)), Qnil);
  result = Fcons (Fcons (Qmm_hash_worked, MLVAL (M_Hash_Worked)), result);
  result = Fcons (Fcons (Qmm_addr_looked_up, MLVAL (M_Address_Lookup)), result);
  result = Fcons (Fcons (Qmm_live_pages, MLVAL (M_Live_Pages)), result);
  result = Fcons (Fcons (Qmm_wastage, MLVAL (M_Wastage)), result);
  result = Fcons (Fcons (Qmm_average_bumpval, MLVAL (M_Average_Bumpval)),
		  result);
  result = Fcons (Fcons (Qmm_pages_copied, MLVAL (M_Copy_Pages)), result);
  result = Fcons (Fcons (Qmm_didnt_copy, MLVAL (M_Didnt_Copy)), result);
  result = Fcons (Fcons (Qmm_times_remapped, MLVAL (M_Remap)), result);
  result = Fcons (Fcons (Qmm_times_unmapped, MLVAL (M_Unmap)), result);
  result = Fcons (Fcons (Qmm_pages_mapped, MLVAL (M_Pages_Map)), result);
  result = Fcons (Fcons (Qmm_times_mapped, MLVAL (M_Map)), result);

  return result;
}

#else /* !MMAP_METERING */

#define MEMMETER(x) 
#define MVAL(x)

#endif /* MMAP_METERING */

static MMAP_HANDLE
find_mmap_handle (POINTER *alias)
{
  int kval  = MHASH( *alias );
  MEMMETER( MVAL(M_Address_Lookup)++ )
  switch( MHASH_HITS[kval].n_hits)
    {
    case 0:
      MEMMETER( MVAL( M_Hash_Worked )++ )
      return 0;

    case 1:
      if (*alias == MHASH_HITS[kval].addr) 
	{ 
	  MEMMETER( MVAL( M_Hash_Worked) ++ );
	  return MHASH_HITS[kval].handle;
	}
      /* FALL THROUGH */
    default:
      return find_mmap_handle_lsearch( alias );
    } /* switch */
}

/* 
   Some kernels don't like being asked to pick addresses for mapping
   themselves---IRIX is known to become extremely slow if mmap is
   passed a ZERO as the first argument.  In such cases, we use an
   address map which is managed local to the XEmacs process.  The
   address map maintains an ordered linked list of (address, size,
   occupancy) triples ordered by the absolute address.  Initially, a
   large address area is marked as being empty.  The address picking
   scheme takes bites off the first block which is still empty and
   large enough.  If mmap with the specified address fails, it is
   marked unavailable and not attempted thereafter.  The scheme will
   keep fragmenting the large empty block until it finds an address
   which can be successfully mmapped, or until there are no free
   blocks of the given size left.

   Note that this scheme, given it's first-fit strategy, is prone to
   fragmentation of the first part of memory earmarked for this
   purpose. [ACP Vol I].  We can't use the workaround of using a
   randomized first fit because we don't want to presume too much
   about the memory map.  Instead, we try to coalesce empty or
   unavailable blocks at any available opportunity.  */

/* Initialization procedure for address picking scheme */
static void Addr_Block_initialize(void);

/* Get a suitable VM_ADDR via mmap */
static VM_ADDR New_Addr_Block( SIZE sz );

/* Free a VM_ADDR allocated via New_Addr_Block */
static void Free_Addr_Block( VM_ADDR addr, SIZE sz );

#ifdef MMAP_GENERATE_ADDRESSES
/* Implementation of the three calls for address picking when XEmacs is incharge */

/* The enum denotes the status of the following block. */
typedef enum { empty = 0, occupied, unavailable } addr_status;

typedef struct addr_chain
{
  POINTER addr;
  SIZE sz;
  addr_status flag;
  struct addr_chain *next;
} ADDRESS_BLOCK, *ADDRESS_CHAIN;
/* NB: empty and unavailable blocks are concatenated. */

static ADDRESS_CHAIN addr_chain = 0;
/* Start off the address block chain with a humongous address block
   which is empty to start with.  Note that addr_chain is invariant
   WRT the addition/deletion of address blocks because of the assert
   in Coalesce() and the strict ordering of blocks by their address 
   */
static void Addr_Block_initialize()
{
  MEMMETER( MVAL( M_Addrlist_Size )++)
  addr_chain = (ADDRESS_CHAIN) UNDERLYING_MALLOC( sizeof( ADDRESS_BLOCK ));
  addr_chain->next = 0;		/* Last block in chain */
  addr_chain->sz = 0x0c000000;	/* Size */
  addr_chain->addr = (POINTER) (0x04000000 | DATA_SEG_BITS);
  addr_chain->flag = empty;
}

/* Coalesce address blocks if they are contiguous.  Only empty and
   unavailable slots are coalesced. */
static void Coalesce_Addr_Blocks()
{
  ADDRESS_CHAIN p;
  for (p = addr_chain; p; p = p->next)
    {
      while (p->next && p->flag == p->next->flag)
	{
	  ADDRESS_CHAIN np;
	  np = p->next;

	  if (p->flag == occupied) break; /* No cigar */

	  /* Check if the addresses are contiguous. */
	  if (p->addr + p->sz != np->addr) break; 
	  
	  MEMMETER( MVAL( M_Addrlist_Size )--)
	  /* We can coalesce these two. */
	  p->sz += np->sz;
	  p->next = np->next;
	  assert( np != addr_chain ); /* We're not freeing the head of the list. */
	  UNDERLYING_FREE( np );
	}
    } /* for all p */
}

/* Get an empty address block of specified size. */
static VM_ADDR New_Addr_Block( SIZE sz )
{
  ADDRESS_CHAIN p = addr_chain;
  VM_ADDR new_addr = VM_FAILURE_ADDR;
  for (; p; p = p->next)
    {
      if (p->flag == empty && p->sz > sz)
	{
	  /* Create a new entry following p which is empty. */
	  ADDRESS_CHAIN remainder = (ADDRESS_CHAIN) UNDERLYING_MALLOC( sizeof( ADDRESS_BLOCK ) );
	  remainder->next = p->next;
	  remainder->flag = empty;
	  remainder->addr = p->addr + sz;
	  remainder->sz = p->sz - sz;

	  MEMMETER( MVAL( M_Addrlist_Size )++)
	  
	  /* Now make p become an occupied block with the appropriate size */
	  p->next = remainder;
	  p->sz = sz;
	  new_addr = mmap( (VM_ADDR) p->addr, p->sz, PROT_READ|PROT_WRITE,
			   MAP_FLAGS, DEV_ZERO_FD, 0 );
	  if (new_addr == VM_FAILURE_ADDR)
	    {
	      p->flag = unavailable;
	      continue;
	    }
	  p->flag = occupied;
	  break;
	}
    }
  Coalesce_Addr_Blocks();
  return new_addr;
}

/* Free an address block.  We mark the block as being empty, and attempt to
   do any coalescing that may have resulted from this. */
static void Free_Addr_Block( VM_ADDR addr, SIZE sz )
{
  ADDRESS_CHAIN p = addr_chain;
  for (; p; p = p->next )
    {
      if (p->addr == addr)
	{
	  if (p->sz != sz) abort(); /* ACK! Shouldn't happen at all. */
	  munmap( (VM_ADDR) p->addr, p->sz );
	  p->flag = empty;
	  break;
	}
    }
  if (!p) abort(); /* Can't happen... we've got a block to free which is not in 
		      the address list. */
  Coalesce_Addr_Blocks();
}
#else /* !MMAP_GENERATE_ADDRESSES */
/* This is an alternate (simpler) implementation in cases where the
   address is picked by the kernel. */

static void Addr_Block_initialize(void)
{
  /* Nothing. */
}

static VM_ADDR New_Addr_Block( SIZE sz )
{
  return mmap( 0, sz, PROT_READ|PROT_WRITE, MAP_FLAGS,
	       DEV_ZERO_FD, 0 );
}

static void Free_Addr_Block( VM_ADDR addr, SIZE sz )
{
  munmap( addr, sz );
}

#endif /* MMAP_GENERATE_ADDRESSES */


/* IMPLEMENTATION OF EXPORTED RELOCATOR INTERFACE */

/*
 r_alloc( POINTER, SIZE ): Allocate a relocatable area with the start
 address aliased to the first parameter. 
 */

POINTER r_alloc (POINTER *ptr, SIZE size);
POINTER
r_alloc (POINTER *ptr, SIZE size)
{
  MMAP_HANDLE mh;
  
  switch(r_alloc_initialized)
    {
    case 0:
      abort();
    case 1:
      *ptr = (POINTER) UNDERLYING_MALLOC(size);
      break;
    default:
      mh = new_mmap_handle( size );
      if (mh)
	{
	  SIZE hysteresis = (mmap_hysteresis > 0 ?  mmap_hysteresis  : 0);
	  SIZE  mmapped_size = ROUNDUP( size + hysteresis );
	  MEMMETER( MVAL(M_Map)++ )
	  MEMMETER( MVAL(M_Pages_Map) += (mmapped_size/page_size) )
	  MEMMETER( MVAL(M_Wastage) += mmapped_size - size )
          MEMMETER( MVAL(M_Live_Pages) += (mmapped_size/page_size) )
	  mh->vm_addr = New_Addr_Block( mmapped_size );
	  if (mh->vm_addr == VM_FAILURE_ADDR) {
	    free_mmap_handle( mh ); /* Free the loser */
	    *ptr = 0;
	    return 0;		/* ralloc failed due to mmap() failure. */
	  }
	  MHASH_ADD( mh->vm_addr, mh );
	  mh->space_for = mmapped_size;
	  mh->aliased_address = ptr;
	  *ptr = mh->vm_addr;
	}
      else
	*ptr = 0;		/* Malloc of block failed */
      break;
    }
  return *ptr;
}

/* Free a bloc of relocatable storage whose data is pointed to by PTR.
   Store 0 in *PTR to show there's no block allocated.  */

void r_alloc_free (POINTER *ptr);
void
r_alloc_free (POINTER *ptr)
{
  switch( r_alloc_initialized) {
    case 0:
      abort();

    case 1:
      UNDERLYING_FREE( *ptr );		/* Certain this is from the heap. */
      break;

    default:
      {
	MMAP_HANDLE dead_handle = find_mmap_handle( ptr );
	/* Check if we've got it. */
	if (dead_handle == 0)	/* Didn't find it in the list of mmap handles */
	  {
	    UNDERLYING_FREE( *ptr );
	  }
	else
	  {
	    MEMMETER( MVAL( M_Wastage ) -= (dead_handle->space_for - dead_handle->size) )
	    MEMMETER( MVAL( M_Live_Pages ) -= (dead_handle->space_for / page_size ))
	    MEMMETER(MVAL(M_Unmap)++)
	    MHASH_DEL( dead_handle->vm_addr );
	    Free_Addr_Block( dead_handle->vm_addr, dead_handle->space_for );
	    free_mmap_handle (dead_handle);
	  }
      }
      break;
    } /* r_alloc_initialized */
  *ptr = 0;			/* Zap the pointer's contents. */
}

/* Given a pointer at address PTR to relocatable data, resize it to SIZE.

   Change *PTR to reflect the new bloc, and return this value.

   If more memory cannot be allocated, then leave *PTR unchanged, and
   return zero.  */

POINTER r_re_alloc (POINTER *ptr, SIZE sz);
POINTER
r_re_alloc (POINTER *ptr, SIZE sz)
{
  if (r_alloc_initialized == 0)
    {
      abort ();
      return 0; /* suppress compiler warning */
    }
  else if (r_alloc_initialized == 1)
    {
      POINTER tmp = (POINTER) realloc(*ptr, sz);
      if (tmp)
	*ptr = tmp;
      return tmp;
    }
  else
    {
      SIZE hysteresis = (mmap_hysteresis > 0 ?  mmap_hysteresis : 0);
      SIZE actual_sz = ROUNDUP( sz + hysteresis );
      MMAP_HANDLE h = find_mmap_handle( ptr );
      VM_ADDR new_vm_addr;

      if ( h == 0 )		/* Was allocated using malloc. */
	{
	  POINTER tmp = (POINTER) UNDERLYING_REALLOC(*ptr, sz);
	  if (tmp)
	    *ptr = tmp;
	  return tmp;
	}

      MEMMETER(
	       MVAL(M_Average_Bumpval) =
	       (((double) MVAL(M_Remap) * MVAL(M_Average_Bumpval)) + (sz - h->size))
	       / (double) (MVAL(M_Remap) + 1))
      MEMMETER(MVAL(M_Remap)++)
      if (h->space_for > sz)	/* We've got some more room */
	{			/* Also, if a shrinkage was asked for. */
	  MEMMETER( MVAL(M_Didnt_Copy)++ )
          MEMMETER( MVAL(M_Wastage) -= (sz - h->size))
	  /* We're pretty dumb at handling shrinkage.  We should check for 
	     a larger gap than the standard hysteresis allowable, and if so,
	     shrink the number of pages.  Right now, we simply reset the size
	     component and return. */
	  h->size = sz;
	  return *ptr;
	}
      
      new_vm_addr = New_Addr_Block( actual_sz );
      if (new_vm_addr == VM_FAILURE_ADDR) 
	{/* Failed to realloc. */
          /* *ptr = 0; */
	  return 0;
	}

      MHASH_ADD( new_vm_addr, h );
      /* We got a block OK: now we should move the old contents to the
	 new address.  We use the old size of this block.  */
      memmove(new_vm_addr, h->vm_addr, h->size);
      MHASH_DEL( h->vm_addr );
      Free_Addr_Block( h->vm_addr, h->space_for ); /* Unmap old area. */

      MEMMETER( MVAL( M_Copy_Pages ) += (h->space_for/page_size) )
      MEMMETER( MVAL( M_Live_Pages ) -= (h->space_for / page_size))
      MEMMETER( MVAL( M_Live_Pages ) += (actual_sz / page_size))
      MEMMETER( MVAL( M_Wastage ) -= (h->space_for - h->size))
      MEMMETER( MVAL( M_Wastage ) += (actual_sz - sz) )

      /* Update block datastructure. */
      h->space_for = actual_sz;	/* New total space */
      h->size = sz;		/* New (requested) size */
      h->vm_addr = new_vm_addr;	/* New VM start address */
      h->aliased_address = ptr;	/* Change alias to reflect block relocation. */
      *ptr = h->vm_addr;
      return *ptr;
    }
}


/* Initialize various things for memory allocation.
 */
void
init_ralloc (void)
{
  int i = 0;
  if (r_alloc_initialized > 1)
    return;	/* used to return 1 */

  if (++r_alloc_initialized == 1)
    return;	/* used to return 1 */

  Addr_Block_initialize();	/* Initialize the address picker, if required. */
  page_size = PAGE;
  assert( page_size > 0 );	/* getpagesize() bogosity check. */

#ifndef MAP_ANONYMOUS
  DEV_ZERO_FD = open( "/dev/zero", O_RDWR );
  if (DEV_ZERO_FD < 0)
    /* Failed.  Perhaps we should abort here? */
    return;	/* used to return 0 */
#endif

#ifdef MMAP_METERING
  for(i = 0; i < N_Meterables; i++ )
    {
      meter[i] = 0;
    }
#endif /* MMAP_METERING */
}

void
syms_of_ralloc (void)
{
#ifdef MMAP_METERING
  defsymbol (&Qmm_times_mapped, "mmap-times-mapped");
  defsymbol (&Qmm_pages_mapped, "mmap-pages-mapped");
  defsymbol (&Qmm_times_unmapped, "mmap-times-unmapped");
  defsymbol (&Qmm_times_remapped, "mmap-times-remapped");
  defsymbol (&Qmm_didnt_copy, "mmap-didnt-copy");
  defsymbol (&Qmm_pages_copied, "mmap-pages-copied");
  defsymbol (&Qmm_average_bumpval, "mmap-average-bumpval");
  defsymbol (&Qmm_wastage, "mmap-wastage");
  defsymbol (&Qmm_live_pages, "mmap-live-pages");
  defsymbol (&Qmm_addr_looked_up, "mmap-had-to-look-up-address");
  defsymbol (&Qmm_hash_worked, "mmap-hash-table-worked");
  defsymbol (&Qmm_addrlist_size, "mmap-addrlist-size");
  DEFSUBR (Fmmap_allocator_status);
#endif /* MMAP_METERING */
}

void
vars_of_ralloc (void)
{
  DEFVAR_INT ("mmap-hysteresis", &mmap_hysteresis /*
Extra room left at the end of an allocated arena,
so that a re-alloc requesting extra space smaller than this 
does not actually cause a new arena to be allocated.

A negative value is considered equal to zero.  This is the 
minimum amount of space guaranteed to be left at the end of 
the arena.  Because allocation happens in multiples of the OS
page size, it is possible for more space to be left unused.
*/ );
  mmap_hysteresis = 0;
}

#endif /* HAVE_MMAP */
