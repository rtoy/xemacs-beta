/* New size-based allocator for XEmacs.
   Copyright (C) 2005 Marcus Crestani.
   Copyright (C) 2010 Ben Wing.

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

/* 
   The New Allocator

   The ideas and algorithms are based on the allocator of the
   Boehm-Demers-Weiser conservative garbage collector. See
   http://www.hpl.hp.com/personal/Hans_ Boehm/gc/index.html.

   The new allocator is enabled when the new garbage collector
   is enabled (with `--with-newgc').  The implementation of
   the new garbage collector is in gc.c.

   The new allocator takes care of:
   - allocating objects in a write-barrier-friendly way
   - manage object's mark bits 

   Three-Level Allocation

   The new allocator efficiently manages the allocation of Lisp
   objects by minimizing the number of times malloc() and free() are
   called. The allocation process has three layers of abstraction:

   1. It allocates memory in very large chunks called heap sections. 
   
   2. The heap sections are subdivided into pages. The page size is
      determined by the constant PAGE_SIZE. It holds the size of a page
      in bytes.

   3. One page consists of one or more cells. Each cell represents
      a memory location for an object. The cells on one page all have
      the same size, thus every page only contains equal-sized
      objects.

   If an object is bigger than page size, it is allocated on a
   multi-page. Then there is only one cell on a multi-page (the cell
   covers the full multi-page). Is an object smaller than 1/2 PAGE_SIZE,
   a page contains several objects and several cells. There
   is only one cell on a page for object sizes from 1/2 PAGE_SIZE to
   PAGE_SIZE (whereas multi-pages always contain 2 only one
   cell). Only in layer one malloc() and free() are called.


   Size Classes and Page Lists

   Meta-information about every page and multi-page is kept in a page
   header. The page header contains some bookkeeping information like
   number of used and free cells, and pointers to other page
   headers. The page headers are linked in a page list.

   Every page list builds a size class. A size class contains all
   pages (linked via page headers) for objects of the same size. The
   new allocator does not group objects based on their type, it groups
   objects based on their sizes.

   Here is an example: A cons contains a lrecord_header, a car and cdr
   field. Altogether it uses 12 bytes of memory (on 32 bits
   machines). All conses are allocated on pages with a cell size of 12
   bytes. All theses pages are kept together in a page list, which
   represents the size class for 12 bytes objects. But this size class
   is not exclusively for conses only. Other objects, which are also
   12 bytes big (e.g. weak-boxes), are allocated in the same size
   class and on the same pages.

   The number of size classes is customizable, so is the size step
   between successive size classes.


   Used and Unused Heap

   The memory which is managed by the allocator can be divided in two
   logical parts:

   The used heap contains pages, on which objects are allocated. These
   pages are com- pletely or partially occupied. In the used heap, it
   is important to quickly find a free spot for a new
   object. Therefore the size classes of the used heap are defined by
   the size of the cells on the pages. The size classes should match
   common object sizes, to avoid wasting memory.

   The unused heap only contains completely empty pages. They have
   never been used or have been freed completely again. In the unused
   heap, the size of consecutive memory tips the scales. A page is the
   smallest entity which is asked for. Therefore, the size classes of
   the unused heap are defined by the number of consecutive pages.

   The parameters for the different size classes can be adjusted
   independently, see `configurable values' below.


   The Allocator's Data Structures

   The struct `mc_allocator_globals holds' all the data structures
   that the new allocator uses (lists of used and unused pages, mark
   bits, etc.).


   Mapping of Heap Pointers to Page Headers

   For caching benefits, the page headers and mark bits are stored
   separately from their associated page. During garbage collection
   (i.e. for marking and freeing objects) it is important to identify
   the page header which is responsible for a given Lisp object.

   To do this task quickly, I added a two level search tree: the upper
   10 bits of the heap pointer are the index of the first level. This
   entry of the first level links to the second level, where the next
   10 bits of the heap pointer are used to identify the page
   header. The remaining bits point to the object relative to the
   page.

   On architectures with more than 32 bits pointers, a hash value of
   the upper bits is used to index into the first level.

   
   Mark Bits

   For caching purposes, the mark bits are no longer kept within the
   objects, they are kept in a separate bit field.

   Every page header has a field for the mark bits of the objects on
   the page. If there are less cells on the page than there fit bits
   in the integral data type EMACS_INT, the mark bits are stored
   directly in this EMACS_INT.

   Otherwise, the mark bits are written in a separate space, with the
   page header pointing to this space. This happens to pages with
   rather small objects: many cells fit on a page, thus many mark bits
   are needed.


   Allocate Memory

   Use
      void *mc_alloc (size_t size) 
   to request memory from the allocator.  This returns a pointer to a
   newly allocated block of memory of given size.

   This is how the new allocator allocates memory: 
   1. Determine the size class of the object. 
   2. Is there already a page in this size class and is there a free
      cell on this page?
      * YES 
        3. Unlink free cell from free list, return address of free cell. 
        DONE.
      * NO 
        3. Is there a page in the unused heap?
	* YES 
          4. Move unused page to used heap. 
          5. Initialize page header, free list, and mark bits. 
          6. Unlink first cell from free list, return address of cell. 
          DONE.
	* NO 
          4. Expand the heap, add new memory to unused heap 
             [go back to 3. and proceed with the YES case].

  The allocator puts partially filled pages to the front of the page
  list, completely filled ones to the end. That guarantees a fast
  terminating search for free cells. Are there two successive full
  pages at the front of the page list, the complete size class is
  full, a new page has to be added.


  Expand Heap

  To expand the heap, a big chunk of contiguous memory is allocated
  using malloc(). These pieces are called heap sections. How big a new
  heap section is (and thus the growth of the heap) is adjustable:  See
  MIN_HEAP_INCREASE, MAX_HEAP_INCREASE, and HEAP_GROWTH_DIVISOR below.


  Free Memory

  One optimization in XEmacs is that locally used Lisp objects are
  freed manually (the memory is not wasted till the next garbage
  collection). Therefore the new allocator provides this function:
    void mc_free (void *ptr) 
  That frees the object pointed to by ptr.

  This function is also used internally during sweep phase of the
  garbage collection.  This is how it works in detail:

  1. Use pointer to identify page header 
     (use lookup mechanism described above).
  2. Mark cell as free and hook it into free list. 
  3. Is the page completely empty?
     * YES 
       4. Unlink page from page list. 
       5. Remove page header, free list, and mark bits. 
       6. Move page to unused heap.
     * NO 
       4. Move page to front of size class (to speed up allocation 
          of objects).

  If the last object of a page is freed, the empty page is returned to
  the unused heap. The allocator tries to coalesce adjacent pages, to
  gain a big piece of contiguous memory. The resulting chunk is hooked
  into the according size class of the unused heap. If this created a
  complete heap section, the heap section is returned to the operating
  system by using free().


  Allocator and Garbage Collector

  The new allocator simplifies the interface to the Garbage Collector:
  * mark live objects: MARK_[WHITE|GREY|BLACK] (ptr)
  * sweep heap: EMACS_INT mc_sweep (void)
  * run finalizers: EMACS_INT mc_finalize (void)


  Allocator and Dumper

  The new allocator provides special finalization for the portable
  dumper (to save disk space): EMACS_INT mc_finalize_for_disksave (void)


  More Information

  More details can be found in
  http://crestani.de/xemacs/pdf/mc-alloc.pdf .
  
*/

#include <config.h>

#include "lisp.h"
#include "mc-alloc.h"
#include "getpagesize.h"


#if 0
# define USE_MARK_BITS_FREE_LIST 1
#endif
#if 1
# define BLOCKTYPE_ALLOC_PAGE_HEADER 1
#endif

/* Memory protection needs the real system-dependent pagesize. */
#ifndef WIN32_NATIVE
#include <unistd.h> /* for getpagesize () */
#endif
#if defined (HAVE_GETPAGESIZE)
# define SYS_PAGE_SIZE getpagesize ()
#elif defined (_SC_PAGESIZE)
# define SYS_PAGE_SIZE sysconf (_SC_PAGESIZE)
#elif defined (_SC_PAGE_SIZE)
# define SYS_PAGE_SIZE sysconf (_SC_PAGE_SIZE)
#elif defined(get_page_size)
# define SYS_PAGE_SIZE get_page_size ()
#elif defined(PAGESIZE)
# define SYS_PAGE_SIZE PAGESIZE 
#elif defined(PAGE_SIZE)
# define SYS_PAGE_SIZE PAGE_SIZE
#else
 /* Valid page sizes are powers of 2. */
# define SYS_PAGE_SIZE 4096
#endif


/*--- configurable values ----------------------------------------------*/

/* Definition of size classes */

/* Heap used list constants: In the used heap, it is important to
   quickly find a free spot for a new object. Therefore the size
   classes of the used heap are defined by the size of the cells on
   the pages. The size classes should match common object sizes, to
   avoid wasting memory. */

/* Minimum object size in bytes. */
#if BITS_PER_EMACS_INT > 32
# define USED_LIST_MIN_OBJECT_SIZE   16
#else
# define USED_LIST_MIN_OBJECT_SIZE    8
#endif

/* The step size by which the size classes increase (up to upper
   threshold). This many bytes are mapped to a single used list: */
#if BITS_PER_EMACS_INT > 32
# define USED_LIST_LIN_STEP           8
#else
# define USED_LIST_LIN_STEP           4
#endif

/* The upper threshold should always be set to PAGE_SIZE/2, because if
   a object is larger than PAGE_SIZE/2 there is no room for any other
   object on this page. Objects this big are kept in the page list of
   the multiple pages, since a quick search for free spots is not
   needed for this kind of pages (because there are no free spots).
   PAGE_SIZES_DIV_2 defines maximum size of a used space list. */
#define USED_LIST_UPPER_THRESHOLD    PAGE_SIZE_DIV_2


/* Heap free list constants: In the unused heap, the size of
   consecutive memory tips the scales. A page is smallest entity which
   is asked for. Therefore, the size classes of the unused heap are
   defined by the number of consecutive pages. */
/* Sizes up to this many pages each have their own free list. */
#define FREE_LIST_LOWER_THRESHOLD   32
/* The step size by which the size classes increase (up to upper
   threshold). FREE_LIST_LIN_STEP number of sizes are mapped to a
   single free list for sizes between FREE_LIST_LOWER_THRESHOLD and
   FREE_LIST_UPPER_THRESHOLD. */
#define FREE_LIST_LIN_STEP 8
/* Sizes of at least this many pages are mapped to a single free
   list. Blocks of memory larger than this number are all kept in a
   single list, which makes searching this list slow. But objects that
   big are really seldom. */
#define FREE_LIST_UPPER_THRESHOLD  256


/* used heap list count */
#define N_USED_PAGE_LISTS (((USED_LIST_UPPER_THRESHOLD		\
                             - USED_LIST_MIN_OBJECT_SIZE)	\
                            / USED_LIST_LIN_STEP) + 1 ) + 1

/* free heap list count */
#define N_FREE_PAGE_LISTS (((FREE_LIST_UPPER_THRESHOLD		\
                              - FREE_LIST_LOWER_THRESHOLD)	\
                             / FREE_LIST_LIN_STEP)		\
                            + FREE_LIST_LOWER_THRESHOLD)


/* Maximum number of separately added heap sections. */
#if BITS_PER_EMACS_INT > 32
# define MAX_HEAP_SECTS          2048
#else
# define MAX_HEAP_SECTS           768
#endif


/* Heap growth constants. Heap increases by any number between the
   boundaries (unit is PAGE_SIZE). */
#define MIN_HEAP_INCREASE         256
#define MAX_HEAP_INCREASE         256 /* not used */

/* Every heap growth is calculated like this:
   needed_pages + ( HEAP_SIZE / ( PAGE_SIZE * HEAP_GROWTH_DIVISOR )).
   So the growth of the heap is influenced by the current size of the
   heap, but kept between MIN_HEAP_INCREASE and MAX_HEAP_INCREASE
   boundaries.
   This reduces the number of heap sectors, the larger the heap grows
   the larger are the newly allocated chunks. */
#define HEAP_GROWTH_DIVISOR         3


/* Zero memory before putting on free lists. */
#define ZERO_MEM                     1


#ifndef CHAR_BIT    /* should be included by limits.h */
# define CHAR_BIT BITS_PER_CHAR
#endif



/*--- values depending on PAGE_SIZE ------------------------------------*/

/* initialized in init_mc_allocator () */
static EMACS_INT log_page_size;
static EMACS_INT page_size_div_2;

#undef PAGE_SIZE
#define PAGE_SIZE        SYS_PAGE_SIZE
#define LOG_PAGE_SIZE    log_page_size
#define PAGE_SIZE_DIV_2  page_size_div_2


/* Constants for heap address to page header mapping. */
#define  LOG_LEVEL2_SIZE   10
#define LEVEL2_SIZE (1 << LOG_LEVEL2_SIZE)
#if BITS_PER_EMACS_INT > 32
# define USE_HASH_TABLE 1
# define LOG_LEVEL1_SIZE 11
#else
# define LOG_LEVEL1_SIZE \
   (BITS_PER_EMACS_INT - LOG_LEVEL2_SIZE - LOG_PAGE_SIZE)
#endif
#define LEVEL1_SIZE (1 << LOG_LEVEL1_SIZE)

#ifdef USE_HASH_TABLE
# define HASH(hi) ((hi) & (LEVEL1_SIZE - 1))
# define L1_INDEX(p) HASH ((EMACS_UINT) p >> (LOG_LEVEL2_SIZE + LOG_PAGE_SIZE))
#else
# define L1_INDEX(p) ((EMACS_UINT) p >> (LOG_LEVEL2_SIZE + LOG_PAGE_SIZE))
#endif
#define L2_INDEX(p) (((EMACS_UINT) p >> LOG_PAGE_SIZE) & (LEVEL2_SIZE - 1))




/*--- structs and typedefs ---------------------------------------------*/

/* Links the free lists (mark_bit_free_list and cell free list). */
typedef struct free_link
{
  struct lrecord_header lheader;
  struct free_link *next_free;
} free_link;


/* Header for pages. They are held in a doubly linked list. */
typedef struct page_header
{
  struct page_header      *next;         /* next page_header */
  struct page_header      *prev;         /* previous page_header */
  /* Field plh holds pointer to the according header of the page list.*/
  struct page_list_header *plh;          /* page list header */
  free_link               *free_list;    /* links free cells on page */
  EMACS_INT               n_pages;       /* number of pages */
  EMACS_INT               cell_size;     /* size of cells on page */
  EMACS_INT               cells_on_page; /* total number of cells on page */
  EMACS_INT               cells_used;    /* number of used cells on page */
  /* If the number of objects on page is bigger than BITS_PER_EMACS_INT,
     the mark bits are put in an extra memory area. Then the field
     mark_bits holds the pointer to this area. Is the number of
     objects smaller than BITS_PER_EMACS_INT, the mark bits are held in the
     mark_bit EMACS_INT directly, without an additional indirection. */
  unsigned int            black_bit:1;   /* objects on page are black */
  unsigned int            dirty_bit:1;   /* page is dirty */
  unsigned int            protection_bit:1;   /* page is write protected */
  unsigned int            array_bit:1;   /* page holds arrays */
  Rawbyte                 *mark_bits;    /* pointer to mark bits */
  void                    *heap_space;   /* pointer to heap, where objects
                                            are stored */
} page_header;


/* Different list types. */
enum list_type_enum {
  USED_LIST,
  FREE_LIST
};


/* Header for page lists. Field list_type holds the type of the list. */
typedef struct page_list_header
{
  enum list_type_enum     list_type;     /* the type of the list */
  /* Size holds the size of one cell (in bytes) in a used heap list, or the
     size of the heap sector (in number of pages). */
  size_t                  size;          /* size of one cell / heap sector */
  page_header             *first;        /* first of page_header list */
  page_header             *last;         /* last of page_header list */
  /* If the number of objects on page is bigger than
     BITS_PER_EMACS_INT, the mark bits are put in an extra memory
     area, which is linked in this free list, if not used.  Is the
     number of objects smaller than BITS_PER_EMACS_INT, the mark bits
     are hold in the mark bit EMACS_INT directly, without an
     additional indirection. */
  free_link               *mark_bit_free_list;

#ifdef MEMORY_USAGE_STATS
  EMACS_INT               page_count;    /* number if pages in list */
  EMACS_INT               used_cells;    /* number of objects in list */
  EMACS_INT               used_space;    /* used space */
  EMACS_INT               total_cells;   /* number of available cells */
  EMACS_INT               total_space;   /* available space */
#endif
} page_list_header;


/* The heap sectors are stored with their real start pointer and their
   real size. Not aligned to PAGE_SIZE. Needed for freeing heap sectors. */
typedef struct heap_sect {
  void                    *real_start;   /* real start pointer (NOT aligned) */
  size_t                  real_size;     /* NOT multiple of PAGE_SIZE */
  void                    *start;        /* aligned start pointer */
  EMACS_INT               n_pages;       /* multiple of PAGE_SIZE */
} heap_sect;


/* 2nd tree level for mapping of heap addresses to page headers. */
typedef struct level_2_lookup_tree {
  page_header             *index[LEVEL2_SIZE]; /* link to page header */
  EMACS_INT               key;                 /* high order address bits */
#ifdef USE_HASH_TABLE
  struct level_2_lookup_tree *hash_link; /* hash chain link */
#endif
} level_2_lookup_tree;



/*--- global variable definitions --------------------------------------*/

/* All global allocator variables are kept in this struct. */
typedef struct mc_allocator_globals_type {

  /* heap size */
  EMACS_INT            heap_size;

  /* list of all separatly allocated chunks of heap */
  heap_sect            heap_sections[MAX_HEAP_SECTS];
  EMACS_INT            n_heap_sections;

  /* Holds all allocated pages, each object size class in its separate list,
     to guarantee fast allocation on partially filled pages. */
  page_list_header     *used_heap_pages;

  /* Holds all allocated pages that contain array elements. */
  page_list_header     array_heap_pages;

  /* Holds all free pages in the heap. N multiples of PAGE_SIZE are
     kept on the Nth free list. Contiguos pages are coalesced. */
  page_list_header     free_heap_pages[N_FREE_PAGE_LISTS];

  /* ptr lookup table */
  level_2_lookup_tree  **ptr_lookup_table;

#ifndef BLOCKTYPE_ALLOC_PAGE_HEADER
  /* page header free list */
  free_link            *page_header_free_list;
#endif /* not BLOCKTYPE_ALLOC_PAGE_HEADER */

#ifdef MEMORY_USAGE_STATS
  EMACS_INT            malloced_bytes;
#endif
} mc_allocator_globals_type;

mc_allocator_globals_type mc_allocator_globals;




/*--- macro accessors --------------------------------------------------*/

#define USED_HEAP_PAGES(i) \
  ((page_list_header*) &mc_allocator_globals.used_heap_pages[i])

#define ARRAY_HEAP_PAGES \
  ((page_list_header*) &mc_allocator_globals.array_heap_pages)

#define FREE_HEAP_PAGES(i) \
  ((page_list_header*) &mc_allocator_globals.free_heap_pages[i])

#define PLH(plh) plh
# define PLH_LIST_TYPE(plh) PLH (plh)->list_type
# define PLH_SIZE(plh) PLH (plh)->size
# define PLH_FIRST(plh) PLH (plh)->first
# define PLH_LAST(plh)  PLH (plh)->last
# define PLH_MARK_BIT_FREE_LIST(plh) PLH (plh)->mark_bit_free_list
#ifdef MEMORY_USAGE_STATS
# define PLH_PAGE_COUNT(plh) PLH (plh)->page_count
# define PLH_USED_CELLS(plh) PLH (plh)->used_cells
# define PLH_USED_SPACE(plh) PLH (plh)->used_space
# define PLH_TOTAL_CELLS(plh) PLH (plh)->total_cells
# define PLH_TOTAL_SPACE(plh) PLH (plh)->total_space
#endif

#define PH(ph) ph
# define PH_NEXT(ph) PH (ph)->next
# define PH_PREV(ph) PH (ph)->prev
# define PH_PLH(ph) PH (ph)->plh
# define PH_FREE_LIST(ph) PH (ph)->free_list
# define PH_N_PAGES(ph) PH (ph)->n_pages
# define PH_CELL_SIZE(ph) PH (ph)->cell_size
# define PH_CELLS_ON_PAGE(ph) PH (ph)->cells_on_page
# define PH_CELLS_USED(ph) PH (ph)->cells_used
# define PH_BLACK_BIT(ph) PH (ph)->black_bit
# define PH_DIRTY_BIT(ph) PH (ph)->dirty_bit
# define PH_PROTECTION_BIT(ph) PH (ph)->protection_bit
# define PH_ARRAY_BIT(ph) PH (ph)->array_bit
# define PH_MARK_BITS(ph) PH (ph)->mark_bits
# define PH_HEAP_SPACE(ph) PH (ph)->heap_space
#define PH_LIST_TYPE(ph) PLH_LIST_TYPE (PH_PLH (ph))
#define PH_MARK_BIT_FREE_LIST(ph) PLH_MARK_BIT_FREE_LIST (PH_PLH (ph))

#define HEAP_SIZE mc_allocator_globals.heap_size

#ifdef MEMORY_USAGE_STATS
# define MC_MALLOCED_BYTES mc_allocator_globals.malloced_bytes
#endif

#define HEAP_SECTION(index) mc_allocator_globals.heap_sections[index]
#define N_HEAP_SECTIONS mc_allocator_globals.n_heap_sections

#ifndef BLOCKTYPE_ALLOC_PAGE_HEADER
#define PAGE_HEADER_FREE_LIST mc_allocator_globals.page_header_free_list
#endif /* not BLOCKTYPE_ALLOC_PAGE_HEADER */

#define NEXT_FREE(free_list) ((free_link*) free_list)->next_free
#define FREE_LIST(free_list) (free_link*) (free_list)

#define PTR_LOOKUP_TABLE(i) mc_allocator_globals.ptr_lookup_table[i]
#define LEVEL2(l2, i) l2->index[i]
# define LEVEL2_KEY(l2) l2->key
#ifdef USE_HASH_TABLE
# define LEVEL2_HASH_LINK(l2) l2->hash_link
#endif

#if ZERO_MEM
# define ZERO_HEAP_SPACE(ph) \
   memset (PH_HEAP_SPACE (ph), '\0', PH_N_PAGES (ph) * PAGE_SIZE)
# define ZERO_PAGE_HEADER(ph) memset (ph, '\0', sizeof (page_header))
#endif

#define div_PAGE_SIZE(x) (x >> LOG_PAGE_SIZE)
#define mult_PAGE_SIZE(x) (x << LOG_PAGE_SIZE)

#define BYTES_TO_PAGES(bytes) (div_PAGE_SIZE ((bytes + (PAGE_SIZE - 1))))

#define PAGE_SIZE_ALIGNMENT(address) \
  (void *) ((((EMACS_UINT) (address)) + PAGE_SIZE) & ~(PAGE_SIZE - 1))

#define PH_ON_FREE_LIST_P(ph) \
  (ph && PH_PLH (ph) && (PLH_LIST_TYPE (PH_PLH (ph)) == FREE_LIST))

#define PH_ON_USED_LIST_P(ph) \
  (ph && PH_PLH (ph) && (PLH_LIST_TYPE (PH_PLH (ph)) == USED_LIST))


/* Number of mark bits: minimum 1, maximum 8. */
#define N_MARK_BITS 2



/************************************************************************/
/*                           MC Allocator                               */
/************************************************************************/

/* Set to 1 if memory becomes short. */
EMACS_INT memory_shortage;

/*--- misc functions ---------------------------------------------------*/

/* Visits all pages (page_headers) hooked into the used heap pages
   list and executes f with the current page header as
   argument. Needed for sweep.  Returns number of processed pages. */
static EMACS_INT
visit_all_used_page_headers (EMACS_INT (*f) (page_header *ph))
{
  EMACS_INT number_of_pages_processed = 0;
  EMACS_INT i;
  for (i = 0; i < N_USED_PAGE_LISTS; i++)
    if (PLH_FIRST (USED_HEAP_PAGES (i)))
      {
        page_header *ph = PLH_FIRST (USED_HEAP_PAGES (i));
        while (PH_NEXT (ph))
          {
            page_header *next = PH_NEXT (ph); /* in case f removes the page */
            number_of_pages_processed += f (ph);
            ph = next;
          }
        number_of_pages_processed += f (ph);
      }

  if (PLH_FIRST (ARRAY_HEAP_PAGES))
    {
      page_header *ph = PLH_FIRST (ARRAY_HEAP_PAGES);
      while (PH_NEXT (ph))
	{
	  page_header *next = PH_NEXT (ph); /* in case f removes the page */
	  number_of_pages_processed += f (ph);
	  ph = next;
	}
      number_of_pages_processed += f (ph);
    }
  
  return number_of_pages_processed;
}




/*--- mapping of heap addresses to page headers and mark bits ----------*/

/* Sets a heap pointer and page header pair into the lookup table. */
static void
set_lookup_table (void *ptr, page_header *ph)
{
  EMACS_INT l1_index = L1_INDEX (ptr);
  level_2_lookup_tree *l2 = PTR_LOOKUP_TABLE (l1_index);
#ifdef USE_HASH_TABLE
  while ((l2) && (LEVEL2_KEY (l2) != l1_index))
    l2 = LEVEL2_HASH_LINK (l2);
#endif
  if (!l2)
    {
      l2 = (level_2_lookup_tree*) 
	xmalloc_and_zero (sizeof (level_2_lookup_tree));
#ifdef MEMORY_USAGE_STATS
      MC_MALLOCED_BYTES += 
	malloced_storage_size (0, sizeof (level_2_lookup_tree), 0);
#endif
      memset (l2, '\0', sizeof (level_2_lookup_tree));
#ifdef USE_HASH_TABLE
      LEVEL2_HASH_LINK (l2) = PTR_LOOKUP_TABLE (l1_index);
#endif
      PTR_LOOKUP_TABLE (l1_index) = l2;
      LEVEL2_KEY (l2) = l1_index;
    }
  LEVEL2 (l2, L2_INDEX (ptr)) = ph;
}


#ifdef UNSET_LOOKUP_TABLE
/* Set the lookup table to 0 for given heap address. */
static void
unset_lookup_table (void *ptr)
{
  EMACS_INT l1_index = L1_INDEX (ptr);
  level_2_lookup_tree *l2 = PTR_LOOKUP_TABLE (l1_index);
#ifdef USE_HASH_TABLE
  while ((l2) && (LEVEL2_KEY (l2) != l1_index))
    l2 = LEVEL2_HASH_LINK (l2);
#endif
  if (l2) { 
    LEVEL2 (l2, L2_INDEX (ptr)) = 0;
  }
}
#endif

/* Returns the page header of a given heap address, or 0 if not in table. 
   For internal use, no error checking. */
static page_header *
get_page_header_internal (void *ptr)
{
  EMACS_INT l1_index = L1_INDEX (ptr);
  level_2_lookup_tree *l2 = PTR_LOOKUP_TABLE (l1_index);
#ifdef USE_HASH_TABLE
  while ((l2) && (LEVEL2_KEY (l2) != l1_index))
    l2 = LEVEL2_HASH_LINK (l2);
#endif
  if (!l2) 
      return 0;
  return LEVEL2 (l2, L2_INDEX (ptr));
}

/* Returns the page header of a given heap address, or 0 if not in table. */
static page_header *
get_page_header (void *ptr)
{
  EMACS_INT l1_index = L1_INDEX (ptr);
  level_2_lookup_tree *l2 = PTR_LOOKUP_TABLE (l1_index);
  assert (l2);
#ifdef USE_HASH_TABLE
  while ((l2) && (LEVEL2_KEY (l2) != l1_index))
    l2 = LEVEL2_HASH_LINK (l2);
#endif
  assert (LEVEL2 (l2, L2_INDEX (ptr)));
  return LEVEL2 (l2, L2_INDEX (ptr));
}

/* Returns the mark bit index of a given heap address. */
static EMACS_INT
get_mark_bit_index (void *ptr, page_header *ph)
{
  EMACS_INT cell_size = PH_CELL_SIZE (ph);
  if (cell_size)
    return (((EMACS_INT) ptr - (EMACS_INT)(PH_HEAP_SPACE (ph))) / cell_size) 
      * N_MARK_BITS;
  else /* only one object on page */
    return 0;
}


/* Adds addresses of pages to lookup table. */
static void
add_pages_to_lookup_table (page_header *ph, EMACS_INT n_pages)
{
  Rawbyte *p = (Rawbyte *) PH_HEAP_SPACE (ph);
  EMACS_INT end_of_section = (EMACS_INT) p + (PAGE_SIZE * n_pages);
  for (p = (Rawbyte *) PH_HEAP_SPACE (ph);
       (EMACS_INT) p < end_of_section; p += PAGE_SIZE)
    set_lookup_table (p, ph);
}


/* Initializes lookup table. */
static void
init_lookup_table (void)
{
  EMACS_INT i;
  for (i = 0; i < LEVEL1_SIZE; i++)
    PTR_LOOKUP_TABLE (i) = 0;
}




/*--- mark bits --------------------------------------------------------*/

/*--- bit operations --- */

/* Allocates a bit array of length bits. */
static Rawbyte *
alloc_bit_array(size_t bits)
{
  Rawbyte *bit_array;
#ifdef USE_MARK_BITS_FREE_LIST
  size_t size = ((bits + CHAR_BIT - 1) / CHAR_BIT) * sizeof (Rawbyte); 
#else /* not USE_MARK_BITS_FREE_LIST */
  size_t size = 
    ALIGN_FOR_TYPE (((bits + CHAR_BIT - 1) / CHAR_BIT) * sizeof (Rawbyte), 
		    Rawbyte *);
#endif /* not USE_MARK_BITS_FREE_LIST */
  if (size < sizeof (free_link)) size = sizeof (free_link);
#ifdef MEMORY_USAGE_STATS
  MC_MALLOCED_BYTES += malloced_storage_size (0, size, 0);
#endif
  bit_array = (Rawbyte *) xmalloc_and_zero (size);
  return bit_array;
}


/* Returns the bit value at pos. */
static EMACS_INT
get_bit (Rawbyte *bit_array, EMACS_INT pos)
{
#if N_MARK_BITS > 1
  EMACS_INT result = 0;
  EMACS_INT i;
#endif
  bit_array += pos / CHAR_BIT;
#if N_MARK_BITS > 1
  for (i = 0; i < N_MARK_BITS; i++)
    result |= ((*bit_array & (1 << ((pos + i) % CHAR_BIT))) != 0) << i;
  return result;
#else
  return (*bit_array & (1 << (pos % CHAR_BIT))) != 0;
#endif
}


/* Bit_Arrays bit at pos to val. */
static void
set_bit (Rawbyte *bit_array, EMACS_INT pos, EMACS_UINT val)
{
#if N_MARK_BITS > 1
  EMACS_INT i;
#endif
  bit_array += pos / CHAR_BIT;
#if N_MARK_BITS > 1
  for (i = 0; i < N_MARK_BITS; i++)
    if ((val >> i) & 1)
      *bit_array |= 1 << ((pos + i) % CHAR_BIT);
    else
      *bit_array &= ~(1 << ((pos + i) % CHAR_BIT));
#else
  if (val)
    *bit_array |= 1 << (pos % CHAR_BIT);
  else
    *bit_array &= ~(1 << (pos % CHAR_BIT));
#endif
}


/*--- mark bit functions ---*/
#define USE_PNTR_MARK_BITS(ph) \
  ((PH_CELLS_ON_PAGE (ph) * N_MARK_BITS) > BITS_PER_EMACS_INT)
#define USE_WORD_MARK_BITS(ph) \
  ((PH_CELLS_ON_PAGE (ph) * N_MARK_BITS) <= BITS_PER_EMACS_INT)

#define GET_BIT_WORD(b, p) get_bit ((Rawbyte *) &b, p)
#define GET_BIT_PNTR(b, p) get_bit (b, p)

#define SET_BIT_WORD(b, p, v) set_bit ((Rawbyte *) &b, p, v)
#define SET_BIT_PNTR(b, p, v) set_bit (b, p, v)

#define ZERO_MARK_BITS_WORD(ph) PH_MARK_BITS (ph) = 0
#define ZERO_MARK_BITS_PNTR(ph)					\
do {								\
  memset (PH_MARK_BITS (ph), '\0',				\
          ((PH_CELLS_ON_PAGE (ph) * N_MARK_BITS)		\
          + CHAR_BIT - 1) / CHAR_BIT * sizeof (Rawbyte));	\
} while (0)

#define GET_BIT(bit, ph, p)			\
do {						\
  if (USE_PNTR_MARK_BITS (ph))			\
    bit = GET_BIT_PNTR (PH_MARK_BITS (ph), p);	\
  else						\
    bit = GET_BIT_WORD (PH_MARK_BITS (ph), p);	\
} while (0)

#define SET_BIT(ph, p, v)			\
do {						\
  if (USE_PNTR_MARK_BITS (ph))			\
    SET_BIT_PNTR (PH_MARK_BITS (ph), p, v);	\
  else						\
    SET_BIT_WORD (PH_MARK_BITS (ph), p, v);	\
} while (0)

#define ZERO_MARK_BITS(ph)			\
do {						\
  if (USE_PNTR_MARK_BITS (ph))			\
   ZERO_MARK_BITS_PNTR (ph);			\
  else						\
    ZERO_MARK_BITS_WORD (ph);			\
} while (0)


/* Allocates mark-bit space either from a free list or from the OS
   for the given page header. */
static Rawbyte *
alloc_mark_bits (page_header *ph)
{
  Rawbyte *result;
#ifdef USE_MARK_BITS_FREE_LIST
  if (PH_MARK_BIT_FREE_LIST (ph) == 0)
    result = (Rawbyte *) alloc_bit_array (PH_CELLS_ON_PAGE (ph) * N_MARK_BITS);
  else
    {
      result = (Rawbyte *) PH_MARK_BIT_FREE_LIST (ph);
      PH_MARK_BIT_FREE_LIST (ph) = NEXT_FREE (result);
    }
#else /* not USE_MARK_BITS_FREE_LIST */
  result = (Rawbyte *) alloc_bit_array (PH_CELLS_ON_PAGE (ph) * N_MARK_BITS);
#endif /* not USE_MARK_BITS_FREE_LIST */
  return result;
}


/* Frees by maintaining a free list. */
static void
free_mark_bits (page_header *ph)
{
#ifdef USE_MARK_BITS_FREE_LIST
  NEXT_FREE (PH_MARK_BITS (ph)) = PH_MARK_BIT_FREE_LIST (ph);
  PH_MARK_BIT_FREE_LIST (ph) = FREE_LIST (PH_MARK_BITS (ph));
#else /* not USE_MARK_BITS_FREE_LIST */
  if (PH_MARK_BITS (ph))
    free (PH_MARK_BITS (ph));
#endif /* not USE_MARK_BITS_FREE_LIST */
}


/* Installs mark bits and zeros bits. */
static void
install_mark_bits (page_header *ph)
{
  if (USE_PNTR_MARK_BITS (ph))
    {
      PH_MARK_BITS (ph) = alloc_mark_bits (ph);
      ZERO_MARK_BITS_PNTR (ph);
    }
  else
    ZERO_MARK_BITS_WORD (ph);
}


/* Cleans and frees the mark bits of the given page_header. */
static void
remove_mark_bits (page_header *ph)
{
  if (USE_PNTR_MARK_BITS (ph))
    free_mark_bits (ph);
}


/* Zeros all mark bits in given header. */
static void
zero_mark_bits (page_header *ph)
{
  ZERO_MARK_BITS (ph);
}


/* Returns mark bit for given heap pointer. */
EMACS_INT
get_mark_bit (void *ptr)
{
  EMACS_INT bit = 0;
  page_header *ph = get_page_header (ptr);
  gc_checking_assert (ph && PH_ON_USED_LIST_P (ph));
  if (ph)
    {
      GET_BIT (bit, ph, get_mark_bit_index (ptr, ph));
    }
  return bit;
}


/* Sets mark bit for given heap pointer. */
void
set_mark_bit (void *ptr, EMACS_INT value)
{
  page_header *ph = get_page_header (ptr);
  assert (ph && PH_ON_USED_LIST_P (ph));
  if (ph)
    {
      if (value == BLACK)
	if (!PH_BLACK_BIT (ph))
	  PH_BLACK_BIT (ph) = 1;
      SET_BIT (ph, get_mark_bit_index (ptr, ph), value);
    }
}




/*--- page header functions --------------------------------------------*/

#ifdef BLOCKTYPE_ALLOC_PAGE_HEADER
#include "blocktype.h"

struct page_header_blocktype 
{ 
  Blocktype_declare (page_header); 
} *the_page_header_blocktype;
#endif /* BLOCKTYPE_ALLOC_PAGE_HEADER */

/* Allocates a page header either from a free list or from the OS. */
static page_header *
alloc_page_header (void)
{
#ifdef BLOCKTYPE_ALLOC_PAGE_HEADER
  page_header *result;
#ifdef MEMORY_USAGE_STATS
  MC_MALLOCED_BYTES += malloced_storage_size (0, sizeof (page_header), 0);
#endif
  result = Blocktype_alloc (the_page_header_blocktype);
  ZERO_PAGE_HEADER (result);
  return result;
#else /* not BLOCKTYPE_ALLOC_PAGE_HEADER */
  page_header *result;
  if (PAGE_HEADER_FREE_LIST == 0)
    {
      result = 
	(page_header *) xmalloc_and_zero ((EMACS_INT) (sizeof (page_header)));
#ifdef MEMORY_USAGE_STATS
      MC_MALLOCED_BYTES += malloced_storage_size (0, sizeof (page_header), 0);
#endif
    }
  else
    {
      result = (page_header*) PAGE_HEADER_FREE_LIST;
      PAGE_HEADER_FREE_LIST = NEXT_FREE (result);
    }
  return result;
#endif /* not BLOCKTYPE_ALLOC_PAGE_HEADER */
}


/* Frees given page header by maintaining a free list. */
static void
free_page_header (page_header *ph)
{
#ifdef BLOCKTYPE_ALLOC_PAGE_HEADER
  Blocktype_free (the_page_header_blocktype, ph);
#else /* not BLOCKTYPE_ALLOC_PAGE_HEADER */
#if ZERO_MEM
  ZERO_PAGE_HEADER (ph);
#endif
  NEXT_FREE (ph) = PAGE_HEADER_FREE_LIST;
  PAGE_HEADER_FREE_LIST = FREE_LIST (ph);
#endif /* not BLOCKTYPE_ALLOC_PAGE_HEADER */
}


/* Adds given page header to given page list header's list. */
static void
add_page_header_to_plh (page_header *ph, page_list_header *plh)
{
  /* insert at the front of the list */
  PH_PREV (ph) = 0;
  PH_NEXT (ph) = PLH_FIRST (plh);
  PH_PLH (ph) = plh;
  /* if list is not empty, set prev in the first element */
  if (PLH_FIRST (plh))
    PH_PREV (PLH_FIRST (plh)) = ph;
  /* one element in list is first and last at the same time */
  PLH_FIRST (plh) = ph;
  if (!PLH_LAST (plh))
    PLH_LAST (plh) = ph;

#ifdef MEMORY_USAGE_STATS
  /* bump page count */
  PLH_PAGE_COUNT (plh)++;
#endif

}


/* Removes given page header from given page list header's list. */
static void
remove_page_header_from_plh (page_header *ph, page_list_header *plh)
{
  if (PLH_FIRST (plh) == ph)
    PLH_FIRST (plh) = PH_NEXT (ph);
  if (PLH_LAST (plh) == ph)
    PLH_LAST (plh) = PH_PREV (ph);
  if (PH_NEXT (ph))
    PH_PREV (PH_NEXT (ph)) = PH_PREV (ph);
  if (PH_PREV (ph))
    PH_NEXT (PH_PREV (ph)) = PH_NEXT (ph);

#ifdef MEMORY_USAGE_STATS
  /* decrease page count */
  PLH_PAGE_COUNT (plh)--;
#endif
}


/* Moves a page header to the front of its the page header list.
   This is used during sweep: Pages with some alive objects are moved to
   the front. This makes allocation faster, all pages with free slots
   can be found at the front of the list. */
static void
move_page_header_to_front (page_header *ph)
{
  page_list_header *plh = PH_PLH (ph);
  /* is page already first? */
  if (ph == PLH_FIRST (plh)) return;
  /* remove from list */
  if (PLH_LAST (plh) == ph)
    PLH_LAST (plh) = PH_PREV (ph);
  if (PH_NEXT (ph))
    PH_PREV (PH_NEXT (ph)) = PH_PREV (ph);
  if (PH_PREV (ph))
    PH_NEXT (PH_PREV (ph)) = PH_NEXT (ph);
  /* insert at the front */
  PH_NEXT (ph) = PLH_FIRST (plh);
  PH_PREV (ph) = 0;
  PH_PREV (PH_NEXT (ph)) = ph;
  PLH_FIRST (plh) = ph;
}




/*--- page list functions ----------------------------------------------*/

/* Returns the index of the used heap list according to given size. */
static int
get_used_list_index (size_t size)
{
  if (size <= USED_LIST_MIN_OBJECT_SIZE)
    {
      /*      printf ("size %d -> index %d\n", size, 0); */
      return 0;
    }
  if (size <= (size_t) USED_LIST_UPPER_THRESHOLD)
    {
      /*      printf ("size %d -> index %d\n", size, */
      /*	      ((size - USED_LIST_MIN_OBJECT_SIZE - 1) */
      /*	       / USED_LIST_LIN_STEP) + 1); */
      return ((size - USED_LIST_MIN_OBJECT_SIZE - 1)
	      / USED_LIST_LIN_STEP) + 1;
    }
  /*  printf ("size %d -> index %d\n", size, N_USED_PAGE_LISTS - 1); */
  return N_USED_PAGE_LISTS - 1;
}

/* Returns the size of the used heap list according to given index. */
static size_t
get_used_list_size_value (int used_index)
{
  if (used_index < N_USED_PAGE_LISTS - 1)
    return (used_index * USED_LIST_LIN_STEP) + USED_LIST_MIN_OBJECT_SIZE;
  return 0;
}


/* Returns the index of the free heap list according to given size. */
static EMACS_INT
get_free_list_index (EMACS_INT n_pages)
{
  if (n_pages == 0)
    return 0;
  if (n_pages <= FREE_LIST_LOWER_THRESHOLD)
    return n_pages - 1;
  if (n_pages >= FREE_LIST_UPPER_THRESHOLD - 1)
    return N_FREE_PAGE_LISTS - 1;
  return ((n_pages - FREE_LIST_LOWER_THRESHOLD - 1)
          / FREE_LIST_LIN_STEP) + FREE_LIST_LOWER_THRESHOLD;

}


/* Returns the size in number of pages of the given free list at index. */
static size_t
get_free_list_size_value (EMACS_INT free_index)
{
  if (free_index < FREE_LIST_LOWER_THRESHOLD)
    return free_index + 1;
  if (free_index >= N_FREE_PAGE_LISTS)
    return FREE_LIST_UPPER_THRESHOLD;
  return ((free_index + 1 - FREE_LIST_LOWER_THRESHOLD)
          * FREE_LIST_LIN_STEP) + FREE_LIST_LOWER_THRESHOLD;
}


Bytecount
mc_alloced_storage_size (Bytecount claimed_size, struct usage_stats *stats)
{
  size_t used_size = 
    get_used_list_size_value (get_used_list_index (claimed_size));
  if (used_size == 0)
    used_size = mult_PAGE_SIZE (BYTES_TO_PAGES (claimed_size));

  if (stats)
    {
      stats->was_requested += claimed_size;
      stats->malloc_overhead += used_size - claimed_size;
    }

  return used_size;
}



/*--- free heap functions ----------------------------------------------*/

/* Frees a heap section, if the heap_section is completly free */
static EMACS_INT
free_heap_section (page_header *ph)
{
  EMACS_INT i;
  EMACS_INT removed = 0;
  for (i = 0; i < N_HEAP_SECTIONS; i++)
    if (!removed)
      {
	if ((PH_HEAP_SPACE (ph) == HEAP_SECTION(i).start)
	    && (PH_N_PAGES (ph) == HEAP_SECTION(i).n_pages))
	  {
	    xfree_1 (HEAP_SECTION(i).real_start);
#ifdef MEMORY_USAGE_STATS
	    MC_MALLOCED_BYTES
	      -= malloced_storage_size (0, HEAP_SECTION(i).real_size, 0);
#endif

	    HEAP_SIZE -= PH_N_PAGES (ph) * PAGE_SIZE;

	    removed = 1;
	  }
      }
    else
      {
	HEAP_SECTION(i-1).real_start = HEAP_SECTION(i).real_start;
	HEAP_SECTION(i-1).real_size = HEAP_SECTION(i).real_size;
	HEAP_SECTION(i-1).start = HEAP_SECTION(i).start;
	HEAP_SECTION(i-1).n_pages = HEAP_SECTION(i).n_pages;
      }

  N_HEAP_SECTIONS = N_HEAP_SECTIONS - removed;
  
  return removed;
}

/* Removes page from free list. */
static void
remove_page_from_free_list (page_header *ph)
{
  remove_page_header_from_plh (ph, PH_PLH (ph));
  PH_PLH (ph) = 0;
}


/* Adds page to according free list. */
static void
add_page_to_free_list (page_header *ph)
{
  PH_PLH (ph) = FREE_HEAP_PAGES (get_free_list_index (PH_N_PAGES (ph)));
  add_page_header_to_plh (ph, PH_PLH (ph));
}


/* Merges two adjacent pages. */
static page_header *
merge_pages (page_header *first_ph, page_header *second_ph)
{
  /* merge */
  PH_N_PAGES (first_ph) += PH_N_PAGES (second_ph);
  /* clean up left over page header */
  free_page_header (second_ph);
  /* update lookup table */
  add_pages_to_lookup_table (first_ph, PH_N_PAGES (first_ph));

  return first_ph;
}


/* Checks if pages are adjacent, merges them, and adds merged page to
   free list */
static void
merge_into_free_list (page_header *ph)
{
  /* check if you can coalesce adjacent pages */
  page_header *prev_ph =
    get_page_header_internal ((void*) (((EMACS_INT) PH_HEAP_SPACE (ph)) 
				       - PAGE_SIZE));
  page_header *succ_ph =
    get_page_header_internal ((void*) (((EMACS_INT) PH_HEAP_SPACE (ph))
			      + (PH_N_PAGES (ph) * PAGE_SIZE)));
  if (PH_ON_FREE_LIST_P (prev_ph))
    {
      remove_page_from_free_list (prev_ph);
      ph = merge_pages (prev_ph, ph);
    }
  if (PH_ON_FREE_LIST_P (succ_ph))
    {
      remove_page_from_free_list (succ_ph);
      ph = merge_pages (ph, succ_ph);
    }
  /* try to free heap_section, if the section is complete */
  if (!free_heap_section (ph))
    /* else add merged page to free list */
    add_page_to_free_list (ph);
}


/* Cuts given page header after n_pages, returns the first (cut) part, and
   puts the rest on the free list. */
static page_header *
split_page (page_header *ph, EMACS_INT n_pages)
{
  page_header *new_ph;
  EMACS_INT rem_pages = PH_N_PAGES (ph) - n_pages;

  /* remove the page from the free list if already hooked in */
  if (PH_PLH (ph))
    remove_page_from_free_list (ph);
  /* set new number of pages */
  PH_N_PAGES (ph) = n_pages;
  /* add new page to lookup table */
  add_pages_to_lookup_table (ph, n_pages);

  if (rem_pages)
    {
      /* build new page with reminder */
      new_ph = alloc_page_header ();
      PH_N_PAGES (new_ph) = rem_pages;
      PH_HEAP_SPACE (new_ph) =
        (void*) ((EMACS_INT) (PH_HEAP_SPACE (ph)) + (n_pages * PAGE_SIZE));
      /* add new page to lookup table */
      add_pages_to_lookup_table (new_ph, rem_pages);
      /* hook the rest into free list */
      add_page_to_free_list (new_ph);
    }
  return ph;
}


/* Expands the heap by given number of pages. */
static page_header *
expand_heap (EMACS_INT needed_pages)
{
  page_header *ph;
  EMACS_INT n_pages;
  size_t real_size;
  void *real_start;

  /* determine number of pages the heap should grow */
  if (memory_shortage)
    n_pages = needed_pages;
  else
    n_pages = max (MIN_HEAP_INCREASE, 
		   needed_pages
		   + (HEAP_SIZE / (PAGE_SIZE * HEAP_GROWTH_DIVISOR)));

  /* get the real values */
  real_size = (n_pages * PAGE_SIZE) + PAGE_SIZE;
  real_start = xmalloc_and_zero (real_size);
#ifdef MEMORY_USAGE_STATS
  MC_MALLOCED_BYTES += malloced_storage_size (0, real_size, 0);
#endif

  /* maintain heap section count */
  if (N_HEAP_SECTIONS >= MAX_HEAP_SECTS)
    {
      stderr_out ("Increase number of MAX_HEAP_SECTS");
      ABORT ();
    }
  HEAP_SECTION(N_HEAP_SECTIONS).real_start = real_start;
  HEAP_SECTION(N_HEAP_SECTIONS).real_size = real_size;
  HEAP_SECTION(N_HEAP_SECTIONS).start = PAGE_SIZE_ALIGNMENT (real_start);
  HEAP_SECTION(N_HEAP_SECTIONS).n_pages = n_pages;
  N_HEAP_SECTIONS ++;

  /* get page header */
  ph = alloc_page_header ();

  /* setup page header */
  PH_N_PAGES (ph) = n_pages;
  PH_HEAP_SPACE (ph) = PAGE_SIZE_ALIGNMENT (real_start);
  assert (((EMACS_INT) (PH_HEAP_SPACE (ph)) % PAGE_SIZE) == 0);
  HEAP_SIZE += n_pages * PAGE_SIZE;

  /* this was also done by allocate_lisp_storage */
  if (need_to_check_c_alloca)
    xemacs_c_alloca (0);

  /* return needed size, put rest on free list */
  return split_page (ph, needed_pages);
}




/*--- used heap functions ----------------------------------------------*/
/* Installs initial free list. */
static void
install_cell_free_list (page_header *ph)
{
  Rawbyte *p;
  EMACS_INT i;
  EMACS_INT cell_size = PH_CELL_SIZE (ph);
  /* write initial free list if cell_size is < PAGE_SIZE */
  p = (Rawbyte *) PH_HEAP_SPACE (ph);
  for (i = 0; i < PH_CELLS_ON_PAGE (ph) - 1; i++)
    {
#ifdef ERROR_CHECK_GC
      assert (!LRECORD_FREE_P (p));
      MARK_LRECORD_AS_FREE (p);
#endif
      if (!PH_ARRAY_BIT (ph))
	NEXT_FREE (p) = FREE_LIST (p + cell_size);
      set_lookup_table (p, ph);
      p += cell_size;
    }
#ifdef ERROR_CHECK_GC
  assert (!LRECORD_FREE_P (p));
  MARK_LRECORD_AS_FREE (p);
#endif
  NEXT_FREE (p) = 0;
  set_lookup_table (p, ph);

  /* hook free list into header */
  PH_FREE_LIST (ph) = FREE_LIST (PH_HEAP_SPACE (ph));
}


/* Cleans the object space of the given page_header. */
static void
remove_cell_free_list (page_header *ph)
{
#if ZERO_MEM
  ZERO_HEAP_SPACE (ph);
#endif
  PH_FREE_LIST (ph) = 0;
}


/* Installs a new page and hooks it into given page_list_header. */
static page_header *
install_page_in_used_list (page_header *ph, page_list_header *plh, 
			   size_t size, EMACS_INT elemcount)
{
  /* add to list */
  add_page_header_to_plh (ph, plh);

  /* determine cell size */
  if (PLH_SIZE (plh))
    PH_CELL_SIZE (ph) = PLH_SIZE (plh);
  else
    PH_CELL_SIZE (ph) = size;
  if (elemcount == 1)
    {
      PH_CELLS_ON_PAGE (ph) = (PAGE_SIZE * PH_N_PAGES (ph)) / PH_CELL_SIZE (ph);
      PH_ARRAY_BIT (ph) = 0;
    }
  else
    {
      PH_CELLS_ON_PAGE (ph) = elemcount;
      PH_ARRAY_BIT (ph) = 1;
    }

  /* init cell count */
  PH_CELLS_USED (ph) = 0;

  /* install mark bits and initialize cell free list */
  install_mark_bits (ph);

  install_cell_free_list (ph);

#ifdef MEMORY_USAGE_STATS
  PLH_TOTAL_CELLS (plh) += PH_CELLS_ON_PAGE (ph);
  PLH_TOTAL_SPACE (plh) += PAGE_SIZE * PH_N_PAGES (ph);
#endif

  return ph;
}


/* Cleans and frees a page, identified by the given page_header. */
static void
remove_page_from_used_list (page_header *ph)
{
  page_list_header *plh = PH_PLH (ph);

  assert (!(gc_in_progress && PH_PROTECTION_BIT (ph)));
  /* cleanup: remove memory protection, zero page_header bits. */

#ifdef MEMORY_USAGE_STATS
  PLH_TOTAL_CELLS (plh) -= PH_CELLS_ON_PAGE (ph);
  PLH_TOTAL_SPACE (plh) -= PAGE_SIZE * PH_N_PAGES (ph);
#endif

  /* clean up mark bits and cell free list */
  remove_cell_free_list (ph);
  if (PH_ON_USED_LIST_P (ph))
    remove_mark_bits (ph);

  /* clean up page header */
  PH_CELL_SIZE (ph) = 0;
  PH_CELLS_ON_PAGE (ph) = 0;
  PH_CELLS_USED (ph) = 0;

  /* remove from used list */
  remove_page_header_from_plh (ph, plh);

  /* move to free list */
  merge_into_free_list (ph);
}




/*--- allocation -------------------------------------------------------*/

/* Allocates from cell free list on already allocated pages. */
static page_header *
allocate_cell (page_list_header *plh)
{
  page_header *ph = PLH_FIRST (plh);
  if (ph)
    {
      if (PH_FREE_LIST (ph))
        /* elements free on first page */
        return ph;
      else if ((PH_NEXT (ph))
               && (PH_FREE_LIST (PH_NEXT (ph))))
        /* elements free on second page */
        {
          page_header *temp = PH_NEXT (ph);
          /* move full page (first page) to end of list */
          PH_NEXT (PLH_LAST (plh)) = ph;
          PH_PREV (ph) = PLH_LAST (plh);
          PLH_LAST (plh) = ph;
          PH_NEXT (ph) = 0;
          /* install second page as first page */
          ph = temp;
          PH_PREV (ph) = 0;
          PLH_FIRST (plh) = ph;
          return ph;
        }
    }
  return 0;
}


/* Finds a page which has at least the needed number of pages.
   Algorithm: FIRST FIT. */
static page_header *
find_free_page_first_fit (EMACS_INT needed_pages, page_header *ph)
{
  while (ph)
    {
      if (PH_N_PAGES (ph) >= needed_pages)
        return ph;
      ph = PH_NEXT (ph);
    }
  return 0;
}


/* Allocates a page from the free list. */
static page_header *
allocate_page_from_free_list (EMACS_INT needed_pages)
{
  page_header *ph = 0;
  EMACS_INT i;
  for (i = get_free_list_index (needed_pages); i < N_FREE_PAGE_LISTS; i++)
    if ((ph = find_free_page_first_fit (needed_pages,
                                        PLH_FIRST (FREE_HEAP_PAGES (i)))) != 0)
      {
        if (PH_N_PAGES (ph) > needed_pages)
          return split_page (ph, needed_pages);
        else
          {
            remove_page_from_free_list (ph);
            return ph;
          }
      }
  return 0;
}


/* Allocates a new page, either from free list or by expanding the heap. */
static page_header *
allocate_new_page (page_list_header *plh, size_t size, EMACS_INT elemcount)
{
  EMACS_INT needed_pages = BYTES_TO_PAGES (size * elemcount);
  /* first check free list */
  page_header *result = allocate_page_from_free_list (needed_pages);
  if (!result)
    /* expand heap */
    result = expand_heap (needed_pages);
  install_page_in_used_list (result, plh, size, elemcount);
  return result;
}


/* Selects the correct size class, tries to allocate a cell of this size
   from the free list, if this fails, a new page is allocated. */
static void *
mc_alloc_1 (size_t size, EMACS_INT elemcount)
{
  page_list_header *plh = 0;
  page_header *ph = 0;
  void *result = 0;

  if (size == 0)
    return 0;

  if (elemcount == 1) 
    {
      plh = USED_HEAP_PAGES (get_used_list_index (size));
      if (size < (size_t) USED_LIST_UPPER_THRESHOLD)
	/* first check any free cells */
	ph = allocate_cell (plh);
    } 
  else 
    {
      plh = ARRAY_HEAP_PAGES;
    }

  if (!ph)
    /* allocate a new page */
    ph = allocate_new_page (plh, size, elemcount);

  /* return first element of free list and remove it from the list */
  result = (void*) PH_FREE_LIST (ph);
  PH_FREE_LIST (ph) =
    NEXT_FREE (PH_FREE_LIST (ph));

  memset (result, '\0', (size * elemcount));
  MARK_LRECORD_AS_FREE (result);

  /* bump used cells counter */
  PH_CELLS_USED (ph) += elemcount;

#ifdef MEMORY_USAGE_STATS
  PLH_USED_CELLS (plh) += elemcount;
  PLH_USED_SPACE (plh) += size * elemcount;
#endif

  return result;
}

/* Array allocation. */
void *
mc_alloc_array (size_t size, EMACS_INT elemcount)
{
  return mc_alloc_1 (size, elemcount);
}

void *
mc_alloc (size_t size)
{
  return mc_alloc_1 (size, 1);
}



/*--- sweep & free & finalize-------------------------------------------*/

/* Frees a heap pointer. */
static void
remove_cell (void *ptr, page_header *ph)
{
#ifdef MEMORY_USAGE_STATS
  PLH_USED_CELLS (PH_PLH (ph))--;
  if (PH_ON_USED_LIST_P (ph))
    PLH_USED_SPACE (PH_PLH (ph)) -= 
      detagged_lisp_object_size ((const struct lrecord_header *) ptr);
  else
    PLH_USED_SPACE (PH_PLH (ph)) -= PH_CELL_SIZE (ph);
#endif
  if (PH_ON_USED_LIST_P (ph)) 
    {
#ifdef ALLOC_TYPE_STATS
      dec_lrecord_stats (PH_CELL_SIZE (ph), 
			 (const struct lrecord_header *) ptr);
#endif /* ALLOC_TYPE_STATS */
#ifdef ERROR_CHECK_GC
      assert (!LRECORD_FREE_P (ptr));
      deadbeef_memory (ptr, PH_CELL_SIZE (ph));
      MARK_LRECORD_AS_FREE (ptr);
#endif
    }

  /* hooks cell into free list */
  NEXT_FREE (ptr) = PH_FREE_LIST (ph);
  PH_FREE_LIST (ph) = FREE_LIST (ptr);
  /* decrease cells used */
  PH_CELLS_USED (ph)--;
}


/* Mark free list marks all free list entries. */
static void
mark_free_list (page_header *ph)
{
  free_link *fl = PH_FREE_LIST (ph);
  while (fl)
    {
      SET_BIT (ph, get_mark_bit_index (fl, ph), BLACK);
      fl = NEXT_FREE (fl);
    }
}


/* Finalize a page for disksave. XEmacs calls this routine before it
   dumps the heap image. You have to tell mc-alloc how to call your
   object's finalizer for disksave. Therefore, you have to define the
   macro MC_ALLOC_CALL_FINALIZER_FOR_DISKSAVE(ptr). This macro should
   do nothing else then test if there is a finalizer and call it on
   the given argument, which is the heap address of the object.
   Returns number of processed pages. */
static EMACS_INT
finalize_page_for_disksave (page_header *ph)
{
  EMACS_INT heap_space = (EMACS_INT) PH_HEAP_SPACE (ph);
  EMACS_INT heap_space_step = PH_CELL_SIZE (ph);
  EMACS_INT mark_bit = 0;
  EMACS_INT mark_bit_max_index = PH_CELLS_ON_PAGE (ph);

  for (mark_bit = 0; mark_bit < mark_bit_max_index; mark_bit++)
    {
      EMACS_INT ptr = (heap_space + (heap_space_step * mark_bit));
      MC_ALLOC_CALL_FINALIZER_FOR_DISKSAVE ((void *) ptr);
    }
  return 1;
}


/* Finalizes the heap for disksave.  Returns number of processed
   pages. */
EMACS_INT
mc_finalize_for_disksave (void)
{
  return visit_all_used_page_headers (finalize_page_for_disksave);
}


/* Sweeps a page: all the non-marked cells are freed. If the page is
   empty in the end, it is removed. If some cells are free, it is
   moved to the front of its page header list. Full pages stay where
   they are.  Returns number of processed pages.*/
static EMACS_INT
sweep_page (page_header *ph)
{
  Rawbyte *heap_space = (Rawbyte *) PH_HEAP_SPACE (ph);
  EMACS_INT heap_space_step = PH_CELL_SIZE (ph);
  EMACS_INT mark_bit = 0;
  EMACS_INT mark_bit_max_index = PH_CELLS_ON_PAGE (ph);
  unsigned int bit = 0;

  mark_free_list (ph);

  /* ARRAY_BIT_HACK */
  if (PH_ARRAY_BIT (ph))
    for (mark_bit = 0; mark_bit < mark_bit_max_index; mark_bit++)
      {
	GET_BIT (bit, ph, mark_bit * N_MARK_BITS);
	if (bit)
	  {
	    zero_mark_bits (ph);
	    PH_BLACK_BIT (ph) = 0;
	    return 1;
	  }
      }

  for (mark_bit = 0; mark_bit < mark_bit_max_index; mark_bit++)
    {
      GET_BIT (bit, ph, mark_bit * N_MARK_BITS);
      if (bit == WHITE) 
	{
	  GC_STAT_FREED;
	  remove_cell (heap_space + (heap_space_step * mark_bit), ph);
	}
    }
  zero_mark_bits (ph);
  PH_BLACK_BIT (ph) = 0;
  if (PH_CELLS_USED (ph) == 0)
    remove_page_from_used_list (ph);
  else if (PH_CELLS_USED (ph) < PH_CELLS_ON_PAGE (ph))
    move_page_header_to_front (ph);

  return 1;
}


/* Sweeps the heap.  Returns number of processed pages. */
EMACS_INT
mc_sweep (void)
{
  return visit_all_used_page_headers (sweep_page);
}


/* Changes the size of the cell pointed to by ptr.
   Returns the new address of the new cell with new size. */
static void *
mc_realloc_1 (void *ptr, size_t size, int elemcount)
{
  if (ptr)
    {
      if (size * elemcount)
	{
	  void *result = mc_alloc_1 (size, elemcount);
	  size_t from_size = PH_CELL_SIZE (get_page_header (ptr));
	  size_t cpy_size = size * elemcount;
	  if (cpy_size > from_size) 
	    cpy_size = from_size;
	  memcpy (result, ptr, cpy_size);
#ifdef ALLOC_TYPE_STATS
	  inc_lrecord_stats (size, (struct lrecord_header *) result);
#endif /* not ALLOC_TYPE_STATS */
	  return result;
	}
      else
	{
	  return 0;
	}
    }
  else
    return mc_alloc_1 (size, elemcount);
}

void *
mc_realloc (void *ptr, size_t size)
{
  return mc_realloc_1 (ptr, size, 1);
}

void *
mc_realloc_array (void *ptr, size_t size, EMACS_INT elemcount)
{
  return mc_realloc_1 (ptr, size, elemcount);
}



/*--- initialization ---------------------------------------------------*/

/* Call once at the very beginning. */
void
init_mc_allocator (void)
{
  EMACS_INT i;

#ifdef MEMORY_USAGE_STATS
  MC_MALLOCED_BYTES = 0;
#endif

  /* init of pagesize dependent values */
  switch (SYS_PAGE_SIZE)
    {
    case   512: log_page_size =  9; break;
    case  1024: log_page_size = 10; break;
    case  2048: log_page_size = 11; break;
    case  4096: log_page_size = 12; break;
    case  8192: log_page_size = 13; break;
    case 16384: log_page_size = 14; break;
    case 32768: log_page_size = 15; break;
    case 65536: log_page_size = 16; break;
    default: 
      fprintf(stderr, "##### SYS_PAGE_SIZE=%d not supported #####\n",
	      SYS_PAGE_SIZE);
      ABORT ();
    }
  
  page_size_div_2 = (EMACS_UINT) SYS_PAGE_SIZE >> 1;
  
  mc_allocator_globals.used_heap_pages = 
    (page_list_header *) xmalloc_and_zero ((N_USED_PAGE_LISTS + 1)
					   * sizeof (page_list_header));
#ifdef MEMORY_USAGE_STATS
  MC_MALLOCED_BYTES += (N_USED_PAGE_LISTS + 1) * sizeof (page_list_header);
#endif

  mc_allocator_globals.ptr_lookup_table = 
    (level_2_lookup_tree **) 
    xmalloc_and_zero ((LEVEL1_SIZE + 1) * sizeof (level_2_lookup_tree *));
#ifdef MEMORY_USAGE_STATS
  MC_MALLOCED_BYTES += (LEVEL1_SIZE + 1) * sizeof (level_2_lookup_tree *);
#endif
				
#ifdef BLOCKTYPE_ALLOC_PAGE_HEADER
  the_page_header_blocktype = Blocktype_new (struct page_header_blocktype);
#endif /* BLOCKTYPE_ALLOC_PAGE_HEADER */

  for (i = 0; i < N_USED_PAGE_LISTS; i++)
    {
      page_list_header *plh = USED_HEAP_PAGES (i);
      PLH_LIST_TYPE (plh) = USED_LIST;
      PLH_SIZE (plh) = get_used_list_size_value (i);
      PLH_FIRST (plh) = 0;
      PLH_LAST (plh) = 0;
      PLH_MARK_BIT_FREE_LIST (plh) = 0;
#ifdef MEMORY_USAGE_STATS
      PLH_PAGE_COUNT (plh) = 0;
      PLH_USED_CELLS (plh) = 0;
      PLH_USED_SPACE (plh) = 0;
      PLH_TOTAL_CELLS (plh) = 0;
      PLH_TOTAL_SPACE (plh) = 0;
#endif
    }

  {
    page_list_header *plh = ARRAY_HEAP_PAGES;
    PLH_LIST_TYPE (plh) = USED_LIST;
    PLH_SIZE (plh) = 0;
    PLH_FIRST (plh) = 0;
    PLH_LAST (plh) = 0;
    PLH_MARK_BIT_FREE_LIST (plh) = 0;
#ifdef MEMORY_USAGE_STATS
    PLH_PAGE_COUNT (plh) = 0;
    PLH_USED_CELLS (plh) = 0;
    PLH_USED_SPACE (plh) = 0;
    PLH_TOTAL_CELLS (plh) = 0;
    PLH_TOTAL_SPACE (plh) = 0;
#endif
  }

  for (i = 0; i < N_FREE_PAGE_LISTS; i++)
    {
      page_list_header *plh = FREE_HEAP_PAGES (i);
      PLH_LIST_TYPE (plh) = FREE_LIST;
      PLH_SIZE (plh) = get_free_list_size_value (i);
      PLH_FIRST (plh) = 0;
      PLH_LAST (plh) = 0;
      PLH_MARK_BIT_FREE_LIST (plh) = 0;
#ifdef MEMORY_USAGE_STATS
      PLH_PAGE_COUNT (plh) = 0;
      PLH_USED_CELLS (plh) = 0;
      PLH_USED_SPACE (plh) = 0;
      PLH_TOTAL_CELLS (plh) = 0;
      PLH_TOTAL_SPACE (plh) = 0;
#endif
    }

#ifndef BLOCKTYPE_ALLOC_PAGE_HEADER
  PAGE_HEADER_FREE_LIST = 0;
#endif /* not BLOCKTYPE_ALLOC_PAGE_HEADER */

#ifdef MEMORY_USAGE_STATS
  MC_MALLOCED_BYTES += sizeof (mc_allocator_globals);
#endif

  init_lookup_table ();
}




/*--- lisp function for statistics -------------------------------------*/

#ifdef MEMORY_USAGE_STATS
DEFUN ("mc-alloc-memory-usage", Fmc_alloc_memory_usage, 0, 0, 0, /*
Returns stats about the mc-alloc memory usage. See diagnose.el.
*/
       ())
{
  Lisp_Object free_plhs = Qnil;
  Lisp_Object used_plhs = Qnil;
  Lisp_Object heap_sects = Qnil;
  EMACS_INT used_size = 0;
  EMACS_INT real_size = 0;

  EMACS_INT i;

  for (i = 0; i < N_FREE_PAGE_LISTS; i++) 
    if (PLH_PAGE_COUNT (FREE_HEAP_PAGES(i)) > 0)
      free_plhs = 
	Facons (make_int (PLH_SIZE (FREE_HEAP_PAGES(i))),
		list1 (make_int (PLH_PAGE_COUNT (FREE_HEAP_PAGES(i)))),
		free_plhs);

  for (i = 0; i < N_USED_PAGE_LISTS; i++) 
    if (PLH_PAGE_COUNT (USED_HEAP_PAGES(i)) > 0)
      used_plhs = 
	Facons (make_int (PLH_SIZE (USED_HEAP_PAGES(i))),
		list5 (make_int (PLH_PAGE_COUNT (USED_HEAP_PAGES(i))),
		       make_int (PLH_USED_CELLS (USED_HEAP_PAGES(i))),
		       make_int (PLH_USED_SPACE (USED_HEAP_PAGES(i))),
		       make_int (PLH_TOTAL_CELLS (USED_HEAP_PAGES(i))),
		       make_int (PLH_TOTAL_SPACE (USED_HEAP_PAGES(i)))),
		used_plhs);

  used_plhs =
    Facons (make_int (0),
	    list5 (make_int (PLH_PAGE_COUNT(ARRAY_HEAP_PAGES)),
		   make_int (PLH_USED_CELLS (ARRAY_HEAP_PAGES)),
		   make_int (PLH_USED_SPACE (ARRAY_HEAP_PAGES)),
		   make_int (PLH_TOTAL_CELLS (ARRAY_HEAP_PAGES)),
		   make_int (PLH_TOTAL_SPACE (ARRAY_HEAP_PAGES))),
	    used_plhs);
  
  for (i = 0; i < N_HEAP_SECTIONS; i++) {
    used_size += HEAP_SECTION(i).n_pages * PAGE_SIZE;
    real_size += 
      malloced_storage_size (0, HEAP_SECTION(i).real_size, 0);
  }

  heap_sects =
    list3 (make_int (N_HEAP_SECTIONS),
	   make_int (used_size),
	   make_int (real_size));

  return Fcons (make_int (PAGE_SIZE), 
		list5 (heap_sects, 
		       Fnreverse (used_plhs), 
		       Fnreverse (free_plhs), 
		       make_int (sizeof (mc_allocator_globals)), 
		       make_int (MC_MALLOCED_BYTES)));
}
#endif /* MEMORY_USAGE_STATS */

void
syms_of_mc_alloc (void)
{
#ifdef MEMORY_USAGE_STATS
  DEFSUBR (Fmc_alloc_memory_usage);
#endif /* MEMORY_USAGE_STATS */
}


/*--- incremental garbage collector ----------------------------------*/

#if 0 /* currently unused */

/* access dirty bit of page header */
static void
set_dirty_bit (page_header *ph, unsigned int value)
{
  PH_DIRTY_BIT (ph) = value;
}

static void
set_dirty_bit_for_address (void *ptr, unsigned int value)
{
  set_dirty_bit (get_page_header (ptr), value);
}

static unsigned int
get_dirty_bit (page_header *ph)
{
  return PH_DIRTY_BIT (ph);
}

static unsigned int
get_dirty_bit_for_address (void *ptr)
{
  return get_dirty_bit (get_page_header (ptr));
}


/* access protection bit of page header */
static void
set_protection_bit (page_header *ph, unsigned int value)
{
  PH_PROTECTION_BIT (ph) = value;
}

static void
set_protection_bit_for_address (void *ptr, unsigned int value)
{
  set_protection_bit (get_page_header (ptr), value);
}

static unsigned int
get_protection_bit (page_header *ph)
{
  return PH_PROTECTION_BIT (ph);
}

static unsigned int
get_protection_bit_for_address (void *ptr)
{
  return get_protection_bit (get_page_header (ptr));
}


/* Returns the start of the page of the object pointed to by ptr. */
static void *
get_page_start (void *ptr)
{
  return PH_HEAP_SPACE (get_page_header (ptr));
}

#endif /* 0 */

/* Make PAGE_SIZE globally available. */
EMACS_INT
mc_get_page_size ()
{
  return PAGE_SIZE;
}

/* Is the fault at ptr on a protected page? */
EMACS_INT
fault_on_protected_page (void *ptr)
{
  page_header *ph = get_page_header_internal (ptr);
  return (ph
	  && PH_HEAP_SPACE (ph)
	  && (PH_HEAP_SPACE (ph) <= ptr) 
	  && ((void *) ((EMACS_INT) PH_HEAP_SPACE (ph) 
			+ PH_N_PAGES (ph) * PAGE_SIZE) > ptr)
	  && (PH_PROTECTION_BIT (ph) == 1));
}


/* Protect the heap page of given page header ph if black objects are
   on the page.  Returns number of processed pages. */
static EMACS_INT
protect_heap_page (page_header *ph)
{
  if (PH_BLACK_BIT (ph))
    {
      void *heap_space = PH_HEAP_SPACE (ph);
      EMACS_INT heap_space_size = PH_N_PAGES (ph) * PAGE_SIZE;
      vdb_protect ((void *) heap_space, heap_space_size);
      PH_PROTECTION_BIT (ph) = 1;
      return 1;
    }
  return 0;
}

/* Protect all heap pages with black objects.  Returns number of
   processed pages.*/
EMACS_INT
protect_heap_pages (void)
{
  return visit_all_used_page_headers (protect_heap_page);
}


/* Remove protection (if there) of heap page of given page header
   ph.  Returns number of processed pages. */
static EMACS_INT
unprotect_heap_page (page_header *ph)
{
  if (PH_PROTECTION_BIT (ph))
    {
      void *heap_space = PH_HEAP_SPACE (ph);
      EMACS_INT heap_space_size = PH_N_PAGES (ph) * PAGE_SIZE;
      vdb_unprotect (heap_space, heap_space_size);
      PH_PROTECTION_BIT (ph) = 0;
      return 1;
    }
  return 0;
}

/* Remove protection for all heap pages which are protected.  Returns
   number of processed pages. */
EMACS_INT
unprotect_heap_pages (void)
{
  return visit_all_used_page_headers (unprotect_heap_page);
}

/* Remove protection and mark page dirty. */
void
unprotect_page_and_mark_dirty (void *ptr)
{
  page_header *ph = get_page_header (ptr);
  unprotect_heap_page (ph);
  PH_DIRTY_BIT (ph) = 1;
}

/* Repush all objects on dirty pages onto the mark stack. */
int
repush_all_objects_on_page (void *ptr)
{
  int repushed_objects = 0;
  page_header *ph = get_page_header (ptr);
  Rawbyte *heap_space = (Rawbyte *) PH_HEAP_SPACE (ph);
  EMACS_INT heap_space_step = PH_CELL_SIZE (ph);
  EMACS_INT mark_bit = 0;
  EMACS_INT mark_bit_max_index = PH_CELLS_ON_PAGE (ph);
  unsigned int bit = 0;
  for (mark_bit = 0; mark_bit < mark_bit_max_index; mark_bit++)
    {
      GET_BIT (bit, ph, mark_bit * N_MARK_BITS);
      if (bit == BLACK)
	{
	  repushed_objects++;
	  gc_write_barrier 
	    (wrap_pointer_1 ((heap_space + (heap_space_step * mark_bit))));
	}
    }
  PH_BLACK_BIT (ph) = 0;
  PH_DIRTY_BIT (ph) = 0;
  return repushed_objects;
}

/* Mark black if object is currently grey.  This first checks, if the
   object is really allocated on the mc-heap.  If it is, it can be
   marked black; if it is not, it cannot be marked. */
EMACS_INT
maybe_mark_black (void *ptr)
{
  page_header *ph = get_page_header_internal (ptr);
  unsigned int bit = 0;

  if (ph && PH_PLH (ph) && PH_ON_USED_LIST_P (ph))
    {
      GET_BIT (bit, ph, get_mark_bit_index (ptr, ph));
      if (bit == GREY)
	{
	  if (!PH_BLACK_BIT (ph))
	    PH_BLACK_BIT (ph) = 1;
	  SET_BIT (ph, get_mark_bit_index (ptr, ph), BLACK);
	}
      return 1;
    }
  return 0;
}

/* Only for debugging --- not used anywhere in the sources. */
EMACS_INT
object_on_heap_p (void *ptr)
{
  page_header *ph = get_page_header_internal (ptr);
  return (ph && PH_ON_USED_LIST_P (ph));
}
