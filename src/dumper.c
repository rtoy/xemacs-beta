/* Portable data dumper for XEmacs.
   Copyright (C) 1999-2000 Olivier Galibert
   Copyright (C) 2001 Martin Buchholz
   Copyright (C) 2001 Ben Wing.

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

/* !!#### Not yet Mule-ized */

#include <config.h>
#include "lisp.h"

#include "specifier.h"
#include "file-coding.h"
#include "elhash.h"
#include "sysfile.h"
#include "console-stream.h"
#include "dumper.h"

#ifdef WIN32_NATIVE
#include "syswindows.h"
#else
#ifdef HAVE_MMAP
#include <sys/mman.h>
#endif
#endif

typedef struct
{
  const void *varaddress;
  Bytecount size;
} pdump_opaque;

typedef struct
{
  Dynarr_declare (pdump_opaque);
} pdump_opaque_dynarr;

typedef struct
{
  void **ptraddress;
  const struct struct_description *desc;
} pdump_root_struct_ptr;

typedef struct
{
  Dynarr_declare (pdump_root_struct_ptr);
} pdump_root_struct_ptr_dynarr;

typedef struct
{
  Lisp_Object *address;
  Lisp_Object value;
} pdump_static_Lisp_Object;

typedef struct
{
  char **address; /* char * for ease of doing relocation */
  char * value;
} pdump_static_pointer;

static pdump_opaque_dynarr *pdump_opaques;
static pdump_root_struct_ptr_dynarr *pdump_root_struct_ptrs;
static Lisp_Object_ptr_dynarr *pdump_root_objects;
static Lisp_Object_ptr_dynarr *pdump_weak_object_chains;

/* Mark SIZE bytes at non-heap address VARADDRESS for dumping as is,
   without any bit-twiddling. */
void
dump_add_opaque (const void *varaddress, Bytecount size)
{
  pdump_opaque info;
  info.varaddress = varaddress;
  info.size = size;
  if (pdump_opaques == NULL)
    pdump_opaques = Dynarr_new (pdump_opaque);
  Dynarr_add (pdump_opaques, info);
}

/* Mark the struct described by DESC and pointed to by the pointer at
   non-heap address VARADDRESS for dumping.
   All the objects reachable from this pointer will also be dumped. */
void
dump_add_root_struct_ptr (void *ptraddress,
			  const struct struct_description *desc)
{
  pdump_root_struct_ptr info;
  info.ptraddress = (void **) ptraddress;
  info.desc = desc;
  if (pdump_root_struct_ptrs == NULL)
    pdump_root_struct_ptrs = Dynarr_new (pdump_root_struct_ptr);
  Dynarr_add (pdump_root_struct_ptrs, info);
}

/* Mark the Lisp_Object at non-heap address VARADDRESS for dumping.
   All the objects reachable from this var will also be dumped. */
void
dump_add_root_object (Lisp_Object *varaddress)
{
  if (pdump_root_objects == NULL)
    pdump_root_objects = Dynarr_new2 (Lisp_Object_ptr_dynarr, Lisp_Object *);
  Dynarr_add (pdump_root_objects, varaddress);
}

/* Mark the list pointed to by the Lisp_Object at VARADDRESS for dumping. */
void
dump_add_weak_object_chain (Lisp_Object *varaddress)
{
  if (pdump_weak_object_chains == NULL)
    pdump_weak_object_chains = Dynarr_new2 (Lisp_Object_ptr_dynarr, Lisp_Object *);
  Dynarr_add (pdump_weak_object_chains, varaddress);
}


inline static void
pdump_align_stream (FILE *stream, Bytecount alignment)
{
  long offset = ftell (stream);
  long adjustment = ALIGN_SIZE (offset, alignment) - offset;
  if (adjustment)
    fseek (stream, adjustment, SEEK_CUR);
}

#define PDUMP_ALIGN_OUTPUT(type) pdump_align_stream (pdump_out, ALIGNOF (type))

#define PDUMP_WRITE(type, object) \
retry_fwrite (&object, sizeof (object), 1, pdump_out);

#define PDUMP_WRITE_ALIGNED(type, object) do {	\
  PDUMP_ALIGN_OUTPUT (type);			\
  PDUMP_WRITE (type, object);			\
} while (0)

#define PDUMP_READ(ptr, type) \
(((type *) (ptr = (char*) (((type *) ptr) + 1)))[-1])

#define PDUMP_READ_ALIGNED(ptr, type) \
((ptr = (char *) ALIGN_PTR (ptr, ALIGNOF (type))), PDUMP_READ (ptr, type))



typedef struct
{
  const struct lrecord_description *desc;
  int count;
} pdump_reloc_table;

static char *pdump_rt_list = 0;

void
pdump_objects_unmark (void)
{
  int i;
  char *p = pdump_rt_list;
  if (p)
    for (;;)
      {
	pdump_reloc_table *rt = (pdump_reloc_table *)p;
	p += sizeof (pdump_reloc_table);
	if (rt->desc)
	  {
	    for (i=0; i<rt->count; i++)
	      {
		struct lrecord_header *lh = * (struct lrecord_header **) p;
		if (! C_READONLY_RECORD_HEADER_P (lh))
		  UNMARK_RECORD_HEADER (lh);
		p += sizeof (EMACS_INT);
	      }
	  } else
	    break;
      }
}


/* The structure of the file
 0		- header
		- dumped objects
 stab_offset	- nb_root_struct_ptrs*pair(void *, adr)
		  for pointers to structures
		- nb_opaques*pair(void *, size) for raw bits to restore
		- relocation table
		- root lisp object address/value couples with the count
		  preceding the list
 */


#define PDUMP_SIGNATURE "XEmacsDP"
#define PDUMP_SIGNATURE_LEN (sizeof (PDUMP_SIGNATURE) - 1)

typedef struct
{
  char signature[PDUMP_SIGNATURE_LEN];
  unsigned int id;
  EMACS_UINT stab_offset;
  EMACS_UINT reloc_address;
  int nb_root_struct_ptrs;
  int nb_opaques;
} pdump_header;

char *pdump_start;
char *pdump_end;
static Bytecount pdump_length;

#ifdef WIN32_NATIVE
/* Handle for the dump file */
static HANDLE pdump_hFile = INVALID_HANDLE_VALUE;
/* Handle for the file mapping object for the dump file */
static HANDLE pdump_hMap = INVALID_HANDLE_VALUE;
#endif

static void (*pdump_free) (void);

static unsigned char pdump_align_table[] =
{
  64, 1, 2, 1, 4, 1, 2, 1, 8, 1, 2, 1, 4, 1, 2, 1,
  16, 1, 2, 1, 4, 1, 2, 1, 8, 1, 2, 1, 4, 1, 2, 1,
  32, 1, 2, 1, 4, 1, 2, 1, 8, 1, 2, 1, 4, 1, 2, 1,
  16, 1, 2, 1, 4, 1, 2, 1, 8, 1, 2, 1, 4, 1, 2, 1
};

static inline int
pdump_size_to_align (Bytecount size)
{
  return pdump_align_table[size % countof (pdump_align_table)];
}

typedef struct pdump_entry_list_elt
{
  struct pdump_entry_list_elt *next;
  const void *obj;
  Bytecount size;
  int count;
  EMACS_INT save_offset;
} pdump_entry_list_elt;

typedef struct
{
  pdump_entry_list_elt *first;
  int align;
  int count;
} pdump_entry_list;

typedef struct pdump_struct_list_elt
{
  pdump_entry_list list;
  const struct struct_description *sdesc;
} pdump_struct_list_elt;

typedef struct
{
  pdump_struct_list_elt *list;
  int count;
  int size;
} pdump_struct_list;

static pdump_entry_list *pdump_object_table;
static pdump_entry_list pdump_opaque_data_list;
static pdump_struct_list pdump_struct_table;

static int *pdump_alert_undump_object;

static unsigned long cur_offset;
static Bytecount max_size;
static int pdump_fd;
static void *pdump_buf;
static FILE *pdump_out;

#define PDUMP_HASHSIZE 200001

static pdump_entry_list_elt **pdump_hash;

/* Since most pointers are eight bytes aligned, the >>3 allows for a better hash */
static int
pdump_make_hash (const void *obj)
{
  return ((unsigned long)(obj)>>3) % PDUMP_HASHSIZE;
}

static pdump_entry_list_elt *
pdump_get_entry (const void *obj)
{
  int pos = pdump_make_hash (obj);
  pdump_entry_list_elt *e;

  assert (obj != 0);

  while ((e = pdump_hash[pos]) != 0)
    {
      if (e->obj == obj)
	return e;

      pos++;
      if (pos == PDUMP_HASHSIZE)
	pos = 0;
    }
  return 0;
}

static void
pdump_add_entry (pdump_entry_list *list, const void *obj, Bytecount size,
		 int count)
{
  pdump_entry_list_elt *e;
  int pos = pdump_make_hash (obj);

  while ((e = pdump_hash[pos]) != 0)
    {
      if (e->obj == obj)
	return;

      pos++;
      if (pos == PDUMP_HASHSIZE)
	pos = 0;
    }

  e = xnew (pdump_entry_list_elt);

  e->next = list->first;
  e->obj = obj;
  e->size = size;
  e->count = count;
  list->first = e;

  list->count += count;
  pdump_hash[pos] = e;

  {
    int align = pdump_size_to_align (size);

    if (align < list->align)
      list->align = align;
  }
}

static pdump_entry_list *
pdump_get_entry_list (const struct struct_description *sdesc)
{
  int i;
  for (i=0; i<pdump_struct_table.count; i++)
    if (pdump_struct_table.list[i].sdesc == sdesc)
      return &pdump_struct_table.list[i].list;

  if (pdump_struct_table.size <= pdump_struct_table.count)
    {
      if (pdump_struct_table.size == -1)
	pdump_struct_table.size = 10;
      else
	pdump_struct_table.size = pdump_struct_table.size * 2;
      pdump_struct_table.list = (pdump_struct_list_elt *)
	xrealloc (pdump_struct_table.list,
		  pdump_struct_table.size * sizeof (pdump_struct_list_elt));
    }
  pdump_struct_table.list[pdump_struct_table.count].list.first = 0;
  pdump_struct_table.list[pdump_struct_table.count].list.align = ALIGNOF (max_align_t);
  pdump_struct_table.list[pdump_struct_table.count].list.count = 0;
  pdump_struct_table.list[pdump_struct_table.count].sdesc = sdesc;

  return &pdump_struct_table.list[pdump_struct_table.count++].list;
}

static struct
{
  struct lrecord_header *obj;
  int position;
  int offset;
} backtrace[65536];

static int depth;

static void
pdump_backtrace (void)
{
  int i;
  stderr_out ("pdump backtrace :\n");
  for (i = 0; i < depth; i++)
    {
      if (!backtrace[i].obj)
	stderr_out ("  - ind. (%d, %d)\n",
		    backtrace[i].position,
		    backtrace[i].offset);
      else
	{
	  stderr_out ("  - %s (%d, %d)\n",
		   LHEADER_IMPLEMENTATION (backtrace[i].obj)->name,
		   backtrace[i].position,
		   backtrace[i].offset);
	}
    }
}

static void pdump_register_object (Lisp_Object obj);
static void pdump_register_struct_contents (const void *data,
					    const struct struct_description *
					    sdesc,
					    int count);
static void pdump_register_struct (const void *data,
				   const struct struct_description *sdesc,
				   int count);

static EMACS_INT
pdump_get_indirect_count (EMACS_INT code,
			  const struct lrecord_description *idesc,
			  const void *idata)
{
  EMACS_INT count;
  const void *irdata;

  int line = XD_INDIRECT_VAL (code);
  int delta = XD_INDIRECT_DELTA (code);

  irdata = ((char *)idata) + idesc[line].offset;
  switch (idesc[line].type)
    {
    case XD_BYTECOUNT:
      count = *(Bytecount *)irdata;
      break;
    case XD_ELEMCOUNT:
      count = *(Elemcount *)irdata;
      break;
    case XD_HASHCODE:
      count = *(Hashcode *)irdata;
      break;
    case XD_INT:
      count = *(int *)irdata;
      break;
    case XD_LONG:
      count = *(long *)irdata;
      break;
    default:
      stderr_out ("Unsupported count type : %d (line = %d, code=%ld)\n",
		  idesc[line].type, line, (long)code);
      pdump_backtrace ();
      count = 0; /* warning suppression */
      abort ();
    }
  count += delta;
  return count;
}

static void
pdump_register_sub (const void *data, const struct lrecord_description *desc, int me)
{
  int pos;

 restart:
  for (pos = 0; desc[pos].type != XD_END; pos++)
    {
      const void *rdata = (const char *)data + desc[pos].offset;

      backtrace[me].position = pos;
      backtrace[me].offset = desc[pos].offset;

      switch (desc[pos].type)
	{
	case XD_SPECIFIER_END:
	  pos = 0;
	  desc = ((const Lisp_Specifier *)data)->methods->extra_description;
	  goto restart;
	case XD_CODING_SYSTEM_END:
	  pos = 0;
	  desc =
	    ((const Lisp_Coding_System *)data)->methods->extra_description;
	  goto restart;
	case XD_BYTECOUNT:
	case XD_ELEMCOUNT:
	case XD_HASHCODE:
	case XD_INT:
	case XD_LONG:
	case XD_INT_RESET:
	case XD_LO_LINK:
	  break;
	case XD_OPAQUE_DATA_PTR:
	  {
	    EMACS_INT count = desc[pos].data1;
	    if (XD_IS_INDIRECT (count))
	      count = pdump_get_indirect_count (count, desc, data);

	    pdump_add_entry (&pdump_opaque_data_list,
			     *(void **)rdata, count, 1);
	    break;
	  }
	case XD_C_STRING:
	  {
	    const char *str = *(const char **)rdata;
	    if (str)
	      pdump_add_entry (&pdump_opaque_data_list, str, strlen (str)+1, 1);
	    break;
	  }
	case XD_DOC_STRING:
	  {
	    const char *str = *(const char **)rdata;
	    if ((EMACS_INT)str > 0)
	      pdump_add_entry (&pdump_opaque_data_list, str, strlen (str)+1, 1);
	    break;
	  }
	case XD_LISP_OBJECT:
	  {
	    const Lisp_Object *pobj = (const Lisp_Object *)rdata;

	    assert (desc[pos].data1 == 0);

	    backtrace[me].offset = (const char *)pobj - (const char *)data;
	    pdump_register_object (*pobj);
	    break;
	  }
	case XD_LISP_OBJECT_ARRAY:
	  {
	    int i;
	    EMACS_INT count = desc[pos].data1;
	    if (XD_IS_INDIRECT (count))
	      count = pdump_get_indirect_count (count, desc, data);

	    for (i = 0; i < count; i++)
	      {
		const Lisp_Object *pobj = ((const Lisp_Object *)rdata) + i;
		Lisp_Object dobj = *pobj;

		backtrace[me].offset = (const char *)pobj - (const char *)data;
		pdump_register_object (dobj);
	      }
	    break;
	  }
	case XD_STRUCT_PTR:
	  {
	    EMACS_INT count = desc[pos].data1;
	    const struct struct_description *sdesc = desc[pos].data2;
	    const char *dobj = *(const char **)rdata;
	    if (dobj)
	      {
		if (XD_IS_INDIRECT (count))
		  count = pdump_get_indirect_count (count, desc, data);

		pdump_register_struct (dobj, sdesc, count);
	      }
	    break;
	  }
	case XD_STRUCT_ARRAY:
	  {
	    EMACS_INT count = desc[pos].data1;
	    const struct struct_description *sdesc = desc[pos].data2;

	    if (XD_IS_INDIRECT (count))
	      count = pdump_get_indirect_count (count, desc, data);

	    pdump_register_struct_contents (rdata, sdesc, count);
	    break;
	  }
	case XD_UNION:
	  abort (); /* #### IMPLEMENT ME!  NEEDED FOR UNICODE SUPPORT */

	default:
	  stderr_out ("Unsupported dump type : %d\n", desc[pos].type);
	  pdump_backtrace ();
	  abort ();
	};
    }
}

static void
pdump_register_object (Lisp_Object obj)
{
  struct lrecord_header *objh;
  const struct lrecord_implementation *imp;

  if (!POINTER_TYPE_P (XTYPE (obj)))
    return;

  objh = XRECORD_LHEADER (obj);
  if (!objh)
    return;

  if (pdump_get_entry (objh))
    return;

  imp = LHEADER_IMPLEMENTATION (objh);

  if (imp->description)
    {
      int me = depth++;
      if (me > 65536)
	{
	  stderr_out ("Backtrace overflow, loop ?\n");
	  abort ();
	}
      backtrace[me].obj = objh;
      backtrace[me].position = 0;
      backtrace[me].offset = 0;

      pdump_add_entry (pdump_object_table + objh->type,
		       objh,
		       imp->static_size ?
		       imp->static_size :
		       imp->size_in_bytes_method (objh),
		       1);
      pdump_register_sub (objh, imp->description, me);
      --depth;
    }
  else
    {
      pdump_alert_undump_object[objh->type]++;
      stderr_out ("Undumpable object type : %s\n", imp->name);
      pdump_backtrace ();
    }
}

/* Return the size of the memory block (NOT necessarily a structure!) 
   described by SDESC and pointed to by OBJ.  If SDESC records an
   explicit size (i.e. non-zero), it is simply returned; otherwise,
   the size is calculated by the maximum offset and the size of the
   object at that offset, rounded up to the maximum alignment.  In
   this case, we may need the object, for example when retrieving an
   "indirect count" of an inlined array (the count is not constant,
   but is specified by one of the elements of the memory block). (It
   is generally not a problem if we return an overly large size -- we
   will simply end up reserving more space than necessary; but if the
   size is too small we could be in serious trouble, in particular
   with nested inlined structures, where there may be alignment
   padding in the middle of a block. #### In fact there is an (at
   least theoretical) problem with an overly large size -- we may
   trigger a protection fault when reading from invalid memory.  We
   need to handle this -- perhaps in a stupid but dependable way,
   i.e. by trapping SIGSEGV and SIGBUS.) */

static Bytecount
pdump_structure_size (const void *obj, const struct struct_description *sdesc)
{
  int max_offset = -1;
  int max_offset_pos = -1;
  int size_at_max = 0;
  int pos;
  const struct lrecord_description *desc;
  void *rdata;

  if (sdesc->size)
    return sdesc->size;

  desc = sdesc->description;

  for (pos = 0; desc[pos].type != XD_END; pos++)
    {
      if (desc[pos].offset == max_offset)
	{
	  stderr_out ("Two relocatable elements at same offset?\n");
	  abort ();
	}
      else if (desc[pos].offset > max_offset)
	{
	  max_offset = desc[pos].offset;
	  max_offset_pos = pos;
	}
    }

  if (max_offset_pos < 0)
    return 0;

  pos = max_offset_pos;
  rdata = (char *) obj + desc[pos].offset;

  switch (desc[pos].type)
    {
    case XD_LISP_OBJECT_ARRAY:
      {
	EMACS_INT val = desc[pos].data1;
	if (XD_IS_INDIRECT (val))
	  val = pdump_get_indirect_count (val, desc, obj);
	size_at_max = val * sizeof (Lisp_Object);
	break;
      }
    case XD_LISP_OBJECT:
    case XD_LO_LINK:
      size_at_max = sizeof (Lisp_Object);
      break;
    case XD_OPAQUE_PTR:
      size_at_max = sizeof (void *);
      break;
    case XD_STRUCT_PTR:
      {
	EMACS_INT val = desc[pos].data1;
	if (XD_IS_INDIRECT (val))
	  val = pdump_get_indirect_count (val, desc, obj);
	size_at_max = val * sizeof (void *);
	break;
      }
      break;
    case XD_STRUCT_ARRAY:
      {
	EMACS_INT val = desc[pos].data1;

	if (XD_IS_INDIRECT (val))
	  val = pdump_get_indirect_count (val, desc, obj);
	    
	size_at_max = val * pdump_structure_size (rdata, desc[pos].data2);
	break;
      }
      break;
    case XD_OPAQUE_DATA_PTR:
      size_at_max = sizeof (void *);
      break;
    case XD_UNION:
      abort ();			/* #### IMPLEMENT ME!  NEEDED FOR UNICODE
				   SUPPORT */
      break;
    case XD_C_STRING:
      size_at_max = sizeof (void *);
      break;
    case XD_DOC_STRING:
      size_at_max = sizeof (void *);
      break;
    case XD_INT_RESET:
      size_at_max = sizeof (int);
      break;
    case XD_BYTECOUNT:
      size_at_max = sizeof (Bytecount);
      break;
    case XD_ELEMCOUNT:
      size_at_max = sizeof (Elemcount);
      break;
    case XD_HASHCODE:
      size_at_max = sizeof (Hashcode);
      break;
    case XD_INT:
      size_at_max = sizeof (int);
      break;
    case XD_LONG:
      size_at_max = sizeof (long);
      break;
    case XD_SPECIFIER_END:
    case XD_CODING_SYSTEM_END:
      stderr_out
	("Should not be seeing XD_SPECIFIER_END or\n"
	 "XD_CODING_SYSTEM_END outside of struct Lisp_Specifier\n"
	 "and struct Lisp_Coding_System.\n");
      abort ();
    default:
      stderr_out ("Unsupported dump type : %d\n", desc[pos].type);
      abort ();
    }

  /* We have no way of knowing the required alignment for this structure,
     so just max it maximally aligned. */
  return ALIGN_SIZE (max_offset + size_at_max, ALIGNOF (max_align_t));
}

/* Register the referenced objects in the array of COUNT objects of
   located at DATA; each object is described by SDESC.  "Object" here
   simply means any block of memory; it need not actually be a C
   "struct".  It could be a single integer or Lisp_Object, for
   example, as long as the description is accurate.

   This does not register the block of memory itself; it may, for
   example, be an array of structures inlined in another memory block
   and thus should not be registered.  See pdump_register_struct(),
   which does register the memory block. */

static void
pdump_register_struct_contents (const void *data,
				const struct struct_description *sdesc,
				int count)

{
  int me = depth++;
  int i;
  Bytecount elsize;

  if (me>65536)
    {
      stderr_out ("Backtrace overflow, loop ?\n");
      abort ();
    }
  backtrace[me].obj = 0;
  backtrace[me].position = 0;
  backtrace[me].offset = 0;

  elsize = pdump_structure_size (data, sdesc);

  for (i = 0; i < count; i++)
    {
      pdump_register_sub (((char *) data) + elsize * i,
			  sdesc->description,
			  me);
    }
  --depth;
}

/* Register the array of COUNT objects of located at DATA; each object is
   described by SDESC.  "Object" here simply means any block of memory;
   it need not actually be a C "struct".  It could be a single integer
   or Lisp_Object, for example, as long as the description is accurate.

   This is like pdump_register_struct_contents() but also registers
   the memory block itself. */

static void
pdump_register_struct (const void *data,
		       const struct struct_description *sdesc,
		       int count)
{
  if (data && !pdump_get_entry (data))
    {
      pdump_add_entry (pdump_get_entry_list (sdesc), data,
		       pdump_structure_size (data, sdesc), count);

      pdump_register_struct_contents (data, sdesc, count);
    }
}

/* Store the already-calculated new pointer offsets for all pointers
   in the COUNT contiguous blocks of memory, each described by DESC
   and of size SIZE, whose original is located at ORIG_DATA and the
   modifiable copy at DATA.

   This is done just before writing the modified block of memory to
   the dump file.  The new pointer offsets have been carefully
   calculated so that the data being pointed gets written at that
   offset in the dump file.  That way, the dump file is a correct
   memory image except perhaps for a constant that needs to be added
   to all pointers. (#### In fact, we SHOULD be starting up a dumped
   XEmacs, seeing where the dumped file gets loaded into memory, and
   then rewriting the dumped file after relocating all the pointers
   relative to this memory location.  That way, if the file gets
   loaded again at the same location, which will be common, we don't
   have to do any relocating, which is both faster at startup and
   allows the read-only part of the dumped data to be shared read-only
   between different invocations of XEmacs.)

   #### Do we distinguish between read-only and writable dumped data?
   Should we?  It's tricky because the dumped data, once loaded again,
   cannot really be free()d or garbage collected since it's all stored
   in one contiguous block of data with no malloc() headers, and we
   don't keep track of the pointers used internally in malloc() and
   the Lisp allocator to track allocated blocks of memory. */

static void
pdump_store_new_pointer_offsets (int count, void *data, const void *orig_data,
				 const struct lrecord_description *desc,
				 int size)
{
  int pos, i;
  /* Process each block one by one */
  for (i = 0; i < count; i++)
    {
      /* CUR points to the beginning of each block in the new data. */
      char *cur = ((char *)data) + i*size;
    restart:
      /* Scan each line of the description for relocatable pointers */
      for (pos = 0; desc[pos].type != XD_END; pos++)
	{
	  /* RDATA points to the beginning of each element in the new data. */
	  void *rdata = cur + desc[pos].offset;
	  switch (desc[pos].type)
	    {
	    case XD_SPECIFIER_END:
	      desc = ((const Lisp_Specifier *)(orig_data))->
		methods->extra_description;
	      goto restart;
	    case XD_CODING_SYSTEM_END:
	      desc = ((const Lisp_Coding_System *)(orig_data))->
		methods->extra_description;
	      goto restart;
	    case XD_BYTECOUNT:
	    case XD_ELEMCOUNT:
	    case XD_HASHCODE:
	    case XD_INT:
	    case XD_LONG:
	      break;
	    case XD_INT_RESET:
	      {
		EMACS_INT val = desc[pos].data1;
		if (XD_IS_INDIRECT (val))
		  val = pdump_get_indirect_count (val, desc, orig_data);
		* (int *) rdata = val;
		break;
	      }
	    case XD_OPAQUE_DATA_PTR:
	    case XD_C_STRING:
	    case XD_STRUCT_PTR:
	      {
		void *ptr = * (void **) rdata;
		if (ptr)
		  * (EMACS_INT *) rdata = pdump_get_entry (ptr)->save_offset;
		break;
	      }
	    case XD_LO_LINK:
	      {
		/* As described in lrecord.h, this is a weak link.
                   Thus, we need to link this object not (necessarily)
                   to the object directly pointed to, but to the next
                   referenced object in the chain.  None of the
                   intermediate objects will be written out, so we
                   traverse down the chain of objects until we find a
                   referenced one. (The Qnil or Qunbound that ends the
                   chain will always be a referenced object.) */
		Lisp_Object obj = * (Lisp_Object *) rdata;
		pdump_entry_list_elt *elt1;
		for (;;)
		  {
		    elt1 = pdump_get_entry (XRECORD_LHEADER (obj));
		    if (elt1)
		      break;
		    obj = * (Lisp_Object *) (desc[pos].offset +
					     (char *)(XRECORD_LHEADER (obj)));
		  }
		* (EMACS_INT *) rdata = elt1->save_offset;
		break;
	      }
	    case XD_LISP_OBJECT:
	      {
		Lisp_Object *pobj = (Lisp_Object *) rdata;

		assert (desc[pos].data1 == 0);

		if (POINTER_TYPE_P (XTYPE (*pobj)) && XRECORD_LHEADER (*pobj))
		  * (EMACS_INT *) pobj =
		    pdump_get_entry (XRECORD_LHEADER (*pobj))->save_offset;
		break;
	      }
	    case XD_LISP_OBJECT_ARRAY:
	      {
		EMACS_INT num = desc[pos].data1;
		int j;
		if (XD_IS_INDIRECT (num))
		  num = pdump_get_indirect_count (num, desc, orig_data);

		for (j = 0; j < num; j++)
		  {
		    Lisp_Object *pobj = ((Lisp_Object *) rdata) + j;
		    if (POINTER_TYPE_P (XTYPE (*pobj)) &&
			XRECORD_LHEADER (*pobj))
		      * (EMACS_INT *) pobj =
			pdump_get_entry (XRECORD_LHEADER (*pobj))->save_offset;
		  }
		break;
	      }
	    case XD_DOC_STRING:
	      {
		EMACS_INT str = *(EMACS_INT *)rdata;
		if (str > 0)
		  * (EMACS_INT *) rdata =
		    pdump_get_entry ((void *)str)->save_offset;
		break;
	      }
	    case XD_STRUCT_ARRAY:
	      {
		EMACS_INT num = desc[pos].data1;
		if (XD_IS_INDIRECT (num))
		  num = pdump_get_indirect_count (num, desc, orig_data);

		pdump_store_new_pointer_offsets
		  (num, rdata,
		   ((char *) rdata - (char *) data) + (char *) orig_data,
		   desc[pos].data2->description,
		   pdump_structure_size
		   (((char *) rdata - (char *) data) + (char *) orig_data,
		    desc[pos].data2));
		break;
	      }
	    case XD_UNION:
	      abort ();		/* #### IMPLEMENT ME!  NEEDED FOR UNICODE
				   SUPPORT */

	    default:
	      stderr_out ("Unsupported dump type : %d\n", desc[pos].type);
	      abort ();
	    }
	}
    }
}

/* Write out to global file descriptor PDUMP_OUT the element (one or
   more contiguous blocks of identical size/description) recorded in
   ELT and described by DESC.  The element is first copied to a buffer
   and then all pointers (this includes Lisp_Objects other than
   integer/character) are relocated to the (pre-computed) offset in
   the dump file. */

static void
pdump_dump_data (pdump_entry_list_elt *elt,
		 const struct lrecord_description *desc)
{
  Bytecount size = elt->size;
  int count = elt->count;
  if (desc)
    {
      /* Copy to temporary buffer */
      memcpy (pdump_buf, elt->obj, size*count);

      /* Store new offsets into all pointers in block */
      pdump_store_new_pointer_offsets (count, pdump_buf, elt->obj, desc, size);
    }
  retry_fwrite (desc ? pdump_buf : elt->obj, size, count, pdump_out);
}

/* Relocate a single memory block at DATA, described by DESC, from its
   assumed load location to its actual one by adding DELTA to all
   pointers in the block.  Does not recursively relocate any other
   memory blocks pointed to. (We already have a list of all memory
   blocks in the dump file.) */

static void
pdump_reloc_one (void *data, EMACS_INT delta,
		 const struct lrecord_description *desc)
{
  int pos;

 restart:
  for (pos = 0; desc[pos].type != XD_END; pos++)
    {
      void *rdata = (char *)data + desc[pos].offset;
      switch (desc[pos].type)
	{
	case XD_SPECIFIER_END:
	  pos = 0;
	  desc = ((const Lisp_Specifier *)data)->methods->extra_description;
	  goto restart;
	case XD_CODING_SYSTEM_END:
	  pos = 0;
	  desc =
	    ((const Lisp_Coding_System *)data)->methods->extra_description;
	  goto restart;
	case XD_BYTECOUNT:
	case XD_ELEMCOUNT:
	case XD_HASHCODE:
	case XD_INT:
	case XD_LONG:
	case XD_INT_RESET:
	  break;
	case XD_OPAQUE_DATA_PTR:
	case XD_C_STRING:
	case XD_STRUCT_PTR:
	case XD_LO_LINK:
	  {
	    EMACS_INT ptr = *(EMACS_INT *)rdata;
	    if (ptr)
	      *(EMACS_INT *)rdata = ptr+delta;
	    break;
	  }
	case XD_LISP_OBJECT:
	  {
	    Lisp_Object *pobj = (Lisp_Object *) rdata;

	    assert (desc[pos].data1 == 0);

	    if (POINTER_TYPE_P (XTYPE (*pobj))
		&& ! EQ (*pobj, Qnull_pointer))
	      *pobj = wrap_pointer_1 ((char *) XPNTR (*pobj) + delta);

	    break;
	  }
	case XD_LISP_OBJECT_ARRAY:
	  {
	    EMACS_INT num = desc[pos].data1;
	    int j;
	    if (XD_IS_INDIRECT (num))
	      num = pdump_get_indirect_count (num, desc, data);

	    for (j=0; j<num; j++)
	      {
		Lisp_Object *pobj = (Lisp_Object *) rdata + j;

		if (POINTER_TYPE_P (XTYPE (*pobj))
		    && ! EQ (*pobj, Qnull_pointer))
		  *pobj = wrap_pointer_1 ((char *) XPNTR (*pobj) + delta);
	      }
	    break;
	  }
	case XD_DOC_STRING:
	  {
	    EMACS_INT str = *(EMACS_INT *)rdata;
	    if (str > 0)
	      *(EMACS_INT *)rdata = str + delta;
	    break;
	  }
	case XD_STRUCT_ARRAY:
	  {
	    EMACS_INT num = desc[pos].data1;
	    int j;
	    const struct struct_description *sdesc = desc[pos].data2;
	    Bytecount size = pdump_structure_size (rdata, sdesc);

	    if (XD_IS_INDIRECT (num))
	      num = pdump_get_indirect_count (num, desc, data);
	    /* Note: We are recursing over data in the block itself */
	    for (j = 0; j < num; j++)
	      pdump_reloc_one ((char *) rdata + j * size, delta,
			       sdesc->description);

	    break;
	  }

	case XD_UNION:
	  abort (); /* #### IMPLEMENT ME!  NEEDED FOR UNICODE SUPPORT */
	default:
	  stderr_out ("Unsupported dump type : %d\n", desc[pos].type);
	  abort ();
	};
    }
}

static void
pdump_allocate_offset (pdump_entry_list_elt *elt,
		       const struct lrecord_description *desc)
{
  Bytecount size = elt->count * elt->size;
  elt->save_offset = cur_offset;
  if (size>max_size)
    max_size = size;
  cur_offset += size;
}

static void
pdump_scan_by_alignment (void (*f)(pdump_entry_list_elt *,
				   const struct lrecord_description *))
{
  int align;

  for (align = ALIGNOF (max_align_t); align; align>>=1)
    {
      int i;
      pdump_entry_list_elt *elt;

      for (i=0; i<lrecord_type_count; i++)
	if (pdump_object_table[i].align == align)
	  for (elt = pdump_object_table[i].first; elt; elt = elt->next)
	    f (elt, lrecord_implementations_table[i]->description);

      for (i=0; i<pdump_struct_table.count; i++)
	{
	  pdump_struct_list_elt list = pdump_struct_table.list[i];
	  if (list.list.align == align)
	    for (elt = list.list.first; elt; elt = elt->next)
	      f (elt, list.sdesc->description);
	}

      for (elt = pdump_opaque_data_list.first; elt; elt = elt->next)
	if (pdump_size_to_align (elt->size) == align)
	  f (elt, 0);
    }
}

static void
pdump_dump_root_struct_ptrs (void)
{
  int i;
  Elemcount count = Dynarr_length (pdump_root_struct_ptrs);
  pdump_static_pointer *data = alloca_array (pdump_static_pointer, count);
  for (i = 0; i < count; i++)
    {
      data[i].address = (char **) Dynarr_atp (pdump_root_struct_ptrs, i)->ptraddress;
      data[i].value   = (char *) pdump_get_entry (* data[i].address)->save_offset;
    }
  PDUMP_ALIGN_OUTPUT (pdump_static_pointer);
  retry_fwrite (data, sizeof (pdump_static_pointer), count, pdump_out);
}

static void
pdump_dump_opaques (void)
{
  int i;
  for (i = 0; i < Dynarr_length (pdump_opaques); i++)
    {
      pdump_opaque *info = Dynarr_atp (pdump_opaques, i);
      PDUMP_WRITE_ALIGNED (pdump_opaque, *info);
      retry_fwrite (info->varaddress, info->size, 1, pdump_out);
    }
}

static void
pdump_dump_rtables (void)
{
  int i;
  pdump_entry_list_elt *elt;
  pdump_reloc_table rt;

  for (i=0; i<lrecord_type_count; i++)
    {
      elt = pdump_object_table[i].first;
      if (!elt)
	continue;
      rt.desc = lrecord_implementations_table[i]->description;
      rt.count = pdump_object_table[i].count;
      PDUMP_WRITE_ALIGNED (pdump_reloc_table, rt);
      while (elt)
	{
	  EMACS_INT rdata = pdump_get_entry (elt->obj)->save_offset;
	  PDUMP_WRITE_ALIGNED (EMACS_INT, rdata);
	  elt = elt->next;
	}
    }

  rt.desc = 0;
  rt.count = 0;
  PDUMP_WRITE_ALIGNED (pdump_reloc_table, rt);

  for (i=0; i<pdump_struct_table.count; i++)
    {
      elt = pdump_struct_table.list[i].list.first;
      rt.desc = pdump_struct_table.list[i].sdesc->description;
      rt.count = pdump_struct_table.list[i].list.count;
      PDUMP_WRITE_ALIGNED (pdump_reloc_table, rt);
      while (elt)
	{
	  EMACS_INT rdata = pdump_get_entry (elt->obj)->save_offset;
	  int j;
	  for (j=0; j<elt->count; j++)
	    {
	      PDUMP_WRITE_ALIGNED (EMACS_INT, rdata);
	      rdata += elt->size;
	    }
	  elt = elt->next;
	}
    }
  rt.desc = 0;
  rt.count = 0;
  PDUMP_WRITE_ALIGNED (pdump_reloc_table, rt);
}

static void
pdump_dump_root_objects (void)
{
  Elemcount count = (Dynarr_length (pdump_root_objects) +
			 Dynarr_length (pdump_weak_object_chains));
  Elemcount i;

  PDUMP_WRITE_ALIGNED (Elemcount, count);
  PDUMP_ALIGN_OUTPUT (pdump_static_Lisp_Object);

  for (i = 0; i < Dynarr_length (pdump_root_objects); i++)
    {
      pdump_static_Lisp_Object obj;
      obj.address = Dynarr_at (pdump_root_objects, i);
      obj.value   = * obj.address;

      if (POINTER_TYPE_P (XTYPE (obj.value)))
	obj.value =
	  wrap_pointer_1 ((void *) pdump_get_entry (XRECORD_LHEADER
						    (obj.value))->save_offset);

      PDUMP_WRITE (pdump_static_Lisp_Object, obj);
    }

  for (i=0; i<Dynarr_length (pdump_weak_object_chains); i++)
    {
      pdump_entry_list_elt *elt;
      pdump_static_Lisp_Object obj;

      obj.address = Dynarr_at (pdump_weak_object_chains, i);
      obj.value   = * obj.address;

      for (;;)
	{
	  const struct lrecord_description *desc;
	  int pos;
	  elt = pdump_get_entry (XRECORD_LHEADER (obj.value));
	  if (elt)
	    break;
	  desc = XRECORD_LHEADER_IMPLEMENTATION (obj.value)->description;
	  for (pos = 0; desc[pos].type != XD_LO_LINK; pos++)
	    assert (desc[pos].type != XD_END);

	  obj.value = *(Lisp_Object *)(desc[pos].offset + (char *)(XRECORD_LHEADER (obj.value)));
	}
      obj.value = wrap_pointer_1 ((void *) elt->save_offset);

      PDUMP_WRITE (pdump_static_Lisp_Object, obj);
    }
}

void
pdump (void)
{
  int i;
  Lisp_Object t_console, t_device, t_frame;
  int none;
  pdump_header header;

  pdump_object_table = xnew_array (pdump_entry_list, lrecord_type_count);
  pdump_alert_undump_object = xnew_array (int, lrecord_type_count);

  assert (ALIGNOF (max_align_t) <= pdump_align_table[0]);

  for (i = 0; i < countof (pdump_align_table); i++)
    if (pdump_align_table[i] > ALIGNOF (max_align_t))
      pdump_align_table[i] = ALIGNOF (max_align_t);

  flush_all_buffer_local_cache ();

  /* These appear in a DEFVAR_LISP, which does a staticpro() */
  t_console = Vterminal_console; Vterminal_console = Qnil;
  t_frame   = Vterminal_frame;   Vterminal_frame   = Qnil;
  t_device  = Vterminal_device;  Vterminal_device  = Qnil;

  dump_add_opaque (&lrecord_implementations_table,
		   lrecord_type_count * sizeof (lrecord_implementations_table[0]));
  dump_add_opaque (&lrecord_markers,
		   lrecord_type_count * sizeof (lrecord_markers[0]));

  pdump_hash = xnew_array_and_zero (pdump_entry_list_elt *, PDUMP_HASHSIZE);

  for (i=0; i<lrecord_type_count; i++)
    {
      pdump_object_table[i].first = 0;
      pdump_object_table[i].align = ALIGNOF (max_align_t);
      pdump_object_table[i].count = 0;
      pdump_alert_undump_object[i] = 0;
    }
  pdump_struct_table.count = 0;
  pdump_struct_table.size = -1;

  pdump_opaque_data_list.first = 0;
  pdump_opaque_data_list.align = ALIGNOF (max_align_t);
  pdump_opaque_data_list.count = 0;
  depth = 0;

  for (i=0; i<Dynarr_length (pdump_root_objects); i++)
    pdump_register_object (* Dynarr_at (pdump_root_objects, i));

  none = 1;
  for (i=0; i<lrecord_type_count; i++)
    if (pdump_alert_undump_object[i])
      {
	if (none)
	  printf ("Undumpable types list :\n");
	none = 0;
	printf ("  - %s (%d)\n", lrecord_implementations_table[i]->name, pdump_alert_undump_object[i]);
      }
  if (!none)
    return;

  for (i=0; i<Dynarr_length (pdump_root_struct_ptrs); i++)
    {
      pdump_root_struct_ptr info = Dynarr_at (pdump_root_struct_ptrs, i);
      pdump_register_struct (*(info.ptraddress), info.desc, 1);
    }

  memcpy (header.signature, PDUMP_SIGNATURE, PDUMP_SIGNATURE_LEN);
  header.id = dump_id;
  header.reloc_address = 0;
  header.nb_root_struct_ptrs = Dynarr_length (pdump_root_struct_ptrs);
  header.nb_opaques = Dynarr_length (pdump_opaques);

  cur_offset = ALIGN_SIZE (sizeof (header), ALIGNOF (max_align_t));
  max_size = 0;

  pdump_scan_by_alignment (pdump_allocate_offset);
  cur_offset = ALIGN_SIZE (cur_offset, ALIGNOF (max_align_t));
  header.stab_offset = cur_offset;

  pdump_buf = xmalloc (max_size);
  pdump_fd = open (EMACS_PROGNAME ".dmp",
		   O_WRONLY | O_CREAT | O_TRUNC | OPEN_BINARY, 0666);
  if (pdump_fd < 0)
    report_file_error ("Unable to open dump file",
		       build_string (EMACS_PROGNAME ".dmp"));
  pdump_out = fdopen (pdump_fd, "w");
  if (pdump_out < 0)
    report_file_error ("Unable to open dump file for writing",
		       build_string (EMACS_PROGNAME ".dmp"));

  retry_fwrite (&header, sizeof (header), 1, pdump_out);
  PDUMP_ALIGN_OUTPUT (max_align_t);

  pdump_scan_by_alignment (pdump_dump_data);

  fseek (pdump_out, header.stab_offset, SEEK_SET);

  pdump_dump_root_struct_ptrs ();
  pdump_dump_opaques ();
  pdump_dump_rtables ();
  pdump_dump_root_objects ();

  retry_fclose (pdump_out);
  retry_close (pdump_fd);

  free (pdump_buf);

  free (pdump_hash);

  Vterminal_console = t_console;
  Vterminal_frame   = t_frame;
  Vterminal_device  = t_device;
}

static int
pdump_load_check (void)
{
  return (!memcmp (((pdump_header *)pdump_start)->signature,
		   PDUMP_SIGNATURE, PDUMP_SIGNATURE_LEN)
	  && ((pdump_header *)pdump_start)->id == dump_id);
}

/*----------------------------------------------------------------------*/
/*			Reading the dump file				*/
/*----------------------------------------------------------------------*/
static int
pdump_load_finish (void)
{
  int i;
  char *p;
  EMACS_INT delta;
  EMACS_INT count;
  pdump_header *header = (pdump_header *)pdump_start;

  pdump_end = pdump_start + pdump_length;

  delta = ((EMACS_INT)pdump_start) - header->reloc_address;
  p = pdump_start + header->stab_offset;

  /* Put back the pdump_root_struct_ptrs */
  p = (char *) ALIGN_PTR (p, ALIGNOF (pdump_static_pointer));
  for (i=0; i<header->nb_root_struct_ptrs; i++)
    {
      pdump_static_pointer ptr = PDUMP_READ (p, pdump_static_pointer);
      (* ptr.address) = ptr.value + delta;
    }

  /* Put back the pdump_opaques */
  for (i=0; i<header->nb_opaques; i++)
    {
      pdump_opaque info = PDUMP_READ_ALIGNED (p, pdump_opaque);
      memcpy ((void*)info.varaddress, p, info.size);
      p += info.size;
    }

  /* Do the relocations */
  pdump_rt_list = p;
  count = 2;
  for (;;)
    {
      pdump_reloc_table rt = PDUMP_READ_ALIGNED (p, pdump_reloc_table);
      p = (char *) ALIGN_PTR (p, ALIGNOF (char *));
      if (rt.desc)
	{
	  char **reloc = (char **)p;
	  for (i=0; i < rt.count; i++)
	    {
	      reloc[i] += delta;
	      pdump_reloc_one (reloc[i], delta, rt.desc);
	    }
	  p += rt.count * sizeof (char *);
	} else
	  if (!(--count))
	    break;
    }

  /* Put the pdump_root_objects variables in place */
  i = PDUMP_READ_ALIGNED (p, Elemcount);
  p = (char *) ALIGN_PTR (p, ALIGNOF (pdump_static_Lisp_Object));
  while (i--)
    {
      pdump_static_Lisp_Object obj = PDUMP_READ (p, pdump_static_Lisp_Object);

      if (POINTER_TYPE_P (XTYPE (obj.value)))
	obj.value = wrap_pointer_1 ((char *) XPNTR (obj.value) + delta);

      (* obj.address) = obj.value;
    }

  /* Final cleanups */
  /*   reorganize hash tables */
  p = pdump_rt_list;
  for (;;)
    {
      pdump_reloc_table rt = PDUMP_READ_ALIGNED (p, pdump_reloc_table);
      p = (char *) ALIGN_PTR (p, ALIGNOF (Lisp_Object));
      if (!rt.desc)
	break;
      if (rt.desc == hash_table_description)
	{
	  for (i=0; i < rt.count; i++)
	    pdump_reorganize_hash_table (PDUMP_READ (p, Lisp_Object));
	  break;
	} else
	  p += sizeof (Lisp_Object) * rt.count;
    }

  return 1;
}

#ifdef WIN32_NATIVE
/* Free the mapped file if we decide we don't want it after all */
static void
pdump_file_unmap (void)
{
  UnmapViewOfFile (pdump_start);
  CloseHandle (pdump_hFile);
  CloseHandle (pdump_hMap);
}

static int
pdump_file_get (const char *path)
{

  pdump_hFile = CreateFileA (path,
		            GENERIC_READ + GENERIC_WRITE,  /* Required for copy on write */
			    0,		            /* Not shared */
			    NULL,		    /* Not inheritable */
			    OPEN_EXISTING,
			    FILE_ATTRIBUTE_NORMAL,
			    NULL);		    /* No template file */
  if (pdump_hFile == INVALID_HANDLE_VALUE)
    return 0;

  pdump_length = GetFileSize (pdump_hFile, NULL);
  pdump_hMap = CreateFileMappingA (pdump_hFile,
				  NULL,		    /* No security attributes */
				  PAGE_WRITECOPY,   /* Copy on write */
				  0,		    /* Max size, high half */
				  0,		    /* Max size, low half */
				  NULL);	    /* Unnamed */
  if (pdump_hMap == INVALID_HANDLE_VALUE)
    return 0;

  pdump_start = MapViewOfFile (pdump_hMap,
			       FILE_MAP_COPY, /* Copy on write */
			       0,	      /* Start at zero */
			       0,
			       0);	      /* Map all of it */
  pdump_free = pdump_file_unmap;
  return 1;
}

/* pdump_resource_free is called (via the pdump_free pointer) to release
   any resources allocated by pdump_resource_get.  Since the Windows API
   specs specifically state that you don't need to (and shouldn't) free the
   resources allocated by FindResource, LoadResource, and LockResource this
   routine does nothing.  */
static void
pdump_resource_free (void)
{
}

static int
pdump_resource_get (void)
{
  HRSRC hRes;			/* Handle to dump resource */
  HRSRC hResLoad;		/* Handle to loaded dump resource */

  /* See Q126630 which describes how Windows NT and 95 trap writes to
     resource sections and duplicate the page to allow the write to proceed.
     It also describes how to make the resource section read/write (and hence
     private to each process).  Doing this avoids the exceptions and related
     overhead, but causes the resource section to be private to each process
     that is running XEmacs.  Since the resource section contains little
     other than the dumped data, which should be private to each process, we
     make the whole resource section read/write so we don't have to copy it. */

  hRes = FindResourceA (NULL, MAKEINTRESOURCE (101), "DUMP");
  if (hRes == NULL)
    return 0;

  /* Found it, use the data in the resource */
  hResLoad = LoadResource (NULL, hRes);
  if (hResLoad == NULL)
    return 0;

  pdump_start = LockResource (hResLoad);
  if (pdump_start == NULL)
    return 0;

  pdump_free = pdump_resource_free;
  pdump_length = SizeofResource (NULL, hRes);
  if (pdump_length <= (Bytecount) sizeof (pdump_header))
    {
      pdump_start = 0;
      return 0;
    }

  return 1;
}

#else /* !WIN32_NATIVE */

static void
pdump_file_free (void)
{
  xfree (pdump_start);
}

#ifdef HAVE_MMAP
static void
pdump_file_unmap (void)
{
  munmap (pdump_start, pdump_length);
}
#endif

static int
pdump_file_get (const char *path)
{
  int fd = open (path, O_RDONLY | OPEN_BINARY);
  if (fd<0)
    return 0;

  pdump_length = lseek (fd, 0, SEEK_END);
  if (pdump_length < (Bytecount) sizeof (pdump_header))
    {
      retry_close (fd);
      return 0;
    }

  lseek (fd, 0, SEEK_SET);

#ifdef HAVE_MMAP
/* Unix 98 requires that sys/mman.h define MAP_FAILED,
   but many earlier implementations don't. */
# ifndef MAP_FAILED
#  define MAP_FAILED ((void *) -1L)
# endif
  pdump_start = (char *) mmap (0, pdump_length, PROT_READ|PROT_WRITE, MAP_PRIVATE, fd, 0);
  if (pdump_start != (char *) MAP_FAILED)
    {
      pdump_free = pdump_file_unmap;
      retry_close (fd);
      return 1;
    }
#endif /* HAVE_MMAP */

  pdump_start = xnew_array (char, pdump_length);
  pdump_free = pdump_file_free;
  retry_read (fd, pdump_start, pdump_length);

  retry_close (fd);
  return 1;
}
#endif /* !WIN32_NATIVE */


static int
pdump_file_try (char *exe_path)
{
  char *w = exe_path + strlen (exe_path);

  do
    {
      sprintf (w, "-%s-%08x.dmp", EMACS_VERSION, dump_id);
      if (pdump_file_get (exe_path))
	{
	  if (pdump_load_check ())
	    return 1;
	  pdump_free ();
	}

      sprintf (w, "-%08x.dmp", dump_id);
      if (pdump_file_get (exe_path))
	{
	  if (pdump_load_check ())
	    return 1;
	  pdump_free ();
	}

      sprintf (w, ".dmp");
      if (pdump_file_get (exe_path))
	{
	  if (pdump_load_check ())
	    return 1;
	  pdump_free ();
	}

      do
	w--;
      while (w>exe_path && !IS_DIRECTORY_SEP (*w) && (*w != '-') && (*w != '.'));
    }
  while (w>exe_path && !IS_DIRECTORY_SEP (*w));
  return 0;
}

int
pdump_load (const Extbyte *argv0)
{
  Extbyte exe_path[PATH_MAX];
#ifdef WIN32_NATIVE
  GetModuleFileNameA (NULL, exe_path, PATH_MAX);
#else /* !WIN32_NATIVE */
  Extbyte *w;
  const Extbyte *dir, *p;

  dir = argv0;
  if (dir[0] == '-')
    {
      /* XEmacs as a login shell, oh goody! */
      dir = getenv ("SHELL"); /* not egetenv -- not yet initialized */
    }

  p = dir + strlen (dir);
  while (p != dir && !IS_ANY_SEP (p[-1])) p--;

  if (p != dir)
    {
      /* invocation-name includes a directory component -- presumably it
	 is relative to cwd, not $PATH */
      strcpy (exe_path, dir);
    }
  else
    {
      const Extbyte *path = getenv ("PATH"); /* not egetenv -- not yet init. */
      const Extbyte *name = p;
      for (;;)
	{
	  p = path;
	  while (*p && *p != SEPCHAR)
	    p++;
	  if (p == path)
	    {
	      exe_path[0] = '.';
	      w = exe_path + 1;
	    }
	  else
	    {
	      memcpy (exe_path, path, p - path);
	      w = exe_path + (p - path);
	    }
	  if (!IS_DIRECTORY_SEP (w[-1]))
	    {
	      *w++ = '/';
	    }
	  strcpy (w, name);

	  if (!access (exe_path, X_OK))
	    break;
	  if (!*p)
	    {
	      /* Oh well, let's have some kind of default */
	      sprintf (exe_path, "./%s", name);
	      break;
	    }
	  path = p+1;
	}
    }
#endif /* WIN32_NATIVE */

  if (pdump_file_try (exe_path))
    {
      pdump_load_finish ();
      return 1;
    }

#ifdef WIN32_NATIVE
  if (pdump_resource_get ())
    {
      if (pdump_load_check ())
	{
	  pdump_load_finish ();
	  return 1;
	}
      pdump_free ();
    }
#endif

  return 0;
}
