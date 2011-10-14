/* Opaque Lisp objects.
   Copyright (C) 1993, 1994, 1995 Sun Microsystems, Inc.
   Copyright (C) 1995, 1996, 2002, 2010 Ben Wing.

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

/* Written by Ben Wing, October 1993. */

/* "Opaque" is used internally to hold keep track of allocated memory
   so it gets GC'd properly, and to store arbitrary data in places
   where a Lisp_Object is required and which may get GC'd. (e.g.  as
   the argument to record_unwind_protect()).  Once created in C,
   opaque objects cannot be resized.

   OPAQUE OBJECTS SHOULD NEVER ESCAPE TO THE LISP LEVEL.  Some code
   depends on this.  As such, opaque objects are a generalization
   of the Qunbound marker.
 */

#include <config.h>
#include "lisp.h"
#include "opaque.h"

#ifndef NEW_GC
Lisp_Object Vopaque_ptr_free_list;
#endif /* not NEW_GC */

/* Should never, ever be called. (except by an external debugger) */
static void
print_opaque (Lisp_Object obj, Lisp_Object printcharfun,
	      int UNUSED (escapeflag))
{
  const Lisp_Opaque *p = XOPAQUE (obj);

  write_fmt_string
    (printcharfun,
     "#<INTERNAL OBJECT (XEmacs bug?) (opaque, size=%lu) 0x%x>",
     (long)(p->size), LISP_OBJECT_UID (obj));
}

inline static Bytecount
aligned_sizeof_opaque (Bytecount opaque_size)
{
  return MAX_ALIGN_SIZE (offsetof (Lisp_Opaque, data) + opaque_size);
}

static Bytecount
sizeof_opaque (Lisp_Object obj)
{
  return aligned_sizeof_opaque (XOPAQUE (obj)->size);
}

/* Return an opaque object of size SIZE.
   If DATA is OPAQUE_CLEAR, the object's data is memset to '\0' bytes.
   If DATA is OPAQUE_UNINIT, the object's data is uninitialized.
   Else the object's data is initialized by copying from DATA. */
Lisp_Object
make_opaque (const void *data, Bytecount size)
{
  Lisp_Object obj =
    ALLOC_SIZED_LISP_OBJECT (aligned_sizeof_opaque (size), opaque);
  Lisp_Opaque *p = XOPAQUE (obj);
  p->size = size;

  if (data == OPAQUE_CLEAR)
    memset (p->data, '\0', size);
  else if (data == OPAQUE_UNINIT)
    DO_NOTHING;
  else
    memcpy (p->data, data, size);

  return obj;
}

/* This will not work correctly for opaques with subobjects! */

static int
equal_opaque (Lisp_Object obj1, Lisp_Object obj2, int UNUSED (depth),
	      int UNUSED (foldcase))
{
  Bytecount size;
  return ((size = XOPAQUE_SIZE (obj1)) == XOPAQUE_SIZE (obj2) &&
	  !memcmp (XOPAQUE_DATA (obj1), XOPAQUE_DATA (obj2), size));
}

/* This will not work correctly for opaques with subobjects! */

static Hashcode
hash_opaque (Lisp_Object obj, int UNUSED (depth), int UNUSED (equalp))
{
  if (XOPAQUE_SIZE (obj) == sizeof (unsigned long))
    return *((Hashcode *) XOPAQUE_DATA (obj));
  else
    return memory_hash (XOPAQUE_DATA (obj), XOPAQUE_SIZE (obj));
}

static const struct memory_description opaque_description[] = {
  { XD_END }
};

DEFINE_DUMPABLE_SIZABLE_LISP_OBJECT ("opaque", opaque,
				     0, print_opaque, 0,
				     equal_opaque, hash_opaque,
				     opaque_description,
				     sizeof_opaque, Lisp_Opaque);

/* stuff to handle opaque pointers */

/* Should never, ever be called. (except by an external debugger) */
static void
print_opaque_ptr (Lisp_Object obj, Lisp_Object printcharfun,
		  int UNUSED (escapeflag))
{
  const Lisp_Opaque_Ptr *p = XOPAQUE_PTR (obj);

  write_fmt_string
    (printcharfun,
     "#<INTERNAL OBJECT (XEmacs bug?) (opaque-ptr, adr=0x%lx) 0x%x>",
     (long)(p->ptr), LISP_OBJECT_UID (obj));
}

static int
equal_opaque_ptr (Lisp_Object obj1, Lisp_Object obj2, int UNUSED (depth),
		  int UNUSED (foldcase))
{
  return (XOPAQUE_PTR (obj1)->ptr == XOPAQUE_PTR (obj2)->ptr);
}

static Hashcode
hash_opaque_ptr (Lisp_Object obj, int UNUSED (depth), int UNUSED (equalp))
{
  return (Hashcode) XOPAQUE_PTR (obj)->ptr;
}

static const struct memory_description opaque_ptr_description[] = {
  { XD_END }
};

DEFINE_NODUMP_LISP_OBJECT ("opaque-ptr", opaque_ptr,
			   0, print_opaque_ptr, 0,
			   equal_opaque_ptr, hash_opaque_ptr,
			   opaque_ptr_description, Lisp_Opaque_Ptr);

Lisp_Object
make_opaque_ptr (void *val)
{
#ifdef NEW_GC
  Lisp_Object res = ALLOC_NORMAL_LISP_OBJECT (opaque_ptr);
#else /* not NEW_GC */
  Lisp_Object res = alloc_managed_lcrecord (Vopaque_ptr_free_list);
#endif /* not NEW_GC */
  set_opaque_ptr (res, val);
  return res;
}

/* Be very very careful with this.  Same admonitions as with
   free_cons() apply. */

void
free_opaque_ptr (Lisp_Object ptr)
{
#ifdef NEW_GC
  free_normal_lisp_object (ptr);
#else /* not NEW_GC */
  free_managed_lcrecord (Vopaque_ptr_free_list, ptr);
#endif /* not NEW_GC */
}

#ifndef NEW_GC
void
reinit_opaque_early (void)
{
  Vopaque_ptr_free_list = make_lcrecord_list (sizeof (Lisp_Opaque_Ptr),
					      &lrecord_opaque_ptr);
  staticpro_nodump (&Vopaque_ptr_free_list);
}
#endif /* not NEW_GC */

void
init_opaque_once_early (void)
{
  INIT_LISP_OBJECT (opaque);
  INIT_LISP_OBJECT (opaque_ptr);

#ifndef NEW_GC
  reinit_opaque_early ();
#endif /* not NEW_GC */
}
