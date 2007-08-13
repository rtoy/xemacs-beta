/* Epoch functionality.
   Copyright (C) 1985-1995 Free Software Foundation, Inc.
   Copyright (C) 1996 Ben Wing.

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

#include "console-x.h"
#include "objects-x.h"
#include "events.h"
#include "frame.h"

Lisp_Object Qx_property_change, Qx_client_message, Qx_map, Qx_unmap;
Lisp_Object Vepoch_event, Vepoch_event_handler;


/************************************************************************/
/*                              X resources                             */
/************************************************************************/

Lisp_Object Qx_resource_live_p;

#define XX_RESOURCE(x) XRECORD (x, x_resource, struct Lisp_X_Resource)
#define XSETX_RESOURCE(x, p) XSETRECORD (x, p, x_resource)
#define X_RESOURCEP(x) RECORDP (x, x_resource)
#define GC_X_RESOURCEP(x) GC_RECORDP (x, x_resource)
#define CHECK_X_RESOURCE(x) CHECK_RECORD (x, x_resource)

#define X_RESOURCE_LIVE_P(xr) (DEVICE_LIVE_P (XDEVICE ((xr)->device)))
#define CHECK_LIVE_X_RESOURCE(x) 	       				\
  do { CHECK_X_RESOURCE (x);						\
       if (!X_RESOURCE_LIVE_P (XX_RESOURCE (x)))			\
	 x = wrong_type_argument (Qx_resource_live_p, (x));    		\
     } while (0)

struct Lisp_X_Resource
{
  struct lcrecord_header header;

  XID xid;
  Atom type;
  Lisp_Object device;
};

Lisp_Object Qx_resourcep;
static Lisp_Object mark_x_resource (Lisp_Object, void (*) (Lisp_Object));
static void print_x_resource (Lisp_Object, Lisp_Object, int);
static void finalize_x_resource (void *, int);
static int x_resource_equal (Lisp_Object o1, Lisp_Object o2, int depth);
static unsigned long x_resource_hash (Lisp_Object obj, int depth);
DEFINE_LRECORD_IMPLEMENTATION ("x-resource", x_resource,
			       mark_x_resource, print_x_resource,
			       finalize_x_resource, x_resource_equal,
			       x_resource_hash, struct Lisp_X_Resource);

static Lisp_Object
mark_x_resource (Lisp_Object obj, void (*markobj) (Lisp_Object))
{
  return XX_RESOURCE (obj)->device;
}

static void
print_x_resource (Lisp_Object obj, Lisp_Object printcharfun, int escapeflag)
{
  char buf[100];
  Bufbyte *default_string = "Resource";
  Lisp_Object atom_symbol;
  Lisp_Object device = XX_RESOURCE (obj)->device;

  if (print_readably)
    {
      if (!DEVICE_LIVE_P (XDEVICE (device)))
	error ("printing unreadable object #<dead x-resource>");
      else
	error ("printing unreadable object #<x-resource 0x%x>",
	       (unsigned int) XX_RESOURCE (obj)->xid);
    }

  if (!DEVICE_LIVE_P (XDEVICE (device)))
    write_c_string ("#<dead x-resource>", printcharfun);
  else
    {
      atom_symbol = x_atom_to_symbol (XDEVICE (device),
				      XX_RESOURCE (obj)->type);
      sprintf (buf, "#<x-resource %s on ",
	       (NILP (atom_symbol)
		? default_string
		: string_data (XSTRING (Fsymbol_name (atom_symbol)))));
      write_c_string (buf, printcharfun);
      print_internal (device, printcharfun, escapeflag);
      sprintf (buf, " 0x%x>",(unsigned int) XX_RESOURCE (obj)->xid);
      write_c_string (buf, printcharfun);
    }
}

static void
finalize_x_resource (void *header, int for_disksave)
{
}

static int
x_resource_equal (Lisp_Object o1, Lisp_Object o2, int depth)
{
  return (XX_RESOURCE (o1)->xid == XX_RESOURCE (o2)->xid &&
	  EQ (XX_RESOURCE (o1)->device, XX_RESOURCE (o2)->device));
}

static unsigned long
x_resource_hash (Lisp_Object obj, int depth)
{
  return HASH2 (XX_RESOURCE (obj)->xid,
		internal_hash (XX_RESOURCE (obj)->device, depth));
}

/*
 * Epoch equivalent:  epoch::resourcep
 */
DEFUN ("x-resource-p", Fx_resource_p, Sx_resource_p, 1, 1, 0 /*
Return non-nil if OBJECT is an X resource object.
*/ )
     (object)
     Lisp_Object object;
{
  return (X_RESOURCEP (object) ? Qt : Qnil);
}

DEFUN ("x-resource-live-p", Fx_resource_live_p, Sx_resource_live_p, 1, 1, 0 /*
Return non-nil if OBJECT is a live X resource object.
That means that the X resource's device is live.
*/ )
     (object)
     Lisp_Object object;
{
  return (X_RESOURCEP (object) &&
	  X_RESOURCE_LIVE_P (XX_RESOURCE (object)) ? Qt : Qnil);
}

DEFUN ("x-resource-device", Fx_resource_device, Sx_resource_device, 1, 1, 0 /*
Return the device that OBJECT (an X resource object) exists on.
*/ )
     (object)
     Lisp_Object object;
{
  CHECK_LIVE_X_RESOURCE (object);
  return XX_RESOURCE (object)->device;
}

/*
 * Epoch equivalent:  epoch::set-resource-type
*/
DEFUN ("set-x-resource-type", Fset_x_resource_type, Sset_x_resource_type,
       2, 2, 0 /*
Set the type of RESOURCE to TYPE.  The new type must be an atom.
*/ )
     (resource, type)
     Lisp_Object resource, type;
{
  CHECK_LIVE_X_RESOURCE (resource);
  CHECK_LIVE_X_RESOURCE (type);

  if (XX_RESOURCE (type)->type != XA_ATOM)
    error ("New type must be an atom");

  XX_RESOURCE (resource)->type = XX_RESOURCE (type)->xid;
  return resource;
}

static Lisp_Object
make_x_resource (XID xid, Atom type, Lisp_Object device)
{
  struct Lisp_X_Resource *xr =
    alloc_lcrecord (sizeof (struct Lisp_X_Resource), lrecord_x_resource);
  Lisp_Object val;

  xr->xid = xid;
  xr->type = type;
  xr->device = device;
  XSETX_RESOURCE (val, xr);

  return val;
}

static Lisp_Object
get_symbol_or_string_as_symbol (Lisp_Object name)
{
 retry:
  if (SYMBOLP (name))
    return name;
  else if (STRINGP (name))
    return Fintern (name, Qnil);
  else
    {
      signal_simple_continuable_error ("Must be symbol or string",
				       name);
      goto retry;
    }
  return Qnil; /* not reached */
}

/*
 * Epoch equivalent:  epoch::intern-atom
 */
DEFUN ("x-intern-atom", Fx_intern_atom, Sx_intern_atom, 1, 2, 0 /*
Convert a string or symbol into an atom and return as an X resource.
Optional argument DEVICE specifies the display connection and defaults
to the selected device.
*/ )
     (name, device)
     Lisp_Object name, device;
{
  Atom atom;
  struct device *d = decode_x_device (device);

  XSETDEVICE (device, d);
  atom = symbol_to_x_atom (d, get_symbol_or_string_as_symbol (name), 0);
  return make_x_resource (atom, XA_ATOM, device);
}

/*
 * Epoch equivalent:  epoch::unintern-atom
 */
DEFUN ("x-atom-name", Fx_atom_name, Sx_atom_name, 1, 1, 0 /*
Return the name of an X atom resource as a string.
*/ )
     (atom)
     Lisp_Object atom;
{
  Lisp_Object val;

  CHECK_LIVE_X_RESOURCE (atom);
  if (XX_RESOURCE (atom)->type != XA_ATOM)
    signal_simple_error ("Resource is not an atom", atom);

  val = x_atom_to_symbol (XDEVICE (XX_RESOURCE (atom)->device),
			  XX_RESOURCE (atom)->xid);
  if (NILP (val))
    return Qnil;
  return Fsymbol_name (val);
}

/*
 * Epoch equivalent:  epoch::string-to-resource
 */
DEFUN ("string-to-x-resource", Fstring_to_x_resource,
       Sstring_to_x_resource, 2, 3, 0 /*
Convert a numeric STRING to an X-RESOURCE.
STRING is assumed to represent a 32-bit numer value. X-RESOURCE must be
an X atom.  Optional BASE argument should be a number between 2 and 36,
specifying the base for converting STRING.
*/ )
     (string, type, base)
     Lisp_Object string, type, base;
{
  XID xid;
  struct Lisp_X_Resource *xr;
  char *ptr;
  int b;

  CHECK_STRING (string);
  CHECK_LIVE_X_RESOURCE (type);

  if (NILP (base))
    b = 0;
  else
    {
      CHECK_INT (base);
      b = XINT (base);
      check_int_range (b, 2, 36);
    }

  if (XX_RESOURCE (type)->type != XA_ATOM)
    error ("Resource must be an atom");
  xr = XX_RESOURCE (type);

  xid = (XID) strtol ((CONST char *) string_data (XSTRING (string)), &ptr, b);

  return ((ptr == (char *) string_data (XSTRING (string)))
	  ? Qnil
	  : make_x_resource (xid, xr->xid, xr->device));
}

/*
 * Epoch equivalent:  epoch::resource-to-type
 */
DEFUN ("x-resource-to-type", Fx_resource_to_type, Sx_resource_to_type,
       1, 1, 0 /*
Return an x-resource of type ATOM whose value is the type of the argument
*/ )
     (resource)
     Lisp_Object resource;
{
  struct Lisp_X_Resource *xr;

  CHECK_LIVE_X_RESOURCE (resource);
  xr = XX_RESOURCE (resource);

  return make_x_resource (xr->type, XA_ATOM, xr->device);
}

/* internal crap stolen from Epoch */
static char LongToStringBuffer[33]; /* can't have statics inside functions! */
static char *
long_to_string (unsigned long n, unsigned int base)
{
  char *digit = "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ";
  char *s = LongToStringBuffer + 32; /* at most 33 characters in binary */

  *s = 0;			/* terminate */
  while (n)			/* something there */
    {
    *--s = digit[n % base];		/* store bottom digit */
    n /= base;			/* shift right */
    }
  if (*s == 0) *--s = '0';		/* in case nothing was put in string */
  return s;
}

/*
 * Epoch equivalent:  epoch::resource-to-string
 */
DEFUN ("x-resource-to-string", Fx_resource_to_string, Sx_resource_to_string,
       1, 2, 0 /*
Convert the xid of RESOURCE to a numeric string.
Optional BASE specifies the base for the conversion (2..36 inclusive)
*/ )
     (resource, base)
     Lisp_Object resource, base;
{
  int cbase = 10;

  CHECK_LIVE_X_RESOURCE (resource);
  if (!NILP (base))
    {
      CHECK_INT (base);
      cbase = XINT (base);
      check_int_range (cbase, 2, 36);
    }

  return build_string (long_to_string (XX_RESOURCE (resource)->xid, cbase));
}

/*
 * Epoch equivalent:  epoch::xid-of-frame
 */
DEFUN ("x-id-of-frame", Fx_id_of_frame, Sx_id_of_frame, 0, 1, 0 /*
Return the window ID of FRAME as an x-resource.
This differs from `x-window-id' in that its return value is an
x-resource rather than a string.
*/ )
     (frame)
     Lisp_Object frame;
{
  struct frame *f = decode_x_frame (frame);

  return make_x_resource (XtWindow (FRAME_X_SHELL_WIDGET (f)), XA_WINDOW,
			  FRAME_DEVICE (f));
}

/* Given a frame or ID X resource, return the X window and device
   it refers to.  If text_p is non-zero, the window returned corresponds
   to the text widget of the frame rather than the shell widget. */

static void
epoch_get_window_and_device (Lisp_Object frame, Window *window,
			     Lisp_Object *device, int text_p)
{
  if (X_RESOURCEP (frame))
    {
      CHECK_LIVE_X_RESOURCE (frame);
      if (XX_RESOURCE (frame)->type != XA_WINDOW)
	error ("Frame resource must be of type WINDOW");
      *window = XX_RESOURCE (frame)->xid;
      *device = XX_RESOURCE (frame)->device;
    }
  else
    {
      struct frame *f = decode_x_frame (frame);

      XSETFRAME (frame, f);
      if (text_p)
	*window = XtWindow (FRAME_X_TEXT_WIDGET (f));
      else
	*window = XX_RESOURCE (Fx_id_of_frame (frame))->xid;
      *device = FRAME_DEVICE (f);
    }

}

/*
 * Epoch equivalent:  epoch::query-tree
*/
DEFUN ("x-query-tree", Fx_query_tree, Sx_query_tree, 0, 1, 0 /*
Return the portion of the window tree adjacent to FRAME.
Return value is the list ( ROOT PARENT . CHILDREN ).  The FRAME arg
can either be a frame object or an x-resource of type window.
*/ )
     (frame)
     Lisp_Object frame;
{
  Window win;
  Window root, parent, *children;
  unsigned int count;
  int retval;
  Lisp_Object val;
  Lisp_Object device;

  epoch_get_window_and_device (frame, &win, &device, 0);

  retval =
    XQueryTree (DEVICE_X_DISPLAY (XDEVICE (device)), win, &root, &parent,
		&children, &count);

  /* Thank you, X-Consortium. XQueryTree doesn't return Success like everyone
   * else, it returns 1. (Success is defined to be 0 in the standard header
   * files)
   */
  if (!retval) return Qnil;

  val = Qnil;
  while (count)
    val = Fcons (make_x_resource (children[--count], XA_WINDOW, device), val);

  XFree (children);

  return Fcons (make_x_resource (root, XA_WINDOW, device),
		Fcons ((parent
			? make_x_resource (parent, XA_WINDOW, device)
			: Qnil),
		       val));
}

/* more internal crap stolen from Epoch */

static void
verify_vector_has_consistent_type (Lisp_Object vector)
{
  int i;			/* vector index */
  XID rtype;			/* X_resource type (if vector of
				   X_resources) */
  int length;			/* vector length */
  struct Lisp_Vector *v = XVECTOR (vector);
  Lisp_Object *element;
  Lisp_Object sample;
  Lisp_Object type_obj;		/* base type of vector elements */
  Lisp_Object device;

  sample = v->contents[0];
  type_obj = sample;
  if (X_RESOURCEP (sample))
    {
      CHECK_LIVE_X_RESOURCE (sample);
      rtype = XX_RESOURCE (sample)->type;
      device = XX_RESOURCE (sample)->device;
    }
  length = v->size;
  element = v->contents;

  for (i = 1; i < length; ++i, ++element)
    {
      QUIT;
      if (X_RESOURCEP (type_obj))
	CHECK_LIVE_X_RESOURCE (type_obj);
      if ((XTYPE (*element) != XTYPE (type_obj))
	  || (LRECORDP (type_obj) &&
	      (XRECORD_LHEADER (*element)->implementation !=
	       XRECORD_LHEADER (type_obj)->implementation))
	  || (X_RESOURCEP (type_obj) &&
	      (rtype != XX_RESOURCE (*element)->type
	       || !EQ (device, XX_RESOURCE (*element)->device))))
	error ("Vector has inconsistent types");
    }
}

static void
verify_list_has_consistent_type (Lisp_Object list)
{
  Lisp_Object type_obj;
  XID rtype;			/* X_resource type (if vector of
				   X_resources) */
  Lisp_Object temp = Fcar (list);
  Lisp_Object device;

  type_obj = temp;
  if (X_RESOURCEP (temp))
    {
      CHECK_LIVE_X_RESOURCE (temp);
      rtype = XX_RESOURCE (temp)->type;
      device = XX_RESOURCE (temp)->device;
    }
  list = Fcdr (list);

  for ( ; !NILP (list) ; list = Fcdr (list))
    {
      QUIT;
      temp = Fcar (list);
      if (X_RESOURCEP (temp))
	CHECK_LIVE_X_RESOURCE (temp);
      if ((XTYPE (temp) != XTYPE (type_obj))
	  || (LRECORDP (type_obj) &&
	      (XRECORD_LHEADER (temp)->implementation !=
	       XRECORD_LHEADER (type_obj)->implementation))
	  || (X_RESOURCEP (type_obj) &&
	      (rtype != XX_RESOURCE (temp)->type
	       || !EQ (device, XX_RESOURCE (temp)->device))))
	error ("List has inconsistent types");
    }
}

#define BYTESIZE 8
/* 16 bit types */
typedef short int int16;
typedef short unsigned int uint16;

/* the Calculate functions return allocated memory that must be free'd.
   I tried to use alloca, but that fails. Sigh.
*/
static void *
calculate_vector_property (Lisp_Object vector, unsigned long *count,
			   Atom *type, int *format)
{
  /* !!#### This function has not been Mule-ized */
  int length;
  unsigned int size,tsize;
  int i;
  struct Lisp_Vector *v;
  void *addr;

  v = XVECTOR (vector);
  *count = length = v->size;

  switch (XTYPE (v->contents[0]))
    {
    case Lisp_Int:
      *type = XA_INTEGER;
      if (*format != 8 && *format != 16) *format = 32;
      size = *format * length;
      addr = (void *) xmalloc (size);
      for ( i = 0 ; i < length ; ++i )
	switch (*format)
	  {
	  case 32 :
	    ((int *)addr)[i] = XINT (v->contents[i]);
	    break;
	  case 16 :
	    ((int16 *)addr)[i] = XINT (v->contents[i]);
	    break;
	  case 8 :
	    ((char *)addr)[i] = XINT (v->contents[i]);
	    break;
	  }
      break;

    case Lisp_Record:
      if (X_RESOURCEP (v->contents[0]))
	{
	  CHECK_LIVE_X_RESOURCE (v->contents[0]);
	  size = BYTESIZE * sizeof (XID) * length;
	  *format = BYTESIZE * sizeof (XID);
	  *type = XX_RESOURCE (v->contents[0])->type;
	  addr = (void *) xmalloc (size);
	  for ( i = 0 ; i < length ; ++i )
	    ( (XID *) addr) [i] = XX_RESOURCE (v->contents[i])->xid;
	}
      break;

    case Lisp_String:
      *format = BYTESIZE * sizeof (char);
      *type = XA_STRING;
      for ( i=0, size=0 ; i < length ; ++i )
	size += (string_length (XSTRING (v->contents[i])) +
		 1); /* include null */
      addr = (void *) xmalloc (size);
      *count = size;
      for ( i = 0 , size = 0 ; i < length ; ++i )
	{
	  tsize = string_length (XSTRING (v->contents[i])) + 1;
	  memmove (((char *) addr), string_data (XSTRING (v->contents[i])),
		   tsize);
	  size += tsize;
	}
      break;

    default:
      error ("Invalid type for conversion");
    }
  return addr;
}

static void *
calculate_list_property (Lisp_Object list, unsigned long *count,
			 Atom *type, int *format)
{
  /* !!#### This function has not been Mule-ized */
  int length;
  unsigned int size, tsize;
  int i;
  Lisp_Object tlist,temp;
  void *addr;

  *count = length = XINT (Flength (list));

  switch (XTYPE (Fcar (list)))
    {
    case Lisp_Int:
      *type = XA_INTEGER;
      if (*format != 8 && *format != 16) *format = 32;
      size = *format * length;
      addr = (void *) xmalloc (size);
      for ( i = 0 ; i < length ; ++i, list = Fcdr (list))
	switch (*format)
	  {
	  case 32 : ((int *)addr)[i] = XINT (Fcar (list)); break;
	  case 16 : ((int16 *)addr)[i] = XINT (Fcar (list)); break;
	  case 8 : ((char *)addr)[i] = XINT (Fcar (list)); break;
	  }
      break;

    case Lisp_Record:
      if (X_RESOURCEP (Fcar (list)))
	{
	  Lisp_Object car = Fcar (list);
	  CHECK_LIVE_X_RESOURCE (car);
	  size = BYTESIZE * sizeof (XID) * length;
	  *format = BYTESIZE * sizeof (XID);
	  *type = XX_RESOURCE (Fcar (list))->type;
	  addr = (void *) xmalloc (size);
	  for ( i = 0 ; i < length ; ++i, list = Fcdr (list))
	    {
	      Lisp_Object carr = Fcar (list);
	      CHECK_LIVE_X_RESOURCE (carr);
	      ((XID *)addr)[i] = XX_RESOURCE (carr)->xid;
	    }
	}
      break;

    case Lisp_String:
      *format = BYTESIZE * sizeof (char);
      *type = XA_STRING;
      for ( i=0, size=0 , tlist=list ; i < length ; ++i, tlist = Fcdr (tlist) )
	size += string_length (XSTRING (Fcar (tlist))) + 1; /* include null */
      addr = (void *) xmalloc (size);
      *count = size;
      for ( i=0, size=0, tlist=list ; i < length  ;
	   ++i , tlist = Fcdr (tlist) )
	{
	  temp = Fcar (tlist);
	  tsize = string_length (XSTRING (temp)) + 1;
	  memmove (((char *) addr), string_data (XSTRING (temp)), tsize);
	  size += tsize;
	}
      break;

    default:
      error ("Invalid type for conversion");
    }
  return addr;
}

/* Returns whether the conversion was successful or not */
static int
convert_elisp_to_x (Lisp_Object value, void **addr, unsigned long *count,
		    Atom *type, int *format, int *free_storage)
{
  /* !!#### This function has not been Mule-ized */
  if (VECTORP (value))
    verify_vector_has_consistent_type (value);
  else if (CONSP (value))
    verify_list_has_consistent_type (value);

  *free_storage = 0;
  switch (XTYPE (value))
    {
    case Lisp_String:
      *format = BYTESIZE;
      *type = XA_STRING;
      *count = strlen ((CONST char *) string_data (XSTRING (value))) + 1;
      *addr = (void *) string_data (XSTRING (value));
      break;

    case Lisp_Int:
      *type = XA_INTEGER;
      *count = 1;
      *free_storage = 1;
      *addr = (void *) xmalloc (sizeof (int));
      /* This is ugly -
       * we have to deal with the possibility of different formats
       */
      switch (*format)
	{
	default :
	case 32 :
	  *format = 32;
	  *((int *)(*addr)) = XINT (value);
	  break;
        case 16 :
	  *((int16 *)(*addr)) = XINT (value);
	  break;
        case 8 :
	  *((char *)(*addr)) = XINT (value);
	  break;
      }
      break;

    case Lisp_Record:
      if (X_RESOURCEP (value))
	{
	  CHECK_LIVE_X_RESOURCE (value);
	  *format = sizeof (XID) * BYTESIZE;
	  *type = XX_RESOURCE (value)->type;
	  *count = 1;
	  *addr = (void *) & (XX_RESOURCE (value)->xid);
	}
      break;

    case Lisp_Cons:
      *addr = calculate_list_property (value, count, type, format);
      *free_storage = 1;	/* above allocates storage */
      break;

    case Lisp_Vector:
      *addr = calculate_vector_property (value, count, type, format);
      *free_storage = 1;	/* above allocates storage */
      break;

    default :
      error ("Improper type for conversion");
    }

  return 1;
}

static Lisp_Object
format_size_hints (XSizeHints *hints)
{
  Lisp_Object result;
  struct Lisp_Vector *v;

  result = Fmake_vector (make_int (6), Qnil);
  v = XVECTOR (result);

  /* ugly but straightforward - just step through the members and flags
   * and stick in the ones that are there
   */
  if (hints->flags & (PPosition|USPosition))
    v->contents[0] = Fcons (make_int (hints->x), make_int (hints->y));
  if (hints->flags & (PSize|USSize))
    v->contents[1] = Fcons (make_int (hints->width),
			   make_int (hints->height));
  if (hints->flags & PMinSize)
    v->contents[2] = Fcons (make_int (hints->min_width),
			   make_int (hints->min_height));
  if (hints->flags & PMaxSize)
    v->contents[3] = Fcons (make_int (hints->max_width),
			   make_int (hints->max_height));
  if (hints->flags & PResizeInc)
        v->contents[4] = Fcons (make_int (hints->width_inc),
                               make_int (hints->height_inc));
  if (hints->flags & PAspect)
    v->contents[5] = Fcons (make_int (hints->min_aspect.x),
			   Fcons (make_int (hints->min_aspect.y),
				 Fcons (make_int (hints->max_aspect.x),
				       make_int (hints->max_aspect.y))));

  return result;
}

static Lisp_Object
format_string_property (char *buffer, unsigned long count)
{
  /* !!#### This function has not been Mule-ized */
  Lisp_Object value = Qnil;		/* data */
  Lisp_Object temp;			/* temp value holder */
  int len;				/* length of current string */
  char *strend;

  while (count)
    {
      strend = memchr (buffer, 0, (int) count);
      len = strend ? strend - buffer : count;
      if (len)
	{
	  temp = make_string ((Bufbyte *) buffer, len);
	  value = Fcons (temp, value);
	}
      buffer = strend + 1;	/* skip null, or leaving loop if no null */
      count -= len + !!strend;
    }

  return (NILP (Fcdr (value))
	  ? Fcar (value)
	  : Fnreverse (value));
}

static Lisp_Object
format_integer_32_property (long *buff, unsigned long count)
{
  Lisp_Object value = Qnil;	/* return value */
  while (count)
    value = Fcons (make_int (buff[--count]), value);

  return (NILP (Fcdr (value))
	  ? Fcar (value)
	  : value);
}

static Lisp_Object
format_integer_16_property (int16 *buff, unsigned long count)
{
  Lisp_Object value = Qnil;	/* return value */

  while (count)
    value = Fcons (make_int (buff[--count]), value);

  return (NILP (Fcdr (value))
	  ? Fcar (value)
	  : value);
}

static Lisp_Object
format_integer_8_property (char *buff, unsigned long count)
{
  Lisp_Object value = Qnil;	/* return value */

  while (count)
    value = Fcons (make_int (buff[--count]), value);

  return (NILP (Fcdr (value))
	  ? Fcar (value)
	  : value);
}

static Lisp_Object
format_integer_property (void *buff, unsigned long count, int format)
{
  switch (format)
    {
    case 8:
      return format_integer_8_property ((char *) buff, count);
      break;
    case 16:
      return format_integer_16_property ((int16 *) buff, count);
      break;
    case 32:
      return format_integer_32_property ((long *) buff, count);
      break;
    default:
      return Qnil;
    }
}

static Lisp_Object
format_cardinal_32_property (unsigned long *buff, unsigned long count)
{
  Lisp_Object value = Qnil;	/* return value */

  while (count)
    value = Fcons (make_int (buff[--count]), value);

  return (NILP (Fcdr (value))
	  ? Fcar (value)
	  : value);
}

static Lisp_Object
format_cardinal_16_property (uint16 *buff, unsigned long count)
{
  Lisp_Object value = Qnil;	/* return value */

  while (count)
    value = Fcons (make_int (buff[--count]), value);

  return (NILP (Fcdr (value))
	  ? Fcar (value)
	  : value);
}

static Lisp_Object
format_cardinal_8_property (unsigned char *buff, unsigned long count)
{
  Lisp_Object value = Qnil;	/* return value */

  while (count)
    value = Fcons (make_int (buff[--count]), value);

  return (NILP (Fcdr (value))
	  ? Fcar (value)
	  : value);
}

static Lisp_Object
format_cardinal_property (void *buff, unsigned long count, int format)
{
  switch (format)
    {
    case 8:
      return format_cardinal_8_property ((unsigned char *) buff, count);
      break;
    case 16:
      return format_cardinal_16_property ((uint16 *) buff, count);
      break;
    case 32:
      return format_cardinal_32_property ((unsigned long *) buff, count);
    default:
      return Qnil;
    }
}

static Lisp_Object
format_unknown_property (struct device *d, void *buff, unsigned long count,
			 Atom type, int format)
{
  Lisp_Object value = Qnil;	/* return value */
  Lisp_Object device = Qnil;

  XSETDEVICE (device, d);

  switch (format)
    {
    case 32:
      {
	XID *xid = (XID *) buff;
	int non_zero = 0;
	while (count--)
	  if (non_zero || xid[count])
	    {
	      value = Fcons (make_x_resource (xid[count], type, device),
			     value);
	      non_zero = 1;
	    }
      }
      break;
    }

  return (NILP (Fcdr (value))
	  ? Fcar (value)
	  : value);
}

static Lisp_Object
convert_x_to_elisp (struct device *d, void *buffer, unsigned long count,
		    Atom type, int format)
{
  /* !!#### This function has not been Mule-ized */
  Lisp_Object value = Qnil;

  switch (type)
    {
    case None:
      value = Qnil;
      break;
    case XA_STRING:
      value = format_string_property (buffer, count);
      break;
    case XA_INTEGER:
      value = format_integer_property ((long *) buffer, count, format);
      break;
    case XA_CARDINAL:
      value = format_cardinal_property ((unsigned long *) buffer,
					count, format);
      break;
    case XA_WM_SIZE_HINTS:
      value = format_size_hints ((XSizeHints *) buffer);
      break;
    default:
      value = format_unknown_property (d, (void *) buffer, count, type,
				       format);
      break;
    }

  return value;
}

/* get a property given its atom, device, and window */
static Lisp_Object
raw_get_property (struct device *d, Window win, Atom prop)
{
  /* !!#### This function has not been Mule-ized */
  Lisp_Object value = Qnil;
  Atom actual_type;
  int actual_format;
  unsigned char *buffer;
  unsigned long count, remaining;
  int zret;
  Display *dpy = DEVICE_X_DISPLAY (d);

  zret = XGetWindowProperty (dpy, win, prop,
			     0L, 1024L, False, AnyPropertyType,
			     &actual_type, &actual_format,
			     &count, &remaining, &buffer);

  /* If remaining is set, then there's more of the property to get.
     Let's just do the whole read again, this time with enough space
     to get it all. */
  if (zret == Success && remaining > 0)
    {
      XFree (buffer);
      zret = XGetWindowProperty (dpy, win, prop,
				 0L, 1024L + ((remaining + 3) / 4),
				 False, AnyPropertyType,
				 &actual_type, &actual_format,
				 &count, &remaining, &buffer);
    }

  if (zret != Success)
    return Qnil;		/* failed */

  value = convert_x_to_elisp (d, buffer, count, actual_type, actual_format);

  XFree (buffer);
  return value;
}

/*
 * Epoch equivalent:  epoch::get-property
 */
DEFUN ("x-get-property", Fx_get_property, Sx_get_property, 1, 2, 0 /*
Retrieve the X window property for a frame. Arguments are
PROPERTY: must be a string or an X-resource of type ATOM.
FRAME: (optional) If present, must be a frame object, a frame id, or
and X-resource of type WINDOW. Defaults to the current frame.
Returns the value of the property, or nil if the property couldn't
be retrieved.
*/ )
     (name, frame)
     Lisp_Object name, frame;
{
  Atom prop = None;
  Lisp_Object device;
  Display *dpy;
  Window win;
  
  /* We can't use Fx_id_of_frame because it returns the xid of
     the shell widget.  But the property change has to take place
     on the edit widget in order for a PropertyNotify event to
     be generated */
  epoch_get_window_and_device (frame, &win, &device, 1);
  dpy = DEVICE_X_DISPLAY (XDEVICE (device));

  if (STRINGP (name) || SYMBOLP (name))
    {
      prop = symbol_to_x_atom (XDEVICE (device),
			       get_symbol_or_string_as_symbol (name),
			       1);
    }
  else if (X_RESOURCEP (name))
    {
      CHECK_LIVE_X_RESOURCE (name);
      if (XX_RESOURCE (name)->type != XA_ATOM)
	error ("Property must be an ATOM X-resource");
      prop = XX_RESOURCE (name)->xid;
    }
  else
    error ("Property must be a string or X-resource ATOM");

  if (prop == None)
    return Qnil;

  /* now we have the atom, let's ask for the property! */
  return raw_get_property (XDEVICE (device), win, prop);
}

static Lisp_Object
raw_set_property (Display *dpy, Window win, Atom prop, Lisp_Object value)
{
  /* !!#### This function has not been Mule-ized */
  Atom actual_type;		/* X type of items */
  int actual_format;		/* size of data items (8,16,32) */
  unsigned long count;		/* Number of data items */
  void* addr;			/* address of data item array */
  int zret;			/* X call return value */
  int free_storage;		/* set if addr points at non-malloc'd store */

  actual_format = 0;		/* don't force a particular format */
  convert_elisp_to_x (value, &addr, &count, &actual_type, &actual_format,
		      &free_storage);

  zret = XChangeProperty (dpy, win, prop, actual_type, actual_format,
			  PropModeReplace, (char *) addr, count);
  XFlush (dpy);

  if (free_storage)
    xfree (addr);

  return value;
}

DEFUN ("x-set-property", Fx_set_property, Sx_set_property, 2, 3, 0 /*
Set a named property for a frame. The first argument (required)
is the name of the property. The second is the value to set the propery
to. The third (optional) is the frame, default is
the current frame.
*/ )
     (name, value, frame)
     Lisp_Object name, value, frame;
{
  Atom prop = None;		/* name of the property */
  Lisp_Object device;
  Display *dpy;
  Window win;
  
  /* We can't use Fx_id_of_frame because it returns the xid of
     the shell widget.  But the property change has to take place
     on the edit widget in order for a PropertyNotify event to
     be generated */
  epoch_get_window_and_device (frame, &win, &device, 1);
  dpy = DEVICE_X_DISPLAY (XDEVICE (device));

  /* parse the atom name, either a string or an actual atom */
  if (STRINGP (name) || SYMBOLP (name))
    {
      prop = symbol_to_x_atom (XDEVICE (device),
			       get_symbol_or_string_as_symbol (name),
			       0);
    }
  else if (X_RESOURCEP (name))
    {
      CHECK_LIVE_X_RESOURCE (name);
      if (XX_RESOURCE (name)->type != XA_ATOM)
	error ("Property must be an X-resource ATOM");
      prop = XX_RESOURCE (name)->xid;
    }
  else
    error ("Property must be a string or X-resource ATOM");

  if (prop == None)
    return Qnil;

  /* that's it. Now set it */
  return raw_set_property (dpy, win, prop, value);
}

/*
 * Epoch equivalent:  epoch::send-client-message
 */
DEFUN ("x-send-client-message", Fx_send_client_message, Sx_send_client_message,
       1, 5, 0 /*
Send a client message to DEST, marking it as being from SOURCE.
The message is DATA of TYPE with FORMAT.  If TYPE and FORMAT are omitted,
they are deduced from DATA.  If SOURCE is nil, the current frame is used.
*/ )
     (dest, source, data, type, format)
     Lisp_Object dest, source, data, type, format;
{
  /* !!#### This function has not been Mule-ized */
  int actual_format = 0;
  Atom actual_type;
  unsigned long count;
  void *addr;
  int free_storage;
  XEvent ev;
  Lisp_Object result;
  Window dest_win;
  Lisp_Object dest_device;
  Window src_win;
  Lisp_Object src_device;
  Display *dpy;

  epoch_get_window_and_device (dest, &dest_win, &dest_device, 0);

  if (NILP (source))
    /* This catches a return of nil */
    XSETFRAME (source, device_selected_frame (XDEVICE (dest_device)));

  epoch_get_window_and_device (source, &src_win, &src_device, 0);

  if (!EQ (src_device, dest_device))
    error ("Destination and source must be on the same device");

  dpy = DEVICE_X_DISPLAY (XDEVICE (dest_device));

  ev.xclient.window = src_win;

  /* check format before data, because it can cause the data format to vary */
  if (!NILP (format))
    {
      CHECK_INT (format);
      actual_format = XINT (format);
      if (actual_format != 8 && actual_format != 16 && actual_format != 32)
	error ("Format must be 8, 16, or 32, or nil");
    }

  /* clear out any cruft */
  memset ((char *) &ev.xclient.data, 0, 20);

  /* look for the data */
  if (!NILP (data))
    {
      convert_elisp_to_x (data, &addr, &count, &actual_type, &actual_format,
			  &free_storage);
      if ((count * actual_format) > 20*8)
	{
	  if (free_storage)
	    xfree (addr);
	  error ("Data is too big to fit in a client message");
	}
      memmove (&ev.xclient.data, (char *)addr, count * (actual_format/8));
      if (free_storage)
	xfree (addr);
    }

  if (!NILP (type))
    {
      CHECK_LIVE_X_RESOURCE (type);
      if (XX_RESOURCE (type)->type != XA_ATOM)
        error ("Resource for message type must be an atom");
      actual_type = XX_RESOURCE (type)->xid;
    }
      
  ev.xany.type = ClientMessage;
  ev.xclient.message_type = actual_type;
  ev.xclient.format = actual_format;
  /* There's no better way to set the mask than to hard code the correct
   * width bit pattern. 1L<<24 == OwnerGrabButtonMask, is the largest
   * This is the word from the X-consortium.
   */
  result = (XSendEvent (dpy, dest_win, False, (1L<<25)-1L,&ev)
	    ? Qt
	    : Qnil);
  XFlush (dpy);
  return result;
}

/*
 * These duplicate the needed functionality from the Epoch event handler.
 */
static Lisp_Object
read_client_message (struct device *d, XClientMessageEvent *cm)
{
  Lisp_Object result;
  Lisp_Object device = Qnil;

  XSETDEVICE (device, d);
  if (!cm->format)	/* this is probably a sign of a bug somewhere else */
    result = Qnil;
  else
    result = Fcons (make_x_resource (cm->message_type, XA_ATOM, device),
		    Fcons (make_x_resource (cm->window, XA_WINDOW, device),
			   convert_x_to_elisp (d, (void *) cm->data.b,
					       (20*8)/cm->format,
					       cm->message_type,
					       cm->format)));

  return result;
}

static Lisp_Object
read_property_event (XPropertyEvent *pe, Lisp_Object frame)
{
  Lisp_Object result, value;
  struct frame *f = XFRAME (frame);
  struct device *d = XDEVICE (FRAME_DEVICE (f));
  Lisp_Object atom;

  atom = x_atom_to_symbol (d, pe->atom);

  /* didn't get a name, blow this one off */
  if (NILP (atom))
    return Qnil;

  /* We can't use Fx_id_of_frame because it returns the xid of
     the shell widget.  But the property change has to take place
     on the edit widget in order for a PropertyNotify event to
     be generated */
  value = raw_get_property (d, XtWindow (FRAME_X_TEXT_WIDGET (f)),
			    pe->atom);
  result = Fcons (Fsymbol_name (atom), value);

  return result;
}

void dispatch_epoch_event (struct frame *f, XEvent *event, Lisp_Object type);
void
dispatch_epoch_event (struct frame *f, XEvent *event, Lisp_Object type)
{
  /* This function can GC */
  struct Lisp_Vector *evp;
  struct device *d = XDEVICE (FRAME_DEVICE (f));

  if (NILP (Vepoch_event_handler))
    return;

  if (!VECTORP (Vepoch_event) || XVECTOR (Vepoch_event)->size < 3)
    Vepoch_event = Fmake_vector (make_int (3), Qnil);
  evp = XVECTOR (Vepoch_event);

  XSETFRAME (evp->contents[2], f);

  if (EQ (type, Qx_property_change))
    {
      evp->contents[0] = Qx_property_change;
      evp->contents[1] =
	read_property_event (&event->xproperty, evp->contents[2]);
    }
  else if (EQ (type, Qx_client_message))
    {
      evp->contents[0] = Qx_client_message;
      evp->contents[1] = read_client_message (d, &event->xclient);
    }
  else if (EQ (type, Qx_map))
    {
      evp->contents[0] = Qx_map;
      evp->contents[1] = Qt;
    }
  else if (EQ (type, Qx_unmap))
    {
      evp->contents[0] = Qx_unmap;
      evp->contents[1] = Qnil;
    }
  else
    {
      Vepoch_event = Qnil;
    }

  if (NILP (Vepoch_event))
    return;

  Ffuncall (1, &Vepoch_event_handler);

  Vepoch_event = Qnil;
  return;
}


void
syms_of_epoch (void)
{
  defsubr (&Sx_intern_atom);
  defsubr (&Sx_atom_name);
  defsubr (&Sstring_to_x_resource);
  defsubr (&Sx_resource_to_type);
  defsubr (&Sx_resource_to_string);
  defsubr (&Sx_id_of_frame);
  defsubr (&Sx_query_tree);
  defsubr (&Sx_get_property);
  defsubr (&Sx_set_property);
  defsubr (&Sx_send_client_message);
  defsubr (&Sx_resource_p);
  defsubr (&Sx_resource_device);
  defsubr (&Sx_resource_live_p);
  defsubr (&Sset_x_resource_type);

  defsymbol (&Qx_resourcep, "x-resource-p");
  defsymbol (&Qx_resource_live_p, "x-resource-live-p");
  defsymbol (&Qx_property_change, "x-property-change");
  defsymbol (&Qx_client_message, "x-client-message");
  defsymbol (&Qx_map, "x-map");
  defsymbol (&Qx_unmap, "x-unmap");
}

void
vars_of_epoch (void)
{
  Fprovide (intern ("epoch"));

  DEFVAR_LISP ("epoch-event-handler", &Vepoch_event_handler /*
If this variable is not nil, then it is assumed to have
a function in it.  When an epoch event is received for a frame, this
function is called.
*/ );
  Vepoch_event_handler = Qnil;

  DEFVAR_LISP ("epoch-event", &Vepoch_event /*
Bound to the value of the current event when epoch-event-handler is called.
*/ );
  Vepoch_event = Qnil;
}
