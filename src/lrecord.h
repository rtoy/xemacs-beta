/* The "lrecord" structure (header of a compound lisp object).
   Copyright (C) 1993, 1994, 1995 Free Software Foundation, Inc.
   Copyright (C) 1996, 2001, 2002 Ben Wing.

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

#ifndef INCLUDED_lrecord_h_
#define INCLUDED_lrecord_h_

/* The "lrecord" type of Lisp object is used for all object types
   other than a few simple ones.  This allows many types to be
   implemented but only a few bits required in a Lisp object for type
   information. (The tradeoff is that each object has its type marked
   in it, thereby increasing its size.) All lrecords begin with a
   `struct lrecord_header', which identifies the lisp object type, by
   providing an index into a table of `struct lrecord_implementation',
   which describes the behavior of the lisp object.  It also contains
   some other data bits.

   Lrecords are of two types: straight lrecords, and lcrecords.
   Straight lrecords are used for those types of objects that have
   their own allocation routines (typically allocated out of 2K chunks
   of memory called `frob blocks').  These objects have a `struct
   lrecord_header' at the top, containing only the bits needed to find
   the lrecord_implementation for the object.  There are special
   routines in alloc.c to deal with each such object type.

   Lcrecords are used for less common sorts of objects that don't do
   their own allocation.  Each such object is malloc()ed individually,
   and the objects are chained together through a `next' pointer.
   Lcrecords have a `struct lcrecord_header' at the top, which
   contains a `struct lrecord_header' and a `next' pointer, and are
   allocated using alloc_lcrecord().

   Creating a new lcrecord type is fairly easy; just follow the
   lead of some existing type (e.g. hash tables).  Note that you
   do not need to supply all the methods (see below); reasonable
   defaults are provided for many of them.  Alternatively, if you're
   just looking for a way of encapsulating data (which possibly
   could contain Lisp_Objects in it), you may well be able to use
   the opaque type. */

struct lrecord_header
{
  /* index into lrecord_implementations_table[] */
  unsigned int type :8;

  /* If `mark' is 0 after the GC mark phase, the object will be freed
     during the GC sweep phase.  There are 2 ways that `mark' can be 1:
     - by being referenced from other objects during the GC mark phase
     - because it is permanently on, for c_readonly objects */
  unsigned int mark :1;

  /* 1 if the object resides in logically read-only space, and does not
     reference other non-c_readonly objects.
     Invariant: if (c_readonly == 1), then (mark == 1 && lisp_readonly == 1) */
  unsigned int c_readonly :1;

  /* 1 if the object is readonly from lisp */
  unsigned int lisp_readonly :1;

  unsigned int unused :21;
};

struct lrecord_implementation;
int lrecord_type_index (const struct lrecord_implementation *implementation);

#define set_lheader_implementation(header,imp) do {	\
  struct lrecord_header* SLI_header = (header);		\
  SLI_header->type = (imp)->lrecord_type_index;		\
  SLI_header->mark = 0;					\
  SLI_header->c_readonly = 0;				\
  SLI_header->lisp_readonly = 0;			\
} while (0)

struct lcrecord_header
{
  struct lrecord_header lheader;

  /* The `next' field is normally used to chain all lcrecords together
     so that the GC can find (and free) all of them.
     `alloc_lcrecord' threads lcrecords together.

     The `next' field may be used for other purposes as long as some
     other mechanism is provided for letting the GC do its work.

     For example, the event and marker object types allocate members
     out of memory chunks, and are able to find all unmarked members
     by sweeping through the elements of the list of chunks.  */
  struct lcrecord_header *next;

  /* The `uid' field is just for debugging/printing convenience.
     Having this slot doesn't hurt us much spacewise, since an
     lcrecord already has the above slots plus malloc overhead. */
  unsigned int uid :31;

  /* The `free' field is a flag that indicates whether this lcrecord
     is on a "free list".  Free lists are used to minimize the number
     of calls to malloc() when we're repeatedly allocating and freeing
     a number of the same sort of lcrecord.  Lcrecords on a free list
     always get marked in a different fashion, so we can use this flag
     as a sanity check to make sure that free lists only have freed
     lcrecords and there are no freed lcrecords elsewhere. */
  unsigned int free :1;
};

/* Used for lcrecords in an lcrecord-list. */
struct free_lcrecord_header
{
  struct lcrecord_header lcheader;
  Lisp_Object chain;
};

enum lrecord_type
{
  /* Symbol value magic types come first to make SYMBOL_VALUE_MAGIC_P fast.
     #### This should be replaced by a symbol_value_magic_p flag
     in the Lisp_Symbol lrecord_header. */
  lrecord_type_symbol_value_forward,
  lrecord_type_symbol_value_varalias,
  lrecord_type_symbol_value_lisp_magic,
  lrecord_type_symbol_value_buffer_local,
  lrecord_type_max_symbol_value_magic = lrecord_type_symbol_value_buffer_local,

  lrecord_type_symbol,
  lrecord_type_subr,
  lrecord_type_cons,
  lrecord_type_vector,
  lrecord_type_string,
  lrecord_type_lcrecord_list,
  lrecord_type_compiled_function,
  lrecord_type_weak_list,
  lrecord_type_bit_vector,
  lrecord_type_float,
  lrecord_type_hash_table,
  lrecord_type_lstream,
  lrecord_type_process,
  lrecord_type_charset,
  lrecord_type_coding_system,
  lrecord_type_char_table,
  lrecord_type_char_table_entry,
  lrecord_type_range_table,
  lrecord_type_opaque,
  lrecord_type_opaque_ptr,
  lrecord_type_buffer,
  lrecord_type_extent,
  lrecord_type_extent_info,
  lrecord_type_extent_auxiliary,
  lrecord_type_marker,
  lrecord_type_event,
  lrecord_type_keymap,
  lrecord_type_command_builder,
  lrecord_type_timeout,
  lrecord_type_specifier,
  lrecord_type_console,
  lrecord_type_device,
  lrecord_type_frame,
  lrecord_type_window,
  lrecord_type_window_mirror,
  lrecord_type_window_configuration,
  lrecord_type_gui_item,
  lrecord_type_popup_data,
  lrecord_type_toolbar_button,
  lrecord_type_scrollbar_instance,
  lrecord_type_color_instance,
  lrecord_type_font_instance,
  lrecord_type_image_instance,
  lrecord_type_glyph,
  lrecord_type_face,
  lrecord_type_database,
  lrecord_type_tooltalk_message,
  lrecord_type_tooltalk_pattern,
  lrecord_type_ldap,
  lrecord_type_pgconn,
  lrecord_type_pgresult,
  lrecord_type_devmode,
  lrecord_type_mswindows_dialog_id,
  lrecord_type_case_table,
  lrecord_type_emacs_ffi,
  lrecord_type_emacs_gtk_object,
  lrecord_type_emacs_gtk_boxed,
  lrecord_type_free, /* only used for "free" lrecords */
  lrecord_type_undefined, /* only used for debugging */
  lrecord_type_last_built_in_type /* must be last */
};

extern int lrecord_type_count;

struct lrecord_implementation
{
  const char *name;

  /* `marker' is called at GC time, to make sure that all Lisp_Objects
     pointed to by this object get properly marked.  It should call
     the mark_object function on all Lisp_Objects in the object.  If
     the return value is non-nil, it should be a Lisp_Object to be
     marked (don't call the mark_object function explicitly on it,
     because the GC routines will do this).  Doing it this way reduces
     recursion, so the object returned should preferably be the one
     with the deepest level of Lisp_Object pointers.  This function
     can be NULL, meaning no GC marking is necessary. */
  Lisp_Object (*marker) (Lisp_Object);

  /* `printer' converts the object to a printed representation.
     This can be NULL; in this case default_object_printer() will be
     used instead. */
  void (*printer) (Lisp_Object, Lisp_Object printcharfun, int escapeflag);

  /* `finalizer' is called at GC time when the object is about to
     be freed, and at dump time (FOR_DISKSAVE will be non-zero in this
     case).  It should perform any necessary cleanup (e.g. freeing
     malloc()ed memory).  This can be NULL, meaning no special
     finalization is necessary.

     WARNING: remember that `finalizer' is called at dump time even
     though the object is not being freed. */
  void (*finalizer) (void *header, int for_disksave);

  /* This can be NULL, meaning compare objects with EQ(). */
  int (*equal) (Lisp_Object obj1, Lisp_Object obj2, int depth);

  /* `hash' generates hash values for use with hash tables that have
     `equal' as their test function.  This can be NULL, meaning use
     the Lisp_Object itself as the hash.  But, you must still satisfy
     the constraint that if two objects are `equal', then they *must*
     hash to the same value in order for hash tables to work properly.
     This means that `hash' can be NULL only if the `equal' method is
     also NULL. */
  unsigned long (*hash) (Lisp_Object, int);

  /* External data layout description */
  const struct lrecord_description *description;

  /* These functions allow any object type to have builtin property
     lists that can be manipulated from the lisp level with
     `get', `put', `remprop', and `object-plist'. */
  Lisp_Object (*getprop) (Lisp_Object obj, Lisp_Object prop);
  int (*putprop) (Lisp_Object obj, Lisp_Object prop, Lisp_Object val);
  int (*remprop) (Lisp_Object obj, Lisp_Object prop);
  Lisp_Object (*plist) (Lisp_Object obj);

  /* Only one of `static_size' and `size_in_bytes_method' is non-0.
     If both are 0, this type is not instantiable by alloc_lcrecord(). */
  Bytecount static_size;
  Bytecount (*size_in_bytes_method) (const void *header);

  /* The (constant) index into lrecord_implementations_table */
  enum lrecord_type lrecord_type_index;

  /* A "basic" lrecord is any lrecord that's not an lcrecord, i.e.
     one that does not have an lcrecord_header at the front and which
     is (usually) allocated in frob blocks.  We only use this flag for
     some consistency checking, and that only when error-checking is
     enabled. */
  unsigned int basic_p :1;
};

/* All the built-in lisp object types are enumerated in `enum lrecord_type'.
   Additional ones may be defined by a module (none yet).  We leave some
   room in `lrecord_implementations_table' for such new lisp object types. */
#define MODULE_DEFINABLE_TYPE_COUNT 32

extern const struct lrecord_implementation *lrecord_implementations_table[lrecord_type_last_built_in_type + MODULE_DEFINABLE_TYPE_COUNT];

#define XRECORD_LHEADER_IMPLEMENTATION(obj) \
   LHEADER_IMPLEMENTATION (XRECORD_LHEADER (obj))
#define LHEADER_IMPLEMENTATION(lh) lrecord_implementations_table[(lh)->type]

extern int gc_in_progress;

#define MARKED_RECORD_P(obj) (XRECORD_LHEADER (obj)->mark)
#define MARKED_RECORD_HEADER_P(lheader) ((lheader)->mark)
#define MARK_RECORD_HEADER(lheader)   ((void) ((lheader)->mark = 1))
#define UNMARK_RECORD_HEADER(lheader) ((void) ((lheader)->mark = 0))

#define C_READONLY_RECORD_HEADER_P(lheader)  ((lheader)->c_readonly)
#define LISP_READONLY_RECORD_HEADER_P(lheader)  ((lheader)->lisp_readonly)
#define SET_C_READONLY_RECORD_HEADER(lheader) do {	\
  struct lrecord_header *SCRRH_lheader = (lheader);	\
  SCRRH_lheader->c_readonly = 1;			\
  SCRRH_lheader->lisp_readonly = 1;			\
  SCRRH_lheader->mark = 1;				\
} while (0)
#define SET_LISP_READONLY_RECORD_HEADER(lheader) \
  ((void) ((lheader)->lisp_readonly = 1))
#define RECORD_MARKER(lheader) lrecord_markers[(lheader)->type]

/* External description stuff

   PLEASE NOTE: Both lrecord_description and struct_description are
   badly misnamed.  In reality, an lrecord_description is nothing more
   than a list of the elements in a block of memory that need
   relocating or other special handling, and a struct_description is
   no more than an lrecord_description plus the size of the block of
   memory. (In fact, a struct_description can now have its size given
   as zero, i.e. unspecified, meaning that the last element in the
   structure is noted in the list and the size of the block can
   therefore be computed from it.) The names stem from the fact
   lrecord_descriptions are used to describe lrecords (the size of the
   lrecord is elsewhere in its description, attached to its methods,
   so it does not need to be given here), while struct_descriptions
   are used to describe C structs; but both are used in various
   additional ways.  Much better terms would be memory_description and
   sized_memory_description.

   An lrecord_description is an array of values. (This is actually
   misnamed, in that it does not just describe lrecords, but any
   blocks of memory.) The first value of each line is a type, the
   second the offset in the lrecord structure.  The third and
   following elements are parameters; their presence, type and number
   is type-dependent.

   The description ends with a "XD_END", "XD_SPECIFIER_END" or
   "XD_CODING_SYSTEM_END" record.

   The top-level description of an lrecord or lcrecord does not need
   to describe every element, just the ones that need to be relocated,
   since the size of the lrecord is known. (The same goes for nested
   structures, whenever the structure size is given, rather than being
   defaulted by specifying 0 for the size.)

   A struct_description is used for describing nested "structures".
   (Again a misnomer, since it can be used for any blocks of memory,
   not just structures.) It just contains a size for the memory block,
   a pointer to an lrecord_description, and (for unions only) a union
   constant, described below.  The size can be 0 (#### not yet
   implemented!), in which case the size will be determined from the
   largest offset logically referenced (i.e. last offset mentioned +
   size of that object).  This is useful for stretchy arrays.

   Some example descriptions :

   static const struct lrecord_description cons_description[] = {
     { XD_LISP_OBJECT, offsetof (Lisp_Cons, car) },
     { XD_LISP_OBJECT, offsetof (Lisp_Cons, cdr) },
     { XD_END }
   };

   Which means "two lisp objects starting at the 'car' and 'cdr' elements"

  static const struct lrecord_description string_description[] = {
    { XD_BYTECOUNT,       offsetof (Lisp_String, size) },
    { XD_OPAQUE_DATA_PTR, offsetof (Lisp_String, data), XD_INDIRECT(0, 1) },
    { XD_LISP_OBJECT,     offsetof (Lisp_String, plist) },
    { XD_END }
  };
  "A pointer to string data at 'data', the size of the pointed array being the value
   of the size variable plus 1, and one lisp object at 'plist'"

  The existing types :
    XD_LISP_OBJECT
  A Lisp object.  This is also the type to use for pointers to other lrecords.

    XD_LISP_OBJECT_ARRAY
  An array of Lisp objects or (equivalently) pointers to lrecords.
  The parameter (i.e. third element) is the count.  This would be declared
  as Lisp_Object foo[666].  For something declared as Lisp_Object *foo,
  use XD_STRUCT_PTR, whose description parameter is a struct_description
  consisting of only XD_LISP_OBJECT and XD_END.

    XD_LO_LINK
  Weak link in a linked list of objects of the same type.  This is a
  link that does NOT generate a GC reference.  Thus the pdumper will
  not automatically add the referenced object to the table of all
  objects to be dumped, and when storing and loading the dumped data
  will automatically prune unreferenced objects in the chain and link
  each referenced object to the next referenced object, even if it's
  many links away.  We also need to special handling of a similar
  nature for the root of the chain, which will be a staticpro()ed
  object.

    XD_OPAQUE_PTR
  Pointer to undumpable data.  Must be NULL when dumping.

    XD_STRUCT_PTR
  Pointer to block of described memory. (This is misnamed: It is NOT
  necessarily a pointer to a struct foo.) Parameters are number of
  contiguous blocks and struct_description.

    XD_STRUCT_ARRAY
  Array of blocks of described memory.  Parameters are number of
  structures and struct_description.  This differs from XD_STRUCT_PTR
  in that the parameter is declared as struct foo[666] instead of
  struct *foo.  In other words, the block of memory holding the
  structures is within the containing structure, rather than being
  elsewhere, with a pointer in the containing structure.

    XD_OPAQUE_DATA_PTR
  Pointer to dumpable opaque data.  Parameter is the size of the data.
  Pointed data must be relocatable without changes.

    XD_UNION
  Union of two or more different types of data.  Parameters are a
  constant which determines which type the data is (this is usually an
  XD_INDIRECT, referring to one of the fields in the structure), and
  an array of struct_descriptions, whose values are used as follows,
  which is *DIFFERENT* from their usage in XD_STRUCT_PTR: the first
  field is a constant, which is compared to the first parameter of the
  XD_UNION descriptor to determine if this description applies to the
  data at the given offset, and the second is a pointer to a *SINGLE*
  lrecord_description structure, describing the data being pointed at
  when the associated constant matches.  You can go ahead and create
  an array of lrecord_description structures and put an XD_END on it,
  but only the first one is used.  If the data being pointed at is a
  structure, you *MAY NOT* substitute an array of lrecord_description
  structures describing the structure; instead, use a single
  lrecord_description structure with an XD_STRUCT_PTR in it, and point
  it in turn to the description of the structure.  See charset.h for a
  description of how to use XD_UNION. (In other words, if the constant
  matches, the lrecord_description pointed at will in essence be
  substituted for the XD_UNION declaration.)

    XD_C_STRING
  Pointer to a C string.

    XD_DOC_STRING
  Pointer to a doc string (C string if positive, opaque value if negative)

    XD_INT_RESET
  An integer which will be reset to a given value in the dump file.


    XD_ELEMCOUNT
  Elemcount value.  Used for counts.

    XD_BYTECOUNT
  Bytecount value.  Used for counts.

    XD_HASHCODE
  Hashcode value.  Used for the results of hashing functions.

    XD_INT
  int value.  Used for counts.

    XD_LONG
  long value.  Used for counts.

    XD_BYTECOUNT
  bytecount value.  Used for counts.

    XD_END
  Special type indicating the end of the array.

    XD_SPECIFIER_END
  Special type indicating the end of the array for a specifier.  Extra
  description, describing the specifier-type-specific data at the end
  of the specifier object, is going to be fetched from the specifier
  methods.  This should occur exactly once, in the description of the
  specifier object, and the dump code knows how to special-case this
  by fetching the specifier_methods pointer from the appropriate place
  in the memory block (which will, of course, be a struct
  Lisp_Specifier), fetching the description of the
  specifier-type-specific data from this, and continuing processing
  the memory block.

    XD_CODING_SYSTEM_END
  Special type indicating the end of the array for a coding system.
  Extra description is going to be fetched from the coding system
  methods.  Works just like XD_SPECIFIER_END.


  Special macros:
    XD_INDIRECT(line, delta)
  Usable where  a "count" or "size"  is requested.  Gives the value of
  the element which is at line number 'line' in the description (count
  starts at zero) and adds delta to it.
*/

enum lrecord_description_type
{
  XD_LISP_OBJECT_ARRAY,
  XD_LISP_OBJECT,
  XD_LO_LINK,
  XD_OPAQUE_PTR,
  XD_STRUCT_PTR,
  XD_STRUCT_ARRAY,
  XD_OPAQUE_DATA_PTR,
  XD_UNION,
  XD_C_STRING,
  XD_DOC_STRING,
  XD_INT_RESET,
  XD_BYTECOUNT,
  XD_ELEMCOUNT,
  XD_HASHCODE,
  XD_INT,
  XD_LONG,
  XD_END,
  XD_SPECIFIER_END,
  XD_CODING_SYSTEM_END
};

struct lrecord_description
{
  enum lrecord_description_type type;
  int offset;
  EMACS_INT data1;
  const struct struct_description *data2;
};

struct struct_description
{
  Bytecount size;
  const struct lrecord_description *description;
};

#define XD_INDIRECT(val, delta) (-1-((val)|(delta<<8)))

#define XD_IS_INDIRECT(code) (code<0)
#define XD_INDIRECT_VAL(code) ((-1-code) & 255)
#define XD_INDIRECT_DELTA(code) (((-1-code)>>8) & 255)

#define XD_DYNARR_DESC(base_type, sub_desc) \
  { XD_STRUCT_PTR, offsetof (base_type, base), XD_INDIRECT(1, 0), sub_desc }, \
  { XD_INT,        offsetof (base_type, cur) }, \
  { XD_INT_RESET,  offsetof (base_type, max), XD_INDIRECT(1, 0) }

/* DEFINE_LRECORD_IMPLEMENTATION is for objects with constant size.
   DEFINE_LRECORD_SEQUENCE_IMPLEMENTATION is for objects whose size varies.
 */

#if defined (ERROR_CHECK_TYPECHECK)
# define DECLARE_ERROR_CHECK_TYPECHECK(c_name, structtype)
#else
# define DECLARE_ERROR_CHECK_TYPECHECK(c_name, structtype)
#endif

#define DEFINE_BASIC_LRECORD_IMPLEMENTATION(name,c_name,marker,printer,nuker,equal,hash,desc,structtype) \
DEFINE_BASIC_LRECORD_IMPLEMENTATION_WITH_PROPS(name,c_name,marker,printer,nuker,equal,hash,desc,0,0,0,0,structtype)

#define DEFINE_BASIC_LRECORD_IMPLEMENTATION_WITH_PROPS(name,c_name,marker,printer,nuker,equal,hash,desc,getprop,putprop,remprop,plist,structtype) \
MAKE_LRECORD_IMPLEMENTATION(name,c_name,marker,printer,nuker,equal,hash,desc,getprop,putprop,remprop,plist,sizeof(structtype),0,1,structtype)

#define DEFINE_LRECORD_IMPLEMENTATION(name,c_name,marker,printer,nuker,equal,hash,desc,structtype) \
DEFINE_LRECORD_IMPLEMENTATION_WITH_PROPS(name,c_name,marker,printer,nuker,equal,hash,desc,0,0,0,0,structtype)

#define DEFINE_LRECORD_IMPLEMENTATION_WITH_PROPS(name,c_name,marker,printer,nuker,equal,hash,desc,getprop,putprop,remprop,plist,structtype) \
MAKE_LRECORD_IMPLEMENTATION(name,c_name,marker,printer,nuker,equal,hash,desc,getprop,putprop,remprop,plist,sizeof (structtype),0,0,structtype)

#define DEFINE_LRECORD_SEQUENCE_IMPLEMENTATION(name,c_name,marker,printer,nuker,equal,hash,desc,sizer,structtype) \
DEFINE_LRECORD_SEQUENCE_IMPLEMENTATION_WITH_PROPS(name,c_name,marker,printer,nuker,equal,hash,desc,0,0,0,0,sizer,structtype)

#define DEFINE_BASIC_LRECORD_SEQUENCE_IMPLEMENTATION(name,c_name,marker,printer,nuker,equal,hash,desc,sizer,structtype) \
MAKE_LRECORD_IMPLEMENTATION(name,c_name,marker,printer,nuker,equal,hash,desc,0,0,0,0,0,sizer,1,structtype)

#define DEFINE_LRECORD_SEQUENCE_IMPLEMENTATION_WITH_PROPS(name,c_name,marker,printer,nuker,equal,hash,desc,getprop,putprop,remprop,plist,sizer,structtype) \
MAKE_LRECORD_IMPLEMENTATION(name,c_name,marker,printer,nuker,equal,hash,desc,getprop,putprop,remprop,plist,0,sizer,0,structtype)

#define MAKE_LRECORD_IMPLEMENTATION(name,c_name,marker,printer,nuker,equal,hash,desc,getprop,putprop,remprop,plist,size,sizer,basic_p,structtype) \
DECLARE_ERROR_CHECK_TYPECHECK(c_name, structtype)			\
const struct lrecord_implementation lrecord_##c_name =			\
  { name, marker, printer, nuker, equal, hash, desc,			\
    getprop, putprop, remprop, plist, size, sizer,			\
    lrecord_type_##c_name, basic_p }

#define DEFINE_EXTERNAL_LRECORD_IMPLEMENTATION(name,c_name,marker,printer,nuker,equal,hash,desc,structtype) \
DEFINE_EXTERNAL_LRECORD_IMPLEMENTATION_WITH_PROPS(name,c_name,marker,printer,nuker,equal,hash,desc,0,0,0,0,structtype)

#define DEFINE_EXTERNAL_LRECORD_IMPLEMENTATION_WITH_PROPS(name,c_name,marker,printer,nuker,equal,hash,desc,getprop,putprop,remprop,plist,structtype) \
MAKE_EXTERNAL_LRECORD_IMPLEMENTATION(name,c_name,marker,printer,nuker,equal,hash,desc,getprop,putprop,remprop,plist,sizeof (structtype),0,0,structtype)

#define DEFINE_EXTERNAL_LRECORD_SEQUENCE_IMPLEMENTATION(name,c_name,marker,printer,nuker,equal,hash,desc,sizer,structtype) \
DEFINE_EXTERNAL_LRECORD_SEQUENCE_IMPLEMENTATION_WITH_PROPS(name,c_name,marker,printer,nuker,equal,hash,desc,0,0,0,0,sizer,structtype)

#define DEFINE_EXTERNAL_LRECORD_SEQUENCE_IMPLEMENTATION_WITH_PROPS(name,c_name,marker,printer,nuker,equal,hash,desc,getprop,putprop,remprop,plist,sizer,structtype) \
MAKE_EXTERNAL_LRECORD_IMPLEMENTATION(name,c_name,marker,printer,nuker,equal,hash,desc,getprop,putprop,remprop,plist,0,sizer,0,structtype)

#define MAKE_EXTERNAL_LRECORD_IMPLEMENTATION(name,c_name,marker,printer,nuker,equal,hash,desc,getprop,putprop,remprop,plist,size,sizer,basic_p,structtype) \
DECLARE_ERROR_CHECK_TYPECHECK(c_name, structtype)			\
int lrecord_type_##c_name;						\
struct lrecord_implementation lrecord_##c_name =			\
  { name, marker, printer, nuker, equal, hash, desc,			\
    getprop, putprop, remprop, plist, size, sizer,			\
    lrecord_type_last_built_in_type, basic_p }


extern Lisp_Object (*lrecord_markers[]) (Lisp_Object);

#define INIT_LRECORD_IMPLEMENTATION(type) do {				\
  lrecord_implementations_table[lrecord_type_##type] = &lrecord_##type;	\
  lrecord_markers[lrecord_type_##type] =				\
    lrecord_implementations_table[lrecord_type_##type]->marker;		\
} while (0)

#define INIT_EXTERNAL_LRECORD_IMPLEMENTATION(type) do {			\
  lrecord_type_##type = lrecord_type_count++;				\
  lrecord_##type.lrecord_type_index = lrecord_type_##type;		\
  INIT_LRECORD_IMPLEMENTATION(type);					\
} while (0)

#define LRECORDP(a) (XTYPE (a) == Lisp_Type_Record)
#define XRECORD_LHEADER(a) ((struct lrecord_header *) XPNTR (a))

#define RECORD_TYPEP(x, ty) \
  (LRECORDP (x) && (XRECORD_LHEADER (x)->type == (unsigned int) (ty)))

/* Steps to create a new object:

   1. Declare the struct for your object in a header file somewhere.
   Remember that it must begin with

   struct lcrecord_header header;

   2. Put the "standard junk" (DECLARE_RECORD()/XFOO/XSETFOO/etc.) below the
      struct definition -- see below.

   3. Add this header file to inline.c.

   4. Create the methods for your object.  Note that technically you don't
   need any, but you will almost always want at least a mark method.

   5. Define your object with DEFINE_LRECORD_IMPLEMENTATION() or some
   variant.

   6. Include the header file in the .c file where you defined the object.

   7. Put a call to INIT_LRECORD_IMPLEMENTATION() for the object in the
   .c file's syms_of_foo() function.

   8. Add a type enum for the object to enum lrecord_type, earlier in this
   file.

An example:

------------------------------ in toolbar.h -----------------------------

struct toolbar_button
{
  struct lcrecord_header header;

  Lisp_Object next;
  Lisp_Object frame;

  Lisp_Object up_glyph;
  Lisp_Object down_glyph;
  Lisp_Object disabled_glyph;

  Lisp_Object cap_up_glyph;
  Lisp_Object cap_down_glyph;
  Lisp_Object cap_disabled_glyph;

  Lisp_Object callback;
  Lisp_Object enabled_p;
  Lisp_Object help_string;

  char enabled;
  char down;
  char pushright;
  char blank;

  int x, y;
  int width, height;
  int dirty;
  int vertical;
  int border_width;
};

[[ the standard junk: ]]

DECLARE_LRECORD (toolbar_button, struct toolbar_button);
#define XTOOLBAR_BUTTON(x) XRECORD (x, toolbar_button, struct toolbar_button)
#define XSETTOOLBAR_BUTTON(x, p) XSETRECORD (x, p, toolbar_button)
#define wrap_toolbar_button(p) wrap_record (p, toolbar_button)
#define TOOLBAR_BUTTONP(x) RECORDP (x, toolbar_button)
#define CHECK_TOOLBAR_BUTTON(x) CHECK_RECORD (x, toolbar_button)
#define CONCHECK_TOOLBAR_BUTTON(x) CONCHECK_RECORD (x, toolbar_button)

------------------------------ in toolbar.c -----------------------------

#include "toolbar.h"

...

static Lisp_Object
mark_toolbar_button (Lisp_Object obj)
{
  struct toolbar_button *data = XTOOLBAR_BUTTON (obj);
  mark_object (data->next);
  mark_object (data->frame);
  mark_object (data->up_glyph);
  mark_object (data->down_glyph);
  mark_object (data->disabled_glyph);
  mark_object (data->cap_up_glyph);
  mark_object (data->cap_down_glyph);
  mark_object (data->cap_disabled_glyph);
  mark_object (data->callback);
  mark_object (data->enabled_p);
  return data->help_string;
}

[[ If your object should never escape to Lisp, declare its print method
   as internal_object_printer instead of 0. ]]

DEFINE_LRECORD_IMPLEMENTATION ("toolbar-button", toolbar_button,
			       mark_toolbar_button, 0,
			       0, 0, 0, 0, struct toolbar_button);

...

void
syms_of_toolbar (void)
{
  INIT_LRECORD_IMPLEMENTATION (toolbar_button);

  ...;
}

------------------------------ in inline.c -----------------------------

#ifdef HAVE_TOOLBARS
#include "toolbar.h"
#endif

------------------------------ in lrecord.h -----------------------------

enum lrecord_type
{
  ...
  lrecord_type_toolbar_button,
  ...
};

*/

/*

Note: Object types defined in external dynamically-loaded modules (not
part of the XEmacs main source code) should use DECLARE_EXTERNAL_LRECORD
and DEFINE_EXTERNAL_LRECORD_IMPLEMENTATION rather than DECLARE_LRECORD
and DEFINE_LRECORD_IMPLEMENTATION.

*/


#ifdef ERROR_CHECK_TYPECHECK

# define DECLARE_LRECORD(c_name, structtype)				  \
extern const struct lrecord_implementation lrecord_##c_name;		  \
INLINE_HEADER structtype *						  \
error_check_##c_name (Lisp_Object obj, const char *file, int line);	  \
INLINE_HEADER structtype *						  \
error_check_##c_name (Lisp_Object obj, const char *file, int line)	  \
{									  \
  assert_at_line (RECORD_TYPEP (obj, lrecord_type_##c_name), file, line); \
  return (structtype *) XPNTR (obj);					  \
}									  \
extern Lisp_Object Q##c_name##p

# define DECLARE_EXTERNAL_LRECORD(c_name, structtype)			  \
extern int lrecord_type_##c_name;					  \
extern struct lrecord_implementation lrecord_##c_name;			  \
INLINE_HEADER structtype *						  \
error_check_##c_name (Lisp_Object obj, const char *file, int line);	  \
INLINE_HEADER structtype *						  \
error_check_##c_name (Lisp_Object obj, const char *file, int line)	  \
{									  \
  assert_at_line (RECORD_TYPEP (obj, lrecord_type_##c_name), file, line); \
  return (structtype *) XPNTR (obj);					  \
}									  \
extern Lisp_Object Q##c_name##p

# define DECLARE_NONRECORD(c_name, type_enum, structtype)		\
INLINE_HEADER structtype *						\
error_check_##c_name (Lisp_Object obj, const char *file, int line);	\
INLINE_HEADER structtype *						\
error_check_##c_name (Lisp_Object obj, const char *file, int line)	\
{									\
  assert_at_line (XTYPE (obj) == type_enum, file, line);		\
  return (structtype *) XPNTR (obj);					\
}									\
extern Lisp_Object Q##c_name##p

# define XRECORD(x, c_name, structtype) \
  error_check_##c_name (x, __FILE__, __LINE__)
# define XNONRECORD(x, c_name, type_enum, structtype) \
  error_check_##c_name (x, __FILE__, __LINE__)

# define XSETRECORD(var, p, c_name) do				\
{								\
  XSETOBJ (var, p);						\
  assert (RECORD_TYPEP (var, lrecord_type_##c_name));		\
} while (0)

INLINE_HEADER Lisp_Object wrap_record_1 (void *ptr, enum lrecord_type ty,
					 const char *file, int line);
INLINE_HEADER Lisp_Object
wrap_record_1 (void *ptr, enum lrecord_type ty, const char *file, int line)
{
  Lisp_Object obj;
  XSETOBJ (obj, ptr);
  assert_at_line (RECORD_TYPEP (obj, ty), file, line);
  return obj;
}

#define wrap_record(ptr, ty) \
  wrap_record_1 (ptr, lrecord_type_##ty, __FILE__, __LINE__)

#else /* not ERROR_CHECK_TYPECHECK */

# define DECLARE_LRECORD(c_name, structtype)			\
extern Lisp_Object Q##c_name##p;				\
extern const struct lrecord_implementation lrecord_##c_name
# define DECLARE_EXTERNAL_LRECORD(c_name, structtype)		\
extern Lisp_Object Q##c_name##p;				\
extern int lrecord_type_##c_name;				\
extern struct lrecord_implementation lrecord_##c_name
# define DECLARE_NONRECORD(c_name, type_enum, structtype)	\
extern Lisp_Object Q##c_name##p
# define XRECORD(x, c_name, structtype) ((structtype *) XPNTR (x))
# define XNONRECORD(x, c_name, type_enum, structtype)		\
  ((structtype *) XPNTR (x))
# define XSETRECORD(var, p, c_name) XSETOBJ (var, p)
/* wrap_pointer_1 is so named as a suggestion not to use it unless you
   know what you're doing. */
#define wrap_record(ptr, ty) wrap_pointer_1 (ptr)

#endif /* not ERROR_CHECK_TYPECHECK */

#define RECORDP(x, c_name) RECORD_TYPEP (x, lrecord_type_##c_name)

/* Note: we now have two different kinds of type-checking macros.
   The "old" kind has now been renamed CONCHECK_foo.  The reason for
   this is that the CONCHECK_foo macros signal a continuable error,
   allowing the user (through debug-on-error) to substitute a different
   value and return from the signal, which causes the lvalue argument
   to get changed.  Quite a lot of code would crash if that happened,
   because it did things like

   foo = XCAR (list);
   CHECK_STRING (foo);

   and later on did XSTRING (XCAR (list)), assuming that the type
   is correct (when it might be wrong, if the user substituted a
   correct value in the debugger).

   To get around this, I made all the CHECK_foo macros signal a
   non-continuable error.  Places where a continuable error is OK
   (generally only when called directly on the argument of a Lisp
   primitive) should be changed to use CONCHECK().

   FSF Emacs does not have this problem because RMS took the cheesy
   way out and disabled returning from a signal entirely. */

#define CONCHECK_RECORD(x, c_name) do {			\
 if (!RECORD_TYPEP (x, lrecord_type_##c_name))		\
   x = wrong_type_argument (Q##c_name##p, x);		\
}  while (0)
#define CONCHECK_NONRECORD(x, lisp_enum, predicate) do {\
 if (XTYPE (x) != lisp_enum)				\
   x = wrong_type_argument (predicate, x);		\
 } while (0)
#define CHECK_RECORD(x, c_name) do {			\
 if (!RECORD_TYPEP (x, lrecord_type_##c_name))		\
   dead_wrong_type_argument (Q##c_name##p, x);		\
 } while (0)
#define CHECK_NONRECORD(x, lisp_enum, predicate) do {	\
 if (XTYPE (x) != lisp_enum)				\
   dead_wrong_type_argument (predicate, x);		\
 } while (0)

void *alloc_lcrecord (Bytecount size,
		      const struct lrecord_implementation *);

void *alloc_automanaged_lcrecord (Bytecount size,
				  const struct lrecord_implementation *);

#define alloc_unmanaged_lcrecord_type(type, lrecord_implementation) \
  ((type *) alloc_lcrecord (sizeof (type), lrecord_implementation))

#define alloc_lcrecord_type(type, lrecord_implementation) \
  ((type *) alloc_automanaged_lcrecord (sizeof (type), lrecord_implementation))

void free_lcrecord (Lisp_Object rec);


/* Copy the data from one lcrecord structure into another, but don't
   overwrite the header information. */

#define copy_sized_lcrecord(dst, src, size)			\
  memcpy ((char *) (dst) + sizeof (struct lcrecord_header),	\
	  (char *) (src) + sizeof (struct lcrecord_header),	\
	  (size) - sizeof (struct lcrecord_header))

#define copy_lcrecord(dst, src) copy_sized_lcrecord (dst, src, sizeof (*(dst)))

#define zero_sized_lcrecord(lcr, size)				\
   memset ((char *) (lcr) + sizeof (struct lcrecord_header), 0,	\
	   (size) - sizeof (struct lcrecord_header))

#define zero_lcrecord(lcr) zero_sized_lcrecord(lcr, sizeof (*(lcr)))

#endif /* INCLUDED_lrecord_h_ */
