/* The "lrecord" structure (header of a compound lisp object).
   Copyright (C) 1993, 1994, 1995 Free Software Foundation, Inc.
   Copyright (C) 1996, 2001, 2002, 2004, 2005, 2010 Ben Wing.

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

/* This file has been Mule-ized, Ben Wing, 10-13-04. */

#ifndef INCLUDED_lrecord_h_
#define INCLUDED_lrecord_h_

/* The "lrecord" type of Lisp object is used for all object types other
   than a few simple ones (like char and int). This allows many types to be
   implemented but only a few bits required in a Lisp object for type
   information. (The tradeoff is that each object has its type marked in
   it, thereby increasing its size.) All lrecords begin with a `struct
   lrecord_header', which identifies the lisp object type, by providing an
   index into a table of `struct lrecord_implementation', which describes
   the behavior of the lisp object.  It also contains some other data bits.

#ifndef NEW_GC
   Lrecords are of two types: straight lrecords, and lcrecords.
   Straight lrecords are used for those types of objects that have
   their own allocation routines (typically allocated out of 2K chunks
   of memory called `frob blocks').  These objects have a `struct
   lrecord_header' at the top, containing only the bits needed to find
   the lrecord_implementation for the object.  There are special
   routines in alloc.c to create an object of each such type.

   Lcrecords are used for less common sorts of objects that don't do
   their own allocation.  Each such object is malloc()ed individually,
   and the objects are chained together through a `next' pointer.
   Lcrecords have a `struct old_lcrecord_header' at the top, which
   contains a `struct lrecord_header' and a `next' pointer, and are
   allocated using old_alloc_lcrecord_type() or its variants.
#endif

   Creating a new Lisp object type is fairly easy; just follow the
   lead of some existing type (e.g. hash tables).  Note that you
   do not need to supply all the methods (see below); reasonable
   defaults are provided for many of them.  Alternatively, if you're
   just looking for a way of encapsulating data (which possibly
   could contain Lisp_Objects in it), you may well be able to use
   the opaque type.
*/

#ifdef NEW_GC
/*
  There are some limitations under New-GC that lead to the creation of a
  large number of new internal object types.  I'm not completely sure what
  all of them are, but they are at least partially related to limitations
  on finalizers.  Something else must be going on as well, because
  non-dumpable, non-finalizable objects like devices and frames also have
  their window-system-specific substructures converted into Lisp objects.
  It must have something to do with the fact that these substructures
  contain pointers to Lisp objects, but it's not completely clear why --
  object descriptions exist to indicate the size of these structures and
  the Lisp object pointers within them.

 At least one definite issue is that under New-GC dumpable objects cannot
 contain any finalizers (see pdump_register_object()).  This means that any
 substructures in dumpable objects that are allocated separately and
 normally freed in a finalizer need instead to be made into actual Lisp
 objects.  If those structures are Dynarrs, they need to be made into
 Dynarr Lisp objects (e.g. face-cachel-dynarr or glyph-cachel-dynarr),
 which are created using Dynarr_lisp_new() or Dynarr_new_new2().
 Furthermore, the objects contained in the Dynarr also need to be Lisp
 objects (e.g. face-cachel or glyph-cachel).

 --ben
 */

#endif



#ifdef NEW_GC
#define ALLOC_LCRECORD_TYPE alloc_lrecord_type
#define COPY_SIZED_LCRECORD copy_sized_lrecord
#define COPY_LCRECORD copy_lrecord
#define LISPOBJ_STORAGE_SIZE(ptr, size, stats) \
  mc_alloced_storage_size (size, stats)
#define ZERO_LCRECORD zero_lrecord
#define LCRECORD_HEADER lrecord_header
#define BASIC_ALLOC_LCRECORD alloc_lrecord
#define FREE_LCRECORD free_lrecord
#else /* not NEW_GC */
#define ALLOC_LCRECORD_TYPE old_alloc_lcrecord_type
#define COPY_SIZED_LCRECORD old_copy_sized_lcrecord
#define COPY_LCRECORD old_copy_lcrecord
#define LISPOBJ_STORAGE_SIZE malloced_storage_size
#define ZERO_LCRECORD old_zero_lcrecord
#define LCRECORD_HEADER old_lcrecord_header
#define BASIC_ALLOC_LCRECORD old_basic_alloc_lcrecord
#define FREE_LCRECORD old_free_lcrecord
#endif /* not NEW_GC */

BEGIN_C_DECLS

struct lrecord_header
{
  /* Index into lrecord_implementations_table[].  Objects that have been
     explicitly freed using e.g. free_cons() have lrecord_type_free in this
     field. */
  unsigned int type :8;

#ifdef NEW_GC
  /* 1 if the object is readonly from lisp */
  unsigned int lisp_readonly :1;

  /* The `free' field is a flag that indicates whether this lrecord
     is currently free or not. This is used for error checking and
     debugging. */
  unsigned int free :1;

  /* The `uid' field is just for debugging/printing convenience.  Having
     this slot doesn't hurt us spacewise, since the bits are unused
     anyway. (The bits are used for strings, though.) */
  unsigned int uid :22;

#else /* not NEW_GC */
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

  /* The `uid' field is just for debugging/printing convenience.  Having
     this slot doesn't hurt us spacewise, since the bits are unused
     anyway. (The bits are used for strings, though.) */
  unsigned int uid :21;

#endif /* not NEW_GC */
};

struct lrecord_implementation;
int lrecord_type_index (const struct lrecord_implementation *implementation);
extern int lrecord_uid_counter;

#ifdef NEW_GC
#define set_lheader_implementation(header,imp) do {	\
  struct lrecord_header* SLI_header = (header);		\
  SLI_header->type = (imp)->lrecord_type_index;		\
  SLI_header->lisp_readonly = 0;			\
  SLI_header->free = 0;					\
  SLI_header->uid = lrecord_uid_counter++;		\
} while (0)
#else /* not NEW_GC */
#define set_lheader_implementation(header,imp) do {	\
  struct lrecord_header* SLI_header = (header);		\
  SLI_header->type = (imp)->lrecord_type_index;		\
  SLI_header->mark = 0;					\
  SLI_header->c_readonly = 0;				\
  SLI_header->lisp_readonly = 0;			\
  SLI_header->uid = lrecord_uid_counter++;		\
} while (0)
#endif /* not NEW_GC */

#ifndef NEW_GC
struct old_lcrecord_header
{
  struct lrecord_header lheader;

  /* The `next' field is normally used to chain all lcrecords together
     so that the GC can find (and free) all of them.
     `old_basic_alloc_lcrecord' threads lcrecords together.

     The `next' field may be used for other purposes as long as some
     other mechanism is provided for letting the GC do its work.

     For example, the event and marker object types allocate members
     out of memory chunks, and are able to find all unmarked members
     by sweeping through the elements of the list of chunks.  */
  struct old_lcrecord_header *next;

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
  struct old_lcrecord_header lcheader;
  Lisp_Object chain;
};
#endif /* not NEW_GC */

/* DON'T FORGET to update .gdbinit.in.in if you change this list. */
enum lrecord_type
{
  /* Symbol value magic types come first so that SYMBOL_VALUE_MAGIC_P
     can be written as a single '<= lrecord_type_max_symbol_value_magic'.
     In practice, if we moved them somewhere else but kept them together,
     the single extra comparison would hardly make a difference.
     #### This should be replaced by a symbol_value_magic_p flag
     in the Lisp_Symbol lrecord_header. */
  lrecord_type_symbol_value_forward = 0, /* struct symbol_value_forward */
  lrecord_type_symbol_value_varalias,    /* struct symbol_value_varalias */
  lrecord_type_symbol_value_lisp_magic,  /* struct symbol_value_lisp_magic */
  lrecord_type_symbol_value_buffer_local,/* struct symbol_value_buffer_local */
  lrecord_type_max_symbol_value_magic = lrecord_type_symbol_value_buffer_local,

  /* Keep the rest of these (up to #ifdef EVENT_DATA_AS_OBJECTS) sorted,
     to facilitate keeping .gdbinit.in.in in sync.  Also sort within
     the #ifdef EVENT_DATA_AS_OBJECTS and within the commented-out
     #ifdef NEW_GC.  Leave the symbol-value stuff at the beginning and the
     free/undefined/last at the end as-is. */
  lrecord_type_bigfloat,		/* Lisp_Bigfloat */
  lrecord_type_bignum,			/* Lisp_Bignum */
  lrecord_type_bit_vector,		/* Lisp_Bit_Vector */
  lrecord_type_buffer,			/* struct buffer */
  lrecord_type_case_table,		/* Lisp_Case_Table */
  lrecord_type_char_subtable,		/* Lisp_Char_Subtable */
  lrecord_type_char_table,		/* Lisp_Char_Table */
  lrecord_type_charset,			/* Lisp_Charset */
  lrecord_type_coding_system,		/* Lisp_Coding_System */
  lrecord_type_color_instance,		/* Lisp_Color_Instance */
  lrecord_type_command_builder,		/* struct command_builder */
  lrecord_type_compiled_function,	/* Lisp_Compiled_Function */
  lrecord_type_cons,			/* Lisp_Cons */
  lrecord_type_console,			/* struct console */
  lrecord_type_database,		/* Lisp_Database */
  lrecord_type_detection_state,		/* struct detection_state */
  lrecord_type_device,			/* struct device */
  lrecord_type_devmode,			/* Lisp_Devmode */
  lrecord_type_emacs_ffi,		/* emacs_ffi_data */
  lrecord_type_emacs_gtk_boxed,		/* emacs_gtk_boxed_data */
  lrecord_type_emacs_gtk_object,	/* emacs_gtk_object_data */
  lrecord_type_ephemeron,		/* struct ephemeron */
  lrecord_type_event,			/* Lisp_Event */
  lrecord_type_extent,			/* struct extent */
  lrecord_type_extent_auxiliary,	/* struct extent_auxiliary */
  lrecord_type_extent_info,		/* struct extent_info */
  lrecord_type_face,			/* Lisp_Face */
  lrecord_type_fc_config,		/* struct fc_config */
  lrecord_type_fc_pattern,		/* struct fc_pattern */
  lrecord_type_float,			/* Lisp_Float */
  lrecord_type_font_instance,		/* Lisp_Font_Instance */
  lrecord_type_frame,			/* struct frame */
  lrecord_type_glyph,			/* Lisp_Glyph */
  lrecord_type_gui_item,		/* Lisp_Gui_Item */
  lrecord_type_hash_table,		/* Lisp_Hash_Table */
  lrecord_type_image_instance,		/* Lisp_Image_Instance */
  lrecord_type_keymap,			/* Lisp_Keymap */
  /* #ifndef NEW_GC */
  lrecord_type_lcrecord_list,		/* struct lcrecord_list */
  /* #endif not NEW_GC */
  /* #### Do we need the following module entry? */
  lrecord_type_ldap,			/* Lisp_LDAP */
  lrecord_type_lstream,			/* struct lstream */
  lrecord_type_marker,			/* Lisp_Marker */
  lrecord_type_mswindows_dialog_id,	/* struct mswindows_dialog_id */
  lrecord_type_multiple_value,		/* multiple_value */
  lrecord_type_opaque,			/* Lisp_Opaque */
  lrecord_type_opaque_ptr,		/* Lisp_Opaque_Ptr */
  /* #### Do we need the following two module entries? */
  lrecord_type_pgconn,			/* Lisp_PGconn */
  lrecord_type_pgresult,		/* Lisp_PGresult */
  lrecord_type_precedence_array,	/* struct precedence_array */
  lrecord_type_process,			/* Lisp_Process */
  lrecord_type_range_table,		/* Lisp_Range_Table */
  lrecord_type_ratio,			/* Lisp_Ratio */
  lrecord_type_scrollbar_instance,	/* struct scrollbar_instance */
  lrecord_type_specifier,		/* Lisp_Specifier */
  lrecord_type_string,			/* Lisp_String */
  lrecord_type_subr,			/* Lisp_Subr */
  lrecord_type_symbol,			/* Lisp_Symbol */
  lrecord_type_timeout,			/* Lisp_Timeout */
  lrecord_type_toolbar_button,		/* struct toolbar_button */
  lrecord_type_tooltalk_message,	/* Lisp_Tooltalk_Message */
  lrecord_type_tooltalk_pattern,	/* Lisp_Tooltalk_Pattern */
  lrecord_type_vector,			/* Lisp_Vector */
  lrecord_type_weak_box,		/* struct weak_box */
  lrecord_type_weak_list,		/* struct weak_list */
  lrecord_type_window,			/* struct window */
  lrecord_type_window_configuration,	/* struct window_config */
  lrecord_type_window_mirror,		/* struct window_mirror */

#ifdef EVENT_DATA_AS_OBJECTS /* not defined */
  lrecord_type_button_data,		/* Lisp_Button_Data */
  lrecord_type_eval_data,		/* Lisp_Eval_Data */
  lrecord_type_key_data,		/* Lisp_Key_Data */
  lrecord_type_magic_data,		/* Lisp_Magic_Data */
  lrecord_type_magic_eval_data,		/* Lisp_Magic_Eval_Data */
  lrecord_type_misc_user_data,		/* Lisp_Misc_User_Data */
  lrecord_type_motion_data,		/* Lisp_Motion_Data */
  lrecord_type_process_data,		/* Lisp_Process_Data */
  lrecord_type_timeout_data,		/* Lisp_Timeout_Data */
#endif /* EVENT_DATA_AS_OBJECTS */

  /* #ifdef NEW_GC */
  /* See comment up top explaining why these extra object types must exist. */
  lrecord_type_buffer_text,		/* Lisp_Buffer_Text */
  lrecord_type_compiled_function_args,	/* Lisp_Compiled_Function_Args */
  lrecord_type_dynarr,			/* Dynarr */
  lrecord_type_expose_ignore,		/* struct expose_ignore */
  lrecord_type_extent_list,		/* struct extent_list */
  lrecord_type_extent_list_marker,	/* struct extent_list_marker */
  lrecord_type_face_cachel,		/* Lisp_Face_Cachel */
  lrecord_type_face_cachel_dynarr,	/* face_cachel_dynarr */
  lrecord_type_gap_array,		/* struct gap_array */
  lrecord_type_gap_array_marker,	/* struct gap_array_marker */
  lrecord_type_glyph_cachel,		/* Lisp_Glyph_Cachel */
  lrecord_type_glyph_cachel_dynarr,	/* glyph_cachel_dynarr */
  lrecord_type_gtk_device,		/* Lisp_Gtk_Device */
  lrecord_type_gtk_frame,		/* Lisp_Gtk_Frame */
  lrecord_type_hash_table_entry,	/* Lisp_Hash_Table_Entry */
  lrecord_type_msprinter_device,	/* Lisp_Msprinter_Device */
  lrecord_type_mswindows_device,	/* Lisp_Mswindows_Device */
  lrecord_type_mswindows_frame,		/* Lisp_Mswindows_Frame */
  lrecord_type_specifier_caching,	/* struct specifier_caching */
  lrecord_type_stack_of_extents,	/* struct stack_of_extents */
  lrecord_type_stream_console,		/* Lisp_Stream_Console */
  lrecord_type_string_direct_data,	/* Lisp_String_Direct_Data */
  lrecord_type_string_indirect_data,	/* Lisp_String_Indirect_Data */
  lrecord_type_syntax_cache,		/* Lisp_Syntax_Cache */
  lrecord_type_tty_color_instance_data,	/* struct tty_color_instance_data */
  lrecord_type_tty_console,		/* Lisp_Tty_Console */
  lrecord_type_tty_device,		/* Lisp_Tty_Device */
  lrecord_type_tty_font_instance_data,	/* struct tty_font_instance_data */
  lrecord_type_x_device,		/* Lisp_X_Device */
  lrecord_type_x_frame,			/* Lisp_X_Frame */
  /* #endif NEW_GC */

  /* next two not used under NEW_GC */
  lrecord_type_free,			/* only used for "free" lrecords */
  lrecord_type_undefined,		/* only used for debugging */
  lrecord_type_last_built_in_type,	/* must be last */
};

extern MODULE_API int lrecord_type_count;

struct lrecord_implementation
{
  const Ascbyte *name;

  /* information for the dumper: is the object dumpable and should it 
     be dumped. */
  unsigned int dumpable :1;

  /* `marker' is called at GC time, to make sure that all Lisp_Objects
     pointed to by this object get properly marked.  It should call
     the mark_object function on all Lisp_Objects in the object.  If
     the return value is non-nil, it should be a Lisp_Object to be
     marked (don't call the mark_object function explicitly on it,
     because the GC routines will do this).  Doing it this way reduces
     recursion, so the object returned should preferably be the one
     with the deepest level of Lisp_Object pointers.  This function
     can be NULL, meaning no GC marking is necessary.

     NOTE NOTE NOTE: This is not used by KKCC (which uses the data
     description below instead), unless the data description is missing.
     Yes, this currently means there is logic duplication.  Eventually the
     mark methods will be removed. */
  Lisp_Object (*marker) (Lisp_Object);

  /* `printer' converts the object to a printed representation.
     This can be NULL; in this case default_object_printer() will be
     used instead. */
  void (*printer) (Lisp_Object, Lisp_Object printcharfun, int escapeflag);

  /* `finalizer' is called at GC time when the object is about to be freed,
     and at dump time (FOR_DISKSAVE will be non-zero in this case).  It
     should perform any necessary cleanup (e.g. freeing malloc()ed memory
     or releasing objects created in external libraries, such as
     window-system windows or file handles).  This can be NULL, meaning no
     special finalization is necessary.

     WARNING: remember that `finalizer' is called at dump time even though
     the object is not being freed -- check the FOR_DISKSAVE argument.   */
  void (*finalizer) (void *header, int for_disksave);

  /* This can be NULL, meaning compare objects with EQ(). */
  int (*equal) (Lisp_Object obj1, Lisp_Object obj2, int depth,
		int foldcase);

  /* `hash' generates hash values for use with hash tables that have
     `equal' as their test function.  This can be NULL, meaning use
     the Lisp_Object itself as the hash.  But, you must still satisfy
     the constraint that if two objects are `equal', then they *must*
     hash to the same value in order for hash tables to work properly.
     This means that `hash' can be NULL only if the `equal' method is
     also NULL. */
  Hashcode (*hash) (Lisp_Object, int);

  /* Data layout description for your object.  See long comment below. */
  const struct memory_description *description;

  /* These functions allow any object type to have builtin property
     lists that can be manipulated from the lisp level with
     `get', `put', `remprop', and `object-plist'. */
  Lisp_Object (*getprop) (Lisp_Object obj, Lisp_Object prop);
  int (*putprop) (Lisp_Object obj, Lisp_Object prop, Lisp_Object val);
  int (*remprop) (Lisp_Object obj, Lisp_Object prop);
  Lisp_Object (*plist) (Lisp_Object obj);

#ifdef NEW_GC
  /* Only one of `static_size' and `size_in_bytes_method' is non-0. */
#else /* not NEW_GC */
  /* Only one of `static_size' and `size_in_bytes_method' is non-0.
     If both are 0, this type is not instantiable by
     old_basic_alloc_lcrecord(). */
#endif /* not NEW_GC */
  Bytecount static_size;
  Bytecount (*size_in_bytes_method) (const void *header);

  /* The (constant) index into lrecord_implementations_table */
  enum lrecord_type lrecord_type_index;

#ifndef NEW_GC
  /* A "basic" lrecord is any lrecord that's not an lcrecord, i.e.
     one that does not have an old_lcrecord_header at the front and which
     is (usually) allocated in frob blocks. */
  unsigned int basic_p :1;
#endif /* not NEW_GC */
};

/* All the built-in lisp object types are enumerated in `enum lrecord_type'.
   Additional ones may be defined by a module (none yet).  We leave some
   room in `lrecord_implementations_table' for such new lisp object types. */
#define MODULE_DEFINABLE_TYPE_COUNT 32

extern MODULE_API const struct lrecord_implementation *
lrecord_implementations_table[lrecord_type_last_built_in_type + MODULE_DEFINABLE_TYPE_COUNT];

#define XRECORD_LHEADER_IMPLEMENTATION(obj) \
   LHEADER_IMPLEMENTATION (XRECORD_LHEADER (obj))
#define LHEADER_IMPLEMENTATION(lh) lrecord_implementations_table[(lh)->type]

#include "gc.h"

#ifdef NEW_GC
#include "vdb.h"
#endif /* NEW_GC */

extern int gc_in_progress;

#ifdef NEW_GC
#include "mc-alloc.h"

#ifdef ALLOC_TYPE_STATS
void init_lrecord_stats (void);
void inc_lrecord_stats (Bytecount size, const struct lrecord_header *h);
void dec_lrecord_stats (Bytecount size_including_overhead, 
			const struct lrecord_header *h);
int lrecord_stats_heap_size (void);
#endif /* ALLOC_TYPE_STATS */

/* Tell mc-alloc how to call a finalizer. */
#define MC_ALLOC_CALL_FINALIZER(ptr)					\
{									\
  Lisp_Object MCACF_obj = wrap_pointer_1 (ptr);				\
  struct lrecord_header *MCACF_lheader = XRECORD_LHEADER (MCACF_obj);   \
  if (XRECORD_LHEADER (MCACF_obj) && LRECORDP (MCACF_obj)		\
      && !LRECORD_FREE_P (MCACF_lheader)  )				\
    {									\
      const struct lrecord_implementation *MCACF_implementation		\
	= LHEADER_IMPLEMENTATION (MCACF_lheader);			\
      if (MCACF_implementation && MCACF_implementation->finalizer)	\
        {								\
	  GC_STAT_FINALIZED;						\
          MCACF_implementation->finalizer (ptr, 0);			\
        }								\
    }									\
} while (0)

/* Tell mc-alloc how to call a finalizer for disksave. */
#define MC_ALLOC_CALL_FINALIZER_FOR_DISKSAVE(ptr)			\
{									\
  Lisp_Object MCACF_obj = wrap_pointer_1 (ptr);				\
  struct lrecord_header *MCACF_lheader = XRECORD_LHEADER (MCACF_obj);   \
  if (XRECORD_LHEADER (MCACF_obj) && LRECORDP (MCACF_obj)		\
      && !LRECORD_FREE_P (MCACF_lheader)  )				\
    {									\
      const struct lrecord_implementation *MCACF_implementation		\
	= LHEADER_IMPLEMENTATION (MCACF_lheader);			\
      if (MCACF_implementation && MCACF_implementation->finalizer)	\
	MCACF_implementation->finalizer (ptr, 1);			\
    }									\
} while (0)

#define LRECORD_FREE_P(ptr)			\
(((struct lrecord_header *) ptr)->free)

#define MARK_LRECORD_AS_FREE(ptr)			\
((void) (((struct lrecord_header *) ptr)->free = 1))

#define MARK_LRECORD_AS_NOT_FREE(ptr)			\
((void) (((struct lrecord_header *) ptr)->free = 0))

#define MARKED_RECORD_P(obj) MARKED_P (obj)
#define MARKED_RECORD_HEADER_P(lheader) MARKED_P (lheader)
#define MARK_RECORD_HEADER(lheader)   MARK (lheader)
#define UNMARK_RECORD_HEADER(lheader) UNMARK (lheader)

#define LISP_READONLY_RECORD_HEADER_P(lheader)  ((lheader)->lisp_readonly)
#define SET_LISP_READONLY_RECORD_HEADER(lheader) \
  ((void) ((lheader)->lisp_readonly = 1))
#define MARK_LRECORD_AS_LISP_READONLY(ptr)			\
((void) (((struct lrecord_header *) ptr)->lisp_readonly = 1))

#else /* not NEW_GC */

enum lrecord_alloc_status
{
  ALLOC_IN_USE,
  ALLOC_FREE,
  ALLOC_ON_FREE_LIST
};

void tick_lrecord_stats (const struct lrecord_header *h,
			 enum lrecord_alloc_status status);

#define LRECORD_FREE_P(ptr)					\
(((struct lrecord_header *) ptr)->type == lrecord_type_free)

#define MARK_LRECORD_AS_FREE(ptr)					\
((void) (((struct lrecord_header *) ptr)->type = lrecord_type_free))

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
#endif /* not NEW_GC */

#ifdef USE_KKCC
#define RECORD_DESCRIPTION(lheader) lrecord_memory_descriptions[(lheader)->type]
#else /* not USE_KKCC */
#define RECORD_MARKER(lheader) lrecord_markers[(lheader)->type]
#endif /* not USE_KKCC */

#define RECORD_DUMPABLE(lheader) (lrecord_implementations_table[(lheader)->type])->dumpable

/* Data description stuff

   Data layout descriptions describe blocks of memory (in particular, Lisp
   objects and other objects on the heap, and global objects with pointers
   to such heap objects), including their size and a list of the elements
   that need relocating, marking or other special handling.  They are
   currently used in two places: by pdump [the new, portable dumper] and
   KKCC [the new garbage collector].  The two subsystems use the
   descriptions in different ways, and as a result some of the descriptions
   are appropriate only for one or the other, when it is known that only
   that subsystem will use the description. (This is particularly the case
   with objects that can't be dumped, because pdump needs more info than
   KKCC.) However, properly written descriptions are appropriate for both,
   and you should strive to write your descriptions that way, since the
   dumpable status of an object may change and new uses for the
   descriptions may be created. (An example that comes to mind is a
   facility for determining the memory usage of XEmacs data structures --
   like `buffer-memory-usage', `window-memory-usage', etc. but more
   general.)

   More specifically:

   Pdump (the portable dumper) needs to write out all objects in heap
   space, and later on (in another invocation of XEmacs) load them back
   into the heap, relocating all pointers to the heap objects in the global
   data space. ("Heap" means anything malloc()ed, including all Lisp
   objects, and "global data" means anything declared globally or
   `static'.) Pdump, then, needs to be told about the location of all
   global pointers to heap objects, all the description of all such
   objects, including their size and any pointers to other heap (aka
   "relocatable") objects. (Pdump assumes that the heap may occur in
   different places in different invocations -- therefore, it is not enough
   simply to write out the entire heap and later reload it at the same
   location -- but that global data is always in the same place, and hence
   pointers to it do not need to be relocated.  This assumption holds true
   in general for modern operating systems, but would be broken, for
   example, in a system without virtual memory, or when dealing with shared
   libraries.  Also, unlike unexec, pdump does not usually write out or
   restore objects in the global data space, and thus they need to be
   initialized every time XEmacs is loaded.  This is the purpose of the
   reinit_*() functions throughout XEmacs. [It's possible, however, to make
   pdump restore global data.  This must be done, of course, for heap
   pointers, but is also done for other values that are not easy to
   recompute -- in particular, values established by the Lisp code loaded
   at dump time.]) Note that the data type `Lisp_Object' is basically just
   a relocatable pointer disguised as a long, and in general pdump treats
   the Lisp_Object values and pointers to Lisp objects (e.g. Lisp_Object
   vs. `struct frame *') identically. (NOTE: This equivalence depends
   crucially on the current "minimal tagbits" implementation of Lisp_Object
   pointers.)

   Descriptions are used by pdump in three places: (a) descriptions of Lisp
   objects, referenced in the DEFINE_*LRECORD_*IMPLEMENTATION*() call; (b)
   descriptions of global objects to be dumped, registered by
   dump_add_root_block(); (c) descriptions of global pointers to
   non-Lisp_Object heap objects, registered by dump_add_root_block_ptr().
   The descriptions need to tell pdump which elements of your structure are
   Lisp_Objects or structure pointers, plus the descriptions in turn of the
   non-Lisp_Object structures pointed to.  If these structures are you own
   private ones, you will have to write these recursive descriptions
   yourself; otherwise, you are reusing a structure already in existence
   elsewhere and there is probably already a description for it.

   Pdump does not care about Lisp objects that cannot be dumped (the
   dumpable flag to DEFINE_*LRECORD_*IMPLEMENTATION*() is 0).

   KKCC also uses data layout descriptions, but differently.  It cares
   about all objects, dumpable or not, but specifically only wants to know
   about Lisp_Objects in your object and in structures pointed to.  Thus,
   it doesn't care about things like pointers to structures ot other blocks
   of memory with no Lisp Objects in them, which pdump would care a lot
   about.

   Technically, then, you could write your description differently
   depending on whether your object is dumpable -- the full pdump
   description if so, the abbreviated KKCC description if not.  In fact,
   some descriptions are written this way.  This is dangerous, though,
   because another use might come along for the data descriptions, that
   doesn't care about the dumper flag and makes use of some of the stuff
   normally omitted from the "abbreviated" description -- see above.

   A memory_description is an array of values. (This is actually
   misnamed, in that it does not just describe lrecords, but any
   blocks of memory.) The first value of each line is a type, the
   second the offset in the lrecord structure.  The third and
   following elements are parameters; their presence, type and number
   is type-dependent.

   The description ends with an "XD_END" record.

   The top-level description of an lrecord or lcrecord does not need
   to describe every element, just the ones that need to be relocated,
   since the size of the lrecord is known. (The same goes for nested
   structures, whenever the structure size is given, rather than being
   defaulted by specifying 0 for the size.)

   A sized_memory_description is a memory_description plus the size of the
   block of memory.  The size field in a sized_memory_description can be
   given as zero, i.e. unspecified, meaning that the last element in the
   structure is described in the description and the size of the block can
   therefore be computed from it. (This is useful for stretchy arrays.)

   memory_descriptions are used to describe lrecords (the size of the
   lrecord is elsewhere in its description, attached to its methods, so it
   does not need to be given here) and global objects, where the size is an
   argument to the call to dump_add_root_block().
   sized_memory_descriptions are used for pointers and arrays in
   memory_descriptions and for calls to dump_add_root_block_ptr(). (####
   It is not obvious why this is so in the latter case.  Probably, calls to
   dump_add_root_block_ptr() should use plain memory_descriptions and have
   the size be an argument to the call.)

   NOTE: Anywhere that a sized_memory_description occurs inside of a plain
   memory_description, a "description map" can be substituted.  Rather than
   being an actual description, this describes how to find the description
   by looking inside of the object being described.  This is a convenient
   way to describe Lisp objects with subtypes and corresponding
   type-specific data.

   Some example descriptions :

   struct Lisp_String
   {
     struct lrecord_header lheader;
     Bytecount size;
     Ibyte *data;
     Lisp_Object plist;
   };

   static const struct memory_description cons_description[] = {
     { XD_LISP_OBJECT, offsetof (Lisp_Cons, car) },
     { XD_LISP_OBJECT, offsetof (Lisp_Cons, cdr) },
     { XD_END }
   };

   Which means "two lisp objects starting at the 'car' and 'cdr' elements"

   static const struct memory_description string_description[] = {
     { XD_BYTECOUNT,       offsetof (Lisp_String, size) },
     { XD_OPAQUE_DATA_PTR, offsetof (Lisp_String, data), XD_INDIRECT (0, 1) },
     { XD_LISP_OBJECT,     offsetof (Lisp_String, plist) },
     { XD_END }
   };

   "A pointer to string data at 'data', the size of the pointed array being
   the value of the size variable plus 1, and one lisp object at 'plist'"

   If your object has a pointer to an array of Lisp_Objects in it, something
   like this:

   struct Lisp_Foo
   {
     ...;
     int count;
     Lisp_Object *objects;
     ...;
   }

   You'd use XD_BLOCK_PTR, something like:

   static const struct memory_description foo_description[] = {
     ...
     { XD_INT,		offsetof (Lisp_Foo, count) },
     { XD_BLOCK_PTR,	offsetof (Lisp_Foo, objects),
       XD_INDIRECT (0, 0), { &lisp_object_description } },
     ...
   };

   lisp_object_description is declared in gc.c, like this:

   static const struct memory_description lisp_object_description_1[] = {
     { XD_LISP_OBJECT, 0 },
     { XD_END }
   };

   const struct sized_memory_description lisp_object_description = {
     sizeof (Lisp_Object),
     lisp_object_description_1
   };

   Another example of XD_BLOCK_PTR:

   typedef struct htentry
   {
     Lisp_Object key;
     Lisp_Object value;
   } htentry;
   
   struct Lisp_Hash_Table
   {
     struct LCRECORD_HEADER header;
     Elemcount size;
     Elemcount count;
     Elemcount rehash_count;
     double rehash_size;
     double rehash_threshold;
     Elemcount golden_ratio;
     hash_table_hash_function_t hash_function;
     hash_table_test_function_t test_function;
     htentry *hentries;
     enum hash_table_weakness weakness;
     Lisp_Object next_weak;     // Used to chain together all of the weak
   			        // hash tables.  Don't mark through this.
   };

   static const struct memory_description htentry_description_1[] = {
     { XD_LISP_OBJECT, offsetof (htentry, key) },
     { XD_LISP_OBJECT, offsetof (htentry, value) },
     { XD_END }
   };
   
   static const struct sized_memory_description htentry_description = {
     sizeof (htentry),
     htentry_description_1
   };
   
   const struct memory_description hash_table_description[] = {
     { XD_ELEMCOUNT,     offsetof (Lisp_Hash_Table, size) },
     { XD_BLOCK_PTR, offsetof (Lisp_Hash_Table, hentries), XD_INDIRECT (0, 1),
	 { &htentry_description } },
     { XD_LO_LINK,    offsetof (Lisp_Hash_Table, next_weak) },
     { XD_END }
   };

   Note that we don't need to declare all the elements in the structure, just
   the ones that need to be relocated (Lisp_Objects and structures) or that
   need to be referenced as counts for relocated objects.

   A description map looks like this:

   static const struct sized_memory_description specifier_extra_description_map [] = {
   { offsetof (Lisp_Specifier, methods) },
   { offsetof (struct specifier_methods, extra_description) },
   { -1 }
   };
 
   const struct memory_description specifier_description[] = {
     ...
     { XD_BLOCK_ARRAY, offset (Lisp_Specifier, data), 1,
       { specifier_extra_description_map } },
     ...
     { XD_END }
   };

   This would be appropriate for an object that looks like this:
 
   struct specifier_methods
   {
     ...
     const struct sized_memory_description *extra_description;
     ...
   };

   struct Lisp_Specifier
   {
     struct LCRECORD_HEADER header;
     struct specifier_methods *methods;
   
     ...
     // type-specific extra data attached to a specifier
     max_align_t data[1];
   };

   The description map means "retrieve a pointer into the object at offset
   `offsetof (Lisp_Specifier, methods)' , then in turn retrieve a pointer
   into that object at offset `offsetof (struct specifier_methods,
   extra_description)', and that is the sized_memory_description to use." 
   There can be any number of indirections, which can be either into
   straight pointers or Lisp_Objects.  The way that description maps are
   distinguished from normal sized_memory_descriptions is that in the
   former, the memory_description pointer is NULL.

   --ben


   The existing types :


    XD_LISP_OBJECT

  A Lisp object.  This is also the type to use for pointers to other lrecords
  (e.g. struct frame *).

    XD_LISP_OBJECT_ARRAY

  An array of Lisp objects or (equivalently) pointers to lrecords.
  The parameter (i.e. third element) is the count.  This would be declared
  as Lisp_Object foo[666].  For something declared as Lisp_Object *foo,
  use XD_BLOCK_PTR, whose description parameter is a sized_memory_description
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

    XD_OPAQUE_PTR_CONVERTIBLE

  Pointer to data which is not directly dumpable but can be converted
  to a dumpable, opaque external representation.  The parameter is
  a pointer to an opaque_convert_functions struct.

    XD_OPAQUE_DATA_CONVERTIBLE

  Data which is not directly dumpable but can be converted to a
  dumpable, opaque external representation.  The parameter is a
  pointer to an opaque_convert_functions struct.

    XD_BLOCK_PTR

  Pointer to block of described memory. (This is misnamed: It is NOT
  necessarily a pointer to a struct foo.) Parameters are number of
  contiguous blocks and sized_memory_description.

    XD_BLOCK_ARRAY

  Array of blocks of described memory.  Parameters are number of
  structures and sized_memory_description.  This differs from XD_BLOCK_PTR
  in that the parameter is declared as struct foo[666] instead of
  struct *foo.  In other words, the block of memory holding the
  structures is within the containing structure, rather than being
  elsewhere, with a pointer in the containing structure.

  NOTE NOTE NOTE: Be sure that you understand the difference between
  XD_BLOCK_PTR and XD_BLOCK_ARRAY:
    - struct foo bar[666], i.e. 666 inline struct foos
        --> XD_BLOCK_ARRAY, argument 666, pointing to a description of
            struct foo
    - struct foo *bar, i.e. pointer to a block of 666 struct foos
        --> XD_BLOCK_PTR, argument 666, pointing to a description of
            struct foo
    - struct foo *bar[666], i.e. 666 pointers to separate blocks of struct foos
        --> XD_BLOCK_ARRAY, argument 666, pointing to a description of
	    a single pointer to struct foo; the description is a single
	    XD_BLOCK_PTR, argument 1, which in turn points to a description
	    of struct foo.

  NOTE also that an XD_BLOCK_PTR of 666 foos is equivalent to an
  XD_BLOCK_PTR of 1 bar, where the description of `bar' is an
  XD_BLOCK_ARRAY of 666 foos. 

    XD_OPAQUE_DATA_PTR

  Pointer to dumpable opaque data.  Parameter is the size of the data.
  Pointed data must be relocatable without changes.

    XD_UNION

  Union of two or more different types of data.  Parameters are a constant
  which determines which type the data is (this is usually an XD_INDIRECT,
  referring to one of the fields in the structure), and a "sizing lobby" (a
  sized_memory_description, which points to a memory_description and
  indicates its size).  The size field in the sizing lobby describes the
  size of the union field in the object, and the memory_description in it
  is referred to as a "union map" and has a special interpretation: The
  offset field is replaced by a constant, which is compared to the first
  parameter of the XD_UNION descriptor to determine if this description
  applies to the union data, and XD_INDIRECT references refer to the
  containing object and description.  Note that the description applies
  "inline" to the union data, like XD_BLOCK_ARRAY and not XD_BLOCK_PTR.
  If the union data is a pointer to different types of structures, each
  element in the memory_description should be an XD_BLOCK_PTR.  See
  unicode.c, redisplay.c and objects.c for examples of XD_UNION.

    XD_UNION_DYNAMIC_SIZE

  Same as XD_UNION except that this is used for objects where the size of
  the object containing the union varies depending on the particular value
  of the union constant.  That is, an object with plain XD_UNION typically
  has the union declared as `union foo' or as `void *', where an object
  with XD_UNION_DYNAMIC_SIZE typically has the union as the last element,
  and declared as something like Rawbyte foo[1].  With plain XD_UNION, the
  object is (usually) of fixed size and always contains enough space for
  the data associated with all possible union constants, and thus the union
  constant can potentially change during the lifetime of the object.  With
  XD_UNION_DYNAMIC_SIZE, however, the union constant is fixed at the time
  of creation of the object, and the size of the object is computed
  dynamically at creation time based on the size of the data associated
  with the union constant.  Currently, the only difference between XD_UNION
  and XD_UNION_DYNAMIC_SIZE is how the size of the union data is
  calculated, when (a) the structure containing the union has no size
  given; (b) the union occurs as the last element in the structure; and (c)
  the union has no size given (in the first-level sized_memory_description
  pointed to).  In this circumstance, the size of XD_UNION comes from the
  max size of the data associated with all possible union constants,
  whereas the size of XD_UNION_DYNAMIC_SIZE comes from the size of the data
  associated with the currently specified (and unchangeable) union
  constant.

    XD_ASCII_STRING

  Pointer to a C string, purely ASCII.

    XD_DOC_STRING

  Pointer to a doc string (C string in pure ASCII if positive,
  opaque value if negative)

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


  Special macros:

    XD_INDIRECT (line, delta)
  Usable where a count, size, offset or union constant is requested.  Gives
  the value of the element which is at line number 'line' in the
  description (count starts at zero) and adds delta to it, which must
  (currently) be positive.
*/

enum memory_description_type
{
  XD_LISP_OBJECT_ARRAY,
  XD_LISP_OBJECT,
#ifdef NEW_GC
  XD_LISP_OBJECT_BLOCK_PTR,
#endif /* NEW_GC */
  XD_LO_LINK,
  XD_OPAQUE_PTR,
  XD_OPAQUE_PTR_CONVERTIBLE,
  XD_OPAQUE_DATA_CONVERTIBLE,
  XD_OPAQUE_DATA_PTR,
  XD_BLOCK_PTR,
  XD_BLOCK_ARRAY,
  XD_UNION,
  XD_UNION_DYNAMIC_SIZE,
  XD_ASCII_STRING,
  XD_DOC_STRING,
  XD_INT_RESET,
  XD_BYTECOUNT,
  XD_ELEMCOUNT,
  XD_HASHCODE,
  XD_INT,
  XD_LONG,
  XD_END
};

enum data_description_entry_flags
{
  /* If set, KKCC does not process this entry.

  (1) One obvious use is with things that pdump saves but which do not get
  marked normally -- for example the next and prev fields in a marker.  The
  marker chain is weak, with its entries removed when they are finalized.

  (2) This can be set on structures not containing any Lisp objects, or (more
  usefully) on structures that contain Lisp objects but where the objects
  always occur in another structure as well.  For example, the extent lists
  kept by a buffer keep the extents in two lists, one sorted by the start
  of the extent and the other by the end.  There's no point in marking
  both, since each contains the same objects as the other; but when dumping
  (if we were to dump such a structure), when computing memory size, etc.,
  it's crucial to tag both sides.
  */
  XD_FLAG_NO_KKCC = 1,
  /* If set, pdump does not process this entry. */
  XD_FLAG_NO_PDUMP = 2,
  /* Indicates that this is a "default" entry in a union map. */
  XD_FLAG_UNION_DEFAULT_ENTRY = 4,
#ifndef NEW_GC
  /* Indicates that this is a free Lisp object we're marking.
     Only relevant for ERROR_CHECK_GC.  This occurs when we're marking
     lcrecord-lists, where the objects have had their type changed to
     lrecord_type_free and also have had their free bit set, but we mark
     them as normal. */
  XD_FLAG_FREE_LISP_OBJECT = 8
#endif /* not NEW_GC */
#if 0
  ,
  /* Suggestions for other possible flags: */

  /* Eliminate XD_UNION_DYNAMIC_SIZE and replace it with a flag, like this. */
  XD_FLAG_UNION_DYNAMIC_SIZE = 16,
  /* Require that everyone who uses a description map has to flag it, so
     that it's easy to tell, when looking through the code, where the
     description maps are and who's using them.  This might also become
     necessary if for some reason the format of the description map is
     expanded and we need to stick a pointer in the second slot (although
     we could still ensure that the second slot in the first entry was NULL
     or <0). */
  XD_FLAG_DESCRIPTION_MAP = 32
#endif
};

union memory_contents_description
{
  /* The first element is used by static initializers only.  We always read
     from one of the other two pointers. */
  const void *write_only;
  const struct sized_memory_description *descr;
  const struct opaque_convert_functions *funcs;
};

struct memory_description
{
  enum memory_description_type type;
  Bytecount offset;
  EMACS_INT data1;
  union memory_contents_description data2;
  /* Indicates which subsystems process this entry, plus (potentially) other
     flags that apply to this entry. */
  int flags;
};

struct sized_memory_description
{
  Bytecount size;
  const struct memory_description *description;
};


struct opaque_convert_functions
{
  /* Used by XD_OPAQUE_PTR_CONVERTIBLE and
     XD_OPAQUE_DATA_CONVERTIBLE */

  /* Converter to external representation, for those objects from
     external libraries that can't be directly dumped as opaque data
     because they contain pointers.  This is called at dump time to
     convert to an opaque, pointer-less representation.

     This function must put a pointer to the opaque result in *data
     and its size in *size. */
  void (*convert)(const void *object, void **data, Bytecount *size);

  /* Post-conversion cleanup.  Optional (null if not provided).

     When provided it will be called post-dumping to free any storage
     allocated for the conversion results. */
  void (*convert_free)(const void *object, void *data, Bytecount size);

  /* De-conversion.

     At reload time, rebuilds the object from the converted form.
     "object" is 0 for the PTR case, return is ignored in the DATA
     case. */
  void *(*deconvert)(void *object, void *data, Bytecount size);

};

#define XD_INDIRECT(val, delta) (-1 - (Bytecount) ((val) | ((delta) << 8)))

#define XD_IS_INDIRECT(code) ((code) < 0)
#define XD_INDIRECT_VAL(code) ((-1 - (code)) & 255)
#define XD_INDIRECT_DELTA(code) ((-1 - (code)) >> 8)

/* DEFINE_LRECORD_IMPLEMENTATION is for objects with constant size.
   DEFINE_LRECORD_SEQUENCE_IMPLEMENTATION is for objects whose size varies.
 */

#if defined (ERROR_CHECK_TYPES)
# define DECLARE_ERROR_CHECK_TYPES(c_name, structtype)
#else
# define DECLARE_ERROR_CHECK_TYPES(c_name, structtype)
#endif


#define DEFINE_BASIC_LRECORD_IMPLEMENTATION(name,c_name,dumpable,marker,printer,nuker,equal,hash,desc,structtype) \
DEFINE_BASIC_LRECORD_IMPLEMENTATION_WITH_PROPS(name,c_name,dumpable,marker,printer,nuker,equal,hash,desc,0,0,0,0,structtype)

#define DEFINE_BASIC_LRECORD_IMPLEMENTATION_WITH_PROPS(name,c_name,dumpable,marker,printer,nuker,equal,hash,desc,getprop,putprop,remprop,plist,structtype) \
MAKE_LRECORD_IMPLEMENTATION(name,c_name,dumpable,marker,printer,nuker,equal,hash,desc,getprop,putprop,remprop,plist,sizeof(structtype),0,1,structtype)

#define DEFINE_LRECORD_IMPLEMENTATION(name,c_name,dumpable,marker,printer,nuker,equal,hash,desc,structtype) \
DEFINE_LRECORD_IMPLEMENTATION_WITH_PROPS(name,c_name,dumpable,marker,printer,nuker,equal,hash,desc,0,0,0,0,structtype)

#define DEFINE_LRECORD_IMPLEMENTATION_WITH_PROPS(name,c_name,dumpable,marker,printer,nuker,equal,hash,desc,getprop,putprop,remprop,plist,structtype) \
MAKE_LRECORD_IMPLEMENTATION(name,c_name,dumpable,marker,printer,nuker,equal,hash,desc,getprop,putprop,remprop,plist,sizeof (structtype),0,0,structtype)

#define DEFINE_LRECORD_SEQUENCE_IMPLEMENTATION(name,c_name,dumpable,marker,printer,nuker,equal,hash,desc,sizer,structtype) \
DEFINE_LRECORD_SEQUENCE_IMPLEMENTATION_WITH_PROPS(name,c_name,dumpable,marker,printer,nuker,equal,hash,desc,0,0,0,0,sizer,structtype)

#define DEFINE_BASIC_LRECORD_SEQUENCE_IMPLEMENTATION(name,c_name,dumpable,marker,printer,nuker,equal,hash,desc,sizer,structtype) \
MAKE_LRECORD_IMPLEMENTATION(name,c_name,dumpable,marker,printer,nuker,equal,hash,desc,0,0,0,0,0,sizer,1,structtype)

#define DEFINE_LRECORD_SEQUENCE_IMPLEMENTATION_WITH_PROPS(name,c_name,dumpable,marker,printer,nuker,equal,hash,desc,getprop,putprop,remprop,plist,sizer,structtype) \
MAKE_LRECORD_IMPLEMENTATION(name,c_name,dumpable,marker,printer,nuker,equal,hash,desc,getprop,putprop,remprop,plist,0,sizer,0,structtype)

#ifdef NEW_GC
#define MAKE_LRECORD_IMPLEMENTATION(name,c_name,dumpable,marker,printer,nuker,equal,hash,desc,getprop,putprop,remprop,plist,size,sizer,basic_p,structtype) \
DECLARE_ERROR_CHECK_TYPES(c_name, structtype)				\
const struct lrecord_implementation lrecord_##c_name =			\
  { name, dumpable, marker, printer, nuker, equal, hash, desc,		\
    getprop, putprop, remprop, plist, size, sizer,			\
    lrecord_type_##c_name }
#else /* not NEW_GC */
#define MAKE_LRECORD_IMPLEMENTATION(name,c_name,dumpable,marker,printer,nuker,equal,hash,desc,getprop,putprop,remprop,plist,size,sizer,basic_p,structtype) \
DECLARE_ERROR_CHECK_TYPES(c_name, structtype)				\
const struct lrecord_implementation lrecord_##c_name =			\
  { name, dumpable, marker, printer, nuker, equal, hash, desc,		\
    getprop, putprop, remprop, plist, size, sizer,			\
    lrecord_type_##c_name, basic_p }
#endif /* not NEW_GC */

#define DEFINE_EXTERNAL_LRECORD_IMPLEMENTATION(name,c_name,dumpable,marker,printer,nuker,equal,hash,desc,structtype) \
DEFINE_EXTERNAL_LRECORD_IMPLEMENTATION_WITH_PROPS(name,c_name,dumpable,marker,printer,nuker,equal,hash,desc,0,0,0,0,structtype)

#define DEFINE_EXTERNAL_LRECORD_IMPLEMENTATION_WITH_PROPS(name,c_name,dumpable,marker,printer,nuker,equal,hash,desc,getprop,putprop,remprop,plist,structtype) \
MAKE_EXTERNAL_LRECORD_IMPLEMENTATION(name,c_name,dumpable,marker,printer,nuker,equal,hash,desc,getprop,putprop,remprop,plist,sizeof (structtype),0,0,structtype)

#define DEFINE_EXTERNAL_LRECORD_SEQUENCE_IMPLEMENTATION(name,c_name,dumpable,marker,printer,nuker,equal,hash,desc,sizer,structtype) \
DEFINE_EXTERNAL_LRECORD_SEQUENCE_IMPLEMENTATION_WITH_PROPS(name,c_name,dumpable,marker,printer,nuker,equal,hash,desc,0,0,0,0,sizer,structtype)

#define DEFINE_EXTERNAL_LRECORD_SEQUENCE_IMPLEMENTATION_WITH_PROPS(name,c_name,dumpable,marker,printer,nuker,equal,hash,desc,getprop,putprop,remprop,plist,sizer,structtype) \
MAKE_EXTERNAL_LRECORD_IMPLEMENTATION(name,c_name,dumpable,marker,printer,nuker,equal,hash,desc,getprop,putprop,remprop,plist,0,sizer,0,structtype)

#ifdef NEW_GC
#define MAKE_EXTERNAL_LRECORD_IMPLEMENTATION(name,c_name,dumpable,marker,printer,nuker,equal,hash,desc,getprop,putprop,remprop,plist,size,sizer,basic_p,structtype) \
DECLARE_ERROR_CHECK_TYPES(c_name, structtype)				\
int lrecord_type_##c_name;						\
struct lrecord_implementation lrecord_##c_name =			\
  { name, dumpable, marker, printer, nuker, equal, hash, desc,		\
    getprop, putprop, remprop, plist, size, sizer,			\
    lrecord_type_last_built_in_type }
#else /* not NEW_GC */
#define MAKE_EXTERNAL_LRECORD_IMPLEMENTATION(name,c_name,dumpable,marker,printer,nuker,equal,hash,desc,getprop,putprop,remprop,plist,size,sizer,basic_p,structtype) \
DECLARE_ERROR_CHECK_TYPES(c_name, structtype)				\
int lrecord_type_##c_name;						\
struct lrecord_implementation lrecord_##c_name =			\
  { name, dumpable, marker, printer, nuker, equal, hash, desc,		\
    getprop, putprop, remprop, plist, size, sizer,			\
    lrecord_type_last_built_in_type, basic_p }
#endif /* not NEW_GC */

#ifdef USE_KKCC
extern MODULE_API const struct memory_description *lrecord_memory_descriptions[];

#define INIT_LRECORD_IMPLEMENTATION(type) do {				\
  lrecord_implementations_table[lrecord_type_##type] = &lrecord_##type;	\
  lrecord_memory_descriptions[lrecord_type_##type] =			\
    lrecord_implementations_table[lrecord_type_##type]->description;	\
} while (0)
#else /* not USE_KKCC */
extern MODULE_API Lisp_Object (*lrecord_markers[]) (Lisp_Object);

#define INIT_LRECORD_IMPLEMENTATION(type) do {				\
  lrecord_implementations_table[lrecord_type_##type] = &lrecord_##type;	\
  lrecord_markers[lrecord_type_##type] =				\
    lrecord_implementations_table[lrecord_type_##type]->marker;		\
} while (0)
#endif /* not USE_KKCC */

#define INIT_EXTERNAL_LRECORD_IMPLEMENTATION(type) do {			\
  lrecord_type_##type = lrecord_type_count++;				\
  lrecord_##type.lrecord_type_index = lrecord_type_##type;		\
  INIT_LRECORD_IMPLEMENTATION(type);					\
} while (0)

#ifdef HAVE_SHLIB
/* Allow undefining types in order to support module unloading. */

#ifdef USE_KKCC
#define UNDEF_LRECORD_IMPLEMENTATION(type) do {				\
  lrecord_implementations_table[lrecord_type_##type] = NULL;		\
  lrecord_memory_descriptions[lrecord_type_##type] = NULL;		\
} while (0)
#else /* not USE_KKCC */
#define UNDEF_LRECORD_IMPLEMENTATION(type) do {				\
  lrecord_implementations_table[lrecord_type_##type] = NULL;		\
  lrecord_markers[lrecord_type_##type] = NULL;				\
} while (0)
#endif /* not USE_KKCC */

#define UNDEF_EXTERNAL_LRECORD_IMPLEMENTATION(type) do {		\
  if (lrecord_##type.lrecord_type_index == lrecord_type_count - 1) {	\
    /* This is the most recently defined type.  Clean up nicely. */	\
    lrecord_type_##type = lrecord_type_count--;				\
  } /* Else we can't help leaving a hole with this implementation. */	\
  UNDEF_LRECORD_IMPLEMENTATION(type);					\
} while (0)

#endif /* HAVE_SHLIB */

#define LRECORDP(a) (XTYPE (a) == Lisp_Type_Record)
#define XRECORD_LHEADER(a) ((struct lrecord_header *) XPNTR (a))

#define RECORD_TYPEP(x, ty) \
  (LRECORDP (x) && (XRECORD_LHEADER (x)->type == (unsigned int) (ty)))

/* Steps to create a new object:

   1. Declare the struct for your object in a header file somewhere.
   Remember that it must begin with

   struct LCRECORD_HEADER header;

   2. Put the "standard junk" (DECLARE_RECORD()/XFOO/etc.) below the
      struct definition -- see below.

   3. Add this header file to inline.c.

   4. Create the methods for your object.  Note that technically you don't
   need any, but you will almost always want at least a mark method.

   4. Create the data layout description for your object.  See
   toolbar_button_description below; the comment above in `struct lrecord',
   describing the purpose of the descriptions; and comments elsewhere in
   this file describing the exact syntax of the description structures.

   6. Define your object with DEFINE_LRECORD_IMPLEMENTATION() or some
   variant.

   7. Include the header file in the .c file where you defined the object.

   8. Put a call to INIT_LRECORD_IMPLEMENTATION() for the object in the
   .c file's syms_of_foo() function.

   9. Add a type enum for the object to enum lrecord_type, earlier in this
   file.

   --ben

An example:

------------------------------ in toolbar.h -----------------------------

struct toolbar_button
{
  struct LCRECORD_HEADER header;

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
#define wrap_toolbar_button(p) wrap_record (p, toolbar_button)
#define TOOLBAR_BUTTONP(x) RECORDP (x, toolbar_button)
#define CHECK_TOOLBAR_BUTTON(x) CHECK_RECORD (x, toolbar_button)
#define CONCHECK_TOOLBAR_BUTTON(x) CONCHECK_RECORD (x, toolbar_button)

------------------------------ in toolbar.c -----------------------------

#include "toolbar.h"

...

static const struct memory_description toolbar_button_description [] = {
  { XD_LISP_OBJECT, offsetof (struct toolbar_button, next) },
  { XD_LISP_OBJECT, offsetof (struct toolbar_button, frame) },
  { XD_LISP_OBJECT, offsetof (struct toolbar_button, up_glyph) },
  { XD_LISP_OBJECT, offsetof (struct toolbar_button, down_glyph) },
  { XD_LISP_OBJECT, offsetof (struct toolbar_button, disabled_glyph) },
  { XD_LISP_OBJECT, offsetof (struct toolbar_button, cap_up_glyph) },
  { XD_LISP_OBJECT, offsetof (struct toolbar_button, cap_down_glyph) },
  { XD_LISP_OBJECT, offsetof (struct toolbar_button, cap_disabled_glyph) },
  { XD_LISP_OBJECT, offsetof (struct toolbar_button, callback) },
  { XD_LISP_OBJECT, offsetof (struct toolbar_button, enabled_p) },
  { XD_LISP_OBJECT, offsetof (struct toolbar_button, help_string) },
  { XD_END }
};

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
 			       0, mark_toolbar_button, 0, 0, 0, 0,
                               toolbar_button_description,
 			       struct toolbar_button);

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


--ben

*/

/*

Note: Object types defined in external dynamically-loaded modules (not
part of the XEmacs main source code) should use DECLARE_EXTERNAL_LRECORD
and DEFINE_EXTERNAL_LRECORD_IMPLEMENTATION rather than DECLARE_LRECORD
and DEFINE_LRECORD_IMPLEMENTATION.  The EXTERNAL versions declare and
allocate an enumerator for the type being defined.

*/


#ifdef ERROR_CHECK_TYPES

# define DECLARE_LRECORD(c_name, structtype)				  \
extern const struct lrecord_implementation lrecord_##c_name;		  \
DECLARE_INLINE_HEADER (							  \
structtype *								  \
error_check_##c_name (Lisp_Object obj, const Ascbyte *file, int line)	  \
)									  \
{									  \
  assert_at_line (RECORD_TYPEP (obj, lrecord_type_##c_name), file, line); \
  return (structtype *) XPNTR (obj);					  \
}									  \
extern Lisp_Object Q##c_name##p

# define DECLARE_MODULE_API_LRECORD(c_name, structtype)			  \
extern MODULE_API const struct lrecord_implementation lrecord_##c_name;	  \
DECLARE_INLINE_HEADER (							  \
structtype *								  \
error_check_##c_name (Lisp_Object obj, const Ascbyte *file, int line)	  \
)									  \
{									  \
  assert_at_line (RECORD_TYPEP (obj, lrecord_type_##c_name), file, line); \
  return (structtype *) XPNTR (obj);					  \
}									  \
extern MODULE_API Lisp_Object Q##c_name##p

# define DECLARE_EXTERNAL_LRECORD(c_name, structtype)			  \
extern int lrecord_type_##c_name;					  \
extern struct lrecord_implementation lrecord_##c_name;			  \
DECLARE_INLINE_HEADER (							  \
structtype *								  \
error_check_##c_name (Lisp_Object obj, const Ascbyte *file, int line)	  \
)									  \
{									  \
  assert_at_line (RECORD_TYPEP (obj, lrecord_type_##c_name), file, line); \
  return (structtype *) XPNTR (obj);					  \
}									  \
extern Lisp_Object Q##c_name##p

# define DECLARE_NONRECORD(c_name, type_enum, structtype)		\
DECLARE_INLINE_HEADER (							\
structtype *								\
error_check_##c_name (Lisp_Object obj, const Ascbyte *file, int line)	\
)									\
{									\
  assert_at_line (XTYPE (obj) == type_enum, file, line);		\
  return (structtype *) XPNTR (obj);					\
}									\
extern Lisp_Object Q##c_name##p

# define XRECORD(x, c_name, structtype) \
  error_check_##c_name (x, __FILE__, __LINE__)
# define XNONRECORD(x, c_name, type_enum, structtype) \
  error_check_##c_name (x, __FILE__, __LINE__)

DECLARE_INLINE_HEADER (
Lisp_Object
wrap_record_1 (const void *ptr, enum lrecord_type ty, const Ascbyte *file,
	       int line)
)
{
  Lisp_Object obj = wrap_pointer_1 (ptr);

  assert_at_line (RECORD_TYPEP (obj, ty), file, line);
  return obj;
}

#define wrap_record(ptr, ty) \
  wrap_record_1 (ptr, lrecord_type_##ty, __FILE__, __LINE__)

#else /* not ERROR_CHECK_TYPES */

# define DECLARE_LRECORD(c_name, structtype)			\
extern Lisp_Object Q##c_name##p;				\
extern const struct lrecord_implementation lrecord_##c_name
# define DECLARE_MODULE_API_LRECORD(c_name, structtype)			\
extern MODULE_API Lisp_Object Q##c_name##p;				\
extern MODULE_API const struct lrecord_implementation lrecord_##c_name
# define DECLARE_EXTERNAL_LRECORD(c_name, structtype)		\
extern Lisp_Object Q##c_name##p;				\
extern int lrecord_type_##c_name;				\
extern struct lrecord_implementation lrecord_##c_name
# define DECLARE_NONRECORD(c_name, type_enum, structtype)	\
extern Lisp_Object Q##c_name##p
# define XRECORD(x, c_name, structtype) ((structtype *) XPNTR (x))
# define XNONRECORD(x, c_name, type_enum, structtype)		\
  ((structtype *) XPNTR (x))
/* wrap_pointer_1 is so named as a suggestion not to use it unless you
   know what you're doing. */
#define wrap_record(ptr, ty) wrap_pointer_1 (ptr)

#endif /* not ERROR_CHECK_TYPES */

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

#ifndef NEW_GC
/*-------------------------- lcrecord-list -----------------------------*/

struct lcrecord_list
{
  struct LCRECORD_HEADER header;
  Lisp_Object free;
  Elemcount size;
  const struct lrecord_implementation *implementation;
};

DECLARE_LRECORD (lcrecord_list, struct lcrecord_list);
#define XLCRECORD_LIST(x) XRECORD (x, lcrecord_list, struct lcrecord_list)
#define wrap_lcrecord_list(p) wrap_record (p, lcrecord_list)
#define LCRECORD_LISTP(x) RECORDP (x, lcrecord_list)
/* #define CHECK_LCRECORD_LIST(x) CHECK_RECORD (x, lcrecord_list)
   Lcrecord lists should never escape to the Lisp level, so
   functions should not be doing this. */

/* Various ways of allocating lcrecords.  All bytes (except lcrecord
   header) are zeroed in returned structure.

   See above for a discussion of the difference between plain lrecords and
   lrecords.  lcrecords themselves are divided into three types: (1)
   auto-managed, (2) hand-managed, and (3) unmanaged.  "Managed" refers to
   using a special object called an lcrecord-list to keep track of freed
   lcrecords, which can freed with FREE_LCRECORD() or the like and later be
   recycled when a new lcrecord is required, rather than requiring new
   malloc().  Thus, allocation of lcrecords can be very
   cheap. (Technically, the lcrecord-list manager could divide up large
   chunks of memory and allocate out of that, mimicking what happens with
   lrecords.  At that point, however, we'd want to rethink the whole
   division between lrecords and lcrecords.) 

   NOTE: There is a fundamental limitation of lcrecord-lists, which is that
   they only handle blocks of a particular, fixed size.  Thus, objects that
   can be of varying sizes need to do various tricks.  These considerations
   in particular dictate the various types of management:

   -- "Auto-managed" means that you just go ahead and allocate the lcrecord
   whenever you want, using old_alloc_lcrecord_type(), and the appropriate
   lcrecord-list manager is automatically created.  To free, you just call
   "FREE_LCRECORD()" and the appropriate lcrecord-list manager is
   automatically located and called.  The limitation here of course is that
   all your objects are of the same size. (#### Eventually we should have a
   more sophisticated system that tracks the sizes seen and creates one
   lcrecord list per size, indexed in a hash table.  Usually there are only
   a limited number of sizes, so this works well.)

   -- "Hand-managed" exists because we haven't yet written the more
   sophisticated scheme for auto-handling different-sized lcrecords, as
   described in the end of the last paragraph.  In this model, you go ahead
   and create the lcrecord-list objects yourself for the sizes you will
   need, using make_lcrecord_list().  Then, create lcrecords using
   alloc_managed_lcrecord(), passing in the lcrecord-list you created, and
   free them with free_managed_lcrecord().

   -- "Unmanaged" means you simply allocate lcrecords, period.  No
   lcrecord-lists, no way to free them.  This may be suitable when the
   lcrecords are variable-sized and (a) you're too lazy to write the code
   to hand-manage them, or (b) the objects you create are always or almost
   always Lisp-visible, and thus there's no point in freeing them (and it
   wouldn't be safe to do so).  You just create them with
   BASIC_ALLOC_LCRECORD(), and that's it.

   --ben

   Here is an in-depth look at the steps required to create a allocate an
   lcrecord using the hand-managed style.  Since this is the most
   complicated, you will learn a lot about the other styles as well.  In
   addition, there is useful general information about what freeing an
   lcrecord really entails, and what are the precautions:

   1) Create an lcrecord-list object using make_lcrecord_list().  This is
      often done at initialization.  Remember to staticpro_nodump() this
      object!  The arguments to make_lcrecord_list() are the same as would be
      passed to BASIC_ALLOC_LCRECORD().

   2) Instead of calling BASIC_ALLOC_LCRECORD(), call alloc_managed_lcrecord()
      and pass the lcrecord-list earlier created.

   3) When done with the lcrecord, call free_managed_lcrecord().  The
      standard freeing caveats apply: ** make sure there are no pointers to
      the object anywhere! **

   4) Calling free_managed_lcrecord() is just like kissing the
      lcrecord goodbye as if it were garbage-collected.  This means:
      -- the contents of the freed lcrecord are undefined, and the
         contents of something produced by alloc_managed_lcrecord()
	 are undefined, just like for BASIC_ALLOC_LCRECORD().
      -- the mark method for the lcrecord's type will *NEVER* be called
         on freed lcrecords.
      -- the finalize method for the lcrecord's type will be called
         at the time that free_managed_lcrecord() is called.
 */

/* UNMANAGED MODEL: */
void *old_basic_alloc_lcrecord (Bytecount size,
				const struct lrecord_implementation *);

/* HAND-MANAGED MODEL: */
Lisp_Object make_lcrecord_list (Elemcount size,
				const struct lrecord_implementation
				*implementation);
Lisp_Object alloc_managed_lcrecord (Lisp_Object lcrecord_list);
void free_managed_lcrecord (Lisp_Object lcrecord_list, Lisp_Object lcrecord);

/* AUTO-MANAGED MODEL: */
MODULE_API void *
alloc_automanaged_lcrecord (Bytecount size,
			    const struct lrecord_implementation *);

#define old_alloc_lcrecord_type(type, lrecord_implementation) \
  ((type *) alloc_automanaged_lcrecord (sizeof (type), lrecord_implementation))

void old_free_lcrecord (Lisp_Object rec);


/* Copy the data from one lcrecord structure into another, but don't
   overwrite the header information. */

#define old_copy_sized_lcrecord(dst, src, size)				\
  memcpy ((Rawbyte *) (dst) + sizeof (struct old_lcrecord_header),	\
	  (Rawbyte *) (src) + sizeof (struct old_lcrecord_header),	\
	  (size) - sizeof (struct old_lcrecord_header))

#define old_copy_lcrecord(dst, src) \
  old_copy_sized_lcrecord (dst, src, sizeof (*(dst)))

#define old_zero_sized_lcrecord(lcr, size)				\
   memset ((Rawbyte *) (lcr) + sizeof (struct old_lcrecord_header), 0,	\
	   (size) - sizeof (struct old_lcrecord_header))

#define old_zero_lcrecord(lcr) old_zero_sized_lcrecord (lcr, sizeof (*(lcr)))

#else /* NEW_GC */

/* How to allocate a lrecord:
   
   - If the size of the lrecord is fix, say it equals its size of its
   struct, then use alloc_lrecord_type.

   - If the size varies, i.e. it is not equal to the size of its
   struct, use alloc_lrecord and specify the amount of storage you
   need for the object.

   - Some lrecords, which are used totally internally, use the
   noseeum-* functions for the reason of debugging. 

   - To free a Lisp_Object manually, use free_lrecord. */

void *alloc_lrecord (Bytecount size,
		     const struct lrecord_implementation *);

void *alloc_lrecord_array (Bytecount size, int elemcount,
			   const struct lrecord_implementation *);

#define alloc_lrecord_type(type, lrecord_implementation) \
  ((type *) alloc_lrecord (sizeof (type), lrecord_implementation))

void *noseeum_alloc_lrecord (Bytecount size,
			     const struct lrecord_implementation *);

#define noseeum_alloc_lrecord_type(type, lrecord_implementation) \
  ((type *) noseeum_alloc_lrecord (sizeof (type), lrecord_implementation))

void free_lrecord (Lisp_Object rec);


/* Copy the data from one lrecord structure into another, but don't
   overwrite the header information. */

#define copy_sized_lrecord(dst, src, size)			\
  memcpy ((char *) (dst) + sizeof (struct lrecord_header),	\
	  (char *) (src) + sizeof (struct lrecord_header),	\
	  (size) - sizeof (struct lrecord_header))

#define copy_lrecord(dst, src) copy_sized_lrecord (dst, src, sizeof (*(dst)))

#endif /* NEW_GC */

#define zero_sized_lrecord(lcr, size)				\
   memset ((char *) (lcr) + sizeof (struct lrecord_header), 0,	\
	   (size) - sizeof (struct lrecord_header))

#define zero_lrecord(lcr) zero_sized_lrecord (lcr, sizeof (*(lcr)))

DECLARE_INLINE_HEADER (
Bytecount
detagged_lisp_object_size (const struct lrecord_header *h)
)
{
  const struct lrecord_implementation *imp = LHEADER_IMPLEMENTATION (h);

  return (imp->size_in_bytes_method ?
	  imp->size_in_bytes_method (h) :
	  imp->static_size);
}

DECLARE_INLINE_HEADER (
Bytecount
lisp_object_size (Lisp_Object o)
)
{
  return detagged_lisp_object_size (XRECORD_LHEADER (o));
}


/************************************************************************/
/*		                 Dumping                		*/
/************************************************************************/

/* dump_add_root_block_ptr (&var, &desc) dumps the structure pointed to by
   `var'.  This is for a single relocatable pointer located in the data
   segment (i.e. the block pointed to is in the heap).

   If the structure pointed to is not a `struct' but an array, you should
   set the size field of the sized_memory_description to 0, and use
   XD_BLOCK_ARRAY in the inner memory_description.

   NOTE that a "root struct pointer" could also be described using
   dump_add_root_block(), with SIZE == sizeof (void *), and a description
   containing a single XD_BLOCK_PTR entry, offset 0, size 1, with a
   structure description the same as the value passed to
   dump_add_root_block_ptr().  That would require an extra level of
   description, though, as compared to using dump_add_root_block_ptr(),
   and thus this function is generally more convenient.
    */
#ifdef PDUMP
void dump_add_root_block_ptr (void *, const struct sized_memory_description *);
#else
#define dump_add_root_block_ptr(varaddr, descaddr) DO_NOTHING
#endif

/* dump_add_opaque (&var, size) dumps the opaque static structure `var'.
   This is for a static block of memory (in the data segment, not the
   heap), with no relocatable pointers in it. */
#ifdef PDUMP
#define dump_add_opaque(varaddr,size) dump_add_root_block (varaddr, size, NULL)
#else
#define dump_add_opaque(varaddr,size) DO_NOTHING
#endif

/* dump_add_root_block (ptr, size, desc) dumps the static structure
   located at `var' of size SIZE and described by DESC.  This is for a
   static block of memory (in the data segment, not the heap), with
   relocatable pointers in it. */
#ifdef PDUMP
void dump_add_root_block (const void *ptraddress, Bytecount size,
			  const struct memory_description *desc);
#else
#define dump_add_root_block(ptraddress, size, desc) DO_NOTHING
#endif

/* Call dump_add_opaque_int (&int_var) to dump `int_var', of type `int'. */
#ifdef PDUMP
#define dump_add_opaque_int(int_varaddr) do {	\
  int *dao_ = (int_varaddr); /* type check */	\
  dump_add_opaque (dao_, sizeof (*dao_));	\
} while (0)
#else
#define dump_add_opaque_int(int_varaddr) DO_NOTHING
#endif

/* Call dump_add_opaque_fixnum (&fixnum_var) to dump `fixnum_var', of type
   `Fixnum'. */
#ifdef PDUMP
#define dump_add_opaque_fixnum(fixnum_varaddr) do {	\
  Fixnum *dao_ = (fixnum_varaddr); /* type check */	\
  dump_add_opaque (dao_, sizeof (*dao_));		\
} while (0)
#else
#define dump_add_opaque_fixnum(fixnum_varaddr) DO_NOTHING
#endif

/* Call dump_add_root_lisp_object (&var) to ensure that var is properly
   updated after pdump. */
#ifdef PDUMP
void dump_add_root_lisp_object (Lisp_Object *);
#else
#define dump_add_root_lisp_object(varaddr) DO_NOTHING
#endif

/* Call dump_add_weak_lisp_object (&var) to ensure that var is properly
   updated after pdump.  var must point to a linked list of objects out of
   which some may not be dumped */
#ifdef PDUMP
void dump_add_weak_object_chain (Lisp_Object *);
#else
#define dump_add_weak_object_chain(varaddr) DO_NOTHING
#endif

/* Nonzero means Emacs has already been initialized.
   Used during startup to detect startup of dumped Emacs.  */
extern MODULE_API int initialized;

#ifdef PDUMP
#include "dumper.h"
#ifdef NEW_GC
#define DUMPEDP(adr) 0
#else /* not NEW_GC */
#define DUMPEDP(adr) ((((Rawbyte *) (adr)) < pdump_end) && \
                      (((Rawbyte *) (adr)) >= pdump_start))
#endif /* not NEW_GC */
#else
#define DUMPEDP(adr) 0
#endif

#define OBJECT_DUMPED_P(obj) DUMPEDP (XPNTR (obj))

/***********************************************************************/
/*                           data descriptions                         */
/***********************************************************************/


#if defined (USE_KKCC) || defined (PDUMP)

extern int in_pdump;

EMACS_INT lispdesc_indirect_count_1 (EMACS_INT code,
				     const struct memory_description *idesc,
				     const void *idata);
const struct sized_memory_description *lispdesc_indirect_description_1
 (const void *obj, const struct sized_memory_description *sdesc);
Bytecount lispdesc_block_size_1 (const void *obj, Bytecount size,
			         const struct memory_description *desc);

DECLARE_INLINE_HEADER (
Bytecount lispdesc_block_size (const void *obj,
			       const struct sized_memory_description *sdesc))
{
  return lispdesc_block_size_1 (obj, sdesc->size, sdesc->description);
}

DECLARE_INLINE_HEADER (
EMACS_INT
lispdesc_indirect_count (EMACS_INT code,
			 const struct memory_description *idesc,
			 const void *idata)
)
{
  if (XD_IS_INDIRECT (code))
    code = lispdesc_indirect_count_1 (code, idesc, idata);
  return code;
}

DECLARE_INLINE_HEADER (
const struct sized_memory_description *
lispdesc_indirect_description (const void *obj,
			       const struct sized_memory_description *sdesc)
)
{
  if (sdesc->description)
    return sdesc;
  else
    return lispdesc_indirect_description_1 (obj, sdesc);
}


/* Do standard XD_UNION processing.  DESC1 is an entry in DESC, which
   describes the entire data structure.  Returns NULL (do nothing, nothing
   matched), or a new value for DESC1.  In the latter case, assign to DESC1
   in your function and goto union_switcheroo. */

DECLARE_INLINE_HEADER (
const struct memory_description *
lispdesc_process_xd_union (const struct memory_description *desc1,
			   const struct memory_description *desc,
			   const void *data)
)
{
  int count = 0;
  EMACS_INT variant = lispdesc_indirect_count (desc1->data1, desc,
					       data);
  desc1 =
    lispdesc_indirect_description (data, desc1->data2.descr)->description;
  
  for (count = 0; desc1[count].type != XD_END; count++)
    {
      if ((desc1[count].flags & XD_FLAG_UNION_DEFAULT_ENTRY) ||
	  desc1[count].offset == variant)
	{
	  return &desc1[count];
	}
    }

  return NULL;
}

#endif /* defined (USE_KKCC) || defined (PDUMP) */

END_C_DECLS

#endif /* INCLUDED_lrecord_h_ */
