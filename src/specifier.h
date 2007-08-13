/* Generic specifier list implementation
   Copyright (C) 1994, 1995 Board of Trustees, University of Illinois.
   Copyright (C) 1995 Ben Wing

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

#ifndef _XEMACS_SPECIFIER_H_
#define _XEMACS_SPECIFIER_H_

struct specifier_methods
{
  CONST char *name;
  Lisp_Object predicate_symbol;

  /* Implementation specific methods: */

  /* #### Document me */
  /* Create method */
  void (*create_method) (Lisp_Object specifier);
  /* Mark method */
  void (*mark_method) (Lisp_Object specifier, void (*markobj) (Lisp_Object));
  /* Equal method */
  int (*equal_method) (Lisp_Object sp1, Lisp_Object sp2, int depth);
  /* Hash method */
  unsigned long (*hash_method) (Lisp_Object specifier, int depth);

  /* Validate method: Given an instantiator, verify that it's
     valid for this specifier type.  If not, signal an error.

     If this functions is not present, all instantiators are
     considered valid. */
  void (*validate_method) (Lisp_Object instantiator);

  /* Validate-matchspec method: Given a matchspec, verify that it's
     valid for this specifier type.  If not, signal an error.

     If this functions is not present, *no* matchspecs are considered
     valid.  Note that this differs from validate_method(). */
  void (*validate_matchspec_method) (Lisp_Object matchspec);

  /* Instantiate method */
  Lisp_Object (*instantiate_method) (Lisp_Object specifier,
				     Lisp_Object matchspec,
				     Lisp_Object domain,
				     Lisp_Object instantiator,
				     /* always an integer, but encapsulated
					as a Lisp_Object so it can be called
					from call_with_suspended_errors() */
				     Lisp_Object depth);
  /* Going-to-add method: Called when an instantiator is about
     to be added to a specifier.  This function can specify
     that different instantiators should be used instead by
     returning an inst-list (possibly containing zero elements).
     If the instantiator is fine as-is, return Qt.  The
     instantiator has been copied with copy-tree, so feel
     free to reuse parts of it to create a new instantiator.
     The tag-set, however, is not copied and is not canonicalized
     (that will be done on the result of this function).
     */
  Lisp_Object (*going_to_add_method) (Lisp_Object specifier,
				      Lisp_Object locale,
				      Lisp_Object tag_set,
				      Lisp_Object instantiator);
  /* After-change method */
  void (*after_change_method) (Lisp_Object specifier,
			       Lisp_Object locale);

  int extra_data_size;
};

struct Lisp_Specifier
{
  struct lcrecord_header header;
  struct specifier_methods *methods;

  /* we keep a chained list of all current specifiers, for GC cleanup
     purposes.  Do NOT mark through this, or specifiers will never
     be GC'd. */
  Lisp_Object next_specifier;

  /* This is a straight list of instantiators. */
  Lisp_Object global_specs;

  /* These are all assoc lists where the key is type of object the
     list represents (buffer, window, etc.) and the associated list is
     the actual list of instantiators. */
  Lisp_Object device_specs;
  Lisp_Object frame_specs;
  /* window_specs is actually a key-assoc weak list.  See specifier.c
     for an explanation of why (it boils down to the fact that
     dead windows can become live again through window configurations).
     */
  Lisp_Object window_specs;
  Lisp_Object buffer_specs;

  struct specifier_caching *caching;
  Lisp_Object fallback;
  /* type-specific extra data attached to a specifier */
  char data[1];
};

DECLARE_LRECORD (specifier, struct Lisp_Specifier);
#define XSPECIFIER(x) XRECORD (x, specifier, struct Lisp_Specifier)
#define XSETSPECIFIER(x, p) XSETRECORD (x, p, specifier)
#define SPECIFIERP(x) RECORDP (x, specifier)
#define GC_SPECIFIERP(x) GC_RECORDP (x, specifier)
#define CHECK_SPECIFIER(x) CHECK_RECORD (x, specifier)
#define CONCHECK_SPECIFIER(x) CONCHECK_RECORD (x, specifier)

/***** Calling a specifier method *****/

#define RAW_SPECMETH(sp, m) ((sp)->methods->m##_method)
#define HAS_SPECMETH_P(sp, m) (!!RAW_SPECMETH (sp, m))
#define SPECMETH(sp, m, args) (((sp)->methods->m##_method) args)

/* Call a void-returning specifier method, if it exists */
#define MAYBE_SPECMETH(sp, m, args)				\
do {								\
  struct Lisp_Specifier *_maybe_specmeth_sp = (sp);		\
  if (HAS_SPECMETH_P (_maybe_specmeth_sp, m))			\
    SPECMETH (_maybe_specmeth_sp, m, args);			\
} while (0)

MAC_DECLARE_EXTERN (struct Lisp_Specifier *, MTspecmeth_or_given)

/* Call a specifier method, if it exists; otherwise return
   the specified value */

#define SPECMETH_OR_GIVEN(sp, m, args, given)				\
MAC_BEGIN								\
  MAC_DECLARE (struct Lisp_Specifier *, MTspecmeth_or_given, sp)	\
  HAS_SPECMETH_P (MTspecmeth_or_given, m) ?				\
    SPECMETH (MTspecmeth_or_given, m, args) : (given)			\
MAC_END

/***** Defining new specifier types *****/

#define DECLARE_SPECIFIER_TYPE(type)				\
extern struct specifier_methods * type##_specifier_methods

#define DEFINE_SPECIFIER_TYPE(type)			\
struct specifier_methods * type##_specifier_methods

#define INITIALIZE_SPECIFIER_TYPE(type, obj_name, pred_sym)		\
  do {									\
    type##_specifier_methods =						\
      malloc_type_and_zero (struct specifier_methods);			\
    type##_specifier_methods->name = obj_name;				\
    defsymbol (&type##_specifier_methods->predicate_symbol,		\
	       pred_sym);						\
    add_entry_to_specifier_type_list (Q##type,				\
				      type##_specifier_methods);	\
  } while (0)								\

#define INITIALIZE_SPECIFIER_TYPE_WITH_DATA(type, obj_name, pred_sym)	\
  do {									\
    INITIALIZE_SPECIFIER_TYPE (type, obj_name, pred_sym);		\
    type##_specifier_methods->extra_data_size =				\
      sizeof (struct type##_specifier);					\
  } while (0)

/* Declare that specifier-type TYPE has method M; used in
   initialization routines */
#define SPECIFIER_HAS_METHOD(type, m) \
  (type##_specifier_methods->m##_method = type##_##m)

/***** Macros for accessing specifier types *****/

#define SPECIFIER_TYPE_P(sp, type)		\
  ((sp)->methods == type##_specifier_methods)

#ifdef ERROR_CHECK_TYPECHECK
MAC_DECLARE_EXTERN (struct Lisp_Specifier *, MTspecifier_data)
# define SPECIFIER_TYPE_DATA(sp, type)				\
MAC_BEGIN							\
  MAC_DECLARE (struct Lisp_Specifier *, MTspecifier_data, sp)	\
  assert (SPECIFIER_TYPE_P (MTspecifier_data, type))		\
  MAC_SEP							\
  (struct type##_specifier *) MTspecifier_data->data		\
MAC_END
#else
# define SPECIFIER_TYPE_DATA(sp, type)		\
  ((struct type##_specifier *) (sp)->data)
#endif

/* #### Need to create ERROR_CHECKING versions of these. */

#define XSPECIFIER_TYPE(x, type) XSPECIFIER (x)
#define XSETSPECIFIER_TYPE(x, p, type) XSETSPECIFIER (x, p)
#define SPECIFIER_TYPEP(x, type)				\
  (SPECIFIERP (x) && SPECIFIER_TYPE_P (XSPECIFIER (x), type))
#define CHECK_SPECIFIER_TYPE(x, type)				\
  do {								\
    CHECK_SPECIFIER (x);					\
    if (!(SPECIFIERP (x) && SPECIFIER_TYPE_P (XSPECIFIER (x),	\
					       type)))		\
      dead_wrong_type_argument					\
	(type##_specifier_methods->predicate_symbol, x);	\
  } while (0)
#define CONCHECK_SPECIFIER_TYPE(x, type)			\
  do {								\
    CONCHECK_SPECIFIER (x);					\
    if (!(SPECIFIERP (x) && SPECIFIER_TYPE_P (XSPECIFIER (x),	\
					       type)))		\
      x = wrong_type_argument					\
	(type##_specifier_methods->predicate_symbol, x);	\
  } while (0)

/***** Miscellaneous structures *****/

enum spec_locale_type
{
  LOCALE_GLOBAL,
  LOCALE_DEVICE,
  LOCALE_FRAME,
  LOCALE_WINDOW,
  LOCALE_BUFFER
};

enum spec_add_meth
{
  SPEC_PREPEND,
  SPEC_APPEND,
  SPEC_REMOVE_TAG_SET_PREPEND,
  SPEC_REMOVE_TAG_SET_APPEND,
  SPEC_REMOVE_LOCALE,
  SPEC_REMOVE_LOCALE_TYPE,
  SPEC_REMOVE_ALL
};

struct specifier_caching
{
  int offset_into_struct_window;
  void (*value_changed_in_window) (Lisp_Object specifier, struct window *w,
				   Lisp_Object oldval);
  int offset_into_struct_frame;
  void (*value_changed_in_frame) (Lisp_Object specifier, struct frame *f,
				  Lisp_Object oldval);
};

extern Lisp_Object decode_locale (Lisp_Object locale);
extern Lisp_Object decode_locale_list (Lisp_Object locale);
extern Lisp_Object decode_domain (Lisp_Object domain);
extern enum spec_add_meth
decode_how_to_add_specification (Lisp_Object how_to_add);
extern Lisp_Object decode_specifier_tag_set (Lisp_Object tag_set);

extern void add_entry_to_specifier_type_list (Lisp_Object symbol,
					      struct specifier_methods *meths);
extern void set_specifier_caching (Lisp_Object specifier,
				   int struct_window_offset,
				   void (*value_changed_in_window)
				   (Lisp_Object specifier, struct window *w,
				    Lisp_Object oldval),
				   int struct_frame_offset,
				   void (*value_changed_in_frame)
				   (Lisp_Object specifier, struct frame *f,
				    Lisp_Object oldval));
extern void set_specifier_fallback (Lisp_Object specifier,
				    Lisp_Object fallback);
extern void recompute_all_cached_specifiers_in_window (struct window *w);
extern void recompute_all_cached_specifiers_in_frame (struct frame *f);

extern void cleanup_specifiers (void);
extern void prune_specifiers (int (*obj_marked_p) (Lisp_Object));
extern void setup_device_initial_specifier_tags (struct device *d);
void kill_specifier_buffer_locals (Lisp_Object buffer);

DECLARE_SPECIFIER_TYPE (generic);
#define XGENERIC_SPECIFIER(x) XSPECIFIER_TYPE (x, generic)
#define XSETGENERIC_SPECIFIER(x, p) XSETSPECIFIER_TYPE (x, p, generic)
#define GENERIC_SPECIFIERP(x) SPECIFIER_TYPEP (x, generic)
#define CHECK_GENERIC_SPECIFIER(x) CHECK_SPECIFIER_TYPE (x, generic)
#define CONCHECK_GENERIC_SPECIFIER(x) CONCHECK_SPECIFIER_TYPE (x, generic)

DECLARE_SPECIFIER_TYPE (integer);
#define XINTEGER_SPECIFIER(x) XSPECIFIER_TYPE (x, integer)
#define XSETINTEGER_SPECIFIER(x, p) XSETSPECIFIER_TYPE (x, p, integer)
#define INTEGER_SPECIFIERP(x) SPECIFIER_TYPEP (x, integer)
#define CHECK_INTEGER_SPECIFIER(x) CHECK_SPECIFIER_TYPE (x, integer)
#define CONCHECK_INTEGER_SPECIFIER(x) CONCHECK_SPECIFIER_TYPE (x, integer)

DECLARE_SPECIFIER_TYPE (natnum);
#define XNATNUM_SPECIFIER(x) XSPECIFIER_TYPE (x, natnum)
#define XSETNATNUM_SPECIFIER(x, p) XSETSPECIFIER_TYPE (x, p, natnum)
#define NATNUM_SPECIFIERP(x) SPECIFIER_TYPEP (x, natnum)
#define CHECK_NATNUM_SPECIFIER(x) CHECK_SPECIFIER_TYPE (x, natnum)
#define CONCHECK_NATNUM_SPECIFIER(x) CONCHECK_SPECIFIER_TYPE (x, natnum)

DECLARE_SPECIFIER_TYPE (boolean);
#define XBOOLEAN_SPECIFIER(x) XSPECIFIER_TYPE (x, boolean)
#define XSETBOOLEAN_SPECIFIER(x, p) XSETSPECIFIER_TYPE (x, p, boolean)
#define BOOLEAN_SPECIFIERP(x) SPECIFIER_TYPEP (x, boolean)
#define CHECK_BOOLEAN_SPECIFIER(x) CHECK_SPECIFIER_TYPE (x, boolean)
#define CONCHECK_BOOLEAN_SPECIFIER(x) CONCHECK_SPECIFIER_TYPE (x, boolean)

DECLARE_SPECIFIER_TYPE (display_table);
#define XDISPLAYTABLE_SPECIFIER(x) XSPECIFIER_TYPE (x, display_table)
#define XSETDISPLAYTABLE_SPECIFIER(x, p) XSETSPECIFIER_TYPE (x, p, display_table)
#define DISPLAYTABLE_SPECIFIERP(x) SPECIFIER_TYPEP (x, display_table)
#define CHECK_DISPLAYTABLE_SPECIFIER(x) CHECK_SPECIFIER_TYPE (x, display_table)
#define CONCHECK_DISPLAYTABLE_SPECIFIER(x) CONCHECK_SPECIFIER_TYPE (x, display_table)

#endif /* _XEMACS_SPECIFIER_H_ */
