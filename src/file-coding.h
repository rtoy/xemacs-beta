/* Header for encoding conversion functions; coding-system object.
   #### rename me to coding-system.h
   Copyright (C) 1991, 1995 Free Software Foundation, Inc.
   Copyright (C) 1995 Sun Microsystems, Inc.
   Copyright (C) 2000, 2001, 2002 Ben Wing.

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

/* Synched up with: Mule 2.3.  Not in FSF. */

/* Authorship:

   Current primary author: Ben Wing <ben@xemacs.org>
   
   Written by Ben Wing <ben@xemacs.org> for XEmacs, 1995, loosely based
     on code written 91.10.09 by K.Handa <handa@etl.go.jp>.
   Rewritten again 2000-2001 by Ben Wing to support properly
   abstracted coding systems.
   September 2001: Finished last part of abstraction, the detection
   mechanism.
*/

#ifndef INCLUDED_file_coding_h_
#define INCLUDED_file_coding_h_

/* Capsule description of the different structures, what their purpose is,
   how they fit together, and where various bits of data are stored.

   A "coding system" is an algorithm for converting data in one format into
   data in another format.  Currently most of the coding systems we have
   created concern internationalized text, and convert between the XEmacs
   internal format for multilingual text, and various external
   representations of such text.  However, any such conversion is possible,
   for example, compressing or uncompressing text using the gzip algorithm.
   All coding systems provide both encode and decode routines, so that the
   conversion can go both ways.

   The way we handle this is by dividing the various potential coding
   systems into types, analogous to classes in C++.  Each coding system
   type encompasses a series of related coding systems that it can
   implement, and it has properties which control how exactly the encoding
   works.  A particular set of values for each of the properties makes up a
   "coding system", and specifies one particular encoding.  A `struct
   Lisp_Coding_System' object encapsulates those settings -- its type, the
   values chosen for all properties of that type, a name for the coding
   system, some documentation.

   In addition, there are of course methods associated with a coding system
   type, implementing the encoding, decoding, etc.  These are stored in a
   `struct coding_system_methods' object, one per coding-system type, which
   contains mostly function pointers.  This is retrievable from the
   coding-system object (i.e. the struct Lisp_Coding_System), which has a
   pointer to it.

   In order to actually use a coding system to do an encoding or decoding
   operation, you need to use a coding Lstream.

   Now let's look more at attached data.  All coding systems have certain
   common data fields -- name, type, documentation, etc. -- as well as a
   bunch more that are defined by the coding system type.  To handle this
   cleanly, each coding system type defines a structure that holds just the
   fields of data particular to it, and calls it e.g. `struct
   iso2022_coding_system' for coding system type `iso2022'.  When the
   memory block holding the coding system object is created, it is sized
   such that it can hold both the struct Lisp_Coding_System and the struct
   iso2022_coding_system (or whatever) directly following it. (This is a
   common trick; another possibility is to have a void * pointer in the
   struct Lisp_Coding_System, which points to another memory block holding
   the struct iso2022_coding_system.) A macro is provided
   (CODING_SYSTEM_TYPE_DATA) to retrieve a pointer of the right type to the
   type-specific data contained within the overall `struct
   Lisp_Coding_System' block.

   Lstreams, similarly, are objects of type `struct lstream' holding data
   about the stream operation (how much data has been read or written, any
   buffered data, any error conditions, etc.), and like coding systems have
   different types.  They have a structure called `Lstream_implementation',
   one per lstream type, exactly analogous to `struct
   coding_system_methods'.  In addition, they have type-specific data
   (specifying, e.g., the file number, FILE *, memory location, other
   lstream, etc. to read the data from or write it to, and for conversion
   processes, the current state of the process -- are we decoding ASCII or
   Kanji characters?  are we in the middle of a processing an escape
   sequence?  etc.).  This type-specific data is stored in a structure
   named `struct coding_stream'.  Just like for coding systems, the
   type-independent data in the `struct lstream' and the type-dependent
   data in the `struct coding_stream' are stored together in the same
   memory block.

   Now things get a bit tricky.  The `struct coding_stream' is
   type-specific from the point of view of an lstream, but not from the
   point of view of a coding system.  It contains only general data about
   the conversion process, e.g. the name of the coding system used for
   conversion, the lstream that we take data from or write it to (depending
   on whether this was created as a read stream or a write stream), a
   buffer to hold extra data we retrieved but can't send on yet, some
   flags, etc.  It also needs some data specific to the particular coding
   system and thus to the particular operation going on.  This data is held
   in a structure named (e.g.) `struct iso2022_coding_stream', and it's
   held in a separate memory block and pointed to by the generic `struct
   coding_stream'.  It's not glommed into a single memory block both
   because that would require making changes to the generic lstream code
   and more importantly because the coding system used in a particular
   coding lstream can be changed at any point during the lifetime of the
   lstream, and possibly multiple times. (For example, it can be set using
   the Lisp primitives `set-process-input-coding-system' and
   `set-console-tty-input-coding-system', as well as getting set when a
   conversion operation was started with coding system `undecided' and the
   correct coding system was then detected.)

   IMPORTANT NOTE: There are at least two ancillary data structures
   associated with a coding system type. (There may also be detection data;
   see elsewhere.) It's important, when writing a coding system type, to
   keep straight which type of data goes where.  In particular, `struct
   foo_coding_system' is attached to the coding system object itself.  This
   is a permanent object and there's only one per coding system.  It's
   created once, usually at init time, and never destroyed.  So, `struct
   foo_coding_system' should in general not contain dynamic data! (Just
   data describing the properties of the coding system.) In particular,
   *NO* data about any conversion in progress.  There may be many
   conversions going on simultaneously using a particular coding system,
   and by storing conversion data in the coding system, these conversions
   will overwrite each other's data.

   Instead, use the lstream object, whose purpose is to encapsulate a
   particular conversion and all associated data.  From the lstream object,
   you can get the struct coding_stream using something like

   struct coding_stream *str = LSTREAM_TYPE_DATA (lstr, coding);

   But usually this structure is already passed to you as one of the
   parameters of the method being invoked.

   From the struct coding_stream, you can retrieve the
   coding-system-type-specific data using something like

   struct foo_coding_stream *data = CODING_STREAM_TYPE_DATA (str, foo);

   Then, use this structure to hold all data relevant to the particular
   conversion being done.

   Initialize this structure whenever init_coding_stream_method is called
   (this may happen more than once), and finalize it (free resources, etc.)
   when finalize_coding_stream_method is called.
    */

struct coding_stream;
struct detection_state;

extern const struct struct_description coding_system_methods_description;

struct coding_system_methods;

enum source_sink_type
{
  DECODES_CHARACTER_TO_BYTE,
  DECODES_BYTE_TO_BYTE,
  DECODES_BYTE_TO_CHARACTER,
  DECODES_CHARACTER_TO_CHARACTER
};

enum eol_type
{
  EOL_LF,
  EOL_CRLF,
  EOL_CR,
  EOL_AUTODETECT,
};

struct Lisp_Coding_System
{
  struct lcrecord_header header;
  struct coding_system_methods *methods;

  /* Name and description of this coding system.  The description
     should be suitable for a menu entry. */
  Lisp_Object name;
  Lisp_Object description;

  /* Mnemonic string displayed in the modeline when this coding
     system is active for a particular buffer. */
  Lisp_Object mnemonic;

  /* Long documentation on the coding system. */
  Lisp_Object documentation;
  /* Functions to handle additional conversion after reading or before
     writing. #### This mechanism should be replaced by the ability to
     simply create new coding system types. */
  Lisp_Object post_read_conversion;
  Lisp_Object pre_write_conversion;

  /* If this coding system is not of the correct type for text file
     conversion (i.e. decodes byte->char), we wrap it with appropriate
     char<->byte converters.  This is created dynamically, when it's
     needed, and cached here. */
  Lisp_Object text_file_wrapper;

  /* If true, this is an internal coding system, which will not show up in
     coding-system-list unless a special parameter is given to it. */
  int internal_p;

  /* ------------------------ junk to handle EOL -------------------------
     I had hoped that we could handle this without lots of special-case
     code, but it appears not to be the case if we want to maintain
     compatibility with the existing way.  However, at least with the way
     we do things now, we avoid EOL junk in most of the coding system
     methods themselves, or in the decode/encode functions.  The EOL
     special-case code is limited to coding-system creation and to the
     convert-eol and undecided coding system types. */

  /* If this coding system wants autodetection of the EOL type, then at the
     appropriate time we wrap this coding system with
     convert-eol-autodetect. (We do NOT do this at creation time because
     then we end up with multiple convert-eols wrapped into the final
     result -- esp. with autodetection using `undecided' -- leading to a
     big mess.) We cache the wrapped coding system here. */
  Lisp_Object auto_eol_wrapper;
  
  /* Eol type requested by user. */
  enum eol_type eol_type;

  /* Subsidiary coding systems that specify a particular type of EOL
     marking, rather than autodetecting it.  These will only be non-nil
     if (eol_type == EOL_AUTODETECT).  These are chains. */
  Lisp_Object eol[3];
  /* If this coding system is a subsidiary, this element points back to its
     parent. */
  Lisp_Object subsidiary_parent;

  /* At decoding or encoding time, we use the following coding system, if
     it exists, in place of the coding system object.  This is how we
     handle coding systems with EOL types of CRLF or CR.  Formerly, we did
     the canonicalization at creation time, returning a chain in place of
     the original coding system; but that interferes with
     `coding-system-property' and causes other complications.  CANONICAL is
     used when determining the end types of a coding system.
     canonicalize-after-coding also consults CANONICAL (it has to, because
     the data in the lstream is based on CANONICAL, not on the original
     coding system). */
  Lisp_Object canonical;
  
  /* type-specific extra data attached to a coding_system */
  char data[1];
};
typedef struct Lisp_Coding_System Lisp_Coding_System;

DECLARE_LRECORD (coding_system, Lisp_Coding_System);
#define XCODING_SYSTEM(x) XRECORD (x, coding_system, Lisp_Coding_System)
#define wrap_coding_system(p) wrap_record (p, coding_system)
#define CODING_SYSTEMP(x) RECORDP (x, coding_system)
#define CHECK_CODING_SYSTEM(x) CHECK_RECORD (x, coding_system)
#define CONCHECK_CODING_SYSTEM(x) CONCHECK_RECORD (x, coding_system)

struct coding_system_methods
{
  Lisp_Object type;
  Lisp_Object predicate_symbol;

  /* Implementation specific methods: */

  /* Init method: Initialize coding-system data.  Optional. */
  void (*init_method) (Lisp_Object coding_system);

  /* Mark method: Mark any Lisp objects in the type-specific data
     attached to the coding-system object.  Optional. */
  void (*mark_method) (Lisp_Object coding_system);

  /* Print method: Print the type-specific properties of this coding
     system, as part of `print'-ing the object.  If this method is defined
     and prints anything, it should print a space as the first thing it
     does.  Optional. */
  void (*print_method) (Lisp_Object cs, Lisp_Object printcharfun,
			int escapeflag);

  /* Canonicalize method: Convert this coding system to another one; called
     once, at creation time, after all properties have been parsed.  The
     returned value should be a coding system created with
     make_internal_coding_system() (passing the existing coding system as the
     first argument), and will become the coding system returned by
     `make-coding-system'. Optional.

     NOTE: There are *three* different uses of "canonical" or "canonicalize"
     w.r.t. coding systems, and it's important to keep them straight.

     1. The canonicalize method.  Used to specify a different coding
        system, used when doing conversions, in place of the actual coding
        system itself.  Stored in the CANONICAL field of a coding system.

     2. The canonicalize-after-coding method.  Used to return the encoding
        that was "actually" used to decode some text, such that this
	particular encoding can be used to encode the text again with the
	expectation that the result will be the same as the original encoding.
	Particularly important with auto-detecting coding systems.

     3. From the perspective of aliases, a "canonical" coding system is one
        that's not an alias to some other coding system, and "canonicalization"
	is the process of traversing the alias pointers to find the canonical
	coding system that's equivalent to the alias.
     */
  Lisp_Object (*canonicalize_method) (Lisp_Object coding_system);

  /* Canonicalize after coding method: Convert this coding system to
     another one, after coding (usually decoding) has finished.  This is
     meant to be used by auto-detecting coding systems, which should return
     the actually detected coding system.  Optional. */
  Lisp_Object (*canonicalize_after_coding_method)
    (struct coding_stream *str);

  /* Convert method: Decode or encode the data in SRC of size N, writing
     the results into the Dynarr DST.  If the conversion_end_type method
     indicates that the source is characters (as opposed to bytes), you are
     guaranteed to get only whole characters in the data in SRC/N.  STR, a
     struct coding_stream, stores all necessary state and other info about
     the conversion.  Coding-specific state (struct TYPE_coding_stream) can
     be retrieved from STR using CODING_STREAM_TYPE_DATA().  Return value
     indicates the number of bytes of the *INPUT* that were converted (not
     the number of bytes written to the Dynarr!).  This can be less than
     the total amount of input passed in; if so, the remainder is
     considered "rejected" and will appear again at the beginning of the
     data passed in the next time the convert method is called.  When EOF
     is returned on the other end and there's no more data, the convert
     method will be called one last time, STR->eof set and the passed-in
     data will consist only of any rejected data from the previous
     call. (At this point, file handles and similar resources can be
     closed, but do NOT arbitrarily free data structures in the
     type-specific data, because there are operations that can be done on
     closed streams to query the results of the processing -- specifically,
     for coding streams, there's the canonicalize_after_coding() method.)
     Required. */
  Bytecount (*convert_method) (struct coding_stream *str,
				  const unsigned char *src,
				  unsigned_char_dynarr *dst, Bytecount n);

  /* Coding mark method: Mark any Lisp objects in the type-specific data
     attached to `struct coding_stream'.  Optional. */
  void (*mark_coding_stream_method) (struct coding_stream *str);

  /* Init coding stream method: Initialize the type-specific data attached
     to the coding stream (i.e. in struct TYPE_coding_stream), when the
     coding stream is opened.  The type-specific data will be zeroed out.
     Optional. */
  void (*init_coding_stream_method) (struct coding_stream *str);

  /* Rewind coding stream method: Reset any necessary type-specific data as
     a result of the stream being rewound.  Optional. */
  void (*rewind_coding_stream_method) (struct coding_stream *str);

  /* Finalize coding stream method: Clean up the type-specific data
     attached to the coding stream (i.e. in struct TYPE_coding_stream).
     Happens when the Lstream is deleted using Lstream_delete() or is
     garbage-collected.  Most streams are deleted after they've been used,
     so it's less likely (but still possible) that allocated data will
     stick around until GC time. (File handles can also be closed when EOF
     is signalled; but some data must stick around after this point, for
     the benefit of canonicalize_after_coding.  See the convert method.)
     Called only once (NOT called at disksave time).  Optional. */
  void (*finalize_coding_stream_method) (struct coding_stream *str);

  /* Finalize method: Clean up type-specific data (e.g. free allocated
     data) attached to the coding system (i.e. in struct
     TYPE_coding_system), when the coding system is about to be garbage
     collected. (Currently not called.) Called only once (NOT called at
     disksave time).  Optional. */
  void (*finalize_method) (Lisp_Object codesys);

  /* Conversion end type method: Does this coding system encode bytes ->
     characters, characters -> characters, bytes -> bytes, or
     characters -> bytes?.  Default is characters -> bytes.  Optional. */
  enum source_sink_type (*conversion_end_type_method) (Lisp_Object codesys);

  /* Putprop method: Set the value of a type-specific property.  If
     the property name is unrecognized, return 0.  If the value is disallowed
     or erroneous, signal an error.  Currently called only at creation time.
     Optional. */
  int (*putprop_method) (Lisp_Object codesys,
			 Lisp_Object key,
			 Lisp_Object value);

  /* Getprop method: Return the value of a type-specific property.  If
     the property name is unrecognized, return Qunbound.  Optional.
   */
  Lisp_Object (*getprop_method) (Lisp_Object coding_system,
				 Lisp_Object prop);

  /* These next three are set as part of the call to
     INITIALIZE_CODING_SYSTEM_TYPE_WITH_DATA. */

  /* Description of the extra data (struct foo_coding_system) attached to a
     coding system, for pdump purposes.  NOTE: All offsets must have
     coding_system_data_offset added to them! */
  const struct lrecord_description *extra_description;
  /* size of struct foo_coding_system -- extra data associated with
     the coding system */
  int extra_data_size;
  /* size of struct foo_coding_stream -- extra data associated with the
     struct coding_stream, needed for each active coding process
     using this coding system.  note that we can have more than one
     process active at once (simply by creating more than one coding
     lstream using this coding system), so we can't store this data in
     the coding system object. */
  int coding_data_size;
};

/***** Calling a coding-system method *****/

#define RAW_CODESYSMETH(cs, m) ((cs)->methods->m##_method)
#define HAS_CODESYSMETH_P(cs, m) (!!RAW_CODESYSMETH (cs, m))
#define CODESYSMETH(cs, m, args) (((cs)->methods->m##_method) args)

/* Call a void-returning coding-system method, if it exists.  */
#define MAYBE_CODESYSMETH(cs, m, args) do {	\
  Lisp_Coding_System *maybe_codesysmeth_cs = (cs);	\
  if (HAS_CODESYSMETH_P (maybe_codesysmeth_cs, m))	\
    CODESYSMETH (maybe_codesysmeth_cs, m, args);	\
} while (0)

/* Call a coding-system method, if it exists, or return GIVEN.
   NOTE: Multiply-evaluates CS. */
#define CODESYSMETH_OR_GIVEN(cs, m, args, given)	\
  (HAS_CODESYSMETH_P (cs, m) ?				\
   CODESYSMETH (cs, m, args) : (given))

#define XCODESYSMETH(cs, m, args) \
  CODESYSMETH (XCODING_SYSTEM (cs), m, args)
#define MAYBE_XCODESYSMETH(cs, m, args) \
  MAYBE_CODESYSMETH (XCODING_SYSTEM (cs), m, args)
#define XCODESYSMETH_OR_GIVEN(cs, m, args, given) \
  CODESYSMETH_OR_GIVEN (XCODING_SYSTEM (cs), m, args, given)


/***** Defining new coding-system types *****/

#define coding_system_data_offset (offsetof (Lisp_Coding_System, data))
extern const struct lrecord_description coding_system_empty_extra_description[];

#ifdef ERROR_CHECK_TYPES
#define DECLARE_CODING_SYSTEM_TYPE(type)				\
									\
extern struct coding_system_methods * type##_coding_system_methods;	\
DECLARE_INLINE_HEADER (							\
struct type##_coding_system *						\
error_check_##type##_coding_system_data (Lisp_Coding_System *cs)	\
)									\
{									\
  assert (CODING_SYSTEM_TYPE_P (cs, type));				\
  /* Catch accidental use of INITIALIZE_CODING_SYSTEM_TYPE in place	\
     of INITIALIZE_CODING_SYSTEM_TYPE_WITH_DATA. */			\
  assert (cs->methods->extra_data_size > 0);				\
  return (struct type##_coding_system *) cs->data;			\
}									\
									\
DECLARE_INLINE_HEADER (							\
struct type##_coding_stream *						\
error_check_##type##_coding_stream_data (struct coding_stream *s)	\
)									\
{									\
  assert (XCODING_SYSTEM_TYPE_P (s->codesys, type));			\
  return (struct type##_coding_stream *) s->data;			\
}									\
									\
DECLARE_INLINE_HEADER (							\
Lisp_Coding_System *							\
error_check_##type##_coding_system_type (Lisp_Object obj)		\
)									\
{									\
  Lisp_Coding_System *cs = XCODING_SYSTEM (obj);			\
  assert (CODING_SYSTEM_TYPE_P (cs, type));				\
  return cs;								\
}									\
									\
DECLARE_NOTHING
#else
#define DECLARE_CODING_SYSTEM_TYPE(type)				\
extern struct coding_system_methods * type##_coding_system_methods
#endif /* ERROR_CHECK_TYPES */

#define DEFINE_CODING_SYSTEM_TYPE(type)					\
struct coding_system_methods * type##_coding_system_methods

#define INITIALIZE_CODING_SYSTEM_TYPE(ty, pred_sym) do {		\
  ty##_coding_system_methods =						\
    xnew_and_zero (struct coding_system_methods);			\
  ty##_coding_system_methods->type = Q##ty;				\
  ty##_coding_system_methods->extra_description =			\
    coding_system_empty_extra_description;				\
  defsymbol_nodump (&ty##_coding_system_methods->predicate_symbol,	\
                    pred_sym);						\
  add_entry_to_coding_system_type_list (ty##_coding_system_methods);	\
  dump_add_root_struct_ptr (&ty##_coding_system_methods,		\
                            &coding_system_methods_description);	\
} while (0)

#define REINITIALIZE_CODING_SYSTEM_TYPE(type) do {			\
  staticpro_nodump (&type##_coding_system_methods->predicate_symbol);	\
} while (0)

/* This assumes the existence of two structures:

   struct foo_coding_system (attached to the coding system)
   struct foo_coding_stream (per coding process, attached to the
                             struct coding_stream)
   const struct foo_coding_system_description[] (pdump description of
                                                 struct foo_coding_system)

   NOTE: The description must have coding_system_data_offset added to
   all offsets in it!  For an example of how to do things, see
   chain_coding_system_description.
*/
#define INITIALIZE_CODING_SYSTEM_TYPE_WITH_DATA(type, pred_sym)	\
do {								\
  INITIALIZE_CODING_SYSTEM_TYPE (type, pred_sym);		\
  type##_coding_system_methods->extra_data_size =		\
    sizeof (struct type##_coding_system);			\
  type##_coding_system_methods->extra_description =		\
    type##_coding_system_description;				\
  type##_coding_system_methods->coding_data_size =		\
    sizeof (struct type##_coding_stream);			\
} while (0)

/* Declare that coding-system-type TYPE has method METH; used in
   initialization routines */
#define CODING_SYSTEM_HAS_METHOD(type, meth) \
  (type##_coding_system_methods->meth##_method = type##_##meth)

/***** Macros for accessing coding-system types *****/

#define CODING_SYSTEM_TYPE_P(cs, type) \
  ((cs)->methods == type##_coding_system_methods)
#define XCODING_SYSTEM_TYPE_P(cs, type) \
  CODING_SYSTEM_TYPE_P (XCODING_SYSTEM (cs), type)

#ifdef ERROR_CHECK_TYPES
# define CODING_SYSTEM_TYPE_DATA(cs, type) \
   error_check_##type##_coding_system_data (cs)
#else
# define CODING_SYSTEM_TYPE_DATA(cs, type)		\
  ((struct type##_coding_system *)			\
    (cs)->data)
#endif

#define XCODING_SYSTEM_TYPE_DATA(cs, type) \
  CODING_SYSTEM_TYPE_DATA (XCODING_SYSTEM_OF_TYPE (cs, type), type)

#ifdef ERROR_CHECK_TYPES
# define XCODING_SYSTEM_OF_TYPE(x, type)	\
   error_check_##type##_coding_system_type (x)
# define XSETCODING_SYSTEM_OF_TYPE(x, p, type)	do		\
{								\
  x = wrap_coding_system (p);					\
  assert (CODING_SYSTEM_TYPEP (XCODING_SYSTEM (x), type));	\
} while (0)
#else
# define XCODING_SYSTEM_OF_TYPE(x, type) XCODING_SYSTEM (x)
# define XSETCODING_SYSTEM_OF_TYPE(x, p, type) do	\
{							\
  x = wrap_coding_system (p);				\
} while (0)
#endif /* ERROR_CHECK_TYPE_CHECK */

#define CODING_SYSTEM_TYPEP(x, type)					  \
  (CODING_SYSTEMP (x) && CODING_SYSTEM_TYPE_P (XCODING_SYSTEM (x), type))
#define CHECK_CODING_SYSTEM_OF_TYPE(x, type) do {			\
  CHECK_CODING_SYSTEM (x);					\
  if (!CODING_SYSTEM_TYPE_P (XCODING_SYSTEM (x), type))		\
    dead_wrong_type_argument					\
      (type##_coding_system_methods->predicate_symbol, x);	\
} while (0)
#define CONCHECK_CODING_SYSTEM_OF_TYPE(x, type) do {		\
  CONCHECK_CODING_SYSTEM (x);					\
  if (!(CODING_SYSTEM_TYPEP (x, type)))				\
    x = wrong_type_argument					\
      (type##_coding_system_methods->predicate_symbol, x);	\
} while (0)

#define CODING_SYSTEM_METHODS(codesys) ((codesys)->methods)
#define CODING_SYSTEM_NAME(codesys) ((codesys)->name)
#define CODING_SYSTEM_DESCRIPTION(codesys) ((codesys)->description)
#define CODING_SYSTEM_TYPE(codesys) ((codesys)->methods->type)
#define CODING_SYSTEM_MNEMONIC(codesys) ((codesys)->mnemonic)
#define CODING_SYSTEM_DOCUMENTATION(codesys) ((codesys)->documentation)
#define CODING_SYSTEM_POST_READ_CONVERSION(codesys) \
  ((codesys)->post_read_conversion)
#define CODING_SYSTEM_PRE_WRITE_CONVERSION(codesys) \
  ((codesys)->pre_write_conversion)
#define CODING_SYSTEM_EOL_TYPE(codesys) ((codesys)->eol_type)
#define CODING_SYSTEM_EOL_LF(codesys)   ((codesys)->eol[EOL_LF])
#define CODING_SYSTEM_EOL_CRLF(codesys) ((codesys)->eol[EOL_CRLF])
#define CODING_SYSTEM_EOL_CR(codesys)   ((codesys)->eol[EOL_CR])
#define CODING_SYSTEM_TEXT_FILE_WRAPPER(codesys) ((codesys)->text_file_wrapper)
#define CODING_SYSTEM_AUTO_EOL_WRAPPER(codesys) ((codesys)->auto_eol_wrapper)
#define CODING_SYSTEM_SUBSIDIARY_PARENT(codesys) ((codesys)->subsidiary_parent)
#define CODING_SYSTEM_CANONICAL(codesys) ((codesys)->canonical)

#define CODING_SYSTEM_CHAIN_CHAIN(codesys) \
  (CODING_SYSTEM_TYPE_DATA (codesys, chain)->chain)
#define CODING_SYSTEM_CHAIN_COUNT(codesys) \
  (CODING_SYSTEM_TYPE_DATA (codesys, chain)->count)
#define CODING_SYSTEM_CHAIN_CANONICALIZE_AFTER_CODING(codesys) \
  (CODING_SYSTEM_TYPE_DATA (codesys, chain)->canonicalize_after_coding)

#define XCODING_SYSTEM_METHODS(codesys) \
  CODING_SYSTEM_METHODS (XCODING_SYSTEM (codesys))
#define XCODING_SYSTEM_NAME(codesys) \
  CODING_SYSTEM_NAME (XCODING_SYSTEM (codesys))
#define XCODING_SYSTEM_DESCRIPTION(codesys) \
  CODING_SYSTEM_DESCRIPTION (XCODING_SYSTEM (codesys))
#define XCODING_SYSTEM_TYPE(codesys) \
  CODING_SYSTEM_TYPE (XCODING_SYSTEM (codesys))
#define XCODING_SYSTEM_MNEMONIC(codesys) \
  CODING_SYSTEM_MNEMONIC (XCODING_SYSTEM (codesys))
#define XCODING_SYSTEM_DOCUMENTATION(codesys) \
  CODING_SYSTEM_DOCUMENTATION (XCODING_SYSTEM (codesys))
#define XCODING_SYSTEM_POST_READ_CONVERSION(codesys) \
  CODING_SYSTEM_POST_READ_CONVERSION (XCODING_SYSTEM (codesys))
#define XCODING_SYSTEM_PRE_WRITE_CONVERSION(codesys) \
  CODING_SYSTEM_PRE_WRITE_CONVERSION (XCODING_SYSTEM (codesys))
#define XCODING_SYSTEM_EOL_TYPE(codesys) \
  CODING_SYSTEM_EOL_TYPE (XCODING_SYSTEM (codesys))
#define XCODING_SYSTEM_EOL_LF(codesys) \
  CODING_SYSTEM_EOL_LF (XCODING_SYSTEM (codesys))
#define XCODING_SYSTEM_EOL_CRLF(codesys) \
  CODING_SYSTEM_EOL_CRLF (XCODING_SYSTEM (codesys))
#define XCODING_SYSTEM_EOL_CR(codesys) \
  CODING_SYSTEM_EOL_CR (XCODING_SYSTEM (codesys))
#define XCODING_SYSTEM_TEXT_FILE_WRAPPER(codesys) \
  CODING_SYSTEM_TEXT_FILE_WRAPPER (XCODING_SYSTEM (codesys))
#define XCODING_SYSTEM_AUTO_EOL_WRAPPER(codesys) \
  CODING_SYSTEM_AUTO_EOL_WRAPPER (XCODING_SYSTEM (codesys))
#define XCODING_SYSTEM_SUBSIDIARY_PARENT(codesys) \
  CODING_SYSTEM_SUBSIDIARY_PARENT (XCODING_SYSTEM (codesys))
#define XCODING_SYSTEM_CANONICAL(codesys) \
  CODING_SYSTEM_CANONICAL (XCODING_SYSTEM (codesys))

#define XCODING_SYSTEM_CHAIN_CHAIN(codesys) \
  CODING_SYSTEM_CHAIN_CHAIN (XCODING_SYSTEM (codesys))
#define XCODING_SYSTEM_CHAIN_COUNT(codesys) \
  CODING_SYSTEM_CHAIN_COUNT (XCODING_SYSTEM (codesys))
#define XCODING_SYSTEM_CHAIN_CANONICALIZE_AFTER_CODING(codesys) \
  CODING_SYSTEM_CHAIN_CANONICALIZE_AFTER_CODING (XCODING_SYSTEM (codesys))

/**************************************************/
/*                   Detection                    */
/**************************************************/

#define MAX_DETECTOR_CATEGORIES 256
#define MAX_DETECTORS 64

#define MAX_BYTES_PROCESSED_FOR_DETECTION 65536

struct detection_state
{
  int seen_non_ascii;
  Bytecount bytes_seen;

  char categories[MAX_DETECTOR_CATEGORIES];
  Bytecount data_offset[MAX_DETECTORS];
  /* ... more data follows; data_offset[detector_##TYPE] points to
     the data for that type */
};

#define DETECTION_STATE_DATA(st, type)				\
  ((struct type##_detector *)					\
   ((char *) (st) + (st)->data_offset[detector_##type]))

/* Distinguishable categories of encodings.

   This list determines the initial priority of the categories.

   For better or worse, currently Mule files are encoded in 7-bit ISO 2022.
   For this reason, under Mule ISO_7 gets highest priority.

   Putting NO_CONVERSION second prevents "binary corruption" in the
   default case in all but the (presumably) extremely rare case of a
   binary file which contains redundant escape sequences but no 8-bit
   characters.

   The remaining priorities are based on perceived "internationalization
   political correctness."  An exception is UCS-4 at the bottom, since
   basically everything is compatible with UCS-4, but it is likely to
   be very rare as an external encoding. */

/* Macros to define code of control characters for ISO2022's functions.  */
/* Used by the detection routines of other coding system types as well. */
			/* code */	/* function */
#define ISO_CODE_LF	0x0A		/* line-feed */
#define ISO_CODE_CR	0x0D		/* carriage-return */
#define ISO_CODE_SO	0x0E		/* shift-out */
#define ISO_CODE_SI	0x0F		/* shift-in */
#define ISO_CODE_ESC	0x1B		/* escape */
#define ISO_CODE_DEL	0x7F		/* delete */
#define ISO_CODE_SS2	0x8E		/* single-shift-2 */
#define ISO_CODE_SS3	0x8F		/* single-shift-3 */
#define ISO_CODE_CSI	0x9B		/* control-sequence-introduce */

enum detection_result
 {
  /* Basically means a magic cookie was seen indicating this type, or
     something similar. */
  DET_NEAR_CERTAINTY = 4,
  DET_HIGHEST = 4,
  /* Characteristics seen that are unlikely to be other coding system types
     -- e.g. ISO-2022 escape sequences, or perhaps a consistent pattern of
     alternating zero bytes in UTF-16, along with Unicode LF or CRLF
     sequences at regular intervals. (Zero bytes are unlikely or impossible
     in most text encodings.) */
  DET_QUITE_PROBABLE = 3,
  /* Strong or medium statistical likelihood.  At least some
     characteristics seen that match what's normally found in this encoding
     -- e.g. in Shift-JIS, a number of two-byte Japanese character
     sequences in the right range, and nothing out of range; or in Unicode,
     much higher statistical variance in the odd bytes than in the even
     bytes, or vice-versa (perhaps the presence of regular EOL sequences
     would bump this too to DET_QUITE_PROBABLE).  This is quite often a
     statistical test. */
  DET_SOMEWHAT_LIKELY = 2,
  /* Weak statistical likelihood.  Pretty much any features at all that
     characterize this encoding, and nothing that rules against it. */
  DET_SLIGHTLY_LIKELY = 1,
  /* Default state.  Perhaps it indicates pure ASCII or something similarly
     vague seen in Shift-JIS, or, exactly as the level says, it might mean
     in a statistical-based detector that the pros and cons are balanced
     out.  This is also the lowest level that will be accepted by the
     auto-detector without asking the user: If all available detectors
     report lower levels for all categories with attached coding systems,
     the user will be shown the results and explicitly prompted for action.
     The user will also be prompted if this is the highest available level
     and more than one detector reports the level. (See below about the
     consequent necessity of an "ASCII" detector, which will return level 1
     or higher for most plain text files.) */
  DET_AS_LIKELY_AS_UNLIKELY = 0,
  /* Some characteristics seen that are unusual for this encoding --
     e.g. unusual control characters in a plain-text encoding, lots of
     8-bit characters, or little statistical variance in the odd and even
     bytes in UTF-16. */
  DET_SOMEWHAT_UNLIKELY = -1,
  /* This indicates that there is very little chance the data is in the
     right format; this is probably the lowest level you can get when
     presenting random binary data to a text file, because there are no
     "specific sequences" you can see that would totally rule out
     recognition. */
  DET_QUITE_IMPROBABLE = -2,
  /* An erroneous sequence was seen. */
  DET_NEARLY_IMPOSSIBLE = -3,
  DET_LOWEST = 3,
 };

extern int coding_detector_count;
extern int coding_detector_category_count;

struct detector_category
{
  int id;
  Lisp_Object sym;
};

typedef struct
{
  Dynarr_declare (struct detector_category);
} detector_category_dynarr;

struct detector
{
  int id;
  detector_category_dynarr *cats;
  Bytecount data_size;
  /* Detect method: Required. */
  void (*detect_method) (struct detection_state *st,
			 const unsigned char *src, Bytecount n);
  /* Finalize detection state method: Clean up any allocated data in the
     detection state.  Called only once (NOT called at disksave time).
     Optional. */
  void (*finalize_detection_state_method) (struct detection_state *st);
};

/* Lvalue for a particular detection result -- detection state ST,
   category CAT */
#define DET_RESULT(st, cat) ((st)->categories[detector_category_##cat])
/* In state ST, set all detection results associated with detector DET to
   RESULT. */
#define SET_DET_RESULTS(st, det, result) \
  set_detection_results (st, detector_##det, result)

typedef struct
{
  Dynarr_declare (struct detector);
} detector_dynarr;

extern detector_dynarr *all_coding_detectors;

#define DEFINE_DETECTOR_CATEGORY(detector, cat) \
int detector_category_##cat
#define DECLARE_DETECTOR_CATEGORY(detector, cat) \
extern int detector_category_##cat
#define INITIALIZE_DETECTOR_CATEGORY(detector, cat)			  \
do {									  \
  struct detector_category dog;						  \
  xzero (dog);								  \
  detector_category_##cat = coding_detector_category_count++;		  \
  dump_add_opaque_int (&detector_category_##cat);			  \
  dog.id = detector_category_##cat;					  \
  dog.sym = Q##cat;							  \
  Dynarr_add (Dynarr_at (all_coding_detectors, detector_##detector).cats, \
	      dog);							  \
} while (0)

#define DEFINE_DETECTOR(Detector) \
int detector_##Detector
#define DECLARE_DETECTOR(Detector) \
extern int detector_##Detector
#define INITIALIZE_DETECTOR(Detector)			\
do {							\
  struct detector det;					\
  xzero (det);						\
  detector_##Detector = coding_detector_count++;	\
  dump_add_opaque_int (&detector_##Detector);		\
  det.id = detector_##Detector;				\
  det.cats = Dynarr_new2 (detector_category_dynarr,	\
			  struct detector_category);	\
  det.data_size = sizeof (struct Detector##_detector);	\
  Dynarr_add (all_coding_detectors, det);		\
} while (0)
#define DETECTOR_HAS_METHOD(Detector, Meth)				\
  Dynarr_at (all_coding_detectors, detector_##Detector).Meth##_method =	\
    Detector##_##Meth
  

/**************************************************/
/*               Decoding/Encoding                */
/**************************************************/

/* Is the source (SOURCEP == 1) or sink (SOURCEP == 0) when encoding specified
   in characters? */

enum source_or_sink
{
  CODING_SOURCE,
  CODING_SINK
};

enum encode_decode
{
  CODING_ENCODE,
  CODING_DECODE
};

/* Data structure attached to an lstream of type `coding',
   containing values specific to the coding process.  Additional
   data is stored in the DATA field below; the exact form of that data
   is controlled by the type of the coding system that governs the
   conversion (field CODESYS).  CODESYS may be set at any time
   throughout the lifetime of the lstream and possibly more than once.
   See long comment above for more info. */

struct coding_stream
{
  /* Coding system that governs the conversion. */
  Lisp_Object codesys;
  /* Original coding system, pre-canonicalization. */
  Lisp_Object orig_codesys;

  /* Back pointer to current stream. */
  Lstream *us;

  /* Stream that we read the unprocessed data from or write the processed
     data to. */
  Lstream *other_end;

  /* In order to handle both reading to and writing from a coding stream,
     we phrase the conversion methods like write methods -- we can
     implement reading in terms of a write method but not vice-versa,
     because the write method is forced to take only what it's given but
     the read method can read more data from the other end if necessary.
     On the other hand, the write method is free to generate all the data
     it wants (and just write it to the other end), but the the read method
     can return only as much as was asked for, so we need to implement our
     own buffering. */

  /* If we are reading, then we can return only a fixed amount of data, but
     the converter is free to return as much as it wants, so we direct it
     to store the data here and lop off chunks as we need them.  If we are
     writing, we use this because the converter takes a Dynarr but we are
     supposed to write into a fixed buffer. (NOTE: This introduces an extra
     memory copy.) */
  unsigned_char_dynarr *convert_to;

  /* The conversion method might reject some of the data -- this typically
     includes partial characters, partial escape sequences, etc.  When
     writing, we just pass the rejection up to the Lstream module, and it
     will buffer the data.  When reading, however, we need to do the
     buffering ourselves, and we put it here, combined with newly read
     data. */
  unsigned_char_dynarr *convert_from;

  /* If set, this is the last chunk of data being processed.  When this is
     finished, output any necessary terminating control characters, escape
     sequences, etc. */
  unsigned int eof:1;
  
  /* CH holds a partially built-up character.  This is really part of the
     state-dependent data and should be moved there. */
  unsigned int ch;

  /* Coding-system-specific data holding extra state about the
     conversion.  Logically a struct TYPE_coding_stream; a pointer
     to such a struct, with (when ERROR_CHECK_TYPES is defined)
     error-checking that this is really a structure of that type
     (checking the corresponding coding system type) can be retrieved using
     CODING_STREAM_TYPE_DATA().  Allocated at the same time that
     CODESYS is set (which may occur at any time, even multiple times,
     during the lifetime of the stream).  The size comes from
     methods->coding_data_size.  */
  void *data;

  enum encode_decode direction;

  /* If set, don't close the stream at the other end when being closed. */
  unsigned int no_close_other:1;
  /* If set, read only one byte at a time from other end to avoid any
     possible blocking. */
  unsigned int one_byte_at_a_time:1;
  /* If set, and we're a read stream, we init char mode on ourselves as
     necessary to prevent the caller from getting partial characters. (the
     default) */
  unsigned int set_char_mode_on_us_when_reading:1;
  
  /* #### Temporary test */
  unsigned int finalized:1;
};

#define CODING_STREAM_DATA(stream) LSTREAM_TYPE_DATA (stream, coding)    

#ifdef ERROR_CHECK_TYPES
# define CODING_STREAM_TYPE_DATA(s, type) \
   error_check_##type##_coding_stream_data (s)
#else
# define CODING_STREAM_TYPE_DATA(s, type) \
  ((struct type##_coding_stream *) (s)->data)
#endif

/* C should be a binary character in the range 0 - 255; convert
   to internal format and add to Dynarr DST. */

#ifdef MULE

#define DECODE_ADD_BINARY_CHAR(c, dst)			\
do {							\
  if (byte_ascii_p (c))					\
    Dynarr_add (dst, c);				\
  else if (byte_c1_p (c))				\
    {							\
      Dynarr_add (dst, LEADING_BYTE_CONTROL_1);		\
      Dynarr_add (dst, c + 0x20);			\
    }							\
  else							\
    {							\
      Dynarr_add (dst, LEADING_BYTE_LATIN_ISO8859_1);	\
      Dynarr_add (dst, c);				\
    }							\
} while (0)

#else /* not MULE */

#define DECODE_ADD_BINARY_CHAR(c, dst)		\
do {						\
  Dynarr_add (dst, c);				\
} while (0)

#endif /* MULE */

#define DECODE_OUTPUT_PARTIAL_CHAR(ch, dst)	\
do {						\
  if (ch)					\
    {						\
      DECODE_ADD_BINARY_CHAR (ch, dst);		\
      ch = 0;					\
    }						\
} while (0)

#ifdef MULE
/* Convert shift-JIS code (sj1, sj2) into internal string
   representation (c1, c2). (The leading byte is assumed.) */

#define DECODE_SHIFT_JIS(sj1, sj2, c1, c2)		\
do {							\
  int I1 = sj1, I2 = sj2;				\
  if (I2 >= 0x9f)					\
    c1 = (I1 << 1) - ((I1 >= 0xe0) ? 0xe0 : 0x60),	\
    c2 = I2 + 2;					\
  else							\
    c1 = (I1 << 1) - ((I1 >= 0xe0) ? 0xe1 : 0x61),	\
    c2 = I2 + ((I2 >= 0x7f) ? 0x60 : 0x61);		\
} while (0)

/* Convert the internal string representation of a Shift-JIS character
   (c1, c2) into Shift-JIS code (sj1, sj2).  The leading byte is
   assumed. */

#define ENCODE_SHIFT_JIS(c1, c2, sj1, sj2)		\
do {							\
  int I1 = c1, I2 = c2;					\
  if (I1 & 1)						\
    sj1 = (I1 >> 1) + ((I1 < 0xdf) ? 0x31 : 0x71),	\
    sj2 = I2 - ((I2 >= 0xe0) ? 0x60 : 0x61);		\
  else							\
    sj1 = (I1 >> 1) + ((I1 < 0xdf) ? 0x30 : 0x70),	\
    sj2 = I2 - 2;					\
} while (0)
#endif /* MULE */

DECLARE_CODING_SYSTEM_TYPE (no_conversion);
DECLARE_CODING_SYSTEM_TYPE (convert_eol);
#if 0
DECLARE_CODING_SYSTEM_TYPE (text_file_wrapper);
#endif /* 0 */
DECLARE_CODING_SYSTEM_TYPE (undecided);
DECLARE_CODING_SYSTEM_TYPE (chain);

#ifdef DEBUG_XEMACS
DECLARE_CODING_SYSTEM_TYPE (internal);
#endif

#ifdef MULE
DECLARE_CODING_SYSTEM_TYPE (iso2022);
DECLARE_CODING_SYSTEM_TYPE (ccl);
DECLARE_CODING_SYSTEM_TYPE (shift_jis);
DECLARE_CODING_SYSTEM_TYPE (big5);
#endif

#ifdef HAVE_ZLIB
DECLARE_CODING_SYSTEM_TYPE (gzip);
#endif

DECLARE_CODING_SYSTEM_TYPE (unicode);

#ifdef HAVE_WIN32_CODING_SYSTEMS
DECLARE_CODING_SYSTEM_TYPE (mswindows_multibyte_to_unicode);
DECLARE_CODING_SYSTEM_TYPE (mswindows_multibyte);
#endif

Lisp_Object coding_stream_detected_coding_system (Lstream *stream);
Lisp_Object coding_stream_coding_system (Lstream *stream);
void set_coding_stream_coding_system (Lstream *stream,
				      Lisp_Object codesys);
Lisp_Object detect_coding_stream (Lisp_Object stream);
Ichar decode_big5_char (int o1, int o2);
void add_entry_to_coding_system_type_list (struct coding_system_methods *m);
Lisp_Object make_internal_coding_system (Lisp_Object existing,
					 Char_ASCII *prefix,
					 Lisp_Object type,
					 Lisp_Object description,
					 Lisp_Object props);

#define LSTREAM_FL_NO_CLOSE_OTHER	(1 << 16)
#define LSTREAM_FL_READ_ONE_BYTE_AT_A_TIME (1 << 17)
#define LSTREAM_FL_NO_INIT_CHAR_MODE_WHEN_READING (1 << 18)

Lisp_Object make_coding_input_stream (Lstream *stream, Lisp_Object codesys,
				      enum encode_decode direction,
				      int flags);
Lisp_Object make_coding_output_stream (Lstream *stream, Lisp_Object codesys,
				       enum encode_decode direction,
				       int flags);
void set_detection_results (struct detection_state *st, int detector,
			    int given);

#endif /* INCLUDED_file_coding_h_ */

