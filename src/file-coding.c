/* Text encoding conversion functions; coding-system object.
   #### rename me to coding-system.c or coding.c
   Copyright (C) 1991, 1995 Free Software Foundation, Inc.
   Copyright (C) 1995 Sun Microsystems, Inc.
   Copyright (C) 2000, 2001, 2002, 2003 Ben Wing.

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

/* Authorship:

   Current primary author: Ben Wing <ben@xemacs.org>
   
   Rewritten by Ben Wing <ben@xemacs.org>, based originally on coding.c
   from Mule 2.? but probably does not share one line of code with that
   original source.  Rewriting work started around Dec. 1994. or Jan. 1995.
   Proceeded in earnest till Nov. 1995.

   Around Feb. 17, 1998, Andy Piper renamed what was then mule-coding.c to
   file-coding.c, with the intention of using it to do end-of-line conversion
   on non-MULE machines (specifically, on Windows machines).  He separated
   out the MULE stuff from non-MULE using ifdef's, and searched throughout
   the rest of the source tree looking for coding-system-related code that
   was ifdef MULE but should be ifdef HAVE_CODING_SYSTEMS.

   Sept. 4 - 8, 1998, Tomohiko Morioka added the UCS_4 and UTF_8 coding system
   types, providing a primitive means of decoding and encoding externally-
   formatted Unicode/UCS_4 and Unicode/UTF_8 data.

   January 25, 2000, Martin Buchholz redid and fleshed out the coding
   system alias handling that was first added in prototype form by
   Hrjove Niksic, April 15, 1999.

   April to May 2000, Ben Wing: More major reorganization.  Adding features
   needed for MS Windows (multibyte, unicode, unicode-to-multibyte), the
   "chain" coding system for chaining two together, and doing a lot of
   reorganization in preparation for properly abstracting out the different
   coding system types.

   June 2001, Ben Wing: Added Unicode support.  Eliminated previous
   junky Unicode translation support.

   August 2001, Ben Wing: Moved Unicode support to unicode.c.  Finished
   abstracting everything except detection, which is hard to abstract (see
   just below).

   September 2001, Ben Wing: Moved Mule code to mule-coding.c, Windows code
   to intl-win32.c.  Lots more rewriting; very little code is untouched
   from before April 2000.  Abstracted the detection code, added multiple
   levels of likelihood to increase the reliability of the algorithm.

   October 2001, Ben Wing: HAVE_CODING_SYSTEMS is always now defined.
   Removed the conditionals.
   */

#include <config.h>
#include "lisp.h"

#include "buffer.h"
#include "elhash.h"
#include "insdel.h"
#include "lstream.h"
#include "opaque.h"
#include "file-coding.h"

#ifdef HAVE_ZLIB
#include "zlib.h"
#endif

Lisp_Object Vkeyboard_coding_system;
Lisp_Object Vterminal_coding_system;
Lisp_Object Vcoding_system_for_read;
Lisp_Object Vcoding_system_for_write;
Lisp_Object Vfile_name_coding_system;

#ifdef DEBUG_XEMACS
Lisp_Object Vdebug_coding_detection;
#endif

typedef struct coding_system_type_entry
{
  struct coding_system_methods *meths;
} coding_system_type_entry;

typedef struct
{
  Dynarr_declare (coding_system_type_entry);
} coding_system_type_entry_dynarr;

static coding_system_type_entry_dynarr *the_coding_system_type_entry_dynarr;

static const struct memory_description cste_description_1[] = {
  { XD_BLOCK_PTR,  offsetof (coding_system_type_entry, meths), 1,
    { &coding_system_methods_description } },
  { XD_END }
};

static const struct sized_memory_description cste_description = {
  sizeof (coding_system_type_entry),
  cste_description_1
};

static const struct memory_description csted_description_1[] = {
  XD_DYNARR_DESC (coding_system_type_entry_dynarr, &cste_description),
  { XD_END }
};

static const struct sized_memory_description csted_description = {
  sizeof (coding_system_type_entry_dynarr),
  csted_description_1
};

static Lisp_Object Vcoding_system_type_list;

/* Coding system currently associated with each coding category. */
Lisp_Object coding_category_system[MAX_DETECTOR_CATEGORIES];

/* Table of all coding categories in decreasing order of priority.
   This describes a permutation of the possible coding categories. */
int coding_category_by_priority[MAX_DETECTOR_CATEGORIES];

/* Value used with to give a unique name to nameless coding systems */
int coding_system_tick;

int coding_detector_count;
int coding_detector_category_count;

detector_dynarr *all_coding_detectors;

static const struct memory_description struct_detector_category_description_1[]
=
{
  { XD_LISP_OBJECT, offsetof (struct detector_category, sym) },
  { XD_END }
};

static const struct sized_memory_description struct_detector_category_description =
{
  sizeof (struct detector_category),
  struct_detector_category_description_1
};

static const struct memory_description detector_category_dynarr_description_1[] =
{
  XD_DYNARR_DESC (detector_category_dynarr,
		  &struct_detector_category_description),
  { XD_END }
};

static const struct sized_memory_description detector_category_dynarr_description = {
  sizeof (detector_category_dynarr),
  detector_category_dynarr_description_1
};

static const struct memory_description struct_detector_description_1[]
=
{
  { XD_BLOCK_PTR, offsetof (struct detector, cats), 1,
    { &detector_category_dynarr_description } },
  { XD_END }
};

static const struct sized_memory_description struct_detector_description =
{
  sizeof (struct detector),
  struct_detector_description_1
};

static const struct memory_description detector_dynarr_description_1[] =
{
  XD_DYNARR_DESC (detector_dynarr, &struct_detector_description),
  { XD_END }
};

static const struct sized_memory_description detector_dynarr_description = {
  sizeof (detector_dynarr),
  detector_dynarr_description_1
};

Lisp_Object Qcoding_systemp;

Lisp_Object Qraw_text;

Lisp_Object Qmnemonic, Qeol_type;
Lisp_Object Qcr, Qcrlf, Qlf;
Lisp_Object Qeol_cr, Qeol_crlf, Qeol_lf;
Lisp_Object Qpost_read_conversion;
Lisp_Object Qpre_write_conversion;

Lisp_Object Qtranslation_table_for_decode;
Lisp_Object Qtranslation_table_for_encode;
Lisp_Object Qsafe_chars;
Lisp_Object Qsafe_charsets;
Lisp_Object Qmime_charset;
Lisp_Object Qvalid_codes;

Lisp_Object Qno_conversion;
Lisp_Object Qconvert_eol;
Lisp_Object Qescape_quoted;
Lisp_Object Qencode, Qdecode;

Lisp_Object Qconvert_eol_lf, Qconvert_eol_cr, Qconvert_eol_crlf;
Lisp_Object Qconvert_eol_autodetect;

Lisp_Object Qnear_certainty, Qquite_probable, Qsomewhat_likely;
Lisp_Object Qslightly_likely;
Lisp_Object Qas_likely_as_unlikely, Qsomewhat_unlikely, Qquite_improbable;
Lisp_Object Qnearly_impossible;

Lisp_Object Qdo_eol, Qdo_coding;

Lisp_Object Qcanonicalize_after_coding;

Lisp_Object QScoding_system_cookie;

/* This is used to convert autodetected coding systems into existing
   systems.  For example, the chain undecided->convert-eol-autodetect may
   have its separate parts detected as mswindows-multibyte and
   convert-eol-crlf, and the result needs to be mapped to
   mswindows-multibyte-dos. */
/* #### It's not clear we need this whole chain-canonicalize mechanism
   any more. */
static Lisp_Object Vchain_canonicalize_hash_table;

#ifdef HAVE_ZLIB
Lisp_Object Qgzip;
#endif

/* Maps symbols (coding system names) to either coding system objects or
   (for aliases) other names. */
static Lisp_Object Vcoding_system_hash_table;

int enable_multibyte_characters;

EXFUN (Fcopy_coding_system, 2);


/************************************************************************/
/*                     Coding system object methods                     */
/************************************************************************/

static Lisp_Object
mark_coding_system (Lisp_Object obj)
{
  Lisp_Coding_System *codesys = XCODING_SYSTEM (obj);

#define MARKED_SLOT(x) mark_object (codesys->x);
#include "coding-system-slots.h"

  MAYBE_CODESYSMETH (codesys, mark, (obj));

  return Qnil;
}

static void
print_coding_system_properties (Lisp_Object obj, Lisp_Object printcharfun)
{
  Lisp_Coding_System *c = XCODING_SYSTEM (obj);
  print_internal (c->methods->type, printcharfun, 1);
  MAYBE_CODESYSMETH (c, print, (obj, printcharfun, 1));
  if (CODING_SYSTEM_EOL_TYPE (c) != EOL_AUTODETECT)
    write_fmt_string_lisp (printcharfun, " eol-type=%s",
			   1, Fcoding_system_property (obj, Qeol_type));
}

static void
print_coding_system (Lisp_Object obj, Lisp_Object printcharfun,
		     int UNUSED (escapeflag))
{
  Lisp_Coding_System *c = XCODING_SYSTEM (obj);
  if (print_readably)
    printing_unreadable_object
      ("printing unreadable object #<coding-system 0x%x>", c->header.uid);

  write_fmt_string_lisp (printcharfun, "#<coding-system %s ", 1, c->name);
  print_coding_system_properties (obj, printcharfun);
  write_c_string (printcharfun, ">");
}

/* Print an abbreviated version of a coding system (but still containing
   all the information), for use within a coding system print method. */

static void
print_coding_system_in_print_method (Lisp_Object cs, Lisp_Object printcharfun,
				     int UNUSED (escapeflag))
{
  write_fmt_string_lisp (printcharfun, "%s[", 1, XCODING_SYSTEM_NAME (cs));
  print_coding_system_properties (cs, printcharfun);
  write_c_string (printcharfun, "]");
}

static void
finalize_coding_system (void *header, int for_disksave)
{
  Lisp_Object cs = wrap_coding_system ((Lisp_Coding_System *) header);
  /* Since coding systems never go away, this function is not
     necessary.  But it would be necessary if we changed things
     so that coding systems could go away. */
  if (!for_disksave) /* see comment in lstream.c */
    MAYBE_XCODESYSMETH (cs, finalize, (cs));
}

static Bytecount
sizeof_coding_system (const void *header)
{
  const Lisp_Coding_System *p = (const Lisp_Coding_System *) header;
  return offsetof (Lisp_Coding_System, data) + p->methods->extra_data_size;
}

static const struct memory_description coding_system_methods_description_1[]
= {
  { XD_LISP_OBJECT,
    offsetof (struct coding_system_methods, type) },
  { XD_LISP_OBJECT,
    offsetof (struct coding_system_methods, predicate_symbol) },
  { XD_END }
};

const struct sized_memory_description coding_system_methods_description = {
  sizeof (struct coding_system_methods),
  coding_system_methods_description_1
};

static const struct sized_memory_description coding_system_extra_description_map[] =
{
  { offsetof (Lisp_Coding_System, methods) },
  { offsetof (struct coding_system_methods, extra_description) },
  { -1 },
};

static const struct memory_description coding_system_description[] =
{
  { XD_BLOCK_PTR,  offsetof (Lisp_Coding_System, methods), 1,
    { &coding_system_methods_description } },
#define MARKED_SLOT(x) { XD_LISP_OBJECT, offsetof (Lisp_Coding_System, x) },
#define MARKED_SLOT_ARRAY(slot, size) \
  { XD_LISP_OBJECT_ARRAY, offsetof (Lisp_Coding_System, slot), size },
#include "coding-system-slots.h"
  { XD_BLOCK_ARRAY, offsetof (Lisp_Coding_System, data), 1,
    { coding_system_extra_description_map } },
  { XD_END }
};

static const struct memory_description coding_system_empty_extra_description_1[] =
{
  { XD_END }
};

const struct sized_memory_description coding_system_empty_extra_description = {
  0, coding_system_empty_extra_description_1
};

DEFINE_LRECORD_SEQUENCE_IMPLEMENTATION ("coding-system", coding_system,
					1, /*dumpable-flag*/
					mark_coding_system,
					print_coding_system,
					finalize_coding_system,
					0, 0, coding_system_description,
					sizeof_coding_system,
					Lisp_Coding_System);

/************************************************************************/
/*                       Creating coding systems                        */
/************************************************************************/

static struct coding_system_methods *
decode_coding_system_type (Lisp_Object type, Error_Behavior errb)
{
  int i;

  for (i = 0; i < Dynarr_length (the_coding_system_type_entry_dynarr); i++)
    {
      if (EQ (type,
	      Dynarr_at (the_coding_system_type_entry_dynarr, i).meths->type))
	return Dynarr_at (the_coding_system_type_entry_dynarr, i).meths;
    }

  maybe_invalid_constant ("Invalid coding system type", type,
			  Qcoding_system, errb);

  return 0;
}

static int
valid_coding_system_type_p (Lisp_Object type)
{
  return decode_coding_system_type (type, ERROR_ME_NOT) != 0;
}

DEFUN ("valid-coding-system-type-p", Fvalid_coding_system_type_p, 1, 1, 0, /*
Given a CODING-SYSTEM-TYPE, return non-nil if it is valid.
Valid types depend on how XEmacs was compiled but may include
'undecided, 'chain, 'integer, 'ccl, 'iso2022, 'big5, 'shift-jis,
'utf-16, 'ucs-4, 'utf-8, etc.
*/
       (coding_system_type))
{
  return valid_coding_system_type_p (coding_system_type) ? Qt : Qnil;
}

DEFUN ("coding-system-type-list", Fcoding_system_type_list, 0, 0, 0, /*
Return a list of valid coding system types.
*/
       ())
{
  return Fcopy_sequence (Vcoding_system_type_list);
}

void
add_entry_to_coding_system_type_list (struct coding_system_methods *meths)
{
  struct coding_system_type_entry entry;

  entry.meths = meths;
  Dynarr_add (the_coding_system_type_entry_dynarr, entry);
  Vcoding_system_type_list = Fcons (meths->type, Vcoding_system_type_list);
}

DEFUN ("coding-system-p", Fcoding_system_p, 1, 1, 0, /*
Return t if OBJECT is a coding system.
A coding system is an object that defines how text containing multiple
character sets is encoded into a stream of (typically 8-bit) bytes.
The coding system is used to decode the stream into a series of
characters (which may be from multiple charsets) when the text is read
from a file or process, and is used to encode the text back into the
same format when it is written out to a file or process.

For example, many ISO2022-compliant coding systems (such as Compound
Text, which is used for inter-client data under the X Window System)
use escape sequences to switch between different charsets -- Japanese
Kanji, for example, is invoked with "ESC $ ( B"; ASCII is invoked
with "ESC ( B"; and Cyrillic is invoked with "ESC - L".  See
`make-coding-system' for more information.

Coding systems are normally identified using a symbol, and the
symbol is accepted in place of the actual coding system object whenever
a coding system is called for. (This is similar to how faces work.)
*/
       (object))
{
  return CODING_SYSTEMP (object) ? Qt : Qnil;
}

DEFUN ("find-coding-system", Ffind_coding_system, 1, 1, 0, /*
Retrieve the coding system of the given name.

If CODING-SYSTEM-OR-NAME is a coding-system object, it is simply
returned.  Otherwise, CODING-SYSTEM-OR-NAME should be a symbol.
If there is no such coding system, nil is returned.  Otherwise the
associated coding system object is returned.
*/
       (coding_system_or_name))
{
  if (NILP (coding_system_or_name))
    coding_system_or_name = Qbinary;
  else if (CODING_SYSTEMP (coding_system_or_name))
    return coding_system_or_name;
  else
    CHECK_SYMBOL (coding_system_or_name);

  while (1)
    {
      coding_system_or_name =
	Fgethash (coding_system_or_name, Vcoding_system_hash_table, Qnil);

      if (CODING_SYSTEMP (coding_system_or_name)
	  || NILP (coding_system_or_name))
	return coding_system_or_name;
    }
}

DEFUN ("get-coding-system", Fget_coding_system, 1, 1, 0, /*
Retrieve the coding system of the given name.
Same as `find-coding-system' except that if there is no such
coding system, an error is signaled instead of returning nil.
*/
       (name))
{
  Lisp_Object coding_system = Ffind_coding_system (name);

  if (NILP (coding_system))
    invalid_argument ("No such coding system", name);
  return coding_system;
}

int
coding_system_is_binary (Lisp_Object coding_system)
{
  Lisp_Coding_System *cs = XCODING_SYSTEM (coding_system);
  return
    (EQ (CODING_SYSTEM_TYPE (cs), Qno_conversion) &&
     CODING_SYSTEM_EOL_TYPE (cs) == EOL_LF &&
     EQ (CODING_SYSTEM_POST_READ_CONVERSION (cs), Qnil) &&
     EQ (CODING_SYSTEM_PRE_WRITE_CONVERSION (cs), Qnil));
}

static Lisp_Object
coding_system_real_canonical (Lisp_Object cs)
{
  if (!NILP (XCODING_SYSTEM_CANONICAL (cs)))
    return XCODING_SYSTEM_CANONICAL (cs);
  return cs;
}

/* Return true if coding system is of the "standard" type that decodes
   bytes into characters (suitable for decoding a text file). */
int
coding_system_is_for_text_file (Lisp_Object coding_system)
{
  return (XCODESYSMETH_OR_GIVEN
	  (coding_system, conversion_end_type,
	   (coding_system_real_canonical (coding_system)),
	   DECODES_BYTE_TO_CHARACTER) ==
	  DECODES_BYTE_TO_CHARACTER);
}

static int
decoding_source_sink_type_is_char (Lisp_Object cs, enum source_or_sink sex)
{
  enum source_sink_type type =
    XCODESYSMETH_OR_GIVEN (cs, conversion_end_type,
			   (coding_system_real_canonical (cs)),
			   DECODES_BYTE_TO_CHARACTER);
  if (sex == CODING_SOURCE)
    return (type == DECODES_CHARACTER_TO_CHARACTER ||
	    type == DECODES_CHARACTER_TO_BYTE);
  else
    return (type == DECODES_CHARACTER_TO_CHARACTER ||
	    type == DECODES_BYTE_TO_CHARACTER);
}

static int
encoding_source_sink_type_is_char (Lisp_Object cs, enum source_or_sink sex)
{
  return decoding_source_sink_type_is_char (cs,
					    /* Sex change */
					    sex == CODING_SOURCE ?
					    CODING_SINK : CODING_SOURCE);
}

/* Like Ffind_coding_system() but check that the coding system is of the
   "standard" type that decodes bytes into characters (suitable for
   decoding a text file), and if not, returns an appropriate wrapper that
   does.  Also, if EOL_WRAP is non-zero, check whether this coding system
   wants EOL auto-detection, and if so, wrap with a convert-eol coding
   system to do this. */

Lisp_Object
find_coding_system_for_text_file (Lisp_Object name, int eol_wrap)
{
  Lisp_Object coding_system = Ffind_coding_system (name);
  Lisp_Object wrapper = coding_system;

  if (NILP (coding_system))
    return Qnil;
  if (!coding_system_is_for_text_file (coding_system))
    {
      wrapper = XCODING_SYSTEM_TEXT_FILE_WRAPPER (coding_system);
      if (NILP (wrapper))
        {
	  Lisp_Object chain;
          if (!decoding_source_sink_type_is_char (coding_system, CODING_SINK))
            chain = list2 (coding_system, Qbinary);
          else
            chain = list1 (coding_system);
          if (decoding_source_sink_type_is_char (coding_system, CODING_SOURCE))
            chain = Fcons (Qbinary, chain);
          wrapper =
	    make_internal_coding_system
	      (coding_system,
	       "internal-text-file-wrapper",
	       Qchain,
	       Qunbound, list4 (Qchain, chain,
				Qcanonicalize_after_coding, coding_system));
	  XCODING_SYSTEM_TEXT_FILE_WRAPPER (coding_system) = wrapper;
	}
    }

  if (!eol_wrap || XCODING_SYSTEM_EOL_TYPE (coding_system) != EOL_AUTODETECT)
    return wrapper;

  coding_system = wrapper;
  wrapper = XCODING_SYSTEM_AUTO_EOL_WRAPPER (coding_system);
  if (!NILP (wrapper))
    return wrapper;
  wrapper =
    make_internal_coding_system
      (coding_system,
       "internal-auto-eol-wrapper",
       Qundecided, Qunbound,
       list4 (Qcoding_system, coding_system,
	      Qdo_eol, Qt));
  XCODING_SYSTEM_AUTO_EOL_WRAPPER (coding_system) = wrapper;
  return wrapper;
}

/* Like Fget_coding_system() but verify that the coding system is of the
   "standard" type that decodes bytes into characters (suitable for
   decoding a text file), and if not, returns an appropriate wrapper that
   does.  Also, if EOL_WRAP is non-zero, check whether this coding system
   wants EOL auto-detection, and if so, wrap with a convert-eol coding
   system to do this. */

Lisp_Object
get_coding_system_for_text_file (Lisp_Object name, int eol_wrap)
{
  Lisp_Object coding_system = find_coding_system_for_text_file (name,
								eol_wrap);
  if (NILP (coding_system))
    invalid_argument ("No such coding system", name);
  return coding_system;
}

/* We store the coding systems in hash tables with the names as the
   key and the actual coding system object as the value.  Occasionally
   we need to use them in a list format.  These routines provide us
   with that. */
struct coding_system_list_closure
{
  Lisp_Object *coding_system_list;
  int normal;
  int internal;
};

static int
add_coding_system_to_list_mapper (Lisp_Object key, Lisp_Object UNUSED (value),
				  void *coding_system_list_closure)
{
  /* This function can GC */
  struct coding_system_list_closure *cscl =
    (struct coding_system_list_closure *) coding_system_list_closure;
  Lisp_Object *coding_system_list = cscl->coding_system_list;

  /* We can't just use VALUE because KEY might be an alias, and we need
     the real coding system object. */
  if (XCODING_SYSTEM (Ffind_coding_system (key))->internal_p ?
      cscl->internal : cscl->normal)
    *coding_system_list = Fcons (key, *coding_system_list);
  return 0;
}

/* #### should we specify a conventional for "all coding systems"? */
DEFUN ("coding-system-list", Fcoding_system_list, 0, 1, 0, /*
Return a list of the names of all defined coding systems.
If INTERNAL is nil, only the normal (non-internal) coding systems are
included. (Internal coding systems are created for various internal
purposes, such as implementing EOL types of CRLF and CR; generally, you do
not want to see these.)  If it is t, only the internal coding systems are
included.  If it is any other non-nil value both normal and internal are
included.
*/
       (internal))
{
  Lisp_Object coding_system_list = Qnil;
  struct gcpro gcpro1;
  struct coding_system_list_closure coding_system_list_closure;

  GCPRO1 (coding_system_list);
  coding_system_list_closure.coding_system_list = &coding_system_list;
  coding_system_list_closure.normal = !EQ (internal, Qt);
  coding_system_list_closure.internal = !NILP (internal);
  elisp_maphash (add_coding_system_to_list_mapper, Vcoding_system_hash_table,
		 &coding_system_list_closure);
  UNGCPRO;

  return coding_system_list;
}

DEFUN ("coding-system-name", Fcoding_system_name, 1, 1, 0, /*
Return the name of the given coding system.
*/
       (coding_system))
{
  coding_system = Fget_coding_system (coding_system);
  return XCODING_SYSTEM_NAME (coding_system);
}

static Lisp_Coding_System *
allocate_coding_system (struct coding_system_methods *codesys_meths,
			Bytecount data_size,
			Lisp_Object name)
{
  Bytecount total_size = offsetof (Lisp_Coding_System, data) + data_size;
  Lisp_Coding_System *codesys =
#ifdef MC_ALLOC
    (Lisp_Coding_System *) alloc_lrecord (total_size,
					  &lrecord_coding_system);
#else /* not MC_ALLOC */
    (Lisp_Coding_System *) basic_alloc_lcrecord (total_size,
						 &lrecord_coding_system);
#endif /* not MC_ALLOC */

  codesys->methods = codesys_meths;
#define MARKED_SLOT(x) codesys->x = Qnil;
#include "coding-system-slots.h"

  CODING_SYSTEM_EOL_TYPE (codesys) = EOL_LF;
  CODING_SYSTEM_NAME     (codesys) = name;

  MAYBE_CODESYSMETH (codesys, init, (wrap_coding_system (codesys)));

  return codesys;
}

static enum eol_type
symbol_to_eol_type (Lisp_Object symbol)
{
  CHECK_SYMBOL (symbol);
  if (NILP (symbol))      return EOL_AUTODETECT;
  if (EQ (symbol, Qlf))   return EOL_LF;
  if (EQ (symbol, Qcrlf)) return EOL_CRLF;
  if (EQ (symbol, Qcr))   return EOL_CR;

  invalid_constant ("Unrecognized eol type", symbol);
  RETURN_NOT_REACHED (EOL_AUTODETECT);
}

static Lisp_Object
eol_type_to_symbol (enum eol_type type)
{
  switch (type)
    {
    default: ABORT ();
    case EOL_LF:         return Qlf;
    case EOL_CRLF:       return Qcrlf;
    case EOL_CR:         return Qcr;
    case EOL_AUTODETECT: return Qnil;
    }
}

struct subsidiary_type
{
  Ascbyte *extension;
  Ascbyte *mnemonic_ext;
  enum eol_type eol;
};

static struct subsidiary_type coding_subsidiary_list[] =
{ { "-unix", "",   EOL_LF },
  { "-dos",  ":T", EOL_CRLF },
  { "-mac",  ":t", EOL_CR } };

/* kludge */
static void
setup_eol_coding_systems (Lisp_Object codesys)
{
  int len = XSTRING_LENGTH (XSYMBOL (XCODING_SYSTEM_NAME (codesys))->name);
  Ibyte *codesys_name = alloca_ibytes (len + 7);
  int mlen = -1;
  Ibyte *codesys_mnemonic = 0;
  Lisp_Object codesys_name_sym, sub_codesys;
  int i;

  memcpy (codesys_name,
	  XSTRING_DATA (XSYMBOL (XCODING_SYSTEM_NAME (codesys))->name), len);

  if (STRINGP (XCODING_SYSTEM_MNEMONIC (codesys)))
    {
      mlen = XSTRING_LENGTH (XCODING_SYSTEM_MNEMONIC (codesys));
      codesys_mnemonic = alloca_ibytes (mlen + 7);
      memcpy (codesys_mnemonic,
	      XSTRING_DATA (XCODING_SYSTEM_MNEMONIC (codesys)), mlen);
    }

  /* Create three "subsidiary" coding systems, decoding data encoded using
     each of the three EOL types.  We do this for each subsidiary by
     copying the original coding system, setting the EOL type
     appropriately, and setting the CANONICAL member of the new coding
     system to be a chain consisting of the original coding system followed
     by a convert-eol coding system to do the EOL decoding.  For EOL type
     LF, however, we don't need any decoding, so we skip creating a
     CANONICAL.

     If the original coding system is not a text-type coding system
     (decodes byte->char), we need to coerce it to one by the appropriate
     wrapping in CANONICAL. */
  
  for (i = 0; i < countof (coding_subsidiary_list); i++)
    {
      Ascbyte *extension = coding_subsidiary_list[i].extension;
      Ascbyte *mnemonic_ext = coding_subsidiary_list[i].mnemonic_ext;
      enum eol_type eol = coding_subsidiary_list[i].eol;

      qxestrcpy_ascii (codesys_name + len, extension);
      codesys_name_sym = intern_int (codesys_name);
      if (mlen != -1)
	qxestrcpy_ascii (codesys_mnemonic + mlen, mnemonic_ext);

      sub_codesys = Fcopy_coding_system (codesys, codesys_name_sym);
      if (mlen != -1)
	XCODING_SYSTEM_MNEMONIC (sub_codesys) =
	  build_intstring (codesys_mnemonic);

      if (eol != EOL_LF)
	{
	  Lisp_Object chain = list2 (get_coding_system_for_text_file
				     (codesys, 0),
				     eol == EOL_CR ? Qconvert_eol_cr :
				     Qconvert_eol_crlf);
	  Lisp_Object canon =
	    make_internal_coding_system
	      (sub_codesys, "internal-subsidiary-eol-wrapper",
	       Qchain, Qunbound,
	       mlen != -1 ?
	       list6 (Qmnemonic, build_intstring (codesys_mnemonic),
		      Qchain, chain,
		      Qcanonicalize_after_coding, sub_codesys) :
	       list4 (Qchain, chain,
		      Qcanonicalize_after_coding, sub_codesys));
	  XCODING_SYSTEM_CANONICAL (sub_codesys) = canon;
	}
      XCODING_SYSTEM_EOL_TYPE (sub_codesys) = eol;
      XCODING_SYSTEM_SUBSIDIARY_PARENT (sub_codesys) = codesys;
      XCODING_SYSTEM (codesys)->eol[eol] = sub_codesys;
    }
}

/* Basic function to create new coding systems.  For `make-coding-system',
   NAME-OR-EXISTING is the NAME argument, PREFIX is null, and TYPE,
   DESCRIPTION, and PROPS are the same.  All created coding systems are put
   in a hash table indexed by NAME.

   If PREFIX is a string, NAME-OR-EXISTING should specify an existing
   coding system (or nil), and an internal coding system will be created.
   The name of the coding system will be constructed by combining PREFIX
   with the name of the existing coding system (if given), and a number
   will be appended to insure uniqueness.  In such a case, if Qunbound is
   given for DESCRIPTION, the description gets created based on the
   generated name.  Also, if no mnemonic is given in the properties list, a
   mnemonic is created based on the generated name.

   For internal coding systems, the coding system is marked as internal
   (see `coding-system-list'), and no subsidiaries will be created or
   eol-wrapping will happen.  Otherwise:

   -- if the eol-type property is `lf' or t, the coding system is merely
      created and returned. (For t, the coding system will be wrapped with
      an EOL autodetector when it's used to read a file.)

   -- if eol-type is `crlf' or `cr', after the coding system object is
      created, it will be wrapped in a chain with the appropriate
      convert-eol coding system (either `convert-eol-crlf' or
      `convert-eol-cr'), so that CRLF->LF or CR->LF conversion is done at
      decoding time, and the opposite at encoding time.  The resulting
      chain becomes the CANONICAL field of the coding system object.

   -- if eol-type is nil or omitted, "subsidiaries" are generated: Three
      coding systems where the original coding system (before wrapping with
      convert-eol-autodetect) is either unwrapped or wrapped with
      convert-eol-crlf or convert-eol-cr, respectively, so that coding systems
      to handle LF, CRLF, and CR end-of-line indicators are created. (This
      crazy crap is based on existing behavior in other Mule versions,
      including FSF Emacs.)
   */

static Lisp_Object
make_coding_system_1 (Lisp_Object name_or_existing, Ascbyte *prefix,
		      Lisp_Object type, Lisp_Object description,
		      Lisp_Object props)
{
  Lisp_Coding_System *cs;
  int need_to_setup_eol_systems = 1;
  enum eol_type eol_wrapper = EOL_AUTODETECT;
  struct coding_system_methods *meths;
  Lisp_Object csobj;
  Lisp_Object defmnem = Qnil;

  if (NILP (type))
    type = Qundecided;
  meths = decode_coding_system_type (type, ERROR_ME);

  if (prefix)
    {
      Ibyte *newname =
	emacs_sprintf_malloc (NULL, "%s-%s-%d",
			      prefix,
			      NILP (name_or_existing) ? (Ibyte *) "nil" :
			      XSTRING_DATA (Fsymbol_name (XCODING_SYSTEM_NAME
							  (name_or_existing))),
			      ++coding_system_tick);
      name_or_existing = intern_int (newname);
      xfree (newname, Ibyte *);
      
      if (UNBOUNDP (description))
	{
	  newname =
	    emacs_sprintf_malloc
	      (NULL, "For Internal Use (%s)",
	       XSTRING_DATA (Fsymbol_name (name_or_existing)));
	  description = build_intstring (newname);
	  xfree (newname, Ibyte *);
	}

      newname = emacs_sprintf_malloc (NULL, "Int%d", coding_system_tick);
      defmnem = build_intstring (newname);
      xfree (newname, Ibyte *);
    }
  else
    CHECK_SYMBOL (name_or_existing);

  if (!NILP (Ffind_coding_system (name_or_existing)))
    invalid_operation ("Cannot redefine existing coding system",
		       name_or_existing);

  cs = allocate_coding_system (meths, meths->extra_data_size,
			       name_or_existing);
  csobj = wrap_coding_system (cs);

  cs->internal_p = !!prefix;

  if (NILP (description))
    description = build_string ("");
  else
    CHECK_STRING (description);
  CODING_SYSTEM_DESCRIPTION (cs) = description;

  if (!NILP (defmnem))
    CODING_SYSTEM_MNEMONIC (cs) = defmnem;

  {
    EXTERNAL_PROPERTY_LIST_LOOP_3 (key, value, props)
      {
	int recognized = 1;

	if (EQ (key, Qmnemonic))
	  {
	    if (!NILP (value))
	      CHECK_STRING (value);
	    CODING_SYSTEM_MNEMONIC (cs) = value;
	  }

	else if (EQ (key, Qdocumentation))
	  {
	    if (!NILP (value))
	      CHECK_STRING (value);
	    CODING_SYSTEM_DOCUMENTATION (cs) = value;
	  }

	else if (EQ (key, Qeol_type))
	  {
	    need_to_setup_eol_systems = NILP (value);
	    if (EQ (value, Qt))
	      value = Qnil;
	    eol_wrapper = symbol_to_eol_type (value);
	  }

	else if (EQ (key, Qpost_read_conversion))
	  CODING_SYSTEM_POST_READ_CONVERSION (cs) = value;
	else if (EQ (key, Qpre_write_conversion))
	  CODING_SYSTEM_PRE_WRITE_CONVERSION (cs) = value;
	/* FSF compatibility */
	else if (EQ (key, Qtranslation_table_for_decode))
	  ;
	else if (EQ (key, Qtranslation_table_for_encode))
	  ;
	else if (EQ (key, Qsafe_chars))
	  ;
	else if (EQ (key, Qsafe_charsets))
	  ;
	else if (EQ (key, Qmime_charset))
	  ;
	else if (EQ (key, Qvalid_codes))
	  ;
	else
	  recognized = CODESYSMETH_OR_GIVEN (cs, putprop,
					     (csobj, key, value), 0);

	if (!recognized)
	  invalid_constant ("Unrecognized property", key);
      }
  }

  {
    XCODING_SYSTEM_CANONICAL (csobj) =
      CODESYSMETH_OR_GIVEN (cs, canonicalize, (csobj), Qnil);
    XCODING_SYSTEM_EOL_TYPE (csobj) = EOL_AUTODETECT; /* for copy-coding-system
							 below */

    if (need_to_setup_eol_systems && !cs->internal_p)
      setup_eol_coding_systems (csobj);
    else if (eol_wrapper == EOL_CR || eol_wrapper == EOL_CRLF)
      {
	/* If a specific eol-type (other than LF) was specified, we handle
	   this by converting the coding system into a chain that wraps the
	   coding system along with a convert-eol system after it, in
	   exactly that same switcheroo fashion that the normal
	   canonicalize method works -- BUT we will run into a problem if
	   we do it the obvious way, because when `chain' creates its
	   substreams, the substream containing the coding system we're
	   creating will have canonicalization expansion done on it,
	   leading to infinite recursion.  So we have to generate a new,
	   internal coding system with the previous value of CANONICAL. */
	Ibyte *newname =
	  emacs_sprintf_malloc
	    (NULL, "internal-eol-copy-%s-%d",
	     XSTRING_DATA (Fsymbol_name (name_or_existing)),
	     ++coding_system_tick);
	Lisp_Object newnamesym = intern_int (newname);
	Lisp_Object copied = Fcopy_coding_system (csobj, newnamesym);
	xfree (newname, Ibyte *);
	
	XCODING_SYSTEM_CANONICAL (csobj) =
	  make_internal_coding_system
	    (csobj,
	     "internal-eol-wrapper",
	     Qchain, Qunbound,
	     list4 (Qchain,
		    list2 (copied,
			   eol_wrapper == EOL_CR ?
			   Qconvert_eol_cr :
			   Qconvert_eol_crlf),
		    Qcanonicalize_after_coding,
		    csobj));
      }
    XCODING_SYSTEM_EOL_TYPE (csobj) = eol_wrapper;
  }
  
  Fputhash (name_or_existing, csobj, Vcoding_system_hash_table);

  return csobj;
}

Lisp_Object
make_internal_coding_system (Lisp_Object existing, Ascbyte *prefix,
			     Lisp_Object type, Lisp_Object description,
			     Lisp_Object props)
{
  return make_coding_system_1 (existing, prefix, type, description, props);
}

DEFUN ("make-coding-system", Fmake_coding_system, 2, 4, 0, /*
Register symbol NAME as a coding system.

TYPE describes the conversion method used and should be one of

nil or 'undecided
     Automatic conversion.  XEmacs attempts to detect the coding system
     used in the file.
'chain
     Chain two or more coding systems together to make a combination coding
     system.
'no-conversion
     No conversion.  Use this for binary files and such.  On output,
     graphic characters that are not in ASCII or Latin-1 will be
     replaced by a ?. (For a no-conversion-encoded buffer, these
     characters will only be present if you explicitly insert them.)
'convert-eol
     Convert CRLF sequences or CR to LF.
'shift-jis
     Shift-JIS (a Japanese encoding commonly used in PC operating systems).
'unicode
     Any Unicode encoding (UCS-4, UTF-8, UTF-16, etc.).
'mswindows-unicode-to-multibyte
     (MS Windows only) Converts from Windows Unicode to Windows Multibyte
     (any code page encoding) upon encoding, and the other way upon decoding.
'mswindows-multibyte
     Converts to or from Windows Multibyte (any code page encoding).
     This is resolved into a chain of `mswindows-unicode' and
     `mswindows-unicode-to-multibyte'.
'iso2022
     Any ISO2022-compliant encoding.  Among other things, this includes
     JIS (the Japanese encoding commonly used for e-mail), EUC (the
     standard Unix encoding for Japanese and other languages), and
     Compound Text (the encoding used in X11).  You can specify more
     specific information about the conversion with the PROPS argument.
'big5
     Big5 (the encoding commonly used for Mandarin Chinese in Taiwan).
'ccl
     The conversion is performed using a user-written pseudo-code
     program.  CCL (Code Conversion Language) is the name of this
     pseudo-code.
'gzip
     GZIP compression format.
'internal
     Write out or read in the raw contents of the memory representing
     the buffer's text.  This is primarily useful for debugging
     purposes, and is only enabled when XEmacs has been compiled with
     DEBUG_XEMACS defined (via the --debug configure option).
     WARNING: Reading in a file using 'internal conversion can result
     in an internal inconsistency in the memory representing a
     buffer's text, which will produce unpredictable results and may
     cause XEmacs to crash.  Under normal circumstances you should
     never use 'internal conversion.

DESCRIPTION is a short English phrase describing the coding system,
suitable for use as a menu item. (See also the `documentation' property
below.)

PROPS is a property list, describing the specific nature of the
character set.  Recognized properties are:

'mnemonic
     String to be displayed in the modeline when this coding system is
     active.

'documentation
     Detailed documentation on the coding system.

'eol-type
     End-of-line conversion to be used.  It should be one of

	nil
		Automatically detect the end-of-line type (LF, CRLF,
		or CR).  Also generate subsidiary coding systems named
		`NAME-unix', `NAME-dos', and `NAME-mac', that are
		identical to this coding system but have an EOL-TYPE
		value of 'lf, 'crlf, and 'cr, respectively.
	'lf
		The end of a line is marked externally using ASCII LF.
		Since this is also the way that XEmacs represents an
		end-of-line internally, specifying this option results
		in no end-of-line conversion.  This is the standard
		format for Unix text files.
	'crlf
		The end of a line is marked externally using ASCII
		CRLF.  This is the standard format for MS-DOS text
		files.
	'cr
		The end of a line is marked externally using ASCII CR.
		This is the standard format for Macintosh text files.
	t
		Automatically detect the end-of-line type but do not
		generate subsidiary coding systems.  (This value is
		converted to nil when stored internally, and
		`coding-system-property' will return nil.)

'post-read-conversion
     The value is a function to call after some text is inserted and
     decoded by the coding system itself and before any functions in
     `after-change-functions' are called. (#### Not actually true in
     XEmacs. `after-change-functions' will be called twice if
     `post-read-conversion' changes something.) The argument of this
     function is the same as for a function in
     `after-insert-file-functions', i.e. LENGTH of the text inserted,
     with point at the head of the text to be decoded.

'pre-write-conversion
     The value is a function to call after all functions in
     `write-region-annotate-functions' and `buffer-file-format' are
     called, and before the text is encoded by the coding system itself.
     The arguments to this function are the same as those of a function
     in `write-region-annotate-functions', i.e. FROM and TO, specifying
     a region of text.



The following properties are allowed for FSF compatibility but currently
ignored:

'translation-table-for-decode
     The value is a translation table to be applied on decoding.  See
     the function `make-translation-table' for the format of translation
     table.  This is not applicable to CCL-based coding systems.
    
'translation-table-for-encode
     The value is a translation table to be applied on encoding.  This is
     not applicable to CCL-based coding systems.
    
'safe-chars
     The value is a char table.  If a character has non-nil value in it,
     the character is safely supported by the coding system.  This
     overrides the specification of safe-charsets.
   
'safe-charsets
     The value is a list of charsets safely supported by the coding
     system.  The value t means that all charsets Emacs handles are
     supported.  Even if some charset is not in this list, it doesn't
     mean that the charset can't be encoded in the coding system;
     it just means that some other receiver of text encoded
     in the coding system won't be able to handle that charset.
    
'mime-charset
     The value is a symbol of which name is `MIME-charset' parameter of
     the coding system.
    
'valid-codes (meaningful only for a coding system based on CCL)
     The value is a list to indicate valid byte ranges of the encoded
     file.  Each element of the list is an integer or a cons of integer.
     In the former case, the integer value is a valid byte code.  In the
     latter case, the integers specifies the range of valid byte codes.



The following additional property is recognized if TYPE is 'convert-eol:

'subtype
     One of `lf', `crlf', `cr' or nil (for autodetection).  When decoding,
     the corresponding sequence will be converted to LF.  When encoding,
     the opposite happens.  This coding system converts characters to
     characters.



The following additional properties are recognized if TYPE is 'iso2022:

'charset-g0
'charset-g1
'charset-g2
'charset-g3
     The character set initially designated to the G0 - G3 registers.
     The value should be one of

          -- A charset object (designate that character set)
	  -- nil (do not ever use this register)
	  -- t (no character set is initially designated to
		the register, but may be later on; this automatically
		sets the corresponding `force-g*-on-output' property)

'force-g0-on-output
'force-g1-on-output
'force-g2-on-output
'force-g2-on-output
     If non-nil, send an explicit designation sequence on output before
     using the specified register.

'short
     If non-nil, use the short forms "ESC $ @", "ESC $ A", and
     "ESC $ B" on output in place of the full designation sequences
     "ESC $ ( @", "ESC $ ( A", and "ESC $ ( B".

'no-ascii-eol
     If non-nil, don't designate ASCII to G0 at each end of line on output.
     Setting this to non-nil also suppresses other state-resetting that
     normally happens at the end of a line.

'no-ascii-cntl
     If non-nil, don't designate ASCII to G0 before control chars on output.

'seven
     If non-nil, use 7-bit environment on output.  Otherwise, use 8-bit
     environment.

'lock-shift
     If non-nil, use locking-shift (SO/SI) instead of single-shift
     or designation by escape sequence.

'no-iso6429
     If non-nil, don't use ISO6429's direction specification.

'escape-quoted
     If non-nil, literal control characters that are the same as
     the beginning of a recognized ISO2022 or ISO6429 escape sequence
     (in particular, ESC (0x1B), SO (0x0E), SI (0x0F), SS2 (0x8E),
     SS3 (0x8F), and CSI (0x9B)) are "quoted" with an escape character
     so that they can be properly distinguished from an escape sequence.
     (Note that doing this results in a non-portable encoding.) This
     encoding flag is used for byte-compiled files.  Note that ESC
     is a good choice for a quoting character because there are no
     escape sequences whose second byte is a character from the Control-0
     or Control-1 character sets; this is explicitly disallowed by the
     ISO2022 standard.

'input-charset-conversion
     A list of conversion specifications, specifying conversion of
     characters in one charset to another when decoding is performed.
     Each specification is a list of two elements: the source charset,
     and the destination charset.

'output-charset-conversion
     A list of conversion specifications, specifying conversion of
     characters in one charset to another when encoding is performed.
     The form of each specification is the same as for
     'input-charset-conversion.



The following additional properties are recognized (and required)
if TYPE is 'ccl:

'decode
     CCL program used for decoding (converting to internal format).

'encode
     CCL program used for encoding (converting to external format).


The following additional properties are recognized if TYPE is 'chain:

'chain
     List of coding systems to be chained together, in decoding order.

'canonicalize-after-coding
     Coding system to be returned by the detector routines in place of
     this coding system.



The following additional properties are recognized if TYPE is 'unicode:

'type
     One of `utf-16', `utf-8', `ucs-4', or `utf-7' (the latter is not
     yet implemented).  `utf-16' is the basic two-byte encoding;
     `ucs-4' is the four-byte encoding; `utf-8' is an ASCII-compatible
     variable-width 8-bit encoding; `utf-7' is a 7-bit encoding using
     only characters that will safely pass through all mail gateways.
     [[ This should be \"transformation format\".  There should also be
     `ucs-2' (or `bmp' -- no surrogates) and `utf-32' (range checked). ]]

'little-endian
     If non-nil, `utf-16' and `ucs-4' will write out the groups of two
     or four bytes little-endian instead of big-endian.  This is required,
     for example, under Windows.

'need-bom
     If non-nil, a byte order mark (BOM, or Unicode FFFE) should be
     written out at the beginning of the data.  This serves both to
     identify the endianness of the following data and to mark the
     data as Unicode (at least, this is how Windows uses it).
     [[ The correct term is \"signature\", since this technique may also
     be used with UTF-8.  That is the term used in the standard. ]]


The following additional properties are recognized if TYPE is
'mswindows-multibyte:

'code-page
     Either a number (specifying a particular code page) or one of the
     symbols `ansi', `oem', `mac', or `ebcdic', specifying the ANSI,
     OEM, Macintosh, or EBCDIC code page associated with a particular
     locale (given by the `locale' property).  NOTE: EBCDIC code pages
     only exist in Windows 2000 and later.

'locale
     If `code-page' is a symbol, this specifies the locale whose code
     page of the corresponding type should be used.  This should be
     one of the following: A cons of two strings, (LANGUAGE
     . SUBLANGUAGE) (see `mswindows-set-current-locale'); a string (a
     language; SUBLANG_DEFAULT, i.e. the default sublanguage, is
     used); or one of the symbols `current', `user-default', or
     `system-default', corresponding to the values of
     `mswindows-current-locale', `mswindows-user-default-locale', or
     `mswindows-system-default-locale', respectively.



The following additional properties are recognized if TYPE is 'undecided:
[[ Doesn't GNU use \"detect-*\" for the following two? ]]

'do-eol
     Do EOL detection.

'do-coding
     Do encoding detection.

'coding-system
     If encoding detection is not done, use the specified coding system
     to do decoding.  This is used internally when implementing coding
     systems with an EOL type that specifies autodetection (the default),
     so that the detector routines return the proper subsidiary.



The following additional property is recognized if TYPE is 'gzip:

'level
     Compression level: 0 through 9, or `default' (currently 6).

*/
       (name, type, description, props))
{
  return make_coding_system_1 (name, 0, type, description, props);
}

DEFUN ("copy-coding-system", Fcopy_coding_system, 2, 2, 0, /*
Copy OLD-CODING-SYSTEM to NEW-NAME.
If NEW-NAME does not name an existing coding system, a new one will
be created.
If you are using this function to create an alias, think again:
Use `define-coding-system-alias' instead.
*/
       (old_coding_system, new_name))
{
  Lisp_Object new_coding_system;
  old_coding_system = Fget_coding_system (old_coding_system);
  new_coding_system =
    UNBOUNDP (new_name) ? Qnil : Ffind_coding_system (new_name);
  if (NILP (new_coding_system))
    {
      new_coding_system =
	wrap_coding_system
	  (allocate_coding_system
	   (XCODING_SYSTEM (old_coding_system)->methods,
	    XCODING_SYSTEM (old_coding_system)->methods->extra_data_size,
	    new_name));
      if (!UNBOUNDP (new_name))
	Fputhash (new_name, new_coding_system, Vcoding_system_hash_table);
    }
  else if (XCODING_SYSTEM (old_coding_system)->methods !=
	   XCODING_SYSTEM (new_coding_system)->methods)
    invalid_operation_2 ("Coding systems not same type",
			 old_coding_system, new_coding_system);

  {
    Lisp_Coding_System *to = XCODING_SYSTEM (new_coding_system);
    Lisp_Coding_System *from = XCODING_SYSTEM (old_coding_system);
#ifdef MC_ALLOC
    copy_sized_lrecord (to, from, sizeof_coding_system (from));
#else /* not MC_ALLOC */
    copy_sized_lcrecord (to, from, sizeof_coding_system (from));
#endif /* not MC_ALLOC */
    to->name = new_name;
  }
  return new_coding_system;
}

DEFUN ("coding-system-canonical-name-p", Fcoding_system_canonical_name_p,
       1, 1, 0, /*
Return t if OBJECT names a coding system, and is not a coding system alias.
*/
       (object))
{
  return CODING_SYSTEMP (Fgethash (object, Vcoding_system_hash_table, Qnil))
    ? Qt : Qnil;
}

/* #### Shouldn't this really be a find/get pair? */

DEFUN ("coding-system-alias-p", Fcoding_system_alias_p, 1, 1, 0, /*
Return t if OBJECT is a coding system alias.
All coding system aliases are created by `define-coding-system-alias'.
*/
       (object))
{
  return SYMBOLP (Fgethash (object, Vcoding_system_hash_table, Qzero))
    ? Qt : Qnil;
}

DEFUN ("coding-system-aliasee", Fcoding_system_aliasee, 1, 1, 0, /*
Return the coding-system symbol for which symbol ALIAS is an alias.
*/
       (alias))
{
  Lisp_Object aliasee = Fgethash (alias, Vcoding_system_hash_table, Qnil);
  if (SYMBOLP (aliasee))
    return aliasee;
  else
    invalid_argument ("Symbol is not a coding system alias", alias);
  RETURN_NOT_REACHED (Qnil);
}

/* A maphash function, for removing dangling coding system aliases. */
static int
dangling_coding_system_alias_p (Lisp_Object UNUSED (alias),
				Lisp_Object aliasee,
				void *dangling_aliases)
{
  if (SYMBOLP (aliasee)
      && NILP (Fgethash (aliasee, Vcoding_system_hash_table, Qnil)))
    {
      (*(int *) dangling_aliases)++;
      return 1;
    }
  else
    return 0;
}

DEFUN ("define-coding-system-alias", Fdefine_coding_system_alias, 2, 2, 0, /*
Define symbol ALIAS as an alias for coding system ALIASEE.

You can use this function to redefine an alias that has already been defined,
but you cannot redefine a name which is the canonical name for a coding system.
\(a canonical name of a coding system is what is returned when you call
`coding-system-name' on a coding system).

ALIASEE itself can be an alias, which allows you to define nested aliases.

You are forbidden, however, from creating alias loops or `dangling' aliases.
These will be detected, and an error will be signaled if you attempt to do so.

If ALIASEE is nil, then ALIAS will simply be undefined.

See also `coding-system-alias-p', `coding-system-aliasee',
and `coding-system-canonical-name-p'.
*/
       (alias, aliasee))
{
  Lisp_Object probe;

  CHECK_SYMBOL (alias);

  if (!NILP (Fcoding_system_canonical_name_p (alias)))
    invalid_change
      ("Symbol is the canonical name of a coding system and cannot be redefined",
       alias);

  if (NILP (aliasee))
    {
      Lisp_Object subsidiary_unix = add_suffix_to_symbol (alias, "-unix");
      Lisp_Object subsidiary_dos  = add_suffix_to_symbol (alias, "-dos");
      Lisp_Object subsidiary_mac  = add_suffix_to_symbol (alias, "-mac");

      Fremhash (alias, Vcoding_system_hash_table);

      /* Undefine subsidiary aliases,
	 presumably created by a previous call to this function */
      if (! NILP (Fcoding_system_alias_p (subsidiary_unix)) &&
	  ! NILP (Fcoding_system_alias_p (subsidiary_dos))  &&
	  ! NILP (Fcoding_system_alias_p (subsidiary_mac)))
	{
	  Fdefine_coding_system_alias (subsidiary_unix, Qnil);
	  Fdefine_coding_system_alias (subsidiary_dos,  Qnil);
	  Fdefine_coding_system_alias (subsidiary_mac,  Qnil);
	}

      /* Undefine dangling coding system aliases. */
      {
	int dangling_aliases;

	do {
	  dangling_aliases = 0;
	  elisp_map_remhash (dangling_coding_system_alias_p,
			     Vcoding_system_hash_table,
			     &dangling_aliases);
	} while (dangling_aliases > 0);
      }

      return Qnil;
    }

  if (CODING_SYSTEMP (aliasee))
    aliasee = XCODING_SYSTEM_NAME (aliasee);

  /* Checks that aliasee names a coding-system */
  (void) Fget_coding_system (aliasee);

  /* Check for coding system alias loops */
  if (EQ (alias, aliasee))
    alias_loop: invalid_operation_2
      ("Attempt to create a coding system alias loop", alias, aliasee);

  for (probe = aliasee;
       SYMBOLP (probe);
       probe = Fgethash (probe, Vcoding_system_hash_table, Qzero))
    {
      if (EQ (probe, alias))
	goto alias_loop;
    }

  Fputhash (alias, aliasee, Vcoding_system_hash_table);

  /* Set up aliases for subsidiaries.
     #### There must be a better way to handle subsidiary coding systems.
     Inquiring Minds Want To Know: shouldn't they always be chains? */
  {
    static const char *suffixes[] = { "-unix", "-dos", "-mac" };
    int i;
    for (i = 0; i < countof (suffixes); i++)
      {
	Lisp_Object alias_subsidiary =
	  add_suffix_to_symbol (alias, suffixes[i]);
	Lisp_Object aliasee_subsidiary =
	  add_suffix_to_symbol (aliasee, suffixes[i]);

	if (! NILP (Ffind_coding_system (aliasee_subsidiary)))
	  Fdefine_coding_system_alias (alias_subsidiary, aliasee_subsidiary);
      }
  }
  /* FSF return value is a vector of [ALIAS-unix ALIAS-dos ALIAS-mac],
     but it doesn't look intentional, so I'd rather return something
     meaningful or nothing at all. */
  return Qnil;
}

static Lisp_Object
subsidiary_coding_system (Lisp_Object coding_system, enum eol_type type)
{
  Lisp_Coding_System *cs = XCODING_SYSTEM (coding_system);
  Lisp_Object new_coding_system;

  switch (type)
    {
    case EOL_AUTODETECT: return coding_system;
    case EOL_LF:   new_coding_system = CODING_SYSTEM_EOL_LF   (cs); break;
    case EOL_CR:   new_coding_system = CODING_SYSTEM_EOL_CR   (cs); break;
    case EOL_CRLF: new_coding_system = CODING_SYSTEM_EOL_CRLF (cs); break;
    default:       ABORT (); return Qnil;
    }

  return NILP (new_coding_system) ? coding_system : new_coding_system;
}

DEFUN ("subsidiary-coding-system", Fsubsidiary_coding_system, 2, 2, 0, /*
Return the subsidiary coding system of CODING-SYSTEM with eol type EOL-TYPE.
The logically opposite operation is `coding-system-base'.
*/
       (coding_system, eol_type))
{
  coding_system = get_coding_system_for_text_file (coding_system, 0);

  return subsidiary_coding_system (coding_system,
				   symbol_to_eol_type (eol_type));
}

DEFUN ("coding-system-base", Fcoding_system_base,
       1, 1, 0, /*
Return the base coding system of CODING-SYSTEM.
If CODING-SYSTEM is a subsidiary, this returns its parent; otherwise, it
returns CODING-SYSTEM.
The logically opposite operation is `subsidiary-coding-system'.
*/
       (coding_system))
{
  Lisp_Object base;

  coding_system = Fget_coding_system (coding_system);
  if (EQ (XCODING_SYSTEM_NAME (coding_system), Qbinary))
    return Fget_coding_system (Qraw_text); /* hack! */
  base = XCODING_SYSTEM_SUBSIDIARY_PARENT (coding_system);
  if (!NILP (base))
    return base;
  return coding_system;
}

DEFUN ("coding-system-used-for-io", Fcoding_system_used_for_io,
       1, 1, 0, /*
Return the coding system actually used for I/O.
In some cases (e.g. when a particular EOL type is specified) this won't be
the coding system itself.  This can be useful when trying to determine
precisely how data was decoded.
*/
       (coding_system))
{
  Lisp_Object canon;

  coding_system = Fget_coding_system (coding_system);
  canon = XCODING_SYSTEM_CANONICAL (coding_system);
  if (!NILP (canon))
    return canon;
  return coding_system;
}


/************************************************************************/
/*                         Coding system accessors                      */
/************************************************************************/

DEFUN ("coding-system-description", Fcoding_system_description, 1, 1, 0, /*
Return the description for CODING-SYSTEM.
The `description' of a coding system is a short English phrase giving the
name rendered according to English punctuation rules, plus possibly some
explanatory text (typically in the form of a parenthetical phrase).  The
description is intended to be short enough that it can appear as a menu item,
and clear enough to be recognizable even to someone who is assumed to have
some basic familiarity with different encodings but may not know all the
technical names; thus, for `cn-gb-2312' is described as "Chinese EUC" and
`hz-gb-2312' is described as "Hz/ZW (Chinese)", where the actual name of
the encoding is given, followed by a note that this is a Chinese encoding,
because the great majority of people encountering this would have no idea
what it is, and giving the language indicates whether the encoding should
just be ignored or (conceivably) investigated more thoroughly.
*/
       (coding_system))
{
  coding_system = Fget_coding_system (coding_system);
  return XCODING_SYSTEM_DESCRIPTION (coding_system);
}

DEFUN ("coding-system-type", Fcoding_system_type, 1, 1, 0, /*
Return the type of CODING-SYSTEM.
*/
       (coding_system))
{
  coding_system = Fget_coding_system (coding_system);
  return XCODING_SYSTEM_TYPE (coding_system);
}

DEFUN ("coding-system-property", Fcoding_system_property, 2, 2, 0, /*
Return the PROP property of CODING-SYSTEM.
*/
       (coding_system, prop))
{
  coding_system = Fget_coding_system (coding_system);
  CHECK_SYMBOL (prop);

  if (EQ (prop, Qname))
    return XCODING_SYSTEM_NAME (coding_system);
  else if (EQ (prop, Qtype))
    return Fcoding_system_type (coding_system);
  else if (EQ (prop, Qdescription))
    return XCODING_SYSTEM_DESCRIPTION (coding_system);
  else if (EQ (prop, Qmnemonic))
    return XCODING_SYSTEM_MNEMONIC (coding_system);
  else if (EQ (prop, Qdocumentation))
    return XCODING_SYSTEM_DOCUMENTATION (coding_system);
  else if (EQ (prop, Qeol_type))
    return eol_type_to_symbol (XCODING_SYSTEM_EOL_TYPE
			       (coding_system));
  else if (EQ (prop, Qeol_lf))
    return XCODING_SYSTEM_EOL_LF (coding_system);
  else if (EQ (prop, Qeol_crlf))
    return XCODING_SYSTEM_EOL_CRLF (coding_system);
  else if (EQ (prop, Qeol_cr))
    return XCODING_SYSTEM_EOL_CR (coding_system);
  else if (EQ (prop, Qpost_read_conversion))
    return XCODING_SYSTEM_POST_READ_CONVERSION (coding_system);
  else if (EQ (prop, Qpre_write_conversion))
    return XCODING_SYSTEM_PRE_WRITE_CONVERSION (coding_system);
  else
    {
      Lisp_Object value = CODESYSMETH_OR_GIVEN (XCODING_SYSTEM (coding_system),
						getprop,
						(coding_system, prop),
						Qunbound);
      if (UNBOUNDP (value))
	invalid_constant ("Unrecognized property", prop);
      return value;
    }
}


/************************************************************************/
/*                       Coding stream functions                        */
/************************************************************************/

/* A coding stream is a stream used for encoding or decoding text.  The
   coding-stream object keeps track of the actual coding system, the stream
   that is at the other end, and data that needs to be persistent across
   the lifetime of the stream. */

extern const struct sized_memory_description chain_coding_stream_description;
extern const struct sized_memory_description undecided_coding_stream_description;

static const struct memory_description coding_stream_data_description_1 []= {
  { XD_BLOCK_PTR, chain_coding_system, 1,
    { &chain_coding_stream_description } },
  { XD_BLOCK_PTR, undecided_coding_system, 1,
    { &undecided_coding_stream_description } },
  { XD_END }
};

static const struct sized_memory_description coding_stream_data_description = {
  sizeof (void *), coding_stream_data_description_1
};

static const struct memory_description coding_lstream_description[] = {
  { XD_INT, offsetof (struct coding_stream, type) },
  { XD_LISP_OBJECT, offsetof (struct coding_stream, orig_codesys) },
  { XD_LISP_OBJECT, offsetof (struct coding_stream, codesys) },
  { XD_LISP_OBJECT, offsetof (struct coding_stream, other_end) },
  { XD_UNION, offsetof (struct coding_stream, data), 
    XD_INDIRECT (0, 0), { &coding_stream_data_description } },
  { XD_END }
};

DEFINE_LSTREAM_IMPLEMENTATION_WITH_DATA ("coding", coding);

/* Encoding and decoding are parallel operations, so we create just one
   stream for both. "Decoding" may involve the extra step of autodetection
   of the data format, but that's only because of the conventional
   definition of decoding as converting from external- to
   internal-formatted data.

   [[ REWRITE ME! ]]

   #### We really need to abstract out the concept of "data formats" and
   define "converters" that convert from and to specified formats,
   eliminating the idea of decoding and encoding.  When specifying a
   conversion process, we need to give the data formats themselves, not the
   conversion processes -- e.g. a coding system called "Unicode->multibyte"
   converts in both directions, and we could auto-detect the format of data
   at either end. */

static Bytecount
coding_reader (Lstream *stream, unsigned char *data, Bytecount size)
{
  unsigned char *orig_data = data;
  Bytecount read_size;
  int error_occurred = 0;
  struct coding_stream *str = CODING_STREAM_DATA (stream);

  /* We need to interface to coding_{de,en}code_1(), which expects to take
     some amount of data and store the result into a Dynarr.  We have
     coding_{de,en}code_1() store into c->runoff, and take data from there
     as necessary. */

  /* We loop until we have enough data, reading chunks from the other
     end and converting it. */
  while (1)
    {
      /* Take data from convert_to if we can.  Make sure to take at
	 most SIZE bytes, and delete the data from convert_to. */
      if (Dynarr_length (str->convert_to) > 0)
	{
	  Bytecount chunk =
	    min (size, (Bytecount) Dynarr_length (str->convert_to));
	  memcpy (data, Dynarr_atp (str->convert_to, 0), chunk);
	  Dynarr_delete_many (str->convert_to, 0, chunk);
	  data += chunk;
	  size -= chunk;
	}

      if (size == 0)
	break; /* No more room for data */

      if (str->eof)
	break;

      {
	/* Exhausted convert_to, so get some more.  Read into convert_from,
           after existing "rejected" data from the last conversion. */
	Bytecount rejected = Dynarr_length (str->convert_from);
	/* #### 1024 is arbitrary; we really need to separate 0 from EOF,
           and when we get 0, keep taking more data until we don't get 0 --
           we don't know how much data the conversion routine might need
           before it can generate any data of its own (eg, bzip2). */
	Bytecount readmore =
	  str->one_byte_at_a_time ? (Bytecount) 1 :
	    max (size, (Bytecount) 1024);

	Dynarr_add_many (str->convert_from, 0, readmore);
	read_size = Lstream_read (str->other_end,
				  Dynarr_atp (str->convert_from, rejected),
				  readmore);
	/* Trim size down to how much we actually got */
	Dynarr_set_size (str->convert_from, rejected + max (0, read_size));
      }

      if (read_size < 0) /* LSTREAM_ERROR */
	{
	  error_occurred = 1;
	  break;
	}
      if (read_size == 0) /* LSTREAM_EOF */
	/* There might be some more end data produced in the translation,
	   so we set a flag and call the conversion method once more to
	   output any final stuff it may be holding, any "go back to a sane
	   state" escape sequences, etc.  The conversion method is free to
	   look at this flag, and we use it above to stop looping. */
	str->eof = 1;
      {
	Bytecount processed;
	Bytecount to_process = Dynarr_length (str->convert_from);

	/* Convert the data, and save any rejected data in convert_from */
	processed =
	  XCODESYSMETH (str->codesys, convert,
			(str, Dynarr_atp (str->convert_from, 0),
			 str->convert_to, to_process));
	if (processed < 0)
	  {
	    error_occurred = 1;
	    break;
	  }
	assert (processed <= to_process);
	if (processed < to_process)
	  memmove (Dynarr_atp (str->convert_from, 0),
		   Dynarr_atp (str->convert_from, processed),
		   to_process - processed);
	Dynarr_set_size (str->convert_from, to_process - processed);
      }
    }

  if (data - orig_data == 0)
    return error_occurred ? -1 : 0;
  else
    return data - orig_data;
}

static Bytecount
coding_writer (Lstream *stream, const unsigned char *data, Bytecount size)
{
  struct coding_stream *str = CODING_STREAM_DATA (stream);

  /* Convert all our data into convert_to, and then attempt to write
     it all out to the other end. */
  Dynarr_reset (str->convert_to);
  size = XCODESYSMETH (str->codesys, convert,
		       (str, data, str->convert_to, size));
  if (Lstream_write (str->other_end, Dynarr_atp (str->convert_to, 0),
		     Dynarr_length (str->convert_to)) < 0)
    return -1;
  else
    /* The return value indicates how much of the incoming data was
       processed, not how many bytes were written. */
    return size;
}

static int
encode_decode_source_sink_type_is_char (Lisp_Object cs,
					enum source_or_sink sex,
					enum encode_decode direction)
{
  return (direction == CODING_DECODE ?
	  decoding_source_sink_type_is_char (cs, sex) :
	  encoding_source_sink_type_is_char (cs, sex));
}

/* Ensure that the convert methods only get full characters sent to them to
   convert if the source of that conversion is characters; and that no such
   full-character checking happens when the source is bytes.  Keep in mind
   that (1) the conversion_end_type return values take the perspective of
   encoding; (2) the source for decoding is the same as the sink for
   encoding; (3) when writing, the data is given to us, and we set our own
   stream to be character mode or not; (4) when reading, the data comes
   from the other_end stream, and we set that one to be character mode or
   not.  This is consistent with the comment above the prototype for
   Lstream_set_character_mode(), which lays out rules for who is allowed to
   modify the character type mode on a stream.

   If we're a read stream, we're always setting character mode on the
   source, but we also set it on ourselves consistent with the flag that
   can disable this (see again the comment above
   Lstream_set_character_mode()).
 */

static void
set_coding_character_mode (Lstream *stream)
{
  struct coding_stream *str = CODING_STREAM_DATA (stream);
  Lstream *stream_to_set =
    stream->flags & LSTREAM_FL_WRITE ? stream : str->other_end;
  if (encode_decode_source_sink_type_is_char
      (str->codesys, CODING_SOURCE, str->direction))
    Lstream_set_character_mode (stream_to_set);
  else
    Lstream_unset_character_mode (stream_to_set);
  if (str->set_char_mode_on_us_when_reading &&
      (stream->flags & LSTREAM_FL_READ))
    {
      if (encode_decode_source_sink_type_is_char
	  (str->codesys, CODING_SINK, str->direction))
	Lstream_set_character_mode (stream);
      else
	Lstream_unset_character_mode (stream);
    }
}

static Lisp_Object
coding_marker (Lisp_Object stream)
{
  struct coding_stream *str = CODING_STREAM_DATA (XLSTREAM (stream));

  mark_object (str->orig_codesys);
  mark_object (str->codesys);
  MAYBE_XCODESYSMETH (str->codesys, mark_coding_stream, (str));
  return wrap_lstream (str->other_end);
}

static int
coding_rewinder (Lstream *stream)
{
  struct coding_stream *str = CODING_STREAM_DATA (stream);
  MAYBE_XCODESYSMETH (str->codesys, rewind_coding_stream, (str));

  str->ch = 0;
  Dynarr_reset (str->convert_to);
  Dynarr_reset (str->convert_from);
  return Lstream_rewind (str->other_end);
}

static int
coding_seekable_p (Lstream *stream)
{
  struct coding_stream *str = CODING_STREAM_DATA (stream);
  return Lstream_seekable_p (str->other_end);
}

static int
coding_flusher (Lstream *stream)
{
  struct coding_stream *str = CODING_STREAM_DATA (stream);
  return Lstream_flush (str->other_end);
}

static int
coding_closer (Lstream *stream)
{
  struct coding_stream *str = CODING_STREAM_DATA (stream);
  if (stream->flags & LSTREAM_FL_WRITE)
    {
      str->eof = 1;
      coding_writer (stream, 0, 0);
      str->eof = 0;
    }
  /* It's safe to free the runoff dynarrs now because they are used only
     during conversion.  We need to keep the type-specific data around,
     though, because of canonicalize_after_coding. */
  if (str->convert_to)
    {
      Dynarr_free (str->convert_to);
      str->convert_to = 0;
    }
  if (str->convert_from)
    {
      Dynarr_free (str->convert_from);
      str->convert_from = 0;
    }

  if (str->no_close_other)
    return Lstream_flush (str->other_end);
  else
    return Lstream_close (str->other_end);
}

static void
coding_finalizer (Lstream *stream)
{
  struct coding_stream *str = CODING_STREAM_DATA (stream);

  assert (!str->finalized);
  MAYBE_XCODESYSMETH (str->codesys, finalize_coding_stream, (str));
  if (str->data)
    {
      xfree (str->data, void *);
      str->data = 0;
    }
  str->finalized = 1;
}

static Lisp_Object
coding_stream_canonicalize_after_coding (Lstream *stream)
{
  struct coding_stream *str = CODING_STREAM_DATA (stream);

  return XCODESYSMETH_OR_GIVEN (str->codesys, canonicalize_after_coding,
				(str), str->codesys);
}

Lisp_Object
coding_stream_detected_coding_system (Lstream *stream)
{
  Lisp_Object codesys =
    coding_stream_canonicalize_after_coding (stream);
  if (NILP (codesys))
    return Fget_coding_system (Qidentity);
  return codesys;
}

Lisp_Object
coding_stream_coding_system (Lstream *stream)
{
  return CODING_STREAM_DATA (stream)->codesys;
}

/* Change the coding system associated with a stream. */

void
set_coding_stream_coding_system (Lstream *lstr, Lisp_Object codesys)
{
  struct coding_stream *str = CODING_STREAM_DATA (lstr);
  if (EQ (str->orig_codesys, codesys))
    return;
  /* We do the equivalent of closing the stream, destroying it, and
     reinitializing it.  This includes flushing out the data and signalling
     EOF, if we're a writing stream; we also replace the type-specific data
     with the data appropriate for the new coding system. */
  if (!NILP (str->codesys))
    {
      if (lstr->flags & LSTREAM_FL_WRITE)
	{
	  Lstream_flush (lstr);
	  str->eof = 1;
	  coding_writer (lstr, 0, 0);
	  str->eof = 0;
	}
      MAYBE_XCODESYSMETH (str->codesys, finalize_coding_stream, (str));
    }
  str->orig_codesys = codesys;
  str->codesys = coding_system_real_canonical (codesys);
  
  if (str->data)
    {
      xfree (str->data, void *);
      str->data = 0;
    }
  if (XCODING_SYSTEM_METHODS (str->codesys)->coding_data_size)
    {
      str->data =
	xmalloc_and_zero (XCODING_SYSTEM_METHODS (str->codesys)->
			  coding_data_size);
      str->type = XCODING_SYSTEM_METHODS (str->codesys)->enumtype;
    }
  MAYBE_XCODESYSMETH (str->codesys, init_coding_stream, (str));
  /* The new coding system may have different ideas regarding whether its
     ends are characters or bytes. */
  set_coding_character_mode (lstr);
}

/* WARNING WARNING WARNING WARNING!!!!!  If you open up a coding
   stream for writing, no automatic code detection will be performed.
   The reason for this is that automatic code detection requires a
   seekable input.  Things will also fail if you open a coding
   stream for reading using a non-fully-specified coding system and
   a non-seekable input stream. */

static Lisp_Object
make_coding_stream_1 (Lstream *stream, Lisp_Object codesys,
		      const char *mode, enum encode_decode direction,
		      int flags)
{
  Lstream *lstr = Lstream_new (lstream_coding, mode);
  struct coding_stream *str = CODING_STREAM_DATA (lstr);

  codesys = Fget_coding_system (codesys);
  xzero (*str);
  str->codesys = Qnil;
  str->orig_codesys = Qnil;
  str->us = lstr;
  str->other_end = stream;
  str->convert_to = Dynarr_new (unsigned_char);
  str->convert_from = Dynarr_new (unsigned_char);
  str->direction = direction;
  if (flags & LSTREAM_FL_NO_CLOSE_OTHER)
    str->no_close_other = 1;
  if (flags & LSTREAM_FL_READ_ONE_BYTE_AT_A_TIME)
    str->one_byte_at_a_time = 1;
  if (!(flags & LSTREAM_FL_NO_INIT_CHAR_MODE_WHEN_READING))
    str->set_char_mode_on_us_when_reading = 1;
    
  set_coding_stream_coding_system (lstr, codesys);
  return wrap_lstream (lstr);
}

/* FLAGS:

   LSTREAM_FL_NO_CLOSE_OTHER
     Don't close STREAM (the stream at the other end) when this stream is
     closed.

   LSTREAM_FL_READ_ONE_BYTE_AT_A_TIME
     When reading from STREAM, read and process one byte at a time rather
     than in large chunks.  This is for reading from TTY's, so we don't
     block.  #### We should instead create a non-blocking filedesc stream
     that emulates the behavior as necessary using select(), when the
     fcntls don't work. (As seems to be the case on Cygwin.)

  LSTREAM_FL_NO_INIT_CHAR_MODE_WHEN_READING
     When reading from STREAM, read and process one byte at a time rather
     than in large chunks.  This is for reading from TTY's, so we don't
     block.  #### We should instead create a non-blocking filedesc stream
     that emulates the behavior as necessary using select(), when the
     fcntls don't work. (As seems to be the case on Cygwin.)
*/
Lisp_Object
make_coding_input_stream (Lstream *stream, Lisp_Object codesys,
			  enum encode_decode direction, int flags)
{
  return make_coding_stream_1 (stream, codesys, "r", direction,
                               flags);
}

/* FLAGS:

   LSTREAM_FL_NO_CLOSE_OTHER
     Don't close STREAM (the stream at the other end) when this stream is
     closed.
 */
Lisp_Object
make_coding_output_stream (Lstream *stream, Lisp_Object codesys,
			  enum encode_decode direction, int flags)
{
  return make_coding_stream_1 (stream, codesys, "w", direction,
                               flags);
}

static Lisp_Object
encode_decode_coding_region (Lisp_Object start, Lisp_Object end,
			     Lisp_Object coding_system, Lisp_Object buffer,
			     enum encode_decode direction)
{
  Charbpos b, e;
  struct buffer *buf = decode_buffer (buffer, 0);
  Lisp_Object instream = Qnil, to_outstream = Qnil, outstream = Qnil;
  Lisp_Object from_outstream = Qnil, auto_outstream = Qnil;
  Lisp_Object lb_outstream = Qnil;
  Lisp_Object next;
  Lstream *istr, *ostr;
  struct gcpro gcpro1, gcpro2, gcpro3, gcpro4, gcpro5;
  struct gcpro ngcpro1;
  int source_char, sink_char;

  get_buffer_range_char (buf, start, end, &b, &e, 0);
  barf_if_buffer_read_only (buf, b, e);

  GCPRO5 (instream, to_outstream, outstream, from_outstream, lb_outstream);
  NGCPRO1 (auto_outstream);

  coding_system = Fget_coding_system (coding_system);
  source_char = encode_decode_source_sink_type_is_char (coding_system,
							CODING_SOURCE,
							direction);
  sink_char = encode_decode_source_sink_type_is_char (coding_system,
						      CODING_SINK,
						      direction);

  /* Order is IN <---> [TO] -> OUT -> [FROM] -> [AUTODETECT-EOL] -> LB */
  instream  = make_lisp_buffer_input_stream  (buf, b, e, 0);
  next = lb_outstream = make_lisp_buffer_output_stream (buf, b, 0);
  
  if (direction == CODING_DECODE &&
      XCODING_SYSTEM_EOL_TYPE (coding_system) == EOL_AUTODETECT)
    next = auto_outstream =
      make_coding_output_stream
	(XLSTREAM (next), Fget_coding_system (Qconvert_eol_autodetect),
	 CODING_DECODE, 0);
    
  if (!sink_char)
    next = from_outstream =
      make_coding_output_stream (XLSTREAM (next), Qbinary, CODING_DECODE, 0);
  outstream = make_coding_output_stream (XLSTREAM (next), coding_system,
					 direction, 0);
  if (!source_char)
    {
      to_outstream =
	make_coding_output_stream (XLSTREAM (outstream),
				   Qbinary, CODING_ENCODE, 0);
      ostr = XLSTREAM (to_outstream);
    }
  else
    ostr = XLSTREAM (outstream);
  istr = XLSTREAM (instream);

  /* The chain of streams looks like this:

     [BUFFER] <----- (( read from/send to loop ))
                     ------> [CHAR->BYTE i.e. ENCODE AS BINARY if source is
                              in bytes]
		             ------> [ENCODE/DECODE AS SPECIFIED]
			             ------> [BYTE->CHAR i.e. DECODE AS BINARY
                                              if sink is in bytes]
					     ------> [AUTODETECT EOL if
					              we're decoding and
						      coding system calls
						      for this]
			                              ------> [BUFFER]
   */

  /* #### See comment

     EFFICIENCY OF CODING CONVERSION WITH MULTIPLE COPIES/CHAINS

     in text.c.
  */

  while (1)
    {
      char tempbuf[1024]; /* some random amount */
      Charbpos newpos, even_newer_pos;
      Charbpos oldpos = lisp_buffer_stream_startpos (istr);
      Bytecount size_in_bytes =
	Lstream_read (istr, tempbuf, sizeof (tempbuf));

      if (!size_in_bytes)
	break;
      newpos = lisp_buffer_stream_startpos (istr);
      Lstream_write (ostr, tempbuf, size_in_bytes);
      even_newer_pos = lisp_buffer_stream_startpos (istr);
      buffer_delete_range (buf, even_newer_pos - (newpos - oldpos),
			   even_newer_pos, 0);
    }

  {
    Charcount retlen =
      lisp_buffer_stream_startpos (XLSTREAM (instream)) - b;
    Lstream_close (istr);
    Lstream_close (ostr);
    NUNGCPRO;
    UNGCPRO;
    Lstream_delete (istr);
    if (!NILP (from_outstream))
      Lstream_delete (XLSTREAM (from_outstream));
    Lstream_delete (XLSTREAM (outstream));
    if (!NILP (to_outstream))
      Lstream_delete (XLSTREAM (to_outstream));
    if (!NILP (auto_outstream))
      Lstream_delete (XLSTREAM (auto_outstream));
    Lstream_delete (XLSTREAM (lb_outstream));
    return make_int (retlen);
  }
}

DEFUN ("decode-coding-region", Fdecode_coding_region, 3, 4, 0, /*
Decode the text between START and END which is encoded in CODING-SYSTEM.
This is useful if you've read in encoded text from a file without decoding
it (e.g. you read in a JIS-formatted file but used the `binary' or
`no-conversion' coding system, so that it shows up as "^[$B!<!+^[(B").
Return length of decoded text.
BUFFER defaults to the current buffer if unspecified.
*/
       (start, end, coding_system, buffer))
{
  return encode_decode_coding_region (start, end, coding_system, buffer,
				      CODING_DECODE);
}

DEFUN ("encode-coding-region", Fencode_coding_region, 3, 4, 0, /*
Encode the text between START and END using CODING-SYSTEM.
This will, for example, convert Japanese characters into stuff such as
"^[$B!<!+^[(B" if you use the JIS encoding.  Return length of encoded
text.  BUFFER defaults to the current buffer if unspecified.
*/
       (start, end, coding_system, buffer))
{
  return encode_decode_coding_region (start, end, coding_system, buffer,
				      CODING_ENCODE);
}


/************************************************************************/
/*                             Chain methods                            */
/************************************************************************/

/* #### Need a way to create "opposite-direction" coding systems. */

/* Chain two or more coding systems together to make a combination coding
   system. */

struct chain_coding_system
{
  /* List of coding systems, in decode order */
  Lisp_Object *chain;
  /* Number of coding systems in list */
  int count;
  /* Coding system to return as a result of canonicalize-after-coding */
  Lisp_Object canonicalize_after_coding;
};

struct chain_coding_stream
{
  int initted;
  /* Lstreams for chain coding system */
  Lisp_Object *lstreams;
  int lstream_count;
};

static const struct memory_description chain_coding_system_description[] = {
  { XD_INT, offsetof (struct chain_coding_system, count) },
  { XD_BLOCK_PTR, offsetof (struct chain_coding_system, chain),
    XD_INDIRECT (0, 0), { &lisp_object_description } },
  { XD_LISP_OBJECT, offsetof (struct chain_coding_system,
			      canonicalize_after_coding) },
  { XD_END }
};

static const struct memory_description chain_coding_stream_description_1 [] = {
  { XD_INT, offsetof (struct chain_coding_stream, lstream_count) },
  { XD_BLOCK_PTR, offsetof (struct chain_coding_stream, lstreams),
    XD_INDIRECT (0, 0), { &lisp_object_description } },
  { XD_END }
};

const struct sized_memory_description chain_coding_stream_description = {
  sizeof (struct chain_coding_stream), chain_coding_stream_description_1
};

DEFINE_CODING_SYSTEM_TYPE_WITH_DATA (chain);

static Lisp_Object
chain_canonicalize (Lisp_Object codesys)
{
  /* We make use of the fact that this method is called at init time, after
     properties have been parsed.  init_method is called too early. */
  /* #### It's not clear we need this whole chain-canonicalize mechanism
     any more. */
  Lisp_Object chain = Flist (XCODING_SYSTEM_CHAIN_COUNT (codesys),
			     XCODING_SYSTEM_CHAIN_CHAIN (codesys));
  chain = Fcons (XCODING_SYSTEM_PRE_WRITE_CONVERSION (codesys),
		 Fcons (XCODING_SYSTEM_POST_READ_CONVERSION (codesys),
			chain));
  Fputhash (chain, codesys, Vchain_canonicalize_hash_table);
  return codesys;
}

static Lisp_Object
chain_canonicalize_after_coding (struct coding_stream *str)
{
  Lisp_Object cac =
    XCODING_SYSTEM_CHAIN_CANONICALIZE_AFTER_CODING (str->codesys);
  if (!NILP (cac))
    return cac;
  return str->codesys;
#if 0
  struct chain_coding_stream *data = CODING_STREAM_TYPE_DATA (str, chain);
  Lisp_Object us = str->codesys, codesys;
  int i;
  Lisp_Object chain;
  Lisp_Object tail;
  int changed = 0;

  /* #### It's not clear we need this whole chain-canonicalize mechanism
     any more. */
  if (str->direction == CODING_ENCODE || !data->initted)
    return us;
  
  chain = Flist (XCODING_SYSTEM_CHAIN_COUNT (us),
		 XCODING_SYSTEM_CHAIN_CHAIN (us));

  tail = chain;
  for (i = 0; i < XCODING_SYSTEM_CHAIN_COUNT (us); i++)
    {
      codesys = (coding_stream_canonicalize_after_coding
		 (XLSTREAM (data->lstreams[i])));
      if (!EQ (codesys, XCAR (tail)))
	changed = 1;
      XCAR (tail) = codesys;
      tail = XCDR (tail);
    }

  if (!changed)
    return us;

  chain = delq_no_quit (Qnil, chain);
  
  if (NILP (XCODING_SYSTEM_PRE_WRITE_CONVERSION (us)) &&
      NILP (XCODING_SYSTEM_POST_READ_CONVERSION (us)))
    {
      if (NILP (chain))
	return Qnil;
      if (NILP (XCDR (chain)))
	return XCAR (chain);
    }
  
  codesys = Fgethash (Fcons (XCODING_SYSTEM_PRE_WRITE_CONVERSION (us),
			     Fcons (XCODING_SYSTEM_POST_READ_CONVERSION (us),
				    chain)), Vchain_canonicalize_hash_table,
		      Qnil);
  if (!NILP (codesys))
    return codesys;
  return make_internal_coding_system
    (us, "internal-chain-canonicalizer-wrapper",
     Qchain, Qunbound, list2 (Qchain, chain));
#endif /* 0 */
}

static void
chain_init (Lisp_Object codesys)
{
  XCODING_SYSTEM_CHAIN_CANONICALIZE_AFTER_CODING (codesys) = Qnil;
}

static void
chain_mark (Lisp_Object codesys)
{
  int i;

  for (i = 0; i < XCODING_SYSTEM_CHAIN_COUNT (codesys); i++)
    mark_object (XCODING_SYSTEM_CHAIN_CHAIN (codesys)[i]);
  mark_object (XCODING_SYSTEM_CHAIN_CANONICALIZE_AFTER_CODING (codesys));
}

static void
chain_mark_coding_stream_1 (struct chain_coding_stream *data)
{
  int i;

  for (i = 0; i < data->lstream_count; i++)
    mark_object (data->lstreams[i]);
}

static void
chain_mark_coding_stream (struct coding_stream *str)
{
  chain_mark_coding_stream_1 (CODING_STREAM_TYPE_DATA (str, chain));
}

static void
chain_print (Lisp_Object cs, Lisp_Object printcharfun, int escapeflag)
{
  int i;

  write_c_string (printcharfun, "(");
  for (i = 0; i < XCODING_SYSTEM_CHAIN_COUNT (cs); i++)
    {
      write_c_string (printcharfun, i == 0 ? "" : "->");
      print_coding_system_in_print_method (XCODING_SYSTEM_CHAIN_CHAIN (cs)[i],
					   printcharfun, escapeflag);
    }
  {
    Lisp_Object cac = XCODING_SYSTEM_CHAIN_CANONICALIZE_AFTER_CODING (cs);
    if (!NILP (cac))
      {
	if (i > 0)
	  write_c_string (printcharfun, " ");
	write_c_string (printcharfun, "canonicalize-after-coding=");
	print_coding_system_in_print_method (cac, printcharfun, escapeflag);
      }
  }

  write_c_string (printcharfun, ")");
}

static void
chain_rewind_coding_stream_1 (struct chain_coding_stream *data)
{
  /* Each will rewind the next; there is always at least one stream (the
     dynarr stream at the end) if we're initted */
  if (data->initted)
    Lstream_rewind (XLSTREAM (data->lstreams[0]));
}

static void
chain_rewind_coding_stream (struct coding_stream *str)
{
  chain_rewind_coding_stream_1 (CODING_STREAM_TYPE_DATA (str, chain));
}

static void
chain_init_coding_streams_1 (struct chain_coding_stream *data,
			     unsigned_char_dynarr *dst,
			     int ncodesys, Lisp_Object *codesys,
			     enum encode_decode direction)
{
  int i;
  Lisp_Object lstream_out;

  data->lstream_count = ncodesys + 1;
  data->lstreams = xnew_array (Lisp_Object, data->lstream_count);

  lstream_out = make_dynarr_output_stream (dst);
  Lstream_set_buffering (XLSTREAM (lstream_out), LSTREAM_UNBUFFERED, 0);
  data->lstreams[data->lstream_count - 1] = lstream_out;

  for (i = ncodesys - 1; i >= 0; i--)
    {
      data->lstreams[i] =
	make_coding_output_stream
	  (XLSTREAM (lstream_out),
	   codesys[direction == CODING_ENCODE ? ncodesys - (i + 1) : i],
	   direction, 0);
      lstream_out = data->lstreams[i];
      Lstream_set_buffering (XLSTREAM (lstream_out), LSTREAM_UNBUFFERED,
			     0);
    }
  data->initted = 1;
}

static Bytecount
chain_convert (struct coding_stream *str, const UExtbyte *src,
	       unsigned_char_dynarr *dst, Bytecount n)
{
  struct chain_coding_stream *data = CODING_STREAM_TYPE_DATA (str, chain);

  if (str->eof)
    {
      /* Each will close the next; there is always at least one stream (the
	 dynarr stream at the end) if we're initted.  We need to close now
	 because more data may be generated. */
      if (data->initted)
	Lstream_close (XLSTREAM (data->lstreams[0]));
      return n;
    }

  if (!data->initted)
    chain_init_coding_streams_1
      (data, dst, XCODING_SYSTEM_CHAIN_COUNT (str->codesys),
       XCODING_SYSTEM_CHAIN_CHAIN (str->codesys), str->direction);

  if (Lstream_write (XLSTREAM (data->lstreams[0]), src, n) < 0)
    return -1;
  return n;
}

static void
chain_finalize_coding_stream_1 (struct chain_coding_stream *data)
{  
  if (data->lstreams)
    {
      /* During GC, these objects are unmarked, and are about to be freed.
	 We do NOT want them on the free list, and that will cause lots of
	 nastiness including crashes.  Just let them be freed normally. */
      if (!gc_in_progress)
	{
	  int i;
	  /* Order of deletion is important here!  Delete from the head of
	     the chain and work your way towards the tail.  In general,
	     when you delete an object, there should be *NO* pointers to it
	     anywhere.  Deleting back-to-front would be a problem because
	     there are pointers going forward.  If there were pointers in
	     both directions, you'd have to disconnect the pointers to a
	     particular object before deleting it. */
	  for (i = 0; i < data->lstream_count; i++)
	    Lstream_delete (XLSTREAM ((data->lstreams)[i]));
	}
      xfree (data->lstreams, Lisp_Object *);
    }
}

static void
chain_finalize_coding_stream (struct coding_stream *str)
{
  chain_finalize_coding_stream_1 (CODING_STREAM_TYPE_DATA (str, chain));
}

static void
chain_finalize (Lisp_Object c)
{
  if (XCODING_SYSTEM_CHAIN_CHAIN (c))
    xfree (XCODING_SYSTEM_CHAIN_CHAIN (c), Lisp_Object *);
}

static int
chain_putprop (Lisp_Object codesys, Lisp_Object key, Lisp_Object value)
{
  if (EQ (key, Qchain))
    {
      Lisp_Object *cslist;
      int count = 0;
      int i;

      {
	EXTERNAL_LIST_LOOP_2 (elt, value)
	  {
	    Fget_coding_system (elt);
	    count++;
	  }
      }

      cslist = xnew_array (Lisp_Object, count);
      XCODING_SYSTEM_CHAIN_CHAIN (codesys) = cslist;

      count = 0;
      {
	EXTERNAL_LIST_LOOP_2 (elt, value)
	  {
	    cslist[count] = Fget_coding_system (elt);
	    count++;
	  }
      }

      XCODING_SYSTEM_CHAIN_COUNT (codesys) = count;

      for (i = 0; i < count - 1; i++)
	{
	  if (decoding_source_sink_type_is_char (cslist[i], CODING_SINK) !=
	      decoding_source_sink_type_is_char (cslist[i + 1], CODING_SOURCE))
	    invalid_argument_2 ("Sink of first must match source of second",
			        cslist[i], cslist[i + 1]);
	}
    }
  else if (EQ (key, Qcanonicalize_after_coding))
    XCODING_SYSTEM_CHAIN_CANONICALIZE_AFTER_CODING (codesys) =
      Fget_coding_system (value);
  else
    return 0;
  return 1;
}

static Lisp_Object
chain_getprop (Lisp_Object coding_system, Lisp_Object prop)
{
  if (EQ (prop, Qchain))
    {
      Lisp_Object result = Qnil;
      int i;

      for (i = 0; i < XCODING_SYSTEM_CHAIN_COUNT (coding_system); i++)
	result = Fcons (XCODING_SYSTEM_CHAIN_CHAIN (coding_system)[i],
			result);

      return Fnreverse (result);
    }
  else if (EQ (prop, Qcanonicalize_after_coding))
    return XCODING_SYSTEM_CHAIN_CANONICALIZE_AFTER_CODING (coding_system);
  else
    return Qunbound;
}

static enum source_sink_type
chain_conversion_end_type (Lisp_Object codesys)
{
  Lisp_Object *cslist = XCODING_SYSTEM_CHAIN_CHAIN (codesys);
  int n = XCODING_SYSTEM_CHAIN_COUNT (codesys);
  int charp_source, charp_sink;

  if (n == 0)
    return DECODES_BYTE_TO_BYTE; /* arbitrary */
  charp_source = decoding_source_sink_type_is_char (cslist[0], CODING_SOURCE);
  charp_sink = decoding_source_sink_type_is_char (cslist[n - 1], CODING_SINK);

  switch (charp_source * 2 + charp_sink)
    {
    case 0: return DECODES_BYTE_TO_BYTE;
    case 1: return DECODES_BYTE_TO_CHARACTER;
    case 2: return DECODES_CHARACTER_TO_BYTE;
    case 3: return DECODES_CHARACTER_TO_CHARACTER;
    }

  ABORT ();
  return DECODES_BYTE_TO_BYTE;
}


/************************************************************************/
/*                     No-conversion methods                            */
/************************************************************************/

/* "No conversion"; used for binary files.  We use quotes because there
   really is some conversion being applied (it does byte<->char
   conversion), but it appears to the user as if the text is read in
   without conversion.

   #### Shouldn't we _call_ it that, then?  And while we're at it,
   separate it into "to_internal" and "to_external"? */
DEFINE_CODING_SYSTEM_TYPE (no_conversion);

/* This is used when reading in "binary" files -- i.e. files that may
   contain all 256 possible byte values and that are not to be
   interpreted as being in any particular encoding. */
static Bytecount
no_conversion_convert (struct coding_stream *str,
		       const UExtbyte *src,
		       unsigned_char_dynarr *dst, Bytecount n)
{
  UExtbyte c;
  unsigned int ch     = str->ch;
  Bytecount orign = n;

  if (str->direction == CODING_DECODE)
    {
      while (n--)
	{
	  c = *src++;

	  DECODE_ADD_BINARY_CHAR (c, dst);
	}

      if (str->eof)
	DECODE_OUTPUT_PARTIAL_CHAR (ch, dst);
    }
  else
    {

      while (n--)
	{
	  c = *src++;
	  if (byte_ascii_p (c))
	    {
	      assert (ch == 0);
	      Dynarr_add (dst, c);
	    }
#ifdef MULE
	  else if (ibyte_leading_byte_p (c))
	    {
	      assert (ch == 0);
	      if (c == LEADING_BYTE_LATIN_ISO8859_1 ||
		  c == LEADING_BYTE_CONTROL_1)
		ch = c;
	      else
		/* #### This is just plain unacceptable. */
		Dynarr_add (dst, '~'); /* untranslatable character */
	    }
	  else
	    {
	      if (ch == LEADING_BYTE_LATIN_ISO8859_1)
		Dynarr_add (dst, c);
	      else if (ch == LEADING_BYTE_CONTROL_1)
		{
		  assert (c < 0xC0);
		  Dynarr_add (dst, c - 0x20);
		}
	      /* else it should be the second or third byte of an
		 untranslatable character, so ignore it */
	      ch = 0;
	    }
#endif /* MULE */

	}
    }

  str->ch    = ch;
  return orign;
}

DEFINE_DETECTOR (no_conversion);
DEFINE_DETECTOR_CATEGORY (no_conversion, no_conversion);

struct no_conversion_detector
{
  int dummy;
};

static void
no_conversion_detect (struct detection_state *st, const UExtbyte *UNUSED (src),
		      Bytecount UNUSED (n))
{
  /* Hack until we get better handling of this stuff! */
  DET_RESULT (st, no_conversion) = DET_SLIGHTLY_LIKELY;
}


/************************************************************************/
/*                       Convert-eol methods                            */
/************************************************************************/

/* This is used to handle end-of-line (EOL) differences.  It is
character-to-character, and works (when encoding) *BEFORE* sending data to
the main encoding routine -- thus, that routine must handle different EOL
types itself if it does line-oriented type processing.  This is unavoidable
because we don't know whether the output of the main encoding routine is
ASCII compatible (UTF-16 is definitely not, for example).  [[ sjt sez this
is bogus.  There should be _no_ EOL processing (or processing of any kind)
after conversion to external. ]]

There is one parameter: `subtype', either `cr', `lf', `crlf', or nil.
*/

struct convert_eol_coding_system
{
  enum eol_type subtype;
  int dummy; /* On some architectures (eg ia64) the portable dumper can
                produce unaligned access errors without this field.  Probably
                because the combined structure of this structure and
                Lisp_Coding_System is not properly aligned. */
};

#define CODING_SYSTEM_CONVERT_EOL_SUBTYPE(codesys) \
  (CODING_SYSTEM_TYPE_DATA (codesys, convert_eol)->subtype)
#define XCODING_SYSTEM_CONVERT_EOL_SUBTYPE(codesys) \
  (XCODING_SYSTEM_TYPE_DATA (codesys, convert_eol)->subtype)

struct convert_eol_coding_stream
{
  enum eol_type actual;
};

static const struct memory_description
  convert_eol_coding_system_description[] = {
  { XD_END }
};

DEFINE_CODING_SYSTEM_TYPE_WITH_DATA (convert_eol);

static void
convert_eol_print (Lisp_Object cs, Lisp_Object printcharfun,
		   int UNUSED (escapeflag))
{
  struct convert_eol_coding_system *data =
    XCODING_SYSTEM_TYPE_DATA (cs, convert_eol);

  write_fmt_string (printcharfun, "(%s)",
		    data->subtype == EOL_LF ? "lf" :
		    data->subtype == EOL_CRLF ? "crlf" :
		    data->subtype == EOL_CR ? "cr" :
		    data->subtype == EOL_AUTODETECT ? "nil" :
		    (ABORT(), ""));
}

static enum source_sink_type
convert_eol_conversion_end_type (Lisp_Object UNUSED (codesys))
{
  return DECODES_CHARACTER_TO_CHARACTER;
}

static int
convert_eol_putprop (Lisp_Object codesys,
		     Lisp_Object key,
		     Lisp_Object value)
{
  struct convert_eol_coding_system *data =
    XCODING_SYSTEM_TYPE_DATA (codesys, convert_eol);

  if (EQ (key, Qsubtype))
    {
      if (EQ (value, Qlf) /* || EQ (value, Qunix) */)
	data->subtype = EOL_LF;
      else if (EQ (value, Qcrlf) /* || EQ (value, Qdos) */)
	data->subtype = EOL_CRLF;
      else if (EQ (value, Qcr) /* || EQ (value, Qmac) */)
	data->subtype = EOL_CR;
      else if (EQ (value, Qnil))
	data->subtype = EOL_AUTODETECT;
      else invalid_constant ("Unrecognized eol type", value);
    }
  else
    return 0;
  return 1;
}

static Lisp_Object
convert_eol_getprop (Lisp_Object coding_system, Lisp_Object prop)
{
  struct convert_eol_coding_system *data =
    XCODING_SYSTEM_TYPE_DATA (coding_system, convert_eol);

  if (EQ (prop, Qsubtype))
    {
      switch (data->subtype)
	{
	case EOL_LF: return Qlf;
	case EOL_CRLF: return Qcrlf;
	case EOL_CR: return Qcr;
	case EOL_AUTODETECT: return Qnil;
	default: ABORT ();
	}
    }

  return Qunbound;
}

static void
convert_eol_init_coding_stream (struct coding_stream *str)
{
  struct convert_eol_coding_stream *data =
    CODING_STREAM_TYPE_DATA (str, convert_eol);
  data->actual = XCODING_SYSTEM_CONVERT_EOL_SUBTYPE (str->codesys);
}

static Bytecount
convert_eol_convert (struct coding_stream *str, const Ibyte *src,
		     unsigned_char_dynarr *dst, Bytecount n)
{
  if (str->direction == CODING_DECODE)
    {
      struct convert_eol_coding_stream *data =
	CODING_STREAM_TYPE_DATA (str, convert_eol);

      if (data->actual == EOL_AUTODETECT)
	{
	  Bytecount n2 = n;
	  const Ibyte *src2 = src;
  
	  for (; n2; n2--)
	    {
	      Ibyte c = *src2++;
	      if (c == '\n')
		{
		  data->actual = EOL_LF;
		  break;
		}
	      else if (c == '\r')
		{
		  if (n2 == 1)
		    {
		      /* If we're seeing a '\r' at the end of the data, then
			 reject the '\r' right now so it doesn't become an
			 issue in the code below -- unless we're at the end of
			 the stream, in which case we can't do that (because
			 then the '\r' will never get written out), and in any
			 case we should be recognizing it at EOL_CR format. */
		      if (str->eof)
			data->actual = EOL_CR;
		      else
			n--;
		      break;
		    }
		  else if (*src2 == '\n')
		    data->actual = EOL_CRLF;
		  else
		    data->actual = EOL_CR;
		  break;
		}
	    }
	}

      /* str->eof is set, the caller reached EOF on the other end and has
	 no new data to give us.  The only data we get is the data we
	 rejected from last time. */
      if (data->actual == EOL_LF || data->actual == EOL_AUTODETECT ||
	  (str->eof))
	Dynarr_add_many (dst, src, n);
      else
	{
	  const Ibyte *end = src + n;
	  while (1)
	    {
	      /* Find the next section with no \r and add it. */
	      const Ibyte *runstart = src;
	      src = (Ibyte *) memchr (src, '\r', end - src);
	      if (!src)
		src = end;
	      Dynarr_add_many (dst, runstart, src - runstart);
	      /* Stop if at end ... */
	      if (src == end)
		break;
	      /* ... else, translate as necessary. */
	      src++;
	      if (data->actual == EOL_CR)
		Dynarr_add (dst, '\n');
	      /* We need to be careful here with CRLF.  If we see a CR at the
		 end of the data, we don't know if it's part of a CRLF, so we
		 reject it.  Otherwise: If it's part of a CRLF, eat it and
		 loop; the following LF gets added next time around.  If it's
		 not part of a CRLF, add the CR and loop.  The following
		 character will be processed in the next loop iteration.  This
		 correctly handles a sequence like CR+CR+LF. */
	      else if (src == end)
		return n - 1;	/* reject the CR at the end; we'll get it again
				   next time the convert method is called */
	      else if (*src != '\n')
		Dynarr_add (dst, '\r');
	    }
	}

      return n;
    }
  else
    {
      enum eol_type subtype =
	XCODING_SYSTEM_CONVERT_EOL_SUBTYPE (str->codesys);
      const Ibyte *end = src + n;

      /* We try to be relatively efficient here. */
      if (subtype == EOL_LF)
	Dynarr_add_many (dst, src, n);
      else
	{
	  while (1)
	    {
	      /* Find the next section with no \n and add it. */
	      const Ibyte *runstart = src;
	      src = (Ibyte *) memchr (src, '\n', end - src);
	      if (!src)
		src = end;
	      Dynarr_add_many (dst, runstart, src - runstart);
	      /* Stop if at end ... */
	      if (src == end)
		break;
	      /* ... else, skip over \n and add its translation. */
	      src++;
	      Dynarr_add (dst, '\r');
	      if (subtype == EOL_CRLF)
		Dynarr_add (dst, '\n');
	    }
	}

      return n;
    }
}

static Lisp_Object
convert_eol_canonicalize_after_coding (struct coding_stream *str)
{
  struct convert_eol_coding_stream *data =
    CODING_STREAM_TYPE_DATA (str, convert_eol);

  if (str->direction == CODING_ENCODE)
    return str->codesys;
  
  switch (data->actual)
    {
    case EOL_LF: return Fget_coding_system (Qconvert_eol_lf);
    case EOL_CRLF: return Fget_coding_system (Qconvert_eol_crlf);
    case EOL_CR: return Fget_coding_system (Qconvert_eol_cr);
    case EOL_AUTODETECT: return str->codesys;
    default: ABORT (); return Qnil;
    }
}


/************************************************************************/
/*                            Undecided methods                         */
/************************************************************************/

/* Do autodetection.  We can autodetect the EOL type only, the coding
   system only, or both.  We only do autodetection when decoding; when
   encoding, we just pass the data through.

   When doing just EOL detection, a coding system can be specified; if so,
   we will decode this data through the coding system before doing EOL
   detection.  The reason for specifying this is so that
   canonicalize-after-coding works: We will canonicalize the specified
   coding system into the appropriate EOL type.  When doing both coding and
   EOL detection, we do similar canonicalization, and also catch situations
   where the EOL type is overspecified, i.e. the detected coding system
   specifies an EOL type, and either switch to the equivalent
   non-EOL-processing coding system (if possible), or terminate EOL
   detection and use the specified EOL type.  This prevents data from being
   EOL-processed twice.
   */

struct undecided_coding_system
{
  int do_eol, do_coding;
  Lisp_Object cs;
};

struct undecided_coding_stream
{
  Lisp_Object actual;
  /* Either 2 or 3 lstreams here; see undecided_convert */
  struct chain_coding_stream c;

  struct detection_state *st;
};

static const struct memory_description undecided_coding_system_description[] = {
  { XD_LISP_OBJECT, offsetof (struct undecided_coding_system, cs) },
  { XD_END }
};

static const struct memory_description undecided_coding_stream_description_1 [] = {
  { XD_LISP_OBJECT, offsetof (struct undecided_coding_stream, actual) },
  { XD_BLOCK_ARRAY, offsetof (struct undecided_coding_stream, c),
    1, { &chain_coding_stream_description } },
  { XD_END }
};

const struct sized_memory_description undecided_coding_stream_description = {
  sizeof (struct undecided_coding_stream), undecided_coding_stream_description_1
};

DEFINE_CODING_SYSTEM_TYPE_WITH_DATA (undecided);

static void
undecided_init (Lisp_Object codesys)
{
  struct undecided_coding_system *data =
    XCODING_SYSTEM_TYPE_DATA (codesys, undecided);

  data->cs = Qnil;
}

static void
undecided_mark (Lisp_Object codesys)
{
  struct undecided_coding_system *data =
    XCODING_SYSTEM_TYPE_DATA (codesys, undecided);

  mark_object (data->cs);
}

static void
undecided_print (Lisp_Object cs, Lisp_Object printcharfun, int escapeflag)
{
  struct undecided_coding_system *data =
    XCODING_SYSTEM_TYPE_DATA (cs, undecided);
  int need_space = 0;

  write_c_string (printcharfun, "(");
  if (data->do_eol)
    {
      write_c_string (printcharfun, "do-eol");
      need_space = 1;
    }
  if (data->do_coding)
    {
      if (need_space)
	write_c_string (printcharfun, " ");
      write_c_string (printcharfun, "do-coding");
      need_space = 1;
    }
  if (!NILP (data->cs))
    {
      if (need_space)
	write_c_string (printcharfun, " ");
      write_c_string (printcharfun, "coding-system=");
      print_coding_system_in_print_method (data->cs, printcharfun, escapeflag);
    }      
  write_c_string (printcharfun, ")");
}

static void
undecided_mark_coding_stream (struct coding_stream *str)
{
  mark_object (CODING_STREAM_TYPE_DATA (str, undecided)->actual);
  chain_mark_coding_stream_1 (&CODING_STREAM_TYPE_DATA (str, undecided)->c);
}

static int
undecided_putprop (Lisp_Object codesys, Lisp_Object key, Lisp_Object value)
{
  struct undecided_coding_system *data =
    XCODING_SYSTEM_TYPE_DATA (codesys, undecided);

  if (EQ (key, Qdo_eol))
    data->do_eol = 1;
  else if (EQ (key, Qdo_coding))
    data->do_coding = 1;
  else if (EQ (key, Qcoding_system))
    data->cs = get_coding_system_for_text_file (value, 0);
  else
    return 0;
  return 1;
}

static Lisp_Object
undecided_getprop (Lisp_Object codesys, Lisp_Object prop)
{
  struct undecided_coding_system *data =
    XCODING_SYSTEM_TYPE_DATA (codesys, undecided);

  if (EQ (prop, Qdo_eol))
    return data->do_eol ? Qt : Qnil;
  if (EQ (prop, Qdo_coding))
    return data->do_coding ? Qt : Qnil;
  if (EQ (prop, Qcoding_system))
    return data->cs;
  return Qunbound;
}

static struct detection_state *
allocate_detection_state (void)
{
  int i;
  Bytecount size = MAX_ALIGN_SIZE (sizeof (struct detection_state));
  struct detection_state *block;

  for (i = 0; i < coding_detector_count; i++)
    size += MAX_ALIGN_SIZE (Dynarr_at (all_coding_detectors, i).data_size);

  block = (struct detection_state *) xmalloc_and_zero (size);

  size = MAX_ALIGN_SIZE (sizeof (struct detection_state));
  for (i = 0; i < coding_detector_count; i++)
    {
      block->data_offset[i] = size;
      size += MAX_ALIGN_SIZE (Dynarr_at (all_coding_detectors, i).data_size);
    }

  return block;
}

static void
free_detection_state (struct detection_state *st)
{
  int i;

  for (i = 0; i < coding_detector_count; i++)
    {
      if (Dynarr_at (all_coding_detectors, i).finalize_detection_state_method)
	Dynarr_at (all_coding_detectors, i).finalize_detection_state_method
	  (st);
    }

  xfree (st, struct detection_state *);
}

static int
coding_category_symbol_to_id (Lisp_Object symbol)
{
  int i;

  CHECK_SYMBOL (symbol);
  for (i = 0; i < coding_detector_count; i++)
    {
      detector_category_dynarr *cats =
	Dynarr_at (all_coding_detectors, i).cats;
      int j;

      for (j = 0; j < Dynarr_length (cats); j++)
	if (EQ (Dynarr_at (cats, j).sym, symbol))
	  return Dynarr_at (cats, j).id;
    }
  
  invalid_constant ("Unrecognized coding category", symbol);
  RETURN_NOT_REACHED (0);
}

static Lisp_Object
coding_category_id_to_symbol (int id)
{
  int i;

  for (i = 0; i < coding_detector_count; i++)
    {
      detector_category_dynarr *cats =
	Dynarr_at (all_coding_detectors, i).cats;
      int j;

      for (j = 0; j < Dynarr_length (cats); j++)
	if (id == Dynarr_at (cats, j).id)
	  return Dynarr_at (cats, j).sym;
    }

  ABORT ();
  return Qnil; /* (usually) not reached */
}

static Lisp_Object
detection_result_number_to_symbol (enum detection_result result)
{
  /* let compiler warn if not all enumerators are handled */
  switch (result) {
#define FROB(sym, num) case num: return (sym)
  FROB (Qnear_certainty, DET_NEAR_CERTAINTY);
  FROB (Qquite_probable, DET_QUITE_PROBABLE);
  FROB (Qsomewhat_likely, DET_SOMEWHAT_LIKELY);
  FROB (Qslightly_likely, DET_SLIGHTLY_LIKELY);
  FROB (Qas_likely_as_unlikely, DET_AS_LIKELY_AS_UNLIKELY);
  FROB (Qsomewhat_unlikely, DET_SOMEWHAT_UNLIKELY);
  FROB (Qquite_improbable, DET_QUITE_IMPROBABLE);
  FROB (Qnearly_impossible, DET_NEARLY_IMPOSSIBLE);
#undef FROB
  }

  ABORT ();
  return Qnil; /* (usually) not reached */
}

#if 0 /* not used */
static enum detection_result
detection_result_symbol_to_number (Lisp_Object symbol)
{
  /* using switch here would be bad style, and doesn't help */
#define FROB(sym, num) if (EQ (symbol, sym)) return (num)
  FROB (Qnear_certainty, DET_NEAR_CERTAINTY);
  FROB (Qquite_probable, DET_QUITE_PROBABLE);
  FROB (Qsomewhat_likely, DET_SOMEWHAT_LIKELY);
  FROB (Qslightly_likely, DET_SLIGHTLY_LIKELY);
  FROB (Qas_likely_as_unlikely, DET_AS_LIKELY_AS_UNLIKELY);
  FROB (Qsomewhat_unlikely, DET_SOMEWHAT_UNLIKELY);
  FROB (Qquite_improbable, DET_QUITE_IMPROBABLE);
  FROB (Qnearly_impossible, DET_NEARLY_IMPOSSIBLE);
#undef FROB
  
  invalid_constant ("Unrecognized detection result", symbol);
  return ((enum detection_result) 0); /* not reached */
}
#endif /* 0 */

/* Set all detection results for a given detector to a specified value. */
void
set_detection_results (struct detection_state *st, int detector, int given)
{
  detector_category_dynarr *cats =
    Dynarr_at (all_coding_detectors, detector).cats;
  int i;

  for (i = 0; i < Dynarr_length (cats); i++)
    st->categories[Dynarr_at (cats, i).id] = given;
}

static int
acceptable_control_char_p (int c)
{
  switch (c)
    {
      /* Allow and ignore control characters that you might
	 reasonably see in a text file */
    case '\r':
    case '\n':
    case '\t':
    case  7: /* bell */
    case  8: /* backspace */
    case 11: /* vertical tab */
    case 12: /* form feed */
    case 26: /* MS-DOS C-z junk */
    case 31: /* '^_' -- for info */
      return 1;
    default:
      return 0;
    }
}

#ifdef DEBUG_XEMACS

static UExtbyte
hex_digit_to_char (int digit)
{
  if (digit < 10)
    return digit + '0';
  else
    return digit - 10 + 'A';
}

static void
output_bytes_in_ascii_and_hex (const UExtbyte *src, Bytecount n)
{
  UExtbyte *ascii = alloca_array (UExtbyte, n + 1);
  UExtbyte *hex = alloca_array (UExtbyte, 3 * n + 1);
  int i;

  for (i = 0; i < n; i++)
    {
      UExtbyte c = src[i];
      if (c < 0x20)
	ascii[i] = '.';
      else
	ascii[i] = c;
      hex[3 * i] = hex_digit_to_char (c >> 4);
      hex[3 * i + 1] = hex_digit_to_char (c & 0xF);
      hex[3 * i + 2] = ' ';
    }
  ascii[i] = '\0';
  hex[3 * i - 1] = '\0';
  stderr_out ("%s  %s", ascii, hex);
}

#endif /* DEBUG_XEMACS */

/* Attempt to determine the encoding of the given text.  Before calling
   this function for the first time, you must zero out the detection state.

   Returns:

   0 == keep going
   1 == stop
*/

static int
detect_coding_type (struct detection_state *st, const UExtbyte *src,
		    Bytecount n)
{
  Bytecount n2 = n;
  const UExtbyte *src2 = src;
  int i;

#ifdef DEBUG_XEMACS
  if (!NILP (Vdebug_coding_detection))
    {
      int bytes = min (16, n);
      stderr_out ("detect_coding_type: processing %ld bytes\n", n);
      stderr_out ("First %d: ", bytes);
      output_bytes_in_ascii_and_hex (src, bytes);
      stderr_out ("\nLast %d: ", bytes);
      output_bytes_in_ascii_and_hex (src + n - bytes, bytes);
      stderr_out ("\n");
    }
#endif /* DEBUG_XEMACS */
  if (!st->seen_non_ascii)
    {
      for (; n2; n2--, src2++)
	{
	  UExtbyte c = *src2;
	  if ((c < 0x20 && !acceptable_control_char_p (c)) || c >= 0x80)
	    {
	      st->seen_non_ascii = 1;
	      break;
	    }
	}
    }

  for (i = 0; i < coding_detector_count; i++)
    Dynarr_at (all_coding_detectors, i).detect_method (st, src, n);

  st->bytes_seen += n;

#ifdef DEBUG_XEMACS
  if (!NILP (Vdebug_coding_detection))
    {
      stderr_out ("seen_non_ascii: %d\n", st->seen_non_ascii);
      if (coding_detector_category_count <= 0)
	stderr_out ("found %d detector categories\n",
		    coding_detector_category_count);
      for (i = 0; i < coding_detector_category_count; i++)
	stderr_out_lisp
	  ("%s: %s\n",
	   2,
	   coding_category_id_to_symbol (i),
	   detection_result_number_to_symbol ((enum detection_result)
					      st->categories[i]));
    }
#endif /* DEBUG_XEMACS */

  {
    int not_unlikely = 0;
    int retval;
    
    for (i = 0; i < coding_detector_category_count; i++)
      if (st->categories[i] >= 0)
	not_unlikely++;

    retval = (not_unlikely <= 1
#if 0 /* this is bogus */
	      || st->bytes_seen >= MAX_BYTES_PROCESSED_FOR_DETECTION
#endif
	      );

#ifdef DEBUG_XEMACS
  if (!NILP (Vdebug_coding_detection))
    stderr_out ("detect_coding_type: returning %d (%s)\n",
		retval, retval ? "stop" : "keep going");
#endif /* DEBUG_XEMACS */
    
    return retval;
  }
}

static Lisp_Object
detected_coding_system (struct detection_state *st)
{
  int i;
  int even = 1;

  if (st->seen_non_ascii)
    {
      for (i = 0; i < coding_detector_category_count; i++)
	if (st->categories[i] != DET_AS_LIKELY_AS_UNLIKELY)
	  {
	    even = 0;
	    break;
	  }
    }

  /* #### Here we are ignoring the results of detection when it's all
     ASCII.  This is obviously a bad thing.  But we need to fix up the
     existing detection methods somewhat before we can switch. */
  if (even)
    {
      /* If the file was entirely or basically ASCII, use the
	 default value of `buffer-file-coding-system'. */
      Lisp_Object retval =
	XBUFFER (Vbuffer_defaults)->buffer_file_coding_system;
      if (!NILP (retval))
	{
	  retval = find_coding_system_for_text_file (retval, 0);
	  if (NILP (retval))
	    {
	      warn_when_safe
		(Qbad_variable, Qwarning,
		 "Invalid `default-buffer-file-coding-system', set to nil");
	      XBUFFER (Vbuffer_defaults)->buffer_file_coding_system = Qnil;
	    }
	}
      if (NILP (retval))
	retval = Fget_coding_system (Qraw_text);
      return retval;
    }
  else
    {
      int likelihood;
      Lisp_Object retval = Qnil;

      /* Look through the coding categories first by likelihood and then by
         priority and find the first one that is allowed. */

      for (likelihood = DET_HIGHEST; likelihood >= DET_LOWEST; likelihood--)
	{
	  for (i = 0; i < coding_detector_category_count; i++)
	    {
	      int cat = coding_category_by_priority[i];
	      if (st->categories[cat] == likelihood &&
		  !NILP (coding_category_system[cat]))
		{
		  retval = (get_coding_system_for_text_file
			    (coding_category_system[cat], 0));
		  if (likelihood < DET_AS_LIKELY_AS_UNLIKELY)
		    warn_when_safe_lispobj
		      (intern ("detection"),
		       Qwarning,
		       emacs_sprintf_string_lisp
		       (
"Detected coding %s is unlikely to be correct (likelihood == `%s')",
			Qnil, 2, XCODING_SYSTEM_NAME (retval),
			detection_result_number_to_symbol
			((enum detection_result) likelihood)));
		  return retval;
		}
	    }
	}

      return Fget_coding_system (Qraw_text);
    }
}

/* Look for a coding system in the string (skipping over leading
   blanks).  If found, return it, otherwise nil. */

static Lisp_Object
snarf_coding_system (const UExtbyte *p, Bytecount len)
{
  Bytecount n;
  UExtbyte *name;

  while (*p == ' ' || *p == '\t') p++, len--;
  len = min (len, 1000);
  name = alloca_ibytes (len + 1);
  memcpy (name, p, len);
  name[len] = '\0';

  /* Get coding system name */
  /* Characters valid in a MIME charset name (rfc 1521),
     and in a Lisp symbol name. */
  n = qxestrspn (name,
		 "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
		 "abcdefghijklmnopqrstuvwxyz"
		 "0123456789"
		 "!$%&*+-.^_{|}~");
  if (n > 0)
    {
      name[n] = '\0';
      /* This call to intern_int() is OK because we already verified that
	 there are only ASCII characters in the string */
      return find_coding_system_for_text_file (intern_int ((Ibyte *) name), 0);
    }

  return Qnil;
}

/* Given a seekable read stream and potential coding system and EOL type
   as specified, do any autodetection that is called for.  If the
   coding system and/or EOL type are not `autodetect', they will be left
   alone; but this function will never return an autodetect coding system
   or EOL type.

   This function does not automatically fetch subsidiary coding systems;
   that should be unnecessary with the explicit eol-type argument. */

#define LENGTH(string_constant) (sizeof (string_constant) - 1)

static Lisp_Object
unwind_free_detection_state (Lisp_Object opaque)
{
  struct detection_state *st =
    (struct detection_state *) get_opaque_ptr (opaque);
  free_detection_state (st);
  free_opaque_ptr (opaque);
  return Qnil;
}

/* #### This duplicates code in `find-coding-system-magic-cookie-in-file'
   in files.el.  Look into combining them. */

static Lisp_Object
look_for_coding_system_magic_cookie (const UExtbyte *data, Bytecount len)
{
  const UExtbyte *p;
  const UExtbyte *scan_end;
  Bytecount cookie_len;

  /* Look for initial "-*-"; mode line prefix */
  for (p = data,
       scan_end = data + len - LENGTH ("-*-coding:?-*-");
       p <= scan_end
       && *p != '\n'
       && *p != '\r';
       p++)
    if (*p == '-' && *(p+1) == '*' && *(p+2) == '-')
      {
	const UExtbyte *local_vars_beg = p + 3;
	/* Look for final "-*-"; mode line suffix */
	for (p = local_vars_beg,
	     scan_end = data + len - LENGTH ("-*-");
	     p <= scan_end
	     && *p != '\n'
	     && *p != '\r';
	     p++)
	  if (*p == '-' && *(p+1) == '*' && *(p+2) == '-')
	    {
	      const UExtbyte *suffix = p;
	      /* Look for "coding:" */
	      for (p = local_vars_beg,
		   scan_end = suffix - LENGTH ("coding:?");
		   p <= scan_end;
		   p++)
		if (memcmp ("coding:", p, LENGTH ("coding:")) == 0
		    && (p == local_vars_beg
			|| (*(p-1) == ' '  ||
			    *(p-1) == '\t' ||
			    *(p-1) == ';')))
		  {
		    p += LENGTH ("coding:");
		    return snarf_coding_system (p, suffix - p);
		    break;
		  }
	      break;
	    }
	break;
      }

  /* Look for ;;;###coding system */

  cookie_len = LENGTH (";;;###coding system: ");

  for (p = data,
       scan_end = data + len - cookie_len;
       p <= scan_end;
       p++)
  {
    if (*p == ';' && !memcmp (p, ";;;###coding system: ", cookie_len))
      {
	const UExtbyte *suffix;

	p += cookie_len;
	suffix = p;
	while (suffix < scan_end && !isspace (*suffix))
	  suffix++;
	return snarf_coding_system (p, suffix - p);
      }
  }

  return Qnil;
} 

static Lisp_Object
determine_real_coding_system (Lstream *stream)
{
  struct detection_state *st = allocate_detection_state ();
  int depth = record_unwind_protect (unwind_free_detection_state,
				     make_opaque_ptr (st));
  UExtbyte buf[4096];
  Bytecount nread = Lstream_read (stream, buf, sizeof (buf));
  Lisp_Object coding_system = look_for_coding_system_magic_cookie (buf, nread);

  if (NILP (coding_system))
    {
      while (1)
	{
	  if (detect_coding_type (st, buf, nread))
	    break;
	  nread = Lstream_read (stream, buf, sizeof (buf));
	  if (nread == 0)
	    break;
	}

      coding_system = detected_coding_system (st);
    }

  Lstream_rewind (stream);

  unbind_to (depth);
  return coding_system;
}

static void
undecided_init_coding_stream (struct coding_stream *str)
{
  struct undecided_coding_stream *data =
    CODING_STREAM_TYPE_DATA (str, undecided);
  struct undecided_coding_system *csdata =
    XCODING_SYSTEM_TYPE_DATA (str->codesys, undecided);

  data->actual = Qnil;

  if (str->direction == CODING_DECODE)
    {
      Lstream *lst = str->other_end;
      
      if ((lst->flags & LSTREAM_FL_READ) &&
	  Lstream_seekable_p (lst) &&
	  csdata->do_coding)
	/* We can determine the coding system now. */
	data->actual = determine_real_coding_system (lst);
    }

#ifdef DEBUG_XEMACS
  if (!NILP (Vdebug_coding_detection))
    stderr_out_lisp ("detected coding system: %s\n", 1, data->actual);
#endif /* DEBUG_XEMACS */
}

static void
undecided_rewind_coding_stream (struct coding_stream *str)
{
  chain_rewind_coding_stream_1 (&CODING_STREAM_TYPE_DATA (str, undecided)->c);
}

static void
undecided_finalize_coding_stream (struct coding_stream *str)
{
  struct undecided_coding_stream *data =
    CODING_STREAM_TYPE_DATA (str, undecided);

  chain_finalize_coding_stream_1
    (&CODING_STREAM_TYPE_DATA (str, undecided)->c);
  if (data->st)
    free_detection_state (data->st);
}

static Lisp_Object
undecided_canonicalize (Lisp_Object codesys)
{
  struct undecided_coding_system *csdata =
    XCODING_SYSTEM_TYPE_DATA (codesys, undecided);
  if (!csdata->do_eol && !csdata->do_coding)
    return NILP (csdata->cs) ? Fget_coding_system (Qbinary) : csdata->cs;
  if (csdata->do_eol && !csdata->do_coding && NILP (csdata->cs))
    return Fget_coding_system (Qconvert_eol_autodetect);
  return codesys;
}

static Bytecount
undecided_convert (struct coding_stream *str, const UExtbyte *src,
		   unsigned_char_dynarr *dst, Bytecount n)
{
  int first_time = 0;
  
  if (str->direction == CODING_DECODE)
    {
      /* At this point, we have only the following possibilities:

	 do_eol && do_coding
	 do_coding only
	 do_eol only and a coding system was specified

	 Other possibilities are removed during undecided_canonicalize.

	 Therefore, our substreams are either
	 
	 lstream_coding -> lstream_dynarr, or
	 lstream_coding -> lstream_eol -> lstream_dynarr.
	 */
      struct undecided_coding_system *csdata =
	XCODING_SYSTEM_TYPE_DATA (str->codesys, undecided);
      struct undecided_coding_stream *data =
	CODING_STREAM_TYPE_DATA (str, undecided);

      if (str->eof)
	{
	  /* Each will close the next.  We need to close now because more
	     data may be generated. */
	  if (data->c.initted)
	    Lstream_close (XLSTREAM (data->c.lstreams[0]));
	  return n;
	}

      if (!data->c.initted)
	{
	  data->c.lstream_count = csdata->do_eol ? 3 : 2;
	  data->c.lstreams = xnew_array (Lisp_Object, data->c.lstream_count);

	  data->c.lstreams[data->c.lstream_count - 1] =
	    make_dynarr_output_stream (dst);
	  Lstream_set_buffering
	    (XLSTREAM (data->c.lstreams[data->c.lstream_count - 1]),
	     LSTREAM_UNBUFFERED, 0);
	  if (csdata->do_eol)
	    {
	      data->c.lstreams[1] =
		make_coding_output_stream
		  (XLSTREAM (data->c.lstreams[data->c.lstream_count - 1]),
		   Fget_coding_system (Qconvert_eol_autodetect),
		   CODING_DECODE, 0);
	      Lstream_set_buffering
		(XLSTREAM (data->c.lstreams[1]),
		 LSTREAM_UNBUFFERED, 0);
	    }
	      
	  data->c.lstreams[0] =
	    make_coding_output_stream
	      (XLSTREAM (data->c.lstreams[1]),
	       /* Substitute binary if we need to detect the encoding */
	       csdata->do_coding ? Qbinary : csdata->cs,
	       CODING_DECODE, 0);
	  Lstream_set_buffering (XLSTREAM (data->c.lstreams[0]),
				 LSTREAM_UNBUFFERED, 0);

	  first_time = 1;
	  data->c.initted = 1;
	}

      /* If necessary, do encoding-detection now.  We do this when we're a
	 writing stream or a non-seekable reading stream, meaning that we
	 can't just process the whole input, rewind, and start over. */

      if (csdata->do_coding)
	{
	  int actual_was_nil = NILP (data->actual);
	  if (NILP (data->actual))
	    {
	      if (!data->st)
		data->st = allocate_detection_state ();
	      if (first_time)
		/* #### This is cheesy.  What we really ought to do is buffer
		   up a certain minimum amount of data to get a better result.
		   */
		data->actual = look_for_coding_system_magic_cookie (src, n);
	      if (NILP (data->actual))
		{
		  /* #### This is cheesy.  What we really ought to do is buffer
		     up a certain minimum amount of data so as to get a less
		     random result when doing subprocess detection. */
		  detect_coding_type (data->st, src, n);
		  data->actual = detected_coding_system (data->st);
		}
	    }
	  /* We need to set the detected coding system if we actually have
	     such a coding system but didn't before.  That is the case
	     either when we just detected it in the previous code or when
	     it was detected during undecided_init_coding_stream().  We
	     can check for that using first_time. */
	  if (!NILP (data->actual) && (actual_was_nil || first_time))
	    {
	      /* If the detected coding system doesn't allow for EOL
		 autodetection, try to get the equivalent that does;
		 otherwise, disable EOL detection (overriding whatever
		 may already have been detected). */
	      if (XCODING_SYSTEM_EOL_TYPE (data->actual) != EOL_AUTODETECT)
		{
		  if (!NILP (XCODING_SYSTEM_SUBSIDIARY_PARENT (data->actual)))
		    data->actual =
		      XCODING_SYSTEM_SUBSIDIARY_PARENT (data->actual);
		  else if (data->c.lstream_count == 3)
		    set_coding_stream_coding_system
		      (XLSTREAM (data->c.lstreams[1]),
		       Fget_coding_system (Qidentity));
		}
	      set_coding_stream_coding_system
		(XLSTREAM (data->c.lstreams[0]), data->actual);
	    }
	}

      if (Lstream_write (XLSTREAM (data->c.lstreams[0]), src, n) < 0)
	return -1;
      return n;
    }
  else
    return no_conversion_convert (str, src, dst, n);
}

static Lisp_Object
undecided_canonicalize_after_coding (struct coding_stream *str)
{
  struct undecided_coding_stream *data =
    CODING_STREAM_TYPE_DATA (str, undecided);
  Lisp_Object ret, eolret;

  if (str->direction == CODING_ENCODE)
    return str->codesys;

  if (!data->c.initted)
    return Fget_coding_system (Qundecided);
  
  ret = coding_stream_canonicalize_after_coding
    (XLSTREAM (data->c.lstreams[0]));
  if (NILP (ret))
    ret = Fget_coding_system (Qundecided);
  if (XCODING_SYSTEM_EOL_TYPE (ret) != EOL_AUTODETECT)
    return ret;
  eolret = coding_stream_canonicalize_after_coding
    (XLSTREAM (data->c.lstreams[1]));
  if (!EQ (XCODING_SYSTEM_TYPE (eolret), Qconvert_eol))
    return ret;
  return
    Fsubsidiary_coding_system (ret, Fcoding_system_property (eolret,
							     Qsubtype));
}


/************************************************************************/
/*        Lisp interface: Coding category functions and detection       */
/************************************************************************/

DEFUN ("coding-category-list", Fcoding_category_list, 0, 0, 0, /*
Return a list of all recognized coding categories.
*/
       ())
{
  int i;
  Lisp_Object list = Qnil;

  for (i = 0; i < coding_detector_count; i++)
    {
      detector_category_dynarr *cats =
	Dynarr_at (all_coding_detectors, i).cats;
      int j;

      for (j = 0; j < Dynarr_length (cats); j++)
	list = Fcons (Dynarr_at (cats, j).sym, list);
    }

  return Fnreverse (list);
}

DEFUN ("set-coding-priority-list", Fset_coding_priority_list, 1, 1, 0, /*
Change the priority order of the coding categories.
LIST should be list of coding categories, in descending order of
priority.  Unspecified coding categories will be lower in priority
than all specified ones, in the same relative order they were in
previously.
*/
       (list))
{
  int *category_to_priority =
    alloca_array (int, coding_detector_category_count);
  int i, j;

  /* First generate a list that maps coding categories to priorities. */

  for (i = 0; i < coding_detector_category_count; i++)
    category_to_priority[i] = -1;

  /* Highest priority comes from the specified list. */
  i = 0;
  {
    EXTERNAL_LIST_LOOP_2 (elt, list)
      {
	int cat = coding_category_symbol_to_id (elt);

	if (category_to_priority[cat] >= 0)
	  sferror ("Duplicate coding category in list", elt);
	category_to_priority[cat] = i++;
      }
  }

  /* Now go through the existing categories by priority to retrieve
     the categories not yet specified and preserve their priority
     order. */
  for (j = 0; j < coding_detector_category_count; j++)
    {
      int cat = coding_category_by_priority[j];
      if (category_to_priority[cat] < 0)
	category_to_priority[cat] = i++;
    }

  /* Now we need to construct the inverse of the mapping we just
     constructed. */

  for (i = 0; i < coding_detector_category_count; i++)
    coding_category_by_priority[category_to_priority[i]] = i;

  /* Phew!  That was confusing. */
  return Qnil;
}

DEFUN ("coding-priority-list", Fcoding_priority_list, 0, 0, 0, /*
Return a list of coding categories in descending order of priority.
*/
       ())
{
  int i;
  Lisp_Object list = Qnil;

  for (i = 0; i < coding_detector_category_count; i++)
    list =
      Fcons (coding_category_id_to_symbol (coding_category_by_priority[i]),
	     list);
  return Fnreverse (list);
}

DEFUN ("set-coding-category-system", Fset_coding_category_system, 2, 2, 0, /*
Change the coding system associated with a coding category.
*/
       (coding_category, coding_system))
{
  coding_category_system[coding_category_symbol_to_id (coding_category)] =
    Fget_coding_system (coding_system);
  return Qnil;
}

DEFUN ("coding-category-system", Fcoding_category_system, 1, 1, 0, /*
Return the coding system associated with a coding category.
*/
       (coding_category))
{
  Lisp_Object sys =
    coding_category_system[coding_category_symbol_to_id (coding_category)];

  if (!NILP (sys))
    return XCODING_SYSTEM_NAME (sys);
  return Qnil;
}

/* Detect the encoding of STREAM.  Assumes stream is at the begnning and will
   read through to the end of STREAM, leaving it there but open. */

Lisp_Object
detect_coding_stream (Lisp_Object stream)
{
  Lisp_Object val = Qnil;
  struct gcpro gcpro1, gcpro2, gcpro3;
  UExtbyte random_buffer[65536];
  Lisp_Object binary_instream =
    make_coding_input_stream
      (XLSTREAM (stream), Qbinary,
       CODING_ENCODE, LSTREAM_FL_NO_CLOSE_OTHER);
  Lisp_Object decstream =
    make_coding_input_stream 
      (XLSTREAM (binary_instream),
       Qundecided, CODING_DECODE, 0);
  Lstream *decstr = XLSTREAM (decstream);
  
  GCPRO3 (decstream, stream, binary_instream);
  /* Read and discard all data; detection happens as a side effect of this,
     and we examine what was detected afterwards. */
  while (Lstream_read (decstr, random_buffer, sizeof (random_buffer)) > 0)
    ;

  val = coding_stream_detected_coding_system (decstr);
  Lstream_close (decstr);
  Lstream_delete (decstr);
  Lstream_delete (XLSTREAM (binary_instream));
  UNGCPRO;
  return val;
}

DEFUN ("detect-coding-region", Fdetect_coding_region, 2, 3, 0, /*
Detect coding system of the text in the region between START and END.
Return a list of possible coding systems ordered by priority.
If only ASCII characters are found, return 'undecided or one of
its subsidiary coding systems according to a detected end-of-line
type.  Optional arg BUFFER defaults to the current buffer.
*/
       (start, end, buffer))
{
  Lisp_Object val = Qnil;
  struct buffer *buf = decode_buffer (buffer, 0);
  Charbpos b, e;
  Lisp_Object lb_instream;

  get_buffer_range_char (buf, start, end, &b, &e, 0);
  lb_instream = make_lisp_buffer_input_stream (buf, b, e, 0);
  
  val = detect_coding_stream (lb_instream);
  Lstream_delete (XLSTREAM (lb_instream));
  return val;
}



#ifdef DEBUG_XEMACS

/************************************************************************/
/*                            Internal methods                          */
/************************************************************************/

/* Raw (internally-formatted) data. */
DEFINE_CODING_SYSTEM_TYPE (internal);

static Bytecount
internal_convert (struct coding_stream *UNUSED (str), const UExtbyte *src,
		  unsigned_char_dynarr *dst, Bytecount n)
{
  Bytecount orign = n;
  Dynarr_add_many (dst, src, n);
  return orign;
}

#endif /* DEBUG_XEMACS */



#ifdef HAVE_ZLIB

/************************************************************************/
/*                             Gzip methods                             */
/************************************************************************/

struct gzip_coding_system
{
  int level; /* 0 through 9, or -1 for default */
};

#define CODING_SYSTEM_GZIP_LEVEL(codesys) \
  (CODING_SYSTEM_TYPE_DATA (codesys, gzip)->level)
#define XCODING_SYSTEM_GZIP_LEVEL(codesys) \
  (XCODING_SYSTEM_TYPE_DATA (codesys, gzip)->level)

struct gzip_coding_stream
{
  z_stream stream;
  int stream_initted;
  int reached_eof; /* #### this should be handled by the caller, once we
		      return LSTREAM_EOF */
};

static const struct memory_description
  gzip_coding_system_description[] = {
  { XD_END }
};

DEFINE_CODING_SYSTEM_TYPE_WITH_DATA (gzip);

enum source_sink_type
gzip_conversion_end_type (Lisp_Object codesys)
{
  return DECODES_BYTE_TO_BYTE;
}

static void
gzip_init (Lisp_Object codesys)
{
  struct gzip_coding_system *data = XCODING_SYSTEM_TYPE_DATA (codesys, gzip);
  data->level = -1;
}

static void
gzip_print (Lisp_Object cs, Lisp_Object printcharfun, int escapeflag)
{
  struct gzip_coding_system *data = XCODING_SYSTEM_TYPE_DATA (cs, gzip);

  write_c_string (printcharfun, "(");
  if (data->level == -1)
    write_c_string (printcharfun, "default");
  else
    print_internal (make_int (data->level), printcharfun, 0);
  write_c_string (printcharfun, ")");
}

static int
gzip_putprop (Lisp_Object codesys, Lisp_Object key, Lisp_Object value)
{
  struct gzip_coding_system *data = XCODING_SYSTEM_TYPE_DATA (codesys, gzip);

  if (EQ (key, Qlevel))
    {
      if (EQ (value, Qdefault))
	data->level = -1;
      else
	{
	  CHECK_INT (value);
	  check_int_range (XINT (value), 0, 9);
	  data->level = XINT (value);
	}
    }
  else
    return 0;
  return 1;
}

static Lisp_Object
gzip_getprop (Lisp_Object coding_system, Lisp_Object prop)
{
  struct gzip_coding_system *data =
    XCODING_SYSTEM_TYPE_DATA (coding_system, gzip);

  if (EQ (prop, Qlevel))
    {
      if (data->level == -1)
	return Qdefault;
      return make_int (data->level);
    }

  return Qunbound;
}

static void
gzip_init_coding_stream (struct coding_stream *str)
{
  struct gzip_coding_stream *data = CODING_STREAM_TYPE_DATA (str, gzip);
  if (data->stream_initted)
    {
      if (str->direction == CODING_DECODE)
	inflateEnd (&data->stream);
      else
	deflateEnd (&data->stream);
      data->stream_initted = 0;
    }
  data->reached_eof = 0;
}

static void
gzip_rewind_coding_stream (struct coding_stream *str)
{
  gzip_init_coding_stream (str);
}

static Bytecount
gzip_convert (struct coding_stream *str,
	      const UExtbyte *src,
	      unsigned_char_dynarr *dst, Bytecount n)
{
  struct gzip_coding_stream *data = CODING_STREAM_TYPE_DATA (str, gzip);
  int zerr;
  if (str->direction == CODING_DECODE)
    {
      if (data->reached_eof)
	return n;		/* eat the data */
  
      if (!data->stream_initted)
	{
	  xzero (data->stream);
	  if (inflateInit (&data->stream) != Z_OK)
	    return LSTREAM_ERROR;
	  data->stream_initted = 1;
	}

      data->stream.next_in = (Bytef *) src;
      data->stream.avail_in = n;

      /* Normally we stop when we've fed all data to the decompressor; but
	 if we're at the end of the input, and the decompressor hasn't
	 reported EOF, we need to keep going, as there might be more output
	 to generate.  Z_OK from the decompressor means input was processed
	 or output was generated; if neither, we break out of the loop.
	 Other return values are:
	 
	 Z_STREAM_END		EOF from decompressor
	 Z_DATA_ERROR		Corrupted data
	 Z_BUF_ERROR		No progress possible (this should happen if
	 we try to feed it an incomplete file)
	 Z_MEM_ERROR		Out of memory
	 Z_STREAM_ERROR		(should never happen)
	 Z_NEED_DICT		(#### when will this happen?)
	 */
      while (data->stream.avail_in > 0 || str->eof)
	{
	  /* Reserve an output buffer of the same size as the input buffer;
	     if that's not enough, we keep reserving the same size. */
	  Bytecount reserved = n;
	  Dynarr_add_many (dst, 0, reserved);
	  /* Careful here!  Don't retrieve the pointer until after
	     reserving the space, or it might be bogus */
	  data->stream.next_out =
	    Dynarr_atp (dst, Dynarr_length (dst) - reserved);
	  data->stream.avail_out = reserved;
	  zerr = inflate (&data->stream, Z_NO_FLUSH);
	  /* Lop off the unused portion */
	  Dynarr_set_size (dst, Dynarr_length (dst) - data->stream.avail_out);
	  if (zerr != Z_OK)
	    break;
	}
  
      if (zerr == Z_STREAM_END)
	data->reached_eof = 1;

      if ((Bytecount) data->stream.avail_in < n)
	return n - data->stream.avail_in;

      if (zerr == Z_OK || zerr == Z_STREAM_END)
	return 0;

      return LSTREAM_ERROR;
    }
  else
    {
      if (!data->stream_initted)
	{
	  int level = XCODING_SYSTEM_GZIP_LEVEL (str->codesys);
	  xzero (data->stream);
	  if (deflateInit (&data->stream,
			   level == -1 ? Z_DEFAULT_COMPRESSION : level) !=
	      Z_OK)
	    return LSTREAM_ERROR;
	  data->stream_initted = 1;
	}

      data->stream.next_in = (Bytef *) src;
      data->stream.avail_in = n;

      /* Normally we stop when we've fed all data to the compressor; but if
	 we're at the end of the input, and the compressor hasn't reported
	 EOF, we need to keep going, as there might be more output to
	 generate.  (To signal EOF on our end, we set the FLUSH parameter
	 to Z_FINISH; when all data is output, Z_STREAM_END will be
	 returned.)  Z_OK from the compressor means input was processed or
	 output was generated; if neither, we break out of the loop.  Other
	 return values are:
	 
	 Z_STREAM_END		EOF from compressor
	 Z_BUF_ERROR		No progress possible (should never happen)
	 Z_STREAM_ERROR		(should never happen)
	 */
      while (data->stream.avail_in > 0 || str->eof)
	{
	  /* Reserve an output buffer of the same size as the input buffer;
	     if that's not enough, we keep reserving the same size. */
	  Bytecount reserved = n;
	  Dynarr_add_many (dst, 0, reserved);
	  /* Careful here!  Don't retrieve the pointer until after
	     reserving the space, or it might be bogus */
	  data->stream.next_out =
	    Dynarr_atp (dst, Dynarr_length (dst) - reserved);
	  data->stream.avail_out = reserved;
	  zerr =
	    deflate (&data->stream,
		     str->eof ? Z_FINISH : Z_NO_FLUSH);
	  /* Lop off the unused portion */
	  Dynarr_set_size (dst, Dynarr_length (dst) - data->stream.avail_out);
	  if (zerr != Z_OK)
	    break;
	}
  
      if ((Bytecount) data->stream.avail_in < n)
	return n - data->stream.avail_in;

      if (zerr == Z_OK || zerr == Z_STREAM_END)
	return 0;

      return LSTREAM_ERROR;
    }
}

#endif /* HAVE_ZLIB */


/************************************************************************/
/*                             Initialization                           */
/************************************************************************/

void
syms_of_file_coding (void)
{
  INIT_LRECORD_IMPLEMENTATION (coding_system);

  DEFSUBR (Fvalid_coding_system_type_p);
  DEFSUBR (Fcoding_system_type_list);
  DEFSUBR (Fcoding_system_p);
  DEFSUBR (Ffind_coding_system);
  DEFSUBR (Fget_coding_system);
  DEFSUBR (Fcoding_system_list);
  DEFSUBR (Fcoding_system_name);
  DEFSUBR (Fmake_coding_system);
  DEFSUBR (Fcopy_coding_system);
  DEFSUBR (Fcoding_system_canonical_name_p);
  DEFSUBR (Fcoding_system_alias_p);
  DEFSUBR (Fcoding_system_aliasee);
  DEFSUBR (Fdefine_coding_system_alias);
  DEFSUBR (Fsubsidiary_coding_system);
  DEFSUBR (Fcoding_system_base);
  DEFSUBR (Fcoding_system_used_for_io);

  DEFSUBR (Fcoding_system_type);
  DEFSUBR (Fcoding_system_description);
  DEFSUBR (Fcoding_system_property);

  DEFSUBR (Fcoding_category_list);
  DEFSUBR (Fset_coding_priority_list);
  DEFSUBR (Fcoding_priority_list);
  DEFSUBR (Fset_coding_category_system);
  DEFSUBR (Fcoding_category_system);

  DEFSUBR (Fdetect_coding_region);
  DEFSUBR (Fdecode_coding_region);
  DEFSUBR (Fencode_coding_region);
  DEFSYMBOL_MULTIWORD_PREDICATE (Qcoding_systemp);
  DEFSYMBOL (Qno_conversion);
  DEFSYMBOL (Qconvert_eol);
  DEFSYMBOL (Qconvert_eol_autodetect);
  DEFSYMBOL (Qconvert_eol_lf);
  DEFSYMBOL (Qconvert_eol_cr);
  DEFSYMBOL (Qconvert_eol_crlf);
  DEFSYMBOL (Qraw_text);

  DEFSYMBOL (Qmnemonic);
  DEFSYMBOL (Qeol_type);
  DEFSYMBOL (Qpost_read_conversion);
  DEFSYMBOL (Qpre_write_conversion);

  DEFSYMBOL (Qtranslation_table_for_decode);
  DEFSYMBOL (Qtranslation_table_for_encode);
  DEFSYMBOL (Qsafe_chars);
  DEFSYMBOL (Qsafe_charsets);
  DEFSYMBOL (Qmime_charset);
  DEFSYMBOL (Qvalid_codes);

  DEFSYMBOL (Qcr);
  DEFSYMBOL (Qlf);
  DEFSYMBOL (Qcrlf);
  DEFSYMBOL (Qeol_cr);
  DEFSYMBOL (Qeol_lf);
  DEFSYMBOL (Qeol_crlf);
  DEFSYMBOL (Qencode);
  DEFSYMBOL (Qdecode);

  DEFSYMBOL (Qnear_certainty);
  DEFSYMBOL (Qquite_probable);
  DEFSYMBOL (Qsomewhat_likely);
  DEFSYMBOL (Qslightly_likely);
  DEFSYMBOL (Qas_likely_as_unlikely);
  DEFSYMBOL (Qsomewhat_unlikely);
  DEFSYMBOL (Qquite_improbable);
  DEFSYMBOL (Qnearly_impossible);

  DEFSYMBOL (Qdo_eol);
  DEFSYMBOL (Qdo_coding);

  DEFSYMBOL (Qcanonicalize_after_coding);

  DEFSYMBOL (Qescape_quoted);

#ifdef HAVE_ZLIB
  DEFSYMBOL (Qgzip);
#endif

}

void
lstream_type_create_file_coding (void)
{
  LSTREAM_HAS_METHOD (coding, reader);
  LSTREAM_HAS_METHOD (coding, writer);
  LSTREAM_HAS_METHOD (coding, rewinder);
  LSTREAM_HAS_METHOD (coding, seekable_p);
  LSTREAM_HAS_METHOD (coding, marker);
  LSTREAM_HAS_METHOD (coding, flusher);
  LSTREAM_HAS_METHOD (coding, closer);
  LSTREAM_HAS_METHOD (coding, finalizer);
}

void
coding_system_type_create (void)
{
  int i;

  staticpro (&Vcoding_system_hash_table);
  Vcoding_system_hash_table =
    make_lisp_hash_table (50, HASH_TABLE_NON_WEAK, HASH_TABLE_EQ);

  the_coding_system_type_entry_dynarr = Dynarr_new (coding_system_type_entry);
  dump_add_root_block_ptr (&the_coding_system_type_entry_dynarr,
			    &csted_description);

  Vcoding_system_type_list = Qnil;
  staticpro (&Vcoding_system_type_list);

  /* Initialize to something reasonable ... */
  for (i = 0; i < MAX_DETECTOR_CATEGORIES; i++)
    {
      coding_category_system[i] = Qnil;
      dump_add_root_lisp_object (&coding_category_system[i]);
      coding_category_by_priority[i] = i;
    }

  dump_add_opaque (coding_category_by_priority,
		   sizeof (coding_category_by_priority));

  all_coding_detectors = Dynarr_new2 (detector_dynarr, struct detector);
  dump_add_root_block_ptr (&all_coding_detectors,
			    &detector_dynarr_description);

  dump_add_opaque_int (&coding_system_tick);
  dump_add_opaque_int (&coding_detector_count);
  dump_add_opaque_int (&coding_detector_category_count);

  INITIALIZE_CODING_SYSTEM_TYPE (no_conversion,
				 "no-conversion-coding-system-p");
  CODING_SYSTEM_HAS_METHOD (no_conversion, convert);

  INITIALIZE_DETECTOR (no_conversion);
  DETECTOR_HAS_METHOD (no_conversion, detect);
  INITIALIZE_DETECTOR_CATEGORY (no_conversion, no_conversion);

  INITIALIZE_CODING_SYSTEM_TYPE_WITH_DATA (convert_eol,
					   "convert-eol-coding-system-p");
  CODING_SYSTEM_HAS_METHOD (convert_eol, print);
  CODING_SYSTEM_HAS_METHOD (convert_eol, convert);
  CODING_SYSTEM_HAS_METHOD (convert_eol, getprop);
  CODING_SYSTEM_HAS_METHOD (convert_eol, putprop);
  CODING_SYSTEM_HAS_METHOD (convert_eol, conversion_end_type);
  CODING_SYSTEM_HAS_METHOD (convert_eol, canonicalize_after_coding);
  CODING_SYSTEM_HAS_METHOD (convert_eol, init_coding_stream);

  INITIALIZE_CODING_SYSTEM_TYPE_WITH_DATA (undecided,
					   "undecided-coding-system-p");
  CODING_SYSTEM_HAS_METHOD (undecided, init);
  CODING_SYSTEM_HAS_METHOD (undecided, mark);
  CODING_SYSTEM_HAS_METHOD (undecided, print);
  CODING_SYSTEM_HAS_METHOD (undecided, convert);
  CODING_SYSTEM_HAS_METHOD (undecided, putprop);
  CODING_SYSTEM_HAS_METHOD (undecided, getprop);
  CODING_SYSTEM_HAS_METHOD (undecided, init_coding_stream);
  CODING_SYSTEM_HAS_METHOD (undecided, rewind_coding_stream);
  CODING_SYSTEM_HAS_METHOD (undecided, finalize_coding_stream);
  CODING_SYSTEM_HAS_METHOD (undecided, mark_coding_stream);
  CODING_SYSTEM_HAS_METHOD (undecided, canonicalize);
  CODING_SYSTEM_HAS_METHOD (undecided, canonicalize_after_coding);

  INITIALIZE_CODING_SYSTEM_TYPE_WITH_DATA (chain, "chain-coding-system-p");

  CODING_SYSTEM_HAS_METHOD (chain, print);
  CODING_SYSTEM_HAS_METHOD (chain, canonicalize);
  CODING_SYSTEM_HAS_METHOD (chain, init);
  CODING_SYSTEM_HAS_METHOD (chain, mark);
  CODING_SYSTEM_HAS_METHOD (chain, mark_coding_stream);
  CODING_SYSTEM_HAS_METHOD (chain, convert);
  CODING_SYSTEM_HAS_METHOD (chain, rewind_coding_stream);
  CODING_SYSTEM_HAS_METHOD (chain, finalize_coding_stream);
  CODING_SYSTEM_HAS_METHOD (chain, finalize);
  CODING_SYSTEM_HAS_METHOD (chain, putprop);
  CODING_SYSTEM_HAS_METHOD (chain, getprop);
  CODING_SYSTEM_HAS_METHOD (chain, conversion_end_type);
  CODING_SYSTEM_HAS_METHOD (chain, canonicalize_after_coding);

#ifdef DEBUG_XEMACS
  INITIALIZE_CODING_SYSTEM_TYPE (internal, "internal-coding-system-p");
  CODING_SYSTEM_HAS_METHOD (internal, convert);
#endif

#ifdef HAVE_ZLIB
  INITIALIZE_CODING_SYSTEM_TYPE_WITH_DATA (gzip, "gzip-coding-system-p");
  CODING_SYSTEM_HAS_METHOD (gzip, conversion_end_type);
  CODING_SYSTEM_HAS_METHOD (gzip, convert);
  CODING_SYSTEM_HAS_METHOD (gzip, init);
  CODING_SYSTEM_HAS_METHOD (gzip, print);
  CODING_SYSTEM_HAS_METHOD (gzip, init_coding_stream);
  CODING_SYSTEM_HAS_METHOD (gzip, rewind_coding_stream);
  CODING_SYSTEM_HAS_METHOD (gzip, putprop);
  CODING_SYSTEM_HAS_METHOD (gzip, getprop);
#endif
}

void
reinit_coding_system_type_create (void)
{
  REINITIALIZE_CODING_SYSTEM_TYPE (no_conversion);
  REINITIALIZE_CODING_SYSTEM_TYPE (convert_eol);
  REINITIALIZE_CODING_SYSTEM_TYPE (undecided);
  REINITIALIZE_CODING_SYSTEM_TYPE (chain);
#if 0
  REINITIALIZE_CODING_SYSTEM_TYPE (text_file_wrapper);
#endif /* 0 */
#ifdef DEBUG_XEMACS
  REINITIALIZE_CODING_SYSTEM_TYPE (internal);
#endif
#ifdef HAVE_ZLIB
  REINITIALIZE_CODING_SYSTEM_TYPE (gzip);
#endif
}

void
reinit_vars_of_file_coding (void)
{
}

void
vars_of_file_coding (void)
{
  /* We always have file-coding support */
  Fprovide (intern ("file-coding"));

  QScoding_system_cookie = build_string (";;;###coding system: ");
  staticpro (&QScoding_system_cookie);

#ifdef HAVE_DEFAULT_EOL_DETECTION
  /* #### Find a more appropriate place for this comment.
     WARNING: The existing categories are intimately tied to the function
     `coding-system-category' in coding.el.  If you change a category, or
     change the layout of any coding system associated with a category, you
     need to check that function and make sure it's written properly. */

  Fprovide (intern ("unix-default-eol-detection"));
#endif

  DEFVAR_LISP ("keyboard-coding-system", &Vkeyboard_coding_system /*
Coding system used for TTY keyboard input.
Not used under a windowing system.
*/ );
  Vkeyboard_coding_system = Qnil;

  DEFVAR_LISP ("terminal-coding-system", &Vterminal_coding_system /*
Coding system used for TTY display output.
Not used under a windowing system.
*/ );
  Vterminal_coding_system = Qnil;

  DEFVAR_LISP ("coding-system-for-read", &Vcoding_system_for_read /*
Overriding coding system used when reading from a file or process.
You should bind this variable with `let', but do not set it globally.
If this is non-nil, it specifies the coding system that will be used
to decode input on read operations, such as from a file or process.
It overrides `buffer-file-coding-system-for-read',
`insert-file-contents-pre-hook', etc.  Use those variables instead of
this one for permanent changes to the environment.  */ );
  Vcoding_system_for_read = Qnil;

  DEFVAR_LISP ("coding-system-for-write",
               &Vcoding_system_for_write /*
Overriding coding system used when writing to a file or process.
You should bind this variable with `let', but do not set it globally.
If this is non-nil, it specifies the coding system that will be used
to encode output for write operations, such as to a file or process.
It overrides `buffer-file-coding-system', `write-region-pre-hook', etc.
Use those variables instead of this one for permanent changes to the
environment.  */ );
  Vcoding_system_for_write = Qnil;

  DEFVAR_LISP ("file-name-coding-system", &Vfile_name_coding_system /*
Coding system used to convert pathnames when accessing files.
*/ );
  Vfile_name_coding_system = Qnil;

  DEFVAR_BOOL ("enable-multibyte-characters", &enable_multibyte_characters /*
Setting this has no effect.  It is purely for FSF compatibility.
*/ );
  enable_multibyte_characters = 1;

  Vchain_canonicalize_hash_table =
    make_lisp_hash_table (50, HASH_TABLE_NON_WEAK, HASH_TABLE_EQUAL);
  staticpro (&Vchain_canonicalize_hash_table);

#ifdef DEBUG_XEMACS
  DEFVAR_LISP ("debug-coding-detection", &Vdebug_coding_detection /*
If non-nil, display debug information about detection operations in progress.
Information is displayed on stderr.
*/ );
  Vdebug_coding_detection = Qnil;
#endif
}

/* #### reformat this for consistent appearance? */

void
complex_vars_of_file_coding (void)
{
  Fmake_coding_system
    (Qconvert_eol_cr, Qconvert_eol,
     build_msg_string ("Convert CR to LF"),
     nconc2 (list6 (Qdocumentation,
		    build_msg_string (
"Converts CR (used to mark the end of a line on Macintosh systems) to LF\n"
"(used internally and under Unix to mark the end of a line)."),
		    Qmnemonic, build_string ("CR->LF"),
		    Qsubtype, Qcr),
	     /* VERY IMPORTANT!  Tell make-coding-system not to generate
		subsidiaries -- it needs the coding systems we're creating
		to do so! */
	     list2 (Qeol_type, Qlf)));

  Fmake_coding_system
    (Qconvert_eol_lf, Qconvert_eol,
     build_msg_string ("Convert LF to LF (do nothing)"),
     nconc2 (list6 (Qdocumentation,
		    build_msg_string (
"Do nothing."),
		    Qmnemonic, build_string ("LF->LF"),
		    Qsubtype, Qlf),
	     /* VERY IMPORTANT!  Tell make-coding-system not to generate
		subsidiaries -- it needs the coding systems we're creating
		to do so! */
	     list2 (Qeol_type, Qlf)));

  Fmake_coding_system
    (Qconvert_eol_crlf, Qconvert_eol,
     build_msg_string ("Convert CRLF to LF"),
     nconc2 (list6 (Qdocumentation,
		    build_msg_string (
"Converts CR+LF (used to mark the end of a line on Macintosh systems) to LF\n"
"(used internally and under Unix to mark the end of a line)."),
		    Qmnemonic, build_string ("CRLF->LF"),
		    Qsubtype, Qcrlf),
	     /* VERY IMPORTANT!  Tell make-coding-system not to generate
		subsidiaries -- it needs the coding systems we're creating
		to do so! */
	     list2 (Qeol_type, Qlf)));

  Fmake_coding_system
    (Qconvert_eol_autodetect, Qconvert_eol,
     build_msg_string ("Autodetect EOL type"),
     nconc2 (list6 (Qdocumentation,
		    build_msg_string (
"Autodetect the end-of-line type."),
		    Qmnemonic, build_string ("Auto-EOL"),
		    Qsubtype, Qnil),
	     /* VERY IMPORTANT!  Tell make-coding-system not to generate
		subsidiaries -- it needs the coding systems we're creating
		to do so! */
	     list2 (Qeol_type, Qlf)));

  Fmake_coding_system
    (Qundecided, Qundecided,
     build_msg_string ("Undecided (auto-detect)"),
     nconc2 (list4 (Qdocumentation,
		    build_msg_string
		    ("Automatically detects the correct encoding."),
		    Qmnemonic, build_string ("Auto")),
	     list6 (Qdo_eol, Qt, Qdo_coding, Qt,
		    /* We do EOL detection ourselves so we don't need to be
		       wrapped in an EOL detector. (It doesn't actually hurt,
		       though, I don't think.) */
		    Qeol_type, Qlf)));

  Fmake_coding_system
    (intern ("undecided-dos"), Qundecided,
     build_msg_string ("Undecided (auto-detect) (CRLF)"),
     nconc2 (list4 (Qdocumentation,
		    build_msg_string
		    ("Automatically detects the correct encoding; EOL type of CRLF forced."),
		    Qmnemonic, build_string ("Auto")),
	     list4 (Qdo_coding, Qt,
		    Qeol_type, Qcrlf)));

  Fmake_coding_system
    (intern ("undecided-unix"), Qundecided,
     build_msg_string ("Undecided (auto-detect) (LF)"),
     nconc2 (list4 (Qdocumentation,
		    build_msg_string
		    ("Automatically detects the correct encoding; EOL type of LF forced."),
		    Qmnemonic, build_string ("Auto")),
	     list4 (Qdo_coding, Qt,
		    Qeol_type, Qlf)));

  Fmake_coding_system
    (intern ("undecided-mac"), Qundecided,
     build_msg_string ("Undecided (auto-detect) (CR)"),
     nconc2 (list4 (Qdocumentation,
		    build_msg_string
		    ("Automatically detects the correct encoding; EOL type of CR forced."),
		    Qmnemonic, build_string ("Auto")),
	     list4 (Qdo_coding, Qt,
		    Qeol_type, Qcr)));

  /* Need to create this here or we're really screwed. */
  Fmake_coding_system
    (Qraw_text, Qno_conversion,
     build_msg_string ("Raw Text"),
     list4 (Qdocumentation,
	    build_msg_string ("Raw text converts only line-break codes, and acts otherwise like `binary'."),
	    Qmnemonic, build_string ("Raw")));

  Fmake_coding_system
    (Qbinary, Qno_conversion,
     build_msg_string ("Binary"),
     list6 (Qdocumentation,
	    build_msg_string (
"This coding system is as close as it comes to doing no conversion.\n"
"On input, each byte is converted directly into the character\n"
"with the corresponding code -- i.e. from the `ascii', `control-1',\n"
"or `latin-1' character sets.  On output, these characters are\n"
"converted back to the corresponding bytes, and other characters\n"
"are converted to the default character, i.e. `~'."),
	    Qeol_type, Qlf,
	    Qmnemonic, build_string ("Binary")));

  /* Formerly aliased to raw-text!  Completely bogus and not even the same
     as FSF Emacs. */
  Fdefine_coding_system_alias (Qno_conversion, Qbinary);
  Fdefine_coding_system_alias (intern ("no-conversion-unix"),
			       intern ("raw-text-unix"));
  Fdefine_coding_system_alias (intern ("no-conversion-dos"),
			       intern ("raw-text-dos"));
  Fdefine_coding_system_alias (intern ("no-conversion-mac"),
			       intern ("raw-text-mac"));

  /* These three below will get their defaults set correctly
     in code-init.el.  We init them now so we can handle stuff at dump
     time before we get to code-init.el. */
  Fdefine_coding_system_alias (Qnative, Qbinary);
  Fdefine_coding_system_alias (Qterminal, Qbinary);
  Fdefine_coding_system_alias (Qkeyboard, Qbinary);

  Fdefine_coding_system_alias (Qfile_name, Qnative);
  Fdefine_coding_system_alias (Qidentity, Qconvert_eol_lf);
  
  /* Need this for bootstrapping */
  coding_category_system[detector_category_no_conversion] =
    Fget_coding_system (Qraw_text);
}
