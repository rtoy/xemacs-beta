/* Header file for text manipulation primitives and macros.
   Copyright (C) 1985-1995 Free Software Foundation, Inc.
   Copyright (C) 1995 Sun Microsystems, Inc.
   Copyright (C) 2000, 2001, 2002, 2003, 2004 Ben Wing.

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

/* Synched up with: FSF 19.30. */

/* Authorship:

   Mostly written by Ben Wing, starting around 1995.
   Current TO_IN/EXTERNAL_FORMAT macros written by Martin Buchholz,
     designed by Ben Wing based on earlier macros by Ben Wing.
   Separated out June 18, 2000 from buffer.h into text.h.
 */

#ifndef INCLUDED_text_h_
#define INCLUDED_text_h_

#ifdef HAVE_WCHAR_H
#include <wchar.h>
#else
size_t wcslen (const wchar_t *);
#endif
#ifndef HAVE_STRLWR
char *strlwr (char *);
#endif
#ifndef HAVE_STRUPR
char *strupr (char *);
#endif

BEGIN_C_DECLS

/* ---------------------------------------------------------------------- */
/*                     Super-basic character properties                   */
/* ---------------------------------------------------------------------- */

/* These properties define the specifics of how our current encoding fits
   in the basic model used for the encoding.  Because this model is the same
   as is used for UTF-8, all these properties could be defined for it, too.
   This would instantly make the rest of this file work with UTF-8 (with
   the exception of a few called functions that would need to be redefined).

   (UTF-2000 implementers, take note!)
*/

/* If you want more than this, you need to include charset.h */

#ifndef MULE

#define rep_bytes_by_first_byte(fb) 1
#define byte_ascii_p(byte) 1
#define MAX_ICHAR_LEN 1

#else /* MULE */

/* These are carefully designed to work if BYTE is signed or unsigned. */
/* Note that SPC and DEL are considered ASCII, not control. */

#define byte_ascii_p(byte) (((byte) & ~0x7f) == 0)
#define byte_c0_p(byte)    (((byte) & ~0x1f) == 0)
#define byte_c1_p(byte)    (((byte) & ~0x1f) == 0x80)

/* Does BYTE represent the first byte of a character? */

#ifdef ERROR_CHECK_TEXT

DECLARE_INLINE_HEADER (
int
ibyte_first_byte_p_1 (int byte, const char *file, int line)
)
{
  assert_at_line (byte >= 0 && byte < 256, file, line);
  return byte < 0xA0;
}

#define ibyte_first_byte_p(byte) \
  ibyte_first_byte_p_1 (byte, __FILE__, __LINE__) 

#else

#define ibyte_first_byte_p(byte) ((byte) < 0xA0)

#endif

#ifdef ERROR_CHECK_TEXT

/* Does BYTE represent the first byte of a multi-byte character? */

DECLARE_INLINE_HEADER (
int
ibyte_leading_byte_p_1 (int byte, const char *file, int line)
)
{
  assert_at_line (byte >= 0 && byte < 256, file, line);
  return byte_c1_p (byte);
}

#define ibyte_leading_byte_p(byte) \
  ibyte_leading_byte_p_1 (byte, __FILE__, __LINE__) 

#else

#define ibyte_leading_byte_p(byte) byte_c1_p (byte)

#endif

/* Table of number of bytes in the string representation of a character
   indexed by the first byte of that representation.

   This value can be derived in other ways -- e.g. something like
   XCHARSET_REP_BYTES (charset_by_leading_byte (first_byte))
   but it's faster this way. */
extern MODULE_API const Bytecount rep_bytes_by_first_byte[0xA0];

/* Number of bytes in the string representation of a character. */

#ifdef ERROR_CHECK_TEXT

DECLARE_INLINE_HEADER (
Bytecount
rep_bytes_by_first_byte_1 (int fb, const char *file, int line)
)
{
  assert_at_line (fb >= 0 && fb < 0xA0, file, line);
  return rep_bytes_by_first_byte[fb];
}

#define rep_bytes_by_first_byte(fb) \
  rep_bytes_by_first_byte_1 (fb, __FILE__, __LINE__) 

#else /* ERROR_CHECK_TEXT */

#define rep_bytes_by_first_byte(fb) (rep_bytes_by_first_byte[fb])

#endif /* ERROR_CHECK_TEXT */

/* Is this character represented by more than one byte in a string in the
   default format? */

#define ichar_multibyte_p(c) ((c) >= 0x80)

#define ichar_ascii_p(c) (!ichar_multibyte_p (c))

/* Maximum number of bytes per Emacs character when represented as text, in
 any format.
 */

#define MAX_ICHAR_LEN 4

#endif /* not MULE */

/* For more discussion, see text.c, "handling non-default formats" */

typedef enum internal_format
{
  FORMAT_DEFAULT,
  FORMAT_8_BIT_FIXED,
  FORMAT_16_BIT_FIXED, /* not implemented */
  FORMAT_32_BIT_FIXED  /* not implemented */
} Internal_Format;

#ifdef MULE
/* "OBJECT" below will usually be a buffer, string, or nil.  This needs to
   be passed in because the interpretation of 8-bit-fixed and 16-bit-fixed
   values may depend on the buffer, e.g. depending on what language the
   text in the buffer is in. */

/* True if Ichar CH can be represented in 8-bit-fixed format. */
#define ichar_8_bit_fixed_p(ch, object)   (((ch) & ~0xff) == 0)
/* Convert Ichar CH to an 8-bit int, as will be stored in the buffer. */
#define ichar_to_raw_8_bit_fixed(ch, object) ((Ibyte) (ch))
/* Convert the other way. */
#define raw_8_bit_fixed_to_ichar(ch, object) ((Ichar) (ch))

#define ichar_16_bit_fixed_p(ch, object)   (((ch) & ~0xffff) == 0)
/* Convert Ichar CH to a 16-bit int, as will be stored in the buffer. */
#define ichar_to_raw_16_bit_fixed(ch, object) ((UINT_16_BIT) (ch))
/* Convert the other way. */
#define raw_16_bit_fixed_to_ichar(ch, object) ((Ichar) (ch))

/* Convert Ichar CH to a 32-bit int, as will be stored in the buffer. */
#define ichar_to_raw_32_bit_fixed(ch, object) ((UINT_32_BIT) (ch))
/* Convert the other way. */
#define raw_32_bit_fixed_to_ichar(ch, object) ((Ichar) (ch))

/* Return the "raw value" of a character as stored in the buffer.  In the
   default format, this is just the same as the character.  In fixed-width
   formats, this is the actual value in the buffer, which will be limited
   to the range as established by the format.  This is used when searching
   for a character in a buffer -- it's faster to convert the character to
   the raw value and look for that, than repeatedly convert each raw value
   in the buffer into a character. */

DECLARE_INLINE_HEADER (
Raw_Ichar
ichar_to_raw (Ichar ch, Internal_Format fmt,
	      Lisp_Object UNUSED (object))
)
{
  switch (fmt)
    {
    case FORMAT_DEFAULT:
      return (Raw_Ichar) ch;
    case FORMAT_16_BIT_FIXED:
      text_checking_assert (ichar_16_bit_fixed_p (ch, object));
      return (Raw_Ichar) ichar_to_raw_16_bit_fixed (ch, object);
    case FORMAT_32_BIT_FIXED:
      return (Raw_Ichar) ichar_to_raw_32_bit_fixed (ch, object);
    default:
      text_checking_assert (fmt == FORMAT_8_BIT_FIXED);
      text_checking_assert (ichar_8_bit_fixed_p (ch, object));
      return (Raw_Ichar) ichar_to_raw_8_bit_fixed (ch, object);
    }
}

/* Return whether CH is representable in the given format in the given
   object. */

DECLARE_INLINE_HEADER (
int
ichar_fits_in_format (Ichar ch, Internal_Format fmt,
		      Lisp_Object UNUSED (object))
)
{
  switch (fmt)
    {
    case FORMAT_DEFAULT:
      return 1;
    case FORMAT_16_BIT_FIXED:
      return ichar_16_bit_fixed_p (ch, object);
    case FORMAT_32_BIT_FIXED:
      return 1;
    default:
      text_checking_assert (fmt == FORMAT_8_BIT_FIXED);
      return ichar_8_bit_fixed_p (ch, object);
    }
}

/* Assuming the formats are the same, return whether the two objects
   represent text in exactly the same way. */

DECLARE_INLINE_HEADER (
int
objects_have_same_internal_representation (Lisp_Object UNUSED (srcobj),
					   Lisp_Object UNUSED (dstobj))
)
{
  /* &&#### implement this properly when we allow per-object format
     differences */
  return 1;
}

#else

#define ichar_to_raw(ch, fmt, object) ((Raw_Ichar) (ch))
#define ichar_fits_in_format(ch, fmt, object) 1
#define objects_have_same_internal_representation(srcobj, dstobj) 1

#endif /* MULE */

MODULE_API int dfc_coding_system_is_unicode (Lisp_Object codesys);

DECLARE_INLINE_HEADER (
Bytecount dfc_external_data_len (const void *ptr, Lisp_Object codesys)
)
{
  if (dfc_coding_system_is_unicode (codesys))
    return sizeof (wchar_t) * wcslen ((wchar_t *) ptr);
  else
    return strlen ((char *) ptr);
}


/************************************************************************/
/*									*/
/*		   working with raw internal-format data		*/
/*									*/
/************************************************************************/

/*
  Use the following functions/macros on contiguous text in any of the
  internal formats.  Those that take a format arg work on all internal
  formats; the others work only on the default (variable-width under Mule)
  format.  If the text you're operating on is known to come from a buffer,
  use the buffer-level functions in buffer.h, which automatically know the
  correct format and handle the gap.

  Some terminology:

  "itext" appearing in the macros means "internal-format text" -- type
  `Ibyte *'.  Operations on such pointers themselves, rather than on the
  text being pointed to, have "itext" instead of "itext" in the macro
  name.  "ichar" in the macro names means an Ichar -- the representation
  of a character as a single integer rather than a series of bytes, as part
  of "itext".  Many of the macros below are for converting between the
  two representations of characters.

  Note also that we try to consistently distinguish between an "Ichar" and
  a Lisp character.  Stuff working with Lisp characters often just says
  "char", so we consistently use "Ichar" when that's what we're working
  with. */

/* The three golden rules of macros:

   1) Anything that's an lvalue can be evaluated more than once.

   2) Macros where anything else can be evaluated more than once should
      have the word "unsafe" in their name (exceptions may be made for
      large sets of macros that evaluate arguments of certain types more
      than once, e.g. struct buffer * arguments, when clearly indicated in
      the macro documentation).  These macros are generally meant to be
      called only by other macros that have already stored the calling
      values in temporary variables.

   3) Nothing else can be evaluated more than once.  Use inline
      functions, if necessary, to prevent multiple evaluation.

   NOTE: The functions and macros below are given full prototypes in their
   docs, even when the implementation is a macro.  In such cases, passing
   an argument of a type other than expected will produce undefined
   results.  Also, given that macros can do things functions can't (in
   particular, directly modify arguments as if they were passed by
   reference), the declaration syntax has been extended to include the
   call-by-reference syntax from C++, where an & after a type indicates
   that the argument is an lvalue and is passed by reference, i.e. the
   function can modify its value. (This is equivalent in C to passing a
   pointer to the argument, but without the need to explicitly worry about
   pointers.)

   When to capitalize macros:

   -- Capitalize macros doing stuff obviously impossible with (C)
   functions, e.g. directly modifying arguments as if they were passed by
   reference.

   -- Capitalize macros that evaluate *any* argument more than once regardless
   of whether that's "allowed" (e.g. buffer arguments).

   -- Capitalize macros that directly access a field in a Lisp_Object or
   its equivalent underlying structure.  In such cases, access through the
   Lisp_Object precedes the macro with an X, and access through the underlying
   structure doesn't.

   -- Capitalize certain other basic macros relating to Lisp_Objects; e.g.
      FRAMEP, CHECK_FRAME, etc.

   -- Try to avoid capitalizing any other macros.
*/

/* ---------------------------------------------------------------------- */
/*      Working with itext's (pointers to internally-formatted text)    */
/* ---------------------------------------------------------------------- */

/* Given an itext, does it point to the beginning of a character?
 */

#ifdef MULE
# define valid_ibyteptr_p(ptr) ibyte_first_byte_p (* (ptr))
#else
# define valid_ibyteptr_p(ptr) 1
#endif

/* If error-checking is enabled, assert that the given itext points to
   the beginning of a character.  Otherwise, do nothing.
   */

#define assert_valid_ibyteptr(ptr) text_checking_assert (valid_ibyteptr_p (ptr))

/* Given a itext (assumed to point at the beginning of a character),
   modify that pointer so it points to the beginning of the next character.

   Note that INC_IBYTEPTR() and DEC_IBYTEPTR() have to be written in
   completely separate ways.  INC_IBYTEPTR() cannot use the DEC_IBYTEPTR()
   trick of looking for a valid first byte because it might run off
   the end of the string.  DEC_IBYTEPTR() can't use the INC_IBYTEPTR()
   method because it doesn't have easy access to the first byte of
   the character it's moving over. */

#define INC_IBYTEPTR(ptr) do {			\
  assert_valid_ibyteptr (ptr);			\
  (ptr) += rep_bytes_by_first_byte (* (ptr));	\
} while (0)

#define INC_IBYTEPTR_FMT(ptr, fmt)					   \
do {									   \
  Internal_Format __icf_fmt = (fmt);					   \
  switch (__icf_fmt)							   \
    {									   \
    case FORMAT_DEFAULT:						   \
      INC_IBYTEPTR (ptr);						   \
      break;								   \
    case FORMAT_16_BIT_FIXED:						   \
      text_checking_assert ((void *) ptr == ALIGN_PTR (ptr, UINT_16_BIT)); \
      (ptr) += 2;							   \
      break;								   \
    case FORMAT_32_BIT_FIXED:						   \
      text_checking_assert ((void *) ptr == ALIGN_PTR (ptr, UINT_32_BIT)); \
      (ptr) += 4;							   \
      break;								   \
    default:								   \
      text_checking_assert (fmt == FORMAT_8_BIT_FIXED);			   \
      (ptr)++;								   \
      break;								   \
    }									   \
} while (0)

/* Given a itext (assumed to point at the beginning of a character or at
   the very end of the text), modify that pointer so it points to the
   beginning of the previous character.
   */

#ifdef ERROR_CHECK_TEXT
/* We use a separate definition to avoid warnings about unused dc_ptr1 */
#define DEC_IBYTEPTR(ptr) do {						      \
  const Ibyte *dc_ptr1 = (ptr);						      \
  do {									      \
    (ptr)--;								      \
  } while (!valid_ibyteptr_p (ptr));					      \
  text_checking_assert (dc_ptr1 - (ptr) == rep_bytes_by_first_byte (*(ptr))); \
} while (0)
#else
#define DEC_IBYTEPTR(ptr) do {						      \
  do {									      \
    (ptr)--;								      \
  } while (!valid_ibyteptr_p (ptr));					      \
} while (0)
#endif /* ERROR_CHECK_TEXT */

#define DEC_IBYTEPTR_FMT(ptr, fmt)					   \
do {									   \
  Internal_Format __icf_fmt = (fmt);					   \
  switch (__icf_fmt)							   \
    {									   \
    case FORMAT_DEFAULT:						   \
      DEC_IBYTEPTR (ptr);						   \
      break;								   \
    case FORMAT_16_BIT_FIXED:						   \
      text_checking_assert ((void *) ptr == ALIGN_PTR (ptr, UINT_16_BIT)); \
      (ptr) -= 2;							   \
      break;								   \
    case FORMAT_32_BIT_FIXED:						   \
      text_checking_assert ((void *) ptr == ALIGN_PTR (ptr, UINT_32_BIT)); \
      (ptr) -= 4;							   \
      break;								   \
    default:								   \
      text_checking_assert (fmt == FORMAT_8_BIT_FIXED);			   \
      (ptr)--;								   \
      break;								   \
    }									   \
} while (0)

#ifdef MULE

/* Make sure that PTR is pointing to the beginning of a character.  If not,
   back up until this is the case.  Note that there are not too many places
   where it is legitimate to do this sort of thing.  It's an error if
   you're passed an "invalid" char * pointer.  NOTE: PTR *must* be pointing
   to a valid part of the string (i.e.  not the very end, unless the string
   is zero-terminated or something) in order for this function to not cause
   crashes.
   */

/* Note that this reads the byte at *PTR! */

#define VALIDATE_IBYTEPTR_BACKWARD(ptr) do {	\
  while (!valid_ibyteptr_p (ptr)) ptr--;		\
} while (0)

/* Make sure that PTR is pointing to the beginning of a character.  If not,
   move forward until this is the case.  Note that there are not too many
   places where it is legitimate to do this sort of thing.  It's an error
   if you're passed an "invalid" char * pointer.
   */

/* This needs to be trickier than VALIDATE_IBYTEPTR_BACKWARD() to avoid the
   possibility of running off the end of the string. */

#define VALIDATE_IBYTEPTR_FORWARD(ptr) do {	\
  Ibyte *vcf_ptr = (ptr);			\
  VALIDATE_IBYTEPTR_BACKWARD (vcf_ptr);		\
  if (vcf_ptr != (ptr))				\
    {						\
      (ptr) = vcf_ptr;				\
      INC_IBYTEPTR (ptr);			\
    }						\
} while (0)

#else /* not MULE */
#define VALIDATE_IBYTEPTR_BACKWARD(ptr)
#define VALIDATE_IBYTEPTR_FORWARD(ptr)
#endif /* not MULE */

#ifdef MULE

/* Given a Ibyte string at PTR of size N, possibly with a partial
   character at the end, return the size of the longest substring of
   complete characters.  Does not assume that the byte at *(PTR + N) is
   readable.  Note that there are not too many places where it is
   legitimate to do this sort of thing.  It's an error if you're passed an
   "invalid" offset. */

DECLARE_INLINE_HEADER (
Bytecount
validate_ibyte_string_backward (const Ibyte *ptr, Bytecount n)
)
{
  const Ibyte *ptr2;

  if (n == 0)
    return n;
  ptr2 = ptr + n - 1;
  VALIDATE_IBYTEPTR_BACKWARD (ptr2);
  if (ptr2 + rep_bytes_by_first_byte (*ptr2) != ptr + n)
    return ptr2 - ptr;
  return n;
}

#else

#define validate_ibyte_string_backward(ptr, n) (n)

#endif /* MULE */

#ifdef ERROR_CHECK_TEXT
#define ASSERT_ASCTEXT_ASCII_LEN(ptr, len)			\
do {								\
  int aia2;							\
  const Ascbyte *aia2ptr = (ptr);				\
  int aia2len = (len);						\
								\
  for (aia2 = 0; aia2 < aia2len; aia2++)			\
    assert (aia2ptr[aia2] >= 0x00 && aia2ptr[aia2] < 0x7F);	\
} while (0)
#define ASSERT_ASCTEXT_ASCII(ptr)			\
do {							\
  const Ascbyte *aiaz2 = (ptr);				\
  ASSERT_ASCTEXT_ASCII_LEN (aiaz2, strlen (aiaz2));	\
} while (0)
#else
#define ASSERT_ASCTEXT_ASCII_LEN(ptr, len)
#define ASSERT_ASCTEXT_ASCII(ptr)
#endif

/* -------------------------------------------------------------- */
/*      Working with the length (in bytes and characters) of a    */
/*               section of internally-formatted text 	          */
/* -------------------------------------------------------------- */

#ifdef MULE

MODULE_API Charcount
bytecount_to_charcount_fun (const Ibyte *ptr, Bytecount len);
MODULE_API Bytecount
charcount_to_bytecount_fun (const Ibyte *ptr, Charcount len);

/* Given a pointer to a text string and a length in bytes, return
   the equivalent length in characters. */

DECLARE_INLINE_HEADER (
Charcount
bytecount_to_charcount (const Ibyte *ptr, Bytecount len)
)
{
  if (len < 20) /* Just a random guess, but it should be more or less correct.
		   If number of bytes is small, just do a simple loop,
		   which should be more efficient. */
    {
      Charcount count = 0;
      const Ibyte *end = ptr + len;
      while (ptr < end)
	{
	  INC_IBYTEPTR (ptr);
	  count++;
	}
      /* Bomb out if the specified substring ends in the middle
	 of a character.  Note that we might have already gotten
	 a core dump above from an invalid reference, but at least
	 we will get no farther than here.

	 This also catches len < 0. */
      text_checking_assert (ptr == end);

      return count;
    }
  else
    return bytecount_to_charcount_fun (ptr, len);
}

/* Given a pointer to a text string and a length in characters, return the
   equivalent length in bytes.
*/

DECLARE_INLINE_HEADER (
Bytecount
charcount_to_bytecount (const Ibyte *ptr, Charcount len)
)
{
  text_checking_assert (len >= 0);
  if (len < 20) /* See above */
    {
      const Ibyte *newptr = ptr;
      while (len > 0)
	{
	  INC_IBYTEPTR (newptr);
	  len--;
	}
      return newptr - ptr;
    }
  else
    return charcount_to_bytecount_fun (ptr, len);
}

MODULE_API Bytecount
charcount_to_bytecount_down_fun (const Ibyte *ptr, Charcount len);

/* Given a pointer to a text string and a length in bytes, return
   the equivalent length in characters of the stretch [PTR - LEN, PTR). */

DECLARE_INLINE_HEADER (
Charcount
bytecount_to_charcount_down (const Ibyte *ptr, Bytecount len)
)
{
  /* No need to be clever here */
  return bytecount_to_charcount (ptr - len, len);
}

/* Given a pointer to a text string and a length in characters, return the
   equivalent length in bytes of the stretch of characters of that length
   BEFORE the pointer.
*/

DECLARE_INLINE_HEADER (
Bytecount
charcount_to_bytecount_down (const Ibyte *ptr, Charcount len)
)
{
#define SLEDGEHAMMER_CHECK_TEXT
#ifdef SLEDGEHAMMER_CHECK_TEXT
  Charcount len1 = len;
  Bytecount ret1, ret2;

  /* To test the correctness of the function version, always do the
     calculation both ways and check that the values are the same. */
  text_checking_assert (len >= 0);
  {
    const Ibyte *newptr = ptr;
    while (len1 > 0)
      {
	DEC_IBYTEPTR (newptr);
	len1--;
      }
    ret1 = ptr - newptr;
  }
  ret2 = charcount_to_bytecount_down_fun (ptr, len);
  text_checking_assert (ret1 == ret2);
  return ret1;
#else
  text_checking_assert (len >= 0);
  if (len < 20) /* See above */
    {
      const Ibyte *newptr = ptr;
      while (len > 0)
	{
	  DEC_IBYTEPTR (newptr);
	  len--;
	}
      return ptr - newptr;
    }
  else
    return charcount_to_bytecount_down_fun (ptr, len);
#endif /* SLEDGEHAMMER_CHECK_TEXT */
}

/* Given a pointer to a text string in the specified format and a length in
   bytes, return the equivalent length in characters.
*/

DECLARE_INLINE_HEADER (
Charcount
bytecount_to_charcount_fmt (const Ibyte *ptr, Bytecount len,
			    Internal_Format fmt)
)
{
  switch (fmt)
    {
    case FORMAT_DEFAULT:
      return bytecount_to_charcount (ptr, len);
    case FORMAT_16_BIT_FIXED:
      text_checking_assert ((void *) ptr == ALIGN_PTR (ptr, UINT_16_BIT));
      return (Charcount) (len << 1);
    case FORMAT_32_BIT_FIXED:
      text_checking_assert ((void *) ptr == ALIGN_PTR (ptr, UINT_32_BIT));
      return (Charcount) (len << 2);
    default:
      text_checking_assert (fmt == FORMAT_8_BIT_FIXED);
      return (Charcount) len;
    }
}

/* Given a pointer to a text string in the specified format and a length in
   characters, return the equivalent length in bytes.
*/

DECLARE_INLINE_HEADER (
Bytecount
charcount_to_bytecount_fmt (const Ibyte *ptr, Charcount len,
			    Internal_Format fmt)
)
{
  switch (fmt)
    {
    case FORMAT_DEFAULT:
      return charcount_to_bytecount (ptr, len);
    case FORMAT_16_BIT_FIXED:
      text_checking_assert ((void *) ptr == ALIGN_PTR (ptr, UINT_16_BIT));
      text_checking_assert (!(len & 1));
      return (Bytecount) (len >> 1);
    case FORMAT_32_BIT_FIXED:
      text_checking_assert (!(len & 3));
      text_checking_assert ((void *) ptr == ALIGN_PTR (ptr, UINT_32_BIT));
      return (Bytecount) (len >> 2);
    default:
      text_checking_assert (fmt == FORMAT_8_BIT_FIXED);
      return (Bytecount) len;
    }
}

#else

#define bytecount_to_charcount(ptr, len) ((Charcount) (len))
#define bytecount_to_charcount_fmt(ptr, len, fmt) ((Charcount) (len))
#define charcount_to_bytecount(ptr, len) ((Bytecount) (len))
#define charcount_to_bytecount_fmt(ptr, len, fmt) ((Bytecount) (len))

#endif /* MULE */

/* Return the length of the first character at PTR.  Equivalent to
   charcount_to_bytecount (ptr, 1).

   [Since charcount_to_bytecount() is Written as inline, a smart compiler
   should really optimize charcount_to_bytecount (ptr, 1) to the same as
   the following, with no error checking.  But since this idiom occurs so
   often, we'll be helpful and define a special macro for it.]
*/
     
#define itext_ichar_len(ptr) rep_bytes_by_first_byte (*(ptr))

/* Return the length of the first character at PTR, which is in the
   specified internal format.  Equivalent to charcount_to_bytecount_fmt
   (ptr, 1, fmt).
*/
     
DECLARE_INLINE_HEADER (
Bytecount
itext_ichar_len_fmt (const Ibyte *USED_IF_MULE_OR_CHECK_TEXT (ptr),
		     Internal_Format fmt)
)
{
  switch (fmt)
    {
    case FORMAT_DEFAULT:
      return itext_ichar_len (ptr);
    case FORMAT_16_BIT_FIXED:
      text_checking_assert ((void *) ptr == ALIGN_PTR (ptr, UINT_16_BIT));
      return 2;
    case FORMAT_32_BIT_FIXED:
      text_checking_assert ((void *) ptr == ALIGN_PTR (ptr, UINT_32_BIT));
      return 4;
    default:
      text_checking_assert (fmt == FORMAT_8_BIT_FIXED);
      return 1;
    }
}

/* Return a pointer to the beginning of the character offset N (in
   characters) from PTR.
*/

DECLARE_INLINE_HEADER (
const Ibyte *
itext_n_addr (const Ibyte *ptr, Charcount offset)
)
{
  return ptr + charcount_to_bytecount (ptr, offset);
}

/* Given a itext and an offset into the text pointed to by the itext,
   modify the offset so it points to the beginning of the next character.
*/

#define INC_BYTECOUNT(ptr, pos) do {			\
  assert_valid_ibyteptr (ptr);				\
  (pos += rep_bytes_by_first_byte (* ((ptr) + (pos))));	\
} while (0)

/* -------------------------------------------------------------------- */
/*      Retrieving or changing the character pointed to by a itext    */
/* -------------------------------------------------------------------- */

#define simple_itext_ichar(ptr)		((Ichar) (ptr)[0])
#define simple_set_itext_ichar(ptr, x) \
	((ptr)[0] = (Ibyte) (x), (Bytecount) 1)
#define simple_itext_copy_ichar(src, dst) \
	((dst)[0] = *(src), (Bytecount) 1)

#ifdef MULE

MODULE_API Ichar non_ascii_itext_ichar (const Ibyte *ptr);
MODULE_API Bytecount non_ascii_set_itext_ichar (Ibyte *ptr, Ichar c);
MODULE_API Bytecount non_ascii_itext_copy_ichar (const Ibyte *src, Ibyte *dst);

/* Retrieve the character pointed to by PTR as an Ichar. */

DECLARE_INLINE_HEADER (
Ichar
itext_ichar (const Ibyte *ptr)
)
{
  return byte_ascii_p (*ptr) ?
    simple_itext_ichar (ptr) :
    non_ascii_itext_ichar (ptr);
}

/* Retrieve the character pointed to by PTR (a pointer to text in the
   format FMT, coming from OBJECT [a buffer, string?, or nil]) as an
   Ichar.

   Note: For these and other *_fmt() functions, if you pass in a constant
   FMT, the switch will be optimized out of existence.  Therefore, there is
   no need to create separate versions for the various formats for
   "efficiency reasons".  In fact, we don't really need itext_ichar()
   and such written separately, but they are used often so it's simpler
   that way. */

DECLARE_INLINE_HEADER (
Ichar
itext_ichar_fmt (const Ibyte *ptr, Internal_Format fmt,
		 Lisp_Object UNUSED (object))
)
{
  switch (fmt)
    {
    case FORMAT_DEFAULT:
      return itext_ichar (ptr);
    case FORMAT_16_BIT_FIXED:
      text_checking_assert ((void *) ptr == ALIGN_PTR (ptr, UINT_16_BIT));
      return raw_16_bit_fixed_to_ichar (* (UINT_16_BIT *) ptr, object);
    case FORMAT_32_BIT_FIXED:
      text_checking_assert ((void *) ptr == ALIGN_PTR (ptr, UINT_32_BIT));
      return raw_32_bit_fixed_to_ichar (* (UINT_32_BIT *) ptr, object);
    default:
      text_checking_assert (fmt == FORMAT_8_BIT_FIXED);
      return raw_8_bit_fixed_to_ichar (*ptr, object);
    }
}

/* Return the character at PTR (which is in format FMT), suitable for
   comparison with an ASCII character.  This guarantees that if the
   character at PTR is ASCII (range 0 - 127), that character will be
   returned; otherwise, some character outside of the ASCII range will be
   returned, but not necessarily the character actually at PTR.  This will
   be faster than itext_ichar_fmt() for some formats -- in particular,
   FORMAT_DEFAULT. */

DECLARE_INLINE_HEADER (
Ichar
itext_ichar_ascii_fmt (const Ibyte *ptr, Internal_Format fmt,
		       Lisp_Object UNUSED (object))
)
{
  switch (fmt)
    {
    case FORMAT_DEFAULT:
      return (Ichar) *ptr;
    case FORMAT_16_BIT_FIXED:
      text_checking_assert ((void *) ptr == ALIGN_PTR (ptr, UINT_16_BIT));
      return raw_16_bit_fixed_to_ichar (* (UINT_16_BIT *) ptr, object);
    case FORMAT_32_BIT_FIXED:
      text_checking_assert ((void *) ptr == ALIGN_PTR (ptr, UINT_32_BIT));
      return raw_32_bit_fixed_to_ichar (* (UINT_32_BIT *) ptr, object);
    default:
      text_checking_assert (fmt == FORMAT_8_BIT_FIXED);
      return raw_8_bit_fixed_to_ichar (*ptr, object);
    }
}

/* Return the "raw value" of the character at PTR, in format FMT.  This is
   useful when searching for a character; convert the character using
   ichar_to_raw(). */

DECLARE_INLINE_HEADER (
Raw_Ichar
itext_ichar_raw_fmt (const Ibyte *ptr, Internal_Format fmt)
)
{
  switch (fmt)
    {
    case FORMAT_DEFAULT:
      return (Raw_Ichar) itext_ichar (ptr);
    case FORMAT_16_BIT_FIXED:
      text_checking_assert ((void *) ptr == ALIGN_PTR (ptr, UINT_16_BIT));
      return (Raw_Ichar) (* (UINT_16_BIT *) ptr);
    case FORMAT_32_BIT_FIXED:
      text_checking_assert ((void *) ptr == ALIGN_PTR (ptr, UINT_32_BIT));
      return (Raw_Ichar) (* (UINT_32_BIT *) ptr);
    default:
      text_checking_assert (fmt == FORMAT_8_BIT_FIXED);
      return (Raw_Ichar) (*ptr);
    }
}

/* Store the character CH (an Ichar) as internally-formatted text starting
   at PTR.  Return the number of bytes stored.
*/
     
DECLARE_INLINE_HEADER (
Bytecount
set_itext_ichar (Ibyte *ptr, Ichar x)
)
{
  return !ichar_multibyte_p (x) ?
    simple_set_itext_ichar (ptr, x) :
    non_ascii_set_itext_ichar (ptr, x);
}

/* Store the character CH (an Ichar) as internally-formatted text of
   format FMT starting at PTR, which comes from OBJECT.  Return the number
   of bytes stored.
*/
     
DECLARE_INLINE_HEADER (
Bytecount
set_itext_ichar_fmt (Ibyte *ptr, Ichar x, Internal_Format fmt,
		     Lisp_Object UNUSED (object))
)
{
  switch (fmt)
    {
    case FORMAT_DEFAULT:
      return set_itext_ichar (ptr, x);
    case FORMAT_16_BIT_FIXED:
      text_checking_assert (ichar_16_bit_fixed_p (x, object));
      text_checking_assert ((void *) ptr == ALIGN_PTR (ptr, UINT_16_BIT));
      * (UINT_16_BIT *) ptr = ichar_to_raw_16_bit_fixed (x, object);
      return 2;
    case FORMAT_32_BIT_FIXED:
      text_checking_assert ((void *) ptr == ALIGN_PTR (ptr, UINT_32_BIT));
      * (UINT_32_BIT *) ptr = ichar_to_raw_32_bit_fixed (x, object);
      return 4;
    default:
      text_checking_assert (fmt == FORMAT_8_BIT_FIXED);
      text_checking_assert (ichar_8_bit_fixed_p (x, object));
      *ptr = ichar_to_raw_8_bit_fixed (x, object);
      return 1;
    }
}

/* Retrieve the character pointed to by SRC and store it as
   internally-formatted text in DST.
*/

DECLARE_INLINE_HEADER (
Bytecount
itext_copy_ichar (const Ibyte *src, Ibyte *dst)
)
{
  return byte_ascii_p (*src) ?
    simple_itext_copy_ichar (src, dst) :
    non_ascii_itext_copy_ichar (src, dst);
}

#else /* not MULE */

# define itext_ichar(ptr) simple_itext_ichar (ptr)
# define itext_ichar_fmt(ptr, fmt, object) itext_ichar (ptr)
# define itext_ichar_ascii_fmt(ptr, fmt, object) itext_ichar (ptr)
# define itext_ichar_raw_fmt(ptr, fmt) itext_ichar (ptr)
# define set_itext_ichar(ptr, x) simple_set_itext_ichar (ptr, x)
# define set_itext_ichar_fmt(ptr, x, fmt, obj) set_itext_ichar (ptr, x)
# define itext_copy_ichar(src, dst) simple_itext_copy_ichar (src, dst)

#endif /* not MULE */

/* Retrieve the character at offset N (in characters) from PTR, as an
   Ichar.
*/
     
#define itext_ichar_n(ptr, offset) \
  itext_ichar (itext_n_addr (ptr, offset))


/* ---------------------------- */
/*      Working with Ichars     */
/* ---------------------------- */

/* NOTE: There are other functions/macros for working with Ichars in
   charset.h, for retrieving the charset of an Ichar, the length of an
   Ichar when converted to text, etc.
*/

#ifdef MULE

MODULE_API int non_ascii_valid_ichar_p (Ichar ch);

/* Return whether the given Ichar is valid.
 */

DECLARE_INLINE_HEADER (
int
valid_ichar_p (Ichar ch)
)
{
  return (! (ch & ~0xFF)) || non_ascii_valid_ichar_p (ch);
}

#else /* not MULE */

#define valid_ichar_p(ch) (! (ch & ~0xFF))

#endif /* not MULE */

DECLARE_INLINE_HEADER (
Lisp_Object
make_char (Ichar val)
)
{
  type_checking_assert (valid_ichar_p (val));
  return make_char_1 (val);
}

#define CHAR_INTP(x) (INTP (x) && valid_ichar_p (XINT (x)))

#define CHAR_OR_CHAR_INTP(x) (CHARP (x) || CHAR_INTP (x))

DECLARE_INLINE_HEADER (
Ichar
XCHAR_OR_CHAR_INT (Lisp_Object obj)
)
{
  return CHARP (obj) ? XCHAR (obj) : XINT (obj);
}

/* Signal an error if CH is not a valid character or integer Lisp_Object.
   If CH is an integer Lisp_Object, convert it to a character Lisp_Object,
   but merely by repackaging, without performing tests for char validity.
   */

#define CHECK_CHAR_COERCE_INT(x) do {		\
  if (CHARP (x))				\
     ;						\
  else if (CHAR_INTP (x))			\
    x = make_char (XINT (x));			\
  else						\
    x = wrong_type_argument (Qcharacterp, x);	\
} while (0)



/************************************************************************/
/*									*/
/*		          working with Lisp strings                     */
/*									*/
/************************************************************************/

#define string_char_length(s) \
  string_index_byte_to_char (s, XSTRING_LENGTH (s))
#define string_byte(s, i) (XSTRING_DATA (s)[i] + 0)
/* In case we ever allow strings to be in a different format ... */
#define set_string_byte(s, i, c) (XSTRING_DATA (s)[i] = (c))

#define ASSERT_VALID_CHAR_STRING_INDEX_UNSAFE(s, x) do {		\
  text_checking_assert ((x) >= 0 && x <= string_char_length (s));	\
} while (0)

#define ASSERT_VALID_BYTE_STRING_INDEX_UNSAFE(s, x) do {		\
  text_checking_assert ((x) >= 0 && x <= XSTRING_LENGTH (s));		\
  text_checking_assert (valid_ibyteptr_p (string_byte_addr (s, x)));	\
} while (0)

/* Convert offset I in string S to a pointer to text there. */
#define string_byte_addr(s, i) (&(XSTRING_DATA (s)[i]))
/* Convert pointer to text in string S into the byte offset to that text. */
#define string_addr_to_byte(s, ptr) ((Bytecount) ((ptr) - XSTRING_DATA (s)))
/* Return the Ichar at *CHARACTER* offset I. */
#define string_ichar(s, i) itext_ichar (string_char_addr (s, i))

#ifdef ERROR_CHECK_TEXT
#define SLEDGEHAMMER_CHECK_ASCII_BEGIN
#endif

#ifdef SLEDGEHAMMER_CHECK_ASCII_BEGIN
void sledgehammer_check_ascii_begin (Lisp_Object str);
#else
#define sledgehammer_check_ascii_begin(str)
#endif

/* Make an alloca'd copy of a Lisp string */
#define LISP_STRING_TO_ALLOCA(s, lval)					\
do {									\
  Ibyte **_lta_ = (Ibyte **) &(lval);					\
  Lisp_Object _lta_2 = (s);						\
  *_lta_ = alloca_ibytes (1 + XSTRING_LENGTH (_lta_2));		\
  memcpy (*_lta_, XSTRING_DATA (_lta_2), 1 + XSTRING_LENGTH (_lta_2));	\
} while (0)

void resize_string (Lisp_Object s, Bytecount pos, Bytecount delta);

/* Convert a byte index into a string into a char index. */
DECLARE_INLINE_HEADER (
Charcount
string_index_byte_to_char (Lisp_Object USED_IF_MULE_OR_CHECK_TEXT (s),
			   Bytecount idx)
)
{
  Charcount retval;
  ASSERT_VALID_BYTE_STRING_INDEX_UNSAFE (s, idx);
#ifdef MULE
  if (idx <= (Bytecount) XSTRING_ASCII_BEGIN (s))
    retval = (Charcount) idx;
  else
    retval = (XSTRING_ASCII_BEGIN (s) +
	      bytecount_to_charcount (XSTRING_DATA (s) +
				      XSTRING_ASCII_BEGIN (s),
				      idx - XSTRING_ASCII_BEGIN (s)));
# ifdef SLEDGEHAMMER_CHECK_ASCII_BEGIN
  assert (retval == bytecount_to_charcount (XSTRING_DATA (s), idx));
# endif
#else
  retval = (Charcount) idx;
#endif
  /* Don't call ASSERT_VALID_CHAR_STRING_INDEX_UNSAFE() here because it will
     call string_index_byte_to_char(). */
  return retval;
}

/* Convert a char index into a string into a byte index. */
DECLARE_INLINE_HEADER (
Bytecount
string_index_char_to_byte (Lisp_Object USED_IF_MULE_OR_CHECK_TEXT (s),
			   Charcount idx)
)
{
  Bytecount retval;
  ASSERT_VALID_CHAR_STRING_INDEX_UNSAFE (s, idx);
#ifdef MULE
  if (idx <= (Charcount) XSTRING_ASCII_BEGIN (s))
    retval = (Bytecount) idx;
  else
    retval = (XSTRING_ASCII_BEGIN (s) +
	      charcount_to_bytecount (XSTRING_DATA (s) +
				      XSTRING_ASCII_BEGIN (s),
				      idx - XSTRING_ASCII_BEGIN (s)));
# ifdef SLEDGEHAMMER_CHECK_ASCII_BEGIN
  assert (retval == charcount_to_bytecount (XSTRING_DATA (s), idx));
# endif
#else
  retval = (Bytecount) idx;
#endif
  ASSERT_VALID_BYTE_STRING_INDEX_UNSAFE (s, retval);
  return retval;
}

/* Convert a substring length (starting at byte offset OFF) from bytes to
   chars. */
DECLARE_INLINE_HEADER (
Charcount
string_offset_byte_to_char_len (Lisp_Object USED_IF_MULE_OR_CHECK_TEXT (s),
				Bytecount USED_IF_MULE_OR_CHECK_TEXT (off),
				Bytecount len)
)
{
  Charcount retval;
  ASSERT_VALID_BYTE_STRING_INDEX_UNSAFE (s, off);
  ASSERT_VALID_BYTE_STRING_INDEX_UNSAFE (s, off + len);
#ifdef MULE
  if (off + len <= (Bytecount) XSTRING_ASCII_BEGIN (s))
    retval = (Charcount) len;
  else if (off < (Bytecount) XSTRING_ASCII_BEGIN (s))
    retval =
      XSTRING_ASCII_BEGIN (s) - (Charcount) off +
	bytecount_to_charcount (XSTRING_DATA (s) + XSTRING_ASCII_BEGIN (s),
				len - (XSTRING_ASCII_BEGIN (s) - off));
  else
    retval = bytecount_to_charcount (XSTRING_DATA (s) + off, len);
# ifdef SLEDGEHAMMER_CHECK_ASCII_BEGIN
  assert (retval == bytecount_to_charcount (XSTRING_DATA (s) + off, len));
# endif
#else
  retval = (Charcount) len;
#endif
  return retval;
}

/* Convert a substring length (starting at byte offset OFF) from chars to
   bytes. */
DECLARE_INLINE_HEADER (
Bytecount
string_offset_char_to_byte_len (Lisp_Object USED_IF_MULE_OR_CHECK_TEXT (s),
				Bytecount USED_IF_MULE_OR_CHECK_TEXT (off),
				Charcount len)
)
{
  Bytecount retval;
  ASSERT_VALID_BYTE_STRING_INDEX_UNSAFE (s, off);
#ifdef MULE
  /* casts to avoid errors from combining Bytecount/Charcount and warnings
     from signed/unsigned comparisons */
  if (off + (Bytecount) len <= (Bytecount) XSTRING_ASCII_BEGIN (s))
    retval = (Bytecount) len;
  else if (off < (Bytecount) XSTRING_ASCII_BEGIN (s))
    retval =
      XSTRING_ASCII_BEGIN (s) - off +
	charcount_to_bytecount (XSTRING_DATA (s) + XSTRING_ASCII_BEGIN (s),
				len - (XSTRING_ASCII_BEGIN (s) -
				       (Charcount) off));
  else
    retval = charcount_to_bytecount (XSTRING_DATA (s) + off, len);
# ifdef SLEDGEHAMMER_CHECK_ASCII_BEGIN
  assert (retval == charcount_to_bytecount (XSTRING_DATA (s) + off, len));
# endif
#else
  retval = (Bytecount) len;
#endif
  ASSERT_VALID_BYTE_STRING_INDEX_UNSAFE (s, off + retval);
  return retval;
}

DECLARE_INLINE_HEADER (
const Ibyte *
string_char_addr (Lisp_Object s, Charcount idx)
)
{
  return XSTRING_DATA (s) + string_index_char_to_byte (s, idx);
}

/* WARNING: If you modify an existing string, you must call
   bump_string_modiff() afterwards. */
#ifdef MULE
void set_string_char (Lisp_Object s, Charcount i, Ichar c);
#else
#define set_string_char(s, i, c) set_string_byte (s, i, c)
#endif /* not MULE */

/* Return index to character before the one at IDX. */
DECLARE_INLINE_HEADER (
Bytecount
prev_string_index (Lisp_Object s, Bytecount idx)
)
{
  const Ibyte *ptr = string_byte_addr (s, idx);
  DEC_IBYTEPTR (ptr);
  return string_addr_to_byte (s, ptr);
}

/* Return index to character after the one at IDX. */
DECLARE_INLINE_HEADER (
Bytecount
next_string_index (Lisp_Object s, Bytecount idx)
)
{
  const Ibyte *ptr = string_byte_addr (s, idx);
  INC_IBYTEPTR (ptr);
  return string_addr_to_byte (s, ptr);
}


/************************************************************************/
/*									*/
/*		          working with Eistrings                        */
/*									*/
/************************************************************************/

/*
   #### NOTE: This is a work in progress.  Neither the API nor especially
   the implementation is finished.

   NOTE: An Eistring is a structure that makes it easy to work with
   internally-formatted strings of data.  It provides operations similar
   in feel to the standard strcpy(), strcat(), strlen(), etc., but

   (a) it is Mule-correct
   (b) it does dynamic allocation so you never have to worry about size
       restrictions
   (c) it comes in an ALLOCA() variety (all allocation is stack-local,
       so there is no need to explicitly clean up) as well as a malloc()
       variety
   (d) it knows its own length, so it does not suffer from standard null
       byte brain-damage -- but it null-terminates the data anyway, so
       it can be passed to standard routines
   (e) it provides a much more powerful set of operations and knows about
       all the standard places where string data might reside: Lisp_Objects,
       other Eistrings, Ibyte * data with or without an explicit length,
       ASCII strings, Ichars, etc.
   (f) it provides easy operations to convert to/from externally-formatted
       data, and is easier to use than the standard TO_INTERNAL_FORMAT
       and TO_EXTERNAL_FORMAT macros. (An Eistring can store both the internal
       and external version of its data, but the external version is only
       initialized or changed when you call eito_external().)

   The idea is to make it as easy to write Mule-correct string manipulation
   code as it is to write normal string manipulation code.  We also make
   the API sufficiently general that it can handle multiple internal data
   formats (e.g. some fixed-width optimizing formats and a default variable
   width format) and allows for *ANY* data format we might choose in the
   future for the default format, including UCS2. (In other words, we can't
   assume that the internal format is ASCII-compatible and we can't assume
   it doesn't have embedded null bytes.  We do assume, however, that any
   chosen format will have the concept of null-termination.) All of this is
   hidden from the user.

   #### It is really too bad that we don't have a real object-oriented
   language, or at least a language with polymorphism!


    ********************************************** 
    *                 Declaration                * 
    ********************************************** 

   To declare an Eistring, either put one of the following in the local
   variable section:

   DECLARE_EISTRING (name);
        Declare a new Eistring and initialize it to the empy string.  This
        is a standard local variable declaration and can go anywhere in the
        variable declaration section.  NAME itself is declared as an
        Eistring *, and its storage declared on the stack.

   DECLARE_EISTRING_MALLOC (name);
        Declare and initialize a new Eistring, which uses malloc()ed
        instead of ALLOCA()ed data.  This is a standard local variable
        declaration and can go anywhere in the variable declaration
        section.  Once you initialize the Eistring, you will have to free
        it using eifree() to avoid memory leaks.  You will need to use this
        form if you are passing an Eistring to any function that modifies
        it (otherwise, the modified data may be in stack space and get
        overwritten when the function returns).

   or use

   Eistring ei;
   void eiinit (Eistring *ei);
   void eiinit_malloc (Eistring *einame);
        If you need to put an Eistring elsewhere than in a local variable
        declaration (e.g. in a structure), declare it as shown and then
        call one of the init macros.

   Also note:

   void eifree (Eistring *ei);
        If you declared an Eistring to use malloc() to hold its data,
	or converted it to the heap using eito_malloc(), then this
	releases any data in it and afterwards resets the Eistring
	using eiinit_malloc().  Otherwise, it just resets the Eistring
	using eiinit().


    ********************************************** 
    *                 Conventions                * 
    ********************************************** 

    - The names of the functions have been chosen, where possible, to
      match the names of str*() functions in the standard C API.
    - 


    ********************************************** 
    *               Initialization               * 
    ********************************************** 

   void eireset (Eistring *eistr);
        Initialize the Eistring to the empty string.

   void eicpy_* (Eistring *eistr, ...);
        Initialize the Eistring from somewhere:

   void eicpy_ei (Eistring *eistr, Eistring *eistr2);
        ... from another Eistring.
   void eicpy_lstr (Eistring *eistr, Lisp_Object lisp_string);
        ... from a Lisp_Object string.
   void eicpy_ch (Eistring *eistr, Ichar ch);
        ... from an Ichar (this can be a conventional C character).

   void eicpy_lstr_off (Eistring *eistr, Lisp_Object lisp_string,
                        Bytecount off, Charcount charoff,
                        Bytecount len, Charcount charlen);
        ... from a section of a Lisp_Object string.
   void eicpy_lbuf (Eistring *eistr, Lisp_Object lisp_buf,
		    Bytecount off, Charcount charoff,
		    Bytecount len, Charcount charlen);
        ... from a section of a Lisp_Object buffer.
   void eicpy_raw (Eistring *eistr, const Ibyte *data, Bytecount len);
        ... from raw internal-format data in the default internal format.
   void eicpy_rawz (Eistring *eistr, const Ibyte *data);
        ... from raw internal-format data in the default internal format
        that is "null-terminated" (the meaning of this depends on the nature
        of the default internal format).
   void eicpy_raw_fmt (Eistring *eistr, const Ibyte *data, Bytecount len,
                       Internal_Format intfmt, Lisp_Object object);
        ... from raw internal-format data in the specified format.
   void eicpy_rawz_fmt (Eistring *eistr, const Ibyte *data,
                        Internal_Format intfmt, Lisp_Object object);
        ... from raw internal-format data in the specified format that is
        "null-terminated" (the meaning of this depends on the nature of
        the specific format).
   void eicpy_c (Eistring *eistr, const Ascbyte *c_string);
        ... from an ASCII null-terminated string.  Non-ASCII characters in
	the string are *ILLEGAL* (read abort() with error-checking defined).
   void eicpy_c_len (Eistring *eistr, const Ascbyte *c_string, len);
        ... from an ASCII string, with length specified.  Non-ASCII characters
	in the string are *ILLEGAL* (read abort() with error-checking defined).
   void eicpy_ext (Eistring *eistr, const Extbyte *extdata,
                   Lisp_Object codesys);
        ... from external null-terminated data, with coding system specified.
   void eicpy_ext_len (Eistring *eistr, const Extbyte *extdata,
                       Bytecount extlen, Lisp_Object codesys);
        ... from external data, with length and coding system specified.
   void eicpy_lstream (Eistring *eistr, Lisp_Object lstream);
        ... from an lstream; reads data till eof.  Data must be in default
        internal format; otherwise, interpose a decoding lstream.


    ********************************************** 
    *    Getting the data out of the Eistring    * 
    ********************************************** 

   Ibyte *eidata (Eistring *eistr);
        Return a pointer to the raw data in an Eistring.  This is NOT
        a copy.

   Lisp_Object eimake_string (Eistring *eistr);
        Make a Lisp string out of the Eistring.

   Lisp_Object eimake_string_off (Eistring *eistr,
                                  Bytecount off, Charcount charoff,
				  Bytecount len, Charcount charlen);
        Make a Lisp string out of a section of the Eistring.

   void eicpyout_alloca (Eistring *eistr, LVALUE: Ibyte *ptr_out,
                         LVALUE: Bytecount len_out);
        Make an ALLOCA() copy of the data in the Eistring, using the
        default internal format.  Due to the nature of ALLOCA(), this
        must be a macro, with all lvalues passed in as parameters.
	(More specifically, not all compilers correctly handle using
	ALLOCA() as the argument to a function call -- GCC on x86
	didn't used to, for example.) A pointer to the ALLOCA()ed data
	is stored in PTR_OUT, and the length of the data (not including
	the terminating zero) is stored in LEN_OUT.

   void eicpyout_alloca_fmt (Eistring *eistr, LVALUE: Ibyte *ptr_out,
                             LVALUE: Bytecount len_out,
                             Internal_Format intfmt, Lisp_Object object);
        Like eicpyout_alloca(), but converts to the specified internal
        format. (No formats other than FORMAT_DEFAULT are currently
        implemented, and you get an assertion failure if you try.)

   Ibyte *eicpyout_malloc (Eistring *eistr, Bytecount *intlen_out);
        Make a malloc() copy of the data in the Eistring, using the
        default internal format.  This is a real function.  No lvalues
        passed in.  Returns the new data, and stores the length (not
        including the terminating zero) using INTLEN_OUT, unless it's
        a NULL pointer.

   Ibyte *eicpyout_malloc_fmt (Eistring *eistr, Internal_Format intfmt,
                                 Bytecount *intlen_out, Lisp_Object object);
        Like eicpyout_malloc(), but converts to the specified internal
        format. (No formats other than FORMAT_DEFAULT are currently
        implemented, and you get an assertion failure if you try.)


    ********************************************** 
    *             Moving to the heap             * 
    ********************************************** 

   void eito_malloc (Eistring *eistr);
        Move this Eistring to the heap.  Its data will be stored in a
        malloc()ed block rather than the stack.  Subsequent changes to
        this Eistring will realloc() the block as necessary.  Use this
        when you want the Eistring to remain in scope past the end of
        this function call.  You will have to manually free the data
        in the Eistring using eifree().

   void eito_alloca (Eistring *eistr);
        Move this Eistring back to the stack, if it was moved to the
	heap with eito_malloc().  This will automatically free any
	heap-allocated data.



    ********************************************** 
    *            Retrieving the length           * 
    ********************************************** 

   Bytecount eilen (Eistring *eistr);
        Return the length of the internal data, in bytes.  See also
	eiextlen(), below.
   Charcount eicharlen (Eistring *eistr);
        Return the length of the internal data, in characters.


    ********************************************** 
    *           Working with positions           * 
    ********************************************** 

   Bytecount eicharpos_to_bytepos (Eistring *eistr, Charcount charpos);
        Convert a char offset to a byte offset.
   Charcount eibytepos_to_charpos (Eistring *eistr, Bytecount bytepos);
        Convert a byte offset to a char offset.
   Bytecount eiincpos (Eistring *eistr, Bytecount bytepos);
        Increment the given position by one character.
   Bytecount eiincpos_n (Eistring *eistr, Bytecount bytepos, Charcount n);
        Increment the given position by N characters.
   Bytecount eidecpos (Eistring *eistr, Bytecount bytepos);
        Decrement the given position by one character.
   Bytecount eidecpos_n (Eistring *eistr, Bytecount bytepos, Charcount n);
        Deccrement the given position by N characters.


    ********************************************** 
    *    Getting the character at a position     * 
    ********************************************** 

   Ichar eigetch (Eistring *eistr, Bytecount bytepos);
        Return the character at a particular byte offset.
   Ichar eigetch_char (Eistring *eistr, Charcount charpos);
        Return the character at a particular character offset.


    ********************************************** 
    *    Setting the character at a position     * 
    ********************************************** 

   Ichar eisetch (Eistring *eistr, Bytecount bytepos, Ichar chr);
        Set the character at a particular byte offset.
   Ichar eisetch_char (Eistring *eistr, Charcount charpos, Ichar chr);
        Set the character at a particular character offset.


    ********************************************** 
    *               Concatenation                * 
    ********************************************** 

   void eicat_* (Eistring *eistr, ...);
        Concatenate onto the end of the Eistring, with data coming from the
	same places as above:

   void eicat_ei (Eistring *eistr, Eistring *eistr2);
        ... from another Eistring.
   void eicat_c (Eistring *eistr, Ascbyte *c_string);
        ... from an ASCII null-terminated string.  Non-ASCII characters in
	the string are *ILLEGAL* (read abort() with error-checking defined).
   void eicat_raw (ei, const Ibyte *data, Bytecount len);
        ... from raw internal-format data in the default internal format.
   void eicat_rawz (ei, const Ibyte *data);
        ... from raw internal-format data in the default internal format
        that is "null-terminated" (the meaning of this depends on the nature
        of the default internal format).
   void eicat_lstr (ei, Lisp_Object lisp_string);
        ... from a Lisp_Object string.
   void eicat_ch (ei, Ichar ch);
        ... from an Ichar.

  (All except the first variety are convenience functions.
  In the general case, create another Eistring from the source.)


    ********************************************** 
    *                Replacement                 * 
    ********************************************** 

   void eisub_* (Eistring *eistr, Bytecount off, Charcount charoff,
				  Bytecount len, Charcount charlen, ...);
        Replace a section of the Eistring, specifically:

   void eisub_ei (Eistring *eistr, Bytecount off, Charcount charoff,
		  Bytecount len, Charcount charlen, Eistring *eistr2);
        ... with another Eistring.
   void eisub_c (Eistring *eistr, Bytecount off, Charcount charoff,
		 Bytecount len, Charcount charlen, Ascbyte *c_string);
        ... with an ASCII null-terminated string.  Non-ASCII characters in
	the string are *ILLEGAL* (read abort() with error-checking defined).
   void eisub_ch (Eistring *eistr, Bytecount off, Charcount charoff,
		  Bytecount len, Charcount charlen, Ichar ch);
        ... with an Ichar.

   void eidel (Eistring *eistr, Bytecount off, Charcount charoff,
	       Bytecount len, Charcount charlen);
        Delete a section of the Eistring.


    ********************************************** 
    *      Converting to an external format      * 
    ********************************************** 

   void eito_external (Eistring *eistr, Lisp_Object codesys);
        Convert the Eistring to an external format and store the result
	in the string.  NOTE: Further changes to the Eistring will *NOT*
	change the external data stored in the string.  You will have to
	call eito_external() again in such a case if you want the external
	data.

   Extbyte *eiextdata (Eistring *eistr);
        Return a pointer to the external data stored in the Eistring as
	a result of a prior call to eito_external().

   Bytecount eiextlen (Eistring *eistr);
        Return the length in bytes of the external data stored in the
	Eistring as a result of a prior call to eito_external().


    ********************************************** 
    * Searching in the Eistring for a character  * 
    ********************************************** 

   Bytecount eichr (Eistring *eistr, Ichar chr);
   Charcount eichr_char (Eistring *eistr, Ichar chr);
   Bytecount eichr_off (Eistring *eistr, Ichar chr, Bytecount off,
			Charcount charoff);
   Charcount eichr_off_char (Eistring *eistr, Ichar chr, Bytecount off,
			     Charcount charoff);
   Bytecount eirchr (Eistring *eistr, Ichar chr);
   Charcount eirchr_char (Eistring *eistr, Ichar chr);
   Bytecount eirchr_off (Eistring *eistr, Ichar chr, Bytecount off,
			 Charcount charoff);
   Charcount eirchr_off_char (Eistring *eistr, Ichar chr, Bytecount off,
			      Charcount charoff);


    ********************************************** 
    *   Searching in the Eistring for a string   * 
    ********************************************** 

   Bytecount eistr_ei (Eistring *eistr, Eistring *eistr2);
   Charcount eistr_ei_char (Eistring *eistr, Eistring *eistr2);
   Bytecount eistr_ei_off (Eistring *eistr, Eistring *eistr2, Bytecount off,
			   Charcount charoff);
   Charcount eistr_ei_off_char (Eistring *eistr, Eistring *eistr2,
				Bytecount off, Charcount charoff);
   Bytecount eirstr_ei (Eistring *eistr, Eistring *eistr2);
   Charcount eirstr_ei_char (Eistring *eistr, Eistring *eistr2);
   Bytecount eirstr_ei_off (Eistring *eistr, Eistring *eistr2, Bytecount off,
			    Charcount charoff);
   Charcount eirstr_ei_off_char (Eistring *eistr, Eistring *eistr2,
				 Bytecount off, Charcount charoff);

   Bytecount eistr_c (Eistring *eistr, Ascbyte *c_string);
   Charcount eistr_c_char (Eistring *eistr, Ascbyte *c_string);
   Bytecount eistr_c_off (Eistring *eistr, Ascbyte *c_string, Bytecount off,
			   Charcount charoff);
   Charcount eistr_c_off_char (Eistring *eistr, Ascbyte *c_string,
			       Bytecount off, Charcount charoff);
   Bytecount eirstr_c (Eistring *eistr, Ascbyte *c_string);
   Charcount eirstr_c_char (Eistring *eistr, Ascbyte *c_string);
   Bytecount eirstr_c_off (Eistring *eistr, Ascbyte *c_string,
			   Bytecount off, Charcount charoff);
   Charcount eirstr_c_off_char (Eistring *eistr, Ascbyte *c_string,
				Bytecount off, Charcount charoff);


    ********************************************** 
    *                 Comparison                 * 
    ********************************************** 

   int eicmp_* (Eistring *eistr, ...);
   int eicmp_off_* (Eistring *eistr, Bytecount off, Charcount charoff,
                    Bytecount len, Charcount charlen, ...);
   int eicasecmp_* (Eistring *eistr, ...);
   int eicasecmp_off_* (Eistring *eistr, Bytecount off, Charcount charoff,
                        Bytecount len, Charcount charlen, ...);
   int eicasecmp_i18n_* (Eistring *eistr, ...);
   int eicasecmp_i18n_off_* (Eistring *eistr, Bytecount off, Charcount charoff,
                             Bytecount len, Charcount charlen, ...);

        Compare the Eistring with the other data.  Return value same as
        from strcmp.  The `*' is either `ei' for another Eistring (in
	which case `...' is an Eistring), or `c' for a pure-ASCII string
	(in which case `...' is a pointer to that string).  For anything
	more complex, first create an Eistring out of the source.
	Comparison is either simple (`eicmp_...'), ASCII case-folding
	(`eicasecmp_...'), or multilingual case-folding
	(`eicasecmp_i18n_...).


   More specifically, the prototypes are:

   int eicmp_ei (Eistring *eistr, Eistring *eistr2);
   int eicmp_off_ei (Eistring *eistr, Bytecount off, Charcount charoff,
                     Bytecount len, Charcount charlen, Eistring *eistr2);
   int eicasecmp_ei (Eistring *eistr, Eistring *eistr2);
   int eicasecmp_off_ei (Eistring *eistr, Bytecount off, Charcount charoff,
                         Bytecount len, Charcount charlen, Eistring *eistr2);
   int eicasecmp_i18n_ei (Eistring *eistr, Eistring *eistr2);
   int eicasecmp_i18n_off_ei (Eistring *eistr, Bytecount off,
			      Charcount charoff, Bytecount len,
			      Charcount charlen, Eistring *eistr2);

   int eicmp_c (Eistring *eistr, Ascbyte *c_string);
   int eicmp_off_c (Eistring *eistr, Bytecount off, Charcount charoff,
                    Bytecount len, Charcount charlen, Ascbyte *c_string);
   int eicasecmp_c (Eistring *eistr, Ascbyte *c_string);
   int eicasecmp_off_c (Eistring *eistr, Bytecount off, Charcount charoff,
                        Bytecount len, Charcount charlen,
                        Ascbyte *c_string);
   int eicasecmp_i18n_c (Eistring *eistr, Ascbyte *c_string);
   int eicasecmp_i18n_off_c (Eistring *eistr, Bytecount off, Charcount charoff,
                             Bytecount len, Charcount charlen,
                             Ascbyte *c_string);


    ********************************************** 
    *         Case-changing the Eistring         * 
    ********************************************** 

   void eilwr (Eistring *eistr);
        Convert all characters in the Eistring to lowercase.
   void eiupr (Eistring *eistr);
        Convert all characters in the Eistring to uppercase.
*/


/* Principles for writing Eistring functions:

   (1) Unfortunately, we have to write most of the Eistring functions
       as macros, because of the use of ALLOCA().  The principle used
       below to assure no conflict in local variables is to prefix all
       local variables with "ei" plus a number, which should be unique
       among macros.  In practice, when finding a new number, find the
       highest so far used, and add 1.

   (2) We also suffix the Eistring fields with an _ to avoid problems
       with macro parameters of the same name. (And as the standard
       signal not to access these fields directly.)

   (3) We maintain both the length in bytes and chars of the data in
       the Eistring at all times, for convenient retrieval by outside
       functions.  That means when writing functions that manipulate
       Eistrings, you too need to keep both lengths up to date for all
       data that you work with.

   (4) When writing a new type of operation (e.g. substitution), you
       will often find yourself working with outside data, and thus
       have a series of related API's, for different forms that the
       outside data is in.  Generally, you will want to choose a
       subset of the forms supported by eicpy_*, which has to be
       totally general because that's the fundamental way to get data
       into an Eistring, and once the data is into the string, it
       would be to create a whole series of Ei operations that work on
       nothing but Eistrings.  Although theoretically nice, in
       practice it's a hassle, so we suggest that you provide
       convenience functions.  In particular, there are two paths you
       can take.  One is minimalist -- it only allows other Eistrings
       and ASCII data, and Ichars if the particular operation makes
       sense with a character.  The other provides interfaces for the
       most commonly-used forms -- Eistring, ASCII data, Lisp string,
       raw internal-format string with length, raw internal-format
       string without, and possibly Ichar. (In the function names,
       these are designated `ei', `c', `lstr', `raw', `rawz', and
       `ch', respectively.)

   (5) When coding a new type of operation, such as was discussed in
       previous section, the correct approach is to declare an worker
       function that does the work of everything, and is called by the
       other "container" macros that handle the different outside data
       forms.  The data coming into the worker function, which
       typically ends in `_1', is in the form of three parameters:
       DATA, LEN, CHARLEN. (See point [3] about having two lengths and
       keeping them in sync.)

   (6) Handling argument evaluation in macros: We take great care
       never to evaluate any argument more than once in any macro,
       except the initial Eistring parameter.  This can and will be
       evaluated multiple times, but it should pretty much always just
       be a simple variable.  This means, for example, that if an
       Eistring is the second (not first) argument of a macro, it
       doesn't fall under the "initial Eistring" exemption, so it
       needs protection against multi-evaluation. (Take the address of
       the Eistring structure, store in a temporary variable, and use
       temporary variable for all access to the Eistring.
       Essentially, we want it to appear as if these Eistring macros
       are functions -- we would like to declare them as functions but
       they use ALLOCA(), so we can't (and we can't make them inline
       functions either -- ALLOCA() is explicitly disallowed in inline
       functions.)

   (7) Note that our rules regarding multiple evaluation are *more*
       strict than the rules listed above under the heading "working
       with raw internal-format data".
   */


/*   ----- Declaration -----   */

typedef struct
{
  /* Data for the Eistring, stored in the default internal format.
     Always includes terminating null. */
  Ibyte *data_;
  /* Total number of bytes allocated in DATA (including null). */
  Bytecount max_size_allocated_;
  Bytecount bytelen_;
  Charcount charlen_;
  int mallocp_;

  Extbyte *extdata_;
  Bytecount extlen_;
} Eistring;

extern Eistring the_eistring_zero_init, the_eistring_malloc_zero_init;

#define DECLARE_EISTRING(name)					\
  Eistring __ ## name ## __storage__ = the_eistring_zero_init;	\
  Eistring *name = & __ ## name ## __storage__
#define DECLARE_EISTRING_MALLOC(name)					\
  Eistring __ ## name ## __storage__ = the_eistring_malloc_zero_init;	\
  Eistring *name = & __ ## name ## __storage__

#define eiinit(ei)				\
do {						\
  *(ei) = the_eistring_zero_init;		\
} while (0)

#define eiinit_malloc(ei)			\
do {						\
  *(ei) = the_eistring_malloc_zero_init;	\
} while (0)


/*   ----- Utility -----   */

/* Make sure both LEN and CHARLEN are specified, in case one is given
   as -1.  PTR evaluated at most once, others multiply. */
#define eifixup_bytechar(ptr, len, charlen)		\
do {							\
  if ((len) == -1)					\
    (len) = charcount_to_bytecount (ptr, charlen);	\
  else if ((charlen) == -1)				\
    (charlen) = bytecount_to_charcount (ptr, len);	\
} while (0)

/* Make sure LEN is specified, in case it's is given as -1.  PTR
   evaluated at most once, others multiply. */
#define eifixup_byte(ptr, len, charlen)			\
do {							\
  if ((len) == -1)					\
    (len) = charcount_to_bytecount (ptr, charlen);	\
} while (0)

/* Make sure CHARLEN is specified, in case it's is given as -1.  PTR
   evaluated at most once, others multiply. */
#define eifixup_char(ptr, len, charlen)			\
do {							\
  if ((charlen) == -1)					\
    (charlen) = bytecount_to_charcount (ptr, len);	\
} while (0)



/* Make sure we can hold NEWBYTELEN bytes (which is NEWCHARLEN chars)
   plus a zero terminator.  Preserve existing data as much as possible,
   including existing zero terminator.  Put a new zero terminator where it
   should go if NEWZ if non-zero.  All args but EI are evalled only once. */

#define EI_ALLOC(ei, newbytelen, newcharlen, newz)			      \
do {									      \
  int ei1oldeibytelen = (ei)->bytelen_;					      \
									      \
  (ei)->charlen_ = (newcharlen);					      \
  (ei)->bytelen_ = (newbytelen);					      \
									      \
  if (ei1oldeibytelen != (ei)->bytelen_)				      \
    {									      \
      int ei1newsize = (ei)->max_size_allocated_;			      \
      while (ei1newsize < (ei)->bytelen_ + 1)				      \
	{								      \
	  ei1newsize = (int) (ei1newsize * 1.5);			      \
	  if (ei1newsize < 32)						      \
	    ei1newsize = 32;						      \
	}								      \
      if (ei1newsize != (ei)->max_size_allocated_)			      \
	{								      \
	  if ((ei)->mallocp_)						      \
	    /* xrealloc always preserves existing data as much as possible */ \
	    (ei)->data_ = (Ibyte *) xrealloc ((ei)->data_, ei1newsize);       \
	  else								      \
	    {								      \
	      /* We don't have realloc, so ALLOCA() more space and copy the   \
		 data into it. */					      \
	      Ibyte *ei1oldeidata = (ei)->data_;			      \
	      (ei)->data_ = alloca_ibytes (ei1newsize);		      \
              if (ei1oldeidata)						      \
	        memcpy ((ei)->data_, ei1oldeidata, ei1oldeibytelen + 1);      \
	    }								      \
	  (ei)->max_size_allocated_ = ei1newsize;			      \
	}								      \
      if (newz)								      \
        (ei)->data_[(ei)->bytelen_] = '\0';				      \
    }									      \
} while (0)

#define EI_ALLOC_AND_COPY(ei, data, bytelen, charlen)	\
do {							\
  EI_ALLOC (ei, bytelen, charlen, 1);			\
  memcpy ((ei)->data_, data, (ei)->bytelen_);		\
} while (0)

/*   ----- Initialization -----   */

#define eicpy_ei(ei, eicpy)						\
do {									\
  const Eistring *ei2 = (eicpy);					\
  EI_ALLOC_AND_COPY (ei, ei2->data_, ei2->bytelen_, ei2->charlen_);	\
} while (0)

#define eicpy_lstr(ei, lisp_string)					\
do {									\
  Lisp_Object ei3 = (lisp_string);					\
  EI_ALLOC_AND_COPY (ei, XSTRING_DATA (ei3), XSTRING_LENGTH (ei3),	\
		     string_char_length (ei3));				\
} while (0)

#define eicpy_lstr_off(ei, lisp_string, off, charoff, len, charlen)	\
do {									\
  Lisp_Object ei23lstr = (lisp_string);					\
  int ei23off = (off);							\
  int ei23charoff = (charoff);						\
  int ei23len = (len);							\
  int ei23charlen = (charlen);						\
  const Ibyte *ei23data = XSTRING_DATA (ei23lstr);			\
									\
  int ei23oldbytelen = (ei)->bytelen_;					\
									\
  eifixup_byte (ei23data, ei23off, ei23charoff);			\
  eifixup_bytechar (ei23data + ei23off, ei23len, ei23charlen);		\
									\
  EI_ALLOC_AND_COPY (ei, ei23data + ei23off, ei23len, ei23charlen);	\
} while (0)

#define eicpy_raw_fmt(ei, ptr, len, fmt, object)			\
do {									\
  const Ibyte *ei12ptr = (ptr);						\
  Internal_Format ei12fmt = (fmt);					\
  int ei12len = (len);							\
  assert (ei12fmt == FORMAT_DEFAULT);					\
  EI_ALLOC_AND_COPY (ei, ei12ptr, ei12len,				\
		     bytecount_to_charcount (ei12ptr, ei12len));	\
} while (0)

#define eicpy_raw(ei, ptr, len) \
  eicpy_raw_fmt (ei, ptr, len, FORMAT_DEFAULT, Qnil)

#define eicpy_rawz_fmt(ei, ptr, fmt, object)				\
do {									\
  const Ibyte *ei12p1ptr = (ptr);					\
  Internal_Format ei12p1fmt = (fmt);					\
  assert (ei12p1fmt == FORMAT_DEFAULT);					\
  eicpy_raw_fmt (ei, ei12p1ptr, qxestrlen (ei12p1ptr), fmt, object);	\
} while (0)

#define eicpy_rawz(ei, ptr) eicpy_rawz_fmt (ei, ptr, FORMAT_DEFAULT, Qnil)

#define eicpy_ch(ei, ch)				\
do {							\
  Ibyte ei12p2[MAX_ICHAR_LEN];				\
  Bytecount ei12p2len = set_itext_ichar (ei12p2, ch);	\
  EI_ALLOC_AND_COPY (ei, ei12p2, ei12p2len, 1);		\
} while (0)

#define eicpy_c(ei, c_string)			\
do {						\
  const Ascbyte *ei4 = (c_string);		\
						\
  ASSERT_ASCTEXT_ASCII (ei4);			\
  eicpy_ext (ei, ei4, Qbinary);			\
} while (0)

#define eicpy_c_len(ei, c_string, c_len)	\
do {						\
  const Ascbyte *ei6 = (c_string);		\
  int ei6len = (c_len);				\
						\
  ASSERT_ASCTEXT_ASCII_LEN (ei6, ei6len);	\
  eicpy_ext_len (ei, ei6, ei6len, Qbinary);	\
} while (0)

#define eicpy_ext_len(ei, extdata, extlen, codesys)			 \
do {									 \
  const Extbyte *ei7 = (extdata);					 \
  int ei7len = (extlen);						 \
									 \
  SIZED_EXTERNAL_TO_SIZED_C_STRING (ei7, ei7len, (ei)->data_,		 \
				    (ei)->bytelen_, codesys);		 \
  (ei)->max_size_allocated_ = (ei)->bytelen_ + 1;			 \
  (ei)->charlen_ = bytecount_to_charcount ((ei)->data_, (ei)->bytelen_); \
} while (0)

#define eicpy_ext(ei, extdata, codesys)				\
do {								\
  const Extbyte *ei8 = (extdata);				\
								\
  eicpy_ext_len (ei, ei8, dfc_external_data_len (ei8, codesys),	\
		 codesys);					\
} while (0)

#define eicpy_lbuf(eistr, lisp_buf, off, charoff, len, charlen) \
  NOT YET IMPLEMENTED

#define eicpy_lstream(eistr, lstream) \
  NOT YET IMPLEMENTED

#define eireset(eistr) eicpy_rawz (eistr, (Ibyte *) "")

/*   ----- Getting the data out of the Eistring -----   */

#define eidata(ei) ((ei)->data_)

#define eimake_string(ei) make_string (eidata (ei), eilen (ei))

#define eimake_string_off(eistr, off, charoff, len, charlen)		\
do {									\
  Lisp_Object ei24lstr;							\
  int ei24off = (off);							\
  int ei24charoff = (charoff);						\
  int ei24len = (len);							\
  int ei24charlen = (charlen);						\
									\
  eifixup_byte ((eistr)->data_, ei24off, ei24charoff);			\
  eifixup_byte ((eistr)->data_ + ei24off, ei24len, ei24charlen);	\
									\
  return make_string ((eistr)->data_ + ei24off, ei24len);		\
} while (0)

#define eicpyout_alloca(eistr, ptrout, lenout) \
  eicpyout_alloca_fmt (eistr, ptrout, lenout, FORMAT_DEFAULT, Qnil)
#define eicpyout_malloc(eistr, lenout) \
  eicpyout_malloc_fmt (eistr, lenout, FORMAT_DEFAULT, Qnil)
Ibyte *eicpyout_malloc_fmt (Eistring *eistr, Bytecount *len_out,
			      Internal_Format fmt, Lisp_Object object);
#define eicpyout_alloca_fmt(eistr, ptrout, lenout, fmt, object)	\
do {								\
  Internal_Format ei23fmt = (fmt);				\
  Ibyte *ei23ptrout = &(ptrout);				\
  Bytecount *ei23lenout = &(lenout);				\
								\
  assert (ei23fmt == FORMAT_DEFAULT);				\
								\
  *ei23lenout = (eistr)->bytelen_;				\
  *ei23ptrout = alloca_ibytes ((eistr)->bytelen_ + 1);	\
  memcpy (*ei23ptrout, (eistr)->data_, (eistr)->bytelen_ + 1);	\
} while (0)

/*   ----- Moving to the heap -----   */

#define eifree(ei)				\
do {						\
  if ((ei)->mallocp_)				\
    {						\
      if ((ei)->data_)				\
	xfree ((ei)->data_, Ibyte *);		\
      if ((ei)->extdata_)			\
	xfree ((ei)->extdata_, Extbyte *);	\
      eiinit_malloc (ei);			\
    }						\
  else						\
    eiinit (ei);				\
} while (0)

int eifind_large_enough_buffer (int oldbufsize, int needed_size);
void eito_malloc_1 (Eistring *ei);

#define eito_malloc(ei) eito_malloc_1 (ei)

#define eito_alloca(ei)							\
do {									\
  if (!(ei)->mallocp_)							\
    return;								\
  (ei)->mallocp_ = 0;							\
  if ((ei)->data_)							\
    {									\
      Ibyte *ei13newdata;						\
									\
      (ei)->max_size_allocated_ =					\
	eifind_large_enough_buffer (0, (ei)->bytelen_ + 1);		\
      ei13newdata = alloca_ibytes ((ei)->max_size_allocated_);		\
      memcpy (ei13newdata, (ei)->data_, (ei)->bytelen_ + 1);		\
      xfree ((ei)->data_, Ibyte *);					\
      (ei)->data_ = ei13newdata;					\
    }									\
									\
  if ((ei)->extdata_)							\
    {									\
      Extbyte *ei13newdata = alloca_extbytes ((ei)->extlen_ + 2);	\
									\
      memcpy (ei13newdata, (ei)->extdata_, (ei)->extlen_);		\
      /* Double null-terminate in case of Unicode data */		\
      ei13newdata[(ei)->extlen_] = '\0';				\
      ei13newdata[(ei)->extlen_ + 1] = '\0';				\
      xfree ((ei)->extdata_, Extbyte *);				\
      (ei)->extdata_ = ei13newdata;					\
    }									\
} while (0)


/*   ----- Retrieving the length -----   */

#define eilen(ei) ((ei)->bytelen_)
#define eicharlen(ei) ((ei)->charlen_)


/*   ----- Working with positions -----   */

#define eicharpos_to_bytepos(ei, charpos) \
  charcount_to_bytecount ((ei)->data_, charpos)
#define eibytepos_to_charpos(ei, bytepos) \
  bytecount_to_charcount ((ei)->data_, bytepos)

DECLARE_INLINE_HEADER (Bytecount eiincpos_1 (Eistring *eistr,
					     Bytecount bytepos,
					     Charcount n))
{
  Ibyte *pos = eistr->data_ + bytepos;
  Charcount i;

  text_checking_assert (bytepos >= 0 && bytepos <= eistr->bytelen_);
  text_checking_assert (n >= 0 && n <= eistr->charlen_);
  /* We could check N more correctly now, but that would require a
     call to bytecount_to_charcount(), which would be needlessly
     expensive (it would convert O(N) algorithms into O(N^2) algorithms
     with ERROR_CHECK_TEXT, which would be bad).  If N is bad, we are
     guaranteed to catch it either inside INC_IBYTEPTR() or in the check
     below. */
  for (i = 0; i < n; i++)
    INC_IBYTEPTR (pos);
  text_checking_assert (pos - eistr->data_ <= eistr->bytelen_);
  return pos - eistr->data_;
}

#define eiincpos (ei, bytepos) eiincpos_1 (ei, bytepos, 1)
#define eiincpos_n (ei, bytepos, n) eiincpos_1 (ei, bytepos, n)

DECLARE_INLINE_HEADER (Bytecount eidecpos_1 (Eistring *eistr,
					     Bytecount bytepos,
					     Charcount n))
{
  Ibyte *pos = eistr->data_ + bytepos;
  int i;

  text_checking_assert (bytepos >= 0 && bytepos <= eistr->bytelen_);
  text_checking_assert (n >= 0 && n <= eistr->charlen_);
  /* We could check N more correctly now, but ...  see above. */
  for (i = 0; i < n; i++)
    DEC_IBYTEPTR (pos);
  text_checking_assert (pos - eistr->data_ <= eistr->bytelen_);
  return pos - eistr->data_;
}

#define eidecpos (ei, bytepos) eidecpos_1 (ei, bytepos, 1)
#define eidecpos_n (ei, bytepos, n) eidecpos_1 (ei, bytepos, n)


/*   ----- Getting the character at a position -----   */

#define eigetch(ei, bytepos) \
  itext_ichar ((ei)->data_ + (bytepos))
#define eigetch_char(ei, charpos) itext_ichar_n ((ei)->data_, charpos)


/*   ----- Setting the character at a position -----   */

#define eisetch(ei, bytepos, chr) \
  eisub_ch (ei, bytepos, -1, -1, 1, chr)
#define eisetch_char(ei, charpos, chr) \
  eisub_ch (ei, -1, charpos, -1, 1, chr)


/*   ----- Concatenation -----   */

#define eicat_1(ei, data, bytelen, charlen)		\
do {							\
  int ei14oldeibytelen = (ei)->bytelen_;		\
  int ei14bytelen = (bytelen);				\
  EI_ALLOC (ei, (ei)->bytelen_ + ei14bytelen,		\
	    (ei)->charlen_ + (charlen), 1);		\
  memcpy ((ei)->data_ + ei14oldeibytelen, (data),	\
	  ei14bytelen);					\
} while (0)

#define eicat_ei(ei, ei2)					\
do {								\
  const Eistring *ei9 = (ei2);					\
  eicat_1 (ei, ei9->data_, ei9->bytelen_, ei9->charlen_);	\
} while (0)

#define eicat_c(ei, c_string)					\
do {								\
  const Ascbyte *ei15 = (c_string);				\
  int ei15len = strlen (ei15);					\
								\
  ASSERT_ASCTEXT_ASCII_LEN (ei15, ei15len);			\
  eicat_1 (ei, ei15, ei15len,					\
           bytecount_to_charcount ((Ibyte *) ei15, ei15len));	\
} while (0)

#define eicat_raw(ei, data, len)			\
do {							\
  int ei16len = (len);					\
  const Ibyte *ei16data = (data);			\
  eicat_1 (ei, ei16data, ei16len,			\
           bytecount_to_charcount (ei16data, ei16len));	\
} while (0)

#define eicat_rawz(ei, ptr)				\
do {							\
  const Ibyte *ei16p5ptr = (ptr);			\
  eicat_raw (ei, ei16p5ptr, qxestrlen (ei16p5ptr));	\
} while (0)

#define eicat_lstr(ei, lisp_string)				\
do {								\
  Lisp_Object ei17 = (lisp_string);				\
  eicat_1 (ei, XSTRING_DATA (ei17), XSTRING_LENGTH (ei17),	\
	   string_char_length (ei17));				\
} while (0)

#define eicat_ch(ei, ch)				\
do {							\
  Ibyte ei22ch[MAX_ICHAR_LEN];				\
  Bytecount ei22len = set_itext_ichar (ei22ch, ch);	\
  eicat_1 (ei, ei22ch, ei22len, 1);			\
} while (0)


/*   ----- Replacement -----   */

/* Replace the section of an Eistring at (OFF, LEN) with the data at
   SRC of length LEN.  All positions have corresponding character values,
   and either can be -1 -- it will be computed from the other. */

#define eisub_1(ei, off, charoff, len, charlen, src, srclen, srccharlen) \
do {									 \
  int ei18off = (off);							 \
  int ei18charoff = (charoff);						 \
  int ei18len = (len);							 \
  int ei18charlen = (charlen);						 \
  Ibyte *ei18src = (Ibyte *) (src);					 \
  int ei18srclen = (srclen);						 \
  int ei18srccharlen = (srccharlen);					 \
									 \
  int ei18oldeibytelen = (ei)->bytelen_;				 \
									 \
  eifixup_bytechar ((ei)->data_, ei18off, ei18charoff);			 \
  eifixup_bytechar ((ei)->data_ + ei18off, ei18len, ei18charlen);	 \
  eifixup_bytechar (ei18src, ei18srclen, ei18srccharlen);		 \
									 \
  EI_ALLOC (ei, (ei)->bytelen_ + ei18srclen - ei18len,			 \
	    (ei)->charlen_ + ei18srccharlen - ei18charlen, 0);		 \
  if (ei18len != ei18srclen)						 \
    memmove ((ei)->data_ + ei18off + ei18srclen,			 \
	     (ei)->data_ + ei18off + ei18len,				 \
	     /* include zero terminator. */				 \
	     ei18oldeibytelen - (ei18off + ei18len) + 1);		 \
  if (ei18srclen > 0)							 \
    memcpy ((ei)->data_ + ei18off, ei18src, ei18srclen);		 \
} while (0)

#define eisub_ei(ei, off, charoff, len, charlen, ei2)			\
do {									\
  const Eistring *ei19 = (ei2);						\
  eisub_1 (ei, off, charoff, len, charlen, ei19->data_, ei19->bytelen_,	\
	   ei19->charlen_);						\
} while (0)

#define eisub_c(ei, off, charoff, len, charlen, c_string)	\
do {								\
  const Ascbyte *ei20 = (c_string);				\
  int ei20len = strlen (ei20);					\
  ASSERT_ASCTEXT_ASCII_LEN (ei20, ei20len);			\
  eisub_1 (ei, off, charoff, len, charlen, ei20, ei20len, -1);	\
} while (0)

#define eisub_ch(ei, off, charoff, len, charlen, ch)		\
do {								\
  Ibyte ei21ch[MAX_ICHAR_LEN];					\
  Bytecount ei21len = set_itext_ichar (ei21ch, ch);		\
  eisub_1 (ei, off, charoff, len, charlen, ei21ch, ei21len, 1);	\
} while (0)

#define eidel(ei, off, charoff, len, charlen)		\
  eisub_1(ei, off, charoff, len, charlen, NULL, 0, 0)


/*   ----- Converting to an external format -----   */

#define eito_external(ei, codesys)					\
do {									\
  if ((ei)->mallocp_)							\
    {									\
      if ((ei)->extdata_)						\
	{								\
	  xfree ((ei)->extdata_, Extbyte *);				\
	  (ei)->extdata_ = 0;						\
	}								\
      TO_EXTERNAL_FORMAT (DATA, ((ei)->data_, (ei)->bytelen_),		\
			  MALLOC, ((ei)->extdata_, (ei)->extlen_),	\
			  codesys);					\
    }									\
  else									\
    TO_EXTERNAL_FORMAT (DATA, ((ei)->data_, (ei)->bytelen_),		\
			ALLOCA, ((ei)->extdata_, (ei)->extlen_),	\
			codesys);					\
} while (0)

#define eiextdata(ei) ((ei)->extdata_)
#define eiextlen(ei) ((ei)->extlen_)


/*   ----- Searching in the Eistring for a character -----   */

#define eichr(eistr, chr) \
  NOT YET IMPLEMENTED
#define eichr_char(eistr, chr) \
  NOT YET IMPLEMENTED
#define eichr_off(eistr, chr, off, charoff) \
  NOT YET IMPLEMENTED
#define eichr_off_char(eistr, chr, off, charoff) \
  NOT YET IMPLEMENTED
#define eirchr(eistr, chr) \
  NOT YET IMPLEMENTED
#define eirchr_char(eistr, chr) \
  NOT YET IMPLEMENTED
#define eirchr_off(eistr, chr, off, charoff) \
  NOT YET IMPLEMENTED
#define eirchr_off_char(eistr, chr, off, charoff) \
  NOT YET IMPLEMENTED


/*   ----- Searching in the Eistring for a string -----   */

#define eistr_ei(eistr, eistr2) \
  NOT YET IMPLEMENTED
#define eistr_ei_char(eistr, eistr2) \
  NOT YET IMPLEMENTED
#define eistr_ei_off(eistr, eistr2, off, charoff) \
  NOT YET IMPLEMENTED
#define eistr_ei_off_char(eistr, eistr2, off, charoff) \
  NOT YET IMPLEMENTED
#define eirstr_ei(eistr, eistr2) \
  NOT YET IMPLEMENTED
#define eirstr_ei_char(eistr, eistr2) \
  NOT YET IMPLEMENTED
#define eirstr_ei_off(eistr, eistr2, off, charoff) \
  NOT YET IMPLEMENTED
#define eirstr_ei_off_char(eistr, eistr2, off, charoff) \
  NOT YET IMPLEMENTED

#define eistr_c(eistr, c_string) \
  NOT YET IMPLEMENTED
#define eistr_c_char(eistr, c_string) \
  NOT YET IMPLEMENTED
#define eistr_c_off(eistr, c_string, off, charoff) \
  NOT YET IMPLEMENTED
#define eistr_c_off_char(eistr, c_string, off, charoff) \
  NOT YET IMPLEMENTED
#define eirstr_c(eistr, c_string) \
  NOT YET IMPLEMENTED
#define eirstr_c_char(eistr, c_string) \
  NOT YET IMPLEMENTED
#define eirstr_c_off(eistr, c_string, off, charoff) \
  NOT YET IMPLEMENTED
#define eirstr_c_off_char(eistr, c_string, off, charoff) \
  NOT YET IMPLEMENTED


/*   ----- Comparison -----   */

int eicmp_1 (Eistring *ei, Bytecount off, Charcount charoff,
	     Bytecount len, Charcount charlen, const Ibyte *data,
	     const Eistring *ei2, int is_c, int fold_case);

#define eicmp_ei(eistr, eistr2) \
  eicmp_1 (eistr, 0, -1, -1, -1, 0, eistr2, 0, 0)
#define eicmp_off_ei(eistr, off, charoff, len, charlen, eistr2) \
  eicmp_1 (eistr, off, charoff, len, charlen, 0, eistr2, 0, 0)
#define eicasecmp_ei(eistr, eistr2) \
  eicmp_1 (eistr, 0, -1, -1, -1, 0, eistr2, 0, 1)
#define eicasecmp_off_ei(eistr, off, charoff, len, charlen, eistr2) \
  eicmp_1 (eistr, off, charoff, len, charlen, 0, eistr2, 0, 1)
#define eicasecmp_i18n_ei(eistr, eistr2) \
  eicmp_1 (eistr, 0, -1, -1, -1, 0, eistr2, 0, 2)
#define eicasecmp_i18n_off_ei(eistr, off, charoff, len, charlen, eistr2) \
  eicmp_1 (eistr, off, charoff, len, charlen, 0, eistr2, 0, 2)

#define eicmp_c(eistr, c_string) \
  eicmp_1 (eistr, 0, -1, -1, -1, c_string, 0, 1, 0)
#define eicmp_off_c(eistr, off, charoff, len, charlen, c_string) \
  eicmp_1 (eistr, off, charoff, len, charlen, c_string, 0, 1, 0)
#define eicasecmp_c(eistr, c_string) \
  eicmp_1 (eistr, 0, -1, -1, -1, c_string, 0, 1, 1)
#define eicasecmp_off_c(eistr, off, charoff, len, charlen, c_string) \
  eicmp_1 (eistr, off, charoff, len, charlen, c_string, 0, 1, 1)
#define eicasecmp_i18n_c(eistr, c_string) \
  eicmp_1 (eistr, 0, -1, -1, -1, c_string, 0, 1, 2)
#define eicasecmp_i18n_off_c(eistr, off, charoff, len, charlen, c_string) \
  eicmp_1 (eistr, off, charoff, len, charlen, c_string, 0, 1, 2)


/*   ----- Case-changing the Eistring -----   */

int eistr_casefiddle_1 (Ibyte *olddata, Bytecount len, Ibyte *newdata,
			int downp);

#define EI_CASECHANGE(ei, downp)					\
do {									\
  int ei11new_allocmax = (ei)->charlen_ * MAX_ICHAR_LEN + 1;		\
  Ibyte *ei11storage =							\
     (Ibyte *) alloca_ibytes (ei11new_allocmax);			\
  int ei11newlen = eistr_casefiddle_1 ((ei)->data_, (ei)->bytelen_,	\
				       ei11storage, downp);		\
									\
  if (ei11newlen)							\
    {									\
      (ei)->max_size_allocated_ = ei11new_allocmax;			\
      (ei)->data_ = ei11storage;					\
      (ei)->bytelen_ = ei11newlen;					\
      /* charlen is the same. */					\
    }									\
} while (0)

#define eilwr(ei) EI_CASECHANGE (ei, 1)
#define eiupr(ei) EI_CASECHANGE (ei, 0)

END_C_DECLS


/************************************************************************/
/*                                                                      */
/*         Converting between internal and external format              */
/*                                                                      */
/************************************************************************/
/*
  The macros below are used for converting data between different formats.
  Generally, the data is textual, and the formats are related to
  internationalization (e.g. converting between internal-format text and
  UTF-8) -- but the mechanism is general, and could be used for anything,
  e.g. decoding gzipped data.

  In general, conversion involves a source of data, a sink, the existing
  format of the source data, and the desired format of the sink.  The
  macros below, however, always require that either the source or sink is
  internal-format text.  Therefore, in practice the conversions below
  involve source, sink, an external format (specified by a coding system),
  and the direction of conversion (internal->external or vice-versa).

  Sources and sinks can be raw data (sized or unsized -- when unsized,
  input data is assumed to be null-terminated [double null-terminated for
  Unicode-format data], and on output the length is not stored anywhere),
  Lisp strings, Lisp buffers, lstreams, and opaque data objects.  When the
  output is raw data, the result can be allocated either with alloca() or
  malloc(). (There is currently no provision for writing into a fixed
  buffer.  If you want this, use alloca() output and then copy the data --
  but be careful with the size!  Unless you are very sure of the encoding
  being used, upper bounds for the size are not in general computable.)
  The obvious restrictions on source and sink types apply (e.g. Lisp
  strings are a source and sink only for internal data).

  All raw data outputted will contain an extra null byte (two bytes for
  Unicode -- currently, in fact, all output data, whether internal or
  external, is double-null-terminated, but you can't count on this; see
  below).  This means that enough space is allocated to contain the extra
  nulls; however, these nulls are not reflected in the returned output
  size.

  The most basic macros are TO_EXTERNAL_FORMAT and TO_INTERNAL_FORMAT.
  These can be used to convert between any kinds of sources or sinks.
  However, 99% of conversions involve raw data or Lisp strings as both
  source and sink, and usually data is output as alloca() rather than
  malloc().  For this reason, convenience macros are defined for many types
  of conversions involving raw data and/or Lisp strings, especially when
  the output is an alloca()ed string. (When the destination is a
  Lisp_String, there are other functions that should be used instead --
  build_ext_string() and make_ext_string(), for example.) The convenience
  macros are of two types -- the older kind that store the result into a
  specified variable, and the newer kind that return the result.  The newer
  kind of macros don't exist when the output is sized data, because that
  would have two return values.  NOTE: All convenience macros are
  ultimately defined in terms of TO_EXTERNAL_FORMAT and TO_INTERNAL_FORMAT.
  Thus, any comments below about the workings of these macros also apply to
  all convenience macros.

  TO_EXTERNAL_FORMAT (source_type, source, sink_type, sink, codesys)
  TO_INTERNAL_FORMAT (source_type, source, sink_type, sink, codesys)

  Typical use is

     TO_EXTERNAL_FORMAT (LISP_STRING, str, C_STRING_MALLOC, ptr, Qfile_name);

  which means that the contents of the lisp string `str' are written
  to a malloc'ed memory area which will be pointed to by `ptr', after the
  function returns.  The conversion will be done using the `file-name'
  coding system (which will be controlled by the user indirectly by
  setting or binding the variable `file-name-coding-system').

  Some sources and sinks require two C variables to specify.  We use
  some preprocessor magic to allow different source and sink types, and
  even different numbers of arguments to specify different types of
  sources and sinks.

  So we can have a call that looks like

     TO_INTERNAL_FORMAT (DATA, (ptr, len),
                         MALLOC, (ptr, len),
                         coding_system);

  The parenthesized argument pairs are required to make the
  preprocessor magic work.

  NOTE: GC is inhibited during the entire operation of these macros.  This
  is because frequently the data to be converted comes from strings but
  gets passed in as just DATA, and GC may move around the string data.  If
  we didn't inhibit GC, there'd have to be a lot of messy recoding,
  alloca-copying of strings and other annoying stuff.
		      
  The source or sink can be specified in one of these ways:

  DATA,   (ptr, len),    // input data is a fixed buffer of size len
  ALLOCA, (ptr, len),    // output data is in a ALLOCA()ed buffer of size len
  MALLOC, (ptr, len),    // output data is in a malloc()ed buffer of size len
  C_STRING_ALLOCA, ptr,  // equivalent to ALLOCA (ptr, len_ignored) on output
  C_STRING_MALLOC, ptr,  // equivalent to MALLOC (ptr, len_ignored) on output
  C_STRING,     ptr,     // equivalent to DATA, (ptr, strlen/wcslen (ptr))
                         // on input (the Unicode version is used when correct)
  LISP_STRING,  string,  // input or output is a Lisp_Object of type string
  LISP_BUFFER,  buffer,  // output is written to (point) in lisp buffer
  LISP_LSTREAM, lstream, // input or output is a Lisp_Object of type lstream
  LISP_OPAQUE,  object,  // input or output is a Lisp_Object of type opaque

  When specifying the sink, use lvalues, since the macro will assign to them,
  except when the sink is an lstream or a lisp buffer.

  For the sink types `ALLOCA' and `C_STRING_ALLOCA', the resulting text is
  stored in a stack-allocated buffer, which is automatically freed on
  returning from the function.  However, the sink types `MALLOC' and
  `C_STRING_MALLOC' return `xmalloc()'ed memory.  The caller is responsible
  for freeing this memory using `xfree()'.

  The macros accept the kinds of sources and sinks appropriate for
  internal and external data representation.  See the type_checking_assert
  macros below for the actual allowed types.

  Since some sources and sinks use one argument (a Lisp_Object) to
  specify them, while others take a (pointer, length) pair, we use
  some C preprocessor trickery to allow pair arguments to be specified
  by parenthesizing them, as in the examples above.

  Anything prefixed by dfc_ (`data format conversion') is private.
  They are only used to implement these macros.

  [[Using C_STRING* is appropriate for using with external APIs that
  take null-terminated strings.  For internal data, we should try to
  be '\0'-clean - i.e. allow arbitrary data to contain embedded '\0'.

  Sometime in the future we might allow output to C_STRING_ALLOCA or
  C_STRING_MALLOC _only_ with TO_EXTERNAL_FORMAT(), not
  TO_INTERNAL_FORMAT().]]

  The above comments are not true.  Frequently (most of the time, in
  fact), external strings come as zero-terminated entities, where the
  zero-termination is the only way to find out the length.  Even in
  cases where you can get the length, most of the time the system will
  still use the null to signal the end of the string, and there will
  still be no way to either send in or receive a string with embedded
  nulls.  In such situations, it's pointless to track the length
  because null bytes can never be in the string.  We have a lot of
  operations that make it easy to operate on zero-terminated strings,
  and forcing the user the deal with the length everywhere would only
  make the code uglier and more complicated, for no gain. --ben

  There is no problem using the same lvalue for source and sink.

  Also, when pointers are required, the code (currently at least) is
  lax and allows any pointer types, either in the source or the sink.
  This makes it possible, e.g., to deal with internal format data held
  in char *'s or external format data held in WCHAR * (i.e. Unicode).

  Finally, whenever storage allocation is called for, extra space is
  allocated for a terminating zero, and such a zero is stored in the
  appropriate place, regardless of whether the source data was
  specified using a length or was specified as zero-terminated.  This
  allows you to freely pass the resulting data, no matter how
  obtained, to a routine that expects zero termination (modulo, of
  course, that any embedded zeros in the resulting text will cause
  truncation).  In fact, currently two embedded zeros are allocated
  and stored after the data result.  This is to allow for the
  possibility of storing a Unicode value on output, which needs the
  two zeros.  Currently, however, the two zeros are stored regardless
  of whether the conversion is internal or external and regardless of
  whether the external coding system is in fact Unicode.  This
  behavior may change in the future, and you cannot rely on this --
  the most you can rely on is that sink data in Unicode format will
  have two terminating nulls, which combine to form one Unicode null
  character.

  NOTE: You might ask, why are these not written as functions that
  *RETURN* the converted string, since that would allow them to be used
  much more conveniently, without having to constantly declare temporary
  variables?  The answer is that in fact I originally did write the
  routines that way, but that required either

  (a) calling alloca() inside of a function call, or
  (b) using expressions separated by commas and a global temporary variable, or
  (c) using the GCC extension ({ ... }).

  Turned out that all of the above had bugs, all caused by GCC (hence the
  comments about "those GCC wankers" and "ream gcc up the ass").  As for
  (a), some versions of GCC (especially on Intel platforms), which had
  buggy implementations of alloca() that couldn't handle being called
  inside of a function call -- they just decremented the stack right in the
  middle of pushing args.  Oops, crash with stack trashing, very bad.  (b)
  was an attempt to fix (a), and that led to further GCC crashes, esp. when
  you had two such calls in a single subexpression, because GCC couldn't be
  counted upon to follow even a minimally reasonable order of execution.
  True, you can't count on one argument being evaluated before another, but
  GCC would actually interleave them so that the temp var got stomped on by
  one while the other was accessing it.  So I tried (c), which was
  problematic because that GCC extension has more bugs in it than a
  termite's nest.

  So reluctantly I converted to the current way.  Now, that was awhile ago
  (c. 1994), and it appears that the bug involving alloca in function calls
  has long since been fixed.  More recently, I defined the new-dfc routines
  down below, which DO allow exactly such convenience of returning your
  args rather than store them in temp variables, and I also wrote a
  configure check to see whether alloca() causes crashes inside of function
  calls, and if so use the portable alloca() implementation in alloca.c.
  If you define TEST_NEW_DFC, the old routines get written in terms of the
  new ones, and I've had a beta put out with this on and it appeared to
  this appears to cause no problems -- so we should consider
  switching, and feel no compunctions about writing further such function-
  like alloca() routines in lieu of statement-like ones. --ben */

#define TO_EXTERNAL_FORMAT(source_type, source, sink_type, sink, codesys)  \
do {									   \
  dfc_conversion_type dfc_simplified_source_type;			   \
  dfc_conversion_type dfc_simplified_sink_type;				   \
  dfc_conversion_data dfc_source;					   \
  dfc_conversion_data dfc_sink;						   \
  Lisp_Object dfc_codesys = (codesys);					   \
									   \
  type_checking_assert							   \
    ((DFC_TYPE_##source_type == DFC_TYPE_DATA ||			   \
      DFC_TYPE_##source_type == DFC_TYPE_C_STRING ||			   \
      DFC_TYPE_##source_type == DFC_TYPE_LISP_STRING ||			   \
      DFC_TYPE_##source_type == DFC_TYPE_LISP_OPAQUE ||			   \
      DFC_TYPE_##source_type == DFC_TYPE_LISP_LSTREAM)			   \
    &&									   \
     (DFC_TYPE_##sink_type == DFC_TYPE_ALLOCA ||			   \
      DFC_TYPE_##sink_type == DFC_TYPE_MALLOC ||			   \
      DFC_TYPE_##sink_type == DFC_TYPE_C_STRING_ALLOCA ||		   \
      DFC_TYPE_##sink_type == DFC_TYPE_C_STRING_MALLOC ||		   \
      DFC_TYPE_##sink_type == DFC_TYPE_LISP_LSTREAM ||			   \
      DFC_TYPE_##sink_type == DFC_TYPE_LISP_OPAQUE));			   \
									   \
  DFC_EXT_SOURCE_##source_type##_TO_ARGS (source, dfc_codesys);		   \
  DFC_SINK_##sink_type##_TO_ARGS (sink);				   \
									   \
  dfc_convert_to_external_format (dfc_simplified_source_type, &dfc_source, \
				  dfc_codesys,				   \
				  dfc_simplified_sink_type,   &dfc_sink);  \
									   \
  DFC_##sink_type##_USE_CONVERTED_DATA (sink);				   \
} while (0)

#define TO_INTERNAL_FORMAT(source_type, source, sink_type, sink, codesys)  \
do {									   \
  dfc_conversion_type dfc_simplified_source_type;			   \
  dfc_conversion_type dfc_simplified_sink_type;				   \
  dfc_conversion_data dfc_source;					   \
  dfc_conversion_data dfc_sink;						   \
  Lisp_Object dfc_codesys = (codesys);					   \
									   \
  type_checking_assert							   \
    ((DFC_TYPE_##source_type == DFC_TYPE_DATA ||			   \
      DFC_TYPE_##source_type == DFC_TYPE_C_STRING ||			   \
      DFC_TYPE_##source_type == DFC_TYPE_LISP_OPAQUE ||			   \
      DFC_TYPE_##source_type == DFC_TYPE_LISP_LSTREAM)			   \
     &&									   \
     (DFC_TYPE_##sink_type == DFC_TYPE_ALLOCA ||			   \
      DFC_TYPE_##sink_type == DFC_TYPE_MALLOC ||			   \
      DFC_TYPE_##sink_type == DFC_TYPE_C_STRING_ALLOCA ||		   \
      DFC_TYPE_##sink_type == DFC_TYPE_C_STRING_MALLOC ||		   \
      DFC_TYPE_##sink_type == DFC_TYPE_LISP_STRING ||			   \
      DFC_TYPE_##sink_type == DFC_TYPE_LISP_LSTREAM ||			   \
      DFC_TYPE_##sink_type == DFC_TYPE_LISP_BUFFER));			   \
									   \
  DFC_INT_SOURCE_##source_type##_TO_ARGS (source, dfc_codesys);		   \
  DFC_SINK_##sink_type##_TO_ARGS (sink);				   \
									   \
  dfc_convert_to_internal_format (dfc_simplified_source_type, &dfc_source, \
				  dfc_codesys,				   \
				  dfc_simplified_sink_type,   &dfc_sink);  \
									   \
  DFC_##sink_type##_USE_CONVERTED_DATA (sink);				   \
} while (0)

#ifdef __cplusplus

/* Error if you try to use a union here: "member `struct {anonymous
union}::{anonymous} {anonymous union}::data' with constructor not allowed
in union" (Bytecount is a class) */

typedef struct
#else
typedef union
#endif
{
  struct { const void *ptr; Bytecount len; } data;
  Lisp_Object lisp_object;
} dfc_conversion_data;

enum dfc_conversion_type
{
  DFC_TYPE_DATA,
  DFC_TYPE_ALLOCA,
  DFC_TYPE_MALLOC,
  DFC_TYPE_C_STRING,
  DFC_TYPE_C_STRING_ALLOCA,
  DFC_TYPE_C_STRING_MALLOC,
  DFC_TYPE_LISP_STRING,
  DFC_TYPE_LISP_LSTREAM,
  DFC_TYPE_LISP_OPAQUE,
  DFC_TYPE_LISP_BUFFER
};
typedef enum dfc_conversion_type dfc_conversion_type;

BEGIN_C_DECLS

/* WARNING: These use a static buffer.  This can lead to disaster if
   these functions are not used *very* carefully.  Another reason to only use
   TO_EXTERNAL_FORMAT() and TO_INTERNAL_FORMAT(). */
MODULE_API void
dfc_convert_to_external_format (dfc_conversion_type source_type,
				dfc_conversion_data *source,
				Lisp_Object codesys,
				dfc_conversion_type sink_type,
				dfc_conversion_data *sink);
MODULE_API void
dfc_convert_to_internal_format (dfc_conversion_type source_type,
				dfc_conversion_data *source,
				Lisp_Object codesys,
				dfc_conversion_type sink_type,
				dfc_conversion_data *sink);
/* CPP Trickery */
#define DFC_CPP_CAR(x,y) (x)
#define DFC_CPP_CDR(x,y) (y)

/* Convert `source' to args for dfc_convert_to_external_format() */
#define DFC_EXT_SOURCE_DATA_TO_ARGS(val, codesys) do {	\
  dfc_source.data.ptr = DFC_CPP_CAR val;		\
  dfc_source.data.len = DFC_CPP_CDR val;		\
  dfc_simplified_source_type = DFC_TYPE_DATA;		\
} while (0)
#define DFC_EXT_SOURCE_C_STRING_TO_ARGS(val, codesys) do {	\
  dfc_source.data.len =						\
    strlen ((char *) (dfc_source.data.ptr = (val)));		\
  dfc_simplified_source_type = DFC_TYPE_DATA;			\
} while (0)
#define DFC_EXT_SOURCE_LISP_STRING_TO_ARGS(val, codesys) do {	\
  Lisp_Object dfc_slsta = (val);				\
  type_checking_assert (STRINGP (dfc_slsta));			\
  dfc_source.lisp_object = dfc_slsta;				\
  dfc_simplified_source_type = DFC_TYPE_LISP_STRING;		\
} while (0)
#define DFC_EXT_SOURCE_LISP_LSTREAM_TO_ARGS(val, codesys) do {	\
  Lisp_Object dfc_sllta = (val);				\
  type_checking_assert (LSTREAMP (dfc_sllta));			\
  dfc_source.lisp_object = dfc_sllta;				\
  dfc_simplified_source_type = DFC_TYPE_LISP_LSTREAM;		\
} while (0)
#define DFC_EXT_SOURCE_LISP_OPAQUE_TO_ARGS(val, codesys) do {	\
  Lisp_Opaque *dfc_slota = XOPAQUE (val);			\
  dfc_source.data.ptr = OPAQUE_DATA (dfc_slota);		\
  dfc_source.data.len = OPAQUE_SIZE (dfc_slota);		\
  dfc_simplified_source_type = DFC_TYPE_DATA;			\
} while (0)

/* Convert `source' to args for dfc_convert_to_internal_format() */
#define DFC_INT_SOURCE_DATA_TO_ARGS(val, codesys) \
  DFC_EXT_SOURCE_DATA_TO_ARGS (val, codesys)
#define DFC_INT_SOURCE_C_STRING_TO_ARGS(val, codesys) do {		    \
  dfc_source.data.len = dfc_external_data_len (dfc_source.data.ptr = (val), \
					       codesys);		    \
  dfc_simplified_source_type = DFC_TYPE_DATA;				    \
} while (0)
#define DFC_INT_SOURCE_LISP_STRING_TO_ARGS(val, codesys) \
  DFC_EXT_SOURCE_LISP_STRING_TO_ARGS (val, codesys)
#define DFC_INT_SOURCE_LISP_LSTREAM_TO_ARGS(val, codesys) \
  DFC_EXT_SOURCE_LISP_LSTREAM_TO_ARGS (val, codesys)
#define DFC_INT_SOURCE_LISP_OPAQUE_TO_ARGS(val, codesys) \
  DFC_EXT_SOURCE_LISP_OPAQUE_TO_ARGS (val, codesys)

/* Convert `sink' to args for dfc_convert_to_*_format() */
#define DFC_SINK_ALLOCA_TO_ARGS(val)		\
  dfc_simplified_sink_type = DFC_TYPE_DATA
#define DFC_SINK_C_STRING_ALLOCA_TO_ARGS(val)	\
  dfc_simplified_sink_type = DFC_TYPE_DATA
#define DFC_SINK_MALLOC_TO_ARGS(val)		\
  dfc_simplified_sink_type = DFC_TYPE_DATA
#define DFC_SINK_C_STRING_MALLOC_TO_ARGS(val)	\
  dfc_simplified_sink_type = DFC_TYPE_DATA
#define DFC_SINK_LISP_STRING_TO_ARGS(val)	\
  dfc_simplified_sink_type = DFC_TYPE_DATA
#define DFC_SINK_LISP_OPAQUE_TO_ARGS(val)	\
  dfc_simplified_sink_type = DFC_TYPE_DATA
#define DFC_SINK_LISP_LSTREAM_TO_ARGS(val) do {		\
  Lisp_Object dfc_sllta = (val);			\
  type_checking_assert (LSTREAMP (dfc_sllta));		\
  dfc_sink.lisp_object = dfc_sllta;			\
  dfc_simplified_sink_type = DFC_TYPE_LISP_LSTREAM;	\
} while (0)
#define DFC_SINK_LISP_BUFFER_TO_ARGS(val) do {		\
  struct buffer *dfc_slbta = XBUFFER (val);		\
  dfc_sink.lisp_object =				\
    make_lisp_buffer_output_stream			\
    (dfc_slbta, BUF_PT (dfc_slbta), 0);			\
  dfc_simplified_sink_type = DFC_TYPE_LISP_LSTREAM;	\
} while (0)

/* Assign to the `sink' lvalue(s) using the converted data. */
/* + 2 because we double zero-extended to account for Unicode conversion */
typedef union { char c; void *p; } *dfc_aliasing_voidpp;
#define DFC_ALLOCA_USE_CONVERTED_DATA(sink) do {			\
  void * dfc_sink_ret = ALLOCA (dfc_sink.data.len + 2);			\
  memcpy (dfc_sink_ret, dfc_sink.data.ptr, dfc_sink.data.len + 2);	\
  VOIDP_CAST (DFC_CPP_CAR sink) = dfc_sink_ret;				\
  (DFC_CPP_CDR sink) = dfc_sink.data.len;				\
} while (0)
#define DFC_MALLOC_USE_CONVERTED_DATA(sink) do {			\
  void * dfc_sink_ret = xmalloc (dfc_sink.data.len + 2);		\
  memcpy (dfc_sink_ret, dfc_sink.data.ptr, dfc_sink.data.len + 2);	\
  VOIDP_CAST (DFC_CPP_CAR sink) = dfc_sink_ret;				\
  (DFC_CPP_CDR sink) = dfc_sink.data.len;				\
} while (0)
#define DFC_C_STRING_ALLOCA_USE_CONVERTED_DATA(sink) do {		\
  void * dfc_sink_ret = ALLOCA (dfc_sink.data.len + 2);			\
  memcpy (dfc_sink_ret, dfc_sink.data.ptr, dfc_sink.data.len + 2);	\
  VOIDP_CAST (sink) = dfc_sink_ret;					\
} while (0)
#define DFC_C_STRING_MALLOC_USE_CONVERTED_DATA(sink) do {		\
  void * dfc_sink_ret = xmalloc (dfc_sink.data.len + 2);		\
  memcpy (dfc_sink_ret, dfc_sink.data.ptr, dfc_sink.data.len + 2);	\
  VOIDP_CAST (sink) = dfc_sink_ret;					\
} while (0)
#define DFC_LISP_STRING_USE_CONVERTED_DATA(sink) \
  sink = make_string ((Ibyte *) dfc_sink.data.ptr, dfc_sink.data.len)
#define DFC_LISP_OPAQUE_USE_CONVERTED_DATA(sink) \
  sink = make_opaque (dfc_sink.data.ptr, dfc_sink.data.len)
#define DFC_LISP_LSTREAM_USE_CONVERTED_DATA(sink) /* data already used */
#define DFC_LISP_BUFFER_USE_CONVERTED_DATA(sink) \
  Lstream_delete (XLSTREAM (dfc_sink.lisp_object))

/* #define TEST_NEW_DFC */

/* Convenience macros for extremely common invocations */
#ifdef TEST_NEW_DFC
#define C_STRING_TO_EXTERNAL(in, out, codesys)			\
  do { * (Extbyte **) &(out) = 					\
       NEW_C_STRING_TO_EXTERNAL (in, codesys); } while (0)
#define SIZED_C_STRING_TO_EXTERNAL(in, inlen, out, codesys)		\
  do { * (Extbyte **) &(out) =						\
       NEW_SIZED_C_STRING_TO_EXTERNAL (in, inlen, codesys); } while (0)
#define EXTERNAL_TO_C_STRING(in, out, codesys)			\
  do { * (Ibyte **) &(out) =					\
       NEW_EXTERNAL_TO_C_STRING (in, codesys); } while (0)
#define SIZED_EXTERNAL_TO_C_STRING(in, inlen, out, codesys)		\
  do { * (Ibyte **) &(out) =						\
       NEW_SIZED_EXTERNAL_TO_C_STRING (in, inlen, codesys); } while (0)
#define LISP_STRING_TO_EXTERNAL(in, out, codesys)		\
  do { * (Extbyte **) &(out) =					\
       NEW_LISP_STRING_TO_EXTERNAL (in, codesys); } while (0)
#else
#define C_STRING_TO_EXTERNAL(in, out, codesys) \
  TO_EXTERNAL_FORMAT (C_STRING, in, C_STRING_ALLOCA, out, codesys)
#define SIZED_C_STRING_TO_EXTERNAL(in, inlen, out, codesys) \
  TO_EXTERNAL_FORMAT (DATA, (in, inlen), C_STRING_ALLOCA, out, codesys)
#define EXTERNAL_TO_C_STRING(in, out, codesys) \
  TO_INTERNAL_FORMAT (C_STRING, in, C_STRING_ALLOCA, out, codesys)
#define SIZED_EXTERNAL_TO_C_STRING(in, inlen, out, codesys) \
  TO_INTERNAL_FORMAT (DATA, (in, inlen), C_STRING_ALLOCA, out, codesys)
#define LISP_STRING_TO_EXTERNAL(in, out, codesys) \
  TO_EXTERNAL_FORMAT (LISP_STRING, in, C_STRING_ALLOCA, out, codesys)
#endif /* TEST_NEW_DFC */

#define C_STRING_TO_SIZED_EXTERNAL(in, out, outlen, codesys) \
  TO_EXTERNAL_FORMAT (C_STRING, in, ALLOCA, (out, outlen), codesys)
#define SIZED_C_STRING_TO_SIZED_EXTERNAL(in, inlen, out, outlen, codesys) \
  TO_EXTERNAL_FORMAT (DATA, (in, inlen), ALLOCA, (out, outlen), codesys)
#define EXTERNAL_TO_SIZED_C_STRING(in, out, outlen, codesys) \
  TO_INTERNAL_FORMAT (C_STRING, in, ALLOCA, (out, outlen), codesys)
#define SIZED_EXTERNAL_TO_SIZED_C_STRING(in, inlen, out, outlen, codesys) \
  TO_INTERNAL_FORMAT (DATA, (in, inlen), ALLOCA, (out, outlen), codesys)
#define LISP_STRING_TO_SIZED_EXTERNAL(in, out, outlen, codesys) \
  TO_EXTERNAL_FORMAT (LISP_STRING, in, ALLOCA, (out, outlen), codesys)

/* In place of EXTERNAL_TO_LISP_STRING(), use build_ext_string() and/or
   make_ext_string(). */

#ifdef TEST_NEW_DFC
#define C_STRING_TO_EXTERNAL_MALLOC(in, out, codesys)			\
  do { * (Extbyte **) &(out) =						\
       NEW_C_STRING_TO_EXTERNAL_MALLOC (in, codesys); } while (0)
#define SIZED_C_STRING_TO_EXTERNAL_MALLOC(in, inlen, out, codesys)	\
  do { * (Extbyte **) &(out) =						\
       NEW_SIZED_C_STRING_TO_EXTERNAL_MALLOC (in, inlen, codesys); }	\
  while (0)
#define EXTERNAL_TO_C_STRING_MALLOC(in, out, codesys)			\
  do { * (Ibyte **) &(out) =						\
       NEW_EXTERNAL_TO_C_STRING_MALLOC (in, codesys); } while (0)
#define SIZED_EXTERNAL_TO_C_STRING_MALLOC(in, inlen, out, codesys)	\
  do { * (Ibyte **) &(out) =						\
       NEW_SIZED_EXTERNAL_TO_C_STRING_MALLOC (in, inlen, codesys); }	\
  while (0)
#define LISP_STRING_TO_EXTERNAL_MALLOC(in, out, codesys)		\
  do { * (Extbyte **) &(out) =						\
       NEW_LISP_STRING_TO_EXTERNAL_MALLOC (in, codesys); } while (0)
#else
#define C_STRING_TO_EXTERNAL_MALLOC(in, out, codesys) \
  TO_EXTERNAL_FORMAT (C_STRING, in, C_STRING_MALLOC, out, codesys)
#define SIZED_C_STRING_TO_EXTERNAL_MALLOC(in, inlen, out, codesys) \
  TO_EXTERNAL_FORMAT (DATA, (in, inlen), C_STRING_MALLOC, out, codesys)
#define EXTERNAL_TO_C_STRING_MALLOC(in, out, codesys) \
  TO_INTERNAL_FORMAT (C_STRING, in, C_STRING_MALLOC, out, codesys)
#define SIZED_EXTERNAL_TO_C_STRING_MALLOC(in, inlen, out, codesys) \
  TO_INTERNAL_FORMAT (DATA, (in, inlen), C_STRING_MALLOC, out, codesys)
#define LISP_STRING_TO_EXTERNAL_MALLOC(in, out, codesys) \
  TO_EXTERNAL_FORMAT (LISP_STRING, in, C_STRING_MALLOC, out, codesys)
#endif /* TEST_NEW_DFC */

#define C_STRING_TO_SIZED_EXTERNAL_MALLOC(in, out, outlen, codesys) \
  TO_EXTERNAL_FORMAT (C_STRING, in, MALLOC, (out, outlen), codesys)
#define SIZED_C_STRING_TO_SIZED_EXTERNAL_MALLOC(in, inlen, out, outlen, \
						codesys)		\
  TO_EXTERNAL_FORMAT (DATA, (in, inlen), MALLOC, (out, outlen), codesys)
#define EXTERNAL_TO_SIZED_C_STRING_MALLOC(in, out, outlen, codesys) \
  TO_INTERNAL_FORMAT (C_STRING, in, MALLOC, (out, outlen), codesys)
#define SIZED_EXTERNAL_TO_SIZED_C_STRING_MALLOC(in, inlen, out, outlen, \
						codesys)		\
  TO_INTERNAL_FORMAT (DATA, (in, inlen), MALLOC, (out, outlen), codesys)
#define LISP_STRING_TO_SIZED_EXTERNAL_MALLOC(in, out, outlen, codesys) \
  TO_EXTERNAL_FORMAT (LISP_STRING, in, MALLOC, (out, outlen), codesys)

enum new_dfc_src_type
{
  DFC_EXTERNAL,
  DFC_SIZED_EXTERNAL,
  DFC_INTERNAL,
  DFC_SIZED_INTERNAL,
  DFC_LISP_STRING
};

MODULE_API void *new_dfc_convert_malloc (const void *src, Bytecount src_size,
					 enum new_dfc_src_type type,
					 Lisp_Object codesys);
MODULE_API Bytecount new_dfc_convert_size (const char *srctext,
					   const void *src,
					   Bytecount src_size,
					   enum new_dfc_src_type type,
					   Lisp_Object codesys);
MODULE_API void *new_dfc_convert_copy_data (const char *srctext,
					    void *alloca_data);

END_C_DECLS

/* Version of EXTERNAL_TO_C_STRING that *RETURNS* the translated string,
   still in alloca() space.  Requires some trickiness to do this, but gets
   it done! */

/* NOTE: If you make two invocations of the dfc functions below in the same
   subexpression and use the exact same expression for the source in both
   cases, you will lose.  In this unlikely case, you will get an abort, and
   need to rewrite the code.
*/

/* We need to use ALLOCA_FUNCALL_OK here.  Some compilers have been known
   to choke when alloca() occurs as a funcall argument, and so we check
   this in configure.  Rewriting the expressions below to use a temporary
   variable, so that the call to alloca() is outside of
   new_dfc_convert_copy_data(), won't help because the entire NEW_DFC call
   could be inside of a function call. */

#define NEW_DFC_CONVERT_1_ALLOCA(src, src_size, type, codesys)		\
  new_dfc_convert_copy_data						\
   (#src, ALLOCA_FUNCALL_OK (new_dfc_convert_size (#src, src, src_size,	\
						   type, codesys)))

#define NEW_EXTERNAL_TO_C_STRING(src, codesys)	\
  (Ibyte *) NEW_DFC_CONVERT_1_ALLOCA (src, -1, DFC_EXTERNAL, codesys)
#define NEW_EXTERNAL_TO_C_STRING_MALLOC(src, codesys)	\
  (Ibyte *) new_dfc_convert_malloc (src, -1, DFC_EXTERNAL, codesys)
#define NEW_SIZED_EXTERNAL_TO_C_STRING(src, len, codesys)	\
  (Ibyte *) NEW_DFC_CONVERT_1_ALLOCA (src, len, DFC_SIZED_EXTERNAL, codesys)
#define NEW_SIZED_EXTERNAL_TO_C_STRING_MALLOC(src, len, codesys)	\
  (Ibyte *) new_dfc_convert_malloc (src, len, DFC_SIZED_EXTERNAL, codesys)
#define NEW_C_STRING_TO_EXTERNAL(src, codesys)	\
  (Extbyte *) NEW_DFC_CONVERT_1_ALLOCA (src, -1, DFC_INTERNAL, codesys)
#define NEW_C_STRING_TO_EXTERNAL_MALLOC(src, codesys)	\
  (Extbyte *) new_dfc_convert_malloc (src, -1, DFC_INTERNAL, codesys)
#define NEW_SIZED_C_STRING_TO_EXTERNAL(src, len, codesys)	\
  (Extbyte *) NEW_DFC_CONVERT_1_ALLOCA (src, len, DFC_SIZED_INTERNAL, codesys)
#define NEW_SIZED_C_STRING_TO_EXTERNAL_MALLOC(src, len, codesys)	\
  (Extbyte *) new_dfc_convert_malloc (src, len, DFC_SIZED_INTERNAL, codesys)
#define NEW_LISP_STRING_TO_EXTERNAL(src, codesys)			\
  (Extbyte *) NEW_DFC_CONVERT_1_ALLOCA (LISP_TO_VOID (src), -1,		\
					DFC_LISP_STRING, codesys)
#define NEW_LISP_STRING_TO_EXTERNAL_MALLOC(src, codesys)	\
  (Extbyte *) new_dfc_convert_malloc (LISP_TO_VOID (src), -1,	\
				      DFC_LISP_STRING, codesys)

/* Standins for various encodings. */
#ifdef WEXTTEXT_IS_WIDE
#define Qcommand_argument_encoding Qmswindows_unicode
#define Qenvironment_variable_encoding Qmswindows_unicode
#else
#define Qcommand_argument_encoding Qnative
#define Qenvironment_variable_encoding Qnative
#endif
#define Qunix_host_name_encoding Qnative
#define Qunix_service_name_encoding Qnative
#define Qmswindows_host_name_encoding Qmswindows_multibyte
#define Qmswindows_service_name_encoding Qmswindows_multibyte

/* Wexttext functions.  The type of Wexttext is selected at compile time
   and will sometimes be wchar_t, sometimes char. */

int wcscmp_ascii (const wchar_t *s1, const Ascbyte *s2);
int wcsncmp_ascii (const wchar_t *s1, const Ascbyte *s2, Charcount len);

#ifdef WEXTTEXT_IS_WIDE /* defined under MS Windows i.e. WIN32_NATIVE */
#define WEXTTEXT_ZTERM_SIZE sizeof (wchar_t)
/* Extra indirection needed in case of manifest constant as arg */
#define WEXTSTRING_1(arg) L##arg
#define WEXTSTRING(arg) WEXTSTRING_1(arg)
#define wext_strlen wcslen
#define wext_strcmp wcscmp
#define wext_strncmp wcsncmp
#define wext_strcmp_ascii wcscmp_ascii
#define wext_strncmp_ascii wcsncmp_ascii
#define wext_strcpy wcscpy
#define wext_strncpy wcsncpy
#define wext_strchr wcschr
#define wext_strrchr wcsrchr
#define wext_strdup wcsdup
#define wext_atol(str) wcstol (str, 0, 10)
#define wext_sprintf wsprintfW /* Huh?  both wsprintfA and wsprintfW? */
#define wext_getenv _wgetenv
#define build_wext_string(str, cs) build_ext_string ((Extbyte *) str, cs)
#define WEXTTEXT_TO_8_BIT(arg) WEXTTEXT_TO_MULTIBYTE(arg)
#ifdef WIN32_NATIVE
int XCDECL wext_retry_open (const Wexttext *path, int oflag, ...);
#else
#error Cannot handle Wexttext yet on this system
#endif
#define wext_access _waccess
#define wext_stat _wstat
#else
#define WEXTTEXT_ZTERM_SIZE sizeof (char)
#define WEXTSTRING(arg) arg
#define wext_strlen strlen
#define wext_strcmp strcmp
#define wext_strncmp strncmp
#define wext_strcmp_ascii strcmp
#define wext_strncmp_ascii strncmp
#define wext_strcpy strcpy
#define wext_strncpy strncpy
#define wext_strchr strchr
#define wext_strrchr strrchr
#define wext_strdup xstrdup
#define wext_atol(str) atol (str)
#define wext_sprintf sprintf
#define wext_getenv getenv
#define build_wext_string build_ext_string
#define wext_retry_open retry_open
#define wext_access access
#define wext_stat stat
#define WEXTTEXT_TO_8_BIT(arg) ((Extbyte *) arg)
#endif

/* Standins for various X encodings.

   About encodings in X:

   X works with 5 different encodings:

   -- "Host Portable Character Encoding" == printable ASCII + space, tab,
      newline

   -- STRING encoding == ASCII + Latin-1 + tab, newline

   -- Locale-specific encoding

   -- Compound text == STRING encoding + ISO-2022 escape sequences to
      switch between different locale-specific encodings.

   -- ANSI C wide-character encoding

   The Host Portable Character Encoding (HPCE) is used for atom names, font
   names, color names, keysyms, geometry strings, resource manager quarks,
   display names, locale names, and various other things.  When describing
   such strings, the X manual typically says "If the ... is not in the Host
   Portable Character Encoding, the result is implementation dependent."

   The wide-character encoding is used only in the Xwc* functions, which
   are provided as equivalents to Xmb* functions.

   STRING and compound text are used in the value of string properties and
   selection data, both of which are values with an associated type atom,
   which can be STRING or COMPOUND_TEXT.  It can also be a locale name, as
   specified in setlocale() (#### as usual, there is no normalization
   whatsoever of these names).

   X also defines a type called "TEXT", which is used only as a requested
   type, and produces data in a type "convenient to the owner".  However,
   there is some indication that X expects this to be the locale-specific
   encoding.

   According to the glossary, the locale is used in

   -- Encoding and processing of input method text
   -- Encoding of resource files and values
   -- Encoding and imaging of text strings
   -- Encoding and decoding for inter-client text communication 

   The functions XmbTextListToTextProperty and XmbTextPropertyToTextList
   (and Xwc* equivalents) can be used to convert between the
   locale-specific encoding (XTextStyle), STRING (XStringStyle), and
   compound text (XCompoundTextStyle), as well as XStdICCTextStyle, which
   converts to STRING if possible, and if not, COMPOUND_TEXT.  This is
   used, for example, in XmbSetWMProperties, in the window_name and
   icon_name properties (WM_NAME and WM_ICON_NAME), which are in the
   locale-specific encoding on input, and are stored as STRING if possible,
   COMPOUND_TEXT otherwise.
   */

/* !!#### Need to verify the encoding used in lwlib -- Qnative or Qctext?
   Almost certainly the former.  Use a standin for now. */
#define Qlwlib_encoding Qnative

/* The Host Portable Character Encoding. */
#define Qx_hpc_encoding Qnative

#define Qx_atom_name_encoding Qx_hpc_encoding
#define Qx_font_name_encoding Qx_hpc_encoding
#define Qx_color_name_encoding Qx_hpc_encoding
#define Qx_keysym_encoding Qx_hpc_encoding
#define Qx_geometry_encoding Qx_hpc_encoding
#define Qx_resource_name_encoding Qx_hpc_encoding
#define Qx_application_class_encoding Qx_hpc_encoding
/* the following probably must agree with Qcommand_argument_encoding and
   Qenvironment_variable_encoding */
#define Qx_display_name_encoding Qx_hpc_encoding
#define Qx_xpm_data_encoding Qx_hpc_encoding

/* !!#### Verify these! */
#define Qxt_widget_arg_encoding Qnative
#define Qdt_dnd_encoding Qnative
#define Qoffix_dnd_encoding Qnative

/* RedHat 6.2 contains a locale called "Francais" with the C-cedilla
   encoded in ISO2022! */
#define Qlocale_name_encoding Qctext

#define Qstrerror_encoding Qnative

/* Encoding for strings coming from Offix drag-n-drop */
#define Qoffix_dnd_encoding Qnative

/* !!#### This exists to remind us that our hexify routine is totally
   un-Muleized. */
#define Qdnd_hexify_encoding Qascii

#define GET_STRERROR(var, num)					\
do {								\
  int __gsnum__ = (num);					\
  Extbyte * __gserr__ = strerror (__gsnum__);			\
								\
  if (!__gserr__)						\
    {								\
      var = alloca_ibytes (99);			\
      qxesprintf (var, "Unknown error %d", __gsnum__);		\
    }								\
  else								\
    EXTERNAL_TO_C_STRING (__gserr__, var, Qstrerror_encoding);	\
} while (0)

#endif /* INCLUDED_text_h_ */
