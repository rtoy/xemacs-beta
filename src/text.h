/* Header file for text manipulation primitives and macros.
   Copyright (C) 1985-1995 Free Software Foundation, Inc.
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

/* Synched up with: FSF 19.30. */

/* Authorship:

   Mostly written by Ben Wing, starting around 1995.
   Current TO_IN/EXTERNAL_FORMAT macros written by Martin Buchholz,
     designed by Ben Wing based on earlier macros by Ben Wing.
   Separated out June 18, 2000 from buffer.h into text.h.
 */

#ifndef INCLUDED_text_h_
#define INCLUDED_text_h_

#include <wchar.h>

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

#define REP_BYTES_BY_FIRST_BYTE(fb) 1
#define BYTE_ASCII_P(byte) 1
# define MAX_EMCHAR_LEN 1

#else /* MULE */

/* These are carefully designed to work if BYTE is signed or unsigned. */
/* Note that SPC and DEL are considered ASCII, not control. */

#define BYTE_ASCII_P(byte) (((byte) & ~0x7f) == 0)
#define BYTE_C0_P(byte)    (((byte) & ~0x1f) == 0)
#define BYTE_C1_P(byte)    (((byte) & ~0x1f) == 0x80)

/* Does BYTE represent the first byte of a character? */

#define INTBYTE_FIRST_BYTE_P(byte) ((byte) < 0xA0)

/* Does BYTE represent the first byte of a multi-byte character? */

#define INTBYTE_LEADING_BYTE_P(byte) BYTE_C1_P (byte)

/* Table of number of bytes in the string representation of a character
   indexed by the first byte of that representation.

   This value can be derived in other ways -- e.g. something like
   XCHARSET_REP_BYTES (CHARSET_BY_LEADING_BYTE (first_byte))
   but it's faster this way. */
extern const Bytecount rep_bytes_by_first_byte[0xA0];

/* Number of bytes in the string representation of a character. */

#ifdef ERROR_CHECK_TEXT

INLINE_HEADER Bytecount REP_BYTES_BY_FIRST_BYTE_1 (int fb, const char *file,
						   int line);
INLINE_HEADER Bytecount
REP_BYTES_BY_FIRST_BYTE_1 (int fb, const char *file, int line)
{
  assert_at_line (fb < 0xA0, file, line);
  return rep_bytes_by_first_byte[fb];
}

#define REP_BYTES_BY_FIRST_BYTE(fb) \
  REP_BYTES_BY_FIRST_BYTE_1 (fb, __FILE__, __LINE__) 

#else /* ERROR_CHECK_TEXT */

#define REP_BYTES_BY_FIRST_BYTE(fb) (rep_bytes_by_first_byte[fb])

#endif /* ERROR_CHECK_TEXT */

/* Is this character represented by more than one byte in a string? */

#define CHAR_MULTIBYTE_P(c) ((c) >= 0x80)

#define CHAR_ASCII_P(c) (!CHAR_MULTIBYTE_P (c))

#define MAX_EMCHAR_LEN 4

#endif /* MULE */

int dfc_coding_system_is_unicode (Lisp_Object coding_system);

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

/* NOTE: In all the following macros, we follow these rules concerning
   multiple evaluation of the arguments:

   1) Anything that's an lvalue can be evaluated more than once.
   2) Anything that's a Lisp Object can be evaluated more than once.
      This should probably be changed, but this follows the way
      that all the macros in lisp.h do things.
   3) 'struct buffer *' arguments can be evaluated more than once.
   4) Nothing else can be evaluated more than once.  Use inline
      functions, if necessary, to prevent multiple evaluation.
   5) An exception to (4) is that there are some macros below that
      may evaluate their arguments more than once.  They are all
      denoted with the word "unsafe" in their name and are generally
      meant to be called only by other macros that have already
      stored the calling values in temporary variables.


   Use the following functions/macros on contiguous strings of data.
   If the text you're operating on is known to come from a buffer, use
   the buffer-level functions below -- they know about the gap and may
   be more efficient.


  ----------------------------------------------------------------------------
     (A) For working with charptr's (pointers to internally-formatted text):
  ----------------------------------------------------------------------------

   VALID_CHARPTR_P (ptr):
	Given a charptr, does it point to the beginning of a character?

   ASSERT_VALID_CHARPTR (ptr):
	If error-checking is enabled, assert that the given charptr
	points to the beginning of a character.	 Otherwise, do nothing.

   INC_CHARPTR (ptr):
	Given a charptr (assumed to point at the beginning of a character),
	modify that pointer so it points to the beginning of the next
	character.

   DEC_CHARPTR (ptr):
	Given a charptr (assumed to point at the beginning of a
	character or at the very end of the text), modify that pointer
	so it points to the beginning of the previous character.

   VALIDATE_CHARPTR_BACKWARD (ptr):
	Make sure that PTR is pointing to the beginning of a character.
	If not, back up until this is the case.	  Note that there are not
	too many places where it is legitimate to do this sort of thing.
	It's an error if you're passed an "invalid" char * pointer.
	NOTE: PTR *must* be pointing to a valid part of the string (i.e.
	not the very end, unless the string is zero-terminated or
	something) in order for this function to not cause crashes.

   VALIDATE_CHARPTR_FORWARD (ptr):
	Make sure that PTR is pointing to the beginning of a character.
	If not, move forward until this is the case.  Note that there
	are not too many places where it is legitimate to do this sort
	of thing.  It's an error if you're passed an "invalid" char *
	pointer.

  ---------------------------------------------------------------------
     (B) For working with the length (in bytes and characters) of a   
         section of internally-formatted text:                        
  ---------------------------------------------------------------------

   bytecount_to_charcount (ptr, nbi):
	Given a pointer to a text string and a length in bytes,
	return the equivalent length in characters.

   charcount_to_bytecount (ptr, nch):
	Given a pointer to a text string and a length in characters,
	return the equivalent length in bytes.

   charptr_n_addr (ptr, n):
	Return a pointer to the beginning of the character offset N
	(in characters) from PTR.

  -------------------------------------------------------------------------
     (C) For retrieving or changing the character pointed to by a charptr:
  -------------------------------------------------------------------------

   charptr_emchar (ptr):
	Retrieve the character pointed to by PTR as an Emchar.

   charptr_emchar_n (ptr, n):
	Retrieve the character at offset N (in characters) from PTR,
	as an Emchar.

   set_charptr_emchar (ptr, ch):
	Store the character CH (an Emchar) as internally-formatted
	text starting at PTR.  Return the number of bytes stored.

   charptr_copy_char (src, dst):
	Retrieve the character pointed to by SRC and store it as
	internally-formatted text in DST.

   ----------------------------------
     (D) For working with Emchars:
   ----------------------------------

   [Note that there are other functions/macros for working with Emchars
    in charset.h, for retrieving the charset of an Emchar and such.]

   valid_char_p (ch):
	Return whether the given Emchar is valid.

   CHARP (ch):
	Return whether the given Lisp_Object is a character.

   CHECK_CHAR_COERCE_INT (ch):
	Signal an error if CH is not a valid character or integer Lisp_Object.
	If CH is an integer Lisp_Object, convert it to a character Lisp_Object,
	but merely by repackaging, without performing tests for char validity.

   MAX_EMCHAR_LEN:
	Maximum number of buffer bytes per Emacs character.
*/

/* ---------------------------------------------------------------------- */
/* (A) For working with charptr's (pointers to internally-formatted text) */
/* ---------------------------------------------------------------------- */

#ifdef MULE
# define VALID_CHARPTR_P(ptr) INTBYTE_FIRST_BYTE_P (* (unsigned char *) ptr)
#else
# define VALID_CHARPTR_P(ptr) 1
#endif

#ifdef ERROR_CHECK_TEXT
# define ASSERT_VALID_CHARPTR(ptr) assert (VALID_CHARPTR_P (ptr))
#else
# define ASSERT_VALID_CHARPTR(ptr)
#endif

/* Note that INC_CHARPTR() and DEC_CHARPTR() have to be written in
   completely separate ways.  INC_CHARPTR() cannot use the DEC_CHARPTR()
   trick of looking for a valid first byte because it might run off
   the end of the string.  DEC_CHARPTR() can't use the INC_CHARPTR()
   method because it doesn't have easy access to the first byte of
   the character it's moving over. */

#define REAL_INC_CHARPTR(ptr) \
  ((void) ((ptr) += REP_BYTES_BY_FIRST_BYTE (* (unsigned char *) (ptr))))

#define REAL_INC_CHARBYTEBPOS(ptr, pos) \
  (pos += REP_BYTES_BY_FIRST_BYTE (* (unsigned char *) (ptr)))

#define REAL_DEC_CHARPTR(ptr) do {	\
  (ptr)--;				\
} while (!VALID_CHARPTR_P (ptr))

#ifdef ERROR_CHECK_TEXT
#define INC_CHARPTR(ptr) do {		\
  ASSERT_VALID_CHARPTR (ptr);		\
  REAL_INC_CHARPTR (ptr);		\
} while (0)

#define INC_CHARBYTEBPOS(ptr, pos) do {		\
  ASSERT_VALID_CHARPTR (ptr);			\
  REAL_INC_CHARBYTEBPOS (ptr, pos);		\
} while (0)

#define DEC_CHARPTR(ptr) do {			\
  const Intbyte *dc_ptr1 = (ptr);		\
  const Intbyte *dc_ptr2 = dc_ptr1;		\
  REAL_DEC_CHARPTR (dc_ptr2);			\
  assert (dc_ptr1 - dc_ptr2 ==			\
	  REP_BYTES_BY_FIRST_BYTE (*dc_ptr2));	\
  (ptr) = (Intbyte *) dc_ptr2;			\
} while (0)

#else /* ! ERROR_CHECK_TEXT */
#define INC_CHARBYTEBPOS(ptr, pos) REAL_INC_CHARBYTEBPOS (ptr, pos)
#define INC_CHARPTR(ptr) REAL_INC_CHARPTR (ptr)
#define DEC_CHARPTR(ptr) REAL_DEC_CHARPTR (ptr)
#endif /* ! ERROR_CHECK_TEXT */

#ifdef MULE

/* Note that this reads the byte at *PTR! */

#define VALIDATE_CHARPTR_BACKWARD(ptr) do {	\
  while (!VALID_CHARPTR_P (ptr)) ptr--;		\
} while (0)

/* Given a Intbyte string at PTR of size N, possibly with a partial
   character at the end, return the size of the longest substring of
   complete characters.  Does not assume that the byte at *(PTR + N) is
   readable. */
DECLARE_INLINE_HEADER (
Bytecount
validate_intbyte_string_backward (const Intbyte *ptr, Bytecount n)
)
{
  const Intbyte *ptr2;

  if (n == 0)
    return n;
  ptr2 = ptr + n - 1;
  VALIDATE_CHARPTR_BACKWARD (ptr2);
  if (ptr2 + REP_BYTES_BY_FIRST_BYTE (*ptr2) != ptr + n)
    return ptr2 - ptr;
  return n;
}

/* This needs to be trickier than VALIDATE_CHARPTR_BACKWARD() to avoid the
   possibility of running off the end of the string. */

#define VALIDATE_CHARPTR_FORWARD(ptr) do {	\
  Intbyte *vcf_ptr = (ptr);			\
  VALIDATE_CHARPTR_BACKWARD (vcf_ptr);		\
  if (vcf_ptr != (ptr))				\
    {						\
      (ptr) = vcf_ptr;				\
      INC_CHARPTR (ptr);			\
    }						\
} while (0)

#else /* not MULE */
#define VALIDATE_CHARPTR_BACKWARD(ptr)
#define VALIDATE_CHARPTR_FORWARD(ptr)
#define validate_intbyte_string_backward(ptr, n) (n)
#endif /* not MULE */

/* -------------------------------------------------------------- */
/* (B) For working with the length (in bytes and characters) of a */
/*     section of internally-formatted text 			  */
/* -------------------------------------------------------------- */

INLINE_HEADER const Intbyte *
charptr_n_addr (const Intbyte *ptr, Charcount offset);
INLINE_HEADER const Intbyte *
charptr_n_addr (const Intbyte *ptr, Charcount offset)
{
  return ptr + charcount_to_bytecount (ptr, offset);
}

/* -------------------------------------------------------------------- */
/* (C) For retrieving or changing the character pointed to by a charptr */
/* -------------------------------------------------------------------- */

#define simple_charptr_emchar(ptr)		((Emchar) (ptr)[0])
#define simple_set_charptr_emchar(ptr, x) \
	((ptr)[0] = (Intbyte) (x), (Bytecount) 1)
#define simple_charptr_copy_char(src, dst) \
	((dst)[0] = *(src), (Bytecount) 1)

#ifdef MULE

Emchar non_ascii_charptr_emchar (const Intbyte *ptr);
Bytecount non_ascii_set_charptr_emchar (Intbyte *ptr, Emchar c);
Bytecount non_ascii_charptr_copy_char (const Intbyte *src, Intbyte *dst);

INLINE_HEADER Emchar charptr_emchar (const Intbyte *ptr);
INLINE_HEADER Emchar
charptr_emchar (const Intbyte *ptr)
{
  return BYTE_ASCII_P (*ptr) ?
    simple_charptr_emchar (ptr) :
    non_ascii_charptr_emchar (ptr);
}

INLINE_HEADER Bytecount set_charptr_emchar (Intbyte *ptr, Emchar x);
INLINE_HEADER Bytecount
set_charptr_emchar (Intbyte *ptr, Emchar x)
{
  return !CHAR_MULTIBYTE_P (x) ?
    simple_set_charptr_emchar (ptr, x) :
    non_ascii_set_charptr_emchar (ptr, x);
}

INLINE_HEADER Bytecount
charptr_copy_char (const Intbyte *src, Intbyte *dst);
INLINE_HEADER Bytecount
charptr_copy_char (const Intbyte *src, Intbyte *dst)
{
  return BYTE_ASCII_P (*src) ?
    simple_charptr_copy_char (src, dst) :
    non_ascii_charptr_copy_char (src, dst);
}

#else /* not MULE */

# define charptr_emchar(ptr)		simple_charptr_emchar (ptr)
# define set_charptr_emchar(ptr, x)	simple_set_charptr_emchar (ptr, x)
# define charptr_copy_char(src, dst)	simple_charptr_copy_char (src, dst)

#endif /* not MULE */

#define charptr_emchar_n(ptr, offset) \
  charptr_emchar (charptr_n_addr (ptr, offset))


/* ---------------------------- */
/* (D) For working with Emchars */
/* ---------------------------- */

#ifdef MULE

int non_ascii_valid_char_p (Emchar ch);

INLINE_HEADER int valid_char_p (Emchar ch);
INLINE_HEADER int
valid_char_p (Emchar ch)
{
  return (! (ch & ~0xFF)) || non_ascii_valid_char_p (ch);
}

#else /* not MULE */

#define valid_char_p(ch) (! (ch & ~0xFF))

#endif /* not MULE */

#define CHAR_INTP(x) (INTP (x) && valid_char_p (XINT (x)))

#define CHAR_OR_CHAR_INTP(x) (CHARP (x) || CHAR_INTP (x))

INLINE_HEADER Emchar XCHAR_OR_CHAR_INT (Lisp_Object obj);
INLINE_HEADER Emchar
XCHAR_OR_CHAR_INT (Lisp_Object obj)
{
  return CHARP (obj) ? XCHAR (obj) : XINT (obj);
}

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
   (c) it comes in an alloca() variety (all allocation is stack-local,
       so there is no need to explicitly clean up) as well as a malloc()
       variety
   (d) it knows its own length, so it does not suffer from standard null
       byte brain-damage -- but it null-terminates the data anyway, so
       it can be passed to standard routines
   (e) it provides a much more powerful set of operations and knows about
       all the standard places where string data might reside: Lisp_Objects,
       other Eistrings, Intbyte * data with or without an explicit length,
       ASCII strings, Emchars, etc.
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
        Declare a new Eistring.  This is a standard local variable declaration
        and can go anywhere in the variable declaration section.  NAME itself
        is declared as an Eistring *, and its storage declared on the stack.

   DECLARE_EISTRING_MALLOC (name);
        Declare a new Eistring, which uses malloc()ed instead of alloca()ed
        data.  This is a standard local variable declaration and can go
        anywhere in the variable declaration section.  Once you initialize
	the Eistring, you will have to free it using eifree() to avoid
	memory leaks.  You will need to use this form if you are passing
	an Eistring to any function that modifies it (otherwise, the
	modified data may be in stack space and get overwritten when the
	function returns).

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
   void eicpy_ch (Eistring *eistr, Emchar ch);
        ... from an Emchar (this can be a conventional C character).

   void eicpy_lstr_off (Eistring *eistr, Lisp_Object lisp_string,
                        Bytecount off, Charcount charoff,
                        Bytecount len, Charcount charlen);
        ... from a section of a Lisp_Object string.
   void eicpy_lbuf (Eistring *eistr, Lisp_Object lisp_buf,
		    Bytecount off, Charcount charoff,
		    Bytecount len, Charcount charlen);
        ... from a section of a Lisp_Object buffer.
   void eicpy_raw (Eistring *eistr, const Intbyte *data, Bytecount len);
        ... from raw internal-format data in the default internal format.
   void eicpy_rawz (Eistring *eistr, const Intbyte *data);
        ... from raw internal-format data in the default internal format
        that is "null-terminated" (the meaning of this depends on the nature
        of the default internal format).
   void eicpy_raw_fmt (Eistring *eistr, const Intbyte *data, Bytecount len,
                       Internal_Format intfmt);
        ... from raw internal-format data in the specified format.
   void eicpy_rawz_fmt (Eistring *eistr, const Intbyte *data,
                        Internal_Format intfmt);
        ... from raw internal-format data in the specified format that is
        "null-terminated" (the meaning of this depends on the nature of
        the specific format).
   void eicpy_c (Eistring *eistr, const Char_ASCII *c_string);
        ... from an ASCII null-terminated string.  Non-ASCII characters in
	the string are *ILLEGAL* (read abort() with error-checking defined).
   void eicpy_c_len (Eistring *eistr, const Char_ASCII *c_string, len);
        ... from an ASCII string, with length specified.  Non-ASCII characters
	in the string are *ILLEGAL* (read abort() with error-checking defined).
   void eicpy_ext (Eistring *eistr, const Extbyte *extdata,
                   Lisp_Object coding_system);
        ... from external null-terminated data, with coding system specified.
   void eicpy_ext_len (Eistring *eistr, const Extbyte *extdata,
                       Bytecount extlen, Lisp_Object coding_system);
        ... from external data, with length and coding system specified.
   void eicpy_lstream (Eistring *eistr, Lisp_Object lstream);
        ... from an lstream; reads data till eof.  Data must be in default
        internal format; otherwise, interpose a decoding lstream.


    ********************************************** 
    *    Getting the data out of the Eistring    * 
    ********************************************** 

   Intbyte *eidata (Eistring *eistr);
        Return a pointer to the raw data in an Eistring.  This is NOT
        a copy.

   Lisp_Object eimake_string (Eistring *eistr);
        Make a Lisp string out of the Eistring.

   Lisp_Object eimake_string_off (Eistring *eistr,
                                  Bytecount off, Charcount charoff,
				  Bytecount len, Charcount charlen);
        Make a Lisp string out of a section of the Eistring.

   void eicpyout_alloca (Eistring *eistr, LVALUE: Intbyte *ptr_out,
                         LVALUE: Bytecount len_out);
        Make an alloca() copy of the data in the Eistring, using the
        default internal format.  Due to the nature of alloca(), this
        must be a macro, with all lvalues passed in as parameters.
	(More specifically, not all compilers correctly handle using
	alloca() as the argument to a function call -- GCC on x86
	didn't used to, for example.) A pointer to the alloca()ed data
	is stored in PTR_OUT, and the length of the data (not including
	the terminating zero) is stored in LEN_OUT.

   void eicpyout_alloca_fmt (Eistring *eistr, LVALUE: Intbyte *ptr_out,
                             LVALUE: Bytecount len_out,
                             Internal_Format intfmt);
        Like eicpyout_alloca(), but converts to the specified internal
        format. (No formats other than FORMAT_DEFAULT are currently
        implemented, and you get an assertion failure if you try.)

   Intbyte *eicpyout_malloc (Eistring *eistr, Bytecount *intlen_out);
        Make a malloc() copy of the data in the Eistring, using the
        default internal format.  This is a real function.  No lvalues
        passed in.  Returns the new data, and stores the length (not
        including the terminating zero) using INTLEN_OUT, unless it's
        a NULL pointer.

   Intbyte *eicpyout_malloc_fmt (Eistring *eistr, Internal_Format intfmt,
                                 Bytecount *intlen_out);
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

   Emchar eigetch (Eistring *eistr, Bytecount bytepos);
        Return the character at a particular byte offset.
   Emchar eigetch_char (Eistring *eistr, Charcount charpos);
        Return the character at a particular character offset.


    ********************************************** 
    *    Setting the character at a position     * 
    ********************************************** 

   Emchar eisetch (Eistring *eistr, Bytecount bytepos, Emchar chr);
        Set the character at a particular byte offset.
   Emchar eisetch_char (Eistring *eistr, Charcount charpos, Emchar chr);
        Set the character at a particular character offset.


    ********************************************** 
    *               Concatenation                * 
    ********************************************** 

   void eicat_* (Eistring *eistr, ...);
        Concatenate onto the end of the Eistring, with data coming from the
	same places as above:

   void eicat_ei (Eistring *eistr, Eistring *eistr2);
        ... from another Eistring.
   void eicat_c (Eistring *eistr, Char_ASCII *c_string);
        ... from an ASCII null-terminated string.  Non-ASCII characters in
	the string are *ILLEGAL* (read abort() with error-checking defined).
   void eicat_raw (ei, const Intbyte *data, Bytecount len);
        ... from raw internal-format data in the default internal format.
   void eicat_rawz (ei, const Intbyte *data);
        ... from raw internal-format data in the default internal format
        that is "null-terminated" (the meaning of this depends on the nature
        of the default internal format).
   void eicat_lstr (ei, Lisp_Object lisp_string);
        ... from a Lisp_Object string.
   void eicat_ch (ei, Emchar ch);
        ... from an Emchar.

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
		 Bytecount len, Charcount charlen, Char_ASCII *c_string);
        ... with an ASCII null-terminated string.  Non-ASCII characters in
	the string are *ILLEGAL* (read abort() with error-checking defined).
   void eisub_ch (Eistring *eistr, Bytecount off, Charcount charoff,
		  Bytecount len, Charcount charlen, Emchar ch);
        ... with an Emchar.

   void eidel (Eistring *eistr, Bytecount off, Charcount charoff,
	       Bytecount len, Charcount charlen);
        Delete a section of the Eistring.


    ********************************************** 
    *      Converting to an external format      * 
    ********************************************** 

   void eito_external (Eistring *eistr, Lisp_Object coding_system);
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

   Bytecount eichr (Eistring *eistr, Emchar chr);
   Charcount eichr_char (Eistring *eistr, Emchar chr);
   Bytecount eichr_off (Eistring *eistr, Emchar chr, Bytecount off,
			Charcount charoff);
   Charcount eichr_off_char (Eistring *eistr, Emchar chr, Bytecount off,
			     Charcount charoff);
   Bytecount eirchr (Eistring *eistr, Emchar chr);
   Charcount eirchr_char (Eistring *eistr, Emchar chr);
   Bytecount eirchr_off (Eistring *eistr, Emchar chr, Bytecount off,
			 Charcount charoff);
   Charcount eirchr_off_char (Eistring *eistr, Emchar chr, Bytecount off,
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

   Bytecount eistr_c (Eistring *eistr, Char_ASCII *c_string);
   Charcount eistr_c_char (Eistring *eistr, Char_ASCII *c_string);
   Bytecount eistr_c_off (Eistring *eistr, Char_ASCII *c_string, Bytecount off,
			   Charcount charoff);
   Charcount eistr_c_off_char (Eistring *eistr, Char_ASCII *c_string,
			       Bytecount off, Charcount charoff);
   Bytecount eirstr_c (Eistring *eistr, Char_ASCII *c_string);
   Charcount eirstr_c_char (Eistring *eistr, Char_ASCII *c_string);
   Bytecount eirstr_c_off (Eistring *eistr, Char_ASCII *c_string,
			   Bytecount off, Charcount charoff);
   Charcount eirstr_c_off_char (Eistring *eistr, Char_ASCII *c_string,
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

   int eicmp_c (Eistring *eistr, Char_ASCII *c_string);
   int eicmp_off_c (Eistring *eistr, Bytecount off, Charcount charoff,
                    Bytecount len, Charcount charlen, Char_ASCII *c_string);
   int eicasecmp_c (Eistring *eistr, Char_ASCII *c_string);
   int eicasecmp_off_c (Eistring *eistr, Bytecount off, Charcount charoff,
                        Bytecount len, Charcount charlen,
                        Char_ASCII *c_string);
   int eicasecmp_i18n_c (Eistring *eistr, Char_ASCII *c_string);
   int eicasecmp_i18n_off_c (Eistring *eistr, Bytecount off, Charcount charoff,
                             Bytecount len, Charcount charlen,
                             Char_ASCII *c_string);


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
       as macros, because of the use of alloca().  The principle used
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
       and ASCII data, and Emchars if the particular operation makes
       sense with a character.  The other provides interfaces for the
       most commonly-used forms -- Eistring, ASCII data, Lisp string,
       raw internal-format string with length, raw internal-format
       string without, and possibly Emchar. (In the function names,
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
       they use alloca(), so we can't (and we can't make them inline
       functions either -- alloca() is explicitly disallowed in inline
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
  Intbyte *data_;
  /* Total number of bytes allocated in DATA (including null). */
  Bytecount max_size_allocated_;
  Bytecount bytelen_;
  Charcount charlen_;
  int mallocp_;

  Extbyte *extdata_;
  Bytecount extlen_;
} Eistring;

typedef enum internal_format
{
  FORMAT_DEFAULT,
  FORMAT_FIXED_8,
  FORMAT_FIXED_16,
  FORMAT_FIXED_32
} Internal_Format;

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
	    (ei)->data_ = (Intbyte *) xrealloc ((ei)->data_, ei1newsize);     \
	  else								      \
	    {								      \
	      /* We don't have realloc, so alloca() more space and copy the   \
		 data into it. */					      \
	      Intbyte *ei1oldeidata = (ei)->data_;			      \
	      (ei)->data_ = (Intbyte *) alloca (ei1newsize);		      \
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

#ifdef ERROR_CHECK_TEXT
#define EI_ASSERT_ASCII(ptr, len)			\
do {							\
  int ei5;						\
  const Char_ASCII *ei5ptr = (ptr);			\
  int ei5len = (len);					\
							\
  for (ei5 = 0; ei5 < ei5len; ei5++)			\
    assert (ei5ptr[ei5] >= 0x00 && ei5ptr[ei5] < 0x7F);	\
} while (0)
#define EI_ASSERT_ASCIIZ(ptr)			\
do {						\
  const Char_ASCII *ei5p1 = (ptr);		\
  EI_ASSERT_ASCII (ei5p1, strlen (ei5p1));	\
} while (0)
#else
#define EI_ASSERT_ASCII(ptr, len)
#define EI_ASSERT_ASCIIZ(ptr)
#endif


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
		     XSTRING_CHAR_LENGTH (ei3));			\
} while (0)

#define eicpy_lstr_off(ei, lisp_string, off, charoff, len, charlen)	\
do {									\
  Lisp_Object ei23lstr = (lisp_string);					\
  int ei23off = (off);							\
  int ei23charoff = (charoff);						\
  int ei23len = (len);							\
  int ei23charlen = (charlen);						\
  const Intbyte *ei23data = XSTRING_DATA (ei23lstr);			\
									\
  int ei23oldbytelen = (ei)->bytelen_;					\
									\
  eifixup_byte (ei23data, ei23off, ei23charoff);			\
  eifixup_bytechar (ei23data + ei23off, ei23len, ei23charlen);		\
									\
  EI_ALLOC_AND_COPY (ei, ei23data + ei23off, ei23len, ei23charlen);	\
} while (0)

#define eicpy_raw_fmt(ei, ptr, len, fmt)				\
do {									\
  const Intbyte *ei12ptr = (ptr);					\
  Internal_Format ei12fmt = (fmt);					\
  int ei12len = (len);							\
  assert (ei12fmt == FORMAT_DEFAULT);					\
  EI_ALLOC_AND_COPY (ei, ei12ptr, ei12len,				\
		     bytecount_to_charcount (ei12ptr, ei12len));	\
} while (0)

#define eicpy_raw(ei, ptr, len) eicpy_raw_fmt (ei, ptr, len, FORMAT_DEFAULT)

#define eicpy_rawz_fmt(ei, ptr, fmt)				\
do {								\
  const Intbyte *ei12p1ptr = (ptr);				\
  Internal_Format ei12p1fmt = (fmt);				\
  assert (ei12p1fmt == FORMAT_DEFAULT);				\
  eicpy_raw_fmt (ei, ei12p1ptr, qxestrlen (ei12p1ptr), fmt);	\
} while (0)

#define eicpy_rawz(ei, ptr) eicpy_rawz_fmt (ei, ptr, FORMAT_DEFAULT)

#define eicpy_ch(ei, ch)					\
do {								\
  Intbyte ei12p2[MAX_EMCHAR_LEN];				\
  Bytecount ei12p2len = set_charptr_emchar (ei12p2, ch);	\
  EI_ALLOC_AND_COPY (ei, ei12p2, ei12p2len, 1);			\
} while (0)

#define eicpy_c(ei, c_string)			\
do {						\
  const Char_ASCII *ei4 = (c_string);		\
						\
  EI_ASSERT_ASCIIZ (ei4);			\
  eicpy_ext (ei, ei4, Qbinary);			\
} while (0)

#define eicpy_c_len(ei, c_string, c_len)	\
do {						\
  const Char_ASCII *ei6 = (c_string);		\
  int ei6len = (c_len);				\
						\
  EI_ASSERT_ASCII (ei6, ei6len);		\
  eicpy_ext_len (ei, ei6, ei6len, Qbinary);	\
} while (0)

#define eicpy_ext_len(ei, extdata, extlen, coding_system)		 \
do {									 \
  const Extbyte *ei7 = (extdata);					 \
  int ei7len = (extlen);						 \
									 \
  TO_INTERNAL_FORMAT (DATA, (ei7, ei7len),				 \
		      ALLOCA, ((ei)->data_, (ei)->bytelen_),		 \
		      coding_system);					 \
  (ei)->max_size_allocated_ = (ei)->bytelen_ + 1;			 \
  (ei)->charlen_ = bytecount_to_charcount ((ei)->data_, (ei)->bytelen_); \
} while (0)

#define eicpy_ext(ei, extdata, coding_system)				\
do {									\
  const Extbyte *ei8 = (extdata);					\
									\
  eicpy_ext_len (ei, ei8, dfc_external_data_len (ei8, coding_system),	\
		 coding_system);					\
} while (0)

#define eicpy_lbuf(eistr, lisp_buf, off, charoff, len, charlen) \
  NOT YET IMPLEMENTED

#define eicpy_lstream(eistr, lstream) \
  NOT YET IMPLEMENTED

#define eireset(eistr) eicpy_rawz (eistr, (Intbyte *) "")

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
  eicpyout_alloca_fmt (eistr, ptrout, lenout, FORMAT_DEFAULT)
#define eicpyout_malloc(eistr, lenout) \
  eicpyout_malloc_fmt (eistr, lenout, FORMAT_DEFAULT)
Intbyte *eicpyout_malloc_fmt (Eistring *eistr, Bytecount *len_out,
			      Internal_Format fmt);
#define eicpyout_alloca_fmt(eistr, ptrout, lenout, fmt)		\
do {								\
  Internal_Format ei23fmt = (fmt);				\
  Intbyte *ei23ptrout = &(ptrout);				\
  Bytecount *ei23lenout = &(lenout);				\
								\
  assert (ei23fmt == FORMAT_DEFAULT);				\
								\
  *ei23lenout = (eistr)->bytelen_;				\
  *ei23ptrout = alloca_array (Intbyte, (eistr)->bytelen_ + 1);	\
  memcpy (*ei23ptrout, (eistr)->data_, (eistr)->bytelen_ + 1);	\
} while (0)

/*   ----- Moving to the heap -----   */

#define eifree(ei)				\
do {						\
  if ((ei)->mallocp_)				\
    {						\
      if ((ei)->data_)				\
	xfree ((ei)->data_);			\
      if ((ei)->extdata_)			\
	xfree ((ei)->extdata_);			\
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
      Intbyte *ei13newdata;						\
									\
      (ei)->max_size_allocated_ =					\
	eifind_large_enough_buffer (0, (ei)->bytelen_ + 1);		\
      ei13newdata = (Intbyte *) alloca ((ei)->max_size_allocated_);	\
      memcpy (ei13newdata, (ei)->data_, (ei)->bytelen_ + 1);		\
      xfree ((ei)->data_);						\
      (ei)->data_ = ei13newdata;					\
    }									\
									\
  if ((ei)->extdata_)							\
    {									\
      Extbyte *ei13newdata = (Extbyte *) alloca ((ei)->extlen_ + 2);	\
									\
      memcpy (ei13newdata, (ei)->extdata_, (ei)->extlen_);		\
      /* Double null-terminate in case of Unicode data */		\
      ei13newdata[(ei)->extlen_] = '\0';				\
      ei13newdata[(ei)->extlen_ + 1] = '\0';				\
      xfree ((ei)->extdata_);						\
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
  Intbyte *pos = eistr->data_ + bytepos;
  Charcount i;

  text_checking_assert (bytepos >= 0 && bytepos <= eistr->bytelen_);
  text_checking_assert (n >= 0 && n <= eistr->charlen_);
  /* We could check N more correctly now, but that would require a
     call to bytecount_to_charcount(), which would be needlessly
     expensive (it would convert O(N) algorithms into O(N^2) algorithms
     with ERROR_CHECK_TEXT, which would be bad).  If N is bad, we are
     guaranteed to catch it either inside INC_CHARPTR() or in the check
     below. */
  for (i = 0; i < n; i++)
    INC_CHARPTR (pos);
  text_checking_assert (pos - eistr->data_ <= eistr->bytelen_);
  return pos - eistr->data_;
}

#define eiincpos (ei, bytepos) eiincpos_1 (ei, bytepos, 1)
#define eiincpos_n (ei, bytepos, n) eiincpos_1 (ei, bytepos, n)

DECLARE_INLINE_HEADER (Bytecount eidecpos_1 (Eistring *eistr,
					     Bytecount bytepos,
					     Charcount n))
{
  Intbyte *pos = eistr->data_ + bytepos;
  int i;

  text_checking_assert (bytepos >= 0 && bytepos <= eistr->bytelen_);
  text_checking_assert (n >= 0 && n <= eistr->charlen_);
  /* We could check N more correctly now, but ...  see above. */
  for (i = 0; i < n; i++)
    DEC_CHARPTR (pos);
  text_checking_assert (pos - eistr->data_ <= eistr->bytelen_);
  return pos - eistr->data_;
}

#define eidecpos (ei, bytepos) eidecpos_1 (ei, bytepos, 1)
#define eidecpos_n (ei, bytepos, n) eidecpos_1 (ei, bytepos, n)


/*   ----- Getting the character at a position -----   */

#define eigetch(ei, bytepos) \
  charptr_emchar ((ei)->data_ + (bytepos))
#define eigetch_char(ei, charpos) charptr_emchar_n ((ei)->data_, charpos)


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
  const Char_ASCII *ei15 = (c_string);				\
  int ei15len = strlen (ei15);					\
								\
  EI_ASSERT_ASCII (ei15, ei15len);				\
  eicat_1 (ei, ei15, ei15len,					\
           bytecount_to_charcount ((Intbyte *) ei15, ei15len));	\
} while (0)

#define eicat_raw(ei, data, len)			\
do {							\
  int ei16len = (len);					\
  const Intbyte *ei16data = (data);			\
  eicat_1 (ei, ei16data, ei16len,			\
           bytecount_to_charcount (ei16data, ei16len));	\
} while (0)

#define eicat_rawz(ei, ptr)				\
do {							\
  const Intbyte *ei16p5ptr = (ptr);			\
  eicat_raw (ei, ei16p5ptr, qxestrlen (ei16p5ptr));	\
} while (0)

#define eicat_lstr(ei, lisp_string)				\
do {								\
  Lisp_Object ei17 = (lisp_string);				\
  eicat_1 (ei, XSTRING_DATA (ei17), XSTRING_LENGTH (ei17),	\
	   XSTRING_CHAR_LENGTH (ei17));				\
} while (0)

#define eicat_ch(ei, ch)				\
do {							\
  Intbyte ei22ch[MAX_EMCHAR_LEN];			\
  Bytecount ei22len = set_charptr_emchar (ei22ch, ch);	\
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
  Intbyte *ei18src = (Intbyte *) (src);					 \
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
  const Eistring *ei19 = (ei2);					\
  eisub_1 (ei, off, charoff, len, charlen, ei19->data_, ei19->bytelen_,	\
	   ei19->charlen_);						\
} while (0)

#define eisub_c(ei, off, charoff, len, charlen, c_string)	\
do {								\
  const Char_ASCII *ei20 = (c_string);				\
  int ei20len = strlen (ei20);					\
  EI_ASSERT_ASCII (ei20, ei20len);				\
  eisub_1 (ei, off, charoff, len, charlen, ei20, ei20len, -1);	\
} while (0)

#define eisub_ch(ei, off, charoff, len, charlen, ch)		\
do {								\
  Intbyte ei21ch[MAX_EMCHAR_LEN];				\
  Bytecount ei21len = set_charptr_emchar (ei21ch, ch);		\
  eisub_1 (ei, off, charoff, len, charlen, ei21ch, ei21len, 1);	\
} while (0)

#define eidel(ei, off, charoff, len, charlen)		\
  eisub_1(ei, off, charoff, len, charlen, NULL, 0, 0)


/*   ----- Converting to an external format -----   */

#define eito_external(ei, coding_system)				\
do {									\
  if ((ei)->mallocp_)							\
    {									\
      if ((ei)->extdata_)						\
	{								\
	  xfree ((ei)->extdata_);					\
	  (ei)->extdata_ = 0;						\
	}								\
      TO_EXTERNAL_FORMAT (DATA, ((ei)->data_, (ei)->bytelen_),		\
			  MALLOC, ((ei)->extdata_, (ei)->extlen_),	\
			  coding_system);				\
    }									\
  else									\
    TO_EXTERNAL_FORMAT (DATA, ((ei)->data_, (ei)->bytelen_),		\
			ALLOCA, ((ei)->extdata_, (ei)->extlen_),	\
			coding_system);					\
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
	     Bytecount len, Charcount charlen, const Intbyte *data,
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

int eistr_casefiddle_1 (Intbyte *olddata, Bytecount len, Intbyte *newdata,
			int downp);

#define EI_CASECHANGE(ei, downp)					\
do {									\
  int ei11new_allocmax = (ei)->charlen_ * MAX_EMCHAR_LEN + 1;		\
  Intbyte *ei11storage = (Intbyte *) alloca_array (Intbyte,		\
						   ei11new_allocmax);	\
  int ei11newlen = eistr_casefiddle_1 ((ei)->data_, (ei)->bytelen_,	\
				       ei11storage, downp);		\
									\
  if (ei11newlen)							\
    {									\
      (ei)->max_size_allocated_ = ei11new_allocmax;			\
      (ei)->data_ = ei11storage;						\
      (ei)->bytelen_ = ei11newlen;					\
      /* charlen is the same. */					\
    }									\
} while (0)

#define eilwr(ei) EI_CASECHANGE (ei, 1)
#define eiupr(ei) EI_CASECHANGE (ei, 0)


/************************************************************************/
/*                                                                      */
/*         Converting between internal and external format              */
/*                                                                      */
/************************************************************************/
/*
  All client code should use only the two macros

  TO_EXTERNAL_FORMAT (source_type, source, sink_type, sink, coding_system)
  TO_INTERNAL_FORMAT (source_type, source, sink_type, sink, coding_system)

  Typical use is

  TO_EXTERNAL_FORMAT (DATA, (ptr, len),
                      LISP_BUFFER, buffer,
		      Qfile_name);

  NOTE: GC is inhibited during the entire operation of these macros.  This
  is because frequently the data to be converted comes from strings but
  gets passed in as just DATA, and GC may move around the string data.  If
  we didn't inhibit GC, there'd have to be a lot of messy recoding,
  alloca-copying of strings and other annoying stuff.
		      
  The source or sink can be specified in one of these ways:

  DATA,   (ptr, len),    // input data is a fixed buffer of size len
  ALLOCA, (ptr, len),    // output data is in a alloca()ed buffer of size len
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
  character.  */

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

/* WARNING: These use a static buffer.  This can lead to disaster if
   these functions are not used *very* carefully.  Another reason to only use
   TO_EXTERNAL_FORMAT() and TO_INTERNAL_FORMAT(). */
void
dfc_convert_to_external_format (dfc_conversion_type source_type,
				dfc_conversion_data *source,
				Lisp_Object coding_system,
				dfc_conversion_type sink_type,
				dfc_conversion_data *sink);
void
dfc_convert_to_internal_format (dfc_conversion_type source_type,
				dfc_conversion_data *source,
				Lisp_Object coding_system,
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
  void * dfc_sink_ret = alloca (dfc_sink.data.len + 2);			\
  memcpy (dfc_sink_ret, dfc_sink.data.ptr, dfc_sink.data.len + 2);	\
  ((dfc_aliasing_voidpp) &(DFC_CPP_CAR sink))->p = dfc_sink_ret;	\
  (DFC_CPP_CDR sink) = dfc_sink.data.len;				\
} while (0)
#define DFC_MALLOC_USE_CONVERTED_DATA(sink) do {			\
  void * dfc_sink_ret = xmalloc (dfc_sink.data.len + 2);		\
  memcpy (dfc_sink_ret, dfc_sink.data.ptr, dfc_sink.data.len + 2);	\
  ((dfc_aliasing_voidpp) &(DFC_CPP_CAR sink))->p = dfc_sink_ret;	\
  (DFC_CPP_CDR sink) = dfc_sink.data.len;				\
} while (0)
#define DFC_C_STRING_ALLOCA_USE_CONVERTED_DATA(sink) do {		\
  void * dfc_sink_ret = alloca (dfc_sink.data.len + 2);			\
  memcpy (dfc_sink_ret, dfc_sink.data.ptr, dfc_sink.data.len + 2);	\
  ((dfc_aliasing_voidpp) &(sink))->p = dfc_sink_ret;			\
} while (0)
#define DFC_C_STRING_MALLOC_USE_CONVERTED_DATA(sink) do {		\
  void * dfc_sink_ret = xmalloc (dfc_sink.data.len + 2);		\
  memcpy (dfc_sink_ret, dfc_sink.data.ptr, dfc_sink.data.len + 2);	\
  ((dfc_aliasing_voidpp) &(sink))->p = dfc_sink_ret;			\
} while (0)
#define DFC_LISP_STRING_USE_CONVERTED_DATA(sink) \
  sink = make_string ((Intbyte *) dfc_sink.data.ptr, dfc_sink.data.len)
#define DFC_LISP_OPAQUE_USE_CONVERTED_DATA(sink) \
  sink = make_opaque (dfc_sink.data.ptr, dfc_sink.data.len)
#define DFC_LISP_LSTREAM_USE_CONVERTED_DATA(sink) /* data already used */
#define DFC_LISP_BUFFER_USE_CONVERTED_DATA(sink) \
  Lstream_delete (XLSTREAM (dfc_sink.lisp_object))

/* Convenience macros for extremely common invocations */
#define C_STRING_TO_EXTERNAL(in, out, coding_system) \
  TO_EXTERNAL_FORMAT (C_STRING, in, C_STRING_ALLOCA, out, coding_system)
#define C_STRING_TO_EXTERNAL_MALLOC(in, out, coding_system) \
  TO_EXTERNAL_FORMAT (C_STRING, in, C_STRING_MALLOC, out, coding_system)
#define EXTERNAL_TO_C_STRING(in, out, coding_system) \
  TO_INTERNAL_FORMAT (C_STRING, in, C_STRING_ALLOCA, out, coding_system)
#define EXTERNAL_TO_C_STRING_MALLOC(in, out, coding_system) \
  TO_INTERNAL_FORMAT (C_STRING, in, C_STRING_MALLOC, out, coding_system)
#define LISP_STRING_TO_EXTERNAL(in, out, coding_system) \
  TO_EXTERNAL_FORMAT (LISP_STRING, in, C_STRING_ALLOCA, out, coding_system)
#define LISP_STRING_TO_EXTERNAL_MALLOC(in, out, coding_system) \
  TO_EXTERNAL_FORMAT (LISP_STRING, in, C_STRING_MALLOC, out, coding_system)

/* Standins for various encodings, until we know them better */
#define Qcommand_argument_encoding Qnative
#define Qenvironment_variable_encoding Qnative
#define Qunix_host_name_encoding Qnative
#define Qunix_service_name_encoding Qnative
#define Qmswindows_host_name_encoding Qmswindows_multibyte
#define Qmswindows_service_name_encoding Qmswindows_multibyte

/* Standins for various X encodings, until we know them better */

/* !!#### Need to verify the encoding used in lwlib -- Qnative or Qctext?
   Almost certainly the former.  Use a standin for now. */
#define Qlwlib_encoding Qnative

#define Qx_atom_name_encoding Qctext
/* font names are often stored in atoms, so it gets sticky if we set this
   to something different from atom-name encoding */
#define Qx_font_name_encoding Qctext

#define Qx_color_name_encoding Qctext

/* the following probably must agree with Qcommand_argument_encoding and
   Qenvironment_variable_encoding */
#define Qx_display_name_encoding Qnative

#define Qstrerror_encoding Qnative

#define GET_STRERROR(var, num)					\
do {								\
  int __gsnum__ = (num);					\
  Extbyte * __gserr__ = strerror (__gsnum__);			\
								\
  if (!__gserr__)						\
    {								\
      var = alloca_intbytes (99);				\
      qxesprintf (var, "Unknown error %d", __gsnum__);		\
    }								\
  else								\
    EXTERNAL_TO_C_STRING (__gserr__, var, Qstrerror_encoding);	\
} while (0)

/************************************************************************/
/*		Lisp string representation convenience functions	*/
/************************************************************************/

/* Because the representation of internally formatted data is subject
   to change, it's bad style to do something like

   strcmp (XSTRING_DATA (s), "foo")

   Instead, use the portable:

   intbyte_strcmp (XSTRING_DATA (s), "foo")          or
   intbyte_memcmp (XSTRING_DATA (s), "foo", 3)

*/

/* Like strcmp, except first arg points at internally formatted data,
   while the second points at a string of only ASCII chars. */
DECLARE_INLINE_HEADER (
int
intbyte_strcmp (const Intbyte *bp, const char *ascii_string)
)
{
#ifdef MULE
  while (1)
    {
      int diff;
      type_checking_assert (BYTE_ASCII_P (*ascii_string));
      if ((diff = charptr_emchar (bp) - *(Intbyte *) ascii_string) != 0)
	return diff;
      if (*ascii_string == '\0')
	return 0;
      ascii_string++;
      INC_CHARPTR (bp);
    }
#else
  return strcmp ((char *)bp, ascii_string);
#endif
}

/* Like memcmp, except first arg points at internally formatted data,
   while the second points at a string of only ASCII chars. */

DECLARE_INLINE_HEADER (
int
intbyte_memcmp (const Intbyte *bp, const char *ascii_string, Bytecount len)
)
{
#ifdef MULE
  while (len--)
    {
      int diff = charptr_emchar (bp) - *(Intbyte *) ascii_string;
      type_checking_assert (BYTE_ASCII_P (*ascii_string));
      if (diff != 0)
	return diff;
      ascii_string++;
      INC_CHARPTR (bp);
    }
  return 0;
#else
  return memcmp (bp, ascii_string, len);
#endif
}

#endif /* INCLUDED_text_h_ */
