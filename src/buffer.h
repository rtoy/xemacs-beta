/* Header file for the buffer manipulation primitives.
   Copyright (C) 1985, 1986, 1992, 1993, 1994, 1995
   Free Software Foundation, Inc.
   Copyright (C) 1995 Sun Microsystems, Inc.
   Copyright (C) 2001, 2002, 2004, 2010 Ben Wing.

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

   FSF: long ago.
   JWZ: separated out bufslots.h, early in Lemacs.
   Ben Wing: almost completely rewritten for Mule, 19.12.
 */

#ifndef INCLUDED_buffer_h_
#define INCLUDED_buffer_h_

#ifdef MULE
#include "charset.h"
#endif

/************************************************************************/
/*                                                                      */
/*                    definition of Lisp buffer object                  */
/*                                                                      */
/************************************************************************/

/* Note: we keep both Bytebpos and Charbpos versions of some of the
   important buffer positions because they are accessed so much.
   If we didn't do this, we would constantly be invalidating the
   charbpos<->bytebpos cache under Mule.

   Note that under non-Mule, both versions will always be the
   same so we don't really need to keep track of them.  But it
   simplifies the logic to go ahead and do so all the time and
   the memory loss is insignificant. */

/* Formerly, it didn't much matter what went inside the struct buffer_text
   and what went outside it.  Now it does, with the advent of "indirect
   buffers" that share text with another buffer.  An indirect buffer
   shares the same *text* as another buffer, but has its own buffer-local
   variables, its own accessible region, and its own markers and extents.
   (Due to the nature of markers, it doesn't actually matter much whether
   we stick them inside or out of the struct buffer_text -- the user won't
   notice any difference -- but we go ahead and put them outside for
   consistency and overall saneness of algorithm.)

   FSFmacs gets away with not maintaining any "children" pointers from
   a buffer to the indirect buffers that refer to it by putting the
   markers inside of the struct buffer_text, using markers to keep track
   of BEGV and ZV in indirect buffers, and relying on the fact that
   all intervals (text properties and overlays) use markers for their
   start and end points.  We don't do this for extents (markers are
   inefficient anyway and take up space), so we have to maintain
   children pointers.  This is not terribly hard, though, and the
   code to maintain this is just like the code already present in
   extent-parent and extent-children.
   */

#define NUM_CACHED_POSITIONS 50
#define NUM_MOVED_POSITIONS 10

struct buffer_text
{
#ifdef NEW_GC
  struct lrecord_header header;
#endif /* NEW_GC */
  Ibyte *beg;		/* Actual address of buffer contents. */
  Bytebpos gpt;		/* Index of gap in buffer. */
  Charbpos bufgpt;	/* Equivalent as a Charbpos. */
  Bytebpos z;		/* Index of end of buffer. */
  Charbpos bufz;	/* Equivalent as a Charbpos. */
  Bytecount gap_size;/* Size of buffer's gap */
  Bytecount end_gap_size;/* Size of buffer's end gap */
  long modiff;		/* This counts buffer-modification events
			   for this buffer.  It is incremented for
			   each such event, and never otherwise
			   changed.  */
  long save_modiff;	/* Previous value of modiff, as of last
			   time buffer visited or saved a file.  */

#ifdef MULE

#ifdef OLD_BYTE_CHAR
  /* We keep track of a "known" region for very fast access.  This
     information is text-only so it goes here.  We update this at each
     change to the buffer, so if it's entirely ASCII, these will always
     contain the minimum and maximum positions of the buffer. */
  Charbpos mule_bufmin, mule_bufmax;
  Bytebpos mule_bytmin, mule_bytmax;
  int mule_shifter, mule_three_p;
#endif

  /* And we also cache NUM_CACHED_POSITIONS positions for fairly fast
     access near those positions. */
  Charbpos mule_charbpos_cache[NUM_CACHED_POSITIONS];
  Bytebpos mule_bytebpos_cache[NUM_CACHED_POSITIONS];
  int next_cache_pos;

  Charbpos cached_charpos;
  Bytebpos cached_bytepos;

  /* True if all chars fit into one byte;
     == (format == FORMAT_8_BIT_FIXED ||
         (format == FORMAT_DEFAULT && num_ascii_chars == bufz - 1))
     kept around to speed up (slightly?) the byte-char conversion routines. */
  int entirely_one_byte_p;
  /* Number of ASCII chars in buffer (0 - 127) */
  Charcount num_ascii_chars;
  /* Number of chars in buffer that would fit in an 8-bit-fixed buffer. */
  Charcount num_8_bit_fixed_chars;
  /* Number of chars in buffer that would fit in an 16-bit-fixed buffer. */
  Charcount num_16_bit_fixed_chars;
  
  /* Currently we only handle 8 bit fixed and default */
  Internal_Format format;
#endif /* MULE */

  /* Similar to the above, we keep track of positions for which line
     number has last been calculated.  See line-number.c. */
  Lisp_Object line_number_cache;

  /* Change data that goes with the text. */
  struct buffer_text_change_data *changes;
};

#ifdef NEW_GC
typedef struct buffer_text Lisp_Buffer_Text;

DECLARE_LRECORD (buffer_text, Lisp_Buffer_Text);

#define XBUFFER_TEXT(x) \
  XRECORD (x, buffer_text, Lisp_Buffer_Text)
#define wrap_buffer_text(p) wrap_record (p, buffer_text)
#define BUFFER_TEXT_P(x) RECORDP (x, buffer_text)
#define CHECK_BUFFER_TEXT(x) CHECK_RECORD (x, buffer_text)
#define CONCHECK_BUFFER_TEXT(x) CONCHECK_RECORD (x, buffer_text)
#endif /* NEW_GC */


struct buffer
{
  struct LCRECORD_HEADER header;

  /* This structure holds the coordinates of the buffer contents
     in ordinary buffers.  In indirect buffers, this is not used.  */
  struct buffer_text own_text;

  /* This points to the `struct buffer_text' that is used for this buffer.
     In an ordinary buffer, this is the own_text field above.
     In an indirect buffer, this is the own_text field of another buffer.  */
  struct buffer_text *text;

  Bytebpos pt;		/* Position of point in buffer. */
  Charbpos bufpt;		/* Equivalent as a Charbpos. */
  Bytebpos begv;		/* Index of beginning of accessible range. */
  Charbpos bufbegv;	/* Equivalent as a Charbpos. */
  Bytebpos zv;		/* Index of end of accessible range. */
  Charbpos bufzv;		/* Equivalent as a Charbpos. */

  int face_change;	/* This is set when a change in how the text should
			   be displayed (e.g., font, color) is made. */

  /* Whether buffer specific face is specified. */
  int buffer_local_face_property;

  /* change data indicating what portion of the text has changed
     since the last time this was reset.  Used by redisplay.
     Logically we should keep this with the text structure, but
     redisplay resets it for each buffer individually and we don't
     want interference between an indirect buffer and its base
     buffer. */
  struct each_buffer_change_data *changes;

#ifdef REGION_CACHE_NEEDS_WORK
  /* If the long line scan cache is enabled (i.e. the buffer-local
     variable cache-long-line-scans is non-nil), newline_cache
     points to the newline cache, and width_run_cache points to the
     width run cache.

     The newline cache records which stretches of the buffer are
     known *not* to contain newlines, so that they can be skipped
     quickly when we search for newlines.

     The width run cache records which stretches of the buffer are
     known to contain characters whose widths are all the same.  If
     the width run cache maps a character to a value > 0, that value
     is the character's width; if it maps a character to zero, we
     don't know what its width is.  This allows compute_motion to
     process such regions very quickly, using algebra instead of
     inspecting each character.  See also width_table, below.  */
  struct region_cache *newline_cache;
  struct region_cache *width_run_cache;
#endif /* REGION_CACHE_NEEDS_WORK */

  /* The markers that refer to this buffer.  This is actually a single
     marker -- successive elements in its marker `chain' are the other
     markers referring to this buffer */
  Lisp_Marker *markers;

  /* The buffer's extent info.  This is its own type, an extent-info
     object (done this way for ease in marking / finalizing). */
  Lisp_Object extent_info;

  /* The buffer's syntax cache.  This caches a known region where the
     `syntax-table' property is unchanged, for quick lookup in the routines
     that scan a buffer looking for a particular syntax (regex routines,
     parse-partial-sexp, etc.). */
  struct syntax_cache *syntax_cache;
  
  /* ----------------------------------------------------------------- */
  /* All the stuff above this line is the responsibility of insdel.c,
     with some help from marker.c, extents.c and syntax.c.
     All the stuff below this line is the responsibility of buffer.c. */

  /* In an indirect buffer, this points to the base buffer.
     In an ordinary buffer, it is 0.
     We DO mark through this slot. */
  struct buffer *base_buffer;

  /* List of indirect buffers whose base is this buffer.
     If we are an indirect buffer, this will be nil.
     Do NOT mark through this. */
  Lisp_Object indirect_children;

  /* Flags saying which DEFVAR_PER_BUFFER variables
     are local to this buffer.  */
  int local_var_flags;

  /* Set to the modtime of the visited file when read or written.
     -1 means visited file was nonexistent.
     0  means visited file modtime unknown; in no case complain
     about any mismatch on next save attempt.  */
  int modtime;

  /* the value of text->modiff at the last auto-save.  */
  long auto_save_modified;

  /* The time at which we detected a failure to auto-save,
     Or -1 if we didn't have a failure.  */
  int auto_save_failure_time;

  /* Position in buffer at which display started
     the last time this buffer was displayed.  */
  int last_window_start;

  /* Everything from here down must be a Lisp_Object */

#define MARKED_SLOT(x) Lisp_Object x;
#include "bufslots.h"
#undef MARKED_SLOT
};

DECLARE_LRECORD (buffer, struct buffer);
#define XBUFFER(x) XRECORD (x, buffer, struct buffer)
#define wrap_buffer(p) wrap_record (p, buffer)
#define BUFFERP(x) RECORDP (x, buffer)
#define CHECK_BUFFER(x) CHECK_RECORD (x, buffer)
#define CONCHECK_BUFFER(x) CONCHECK_RECORD (x, buffer)

#define BUFFER_LIVE_P(b) (!NILP ((b)->name))

#define CHECK_LIVE_BUFFER(x) do {			\
  CHECK_BUFFER (x);					\
  if (!BUFFER_LIVE_P (XBUFFER (x)))			\
    dead_wrong_type_argument (Qbuffer_live_p, (x));	\
} while (0)

#define CONCHECK_LIVE_BUFFER(x) do {			\
  CONCHECK_BUFFER (x);					\
  if (!BUFFER_LIVE_P (XBUFFER (x)))			\
    x = wrong_type_argument (Qbuffer_live_p, (x));	\
} while (0)


#define BUFFER_BASE_BUFFER(b) ((b)->base_buffer ? (b)->base_buffer : (b))

/* Map over buffers sharing the same text as MPS_BUF.  MPS_BUFVAR is a
   variable that gets the buffer values (beginning with the base
   buffer, then the children), and MPS_BUFCONS should be a temporary
   Lisp_Object variable.  */
#define MAP_INDIRECT_BUFFERS(mps_buf, mps_bufvar, mps_bufcons)	\
for (mps_bufcons = Qunbound,					\
     mps_bufvar = BUFFER_BASE_BUFFER (mps_buf);			\
     UNBOUNDP (mps_bufcons) ?					\
	(mps_bufcons = mps_bufvar->indirect_children,		\
	1)							\
       : (!NILP (mps_bufcons)					\
	  && (mps_bufvar = XBUFFER (XCAR (mps_bufcons)), 1)	\
	  && (mps_bufcons = XCDR (mps_bufcons), 1));		\
     )


/* All macros below follow the three golden rules of macros (see text.h),
   with the following exception:

   -- 'struct buffer *' arguments can be evaluated more than once.
*/

/*----------------------------------------------------------------------*/
/*	    Accessor macros for important positions in a buffer		*/
/*----------------------------------------------------------------------*/

/* We put them here because some stuff below wants them before the
   place where we would normally put them. */

/* None of these are lvalues.  Use the settor macros below to change
   the positions. */

/* Beginning of buffer.  */
#define BYTE_BUF_BEG(buf) ((Bytebpos) 1)
#define BUF_BEG(buf) ((Charbpos) 1)

/* Beginning of accessible range of buffer.  */
#define BYTE_BUF_BEGV(buf) ((buf)->begv + 0)
#define BUF_BEGV(buf) ((buf)->bufbegv + 0)

/* End of accessible range of buffer.  */
#define BYTE_BUF_ZV(buf) ((buf)->zv + 0)
#define BUF_ZV(buf) ((buf)->bufzv + 0)

/* End of buffer.  */
#define BYTE_BUF_Z(buf) ((buf)->text->z + 0)
#define BUF_Z(buf) ((buf)->text->bufz + 0)

/* Gap location.  */
#define BYTE_BUF_GPT(buf) ((buf)->text->gpt + 0)
#define BUF_GPT(buf) ((buf)->text->bufgpt + 0)

/* Point. */
#define BYTE_BUF_PT(buf) ((buf)->pt + 0)
#define BUF_PT(buf) ((buf)->bufpt + 0)

/* Internal format of buffer.  */
#ifdef MULE
#define BUF_FORMAT(buf) ((buf)->text->format)
#else
#define BUF_FORMAT(buf) FORMAT_DEFAULT
#endif

/*----------------------------------------------------------------------*/
/*		         Validating byte positions         		*/
/*----------------------------------------------------------------------*/

/* Address of byte at position POS in buffer, no error checking. */
DECLARE_INLINE_HEADER (
Ibyte *
BYTE_BUF_BYTE_ADDRESS_NO_VERIFY (struct buffer *buf, Bytebpos pos)
)
{
  return (buf->text->beg +
	  ((pos >= buf->text->gpt ? (pos + buf->text->gap_size) : pos)
	   - 1));
}

/* Given a byte position, does it point to the beginning of a character?
*/
#ifdef MULE
DECLARE_INLINE_HEADER (
int
VALID_BYTEBPOS_P (struct buffer *buf, Bytebpos x)
)
{
  switch (BUF_FORMAT (buf))
    {
    case FORMAT_DEFAULT:
      return ibyte_first_byte_p (*BYTE_BUF_BYTE_ADDRESS_NO_VERIFY (buf, x));
    case FORMAT_16_BIT_FIXED:
      return ((x - 1) & 1) == 0;
    case FORMAT_32_BIT_FIXED:
      return ((x - 1) & 3) == 0;
    default:
      return 1;
    }
}
#else
# define VALID_BYTEBPOS_P(buf, x) 1
#endif

/* If error-checking is enabled, assert that the given char position is
   within range.  Otherwise, do nothing.
*/
# define ASSERT_VALID_CHARBPOS_UNSAFE(buf, x) do {			\
  text_checking_assert (BUFFER_LIVE_P (buf));				\
  text_checking_assert ((x) >= BUF_BEG (buf) && x <= BUF_Z (buf));	\
} while (0)

/* If error-checking is enabled, assert that the given byte position is
   within range and points to the beginning of a character or to the end of
   the buffer.  Otherwise, do nothing.
*/
# define ASSERT_VALID_BYTEBPOS_UNSAFE(buf, x) do {			 \
  text_checking_assert (BUFFER_LIVE_P (buf));				 \
  text_checking_assert ((x) >= BYTE_BUF_BEG (buf) && x <= BYTE_BUF_Z (buf)); \
  text_checking_assert (VALID_BYTEBPOS_P (buf, x));			 \
} while (0)

/* If error-checking is enabled, assert that the given byte position is
   within range and satisfies ASSERT_VALID_BYTEBPOS() and also does not
   refer to the beginning of the buffer. (i.e. movement backwards is OK.) 
   Otherwise, do nothing.
*/
# define ASSERT_VALID_BYTEBPOS_BACKWARD_UNSAFE(buf, x) do {		\
  text_checking_assert (BUFFER_LIVE_P (buf));				\
  text_checking_assert ((x) > BYTE_BUF_BEG (buf) && x <= BYTE_BUF_Z (buf));	\
  text_checking_assert (VALID_BYTEBPOS_P (buf, x));			\
} while (0)

/* If error-checking is enabled, assert that the given byte position is
   within range and satisfies ASSERT_VALID_BYTEBPOS() and also does not
   refer to the end of the buffer. (i.e. movement forwards is OK.) 
   Otherwise, do nothing.
*/
# define ASSERT_VALID_BYTEBPOS_FORWARD_UNSAFE(buf, x) do {		    \
  text_checking_assert (BUFFER_LIVE_P (buf));				    \
  text_checking_assert ((x) >= BYTE_BUF_BEG (buf) && x < BYTE_BUF_Z (buf)); \
  text_checking_assert (VALID_BYTEBPOS_P (buf, x));			    \
} while (0)

#ifdef MULE
/* Make sure that the given byte position is pointing to the beginning of a
   character.  If not, back up until this is the case.  Note that there are
   not too many places where it is legitimate to do this sort of thing.
   It's an error if you're passed an "invalid" byte position.
*/
# define VALIDATE_BYTEBPOS_BACKWARD(buf, x) do {			\
  switch (BUF_FORMAT (buf))						\
    {									\
    case FORMAT_DEFAULT:						\
      {									\
	Ibyte *VBB_ptr = BYTE_BUF_BYTE_ADDRESS_NO_VERIFY (buf, x);	\
	while (!ibyte_first_byte_p (*VBB_ptr))				\
	  VBB_ptr--, (x)--;						\
      }									\
      break;								\
    case FORMAT_16_BIT_FIXED:						\
      if (((x - 1) & 1) != 0)						\
	x--;								\
      break;								\
    case FORMAT_32_BIT_FIXED:						\
      while (((x - 1) & 3) != 0)					\
	x--;								\
      break;								\
    default:								\
      break;								\
    }									\
} while (0)
#else
# define VALIDATE_BYTEBPOS_BACKWARD(buf, x)
#endif

#ifdef MULE
/* Make sure that the given byte position is pointing to the beginning of a
   character.  If not, move forward until this is the case.  Note that
   there are not too many places where it is legitimate to do this sort of
   thing.  It's an error if you're passed an "invalid" byte position.
*/
# define VALIDATE_BYTEBPOS_FORWARD(buf, x) do {				\
  switch (BUF_FORMAT (buf))						\
    {									\
    case FORMAT_DEFAULT:						\
      {									\
	Ibyte *VBF_ptr = BYTE_BUF_BYTE_ADDRESS_NO_VERIFY (buf, x);	\
	while (!ibyte_first_byte_p (*VBF_ptr))				\
	  VBF_ptr++, (x)++;						\
      }									\
      break;								\
    case FORMAT_16_BIT_FIXED:						\
      if (((x - 1) & 1) != 0)						\
	x++;								\
      break;								\
    case FORMAT_32_BIT_FIXED:						\
      while (((x - 1) & 3) != 0)					\
	x++;								\
      break;								\
    default:								\
      break;								\
    }									\
} while (0)
#else
# define VALIDATE_BYTEBPOS_FORWARD(buf, x)
#endif

/*----------------------------------------------------------------------*/
/*			 Working with byte positions			*/
/*----------------------------------------------------------------------*/


/*  Given a byte position (assumed to point at the beginning of a
    character), modify that value so it points to the beginning of the next
    character.
    
    Note that in the simplest case (no MULE, no ERROR_CHECK_TEXT),
    this crap reduces down to simply (x)++. */

#define INC_BYTEBPOS(buf, x) do				\
{							\
  ASSERT_VALID_BYTEBPOS_FORWARD_UNSAFE (buf, x);	\
  /* Note that we do the increment first to		\
     make sure that the pointer in			\
     VALIDATE_BYTEBPOS_FORWARD() ends up on		\
     the correct side of the gap */			\
  (x)++;						\
  VALIDATE_BYTEBPOS_FORWARD (buf, x);			\
} while (0)

/*  Given a byte position (assumed to point at the beginning of a
    character), modify that value so it points to the beginning of the
    previous character.  Unlike for DEC_IBYTEPTR(), we can do all the
    assert()s because there are sentinels at the beginning of the gap and
    the end of the buffer.

    Note that in the simplest case (no MULE, no ERROR_CHECK_TEXT), this
    crap reduces down to simply (x)--. */

#define DEC_BYTEBPOS(buf, x) do				\
{							\
  ASSERT_VALID_BYTEBPOS_BACKWARD_UNSAFE (buf, x);	\
  /* Note that we do the decrement first to		\
     make sure that the pointer in			\
     VALIDATE_BYTEBPOS_BACKWARD() ends up on		\
     the correct side of the gap */			\
  (x)--;						\
  VALIDATE_BYTEBPOS_BACKWARD (buf, x);			\
} while (0)

DECLARE_INLINE_HEADER (
Bytebpos
prev_bytebpos (struct buffer *buf, Bytebpos x)
)
{
  DEC_BYTEBPOS (buf, x);
  return x;
}

DECLARE_INLINE_HEADER (
Bytebpos
next_bytebpos (struct buffer *buf, Bytebpos x)
)
{
  INC_BYTEBPOS (buf, x);
  return x;
}

/* A constant representing an invalid Bytebpos.  Valid Bytebposes
   can never have this value. */

#define BYTEBPOS_INVALID ((Bytebpos) -1)

/*----------------------------------------------------------------------*/
/*	   Converting between byte and character positions		*/
/*----------------------------------------------------------------------*/

/*

Info on Byte-Char conversion:

  (Info-goto-node "(internals)Byte-Char Position Conversion")
*/

#ifdef MULE

Bytebpos charbpos_to_bytebpos_func (struct buffer *buf, Charbpos x);
Charbpos bytebpos_to_charbpos_func (struct buffer *buf, Bytebpos x);
extern short three_to_one_table[];

#endif /* MULE */

/* Given a Charbpos, return the equivalent Bytebpos. */

DECLARE_INLINE_HEADER (
Bytebpos
charbpos_to_bytebpos (struct buffer *buf, Charbpos x)
)
{
  Bytebpos retval;
  ASSERT_VALID_CHARBPOS_UNSAFE (buf, x);
#ifdef MULE
  if (buf->text->entirely_one_byte_p)
    retval = (Bytebpos) x;
  else if (BUF_FORMAT (buf) == FORMAT_16_BIT_FIXED)
    retval = (Bytebpos) (x << 1);
  else if (BUF_FORMAT (buf) == FORMAT_32_BIT_FIXED)
    retval = (Bytebpos) (x << 2);
#ifdef OLD_BYTE_CHAR
  else if (x >= buf->text->mule_bufmin && x <= buf->text->mule_bufmax)
    retval = (buf->text->mule_bytmin +
	    ((x - buf->text->mule_bufmin) << buf->text->mule_shifter) +
	    (buf->text->mule_three_p ? (x - buf->text->mule_bufmin) :
	     (Bytebpos) 0));
#endif /* OLD_BYTE_CHAR */
  else
    retval = charbpos_to_bytebpos_func (buf, x);
#else
  retval = (Bytebpos) x;
#endif
  ASSERT_VALID_BYTEBPOS_UNSAFE (buf, retval);
  return retval;
}

/* Given a Bytebpos, return the equivalent Charbpos. */

DECLARE_INLINE_HEADER (
Charbpos
bytebpos_to_charbpos (struct buffer *buf, Bytebpos x)
)
{
  Charbpos retval;
  ASSERT_VALID_BYTEBPOS_UNSAFE (buf, x);
#ifdef MULE
  if (buf->text->entirely_one_byte_p)
    retval = (Charbpos) x;
  else if (BUF_FORMAT (buf) == FORMAT_16_BIT_FIXED)
    retval = (Charbpos) (x >> 1);
  else if (BUF_FORMAT (buf) == FORMAT_32_BIT_FIXED)
    retval = (Charbpos) (x >> 2);
#ifdef OLD_BYTE_CHAR
  else if (x >= buf->text->mule_bytmin && x <= buf->text->mule_bytmax)
    retval = (buf->text->mule_bufmin +
	      ((buf->text->mule_three_p
		? three_to_one_table[x - buf->text->mule_bytmin]
		: (x - buf->text->mule_bytmin) >> buf->text->mule_shifter)));
#endif /* OLD_BYTE_CHAR */
  else
    retval = bytebpos_to_charbpos_func (buf, x);
#else
  retval = (Charbpos) x;
#endif
  ASSERT_VALID_CHARBPOS_UNSAFE (buf, retval);
  return retval;
}

/* Given a Bytebpos, return the equivalent Charbpos as a Lisp Object. */

#define make_charbpos(buf, ind) make_int (bytebpos_to_charbpos (buf, ind))

/*----------------------------------------------------------------------*/
/*	    Converting between byte and memory positions		*/
/*----------------------------------------------------------------------*/

DECLARE_INLINE_HEADER (
int
valid_membpos_p (struct buffer *buf, Membpos x)
)
{
  return ((x >= 1 && x <= (Membpos) buf->text->gpt) ||
	  (x  > (Membpos) (buf->text->gpt + buf->text->gap_size) &&
	   x <= (Membpos) (buf->text->z   + buf->text->gap_size)));
}

DECLARE_INLINE_HEADER (
Membpos
bytebpos_to_membpos (struct buffer *buf, Bytebpos x)
)
{
  ASSERT_VALID_BYTEBPOS_UNSAFE (buf, x);
  return (Membpos) ((x > buf->text->gpt) ? (x + buf->text->gap_size) : x);
}


DECLARE_INLINE_HEADER (
Bytebpos
membpos_to_bytebpos (struct buffer *buf, Membpos x)
)
{
  Bytebpos retval;
  text_checking_assert (valid_membpos_p (buf, x));
  retval = (Bytebpos) ((x > (Membpos) buf->text->gpt) ?
		       x - buf->text->gap_size :
		       x);
  ASSERT_VALID_BYTEBPOS_UNSAFE (buf, retval);
  return retval;
}

DECLARE_INLINE_HEADER (
Charbpos
membpos_to_charbpos (struct buffer *buf, Membpos x)
)
{
  return bytebpos_to_charbpos (buf, membpos_to_bytebpos (buf, x));
}

DECLARE_INLINE_HEADER (
Membpos
charbpos_to_membpos (struct buffer *buf, Charbpos x)
)
{
  return bytebpos_to_membpos (buf, charbpos_to_bytebpos (buf, x));
}

/*----------------------------------------------------------------------*/
/*             Generalized buffer/string position convertors            */
/*----------------------------------------------------------------------*/

/* These macros generalize many standard buffer-position functions to
   either a buffer or a string. */

/* Converting between Memxpos's and Bytexpos's, for a buffer-or-string.
   For strings, this is a no-op.  For buffers, this resolves
   to the standard membpos<->bytebpos converters. */

DECLARE_INLINE_HEADER (
Memxpos buffer_or_string_bytexpos_to_memxpos (Lisp_Object obj, Bytexpos pos)
)
{
  return (BUFFERP (obj) ? bytebpos_to_membpos (XBUFFER (obj), pos) :
	  (Memxpos) pos);
}

DECLARE_INLINE_HEADER (
Bytexpos buffer_or_string_memxpos_to_bytexpos (Lisp_Object obj, Memxpos pos)
)
{
  return (BUFFERP (obj) ? membpos_to_bytebpos (XBUFFER (obj), pos) :
	  (Bytexpos) pos);
}

/* Converting between Charxpos's and Bytexpos's, for a buffer-or-string.
   For strings, this maps to the bytecount<->charcount converters. */

DECLARE_INLINE_HEADER (
Bytexpos buffer_or_string_charxpos_to_bytexpos (Lisp_Object obj, Charxpos pos)
)
{	
  return (BUFFERP (obj) ? charbpos_to_bytebpos (XBUFFER (obj), pos) :
   (Bytexpos) string_index_char_to_byte (obj, pos));
}

DECLARE_INLINE_HEADER (
Charxpos buffer_or_string_bytexpos_to_charxpos (Lisp_Object obj, Bytexpos pos)
)
{	
  return (BUFFERP (obj) ? bytebpos_to_charbpos (XBUFFER (obj), pos) :
   (Charxpos) string_index_byte_to_char (obj, pos));
}

/* Similar for Charxpos's and Memxpos's. */

DECLARE_INLINE_HEADER (
Memxpos buffer_or_string_charxpos_to_memxpos (Lisp_Object obj, Charxpos pos)
)
{	
  return (BUFFERP (obj) ? charbpos_to_membpos (XBUFFER (obj), pos) :
   (Memxpos) string_index_char_to_byte (obj, pos));
}

DECLARE_INLINE_HEADER (
Charxpos buffer_or_string_memxpos_to_charxpos (Lisp_Object obj, Memxpos pos)
)
{	
  return (BUFFERP (obj) ? membpos_to_charbpos (XBUFFER (obj), pos) :
   (Charxpos) string_index_byte_to_char (obj, pos));
}

DECLARE_INLINE_HEADER (
Internal_Format buffer_or_other_internal_format (Lisp_Object obj)
)
{	
  return BUFFERP (obj) ? BUF_FORMAT (XBUFFER (obj)) : FORMAT_DEFAULT;
}

/* Return the index to the character before the one at X,
   in a buffer or string. */

DECLARE_INLINE_HEADER (
Bytebpos
prev_bytexpos (Lisp_Object obj, Bytebpos x)
)
{
  return BUFFERP (obj) ? prev_bytebpos (XBUFFER (obj), x) : 
         prev_string_index (obj, x);
}

/* Return the index to the character after the one at X,
   in a buffer or string. */

DECLARE_INLINE_HEADER (
Bytebpos
next_bytexpos (Lisp_Object obj, Bytebpos x)
)
{
  return BUFFERP (obj) ? next_bytebpos (XBUFFER (obj), x) :
         next_string_index (obj, x);
}

/*----------------------------------------------------------------------*/
/*		Converting between positions and addresses		*/
/*----------------------------------------------------------------------*/

/* Convert the address of a byte in the buffer into a position.  */
DECLARE_INLINE_HEADER (
Bytebpos
BYTE_BUF_PTR_BYTE_POS (struct buffer *buf, Ibyte *ptr)
)
{
  Bytebpos retval = (ptr - buf->text->beg + 1
		     - ((ptr - buf->text->beg + 1) > buf->text->gpt
			? buf->text->gap_size : (Bytebpos) 0));
  ASSERT_VALID_BYTEBPOS_UNSAFE (buf, retval);
  return retval;
}

#define BUF_PTR_BYTE_POS(buf, ptr) \
  bytebpos_to_charbpos (buf, BYTE_BUF_PTR_BYTE_POS (buf, ptr))

/* Address of byte at position POS in buffer. */
DECLARE_INLINE_HEADER (
Ibyte *
BYTE_BUF_BYTE_ADDRESS (struct buffer *buf, Bytebpos pos)
)
{
  ASSERT_VALID_BYTEBPOS_UNSAFE (buf, pos);
  return BYTE_BUF_BYTE_ADDRESS_NO_VERIFY (buf, pos);
}

#define BUF_BYTE_ADDRESS(buf, pos) \
  BYTE_BUF_BYTE_ADDRESS (buf, charbpos_to_bytebpos (buf, pos))

/* Address of byte before position POS in buffer. */
DECLARE_INLINE_HEADER (
Ibyte *
BYTE_BUF_BYTE_ADDRESS_BEFORE (struct buffer *buf, Bytebpos pos)
)
{
  ASSERT_VALID_BYTEBPOS_UNSAFE (buf, pos);
  return (buf->text->beg +
	  ((pos > buf->text->gpt ? (pos + buf->text->gap_size) : pos)
	   - 2));
}

#define BUF_BYTE_ADDRESS_BEFORE(buf, pos) \
  BYTE_BUF_BYTE_ADDRESS_BEFORE (buf, charbpos_to_bytebpos (buf, pos))

/*----------------------------------------------------------------------*/
/*         Converting between buffer bytes and Emacs characters         */
/*----------------------------------------------------------------------*/

/* The character at position POS in buffer. */

#define BYTE_BUF_FETCH_CHAR(buf, pos)					\
   itext_ichar_fmt (BYTE_BUF_BYTE_ADDRESS (buf, pos), BUF_FORMAT (buf), \
		       wrap_buffer (buf))
#define BUF_FETCH_CHAR(buf, pos) \
  BYTE_BUF_FETCH_CHAR (buf, charbpos_to_bytebpos (buf, pos))

/* The "raw value" of the character at position POS in buffer.
   See ichar_to_raw(). */

#define BYTE_BUF_FETCH_CHAR_RAW(buf, pos)				 \
   itext_ichar_raw_fmt (BYTE_BUF_BYTE_ADDRESS (buf, pos), BUF_FORMAT (buf))
#define BUF_FETCH_CHAR_RAW(buf, pos) \
  BYTE_BUF_FETCH_CHAR_RAW (buf, charbpos_to_bytebpos (buf, pos))

/* The character at position POS in buffer, as a string.  This is
   equivalent to set_itext_ichar (str, BUF_FETCH_CHAR (buf, pos))
   but is faster for Mule. */

# define BYTE_BUF_ITEXT_COPY_ICHAR(buf, pos, str)		\
  (BUF_FORMAT (buf) == FORMAT_DEFAULT ?				\
   itext_copy_ichar (BYTE_BUF_BYTE_ADDRESS (buf, pos), str) :	\
   set_itext_ichar (str, BYTE_BUF_FETCH_CHAR (buf, pos)))
#define BUF_ITEXT_COPY_ICHAR(buf, pos, str) \
  BYTE_BUF_ITEXT_COPY_ICHAR (buf, charbpos_to_bytebpos (buf, pos), str)


/*--------------------------------------------------------------------------*/
/* Converting between characters, charset codepoints and Unicode codepoints */
/*--------------------------------------------------------------------------*/

DECLARE_INLINE_HEADER (
Ichar
buffer_filtered_unicode_to_ichar (int code, struct buffer *buf,
				  charset_pred predicate,
				  enum converr fail)
)
{
  Ichar ch;
  ASSERT_VALID_UNICODE_CODEPOINT (code);
  text_checking_assert (buf);

#if defined (MULE) && !defined (UNICODE_INTERNAL)
  ch = filtered_unicode_to_ichar (code, buf->unicode_precedence_array,
				  predicate, CONVERR_FAIL);
  if (ch < 0)
    ch = filtered_unicode_to_ichar (code, Vdefault_unicode_precedence_array,
				    predicate, fail);
  return ch;
#else
  return filtered_unicode_to_ichar (code, Qnil, predicate, fail);
#endif /* (not) defined (MULE) && !defined (UNICODE_INTERNAL) */
}

DECLARE_INLINE_HEADER (
Ichar
buffer_unicode_to_ichar (int code, struct buffer *buf, enum converr fail)
)
{
  return buffer_filtered_unicode_to_ichar (code, buf, NULL, fail);
}

#ifdef MULE

DECLARE_INLINE_HEADER (
void
buffer_filtered_unicode_to_charset_codepoint (int code, struct buffer *buf,
					      charset_pred predicate,
					      Lisp_Object *charset,
					      int *c1, int *c2,
					      enum converr fail)
)
{
  text_checking_assert (buf);
  filtered_unicode_to_charset_codepoint (code, buf->unicode_precedence_array,
					 predicate, charset, c1, c2,
					 CONVERR_FAIL);
  if (NILP (*charset))
    filtered_unicode_to_charset_codepoint (code,
					   Vdefault_unicode_precedence_array,
					   predicate, charset, c1, c2, fail);
}

DECLARE_INLINE_HEADER (
void
buffer_unicode_to_charset_codepoint (int code, struct buffer *buf,
				     Lisp_Object *charset, int *c1, int *c2,
				     enum converr fail)
)
{
  buffer_filtered_unicode_to_charset_codepoint (code, buf, NULL, charset,
						c1, c2, fail);
}

DECLARE_INLINE_HEADER (
void
buffer_filtered_ichar_to_charset_codepoint (Ichar ch, struct buffer *buf,
					    charset_pred predicate,
					    Lisp_Object *charset,
					    int *c1, int *c2,
					    enum converr fail)
)
{
  text_checking_assert (buf);
#ifdef UNICODE_INTERNAL
  filtered_ichar_to_charset_codepoint (ch, buf->unicode_precedence_array,
				       predicate, charset, c1, c2,
				       CONVERR_FAIL);
  if (NILP (*charset))
    filtered_ichar_to_charset_codepoint (ch, Vdefault_unicode_precedence_array,
					 predicate, charset, c1, c2, fail);
#else
  filtered_ichar_to_charset_codepoint (ch, Qnil, predicate, charset, c1, c2,
				       fail);
#endif /* (not) UNICODE_INTERNAL */
}

DECLARE_INLINE_HEADER (
void
buffer_ichar_to_charset_codepoint (Ichar ch, struct buffer *buf,
				   Lisp_Object *charset, int *c1, int *c2,
				   enum converr fail)
)
{
  buffer_filtered_ichar_to_charset_codepoint (ch, buf, NULL, charset,
					      c1, c2, fail);
}

DECLARE_INLINE_HEADER (
void
buffer_filtered_itext_to_charset_codepoint (const Ibyte *ptr,
					    struct buffer *buf,
					    charset_pred predicate,
					    Lisp_Object *charset,
					    int *c1, int *c2,
					    enum converr fail)
)
{
  text_checking_assert (buf);
#ifdef UNICODE_INTERNAL
  filtered_itext_to_charset_codepoint (ptr, buf->unicode_precedence_array,
				       predicate, charset, c1, c2,
				       CONVERR_FAIL);
  if (NILP (*charset))
    filtered_itext_to_charset_codepoint (ptr,
					 Vdefault_unicode_precedence_array,
					 predicate, charset, c1, c2, fail);
#else
  filtered_itext_to_charset_codepoint (ptr, Qnil, predicate, charset, c1, c2,
				       fail);
#endif /* (not) UNICODE_INTERNAL */
}

DECLARE_INLINE_HEADER (
void
buffer_itext_to_charset_codepoint (const Ibyte *ptr, struct buffer *buf,
				     Lisp_Object *charset, int *c1, int *c2,
				     enum converr fail)
)
{
  buffer_filtered_itext_to_charset_codepoint (ptr, buf, NULL, charset,
					      c1, c2, fail);
}

#endif /* MULE */

/* @@####
   Get rid of this crap now!!!!!!!!!!!!!!

   This will simply not fly in a Unicode world, where there may not be any
   national charset for a particular character.  Almost everywhere that this
   is used, it's used for font handling.  We need to replace device methods
   like find_charset_font() and font_spec_matches_charset() with similar
   methods that operate on a character, not a charset.  We might still need
   to do some charset lookup if we want to implement the idea that we use
   the appropriate Chinese, Japanese or Korean specific font depending
   on the language that a particular character is tagged as (as determined
   by the string extent surrounding the character in a buffer, or a
   buffer-local value indicating the language) -- but we absolutely do not
   want to be *dependent* on finding some national charset. (And in any
   case it probably makes more sense to do such conditionalizing on the
   Unicode range of the character, and just check whether a font
   contains the appropriate character -- or maybe not even conditionalize
   at all on any character-specific property.) */
DECLARE_INLINE_HEADER (
Lisp_Object
buffer_ichar_charset_obsolete_me_baby (struct buffer *
				       USED_IF_UNICODE_INTERNAL (buf),
				       Ichar ch)
)
{
#ifdef UNICODE_INTERNAL
  int byte1, byte2;
  Lisp_Object charset;
  buffer_ichar_to_charset_codepoint (ch, buf, &charset, &byte1, &byte2,
				     CONVERR_FAIL);
  return charset;
#elif defined (MULE)
  return old_mule_ichar_charset (ch);
#else
  return Vcharset_ascii;
#endif /* (not) defined (MULE) */

}


/************************************************************************/
/*                                                                      */
/*                  higher-level buffer-position functions              */
/*                                                                      */
/************************************************************************/

/*----------------------------------------------------------------------*/
/*           Settor macros for important positions in a buffer          */
/*----------------------------------------------------------------------*/

/* Set beginning of accessible range of buffer.  */
#define SET_BOTH_BUF_BEGV(buf, val, bpval)	\
do						\
{						\
  (buf)->begv = (bpval);			\
  (buf)->bufbegv = (val);			\
} while (0)

/* Set end of accessible range of buffer.  */
#define SET_BOTH_BUF_ZV(buf, val, bpval)	\
do						\
{						\
  (buf)->zv = (bpval);				\
  (buf)->bufzv = (val);				\
} while (0)

/* Set point. */
/* Since BEGV and ZV are almost never set, it's reasonable to enforce
   the restriction that the Charbpos and Bytebpos values must both be
   specified.  However, point is set in lots and lots of places.  So
   we provide the ability to specify both (for efficiency) or just
   one. */
#define BOTH_BUF_SET_PT(buf, val, bpval) set_buffer_point (buf, val, bpval)
#define BYTE_BUF_SET_PT(buf, bpval)					\
do									\
{									\
  Bytebpos __bpbsp_bpval = (bpval);					\
  BOTH_BUF_SET_PT (buf, bytebpos_to_charbpos (buf, __bpbsp_bpval),	\
		   __bpbsp_bpval);					\
} while (0)
#define BUF_SET_PT(buf, value)						   \
do									   \
{									   \
  Bytebpos __bsp_val = (value);						   \
  BOTH_BUF_SET_PT (buf, __bsp_val, charbpos_to_bytebpos (buf, __bsp_val)); \
} while (0)


#if 0 /* FSFmacs */
/* These macros exist in FSFmacs because SET_PT() in FSFmacs incorrectly
   does too much stuff, such as moving out of invisible extents. */
#define TEMP_SET_PT(position) (temp_set_point ((position), current_buffer))
#define SET_BUF_PT(buf, value) ((buf)->pt = (value))
#endif /* FSFmacs */

/*----------------------------------------------------------------------*/
/*                      Miscellaneous buffer values                     */
/*----------------------------------------------------------------------*/

/* Number of characters in buffer */
#define BUF_SIZE(buf) (BUF_Z (buf) - BUF_BEG (buf))

/* Is this buffer narrowed? */
#define BUF_NARROWED(buf)				\
   ((BYTE_BUF_BEGV (buf) != BYTE_BUF_BEG (buf)) ||	\
    (BYTE_BUF_ZV   (buf) != BYTE_BUF_Z   (buf)))

/* Modification count */
#define BUF_MODIFF(buf) ((buf)->text->modiff)

/* Saved modification count */
#define BUF_SAVE_MODIFF(buf) ((buf)->text->save_modiff)

/* Face changed.  */
#define BUF_FACECHANGE(buf) ((buf)->face_change)

DECLARE_INLINE_HEADER (
int
POINT_MARKER_P (Lisp_Object marker)
)
{
  return (XMARKER (marker)->buffer != 0 &&
	  EQ (marker, XMARKER (marker)->buffer->point_marker));
}

#define BUF_MARKERS(buf) ((buf)->markers)

/* WARNING:

   The new definitions of CEILING_OF() and FLOOR_OF() differ semantically
   from the old ones (in FSF Emacs and XEmacs 19.11 and before).
   Conversion is as follows:

   OLD_BYTE_CEILING_OF(n) = NEW_BYTE_CEILING_OF(n) - 1
   OLD_BYTE_FLOOR_OF(n) = NEW_BYTE_FLOOR_OF(n + 1)

   The definitions were changed because the new definitions are more
   consistent with the way everything else works in XEmacs.
 */

/* Properties of CEILING_OF and FLOOR_OF (also apply to BYTE_ variants):

   1) FLOOR_OF (CEILING_OF (n)) = n
      CEILING_OF (FLOOR_OF (n)) = n

   2) CEILING_OF (n) = n if and only if n = ZV
      FLOOR_OF (n) = n if and only if n = BEGV

   3) CEILING_OF (CEILING_OF (n)) = ZV
      FLOOR_OF (FLOOR_OF (n)) = BEGV

   4) The bytes in the regions

      [BYTE_ADDRESS (n), BYTE_ADDRESS_BEFORE (CEILING_OF (n))]

      and

      [BYTE_ADDRESS (FLOOR_OF (n)), BYTE_ADDRESS_BEFORE (n)]

      are contiguous.

  A typical loop using CEILING_OF to process contiguous ranges of text
  between [from, to) looks like this:
  
  {
    Bytebpos pos = from;

    while (pos < to)
      {
	Bytebpos ceil = BYTE_BUF_CEILING_OF (buf, pos);
	ceil = min (to, ceil);
	process_ibyte_string (BYTE_BUF_BYTE_ADDRESS (buf, pos), ceil - pos);
	pos = ceil;
      }
  }

  Currently there will be at most two iterations in the loop, but it is
  written in such a way that it will still work if the buffer
  representation is changed to have multiple gaps in it.
  */

/*  Return the maximum position in the buffer it is safe to scan forwards
    past N to.  This is used to prevent buffer scans from running into
    the gap (e.g. search.c).  All characters between N and CEILING_OF(N)
    are located contiguous in memory.  Note that the character *at*
    CEILING_OF(N) is not contiguous in memory. */
#define BYTE_BUF_CEILING_OF(b, n)					\
  ((n) < BYTE_BUF_GPT (b) && BYTE_BUF_GPT (b) < BYTE_BUF_ZV (b) ?	\
   BYTE_BUF_GPT (b) : BYTE_BUF_ZV (b))
#define BUF_CEILING_OF(b, n)				\
  ((n) < BUF_GPT (b) && BUF_GPT (b) < BUF_ZV (b) ?	\
   BUF_GPT (b) : BUF_ZV (b))

/*  Return the minimum position in the buffer it is safe to scan backwards
    past N to.  All characters between FLOOR_OF(N) and N are located
    contiguous in memory.  Note that the character *at* N may not be
    contiguous in memory. */
#define BYTE_BUF_FLOOR_OF(b, n)						  \
        (BYTE_BUF_BEGV (b) < BYTE_BUF_GPT (b) && BYTE_BUF_GPT (b) < (n) ? \
	 BYTE_BUF_GPT (b) : BYTE_BUF_BEGV (b))
#define BUF_FLOOR_OF(b, n)					\
        (BUF_BEGV (b) < BUF_GPT (b) && BUF_GPT (b) < (n) ?	\
	 BUF_GPT (b) : BUF_BEGV (b))

#define BYTE_BUF_CEILING_OF_IGNORE_ACCESSIBLE(b, n)			\
  ((n) < BYTE_BUF_GPT (b) && BYTE_BUF_GPT (b) < BYTE_BUF_Z (b) ?	\
   BYTE_BUF_GPT (b) : BYTE_BUF_Z (b))
#define BUF_CEILING_OF_IGNORE_ACCESSIBLE(b, n)		\
  ((n) < BUF_GPT (b) && BUF_GPT (b) < BUF_Z (b) ?	\
   BUF_GPT (b) : BUF_Z (b))

#define BYTE_BUF_FLOOR_OF_IGNORE_ACCESSIBLE(b, n)			 \
        (BYTE_BUF_BEG (b) < BYTE_BUF_GPT (b) && BYTE_BUF_GPT (b) < (n) ? \
	 BYTE_BUF_GPT (b) : BYTE_BUF_BEG (b))
#define BUF_FLOOR_OF_IGNORE_ACCESSIBLE(b, n)			\
        (BUF_BEG (b) < BUF_GPT (b) && BUF_GPT (b) < (n) ?	\
	 BUF_GPT (b) : BUF_BEG (b))

/* Iterate over contiguous chunks of text in buffer BUF, starting at POS,
   of length LEN.  Evaluates POS and LEN only once, but BUF multiply.  In
   each iteration, store the current chunk into RUNPTR/RUNLEN, which will
   be automatically declared (don't declare them yourself).  This does not
   respect the limits of accessibility (BUF_BEGV/BUF_ZV); if you want these
   limits respected, you need to impose them yourself.

   NOTE: This must be surrounded with braces! */

#define BUFFER_TEXT_LOOP(buf, pos, len, runptr, runlen)			      \
Ibyte *runptr;								      \
Bytecount runlen;							      \
Bytebpos BTL_pos = (pos);						      \
Bytebpos BTL_len = (len);						      \
for (runptr = BYTE_BUF_BYTE_ADDRESS (buf, BTL_pos),			      \
     runlen = BYTE_BUF_CEILING_OF_IGNORE_ACCESSIBLE (buf, BTL_pos) - BTL_pos, \
     runlen = min (BTL_len, runlen);					      \
     BTL_len > 0;							      \
     BTL_pos += runlen,							      \
     BTL_len -= runlen,							      \
     runptr = BYTE_BUF_BYTE_ADDRESS (buf, BTL_pos),			      \
     runlen = BYTE_BUF_CEILING_OF_IGNORE_ACCESSIBLE (buf, BTL_pos) - BTL_pos, \
     runlen = min (BTL_len, runlen))

/* This structure marks which slots in a buffer have corresponding
   default values in Vbuffer_defaults.
   Each such slot has a nonzero value in this structure.
   The value has only one nonzero bit.

   When a buffer has its own local value for a slot,
   the bit for that slot (found in the same slot in this structure)
   is turned on in the buffer's local_var_flags slot.

   If a slot in this structure is zero, then even though there may
   be a DEFVAR_BUFFER_LOCAL for the slot, there is no default value for it;
   and the corresponding slot in Vbuffer_defaults is not used.  */

extern struct buffer buffer_local_flags;


/* Allocation of buffer data. */

#ifdef REL_ALLOC

char *r_alloc (unsigned char **, size_t);
char *r_re_alloc (unsigned char **, size_t);
void r_alloc_free (unsigned char **);

#define BUFFER_ALLOC(data, size) \
  ((Ibyte *) r_alloc ((unsigned char **) &data, (size) * sizeof(Ibyte)))
#define BUFFER_REALLOC(data, size) \
  ((Ibyte *) r_re_alloc ((unsigned char **) &data, (size) * sizeof(Ibyte)))
#define BUFFER_FREE(data) r_alloc_free ((unsigned char **) &(data))
#define R_ALLOC_DECLARE(var,data) r_alloc_declare (&(var), data)

#else /* !REL_ALLOC */

#define BUFFER_ALLOC(data,size)\
	(data = xnew_array (Ibyte, size))
#define BUFFER_REALLOC(data,size)\
	((Ibyte *) xrealloc (data, (size) * sizeof(Ibyte)))
/* Avoid excess parentheses, or syntax errors may rear their heads. */
#define BUFFER_FREE(data) xfree (data, Ibyte *)
#define R_ALLOC_DECLARE(var,data)

#endif /* !REL_ALLOC */

#endif /* INCLUDED_buffer_h_ */
