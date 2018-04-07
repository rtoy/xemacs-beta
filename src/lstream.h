/* Generic stream implementation -- header file.
   Copyright (C) 1995 Free Software Foundation, Inc.
   Copyright (C) 1996, 2001, 2002, 2010 Ben Wing.

This file is part of XEmacs.

XEmacs is free software: you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation, either version 3 of the License, or (at your
option) any later version.

XEmacs is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with XEmacs.  If not, see <http://www.gnu.org/licenses/>. */

/* Synched up with: Not in FSF. */

/* Written by Ben Wing. */

#ifndef INCLUDED_lstream_h_
#define INCLUDED_lstream_h_

#include "tls.h"

struct extent_info;

/************************************************************************/
/*                     definition of Lstream object                     */
/************************************************************************/

DECLARE_LISP_OBJECT (lstream, struct lstream);
#define XLSTREAM(x) XRECORD (x, lstream, struct lstream)
#define wrap_lstream(p) wrap_record (p, lstream)
#define LSTREAMP(x) RECORDP (x, lstream)
/* Can't use the usual CONCHECK_RECORD() macros, since the type is lstream in
   C and stream in Lisp. */
#define CHECK_LSTREAM(x) do {                           \
 if (!RECORD_TYPEP (x, lrecord_type_lstream))		\
   dead_wrong_type_argument (Qstreamp, x);		\
 } while (0)
#define CONCHECK_LSTREAM(x) do {			\
 if (!RECORD_TYPEP (x, lrecord_type_lstream))		\
   x = wrong_type_argument (Qstreamp, x);		\
}  while (0)

#ifndef EOF
#define EOF (-1)
#endif

/* There have been some arguments over the what the type should be that
   specifies a count of bytes in a data block to be written out or read in,
   using Lstream_read(), Lstream_write(), and related functions.
   Originally it was long, which worked fine; Martin "corrected" these to
   size_t and ssize_t on the grounds that this is theoretically cleaner and
   is in keeping with the C standards.  Unfortunately, this practice is
   horribly error-prone due to design flaws in the way that mixed
   signed/unsigned arithmetic happens.  In fact, by doing this change,
   Martin introduced a subtle but fatal error that caused the operation of
   sending large mail messages to the SMTP server under Windows to fail.
   By putting all values back to be signed, avoiding any signed/unsigned
   mixing, the bug immediately went away.  The type then in use was
   Lstream_Data_Count, so that it be reverted cleanly if a vote came to
   that.  Now it is Bytecount.

   Some earlier comments about why the type must be signed: This MUST BE
   SIGNED, since it also is used in functions that return the number of
   bytes actually read to or written from in an operation, and these
   functions can return -1 to signal error.
   
   Note that the standard Unix read() and write() functions define the
   count going in as a size_t, which is UNSIGNED, and the count going
   out as an ssize_t, which is SIGNED.  This is a horrible design
   flaw.  Not only is it highly likely to lead to logic errors when a
   -1 gets interpreted as a large positive number, but operations are
   bound to fail in all sorts of horrible ways when a number in the
   upper-half of the size_t range is passed in -- this number is
   unrepresentable as an ssize_t, so code that checks to see how many
   bytes are actually written (which is mandatory if you are dealing
   with certain types of devices) will get completely screwed up.
   
   --ben
*/
typedef enum lstream_buffering
{
  /* No buffering. */
  LSTREAM_UNBUFFERED,
  /* Buffer until a '\n' character is reached. */
  LSTREAM_LINE_BUFFERED,
  /* Buffer in standard-size (i.e. 512-byte) blocks. */
  LSTREAM_BLOCK_BUFFERED,
  /* Buffer in blocks of a specified size. */
  LSTREAM_BLOCKN_BUFFERED,
  /* Buffer until the stream is closed (only applies to write-only
     streams).  Only one call to the stream writer will be made,
     and that is when the stream is closed. */
  LSTREAM_UNLIMITED
} Lstream_buffering;

#if 0

/* #### not currently implemented; correct EOF handling is quite tricky
   in the presence of various levels of filtering streams, and simply
   interpreting 0 as EOF works fairly well as long as the amount of
   data you're attempting to read is large and you know whether the
   source stream at the end of the chain is a pipe (or other blocking
   source) or not.  we really should fix this, though.  */

/* Return values from Lstream_read().  We do NOT use the C lib trick
   of returning 0 to maybe indicate EOF because that is simply too
   random and error-prone.  It is quite legitimate for there to be no
   data available but no EOF, even when not in the presence of
   non-blocking I/O.  For example, decoding/encoding streams (and in
   general, any type of filtering stream) may only be able to return
   data after a certain amount of data on the other end is
   available. */

#define LSTREAM_EOF -2

#endif /* 0 */

#define LSTREAM_ERROR -1

/* Methods defining how this stream works.  Some may be undefined. */

/* We do not implement the seek/tell paradigm.  I tried to do that,
   but getting the semantics right in the presence of buffering is
   extremely tricky and very error-prone and basically not worth it.
   This is especially the case with complicated streams like
   decoding streams -- the seek pointer in this case can't be a single
   integer but has to be a whole complicated structure that records
   all of the stream's state at the time.

   Rewind semantics are generally easy to implement, so we do provide
   a rewind method.  Even rewind() may not be available on a stream,
   however -- e.g. on process output. */

typedef struct lstream_implementation
{
  const char *name;
  Bytecount size; /* Number of additional bytes to be
		     allocated with this stream.  Access this
		     data using Lstream_data(). */

  /* Description of the extra data (struct foo_lstream) attached to a
     coding system. */
  const struct sized_memory_description *extra_description;

  /* Read some data from the stream's end and store it into DATA, which
     can hold SIZE bytes.  Return the number of bytes read.  A return
     value of 0 means no bytes can be read at this time.  This may
     be because of an EOF, or because there is a granularity greater
     than one byte that the stream imposes on the returned data, and
     SIZE is less than this granularity. (This will happen frequently
     for streams that need to return whole characters, because
     Lstream_read() calls the reader function repeatedly until it
     has the number of bytes it wants or until 0 is returned.)
     The lstream functions do not treat a 0 return as EOF or do
     anything special; however, the calling function will interpret
     any 0 it gets back as EOF.  This will normally not happen unless
     the caller calls Lstream_read() with a very small size.

     This function can be NULL if the stream is output-only. */
  /* The omniscient mly, blinded by the irresistible thrall of Common
     Lisp, thinks that it is bogus that the types and implementations
     of input and output streams are the same. */
  Bytecount (*reader) (Lstream *stream, unsigned char *data,
		       Bytecount size);
  /* Send some data to the stream's end.  Data to be sent is in DATA
     and is SIZE bytes.  Return the number of bytes sent.  This
     function can send and return fewer bytes than is passed in; in
     that case, the function will just be called again until there is
     no data left or 0 is returned.  A return value of 0 means that no
     more data can be currently stored, but there is no error; the
     data will be squirrelled away until the writer can accept
     data. (This is useful, e.g., of you're dealing with a
     non-blocking file descriptor and are getting EWOULDBLOCK errors.)
     This function can be NULL if the stream is input-only. */
  Bytecount (*writer) (Lstream *stream, const unsigned char *data,
		       Bytecount size);

  /* Like WRITER, but take the data from RELOC, a Lisp string, and copy any
     extent information to the other end of the stream. */
  Bytecount (*write_with_extents) (Lstream *stream, Lisp_Object reloc,
                                   Bytecount position, Bytecount length);

  /* Return non-zero if the last write operation on the stream resulted
     in an attempt to block (EWOULDBLOCK). If this method does not
     exists, the implementation returns 0 */
  int (*was_blocked_p) (Lstream *stream);
  /* Rewind the stream.  If this is NULL, the stream is not seekable. */
  int (*rewinder) (Lstream *stream);
  /* Indicate whether this stream is seekable -- i.e. it can be rewound.
     This method is ignored if the stream does not have a rewind
     method.  If this method is not present, the result is determined
     by whether a rewind method is present. */
  int (*seekable_p) (Lstream *stream);

  /* Return the number of complete characters read so far. Respects
     buffering and unget. Returns -1 if unknown or not implemented. */
  Charcount (*character_tell) (Lstream *stream);
  /* Perform any additional operations necessary to flush the
     data in this stream. */
  int (*flusher) (Lstream *stream);
  /* Perform any additional operations necessary to close this stream down.
     May be NULL.  This function is called when Lstream_close() is called
     (which will be called automatically on any open streams when they are
     garbage-collected or deleted with Lstream_delete()).  When this
     function is called, all pending data in the stream will already have
     been written out; however, the closer write more data, e.g. an "end"
     section at the end of a file. */
  int (*closer) (Lstream *stream);
  /* Clean up any remaining data at the time that a stream is
     garbage-collected or deleted with Lstream_delete().  If the stream was
     open at this point, the finalizer is called after calling
     Lstream_close().  Called only once (NOT called at disksave time). */
  void (*finalizer) (Lstream *stream);
  /* Mark this object for garbage collection.  Same semantics as
     a standard Lisp_Object marker.  This function can be NULL. */
  Lisp_Object (*marker) (Lisp_Object lstream);
  /* Return nonzero if this stream is using a TLS connection */
  int (*tls_p) (Lstream *stream);
  /* Perform STARTTLS negotiation on a pair of streams, one for input and one
     for output.  Both are transformed if negotiation is successful. */
  int (*tls_negotiater) (Lstream *instream, Lstream *outstream,
			 const Extbyte *host, Lisp_Object keylist);

  /* Return the extent info associated with the stream, or NULL if none. */ 
  struct extent_info *(*extent_info)(Lstream *stream);
} Lstream_implementation;

#define DEFINE_LSTREAM_IMPLEMENTATION(name, c_name)	\
  Lstream_implementation lstream_##c_name[1] =		\
    { { (name), sizeof (struct c_name##_stream),	\
      &lstream_empty_extra_description } }

#define DEFINE_LSTREAM_IMPLEMENTATION_WITH_DATA(name, c_name)		      \
  static const struct sized_memory_description c_name##_lstream_description_0 \
  = {									      \
    sizeof (struct c_name##_stream),					      \
    c_name##_lstream_description					      \
  };									      \
  Lstream_implementation lstream_##c_name[1] =				      \
    { { (name), sizeof (struct c_name##_stream),			      \
      &c_name##_lstream_description_0 } }

#define DECLARE_LSTREAM(c_name) \
  extern Lstream_implementation lstream_##c_name[]

/* Flags that can be passed to Lstream_new(). */
#define LSTR_READ		(1 << 0)
#define LSTR_WRITE		(1 << 1)
#define LSTR_NO_PARTIAL_CHARS	(1 << 2)
#define LSTR_CLOSE_AT_DISKSAVE	(1 << 3)
#define LSTR_NO_SQUIRREL	(1 << 4)

/* Internal flags that can also be set on the stream. */
#define LSTR_IS_OPEN	        (1 << 5)

struct lstream
{
  NORMAL_LISP_OBJECT_HEADER header;
  const Lstream_implementation *imp; /* methods for this stream */
  Lstream_buffering buffering; /* type of buffering in use */
  Bytecount buffering_size; /* number of bytes buffered */

  unsigned char *in_buffer; /* holds characters read from stream end */
  Bytecount in_buffer_size; /* allocated size of buffer */
  Bytecount in_buffer_current; /* number of characters in buffer */
  Bytecount in_buffer_ind; /* pointer to next character to
				       take from buffer */

  unsigned char *out_buffer; /* holds characters to write to stream end */
  Bytecount out_buffer_size; /* allocated size of buffer */
  Bytecount out_buffer_ind; /* pointer to next buffer spot to
					write a character */

  /* The unget buffer is more or less a stack -- things get pushed
     onto the end and read back from the end.  Lstream_read()
     basically reads backwards from the end to get stuff; Lstream_unread()
     similarly has to push the data on backwards. */
  unsigned char *unget_buffer; /* holds characters pushed back onto input */
  Bytecount unget_buffer_size; /* allocated size of buffer */
  Bytecount unget_buffer_ind; /* Next buffer spot to write a character */

  Charcount unget_character_count; /* Count of complete characters ever ungot. */

  Bytecount byte_count;
  int flags;
  /* Whether an error occurred during the most recent operation */
  unsigned int error_occurred_p:1;
  /* Whether an error has occurred since the user last reset the
     error-occurred flag.  This is the value returned by
     Lstream_error_occurred_p(). */
  unsigned int public_error_occurred_p:1;
  max_align_t data[1];
};

extern const struct sized_memory_description lstream_empty_extra_description;

#define LSTREAM_TYPE_P(lstr, type) \
  ((lstr)->imp == lstream_##type)

#ifdef ERROR_CHECK_TYPES
DECLARE_INLINE_HEADER (
struct lstream *
error_check_lstream_type (struct lstream *stream,
			  const Lstream_implementation *imp)
)
{
  assert (stream->imp == imp);
  return stream;
}
# define LSTREAM_TYPE_DATA(lstr, type)					\
  ((struct type##_stream *)						\
    Lstream_data (error_check_lstream_type (lstr, lstream_##type)))
#else
# define LSTREAM_TYPE_DATA(lstr, type)			\
  ((struct type##_stream *) Lstream_data (lstr))
#endif

/* Declare that lstream-type TYPE has method M; used in initialization
   routines */
#define LSTREAM_HAS_METHOD(type, m) \
  (lstream_##type->m = type##_##m)


Lstream *Lstream_new (const Lstream_implementation *imp, int flags);
void Lstream_reopen (Lstream *lstr);
void Lstream_set_buffering (Lstream *lstr, Lstream_buffering buffering,
			    int buffering_size);
int Lstream_flush (Lstream *lstr);
int Lstream_flush_out (Lstream *lstr);
int Lstream_fputc (Lstream *lstr, int c);
int Lstream_fgetc (Lstream *lstr);
void Lstream_fungetc (Lstream *lstr, int c);
Bytecount Lstream_read (Lstream *lstr, void *data, Bytecount size);
Charcount Lstream_character_tell (Lstream *);
Bytecount Lstream_write (Lstream *lstr, const void *data, Bytecount size);
Bytecount Lstream_write_with_extents (Lstream *lstr, Lisp_Object object,
                                      Bytexpos position, Bytecount len);
int Lstream_errno (Lstream *lstr);
int Lstream_was_blocked_p (Lstream *lstr);
void Lstream_unread (Lstream *lstr, const void *data, Bytecount size);
int Lstream_rewind (Lstream *lstr);
int Lstream_seekable_p (Lstream *lstr);
int Lstream_close (Lstream *lstr);
int Lstream_close_noflush (Lstream *lstr);

int Lstream_tls_p (Lstream *lstr);
int Lstream_tls_negotiate (Lstream *instr, Lstream *outstr,
			   const Extbyte *host, Lisp_Object keylist);

void Lstream_delete (Lstream *lstr);
void Lstream_set_character_mode (Lstream *str);
void Lstream_unset_character_mode (Lstream *lstr);
int Lstream_error_occurred_p (Lstream *lstr);
void Lstream_clear_error_occurred_p (Lstream *lstr);
void Lstream_set_error_occurred_p (Lstream *lstr);

int Lstream_is_type (Lstream *lstr, const Lstream_implementation *imp);

/* Lstream_putc: Write out one byte to the stream.  This is a macro
   and so it is very efficient.  The C argument is only evaluated once
   but the STREAM argument is evaluated more than once.  Returns 0 on
   success, -1 on error. */

#define Lstream_putc(stream, c)						 \
/* Call the function equivalent if the out buffer is full.  Otherwise,	 \
   add to the end of the out buffer and, if line buffering is called for \
   and the character marks the end of a line, write out the buffer. */	 \
  ((stream)->out_buffer_ind >= (stream)->out_buffer_size ?		 \
   Lstream_fputc (stream, c) :						 \
   ((stream)->out_buffer[(stream)->out_buffer_ind++] =			 \
    (unsigned char) (c),						 \
    (stream)->byte_count++,						 \
    (stream)->buffering == LSTREAM_LINE_BUFFERED &&			 \
    (stream)->out_buffer[(stream)->out_buffer_ind - 1] == '\n' ?	 \
    Lstream_flush_out (stream) : 0))

/* Lstream_getc: Read one byte from the stream and returns it as an
   unsigned char cast to an int, or EOF on end of file or error.  This
   is a macro and so it is very efficient.  The STREAM argument is
   evaluated more than once. */

#define Lstream_getc(stream)						\
/* Retrieve from unget buffer if there are any characters there;	\
   else retrieve from in buffer if there's anything there;		\
   else call the function equivalent */					\
  ((stream)->unget_buffer_ind > 0 ?					\
   ((stream)->byte_count++,						\
    (stream)->unget_buffer[--(stream)->unget_buffer_ind]) :		\
   (stream)->in_buffer_ind < (stream)->in_buffer_current ?		\
    ((stream)->byte_count++,						\
     (stream)->in_buffer[(stream)->in_buffer_ind++]) :			\
    Lstream_fgetc (stream))

/* Lstream_ungetc: Push one byte back onto the input queue, cast to
   unsigned char.  This will be the next byte read from the stream.
   Any number of bytes can be pushed back and will be read in the
   reverse order they were pushed back -- most recent first. (This is
   necessary for consistency -- if there are a number of bytes that
   have been unread and I read and unread a byte, it needs to be the
   first to be read again.) */

DECLARE_INLINE_HEADER (
void
Lstream_ungetc (Lstream *lstr, int c)
)
{
  /* Add to the end if it won't overflow buffer; otherwise call the
     function equivalent */
  if (lstr->unget_buffer_ind >= lstr->unget_buffer_size)
    {
      Lstream_fungetc (lstr, c);
    }
  else
    {
      lstr->byte_count--;
      lstr->unget_buffer[lstr->unget_buffer_ind] = (unsigned char) (c);
      lstr->unget_character_count
        += valid_ibyteptr_p (lstr->unget_buffer + lstr->unget_buffer_ind);
      lstr->unget_buffer_ind++;
    }
}

/* Rawbyte *, not void *, access through void * is undefined under
   strict-aliasing rules. */
#define Lstream_data(stream) ((Rawbyte *) ((stream)->data))
#define Lstream_byte_count(stream) ((stream)->byte_count)

struct extent_info *Lstream_extent_info (Lstream *stream);


/************************************************************************/
/*             working with an Lstream as a stream of Ichars           */
/************************************************************************/

#ifdef MULE

DECLARE_INLINE_HEADER (
Ichar
Lstream_get_ichar (Lstream *stream)
)
{
  int c = Lstream_getc (stream);
  return (c < 0x80		/* c == EOF || byte_ascii_p (c) */
	  ? (Ichar) c
	  : Lstream_get_ichar_1 (stream, c));
}

/* Write an Ichar to a stream.  Return value is 0 for success, -1 for
   failure. */

DECLARE_INLINE_HEADER (
int
Lstream_put_ichar (Lstream *stream, Ichar ch)
)
{
  return ichar_ascii_p (ch) ?
    Lstream_putc (stream, ch) :
    Lstream_fput_ichar (stream, ch);
}

DECLARE_INLINE_HEADER (
void
Lstream_unget_ichar (Lstream *stream, Ichar ch)
)
{
  if (ichar_ascii_p (ch))
    Lstream_ungetc (stream, ch);
  else
    Lstream_funget_ichar (stream, ch);
}
#else /* not MULE */

# define Lstream_get_ichar(stream) Lstream_getc (stream)
# define Lstream_put_ichar(stream, ch) Lstream_putc (stream, ch)
# define Lstream_unget_ichar(stream, ch) Lstream_ungetc (stream, ch)

#endif /* not MULE */

/************************************************************************/
/*                        Lstream implementations                       */
/************************************************************************/

/* Flags we can pass to the filedesc and stdio streams. */

/* If set, close the descriptor or FILE * when the stream is closed. */
#define LSTR_CLOSING		(1 << 16)

/* If set, allow quitting out of the actual I/O. */
#define LSTR_ALLOW_QUIT		(1 << 17)

/* If set and filedesc_stream_set_pty_flushing() has been called
   on the stream, do not send more than pty_max_bytes on a single
   line without flushing the data out using the eof_char. */
#define LSTR_PTY_FLUSHING	(1 << 18)

/* If set, an EWOULDBLOCK error is not treated as an error but
   simply causes the write function to return 0 as the number
   of bytes written out. */
#define LSTR_BLOCKED_OK		(1 << 19)

Lisp_Object make_stdio_input_stream (FILE *stream, int flags);
Lisp_Object make_stdio_output_stream (FILE *stream, int flags);
Lisp_Object make_filedesc_input_stream (int filedesc, OFF_T offset,
                                        Bytecount count, int flags,
                                        tls_state_t *state);
Lisp_Object make_filedesc_output_stream (int filedesc, OFF_T offset,
                                         Bytecount count, int flags,
                                         tls_state_t *state);
void filedesc_stream_set_pty_flushing (Lstream *stream,
				       int pty_max_bytes,
				       Ibyte eof_char);
int filedesc_stream_fd (Lstream *stream);
Lisp_Object make_lisp_string_input_stream (Lisp_Object string,
					   Bytecount offset,
					   Bytecount len);
Lisp_Object make_fixed_buffer_input_stream (const void *buf,
					    Bytecount size);
Lisp_Object make_fixed_buffer_output_stream (void *buf,
					     Bytecount size);
const Ibyte *fixed_buffer_input_stream_ptr (Lstream *stream);
Ibyte *fixed_buffer_output_stream_ptr (Lstream *stream);
Lisp_Object make_resizing_buffer_output_stream (void);
const Ibyte *resizing_buffer_stream_ptr (Lstream *stream);
Lisp_Object resizing_buffer_to_lisp_string (Lstream *stream);
Lisp_Object make_dynarr_output_stream (unsigned_char_dynarr *dyn);


/* Flags we can pass to lisp buffer streams. */

#define LSTR_SELECTIVE		(1 << 16)
#define LSTR_IGNORE_ACCESSIBLE	(1 << 17)

Lisp_Object make_lisp_buffer_input_stream (struct buffer *buf, Charbpos start,
					   Charbpos end, int flags);
Lisp_Object make_lisp_buffer_output_stream (struct buffer *buf, Charbpos pos,
					    int flags);
Charbpos lisp_buffer_stream_startpos (Lstream *stream);

#ifdef EXPOSE_FIXED_BUFFER_INTERNALS

/* These internals are exposed for the sake of emacs_vsnprintf (), which needs
   to function as well as possible when the heap and/or Lisp object allocation
   are no longer working. As such, it creates its lstream on the stack. No
   other code should be doing this. */

DECLARE_LSTREAM (fixed_buffer);

struct fixed_buffer_stream
{
  const Ibyte *inbuf;
  Ibyte *outbuf;
  Bytecount size;
  Bytecount offset;
};

#define FIXED_BUFFER_STREAM_DATA(stream) \
  LSTREAM_TYPE_DATA (stream, fixed_buffer)

#define DECLARE_STACK_FIXED_BUFFER_LSTREAM(lname)                       \
  union                                                                 \
  {                                                                     \
    struct lstream l;                                                   \
    /* Make sure we have enough stack space for the type-specific       \
       data. */                                                         \
    Rawbyte s[offsetof (struct lstream, data) +                         \
              sizeof (struct fixed_buffer_stream) ];                    \
  } lname##u;                                                           \
  Lisp_Object lname = wrap_pointer_1 (&(lname##u.l))                    \

#define INIT_STACK_FIXED_BUFFER_OUTPUT_STREAM(lname, buf, bsize) do     \
    {                                                                   \
      memset (lname##u.s, 0, max (sizeof (lname##u.s),                  \
                                  sizeof (lname##u.l)));                \
      set_lheader_implementation ((struct lrecord_header *)&(lname##u.l), \
                                  &lrecord_lstream);                    \
      lname##u.l.imp = lstream_fixed_buffer;                            \
      Lstream_set_buffering (&(lname##u.l), LSTREAM_UNBUFFERED, 0);     \
      lname##u.l.flags = LSTR_IS_OPEN;                                  \
      lname##u.l.flags |= LSTR_WRITE;                                   \
      FIXED_BUFFER_STREAM_DATA (&(lname##u.l))->outbuf = buf;           \
      FIXED_BUFFER_STREAM_DATA (&(lname##u.l))->size = bsize;           \
    } while (0)

#endif

#endif /* INCLUDED_lstream_h_ */
