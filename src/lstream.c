/* Generic stream implementation.
   Copyright (C) 1995 Free Software Foundation, Inc.
   Copyright (C) 1995 Sun Microsystems, Inc.
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

/* Written by Ben Wing. */

#include <config.h>
#include "lisp.h"

#include "buffer.h"
#include "insdel.h"
#include "lstream.h"

#include "sysfile.h"

/* This module provides a generic buffering stream implementation.
   Conceptually, you send data to the stream or read data from the
   stream, not caring what's on the other end of the stream.  The
   other end could be another stream, a file descriptor, a stdio
   stream, a fixed block of memory, a reallocating block of memory,
   etc.  The main purpose of the stream is to provide a standard
   interface and to do buffering.  Macros are defined to read
   or write characters, so the calling functions do not have to
   worry about blocking data together in order to achieve efficiency.

   Note that this object is called "stream" in Lisp but "lstream"
   in C.  The reason for this is that "stream" is too generic a name
   for C; too much likelihood of conflict/confusion with C++, etc. */

#define DEFAULT_BLOCK_BUFFERING_SIZE 512
#define MAX_READ_SIZE 512

static Lisp_Object
mark_lstream (Lisp_Object obj)
{
  Lstream *lstr = XLSTREAM (obj);
  return lstr->imp->marker ? (lstr->imp->marker) (obj) : Qnil;
}

static void
print_lstream (Lisp_Object obj, Lisp_Object printcharfun, int escapeflag)
{
  Lstream *lstr = XLSTREAM (obj);

  write_fmt_string (printcharfun,
		    "#<INTERNAL OBJECT (XEmacs bug?) (%s lstream) 0x%lx>",
		    lstr->imp->name, (long) lstr);
}

static void
finalize_lstream (void *header, int for_disksave)
{
  /* WARNING WARNING WARNING.  This function (and all finalize functions)
     may get called more than once on the same object, and may get called
     (at dump time) on objects that are not being released. */
  Lstream *lstr = (Lstream *) header;

#if 0 /* this may cause weird Broken Pipes? */
  if (for_disksave)
    {
      Lstream_pseudo_close (lstr);
      return;
    }
#endif
  if (lstr->flags & LSTREAM_FL_IS_OPEN)
    {
      if (for_disksave)
	{
	  if (lstr->flags & LSTREAM_FL_CLOSE_AT_DISKSAVE)
	    Lstream_close (lstr);
	}
      else
	/* Just close. */
	Lstream_close (lstr);
    }

  if (!for_disksave)
    {
      if (lstr->imp->finalizer)
	(lstr->imp->finalizer) (lstr);
    }
}

inline static Bytecount
aligned_sizeof_lstream (Bytecount lstream_type_specific_size)
{
  return MAX_ALIGN_SIZE (offsetof (Lstream, data) +
			 lstream_type_specific_size);
}

static Bytecount
sizeof_lstream (const void *header)
{
  return aligned_sizeof_lstream (((const Lstream *) header)->imp->size);
}

DEFINE_LRECORD_SEQUENCE_IMPLEMENTATION ("stream", lstream,
					mark_lstream, print_lstream,
					finalize_lstream, 0, 0, 0,
					sizeof_lstream, Lstream);


/* Change the buffering of a stream.  See lstream.h.  By default the
   buffering is STREAM_BLOCK_BUFFERED. */

void
Lstream_set_buffering (Lstream *lstr, Lstream_buffering buffering,
		       int buffering_size)
{
  lstr->buffering = buffering;
  switch (buffering)
    {
    case LSTREAM_UNBUFFERED:
      lstr->buffering_size = 0; break;
    case LSTREAM_BLOCK_BUFFERED:
      lstr->buffering_size = DEFAULT_BLOCK_BUFFERING_SIZE; break;
    case LSTREAM_BLOCKN_BUFFERED:
      lstr->buffering_size = buffering_size; break;
    case LSTREAM_LINE_BUFFERED:
    case LSTREAM_UNLIMITED:
      lstr->buffering_size = INT_MAX; break;
    }
}

static const Lstream_implementation *lstream_types[32];
static Lisp_Object Vlstream_free_list[32];
static int lstream_type_count;

/* Allocate and return a new Lstream.  This function is not really
   meant to be called directly; rather, each stream type should
   provide its own stream creation function, which creates the stream
   and does any other necessary creation stuff (e.g. opening a
   file). */

Lstream *
Lstream_new (const Lstream_implementation *imp, const char *mode)
{
  Lstream *p;
  int i;

  for (i = 0; i < lstream_type_count; i++)
    {
      if (lstream_types[i] == imp)
	break;
    }

  if (i == lstream_type_count)
    {
      assert (lstream_type_count < countof (lstream_types));
      lstream_types[lstream_type_count] = imp;
      Vlstream_free_list[lstream_type_count] =
	make_lcrecord_list (aligned_sizeof_lstream (imp->size),
			    &lrecord_lstream);
      lstream_type_count++;
    }

  p = XLSTREAM (allocate_managed_lcrecord (Vlstream_free_list[i]));
  /* Zero it out, except the header. */
  memset ((char *) p + sizeof (p->header), '\0',
	  aligned_sizeof_lstream (imp->size) - sizeof (p->header));
  p->imp = imp;
  Lstream_set_buffering (p, LSTREAM_BLOCK_BUFFERED, 0);
  p->flags = LSTREAM_FL_IS_OPEN;

  /* convert mode (one of "r", "w", "rc", "wc") to p->flags */
  assert (mode[0] == 'r' || mode[0] == 'w');
  assert (mode[1] == 'c' || mode[1] == '\0');
  p->flags |= (mode[0] == 'r' ? LSTREAM_FL_READ : LSTREAM_FL_WRITE);
  if (mode[1] == 'c')
    p->flags |= LSTREAM_FL_NO_PARTIAL_CHARS;

  return p;
}

/* Set or unset "character mode" on the stream.  The basic idea is that,
   assuming valid internal-format data is passing through the stream and
   we're processing the data character by character, we don't want partial
   characters at the end of the data. (No partial characters at the
   beginning happens naturally if we eliminate partial characters at the
   end and the stream is implemented correctly.)

   Character mode actually has two somewhat different meanings, depending
   on whether this is a read stream or write stream.  If a read stream,
   character mode means that data returned from calling Lstream_read() on
   the stream will contain only full characters.  If a write stream,
   character mode means that data passed to the write method in the stream
   implementation will contain only full characters.  It's important to
   note the non-parallelism in who should set this mode on the stream: The
   *CALLER* sets character mode on read streams it creates; the *STREAM
   ITSELF* sets character mode on write streams, typically at creation
   time.

   (However, if a read stream always generates internal-format data, then
   the callers will almost always want character mode, and it's allowed to
   set this on behalf of the caller, as long as a flag can be provided at
   creation time to disable this behavior.) */

void
Lstream_set_character_mode (Lstream *lstr)
{
  lstr->flags |= LSTREAM_FL_NO_PARTIAL_CHARS;
}

/* Unset character mode.  See Lstream_set_character_mode(). */

void
Lstream_unset_character_mode (Lstream *lstr)
{
  lstr->flags &= ~LSTREAM_FL_NO_PARTIAL_CHARS;
}

/* Close the stream (if it's open), and free all memory associated with the
   stream.  Put the stream on a free list; later calls to create a new
   stream of this type may reuse this stream.  Calling this is not strictly
   necessary, but it is much more efficient than having the Lstream be
   garbage-collected.  Be VERY VERY SURE there are no pointers to this
   object hanging around anywhere where they might be used!  When streams
   are chained together, be VERY CAREFUL of the order in which you delete
   them! (e.g. if the streams are in a singly-linked list, delete the head
   first; this will close (but check the documentation, e.g. of
   make_coding_input_stream()), and may send data down to the rest.  Then
   proceed to the rest, one by one.  If the chains are in a doubly-linked
   list, close all the streams first (again, from the head to the tail),
   disconnect the back links, then delete starting from the head.  In
   general, it's a good idea to close everything before deleting anything.

   NOTE: DO NOT CALL DURING GARBAGE COLLECTION (e.g. in a finalizer).  You
   will be aborted.  See free_managed_lcrecord(). */

void
Lstream_delete (Lstream *lstr)
{
  int i;
  Lisp_Object val = wrap_lstream (lstr);

  for (i = 0; i < lstream_type_count; i++)
    {
      if (lstream_types[i] == lstr->imp)
	{
	  free_managed_lcrecord (Vlstream_free_list[i], val);
	  return;
	}
    }

  abort ();
}

#define Lstream_internal_error(reason, lstr) \
  signal_error (Qinternal_error, reason, wrap_lstream (lstr))

/* Reopen a closed stream.  This enables I/O on it again.  This is not
   meant to be called except from a wrapper routine that reinitializes
   variables and such -- the close routine may well have freed some
   necessary storage structures, for example. */

void
Lstream_reopen (Lstream *lstr)
{
  if (lstr->flags & LSTREAM_FL_IS_OPEN)
    Lstream_internal_error ("lstream already open", lstr);
  lstr->flags |= LSTREAM_FL_IS_OPEN;
}

/* Try to write as much of DATA as possible to the stream.  Return the
   number of bytes written. */

static int
Lstream_really_write (Lstream *lstr, const unsigned char *data, int size)
{
  Bytecount num_written;
  const unsigned char *orig_data = data;
  int error_occurred = 0;

  while (size > 0)
    {
      if (! (lstr->flags & LSTREAM_FL_IS_OPEN))
	Lstream_internal_error ("lstream not open", lstr);
      if (! (lstr->flags & LSTREAM_FL_WRITE))
	Lstream_internal_error ("lstream not open for writing", lstr);
      if (!lstr->imp->writer)
	Lstream_internal_error ("lstream has no writer", lstr);

      if (lstr->flags & LSTREAM_FL_NO_PARTIAL_CHARS)
	/* It's quite possible for us to get passed an incomplete
	   character at the end.  We need to spit back that
	   incomplete character. */
	{
	  const unsigned char *dataend = data + size - 1;
	  assert (size > 0); /* safety check ... */
	  /* Optimize the most common case. */
	  if (!byte_ascii_p (*dataend))
	    {
	      /* Go back to the beginning of the last (and possibly partial)
		 character, and bump forward to see if the character is
		 complete. */
	      VALIDATE_IBYTEPTR_BACKWARD (dataend);
	      if (dataend + rep_bytes_by_first_byte (*dataend) != data + size)
		/* If not, chop the size down to ignore the last char
		   and stash it away for next time. */
		size = dataend - data;
	      /* If we don't even have one character to write, then just
		 skip out. */
	      if (size == 0)
		break;
	    }
	}

      num_written = (lstr->imp->writer) (lstr, data, size);
      if (num_written == 0)
	/* If nothing got written, then just hold the data.  This may
	   occur, for example, if this stream does non-blocking I/O;
	   the attempt to write the data might have resulted in an
	   EWOULDBLOCK error. */
	break;
      else if (num_written > size)
	abort ();
      else if (num_written > 0)
	{
	  data += num_written;
	  size -= num_written;
	}
      else
	{
	  /* If error, just hold the data, for similar reasons as above. */
	  error_occurred = 1;
	  break;
	}
    }

  if (lstr->imp->flusher)
    error_occurred = (lstr->imp->flusher) (lstr) < 0;

  if (data == orig_data && error_occurred)
    return -1;

  return data - orig_data;
}

/* Attempt to flush out all of the buffered data for writing.  Leaves
   whatever wasn't flushed sitting in the stream's buffers.  Return -1 if
   nothing written and error occurred, 0 otherwise. */

int
Lstream_flush_out (Lstream *lstr)
{
  Bytecount num_written =
    Lstream_really_write (lstr, lstr->out_buffer, lstr->out_buffer_ind);
  if (num_written == lstr->out_buffer_ind)
    {
      lstr->out_buffer_ind = 0;
      return 0;
    }
  else if (num_written > 0)
    {
      memmove (lstr->out_buffer, lstr->out_buffer + num_written,
	       lstr->out_buffer_ind - num_written);
      lstr->out_buffer_ind -= num_written;
      return 0;
    }
  else return num_written;
}

/* Flush out any pending unwritten data in the stream.  Clear any buffered
   input data.  This differs from Lstream_flush_out() in that it also
   clears any unflushable buffered data.  Returns 0 on success, -1 on
   error. */

int
Lstream_flush (Lstream *lstr)
{
  if (Lstream_flush_out (lstr) < 0)
    return -1;

  /* clear out buffered data */
  lstr->in_buffer_current = lstr->in_buffer_ind = 0;
  lstr->unget_buffer_ind = 0;

  return 0;
}

/* We want to add NUM characters.  This function ensures that the
   buffer is large enough for this (per the buffering size specified
   in the stream) and returns the number of characters we can
   actually write.  If FORCE is set, ignore the buffering size
   and go ahead and make space for all the chars even if it exceeds
   the buffering size. (This is used to deal with the possibility
   that the stream writer might refuse to write any bytes now, e.g.
   if it's getting EWOULDBLOCK errors.   We have to keep stocking them
   up until they can be written, so as to avoid losing data.) */

static Bytecount
Lstream_adding (Lstream *lstr, Bytecount num, int force)
{
  Bytecount size = num + lstr->out_buffer_ind;

  if (size <= lstr->out_buffer_size)
    return num;

  /* Maybe chop it down so that we don't buffer more characters
     than our advertised buffering size. */
  if ((size > lstr->buffering_size) && !force)
    {
      size = lstr->buffering_size;
      /* There might be more data buffered than the buffering size. */
      if (size <= lstr->out_buffer_ind)
	return 0;
    }

  DO_REALLOC (lstr->out_buffer, lstr->out_buffer_size, size, unsigned char);

  return size - lstr->out_buffer_ind;
}

/* Like Lstream_write(), but does not handle line-buffering correctly. */

static int
Lstream_write_1 (Lstream *lstr, const void *data, Bytecount size)
{
  const unsigned char *p = (const unsigned char *) data;
  Bytecount off = 0;
  if (! (lstr->flags & LSTREAM_FL_IS_OPEN))
    Lstream_internal_error ("lstream not open", lstr);
  if (! (lstr->flags & LSTREAM_FL_WRITE))
    Lstream_internal_error ("lstream not open for writing", lstr);

  if (lstr->buffering == LSTREAM_UNBUFFERED)
    {
      /* If there is buffered data, it means we ran into blocking
	 errors the previous time and had to buffer our remaining
	 data.  Try to write it now. */
      if (lstr->out_buffer_ind > 0)
	{
	  if (Lstream_flush_out (lstr) < 0)
	    return -1;
	}

      /* If not still blocked, try to write the new data */
      if (lstr->out_buffer_ind == 0)
	{
	  /* we don't need to loop because Lstream_really_write does that
	     for us. */
	  Bytecount num_written = Lstream_really_write (lstr, p, size);
	  if (num_written < 0)
	    return -1;
	  off += num_written;
	}

      /* squirrel away the rest of the data */
      if (off < size)
	{
	  Lstream_adding (lstr, size - off, 1);
	  memcpy (lstr->out_buffer + lstr->out_buffer_ind, p + off,
		  size - off);
	  lstr->out_buffer_ind += size - off;
	}

      lstr->byte_count += size;
      return 0;
    }
  else
    {
      int couldnt_write_last_time = 0;

      while (1)
	{
	  /* Figure out how much we can add to the buffer */
	  Bytecount chunk = Lstream_adding (lstr, size, 0);
	  if (chunk == 0)
	    {
	      if (couldnt_write_last_time)
		/* Ung, we ran out of space and tried to flush
		   the buffer, but it didn't work because the stream
		   writer is refusing to accept any data.  So we
		   just have to squirrel away all the rest of the
		   stuff. */
		chunk = Lstream_adding (lstr, size, 1);
	      else
		couldnt_write_last_time = 1;
	    }
	  /* Do it. */
	  if (chunk > 0)
	    {
	      memcpy (lstr->out_buffer + lstr->out_buffer_ind, p + off, chunk);
	      lstr->out_buffer_ind += chunk;
	      lstr->byte_count     += chunk;
	      size -= chunk;
	      off  += chunk;
	    }
	  /* If the buffer is full and we have more to add, flush it out. */
	  if (size > 0)
	    {
	      if (Lstream_flush_out (lstr) < 0)
		{
		  if (off == 0)
		    return -1;
		  else
		    return 0;
		}
	    }
	  else
	    break;
	}
    }
  return 0;
}

/* Write SIZE bytes of DATA to the stream.  Return value is 0 on success,
   -1 on error.  -1 is only returned when no bytes could be written; if any
   bytes could be written, then 0 is returned and any unwritten bytes are
   buffered and the next call to Lstream_write() will try to write them
   again. (This buffering happens even when the stream's buffering type is
   LSTREAM_UNBUFFERED, and regardless of how much data is passed in or what
   the stream's buffering size was set to. #### There should perhaps be a
   way to control whether this happens.) */

int
Lstream_write (Lstream *lstr, const void *data, Bytecount size)
{
  Bytecount i;
  const unsigned char *p = (const unsigned char *) data;

  /* If the stream is not line-buffered, then we can just call
     Lstream_write_1(), which writes in chunks.  Otherwise, we repeatedly
     call Lstream_putc(), which knows how to handle line buffering.
     Returns 0 on success, -1 on failure. */

  if (size == 0)
    return 0;
  if (lstr->buffering != LSTREAM_LINE_BUFFERED)
    return Lstream_write_1 (lstr, data, size);
  for (i = 0; i < size; i++)
    {
      if (Lstream_putc (lstr, p[i]) < 0)
	break;
    }
  return i == 0 ? -1 : 0;
}

int
Lstream_was_blocked_p (Lstream *lstr)
{
  return lstr->imp->was_blocked_p ? lstr->imp->was_blocked_p (lstr) : 0;
}

static Bytecount
Lstream_raw_read (Lstream *lstr, unsigned char *buffer,
		  Bytecount size)
{
  if (! (lstr->flags & LSTREAM_FL_IS_OPEN))
    Lstream_internal_error ("lstream not open", lstr);
  if (! (lstr->flags & LSTREAM_FL_READ))
    Lstream_internal_error ("lstream not open for reading", lstr);
  if (!lstr->imp->reader)
    Lstream_internal_error ("lstream has no reader", lstr);

  return (lstr->imp->reader) (lstr, buffer, size);
}

/* Assuming the buffer is empty, fill it up again. */

static Bytecount
Lstream_read_more (Lstream *lstr)
{
#if 0
  Bytecount size_needed 
    = max (1, min (MAX_READ_SIZE, lstr->buffering_size));
#else
  /* If someone requested a larger buffer size, so be it! */
  Bytecount size_needed =
    max (1, lstr->buffering_size);
#endif
  Bytecount size_gotten;

  DO_REALLOC (lstr->in_buffer, lstr->in_buffer_size,
	      size_needed, unsigned char);
  size_gotten = Lstream_raw_read (lstr, lstr->in_buffer, size_needed);
  lstr->in_buffer_current = max (0, size_gotten);
  lstr->in_buffer_ind = 0;
  return size_gotten < 0 ? -1 : size_gotten;
}

/* Read SIZE bytes of DATA from the stream.  Return the number of bytes
   read.  0 means EOF (#### sometimes; it may simply indicate we can't read
   any data at other times, particularly if SIZE is too small.  this needs
   to be fixed!). -1 means an error occurred and no bytes were read. */

static Bytecount
Lstream_read_1 (Lstream *lstr, void *data, Bytecount size,
		int override_no_partial_chars)
{
  unsigned char *p = (unsigned char *) data;
  Bytecount off = 0;
  Bytecount chunk;
  int error_occurred = 0;

  if (size == 0)
    return 0;

  /* First try to get some data from the unget buffer */
  chunk = min (size, lstr->unget_buffer_ind);
  if (chunk > 0)
    {
      /* The bytes come back in reverse order. */
      for (; off < chunk; off++)
	p[off] = lstr->unget_buffer[--lstr->unget_buffer_ind];
      lstr->byte_count += chunk;
      size -= chunk;
    }

  while (size > 0)
    {
      /* If unbuffered, then simply read directly into output buffer.
	 No need to copy. */
      if (lstr->buffering == LSTREAM_UNBUFFERED)
	{
	  chunk = Lstream_raw_read (lstr, p + off, size);
	  if (chunk < 0)
	    error_occurred = 1;
	  if (chunk <= 0)
	    break;
          lstr->byte_count += chunk;
          size -= chunk;
          off += chunk;
	}
      else
	{
	  /* Take whatever we can from the in buffer */
	  chunk = min (size, lstr->in_buffer_current - lstr->in_buffer_ind);
	  if (chunk > 0)
	    {
	      memcpy (p + off, lstr->in_buffer + lstr->in_buffer_ind, chunk);
	      lstr->in_buffer_ind += chunk;
	      lstr->byte_count += chunk;
	      size -= chunk;
	      off += chunk;
	    }

	  /* If we need some more, try to get some more from the
             stream's end */
	  if (size > 0)
	    {
	      Bytecount retval = Lstream_read_more (lstr);
	      if (retval < 0)
		error_occurred = 1;
	      if (retval <= 0)
		break;
	    }
	}
    }

  if ((lstr->flags & LSTREAM_FL_NO_PARTIAL_CHARS) &&
      !override_no_partial_chars)
    {
      /* It's quite possible for us to get passed an incomplete
	 character at the end.  We need to spit back that
	 incomplete character. */
      Bytecount newoff = validate_ibyte_string_backward (p, off);
      if (newoff < off)
	{
	  Lstream_unread (lstr, p + newoff, off - newoff);
	  off = newoff;
	}
    }

  return off == 0 && error_occurred ? -1 : off;
}

Bytecount
Lstream_read (Lstream *lstr, void *data, Bytecount size)
{
  return Lstream_read_1 (lstr, data, size, 0);
}


/* Push back SIZE bytes of DATA onto the input queue.  The next call
   to Lstream_read() with the same size will read the same bytes back.
   Note that this will be the case even if there is other pending
   unread data. */

void
Lstream_unread (Lstream *lstr, const void *data, Bytecount size)
{
  const unsigned char *p = (const unsigned char *) data;

  /* Make sure buffer is big enough */
  DO_REALLOC (lstr->unget_buffer, lstr->unget_buffer_size,
	      lstr->unget_buffer_ind + size, unsigned char);

  lstr->byte_count -= size;

  /* Bytes have to go on in reverse order -- they are reversed
     again when read back. */
  while (size--)
    lstr->unget_buffer[lstr->unget_buffer_ind++] = p[size];
}

/* Rewind the stream to the beginning. */

int
Lstream_rewind (Lstream *lstr)
{
  if (!lstr->imp->rewinder)
    Lstream_internal_error ("lstream has no rewinder", lstr);
  if (Lstream_flush (lstr) < 0)
    return -1;
  lstr->byte_count = 0;
  return (lstr->imp->rewinder) (lstr);
}

int
Lstream_seekable_p (Lstream *lstr)
{
  if (!lstr->imp->rewinder)
    return 0;
  if (!lstr->imp->seekable_p)
    return 1;
  return (lstr->imp->seekable_p) (lstr);
}

static int
Lstream_pseudo_close (Lstream *lstr)
{
  if (!lstr->flags & LSTREAM_FL_IS_OPEN)
    Lstream_internal_error ("lstream is not open", lstr);

  /* don't check errors here -- best not to risk file descriptor loss */
  return Lstream_flush (lstr);
}

/* Close the stream.  All data will be flushed out.  If the stream is
   already closed, nothing happens.  Note that, even if all data has
   already been flushed out, the act of closing a stream may generate more
   data -- for example, if the stream implements some sort of conversion,
   such as gzip, there may be special "end-data" that need to be written
   out when the file is closed. */

int
Lstream_close (Lstream *lstr)
{
  int rc = 0;

  if (lstr->flags & LSTREAM_FL_IS_OPEN)
    {
      rc = Lstream_pseudo_close (lstr);
      /*
       * We used to return immediately if the closer method reported
       * failure, leaving the stream open.  But this is no good, for
       * the following reasons.
       *
       * 1. The finalizer method used in GC makes no provision for
       *    failure, so we must not return without freeing buffer
       *    memory.
       *
       * 2. The closer method may have already freed some memory
       *    used for I/O in this stream.  E.g. encoding_closer frees
       *    ENCODING_STREAM_DATA(stream)->runoff.  If a writer method
       *    tries to use this buffer later, it will write into memory
       *    that may have been allocated elsewhere.  Sometime later
       *    you will see a sign that says "Welcome to Crash City."
       *
       * 3. The closer can report failure if a flush fails in the
       *    other stream in a MULE encoding/decoding stream pair.
       *    The other stream in the pair is closed, but returning
       *    early leaves the current stream open.  If we try to
       *    flush the current stream later, we will crash when the
       *    flusher notices that the other end stream is closed.
       *
       * So, we no longer abort the close if the closer method
       * reports some kind of failure.  We still report the failure
       * to the caller.
       */
      if (lstr->imp->closer)
	if ((lstr->imp->closer) (lstr) < 0)
	  rc = -1;
    }

  lstr->flags &= ~LSTREAM_FL_IS_OPEN;
  lstr->byte_count = 0;
  /* Note that Lstream_flush() reset all the buffer indices.  That way,
     the next call to Lstream_putc(), Lstream_getc(), or Lstream_ungetc()
     on a closed stream will call into the function equivalents, which will
     cause an error. */

  /* We set the pointers to 0 so that we don't lose when this function
     is called more than once on the same object */
  if (lstr->out_buffer)
    {
      xfree (lstr->out_buffer);
      lstr->out_buffer = 0;
    }
  if (lstr->in_buffer)
    {
      xfree (lstr->in_buffer);
      lstr->in_buffer = 0;
    }
  if (lstr->unget_buffer)
    {
      xfree (lstr->unget_buffer);
      lstr->unget_buffer = 0;
    }

  return rc;
}


/* Function equivalent of Lstream_putc(). */

int
Lstream_fputc (Lstream *lstr, int c)
{
  unsigned char ch = (unsigned char) c;
  int retval = Lstream_write_1 (lstr, &ch, 1);
  if (retval == 0 && lstr->buffering == LSTREAM_LINE_BUFFERED && ch == '\n')
    return Lstream_flush_out (lstr);
  return retval;
}

/* Function equivalent of Lstream_getc(). */

int
Lstream_fgetc (Lstream *lstr)
{
  unsigned char ch;
  if (Lstream_read_1 (lstr, &ch, 1, 1) <= 0)
    return -1;
  return ch;
}

/* Function equivalent of Lstream_ungetc(). */

void
Lstream_fungetc (Lstream *lstr, int c)
{
  unsigned char ch = (unsigned char) c;
  Lstream_unread (lstr, &ch, 1);
}


/************************ some stream implementations *********************/

/*********** a stdio stream ***********/

struct stdio_stream
{
  FILE *file;
  int closing;
};

#define STDIO_STREAM_DATA(stream) LSTREAM_TYPE_DATA (stream, stdio)

DEFINE_LSTREAM_IMPLEMENTATION ("stdio", stdio);

static Lisp_Object
make_stdio_stream_1 (FILE *stream, int flags, const char *mode)
{
  Lstream *lstr = Lstream_new (lstream_stdio, mode);
  struct stdio_stream *str = STDIO_STREAM_DATA (lstr);
  str->file = stream;
  str->closing = flags & LSTR_CLOSING;
  lstr->flags |= LSTREAM_FL_CLOSE_AT_DISKSAVE;
  return wrap_lstream (lstr);
}

Lisp_Object
make_stdio_input_stream (FILE *stream, int flags)
{
  return make_stdio_stream_1 (stream, flags, "r");
}

Lisp_Object
make_stdio_output_stream (FILE *stream, int flags)
{
  return make_stdio_stream_1 (stream, flags, "w");
}

/* #### From reading the Unix 98 specification, it appears that if we
   want stdio_reader() to be completely correct, we should check for
   0 < val < size and if so, check to see if an error has occurred.
   If an error has occurred, but val is non-zero, we should go ahead
   and act as if the read was successful, but remember in some fashion
   or other, that an error has occurred, and report that on the next
   call to stdio_reader instead of calling retry_fread() again.

   Currently, in such a case, we end up calling retry_fread() twice and we
   assume that

   1) this is not harmful, and
   2) the error will still be reported on the second read.

   This is probably reasonable, so I don't think we should change this
   code (it could even be argued that the error might have fixed
   itself, so we should do the retry_fread() again.  */

static Bytecount
stdio_reader (Lstream *stream, unsigned char *data, Bytecount size)
{
  struct stdio_stream *str = STDIO_STREAM_DATA (stream);
  Bytecount val = retry_fread (data, 1, size, str->file);
  if (!val)
    {
      if (ferror (str->file))
	return LSTREAM_ERROR;
      if (feof (str->file))
	return 0; /* LSTREAM_EOF; */
    }
  return val;
}

static Bytecount
stdio_writer (Lstream *stream, const unsigned char *data,
	      Bytecount size)
{
  struct stdio_stream *str = STDIO_STREAM_DATA (stream);
  Bytecount val = retry_fwrite (data, 1, size, str->file);
  if (!val && ferror (str->file))
    return LSTREAM_ERROR;
  return val;
}

static int
stdio_rewinder (Lstream *stream)
{
  rewind (STDIO_STREAM_DATA (stream)->file);
  return 0;
}

static int
stdio_seekable_p (Lstream *stream)
{
  struct stat lestat;
  struct stdio_stream *str = STDIO_STREAM_DATA (stream);

  if (qxe_fstat (fileno (str->file), &lestat) < 0)
    return 0;
  return S_ISREG (lestat.st_mode);
}

static int
stdio_flusher (Lstream *stream)
{
  struct stdio_stream *str = STDIO_STREAM_DATA (stream);
  if (stream->flags & LSTREAM_FL_WRITE)
    return fflush (str->file);
  else
    return 0;
}

static int
stdio_closer (Lstream *stream)
{
  struct stdio_stream *str = STDIO_STREAM_DATA (stream);
  if (str->closing)
    return retry_fclose (str->file);
  else
  if (stream->flags & LSTREAM_FL_WRITE)
    return fflush (str->file);
  else
    return 0;
}

/*********** a file descriptor ***********/

struct filedesc_stream
{
  int fd;
  int pty_max_bytes;
  Ibyte eof_char;
  int starting_pos;
  int current_pos;
  int end_pos;
  int chars_sans_newline;
  unsigned int closing :1;
  unsigned int allow_quit :1;
  unsigned int blocked_ok :1;
  unsigned int pty_flushing :1;
  unsigned int blocking_error_p :1;
};

#define FILEDESC_STREAM_DATA(stream) LSTREAM_TYPE_DATA (stream, filedesc)

DEFINE_LSTREAM_IMPLEMENTATION ("filedesc", filedesc);

/* Make a stream that reads from or writes to a file descriptor FILEDESC.
   OFFSET is the offset from the *current* file pointer that the reading
   should start at.  COUNT is the number of bytes to be read (it is
   ignored when writing); -1 for unlimited. */
static Lisp_Object
make_filedesc_stream_1 (int filedesc, int offset, int count, int flags,
			const char *mode)
{
  Lstream *lstr = Lstream_new (lstream_filedesc, mode);
  struct filedesc_stream *fstr = FILEDESC_STREAM_DATA (lstr);
  fstr->fd = filedesc;
  fstr->closing      = !!(flags & LSTR_CLOSING);
  fstr->allow_quit   = !!(flags & LSTR_ALLOW_QUIT);
  fstr->blocked_ok   = !!(flags & LSTR_BLOCKED_OK);
  fstr->pty_flushing = !!(flags & LSTR_PTY_FLUSHING);
  fstr->blocking_error_p = 0;
  fstr->chars_sans_newline = 0;
  fstr->starting_pos = lseek (filedesc, offset, SEEK_CUR);
  fstr->current_pos = max (fstr->starting_pos, 0);
  if (count < 0)
    fstr->end_pos = -1;
  else
    fstr->end_pos = fstr->starting_pos + count;
  lstr->flags |= LSTREAM_FL_CLOSE_AT_DISKSAVE;
  return wrap_lstream (lstr);
}

/* Flags:
   
   LSTR_CLOSING
   If set, close the descriptor or FILE * when the stream is closed.

   LSTR_ALLOW_QUIT
   If set, allow quitting out of the actual I/O.

   LSTR_PTY_FLUSHING
   If set and filedesc_stream_set_pty_flushing() has been called
   on the stream, do not send more than pty_max_bytes on a single
   line without flushing the data out using the eof_char.

   LSTR_BLOCKED_OK
   If set, an EWOULDBLOCK error is not treated as an error but
   simply causes the write function to return 0 as the number
   of bytes written out.
 */

Lisp_Object
make_filedesc_input_stream (int filedesc, int offset, int count, int flags)
{
  return make_filedesc_stream_1 (filedesc, offset, count, flags, "r");
}

Lisp_Object
make_filedesc_output_stream (int filedesc, int offset, int count, int flags)
{
  return make_filedesc_stream_1 (filedesc, offset, count, flags, "w");
}

static Bytecount
filedesc_reader (Lstream *stream, unsigned char *data, Bytecount size)
{
  Bytecount nread;
  struct filedesc_stream *str = FILEDESC_STREAM_DATA (stream);
  if (str->end_pos >= 0)
    size = min (size, (Bytecount) (str->end_pos - str->current_pos));
  nread = str->allow_quit ?
    read_allowing_quit (str->fd, data, size) :
    retry_read (str->fd, data, size);
  if (nread > 0)
    str->current_pos += nread;
  if (nread == 0)
    return 0; /* LSTREAM_EOF; */
  if (nread < 0)
    return LSTREAM_ERROR;
  return nread;
}

static int
errno_would_block_p (int val)
{
#ifdef EWOULDBLOCK
  if (val == EWOULDBLOCK)
    return 1;
#endif
#ifdef EAGAIN
  if (val == EAGAIN)
    return 1;
#endif
  return 0;
}

static Bytecount
filedesc_writer (Lstream *stream, const unsigned char *data,
		 Bytecount size)
{
  struct filedesc_stream *str = FILEDESC_STREAM_DATA (stream);
  Bytecount retval;
  int need_newline = 0;

  /* This function would be simple if it were not for the blasted
     PTY max-bytes stuff.  Why the hell can't they just have written
     the PTY drivers right so this problem doesn't exist?

     Maybe all the PTY crap here should be moved into another stream
     that does nothing but periodically insert EOF's as necessary. */
  if (str->pty_flushing)
    {
      /* To make life easy, only send out one line at the most. */
      const unsigned char *ptr;

      ptr = (const unsigned char *) memchr (data, '\n', size);
      if (ptr)
	need_newline = 1;
      else
	ptr = data + size;
      if (ptr - data >= str->pty_max_bytes - str->chars_sans_newline)
	{
	  ptr = data + str->pty_max_bytes - str->chars_sans_newline;
	  need_newline = 0;
	}
      size = ptr - data;
    }

  /**** start of non-PTY-crap ****/
  if (size > 0)
    retval = str->allow_quit ?
      write_allowing_quit (str->fd, data, size) :
      retry_write (str->fd, data, size);
  else
    retval = 0;
  if (retval < 0 && errno_would_block_p (errno) && str->blocked_ok)
    {
      str->blocking_error_p = 1;
      return 0;
    }
  str->blocking_error_p = 0;
  if (retval < 0)
    return LSTREAM_ERROR;
  /**** end non-PTY-crap ****/

  if (str->pty_flushing)
    {
      str->chars_sans_newline += retval;
      /* Note that a newline was not among the bytes written out.
	 Add to the number of non-newline bytes written out,
	 and flush with an EOF if necessary.  Be careful to
	 keep track of write errors as we go along and look
	 out for EWOULDBLOCK. */
      if (str->chars_sans_newline >= str->pty_max_bytes)
	{
	  Bytecount retval2 = str->allow_quit ?
	    write_allowing_quit (str->fd, &str->eof_char, 1) :
	    retry_write (str->fd, &str->eof_char, 1);

	  if (retval2 > 0)
	    str->chars_sans_newline = 0;
	  else if (retval2 < 0)
	    {
	      /* Error writing the EOF char.  If nothing got written,
		 then treat this as an error -- either return an error
		 condition or set the blocking-error flag. */
	      if (retval == 0)
		{
		  if (errno_would_block_p (errno) && str->blocked_ok)
		    {
		      str->blocking_error_p = 1;
		      return 0;
		    }
		  else
		    return LSTREAM_ERROR;
		}
	      else
		return retval;
	    }
	}
    }

  /* The need_newline flag is necessary because otherwise when the
     first byte is a newline, we'd get stuck never writing anything
     in pty-flushing mode. */
  if (need_newline)
    {
      Ibyte nl = '\n';
      Bytecount retval2 = str->allow_quit ?
	write_allowing_quit (str->fd, &nl, 1) :
	retry_write (str->fd, &nl, 1);

      if (retval2 > 0)
        {
          str->chars_sans_newline = 0;
          retval++;
        }
      else if (retval2 < 0)
	{
	  /* Error writing the newline char.  If nothing got written,
	     then treat this as an error -- either return an error
	     condition or set the blocking-error flag. */
	  if (retval == 0)
	    {
	      if (errno_would_block_p (errno) && str->blocked_ok)
		{
		  str->blocking_error_p = 1;
		  return 0;
		}
	      else
		return LSTREAM_ERROR;
	    }
	  else
	    return retval;
	}
    }

  return retval;
}

static int
filedesc_rewinder (Lstream *stream)
{
  struct filedesc_stream *str = FILEDESC_STREAM_DATA (stream);
  if (str->starting_pos < 0 ||
      lseek (FILEDESC_STREAM_DATA (stream)->fd, str->starting_pos,
	     SEEK_SET) == -1)
    return -1;
  else
    {
      str->current_pos = str->starting_pos;
      return 0;
    }
}

static int
filedesc_seekable_p (Lstream *stream)
{
  struct filedesc_stream *str = FILEDESC_STREAM_DATA (stream);
  if (str->starting_pos < 0)
    return 0;
  else
    {
      struct stat lestat;

      if (qxe_fstat (str->fd, &lestat) < 0)
        return 0;
      return S_ISREG (lestat.st_mode);
    }
}

static int
filedesc_closer (Lstream *stream)
{
  struct filedesc_stream *str = FILEDESC_STREAM_DATA (stream);
  if (str->closing)
    return retry_close (str->fd);
  else
    return 0;
}

static int
filedesc_was_blocked_p (Lstream *stream)
{
  struct filedesc_stream *str = FILEDESC_STREAM_DATA (stream);
  return str->blocking_error_p;
}

void
filedesc_stream_set_pty_flushing (Lstream *stream, int pty_max_bytes,
				  Ibyte eof_char)
{
  struct filedesc_stream *str = FILEDESC_STREAM_DATA (stream);
  str->pty_max_bytes = pty_max_bytes;
  str->eof_char = eof_char;
  str->pty_flushing = 1;
}

int
filedesc_stream_fd (Lstream *stream)
{
  struct filedesc_stream *str = FILEDESC_STREAM_DATA (stream);
  return str->fd;
}

/*********** read from a Lisp string ***********/

#define LISP_STRING_STREAM_DATA(stream) LSTREAM_TYPE_DATA (stream, lisp_string)

struct lisp_string_stream
{
  Lisp_Object obj;
  Bytecount init_offset;
  Bytecount offset, end;
};

DEFINE_LSTREAM_IMPLEMENTATION ("lisp-string", lisp_string);

Lisp_Object
make_lisp_string_input_stream (Lisp_Object string, Bytecount offset,
			       Bytecount len)
{
  Lstream *lstr;
  struct lisp_string_stream *str;

  CHECK_STRING (string);
  if (len < 0)
    len = XSTRING_LENGTH (string) - offset;
  assert (offset >= 0);
  assert (len >= 0);
  assert (offset + len <= XSTRING_LENGTH (string));

  lstr = Lstream_new (lstream_lisp_string, "r");
  str = LISP_STRING_STREAM_DATA (lstr);
  str->offset = offset;
  str->end = offset + len;
  str->init_offset = offset;
  str->obj = string;
  return wrap_lstream (lstr);
}

static Bytecount
lisp_string_reader (Lstream *stream, unsigned char *data,
		    Bytecount size)
{
  struct lisp_string_stream *str = LISP_STRING_STREAM_DATA (stream);
  /* Don't lose if the string shrank past us ... */
  Bytecount offset = min (str->offset, XSTRING_LENGTH (str->obj));
  Ibyte *strstart = XSTRING_DATA (str->obj);
  Ibyte *start = strstart + offset;

  /* ... or if someone changed the string and we ended up in the
     middle of a character. */
  /* Being in the middle of a character is `normal' unless
     LSTREAM_NO_PARTIAL_CHARS - mrb */
  if (stream->flags & LSTREAM_FL_NO_PARTIAL_CHARS)
    VALIDATE_IBYTEPTR_BACKWARD (start);
  offset = start - strstart;
  size = min (size, (Bytecount) (str->end - offset));
  memcpy (data, start, size);
  str->offset = offset + size;
  return size;
}

static int
lisp_string_rewinder (Lstream *stream)
{
  struct lisp_string_stream *str = LISP_STRING_STREAM_DATA (stream);
  int pos = str->init_offset;
  if (pos > str->end)
    pos = str->end;
  /* Don't lose if the string shrank past us ... */
  pos = min (pos, XSTRING_LENGTH (str->obj));
  /* ... or if someone changed the string and we ended up in the
     middle of a character. */
  {
    Ibyte *strstart = XSTRING_DATA (str->obj);
    Ibyte *start = strstart + pos;
    VALIDATE_IBYTEPTR_BACKWARD (start);
    pos = start - strstart;
  }
  str->offset = pos;
  return 0;
}

static Lisp_Object
lisp_string_marker (Lisp_Object stream)
{
  struct lisp_string_stream *str = LISP_STRING_STREAM_DATA (XLSTREAM (stream));
  return str->obj;
}

/*********** a fixed buffer ***********/

#define FIXED_BUFFER_STREAM_DATA(stream) \
  LSTREAM_TYPE_DATA (stream, fixed_buffer)

struct fixed_buffer_stream
{
  const unsigned char *inbuf;
  unsigned char *outbuf;
  Bytecount size;
  Bytecount offset;
};

DEFINE_LSTREAM_IMPLEMENTATION ("fixed-buffer", fixed_buffer);

Lisp_Object
make_fixed_buffer_input_stream (const void *buf, Bytecount size)
{
  Lstream *lstr = Lstream_new (lstream_fixed_buffer, "r");
  struct fixed_buffer_stream *str = FIXED_BUFFER_STREAM_DATA (lstr);
  str->inbuf = (const unsigned char *) buf;
  str->size = size;
  return wrap_lstream (lstr);
}

Lisp_Object
make_fixed_buffer_output_stream (void *buf, Bytecount size)
{
  Lstream *lstr = Lstream_new (lstream_fixed_buffer, "w");
  struct fixed_buffer_stream *str = FIXED_BUFFER_STREAM_DATA (lstr);
  str->outbuf = (unsigned char *) buf;
  str->size = size;
  return wrap_lstream (lstr);
}

static Bytecount
fixed_buffer_reader (Lstream *stream, unsigned char *data,
		     Bytecount size)
{
  struct fixed_buffer_stream *str = FIXED_BUFFER_STREAM_DATA (stream);
  size = min (size, str->size - str->offset);
  memcpy (data, str->inbuf + str->offset, size);
  str->offset += size;
  return size;
}

static Bytecount
fixed_buffer_writer (Lstream *stream, const unsigned char *data,
		     Bytecount size)
{
  struct fixed_buffer_stream *str = FIXED_BUFFER_STREAM_DATA (stream);
  if (str->offset == str->size)
    {
      /* If we're at the end, just throw away the data and pretend
	 we wrote all of it.  If we return 0, then the lstream routines
	 will try again and again to write it out. */
      return size;
    }
  size = min (size, str->size - str->offset);
  memcpy (str->outbuf + str->offset, data, size);
  str->offset += size;
  return size;
}

static int
fixed_buffer_rewinder (Lstream *stream)
{
  FIXED_BUFFER_STREAM_DATA (stream)->offset = 0;
  return 0;
}

const unsigned char *
fixed_buffer_input_stream_ptr (Lstream *stream)
{
  assert (stream->imp == lstream_fixed_buffer);
  return FIXED_BUFFER_STREAM_DATA (stream)->inbuf;
}

unsigned char *
fixed_buffer_output_stream_ptr (Lstream *stream)
{
  assert (stream->imp == lstream_fixed_buffer);
  return FIXED_BUFFER_STREAM_DATA (stream)->outbuf;
}

/*********** write to a resizing buffer ***********/

#define RESIZING_BUFFER_STREAM_DATA(stream) \
  LSTREAM_TYPE_DATA (stream, resizing_buffer)

struct resizing_buffer_stream
{
  unsigned char *buf;
  Bytecount allocked;
  int max_stored;
  int stored;
};

DEFINE_LSTREAM_IMPLEMENTATION ("resizing-buffer", resizing_buffer);

Lisp_Object
make_resizing_buffer_output_stream (void)
{
  return wrap_lstream (Lstream_new (lstream_resizing_buffer, "w"));
}

static Bytecount
resizing_buffer_writer (Lstream *stream, const unsigned char *data,
			Bytecount size)
{
  struct resizing_buffer_stream *str = RESIZING_BUFFER_STREAM_DATA (stream);
  DO_REALLOC (str->buf, str->allocked, str->stored + size, unsigned char);
  memcpy (str->buf + str->stored, data, size);
  str->stored += size;
  str->max_stored = max (str->max_stored, str->stored);
  return size;
}

static int
resizing_buffer_rewinder (Lstream *stream)
{
  RESIZING_BUFFER_STREAM_DATA (stream)->stored = 0;
  return 0;
}

static int
resizing_buffer_closer (Lstream *stream)
{
  struct resizing_buffer_stream *str = RESIZING_BUFFER_STREAM_DATA (stream);
  if (str->buf)
    {
      xfree (str->buf);
      str->buf = 0;
    }
  return 0;
}

unsigned char *
resizing_buffer_stream_ptr (Lstream *stream)
{
  return RESIZING_BUFFER_STREAM_DATA (stream)->buf;
}

Lisp_Object
resizing_buffer_to_lisp_string (Lstream *stream)
{
  return make_string (resizing_buffer_stream_ptr (stream),
		     Lstream_byte_count (stream));
}

/*********** write to an unsigned-char dynarr ***********/

/* Note: If you have a dynarr whose type is not unsigned_char_dynarr
   but which is really just an unsigned_char_dynarr (e.g. its type
   is Ibyte or Extbyte), just cast to unsigned_char_dynarr. */

#define DYNARR_STREAM_DATA(stream) \
  LSTREAM_TYPE_DATA (stream, dynarr)

struct dynarr_stream
{
  unsigned_char_dynarr *dyn;
};

DEFINE_LSTREAM_IMPLEMENTATION ("dynarr", dynarr);

Lisp_Object
make_dynarr_output_stream (unsigned_char_dynarr *dyn)
{
  Lisp_Object obj = wrap_lstream (Lstream_new (lstream_dynarr, "w"));

  DYNARR_STREAM_DATA (XLSTREAM (obj))->dyn = dyn;
  return obj;
}

static Bytecount
dynarr_writer (Lstream *stream, const unsigned char *data,
	       Bytecount size)
{
  struct dynarr_stream *str = DYNARR_STREAM_DATA (stream);
  Dynarr_add_many (str->dyn, data, size);
  return size;
}

static int
dynarr_rewinder (Lstream *stream)
{
  Dynarr_reset (DYNARR_STREAM_DATA (stream)->dyn);
  return 0;
}

static int
dynarr_closer (Lstream *stream)
{
  return 0;
}

/************ read from or write to a Lisp buffer ************/

/* Note: Lisp-buffer read streams never return partial characters,
   and Lisp-buffer write streams expect to never get partial
   characters. */

#define LISP_BUFFER_STREAM_DATA(stream) \
  LSTREAM_TYPE_DATA (stream, lisp_buffer)

struct lisp_buffer_stream
{
  Lisp_Object buffer;
  Lisp_Object orig_start;
  /* we use markers to properly deal with insertion/deletion */
  Lisp_Object start, end;
  int flags;
};

DEFINE_LSTREAM_IMPLEMENTATION ("lisp-buffer", lisp_buffer);

static Lisp_Object
make_lisp_buffer_stream_1 (struct buffer *buf, Charbpos start, Charbpos end,
			   int flags, const Char_ASCII *mode)
{
  Lstream *lstr;
  struct lisp_buffer_stream *str;
  Charbpos bmin, bmax;
  int reading = !strcmp (mode, "r");

  /* Make sure the luser didn't pass "w" in. */
  if (!strcmp (mode, "w"))
    abort ();

  if (flags & LSTR_IGNORE_ACCESSIBLE)
    {
      bmin = BUF_BEG (buf);
      bmax = BUF_Z (buf);
    }
  else
    {
      bmin = BUF_BEGV (buf);
      bmax = BUF_ZV (buf);
    }

  if (start == -1)
    start = bmin;
  if (end == -1)
    end = bmax;
  assert (bmin <= start);
  assert (start <= bmax);
  if (reading)
    {
      assert (bmin  <= end);
      assert (end   <= bmax);
      assert (start <= end);
    }

  lstr = Lstream_new (lstream_lisp_buffer, mode);
  str = LISP_BUFFER_STREAM_DATA (lstr);
  {
    Lisp_Object marker;
    Lisp_Object buffer = wrap_buffer (buf);

    marker = Fmake_marker ();
    Fset_marker (marker, make_int (start), buffer);
    str->start = marker;
    marker = Fmake_marker ();
    Fset_marker (marker, make_int (start), buffer);
    str->orig_start = marker;
    if (reading)
      {
        marker = Fmake_marker ();
        Fset_marker (marker, make_int (end), buffer);
        str->end = marker;
      }
    else
      str->end = Qnil;
    str->buffer = buffer;
  }
  str->flags = flags;
  return wrap_lstream (lstr);
}

Lisp_Object
make_lisp_buffer_input_stream (struct buffer *buf, Charbpos start,
			       Charbpos end, int flags)
{
  return make_lisp_buffer_stream_1 (buf, start, end, flags, "r");
}

Lisp_Object
make_lisp_buffer_output_stream (struct buffer *buf, Charbpos pos, int flags)
{
  Lisp_Object lstr = make_lisp_buffer_stream_1 (buf, pos, 0, flags, "wc");

  Lstream_set_character_mode (XLSTREAM (lstr));
  return lstr;
}

static Bytecount
lisp_buffer_reader (Lstream *stream, Ibyte *data, Bytecount size)
{
  struct lisp_buffer_stream *str = LISP_BUFFER_STREAM_DATA (stream);
  Bytebpos start;
  Bytebpos end;
  struct buffer *buf = XBUFFER (str->buffer);
  Bytecount src_used;

  if (!BUFFER_LIVE_P (buf))
    return 0; /* Fut. */

  start = byte_marker_position (str->start);
  end = byte_marker_position (str->end);
  if (!(str->flags & LSTR_IGNORE_ACCESSIBLE))
    {
      start = bytebpos_clip_to_bounds (BYTE_BUF_BEGV (buf), start,
				       BYTE_BUF_ZV (buf));
      end = bytebpos_clip_to_bounds (BYTE_BUF_BEGV (buf), end,
				     BYTE_BUF_ZV (buf));
    }

  size = copy_buffer_text_out (buf, start, end - start, data, size,
			       FORMAT_DEFAULT, Qnil, &src_used);
  end = start + src_used;

  if (EQ (buf->selective_display, Qt) && str->flags & LSTR_SELECTIVE)
    {
      /* What a kludge.  What a kludge.  What a kludge. */
      Ibyte *p;
      for (p = data; p < data + src_used; p++)
	if (*p == '\r')
	  *p = '\n';
    }

  set_byte_marker_position (str->start, end);
  return size;
}

static Bytecount
lisp_buffer_writer (Lstream *stream, const Ibyte *data,
		    Bytecount size)
{
  struct lisp_buffer_stream *str = LISP_BUFFER_STREAM_DATA (stream);
  Charbpos pos;
  struct buffer *buf = XBUFFER (str->buffer);

  if (!BUFFER_LIVE_P (buf))
    return 0; /* Fut. */

  pos = marker_position (str->start);
  pos += buffer_insert_raw_string_1 (buf, pos, data, size, 0);
  set_marker_position (str->start, pos);
  return size;
}

static int
lisp_buffer_rewinder (Lstream *stream)
{
  struct lisp_buffer_stream *str =
    LISP_BUFFER_STREAM_DATA (stream);
  struct buffer *buf = XBUFFER (str->buffer);
  long pos = marker_position (str->orig_start);
  if (!BUFFER_LIVE_P (buf))
    return -1; /* Fut. */
  if (pos > BUF_ZV (buf))
    pos = BUF_ZV (buf);
  if (pos < marker_position (str->orig_start))
    pos = marker_position (str->orig_start);
  if (MARKERP (str->end) && pos > marker_position (str->end))
    pos = marker_position (str->end);
  set_marker_position (str->start, pos);
  return 0;
}

static Lisp_Object
lisp_buffer_marker (Lisp_Object stream)
{
  struct lisp_buffer_stream *str =
    LISP_BUFFER_STREAM_DATA (XLSTREAM (stream));

  mark_object (str->start);
  mark_object (str->end);
  return str->buffer;
}

Charbpos
lisp_buffer_stream_startpos (Lstream *stream)
{
  return marker_position (LISP_BUFFER_STREAM_DATA (stream)->start);
}


/************************************************************************/
/*                            initialization                            */
/************************************************************************/

void
lstream_type_create (void)
{
  LSTREAM_HAS_METHOD (stdio, reader);
  LSTREAM_HAS_METHOD (stdio, writer);
  LSTREAM_HAS_METHOD (stdio, rewinder);
  LSTREAM_HAS_METHOD (stdio, seekable_p);
  LSTREAM_HAS_METHOD (stdio, flusher);
  LSTREAM_HAS_METHOD (stdio, closer);

  LSTREAM_HAS_METHOD (filedesc, reader);
  LSTREAM_HAS_METHOD (filedesc, writer);
  LSTREAM_HAS_METHOD (filedesc, was_blocked_p);
  LSTREAM_HAS_METHOD (filedesc, rewinder);
  LSTREAM_HAS_METHOD (filedesc, seekable_p);
  LSTREAM_HAS_METHOD (filedesc, closer);

  LSTREAM_HAS_METHOD (lisp_string, reader);
  LSTREAM_HAS_METHOD (lisp_string, rewinder);
  LSTREAM_HAS_METHOD (lisp_string, marker);

  LSTREAM_HAS_METHOD (fixed_buffer, reader);
  LSTREAM_HAS_METHOD (fixed_buffer, writer);
  LSTREAM_HAS_METHOD (fixed_buffer, rewinder);

  LSTREAM_HAS_METHOD (resizing_buffer, writer);
  LSTREAM_HAS_METHOD (resizing_buffer, rewinder);
  LSTREAM_HAS_METHOD (resizing_buffer, closer);

  LSTREAM_HAS_METHOD (dynarr, writer);
  LSTREAM_HAS_METHOD (dynarr, rewinder);
  LSTREAM_HAS_METHOD (dynarr, closer);

  LSTREAM_HAS_METHOD (lisp_buffer, reader);
  LSTREAM_HAS_METHOD (lisp_buffer, writer);
  LSTREAM_HAS_METHOD (lisp_buffer, rewinder);
  LSTREAM_HAS_METHOD (lisp_buffer, marker);
}

void
reinit_vars_of_lstream (void)
{
  int i;

  for (i = 0; i < countof (Vlstream_free_list); i++)
    {
      Vlstream_free_list[i] = Qnil;
      staticpro_nodump (&Vlstream_free_list[i]);
    }
}

void
vars_of_lstream (void)
{
  INIT_LRECORD_IMPLEMENTATION (lstream);

  reinit_vars_of_lstream ();
}
