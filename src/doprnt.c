/* Output like sprintf to a buffer of specified size.
   Also takes args differently: pass one pointer to an array of strings
   in addition to the format string which is separate.
   Copyright (C) 1995 Free Software Foundation, Inc.
   Copyright (C) 2001, 2002 Ben Wing.
   Rewritten by mly to use varargs.h.
   Rewritten from scratch by Ben Wing (February 1995) for Mule; expanded
   to full printf spec.
   Support for bignums, ratios, and bigfloats added April 2004 by Jerry James.

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

/* Synched up with: Rewritten by Ben Wing.  Not in FSF. */

#include <config.h>
#include "lisp.h"

#include "buffer.h"
#include "lstream.h"

static const char * const valid_flags = "-+ #0";
static const char * const valid_converters = "dic" "ouxX" "feEgG" "sS"
#if defined(HAVE_BIGNUM) || defined(HAVE_RATIO)
  "npyY"
#endif
#ifdef HAVE_BIGFLOAT
  "FhHkK"
#endif
  ;
static const char * const int_converters = "dic";
static const char * const unsigned_int_converters = "ouxX";
static const char * const double_converters = "feEgG";
static const char * const string_converters = "sS";
#if defined(HAVE_BIGNUM) || defined(HAVE_RATIO)
static const char * const bignum_converters = "npyY";
#endif
#ifdef HAVE_BIGFLOAT
static const char * const bigfloat_converters = "FhHkK";
#endif

typedef struct printf_spec printf_spec;
struct printf_spec
{
  int argnum; /* which argument does this spec want?  This is one-based:
		 The first argument given is numbered 1, the second
		 is 2, etc.  This is to handle %##$x-type specs. */
  int minwidth;
  int precision;
  unsigned int minus_flag:1;
  unsigned int plus_flag:1;
  unsigned int space_flag:1;
  unsigned int number_flag:1;
  unsigned int zero_flag:1;
  unsigned int h_flag:1;
  unsigned int l_flag:1;
  unsigned int forwarding_precision:1;
  char converter; /* converter character or 0 for dummy marker
		     indicating literal text at the end of the
		     specification */
  Bytecount text_before; /* position of the first character of the
			    block of literal text before this spec */
  Bytecount text_before_len; /* length of that text */
};

typedef union printf_arg printf_arg;
union printf_arg
{
  long l;
  unsigned long ul;
  double d;
  Ibyte *bp;
  Lisp_Object obj;
};

/* We maintain a list of all the % specs in the specification,
   along with the offset and length of the block of literal text
   before each spec.  In addition, we have a "dummy" spec that
   represents all the literal text at the end of the specification.
   Its converter is 0. */

typedef struct
{
  Dynarr_declare (struct printf_spec);
} printf_spec_dynarr;

typedef struct
{
  Dynarr_declare (union printf_arg);
} printf_arg_dynarr;

/* Append STRING (of length LEN bytes) to STREAM.
   MINLEN is the minimum field width.
   If MINUS_FLAG is set, left-justify the string in its field;
    otherwise, right-justify.
   If ZERO_FLAG is set, pad with 0's; otherwise pad with spaces.
   If MAXLEN is non-negative, the string is first truncated on the
    right to that many characters.

   Note that MINLEN and MAXLEN are Charcounts but LEN is a Bytecount. */

static void
doprnt_2 (Lisp_Object stream, const Ibyte *string, Bytecount len,
	  Charcount minlen, Charcount maxlen, int minus_flag, int zero_flag)
{
  Lstream *lstr = XLSTREAM (stream);
  Charcount cclen = bytecount_to_charcount (string, len);
  int to_add = minlen - cclen;

  /* Padding at beginning to right-justify ... */
  if (!minus_flag)
    while (to_add-- > 0)
      Lstream_putc (lstr, zero_flag ? '0' : ' ');

  if (0 <= maxlen && maxlen < cclen)
    len = charcount_to_bytecount (string, maxlen);
  Lstream_write (lstr, string, len);

  /* Padding at end to left-justify ... */
  if (minus_flag)
    while (to_add-- > 0)
      Lstream_putc (lstr, zero_flag ? '0' : ' ');
}

static const Ibyte *
parse_off_posnum (const Ibyte *start, const Ibyte *end, int *returned_num)
{
  Ibyte arg_convert[100];
  REGISTER Ibyte *arg_ptr = arg_convert;

  *returned_num = -1;
  while (start != end && isdigit (*start))
    {
      if (arg_ptr - arg_convert >= (int) sizeof (arg_convert) - 1)
	syntax_error ("Format converter number too large", Qunbound);
      *arg_ptr++ = *start++;
    }
  *arg_ptr = '\0';
  if (arg_convert != arg_ptr)
    *returned_num = atoi ((char *) arg_convert);
  return start;
}

#define NEXT_ASCII_BYTE(ch)						\
  do {									\
    if (fmt == fmt_end)							\
      syntax_error ("Premature end of format string", Qunbound);	\
    ch = *fmt;								\
    if (ch >= 0200)							\
      syntax_error ("Non-ASCII character in format converter spec",	\
		    Qunbound);						\
    fmt++;								\
  } while (0)

#define RESOLVE_FLAG_CONFLICTS(spec)				\
  do {								\
    if (spec.space_flag && spec.plus_flag)			\
      spec.space_flag = 0;					\
    if (spec.zero_flag && spec.space_flag)			\
      spec.zero_flag = 0;					\
  } while (0)

static printf_spec_dynarr *
parse_doprnt_spec (const Ibyte *format, Bytecount format_length)
{
  const Ibyte *fmt = format;
  const Ibyte *fmt_end = format + format_length;
  printf_spec_dynarr *specs = Dynarr_new (printf_spec);
  int prev_argnum = 0;

  while (1)
    {
      struct printf_spec spec;
      const Ibyte *text_end;
      Ibyte ch;

      xzero (spec);
      if (fmt == fmt_end)
	return specs;
      text_end = (Ibyte *) memchr (fmt, '%', fmt_end - fmt);
      if (!text_end)
	text_end = fmt_end;
      spec.text_before = fmt - format;
      spec.text_before_len = text_end - fmt;
      fmt = text_end;
      if (fmt != fmt_end)
	{
	  fmt++; /* skip over % */

	  /* A % is special -- no arg number.  According to ANSI specs,
	     field width does not apply to %% conversion. */
	  if (fmt != fmt_end && *fmt == '%')
	    {
	      spec.converter = '%';
	      Dynarr_add (specs, spec);
	      fmt++;
	      continue;
	    }

	  /* Is there a field number specifier? */
	  {
	    const Ibyte *ptr;
	    int fieldspec;

	    ptr = parse_off_posnum (fmt, fmt_end, &fieldspec);
	    if (fieldspec > 0 && ptr != fmt_end && *ptr == '$')
	      {
		/* There is a format specifier */
		prev_argnum = fieldspec;
		fmt = ptr + 1;
	      }
	    else
	      prev_argnum++;
	    spec.argnum = prev_argnum;
	  }

	  /* Parse off any flags */
	  NEXT_ASCII_BYTE (ch);
	  while (strchr (valid_flags, ch))
	    {
	      switch (ch)
		{
		case '-': spec.minus_flag  = 1; break;
		case '+': spec.plus_flag   = 1; break;
		case ' ': spec.space_flag  = 1; break;
		case '#': spec.number_flag = 1; break;
		case '0': spec.zero_flag   = 1; break;
		default: abort ();
		}
	      NEXT_ASCII_BYTE (ch);
	    }

	  /* Parse off the minimum field width */
	  fmt--; /* back up */

	  /*
	   * * means the field width was passed as an argument.
	   * Mark the current spec as one that forwards its
	   * field width and flags to the next spec in the array.
	   * Then create a new spec and continue with the parsing.
	   */
	  if (fmt != fmt_end && *fmt == '*')
	    {
	      spec.converter = '*';
	      RESOLVE_FLAG_CONFLICTS(spec);
	      Dynarr_add (specs, spec);
	      xzero (spec);
	      spec.argnum = ++prev_argnum;
	      fmt++;
	    }
	  else
	    {
	      fmt = parse_off_posnum (fmt, fmt_end, &spec.minwidth);
	      if (spec.minwidth == -1)
		spec.minwidth = 0;
	    }

	  /* Parse off any precision specified */
	  NEXT_ASCII_BYTE (ch);
	  if (ch == '.')
	    {
	      /*
	       * * means the precision was passed as an argument.
	       * Mark the current spec as one that forwards its
	       * fieldwidth, flags and precision to the next spec in
	       * the array.  Then create a new spec and continue
	       * with the parse.
	       */
	      if (fmt != fmt_end && *fmt == '*')
		{
		  spec.converter = '*';
		  spec.forwarding_precision = 1;
		  RESOLVE_FLAG_CONFLICTS(spec);
		  Dynarr_add (specs, spec);
		  xzero (spec);
		  spec.argnum = ++prev_argnum;
		  fmt++;
		}
	      else
		{
		  fmt = parse_off_posnum (fmt, fmt_end, &spec.precision);
		  if (spec.precision == -1)
		    spec.precision = 0;
		}
	      NEXT_ASCII_BYTE (ch);
	    }
	  else
	    /* No precision specified */
	    spec.precision = -1;

	  /* Parse off h or l flag */
	  if (ch == 'h' || ch == 'l')
	    {
	      if (ch == 'h')
		spec.h_flag = 1;
	      else
		spec.l_flag = 1;
	      NEXT_ASCII_BYTE (ch);
	    }

	  if (!strchr (valid_converters, ch))
	    syntax_error ("Invalid converter character", make_char (ch));
	  spec.converter = ch;
	}

      RESOLVE_FLAG_CONFLICTS(spec);
      Dynarr_add (specs, spec);
    }

  RETURN_NOT_REACHED(specs); /* suppress compiler warning */
}

static int
get_args_needed (printf_spec_dynarr *specs)
{
  int args_needed = 0;
  REGISTER int i;

  /* Figure out how many args are needed.  This may be less than
     the number of specs because a spec could be %% or could be
     missing (literal text at end of format string) or there
     could be specs where the field number is explicitly given.
     We just look for the maximum argument number that's referenced. */

  for (i = 0; i < Dynarr_length (specs); i++)
    {
      char ch = Dynarr_at (specs, i).converter;
      if (ch && ch != '%')
	{
	  int argnum = Dynarr_at (specs, i).argnum;
	  if (argnum > args_needed)
	    args_needed = argnum;
	}
    }

  return args_needed;
}

static printf_arg_dynarr *
get_doprnt_args (printf_spec_dynarr *specs, va_list vargs)
{
  printf_arg_dynarr *args = Dynarr_new (printf_arg);
  union printf_arg arg;
  REGISTER int i;
  int args_needed = get_args_needed (specs);

  xzero (arg);
  for (i = 1; i <= args_needed; i++)
    {
      int j;
      char ch;
      struct printf_spec *spec = 0;

      for (j = 0; j < Dynarr_length (specs); j++)
	{
	  spec = Dynarr_atp (specs, j);
	  if (spec->argnum == i)
	    break;
	}

      if (j == Dynarr_length (specs))
	syntax_error ("No conversion spec for argument", make_int (i));

      ch = spec->converter;

      if (strchr (int_converters, ch))
	{
	  if (spec->l_flag)
	    arg.l = va_arg (vargs, long);
	  else
	    /* int even if ch == 'c' or spec->h_flag:
	       "the type used in va_arg is supposed to match the
	       actual type **after default promotions**."
	       Hence we read an int, not a short, if spec->h_flag. */
	    arg.l = va_arg (vargs, int);
	}
      else if (strchr (unsigned_int_converters, ch))
	{
	  if (spec->l_flag)
	    arg.ul = va_arg (vargs, unsigned long);
	  else
	    /* unsigned int even if ch == 'c' or spec->h_flag */
	    arg.ul = (unsigned long) va_arg (vargs, unsigned int);
	}
      else if (strchr (double_converters, ch))
	arg.d = va_arg (vargs, double);
      else if (strchr (string_converters, ch))
	arg.bp = va_arg (vargs, Ibyte *);
#if defined(HAVE_BIGNUM) || defined(HAVE_RATIO)
      else if (strchr (bignum_converters, ch))
	arg.obj = va_arg (vargs, Lisp_Object);
#endif
#ifdef HAVE_BIGFLOAT
      else if (strchr (bigfloat_converters, ch))
	arg.obj = va_arg (vargs, Lisp_Object);
#endif
      else abort ();

      Dynarr_add (args, arg);
    }

  return args;
}

/* Most basic entry point into string formatting.

   Generate output from a format-spec (either a Lisp string
   FORMAT_RELOC, or a C string FORMAT_NONRELOC of length FORMAT_LENGTH
   -- which *MUST NOT* come from Lisp string data, unless GC is
   inhibited).  Output goes to STREAM.  Returns the number of bytes
   stored into STREAM.  Arguments are either C-type arguments in
   va_list VARGS, or an array of Lisp objects in LARGS of size
   NARGS. (Behavior is different in the two cases -- you either get
   standard sprintf() behavior or `format' behavior.) */

static Bytecount
emacs_doprnt_1 (Lisp_Object stream, const Ibyte *format_nonreloc,
		Bytecount format_length, Lisp_Object format_reloc,
		int nargs, const Lisp_Object *largs, va_list vargs)
{
  printf_spec_dynarr *specs = 0;
  printf_arg_dynarr *args = 0;
  REGISTER int i;
  int init_byte_count = Lstream_byte_count (XLSTREAM (stream));
  int count;

  if (!NILP (format_reloc))
    {
      format_nonreloc = XSTRING_DATA (format_reloc);
      format_length = XSTRING_LENGTH (format_reloc);
    }
  if (format_length < 0)
    format_length = (Bytecount) strlen ((const char *) format_nonreloc);

  specs = parse_doprnt_spec (format_nonreloc, format_length);
  count = record_unwind_protect_freeing_dynarr (specs);

  if (largs)
    {
      /* allow too many args for string, but not too few */
      if (nargs < get_args_needed (specs))
	signal_error_1 (Qwrong_number_of_arguments,
			list3 (Qformat,
			       make_int (nargs),
			       !NILP (format_reloc) ? format_reloc :
			       make_string (format_nonreloc, format_length)));
    }
  else
    {
      args = get_doprnt_args (specs, vargs);
      record_unwind_protect_freeing_dynarr (args);
    }

  for (i = 0; i < Dynarr_length (specs); i++)
    {
      struct printf_spec *spec = Dynarr_atp (specs, i);
      char ch;

      /* Copy the text before */
      if (!NILP (format_reloc)) /* refetch in case of GC below */
	format_nonreloc = XSTRING_DATA (format_reloc);

      doprnt_2 (stream, format_nonreloc + spec->text_before,
		spec->text_before_len, 0, -1, 0, 0);

      ch = spec->converter;

      if (!ch)
	continue;

      if (ch == '%')
	{
	  doprnt_2 (stream, (Ibyte *) &ch, 1, 0, -1, 0, 0);
	  continue;
	}

      /* The char '*' as converter means the field width, precision
         was specified as an argument.  Extract the data and forward
         it to the next spec, to which it will apply.  */
      if (ch == '*')
	{
	  struct printf_spec *nextspec = Dynarr_atp (specs, i + 1);
	  Lisp_Object obj = largs[spec->argnum - 1];

	  if (INTP (obj))
	    {
	      if (spec->forwarding_precision)
		{
		  nextspec->precision = XINT (obj);
		  nextspec->minwidth = spec->minwidth;
		}
	      else
		{
		  nextspec->minwidth = XINT (obj);
		  if (XINT (obj) < 0)
		    {
		      spec->minus_flag = 1;
		      nextspec->minwidth = - nextspec->minwidth;
		    }
		}
	      nextspec->minus_flag  = spec->minus_flag;
	      nextspec->plus_flag   = spec->plus_flag;
	      nextspec->space_flag  = spec->space_flag;
	      nextspec->number_flag = spec->number_flag;
	      nextspec->zero_flag   = spec->zero_flag;
	    }
	  continue;
	}

      if (largs && (spec->argnum < 1 || spec->argnum > nargs))
	syntax_error ("Invalid repositioning argument",
		      make_int (spec->argnum));

      else if (ch == 'S' || ch == 's')
	{
	  Ibyte *string;
	  Bytecount string_len;

	  if (!largs)
	    {
	      string = Dynarr_at (args, spec->argnum - 1).bp;
#if 0
	      /* [[ error() can be called with null string arguments.
		 E.g., in fileio.c, the return value of strerror()
		 is never checked.  We'll print (null), like some
		 printf implementations do.  Would it be better (and safe)
		 to signal an error instead?  Or should we just use the
                 empty string?  -dkindred@cs.cmu.edu 8/1997 ]]
		 Do not hide bugs. --ben
	      */
	      if (!string)
		string = (Ibyte *) "(null)";
#else
	      assert (string);
#endif
	      string_len = strlen ((char *) string);
	    }
	  else
	    {
	      Lisp_Object obj = largs[spec->argnum - 1];
	      Lisp_Object ls;

	      if (ch == 'S')
		{
		  /* For `S', prin1 the argument and then treat like
		     a string.  */
		  ls = Fprin1_to_string (obj, Qnil);
		}
	      else if (STRINGP (obj))
		ls = obj;
	      else if (SYMBOLP (obj))
		ls = XSYMBOL (obj)->name;
	      else
		{
		  /* convert to string using princ. */
		  ls = Fprin1_to_string (obj, Qt);
		}
	      string = XSTRING_DATA (ls);
	      string_len = XSTRING_LENGTH (ls);
	    }

	  doprnt_2 (stream, string, string_len, spec->minwidth,
		    spec->precision, spec->minus_flag, spec->zero_flag);
	}

      else
	{
	  /* Must be a number. */
	  union printf_arg arg;

	  if (!largs)
	    {
	      arg = Dynarr_at (args, spec->argnum - 1);
	    }
	  else
	    {
	      Lisp_Object obj = largs[spec->argnum - 1];
	      if (CHARP (obj))
		obj = make_int (XCHAR (obj));
#ifdef WITH_NUMBER_TYPES
	      if (!NUMBERP (obj))
#else
	      if (!INT_OR_FLOATP (obj))
#endif
		{
		  /* WARNING!  This MUST be big enough for the sprintf below */
		  CIbyte msg[48];
		  sprintf (msg,
			   "format specifier %%%c doesn't match argument type",
			   ch);
		  syntax_error (msg, Qnil);
		}
	      else if (strchr (double_converters, ch))
		{
#ifdef WITH_NUMBER_TYPES
		  if (INTP (obj) || FLOATP (obj))
		    arg.d = XFLOATINT (obj);
#ifdef HAVE_BIGNUM
		  else if (BIGNUMP (obj))
		    arg.d = bignum_to_double (XBIGNUM_DATA (obj));
#endif
#ifdef HAVE_RATIO
		  else if (RATIOP (obj))
		    arg.d = ratio_to_double (XRATIO_DATA (obj));
#endif
#ifdef HAVE_BIGFLOAT
		  else if (BIGFLOATP (obj))
		    {
		      arg.obj = obj;
		      switch (ch)
			{
			case 'f': ch = 'F'; break;
			case 'e': ch = 'h'; break;
			case 'E': ch = 'H'; break;
			case 'g': ch = 'k'; break;
			case 'G': ch = 'K'; break;
			}
		    }
#endif
#else /* !WITH_NUMBER_TYPES */
		  arg.d = XFLOATINT (obj);
#endif /* WITH_NUMBER_TYPES */
		}
	      else
		{
		  if (FLOATP (obj))
		    obj = Ftruncate (obj);
#ifdef HAVE_BIGFLOAT
		  else if (BIGFLOATP (obj))
		    {
#ifdef HAVE_BIGNUM
		      bignum_set_bigfloat (scratch_bignum,
					   XBIGFLOAT_DATA (obj));
		      if (strchr (unsigned_int_converters, ch) &&
			  bignum_sign (scratch_bignum) < 0)
			dead_wrong_type_argument (Qnonnegativep, obj);
		      obj =
			Fcanonicalize_number (make_bignum_bg (scratch_bignum));
#else /* !HAVE_BIGNUM */
		      obj = make_int (bigfloat_to_long (XBIGFLOAT_DATA (obj)));
#endif /* HAVE_BIGNUM */
		    }
#endif /* HAVE_BIGFLOAT */
#ifdef HAVE_RATIO
		  else if (RATIOP (obj))
		    {
		      arg.obj = obj;
		      switch (ch)
			{
			case 'i': case 'd': ch = 'n'; break;
			case 'o': ch = 'p'; break;
			case 'x': ch = 'y'; break;
			case 'X': ch = 'Y'; break;
			default: /* ch == 'u' */
			  if (strchr (unsigned_int_converters, ch) &&
			      ratio_sign (XRATIO_DATA (obj)) < 0)
			    dead_wrong_type_argument (Qnonnegativep, obj);
			  else
			    ch = 'n';
			}
		    }
#endif
#ifdef HAVE_BIGNUM
		  if (BIGNUMP (obj))
		    {
		      arg.obj = obj;
		      switch (ch)
			{
			case 'i': case 'd': ch = 'n'; break;
			case 'o': ch = 'p'; break;
			case 'x': ch = 'y'; break;
			case 'X': ch = 'Y'; break;
			default: /* ch == 'u' */
			  if (strchr (unsigned_int_converters, ch) &&
			      bignum_sign (XBIGNUM_DATA (obj)) < 0)
			    dead_wrong_type_argument (Qnatnump, obj);
			  else
			    ch = 'n';
			}
		    }
#endif
		  if (INTP (obj))
		    {
		      if (strchr (unsigned_int_converters, ch))
			{
#ifdef HAVE_BIGNUM
			  if (XINT (obj) < 0)
			    dead_wrong_type_argument (Qnatnump, obj);
#endif
			  arg.ul = (unsigned long) XUINT (obj);
			}
		      else
			arg.l = XINT (obj);
		    }
		}
	    }

	  if (ch == 'c')
	    {
	      Ichar a;
	      Bytecount charlen;
	      Ibyte charbuf[MAX_ICHAR_LEN];

	      a = (Ichar) arg.l;

	      if (!valid_ichar_p (a))
		{
		  /* WARNING!  This MUST be big enough for the sprintf below */
		  CIbyte msg[60];
		  sprintf (msg, "invalid character value %d to %%c spec",
			   a);
		  syntax_error (msg, Qnil);
		}

	      charlen = set_itext_ichar (charbuf, a);
	      doprnt_2 (stream, charbuf, charlen, spec->minwidth,
			-1, spec->minus_flag, spec->zero_flag);
	    }
#if defined(HAVE_BIGNUM) || defined(HAVE_RATIO)
	  else if (strchr (bignum_converters, ch))
	    {
#ifdef HAVE_BIGNUM
	      if (BIGNUMP (arg.obj))
		{
		  Ibyte *text_to_print =
		    (Ibyte *) bignum_to_string (XBIGNUM_DATA (arg.obj),
						ch == 'n' ? 10 :
						(ch == 'p' ? 8 : 16));
		  doprnt_2 (stream, text_to_print,
			    strlen ((const char *) text_to_print),
			    spec->minwidth, -1, spec->minus_flag,
			    spec->zero_flag);
		  xfree (text_to_print, Ibyte *);
		}
#endif
#ifdef HAVE_RATIO
	      if (RATIOP (arg.obj))
		{
		  Ibyte *text_to_print =
		    (Ibyte *) ratio_to_string (XRATIO_DATA (arg.obj),
					       ch == 'n' ? 10 :
					       (ch == 'p' ? 8 : 16));
		  doprnt_2 (stream, text_to_print,
			    strlen ((const char *) text_to_print),
			    spec->minwidth, -1, spec->minus_flag,
			    spec->zero_flag);
		  xfree (text_to_print, Ibyte *);
		}
#endif
	    }
#endif /* HAVE_BIGNUM || HAVE_RATIO */
#ifdef HAVE_BIGFLOAT
	  else if (strchr (bigfloat_converters, ch))
	    {
	      Ibyte *text_to_print =
		(Ibyte *) bigfloat_to_string (XBIGFLOAT_DATA (arg.obj), 10);
	      doprnt_2 (stream, text_to_print,
			strlen ((const char *) text_to_print),
			spec->minwidth, -1, spec->minus_flag, spec->zero_flag);
	      xfree (text_to_print, Ibyte *);
	    }
#endif /* HAVE_BIGFLOAT */
	  else
	    {
	      /* ASCII Decimal representation uses 2.4 times as many
		 bits as machine binary.  */
	      char *text_to_print =
		alloca_array (char, 32 +
			      max (spec->minwidth,
				   (int) max (sizeof (double),
				              sizeof (long)) * 3 +
				   max (spec->precision, 0)));
	      char constructed_spec[100];
	      char *p = constructed_spec;

	      /* Mostly reconstruct the spec and use sprintf() to
		 format the string. */

	      *p++ = '%';
	      if (spec->plus_flag)   *p++ = '+';
	      if (spec->space_flag)  *p++ = ' ';
	      if (spec->number_flag) *p++ = '#';
	      if (spec->minus_flag)  *p++ = '-';
	      if (spec->zero_flag)   *p++ = '0';

	      if (spec->minwidth >= 0)
		{
		  long_to_string (p, spec->minwidth);
		  p += strlen (p);
		}
	      if (spec->precision >= 0)
		{
		  *p++ = '.';
		  long_to_string (p, spec->precision);
		  p += strlen (p);
		}
	      
	      if (strchr (double_converters, ch))
		{
		  *p++ = ch;
		  *p++ = '\0';
		  sprintf (text_to_print, constructed_spec, arg.d);
		}
	      else
		{
		  *p++ = 'l';	/* Always use longs with sprintf() */
		  *p++ = ch;
		  *p++ = '\0';

		  if (strchr (unsigned_int_converters, ch))
		    sprintf (text_to_print, constructed_spec, arg.ul);
		  else
		    sprintf (text_to_print, constructed_spec, arg.l);
		}

	      doprnt_2 (stream, (Ibyte *) text_to_print,
			strlen (text_to_print), 0, -1, 0, 0);
	    }
	}
    }

  unbind_to (count);
  return Lstream_byte_count (XLSTREAM (stream)) - init_byte_count;
}

/* Basic external entry point into string formatting.  See
 emacs_doprnt_1().
 */

Bytecount
emacs_doprnt_va (Lisp_Object stream, const Ibyte *format_nonreloc,
		 Bytecount format_length, Lisp_Object format_reloc,
		 va_list vargs)
{
  return emacs_doprnt_1 (stream, format_nonreloc, format_length,
			 format_reloc, 0, 0, vargs);
}

/* Basic external entry point into string formatting.  See
 emacs_doprnt_1().
 */

Bytecount
emacs_doprnt (Lisp_Object stream, const Ibyte *format_nonreloc,
	      Bytecount format_length, Lisp_Object format_reloc,
	      int nargs, const Lisp_Object *largs, ...)
{
  va_list vargs;
  Bytecount val;
  va_start (vargs, largs);
  val = emacs_doprnt_1 (stream, format_nonreloc, format_length,
			 format_reloc, nargs, largs, vargs);
  va_end (vargs);
  return val;
}

/* Similar to `format' in that its arguments are Lisp objects rather than C
   objects. (For the versions that take C objects, see the
   emacs_[v]sprintf... functions below.) Accepts the format string as
   either a C string (FORMAT_NONRELOC, which *MUST NOT* come from Lisp
   string data, unless GC is inhibited) or a Lisp string (FORMAT_RELOC).
   Return resulting formatted string as a Lisp string.

   All arguments are GCPRO'd, including FORMAT_RELOC; this makes it OK to
   pass newly created objects into this function (as often happens).

   #### It shouldn't be necessary to specify the number of arguments.
   This would require some rewriting of the doprnt() functions, though.
   */

Lisp_Object
emacs_vsprintf_string_lisp (const CIbyte *format_nonreloc,
			    Lisp_Object format_reloc, int nargs,
			    const Lisp_Object *largs)
{
  Lisp_Object stream;
  Lisp_Object obj;
  struct gcpro gcpro1, gcpro2;
  GCPRO2 (largs[0], format_reloc);
  gcpro1.nvars = nargs;

  stream = make_resizing_buffer_output_stream ();
  emacs_doprnt (stream, (Ibyte *) format_nonreloc, format_nonreloc ?
		strlen (format_nonreloc) : 0,
		format_reloc, nargs, largs);
  Lstream_flush (XLSTREAM (stream));
  obj = make_string (resizing_buffer_stream_ptr (XLSTREAM (stream)),
		     Lstream_byte_count (XLSTREAM (stream)));
  Lstream_delete (XLSTREAM (stream));
  UNGCPRO;
  return obj;
}

/* Like emacs_vsprintf_string_lisp() but accepts its extra args directly
   (using variable arguments), rather than as an array. */

Lisp_Object
emacs_sprintf_string_lisp (const CIbyte *format_nonreloc,
			   Lisp_Object format_reloc, int nargs, ...)
{
  Lisp_Object *args = alloca_array (Lisp_Object, nargs);
  va_list va;
  int i;
  Lisp_Object obj;

  va_start (va, nargs);
  for (i = 0; i < nargs; i++)
    args[i] = va_arg (va, Lisp_Object);
  va_end (va);
  obj = emacs_vsprintf_string_lisp (format_nonreloc, format_reloc, nargs,
				    args);
  return obj;
}

/* Like emacs_vsprintf_string_lisp() but returns a malloc()ed memory block.
   Return length out through LEN_OUT, if not null. */

Ibyte *
emacs_vsprintf_malloc_lisp (const CIbyte *format_nonreloc,
			    Lisp_Object format_reloc, int nargs,
			    const Lisp_Object *largs, Bytecount *len_out)
{
  Lisp_Object stream;
  Ibyte *retval;
  Bytecount len;
  struct gcpro gcpro1, gcpro2;

  GCPRO2 (largs[0], format_reloc);
  gcpro1.nvars = nargs;

  stream = make_resizing_buffer_output_stream ();
  emacs_doprnt (stream, (Ibyte *) format_nonreloc, format_nonreloc ?
		strlen (format_nonreloc) : 0,
		format_reloc, nargs, largs);
  Lstream_flush (XLSTREAM (stream));
  len = Lstream_byte_count (XLSTREAM (stream));
  retval = (Ibyte *) xmalloc (len + 1);
  memcpy (retval, resizing_buffer_stream_ptr (XLSTREAM (stream)), len);
  retval[len] = '\0';
  Lstream_delete (XLSTREAM (stream));

  if (len_out)
    *len_out = len;
  UNGCPRO;
  return retval;
}

/* Like emacs_sprintf_string_lisp() but returns a malloc()ed memory block.
   Return length out through LEN_OUT, if not null. */

Ibyte *
emacs_sprintf_malloc_lisp (Bytecount *len_out, const CIbyte *format_nonreloc,
			   Lisp_Object format_reloc, int nargs, ...)
{
  Lisp_Object *args = alloca_array (Lisp_Object, nargs);
  va_list va;
  int i;
  Ibyte *retval;

  va_start (va, nargs);
  for (i = 0; i < nargs; i++)
    args[i] = va_arg (va, Lisp_Object);
  va_end (va);
  retval = emacs_vsprintf_malloc_lisp (format_nonreloc, format_reloc, nargs,
				       args, len_out);
  return retval;
}

/* vsprintf()-like replacement.  Returns a Lisp string.  Data
   from Lisp strings is OK because we explicitly inhibit GC. */

Lisp_Object
emacs_vsprintf_string (const CIbyte *format, va_list vargs)
{
  Lisp_Object stream = make_resizing_buffer_output_stream ();
  Lisp_Object obj;
  int count = begin_gc_forbidden ();

  emacs_doprnt_va (stream, (Ibyte *) format, strlen (format), Qnil,
		   vargs);
  Lstream_flush (XLSTREAM (stream));
  obj = make_string (resizing_buffer_stream_ptr (XLSTREAM (stream)),
		     Lstream_byte_count (XLSTREAM (stream)));
  Lstream_delete (XLSTREAM (stream));
  end_gc_forbidden (count);
  return obj;
}

/* sprintf()-like replacement.  Returns a Lisp string.  Data
   from Lisp strings is OK because we explicitly inhibit GC. */

Lisp_Object
emacs_sprintf_string (const CIbyte *format, ...)
{
  va_list vargs;
  Lisp_Object retval;

  va_start (vargs, format);
  retval = emacs_vsprintf_string (format, vargs);
  va_end (vargs);
  return retval;
}

/* vsprintf()-like replacement.  Returns a malloc()ed memory block.  Data
   from Lisp strings is OK because we explicitly inhibit GC.  Return
   length out through LEN_OUT, if not null. */

Ibyte *
emacs_vsprintf_malloc (const CIbyte *format, va_list vargs,
		       Bytecount *len_out)
{
  int count = begin_gc_forbidden ();
  Lisp_Object stream = make_resizing_buffer_output_stream ();
  Ibyte *retval;
  Bytecount len;

  emacs_doprnt_va (stream, (Ibyte *) format, strlen (format), Qnil,
		   vargs);
  Lstream_flush (XLSTREAM (stream));
  len = Lstream_byte_count (XLSTREAM (stream));
  retval = (Ibyte *) xmalloc (len + 1);
  memcpy (retval, resizing_buffer_stream_ptr (XLSTREAM (stream)), len);
  retval[len] = '\0';
  end_gc_forbidden (count);
  Lstream_delete (XLSTREAM (stream));

  if (len_out)
    *len_out = len;
  return retval;
}

/* sprintf()-like replacement.  Returns a malloc()ed memory block.  Data
   from Lisp strings is OK because we explicitly inhibit GC.  Return length
   out through LEN_OUT, if not null. */

Ibyte *
emacs_sprintf_malloc (Bytecount *len_out, const CIbyte *format, ...)
{
  va_list vargs;
  Ibyte *retval;

  va_start (vargs, format);
  retval = emacs_vsprintf_malloc (format, vargs, len_out);
  va_end (vargs);
  return retval;
}

/* vsprintf() replacement.  Writes output into OUTPUT, which better
   have enough space for the output.  Data from Lisp strings is OK
   because we explicitly inhibit GC.  */

Bytecount
emacs_vsprintf (Ibyte *output, const CIbyte *format, va_list vargs)
{
  Bytecount retval;
  int count = begin_gc_forbidden ();
  Lisp_Object stream = make_resizing_buffer_output_stream ();
  Bytecount len;

  retval = emacs_doprnt_va (stream, (Ibyte *) format, strlen (format), Qnil,
			    vargs);
  Lstream_flush (XLSTREAM (stream));
  len = Lstream_byte_count (XLSTREAM (stream));
  memcpy (output, resizing_buffer_stream_ptr (XLSTREAM (stream)), len);
  output[len] = '\0';
  end_gc_forbidden (count);
  Lstream_delete (XLSTREAM (stream));
  
  return retval;
}

/* sprintf() replacement.  Writes output into OUTPUT, which better
   have enough space for the output.  Data from Lisp strings is OK
   because we explicitly inhibit GC.  */

Bytecount
emacs_sprintf (Ibyte *output, const CIbyte *format, ...)
{
  va_list vargs;
  Bytecount retval;

  va_start (vargs, format);
  retval = emacs_vsprintf (output, format, vargs);
  va_end (vargs);
  return retval;
}
