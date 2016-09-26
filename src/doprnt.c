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

/* Synched up with: Rewritten by Ben Wing.  Not in FSF. */

#include <config.h>
#include "lisp.h"

#include "buffer.h"
#include "lstream.h"

static const Ascbyte * const valid_flags = "-+ #0&~";

/* Don't add the bignum, ratio and bigfloat converters to these.
   valid_converters is just used to check format strings supplied by the user,
   and the bignum, ratio and bigfloat converters are only used internally
   within doprnt.c, they are not to be user-visible. */
static const Ascbyte * const valid_converters = "dic" "ouxX" "feEgG" "sS" "b";
static const Ascbyte * const int_converters = "dic";
static const Ascbyte * const base_converters = "oxXb";
static const Ascbyte * const double_converters = "feEgG";
static const Ascbyte * const string_converters = "sS";
#if defined(HAVE_BIGNUM) || defined(HAVE_RATIO)
static const Ascbyte * const bignum_converters = "npyY\337";
#endif
#ifdef HAVE_BIGFLOAT
static const Ascbyte * const bigfloat_converters = "FhHkK";
#endif

typedef struct printf_spec printf_spec;
struct printf_spec
{
  Bytecount text_before; /* Position of the first character of the block of
                            literal text before this spec. */
  Bytecount text_before_len; /* Length of that text. */
  Ascbyte converter; /* Converter character, or 0 for dummy marker indicating
                        literal text at the end of the specification. */
  unsigned int left_justify:1;
  unsigned int number_flag:2;
  unsigned int zero_flag:1;
  unsigned int h_flag:1;
  unsigned int l_flag:1;
  unsigned int forwarding_precisionp:1;
  unsigned int sign_flag:2;
  unsigned int precision_details:2;
  Elemcount argnum; /* Which argument does this spec want?  This is one-based:
                       The first argument given is numbered 1, the second is
                       2, etc.  This is to handle %##$x-type specs. */
  Charcount minwidth;
  Charcount precision;
  Bytecount spec_length; /* Length of this format spec itself, starting at
                            text_before + text_before_len.  */
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

/* Enums, macros for doprnt_2(). */

/* What sign information should be displayed? */
enum sign_flag {
  SIGN_FLAG_NOTHING = 0,
  SIGN_FLAG_SPACE,
  SIGN_FLAG_PLUS,
  SIGN_FLAG_MINUS
};

/* How should the PRECISION field be treated? */
enum precision_details {
  PRECISION_MAX_CHARACTERS = 0,
  PRECISION_DIGITS_AFTER_DECIMAL,
  PRECISION_MIN_TOTAL_DIGITS,
  PRECISION_MAX_SIGNIFICANT_DIGITS
};

/* Was the # (or &) flag specified, and if so, should it be interpreted as
   favouring Lisp or C syntax? */
enum number_flag {
  NUMBER_FLAG_NOTHING,
  NUMBER_FLAG_LISP_SYNTAX,
  NUMBER_FLAG_C_SYNTAX,
  NUMBER_FLAG_MASOCHISM /* This is C syntax, but with the sign after 0x, not
                           before. */
};

#define NUMBER_FLAG_C_LIKEP(flag) (flag >= NUMBER_FLAG_C_SYNTAX)

#define HANDLE_SIGN_FLAG(sf) do switch (sf)                     \
        {                                                       \
        case SIGN_FLAG_SPACE:                                   \
          Lstream_putc (lstr, ' '),                             \
            result_len += set_itext_ichar (chbuf, ' ');         \
          break;                                                \
        case SIGN_FLAG_PLUS:                                    \
          Lstream_putc (lstr, '+'),                             \
            result_len += set_itext_ichar (chbuf, '+');         \
          break;                                                \
        case SIGN_FLAG_MINUS:                                   \
          Lstream_putc (lstr, '-'),                             \
            result_len += set_itext_ichar (chbuf, '-');         \
          break;                                                \
        default:                                                \
          break;                                                \
        } while (0)

#define HANDLE_SIGN_AND_NUMBER(nf, sf)                              \
  do switch (nf)                                                    \
    {                                                               \
    case NUMBER_FLAG_NOTHING:                                       \
      HANDLE_SIGN_FLAG (sf);                                        \
      break;                                                        \
    case NUMBER_FLAG_C_SYNTAX:                                      \
      HANDLE_SIGN_FLAG (sf);                                        \
      Lstream_putc (lstr, '0'),                                     \
        result_len += set_itext_ichar (chbuf, '0');                 \
      Lstream_putc (lstr, pfsp->converter),                         \
        result_len += set_itext_ichar (chbuf, pfsp->converter);     \
      break;                                                        \
    case NUMBER_FLAG_LISP_SYNTAX:                                   \
      Lstream_putc (lstr, '#'),                                     \
        result_len += set_itext_ichar (chbuf, '#');                 \
      Lstream_putc (lstr, pfsp->converter),                         \
        result_len += set_itext_ichar (chbuf, pfsp->converter);     \
      HANDLE_SIGN_FLAG (sf);                                        \
      break;                                                        \
    case NUMBER_FLAG_MASOCHISM:                                     \
      Lstream_putc (lstr, '0'),                                     \
        result_len += set_itext_ichar (chbuf, '0');                 \
      Lstream_putc (lstr, pfsp->converter),                         \
        result_len += set_itext_ichar (chbuf, pfsp->converter);     \
      HANDLE_SIGN_FLAG (sf);                                        \
      break;                                                        \
    } while (0)

/* Append the string of length LEN starting at OFFSET to STREAM. Preserve any
   extent information.  If RELOC is non-nil, use its string data as the base
   for OFFSET. Otherwise, use NONRELOC.

   RELOC is a Lisp string, or Qnil. NONRELOC is a pointer to internal-format
   text, or NULL, to specify to use RELOC's string data. OFFSET is the byte
   offset within NONRELOC to start.  LEN is the byte length from OFFSET to
   append.

   PFSP is a pointer to a struct printf_spec. If it is non-NULL, the following
   fields within it are respected:
   MINWIDTH is taken as the minimum character field width.
   If LEFT_JUSTIFY is non-zero, left-justify the string in its field;
    otherwise, right-justify.
   If ZERO_FLAG is set, pad with 0's; otherwise pad with spaces.

   Handling of PRECISION depends on the PRECISON_DETAILS enum value.
   If it is PRECISION_MAX_CHARACTERS, the string is first truncated on the
   right to that many characters.
   If it is PRECISION_MIN_TOTAL_DIGITS, the string is padded on the left with
   zero characters if that is necessary to bring up its character count to
   that number given in PRECISION. This overrides ZERO_FLAG. There can be
   additional space padding as specified by MINWIDTH.

   If SIGN_FLAG is SIGN_FLAG_SPACE, output a plus between any space padding
   and before any zero padding. If it is SIGN_FLAG_MINUS, output a minus in
   that position. If it is SIGN_FLAG_SPACE, output a space.
   If it is SIGN_FLAG_HEX_UPPERCASE or SIGN_FLAG_HEX_LOWERCASE, output 0X or
   0x as appropriate. If it is SIGN_FLAG_NOTHING, do not output anything extra
   in this position.

   FORMAT_OBJECT, if non-nil, is taken to be Lisp object that the format spec
   came from. It should not be specified if it is identical to RELOC. If
   FORMAT_OBJECT has extent information, the following fields in PFSP are also
   used to access it:

   SPEC_LENGTH is taken to be the length of this format spec within
   FORMAT_OBJECT.
   TEXT_BEFORE and TEXT_BEFORE_LEN are summed to find the offset within
   FORMAT_OBJECT where the format spec started. */

static Bytecount
doprnt_2 (Lisp_Object stream, const Ibyte *nonreloc, Lisp_Object reloc,
          Bytecount offset, Bytecount len,
          struct printf_spec *pfsp, Lisp_Object format_object)
{
  Lstream *lstr = XLSTREAM (stream);
  Bytecount result_len = 0, begin = Lstream_byte_count (lstr);
  const Ibyte *newnonreloc = ((NILP (reloc)) ? nonreloc : XSTRING_DATA (reloc));
  Ibyte chbuf [MAX_ICHAR_LEN];

  assert (!(EQ (reloc, format_object)) || NILP (reloc));

  if (pfsp != NULL)
    {
      Charcount spaces_to_add = -1, minwidth = pfsp->minwidth;
      Charcount maxlen = -1, insert_zeros_until = -1, zeros_to_add = -1;
      Boolint left_justify = pfsp->left_justify, zero_flag = pfsp->zero_flag;
      enum sign_flag sign_flag = (enum sign_flag) (pfsp->sign_flag);
      enum number_flag number_flag = (enum number_flag) (pfsp->number_flag);

      switch ((enum precision_details) (pfsp->precision_details))
        {
        case PRECISION_MAX_CHARACTERS:
          maxlen = pfsp->precision;
          break;
        case PRECISION_MIN_TOTAL_DIGITS:
          insert_zeros_until = pfsp->precision;
          break;
        case PRECISION_DIGITS_AFTER_DECIMAL:
        case PRECISION_MAX_SIGNIFICANT_DIGITS:
          ABORT (); /* These are for float values, which are unimplemented for
                       the moment, being passed through to the C
                       implementation of snprintf(). */
          break;
        }

      /* LEN / MAX_ICHAR_LEN is an inclusive lower bound on CCLEN. It
         represents the case where NEWNONRELOC comprises exclusively
         characters of the maximum byte length. If minwidth is zero or < (LEN
         / MAX_ICHAR_LEN), definitely no need to pad; otherwise we need to
         calculate the character length to work out whether we need to pad. */
      if ((minwidth != 0 && minwidth >= (len / MAX_ICHAR_LEN))

          /* Sigh, INSERT_ZEROS_UNTIL and ZERO_FLAG need to be interpreted
             separately. While it is not possible to combine
             PRECISION_MIN_TOTAL_DIGITS and ZERO_FLAG, it is possible to
             combine the former with SPACES_TO_ADD, which is managed by the
             same code. */
          || (insert_zeros_until > -1 &&
              insert_zeros_until >= (len / MAX_ICHAR_LEN))

          /* LEN is an inclusive upper bound on CCLEN. It represents the case
             where NEWNONRELOC comprises exclusively ASCII characters. If
             MAXLEN is greater than or equal to LEN, no need to truncate. */
          || (0 <= maxlen && maxlen < len))
        {
          Charcount cclen = bytecount_to_charcount (newnonreloc + offset, len);

          zeros_to_add = max (insert_zeros_until - cclen, 0);
          cclen += (sign_flag != SIGN_FLAG_NOTHING);
          cclen += (number_flag != NUMBER_FLAG_NOTHING) * 2;
          spaces_to_add = minwidth - (cclen + zeros_to_add);

          if (maxlen >= 0 && (maxlen = min (maxlen, cclen), maxlen != cclen))
            {
              len = charcount_to_bytecount (newnonreloc + offset, maxlen);
              /* Truncation shouldn't happen with numbers. */
              text_checking_assert (sign_flag == SIGN_FLAG_NOTHING);
            }
        }

      if (left_justify)
        {
          HANDLE_SIGN_AND_NUMBER (number_flag, sign_flag);
        }
      else if (zero_flag)
        {
          /* If we're padding on the left (= right-justifying) with zero
             characters, then any sign information has to be before those
             zeroes. */
          HANDLE_SIGN_AND_NUMBER (number_flag, sign_flag);
          while (spaces_to_add-- > 0)
            {
              Lstream_putc (lstr, '0'),
                result_len += set_itext_ichar (chbuf, '0');
            }
        }
      else
        {
          while (spaces_to_add-- > 0)
            Lstream_putc (lstr, ' '),
              result_len += set_itext_ichar (chbuf, ' ');
          /* Sign information comes after the spaces. */
          HANDLE_SIGN_AND_NUMBER (number_flag, sign_flag);
        }

      while (zeros_to_add-- > 0)
        Lstream_putc (lstr, '0'), result_len += set_itext_ichar (chbuf, '0');

      Lstream_write (lstr, newnonreloc + offset, len);
      result_len += len;

      /* Padding at end to left-justify ... */
      if (left_justify)
        while (spaces_to_add-- > 0)
          Lstream_putc (lstr, zero_flag ? '0' : ' '),
            result_len += set_itext_ichar (chbuf, zero_flag ? '0' : ' ');

      if (!NILP (format_object) && string_extent_info (format_object) != NULL)
        {
          stretch_string_extents (stream, format_object, begin,
                                  pfsp->text_before + pfsp->text_before_len,
                                  pfsp->spec_length,
                                  Lstream_byte_count (lstr) - begin);
        }
    }
  else
    {
      Lstream_write (lstr, newnonreloc + offset, len);
      result_len += len;
    }
 
  if (!NILP (reloc) && string_extent_info (reloc) != NULL)
    {
      stretch_string_extents (stream, reloc, begin,
                              offset, len, Lstream_byte_count (lstr) - begin);
    }

  return result_len;
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
    if (spec.number_flag == NUMBER_FLAG_LISP_SYNTAX &&          \
        spec.sign_flag == SIGN_FLAG_SPACE)                      \
      {                                                         \
        spec.sign_flag = SIGN_FLAG_NOTHING;                     \
      }                                                         \
    if (spec.zero_flag && spec.sign_flag == SIGN_FLAG_SPACE)    \
      spec.zero_flag = 0;					\
  } while (0)

static printf_spec_dynarr *
parse_doprnt_spec (const Ibyte *format, Bytecount format_length)
{
  const Ibyte *fmt = format;
  const Ibyte *fmt_end = format + format_length;
  printf_spec_dynarr *specs = Dynarr_new (printf_spec);
  Elemcount prev_argnum = 0;

  while (1)
    {
      struct printf_spec spec;
      const Ibyte *text_end;
      Ibyte ch;

      if (fmt == fmt_end)
	return specs;

      xzero (spec);
      text_end = (Ibyte *) memchr (fmt, '%', fmt_end - fmt);
      if (!text_end)
	text_end = fmt_end;
      spec.text_before = fmt - format;
      spec.text_before_len = text_end - fmt;
      spec.precision = -1;
      
      fmt = text_end;
      if (fmt != fmt_end)
	{
	  fmt++; /* skip over % */

	  /* A % is special -- no arg number.  According to ANSI specs,
	     field width does not apply to %% conversion. */
	  if (fmt != fmt_end && *fmt == '%')
	    {
	      spec.converter = '%';
              spec.spec_length = fmt - text_end;
	      Dynarr_add (specs, spec);
	      fmt++;
	      continue;
	    }

	  /* Is there a repositioning specifier? */
	  {
            Ibyte *fmt1;
            Lisp_Object posnum
              = parse_integer (fmt, &fmt1, fmt_end - fmt, 10, 1,
                               Vdigit_fixnum_ascii);

            if (FIXNUMP (posnum) && XREALFIXNUM (posnum) > 0
                && itext_ichar (fmt1) == '$')
              {
                /* There is a repositioning specifier: */
                prev_argnum = XREALFIXNUM (posnum);
                fmt = fmt1;
                INC_IBYTEPTR (fmt);
              }
            else 
              {
                prev_argnum++;
                /* Don't check POSNUM further (e.g. for sign, range), since it
                   may actually be a field width. */
              }

	    spec.argnum = prev_argnum;
	  }

	  /* Parse off any flags */
	  NEXT_ASCII_BYTE (ch);
	  while (strchr (valid_flags, ch))
	    {
	      switch (ch)
		{
		case '-': spec.left_justify  = 1; break;
		case '+': spec.sign_flag = SIGN_FLAG_PLUS; break;
		case ' ': 
                  if (spec.sign_flag != SIGN_FLAG_PLUS)
                    {
                      /* The plus flag overrides. */
                      spec.sign_flag = SIGN_FLAG_SPACE; 
                    }
                  break;
		case '#':
                  if (spec.number_flag != NUMBER_FLAG_MASOCHISM)
                    {
                      spec.number_flag = NUMBER_FLAG_C_SYNTAX;
                    }
                  break;
                case '&': spec.number_flag = NUMBER_FLAG_LISP_SYNTAX; break;
                case '~':
                  if (spec.number_flag != NUMBER_FLAG_LISP_SYNTAX)
                    {
                      spec.number_flag = NUMBER_FLAG_MASOCHISM;
                    }
                  break;
 		case '0': spec.zero_flag   = 1; break;
                  /* So we get at helpful value on abort. */
 		default: text_checking_assert (format == NULL);
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
              spec.spec_length = fmt - text_end;
	      RESOLVE_FLAG_CONFLICTS(spec);
	      Dynarr_add (specs, spec);
	      xzero (spec);
	      spec.argnum = ++prev_argnum;
	      fmt++;
	    }
	  else
	    {
              Lisp_Object mwidth
                = parse_integer (fmt, (Ibyte **) (&fmt), fmt_end - fmt, 10, 1,
                                 Vdigit_fixnum_ascii);
              if (NILP (mwidth))
                {
                  spec.minwidth = -1; /* Failed to parse an integer, which is
                                         fine, use our default. */
                }
              else
                {
                  check_integer_range (mwidth, Qzero,
                                       /* This is a charcount used for
                                          allocation, whence the following
                                          limitation. See the multiplication
                                          below just above the call to
                                          snprintf ().  */
                                       make_fixnum (MOST_POSITIVE_FIXNUM /
                                                    MAX_ICHAR_LEN));
                  /* We have a (somewhat) sensible minwidth. */
                  spec.minwidth = XFIXNUM (mwidth); 
                }
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
                  spec.spec_length = fmt - text_end;
		  spec.forwarding_precisionp = 1;
		  RESOLVE_FLAG_CONFLICTS(spec);
		  Dynarr_add (specs, spec);
		  xzero (spec);
		  spec.argnum = ++prev_argnum;
		  fmt++;
		}
	      else
		{
                  Lisp_Object precis
                    = parse_integer (fmt, (Ibyte **) (&fmt), fmt_end - fmt,
                                     10, 1, Vdigit_fixnum_ascii);
                  if (NILP (precis))
                    {
                      spec.precision = 0; /* There is a dot, but we failed to
                                             parse a fixnum. Treat as a
                                             zero-length precision, as the C
                                             standard says. */
                    }
                  else 
                    {
                      check_integer_range (precis,
                                           /* This is a charcount. */
                                           make_fixnum (MOST_NEGATIVE_FIXNUM
                                                        / MAX_ICHAR_LEN),
                                           make_fixnum (MOST_POSITIVE_FIXNUM /
                                                        MAX_ICHAR_LEN));
                      /* The C standard says a negative fixnum is to be
                         treated as not specified. */
                      spec.precision = max (XFIXNUM (precis), -1);
                    }
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
          /* Transform %i to %d silently. */
          spec.converter = 'i' == ch ? 'd' : ch;
          spec.spec_length = fmt - text_end;
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
      Ascbyte ch;
      struct printf_spec *spec = 0;

      for (j = 0; j < Dynarr_length (specs); j++)
	{
	  spec = Dynarr_atp (specs, j);
	  if (spec->argnum == i)
	    break;
	}

      if (j == Dynarr_length (specs))
	syntax_error ("No conversion spec for argument", make_fixnum (i));

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
      else if (strchr (base_converters, ch))
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
      else ABORT ();

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
  else if (format_length < 0)
    {
      format_length = qxestrlen (format_nonreloc);
    }

  specs = parse_doprnt_spec (format_nonreloc, format_length);
  count = record_unwind_protect_freeing_dynarr (specs);

  if (largs)
    {
      /* allow too many args for string, but not too few */
      if (nargs < get_args_needed (specs))
	signal_error_1 (Qwrong_number_of_arguments,
			list3 (Qformat,
			       make_fixnum (nargs),
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
        {
          format_nonreloc = XSTRING_DATA (format_reloc);
        }

      doprnt_2 (stream, format_nonreloc, format_reloc, spec->text_before,
                spec->text_before_len, NULL, Qnil);

      ch = spec->converter;

      if (!ch)
	continue;

      if (ch == '%')
	{
          Ibyte chbuf[MAX_ICHAR_LEN];
          /* No field widths, no precisions, take any extents from the format
             string.  */
          doprnt_2 (stream, format_nonreloc, format_reloc,
                    spec->text_before + spec->text_before_len,
                    set_itext_ichar (chbuf, '%'), NULL, Qnil);
	  continue;
	}

      /* The char '*' as converter means the field width, precision
         was specified as an argument.  Extract the data and forward
         it to the next spec, to which it will apply.  */
      if (ch == '*')
	{
	  struct printf_spec *nextspec = Dynarr_atp (specs, i + 1);
	  Lisp_Object obj = largs[spec->argnum - 1];

          /* No bignums or random Lisp objects, thanks, and restrict range as
             appropriate for a charcount. */
          check_integer_range (obj,
                               make_fixnum (MOST_NEGATIVE_FIXNUM /
                                            MAX_ICHAR_LEN),
                               make_fixnum (MOST_POSITIVE_FIXNUM /
                                            MAX_ICHAR_LEN));
          if (spec->forwarding_precisionp)
            {
              nextspec->precision = XREALFIXNUM (obj);
              nextspec->minwidth = spec->minwidth;
            }
          else
            {
              nextspec->minwidth = XREALFIXNUM (obj);
              if (XREALFIXNUM (obj) < 0)
                {
                  spec->left_justify = 1;
                  nextspec->minwidth = - nextspec->minwidth;
                }
            }
          nextspec->left_justify = spec->left_justify;
          nextspec->sign_flag = spec->sign_flag;
          nextspec->number_flag = spec->number_flag;
          nextspec->zero_flag = spec->zero_flag;
	  continue;
	}

      if (largs && (spec->argnum < 1 || spec->argnum > nargs))
	syntax_error ("Invalid repositioning argument",
		      make_fixnum (spec->argnum));

      else if (ch == 's')
	{
	  Ibyte *string;
	  Bytecount string_len;
          Lisp_Object ls = Qnil;

	  if (!largs)
	    {
	      string = Dynarr_at (args, spec->argnum - 1).bp;
              text_checking_assert (string != NULL);
	      string_len = qxestrlen (string);
	    }
	  else
	    {
	      Lisp_Object obj = largs[spec->argnum - 1];

	      if (STRINGP (obj))
		ls = obj;
	      else if (SYMBOLP (obj))
		ls = XSYMBOL (obj)->name;
	      else
		{
		  /* convert to string using princ. */
		  ls = prin1_to_string (obj, 1);
		}
	      string = XSTRING_DATA (ls);
	      string_len = XSTRING_LENGTH (ls);
	    }

	  doprnt_2 (stream, string, ls, 0, string_len, spec, format_reloc);
	}
      else if (ch == 'S')
        {
          Lisp_Object obj = Qnil;

          if (largs == NULL)
            {
              signal_error (Qunimplemented,
                            "can't handle %S format with non Lisp arguments",
                            NILP (format_reloc) ?
                            make_string (format_nonreloc,
                                         format_length) :
                            format_reloc);
            }
          else
            {
              /* For `S', prin1 the argument and then treat like a string.  */
	      obj = prin1_to_string (largs[spec->argnum - 1], 0);
            }
          
	  doprnt_2 (stream, NULL, obj, 0, XSTRING_LENGTH (obj), spec,
                    format_reloc);
        }
      else if (ch == 'c')
        {
          Ibyte charbuf[MAX_ICHAR_LEN];
          Ichar a = CHAR_CODE_LIMIT; /* Not a valid ichar. */
          Lisp_Object obj = Qnil;

	  if (!largs)
	    {
	      a = (Ichar) Dynarr_at (args, spec->argnum - 1).l;
              obj = make_fixnum (a);
            }
          else
            {
              obj = largs[spec->argnum - 1];
            }

          if (CHARP (obj))
            {
              a = XCHAR (obj);
            }
          else
            {
              if (FIXNUMP (obj))
                {
                  a = XREALFIXNUM (obj);
                }

              if (!valid_ichar_p (a))
                {
                  syntax_error ("Invalid value for %c spec", obj);
                }
            }

          if (spec->precision != -1)
            {
              syntax_error ("Precision nonsensical for %c",
                            NILP (format_reloc) ?
                            make_string (format_nonreloc, format_length) :
                            format_reloc);
            }

          /* XEmacs; don't attempt (badly) to handle floats, bignums or ratios
             when 'c' is specified, error instead, "Format specifier doesn't
             match arg type". */
          doprnt_2 (stream, charbuf, Qnil, 0, set_itext_ichar (charbuf, a),
                    spec, format_reloc);
        }
      else
	{
	  /* Must be a number or character. */
	  union printf_arg arg;

	  if (!largs)
	    {
	      arg = Dynarr_at (args, spec->argnum - 1);
	    }
	  else
	    {
	      Lisp_Object obj = largs[spec->argnum - 1];

              if (ch == 'u')
                {
                  ch = 'd';
                }

	      if (strchr (double_converters, ch))
		{
                  /* Don't accept a character argument for a float format
                     spec. */
		  if (FIXNUMP (obj))
		    arg.d = XFIXNUM (obj);
		  else if (FLOATP (obj))
		    arg.d = XFLOAT_DATA (obj);
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
		}
	      else
		{
                  if (CHARP (obj))
                    {
                      /* Do accept a character argument for an integer format
                         spec. */
                      obj = make_fixnum (XCHAR (obj));
                    }
                  else if (FLOATP (obj))
                    {
                      obj = IGNORE_MULTIPLE_VALUES (Ftruncate (obj, Qnil));
                    }
#ifdef HAVE_BIGFLOAT
		  else if (BIGFLOATP (obj))
		    {
#ifdef HAVE_BIGNUM
		      bignum_set_bigfloat (scratch_bignum,
					   XBIGFLOAT_DATA (obj));
		      if (strchr (base_converters, ch) &&
			  bignum_sign (scratch_bignum) < 0)
			dead_wrong_type_argument (Qnonnegativep, obj);
		      obj =
			Fcanonicalize_number (make_bignum_bg (scratch_bignum));
#else /* !HAVE_BIGNUM */
		      obj = make_fixnum (bigfloat_to_long (XBIGFLOAT_DATA (obj)));
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
                        case 'b': ch = '\337'; break;
			default: /* ch == 'u' */
			  if (strchr (base_converters, ch) &&
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
                        case 'b': ch = '\337'; break;
			default: /* ch == 'u' */
			  if (strchr (base_converters, ch) &&
			      bignum_sign (XBIGNUM_DATA (obj)) < 0)
			    dead_wrong_type_argument (Qnatnump, obj);
			  else
			    ch = 'n';
			}
		    }
#endif
		  if (FIXNUMP (obj))
		    {
		      if (strchr (base_converters, ch))
			{
#ifdef HAVE_BIGNUM
			  if (XFIXNUM (obj) < 0)
			    dead_wrong_type_argument (Qnatnump, obj);
#endif
			  arg.ul = (unsigned long) XUINT (obj);
			}
		      else
			arg.l = XFIXNUM (obj);
		    }

		}


	      if (!NUMBERP (obj))
		{
                  syntax_error_2 ("Format specifier doesn't match arg type",
                                  make_char (ch), obj);
		}
            }

	  if (strchr (int_converters, ch))
            {
              Ibyte buf[DECIMAL_PRINT_SIZE (EMACS_INT) + MAX_ICHAR_LEN];
              Ibyte *ptr = buf;

              if (arg.l > -1)
                {
                  if (SIGN_FLAG_PLUS == spec->sign_flag)
                    {
                      ptr += set_itext_ichar (buf, '+');
                    }
                  else if (SIGN_FLAG_SPACE == spec->sign_flag)
                    {
                      ptr += set_itext_ichar (buf, ' ');
                    }
                }
              
              doprnt_2 (stream, buf, Qnil, 0,
                        (ptr + long_to_string ((Ascbyte *) ptr, arg.l)) - buf,
                        spec, format_reloc);
            }
          else if (ch == 'b')
            {
              Ascbyte *text_to_print = alloca_array (char, SIZEOF_LONG * 8 + 1);
              
              ulong_to_bit_string (text_to_print, arg.ul);
              doprnt_2 (stream, (const Ibyte *) text_to_print, Qnil,
                        0, strlen (text_to_print), spec, format_reloc);
            }
#if defined(HAVE_BIGNUM) || defined(HAVE_RATIO)
	  else if (strchr (bignum_converters, ch))
	    {
              int base = 16;
              
              if (ch == 'n')
                base = 10;
              else if (ch == 'p')
                base = 8;
              else if (ch == '\337')
                base = 2;

#ifdef HAVE_BIGNUM
	      if (BIGNUMP (arg.obj))
		{
		  Ibyte *text_to_print =
		    (Ibyte *) bignum_to_string (XBIGNUM_DATA (arg.obj),
						base);
                  doprnt_2 (stream, text_to_print, Qnil,
                            0, qxestrlen (text_to_print), spec, format_reloc);
		  xfree (text_to_print);
		}
#endif
#ifdef HAVE_RATIO
	      if (RATIOP (arg.obj))
		{
		  Ibyte *text_to_print =
		    (Ibyte *) ratio_to_string (XRATIO_DATA (arg.obj), base);
                  doprnt_2 (stream, text_to_print, Qnil,
                            0, qxestrlen (text_to_print), spec, format_reloc);
		  xfree (text_to_print);
		}
#endif
	    }
#endif /* HAVE_BIGNUM || HAVE_RATIO */
#ifdef HAVE_BIGFLOAT
	  else if (strchr (bigfloat_converters, ch))
	    {
	      Ibyte *text_to_print =
		(Ibyte *) bigfloat_to_string (XBIGFLOAT_DATA (arg.obj), 10);
	      doprnt_2 (stream, text_to_print, Qnil,
			0, qxestrlen (text_to_print), spec, format_reloc);
	      xfree (text_to_print);
	    }
#endif /* HAVE_BIGFLOAT */
	  else
	    {
	      Ascbyte *text_to_print;
	      Ascbyte constructed_spec[100];
	      Ascbyte *p = constructed_spec;
              int alloca_sz = 350;
              int min = spec->minwidth, prec = spec->precision;

              if (prec < 0)
                prec = 0;
              if (min < 0)
                min = 0;

              if (32+min+prec > alloca_sz)
                alloca_sz = 32 + min + prec;

              text_to_print = alloca_array(char, alloca_sz);

	      /* Mostly reconstruct the spec and use sprintf() to
		 format the string. */

	      *p++ = '%';
	      if (spec->sign_flag == SIGN_FLAG_PLUS)   *p++ = '+';
	      if (spec->sign_flag == SIGN_FLAG_SPACE)  *p++ = ' ';
	      if (spec->number_flag == NUMBER_FLAG_C_SYNTAX) *p++ = '#';
	      if (spec->sign_flag == SIGN_FLAG_MINUS)  *p++ = '-';
	      if (spec->zero_flag)   *p++ = '0';

	      if (spec->minwidth >= 0)
		{
		  p += long_to_string (p, spec->minwidth);
		}
	      if (spec->precision >= 0)
		{
		  *p++ = '.';
		  p += long_to_string (p, spec->precision);
		}
	      
	      if (strchr (double_converters, ch))
		{
		  *p++ = ch;
		  *p++ = '\0';
		  sprintf (text_to_print, constructed_spec, arg.d);
		}
	      else if (strchr (base_converters, ch))
		{
		  *p++ = 'l';	/* Always use longs with sprintf() */
		  *p++ = ch;
		  *p++ = '\0';

                  sprintf (text_to_print, constructed_spec, arg.ul);
		}
              else
                {
                  assert (0);
                }

              if (!NILP (format_reloc))
                {
                  struct printf_spec minimal_spec;
                  bzero (&minimal_spec, sizeof (minimal_spec));
                  minimal_spec.precision = -1;
                  minimal_spec.spec_length = spec->spec_length;
                  minimal_spec.text_before = spec->text_before;
                  minimal_spec.text_before_len = spec->text_before_len;

                  doprnt_2 (stream, (Ibyte *) text_to_print, Qnil, 0,
                            strlen (text_to_print), &minimal_spec,
                            format_reloc);
                }
              else
                {
                  doprnt_2 (stream, (Ibyte *) text_to_print, Qnil, 0,
                            strlen (text_to_print), NULL, format_reloc);
                }
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
  obj = resizing_buffer_to_lisp_string (XLSTREAM (stream));
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
  retval = xnew_ibytes (len + 1);
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
  obj = resizing_buffer_to_lisp_string (XLSTREAM (stream));
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
  retval = xnew_ibytes (len + 1);
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


DEFUN ("format", Fformat, 1, MANY, 0, /*
Format a string out of a control-string and arguments.
The first argument is a control string.
The other arguments are substituted into it to make the result, a string.
It may contain %-sequences meaning to substitute the next argument.
%s means print all objects as-is, using `princ'.
%S means print all objects as s-expressions, using `prin1'.
%d or %i means print as an integer in decimal (%o octal, %x lowercase hex,
  %X uppercase hex, %b binary).
%c means print as a single character.
%f means print as a floating-point number in fixed notation (e.g. 785.200).
%e or %E means print as a floating-point number in scientific notation
  (e.g. 7.85200e+03).
%g or %G means print as a floating-point number in "pretty format";
  depending on the number, either %f or %e/%E format will be used, and
  trailing zeroes are removed from the fractional part.
The argument used for all but %s, %S, and %c must be a number.  It will be
  converted to an integer or a floating-point number as necessary.  In
  addition, the integer %-sequences accept character arguments as equivalent
  to the corresponding fixnums (see `char-int'), while the floating point
  sequences do not.

%$ means reposition to read a specific numbered argument; for example,
  %3$s would apply the `%s' to the third argument after the control string,
  and the next format directive would use the fourth argument, the
  following one the fifth argument, etc. (There must be a positive integer
  between the % and the $).
Zero or more of the flag characters `-', `+', ` ', `0', and `#' may be
  specified between the optional repositioning spec and the conversion
  character; see below.
An optional minimum field width may be specified after any flag characters
  and before the conversion character; it specifies the minimum number of
  characters that the converted argument will take up.  Padding will be
  added on the left (or on the right, if the `-' flag is specified), as
  necessary.  Padding is done with spaces, or with zeroes if the `0' flag
  is specified.
If the field width is specified as `*', the field width is assumed to have
  been specified as an argument.  Any repositioning specification that
  would normally specify the argument to be converted will now specify
  where to find this field width argument, not where to find the argument
  to be converted.  If there is no repositioning specification, the normal
  next argument is used.  The argument to be converted will be the next
  argument after the field width argument unless the precision is also
  specified as `*' (see below).

An optional period character and precision may be specified after any
  minimum field width.  It specifies the minimum number of digits to
  appear in %d, %i, %o, %x, and %X conversions (the number is padded
  on the left with zeroes as necessary); the number of digits printed
  after the decimal point for %f, %e, and %E conversions; the number
  of significant digits printed in %g and %G conversions; and the
  maximum number of non-padding characters printed in %s and %S
  conversions.  The default precision for floating-point conversions
  is six. Using a precision with %c is an error.
If the precision is specified as `*', the precision is assumed to have been
  specified as an argument.  The argument used will be the next argument
  after the field width argument, if any.  If the field width was not
  specified as an argument, any repositioning specification that would
  normally specify the argument to be converted will now specify where to
  find the precision argument.  If there is no repositioning specification,
  the normal next argument is used.

The ` ' and `+' flags mean prefix non-negative numbers with a space or
  plus sign, respectively.
The `#' flag means print numbers in an alternate, more verbose format:
  octal numbers begin with zero; hex numbers begin with a 0x or 0X;
  a decimal point is printed in %f, %e, and %E conversions even if no
  numbers are printed after it; and trailing zeroes are not omitted in
   %g and %G conversions.

Use %% to put a single % into the output.

Extent information in CONTROL-STRING and in ARGS are carried over into the
output, in the same way as `concatenate'.  Any text created by a character or
numeric %-sequence inherits the extents of the text around it, or of the text
abutting it if those extents' `start-open' and `end-open' properties have the
appropriate values.

arguments: (CONTROL-STRING &rest ARGS)
*/
       (int nargs, Lisp_Object *args))
{
  /* It should not be necessary to GCPRO ARGS, because the caller in the
     interpreter should take care of that.  */
  CHECK_STRING (args[0]);
  return emacs_vsprintf_string_lisp (0, args[0], nargs - 1, args + 1);
}

DEFUN ("format-into", Fformat_into, 2, MANY, 0, /*
Like `format', but write the constructed string into STREAM.

STREAM is a Lisp stream as created by, e.g. `make-string-output-stream'.

Return STREAM.  See the documentation for `format' for details of
CONTROL-STRING and the other arguments.

arguments: (STREAM CONTROL-STRING &rest ARGS)
*/
       (int nargs, Lisp_Object *args))
{
  Lisp_Object stream = args[0], control_string = args[1];

  CHECK_LSTREAM (stream);
  CHECK_STRING (control_string);
  
  emacs_doprnt (stream, NULL, XSTRING_LENGTH (control_string),
                control_string, nargs - 2, args + 2);

  return stream;
}

/************************************************************************/
/*                            initialization                            */
/************************************************************************/

void
syms_of_doprnt (void)
{
  DEFSUBR (Fformat);
  DEFSUBR (Fformat_into);
}
