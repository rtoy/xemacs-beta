/* Selection processing for XEmacs -- common btwn select-x.c and select-gtk.c
   Copyright (C) 1990, 1991, 1992, 1993, 1994 Free Software Foundation, Inc.

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

/* Synched up with: Not synched with FSF. */

#ifdef THIS_IS_X
#define XE_ATOM_TYPE Atom
#define XE_ATOM_TO_SYMBOL x_atom_to_symbol
#define XE_SYMBOL_TO_ATOM symbol_to_x_atom
#else
#define XE_ATOM_TYPE GdkAtom
#define XE_ATOM_TO_SYMBOL atom_to_symbol
#define XE_SYMBOL_TO_ATOM symbol_to_gtk_atom
#endif /* THIS_IS_X */

/* #### These are going to move into Lisp code(!) with the aid of
        some new functions I'm working on - ajh */

/* These functions convert from the selection data read from the server into
   something that we can use from elisp, and vice versa.

        Type:   Format: Size: Elisp Type:
        -----   ------- ----- -----------
        *       8       *     String
        ATOM    32      1     Symbol
        ATOM    32      > 1   Vector of Symbols
        *       16      1     Fixnum
        *       16      > 1   Vector of fixnums
        *       32      1     Fixnum, if value fits in FIXNUM_VALBITS
                              Else bignum, if bignum support available
                              Else, cons of top 15 bits, bottom 16 bits
        *       32      > 1   Vector of the above

   NOTE NOTE NOTE:
   Format == 32 means that the buffer will be C longs, which need not be
   32-bit quantities.  See the note in select-x.c (x_get_window_property).

   When converting a Lisp integer to C, it is assumed to be of format 16 if it
   can be represented as a 16-bit twos' complement integer, and of format 32
   if not.

   When converting a vector of integers from Lisp to C, it is assumed to be of
   format 16 if every element in the vector can be represented as a 16-bit
   twos' complement integer, and of format 32 if any element cannot.

   When converting an object to C, it may be of the form (SYMBOL . <data>)
   where SYMBOL is what we should claim that the type is.  Format and
   representation are as above, with the complication that if SYMBOL is
   TIMESTAMP, an integer in DATA is treated as unsigned.

   NOTE: Under Mule, when someone shoves us a string without a type, we
   set the type to `COMPOUND_TEXT' and automatically convert to Compound
   Text.  If the string has a type, we assume that the user wants the
   data sent as-is so we just do "binary" conversion.
 */


static Lisp_Object
selection_data_to_lisp_data (struct device *d,
			     const Rawbyte *data,
			     Bytecount size,
			     XE_ATOM_TYPE type,
			     int fermat)
{
#ifdef THIS_IS_X
  if (type == DEVICE_XATOM_NULL (d))
    return QNULL;

  /* Convert any 8-bit data to a string, for compactness. */
  else if (fermat == 8)
    return make_extstring ((Extbyte *) data, size,
			    type == DEVICE_XATOM_TEXT (d) ||
			    type == DEVICE_XATOM_COMPOUND_TEXT (d)
			    ? Qctext : Qbinary);

  /* Convert a single atom to a Lisp Symbol.
     Convert a set of atoms to a vector of symbols. */
  else if (type == XA_ATOM)
#else
  if (type == gdk_atom_intern ("NULL", 0))
    return QNULL;

  /* Convert any 8-bit data to a string, for compactness. */
  else if (fermat == 8)
    return make_extstring ((Extbyte *) data, size,
			    ((type == gdk_atom_intern ("TEXT", FALSE)) ||
			     (type == gdk_atom_intern ("COMPOUND_TEXT", FALSE)))
			    ? Qctext : Qbinary);

  /* Convert a single atom to a Lisp Symbol.
     Convert a set of atoms to a vector of symbols. */
  else if (type == gdk_atom_intern ("ATOM", FALSE))
#endif /* THIS_IS_X */
    {
      if (size == sizeof (XE_ATOM_TYPE))
	return XE_ATOM_TO_SYMBOL (d, *((XE_ATOM_TYPE *) data));
      else
	{
	  Elemcount i;
	  Elemcount len = size / sizeof (XE_ATOM_TYPE);
	  Lisp_Object v = Fmake_vector (make_fixnum (len), Qzero);
	  for (i = 0; i < len; i++)
	    Faset (v, make_fixnum (i),
                   XE_ATOM_TO_SYMBOL (d, ((XE_ATOM_TYPE *) data) [i]));
	  return v;
	}
    }
  /* Convert a single 16 or 32 bit number to a Lisp integer. If bignums are
     not available, selection_data_to_lisp_data() can return a cons, with the
     car a fixnum containing the higher-order 16 bits, the cdr the lower-order
     16 bits. */
  else if (fermat == 16 && size == sizeof (INT_16_BIT))
    {
      return make_fixnum ((EMACS_INT) (((INT_16_BIT *) data) [0]));
    }
  else if (fermat == 32 && size == sizeof (INT_32_BIT))
    {
      if (type == DEVICE_XATOM_TIMESTAMP (d))
        {
          return uint32_t_to_lisp (((UINT_32_BIT *) data) [0]);
        }

      return int32_t_to_lisp (((INT_32_BIT *) data) [0]);
    }
  else if (fermat == 32 && size == sizeof (long) &&
           sizeof (long) != sizeof (INT_32_BIT))
    {
#ifdef THIS_IS_X
      if (type == DEVICE_XATOM_TIMESTAMP (d))
        {
          return uint32_t_to_lisp ((UINT_32_BIT)(*((long *) data)));
        }
#endif

      /* Sigh. 32 bit values are always passed back as longs, independent of
         the size of longs. */
      return int32_t_to_lisp ((INT_32_BIT)(*((long *) data)));
    }

  /* Convert any other kind of data to a vector of numbers, represented
     as above (as an integer, or a cons of two 16 bit integers).

     #### Perhaps we should return the actual type to lisp as well.

	(x-get-selection-internal 'PRIMARY 'LINE_NUMBER)
	==> [4 4]

     and perhaps it should be

	(x-get-selection-internal 'PRIMARY 'LINE_NUMBER)
	==> (SPAN . [4 4])

     Right now the fact that the return type was SPAN is discarded before
     lisp code gets to see it.
   */
  else if (fermat == 16)
    {
      Elemcount i, count = size / 2;
      Lisp_Object v = make_vector (count, Qzero);
      for (i = 0; i < count; i++)
	{
	  int j = (int) ((INT_16_BIT *) data) [i];
	  Faset (v, make_fixnum (i), make_fixnum (j));
	}
      return v;
    }
  else if (fermat == 32 && sizeof (long) == 4)
    {
      Elemcount i;
      Lisp_Object v = make_vector (size / 4, Qzero);
      for (i = 0; i < size / 4; i++)
	{
	  INT_32_BIT j = ((INT_32_BIT *) data) [i];
	  XVECTOR_DATA (v) [i] = int32_t_to_lisp (j);
	}
      return v;
    }
  else if (fermat == 32)
    {
      Elemcount ii, count = size / sizeof (long);
      Lisp_Object v = make_vector (count, Qzero);

      for (ii = 0; ii < count; ++ii)
        {
          INT_32_BIT jj = (INT_32_BIT) (((long *)(data))[ii]);
          XVECTOR_DATA (v) [ii] = int32_t_to_lisp (jj);
        }

      return v;
    }

  warn_when_safe (Vwindow_system, Qselection_conversion_error,
                  "selection_data_to_lisp_data: format %d: not understood",
                  fermat);

  return Qnil;
}

static void
lisp_data_to_selection_data (struct device *d,
			     Lisp_Object obj,
			     Rawbyte **data_ret,
			     XE_ATOM_TYPE *type_ret,
			     Bytecount *size_ret,
			     int *format_ret)
{
  Lisp_Object type = Qnil;

  if (CONSP (obj) && SYMBOLP (XCAR (obj)))
    {
      type = XCAR (obj);
      obj = XCDR (obj);
      if (CONSP (obj) && NILP (XCDR (obj)))
	obj = XCAR (obj);
    }

  if (EQ (obj, QNULL) || (EQ (type, QNULL)))
    {				/* This is not the same as declining */
      *format_ret = 32;
      *size_ret = 0;
      *data_ret = 0;
      type = QNULL;
    }
  else if (STRINGP (obj))
    {
      const Extbyte *extval;
      Bytecount extvallen;

      LISP_STRING_TO_SIZED_EXTERNAL (obj, extval, extvallen,
				     (NILP (type) ? Qctext : Qbinary));
      *format_ret = 8;
      *size_ret = extvallen;
      *data_ret = xnew_rawbytes (*size_ret);
      memcpy (*data_ret, extval, *size_ret);
#ifdef MULE
      if (NILP (type)) type = QCOMPOUND_TEXT;
#else
      if (NILP (type)) type = QSTRING;
#endif
    }
  else if (CHARP (obj))
    {
      Ibyte buf[MAX_ICHAR_LEN];
      Bytecount len;
      const Extbyte *extval;
      Bytecount extvallen;

      *format_ret = 8;
      len = set_itext_ichar (buf, XCHAR (obj));
      TO_EXTERNAL_FORMAT (DATA, (buf, len),
			  ALLOCA, (extval, extvallen),
			  Qctext);
      *size_ret = extvallen;
      *data_ret = xnew_rawbytes (*size_ret);
      memcpy (*data_ret, extval, *size_ret);
#ifdef MULE
      if (NILP (type)) type = QCOMPOUND_TEXT;
#else
      if (NILP (type)) type = QSTRING;
#endif
    }
  else if (SYMBOLP (obj))
    {
      *format_ret = 32;
      *size_ret = 1;
      *data_ret = xnew_rawbytes (sizeof (XE_ATOM_TYPE) + 1);
      (*data_ret) [sizeof (XE_ATOM_TYPE)] = 0;
      (*(XE_ATOM_TYPE **) data_ret) [0] = XE_SYMBOL_TO_ATOM (d, obj, 0);
      if (NILP (type)) type = QATOM;
    }
  else if (FIXNUMP (obj) && !((EMACS_UINT) (XREALFIXNUM (obj)) & ~0xFFFF))
    {
    sixteen_bit_ok:
      *format_ret = 16;
      *size_ret = 1;
      *data_ret = xnew_rawbytes (sizeof (INT_16_BIT) + 1);
      (*data_ret) [sizeof (INT_16_BIT)] = 0;
      (*(INT_16_BIT **) data_ret) [0] = (INT_16_BIT) XFIXNUM (obj);
      if (NILP (type)) type = QINTEGER;
    }
  else if (EQ (type, QTIMESTAMP) && (INTEGERP (obj) || CONSP (obj)))
    {
      /* We used to treat all our 32-bit integer values as unsigned, which is
         wrong according to the X documentation. However, treating 32-bit
         timestamps as signed isn't practical. Special-case them as
         unsigned.

         Mozilla and Chrome have special magic so timestamps don't wrap around
         on long-running processes, and this may be reasonable for us too. */
      UINT_32_BIT staging = lisp_to_uint32_t (obj);

      *format_ret = 32;
      *size_ret = 1;
      /* Each element in DATA_RET must be sizeof (long) in length, even when
         FORMAT_RET is 32 and sizeof (long) is e.g. 64. */
      *data_ret = xnew_rawbytes (sizeof (long) + 1);
      (*data_ret) [sizeof (long)] = 0;
      (*(long **) data_ret) [0] = staging;
    }
  else if (INTEGERP (obj) || CONSP (obj))
    {
      /* lisp_to_int32_t() can error, call it before allocating anything. */
      INT_32_BIT staging = lisp_to_int32_t (obj);

      if (!((UINT_32_BIT) staging & ~0xFFFF))
        {
          obj = make_fixnum (staging);
          goto sixteen_bit_ok;
        }

      *format_ret = 32;
      *size_ret = 1;
      /* Each element in DATA_RET must be sizeof (long) in length, even when
         FORMAT_RET is 32 and sizeof (long) is e.g. 64. */
      *data_ret = xnew_rawbytes (sizeof (long) + 1);
      (*data_ret) [sizeof (long)] = 0;
      (*(long **) data_ret) [0] = staging;
      if (NILP (type)) type = QINTEGER;
    }
  else if (VECTORP (obj))
    {
      /* Lisp Vectors may represent a set of ATOMs;
	 a set of 16 or 32 bit INTEGERs;
	 or a set of ATOM_PAIRs (represented as [[A1 A2] [A3 A4] ...]
       */
      Elemcount i;

      if (SYMBOLP (XVECTOR_DATA (obj) [0]))
	/* This vector is an ATOM set */
	{
          /* Type-check before allocation. */
          for (i = 0; i < XVECTOR_LENGTH (obj); i++)
            {
              CHECK_SYMBOL (XVECTOR_DATA (obj)[i]);
            }

	  if (NILP (type)) type = QATOM;
	  *size_ret = XVECTOR_LENGTH (obj);
	  *format_ret = 32;
	  *data_ret = xnew_rawbytes ((*size_ret) * sizeof (XE_ATOM_TYPE));
	  for (i = 0; i < *size_ret; i++)
            {
              (*(XE_ATOM_TYPE **) data_ret) [i] =
                XE_SYMBOL_TO_ATOM (d, XVECTOR_DATA (obj) [i], 0);
            }

	}
#if 0 /* #### MULTIPLE doesn't work yet */
      else if (VECTORP (XVECTOR_DATA (obj) [0]))
	/* This vector is an ATOM_PAIR set */
	{
	  if (NILP (type)) type = QATOM_PAIR;
	  *size_ret = XVECTOR_LENGTH (obj);
	  *format_ret = 32;
	  *data_ret =
	    xnew_rawbytes ((*size_ret) * sizeof (XE_ATOM_TYPE) * 2);
	  for (i = 0; i < *size_ret; i++)
	    if (VECTORP (XVECTOR_DATA (obj) [i]))
	      {
		Lisp_Object pair = XVECTOR_DATA (obj) [i];
		if (XVECTOR_LENGTH (pair) != 2)
		  syntax_error
		    ("elements of the vector must be vectors of exactly two elements", pair);

		(*(XE_ATOM_TYPE **) data_ret) [i * 2] =
		  XE_SYMBOL_TO_ATOM (d, XVECTOR_DATA (pair) [0], 0);
		(*(XE_ATOM_TYPE **) data_ret) [(i * 2) + 1] =
		  XE_SYMBOL_TO_ATOM (d, XVECTOR_DATA (pair) [1], 0);
	      }
	    else
	      syntax_error
		("all elements of the vector must be of the same type", obj);
	}
#endif
      else
	/* This vector is an INTEGER set, or something like it */
	{
	  *format_ret = 16;
	  *size_ret = XVECTOR_LENGTH (obj);

	  for (i = 0; i < *size_ret; i++)
            {
              /* Dry-run conversion for type checking, so we error before any
                 allocation. */
              INT_32_BIT checked = lisp_to_int32_t (XVECTOR_DATA (obj)[i]);

              if ((UINT_32_BIT) checked & ~0xFFFF)
                {
                  *format_ret = 32;
                }
            }

	  if (NILP (type)) type = QINTEGER;
	  *data_ret = xnew_rawbytes (*size_ret *
                                     (*format_ret == 16 ? 2 : sizeof (long)));

          if (*format_ret == 32)
            {
              for (i = 0; i < *size_ret; i++)
                {
                  /* Yes, this is intentionally long **. See
                     select-x.c and the XChangeProperty man page. */
                  (*((long **) data_ret)) [i] =
                    lisp_to_int32_t (XVECTOR_DATA (obj) [i]);
                }
            }
          else
            {
              for (i = 0; i < *size_ret; i++)
                {
                  (*((INT_16_BIT **) data_ret)) [i] =
                    (INT_16_BIT) lisp_to_int32_t (XVECTOR_DATA (obj) [i]);
                }
            }
        }
    }
  else
    invalid_argument ("unrecognized selection data", obj);

  *type_ret = XE_SYMBOL_TO_ATOM (d, type, 0);
}

