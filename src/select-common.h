/* Selection processing for XEmacs -- common btwn select-x.c and select-gtk.c
   Copyright (C) 1990, 1991, 1992, 1993, 1994 Free Software Foundation, Inc.

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

/* Synched up with: Not synched with FSF. */

#ifdef PROCESSING_X_CODE
#define XE_ATOM_TYPE Atom
#define XE_ATOM_TO_SYMBOL x_atom_to_symbol
#define XE_SYMBOL_TO_ATOM symbol_to_x_atom
#else
#define XE_ATOM_TYPE GdkAtom
#define XE_ATOM_TO_SYMBOL atom_to_symbol
#define XE_SYMBOL_TO_ATOM symbol_to_gtk_atom
#endif /* PROCESSING_X_CODE */

/* #### These are going to move into Lisp code(!) with the aid of
        some new functions I'm working on - ajh */

/* These functions convert from the selection data read from the server into
   something that we can use from elisp, and vice versa.

	Type:	Format:	Size:		Elisp Type:
	-----	-------	-----		-----------
	*	8	*		String
	ATOM	32	1		Symbol
	ATOM	32	> 1		Vector of Symbols
	*	16	1		Integer
	*	16	> 1		Vector of Integers
	*	32	1		if <=16 bits: Integer
					if > 16 bits: Cons of top16, bot16
	*	32	> 1		Vector of the above

   When converting a Lisp number to C, it is assumed to be of format 16 if
   it is an integer, and of format 32 if it is a cons of two integers.

   When converting a vector of numbers from Elisp to C, it is assumed to be
   of format 16 if every element in the vector is an integer, and is assumed
   to be of format 32 if any element is a cons of two integers.

   When converting an object to C, it may be of the form (SYMBOL . <data>)
   where SYMBOL is what we should claim that the type is.  Format and
   representation are as above.

   NOTE: Under Mule, when someone shoves us a string without a type, we
   set the type to 'COMPOUND_TEXT and automatically convert to Compound
   Text.  If the string has a type, we assume that the user wants the
   data sent as-is so we just do "binary" conversion.
 */


static Lisp_Object
selection_data_to_lisp_data (struct device *d,
			     UChar_Binary *data,
			     Bytecount size,
			     XE_ATOM_TYPE type,
			     int format)
{
#ifdef PROCESSING_X_CODE
  if (type == DEVICE_XATOM_NULL (d))
    return QNULL;

  /* Convert any 8-bit data to a string, for compactness. */
  else if (format == 8)
    return make_ext_string ((Extbyte *) data, size,
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
  else if (format == 8)
    return make_ext_string ((Extbyte *) data, size,
			    ((type == gdk_atom_intern ("TEXT", FALSE)) ||
			     (type == gdk_atom_intern ("COMPOUND_TEXT", FALSE)))
			    ? Qctext : Qbinary);

  /* Convert a single atom to a Lisp Symbol.
     Convert a set of atoms to a vector of symbols. */
  else if (type == gdk_atom_intern ("ATOM", FALSE))
#endif /* PROCESSING_X_CODE */
    {
      if (size == sizeof (XE_ATOM_TYPE))
	return XE_ATOM_TO_SYMBOL (d, *((XE_ATOM_TYPE *) data));
      else
	{
	  Elemcount i;
	  Elemcount len = size / sizeof (XE_ATOM_TYPE);
	  Lisp_Object v = Fmake_vector (make_int (len), Qzero);
	  for (i = 0; i < len; i++)
	    Faset (v, make_int (i), XE_ATOM_TO_SYMBOL (d, ((XE_ATOM_TYPE *) data) [i]));
	  return v;
	}
    }

  /* Convert a single 16 or small 32 bit number to a Lisp Int.
     If the number is > 16 bits, convert it to a cons of integers,
     16 bits in each half.
   */
  else if (format == 32 && size == sizeof (long))
    return word_to_lisp (((unsigned long *) data) [0]);
  else if (format == 16 && size == sizeof (short))
    return make_int ((int) (((unsigned short *) data) [0]));

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
  else if (format == 16)
    {
      Elemcount i;
      Lisp_Object v = make_vector (size / 4, Qzero);
      for (i = 0; i < size / 4; i++)
	{
	  int j = (int) ((unsigned short *) data) [i];
	  Faset (v, make_int (i), make_int (j));
	}
      return v;
    }
  else
    {
      Elemcount i;
      Lisp_Object v = make_vector (size / 4, Qzero);
      for (i = 0; i < size / 4; i++)
	{
	  unsigned long j = ((unsigned long *) data) [i];
	  Faset (v, make_int (i), word_to_lisp (j));
	}
      return v;
    }
}


static void
lisp_data_to_selection_data (struct device *d,
			     Lisp_Object obj,
			     UChar_Binary **data_ret,
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

      TO_EXTERNAL_FORMAT (LISP_STRING, obj,
			  ALLOCA, (extval, extvallen),
			  (NILP (type) ? Qctext : Qbinary));
      *format_ret = 8;
      *size_ret = extvallen;
      *data_ret = (UChar_Binary *) xmalloc (*size_ret);
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
      *data_ret = (UChar_Binary *) xmalloc (*size_ret);
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
      *data_ret = (UChar_Binary *) xmalloc (sizeof (XE_ATOM_TYPE) + 1);
      (*data_ret) [sizeof (XE_ATOM_TYPE)] = 0;
      (*(XE_ATOM_TYPE **) data_ret) [0] = XE_SYMBOL_TO_ATOM (d, obj, 0);
      if (NILP (type)) type = QATOM;
    }
  else if (INTP (obj) &&
	   XINT (obj) <= 0x7FFF &&
	   XINT (obj) >= -0x8000)
    {
      *format_ret = 16;
      *size_ret = 1;
      *data_ret = (UChar_Binary *) xmalloc (sizeof (short) + 1);
      (*data_ret) [sizeof (short)] = 0;
      (*(short **) data_ret) [0] = (short) XINT (obj);
      if (NILP (type)) type = QINTEGER;
    }
  else if (INTP (obj) || CONSP (obj))
    {
      *format_ret = 32;
      *size_ret = 1;
      *data_ret = (UChar_Binary *) xmalloc (sizeof (long) + 1);
      (*data_ret) [sizeof (long)] = 0;
      (*(unsigned long **) data_ret) [0] = lisp_to_word (obj);
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
	  if (NILP (type)) type = QATOM;
	  *size_ret = XVECTOR_LENGTH (obj);
	  *format_ret = 32;
	  *data_ret = (UChar_Binary *) xmalloc ((*size_ret) * sizeof (XE_ATOM_TYPE));
	  for (i = 0; i < *size_ret; i++)
	    if (SYMBOLP (XVECTOR_DATA (obj) [i]))
	      (*(XE_ATOM_TYPE **) data_ret) [i] =
		XE_SYMBOL_TO_ATOM (d, XVECTOR_DATA (obj) [i], 0);
	    else
              syntax_error
		("all elements of the vector must be of the same type", obj);
	}
#if 0 /* #### MULTIPLE doesn't work yet */
      else if (VECTORP (XVECTOR_DATA (obj) [0]))
	/* This vector is an ATOM_PAIR set */
	{
	  if (NILP (type)) type = QATOM_PAIR;
	  *size_ret = XVECTOR_LENGTH (obj);
	  *format_ret = 32;
	  *data_ret = (UChar_Binary *)
	    xmalloc ((*size_ret) * sizeof (XE_ATOM_TYPE) * 2);
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
	  *size_ret = XVECTOR_LENGTH (obj);
	  if (NILP (type)) type = QINTEGER;
	  *format_ret = 16;
	  for (i = 0; i < *size_ret; i++)
	    if (CONSP (XVECTOR_DATA (obj) [i]))
	      *format_ret = 32;
	    else if (!INTP (XVECTOR_DATA (obj) [i]))
	      syntax_error
		("all elements of the vector must be integers or conses of integers", obj);

	  *data_ret = (UChar_Binary *) xmalloc (*size_ret * (*format_ret/8));
	  for (i = 0; i < *size_ret; i++)
	    if (*format_ret == 32)
	      (*((unsigned long **) data_ret)) [i] =
		lisp_to_word (XVECTOR_DATA (obj) [i]);
	    else
	      (*((unsigned short **) data_ret)) [i] =
		(unsigned short) lisp_to_word (XVECTOR_DATA (obj) [i]);
	}
    }
  else
    invalid_argument ("unrecognized selection data", obj);

  *type_ret = XE_SYMBOL_TO_ATOM (d, type, 0);
}

