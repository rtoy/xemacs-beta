/* Header for charsets.
   Copyright (C) 1992, 1995 Free Software Foundation, Inc.
   Copyright (C) 1995 Sun Microsystems, Inc.
   Copyright (C) 2001, 2002 Ben Wing.

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

/* Synched up with: Mule 2.3.  Not in FSF. */

/* Rewritten by Ben Wing <ben@xemacs.org>. */

#ifndef INCLUDED_charset_h_
#define INCLUDED_charset_h_



#ifndef MULE

/************************************************************************/
/*                            fake charset defs                         */
/************************************************************************/

/* used when MULE is not defined, so that Charset-type stuff can still
   be done */

#define Vcharset_ascii Qnil

#define CHAR_CHARSET(ch) Vcharset_ascii
#define CHAR_LEADING_BYTE(ch) LEADING_BYTE_ASCII
#define LEADING_BYTE_ASCII 0x80
#define NUM_LEADING_BYTES 1
#define MIN_LEADING_BYTE 0x80
#define CHARSETP(cs) 1
#define CHARSET_BY_LEADING_BYTE(lb) Vcharset_ascii
#define XCHARSET_LEADING_BYTE(cs) LEADING_BYTE_ASCII
#define XCHARSET_GRAPHIC(cs) -1
#define XCHARSET_COLUMNS(cs) 1
#define XCHARSET_DIMENSION(cs) 1
#define BREAKUP_CHAR(ch, charset, byte1, byte2) do {	\
  (charset) = Vcharset_ascii;				\
  (byte1) = (ch);					\
  (byte2) = 0;						\
} while (0)

#else /* MULE */


/************************************************************************/
/*                    Definition of leading bytes                       */
/************************************************************************/

#define MIN_LEADING_BYTE		0x7F

/** The following are for 1-byte characters in an official charset. **/
enum LEADING_BYTE_OFFICIAL_1
{
  MIN_LEADING_BYTE_OFFICIAL_1 = 0x80,
  /* LEADING_BYTE_LATIN_ISO8859_1 *MUST* be equal to
     MIN_LEADING_BYTE_OFFICIAL_1. */
  LEADING_BYTE_LATIN_ISO8859_1 =  /* 0x80 Right half of ISO 8859-1 */
    MIN_LEADING_BYTE_OFFICIAL_1,
  LEADING_BYTE_LATIN_ISO8859_2,   /* 0x81 Right half of ISO 8859-2 */
  LEADING_BYTE_LATIN_ISO8859_3,   /* 0x82 Right half of ISO 8859-3 */
  LEADING_BYTE_LATIN_ISO8859_4,   /* 0x83 Right half of ISO 8859-4 */
  LEADING_BYTE_THAI_TIS620,       /* 0x84 TIS620-2533 */
  LEADING_BYTE_GREEK_ISO8859_7,   /* 0x85 Right half of ISO 8859-7 */
  LEADING_BYTE_ARABIC_ISO8859_6,  /* 0x86 Right half of ISO 8859-6 */
  LEADING_BYTE_HEBREW_ISO8859_8,  /* 0x87 Right half of ISO 8859-8 */
  LEADING_BYTE_KATAKANA_JISX0201, /* 0x88 Right half of JIS X0201-1976 */
  LEADING_BYTE_LATIN_JISX0201,    /* 0x89 Left  half of JIS X0201-1976 */
  LEADING_BYTE_CYRILLIC_ISO8859_5,/* 0x8A Right half of ISO 8859-5 */
  LEADING_BYTE_LATIN_ISO8859_9,   /* 0x8B Right half of ISO 8859-9 */
  LEADING_BYTE_LATIN_ISO8859_15,  /* 0x8C Right half of ISO 8859-15 */
#ifdef ENABLE_COMPOSITE_CHARS
  LEADING_BYTE_COMPOSITE,         /* 0x8D For a composite character */
  MAX_LEADING_BYTE_OFFICIAL_1 =
    LEADING_BYTE_COMPOSITE - 1,
#else
    /* Does not need to be the last entry, but simplifies things */
  LEADING_BYTE_COMPOSITE_REPLACEMENT, /* 0x8D Replaces ESC 0 - ESC 4 in a
                                         buffer */
  MAX_LEADING_BYTE_OFFICIAL_1 =
    LEADING_BYTE_COMPOSITE_REPLACEMENT,
#endif
                                  /* 0x8E Unused */
};

/* These next 2 + LEADING_BYTE_COMPOSITE need special treatment in a string
   and/or character */

#define LEADING_BYTE_ASCII		0x7F /* Not used except in arrays
						indexed by leading byte */
#define LEADING_BYTE_CONTROL_1          0x8F /* represent normal 80-9F */

/** The following are for 2-byte characters in an official charset. **/
enum LEADING_BYTE_OFFICIAL_2
{
  MIN_LEADING_BYTE_OFFICIAL_2 = 0x90,
  LEADING_BYTE_JAPANESE_JISX0208_1978 =
    MIN_LEADING_BYTE_OFFICIAL_2,       /* 0x90 Japanese JIS X0208-1978 */
  LEADING_BYTE_CHINESE_GB2312,         /* 0x91 Chinese Hanzi GB2312-1980 */
  LEADING_BYTE_JAPANESE_JISX0208,      /* 0x92 Japanese JIS X0208-1983 */
  LEADING_BYTE_KOREAN_KSC5601,         /* 0x93 Hangul KS C5601-1987 */
  LEADING_BYTE_JAPANESE_JISX0212,      /* 0x94 Japanese JIS X0212-1990 */
  LEADING_BYTE_CHINESE_CNS11643_1,     /* 0x95 Chinese CNS11643 Set 1 */
  LEADING_BYTE_CHINESE_CNS11643_2,     /* 0x96 Chinese CNS11643 Set 2 */
  LEADING_BYTE_CHINESE_BIG5_1,         /* 0x97 Big5 Level 1 */
  LEADING_BYTE_CHINESE_BIG5_2,         /* 0x98 Big5 Level 2 */
  MAX_LEADING_BYTE_OFFICIAL_2 =
    LEADING_BYTE_CHINESE_BIG5_2,

                                       /* 0x99 unused */
                                       /* 0x9A unused */
                                       /* 0x9B unused */
                                       /* 0x9C unused */
                                       /* 0x9D unused */
};


/** The following are for 1- and 2-byte characters in a private charset. **/

#define PRE_LEADING_BYTE_PRIVATE_1	0x9E	/* 1-byte char-set */
#define PRE_LEADING_BYTE_PRIVATE_2	0x9F	/* 2-byte char-set */

#define MIN_LEADING_BYTE_PRIVATE_1	0xA0
#define MAX_LEADING_BYTE_PRIVATE_1	0xEF
#define MIN_LEADING_BYTE_PRIVATE_2	0xF0
#define MAX_LEADING_BYTE_PRIVATE_2	0xFF

#define NUM_LEADING_BYTES 129


/************************************************************************/
/*                    Operations on leading bytes                       */
/************************************************************************/

/* Is this leading byte for a private charset? */

#define LEADING_BYTE_PRIVATE_P(lb) ((lb) >= MIN_LEADING_BYTE_PRIVATE_1)

/* Is this a prefix for a private leading byte? */

INLINE_HEADER int LEADING_BYTE_PREFIX_P (Intbyte lb);
INLINE_HEADER int
LEADING_BYTE_PREFIX_P (Intbyte lb)
{
  return (lb == PRE_LEADING_BYTE_PRIVATE_1 ||
	  lb == PRE_LEADING_BYTE_PRIVATE_2);
}

/* Given a private leading byte, return the leading byte prefix stored
   in a string. */

#define PRIVATE_LEADING_BYTE_PREFIX(lb)			\
  ((unsigned int) (lb) < MIN_LEADING_BYTE_PRIVATE_2 ?	\
   PRE_LEADING_BYTE_PRIVATE_1 :				\
   PRE_LEADING_BYTE_PRIVATE_2)


/************************************************************************/
/*            Information about a particular character set              */
/************************************************************************/

struct Lisp_Charset
{
  struct lcrecord_header header;

  int id;
  Lisp_Object name;
  Lisp_Object doc_string;
  Lisp_Object registry;
  Lisp_Object short_name;
  Lisp_Object long_name;

  Lisp_Object reverse_direction_charset;

  Lisp_Object ccl_program;

  /* Unicode translation tables.  See unicode.c for the format of these tables.

     NOTE ABOUT DUMPING: We don't currently dump these tables.  For one
     thing, they're big, and it's very fast to recreate them (a fraction of
     a second on modern processors); for another, there isn't currently
     support in pdump for this. (At one point I started writing it, in the
     form of XD_UNION, but thought better of it.) In order to avoid pdump
     problems (and to make the dump data smaller), we free all the tables
     at dump time (using the disksave finalization method), set them to
     zero, and reinit them to blank when we start up again; they will be
     reloaded when `init-mule-at-startup' is called.

     #### This creates a problem w.r.t. user-defined charsets.  It would be
     inconvenient to require all dumped user-defined charsets to be
     reloaded at init time, but that's what currently needs to be done.
     The solution is either to go ahead and create pdump support for
     dumping the tables, and use it just for these; or to copy the data in
     them into a compressed format, write it out to pdump as a single
     opaque block, and undo the operation at startup.

     #### There's another problem, having to do with starting up in a
     non-ISO-8859-1 directory.  We don't load the tables until after we've
     parsed the current directories, and we run into a real bootstrapping
     problem trying to solve this.  Either we'll have to dump all the
     tables as-is or make some special provisions for writing extra binary
     data into the pdump data, and do the compressed trick mentioned above
     for all the tables. */
  void *to_unicode_table;
  void *from_unicode_table;
  int from_unicode_levels;

  unsigned char unicode_table_loaded;

  /* Final byte of this character set in ISO2022 designating escape sequence */
  Intbyte final;

  /* Number of bytes (1 - 4) required in the internal representation
     for characters in this character set.  This is *not* the
     same as the dimension of the character set). */
  int rep_bytes;

  /* Number of columns a character in this charset takes up, on TTY
     devices.  Not used for X devices. */
  int columns;

  /* Direction of this character set */
  int direction;

  /* Type of this character set (94, 96, 94x94, 96x96) */
  int type;

  /* Number of bytes used in encoding of this character set (1 or 2) */
  int dimension;

  /* Number of chars in each dimension (usually 94 or 96) */
  int chars;

  /* Which half of font to be used to display this character set */
  int graphic;

  /* If set, this is a "temporary" charset created when we encounter
     an unknown final.  This is so that we can successfully compile
     and load such files.  We allow a real charset to be created on top
     of this temporary charset. */
  unsigned int temporary :1;
};
typedef struct Lisp_Charset Lisp_Charset;

DECLARE_LRECORD (charset, Lisp_Charset);
#define XCHARSET(x) XRECORD (x, charset, Lisp_Charset)
#define XSETCHARSET(x, p) XSETRECORD (x, p, charset)
#define wrap_charset(p) wrap_record (p, charset)
#define CHARSETP(x) RECORDP (x, charset)
#define CHECK_CHARSET(x) CHECK_RECORD (x, charset)
#define CONCHECK_CHARSET(x) CONCHECK_RECORD (x, charset)

#define CHARSET_TYPE_94    0	/* This charset includes 94    characters. */
#define CHARSET_TYPE_96    1	/* This charset includes 96    characters. */
#define CHARSET_TYPE_94X94 2	/* This charset includes 94x94 characters. */
#define CHARSET_TYPE_96X96 3	/* This charset includes 96x96 characters. */

#define CHARSET_LEFT_TO_RIGHT	0
#define CHARSET_RIGHT_TO_LEFT	1

/* Leading byte and id have been regrouped. -- OG */
#define CHARSET_ID(cs)		 ((cs)->id)
#define CHARSET_LEADING_BYTE(cs) ((Intbyte) CHARSET_ID (cs))
#define CHARSET_NAME(cs)	 ((cs)->name)
#define CHARSET_SHORT_NAME(cs)	 ((cs)->short_name)
#define CHARSET_LONG_NAME(cs)	 ((cs)->long_name)
#define CHARSET_REP_BYTES(cs)	 ((cs)->rep_bytes)
#define CHARSET_COLUMNS(cs)	 ((cs)->columns)
#define CHARSET_GRAPHIC(cs)	 ((cs)->graphic)
#define CHARSET_TYPE(cs)	 ((cs)->type)
#define CHARSET_DIRECTION(cs)	 ((cs)->direction)
#define CHARSET_FINAL(cs)	 ((cs)->final)
#define CHARSET_DOC_STRING(cs)	 ((cs)->doc_string)
#define CHARSET_REGISTRY(cs)	 ((cs)->registry)
#define CHARSET_CCL_PROGRAM(cs)  ((cs)->ccl_program)
#define CHARSET_DIMENSION(cs)	 ((cs)->dimension)
#define CHARSET_CHARS(cs)	 ((cs)->chars)
#define CHARSET_REVERSE_DIRECTION_CHARSET(cs) ((cs)->reverse_direction_charset)
#define CHARSET_TO_UNICODE_TABLE(cs) ((cs)->to_unicode_table)
#define CHARSET_FROM_UNICODE_TABLE(cs) ((cs)->from_unicode_table)
#define CHARSET_FROM_UNICODE_LEVELS(cs) ((cs)->from_unicode_levels)


#define CHARSET_PRIVATE_P(cs) LEADING_BYTE_PRIVATE_P (CHARSET_LEADING_BYTE (cs))

#define XCHARSET_ID(cs)		  CHARSET_ID           (XCHARSET (cs))
#define XCHARSET_NAME(cs)	  CHARSET_NAME         (XCHARSET (cs))
#define XCHARSET_SHORT_NAME(cs)	  CHARSET_SHORT_NAME   (XCHARSET (cs))
#define XCHARSET_LONG_NAME(cs)	  CHARSET_LONG_NAME    (XCHARSET (cs))
#define XCHARSET_REP_BYTES(cs)	  CHARSET_REP_BYTES    (XCHARSET (cs))
#define XCHARSET_COLUMNS(cs)	  CHARSET_COLUMNS      (XCHARSET (cs))
#define XCHARSET_GRAPHIC(cs)      CHARSET_GRAPHIC      (XCHARSET (cs))
#define XCHARSET_TYPE(cs)	  CHARSET_TYPE         (XCHARSET (cs))
#define XCHARSET_DIRECTION(cs)	  CHARSET_DIRECTION    (XCHARSET (cs))
#define XCHARSET_FINAL(cs)	  CHARSET_FINAL        (XCHARSET (cs))
#define XCHARSET_DOC_STRING(cs)	  CHARSET_DOC_STRING   (XCHARSET (cs))
#define XCHARSET_REGISTRY(cs)	  CHARSET_REGISTRY     (XCHARSET (cs))
#define XCHARSET_LEADING_BYTE(cs) CHARSET_LEADING_BYTE (XCHARSET (cs))
#define XCHARSET_CCL_PROGRAM(cs)  CHARSET_CCL_PROGRAM  (XCHARSET (cs))
#define XCHARSET_DIMENSION(cs)	  CHARSET_DIMENSION    (XCHARSET (cs))
#define XCHARSET_CHARS(cs)	  CHARSET_CHARS        (XCHARSET (cs))
#define XCHARSET_PRIVATE_P(cs)	  CHARSET_PRIVATE_P    (XCHARSET (cs))
#define XCHARSET_REVERSE_DIRECTION_CHARSET(cs) \
  CHARSET_REVERSE_DIRECTION_CHARSET (XCHARSET (cs))
#define XCHARSET_TO_UNICODE_TABLE(cs) \
  CHARSET_TO_UNICODE_TABLE (XCHARSET (cs))
#define XCHARSET_FROM_UNICODE_TABLE(cs) \
  CHARSET_FROM_UNICODE_TABLE (XCHARSET (cs))
#define XCHARSET_FROM_UNICODE_LEVELS(cs) \
  CHARSET_FROM_UNICODE_LEVELS (XCHARSET (cs))

struct charset_lookup
{
  /* Table of charsets indexed by leading byte. */
  Lisp_Object charset_by_leading_byte[NUM_LEADING_BYTES];

  /* Table of charsets indexed by type/final-byte/direction. */
  Lisp_Object charset_by_attributes[4][128][2];
  Intbyte next_allocated_1_byte_leading_byte;
  Intbyte next_allocated_2_byte_leading_byte;
};

INLINE_HEADER Lisp_Object CHARSET_BY_LEADING_BYTE (int lb);
INLINE_HEADER Lisp_Object
CHARSET_BY_LEADING_BYTE (int lb)
{
  extern struct charset_lookup *chlook;

#ifdef ERROR_CHECK_TYPECHECK
  /* When error-checking is on, x86 GCC 2.95.2 -O3 miscompiles the
     following unless we introduce `tem'. */
  int tem = lb;
  type_checking_assert (tem >= MIN_LEADING_BYTE && tem <= 0xFF);
#endif
  return chlook->charset_by_leading_byte[lb - MIN_LEADING_BYTE];
}

INLINE_HEADER Lisp_Object
CHARSET_BY_ATTRIBUTES (int type, int final, int dir);
INLINE_HEADER Lisp_Object
CHARSET_BY_ATTRIBUTES (int type, int final, int dir)
{
  extern struct charset_lookup *chlook;

  type_checking_assert (type  < countof (chlook->charset_by_attributes) &&
			final < countof (chlook->charset_by_attributes[0]) &&
			dir   < countof (chlook->charset_by_attributes[0][0]));
  return chlook->charset_by_attributes[type][final][dir];
}


/************************************************************************/
/*                        Dealing with characters                       */
/************************************************************************/

/* The bit fields of character are divided into 3 parts:
   FIELD1(5bits):FIELD2(7bits):FIELD3(7bits) */

#define CHAR_FIELD1_MASK (0x1F << 14)
#define CHAR_FIELD2_MASK (0x7F << 7)
#define CHAR_FIELD3_MASK 0x7F

/* Macros to access each field of a character code of C.  */

#define CHAR_FIELD1(c) (((c) & CHAR_FIELD1_MASK) >> 14)
#define CHAR_FIELD2(c) (((c) & CHAR_FIELD2_MASK) >> 7)
#define CHAR_FIELD3(c)  ((c) & CHAR_FIELD3_MASK)

/* Field 1, if non-zero, usually holds a leading byte for a
   dimension-2 charset.  Field 2, if non-zero, usually holds a leading
   byte for a dimension-1 charset. */

/* Converting between field values and leading bytes.  */

#define FIELD2_TO_OFFICIAL_LEADING_BYTE (MIN_LEADING_BYTE_OFFICIAL_1 - 1)
#define FIELD2_TO_PRIVATE_LEADING_BYTE  0x80

#define FIELD1_TO_OFFICIAL_LEADING_BYTE (MIN_LEADING_BYTE_OFFICIAL_2 - 1)
#define FIELD1_TO_PRIVATE_LEADING_BYTE  0xE1

/* Minimum and maximum allowed values for the fields. */

#define MIN_CHAR_FIELD2_OFFICIAL \
  (MIN_LEADING_BYTE_OFFICIAL_1 - FIELD2_TO_OFFICIAL_LEADING_BYTE)
#define MAX_CHAR_FIELD2_OFFICIAL \
  (MAX_LEADING_BYTE_OFFICIAL_1 - FIELD2_TO_OFFICIAL_LEADING_BYTE)

#define MIN_CHAR_FIELD1_OFFICIAL \
  (MIN_LEADING_BYTE_OFFICIAL_2 - FIELD1_TO_OFFICIAL_LEADING_BYTE)
#define MAX_CHAR_FIELD1_OFFICIAL \
  (MAX_LEADING_BYTE_OFFICIAL_2 - FIELD1_TO_OFFICIAL_LEADING_BYTE)

#define MIN_CHAR_FIELD2_PRIVATE \
  (MIN_LEADING_BYTE_PRIVATE_1 - FIELD2_TO_PRIVATE_LEADING_BYTE)
#define MAX_CHAR_FIELD2_PRIVATE \
  (MAX_LEADING_BYTE_PRIVATE_1 - FIELD2_TO_PRIVATE_LEADING_BYTE)

#define MIN_CHAR_FIELD1_PRIVATE \
  (MIN_LEADING_BYTE_PRIVATE_2 - FIELD1_TO_PRIVATE_LEADING_BYTE)
#define MAX_CHAR_FIELD1_PRIVATE \
  (MAX_LEADING_BYTE_PRIVATE_2 - FIELD1_TO_PRIVATE_LEADING_BYTE)

/* Minimum character code of each <type> character.  */

#define MIN_CHAR_OFFICIAL_TYPE9N    (MIN_CHAR_FIELD2_OFFICIAL <<  7)
#define MIN_CHAR_PRIVATE_TYPE9N     (MIN_CHAR_FIELD2_PRIVATE  <<  7)
#define MIN_CHAR_OFFICIAL_TYPE9NX9N (MIN_CHAR_FIELD1_OFFICIAL << 14)
#define MIN_CHAR_PRIVATE_TYPE9NX9N  (MIN_CHAR_FIELD1_PRIVATE  << 14)
#define MIN_CHAR_COMPOSITION        (0x1F << 14)

/* Leading byte of a character.

   NOTE: This takes advantage of the fact that
   FIELD2_TO_OFFICIAL_LEADING_BYTE and
   FIELD2_TO_PRIVATE_LEADING_BYTE are the same.
   */

INLINE_HEADER Intbyte CHAR_LEADING_BYTE (Emchar c);
INLINE_HEADER Intbyte
CHAR_LEADING_BYTE (Emchar c)
{
  if (CHAR_ASCII_P (c))
    return LEADING_BYTE_ASCII;
  else if (c < 0xA0)
    return LEADING_BYTE_CONTROL_1;
  else if (c < MIN_CHAR_OFFICIAL_TYPE9NX9N)
    return CHAR_FIELD2 (c) + FIELD2_TO_OFFICIAL_LEADING_BYTE;
  else if (c < MIN_CHAR_PRIVATE_TYPE9NX9N)
    return CHAR_FIELD1 (c) + FIELD1_TO_OFFICIAL_LEADING_BYTE;
  else if (c < MIN_CHAR_COMPOSITION)
    return CHAR_FIELD1 (c) + FIELD1_TO_PRIVATE_LEADING_BYTE;
  else
    {
#ifdef ENABLE_COMPOSITE_CHARS
      return LEADING_BYTE_COMPOSITE;
#else
      abort();
      return 0;
#endif /* ENABLE_COMPOSITE_CHARS */
    }
}

#define CHAR_CHARSET(c) CHARSET_BY_LEADING_BYTE (CHAR_LEADING_BYTE (c))

/* Return a character whose charset is CHARSET and position-codes are C1
   and C2.  TYPE9N character ignores C2. (For typical charsets, i.e. not
   ASCII, Control-1 or Composite, C1 and C2 will be in the range of 32 to
   127 or 33 to 126.  See `make-char'.)

   NOTE: This takes advantage of the fact that
   FIELD2_TO_OFFICIAL_LEADING_BYTE and
   FIELD2_TO_PRIVATE_LEADING_BYTE are the same.
   */

INLINE_HEADER Emchar MAKE_CHAR_1 (Lisp_Object charset, int c1, int c2);
INLINE_HEADER Emchar
MAKE_CHAR_1 (Lisp_Object charset, int c1, int c2)
{
  if (EQ (charset, Vcharset_ascii))
    return c1;
  else if (EQ (charset, Vcharset_control_1))
    return c1 | 0x80;
#ifdef ENABLE_COMPOSITE_CHARS
  else if (EQ (charset, Vcharset_composite))
    return (0x1F << 14) | ((c1) << 7) | (c2);
#endif
  else if (XCHARSET_DIMENSION (charset) == 1)
    return ((XCHARSET_LEADING_BYTE (charset) -
	     FIELD2_TO_OFFICIAL_LEADING_BYTE) << 7) | (c1);
  else if (!XCHARSET_PRIVATE_P (charset))
    return ((XCHARSET_LEADING_BYTE (charset) -
	     FIELD1_TO_OFFICIAL_LEADING_BYTE) << 14) | ((c1) << 7) | (c2);
  else
    return ((XCHARSET_LEADING_BYTE (charset) -
	     FIELD1_TO_PRIVATE_LEADING_BYTE) << 14) | ((c1) << 7) | (c2);
}

#ifdef ERROR_CHECK_CHARBPOS
INLINE_HEADER Emchar MAKE_CHAR (Lisp_Object charset, int c1, int c2);
INLINE_HEADER Emchar
MAKE_CHAR (Lisp_Object charset, int c1, int c2)
{
  Emchar retval = MAKE_CHAR_1 (charset, c1, c2);
  assert (valid_char_p (retval));
  return retval;
}
#else
#define MAKE_CHAR(charset, c1, c2) MAKE_CHAR_1 (charset, c1, c2)
#endif

/* BREAKUP_CHAR_1_UNSAFE assumes that the charset has already been
   calculated, and just computes c1 and c2.

   BREAKUP_CHAR also computes and stores the charset. */

#define BREAKUP_CHAR_1_UNSAFE(c, charset, c1, c2)	\
  XCHARSET_DIMENSION (charset) == 1			\
  ? ((c1) = CHAR_FIELD3 (c), (c2) = 0)			\
  : ((c1) = CHAR_FIELD2 (c),				\
     (c2) = CHAR_FIELD3 (c))

INLINE_HEADER void breakup_char_1 (Emchar c, Lisp_Object *charset, int *c1,
				   int *c2);
INLINE_HEADER void
breakup_char_1 (Emchar c, Lisp_Object *charset, int *c1, int *c2)
{
  charbpos_checking_assert (valid_char_p (c));
  *charset = CHAR_CHARSET (c);
  BREAKUP_CHAR_1_UNSAFE (c, *charset, *c1, *c2);
}

/* BREAKUP_CHAR separates an Emchar into its components.  The charset of
   character C is set to CHARSET, and the position-codes of C are set to C1
   and C2.  C2 of TYPE9N character is 0.  */

#define BREAKUP_CHAR(c, charset, c1, c2) \
  breakup_char_1 (c, &(charset), &(c1), &(c2))

void get_charset_limits (Lisp_Object charset, int *low, int *high);
int char_to_unicode (Emchar chr);

#endif /* MULE */

#endif /* INCLUDED_charset_h_ */
