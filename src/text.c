/* Buffer manipulation primitives for XEmacs.
   Copyright (C) 1995 Sun Microsystems, Inc.
   Copyright (C) 1995, 1996, 2000, 2001, 2002, 2003 Ben Wing.
   Copyright (C) 1999 Martin Buchholz.

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

/* Authorship:
 */

#include <config.h>
#include "lisp.h"

#include "buffer.h"
#include "charset.h"
#include "file-coding.h"
#include "lstream.h"
#include "profile.h"


/************************************************************************/
/*                            long comments                             */
/************************************************************************/

/*
   ==========================================================================
                1. Intro to Characters, Character Sets, and Encodings
   ==========================================================================

   A character (which is, BTW, a surprisingly complex concept) is, in a
   written representation of text, the most basic written unit that has a
   meaning of its own.  It's comparable to a phoneme when analyzing words
   in spoken speech (for example, the sound of `t' in English, which in
   fact has different pronunciations in different words -- aspirated in
   `time', unaspirated in `stop', unreleased or even pronounced as a
   glottal stop in `button', etc. -- but logically is a single concept).
   Like a phoneme, a character is an abstract concept defined by its
   *meaning*.  The character `lowercase f', for example, can always be used
   to represent the first letter in the word `fill', regardless of whether
   it's drawn upright or italic, whether the `fi' combination is drawn as a
   single ligature, whether there are serifs on the bottom of the vertical
   stroke, etc. (These different appearances of a single character are
   often called "graphs" or "glyphs".) Our concern when representing text
   is on representing the abstract characters, and not on their exact
   appearance.

   A character set (or "charset"), as we define it, is a set of characters,
   each with an associated number (or set of numbers -- see below), called
   a "code point".  It's important to understand that a character is not
   defined by any number attached to it, but by its meaning.  For example,
   ASCII and EBCDIC are two charsets containing exactly the same characters
   (lowercase and uppercase letters, numbers 0 through 9, particular
   punctuation marks) but with different numberings. The `comma' character
   in ASCII and EBCDIC, for instance, is the same character despite having
   a different numbering.  Conversely, when comparing ASCII and JIS-Roman,
   which look the same except that the latter has a yen sign substituted
   for the backslash, we would say that the backslash and yen sign are
   *not* the same characters, despite having the same number (95) and
   despite the fact that all other characters are present in both charsets,
   with the same numbering.  ASCII and JIS-Roman, then, do *not* have
   exactly the same characters in them (ASCII has a backslash character but
   no yen-sign character, and vice-versa for JIS-Roman), unlike ASCII and
   EBCDIC, even though the numberings in ASCII and JIS-Roman are closer.

   It's also important to distinguish between charsets and encodings.  For
   a simple charset like ASCII, there is only one encoding normally used --
   each character is represented by a single byte, with the same value as
   its code point.  For more complicated charsets, however, things are not
   so obvious.  Unicode version 2, for example, is a large charset with
   thousands of characters, each indexed by a 16-bit number, often
   represented in hex, e.g. 0x05D0 for the Hebrew letter "aleph".  One
   obvious encoding uses two bytes per character (actually two encodings,
   depending on which of the two possible byte orderings is chosen).  This
   encoding is convenient for internal processing of Unicode text; however,
   it's incompatible with ASCII, so a different encoding, e.g. UTF-8, is
   usually used for external text, for example files or e-mail.  UTF-8
   represents Unicode characters with one to three bytes (often extended to
   six bytes to handle characters with up to 31-bit indices).  Unicode
   characters 00 to 7F (identical with ASCII) are directly represented with
   one byte, and other characters with two or more bytes, each in the range
   80 to FF.

   In general, a single encoding may be able to represent more than one
   charset.

   See also man/lispref/mule.texi.
   
   ==========================================================================
                               2. Character Sets
   ==========================================================================

   A particular character in a charset is indexed using one or
   more "position codes", which are non-negative integers.
   The number of position codes needed to identify a particular
   character in a charset is called the "dimension" of the
   charset.  In XEmacs/Mule, all charsets have 1 or 2 dimensions,
   and the size of all charsets (except for a few special cases)
   is either 94, 96, 94 by 94, or 96 by 96.  The range of
   position codes used to index characters from any of these
   types of character sets is as follows:

   Charset type		Position code 1		Position code 2
   ------------------------------------------------------------
   94			33 - 126		N/A
   96			32 - 127		N/A
   94x94		33 - 126		33 - 126
   96x96		32 - 127		32 - 127

   Note that in the above cases position codes do not start at
   an expected value such as 0 or 1.  The reason for this will
   become clear later.

   For example, Latin-1 is a 96-character charset, and JISX0208
   (the Japanese national character set) is a 94x94-character
   charset.

   [Note that, although the ranges above define the *valid*
   position codes for a charset, some of the slots in a particular
   charset may in fact be empty.  This is the case for JISX0208,
   for example, where (e.g.) all the slots whose first
   position code is in the range 118 - 127 are empty.]

   There are three charsets that do not follow the above rules.
   All of them have one dimension, and have ranges of position
   codes as follows:

   Charset name		Position code 1
   ------------------------------------
   ASCII		0 - 127
   Control-1		0 - 31
   Composite		0 - some large number

   (The upper bound of the position code for composite characters
   has not yet been determined, but it will probably be at
   least 16,383).

   ASCII is the union of two subsidiary character sets:
   Printing-ASCII (the printing ASCII character set,
   consisting of position codes 33 - 126, like for a standard
   94-character charset) and Control-ASCII (the non-printing
   characters that would appear in a binary file with codes 0
   - 32 and 127).

   Control-1 contains the non-printing characters that would
   appear in a binary file with codes 128 - 159.

   Composite contains characters that are generated by
   overstriking one or more characters from other charsets.

   Note that some characters in ASCII, and all characters
   in Control-1, are "control" (non-printing) characters.
   These have no printed representation but instead control
   some other function of the printing (e.g. TAB or 8 moves
   the current character position to the next tab stop).
   All other characters in all charsets are "graphic"
   (printing) characters.

   When a binary file is read in, the bytes in the file are
   assigned to character sets as follows:

   Bytes		Character set		Range
   --------------------------------------------------
   0 - 127		ASCII			0 - 127
   128 - 159		Control-1		0 - 31
   160 - 255		Latin-1			32 - 127

   This is a bit ad-hoc but gets the job done.

   ==========================================================================
                               3. Encodings
   ==========================================================================

   An "encoding" is a way of numerically representing
   characters from one or more character sets.  If an encoding
   only encompasses one character set, then the position codes
   for the characters in that character set could be used
   directly.  This is not possible, however, if more than one
   character set is to be used in the encoding.

   For example, the conversion detailed above between bytes in
   a binary file and characters is effectively an encoding
   that encompasses the three character sets ASCII, Control-1,
   and Latin-1 in a stream of 8-bit bytes.

   Thus, an encoding can be viewed as a way of encoding
   characters from a specified group of character sets using a
   stream of bytes, each of which contains a fixed number of
   bits (but not necessarily 8, as in the common usage of
   "byte").

   Here are descriptions of a couple of common
   encodings:


   A. Japanese EUC (Extended Unix Code)

   This encompasses the character sets:
   - Printing-ASCII,
   - Katakana-JISX0201 (half-width katakana, the right half of JISX0201).
   - Japanese-JISX0208
   - Japanese-JISX0212
   It uses 8-bit bytes.

   Note that Printing-ASCII and Katakana-JISX0201 are 94-character
   charsets, while Japanese-JISX0208 is a 94x94-character charset.

   The encoding is as follows:

   Character set	Representation  (PC == position-code)
   -------------	--------------
   Printing-ASCII	PC1
   Japanese-JISX0208	PC1 + 0x80 | PC2 + 0x80
   Katakana-JISX0201	0x8E       | PC1 + 0x80


   B. JIS7

   This encompasses the character sets:
   - Printing-ASCII
   - Latin-JISX0201 (the left half of JISX0201; this character set is
     very similar to Printing-ASCII and is a 94-character charset)
   - Japanese-JISX0208
   - Katakana-JISX0201
   It uses 7-bit bytes.

   Unlike Japanese EUC, this is a "modal" encoding, which
   means that there are multiple states that the encoding can
   be in, which affect how the bytes are to be interpreted.
   Special sequences of bytes (called "escape sequences")
   are used to change states.

   The encoding is as follows:

   Character set	Representation
   -------------	--------------
   Printing-ASCII	PC1
   Latin-JISX0201	PC1
   Katakana-JISX0201	PC1
   Japanese-JISX0208	PC1 | PC2

   Escape sequence	ASCII equivalent  Meaning
   ---------------	----------------  -------
   0x1B 0x28 0x42	ESC ( B		  invoke Printing-ASCII
   0x1B 0x28 0x4A	ESC ( J		  invoke Latin-JISX0201
   0x1B 0x28 0x49	ESC ( I		  invoke Katakana-JISX0201
   0x1B 0x24 0x42	ESC $ B		  invoke Japanese-JISX0208

   Initially, Printing-ASCII is invoked.

   ==========================================================================
                          4. Internal Mule Encodings
   ==========================================================================

   In XEmacs/Mule, each character set is assigned a unique number,
   called a "leading byte".  This is used in the encodings of a
   character.  Leading bytes are in the range 0x80 - 0xFF
   (except for ASCII, which has a leading byte of 0), although
   some leading bytes are reserved.

   Charsets whose leading byte is in the range 0x80 - 0x9F are
   called "official" and are used for built-in charsets.
   Other charsets are called "private" and have leading bytes
   in the range 0xA0 - 0xFF; these are user-defined charsets.

   More specifically:

   Character set		Leading byte
   -------------		------------
   ASCII			0 (0x7F in arrays indexed by leading byte)
   Composite			0x8D
   Dimension-1 Official		0x80 - 0x8C/0x8D
				  (0x8E is free)
   Control			0x8F
   Dimension-2 Official		0x90 - 0x99
				  (0x9A - 0x9D are free)
   Dimension-1 Private Marker   0x9E
   Dimension-2 Private Marker   0x9F
   Dimension-1 Private		0xA0 - 0xEF
   Dimension-2 Private		0xF0 - 0xFF

   There are two internal encodings for characters in XEmacs/Mule.
   One is called "string encoding" and is an 8-bit encoding that
   is used for representing characters in a buffer or string.
   It uses 1 to 4 bytes per character.  The other is called
   "character encoding" and is a 19-bit encoding that is used
   for representing characters individually in a variable.

   (In the following descriptions, we'll ignore composite
   characters for the moment.  We also give a general (structural)
   overview first, followed later by the exact details.)

   A. Internal String Encoding

   ASCII characters are encoded using their position code directly.
   Other characters are encoded using their leading byte followed
   by their position code(s) with the high bit set.  Characters
   in private character sets have their leading byte prefixed with
   a "leading byte prefix", which is either 0x9E or 0x9F. (No
   character sets are ever assigned these leading bytes.) Specifically:

   Character set		Encoding (PC == position-code)
   -------------		-------- (LB == leading-byte)
   ASCII			PC1  |
   Control-1			LB   | PC1 + 0xA0
   Dimension-1 official		LB   | PC1 + 0x80
   Dimension-1 private		0x9E | LB         | PC1 + 0x80
   Dimension-2 official		LB   | PC1        | PC2 + 0x80
   Dimension-2 private		0x9F | LB         | PC1 + 0x80 | PC2 + 0x80

   The basic characteristic of this encoding is that the first byte
   of all characters is in the range 0x00 - 0x9F, and the second and
   following bytes of all characters is in the range 0xA0 - 0xFF.
   This means that it is impossible to get out of sync, or more
   specifically:

   1. Given any byte position, the beginning of the character it is
      within can be determined in constant time.
   2. Given any byte position at the beginning of a character, the
      beginning of the next character can be determined in constant
      time.
   3. Given any byte position at the beginning of a character, the
      beginning of the previous character can be determined in constant
      time.
   4. Textual searches can simply treat encoded strings as if they
      were encoded in a one-byte-per-character fashion rather than
      the actual multi-byte encoding.

   None of the standard non-modal encodings meet all of these
   conditions.  For example, EUC satisfies only (2) and (3), while
   Shift-JIS and Big5 (not yet described) satisfy only (2). (All
   non-modal encodings must satisfy (2), in order to be unambiguous.)

   B. Internal Character Encoding

   One 19-bit word represents a single character.  The word is
   separated into three fields:

   Bit number:	18 17 16 15 14 13 12 11 10 09 08 07 06 05 04 03 02 01 00
		<------------> <------------------> <------------------>
   Field:	      1		         2		      3

   Note that fields 2 and 3 hold 7 bits each, while field 1 holds 5 bits.

   Character set		Field 1		Field 2		Field 3
   -------------		-------		-------		-------
   ASCII			   0		   0              PC1
      range:                                                   (00 - 7F)
   Control-1			   0		   1              PC1
      range:                                                   (00 - 1F)
   Dimension-1 official            0            LB - 0x7F         PC1
      range:                                    (01 - 0D)      (20 - 7F)
   Dimension-1 private             0            LB - 0x80         PC1
      range:                                    (20 - 6F)      (20 - 7F)
   Dimension-2 official		LB - 0x8F          PC1            PC2
      range:                    (01 - 0A)       (20 - 7F)      (20 - 7F)
   Dimension-2 private          LB - 0xE1          PC1            PC2
      range:                    (0F - 1E)       (20 - 7F)      (20 - 7F)
   Composite			  0x1F              ?              ?

   Note that character codes 0 - 255 are the same as the "binary encoding"
   described above.

   Most of the code in XEmacs knows nothing of the representation of a
   character other than that values 0 - 255 represent ASCII, Control 1,
   and Latin 1.

   WARNING WARNING WARNING: The Boyer-Moore code in search.c, and the
   code in search_buffer() that determines whether that code can be used,
   knows that "field 3" in a character always corresponds to the last
   byte in the textual representation of the character. (This is important
   because the Boyer-Moore algorithm works by looking at the last byte
   of the search string and &&#### finish this.

   ==========================================================================
                  5. Buffer Positions and Other Typedefs
   ==========================================================================

   A. Buffer Positions
   
   There are three possible ways to specify positions in a buffer.  All
   of these are one-based: the beginning of the buffer is position or
   index 1, and 0 is not a valid position.

   As a "buffer position" (typedef Charbpos):

      This is an index specifying an offset in characters from the
      beginning of the buffer.  Note that buffer positions are
      logically *between* characters, not on a character.  The
      difference between two buffer positions specifies the number of
      characters between those positions.  Buffer positions are the
      only kind of position externally visible to the user.

   As a "byte index" (typedef Bytebpos):

      This is an index over the bytes used to represent the characters
      in the buffer.  If there is no Mule support, this is identical
      to a buffer position, because each character is represented
      using one byte.  However, with Mule support, many characters
      require two or more bytes for their representation, and so a
      byte index may be greater than the corresponding buffer
      position.

   As a "memory index" (typedef Membpos):

      This is the byte index adjusted for the gap.  For positions
      before the gap, this is identical to the byte index.  For
      positions after the gap, this is the byte index plus the gap
      size.  There are two possible memory indices for the gap
      position; the memory index at the beginning of the gap should
      always be used, except in code that deals with manipulating the
      gap, where both indices may be seen.  The address of the
      character "at" (i.e. following) a particular position can be
      obtained from the formula

        buffer_start_address + memory_index(position) - 1

      except in the case of characters at the gap position.

   B. Other Typedefs

      Ichar:
      ------
        This typedef represents a single Emacs character, which can be
	ASCII, ISO-8859, or some extended character, as would typically
	be used for Kanji.  Note that the representation of a character
	as an Ichar is *not* the same as the representation of that
	same character in a string; thus, you cannot do the standard
	C trick of passing a pointer to a character to a function that
	expects a string.

	An Ichar takes up 19 bits of representation and (for code
	compatibility and such) is compatible with an int.  This
	representation is visible on the Lisp level.  The important
	characteristics	of the Ichar representation are

	  -- values 0x00 - 0x7f represent ASCII.
	  -- values 0x80 - 0xff represent the right half of ISO-8859-1.
	  -- values 0x100 and up represent all other characters.

	This means that Ichar values are upwardly compatible with
	the standard 8-bit representation of ASCII/ISO-8859-1.

      Ibyte:
      ------
        The data in a buffer or string is logically made up of Ibyte
	objects, where a Ibyte takes up the same amount of space as a
	char. (It is declared differently, though, to catch invalid
	usages.) Strings stored using Ibytes are said to be in
	"internal format".  The important characteristics of internal
	format are

	  -- ASCII characters are represented as a single Ibyte,
	     in the range 0 - 0x7f.
	  -- All other characters are represented as a Ibyte in
	     the range 0x80 - 0x9f followed by one or more Ibytes
	     in the range 0xa0 to 0xff.

	This leads to a number of desirable properties:

	  -- Given the position of the beginning of a character,
	     you can find the beginning of the next or previous
	     character in constant time.
	  -- When searching for a substring or an ASCII character
	     within the string, you need merely use standard
	     searching routines.

      Extbyte:
      --------
        Strings that go in or out of Emacs are in "external format",
	typedef'ed as an array of char or a char *.  There is more
	than one external format (JIS, EUC, etc.) but they all
	have similar properties.  They are modal encodings,
	which is to say that the meaning of particular bytes is
	not fixed but depends on what "mode" the string is currently
	in (e.g. bytes in the range 0 - 0x7f might be
	interpreted as ASCII, or as Hiragana, or as 2-byte Kanji,
	depending on the current mode).  The mode starts out in
	ASCII/ISO-8859-1 and is switched using escape sequences --
	for example, in the JIS encoding, 'ESC $ B' switches to a
	mode where pairs of bytes in the range 0 - 0x7f
	are interpreted as Kanji characters.

	External-formatted data is generally desirable for passing
	data between programs because it is upwardly compatible
	with standard ASCII/ISO-8859-1 strings and may require
	less space than internal encodings such as the one
	described above.  In addition, some encodings (e.g. JIS)
	keep all characters (except the ESC used to switch modes)
	in the printing ASCII range 0x20 - 0x7e, which results in
	a much higher probability that the data will avoid being
	garbled in transmission.  Externally-formatted data is
	generally not very convenient to work with, however, and
	for this reason is usually converted to internal format
	before any work is done on the string.

	NOTE: filenames need to be in external format so that
	ISO-8859-1 characters come out correctly.

      Charcount:
      ----------
        This typedef represents a count of characters, such as
	a character offset into a string or the number of
	characters between two positions in a buffer.  The
	difference between two Charbpos's is a Charcount, and
	character positions in a string are represented using
	a Charcount.

      Bytecount:
      ----------
        Similar to a Charcount but represents a count of bytes.
	The difference between two Bytebpos's is a Bytecount.


   C. Usage of the Various Representations

   Memory indices are used in low-level functions in insdel.c and for
   extent endpoints and marker positions.  The reason for this is that
   this way, the extents and markers don't need to be updated for most
   insertions, which merely shrink the gap and don't move any
   characters around in memory.

   (The beginning-of-gap memory index simplifies insertions w.r.t.
   markers, because text usually gets inserted after markers.  For
   extents, it is merely for consistency, because text can get
   inserted either before or after an extent's endpoint depending on
   the open/closedness of the endpoint.)

   Byte indices are used in other code that needs to be fast,
   such as the searching, redisplay, and extent-manipulation code.

   Buffer positions are used in all other code.  This is because this
   representation is easiest to work with (especially since Lisp
   code always uses buffer positions), necessitates the fewest
   changes to existing code, and is the safest (e.g. if the text gets
   shifted underneath a buffer position, it will still point to a
   character; if text is shifted under a byte index, it might point
   to the middle of a character, which would be bad).

   Similarly, Charcounts are used in all code that deals with strings
   except for code that needs to be fast, which used Bytecounts.

   Strings are always passed around internally using internal format.
   Conversions between external format are performed at the time
   that the data goes in or out of Emacs.

   D. Working With the Various Representations

   We write things this way because it's very important the
   MAX_BYTEBPOS_GAP_SIZE_3 is a multiple of 3. (As it happens,
   65535 is a multiple of 3, but this may not always be the
   case. #### unfinished

   ==========================================================================
                                6. Miscellaneous
   ==========================================================================

   A. Unicode Support

   Unicode support is very desirable.  Currrently we know how to handle
   externally-encoded Unicode data in various encodings -- UTF-16, UTF-8,
   etc.  However, we really need to represent Unicode characters internally
   as-is, rather than converting to some language-specific character set.
   For efficiency, we should represent Unicode characters using 3 bytes
   rather than 4.  This means we need to find leading bytes for Unicode.
   Given that there are 65,536 characters in Unicode and we can attach
   96x96 = 9,216 characters per leading byte, we need eight leading bytes
   for Unicode.  We currently have four free (0x9A - 0x9D), and with a
   little bit of rearranging we can get five: ASCII doesn't really need to
   take up a leading byte. (We could just as well use 0x7F, with a little
   change to the functions that assume that 0x80 is the lowest leading
   byte.) This means we still need to dump three leading bytes and move
   them into private space.  The CNS charsets are good candidates since
   they are rarely used, and JAPANESE_JISX0208_1978 is becoming less and
   less used and could also be dumped.

   B. Composite Characters
      
   Composite characters are characters constructed by overstriking two
   or more regular characters.

   1) The old Mule implementation involves storing composite characters
      in a buffer as a tag followed by all of the actual characters
      used to make up the composite character.  I think this is a bad
      idea; it greatly complicates code that wants to handle strings
      one character at a time because it has to deal with the possibility
      of great big ungainly characters.  It's much more reasonable to
      simply store an index into a table of composite characters.

   2) The current implementation only allows for 16,384 separate
      composite characters over the lifetime of the XEmacs process.
      This could become a potential problem if the user
      edited lots of different files that use composite characters.
      Due to FSF bogosity, increasing the number of allowable
      composite characters under Mule would decrease the number
      of possible faces that can exist.  Mule already has shrunk
      this to 2048, and further shrinkage would become uncomfortable.
      No such problems exist in XEmacs.

      Composite characters could be represented as 0x8D C1 C2 C3,
      where each C[1-3] is in the range 0xA0 - 0xFF.  This allows
      for slightly under 2^20 (one million) composite characters
      over the XEmacs process lifetime, and you only need to
      increase the size of a Mule character from 19 to 21 bits.
      Or you could use 0x8D C1 C2 C3 C4, allowing for about
      85 million (slightly over 2^26) composite characters.

*/


/************************************************************************/
/*                              declarations                            */
/************************************************************************/

Eistring the_eistring_zero_init, the_eistring_malloc_zero_init;

#define MAX_CHARBPOS_GAP_SIZE_3 (65535/3)
#define MAX_BYTEBPOS_GAP_SIZE_3 (3 * MAX_CHARBPOS_GAP_SIZE_3)

short three_to_one_table[1 + MAX_BYTEBPOS_GAP_SIZE_3];

#ifdef MULE

/* Table of number of bytes in the string representation of a character
   indexed by the first byte of that representation.

   rep_bytes_by_first_byte(c) is more efficient than the equivalent
   canonical computation:

   XCHARSET_REP_BYTES (charset_by_leading_byte (c)) */

const Bytecount rep_bytes_by_first_byte[0xA0] =
{ /* 0x00 - 0x7f are for straight ASCII */
  1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
  1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
  1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
  1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
  1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
  1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
  1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
  1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
  /* 0x80 - 0x8f are for Dimension-1 official charsets */
  2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2,
  /* 0x90 - 0x9d are for Dimension-2 official charsets */
  /* 0x9e is for Dimension-1 private charsets */
  /* 0x9f is for Dimension-2 private charsets */
  3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 4
};

#ifdef ENABLE_COMPOSITE_CHARS

/* Hash tables for composite chars.  One maps string representing
   composed chars to their equivalent chars; one goes the
   other way. */
Lisp_Object Vcomposite_char_char2string_hash_table;
Lisp_Object Vcomposite_char_string2char_hash_table;

static int composite_char_row_next;
static int composite_char_col_next;

#endif /* ENABLE_COMPOSITE_CHARS */

#endif /* MULE */

Lisp_Object QSin_char_byte_conversion;
Lisp_Object QSin_internal_external_conversion;


/************************************************************************/
/*                          qxestr***() functions                       */
/************************************************************************/

/* Most are inline functions in lisp.h */

int
qxesprintf (Ibyte *buffer, const CIbyte *format, ...)
{
  va_list args;
  int retval;

  va_start (args, format);
  retval = vsprintf ((char *) buffer, format, args);
  va_end (args);

  return retval;
}

/* strcasecmp() implementation from BSD */
static Ibyte strcasecmp_charmap[] = {
        0000, 0001, 0002, 0003, 0004, 0005, 0006, 0007,
        0010, 0011, 0012, 0013, 0014, 0015, 0016, 0017,
        0020, 0021, 0022, 0023, 0024, 0025, 0026, 0027,
        0030, 0031, 0032, 0033, 0034, 0035, 0036, 0037,
        0040, 0041, 0042, 0043, 0044, 0045, 0046, 0047,
        0050, 0051, 0052, 0053, 0054, 0055, 0056, 0057,
        0060, 0061, 0062, 0063, 0064, 0065, 0066, 0067,
        0070, 0071, 0072, 0073, 0074, 0075, 0076, 0077,
        0100, 0141, 0142, 0143, 0144, 0145, 0146, 0147,
        0150, 0151, 0152, 0153, 0154, 0155, 0156, 0157,
        0160, 0161, 0162, 0163, 0164, 0165, 0166, 0167,
        0170, 0171, 0172, 0133, 0134, 0135, 0136, 0137,
        0140, 0141, 0142, 0143, 0144, 0145, 0146, 0147,
        0150, 0151, 0152, 0153, 0154, 0155, 0156, 0157,
        0160, 0161, 0162, 0163, 0164, 0165, 0166, 0167,
        0170, 0171, 0172, 0173, 0174, 0175, 0176, 0177,
        0200, 0201, 0202, 0203, 0204, 0205, 0206, 0207,
        0210, 0211, 0212, 0213, 0214, 0215, 0216, 0217,
        0220, 0221, 0222, 0223, 0224, 0225, 0226, 0227,
        0230, 0231, 0232, 0233, 0234, 0235, 0236, 0237,
        0240, 0241, 0242, 0243, 0244, 0245, 0246, 0247,
        0250, 0251, 0252, 0253, 0254, 0255, 0256, 0257,
        0260, 0261, 0262, 0263, 0264, 0265, 0266, 0267,
        0270, 0271, 0272, 0273, 0274, 0275, 0276, 0277,
        0300, 0301, 0302, 0303, 0304, 0305, 0306, 0307,
        0310, 0311, 0312, 0313, 0314, 0315, 0316, 0317,
        0320, 0321, 0322, 0323, 0324, 0325, 0326, 0327,
        0330, 0331, 0332, 0333, 0334, 0335, 0336, 0337,
        0340, 0341, 0342, 0343, 0344, 0345, 0346, 0347,
        0350, 0351, 0352, 0353, 0354, 0355, 0356, 0357,
        0360, 0361, 0362, 0363, 0364, 0365, 0366, 0367,
        0370, 0371, 0372, 0373, 0374, 0375, 0376, 0377
};

/* A version that works like generic strcasecmp() -- only collapsing
   case in ASCII A-Z/a-z.  This is safe on Mule strings due to the
   current representation.

   This version was written by some Berkeley coder, favoring
   nanosecond improvements over clarity.  In all other versions below,
   we use symmetrical algorithms that may sacrifice a few machine
   cycles but are MUCH MUCH clearer, which counts a lot more.
*/

int
qxestrcasecmp (const Ibyte *s1, const Ibyte *s2)
{
  Ibyte *cm = strcasecmp_charmap;

  while (cm[*s1] == cm[*s2++])
    if (*s1++ == '\0')
      return (0);

  return (cm[*s1] - cm[*--s2]);
}

int
ascii_strcasecmp (const Char_ASCII *s1, const Char_ASCII *s2)
{
  return qxestrcasecmp ((const Ibyte *) s1, (const Ibyte *) s2);
}

int
qxestrcasecmp_c (const Ibyte *s1, const Char_ASCII *s2)
{
  return qxestrcasecmp (s1, (const Ibyte *) s2);
}

/* An internationalized version that collapses case in a general fashion.
 */

int
qxestrcasecmp_i18n (const Ibyte *s1, const Ibyte *s2)
{
  while (*s1 && *s2)
    {
      if (DOWNCASE (0, itext_ichar (s1)) !=
	  DOWNCASE (0, itext_ichar (s2)))
	break;
      INC_IBYTEPTR (s1);
      INC_IBYTEPTR (s2);
    }

  return (DOWNCASE (0, itext_ichar (s1)) -
	  DOWNCASE (0, itext_ichar (s2)));
}

/* The only difference between these next two and
   qxememcasecmp()/qxememcasecmp_i18n() is that these two will stop if
   both strings are equal and less than LEN in length, while
   the mem...() versions would would run off the end. */

int
qxestrncasecmp (const Ibyte *s1, const Ibyte *s2, Bytecount len)
{
  Ibyte *cm = strcasecmp_charmap;

  while (len--)
    {
      int diff = cm[*s1] - cm[*s2];
      if (diff != 0)
	return diff;
      if (!*s1)
	return 0;
      s1++, s2++;
    }

  return 0;
}

int
ascii_strncasecmp (const Char_ASCII *s1, const Char_ASCII *s2, Bytecount len)
{
  return qxestrncasecmp ((const Ibyte *) s1, (const Ibyte *) s2, len);
}

int
qxestrncasecmp_c (const Ibyte *s1, const Char_ASCII *s2, Bytecount len)
{
  return qxestrncasecmp (s1, (const Ibyte *) s2, len);
}

/* Compare LEN_FROM_S1 worth of characters from S1 with the same number of
   characters from S2, case insensitive.  NOTE: Downcasing can convert
   characters from one length in bytes to another, so reversing S1 and S2
   is *NOT* a symmetric operations!  You must choose a length that agrees
   with S1. */

int
qxestrncasecmp_i18n (const Ibyte *s1, const Ibyte *s2,
		     Bytecount len_from_s1)
{
  while (len_from_s1 > 0)
    {
      const Ibyte *old_s1 = s1;
      int diff = (DOWNCASE (0, itext_ichar (s1)) -
		  DOWNCASE (0, itext_ichar (s2)));
      if (diff != 0)
	return diff;
      if (!*s1)
	return 0;
      INC_IBYTEPTR (s1);
      INC_IBYTEPTR (s2);
      len_from_s1 -= s1 - old_s1;
    }

  return 0;
}

int
qxememcmp (const Ibyte *s1, const Ibyte *s2, Bytecount len)
{
  return memcmp (s1, s2, len);
}

int
qxememcmp4 (const Ibyte *s1, Bytecount len1,
	    const Ibyte *s2, Bytecount len2)
{
  int retval = qxememcmp (s1, s2, min (len1, len2));
  if (retval)
    return retval;
  return len1 - len2;
}

int
qxememcasecmp (const Ibyte *s1, const Ibyte *s2, Bytecount len)
{
  Ibyte *cm = strcasecmp_charmap;

  while (len--)
    {
      int diff = cm[*s1] - cm[*s2];
      if (diff != 0)
	return diff;
      s1++, s2++;
    }

  return 0;
}

int
qxememcasecmp4 (const Ibyte *s1, Bytecount len1,
		const Ibyte *s2, Bytecount len2)
{
  int retval = qxememcasecmp (s1, s2, min (len1, len2));
  if (retval)
    return retval;
  return len1 - len2;
}

/* Do a character-by-character comparison, returning "which is greater" by
   comparing the Ichar values. (#### Should have option to compare Unicode
   points) */

int
qxetextcmp (const Ibyte *s1, Bytecount len1,
	    const Ibyte *s2, Bytecount len2)
{
  while (len1 > 0 && len2 > 0)
    {
      const Ibyte *old_s1 = s1;
      const Ibyte *old_s2 = s2;
      int diff = itext_ichar (s1) - itext_ichar (s2);
      if (diff != 0)
	return diff;
      INC_IBYTEPTR (s1);
      INC_IBYTEPTR (s2);
      len1 -= s1 - old_s1;
      len2 -= s2 - old_s2;
    }

  assert (len1 >= 0 && len2 >= 0);
  return len1 - len2;
}

int
qxetextcmp_matching (const Ibyte *s1, Bytecount len1,
		     const Ibyte *s2, Bytecount len2,
		     Charcount *matching)
{
  *matching = 0;
  while (len1 > 0 && len2 > 0)
    {
      const Ibyte *old_s1 = s1;
      const Ibyte *old_s2 = s2;
      int diff = itext_ichar (s1) - itext_ichar (s2);
      if (diff != 0)
	return diff;
      INC_IBYTEPTR (s1);
      INC_IBYTEPTR (s2);
      len1 -= s1 - old_s1;
      len2 -= s2 - old_s2;
      (*matching)++;
    }

  assert (len1 >= 0 && len2 >= 0);
  return len1 - len2;
}

/* Do a character-by-character comparison, returning "which is greater" by
   comparing the Ichar values, case insensitively (by downcasing both
   first). (#### Should have option to compare Unicode points)

   In this case, both lengths must be specified becaused downcasing can
   convert characters from one length in bytes to another; therefore, two
   blocks of text of different length might be equal.  If both compare
   equal up to the limit in length of one but not the other, the longer one
   is "greater". */

int
qxetextcasecmp (const Ibyte *s1, Bytecount len1,
		const Ibyte *s2, Bytecount len2)
{
  while (len1 > 0 && len2 > 0)
    {
      const Ibyte *old_s1 = s1;
      const Ibyte *old_s2 = s2;
      int diff = (DOWNCASE (0, itext_ichar (s1)) -
		  DOWNCASE (0, itext_ichar (s2)));
      if (diff != 0)
	return diff;
      INC_IBYTEPTR (s1);
      INC_IBYTEPTR (s2);
      len1 -= s1 - old_s1;
      len2 -= s2 - old_s2;
    }

  assert (len1 >= 0 && len2 >= 0);
  return len1 - len2;
}

/* Like qxetextcasecmp() but also return number of characters at
   beginning that match. */

int
qxetextcasecmp_matching (const Ibyte *s1, Bytecount len1,
			 const Ibyte *s2, Bytecount len2,
			 Charcount *matching)
{
  *matching = 0;
  while (len1 > 0 && len2 > 0)
    {
      const Ibyte *old_s1 = s1;
      const Ibyte *old_s2 = s2;
      int diff = (DOWNCASE (0, itext_ichar (s1)) -
		  DOWNCASE (0, itext_ichar (s2)));
      if (diff != 0)
	return diff;
      INC_IBYTEPTR (s1);
      INC_IBYTEPTR (s2);
      len1 -= s1 - old_s1;
      len2 -= s2 - old_s2;
      (*matching)++;
    }

  assert (len1 >= 0 && len2 >= 0);
  return len1 - len2;
}

int
lisp_strcasecmp (Lisp_Object s1, Lisp_Object s2)
{
  Ibyte *cm = strcasecmp_charmap;
  Ibyte *p1 = XSTRING_DATA (s1);
  Ibyte *p2 = XSTRING_DATA (s2);
  Ibyte *e1 = p1 + XSTRING_LENGTH (s1);
  Ibyte *e2 = p2 + XSTRING_LENGTH (s2);

  /* again, we use a symmetric algorithm and favor clarity over
     nanosecond improvements. */
  while (1)
    {
      /* if we reached the end of either string, compare lengths.
	 do NOT compare the final null byte against anything, in case
	 the other string also has a null byte at that position. */
      if (p1 == e1 || p2 == e2)
	return e1 - e2;
      if (cm[*p1] != cm[*p2])
	return cm[*p1] - cm[*p2];
      p1++, p2++;
    }
}

int
lisp_strcasecmp_i18n (Lisp_Object s1, Lisp_Object s2)
{
  return qxetextcasecmp (XSTRING_DATA (s1), XSTRING_LENGTH (s1),
			 XSTRING_DATA (s2), XSTRING_LENGTH (s2));
}


/************************************************************************/
/*               conversion between textual representations             */
/************************************************************************/

/* NOTE: Does not reset the Dynarr. */

void
convert_ibyte_string_into_ichar_dynarr (const Ibyte *str, Bytecount len,
					   Ichar_dynarr *dyn)
{
  const Ibyte *strend = str + len;

  while (str < strend)
    {
      Ichar ch = itext_ichar (str);
      Dynarr_add (dyn, ch);
      INC_IBYTEPTR (str);
    }
}

Charcount
convert_ibyte_string_into_ichar_string (const Ibyte *str, Bytecount len,
					   Ichar *arr)
{
  const Ibyte *strend = str + len;
  Charcount newlen = 0;
  while (str < strend)
    {
      Ichar ch = itext_ichar (str);
      arr[newlen++] = ch;
      INC_IBYTEPTR (str);
    }
  return newlen;
}

/* Convert an array of Ichars into the equivalent string representation.
   Store into the given Ibyte dynarr.  Does not reset the dynarr.
   Does not add a terminating zero. */

void
convert_ichar_string_into_ibyte_dynarr (Ichar *arr, int nels,
					  Ibyte_dynarr *dyn)
{
  Ibyte str[MAX_ICHAR_LEN];
  int i;

  for (i = 0; i < nels; i++)
    {
      Bytecount len = set_itext_ichar (str, arr[i]);
      Dynarr_add_many (dyn, str, len);
    }
}

/* Convert an array of Ichars into the equivalent string representation.
   Malloc the space needed for this and return it.  If LEN_OUT is not a
   NULL pointer, store into LEN_OUT the number of Ibytes in the
   malloc()ed string.  Note that the actual number of Ibytes allocated
   is one more than this: the returned string is zero-terminated. */

Ibyte *
convert_ichar_string_into_malloced_string (Ichar *arr, int nels,
					    Bytecount *len_out)
{
  /* Damn zero-termination. */
  Ibyte *str = (Ibyte *) ALLOCA (nels * MAX_ICHAR_LEN + 1);
  Ibyte *strorig = str;
  Bytecount len;

  int i;

  for (i = 0; i < nels; i++)
    str += set_itext_ichar (str, arr[i]);
  *str = '\0';
  len = str - strorig;
  str = (Ibyte *) xmalloc (1 + len);
  memcpy (str, strorig, 1 + len);
  if (len_out)
    *len_out = len;
  return str;
}

#define COPY_TEXT_BETWEEN_FORMATS(srcfmt, dstfmt)			 \
do									 \
{									 \
  if (dst)								 \
    {									 \
      Ibyte *dstend = dst + dstlen;					 \
      Ibyte *dstp = dst;						 \
      const Ibyte *srcend = src + srclen;				 \
      const Ibyte *srcp = src;					 \
									 \
      while (srcp < srcend)						 \
	{								 \
	  Ichar ch = itext_ichar_fmt (srcp, srcfmt, srcobj);	 \
	  Bytecount len = ichar_len_fmt (ch, dstfmt);			 \
									 \
	    if (dstp + len <= dstend)					 \
	      {								 \
		set_itext_ichar_fmt (dstp, ch, dstfmt, dstobj);	 \
		dstp += len;						 \
	      }								 \
	    else							 \
	      break;							 \
	  INC_IBYTEPTR_FMT (srcp, srcfmt);				 \
	}								 \
      text_checking_assert (srcp <= srcend);				 \
      if (src_used)							 \
	*src_used = srcp - src;						 \
      return dstp - dst;						 \
    }									 \
  else									 \
    {									 \
      const Ibyte *srcend = src + srclen;				 \
      const Ibyte *srcp = src;					 \
      Bytecount total = 0;						 \
									 \
      while (srcp < srcend)						 \
	{								 \
	  total += ichar_len_fmt (itext_ichar_fmt (srcp, srcfmt,	 \
						       srcobj), dstfmt); \
	  INC_IBYTEPTR_FMT (srcp, srcfmt);				 \
	}								 \
      text_checking_assert (srcp == srcend);				 \
      if (src_used)							 \
	*src_used = srcp - src;						 \
      return total;							 \
    }									 \
}									 \
while (0)

/* Copy as much text from SRC/SRCLEN to DST/DSTLEN as will fit, converting
   from SRCFMT/SRCOBJ to DSTFMT/DSTOBJ.  Return number of bytes stored into
   DST as return value, and number of bytes copied from SRC through
   SRC_USED (if not NULL).  If DST is NULL, don't actually store anything
   and just return the size needed to store all the text.  Will not copy
   partial characters into DST. */

Bytecount
copy_text_between_formats (const Ibyte *src, Bytecount srclen,
			   Internal_Format srcfmt,
			   Lisp_Object srcobj,
			   Ibyte *dst, Bytecount dstlen,
			   Internal_Format dstfmt,
			   Lisp_Object dstobj,
			   Bytecount *src_used)
{
  if (srcfmt == dstfmt &&
      objects_have_same_internal_representation (srcobj, dstobj))
    {
      if (dst)
	{
	  srclen = min (srclen, dstlen);
	  srclen = validate_ibyte_string_backward (src, srclen);
	  memcpy (dst, src, srclen);
	  if (src_used)
	    *src_used = srclen;
	  return srclen;
	}
      else
	return srclen;
    }
  /* Everything before the final else statement is an optimization.
     The inner loops inside COPY_TEXT_BETWEEN_FORMATS() have a number
     of calls to *_fmt(), each of which has a switch statement in it.
     By using constants as the FMT argument, these switch statements
     will be optimized out of existence. */
#define ELSE_FORMATS(fmt1, fmt2)		\
  else if (srcfmt == fmt1 && dstfmt == fmt2)	\
    COPY_TEXT_BETWEEN_FORMATS (fmt1, fmt2)
  ELSE_FORMATS (FORMAT_DEFAULT, FORMAT_8_BIT_FIXED);
  ELSE_FORMATS (FORMAT_8_BIT_FIXED, FORMAT_DEFAULT);
  ELSE_FORMATS (FORMAT_DEFAULT, FORMAT_32_BIT_FIXED);
  ELSE_FORMATS (FORMAT_32_BIT_FIXED, FORMAT_DEFAULT);
  else
    COPY_TEXT_BETWEEN_FORMATS (srcfmt, dstfmt);
#undef ELSE_FORMATS
}

/* Copy as much buffer text in BUF, starting at POS, of length LEN, as will
   fit into DST/DSTLEN, converting to DSTFMT.  Return number of bytes
   stored into DST as return value, and number of bytes copied from BUF
   through SRC_USED (if not NULL).  If DST is NULL, don't actually store
   anything and just return the size needed to store all the text. */

Bytecount
copy_buffer_text_out (struct buffer *buf, Bytebpos pos,
		      Bytecount len, Ibyte *dst, Bytecount dstlen,
		      Internal_Format dstfmt, Lisp_Object dstobj,
		      Bytecount *src_used)
{
  Bytecount dst_used = 0;
  if (src_used)
    *src_used = 0;

  {
    BUFFER_TEXT_LOOP (buf, pos, len, runptr, runlen)
      {
	Bytecount the_src_used, the_dst_used;
	
	the_dst_used = copy_text_between_formats (runptr, runlen,
						  BUF_FORMAT (buf),
						  wrap_buffer (buf),
						  dst, dstlen, dstfmt,
						  dstobj, &the_src_used);
	dst_used += the_dst_used;
	if (src_used)
	  *src_used += the_src_used;
	if (dst)
	  {
	    dst += the_dst_used;
	    dstlen -= the_dst_used;
	    /* Stop if we didn't use all of the source text.  Also stop
	       if the destination is full.  We need the first test because
	       there might be a couple bytes left in the destination, but
	       not enough to fit a full character.  The first test will in
	       fact catch the vast majority of cases where the destination
	       is empty, too -- but in case the destination holds *exactly*
	       the run length, we put in the second check. (It shouldn't
	       really matter though -- next time through we'll just get a
	       0.) */
	    if (the_src_used < runlen || !dstlen)
	      break;
	  }
      }
  }

  return dst_used;
}


/************************************************************************/
/*                    charset properties of strings                     */
/************************************************************************/

void
find_charsets_in_ibyte_string (unsigned char *charsets, const Ibyte *str,
				 Bytecount len)
{
#ifndef MULE
  /* Telescope this. */
  charsets[0] = 1;
#else
  const Ibyte *strend = str + len;
  memset (charsets, 0, NUM_LEADING_BYTES);

  /* #### SJT doesn't like this. */
  if (len == 0)
    {
      charsets[XCHARSET_LEADING_BYTE (Vcharset_ascii) - MIN_LEADING_BYTE] = 1;
      return;
    }

  while (str < strend)
    {
      charsets[ichar_leading_byte (itext_ichar (str)) - MIN_LEADING_BYTE] =
	1;
      INC_IBYTEPTR (str);
    }
#endif
}

void
find_charsets_in_ichar_string (unsigned char *charsets, const Ichar *str,
				Charcount len)
{
#ifndef MULE
  /* Telescope this. */
  charsets[0] = 1;
#else
  int i;

  memset (charsets, 0, NUM_LEADING_BYTES);

  /* #### SJT doesn't like this. */
  if (len == 0)
    {
      charsets[XCHARSET_LEADING_BYTE (Vcharset_ascii) - MIN_LEADING_BYTE] = 1;
      return;
    }

  for (i = 0; i < len; i++)
    {
      charsets[ichar_leading_byte (str[i]) - MIN_LEADING_BYTE] = 1;
    }
#endif
}

int
ibyte_string_displayed_columns (const Ibyte *str, Bytecount len)
{
  int cols = 0;
  const Ibyte *end = str + len;

  while (str < end)
    {
#ifdef MULE
      Ichar ch = itext_ichar (str);
      cols += XCHARSET_COLUMNS (ichar_charset (ch));
#else
      cols++;
#endif
      INC_IBYTEPTR (str);
    }

  return cols;
}

int
ichar_string_displayed_columns (const Ichar *str, Charcount len)
{
#ifdef MULE
  int cols = 0;
  int i;

  for (i = 0; i < len; i++)
    cols += XCHARSET_COLUMNS (ichar_charset (str[i]));

  return cols;
#else  /* not MULE */
  return len;
#endif
}

Charcount
ibyte_string_nonascii_chars (const Ibyte *str, Bytecount len)
{
#ifdef MULE
  const Ibyte *end = str + len;
  Charcount retval = 0;

  while (str < end)
    {
      if (!byte_ascii_p (*str))
	retval++;
      INC_IBYTEPTR (str);
    }

  return retval;
#else
  return 0;
#endif
}


/***************************************************************************/
/*                     Eistring helper functions                           */
/***************************************************************************/

int
eistr_casefiddle_1 (Ibyte *olddata, Bytecount len, Ibyte *newdata,
		    int downp)
{
  Ibyte *endp = olddata + len;
  Ibyte *newp = newdata;
  int changedp = 0;

  while (olddata < endp)
    {
      Ichar c = itext_ichar (olddata);
      Ichar newc;

      if (downp)
	newc = DOWNCASE (0, c);
      else
	newc = UPCASE (0, c);

      if (c != newc)
	changedp = 1;

      newp += set_itext_ichar (newp, newc);
      INC_IBYTEPTR (olddata);
    }

  *newp = '\0';

  return changedp ? newp - newdata : 0;
}

int
eifind_large_enough_buffer (int oldbufsize, int needed_size)
{
  while (oldbufsize < needed_size)
    {
      oldbufsize = oldbufsize * 3 / 2;
      oldbufsize = max (oldbufsize, 32);
    }

  return oldbufsize;
}

void
eito_malloc_1 (Eistring *ei)
{
  if (ei->mallocp_)
    return;
  ei->mallocp_ = 1;
  if (ei->data_)
    {
      Ibyte *newdata;

      ei->max_size_allocated_ =
	eifind_large_enough_buffer (0, ei->bytelen_ + 1);
      newdata = (Ibyte *) xmalloc (ei->max_size_allocated_);
      memcpy (newdata, ei->data_, ei->bytelen_ + 1);
      ei->data_ = newdata;
    }

  if (ei->extdata_)
    {
      Extbyte *newdata = (Extbyte *) xmalloc (ei->extlen_ + 2);

      memcpy (newdata, ei->extdata_, ei->extlen_);
      /* Double null-terminate in case of Unicode data */
      newdata[ei->extlen_] = '\0';
      newdata[ei->extlen_ + 1] = '\0';
      ei->extdata_ = newdata;
    }
}  

int
eicmp_1 (Eistring *ei, Bytecount off, Charcount charoff,
	 Bytecount len, Charcount charlen, const Ibyte *data,
	 const Eistring *ei2, int is_c, int fold_case)
{
  assert ((off < 0) != (charoff < 0));
  if (off < 0)
    {
      off = charcount_to_bytecount (ei->data_, charoff);
      if (charlen < 0)
	len = -1;
      else
	len = charcount_to_bytecount (ei->data_ + off, charlen);
    }
  if (len < 0)
    len = ei->bytelen_ - off;

  assert (off >= 0 && off <= ei->bytelen_);
  assert (len >= 0 && off + len <= ei->bytelen_);
  assert ((data == 0) != (ei == 0)); 
  assert ((is_c != 0) == (data != 0));
  assert (fold_case >= 0 && fold_case <= 2);

  {
    Bytecount dstlen;
    const Ibyte *src = ei->data_, *dst;

    if (data)
      {
	dst = data;
	dstlen = qxestrlen (data);
      }
    else
      {
	dst = ei2->data_;
	dstlen = ei2->bytelen_;
      }

    if (is_c)
      EI_ASSERT_ASCII ((Char_ASCII *) dst, dstlen);

    return (fold_case == 0 ? qxememcmp4 (src, len, dst, dstlen) :
	    fold_case == 1 ? qxememcasecmp4 (src, len, dst, dstlen) :
	    qxetextcasecmp (src, len, dst, dstlen));
  }
}

Ibyte *
eicpyout_malloc_fmt (Eistring *eistr, Bytecount *len_out, Internal_Format fmt,
		     Lisp_Object object)
{
  Ibyte *ptr;

  assert (fmt == FORMAT_DEFAULT);
  ptr = xnew_array (Ibyte, eistr->bytelen_ + 1);
  if (len_out)
    *len_out = eistr->bytelen_;
  memcpy (ptr, eistr->data_, eistr->bytelen_ + 1);
  return ptr;
}


/************************************************************************/
/*                    Charcount/Bytecount conversion                    */
/************************************************************************/

/* Optimization.  Do it.  Live it.  Love it.  */

#ifdef MULE

/* Skip as many ASCII bytes as possible in the memory block [PTR, END).
   Return pointer to the first non-ASCII byte.  optimized for long
   stretches of ASCII. */
inline static const Ibyte *
skip_ascii (const Ibyte *ptr, const Ibyte *end)
{
#ifdef EFFICIENT_INT_128_BIT
# define STRIDE_TYPE INT_128_BIT
# define HIGH_BIT_MASK \
    MAKE_128_BIT_UNSIGNED_CONSTANT (0x80808080808080808080808080808080)
#elif defined (EFFICIENT_INT_64_BIT)
# define STRIDE_TYPE INT_64_BIT
# define HIGH_BIT_MASK MAKE_64_BIT_UNSIGNED_CONSTANT (0x8080808080808080)
#else
# define STRIDE_TYPE INT_32_BIT
# define HIGH_BIT_MASK MAKE_32_BIT_UNSIGNED_CONSTANT (0x80808080)
#endif

#define ALIGN_BITS ((EMACS_UINT) (ALIGNOF (STRIDE_TYPE) - 1))
#define ALIGN_MASK (~ ALIGN_BITS)
#define ALIGNED(ptr) ((((EMACS_UINT) ptr) & ALIGN_BITS) == 0)
#define STRIDE sizeof (STRIDE_TYPE)

  const unsigned STRIDE_TYPE *ascii_end;

  /* Need to do in 3 sections -- before alignment start, aligned chunk,
     after alignment end. */
  while (!ALIGNED (ptr))
    {
      if (ptr == end || !byte_ascii_p (*ptr))
	return ptr;
      ptr++;
    }
  ascii_end = (const unsigned STRIDE_TYPE *) ptr;
  /* This loop screams, because we can detect ASCII
     characters 4 or 8 at a time. */
  while ((const Ibyte *) ascii_end + STRIDE <= end
	 && !(*ascii_end & HIGH_BIT_MASK))
    ascii_end++;
  ptr = (Ibyte *) ascii_end;
  while (ptr < end && byte_ascii_p (*ptr))
    ptr++;
  return ptr;
}

/* Function equivalents of bytecount_to_charcount/charcount_to_bytecount.
   These work on strings of all sizes but are more efficient than a simple
   loop on large strings and probably less efficient on sufficiently small
   strings. */

Charcount
bytecount_to_charcount_fun (const Ibyte *ptr, Bytecount len)
{
  Charcount count = 0;
  const Ibyte *end = ptr + len;
  while (1)
    {
      const Ibyte *newptr = skip_ascii (ptr, end);
      count += newptr - ptr;
      ptr = newptr;
      if (ptr == end)
	break;
      {
	/* Optimize for successive characters from the same charset */
	Ibyte leading_byte = *ptr;
	int bytes = rep_bytes_by_first_byte (leading_byte);
	while (ptr < end && *ptr == leading_byte)
	  ptr += bytes, count++;
      }
    }

  /* Bomb out if the specified substring ends in the middle
     of a character.  Note that we might have already gotten
     a core dump above from an invalid reference, but at least
     we will get no farther than here.

     This also catches len < 0. */
  text_checking_assert (ptr == end);

  return count;
}

Bytecount
charcount_to_bytecount_fun (const Ibyte *ptr, Charcount len)
{
  const Ibyte *newptr = ptr;
  while (1)
    {
      const Ibyte *newnewptr = skip_ascii (newptr, newptr + len);
      len -= newnewptr - newptr;
      newptr = newnewptr;
      if (!len)
	break;
      {
	/* Optimize for successive characters from the same charset */
	Ibyte leading_byte = *newptr;
	int bytes = rep_bytes_by_first_byte (leading_byte);
	while (len > 0 && *newptr == leading_byte)
	  newptr += bytes, len--;
      }
    }
  return newptr - ptr;
}

/* The next two functions are the actual meat behind the
   charbpos-to-bytebpos and bytebpos-to-charbpos conversions.  Currently
   the method they use is fairly unsophisticated; see buffer.h.

   Note that charbpos_to_bytebpos_func() is probably the most-called
   function in all of XEmacs.  Therefore, it must be FAST FAST FAST.
   This is the reason why so much of the code is duplicated.

   Similar considerations apply to bytebpos_to_charbpos_func(), although
   less so because the function is not called so often.

   #### At some point this should use a more sophisticated method;
   see buffer.h. */

static int not_very_random_number;

Bytebpos
charbpos_to_bytebpos_func (struct buffer *buf, Charbpos x)
{
  Charbpos bufmin;
  Charbpos bufmax;
  Bytebpos bytmin;
  Bytebpos bytmax;
  int size;
  int forward_p;
  Bytebpos retval;
  int diff_so_far;
  int add_to_cache = 0;
  PROFILE_DECLARE ();

  /* Check for some cached positions, for speed. */
  if (x == BUF_PT (buf))
    return BYTE_BUF_PT (buf);
  if (x == BUF_ZV (buf))
    return BYTE_BUF_ZV (buf);
  if (x == BUF_BEGV (buf))
    return BYTE_BUF_BEGV (buf);

  PROFILE_RECORD_ENTERING_SECTION (QSin_char_byte_conversion);

  bufmin = buf->text->mule_bufmin;
  bufmax = buf->text->mule_bufmax;
  bytmin = buf->text->mule_bytmin;
  bytmax = buf->text->mule_bytmax;
  size = (1 << buf->text->mule_shifter) + !!buf->text->mule_three_p;

  /* The basic idea here is that we shift the "known region" up or down
     until it overlaps the specified position.  We do this by moving
     the upper bound of the known region up one character at a time,
     and moving the lower bound of the known region up as necessary
     when the size of the character just seen changes.

     We optimize this, however, by first shifting the known region to
     one of the cached points if it's close by. (We don't check BEG or
     Z, even though they're cached; most of the time these will be the
     same as BEGV and ZV, and when they're not, they're not likely
     to be used.) */

  if (x > bufmax)
    {
      Charbpos diffmax = x - bufmax;
      Charbpos diffpt = x - BUF_PT (buf);
      Charbpos diffzv = BUF_ZV (buf) - x;
      /* #### This value could stand some more exploration. */
      Charcount heuristic_hack = (bufmax - bufmin) >> 2;

      /* Check if the position is closer to PT or ZV than to the
	 end of the known region. */

      if (diffpt < 0)
	diffpt = -diffpt;
      if (diffzv < 0)
	diffzv = -diffzv;

      /* But also implement a heuristic that favors the known region
	 over PT or ZV.  The reason for this is that switching to
	 PT or ZV will wipe out the knowledge in the known region,
	 which might be annoying if the known region is large and
	 PT or ZV is not that much closer than the end of the known
	 region. */

      diffzv += heuristic_hack;
      diffpt += heuristic_hack;
      if (diffpt < diffmax && diffpt <= diffzv)
	{
	  bufmax = bufmin = BUF_PT (buf);
	  bytmax = bytmin = BYTE_BUF_PT (buf);
	  /* We set the size to 1 even though it doesn't really
	     matter because the new known region contains no
	     characters.  We do this because this is the most
	     likely size of the characters around the new known
	     region, and we avoid potential yuckiness that is
	     done when size == 3. */
	  size = 1;
	}
      if (diffzv < diffmax)
	{
	  bufmax = bufmin = BUF_ZV (buf);
	  bytmax = bytmin = BYTE_BUF_ZV (buf);
	  size = 1;
	}
    }
#ifdef ERROR_CHECK_TEXT
  else if (x >= bufmin)
    abort ();
#endif
  else
    {
      Charbpos diffmin = bufmin - x;
      Charbpos diffpt = BUF_PT (buf) - x;
      Charbpos diffbegv = x - BUF_BEGV (buf);
      /* #### This value could stand some more exploration. */
      Charcount heuristic_hack = (bufmax - bufmin) >> 2;

      if (diffpt < 0)
	diffpt = -diffpt;
      if (diffbegv < 0)
	diffbegv = -diffbegv;

      /* But also implement a heuristic that favors the known region --
	 see above. */

      diffbegv += heuristic_hack;
      diffpt += heuristic_hack;

      if (diffpt < diffmin && diffpt <= diffbegv)
	{
	  bufmax = bufmin = BUF_PT (buf);
	  bytmax = bytmin = BYTE_BUF_PT (buf);
	  /* We set the size to 1 even though it doesn't really
	     matter because the new known region contains no
	     characters.  We do this because this is the most
	     likely size of the characters around the new known
	     region, and we avoid potential yuckiness that is
	     done when size == 3. */
	  size = 1;
	}
      if (diffbegv < diffmin)
	{
	  bufmax = bufmin = BUF_BEGV (buf);
	  bytmax = bytmin = BYTE_BUF_BEGV (buf);
	  size = 1;
	}
    }

  diff_so_far = x > bufmax ? x - bufmax : bufmin - x;
  if (diff_so_far > 50)
    {
      /* If we have to move more than a certain amount, then look
	 into our cache. */
      int minval = INT_MAX;
      int found = 0;
      int i;

      add_to_cache = 1;
      /* I considered keeping the positions ordered.  This would speed
	 up this loop, but updating the cache would take longer, so
	 it doesn't seem like it would really matter. */
      for (i = 0; i < 16; i++)
	{
	  int diff = buf->text->mule_charbpos_cache[i] - x;

	  if (diff < 0)
	    diff = -diff;
	  if (diff < minval)
	    {
	      minval = diff;
	      found = i;
	    }
	}

      if (minval < diff_so_far)
	{
	  bufmax = bufmin = buf->text->mule_charbpos_cache[found];
	  bytmax = bytmin = buf->text->mule_bytebpos_cache[found];
	  size = 1;
	}
    }

  /* It's conceivable that the caching above could lead to X being
     the same as one of the range edges. */
  if (x >= bufmax)
    {
      Bytebpos newmax;
      Bytecount newsize;

      forward_p = 1;
      while (x > bufmax)
	{
	  newmax = bytmax;

	  INC_BYTEBPOS (buf, newmax);
	  newsize = newmax - bytmax;
	  if (newsize != size)
	    {
	      bufmin = bufmax;
	      bytmin = bytmax;
	      size = newsize;
	    }
	  bytmax = newmax;
	  bufmax++;
	}
      retval = bytmax;

      /* #### Should go past the found location to reduce the number
	 of times that this function is called */
    }
  else /* x < bufmin */
    {
      Bytebpos newmin;
      Bytecount newsize;

      forward_p = 0;
      while (x < bufmin)
	{
	  newmin = bytmin;

	  DEC_BYTEBPOS (buf, newmin);
	  newsize = bytmin - newmin;
	  if (newsize != size)
	    {
	      bufmax = bufmin;
	      bytmax = bytmin;
	      size = newsize;
	    }
	  bytmin = newmin;
	  bufmin--;
	}
      retval = bytmin;

      /* #### Should go past the found location to reduce the number
	 of times that this function is called
         */
    }

  /* If size is three, than we have to max sure that the range we
     discovered isn't too large, because we use a fixed-length
     table to divide by 3. */

  if (size == 3)
    {
      int gap = bytmax - bytmin;
      buf->text->mule_three_p = 1;
      buf->text->mule_shifter = 1;

      if (gap > MAX_BYTEBPOS_GAP_SIZE_3)
	{
	  if (forward_p)
	    {
	      bytmin = bytmax - MAX_BYTEBPOS_GAP_SIZE_3;
	      bufmin = bufmax - MAX_CHARBPOS_GAP_SIZE_3;
	    }
	  else
	    {
	      bytmax = bytmin + MAX_BYTEBPOS_GAP_SIZE_3;
	      bufmax = bufmin + MAX_CHARBPOS_GAP_SIZE_3;
	    }
	}
    }
  else
    {
      buf->text->mule_three_p = 0;
      if (size == 4)
	buf->text->mule_shifter = 2;
      else
	buf->text->mule_shifter = size - 1;
    }

  buf->text->mule_bufmin = bufmin;
  buf->text->mule_bufmax = bufmax;
  buf->text->mule_bytmin = bytmin;
  buf->text->mule_bytmax = bytmax;
  
  if (add_to_cache)
    {
      int replace_loc;

      /* We throw away a "random" cached value and replace it with
	 the new value.  It doesn't actually have to be very random
	 at all, just evenly distributed.

	 #### It would be better to use a least-recently-used algorithm
	 or something that tries to space things out, but I'm not sure
	 it's worth it to go to the trouble of maintaining that. */
      not_very_random_number += 621;
      replace_loc = not_very_random_number & 15;
      buf->text->mule_charbpos_cache[replace_loc] = x;
      buf->text->mule_bytebpos_cache[replace_loc] = retval;
    }

  PROFILE_RECORD_EXITING_SECTION (QSin_char_byte_conversion);

  return retval;
}

/* The logic in this function is almost identical to the logic in
   the previous function. */

Charbpos
bytebpos_to_charbpos_func (struct buffer *buf, Bytebpos x)
{
  Charbpos bufmin;
  Charbpos bufmax;
  Bytebpos bytmin;
  Bytebpos bytmax;
  int size;
  int forward_p;
  Charbpos retval;
  int diff_so_far;
  int add_to_cache = 0;
  PROFILE_DECLARE ();

  /* Check for some cached positions, for speed. */
  if (x == BYTE_BUF_PT (buf))
    return BUF_PT (buf);
  if (x == BYTE_BUF_ZV (buf))
    return BUF_ZV (buf);
  if (x == BYTE_BUF_BEGV (buf))
    return BUF_BEGV (buf);

  PROFILE_RECORD_ENTERING_SECTION (QSin_char_byte_conversion);

  bufmin = buf->text->mule_bufmin;
  bufmax = buf->text->mule_bufmax;
  bytmin = buf->text->mule_bytmin;
  bytmax = buf->text->mule_bytmax;
  size = (1 << buf->text->mule_shifter) + !!buf->text->mule_three_p;

  /* The basic idea here is that we shift the "known region" up or down
     until it overlaps the specified position.  We do this by moving
     the upper bound of the known region up one character at a time,
     and moving the lower bound of the known region up as necessary
     when the size of the character just seen changes.

     We optimize this, however, by first shifting the known region to
     one of the cached points if it's close by. (We don't check BYTE_BEG or
     BYTE_Z, even though they're cached; most of the time these will be the
     same as BYTE_BEGV and BYTE_ZV, and when they're not, they're not likely
     to be used.) */

  if (x > bytmax)
    {
      Bytebpos diffmax = x - bytmax;
      Bytebpos diffpt = x - BYTE_BUF_PT (buf);
      Bytebpos diffzv = BYTE_BUF_ZV (buf) - x;
      /* #### This value could stand some more exploration. */
      Bytecount heuristic_hack = (bytmax - bytmin) >> 2;

      /* Check if the position is closer to PT or ZV than to the
	 end of the known region. */

      if (diffpt < 0)
	diffpt = -diffpt;
      if (diffzv < 0)
	diffzv = -diffzv;

      /* But also implement a heuristic that favors the known region
	 over BYTE_PT or BYTE_ZV.  The reason for this is that switching to
	 BYTE_PT or BYTE_ZV will wipe out the knowledge in the known region,
	 which might be annoying if the known region is large and
	 BYTE_PT or BYTE_ZV is not that much closer than the end of the known
	 region. */

      diffzv += heuristic_hack;
      diffpt += heuristic_hack;
      if (diffpt < diffmax && diffpt <= diffzv)
	{
	  bufmax = bufmin = BUF_PT (buf);
	  bytmax = bytmin = BYTE_BUF_PT (buf);
	  /* We set the size to 1 even though it doesn't really
	     matter because the new known region contains no
	     characters.  We do this because this is the most
	     likely size of the characters around the new known
	     region, and we avoid potential yuckiness that is
	     done when size == 3. */
	  size = 1;
	}
      if (diffzv < diffmax)
	{
	  bufmax = bufmin = BUF_ZV (buf);
	  bytmax = bytmin = BYTE_BUF_ZV (buf);
	  size = 1;
	}
    }
#ifdef ERROR_CHECK_TEXT
  else if (x >= bytmin)
    abort ();
#endif
  else
    {
      Bytebpos diffmin = bytmin - x;
      Bytebpos diffpt = BYTE_BUF_PT (buf) - x;
      Bytebpos diffbegv = x - BYTE_BUF_BEGV (buf);
      /* #### This value could stand some more exploration. */
      Bytecount heuristic_hack = (bytmax - bytmin) >> 2;

      if (diffpt < 0)
	diffpt = -diffpt;
      if (diffbegv < 0)
	diffbegv = -diffbegv;

      /* But also implement a heuristic that favors the known region --
	 see above. */

      diffbegv += heuristic_hack;
      diffpt += heuristic_hack;

      if (diffpt < diffmin && diffpt <= diffbegv)
	{
	  bufmax = bufmin = BUF_PT (buf);
	  bytmax = bytmin = BYTE_BUF_PT (buf);
	  /* We set the size to 1 even though it doesn't really
	     matter because the new known region contains no
	     characters.  We do this because this is the most
	     likely size of the characters around the new known
	     region, and we avoid potential yuckiness that is
	     done when size == 3. */
	  size = 1;
	}
      if (diffbegv < diffmin)
	{
	  bufmax = bufmin = BUF_BEGV (buf);
	  bytmax = bytmin = BYTE_BUF_BEGV (buf);
	  size = 1;
	}
    }

  diff_so_far = x > bytmax ? x - bytmax : bytmin - x;
  if (diff_so_far > 50)
    {
      /* If we have to move more than a certain amount, then look
	 into our cache. */
      int minval = INT_MAX;
      int found = 0;
      int i;

      add_to_cache = 1;
      /* I considered keeping the positions ordered.  This would speed
	 up this loop, but updating the cache would take longer, so
	 it doesn't seem like it would really matter. */
      for (i = 0; i < 16; i++)
	{
	  int diff = buf->text->mule_bytebpos_cache[i] - x;

	  if (diff < 0)
	    diff = -diff;
	  if (diff < minval)
	    {
	      minval = diff;
	      found = i;
	    }
	}

      if (minval < diff_so_far)
	{
	  bufmax = bufmin = buf->text->mule_charbpos_cache[found];
	  bytmax = bytmin = buf->text->mule_bytebpos_cache[found];
	  size = 1;
	}
    }

  /* It's conceivable that the caching above could lead to X being
     the same as one of the range edges. */
  if (x >= bytmax)
    {
      Bytebpos newmax;
      Bytecount newsize;

      forward_p = 1;
      while (x > bytmax)
	{
	  newmax = bytmax;

	  INC_BYTEBPOS (buf, newmax);
	  newsize = newmax - bytmax;
	  if (newsize != size)
	    {
	      bufmin = bufmax;
	      bytmin = bytmax;
	      size = newsize;
	    }
	  bytmax = newmax;
	  bufmax++;
	}
      retval = bufmax;

      /* #### Should go past the found location to reduce the number
	 of times that this function is called */
    }
  else /* x <= bytmin */
    {
      Bytebpos newmin;
      Bytecount newsize;

      forward_p = 0;
      while (x < bytmin)
	{
	  newmin = bytmin;

	  DEC_BYTEBPOS (buf, newmin);
	  newsize = bytmin - newmin;
	  if (newsize != size)
	    {
	      bufmax = bufmin;
	      bytmax = bytmin;
	      size = newsize;
	    }
	  bytmin = newmin;
	  bufmin--;
	}
      retval = bufmin;

      /* #### Should go past the found location to reduce the number
	 of times that this function is called
         */
    }

  /* If size is three, than we have to max sure that the range we
     discovered isn't too large, because we use a fixed-length
     table to divide by 3. */

  if (size == 3)
    {
      int gap = bytmax - bytmin;
      buf->text->mule_three_p = 1;
      buf->text->mule_shifter = 1;

      if (gap > MAX_BYTEBPOS_GAP_SIZE_3)
	{
	  if (forward_p)
	    {
	      bytmin = bytmax - MAX_BYTEBPOS_GAP_SIZE_3;
	      bufmin = bufmax - MAX_CHARBPOS_GAP_SIZE_3;
	    }
	  else
	    {
	      bytmax = bytmin + MAX_BYTEBPOS_GAP_SIZE_3;
	      bufmax = bufmin + MAX_CHARBPOS_GAP_SIZE_3;
	    }
	}
    }
  else
    {
      buf->text->mule_three_p = 0;
      if (size == 4)
	buf->text->mule_shifter = 2;
      else
	buf->text->mule_shifter = size - 1;
    }

  buf->text->mule_bufmin = bufmin;
  buf->text->mule_bufmax = bufmax;
  buf->text->mule_bytmin = bytmin;
  buf->text->mule_bytmax = bytmax;

  if (add_to_cache)
    {
      int replace_loc;

      /* We throw away a "random" cached value and replace it with
	 the new value.  It doesn't actually have to be very random
	 at all, just evenly distributed.

	 #### It would be better to use a least-recently-used algorithm
	 or something that tries to space things out, but I'm not sure
	 it's worth it to go to the trouble of maintaining that. */
      not_very_random_number += 621;
      replace_loc = not_very_random_number & 15;
      buf->text->mule_charbpos_cache[replace_loc] = retval;
      buf->text->mule_bytebpos_cache[replace_loc] = x;
    }

  PROFILE_RECORD_EXITING_SECTION (QSin_char_byte_conversion);

  return retval;
}

/* Text of length BYTELENGTH and CHARLENGTH (in different units)
   was inserted at charbpos START. */

void
buffer_mule_signal_inserted_region (struct buffer *buf, Charbpos start,
				    Bytecount bytelength,
				    Charcount charlength)
{
  int size = (1 << buf->text->mule_shifter) + !!buf->text->mule_three_p;
  int i;

  /* Adjust the cache of known positions. */
  for (i = 0; i < 16; i++)
    {

      if (buf->text->mule_charbpos_cache[i] > start)
	{
	  buf->text->mule_charbpos_cache[i] += charlength;
	  buf->text->mule_bytebpos_cache[i] += bytelength;
	}
    }

  if (start >= buf->text->mule_bufmax)
    return;

  /* The insertion is either before the known region, in which case
     it shoves it forward; or within the known region, in which case
     it shoves the end forward. (But it may make the known region
     inconsistent, so we may have to shorten it.) */

  if (start <= buf->text->mule_bufmin)
    {
      buf->text->mule_bufmin += charlength;
      buf->text->mule_bufmax += charlength;
      buf->text->mule_bytmin += bytelength;
      buf->text->mule_bytmax += bytelength;
    }
  else
    {
      Charbpos end = start + charlength;
      /* the insertion point divides the known region in two.
	 Keep the longer half, at least, and expand into the
	 inserted chunk as much as possible. */

      if (start - buf->text->mule_bufmin > buf->text->mule_bufmax - start)
	{
	  Bytebpos bytestart = (buf->text->mule_bytmin
			      + size * (start - buf->text->mule_bufmin));
	  Bytebpos bytenew;

	  while (start < end)
	    {
	      bytenew = bytestart;
	      INC_BYTEBPOS (buf, bytenew);
	      if (bytenew - bytestart != size)
		break;
	      start++;
              bytestart = bytenew;
	    }
	  if (start != end)
	    {
	      buf->text->mule_bufmax = start;
	      buf->text->mule_bytmax = bytestart;
	    }
	  else
	    {
	      buf->text->mule_bufmax += charlength;
	      buf->text->mule_bytmax += bytelength;
	    }
	}
      else
	{
	  Bytebpos byteend = (buf->text->mule_bytmin
			    + size * (start - buf->text->mule_bufmin)
			    + bytelength);
	  Bytebpos bytenew;

	  buf->text->mule_bufmax += charlength;
	  buf->text->mule_bytmax += bytelength;

	  while (end > start)
	    {
	      bytenew = byteend;
	      DEC_BYTEBPOS (buf, bytenew);
	      if (byteend - bytenew != size)
		break;
	      end--;
              byteend = bytenew;
	    }
	  if (start != end)
	    {
	      buf->text->mule_bufmin = end;
	      buf->text->mule_bytmin = byteend;
	    }
	}
    }
}

/* Text from START to END (equivalent in Bytebpos's: from BYTE_START to
   BYTE_END) was deleted. */

void
buffer_mule_signal_deleted_region (struct buffer *buf, Charbpos start,
				   Charbpos end, Bytebpos byte_start,
				   Bytebpos byte_end)
{
  int i;

  /* Adjust the cache of known positions. */
  for (i = 0; i < 16; i++)
    {
      /* After the end; gets shoved backward */
      if (buf->text->mule_charbpos_cache[i] > end)
	{
	  buf->text->mule_charbpos_cache[i] -= end - start;
	  buf->text->mule_bytebpos_cache[i] -= byte_end - byte_start;
	}
      /* In the range; moves to start of range */
      else if (buf->text->mule_charbpos_cache[i] > start)
	{
	  buf->text->mule_charbpos_cache[i] = start;
	  buf->text->mule_bytebpos_cache[i] = byte_start;
	}
    }

  /* We don't care about any text after the end of the known region. */

  end = min (end, buf->text->mule_bufmax);
  byte_end = min (byte_end, buf->text->mule_bytmax);
  if (start >= end)
    return;

  /* The end of the known region offsets by the total amount of deletion,
     since it's all before it. */

  buf->text->mule_bufmax -= end - start;
  buf->text->mule_bytmax -= byte_end - byte_start;

  /* Now we don't care about any text after the start of the known region. */

  end = min (end, buf->text->mule_bufmin);
  byte_end = min (byte_end, buf->text->mule_bytmin);
  if (start < end)
    {
      buf->text->mule_bufmin -= end - start;
      buf->text->mule_bytmin -= byte_end - byte_start;
    }
}

#endif /* MULE */


/************************************************************************/
/*                verifying buffer and string positions                 */
/************************************************************************/

/* Functions below are tagged with either _byte or _char indicating
   whether they return byte or character positions.  For a buffer,
   a character position is a "Charbpos" and a byte position is a "Bytebpos".
   For strings, these are sometimes typed using "Charcount" and
   "Bytecount". */

/* Flags for the functions below are:

   GB_ALLOW_PAST_ACCESSIBLE

     Allow positions to range over the entire buffer (BUF_BEG to BUF_Z),
     rather than just the accessible portion (BUF_BEGV to BUF_ZV).
     For strings, this flag has no effect.

   GB_COERCE_RANGE

     If the position is outside the allowable range, return the lower
     or upper bound of the range, whichever is closer to the specified
     position.

   GB_NO_ERROR_IF_BAD

     If the position is outside the allowable range, return -1.

   GB_NEGATIVE_FROM_END

     If a value is negative, treat it as an offset from the end.
     Only applies to strings.

   The following additional flags apply only to the functions
   that return ranges:

   GB_ALLOW_NIL

     Either or both positions can be nil.  If FROM is nil,
     FROM_OUT will contain the lower bound of the allowed range.
     If TO is nil, TO_OUT will contain the upper bound of the
     allowed range.

   GB_CHECK_ORDER

     FROM must contain the lower bound and TO the upper bound
     of the range.  If the positions are reversed, an error is
     signalled.

   The following is a combination flag:

   GB_HISTORICAL_STRING_BEHAVIOR

     Equivalent to (GB_NEGATIVE_FROM_END | GB_ALLOW_NIL).
 */

/* Return a buffer position stored in a Lisp_Object.  Full
   error-checking is done on the position.  Flags can be specified to
   control the behavior of out-of-range values.  The default behavior
   is to require that the position is within the accessible part of
   the buffer (BEGV and ZV), and to signal an error if the position is
   out of range.

*/

Charbpos
get_buffer_pos_char (struct buffer *b, Lisp_Object pos, unsigned int flags)
{
  /* Does not GC */
  Charbpos ind;
  Charbpos min_allowed, max_allowed;

  CHECK_INT_COERCE_MARKER (pos);
  ind = XINT (pos);
  min_allowed = flags & GB_ALLOW_PAST_ACCESSIBLE ? BUF_BEG (b) : BUF_BEGV (b);
  max_allowed = flags & GB_ALLOW_PAST_ACCESSIBLE ? BUF_Z   (b) : BUF_ZV   (b);

  if (ind < min_allowed || ind > max_allowed)
    {
      if (flags & GB_COERCE_RANGE)
	ind = ind < min_allowed ? min_allowed : max_allowed;
      else if (flags & GB_NO_ERROR_IF_BAD)
	ind = -1;
      else
	{
	  Lisp_Object buffer = wrap_buffer (b);

	  args_out_of_range (buffer, pos);
	}
    }

  return ind;
}

Bytebpos
get_buffer_pos_byte (struct buffer *b, Lisp_Object pos, unsigned int flags)
{
  Charbpos bpos = get_buffer_pos_char (b, pos, flags);
  if (bpos < 0) /* could happen with GB_NO_ERROR_IF_BAD */
    return -1;
  return charbpos_to_bytebpos (b, bpos);
}

/* Return a pair of buffer positions representing a range of text,
   taken from a pair of Lisp_Objects.  Full error-checking is
   done on the positions.  Flags can be specified to control the
   behavior of out-of-range values.  The default behavior is to
   allow the range bounds to be specified in either order
   (however, FROM_OUT will always be the lower bound of the range
   and TO_OUT the upper bound),to require that the positions
   are within the accessible part of the buffer (BEGV and ZV),
   and to signal an error if the positions are out of range.
*/

void
get_buffer_range_char (struct buffer *b, Lisp_Object from, Lisp_Object to,
		       Charbpos *from_out, Charbpos *to_out,
		       unsigned int flags)
{
  /* Does not GC */
  Charbpos min_allowed, max_allowed;

  min_allowed = (flags & GB_ALLOW_PAST_ACCESSIBLE) ?
    BUF_BEG (b) : BUF_BEGV (b);
  max_allowed = (flags & GB_ALLOW_PAST_ACCESSIBLE) ?
    BUF_Z (b) : BUF_ZV (b);

  if (NILP (from) && (flags & GB_ALLOW_NIL))
    *from_out = min_allowed;
  else
    *from_out = get_buffer_pos_char (b, from, flags | GB_NO_ERROR_IF_BAD);

  if (NILP (to) && (flags & GB_ALLOW_NIL))
    *to_out = max_allowed;
  else
    *to_out = get_buffer_pos_char (b, to, flags | GB_NO_ERROR_IF_BAD);

  if ((*from_out < 0 || *to_out < 0) && !(flags & GB_NO_ERROR_IF_BAD))
    {
      Lisp_Object buffer = wrap_buffer (b);

      args_out_of_range_3 (buffer, from, to);
    }

  if (*from_out >= 0 && *to_out >= 0 && *from_out > *to_out)
    {
      if (flags & GB_CHECK_ORDER)
	invalid_argument_2 ("start greater than end", from, to);
      else
	{
	  Charbpos temp = *from_out;
	  *from_out = *to_out;
	  *to_out = temp;
	}
    }
}

void
get_buffer_range_byte (struct buffer *b, Lisp_Object from, Lisp_Object to,
		       Bytebpos *from_out, Bytebpos *to_out,
		       unsigned int flags)
{
  Charbpos s, e;

  get_buffer_range_char (b, from, to, &s, &e, flags);
  if (s >= 0)
    *from_out = charbpos_to_bytebpos (b, s);
  else /* could happen with GB_NO_ERROR_IF_BAD */
    *from_out = -1;
  if (e >= 0)
    *to_out = charbpos_to_bytebpos (b, e);
  else
    *to_out = -1;
}

static Charcount
get_string_pos_char_1 (Lisp_Object string, Lisp_Object pos, unsigned int flags,
		       Charcount known_length)
{
  Charcount ccpos;
  Charcount min_allowed = 0;
  Charcount max_allowed = known_length;

  /* Computation of KNOWN_LENGTH is potentially expensive so we pass
     it in. */
  CHECK_INT (pos);
  ccpos = XINT (pos);
  if (ccpos < 0 && flags & GB_NEGATIVE_FROM_END)
    ccpos += max_allowed;

  if (ccpos < min_allowed || ccpos > max_allowed)
    {
      if (flags & GB_COERCE_RANGE)
	ccpos = ccpos < min_allowed ? min_allowed : max_allowed;
      else if (flags & GB_NO_ERROR_IF_BAD)
	ccpos = -1;
      else
	args_out_of_range (string, pos);
    }

  return ccpos;
}

Charcount
get_string_pos_char (Lisp_Object string, Lisp_Object pos, unsigned int flags)
{
  return get_string_pos_char_1 (string, pos, flags,
				string_char_length (string));
}

Bytecount
get_string_pos_byte (Lisp_Object string, Lisp_Object pos, unsigned int flags)
{
  Charcount ccpos = get_string_pos_char (string, pos, flags);
  if (ccpos < 0) /* could happen with GB_NO_ERROR_IF_BAD */
    return -1;
  return string_index_char_to_byte (string, ccpos);
}

void
get_string_range_char (Lisp_Object string, Lisp_Object from, Lisp_Object to,
		       Charcount *from_out, Charcount *to_out,
		       unsigned int flags)
{
  Charcount min_allowed = 0;
  Charcount max_allowed = string_char_length (string);

  if (NILP (from) && (flags & GB_ALLOW_NIL))
    *from_out = min_allowed;
  else
    *from_out = get_string_pos_char_1 (string, from,
				       flags | GB_NO_ERROR_IF_BAD,
				       max_allowed);

  if (NILP (to) && (flags & GB_ALLOW_NIL))
    *to_out = max_allowed;
  else
    *to_out = get_string_pos_char_1 (string, to,
				     flags | GB_NO_ERROR_IF_BAD,
				     max_allowed);

  if ((*from_out < 0 || *to_out < 0) && !(flags & GB_NO_ERROR_IF_BAD))
    args_out_of_range_3 (string, from, to);

  if (*from_out >= 0 && *to_out >= 0 && *from_out > *to_out)
    {
      if (flags & GB_CHECK_ORDER)
	invalid_argument_2 ("start greater than end", from, to);
      else
	{
	  Charbpos temp = *from_out;
	  *from_out = *to_out;
	  *to_out = temp;
	}
    }
}

void
get_string_range_byte (Lisp_Object string, Lisp_Object from, Lisp_Object to,
		       Bytecount *from_out, Bytecount *to_out,
		       unsigned int flags)
{
  Charcount s, e;

  get_string_range_char (string, from, to, &s, &e, flags);
  if (s >= 0)
    *from_out = string_index_char_to_byte (string, s);
  else /* could happen with GB_NO_ERROR_IF_BAD */
    *from_out = -1;
  if (e >= 0)
    *to_out = string_index_char_to_byte (string, e);
  else
    *to_out = -1;

}

Charxpos
get_buffer_or_string_pos_char (Lisp_Object object, Lisp_Object pos,
			       unsigned int flags)
{
  return STRINGP (object) ?
    get_string_pos_char (object, pos, flags) :
    get_buffer_pos_char (XBUFFER (object), pos, flags);
}

Bytexpos
get_buffer_or_string_pos_byte (Lisp_Object object, Lisp_Object pos,
			       unsigned int flags)
{
  return STRINGP (object) ?
    get_string_pos_byte (object, pos, flags) :
    get_buffer_pos_byte (XBUFFER (object), pos, flags);
}

void
get_buffer_or_string_range_char (Lisp_Object object, Lisp_Object from,
				 Lisp_Object to, Charxpos *from_out,
				 Charxpos *to_out, unsigned int flags)
{
  if (STRINGP (object))
    get_string_range_char (object, from, to, from_out, to_out, flags);
  else
    get_buffer_range_char (XBUFFER (object), from, to, from_out, to_out,
			   flags);
}

void
get_buffer_or_string_range_byte (Lisp_Object object, Lisp_Object from,
				 Lisp_Object to, Bytexpos *from_out,
				 Bytexpos *to_out, unsigned int flags)
{
  if (STRINGP (object))
    get_string_range_byte (object, from, to, from_out, to_out, flags);
  else
    get_buffer_range_byte (XBUFFER (object), from, to, from_out, to_out,
			   flags);
}

Charxpos
buffer_or_string_accessible_begin_char (Lisp_Object object)
{
  return STRINGP (object) ? 0 : BUF_BEGV (XBUFFER (object));
}

Charxpos
buffer_or_string_accessible_end_char (Lisp_Object object)
{
  return STRINGP (object) ?
    string_char_length (object) : BUF_ZV (XBUFFER (object));
}

Bytexpos
buffer_or_string_accessible_begin_byte (Lisp_Object object)
{
  return STRINGP (object) ? 0 : BYTE_BUF_BEGV (XBUFFER (object));
}

Bytexpos
buffer_or_string_accessible_end_byte (Lisp_Object object)
{
  return STRINGP (object) ?
    XSTRING_LENGTH (object) : BYTE_BUF_ZV (XBUFFER (object));
}

Charxpos
buffer_or_string_absolute_begin_char (Lisp_Object object)
{
  return STRINGP (object) ? 0 : BUF_BEG (XBUFFER (object));
}

Charxpos
buffer_or_string_absolute_end_char (Lisp_Object object)
{
  return STRINGP (object) ?
    string_char_length (object) : BUF_Z (XBUFFER (object));
}

Bytexpos
buffer_or_string_absolute_begin_byte (Lisp_Object object)
{
  return STRINGP (object) ? 0 : BYTE_BUF_BEG (XBUFFER (object));
}

Bytexpos
buffer_or_string_absolute_end_byte (Lisp_Object object)
{
  return STRINGP (object) ?
    XSTRING_LENGTH (object) : BYTE_BUF_Z (XBUFFER (object));
}

Charbpos
charbpos_clip_to_bounds (Charbpos lower, Charbpos num, Charbpos upper)
{
  return (num < lower ? lower :
	  num > upper ? upper :
	  num);
}

Bytebpos
bytebpos_clip_to_bounds (Bytebpos lower, Bytebpos num, Bytebpos upper)
{
  return (num < lower ? lower :
	  num > upper ? upper :
	  num);
}

Charxpos
charxpos_clip_to_bounds (Charxpos lower, Charxpos num, Charxpos upper)
{
  return (num < lower ? lower :
	  num > upper ? upper :
	  num);
}

Bytexpos
bytexpos_clip_to_bounds (Bytexpos lower, Bytexpos num, Bytexpos upper)
{
  return (num < lower ? lower :
	  num > upper ? upper :
	  num);
}

/* These could be implemented in terms of the get_buffer_or_string()
   functions above, but those are complicated and handle lots of weird
   cases stemming from uncertain external input. */

Charxpos
buffer_or_string_clip_to_accessible_char (Lisp_Object object, Charxpos pos)
{
  return (charxpos_clip_to_bounds
	  (pos, buffer_or_string_accessible_begin_char (object),
	   buffer_or_string_accessible_end_char (object)));
}

Bytexpos
buffer_or_string_clip_to_accessible_byte (Lisp_Object object, Bytexpos pos)
{
  return (bytexpos_clip_to_bounds
	  (pos, buffer_or_string_accessible_begin_byte (object),
	   buffer_or_string_accessible_end_byte (object)));
}

Charxpos
buffer_or_string_clip_to_absolute_char (Lisp_Object object, Charxpos pos)
{
  return (charxpos_clip_to_bounds
	  (pos, buffer_or_string_absolute_begin_char (object),
	   buffer_or_string_absolute_end_char (object)));
}

Bytexpos
buffer_or_string_clip_to_absolute_byte (Lisp_Object object, Bytexpos pos)
{
  return (bytexpos_clip_to_bounds
	  (pos, buffer_or_string_absolute_begin_byte (object),
	   buffer_or_string_absolute_end_byte (object)));
}


/************************************************************************/
/*           Implement TO_EXTERNAL_FORMAT, TO_INTERNAL_FORMAT           */
/************************************************************************/

typedef struct
{
  Dynarr_declare (Ibyte_dynarr *);
} Ibyte_dynarr_dynarr;

typedef struct
{
  Dynarr_declare (Extbyte_dynarr *);
} Extbyte_dynarr_dynarr;

static Extbyte_dynarr_dynarr *conversion_out_dynarr_list;
static Ibyte_dynarr_dynarr *conversion_in_dynarr_list;

static int dfc_convert_to_external_format_in_use;
static int dfc_convert_to_internal_format_in_use;

void
dfc_convert_to_external_format (dfc_conversion_type source_type,
				dfc_conversion_data *source,
				Lisp_Object coding_system,
				dfc_conversion_type sink_type,
				dfc_conversion_data *sink)
{
  /* It's guaranteed that many callers are not prepared for GC here,
     esp. given that this code conversion occurs in many very hidden
     places. */
  int count;
  Extbyte_dynarr *conversion_out_dynarr;
  PROFILE_DECLARE ();

  PROFILE_RECORD_ENTERING_SECTION (QSin_internal_external_conversion);

  count = begin_gc_forbidden ();

  type_checking_assert
    (((source_type == DFC_TYPE_DATA) ||
      (source_type == DFC_TYPE_LISP_LSTREAM && LSTREAMP (source->lisp_object)) ||
      (source_type == DFC_TYPE_LISP_STRING && STRINGP (source->lisp_object)))
     &&
     ((sink_type == DFC_TYPE_DATA) ||
      (sink_type == DFC_TYPE_LISP_LSTREAM && LSTREAMP (source->lisp_object))));

  if (Dynarr_length (conversion_out_dynarr_list) <=
      dfc_convert_to_external_format_in_use)
    Dynarr_add (conversion_out_dynarr_list, Dynarr_new (Extbyte));
  conversion_out_dynarr = Dynarr_at (conversion_out_dynarr_list,
				     dfc_convert_to_external_format_in_use);
  Dynarr_reset (conversion_out_dynarr);

  internal_bind_int (&dfc_convert_to_external_format_in_use,
		     dfc_convert_to_external_format_in_use + 1);

  coding_system = get_coding_system_for_text_file (coding_system, 0);

  /* Here we optimize in the case where the coding system does no
     conversion. However, we don't want to optimize in case the source
     or sink is an lstream, since writing to an lstream can cause a
     garbage collection, and this could be problematic if the source
     is a lisp string. */
  if (source_type != DFC_TYPE_LISP_LSTREAM &&
      sink_type   != DFC_TYPE_LISP_LSTREAM &&
      coding_system_is_binary (coding_system))
    {
      const Ibyte *ptr;
      Bytecount len;

      if (source_type == DFC_TYPE_LISP_STRING)
	{
	  ptr = XSTRING_DATA   (source->lisp_object);
	  len = XSTRING_LENGTH (source->lisp_object);
	}
      else
	{
	  ptr = (Ibyte *) source->data.ptr;
	  len = source->data.len;
	}

#ifdef MULE
      {
	const Ibyte *end;
	for (end = ptr + len; ptr < end;)
	  {
	    Ibyte c =
	      (byte_ascii_p (*ptr))		   ? *ptr :
	      (*ptr == LEADING_BYTE_CONTROL_1)	   ? (*(ptr+1) - 0x20) :
	      (*ptr == LEADING_BYTE_LATIN_ISO8859_1) ? (*(ptr+1)) :
	      '~';

	    Dynarr_add (conversion_out_dynarr, (Extbyte) c);
	    INC_IBYTEPTR (ptr);
	  }
	text_checking_assert (ptr == end);
      }
#else
      Dynarr_add_many (conversion_out_dynarr, ptr, len);
#endif

    }
#ifdef WIN32_ANY
  /* Optimize the common case involving Unicode where only ASCII is involved */
  else if (source_type != DFC_TYPE_LISP_LSTREAM &&
	   sink_type   != DFC_TYPE_LISP_LSTREAM &&
	   dfc_coding_system_is_unicode (coding_system))
    {
      const Ibyte *ptr, *p;
      Bytecount len;
      const Ibyte *end;

      if (source_type == DFC_TYPE_LISP_STRING)
	{
	  ptr = XSTRING_DATA   (source->lisp_object);
	  len = XSTRING_LENGTH (source->lisp_object);
	}
      else
	{
	  ptr = (Ibyte *) source->data.ptr;
	  len = source->data.len;
	}
      end = ptr + len;

      for (p = ptr; p < end; p++)
	{
	  if (!byte_ascii_p (*p))
	    goto the_hard_way;
	}

      for (p = ptr; p < end; p++)
	{
	  Dynarr_add (conversion_out_dynarr, (Extbyte) (*p));
	  Dynarr_add (conversion_out_dynarr, (Extbyte) '\0');
	}
    }
#endif /* WIN32_ANY */
  else
    {
      Lisp_Object streams_to_delete[3];
      int delete_count;
      Lisp_Object instream, outstream;
      Lstream *reader, *writer;

#ifdef WIN32_ANY
    the_hard_way:
#endif /* WIN32_ANY */
      delete_count = 0;
      if (source_type == DFC_TYPE_LISP_LSTREAM)
	instream = source->lisp_object;
      else if (source_type == DFC_TYPE_DATA)
	streams_to_delete[delete_count++] = instream =
	  make_fixed_buffer_input_stream (source->data.ptr, source->data.len);
      else
	{
	  type_checking_assert (source_type == DFC_TYPE_LISP_STRING);
	  streams_to_delete[delete_count++] = instream =
	    /* This will GCPRO the Lisp string */
	    make_lisp_string_input_stream (source->lisp_object, 0, -1);
	}

      if (sink_type == DFC_TYPE_LISP_LSTREAM)
	outstream = sink->lisp_object;
      else
	{
	  type_checking_assert (sink_type == DFC_TYPE_DATA);
	  streams_to_delete[delete_count++] = outstream =
	    make_dynarr_output_stream
	    ((unsigned_char_dynarr *) conversion_out_dynarr);
	}

      streams_to_delete[delete_count++] = outstream =
	make_coding_output_stream (XLSTREAM (outstream), coding_system,
				   CODING_ENCODE, 0);

      reader = XLSTREAM (instream);
      writer = XLSTREAM (outstream);
      /* decoding_stream will gc-protect outstream */
      {
	struct gcpro gcpro1, gcpro2;
	GCPRO2 (instream, outstream);

	while (1)
	  {
	    Bytecount size_in_bytes;
	    char tempbuf[1024]; /* some random amount */

	    size_in_bytes = Lstream_read (reader, tempbuf, sizeof (tempbuf));

	    if (size_in_bytes == 0)
	      break;
	    else if (size_in_bytes < 0)
	      signal_error (Qtext_conversion_error,
			    "Error converting to external format", Qunbound);

	    if (Lstream_write (writer, tempbuf, size_in_bytes) < 0)
	      signal_error (Qtext_conversion_error,
			    "Error converting to external format", Qunbound);
	  }

	/* Closing writer will close any stream at the other end of writer. */
	Lstream_close (writer);
	Lstream_close (reader);
	UNGCPRO;
      }

      /* The idea is that this function will create no garbage. */
      while (delete_count)
	Lstream_delete (XLSTREAM (streams_to_delete [--delete_count]));
    }

  unbind_to (count);

  if (sink_type != DFC_TYPE_LISP_LSTREAM)
    {
      sink->data.len = Dynarr_length (conversion_out_dynarr);
      /* double zero-extend because we may be dealing with Unicode data */
      Dynarr_add (conversion_out_dynarr, '\0');
      Dynarr_add (conversion_out_dynarr, '\0');
      sink->data.ptr = Dynarr_atp (conversion_out_dynarr, 0);
    }

  PROFILE_RECORD_EXITING_SECTION (QSin_internal_external_conversion);
}

void
dfc_convert_to_internal_format (dfc_conversion_type source_type,
				dfc_conversion_data *source,
				Lisp_Object coding_system,
				dfc_conversion_type sink_type,
				dfc_conversion_data *sink)
{
  /* It's guaranteed that many callers are not prepared for GC here,
     esp. given that this code conversion occurs in many very hidden
     places. */
  int count;
  Ibyte_dynarr *conversion_in_dynarr;
  PROFILE_DECLARE ();

  PROFILE_RECORD_ENTERING_SECTION (QSin_internal_external_conversion);

  count = begin_gc_forbidden ();

  type_checking_assert
    ((source_type == DFC_TYPE_DATA ||
      source_type == DFC_TYPE_LISP_LSTREAM)
    &&
    (sink_type   == DFC_TYPE_DATA ||
     sink_type   == DFC_TYPE_LISP_LSTREAM));

  if (Dynarr_length (conversion_in_dynarr_list) <=
      dfc_convert_to_internal_format_in_use)
    Dynarr_add (conversion_in_dynarr_list, Dynarr_new (Ibyte));
  conversion_in_dynarr = Dynarr_at (conversion_in_dynarr_list,
				    dfc_convert_to_internal_format_in_use);
  Dynarr_reset (conversion_in_dynarr);

  internal_bind_int (&dfc_convert_to_internal_format_in_use,
		     dfc_convert_to_internal_format_in_use + 1);

  coding_system = get_coding_system_for_text_file (coding_system, 1);

  if (source_type != DFC_TYPE_LISP_LSTREAM &&
      sink_type   != DFC_TYPE_LISP_LSTREAM &&
      coding_system_is_binary (coding_system))
    {
#ifdef MULE
      const Ibyte *ptr = (const Ibyte *) source->data.ptr;
      Bytecount len = source->data.len;
      const Ibyte *end = ptr + len;

      for (; ptr < end; ptr++)
        {
          Ibyte c = *ptr;

	  if (byte_ascii_p (c))
	    Dynarr_add (conversion_in_dynarr, c);
	  else if (byte_c1_p (c))
	    {
	      Dynarr_add (conversion_in_dynarr, LEADING_BYTE_CONTROL_1);
	      Dynarr_add (conversion_in_dynarr, c + 0x20);
	    }
	  else
	    {
	      Dynarr_add (conversion_in_dynarr, LEADING_BYTE_LATIN_ISO8859_1);
	      Dynarr_add (conversion_in_dynarr, c);
	    }
        }
#else
      Dynarr_add_many (conversion_in_dynarr, source->data.ptr, source->data.len);
#endif
    }
#ifdef WIN32_ANY
  /* Optimize the common case involving Unicode where only ASCII/Latin-1 is
     involved */
  else if (source_type != DFC_TYPE_LISP_LSTREAM &&
	   sink_type   != DFC_TYPE_LISP_LSTREAM &&
	   dfc_coding_system_is_unicode (coding_system))
    {
      const Ibyte *ptr = (const Ibyte *) source->data.ptr + 1;
      Bytecount len = source->data.len;
      const Ibyte *end = ptr + len;

      if (len & 1)
	goto the_hard_way;

      for (; ptr < end; ptr += 2)
	{
	  if (*ptr)
	    goto the_hard_way;
	}

      ptr = (const Ibyte *) source->data.ptr;
      end = ptr + len;

      for (; ptr < end; ptr += 2)
	{
          Ibyte c = *ptr;

	  if (byte_ascii_p (c))
	    Dynarr_add (conversion_in_dynarr, c);
#ifdef MULE
	  else if (byte_c1_p (c))
	    {
	      Dynarr_add (conversion_in_dynarr, LEADING_BYTE_CONTROL_1);
	      Dynarr_add (conversion_in_dynarr, c + 0x20);
	    }
	  else
	    {
	      Dynarr_add (conversion_in_dynarr, LEADING_BYTE_LATIN_ISO8859_1);
	      Dynarr_add (conversion_in_dynarr, c);
	    }
#endif /* MULE */
        }
    }
#endif /* WIN32_ANY */
  else
    {
      Lisp_Object streams_to_delete[3];
      int delete_count;
      Lisp_Object instream, outstream;
      Lstream *reader, *writer;

#ifdef WIN32_ANY
    the_hard_way:
#endif /* WIN32_ANY */
      delete_count = 0;
      if (source_type == DFC_TYPE_LISP_LSTREAM)
	instream = source->lisp_object;
      else
	{
	  type_checking_assert (source_type == DFC_TYPE_DATA);
	  streams_to_delete[delete_count++] = instream =
	    make_fixed_buffer_input_stream (source->data.ptr, source->data.len);
	}

      if (sink_type == DFC_TYPE_LISP_LSTREAM)
	outstream = sink->lisp_object;
      else
	{
	  type_checking_assert (sink_type == DFC_TYPE_DATA);
	  streams_to_delete[delete_count++] = outstream =
	    make_dynarr_output_stream
	    ((unsigned_char_dynarr *) conversion_in_dynarr);
	}

      streams_to_delete[delete_count++] = outstream =
	make_coding_output_stream (XLSTREAM (outstream), coding_system,
				   CODING_DECODE, 0);

      reader = XLSTREAM (instream);
      writer = XLSTREAM (outstream);
      {
	struct gcpro gcpro1, gcpro2;
	/* outstream will gc-protect its sink stream, if necessary */
	GCPRO2 (instream, outstream);

	while (1)
	  {
	    Bytecount size_in_bytes;
	    char tempbuf[1024]; /* some random amount */

	    size_in_bytes = Lstream_read (reader, tempbuf, sizeof (tempbuf));

	    if (size_in_bytes == 0)
	      break;
	    else if (size_in_bytes < 0)
	      signal_error (Qtext_conversion_error,
			    "Error converting to internal format", Qunbound);

	    if (Lstream_write (writer, tempbuf, size_in_bytes) < 0)
	      signal_error (Qtext_conversion_error,
			    "Error converting to internal format", Qunbound);
	  }

	/* Closing writer will close any stream at the other end of writer. */
	Lstream_close (writer);
	Lstream_close (reader);
	UNGCPRO;
      }

      /* The idea is that this function will create no garbage. */
      while (delete_count)
	Lstream_delete (XLSTREAM (streams_to_delete [--delete_count]));
    }

  unbind_to (count);

  if (sink_type != DFC_TYPE_LISP_LSTREAM)
    {
      sink->data.len = Dynarr_length (conversion_in_dynarr);
      Dynarr_add (conversion_in_dynarr, '\0'); /* remember to NUL-terminate! */
      /* The macros don't currently distinguish between internal and
	 external sinks, and allocate and copy two extra bytes in both
	 cases.  So we add a second zero, just like for external data
	 (in that case, because we may be converting to Unicode). */
      Dynarr_add (conversion_in_dynarr, '\0');
      sink->data.ptr = Dynarr_atp (conversion_in_dynarr, 0);
    }

  PROFILE_RECORD_EXITING_SECTION (QSin_internal_external_conversion);
}

/* ----------------------------------------------------------------------- */
/* New-style DFC converters (data is returned rather than stored into var) */
/* ----------------------------------------------------------------------- */

/* We handle here the cases where SRC is a Lisp_Object, internal data
   (sized or unsized), or external data (sized or unsized), and return type
   is unsized alloca() or malloc() data.  If the return type is a
   Lisp_Object, use build_ext_string() for unsized external data,
   make_ext_string() for sized external data.  If the return type needs to
   be sized data, use the *_TO_SIZED_*() macros, and for other more
   complicated cases, use the original TO_*_FORMAT() macros. */

static void
new_dfc_convert_now_damn_it (const void *src, Bytecount src_size,
			     enum new_dfc_src_type type,
			     void **dst, Bytecount *dst_size,
			     Lisp_Object codesys)
{
  /* #### In the case of alloca(), it would be a bit more efficient, for
     small strings, to use static Dynarr's like are used internally in
     TO_*_FORMAT(), or some other way of avoiding malloc() followed by
     free().  I doubt it really matters, though. */

  switch (type)
    {
    case DFC_EXTERNAL:
      TO_INTERNAL_FORMAT (C_STRING, src,
			  MALLOC, (*dst, *dst_size), codesys);
      break;

    case DFC_SIZED_EXTERNAL:
      TO_INTERNAL_FORMAT (DATA, (src, src_size),
			  MALLOC, (*dst, *dst_size), codesys);
      break;

    case DFC_INTERNAL:
      TO_EXTERNAL_FORMAT (C_STRING, src,
			  MALLOC, (*dst, *dst_size), codesys);
      break;

    case DFC_SIZED_INTERNAL:
      TO_EXTERNAL_FORMAT (DATA, (src, src_size),
			  MALLOC, (*dst, *dst_size), codesys);
      break;

    case DFC_LISP_STRING:
      TO_EXTERNAL_FORMAT (LISP_STRING, VOID_TO_LISP (src),
			  MALLOC, (*dst, *dst_size), codesys);
      break;

    default:
      abort ();
    }
}

void *
new_dfc_convert_malloc (const void *src, Bytecount src_size,
			enum new_dfc_src_type type, Lisp_Object codesys)
{
  void *dst;
  Bytecount dst_size;

  new_dfc_convert_now_damn_it (src, src_size, type, &dst, &dst_size, codesys);
  return dst;
}

/* For alloca(), things are trickier because the calling function needs to
   allocate.  This means that the caller needs to do the following:

   (a) invoke us to do the conversion, remember the data and return the size.
   (b) alloca() the proper size.
   (c) invoke us again to copy the data.

   We need to handle the possibility of two or more invocations of the
   converter in the same expression.  In such cases it's conceivable that
   the evaluation of the sub-expressions will be overlapping (e.g. one size
   function called, then the other one called, then the copy functions
   called).  To handle this, we keep a list of active data, indexed by the
   src expression. (We use the stringize operator to avoid evaluating the
   expression multiple times.) If the caller uses the exact same src
   expression twice in two converter calls in the same subexpression, we
   will lose, but at least we can check for this and abort().  We could
   conceivably try to index on other parameters as well, but there is not
   really any point. */

typedef struct
{
  const char *srctext;
  void *dst;
  Bytecount dst_size;
} dfc_e2c_vals;

typedef struct
{
  Dynarr_declare (dfc_e2c_vals);
} dfc_e2c_vals_dynarr;

static dfc_e2c_vals_dynarr *active_dfc_e2c;

static int
find_pos_of_existing_active_dfc_e2c (const char *srctext)
{
  dfc_e2c_vals *vals = NULL;
  int i;

  for (i = 0; i < Dynarr_length (active_dfc_e2c); i++)
    {
      vals = Dynarr_atp (active_dfc_e2c, i);
      if (vals->srctext == srctext)
	return i;
    }

  return -1;
}

void *
new_dfc_convert_alloca (const char *srctext, void *alloca_data)
{
  dfc_e2c_vals *vals;
  int i = find_pos_of_existing_active_dfc_e2c (srctext);

  assert (i >= 0);
  vals = Dynarr_atp (active_dfc_e2c, i);
  assert (alloca_data);
  memcpy (alloca_data, vals->dst, vals->dst_size + 2);
  xfree (vals->dst, void *);
  Dynarr_delete (active_dfc_e2c, i);
  return alloca_data;
}

Bytecount
new_dfc_convert_size (const char *srctext, const void *src,
		      Bytecount src_size, enum new_dfc_src_type type,
		      Lisp_Object codesys)
{
  dfc_e2c_vals vals;

  assert (find_pos_of_existing_active_dfc_e2c (srctext) < 0);

  vals.srctext = srctext;

  new_dfc_convert_now_damn_it (src, src_size, type, &vals.dst, &vals.dst_size,
			       codesys);

  Dynarr_add (active_dfc_e2c, vals);
  /* The size is always + 2 because we have double zero-termination at the
     end of all data (for Unicode-correctness). */
  return vals.dst_size + 2;
}


/************************************************************************/
/*                       Basic Ichar functions                         */
/************************************************************************/

#ifdef MULE

/* Convert a non-ASCII Mule character C into a one-character Mule-encoded
   string in STR.  Returns the number of bytes stored.
   Do not call this directly.  Use the macro set_itext_ichar() instead.
 */

Bytecount
non_ascii_set_itext_ichar (Ibyte *str, Ichar c)
{
  Ibyte *p;
  Ibyte lb;
  int c1, c2;
  Lisp_Object charset;

  p = str;
  BREAKUP_ICHAR (c, charset, c1, c2);
  lb = ichar_leading_byte (c);
  if (leading_byte_private_p (lb))
    *p++ = private_leading_byte_prefix (lb);
  *p++ = lb;
  if (EQ (charset, Vcharset_control_1))
    c1 += 0x20;
  *p++ = c1 | 0x80;
  if (c2)
    *p++ = c2 | 0x80;

  return (p - str);
}

/* Return the first character from a Mule-encoded string in STR,
   assuming it's non-ASCII.  Do not call this directly.
   Use the macro itext_ichar() instead. */

Ichar
non_ascii_itext_ichar (const Ibyte *str)
{
  Ibyte i0 = *str, i1, i2 = 0;
  Lisp_Object charset;

  if (i0 == LEADING_BYTE_CONTROL_1)
    return (Ichar) (*++str - 0x20);

  if (leading_byte_prefix_p (i0))
    i0 = *++str;

  i1 = *++str & 0x7F;

  charset = charset_by_leading_byte (i0);
  if (XCHARSET_DIMENSION (charset) == 2)
    i2 = *++str & 0x7F;

  return make_ichar (charset, i1, i2);
}

/* Return whether CH is a valid Ichar, assuming it's non-ASCII.
   Do not call this directly.  Use the macro valid_ichar_p() instead. */

int
non_ascii_valid_ichar_p (Ichar ch)
{
  int f1, f2, f3;

  /* Must have only lowest 19 bits set */
  if (ch & ~0x7FFFF)
    return 0;

  f1 = ichar_field1 (ch);
  f2 = ichar_field2 (ch);
  f3 = ichar_field3 (ch);

  if (f1 == 0)
    {
      /* dimension-1 char */
      Lisp_Object charset;

      /* leading byte must be correct */
      if (f2 < MIN_ICHAR_FIELD2_OFFICIAL ||
	  (f2 > MAX_ICHAR_FIELD2_OFFICIAL && f2 < MIN_ICHAR_FIELD2_PRIVATE) ||
	   f2 > MAX_ICHAR_FIELD2_PRIVATE)
	return 0;
      /* octet not out of range */
      if (f3 < 0x20)
	return 0;
      /* charset exists */
      /*
	 NOTE: This takes advantage of the fact that
	 FIELD2_TO_OFFICIAL_LEADING_BYTE and
	 FIELD2_TO_PRIVATE_LEADING_BYTE are the same.
	 */
      charset = charset_by_leading_byte (f2 + FIELD2_TO_OFFICIAL_LEADING_BYTE);
      if (EQ (charset, Qnil))
	return 0;
      /* check range as per size (94 or 96) of charset */
      return ((f3 > 0x20 && f3 < 0x7f) || XCHARSET_CHARS (charset) == 96);
    }
  else
    {
      /* dimension-2 char */
      Lisp_Object charset;

      /* leading byte must be correct */
      if (f1 < MIN_ICHAR_FIELD1_OFFICIAL ||
	  (f1 > MAX_ICHAR_FIELD1_OFFICIAL && f1 < MIN_ICHAR_FIELD1_PRIVATE) ||
	  f1 > MAX_ICHAR_FIELD1_PRIVATE)
	return 0;
      /* octets not out of range */
      if (f2 < 0x20 || f3 < 0x20)
	return 0;

#ifdef ENABLE_COMPOSITE_CHARS
      if (f1 + FIELD1_TO_OFFICIAL_LEADING_BYTE == LEADING_BYTE_COMPOSITE)
	{
	  if (UNBOUNDP (Fgethash (make_int (ch),
				  Vcomposite_char_char2string_hash_table,
				  Qunbound)))
	    return 0;
	  return 1;
	}
#endif /* ENABLE_COMPOSITE_CHARS */

      /* charset exists */
      if (f1 <= MAX_ICHAR_FIELD1_OFFICIAL)
	charset =
	  charset_by_leading_byte (f1 + FIELD1_TO_OFFICIAL_LEADING_BYTE);
      else
	charset =
	  charset_by_leading_byte (f1 + FIELD1_TO_PRIVATE_LEADING_BYTE);

      if (EQ (charset, Qnil))
	return 0;
      /* check range as per size (94x94 or 96x96) of charset */
      return ((f2 != 0x20 && f2 != 0x7F && f3 != 0x20 && f3 != 0x7F) ||
	      XCHARSET_CHARS (charset) == 96);
    }
}

/* Copy the character pointed to by SRC into DST.  Do not call this
   directly.  Use the macro itext_copy_ichar() instead.
   Return the number of bytes copied.  */

Bytecount
non_ascii_itext_copy_ichar (const Ibyte *src, Ibyte *dst)
{
  Bytecount bytes = rep_bytes_by_first_byte (*src);
  Bytecount i;
  for (i = bytes; i; i--, dst++, src++)
    *dst = *src;
  return bytes;
}

#endif /* MULE */


/************************************************************************/
/*                        streams of Ichars                            */
/************************************************************************/

#ifdef MULE

/* Treat a stream as a stream of Ichar's rather than a stream of bytes.
   The functions below are not meant to be called directly; use
   the macros in insdel.h. */

Ichar
Lstream_get_ichar_1 (Lstream *stream, int ch)
{
  Ibyte str[MAX_ICHAR_LEN];
  Ibyte *strptr = str;
  Bytecount bytes;

  str[0] = (Ibyte) ch;

  for (bytes = rep_bytes_by_first_byte (ch) - 1; bytes; bytes--)
    {
      int c = Lstream_getc (stream);
      text_checking_assert (c >= 0);
      *++strptr = (Ibyte) c;
    }
  return itext_ichar (str);
}

int
Lstream_fput_ichar (Lstream *stream, Ichar ch)
{
  Ibyte str[MAX_ICHAR_LEN];
  Bytecount len = set_itext_ichar (str, ch);
  return Lstream_write (stream, str, len);
}

void
Lstream_funget_ichar (Lstream *stream, Ichar ch)
{
  Ibyte str[MAX_ICHAR_LEN];
  Bytecount len = set_itext_ichar (str, ch);
  Lstream_unread (stream, str, len);
}

#endif /* MULE */


/************************************************************************/
/*              Lisp primitives for working with characters             */
/************************************************************************/

DEFUN ("make-char", Fmake_char, 2, 3, 0, /*
Make a character from CHARSET and octets ARG1 and ARG2.
ARG2 is required only for characters from two-dimensional charsets.

Each octet should be in the range 32 through 127 for a 96 or 96x96
charset and 33 through 126 for a 94 or 94x94 charset. (Most charsets
are either 96 or 94x94.) Note that this is 32 more than the values
typically given for 94x94 charsets.  When two octets are required, the
order is "standard" -- the same as appears in ISO-2022 encodings,
reference tables, etc.

\(Note the following non-obvious result: Computerized translation
tables often encode the two octets as the high and low bytes,
respectively, of a hex short, while when there's only one octet, it
goes in the low byte.  When decoding such a value, you need to treat
the two cases differently when calling make-char: One is (make-char
CHARSET HIGH LOW), the other is (make-char CHARSET LOW).)

For example, (make-char 'latin-iso8859-2 185) or (make-char
'latin-iso8859-2 57) will return the Latin 2 character s with caron.

As another example, the Japanese character for "kawa" (stream), which
looks something like this:

   |     |
   |  |  |
   |  |  |
   |  |  |
  /      |

appears in the Unicode Standard (version 2.0) on page 7-287 with the
following values (see also page 7-4):

U 5DDD     (Unicode)
G 0-2008   (GB 2312-80)
J 0-3278   (JIS X 0208-1990)
K 0-8425   (KS C 5601-1987)
B A474     (Big Five)
C 1-4455   (CNS 11643-1986 (1st plane))
A 213C34   (ANSI Z39.64-1989)

These are equivalent to:

\(make-char 'chinese-gb2312 52 40)
\(make-char 'japanese-jisx0208 64 110)
\(make-char 'korean-ksc5601 116 57)
\(make-char 'chinese-cns11643-1 76 87)
\(decode-big5-char '(164 . 116))

\(All codes above are two decimal numbers except for Big Five and ANSI
Z39.64, which we don't support.  We add 32 to each of the decimal
numbers.  Big Five is split in a rather hackish fashion into two
charsets, `big5-1' and `big5-2', due to its excessive size -- 94x157,
with the first codepoint in the range 0xA1 to 0xFE and the second in
the range 0x40 to 0x7E or 0xA1 to 0xFE.  `decode-big5-char' is used to
generate the char from its codes, and `encode-big5-char' extracts the
codes.)

When compiled without MULE, this function does not do much, but it's
provided for compatibility.  In this case, the following CHARSET symbols
are allowed:

`ascii' -- ARG1 should be in the range 0 through 127.
`control-1' -- ARG1 should be in the range 128 through 159.
 else -- ARG1 is coerced to be between 0 and 255, and then the high
         bit is set.

 `int-to-char of the resulting ARG1' is returned, and ARG2 is always ignored. 
*/
       (charset, arg1, arg2))
{
#ifdef MULE
  Lisp_Charset *cs;
  int a1, a2;
  int lowlim, highlim;

  charset = Fget_charset (charset);
  cs = XCHARSET (charset);

  get_charset_limits (charset, &lowlim, &highlim);

  CHECK_INT (arg1);
  /* It is useful (and safe, according to Olivier Galibert) to strip
     the 8th bit off ARG1 and ARG2 because it allows programmers to
     write (make-char 'latin-iso8859-2 CODE) where code is the actual
     Latin 2 code of the character.  */
  a1 = XINT (arg1) & 0x7f;
  if (a1 < lowlim || a1 > highlim)
    args_out_of_range_3 (arg1, make_int (lowlim), make_int (highlim));

  if (CHARSET_DIMENSION (cs) == 1)
    {
      if (!NILP (arg2))
        invalid_argument
          ("Charset is of dimension one; second octet must be nil", arg2);
      return make_char (make_ichar (charset, a1, 0));
    }

  CHECK_INT (arg2);
  a2 = XINT (arg2) & 0x7f;
  if (a2 < lowlim || a2 > highlim)
    args_out_of_range_3 (arg2, make_int (lowlim), make_int (highlim));

  return make_char (make_ichar (charset, a1, a2));
#else
  int a1;
  int lowlim, highlim;

  if      (EQ (charset, Qascii))     lowlim =  0, highlim = 127;
  else if (EQ (charset, Qcontrol_1)) lowlim =  0, highlim =  31;
  else	                             lowlim =  0, highlim = 127;

  CHECK_INT (arg1);
  /* It is useful (and safe, according to Olivier Galibert) to strip
     the 8th bit off ARG1 and ARG2 because it allows programmers to
     write (make-char 'latin-iso8859-2 CODE) where code is the actual
     Latin 2 code of the character.  */
  a1 = XINT (arg1) & 0x7f;
  if (a1 < lowlim || a1 > highlim)
    args_out_of_range_3 (arg1, make_int (lowlim), make_int (highlim));

  if (EQ (charset, Qascii))
    return make_char (a1);
  return make_char (a1 + 128);
#endif /* MULE */
}

#ifdef MULE

DEFUN ("char-charset", Fchar_charset, 1, 1, 0, /*
Return the character set of char CH.
*/
       (ch))
{
  CHECK_CHAR_COERCE_INT (ch);

  return XCHARSET_NAME (charset_by_leading_byte
			(ichar_leading_byte (XCHAR (ch))));
}

DEFUN ("char-octet", Fchar_octet, 1, 2, 0, /*
Return the octet numbered N (should be 0 or 1) of char CH.
N defaults to 0 if omitted.
*/
       (ch, n))
{
  Lisp_Object charset;
  int octet0, octet1;

  CHECK_CHAR_COERCE_INT (ch);

  BREAKUP_ICHAR (XCHAR (ch), charset, octet0, octet1);

  if (NILP (n) || EQ (n, Qzero))
    return make_int (octet0);
  else if (EQ (n, make_int (1)))
    return make_int (octet1);
  else
    invalid_constant ("Octet number must be 0 or 1", n);
}

DEFUN ("split-char", Fsplit_char, 1, 1, 0, /*
Return list of charset and one or two position-codes of CHAR.
*/
       (character))
{
  /* This function can GC */
  struct gcpro gcpro1, gcpro2;
  Lisp_Object charset = Qnil;
  Lisp_Object rc = Qnil;
  int c1, c2;

  GCPRO2 (charset, rc);
  CHECK_CHAR_COERCE_INT (character);

  BREAKUP_ICHAR (XCHAR (character), charset, c1, c2);

  if (XCHARSET_DIMENSION (Fget_charset (charset)) == 2)
    {
      rc = list3 (XCHARSET_NAME (charset), make_int (c1), make_int (c2));
    }
  else
    {
      rc = list2 (XCHARSET_NAME (charset), make_int (c1));
    }
  UNGCPRO;

  return rc;
}

#endif /* MULE */


/************************************************************************/
/*                     composite character functions                    */
/************************************************************************/

#ifdef ENABLE_COMPOSITE_CHARS

Ichar
lookup_composite_char (Ibyte *str, int len)
{
  Lisp_Object lispstr = make_string (str, len);
  Lisp_Object ch = Fgethash (lispstr,
			     Vcomposite_char_string2char_hash_table,
			     Qunbound);
  Ichar emch;

  if (UNBOUNDP (ch))
    {
      if (composite_char_row_next >= 128)
	invalid_operation ("No more composite chars available", lispstr);
      emch = make_ichar (Vcharset_composite, composite_char_row_next,
			composite_char_col_next);
      Fputhash (make_char (emch), lispstr,
	        Vcomposite_char_char2string_hash_table);
      Fputhash (lispstr, make_char (emch),
		Vcomposite_char_string2char_hash_table);
      composite_char_col_next++;
      if (composite_char_col_next >= 128)
	{
	  composite_char_col_next = 32;
	  composite_char_row_next++;
	}
    }
  else
    emch = XCHAR (ch);
  return emch;
}

Lisp_Object
composite_char_string (Ichar ch)
{
  Lisp_Object str = Fgethash (make_char (ch),
			      Vcomposite_char_char2string_hash_table,
			      Qunbound);
  assert (!UNBOUNDP (str));
  return str;
}

DEFUN ("make-composite-char", Fmake_composite_char, 1, 1, 0, /*
Convert a string into a single composite character.
The character is the result of overstriking all the characters in
the string.
*/
       (string))
{
  CHECK_STRING (string);
  return make_char (lookup_composite_char (XSTRING_DATA (string),
					   XSTRING_LENGTH (string)));
}

DEFUN ("composite-char-string", Fcomposite_char_string, 1, 1, 0, /*
Return a string of the characters comprising a composite character.
*/
       (ch))
{
  Ichar emch;

  CHECK_CHAR (ch);
  emch = XCHAR (ch);
  if (ichar_leading_byte (emch) != LEADING_BYTE_COMPOSITE)
    invalid_argument ("Must be composite char", ch);
  return composite_char_string (emch);
}
#endif /* ENABLE_COMPOSITE_CHARS */


/************************************************************************/
/*                            initialization                            */
/************************************************************************/

void
reinit_eistring_early (void)
{
  the_eistring_malloc_zero_init = the_eistring_zero_init;
  the_eistring_malloc_zero_init.mallocp_ = 1;
}

void
init_eistring_once_early (void)
{
  reinit_eistring_early ();
}

void
syms_of_text (void)
{
  DEFSUBR (Fmake_char);

#ifdef MULE
  DEFSUBR (Fchar_charset);
  DEFSUBR (Fchar_octet);
  DEFSUBR (Fsplit_char);

#ifdef ENABLE_COMPOSITE_CHARS
  DEFSUBR (Fmake_composite_char);
  DEFSUBR (Fcomposite_char_string);
#endif
#endif /* MULE */
}

void
reinit_vars_of_text (void)
{
  int i;

  conversion_in_dynarr_list = Dynarr_new2 (Ibyte_dynarr_dynarr,
					   Ibyte_dynarr *);
  conversion_out_dynarr_list = Dynarr_new2 (Extbyte_dynarr_dynarr,
					    Extbyte_dynarr *);
  active_dfc_e2c = Dynarr_new (dfc_e2c_vals);

  for (i = 0; i <= MAX_BYTEBPOS_GAP_SIZE_3; i++)
    three_to_one_table[i] = i / 3;
}

void
vars_of_text (void)
{
  reinit_vars_of_text ();

  QSin_char_byte_conversion = build_msg_string ("(in char-byte conversion)");
  staticpro (&QSin_char_byte_conversion);
  QSin_internal_external_conversion =
    build_msg_string ("(in internal-external conversion)");
  staticpro (&QSin_internal_external_conversion);

#ifdef ENABLE_COMPOSITE_CHARS
  /* #### not dumped properly */
  composite_char_row_next = 32;
  composite_char_col_next = 32;

  Vcomposite_char_string2char_hash_table =
    make_lisp_hash_table (500, HASH_TABLE_NON_WEAK, HASH_TABLE_EQUAL);
  Vcomposite_char_char2string_hash_table =
    make_lisp_hash_table (500, HASH_TABLE_NON_WEAK, HASH_TABLE_EQ);
  staticpro (&Vcomposite_char_string2char_hash_table);
  staticpro (&Vcomposite_char_char2string_hash_table);
#endif /* ENABLE_COMPOSITE_CHARS */
}
