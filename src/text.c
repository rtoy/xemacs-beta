/* Text manipulation primitives for XEmacs.
   Copyright (C) 1995 Sun Microsystems, Inc.
   Copyright (C) 1995, 1996, 2000, 2001, 2002, 2003, 2004 Ben Wing.
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

/* NB: Everything below was written by Ben Wing except as otherwise noted. */

/************************************************************************/
/*                                                                      */
/*                                                                      */
/*               Part A: More carefully-written documentation           */
/*                                                                      */
/*                                                                      */
/************************************************************************/

/* Authorship: Ben Wing


   ==========================================================================
                         7. Handling non-default formats
   ==========================================================================

   We support, at least to some extent, formats other than the default
   variable-width format, for speed; all of these alternative formats are
   fixed-width.  Currently we only handle these non-default formats in
   buffers, because access to their text is strictly controlled and thus
   the details of the format mostly compartmentalized.  The only really
   tricky part is the search code -- the regex, Boyer-Moore, and
   simple-search algorithms in search.c and regex.c.  All other code that
   knows directly about the buffer representation is the basic code to
   modify or retrieve the buffer text.

   Supporting fixed-width formats in Lisp strings is harder, but possible
   -- FSF currently does this, for example.  In this case, however,
   probably only 8-bit-fixed is reasonable for Lisp strings -- getting
   non-ASCII-compatible fixed-width formats to work is much, much harder
   because a lot of code assumes that strings are ASCII-compatible
   (i.e. ASCII + other characters represented exclusively using high-bit
   bytes) and a lot of code mixes Lisp strings and non-Lisp strings freely.

   The different possible fixed-width formats are 8-bit fixed, 16-bit
   fixed, and 32-bit fixed.  The latter can represent all possible
   characters, but at a substantial memory penalty.  The other two can
   represent only a subset of the possible characters.  How these subsets
   are defined can be simple or very tricky.

   Currently we support only the default format and the 8-bit fixed format,
   and in the latter, we only allow these to be the first 256 characters in
   an Ichar (ASCII and Latin 1).
   
   One reasonable approach for 8-bit fixed is to allow the upper half to
   represent any 1-byte charset, which is specified on a per-buffer basis.
   This should work fairly well in practice since most documents are in
   only one foreign language (possibly with some English mixed in).  I
   think FSF does something like this; or at least, they have something
   called nonascii-translation-table and use it when converting from
   8-bit-fixed text ("unibyte text") to default text ("multibyte text").
   With 16-bit fixed, you could do something like assign chunks of the 64K
   worth of characters to charsets as they're encountered in documents.
   This should work well with most Asian documents.

   If/when we switch to using Unicode internally, we might have formats more
   like this:

   -- UTF-8 or some extension as the default format.  Perl uses an
   extension that handles 64-bit chars and requires as much as 13 bytes per
   char, vs. the standard of 31-bit chars and 6 bytes max.  UTF-8 has the
   same basic properties as our own variable-width format (see text.c,
   Internal String Encoding) and so most code would not need to be changed.

   -- UTF-16 as a "pseudo-fixed" format (i.e. 16-bit fixed plus surrogates
   for representing characters not in the BMP, aka >= 65536).  The vast
   majority of documents will have no surrogates in them so byte/char
   conversion will be very fast.

   -- an 8-bit fixed format, like currently.
   
   -- possibly, UCS-4 as a 32-bit fixed format.

   The fixed-width formats essentially treat the buffer as an array of
   8-bit, 16-bit or 32-bit integers.  This means that how they are stored
   in memory (in particular, big-endian or little-endian) depends on the
   native format of the machine's processor.  It also means we have to
   worry a bit about alignment (basically, we just need to keep the gap an
   integral size of the character size, and get things aligned properly
   when converting the buffer between formats).

   ==========================================================================
                     8. Using UTF-16 as the default text format
   ==========================================================================

   NOTE: The Eistring API is (or should be) Mule-correct even without
   an ASCII-compatible internal representation.

   #### Currently, the assumption that text units are one byte in size is
   embedded throughout XEmacs, and `Ibyte *' is used where `Itext *' should
   be.  The way to fix this is to (among other things)

   (a) review all places referencing `Ibyte' and `Ibyte *', change them to
       use Itext, and fix up the code.
   (b) change XSTRING_DATA to be of type Itext *
   (c) review all uses of XSTRING_DATA
   (d) eliminate XSTRING_LENGTH, splitting it into XSTRING_BYTE_LENGTH and
       XSTRING_TEXT_LENGTH and reviewing all places referencing this
   (e) make similar changes to other API's that refer to the "length" of
       something, such as qxestrlen() and eilen()
   (f) review all use of `CIbyte *'.  Currently this is usually a way of
       passing literal ASCII text strings in places that want internal text.
       Either create separate _ascii() and _itext() versions of the
       functions taking CIbyte *, or make use of something like the
       WEXTTEXT() macro, which will generate wide strings as appropriate.
   (g) review all uses of Bytecount and see which ones should be Textcount.
   (h) put in error-checking code that will be tripped as often as possible
       when doing anything with internal text, and check to see that ASCII
       text has not mistakenly filtered in.  This should be fairly easy as
       ASCII text will generally be entirely spaces and letters whereas every
       second byte of Unicode text will generally be a null byte.  Either we
       abort if the second bytes are entirely letters and numbers, or,
       perhaps better, do the equivalent of a non-MULE build, where we should
       be dealing entirely with 8-bit characters, and assert that the high
       bytes of each pair are null.
   (i) review places where xmalloc() is called.  If we convert each use of
       xmalloc() to instead be xnew_array() or some other typed routine,
       then we will find every place that allocates space for Itext and
       assumes it is based on one-byte units.
   (j) encourage the use of ITEXT_ZTERM_SIZE instead of '+ 1' whenever we
       are adding space for a zero-terminator, to emphasize what we are
       doing and make sure the calculations are correct.  Similarly for
       EXTTEXT_ZTERM_SIZE.
   (k) Note that the qxestr*() functions, among other things, will need to
       be rewritten.

   Note that this is a lot of work, and is not high on the list of priorities
   currently.

   ==========================================================================
                                9. Miscellaneous
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

      Composite characters could be represented as 0x8D C1 C2 C3, where each
      C[1-3] is in the range 0xA0 - 0xFF.  This allows for slightly under
      2^20 (one million) composite characters over the XEmacs process
      lifetime. Or you could use 0x8D C1 C2 C3 C4, allowing for about 85
      million (slightly over 2^26) composite characters.

   ==========================================================================
                               10. Internal API's
   ==========================================================================

   All of these are documented in more detail in text.h.

@enumerate
@item
Basic internal-format API's

These are simple functions and macros to convert between text
representation and characters, move forward and back in text, etc.

@item
The DFC API

This is for conversion between internal and external text.  Note that
there is also the "new DFC" API, which *returns* a pointer to the
converted text (in alloca space), rather than storing it into a
variable.

@item
The Eistring API

(This API is currently under-used) When doing simple things with
internal text, the basic internal-format API's are enough.  But to do
things like delete or replace a substring, concatenate various strings,
etc. is difficult to do cleanly because of the allocation issues.
The Eistring API is designed to deal with this, and provides a clean
way of modifying and building up internal text. (Note that the former
lack of this API has meant that some code uses Lisp strings to do
similar manipulations, resulting in excess garbage and increased
garbage collection.)

NOTE: The Eistring API is (or should be) Mule-correct even without
an ASCII-compatible internal representation.
@end enumerate

   ==========================================================================
                      11. Other Sources of Documentation
   ==========================================================================

   man/lispref/mule.texi
@enumerate
@item
another intro to characters, encodings, etc; #### Merge with the
above info
@item
documentation of ISO-2022
@item
The charset and coding-system Lisp API's
@item
The CCL conversion language for writing encoding conversions
@item
The Latin-Unity package for unifying Latin charsets
@end enumerate

   man/internals/internals.texi (the Internals manual)
@enumerate
@item
"Coding for Mule" -- how to write Mule-aware code
@item
"Modules for Internationalization"
@item
"The Text in a Buffer" -- more about the different ways of
viewing buffer positions; #### Merge with the above info
@item
"MULE Character Sets and Encodings" -- yet another intro
to characters, encodings, etc; #### Merge with the
above info; also some documentation of Japanese EUC and JIS7,
and CCL internals
@end enumerate

   text.h -- info about specific XEmacs-C API's for handling internal and
             external text

   intl-win32.c -- Windows-specific I18N information 

   lisp.h -- some info appears alongside the definitions of the basic
             character-related types

   unicode.c -- documentation about Unicode translation tables
*/


/************************************************************************/
/*                                                                      */
/*                                                                      */
/*               Part B: Random proposals for work to be done           */
/*                                                                      */
/*                                                                      */
/************************************************************************/


/*


   ==========================================================================
                   - Mule design issues (ben)
   ==========================================================================

circa 1999

Here is a more detailed list of Mule-related projects that we will be
working on.  They are more or less ordered according to how we will
proceed, but it's not exact.  In particular, there will probably be
time overlap among adjacent projects.

@enumerate
@item
Modify the internal/external conversion macros to allow for
MS Windows support.

@item
Modify the buffer macros to allow for more than one internal
representation, e.g. fixed width and variable width.

@item
Review the existing Mule code, especially the lisp code, for code
quality issues and improve the cleanliness of it.  Also work on
creating a specification for the Mule API.

@item
Write some more automated mule tests.

@item
Integrate Tomohiko's UTF-2000 code, fixing it up so that nothing is
broken when the UTF-2000 configure option is not enabled.

@item
Fix up the MS Windows code to be Mule-correct, so that you can
compile with Mule support under MS windows and have a working
XEmacs, at least just with Latin-1.

@item
Implement a scheme to guarantee no corruption of files, even with
an incorrect coding system - in particular, guarantee no corruption
of binary files.

@item
Make the text property support in XEmacs robust with respect to
string and text operations, so that the `no corruption' support in
the previous entry works properly, even if a lot of cutting and
pasting is done.

@item
Improve the handling of auto-detection so that, when there is any
possibility at all of mistake, the user is informed of the detected
encoding and given the choice of choosing other possibilities.

@item
Improve the support for different language environments in XEmacs,
for example, the priority of coding systems used in auto-detection
should properly reflect the language environment.  This probably
necessitates rethinking the current `coding system priority'
scheme.

@item
Do quality work to improve the existing UTF-2000 implementation.

@item
Implement preliminary support for 8-bit fixed width
representation.  First, we will only implement 7-bit support, and
will fall back to variable width as soon as any non-ASCII
character is encountered.  Then we will improve the support to
handle an arbitrary character set in the upper half of the 8-bit space.

@item
Investigate any remaining hurdles to making --with-mule be the
default configure option.
@end enumerate

   ==========================================================================
                   - Mule design issues (stephen)
   ==========================================================================

What I see as Mule priorities (in rough benefit order, I am not taking
account of difficulty, nor the fact that some - eg 8 & 10 - will
probably come as packages):

@enumerate
@item
Fix the autodetect problem (by making the coding priority list
user-configurable, as short as he likes, even null, with "binary"
as the default).
@item
Document the language environments and other Mule "APIs" as
implemented (since there is no real design spec).  Check to see 
how and where they are broken.
@item
Make the Mule menu useful to non-ISO-2022-literate folks.
@item
Redo the lstreams stuff to make it easy and robust to "pipeline",
eg, libz | gnupg | jis2mule.
@item
Make Custom Mule-aware.  (This probably depends on a sensible
fonts model.)
@item
Implement the "literal byte stream" memory feature.
@item
Study the FSF implementation of Mule for background for 7 & 8.
@item
Identify desirable Mule features (eg, i18n-ized messages as above, 
collating tables by language environment, etc).  (New features
might have priority as high as 9.)
@item
Specify Mule UIs, APIs, etc, and design and (re)implement them.
@item
Implement the 8-bit-wide buffer optimization.
@item
Move the internal encoding to UTF-32 (subject to Olivier's caveats 
regarding compose characters), with the variable-width char
buffers using UTF-8.
@item
Implement the 16- and 32-bit-wide buffer optimizations.
@end enumerate

   ==========================================================================
                   - Mule design issues "short term" (ben)
   ==========================================================================

@enumerate
@item
Finish changes in fixup/directory, get in CVS.

(Test with and without "quick-build", to see if really faster)
(need autoconf)

@item
Finish up Windows/Mule changes.  Outline of this elsewhere;  Do
*minimal* effort.

@item
Continue work on Windows stability, e.g. go through existing notes
on Windows Mule-ization + extract all info.

@item
Get Unicode translation tables integrated.

Finish UCS2/UTF16 coding system.

@item
Make sure coding system priority list is language-environment specific.

@item
Consider moving language selection Menu up to be parallel with Mule menu.

@item
Check to make sure we grok the default locale at startup under
Windows and understand the Windows locales.  Finish implementation
of mswindows-multibyte and make sure it groks all the locales.

@item
Do the above as best as we can without using Unicode tables.

@item
Start tagging all text with a language text property,
indicating the current language environment when the text was input.

@item
Make sure we correctly accept input of non-ASCII chars
(probably already do!)

@item
Implement active language/keyboard switching under Windows.

@item
Look into implementing support for "MS IME" protocol (Microsoft
fancy built-in Asian input methods).

@item
Redo implementation of mswindows-multibyte and internal display to
entirely use translation to/from Unicode for increased accuracy.

@item
Implement buf<->char improvements from FSF.  Also implement
my string byte<->char optimization structure.

@item
Integrate all Mule DOCS from 20.6 or 21.0.  Try to add sections
for what we've added.

@item
Implement 8-bit fixed width optimizations.  Then work on 16-bit.
@end enumerate

   ==========================================================================
                   - Mule design issues (more) (ben)
   ==========================================================================

   Get minimal Mule for Windows working using Ikeyama's patches.  At
   first, rely on his conversion of internal -> external
   locale-specific but very soon (as soon as we get translation
   tables) can switch to using Unicode versions of display funs, which
   will allow many more charsets to be handled and in a more
   consistent fashion.

   i.e. to convert an internal string to an external format, at first
   we use our own knowledge of the Microsoft locale file formats but
   an alternative is to convert to Unicode and use Microsoft's
   convert-Unicode-to-locale encoding functions.  This gains us a
   great deal of generality, since in practice all charset caching
   points can be wrapped into Unicode caching points.

   This requires adding UCS2 support, which I'm doing.  This support
   would let us convert internal -> Unicode, which is exactly what we
   want.

   At first, though, I would do the UCS2 support, but leave the
   existing way of doing things in redisplay.  Meanwhile, I'd go
   through and fix up the places in the code that assume we are
   dealing with unibytes.

   After this, the font problems will be fixed , we should have a
   pretty well working XEmacs + MULE under Windows.  The only real
   other work is the clipboard code, which should be straightforward.

   ==========================================================================
                   - Mule design discussion
   ==========================================================================

--------------------------------------------------------------------------

Ben

April 11, 2000

Well yes, this was the whole point of my "no lossage" proposal of being
able to undo any coding-system transformation on a buffer.  The idea was
to figure out which transformations were definitely reversable, and for
all the others, cache the original text in a text property.  This way, you
could probably still do a fairly good job at constructing a good reversal
even after you've gone into the text and added, deleted, and rearranged
some things.

But you could implement it much more simply and usefully by just
determining, for any text being decoded into mule-internal, can we go back
and read the source again?  If not, remember the entire file (GNUS
message, etc) in text properties.  Then, implement the UI interface (like
Netscape's) on top of that.  This way, you have something that at least
works, but it might be inefficient.  All we would need to do is work on
making the
underlying implementation more efficient.

Are you interested in doing this?  It would be a huge win for users.
Hrvoje Niksic wrote:

> Ben Wing <ben@666.com> writes:
>
> > let me know exactly what "rethink" functionality you want and i'll
> > come up with an interface.  perhaps you just want something like
> > netscape's encoding menu, where if you switch encodings, it reloads
> > and reencodes?
>
> It might be a bit more complex than that.  In many cases, it's hard or
> impossible to meaningfully "reload" -- for instance, this
> functionality should be available while editing a Gnus message, as
> well as while visiting a file.
>
> For the special case of Latin-N <-> Latin-M conversion, things could
> be done easily -- to convert from N to M, you only need to convert
> internal representation back to N, and then convert it forth to M.

--------------------------------------------------------------------------
April 11, 2000

Well yes, this was the whole point of my "no lossage" proposal of being
able to undo any coding-system transformation on a buffer.  The idea was
to figure out which transformations were definitely reversable, and for
all the others, cache the original text in a text property.  This way, you
could probably still do a fairly good job at constructing a good reversal
even after you've gone into the text and added, deleted, and rearranged
some things.

But you could implement it much more simply and usefully by just
determining, for any text being decoded into mule-internal, can we go back
and read the source again?  If not, remember the entire file (GNUS
message, etc) in text properties.  Then, implement the UI interface (like
Netscape's) on top of that.  This way, you have something that at least
works, but it might be inefficient.  All we would need to do is work on
making the
underlying implementation more efficient.

Are you interested in doing this?  It would be a huge win for users.
Hrvoje Niksic wrote:

> Ben Wing <ben@666.com> writes:
>
> > let me know exactly what "rethink" functionality you want and i'll
> > come up with an interface.  perhaps you just want something like
> > netscape's encoding menu, where if you switch encodings, it reloads
> > and reencodes?
>
> It might be a bit more complex than that.  In many cases, it's hard or
> impossible to meaningfully "reload" -- for instance, this
> functionality should be available while editing a Gnus message, as
> well as while visiting a file.
>
> For the special case of Latin-N <-> Latin-M conversion, things could
> be done easily -- to convert from N to M, you only need to convert
> internal representation back to N, and then convert it forth to M.


------------------------------------------------------------------------

   ==========================================================================
   - Redoing translation macros [old]
   ==========================================================================

  Currently the translation macros (the macros with names such as
  GET_C_STRING_CTEXT_DATA_ALLOCA) have names that are difficult to parse
  or remember, and are not all that general.  In the process of
  reviewing the Windows code so that it could be muleized, I discovered
  that these macros need to be extended in various ways to allow for
  the Windows code to be easily muleized.
  
  Since the macros needed to be changed anyways, I figured it would be a
  good time to redo them properly.  I propose new macros which have
  names like this:
  
  @itemize @bullet
  @item
  <A>_TO_EXTERNAL_FORMAT_<B>
  @item
  <A>_TO_EXTERNAL_FORMAT_<B>_1
  @item
  <C>_TO_INTERNAL_FORMAT_<D>
  @item
  <C>_TO_INTERNAL_FORMAT_<D>_1
  @end itemize
  
  A and C represent the source of the data, and B and D represent the
  sink of the data.
  
  All of these macros call either the functions
  convert_to_external_format or convert_to_internal_format internally,
  with some massaging of the arguments.
  
  All of these macros take the following arguments:
  
  @itemize @bullet
  @item
  First, one or two arguments indicating the source of the data.
  @item
  Second, an argument indicating the coding system. (In order to avoid
  an excessive number of macros, we no longer provide separate macros
  for specific coding systems.)
  @item
  Third, one or two arguments indicating the sink of the data.
  @item
  Fourth, optionally, arguments indicating the error behavior and the
  warning class (these arguments are only present in the _1 versions
  of the macros).  The other, shorter named macros are trivial
  interfaces onto these macros with the error behavior being
  ERROR_ME_WARN, with the warning class being Vstandard_warning_class.
  @end itemize
  
  <A> can be one of the following:
  @itemize @bullet
  @item
  LISP (which means a Lisp string) Takes one argument, a Lisp Object.
  @item
  LSTREAM (which indicates an lstream) Takes one argument, an
  lstream.  The data is read from the lstream until EOF is reached.
  @item
  DATA (which indicates a raw memory area) Takes two arguments, a
  pointer and a length in bytes.
  (You must never use this if the source of the data is a Lisp string,
  because of the possibility of relocation during garbage collection.)
  @end itemize
  
  <B> can be one of the following:
  @itemize @bullet
  @item
  ALLOCA (which means that the resulting data is stored in alloca()ed
  memory.  Two arguments should be specified, a pointer and a length,
  which should be lvalues.)
  @item
  MALLOC (which means that the resulting data is stored in malloc()ed
  memory.  Two arguments should be specified, a pointer and a
  length.  The memory must be free()d by the caller.
  @item
  OPAQUE (which means the resulting data is stored in an opaque Lisp
  Object.  This takes one argument, a lvalue Lisp Object.
  @item
  LSTREAM. The data is written to an lstream.
  @end itemize
  
  <C> can be one of the :
  @itemize @bullet
  @item
  DATA
  @item
  LSTREAM
  @end itemize
  (just like <A> above)
  
  <D> can be one of
  @itemize @bullet
  @item
  ALLOCA
  @item
  MALLOC
  @item
  LISP This means a Lisp String.
  @item
  BUFFER The resulting data is inserted into a buffer at the buffer's
  value of point.
  @item
  LSTREAM The data is written to the lstream.
  @end itemize
  
  
  Note that I have eliminated the FORMAT argument of previous macros,
  and replaced it with a coding system.  This was made possible by
  coding system aliases.  In place of old `format's, we use a `virtual
  coding system', which is aliased to the actual coding system.
  
  The value of the coding system argument can be anything that is legal
  input to get_coding_system, i.e. a symbol or a coding system object.

   ==========================================================================
   - creation of generic macros for accessing internally formatted data [old]
   ==========================================================================

 I have a design; it's all written down (I did it in Tsukuba), and I just have
 to have it transcribed.  It's higher level than the macros, though; it's Lisp
 primitives that I'm designing.
 
 As for the design of the macros, don't worry so much about all files having to
 get included (which is inevitable with macros), but about how the files are
 separated.  Your design might go like this:
 
 @enumerate
 @item
 you have generic macro interfaces, which specify a particular
 behavior but not an implementation.  these generic macros have
 complementary versions for buffers and for strings (and the buffer
 or string is an argument to all of the macros), and do such things
 as convert between byte and char indices, retrieve the character at
 a particular byte or char index, increment or decrement a byte
 index to the beginning of the next or previous character, indicate
 the number of bytes occupied by the character at a particular byte
 or character index, etc.  These are similar to what's already out
 there except that they confound buffers and strings and that they
 can also work with actual char *'s, which I think is a really bad
 idea because it encourages code to "assume" that the representation
 is ASCII compatible, which is might not be (e.g. 16-bit fixed
 width).  In fact, one thing I'm planning on doing is redefining
 Bufbyte as a struct, for debugging purposes, to catch all places
 that cavalierly compare them with ASCII char's.  Note also that I
 really want to rename Bufpos and Bytind, which are confusing and
 wrong in that they also apply to strings. They should be Bytepos
 and Charpos, or something like that, to go along with Bytecount and
 Charcount. Similarly, Bufbyte is similarly a misnomer and should be
 Intbyte -- a byte in the internal string representation (any of the
 internal representations) of a string or buffer.  Corresponding to
 this is Extbyte (which we already have), a byte in any external
 string representation.  We also have Extcount, which makes sense,
 and we might possibly want Extcharcount, the number of characters
 in an external string representation; but that gets sticky in modal
 encodings, and it's not clear how useful it would be.
 
 @item
 for all generic macro interfaces, there are specific versions of
 each of them for each possible representation (pure ASCII in the
 non-Mule world, Mule standard, UTF-8, 8-bit fixed, 16-bit fixed,
 32-bit fixed, etc.; there may well be more than one possible 16-bit
 fixed version, as well). Each representation has a corresponding
 prefix, e.g. MULE_ or FIXED16_ or whatever, which is prefixed onto
 the generic macro names.  The resulting macros perform the
 operation defined for the macro, but assume, and only work
 correctly with, text in the corresponding representation.
 
 @item
 The definition of the generic versions merely conditionalizes on
 the appropriate things (i.e. bit flags in the buffer or string
 object) and calls the appropriate representation-specific version.
 There may be more than one definition (protected by ifdefs, of
 course), or one definition that amalgamated out of many ifdef'ed
 sections.
 
 @item
 You should probably put each different representation in its own
 header file, e.g. charset-mule.h or charset-fixed16.h or
 charset-ascii.h or whatever.  Then put the main macros into
 charset.h, and conditionalize in this file appropriately to include
 the other ones.  That way, code that actually needs to play around
 with internal-format text at this level can include "charset.h"
 (certainly a much better place than buffer.h), and everyone else
 uses higher-level routines.  The representation-specific macros
 should not normally be used *directly* at all; they are invoked
 automatically from the generic macros.  However, code that needs to
 be highly, highly optimized might choose to take a loop and write
 two versions of it, one for each representation, to avoid the
 per-loop-iteration cost of a comparison. Until the macro interface
 is rock stable and solid, we should strongly discourage such
 nanosecond optimizations.
 @end enumerate
 
   ==========================================================================
                   - UTF-16 compatible representation
   ==========================================================================

NOTE: One possible default internal representation that was compatible
with UTF16 but allowed all possible chars in UCS4 would be to take a
more-or-less unused range of 2048 chars (not from the private area
because Microsoft actually uses up most or all of it with EUDC chars).
Let's say we picked A400 - ABFF.  Then, we'd have:

0000 - FFFF    Simple chars

D[8-B]xx D[C-F]xx  Surrogate char, represents 1M chars

A[4-B]xx D[C-F]xx D[C-F]xx   Surrogate char, represents 2G chars

This is exactly the same number of chars as UCS-4 handles, and it follows the
same property as UTF8 and Mule-internal:

@enumerate
@item
There are two disjoint groupings of units, one representing leading units
and one representing non-leading units.
@item
Given a leading unit, you immediately know how many units follow to make
up a valid char, irrespective of any other context.
@end enumerate

Note that A4xx is actually currently assigned to Yi.  Since this is an
internal representation, we could just move these elsewhere.

An alternative is to pick two disjoint ranges, e.g. 2D00 - 2DFF and
A500 - ABFF.

   ==========================================================================
                        New API for char->font mapping
   ==========================================================================
- ; supersedes charset-registry and CCL;
  supports all windows systems; powerful enough for Unicode; etc.

  (charset-font-mapping charset)

font-mapping-specifier  string

char-font-mapping-table

  char-table, specifier; elements of char table are either strings (which
  specify a registry or comparable font property, or vectors of a string
  (same) followed by keyword-value pairs (optional).  The only allowable
  keyword currently is :ccl-program, which specifies a CCL program to map
  the characters into font indices.  Other keywords may be added
  e.g. allowing Elisp fragments instead of CCL programs, also allowed is
  [inherit], which inherits from the next less-specific char-table in the
  specifier.

  The preferred interface onto this mapping (which should be portable
  across Emacsen) is

  (set-char-font-mapping key value &optional locale tag-set how-to-add)

  where key is a char, range or charset (as for put-char-table), value is
  as above, and the other arguments are standard for specifiers.  This
  automatically creates a char table in the locale, as necessary (all
  elements default to [inherit]).  On GNU Emacs, some specifiers arguments
  may be unimplemented.

 (char-font-mapping key value &optional locale)
works vaguely like get-specifier?   But does inheritance processing.
locale should clearly default here to current-buffer

#### should get-specifier as well?  Would make it work most like
#### buffer-local variables.

NB.  set-charset-registry and set-charset-ccl-program are obsoleted.

   ==========================================================================
                 Implementing fixed-width 8,16,32 bit buffer optimizations
   ==========================================================================

Add set-buffer-optimization (buffer &rest keywords) for
controlling these things.

Also, put in hack so that correct arglist can be retrieved by
Lisp code.

Look at the way keyword primitives are currently handled; make
sure it works and is documented, etc.

Implement 8-bit fixed width optimization.  Take the things that
know about the actual implementation and put them in a single
file, in essence creating an abstraction layer to allow
pluggable internal representations.  Implement a fairly general
scheme for mapping between character codes in the 8 bits or 16
bits representation and on actual charset characters.  As part of
set-buffer-optimization, you can specify a list of character sets
to be used in the 8 bit to 16 bit, etc. world.  You can also
request that the buffer be in 8, 16, etc. if possible.

-> set defaults wrt this.
-> perhaps this should be just buffer properties.
-> this brings up the idea of default properties on an object.
-> Implement default-put, default-get, etc.

What happens when a character not assigned in the range gets
added?  Then, must convert to variable width of some sort.

Note: at first, possibly we just convert whole hog to get things
right.  Then we'd have to poy alternative to characters that got
added + deleted that were unassigned in the fixed width.  When
this goes to zero and there's been enough time (heuristics), we
go back to fixed.

Side note:  We could dynamically build up the set of assigned
chars as they go.  Conceivably this could even go down to the
single char level: Just keep a big array of mapping from 16 bit
values to chars, and add empty time, a char has been encountered
that wasn't there before.  Problem need inverse mapping.

-> Possibility; chars are actual objects, not just numbers.
Then you could keep track of such info in the chars itself.
*Think about this.*

Eventually, we might consider allowing mixed fixed-width,
variable-width buffer encodings.  Then, we use range tables to
indicate which sections are fixed and which variable and INC_CHAR does
something like this: binary search to find the current range, which
indicates whether it's fixed or variable, and tells us what the
increment is.  We can cache this info and use it next time to speed
up.

-> We will then have two partially shared range tables - one for
overall fixed width vs. variable width, and possibly one containing
this same info, but partitioning the variable width in one.  Maybe
need fancier nested range table model.

   ==========================================================================
        Expansion of display table and case mapping table support for all
                           chars, not just ASCII/Latin1.
   ==========================================================================

   ==========================================================================
       Improved flexibility for display tables, and evaluation of its
      features to make sure it meshes with and complements the char<->font
                       mapping API mentioned earlier
   ==========================================================================

   ==========================================================================
                              String access speedup:
   ==========================================================================

  For strings larger than some size in bytes (10?), keep extra fields of
  info: length in chars, and a (char, byte) pair in the middle to speed
  up sequential access.
  
  (Better idea: do this for any size string, but only if it contains
  non-ASCII chars.  Then if info is missing, we know string is
  ASCII-only.)
  
  Use a string-extra-info object, replacing string property slot and
  containing fields for string mod tick, string extents, string props,
  and string char length, and cached (char,byte) pair.
  string-extra-info (or string-auxiliary?) objects could be in frob
  blocks, esp. if creating frob blocks is easy + worth it.

- Caching of char<->byte conversions in strings - should make nearly
  all operations on strings O(N)

   ==========================================================================
                    Improvements in buffer char<->byte mapping
   ==========================================================================

  - Range table implementation - especially when there are few runs of
    different widths, e.g. recently converted from fixed-width
    optimization to variable width

  Range Tables to speed up Bufpos <-> Bytind caching
  ==================================================
  
  This describes an alternative implementation using ranges.  We
  maintain a range table of all spans of characters of a fixed width.
  Updating this table could take time if there are a large number of
  spans; but constant factors of operations should be quick.  This method really wins
  when you have 8-bit buffers just converted to variable width, where
  there will be few spans.  More specifically, lookup in this range
  table is O(log N) and can be done with simple binary search, which is
  very fast.  If we maintain the ranges using a gap array, updating this
  table will be fast for local operations, which is most of the time.
  
  We will also provide (at first, at least) a Lisp function to set the
  caching mechanism explicitly - either range tables or the existing
  implementation.  Eventually, we want to improve things, to the point
  where we automatically pick the right caching for the situation and
  have more caching schemes implemented.

   ==========================================================================
                        - Robustify Text Properties
   ==========================================================================

   ==========================================================================
           Support for unified internal representation, e.g. Unicode
   ==========================================================================

   Start tagging all text with a language text property,
   indicating the current language environment when the text was input.
   (needs "Robustify Text Properties")

   ==========================================================================
                          - Generalized Coding Systems
   ==========================================================================

  - Lisp API for Defining Coding Systems

  User-defined coding systems.
  
  (define-coding-system-type 'type
    :encode-function fun
    :decode-function fun
    :detect-function fun
    :buffering (number = at least this many chars
                line   = buffer up to end of line
                regexp = buffer until this regexp is found in match
                source data.  match data will be appropriate when fun is
                called
  
  encode fun is called as
  
  (encode instream outstream)
  
  should read data from instream and write converted result onto
  outstream.  Can leave some data stuff in stream, it will reappear
  next time.  Generally, there is a finite amount of data in instream
  and further attempts to read lead to would-block errors or retvals.
  Can use instream properties to record state.  May use read-stream
  functionality to read everything into a vector or string.
  
  ->Need vectors + string exposed to resizing of Lisp implementation
    where necessary.
  
   ==========================================================================
     Support Windows Active Kbd Switching, Far East IME API (done already?)
   ==========================================================================

   ==========================================================================
              - UI/design changes for Coding System Pipelining
   ==========================================================================

  ------------------------------------------------------------------
                            CODING-SYSTEM CHAINS
  ------------------------------------------------------------------

  sjt sez:

  There should be no elementary coding systems in the Lisp API, only
  chains.  Chains should be declared, not computed, as a sequence of coding
  formats.  (Probably the internal representation can be a vector for
  efficiency but programmers would probably rather work with lists.)  A
  stream has a token type.  Most streams are octet streams.  Text is a
  stream of characters (in _internal_ format; a file on disk is not text!)
  An octet-stream has no implicit semantics, so its format must always be
  specified.  The only type currently having semantics is characters.  This
  means that the chain [euc-jp -> internal -> shift_jis) may be specified
  (euc-jp, shift_jis), and if no euc-jp -> shift_jis converter is
  available, then the chain is automatically constructed.  (N.B.  I f we
  have fixed width buffers in the future, then we could have ASCII -> 8-bit
  char -> 16-bit char -> ISO-2022-JP (with escape sequences).

  EOL handling is a char <-> char coding.  It should not be part of another
  coding system except as a convenience for users.  For text coding,
  automatically insert EOL handlers between char <-> octet boundaries.

  ------------------------------------------------------------------
                            ABOUT DETECTION
  ------------------------------------------------------------------


  ------------------------------------------------------------------
     EFFICIENCY OF CODING CONVERSION WITH MULTIPLE COPIES/CHAINS
  ------------------------------------------------------------------

   A comment in encode_decode_coding_region():

   The chain of streams looks like this:

     [BUFFER] <----- (( read from/send to loop ))
                     ------> [CHAR->BYTE i.e. ENCODE AS BINARY if source is
                              in bytes]
		             ------> [ENCODE/DECODE AS SPECIFIED]
			             ------> [BYTE->CHAR i.e. DECODE AS BINARY
                                              if sink is in bytes]
					     ------> [AUTODETECT EOL if
					              we're decoding and
						      coding system calls
						      for this]
			                              ------> [BUFFER]

    sjt (?) responds:

     Of course, this is just horrible.  BYTE<->CHAR should only be available
     to I/O routines.  It should not be visible to Mule proper.

     A comment on the implementation.  Hrvoje and Kyle worry about the
     inefficiency of repeated copying among buffers that chained coding
     systems entail.  But this may not be as time inefficient as it appears
     in the Mule ("house rules") context.  The issue is how do you do chain
     coding systems without copying?  In theory you could have

     IChar external_to_raw (ExtChar *cp, State *s);
     IChar decode_utf16 (IChar c, State *s);
     IChar decode_crlf (ExtChar *cp, State *s);

     typedef Ichar (*Converter[]) (Ichar, State*);

     Converter utf16[2] = { &decode_utf16, &decode_crlf };

     void convert (ExtChar *inbuf, IChar *outbuf, Converter cvtr)
     {
       int i;
       ExtChar c;
       State s;

       while (c = external_to_raw (*inbuf++, &s))
	 {
	   for (i = 0; i < sizeof(cvtr)/sizeof(Converter); ++i)
	     if (s.ready)
	       c = (*cvtr[i]) (c, &s);
	 }
       if (s.ready)
         *outbuf++ = c;
     }

     But this is a lot of function calls; what Ben is doing is basically
     reducing this to one call per buffer-full.  The only way to avoid this
     is to hardcode all the "interesting" coding systems, maybe using
     inline or macros to give structure.  But this is still a huge amount
     of work, and code.

     One advantage to the call-per-char approach is that we might be able
     to do something about the marker/extent destruction that coding
     normally entails.

    ben sez:

     it should be possible to preserve the markers/extents without
     switching completely to one-call-per-char -- we could at least do one
     call per "run", where a run is more or less the maximal stretch of
     text not overlapping any markers or extent boundaries. (It's a bit
     more complicated if we want to properly support the different extent
     begins/ends; in some cases we might have to pump a single character
     adjacent to where two extents meet.) The "stateless" way that I wrote
     all of the conversion routines may be a real hassle but it allows
     something like this to work without too much problem -- pump in one
     run at a time into one end of the chain, do a flush after each
     iteration, and stick what comes out the other end in its place.

  ------------------------------------------------------------------
                              ABOUT FORMATS
  ------------------------------------------------------------------
  
  when calling make-coding-system, the name can be a cons of (format1 .
  format2), specifying that it decodes format1->format2 and encodes the other
  way.  if only one name is given, that is assumed to be format1, and the
  other is either `external' or `internal' depending on the end type.
  normally the user when decoding gives the decoding order in formats, but
  can leave off the last one, `internal', which is assumed.  a multichain
  might look like gzip|multibyte|unicode, using the coding systems named
  `gzip', `(unicode . multibyte)' and `unicode'.  the way this actually works
  is by searching for gzip->multibyte; if not found, look for gzip->external
  or gzip->internal. (In general we automatically do conversion between
  internal and external as necessary: thus gzip|crlf does the expected, and
  maps to gzip->external, external->internal, crlf->internal, which when
  fully specified would be gzip|external:external|internal:crlf|internal --
  see below.)  To forcibly fit together two converters that have explicitly
  specified and incompatible names (say you have unicode->multibyte and
  iso8859-1->ebcdic and you know that the multibyte and iso8859-1 in this
  case are compatible), you can force-cast using :, like this:
  ebcdic|iso8859-1:multibyte|unicode. (again, if you force-cast between
  internal and external formats, the conversion happens automatically.)
  
  --------------------------------------------------------------------------
  ABOUT PDUMP, UNICODE, AND RUNNING XEMACS FROM A DIRECTORY WITH WEIRD CHARS
  --------------------------------------------------------------------------

-- there's the problem that XEmacs can't be run in a directory with
   non-ASCII/Latin-1 chars in it, since it will be doing Unicode
   processing before we've had a chance to load the tables.  In fact,
   even finding the tables in such a situation is problematic using
   the normal commands.  my idea is to eventually load the stuff
   extremely extremely early, at the same time as the pdump data gets
   loaded.  in fact, the unicode table data (stored in an efficient
   binary format) can even be stuck into the pdump file (which would
   mean as a resource to the executable, for windows).  we'd need to
   extend pdump a bit: to allow for attaching extra data to the pdump
   file. (something like pdump_attach_extra_data (addr, length)
   returns a number of some sort, an index into the file, which you
   can then retrieve with pdump_load_extra_data(), which returns an
   addr (mmap()ed or loaded), and later you pdump_unload_extra_data()
   when finished.  we'd probably also need
   pdump_attach_extra_data_append(), which appends data to the data
   just written out with pdump_attach_extra_data().  this way,
   multiple tables in memory can be written out into one contiguous
   table. (we'd use the tar-like trick of allowing new blocks to be
   written without going back to change the old blocks -- we just rely
   on the end of file/end of memory.) this same mechanism could be
   extracted out of pdump and used to handle the non-pdump situation
   (or alternatively, we could just dump either the memory image of
   the tables themselves or the compressed binary version).  in the
   case of extra unicode tables not known about at compile time that
   get loaded before dumping, we either just dump them into the image
   (pdump and all) or extract them into the compressed binary format,
   free the original tables, and treat them like all other tables.


   ==========================================================================
        - Generalized language appropriate word wrapping (requires
                 layout-exposing API defined in BIDI section)
   ==========================================================================

   ==========================================================================
                            - Make Custom Mule-aware
   ==========================================================================

   ==========================================================================
                         - Composite character support
   ==========================================================================

   ==========================================================================
                 - Language appropriate sorting and searching
   ==========================================================================

   ==========================================================================
                    - Glyph shaping for Arabic and Devanagari
   ==========================================================================

-  (needs to be handled mostly
  at C level, as part of layout; luckily it's entirely local in its
  changes, as this is not hard)


   ==========================================================================
    Consider moving language selection Menu up to be parallel with Mule menu
   ==========================================================================

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
  retval = vsprintf ((Chbyte *) buffer, format, args);
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
ascii_strcasecmp (const Ascbyte *s1, const Ascbyte *s2)
{
  return qxestrcasecmp ((const Ibyte *) s1, (const Ibyte *) s2);
}

int
qxestrcasecmp_ascii (const Ibyte *s1, const Ascbyte *s2)
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
ascii_strncasecmp (const Ascbyte *s1, const Ascbyte *s2, Bytecount len)
{
  return qxestrncasecmp ((const Ibyte *) s1, (const Ibyte *) s2, len);
}

int
qxestrncasecmp_ascii (const Ibyte *s1, const Ascbyte *s2, Bytecount len)
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

/* Compare a wide string with an ASCII string */

int
wcscmp_ascii (const wchar_t *s1, const Ascbyte *s2)
{
  while (*s1 && *s2)
    {
      if (*s1 != (wchar_t) *s2)
       break;
      s1++, s2++;
    }

  return *s1 - *s2;
}

int
wcsncmp_ascii (const wchar_t *s1, const Ascbyte *s2, Charcount len)
{
  while (len--)
    {
      int diff = *s1 - *s2;
      if (diff != 0)
	return diff;
      if (!*s1)
	return 0;
      s1++, s2++;
    }

  return 0;
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
  Ibyte *str = alloca_ibytes (nels * MAX_ICHAR_LEN + 1);
  Ibyte *strorig = str;
  Bytecount len;

  int i;

  for (i = 0; i < nels; i++)
    str += set_itext_ichar (str, arr[i]);
  *str = '\0';
  len = str - strorig;
  str = xnew_ibytes (1 + len);
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
		(void) set_itext_ichar_fmt (dstp, ch, dstfmt, dstobj);	 \
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
			   Lisp_Object USED_IF_MULE (srcobj),
			   Ibyte *dst, Bytecount dstlen,
			   Internal_Format dstfmt,
			   Lisp_Object USED_IF_MULE (dstobj),
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
find_charsets_in_ibyte_string (unsigned char *charsets,
			       const Ibyte *USED_IF_MULE (str),
			       Bytecount USED_IF_MULE (len))
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
find_charsets_in_ichar_string (unsigned char *charsets,
			       const Ichar *USED_IF_MULE (str),
			       Charcount USED_IF_MULE (len))
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

/* A couple of these functions should only be called on a non-Mule build. */
#ifdef MULE
#define ASSERT_BUILT_WITH_MULE() assert(1)
#else /* MULE */
#define ASSERT_BUILT_WITH_MULE() assert(0)
#endif /* MULE */

int
ibyte_string_displayed_columns (const Ibyte *str, Bytecount len)
{
  int cols = 0;
  const Ibyte *end = str + len;
  Ichar ch;

  ASSERT_BUILT_WITH_MULE();

  while (str < end)
    {
      ch = itext_ichar (str);
      cols += XCHARSET_COLUMNS (ichar_charset (ch));
      INC_IBYTEPTR (str);
    }

  return cols;
}

int
ichar_string_displayed_columns (const Ichar * USED_IF_MULE(str), Charcount len)
{
  int cols = 0;
  int i;

  ASSERT_BUILT_WITH_MULE();

  for (i = 0; i < len; i++)
    cols += XCHARSET_COLUMNS (ichar_charset (str[i]));

  return cols;
}

Charcount
ibyte_string_nonascii_chars (const Ibyte *USED_IF_MULE (str),
			     Bytecount USED_IF_MULE (len))
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
      newdata = xnew_ibytes (ei->max_size_allocated_);
      memcpy (newdata, ei->data_, ei->bytelen_ + 1);
      ei->data_ = newdata;
    }

  if (ei->extdata_)
    {
      Extbyte *newdata = xnew_extbytes (ei->extlen_ + 2);

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
	 const Eistring *ei2, int is_ascii, int fold_case)
{
  assert ((data == 0) != (ei == 0)); 
  assert ((is_ascii != 0) == (data != 0));
  assert (fold_case >= 0 && fold_case <= 2);
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

    if (is_ascii)
      ASSERT_ASCTEXT_ASCII_LEN ((Ascbyte *) dst, dstlen);

    return (fold_case == 0 ? qxememcmp4 (src, len, dst, dstlen) :
	    fold_case == 1 ? qxememcasecmp4 (src, len, dst, dstlen) :
	    qxetextcasecmp (src, len, dst, dstlen));
  }
}

Ibyte *
eicpyout_malloc_fmt (Eistring *eistr, Bytecount *len_out, Internal_Format fmt,
		     Lisp_Object UNUSED (object))
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

/* Skip as many ASCII bytes as possible in the memory block [PTR, END).
   Return pointer to the first non-ASCII byte.  optimized for long
   stretches of ASCII. */
inline static const Ibyte *
skip_ascii (const Ibyte *ptr, const Ibyte *end)
{
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

/* Skip as many ASCII bytes as possible in the memory block [END, PTR),
   going downwards.  Return pointer to the location above the first
   non-ASCII byte.  Optimized for long stretches of ASCII. */
inline static const Ibyte *
skip_ascii_down (const Ibyte *ptr, const Ibyte *end)
{
  const unsigned STRIDE_TYPE *ascii_end;

  /* Need to do in 3 sections -- before alignment start, aligned chunk,
     after alignment end. */
  while (!ALIGNED (ptr))
    {
      if (ptr == end || !byte_ascii_p (*(ptr - 1)))
	return ptr;
      ptr--;
    }
  ascii_end = (const unsigned STRIDE_TYPE *) ptr - 1;
  /* This loop screams, because we can detect ASCII
     characters 4 or 8 at a time. */
  while ((const Ibyte *) ascii_end >= end
	 && !(*ascii_end & HIGH_BIT_MASK))
    ascii_end--;
  ptr = (Ibyte *) (ascii_end + 1);
  while (ptr > end && byte_ascii_p (*(ptr - 1)))
    ptr--;
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

/* Function equivalent of charcount_to_bytecount_down.  This works on strings
   of all sizes but is more efficient than a simple loop on large strings
   and probably less efficient on sufficiently small strings. */

Bytecount
charcount_to_bytecount_down_fun (const Ibyte *ptr, Charcount len)
{
  const Ibyte *newptr = ptr;
  while (1)
    {
      const Ibyte *newnewptr = skip_ascii_down (newptr, newptr - len);
      len -= newptr - newnewptr;
      newptr = newnewptr;
      /* Skip over all non-ASCII chars, counting the length and
	 stopping if it's zero */
      while (len && !byte_ascii_p (*(newptr - 1)))
	if (ibyte_first_byte_p (*--newptr))
	  len--;
      if (!len)
	break;
    }
  text_checking_assert (ptr - newptr >= 0);
  return ptr - newptr;
}

/* The next two functions are the actual meat behind the
   charbpos-to-bytebpos and bytebpos-to-charbpos conversions.  Currently
   the method they use is fairly unsophisticated; see buffer.h.

   Note that charbpos_to_bytebpos_func() is probably the most-called
   function in all of XEmacs.  Therefore, it must be FAST FAST FAST.
   This is the reason why so much of the code is duplicated.

   Similar considerations apply to bytebpos_to_charbpos_func(), although
   less so because the function is not called so often.
 */

/*

Info on Byte-Char conversion:

  (Info-goto-node "(internals)Byte-Char Position Conversion")
*/

#ifdef OLD_BYTE_CHAR
static int not_very_random_number;
#endif /* OLD_BYTE_CHAR */

#define OLD_LOOP

/* If we are this many characters away from any known position, cache the
   new position in the buffer's char-byte cache. */
#define FAR_AWAY_DISTANCE 5000

/* Converting between character positions and byte positions.  */

/* There are several places in the buffer where we know
   the correspondence: BEG, BEGV, PT, GPT, ZV and Z,
   and everywhere there is a marker.  So we find the one of these places
   that is closest to the specified position, and scan from there.  */

/* This macro is a subroutine of charbpos_to_bytebpos_func.
   Note that it is desirable that BYTEPOS is not evaluated
   except when we really want its value.  */

#define CONSIDER(CHARPOS, BYTEPOS)					\
do									\
{									\
  Charbpos this_charpos = (CHARPOS);					\
  int changed = 0;							\
									\
  if (this_charpos == x)						\
    {									\
      retval = (BYTEPOS);						\
      goto done;							\
    }									\
  else if (this_charpos > x)						\
    {									\
      if (this_charpos < best_above)					\
	{								\
	  best_above = this_charpos;					\
	  best_above_byte = (BYTEPOS);					\
	  changed = 1;							\
	}								\
    }									\
  else if (this_charpos > best_below)					\
    {									\
      best_below = this_charpos;					\
      best_below_byte = (BYTEPOS);					\
      changed = 1;							\
    }									\
									\
  if (changed)								\
    {									\
      if (best_above - best_below == best_above_byte - best_below_byte)	\
        {								\
	  retval = best_below_byte + (x - best_below);			\
          goto done;							\
	}								\
    }									\
}									\
while (0)


Bytebpos
charbpos_to_bytebpos_func (struct buffer *buf, Charbpos x)
{
#ifdef OLD_BYTE_CHAR
  Charbpos bufmin;
  Charbpos bufmax;
  Bytebpos bytmin;
  Bytebpos bytmax;
  int size;
  int forward_p;
  int diff_so_far;
  int add_to_cache = 0;
#endif /* OLD_BYTE_CHAR */

  Charbpos best_above, best_below;
  Bytebpos best_above_byte, best_below_byte;
  int i;
  struct buffer_text *t;
  Bytebpos retval;

  PROFILE_DECLARE ();

  PROFILE_RECORD_ENTERING_SECTION (QSin_char_byte_conversion);

  best_above = BUF_Z (buf);
  best_above_byte = BYTE_BUF_Z (buf);

  /* In this case, we simply have all one-byte characters.  But this should
     have been intercepted before, in charbpos_to_bytebpos(). */
  text_checking_assert (best_above != best_above_byte);

  best_below = BUF_BEG (buf);
  best_below_byte = BYTE_BUF_BEG (buf);

  /* We find in best_above and best_above_byte
     the closest known point above CHARPOS,
     and in best_below and best_below_byte
     the closest known point below CHARPOS,
     
     If at any point we can tell that the space between those
     two best approximations is all single-byte,
     we interpolate the result immediately.  */

  CONSIDER (BUF_PT (buf), BYTE_BUF_PT (buf));
  CONSIDER (BUF_GPT (buf), BYTE_BUF_GPT (buf));
  CONSIDER (BUF_BEGV (buf), BYTE_BUF_BEGV (buf));
  CONSIDER (BUF_ZV (buf), BYTE_BUF_ZV (buf));

  t = buf->text;
  CONSIDER (t->cached_charpos, t->cached_bytepos);

  /* Check the most recently entered positions first */

  for (i = t->next_cache_pos - 1; i >= 0; i--)
    {
      CONSIDER (t->mule_charbpos_cache[i], t->mule_bytebpos_cache[i]);

      /* If we are down to a range of 50 chars,
	 don't bother checking any other markers;
	 scan the intervening chars directly now.  */
      if (best_above - best_below < 50)
	break;
    }

  /* We get here if we did not exactly hit one of the known places.
     We have one known above and one known below.
     Scan, counting characters, from whichever one is closer.  */

  if (x - best_below < best_above - x)
    {
      int record = x - best_below > FAR_AWAY_DISTANCE;

#ifdef OLD_LOOP /* old code */
      while (best_below != x)
	{
	  best_below++;
	  INC_BYTEBPOS (buf, best_below_byte);
	}
#else
      text_checking_assert (BUF_FORMAT (buf) == FORMAT_DEFAULT);
      /* The gap should not occur between best_below and x, or we will be
	 screwed in using charcount_to_bytecount().  It should not be exactly
	 at x either, because we already should have caught that. */
      text_checking_assert
	(BUF_CEILING_OF_IGNORE_ACCESSIBLE (buf, best_below) > x);

      /* Using charcount_to_bytecount() is potentially a lot faster than a
	 simple loop using INC_BYTEBPOS() because (a) the checks for gap
	 and buffer format are factored out instead of getting checked
	 every time; (b) the checking goes 4 or 8 bytes at a time in ASCII
	 text.
      */
      best_below_byte +=
	charcount_to_bytecount
	(BYTE_BUF_BYTE_ADDRESS (buf, best_below_byte), x - best_below);
      best_below = x;
#endif /* 0 */

      /* If this position is quite far from the nearest known position,
	 cache the correspondence.

	 NB FSF does this: "... by creating a marker here.
	 It will last until the next GC."
      */

      if (record)
	{
	  /* If we have run out of positions to record, discard some of the
	     old ones.  I used to use a circular buffer, which avoids the
	     need to block-move any memory.  But it makes it more difficult
	     to keep track of which positions haven't been used -- commonly
	     we haven't yet filled out anywhere near the whole set of
	     positions and don't want to check them all.  We should not be
	     recording that often, and block-moving is extremely fast in
	     any case. --ben */
	  if (t->next_cache_pos == NUM_CACHED_POSITIONS)
	    {
	      memmove (t->mule_charbpos_cache,
		       t->mule_charbpos_cache + NUM_MOVED_POSITIONS,
		       sizeof (Charbpos) *
		       (NUM_CACHED_POSITIONS - NUM_MOVED_POSITIONS));
	      memmove (t->mule_bytebpos_cache,
		       t->mule_bytebpos_cache + NUM_MOVED_POSITIONS,
		       sizeof (Bytebpos) *
		       (NUM_CACHED_POSITIONS - NUM_MOVED_POSITIONS));
	      t->next_cache_pos -= NUM_MOVED_POSITIONS;
	    }
	  t->mule_charbpos_cache[t->next_cache_pos] = best_below;
	  t->mule_bytebpos_cache[t->next_cache_pos] = best_below_byte;
	  t->next_cache_pos++;
	}

      t->cached_charpos = best_below;
      t->cached_bytepos = best_below_byte;

      retval = best_below_byte;
      text_checking_assert (best_below_byte >= best_below);
      goto done;
    }
  else
    {
      int record = best_above - x > FAR_AWAY_DISTANCE;

#ifdef OLD_LOOP
      while (best_above != x)
	{
	  best_above--;
	  DEC_BYTEBPOS (buf, best_above_byte);
	}
#else
      text_checking_assert (BUF_FORMAT (buf) == FORMAT_DEFAULT);
      /* The gap should not occur between best_above and x, or we will be
	 screwed in using charcount_to_bytecount_down().  It should not be
	 exactly at x either, because we already should have caught
	 that. */
      text_checking_assert
	(BUF_FLOOR_OF_IGNORE_ACCESSIBLE (buf, best_above) < x);

      /* Using charcount_to_bytecount_down() is potentially a lot faster
	 than a simple loop using DEC_BYTEBPOS(); see above. */
      best_above_byte -=
	charcount_to_bytecount_down
	/* BYTE_BUF_BYTE_ADDRESS will return a value on the high side of the
	   gap if we are at the gap, which is the wrong side.  So do the
	   following trick instead. */
	(BYTE_BUF_BYTE_ADDRESS_BEFORE (buf, best_above_byte) + 1,
	 best_above - x);
      best_above = x;
#endif /* SLEDGEHAMMER_CHECK_TEXT */


      /* If this position is quite far from the nearest known position,
	 cache the correspondence.

	 NB FSF does this: "... by creating a marker here.
	 It will last until the next GC."
      */
      if (record)
	{
	  if (t->next_cache_pos == NUM_CACHED_POSITIONS)
	    {
	      memmove (t->mule_charbpos_cache,
		       t->mule_charbpos_cache + NUM_MOVED_POSITIONS,
		       sizeof (Charbpos) *
		       (NUM_CACHED_POSITIONS - NUM_MOVED_POSITIONS));
	      memmove (t->mule_bytebpos_cache,
		       t->mule_bytebpos_cache + NUM_MOVED_POSITIONS,
		       sizeof (Bytebpos) *
		       (NUM_CACHED_POSITIONS - NUM_MOVED_POSITIONS));
	      t->next_cache_pos -= NUM_MOVED_POSITIONS;
	    }
	  t->mule_charbpos_cache[t->next_cache_pos] = best_above;
	  t->mule_bytebpos_cache[t->next_cache_pos] = best_above_byte;
	  t->next_cache_pos++;
	}

      t->cached_charpos = best_above;
      t->cached_bytepos = best_above_byte;

      retval = best_above_byte;
      text_checking_assert (best_above_byte >= best_above);
      goto done;
    }

#ifdef OLD_BYTE_CHAR

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
    ABORT ();
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
      for (i = 0; i < NUM_CACHED_POSITIONS; i++)
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

#endif /* OLD_BYTE_CHAR */

done:
  PROFILE_RECORD_EXITING_SECTION (QSin_char_byte_conversion);

  return retval;
}

#undef CONSIDER

/* bytepos_to_charpos returns the char position corresponding to BYTEPOS.  */

/* This macro is a subroutine of bytebpos_to_charbpos_func.
   It is used when BYTEPOS is actually the byte position.  */

#define CONSIDER(BYTEPOS, CHARPOS)					\
do									\
{									\
  Bytebpos this_bytepos = (BYTEPOS);					\
  int changed = 0;							\
									\
  if (this_bytepos == x)						\
    {									\
      retval = (CHARPOS);						\
      goto done;							\
    }									\
  else if (this_bytepos > x)						\
    {									\
      if (this_bytepos < best_above_byte)				\
	{								\
	  best_above = (CHARPOS);					\
	  best_above_byte = this_bytepos;				\
	  changed = 1;							\
	}								\
    }									\
  else if (this_bytepos > best_below_byte)				\
    {									\
      best_below = (CHARPOS);						\
      best_below_byte = this_bytepos;					\
      changed = 1;							\
    }									\
									\
  if (changed)								\
    {									\
      if (best_above - best_below == best_above_byte - best_below_byte)	\
	{								\
	  retval = best_below + (x - best_below_byte);			\
	  goto done;							\
	}								\
    }									\
}									\
while (0)

/* The logic in this function is almost identical to the logic in
   the previous function. */

Charbpos
bytebpos_to_charbpos_func (struct buffer *buf, Bytebpos x)
{
#ifdef OLD_BYTE_CHAR
  Charbpos bufmin;
  Charbpos bufmax;
  Bytebpos bytmin;
  Bytebpos bytmax;
  int size;
  int forward_p;
  int diff_so_far;
  int add_to_cache = 0;
#endif /* OLD_BYTE_CHAR */

  Charbpos best_above, best_above_byte;
  Bytebpos best_below, best_below_byte;
  int i;
  struct buffer_text *t;
  Charbpos retval;

  PROFILE_DECLARE ();

  PROFILE_RECORD_ENTERING_SECTION (QSin_char_byte_conversion);

  best_above = BUF_Z (buf);
  best_above_byte = BYTE_BUF_Z (buf);

  /* In this case, we simply have all one-byte characters.  But this should
     have been intercepted before, in bytebpos_to_charbpos(). */
  text_checking_assert (best_above != best_above_byte);

  best_below = BUF_BEG (buf);
  best_below_byte = BYTE_BUF_BEG (buf);

  CONSIDER (BYTE_BUF_PT (buf), BUF_PT (buf));
  CONSIDER (BYTE_BUF_GPT (buf), BUF_GPT (buf));
  CONSIDER (BYTE_BUF_BEGV (buf), BUF_BEGV (buf));
  CONSIDER (BYTE_BUF_ZV (buf), BUF_ZV (buf));

  t = buf->text;
  CONSIDER (t->cached_bytepos, t->cached_charpos);

  /* Check the most recently entered positions first */

  for (i = t->next_cache_pos - 1; i >= 0; i--)
    {
      CONSIDER (t->mule_bytebpos_cache[i], t->mule_charbpos_cache[i]);

      /* If we are down to a range of 50 chars,
	 don't bother checking any other markers;
	 scan the intervening chars directly now.  */
      if (best_above - best_below < 50)
	break;
    }

  /* We get here if we did not exactly hit one of the known places.
     We have one known above and one known below.
     Scan, counting characters, from whichever one is closer.  */

  if (x - best_below_byte < best_above_byte - x)
    {
      int record = x - best_below_byte > 5000;

#ifdef OLD_LOOP /* old code */
      while (best_below_byte < x)
	{
	  best_below++;
	  INC_BYTEBPOS (buf, best_below_byte);
	}
#else
      text_checking_assert (BUF_FORMAT (buf) == FORMAT_DEFAULT);
      /* The gap should not occur between best_below and x, or we will be
	 screwed in using charcount_to_bytecount().  It should not be exactly
	 at x either, because we already should have caught that. */
      text_checking_assert
	(BYTE_BUF_CEILING_OF_IGNORE_ACCESSIBLE (buf, best_below_byte) > x);

      /* Using bytecount_to_charcount() is potentially a lot faster than
	 a simple loop above using INC_BYTEBPOS(); see above.
      */
      best_below +=
	bytecount_to_charcount
	(BYTE_BUF_BYTE_ADDRESS (buf, best_below_byte), x - best_below_byte);
      best_below_byte = x;
#endif

      /* If this position is quite far from the nearest known position,
	 cache the correspondence.

	 NB FSF does this: "... by creating a marker here.
	 It will last until the next GC."
      */

      if (record)
	{
	  if (t->next_cache_pos == NUM_CACHED_POSITIONS)
	    {
	      memmove (t->mule_charbpos_cache,
		       t->mule_charbpos_cache + NUM_MOVED_POSITIONS,
		       sizeof (Charbpos) *
		       (NUM_CACHED_POSITIONS - NUM_MOVED_POSITIONS));
	      memmove (t->mule_bytebpos_cache,
		       t->mule_bytebpos_cache + NUM_MOVED_POSITIONS,
		       sizeof (Bytebpos) *
		       (NUM_CACHED_POSITIONS - NUM_MOVED_POSITIONS));
	      t->next_cache_pos -= NUM_MOVED_POSITIONS;
	    }
	  t->mule_charbpos_cache[t->next_cache_pos] = best_below;
	  t->mule_bytebpos_cache[t->next_cache_pos] = best_below_byte;
	  t->next_cache_pos++;
	}


      t->cached_charpos = best_below;
      t->cached_bytepos = best_below_byte;

      retval = best_below;
      text_checking_assert (best_below_byte >= best_below);
      goto done;
    }
  else
    {
      int record = best_above_byte - x > 5000;

#ifdef OLD_LOOP /* old code */
      while (best_above_byte > x)
	{
	  best_above--;
	  DEC_BYTEBPOS (buf, best_above_byte);
	}
#else
      text_checking_assert (BUF_FORMAT (buf) == FORMAT_DEFAULT);
      /* The gap should not occur between best_above and x, or we will be
	 screwed in using bytecount_to_charcount_down().  It should not be
	 exactly at x either, because we already should have caught
	 that. */
      text_checking_assert
	(BYTE_BUF_FLOOR_OF_IGNORE_ACCESSIBLE (buf, best_above_byte) < x);

      /* Using bytecount_to_charcount_down() is potentially a lot faster
	 than a simple loop using INC_BYTEBPOS(); see above. */
      best_above -=
	bytecount_to_charcount_down
	/* BYTE_BUF_BYTE_ADDRESS will return a value on the high side of the
	   gap if we are at the gap, which is the wrong side.  So do the
	   following trick instead. */
	(BYTE_BUF_BYTE_ADDRESS_BEFORE (buf, best_above_byte) + 1,
	best_above_byte - x);
      best_above_byte = x;
#endif


      /* If this position is quite far from the nearest known position,
	 cache the correspondence.

	 NB FSF does this: "... by creating a marker here.
	 It will last until the next GC."
      */
      if (record)
	{
	  if (t->next_cache_pos == NUM_CACHED_POSITIONS)
	    {
	      memmove (t->mule_charbpos_cache,
		       t->mule_charbpos_cache + NUM_MOVED_POSITIONS,
		       sizeof (Charbpos) *
		       (NUM_CACHED_POSITIONS - NUM_MOVED_POSITIONS));
	      memmove (t->mule_bytebpos_cache,
		       t->mule_bytebpos_cache + NUM_MOVED_POSITIONS,
		       sizeof (Bytebpos) *
		       (NUM_CACHED_POSITIONS - NUM_MOVED_POSITIONS));
	      t->next_cache_pos -= NUM_MOVED_POSITIONS;
	    }
	  t->mule_charbpos_cache[t->next_cache_pos] = best_above;
	  t->mule_bytebpos_cache[t->next_cache_pos] = best_above_byte;
	  t->next_cache_pos++;
	}

      t->cached_charpos = best_above;
      t->cached_bytepos = best_above_byte;

      retval = best_above;
      text_checking_assert (best_above_byte >= best_above);
      goto done;
    }

#ifdef OLD_BYTE_CHAR

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
    ABORT ();
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
      for (i = 0; i < NUM_CACHED_POSITIONS; i++)
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
#endif /* OLD_BYTE_CHAR */

done:
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
#ifdef OLD_BYTE_CHAR
  int size = (1 << buf->text->mule_shifter) + !!buf->text->mule_three_p;
#endif /* OLD_BYTE_CHAR */
  int i;

  /* Adjust the cache of known positions. */
  for (i = 0; i < buf->text->next_cache_pos; i++)
    {

      if (buf->text->mule_charbpos_cache[i] > start)
	{
	  buf->text->mule_charbpos_cache[i] += charlength;
	  buf->text->mule_bytebpos_cache[i] += bytelength;
	}
    }

  /* Adjust the special cached position. */

  if (buf->text->cached_charpos > start)
    {
      buf->text->cached_charpos += charlength;
      buf->text->cached_bytepos += bytelength;
    }

#ifdef OLD_BYTE_CHAR
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
#endif /* OLD_BYTE_CHAR */
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
  for (i = 0; i < buf->text->next_cache_pos; i++)
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

  /* Adjust the special cached position. */

  /* After the end; gets shoved backward */
  if (buf->text->cached_charpos > end)
    {
      buf->text->cached_charpos -= end - start;
      buf->text->cached_bytepos -= byte_end - byte_start;
    }
  /* In the range; moves to start of range */
  else if (buf->text->cached_charpos > start)
    {
      buf->text->cached_charpos = start;
      buf->text->cached_bytepos = byte_start;
    }

#ifdef OLD_BYTE_CHAR
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
#endif /* OLD_BYTE_CHAR */
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

  assert (!inhibit_non_essential_conversion_operations);
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
  Lisp_Object underlying_cs;
  PROFILE_DECLARE ();

  assert (!inhibit_non_essential_conversion_operations);
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

  /* The second call does the equivalent of both calls, but we need
     the result after the first call (which wraps just a to-text
     converter) as well as the result after the second call (which
     also wraps an EOL-detection converter). */
  underlying_cs = get_coding_system_for_text_file (coding_system, 0);
  coding_system = get_coding_system_for_text_file (underlying_cs, 1);

  if (source_type != DFC_TYPE_LISP_LSTREAM &&
      sink_type   != DFC_TYPE_LISP_LSTREAM &&
      coding_system_is_binary (underlying_cs))
    {
#ifdef MULE
      const Ibyte *ptr;
      Bytecount len = source->data.len;
      const Ibyte *end;

      /* Make sure no EOL conversion is needed.  With a little work we
	 could handle EOL conversion as well but it may not be needed as an
	 optimization. */
      if (!EQ (coding_system, underlying_cs))
	{
	  for (ptr = (const Ibyte *) source->data.ptr, end = ptr + len;
	       ptr < end; ptr++)
	    {
	      if (*ptr == '\r' || *ptr == '\n')
		goto the_hard_way;
	    }
	}

      for (ptr = (const Ibyte *) source->data.ptr, end = ptr + len;
	   ptr < end; ptr++)
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
	   dfc_coding_system_is_unicode (underlying_cs))
    {
      const Ibyte *ptr;
      Bytecount len = source->data.len;
      const Ibyte *end;

      if (len & 1)
	goto the_hard_way;

      /* Make sure only ASCII/Latin-1 is involved */
      for (ptr = (const Ibyte *) source->data.ptr + 1, end = ptr + len;
	   ptr < end; ptr += 2)
	{
	  if (*ptr)
	    goto the_hard_way;
	}

      /* Make sure no EOL conversion is needed.  With a little work we
	 could handle EOL conversion as well but it may not be needed as an
	 optimization. */
      if (!EQ (coding_system, underlying_cs))
	{
	  for (ptr = (const Ibyte *) source->data.ptr, end = ptr + len;
	       ptr < end; ptr += 2)
	    {
	      if (*ptr == '\r' || *ptr == '\n')
		goto the_hard_way;
	    }
	}

      for (ptr = (const Ibyte *) source->data.ptr, end = ptr + len;
	   ptr < end; ptr += 2)
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

#if defined (WIN32_ANY) || defined (MULE)
    the_hard_way:
#endif
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
/*                         Alloca-conversion helpers                       */
/* ----------------------------------------------------------------------- */

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
   will lose, but at least we can check for this and ABORT().  We could
   conceivably try to index on other parameters as well, but there is not
   really any point. */

alloca_convert_vals_dynarr *active_alloca_convert;

int
find_pos_of_existing_active_alloca_convert (const char *srctext)
{
  alloca_convert_vals *vals = NULL;
  int i;

  if (!active_alloca_convert)
    active_alloca_convert = Dynarr_new (alloca_convert_vals);

  for (i = 0; i < Dynarr_length (active_alloca_convert); i++)
    {
      vals = Dynarr_atp (active_alloca_convert, i);
      /* On my system, two different occurrences of the same stringized
	 argument always point to the same string.  However, on someone
	 else's system, that wasn't the case.  We check for equality
	 first, since it seems systems work my way more than the other
	 way. */
      if (vals->srctext == srctext || !strcmp (vals->srctext, srctext))
	return i;
    }

  return -1;
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
      ABORT ();
    }

  /* The size is always + 2 because we have double zero-termination at the
     end of all data (for Unicode-correctness). */
  *dst_size += 2;
}

Bytecount
new_dfc_convert_size (const char *srctext, const void *src,
		      Bytecount src_size, enum new_dfc_src_type type,
		      Lisp_Object codesys)
{
  alloca_convert_vals vals;

  int i = find_pos_of_existing_active_alloca_convert (srctext);
  assert (i < 0);

  vals.srctext = srctext;

  new_dfc_convert_now_damn_it (src, src_size, type, &vals.dst, &vals.dst_size,
			       codesys);

  Dynarr_add (active_alloca_convert, vals);
  return vals.dst_size;
}

void *
new_dfc_convert_copy_data (const char *srctext, void *alloca_data)
{
  alloca_convert_vals *vals;
  int i = find_pos_of_existing_active_alloca_convert (srctext);

  assert (i >= 0);
  vals = Dynarr_atp (active_alloca_convert, i);
  assert (alloca_data);
  memcpy (alloca_data, vals->dst, vals->dst_size);
  xfree (vals->dst, void *);
  Dynarr_delete (active_alloca_convert, i);
  return alloca_data;
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

  /* Must have only lowest 21 bits set */
  if (ch & ~0x1FFFFF)
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
       (charset, arg1, USED_IF_MULE (arg2)))
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

#endif /* MULE */

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

  if (XCHARSET_DIMENSION (charset) == 2)
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
  DEFSUBR (Fsplit_char);

#ifdef MULE
  DEFSUBR (Fchar_charset);
  DEFSUBR (Fchar_octet);

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

  for (i = 0; i <= MAX_BYTEBPOS_GAP_SIZE_3; i++)
    three_to_one_table[i] = i / 3;
}

void
vars_of_text (void)
{
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
