/* C support for testing XEmacs - see tests/automated/c-tests.el
   Copyright (C) 2000 Martin Buchholz
   Copyright (C) 2001, 2002, 2010 Ben Wing.
   Copyright (C) 2006 The Free Software Foundation, Inc.

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

/* Author: Martin Buchholz

   This file provides support for running tests for XEmacs that cannot
   be written entirely in Lisp.  These tests are run automatically via
   tests/automated/c-tests.el, or can be run by hand using M-x */


#include <config.h>
#include "lisp.h"
#include "buffer.h"
#include "lstream.h"
#include "elhash.h"
#include "opaque.h"
#include "file-coding.h"	/* XCODING_SYSTEM_EOL_TYPE and its values */

static Lisp_Object Vtest_function_list;

DEFUN ("test-data-format-conversion", Ftest_data_format_conversion, 0, 0, "", /*
  Return list of results of test TO_EXTERNAL_FORMAT() and TO_INTERNAL_FORMAT().
For use by the automated test suite.  See tests/automated/c-tests.

Each element is a list (DESCRIPTION, STATUS, REASON).
DESCRIPTION is a string describing the test.
STATUS is a symbol, either t (pass) or nil (fail).
REASON is nil or a string describing the failure (not required).
*/
       ())
{
  void *ptr; Bytecount len;
  Lisp_Object string, opaque, conversion_result = Qnil;

  Ibyte int_foo[] = "\n\nfoo\nbar";
  Extbyte ext_unix[]= "\n\nfoo\nbar";

  Extbyte ext_dos[] = "\r\n\r\nfoo\r\nbar";
  Extbyte ext_mac[] = "\r\rfoo\rbar";
  Lisp_Object opaque_dos = make_opaque (ext_dos, sizeof (ext_dos) - 1);
  Lisp_Object string_foo = make_string (int_foo, sizeof (int_foo) - 1);

  Extbyte ext_latin[]  = "f\372b\343\340";
#ifdef UNICODE_INTERNAL
  /* Internal encoding differs between Unicode-internal (UTF-8) and
     old-Mule */
  Ibyte int_latin1[] = "f\303\272b\303\243\303\240";
  Ibyte int_latin2[] = "f\303\272b\304\203\305\225";
#else /* not UNICODE_INTERNAL */
  Ibyte int_latin1[] = "f\201\372b\201\343\201\340";
  Ibyte int_latin2[] = "f\202\372b\202\343\202\340";
#endif /* (not) UNICODE_INTERNAL */
#ifdef MULE
#ifdef UNICODE_INTERNAL
  /* We also have differences when rendering Latin-1 text in Latin-2 or
     vice-versa.  Unicode-internal unifies Latin-1 \372 (#xFA) and Latin-2
     \372 (#xFA) because they both correspond to the same Unicode value
     (\372 (#xFA), u with an acute accent) */
  Extbyte ext_latin12[]= "f\372b\033-A\343\340\033-B";
  Extbyte ext_untranslatable[]  = "f\372b??";
#else
  Extbyte ext_latin12[]= "f\033-A\372b\343\340\033-B";
  Extbyte ext_untranslatable[]  = "f?b??";
#endif
  Lisp_Object string_latin2 = make_string (int_latin2, sizeof (int_latin2) - 1);
#endif
  Lisp_Object opaque_latin  = make_opaque (ext_latin,  sizeof (ext_latin) - 1);
  Lisp_Object opaque0_latin = make_opaque (ext_latin,  sizeof (ext_latin));
  Lisp_Object string_latin1 = make_string (int_latin1, sizeof (int_latin1) - 1);
  int autodetect_eol_p =
    !NILP (Fsymbol_value (intern ("eol-detection-enabled-p")));

#if defined (MULE) && !defined (UNICODE_INTERNAL)
  /* Check to make sure no one changed the internal charset ID's on us */
  assert (XINT (Fcharset_id (Vcharset_latin_iso8859_1)) == int_latin1[1]);
  assert (XINT (Fcharset_id (Vcharset_latin_iso8859_2)) == int_latin2[1]);
#endif

  /* Check for expected strings before and after conversion.
     Conversions depend on whether MULE is defined. */

  /* #### Any code below that uses iso-latin-2-with-esc is ill-conceived. */

#ifdef MULE
#define DFC_CHECK_DATA_COND_MULE(ptr,len,			\
				 constant_string_mule,		\
				 constant_string_non_mule,	\
				 description)			\
    DFC_CHECK_DATA (ptr, len, constant_string_mule, description)
#define DFC_CHECK_DATA_COND_MULE_NUL(ptr,len,			\
				     constant_string_mule,	\
				     constant_string_non_mule,	\
				     description)		\
    DFC_CHECK_DATA_NUL (ptr, len, constant_string_mule, description)
#else
#define DFC_CHECK_DATA_COND_MULE(ptr,len,			\
				 constant_string_mule,		\
				 constant_string_non_mule,	\
				 description)			\
    DFC_CHECK_DATA (ptr, len, constant_string_non_mule, description)
#define DFC_CHECK_DATA_COND_MULE_NUL(ptr,len,			\
				     constant_string_mule,	\
				     constant_string_non_mule,	\
				     description)		\
    DFC_CHECK_DATA_NUL (ptr, len, constant_string_non_mule, description)
#endif

  /* These now only apply to base coding systems, and
     need to test `eol-detection-enabled-p' at runtime. */
#define DFC_CHECK_DATA_COND_EOL(ptr,len,				\
				constant_string_eol,			\
				constant_string_non_eol,		\
				description) do {			\
    if (autodetect_eol_p)						\
      DFC_CHECK_DATA (ptr, len, constant_string_eol, description);	\
    else								\
      DFC_CHECK_DATA (ptr, len, constant_string_non_eol, description);	\
  } while (0)
#define DFC_CHECK_DATA_COND_EOL_NUL(ptr,len,				\
				    constant_string_eol,		\
				    constant_string_non_eol,		\
				    description) do {			\
    if (autodetect_eol_p)						\
      DFC_CHECK_DATA_NUL (ptr, len, constant_string_eol, description);	\
    else								\
      DFC_CHECK_DATA_NUL (ptr, len, constant_string_non_eol, description); \
  } while (0)

  /* Check for expected strings before and after conversion. */
#define DFC_CHECK_DATA(ptr,len,constant_string,test) do {		\
    DFC_INITIALIZE (test);						\
    DFC_CHECK_LENGTH (len, sizeof (constant_string) - 1, test);	\
    DFC_CHECK_CONTENT (ptr, constant_string, len, test);		\
    DFC_RESULT_PASS (test);						\
  } while (0)

  /* Macro version that includes the trailing NULL byte. */
#define DFC_CHECK_DATA_NUL(ptr,len,constant_string,test) do {	\
    DFC_INITIALIZE (test);					\
    DFC_CHECK_LENGTH (len, sizeof (constant_string), test);	\
    DFC_CHECK_CONTENT (ptr, constant_string, len, test);	\
    DFC_RESULT_PASS (test);					\
  } while (0)

/* WARNING WARNING WARNING!
   The following macros are NOT protected by "do { ... } while (0)"!!
*/

#define DFC_INITIALIZE(test_name) if (0)

#define DFC_CHECK_LENGTH(len1,len2,str1)	\
    else if ((len1) != (len2))			\
      conversion_result =			\
        Fcons (list3 (build_cistring(str1), Qnil, build_ascstring("wrong length")), \
	       conversion_result)

#define DFC_CHECK_CONTENT(str1,str2,len1,str3)				\
    else if (memcmp (str1, str2, len1))					\
      conversion_result =						\
	Fcons (list3 (build_cistring (str3), Qnil,			\
		      concat2						\
		      (concat2						\
		       (build_ascstring ("octet comparison failed: expected "), \
			build_extstring ((Extbyte *) str2, Qbinary)),	\
		       concat2						\
		       (build_ascstring (", got "),			\
			build_extstring ((Extbyte *) str1, Qbinary)))), \
	       conversion_result)

#define DFC_RESULT_PASS(str1)		\
    else				\
      conversion_result =		\
	Fcons (list3 (build_cistring(str1), Qt, Qnil),	\
	       conversion_result)

#ifdef MULE
  ptr = NULL, len = rand();
  TO_EXTERNAL_FORMAT (DATA, (int_latin2, sizeof (int_latin2)),
		      ALLOCA, (ptr, len),
		      intern ("iso-8859-2"));
  DFC_CHECK_DATA_NUL (ptr, len, ext_latin,
		      "Latin-2 DATA, ALLOCA, Latin 2/NUL");

  ptr = NULL, len = rand();
  TO_EXTERNAL_FORMAT (LISP_STRING, string_latin2,
		      ALLOCA, (ptr, len),
		      intern ("iso-8859-2"));
  DFC_CHECK_DATA (ptr, len, ext_latin,
		  "Latin-2 Lisp string, ALLOCA, Latin 2");

  ptr = NULL, len = rand();
  TO_EXTERNAL_FORMAT (LISP_STRING, string_latin1,
		      ALLOCA, (ptr, len),
		      intern ("iso-latin-2-with-esc"));
  DFC_CHECK_DATA (ptr, len, ext_latin12,
		  "Latin-1 Lisp string, ALLOCA, Latin 2/ESC");

  ptr = NULL, len = rand();
  TO_EXTERNAL_FORMAT (DATA, (int_latin2, sizeof (int_latin2) - 1),
		      MALLOC, (ptr, len),
		      intern ("iso-8859-2"));
  DFC_CHECK_DATA (ptr, len, ext_latin, "Latin-2 DATA, MALLOC, Latin-2");
  xfree (ptr);

  TO_EXTERNAL_FORMAT (DATA, (int_latin2, sizeof (int_latin2) - 1),
		      LISP_OPAQUE, opaque,
		      intern ("iso-8859-2"));
  DFC_CHECK_DATA (XOPAQUE_DATA (opaque), XOPAQUE_SIZE (opaque), ext_latin,
		  "Latin-2 DATA, Lisp opaque, Latin-2");

  ptr = NULL, len = rand();
  TO_INTERNAL_FORMAT (DATA, (ext_latin, sizeof (ext_latin) - 1),
		      ALLOCA, (ptr, len),
		      intern ("iso-latin-2-with-esc"));
  DFC_CHECK_DATA (ptr, len, int_latin2,
		  "Latin-2/ESC, ALLOCA, Latin-1 DATA");

  ptr = NULL, len = rand();
  TO_INTERNAL_FORMAT (DATA, (ext_latin, sizeof (ext_latin) - 1),
		      MALLOC, (ptr, len),
		      intern ("iso-latin-2-with-esc"));
  DFC_CHECK_DATA (ptr, len, int_latin2,
		  "Latin-2/ESC, MALLOC, Latin-1 DATA");
  xfree (ptr);

  TO_INTERNAL_FORMAT (DATA, (ext_latin, sizeof (ext_latin) - 1),
		      LISP_STRING, string,
		      intern ("iso-latin-2-with-esc"));
  DFC_CHECK_DATA (XSTRING_DATA (string), XSTRING_LENGTH (string), int_latin2,
		  "Latin-2/ESC, Lisp string, Latin-2");

  TO_INTERNAL_FORMAT (LISP_OPAQUE, opaque_latin,
		      LISP_STRING, string,
		      intern ("iso-latin-2-with-esc"));
  DFC_CHECK_DATA (XSTRING_DATA (string), XSTRING_LENGTH (string), int_latin2,
		  "Lisp opaque, Lisp string, Latin-2/ESC");

  TO_INTERNAL_FORMAT (LISP_OPAQUE, opaque0_latin,
		      LISP_STRING, string,
		      intern ("iso-latin-2-with-esc"));
  DFC_CHECK_DATA_NUL (XSTRING_DATA (string), XSTRING_LENGTH (string), int_latin2,
		      "Lisp opaque, Lisp string, Latin-2/ESC/NUL");

  TO_INTERNAL_FORMAT (LISP_OPAQUE, opaque0_latin,
		      LISP_BUFFER, Fcurrent_buffer(),
		      intern ("iso-latin-2-with-esc"));
  DFC_CHECK_DATA_NUL (BUF_BYTE_ADDRESS (current_buffer, BUF_PT (current_buffer)),
		    sizeof (int_latin2), int_latin2,
		      "Lisp opaque, Lisp buffer, Latin-2/ESC/NUL");

  TO_INTERNAL_FORMAT (LISP_OPAQUE, opaque_latin,
		      LISP_BUFFER, Fcurrent_buffer(),
		      intern ("iso-8859-1"));
  DFC_CHECK_DATA (BUF_BYTE_ADDRESS (current_buffer, BUF_PT (current_buffer)),
		  sizeof (int_latin1) - 1, int_latin1,
		  "Lisp opaque, Lisp buffer, Latin-1");

  TO_INTERNAL_FORMAT (DATA, (ext_latin12, sizeof (ext_latin12) - 1),
		      ALLOCA, (ptr, len),
		      intern ("iso-latin-2-with-esc"));
  DFC_CHECK_DATA (ptr, len, int_latin1, "DATA, ALLOCA, Latin-1");

#endif /* MULE */

  ptr = NULL, len = rand();
  TO_EXTERNAL_FORMAT (DATA, (int_latin1, sizeof (int_latin1) - 1),
		      ALLOCA, (ptr, len),
		      Qbinary);
  DFC_CHECK_DATA_COND_MULE (ptr, len, ext_latin, int_latin1,
			    "Latin-1 DATA, ALLOCA, binary");

  ptr = NULL, len = rand();
  TO_EXTERNAL_FORMAT (DATA, (int_latin1, sizeof (int_latin1)),
		      ALLOCA, (ptr, len),
		      Qbinary);
  DFC_CHECK_DATA_COND_MULE_NUL (ptr, len, ext_latin, int_latin1,
				"Latin-1 DATA, ALLOCA, binary/NUL");

  ptr = NULL, len = rand();
  TO_EXTERNAL_FORMAT (DATA, (int_latin2, sizeof (int_latin2) - 1),
		      ALLOCA, (ptr, len),
		      Qbinary);
  DFC_CHECK_DATA_COND_MULE (ptr, len, ext_untranslatable, int_latin2,
			    "Latin-2 DATA, ALLOCA, binary");

  ptr = NULL, len = rand();
  TO_EXTERNAL_FORMAT (DATA, (int_latin1, sizeof (int_latin1) - 1),
		      ALLOCA, (ptr, len),
		      intern ("iso-8859-1"));
  DFC_CHECK_DATA_COND_MULE (ptr, len, ext_latin, int_latin1,
			    "Latin-1 DATA, ALLOCA, Latin-1");


  ptr = NULL, len = rand();
  TO_EXTERNAL_FORMAT (LISP_STRING, string_latin1,
		      ALLOCA, (ptr, len),
		      Qbinary);
  DFC_CHECK_DATA_COND_MULE (ptr, len, ext_latin, int_latin1,
			    "Latin-1 Lisp string, ALLOCA, binary");

  ptr = NULL, len = rand();
  TO_EXTERNAL_FORMAT (LISP_STRING, string_latin1,
		      ALLOCA, (ptr, len),
		      Qbinary);
  DFC_CHECK_DATA_COND_MULE (ptr, len, ext_latin, int_latin1,
			    "Latin-1 Lisp string, ALLOCA, binary");

  ptr = NULL, len = rand();
  TO_EXTERNAL_FORMAT (LISP_STRING, string_latin1,
		      ALLOCA, (ptr, len),
		      intern ("iso-8859-1"));
  DFC_CHECK_DATA_COND_MULE (ptr, len, ext_latin, int_latin1,
			    "Latin-1 Lisp string, ALLOCA, Latin-1");

  ptr = NULL, len = rand();
  TO_EXTERNAL_FORMAT (DATA, (int_latin1, sizeof (int_latin1) - 1),
		      MALLOC, (ptr, len),
		      Qbinary);
  DFC_CHECK_DATA_COND_MULE (ptr, len, ext_latin, int_latin1,
			    "Latin-1 DATA, MALLOC, binary");
  xfree (ptr);

  ptr = NULL, len = rand();
  TO_EXTERNAL_FORMAT (DATA, (int_latin2, sizeof (int_latin2)),
		      MALLOC, (ptr, len),
		      Qbinary);
  DFC_CHECK_DATA_COND_MULE_NUL (ptr, len, ext_untranslatable, int_latin2,
				"Latin-2 DATA, MALLOC, binary/NUL");
  xfree (ptr);

  ptr = NULL, len = rand();
  TO_EXTERNAL_FORMAT (DATA, (int_latin1, sizeof (int_latin1) - 1),
		      MALLOC, (ptr, len),
		      intern ("iso-8859-1"));
  DFC_CHECK_DATA_COND_MULE (ptr, len, ext_latin, int_latin1,
			    "Latin-1 DATA, MALLOC, Latin-1");
  xfree (ptr);

  TO_EXTERNAL_FORMAT (DATA, (int_latin1, sizeof (int_latin1) - 1),
		      LISP_OPAQUE, opaque,
		      Qbinary);
  DFC_CHECK_DATA_COND_MULE (XOPAQUE_DATA (opaque),
			    XOPAQUE_SIZE (opaque), ext_latin, int_latin1,
			    "Latin-1 DATA, Lisp opaque, binary");

  TO_EXTERNAL_FORMAT (DATA, (int_latin2, sizeof (int_latin2)),
		      LISP_OPAQUE, opaque,
		      Qbinary);
  DFC_CHECK_DATA_COND_MULE_NUL (XOPAQUE_DATA (opaque),
				XOPAQUE_SIZE (opaque), ext_untranslatable,
				int_latin2,
				"Latin-2 DATA, Lisp opaque, binary");

  TO_EXTERNAL_FORMAT (DATA, (int_latin1, sizeof (int_latin1) - 1),
		      LISP_OPAQUE, opaque,
		      intern ("iso-8859-1"));
  DFC_CHECK_DATA_COND_MULE (XOPAQUE_DATA (opaque),
			    XOPAQUE_SIZE (opaque), ext_latin, int_latin1,
			    "Latin-1 DATA, Lisp opaque, Latin-1");

  ptr = NULL, len = rand();
  TO_INTERNAL_FORMAT (DATA, (ext_latin, sizeof (ext_latin) - 1),
		      ALLOCA, (ptr, len),
		      Qbinary);
  DFC_CHECK_DATA_COND_MULE (ptr, len, int_latin1, ext_latin,
			    "Latin-1 DATA, ALLOCA, binary");

  ptr = NULL, len = rand();
  TO_INTERNAL_FORMAT (DATA, (ext_latin, sizeof (ext_latin)),
		      ALLOCA, (ptr, len),
		      intern ("iso-8859-1"));
  DFC_CHECK_DATA_COND_MULE_NUL (ptr, len, int_latin1, ext_latin,
				"Latin-1 DATA, ALLOCA, Latin-1");

  ptr = NULL, len = rand();
  TO_INTERNAL_FORMAT (DATA, (ext_latin, sizeof (ext_latin)),
		      MALLOC, (ptr, len),
		      intern ("iso-8859-1"));
  DFC_CHECK_DATA_COND_MULE_NUL (ptr, len, int_latin1, ext_latin,
				"Latin-1 DATA, MALLOC, Latin-1");
  xfree (ptr);

  ptr = NULL, len = rand();
  TO_INTERNAL_FORMAT (DATA, (ext_latin, sizeof (ext_latin)),
		      MALLOC, (ptr, len),
		      Qnil);
  DFC_CHECK_DATA_COND_MULE_NUL (ptr, len, int_latin1, ext_latin,
				"Latin-1 DATA, MALLOC, nil");
  xfree (ptr);

  TO_INTERNAL_FORMAT (DATA, (ext_latin, sizeof (ext_latin) - 1),
		      LISP_STRING, string,
		      intern ("iso-8859-1"));
  DFC_CHECK_DATA_COND_MULE (XSTRING_DATA (string),
			    XSTRING_LENGTH (string), int_latin1, ext_latin,
			    "Latin-1 DATA, Lisp stirng, Latin-1");

  TO_INTERNAL_FORMAT (LISP_OPAQUE, opaque_latin,
		      LISP_STRING, string,
		      intern ("iso-8859-1"));
  DFC_CHECK_DATA_COND_MULE (XSTRING_DATA (string),
			    XSTRING_LENGTH (string), int_latin1, ext_latin,
			    "Latin-1 Lisp opaque, Lisp string, Latin-1");

  TO_INTERNAL_FORMAT (LISP_OPAQUE, opaque0_latin,
		      LISP_STRING, string,
		      intern ("iso-8859-1"));
  DFC_CHECK_DATA_COND_MULE_NUL (XSTRING_DATA (string),
				XSTRING_LENGTH (string), int_latin1, ext_latin,
				"Latin-1 Lisp opaque, Lisp string, Latin-1/NUL");

  /* This next group used to use the COND_EOL macros, but with the new Mule,
     they all specify an EOL convention, and all XEmacsen can grok them. */
  ptr = NULL, len = rand();
  TO_EXTERNAL_FORMAT (DATA, (int_foo, sizeof (int_foo)),
		      MALLOC, (ptr, len),
		      Qbinary);
  DFC_CHECK_DATA_NUL (ptr, len, ext_unix,
		      "ASCII DATA, MALLOC, binary/LF/NUL");
  xfree (ptr);

  ptr = NULL, len = rand();
  TO_EXTERNAL_FORMAT (DATA, (int_foo, sizeof (int_foo) - 1),
		      LISP_OPAQUE, opaque,
		      intern ("raw-text-mac"));
  DFC_CHECK_DATA (XOPAQUE_DATA (opaque), XOPAQUE_SIZE (opaque), ext_mac,
		      "ASCII DATA, Lisp opaque, binary/CR");

  ptr = NULL, len = rand();
  TO_EXTERNAL_FORMAT (LISP_STRING, string_foo,
		      ALLOCA, (ptr, len),
		      intern ("raw-text-dos"));
  DFC_CHECK_DATA (ptr, len, ext_dos, "ASCII Lisp string, ALLOCA, binary/CRLF");

  ptr = NULL, len = rand();
  TO_EXTERNAL_FORMAT (DATA, (int_foo, sizeof (int_foo) - 1),
		      ALLOCA, (ptr, len),
		      intern ("raw-text-unix"));
  DFC_CHECK_DATA (ptr, len, ext_unix, "ASCII DATA, ALLOCA, binary/LF");

  ptr = NULL, len = rand();
  TO_EXTERNAL_FORMAT (LISP_STRING, string_foo,
		      MALLOC, (ptr, len),
		      intern ("no-conversion-mac"));
  DFC_CHECK_DATA (ptr, len, ext_mac, "ASCII Lisp string, MALLOC, binary/CR");
  xfree (ptr);

  ptr = NULL, len = rand();
  TO_EXTERNAL_FORMAT (DATA, (int_foo, sizeof (int_foo) - 1),
		      ALLOCA, (ptr, len),
		      intern ("no-conversion-dos"));
  DFC_CHECK_DATA (ptr, len, ext_dos, "ASCII DATA, ALLOCA, binary/CRLF");

  ptr = NULL, len = rand();
  TO_EXTERNAL_FORMAT (DATA, (int_foo, sizeof (int_foo)),
		      ALLOCA, (ptr, len),
		      intern ("no-conversion-unix"));
  DFC_CHECK_DATA_NUL (ptr, len, ext_unix, "ASCII DATA, ALLOCA, binary/LF/NUL");

  /* Oh, Lawdy, Lawdy, Lawdy, this done broke mah heart!

     I tried using the technique

     Fget_coding_system (call2
			 (intern ("coding-system-change-eol-conversion"),
			  intern ("undecided"), $EOL_TYPE));
     XCODING_SYSTEM_EOL_TYPE (cs_to_use) = $EOL_DETECT_TYPE;

     with EOL_TYPE = Qlf (for no-detect) and Qnil (for auto-detect),
     and with EOL_DETECT_TYPE = EOL_LF and EOL_AUTODETECT
     respectively, but this doesn't seem to work on the `undecided'
     coding system.  The coding-system-eol-type attribute on the
     coding system itself needs to be changed, too.  I'm not sure at
     the moment how `set-eol-detection' works its magic, but the code
     below gives correct test results without default EOL detection,
     with default EOL detection, and with Mule.  Ship it!

     Mule.  You'll envy the dead.
  */

  {
    /* Check eol autodetection doesn't happen when disabled -- cheat. */
    Lisp_Object cs_to_use = Fget_coding_system (intern ("undecided-unix"));
    TO_INTERNAL_FORMAT (LISP_OPAQUE, opaque_dos,
			LISP_BUFFER, Fcurrent_buffer(),
			cs_to_use);
    DFC_CHECK_DATA (BUF_BYTE_ADDRESS (current_buffer, BUF_PT (current_buffer)),
		    sizeof (ext_dos) - 1, ext_dos,
		    "DOS Lisp opaque, Lisp buffer, undecided-unix");

    /* Check eol autodetection works when enabled -- honest. */
    cs_to_use =
      Fget_coding_system (call2
			  (intern ("coding-system-change-eol-conversion"),
			  intern ("undecided"), Qnil));
    XCODING_SYSTEM_EOL_TYPE (cs_to_use) = EOL_AUTODETECT;
    TO_INTERNAL_FORMAT (LISP_OPAQUE, opaque_dos,
			LISP_BUFFER, Fcurrent_buffer(),
			cs_to_use);
    DFC_CHECK_DATA (BUF_BYTE_ADDRESS (current_buffer, BUF_PT (current_buffer)),
		    sizeof (int_foo) - 1, int_foo,
		    "DOS Lisp opaque, Lisp buffer, undecided");
    /* reset to default */
    XCODING_SYSTEM_EOL_TYPE (cs_to_use) =
      autodetect_eol_p ? EOL_AUTODETECT : EOL_LF;
  }

  /* Does eol-detection-enabled-p reflect the actual state of affairs?
     This probably could be tested in Lisp somehow.  Should it? */
  TO_INTERNAL_FORMAT (LISP_OPAQUE, opaque_dos,
		      LISP_BUFFER, Fcurrent_buffer(),
		      intern ("undecided"));
  if (autodetect_eol_p)
    DFC_CHECK_DATA (BUF_BYTE_ADDRESS (current_buffer,
				      BUF_PT (current_buffer)),
		    sizeof (int_foo) - 1, int_foo,
		    "DOS Lisp opaque, Lisp buffer, autodetect eol");
  else
    DFC_CHECK_DATA (BUF_BYTE_ADDRESS (current_buffer,
				      BUF_PT (current_buffer)),
		    sizeof (ext_dos) - 1, ext_dos,
		    "DOS Lisp opaque, Lisp buffer, no autodetect eol");

  TO_INTERNAL_FORMAT (DATA, (ext_mac, sizeof (ext_mac) - 1),
		      LISP_STRING, string,
		      intern ("iso-8859-1"));
  DFC_CHECK_DATA_COND_EOL (XSTRING_DATA (string),
			   XSTRING_LENGTH (string), int_foo, ext_mac,
			   "Mac DATA, Lisp string, Latin-1/EOL");
  {
    Lisp_Object stream =
      make_fixed_buffer_input_stream (ext_dos, sizeof (ext_dos) - 1);
    TO_INTERNAL_FORMAT (LISP_LSTREAM, stream,
			LISP_STRING, string,
			intern ("iso-8859-1"));
    DFC_CHECK_DATA_COND_EOL (XSTRING_DATA (string),
			     XSTRING_LENGTH (string), int_foo, ext_dos,
			   "DOS lstream, Lisp string, Latin-1/EOL");
  }

  TO_INTERNAL_FORMAT (DATA, (ext_unix, sizeof (ext_unix) - 1),
		      LISP_STRING, string,
		      intern ("no-conversion"));
  DFC_CHECK_DATA_COND_EOL (XSTRING_DATA (string),
			   XSTRING_LENGTH (string), int_foo, ext_unix,
			   "Unix DATA, Lisp string, no-conversion");

  ptr = NULL, len = rand();
  TO_EXTERNAL_FORMAT (LISP_OPAQUE, opaque_dos,
		      ALLOCA, (ptr, len),
		      Qbinary);
  DFC_CHECK_DATA (ptr, len, ext_dos, "DOS Lisp opaque, ALLOCA, binary");

  return conversion_result;
}


/* Hash Table testing */

typedef struct
{
  Lisp_Object hash_table;
  EMACS_INT sum;
} test_hash_tables_data;


static int
test_hash_tables_mapper (Lisp_Object UNUSED (key), Lisp_Object value,
			 void *extra_arg)
{
  test_hash_tables_data *p = (test_hash_tables_data *) extra_arg;
  p->sum += XINT (value);
  return 0;
}

static int
test_hash_tables_modifying_mapper (Lisp_Object key, Lisp_Object value,
				   void *extra_arg)
{
  test_hash_tables_data *p = (test_hash_tables_data *) extra_arg;
  Fputhash (make_int (- XINT (key)),
	    make_int (2 * XINT (value)),
	    p->hash_table);
  p->sum += XINT (value);
  return 0;
}

static int
test_hash_tables_predicate (Lisp_Object key,
			    Lisp_Object UNUSED (value),
			    void *UNUSED (extra_arg))
{
  return XINT (key) < 0;
}


DEFUN ("test-hash-tables", Ftest_hash_tables, 0, 0, "", /*
  Return list of results of testing C interface to hash tables.
For use by the automated test suite.  See tests/automated/c-tests.

Each element is a list (DESCRIPTION, STATUS, REASON).
DESCRIPTION is a string describing the test.
STATUS is a symbol, either t (pass) or nil (fail).
REASON is nil or a string describing the failure (not required).
*/
       ())
{
  Lisp_Object hash_result = Qnil;

  test_hash_tables_data data;
  data.hash_table = make_lisp_hash_table (50, HASH_TABLE_NON_WEAK,
					  HASH_TABLE_EQUAL);

  Fputhash (make_int (1), make_int (2), data.hash_table);
  Fputhash (make_int (3), make_int (4), data.hash_table);

  data.sum = 0;
  elisp_maphash_unsafe (test_hash_tables_mapper,
			data.hash_table, (void *) &data);
  hash_result = Fcons (list3 (build_ascstring ("simple mapper"),
				   (data.sum == 2 + 4) ? Qt : Qnil,
				   build_ascstring ("sum != 2 + 4")),
			    hash_result);

  data.sum = 0;
  elisp_maphash (test_hash_tables_modifying_mapper,
		 data.hash_table, (void *) &data);
  hash_result = Fcons (list3 (build_ascstring ("modifying mapper"),
				   (data.sum == 2 + 4) ? Qt : Qnil,
				   build_ascstring ("sum != 2 + 4")),
			    hash_result);

  /* hash table now contains:  (1, 2) (3, 4) (-1, 2*2) (-3, 2*4) */

  data.sum = 0;
  elisp_maphash_unsafe (test_hash_tables_mapper,
			data.hash_table, (void *) &data);
  hash_result = Fcons (list3 (build_ascstring ("simple mapper"),
				   (data.sum == 3 * (2 + 4)) ? Qt : Qnil,
				   build_ascstring ("sum != 3 * (2 + 4)")),
			    hash_result);

  /* Remove entries with negative keys, added by modifying mapper */
  elisp_map_remhash (test_hash_tables_predicate,
		     data.hash_table, 0);

  data.sum = 0;
  elisp_maphash_unsafe (test_hash_tables_mapper,
			data.hash_table, (void *) &data);
  hash_result = Fcons (list3 (build_ascstring ("remove negatives mapper"),
				   (data.sum == 2 + 4) ? Qt : Qnil,
				   build_ascstring ("sum != 2 + 4")),
			    hash_result);

  return hash_result;
}

DEFUN ("test-store-void-in-lisp", Ftest_store_void_in_lisp, 0, 0, "", /*
  Test STORE_VOID_IN_LISP and its inverse GET_VOID_FROM_LISP.
Tests by internal assert(); only returns if it succeeds.
*/
       ())
{
  struct foobar { int x; int y; short z; void *q; } baz;

#define FROB(val)							\
do									\
{									\
  void *pval = (void *) (val);						\
  assert (GET_VOID_FROM_LISP (STORE_VOID_IN_LISP (pval)) == pval);	\
}									\
while (0)
  assert (INT_VALBITS >= 31);
  FROB (&baz);
  FROB (&baz.x);
  FROB (&baz.y);
  FROB (&baz.z);
  FROB (&baz.q);
  FROB (0);
  FROB (2);
  FROB (&Vtest_function_list);
  FROB (0x00000080);
  FROB (0x00008080);
  FROB (0x00808080);
  FROB (0x80808080);
  FROB (0xCAFEBABE);
  FROB (0xFFFFFFFE);
#if INT_VALBITS >= 63
  FROB (0x0000808080808080);
  FROB (0x8080808080808080);
  FROB (0XDEADBEEFCAFEBABE);
  FROB (0XFFFFFFFFFFFFFFFE);
#endif /* INT_VALBITS >= 63 */

  return list1 (list3 (build_ascstring ("STORE_VOID_IN_LISP"), Qt, Qnil));
}



#ifdef NEW_GC
#define TESTS_DEFSUBR(Fname) do {		\
  DEFSUBR_MC_ALLOC (Fname);			\
  defsubr (S##Fname);				\
  Vtest_function_list =				\
    Fcons (intern (subr_name (S##Fname)),	\
	   Vtest_function_list);		\
} while (0)
#else /* not NEW_GC */
#define TESTS_DEFSUBR(Fname) do {		\
  DEFSUBR (Fname);				\
  Vtest_function_list =				\
    Fcons (intern (subr_name (&S##Fname)),	\
	   Vtest_function_list);		\
} while (0)
#endif /* not NEW_GC */

void
syms_of_tests (void)
{
  Vtest_function_list = Qnil;

  TESTS_DEFSUBR (Ftest_data_format_conversion);
  TESTS_DEFSUBR (Ftest_hash_tables);
  TESTS_DEFSUBR (Ftest_store_void_in_lisp);
  /* Add other test functions here with TESTS_DEFSUBR */
}

void
vars_of_tests (void)
{
  DEFVAR_LISP ("test-function-list", &Vtest_function_list /*
List of all test functions defined in tests.c.
For use by the automated test suite.  See tests/automated/c-tests.
*/ );
}
