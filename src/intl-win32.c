/* Win32 internationalization functions.
   Copyright (C) 2000, 2001, 2002 Ben Wing.
   Copyright (C) 2000 IKEYAMA Tomonori.

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

   Current primary author: Ben Wing <ben@xemacs.org>

   Created summer 2000 by Ben Wing.  Almost completely written by Ben Wing.
   Little bits of code in some of the Lisp primitives from FSF Emacs.
   Versions of wcscpy, wcsncpy from Cygwin newlib.

   Coding systems written by Ben Wing in file-coding.c; moved here Sep 2001.
   */

#include <config.h>
#include "lisp.h"

#include "elhash.h"
#include "faces.h"
#include "file-coding.h"
#include "frame.h"
#include "window.h"

#include "console-msw.h"
#include "objects-msw.h"

#ifndef CYGWIN_HEADERS
# include <mbctype.h>
#elif defined (MINGW)
int _setmbcp (int);
int _getmbcp (void);
#else
# define NO_EXT_MULTIBYTE_FEATURES
#endif

Lisp_Object Qmswindows_multibyte, Qmswindows_multibyte_to_unicode;
Lisp_Object Qmswindows_tstr, Qmswindows_unicode;
Lisp_Object Qmswindows_multibyte_system_default;

Lisp_Object Qansi, Qoem, Qmac, Qebcdic;
/* Qcode_page, Qlocale, Qcurrent, Quser_default, Qsystem_default in
   general-slots.h */

#ifdef MULE

static Lisp_Object Vmswindows_charset_code_page_table;

LCID current_locale;


/************************************************************************/
/*            Language/locale/code page conversion functions            */
/************************************************************************/

/* There are various different ways of representing the vague concept
   of "language", and it can be very confusing.  So:

   -- The C library has the concept of "locale", which is a
   combination of language and country, and which controls the way
   currency and dates are displayed, the encoding of data, etc.

   -- XEmacs has the concept of "language environment", more or less
   like a locale; although currently in most cases it just refers to
   the language, and no sub-language distinctions are
   made. (Exceptions are with Chinese, which has different language
   environments for Taiwan and mainland China, due to the different
   encodings and writing systems.)

   -- Windows has a number of different language concepts:

   1. There are "languages" and "sublanguages", which correspond to
   the languages and countries of the C library -- e.g. LANG_ENGLISH
   and SUBLANG_ENGLISH_US.  These are identified by 8-bit integers,
   called the "primary language identifier" and "sublanguage
   identifier", respectively.  These are combined into a 16-bit
   integer or "language identifier" by MAKELANGID().

   2. The language identifier in turn is combined with a "sort
   identifier" (and optionally a "sort version") to yield a 32-bit
   integer called a "locale identifier" (type LCID), which identifies
   locales -- the primary means of distinguishing language/regional
   settings and similar to C library locales.

   3. "Code pages" identify different text encodings (i.e. a set of
   supported characters, an enumeration of those characters [i.e. an
   association of character with number or number pair; there may be
   disjoint ranges of numbers supported]), and a way of encoding a
   stream of those characters into an 8-bit data stream).  All of the
   encodings are either one-byte or mixed one-byte/two-byte encodings,
   all non-modal; in the mixed encodings, two-byte characters have the
   first byte >= 128, although the second byte may or may not be
   restricted to this range, depending on the encoding.  Code pages
   are similar to XEmacs "charsets"; the latter also define a set of
   supported characters and an enumeration of those characters (but
   code pages in additionally define an encoding, which charsets don't
   do).  Code pages often function in Windows like charsets in XEmacs.

   4. Every Windows locale has a specific code page associated with
   it; more than one locale can share a code page -- e.g. all the
   Western European languages, including English, do.

   5. Windows also has an "input locale identifier" (aka "keyboard
   layout id") or HKL, which is a 32-bit integer composed of the
   16-bit language identifier and a 16-bit "device identifier", which
   originally specified a particular keyboard layout (e.g. the locale
   "US English" can have the QWERTY layout, the Dvorak layout, etc.),
   but has been expanded to include speech-to-text converters and
   other non-keyboard ways of inputting text.  Note that both the HKL
   and LCID share the language identifier in the lower 16 bits, and in
   both cases a 0 in the upper 16 bits means "default" (sort order or
   device), providing a way to convert between HKL's, LCID's, and
   language identifiers (i.e. language/sublanguage pairs).  The
   default keyboard layout for a language is (as far as I can
   determine) established using the Regional Settings control panel
   applet, where you can add input locales as combinations of language
   (actually language/sublanguage) and layout; presumably if you list
   only one input locale with a particular language, the corresponding
   layout is the default for that language.  But what if you list more
   than one?  You can specify a single default input locale, but there
   appears to be no way to do so on a per-language basis.
*/   

struct lang_to_string
{
  int code;
  char *string;
};

struct lang_to_string lang_to_string_table[] =
{
  /* These names change from version to version of VC++, so it's easiest
     just to bracket them all with ifdefs. */
#ifdef LANG_AFRIKAANS
  { LANG_AFRIKAANS, "AFRIKAANS" },
#endif
#ifdef LANG_ALBANIAN
  { LANG_ALBANIAN, "ALBANIAN" },
#endif
#ifdef LANG_ARABIC
  { LANG_ARABIC, "ARABIC" },
#endif
#ifdef LANG_ARMENIAN
  { LANG_ARMENIAN, "ARMENIAN" },
#endif
#ifdef LANG_ASSAMESE
  { LANG_ASSAMESE, "ASSAMESE" },
#endif
#ifdef LANG_AZERI
  { LANG_AZERI, "AZERI" },
#endif
#ifdef LANG_BASQUE
  { LANG_BASQUE, "BASQUE" },
#endif
#ifdef LANG_BELARUSIAN
  { LANG_BELARUSIAN, "BELARUSIAN" },
#endif
#ifdef LANG_BENGALI
  { LANG_BENGALI, "BENGALI" },
#endif
#ifdef LANG_BULGARIAN
  { LANG_BULGARIAN, "BULGARIAN" },
#endif
#ifdef LANG_CATALAN
  { LANG_CATALAN, "CATALAN" },
#endif
#ifdef LANG_CHINESE
  { LANG_CHINESE, "CHINESE" },
#endif
#ifdef LANG_CROATIAN
  { LANG_CROATIAN, "CROATIAN" },
#endif
#ifdef LANG_CZECH
  { LANG_CZECH, "CZECH" },
#endif
#ifdef LANG_DANISH
  { LANG_DANISH, "DANISH" },
#endif
#ifdef LANG_DUTCH
  { LANG_DUTCH, "DUTCH" },
#endif
#ifdef LANG_ENGLISH
  { LANG_ENGLISH, "ENGLISH" },
#endif
#ifdef LANG_ESTONIAN
  { LANG_ESTONIAN, "ESTONIAN" },
#endif
#ifdef LANG_FAEROESE
  { LANG_FAEROESE, "FAEROESE" },
#endif
#ifdef LANG_FARSI
  { LANG_FARSI, "FARSI" },
#endif
#ifdef LANG_FINNISH
  { LANG_FINNISH, "FINNISH" },
#endif
#ifdef LANG_FRENCH
  { LANG_FRENCH, "FRENCH" },
#endif
#ifdef LANG_GEORGIAN
  { LANG_GEORGIAN, "GEORGIAN" },
#endif
#ifdef LANG_GERMAN
  { LANG_GERMAN, "GERMAN" },
#endif
#ifdef LANG_GREEK
  { LANG_GREEK, "GREEK" },
#endif
#ifdef LANG_GUJARATI
  { LANG_GUJARATI, "GUJARATI" },
#endif
#ifdef LANG_HEBREW
  { LANG_HEBREW, "HEBREW" },
#endif
#ifdef LANG_HINDI
  { LANG_HINDI, "HINDI" },
#endif
#ifdef LANG_HUNGARIAN
  { LANG_HUNGARIAN, "HUNGARIAN" },
#endif
#ifdef LANG_ICELANDIC
  { LANG_ICELANDIC, "ICELANDIC" },
#endif
#ifdef LANG_INDONESIAN
  { LANG_INDONESIAN, "INDONESIAN" },
#endif
#ifdef LANG_ITALIAN
  { LANG_ITALIAN, "ITALIAN" },
#endif
#ifdef LANG_JAPANESE
  { LANG_JAPANESE, "JAPANESE" },
#endif
#ifdef LANG_KANNADA
  { LANG_KANNADA, "KANNADA" },
#endif
#ifdef LANG_KASHMIRI
  { LANG_KASHMIRI, "KASHMIRI" },
#endif
#ifdef LANG_KAZAK
  { LANG_KAZAK, "KAZAK" },
#endif
#ifdef LANG_KONKANI
  { LANG_KONKANI, "KONKANI" },
#endif
#ifdef LANG_KOREAN
  { LANG_KOREAN, "KOREAN" },
#endif
#ifdef LANG_LATVIAN
  { LANG_LATVIAN, "LATVIAN" },
#endif
#ifdef LANG_LITHUANIAN
  { LANG_LITHUANIAN, "LITHUANIAN" },
#endif
#ifdef LANG_MACEDONIAN
  { LANG_MACEDONIAN, "MACEDONIAN" },
#endif
#ifdef LANG_MALAY
  { LANG_MALAY, "MALAY" },
#endif
#ifdef LANG_MALAYALAM
  { LANG_MALAYALAM, "MALAYALAM" },
#endif
#ifdef LANG_MANIPURI
  { LANG_MANIPURI, "MANIPURI" },
#endif
#ifdef LANG_MARATHI
  { LANG_MARATHI, "MARATHI" },
#endif
#ifdef LANG_NEPALI
  { LANG_NEPALI, "NEPALI" },
#endif
#ifdef LANG_NEUTRAL
  { LANG_NEUTRAL, "NEUTRAL" },
#endif
#ifdef LANG_NORWEGIAN
  { LANG_NORWEGIAN, "NORWEGIAN" },
#endif
#ifdef LANG_ORIYA
  { LANG_ORIYA, "ORIYA" },
#endif
#ifdef LANG_POLISH
  { LANG_POLISH, "POLISH" },
#endif
#ifdef LANG_PORTUGUESE
  { LANG_PORTUGUESE, "PORTUGUESE" },
#endif
#ifdef LANG_PUNJABI
  { LANG_PUNJABI, "PUNJABI" },
#endif
#ifdef LANG_ROMANIAN
  { LANG_ROMANIAN, "ROMANIAN" },
#endif
#ifdef LANG_RUSSIAN
  { LANG_RUSSIAN, "RUSSIAN" },
#endif
#ifdef LANG_SANSKRIT
  { LANG_SANSKRIT, "SANSKRIT" },
#endif
#ifdef LANG_SERBIAN
  { LANG_SERBIAN, "SERBIAN" },
#endif
#ifdef LANG_SINDHI
  { LANG_SINDHI, "SINDHI" },
#endif
#ifdef LANG_SLOVAK
  { LANG_SLOVAK, "SLOVAK" },
#endif
#ifdef LANG_SLOVENIAN
  { LANG_SLOVENIAN, "SLOVENIAN" },
#endif
#ifdef LANG_SPANISH
  { LANG_SPANISH, "SPANISH" },
#endif
#ifdef LANG_SWAHILI
  { LANG_SWAHILI, "SWAHILI" },
#endif
#ifdef LANG_SWEDISH
  { LANG_SWEDISH, "SWEDISH" },
#endif
#ifdef LANG_TAMIL
  { LANG_TAMIL, "TAMIL" },
#endif
#ifdef LANG_TATAR
  { LANG_TATAR, "TATAR" },
#endif
#ifdef LANG_TELUGU
  { LANG_TELUGU, "TELUGU" },
#endif
#ifdef LANG_THAI
  { LANG_THAI, "THAI" },
#endif
#ifdef LANG_TURKISH
  { LANG_TURKISH, "TURKISH" },
#endif
#ifdef LANG_UKRAINIAN
  { LANG_UKRAINIAN, "UKRAINIAN" },
#endif
#ifdef LANG_URDU
  { LANG_URDU, "URDU" },
#endif
#ifdef LANG_UZBEK
  { LANG_UZBEK, "UZBEK" },
#endif
#ifdef LANG_VIETNAMESE
  { LANG_VIETNAMESE, "VIETNAMESE" },
#endif
};

struct lang_to_string sublang_to_string_table[] =
{
  { LANG_ARABIC, 0 },
#ifdef SUBLANG_ARABIC_ALGERIA
  { SUBLANG_ARABIC_ALGERIA, "ARABIC_ALGERIA" },
#endif
#ifdef SUBLANG_ARABIC_BAHRAIN
  { SUBLANG_ARABIC_BAHRAIN, "ARABIC_BAHRAIN" },
#endif
#ifdef SUBLANG_ARABIC_EGYPT
  { SUBLANG_ARABIC_EGYPT, "ARABIC_EGYPT" },
#endif
#ifdef SUBLANG_ARABIC_IRAQ
  { SUBLANG_ARABIC_IRAQ, "ARABIC_IRAQ" },
#endif
#ifdef SUBLANG_ARABIC_JORDAN
  { SUBLANG_ARABIC_JORDAN, "ARABIC_JORDAN" },
#endif
#ifdef SUBLANG_ARABIC_KUWAIT
  { SUBLANG_ARABIC_KUWAIT, "ARABIC_KUWAIT" },
#endif
#ifdef SUBLANG_ARABIC_LEBANON
  { SUBLANG_ARABIC_LEBANON, "ARABIC_LEBANON" },
#endif
#ifdef SUBLANG_ARABIC_LIBYA
  { SUBLANG_ARABIC_LIBYA, "ARABIC_LIBYA" },
#endif
#ifdef SUBLANG_ARABIC_MOROCCO
  { SUBLANG_ARABIC_MOROCCO, "ARABIC_MOROCCO" },
#endif
#ifdef SUBLANG_ARABIC_OMAN
  { SUBLANG_ARABIC_OMAN, "ARABIC_OMAN" },
#endif
#ifdef SUBLANG_ARABIC_QATAR
  { SUBLANG_ARABIC_QATAR, "ARABIC_QATAR" },
#endif
#ifdef SUBLANG_ARABIC_SAUDI_ARABIA
  { SUBLANG_ARABIC_SAUDI_ARABIA, "ARABIC_SAUDI_ARABIA" },
#endif
#ifdef SUBLANG_ARABIC_SYRIA
  { SUBLANG_ARABIC_SYRIA, "ARABIC_SYRIA" },
#endif
#ifdef SUBLANG_ARABIC_TUNISIA
  { SUBLANG_ARABIC_TUNISIA, "ARABIC_TUNISIA" },
#endif
#ifdef SUBLANG_ARABIC_UAE
  { SUBLANG_ARABIC_UAE, "ARABIC_UAE" },
#endif
#ifdef SUBLANG_ARABIC_YEMEN
  { SUBLANG_ARABIC_YEMEN, "ARABIC_YEMEN" },
#endif
  { LANG_AZERI, 0 },
#ifdef SUBLANG_AZERI_CYRILLIC
  { SUBLANG_AZERI_CYRILLIC, "AZERI_CYRILLIC" },
#endif
#ifdef SUBLANG_AZERI_LATIN
  { SUBLANG_AZERI_LATIN, "AZERI_LATIN" },
#endif
  { LANG_CHINESE, 0 },
#ifdef SUBLANG_CHINESE_HONGKONG
  { SUBLANG_CHINESE_HONGKONG, "CHINESE_HONGKONG" },
#endif
#ifdef SUBLANG_CHINESE_MACAU
  { SUBLANG_CHINESE_MACAU, "CHINESE_MACAU" },
#endif
#ifdef SUBLANG_CHINESE_SIMPLIFIED
  { SUBLANG_CHINESE_SIMPLIFIED, "CHINESE_SIMPLIFIED" },
#endif
#ifdef SUBLANG_CHINESE_SINGAPORE
  { SUBLANG_CHINESE_SINGAPORE, "CHINESE_SINGAPORE" },
#endif
#ifdef SUBLANG_CHINESE_TRADITIONAL
  { SUBLANG_CHINESE_TRADITIONAL, "CHINESE_TRADITIONAL" },
#endif
  { LANG_DUTCH, 0 },
#ifdef SUBLANG_DUTCH
  { SUBLANG_DUTCH, "DUTCH" },
#endif
#ifdef SUBLANG_DUTCH_BELGIAN
  { SUBLANG_DUTCH_BELGIAN, "DUTCH_BELGIAN" },
#endif
  { LANG_ENGLISH, 0 },
#ifdef SUBLANG_ENGLISH_AUS
  { SUBLANG_ENGLISH_AUS, "ENGLISH_AUS" },
#endif
#ifdef SUBLANG_ENGLISH_BELIZE
  { SUBLANG_ENGLISH_BELIZE, "ENGLISH_BELIZE" },
#endif
#ifdef SUBLANG_ENGLISH_CAN
  { SUBLANG_ENGLISH_CAN, "ENGLISH_CAN" },
#endif
#ifdef SUBLANG_ENGLISH_CARIBBEAN
  { SUBLANG_ENGLISH_CARIBBEAN, "ENGLISH_CARIBBEAN" },
#endif
#ifdef SUBLANG_ENGLISH_EIRE
  { SUBLANG_ENGLISH_EIRE, "ENGLISH_EIRE" },
#endif
#ifdef SUBLANG_ENGLISH_JAMAICA
  { SUBLANG_ENGLISH_JAMAICA, "ENGLISH_JAMAICA" },
#endif
#ifdef SUBLANG_ENGLISH_NZ
  { SUBLANG_ENGLISH_NZ, "ENGLISH_NZ" },
#endif
#ifdef SUBLANG_ENGLISH_PHILIPPINES
  { SUBLANG_ENGLISH_PHILIPPINES, "ENGLISH_PHILIPPINES" },
#endif
#ifdef SUBLANG_ENGLISH_SOUTH_AFRICA
  { SUBLANG_ENGLISH_SOUTH_AFRICA, "ENGLISH_SOUTH_AFRICA" },
#endif
#ifdef SUBLANG_ENGLISH_TRINIDAD
  { SUBLANG_ENGLISH_TRINIDAD, "ENGLISH_TRINIDAD" },
#endif
#ifdef SUBLANG_ENGLISH_UK
  { SUBLANG_ENGLISH_UK, "ENGLISH_UK" },
#endif
#ifdef SUBLANG_ENGLISH_US
  { SUBLANG_ENGLISH_US, "ENGLISH_US" },
#endif
#ifdef SUBLANG_ENGLISH_ZIMBABWE
  { SUBLANG_ENGLISH_ZIMBABWE, "ENGLISH_ZIMBABWE" },
#endif
  { LANG_FRENCH, 0 },
#ifdef SUBLANG_FRENCH
  { SUBLANG_FRENCH, "FRENCH" },
#endif
#ifdef SUBLANG_FRENCH_BELGIAN
  { SUBLANG_FRENCH_BELGIAN, "FRENCH_BELGIAN" },
#endif
#ifdef SUBLANG_FRENCH_CANADIAN
  { SUBLANG_FRENCH_CANADIAN, "FRENCH_CANADIAN" },
#endif
#ifdef SUBLANG_FRENCH_LUXEMBOURG
  { SUBLANG_FRENCH_LUXEMBOURG, "FRENCH_LUXEMBOURG" },
#endif
#ifdef SUBLANG_FRENCH_MONACO
  { SUBLANG_FRENCH_MONACO, "FRENCH_MONACO" },
#endif
#ifdef SUBLANG_FRENCH_SWISS
  { SUBLANG_FRENCH_SWISS, "FRENCH_SWISS" },
#endif
  { LANG_GERMAN, 0 },
#ifdef SUBLANG_GERMAN
  { SUBLANG_GERMAN, "GERMAN" },
#endif
#ifdef SUBLANG_GERMAN_AUSTRIAN
  { SUBLANG_GERMAN_AUSTRIAN, "GERMAN_AUSTRIAN" },
#endif
#ifdef SUBLANG_GERMAN_LIECHTENSTEIN
  { SUBLANG_GERMAN_LIECHTENSTEIN, "GERMAN_LIECHTENSTEIN" },
#endif
#ifdef SUBLANG_GERMAN_LUXEMBOURG
  { SUBLANG_GERMAN_LUXEMBOURG, "GERMAN_LUXEMBOURG" },
#endif
#ifdef SUBLANG_GERMAN_SWISS
  { SUBLANG_GERMAN_SWISS, "GERMAN_SWISS" },
#endif
  { LANG_ITALIAN, 0 },
#ifdef SUBLANG_ITALIAN
  { SUBLANG_ITALIAN, "ITALIAN" },
#endif
#ifdef SUBLANG_ITALIAN_SWISS
  { SUBLANG_ITALIAN_SWISS, "ITALIAN_SWISS" },
#endif
  { LANG_KASHMIRI, 0 },
#ifdef SUBLANG_KASHMIRI_INDIA
  { SUBLANG_KASHMIRI_INDIA, "KASHMIRI_INDIA" },
#endif
  { LANG_KOREAN, 0 },
#ifdef SUBLANG_KOREAN
  { SUBLANG_KOREAN, "KOREAN" },
#endif
#ifdef SUBLANG_KOREAN_JOHAB
  /* NOTE: Omitted in more recent versions of VC++ (e.g. v6.0) */
  { SUBLANG_KOREAN_JOHAB, "KOREAN_JOHAB" },
#endif
  { LANG_LITHUANIAN, 0 },
#ifdef SUBLANG_LITHUANIAN
  { SUBLANG_LITHUANIAN, "LITHUANIAN" },
#endif
#ifdef SUBLANG_LITHUANIAN_CLASSIC
  { SUBLANG_LITHUANIAN_CLASSIC, "LITHUANIAN_CLASSIC" },
#endif
  { LANG_MALAY, 0 },
#ifdef SUBLANG_MALAY_BRUNEI_DARUSSALAM
  { SUBLANG_MALAY_BRUNEI_DARUSSALAM, "MALAY_BRUNEI_DARUSSALAM" },
#endif
#ifdef SUBLANG_MALAY_MALAYSIA
  { SUBLANG_MALAY_MALAYSIA, "MALAY_MALAYSIA" },
#endif
  { LANG_NEPALI, 0 },
#ifdef SUBLANG_NEPALI_INDIA
  { SUBLANG_NEPALI_INDIA, "NEPALI_INDIA" },
#endif
  { LANG_NEUTRAL, 0 },
#ifdef SUBLANG_NEUTRAL
  { SUBLANG_NEUTRAL, "NEUTRAL" },
#endif
  { LANG_NORWEGIAN, 0 },
#ifdef SUBLANG_NORWEGIAN_BOKMAL
  { SUBLANG_NORWEGIAN_BOKMAL, "NORWEGIAN_BOKMAL" },
#endif
#ifdef SUBLANG_NORWEGIAN_NYNORSK
  { SUBLANG_NORWEGIAN_NYNORSK, "NORWEGIAN_NYNORSK" },
#endif
  { LANG_PORTUGUESE, 0 },
#ifdef SUBLANG_PORTUGUESE
  { SUBLANG_PORTUGUESE, "PORTUGUESE" },
#endif
#ifdef SUBLANG_PORTUGUESE_BRAZILIAN
  { SUBLANG_PORTUGUESE_BRAZILIAN, "PORTUGUESE_BRAZILIAN" },
#endif
  { LANG_SERBIAN, 0 },
#ifdef SUBLANG_SERBIAN_CYRILLIC
  { SUBLANG_SERBIAN_CYRILLIC, "SERBIAN_CYRILLIC" },
#endif
#ifdef SUBLANG_SERBIAN_LATIN
  { SUBLANG_SERBIAN_LATIN, "SERBIAN_LATIN" },
#endif
  { LANG_SPANISH, 0 },
#ifdef SUBLANG_SPANISH
  { SUBLANG_SPANISH, "SPANISH" },
#endif
#ifdef SUBLANG_SPANISH_ARGENTINA
  { SUBLANG_SPANISH_ARGENTINA, "SPANISH_ARGENTINA" },
#endif
#ifdef SUBLANG_SPANISH_BOLIVIA
  { SUBLANG_SPANISH_BOLIVIA, "SPANISH_BOLIVIA" },
#endif
#ifdef SUBLANG_SPANISH_CHILE
  { SUBLANG_SPANISH_CHILE, "SPANISH_CHILE" },
#endif
#ifdef SUBLANG_SPANISH_COLOMBIA
  { SUBLANG_SPANISH_COLOMBIA, "SPANISH_COLOMBIA" },
#endif
#ifdef SUBLANG_SPANISH_COSTA_RICA
  { SUBLANG_SPANISH_COSTA_RICA, "SPANISH_COSTA_RICA" },
#endif
#ifdef SUBLANG_SPANISH_DOMINICAN_REPUBLIC
  { SUBLANG_SPANISH_DOMINICAN_REPUBLIC, "SPANISH_DOMINICAN_REPUBLIC" },
#endif
#ifdef SUBLANG_SPANISH_ECUADOR
  { SUBLANG_SPANISH_ECUADOR, "SPANISH_ECUADOR" },
#endif
#ifdef SUBLANG_SPANISH_EL_SALVADOR
  { SUBLANG_SPANISH_EL_SALVADOR, "SPANISH_EL_SALVADOR" },
#endif
#ifdef SUBLANG_SPANISH_GUATEMALA
  { SUBLANG_SPANISH_GUATEMALA, "SPANISH_GUATEMALA" },
#endif
#ifdef SUBLANG_SPANISH_HONDURAS
  { SUBLANG_SPANISH_HONDURAS, "SPANISH_HONDURAS" },
#endif
#ifdef SUBLANG_SPANISH_MEXICAN
  { SUBLANG_SPANISH_MEXICAN, "SPANISH_MEXICAN" },
#endif
#ifdef SUBLANG_SPANISH_MODERN
  { SUBLANG_SPANISH_MODERN, "SPANISH_MODERN" },
#endif
#ifdef SUBLANG_SPANISH_NICARAGUA
  { SUBLANG_SPANISH_NICARAGUA, "SPANISH_NICARAGUA" },
#endif
#ifdef SUBLANG_SPANISH_PANAMA
  { SUBLANG_SPANISH_PANAMA, "SPANISH_PANAMA" },
#endif
#ifdef SUBLANG_SPANISH_PARAGUAY
  { SUBLANG_SPANISH_PARAGUAY, "SPANISH_PARAGUAY" },
#endif
#ifdef SUBLANG_SPANISH_PERU
  { SUBLANG_SPANISH_PERU, "SPANISH_PERU" },
#endif
#ifdef SUBLANG_SPANISH_PUERTO_RICO
  { SUBLANG_SPANISH_PUERTO_RICO, "SPANISH_PUERTO_RICO" },
#endif
#ifdef SUBLANG_SPANISH_URUGUAY
  { SUBLANG_SPANISH_URUGUAY, "SPANISH_URUGUAY" },
#endif
#ifdef SUBLANG_SPANISH_VENEZUELA
  { SUBLANG_SPANISH_VENEZUELA, "SPANISH_VENEZUELA" },
#endif
  { LANG_SWEDISH, 0 },
#ifdef SUBLANG_SWEDISH
  { SUBLANG_SWEDISH, "SWEDISH" },
#endif
#ifdef SUBLANG_SWEDISH_FINLAND
  { SUBLANG_SWEDISH_FINLAND, "SWEDISH_FINLAND" },
#endif
  { LANG_URDU, 0 },
#ifdef SUBLANG_URDU_INDIA
  { SUBLANG_URDU_INDIA, "URDU_INDIA" },
#endif
#ifdef SUBLANG_URDU_PAKISTAN
  { SUBLANG_URDU_PAKISTAN, "URDU_PAKISTAN" },
#endif
  { LANG_UZBEK, 0 },
#ifdef SUBLANG_UZBEK_CYRILLIC
  { SUBLANG_UZBEK_CYRILLIC, "UZBEK_CYRILLIC" },
#endif
#ifdef SUBLANG_UZBEK_LATIN
  { SUBLANG_UZBEK_LATIN, "UZBEK_LATIN" },
#endif
};

static int
lang_to_langcode (Lisp_Object lang, struct lang_to_string *table,
		  int table_size)
{
  int i;

  for (i = 0; i < table_size; i++)
    if (!strcmp ((char *) XSTRING_DATA (lang), table[i].string))
      return table[i].code;
  return -1;
}

static int
sublang_to_langcode (Lisp_Object lang, struct lang_to_string *table,
		     int table_size)
{
  int i;

  for (i = 0; i < table_size; i++)
    if (table[i].string &&
	!strcmp ((char *) XSTRING_DATA (lang), table[i].string))
      return table[i].code;

  if (!strcmp ((char *) XSTRING_DATA (lang), "NEUTRAL"))
    return SUBLANG_NEUTRAL;
  if (!strcmp ((char *) XSTRING_DATA (lang), "DEFAULT"))
    return SUBLANG_DEFAULT;
  if (!strcmp ((char *) XSTRING_DATA (lang), "SYS_DEFAULT"))
    return SUBLANG_SYS_DEFAULT;

  return -1;
}

static Lisp_Object
langcode_to_lang (int code, struct lang_to_string *table,
		  int table_size)
{
  int i;

  for (i = 0; i < table_size; i++)
    if (code == table[i].code)
      return build_string (table[i].string);
  return Qnil;
}

static Lisp_Object
sublangcode_to_lang (int lang, int sublang, struct lang_to_string *table,
		     int table_size)
{
  int i;
  int found_lang = 0;

  for (i = 0; i < table_size; i++)
    {
      if (found_lang)
	{
	  if (!table[i].string)
	    break;
	  if (sublang == table[i].code)
	    return build_string (table[i].string);
	}
      else if (!table[i].string && lang == table[i].code)
	found_lang = 1;
    }

  switch (sublang)
    {
    case SUBLANG_NEUTRAL:
      return build_string ("NEUTRAL");
    case SUBLANG_DEFAULT:
      return build_string ("DEFAULT");
    case SUBLANG_SYS_DEFAULT:
      return build_string ("SYS_DEFAULT");
    }

  return Qnil;
}

static LCID
locale_to_lcid (Lisp_Object locale)
{
  int langcode, sublangcode;
  Lisp_Object lang, sublang;

  if (STRINGP (locale))
    {
      lang = locale;
      sublang = Qnil;
    }
  else if (CONSP (locale))
    {
      CHECK_STRING (XCAR (locale));
      CHECK_STRING (XCDR (locale));
      lang = XCAR (locale);
      sublang = XCDR (locale);
    }
  else
    invalid_argument ("Locale must be LANG or (LANG . SUBLANG)", locale);

  langcode = lang_to_langcode (lang, lang_to_string_table,
			       countof (lang_to_string_table));

  if (langcode < 0)
    invalid_constant ("Unrecognized language", lang);

  if (!NILP (sublang))
    {
      sublangcode = sublang_to_langcode (sublang, sublang_to_string_table,
					 countof (sublang_to_string_table));
      if (sublangcode < 0)
	invalid_constant ("Unrecognized sublanguage", sublang);
    }
  else
    sublangcode = SUBLANG_DEFAULT;

  return MAKELCID (MAKELANGID (langcode, sublangcode),
		   SORT_DEFAULT);
}

static Lisp_Object
lcid_to_locale (LCID lcid)
{
  int langid = LANGIDFROMLCID (lcid);
  int langcode = PRIMARYLANGID (langid);
  int sublangcode = SUBLANGID (langid);

  return Fcons (langcode_to_lang (langcode, lang_to_string_table,
				  countof (lang_to_string_table)),
		sublangcode_to_lang (langcode, sublangcode,
				     sublang_to_string_table,
				     countof (sublang_to_string_table)));
}

int
mswindows_locale_to_code_page (LCID lcid)
{
  char codepagestr[10];

  GetLocaleInfoA (lcid, LOCALE_IDEFAULTANSICODEPAGE, codepagestr, 10);
  return atoi (codepagestr);
}

int
mswindows_locale_to_oem_code_page (LCID lcid)
{
  char codepagestr[10];

  GetLocaleInfoA (lcid, LOCALE_IDEFAULTCODEPAGE, codepagestr, 10);
  return atoi (codepagestr);
}

static void
set_current_lcid (LCID lcid)
{
  int cp;

  /* This will fail under Win9x, so we remember our own locale rather than
     consulting GetThreadLocale. */
  SetThreadLocale (lcid);
  current_locale = lcid;
  cp = mswindows_locale_to_code_page (lcid);
#ifndef NO_EXT_MULTIBYTE_FEATURES
  _setmbcp (cp);
#endif
}

DEFUN ("mswindows-set-current-locale", Fmswindows_set_current_locale,
       1, 1, 0, /*
Set the current MS Windows locale.

LOCALE should a language string, or a cons (LANG . SUBLANG).
If SUBLANG is omitted, "SUBLANG_DEFAULT" is used.

Recognized language names are
(some may not be recognized if the compiler is older than VC++ 6.0)

"AFRIKAANS"
"ALBANIAN"
"ARABIC"
"ARMENIAN"
"ASSAMESE"
"AZERI"
"BASQUE"
"BELARUSIAN"
"BENGALI"
"BULGARIAN"
"CATALAN"
"CHINESE"
"CROATIAN"
"CZECH"
"DANISH"
"DUTCH"
"ENGLISH"
"ESTONIAN"
"FAEROESE"
"FARSI"
"FINNISH"
"FRENCH"
"GEORGIAN"
"GERMAN"
"GREEK"
"GUJARATI"
"HEBREW"
"HINDI"
"HUNGARIAN"
"ICELANDIC"
"INDONESIAN"
"ITALIAN"
"JAPANESE"
"KANNADA"
"KASHMIRI"
"KAZAK"
"KONKANI"
"KOREAN"
"LATVIAN"
"LITHUANIAN"
"MACEDONIAN"
"MALAY"
"MALAYALAM"
"MANIPURI"
"MARATHI"
"NEPALI"
"NEUTRAL"
"NORWEGIAN"
"ORIYA"
"POLISH"
"PORTUGUESE"
"PUNJABI"
"ROMANIAN"
"RUSSIAN"
"SANSKRIT"
"SERBIAN"
"SINDHI"
"SLOVAK"
"SLOVENIAN"
"SPANISH"
"SWAHILI"
"SWEDISH"
"TAMIL"
"TATAR"
"TELUGU"
"THAI"
"TURKISH"
"UKRAINIAN"
"URDU"
"UZBEK"
"VIETNAMESE"

Recognized sub-language names are
(some may not be recognized if the compiler is older than VC++ 6.0)

"ARABIC_ALGERIA"
"ARABIC_BAHRAIN"
"ARABIC_EGYPT"
"ARABIC_IRAQ"
"ARABIC_JORDAN"
"ARABIC_KUWAIT"
"ARABIC_LEBANON"
"ARABIC_LIBYA"
"ARABIC_MOROCCO"
"ARABIC_OMAN"
"ARABIC_QATAR"
"ARABIC_SAUDI_ARABIA"
"ARABIC_SYRIA"
"ARABIC_TUNISIA"
"ARABIC_UAE"
"ARABIC_YEMEN"
"AZERI_CYRILLIC"
"AZERI_LATIN"
"CHINESE_HONGKONG"
"CHINESE_MACAU"
"CHINESE_SIMPLIFIED"
"CHINESE_SINGAPORE"
"CHINESE_TRADITIONAL"
"DEFAULT"
"DUTCH"
"DUTCH_BELGIAN"
"ENGLISH_AUS"
"ENGLISH_BELIZE"
"ENGLISH_CAN"
"ENGLISH_CARIBBEAN"
"ENGLISH_EIRE"
"ENGLISH_JAMAICA"
"ENGLISH_NZ"
"ENGLISH_PHILIPPINES"
"ENGLISH_SOUTH_AFRICA"
"ENGLISH_TRINIDAD"
"ENGLISH_UK"
"ENGLISH_US"
"ENGLISH_ZIMBABWE"
"FRENCH"
"FRENCH_BELGIAN"
"FRENCH_CANADIAN"
"FRENCH_LUXEMBOURG"
"FRENCH_MONACO"
"FRENCH_SWISS"
"GERMAN"
"GERMAN_AUSTRIAN"
"GERMAN_LIECHTENSTEIN"
"GERMAN_LUXEMBOURG"
"GERMAN_SWISS"
"ITALIAN"
"ITALIAN_SWISS"
"KASHMIRI_INDIA"
"KOREAN"
"KOREAN_JOHAB" (NOTE: omitted in Visual C++ 6.0 and later)
"LITHUANIAN"
"LITHUANIAN_CLASSIC"
"MALAY_BRUNEI_DARUSSALAM"
"MALAY_MALAYSIA"
"NEPALI_INDIA"
"NEUTRAL"
"NORWEGIAN_BOKMAL"
"NORWEGIAN_NYNORSK"
"PORTUGUESE"
"PORTUGUESE_BRAZILIAN"
"SERBIAN_CYRILLIC"
"SERBIAN_LATIN"
"SPANISH"
"SPANISH_ARGENTINA"
"SPANISH_BOLIVIA"
"SPANISH_CHILE"
"SPANISH_COLOMBIA"
"SPANISH_COSTA_RICA"
"SPANISH_DOMINICAN_REPUBLIC"
"SPANISH_ECUADOR"
"SPANISH_EL_SALVADOR"
"SPANISH_GUATEMALA"
"SPANISH_HONDURAS"
"SPANISH_MEXICAN"
"SPANISH_MODERN"
"SPANISH_NICARAGUA"
"SPANISH_PANAMA"
"SPANISH_PARAGUAY"
"SPANISH_PERU"
"SPANISH_PUERTO_RICO"
"SPANISH_URUGUAY"
"SPANISH_VENEZUELA"
"SWEDISH"
"SWEDISH_FINLAND"
"SYS_DEFAULT"
"URDU_INDIA"
"URDU_PAKISTAN"
"UZBEK_CYRILLIC"
"UZBEK_LATIN"
*/
       (locale))
{
  LCID lcid = locale_to_lcid (locale);

  set_current_lcid (lcid);
  return Qnil;
}

#ifdef DEBUG_XEMACS

static int getacp (void);
int
getacp (void)
{
  return GetACP ();
}

#endif /* DEBUG_XEMACS */

LCID
mswindows_current_locale (void)
{
  /* Even if SetThreadLocale() failed, return the right locale anyway */
  return current_locale;
}

DEFUN ("mswindows-current-locale", Fmswindows_current_locale,
       0, 0, 0, /*
Return the current MS Windows locale.

The return value will be a cons (LANG . SUBLANG).  See
`mswindows-set-current-locale' for more info.
*/
       ())
{
  return lcid_to_locale (mswindows_current_locale ());
}

DEFUN ("mswindows-user-default-locale", Fmswindows_user_default_locale,
       0, 0, 0, /*
Return the MS Windows user-default locale.
*/
       ())
{
  return lcid_to_locale (GetUserDefaultLCID ());
}

DEFUN ("mswindows-system-default-locale", Fmswindows_system_default_locale,
       0, 0, 0, /*
Return the MS Windows system-default locale.
*/
       ())
{
  return lcid_to_locale (GetSystemDefaultLCID ());
}

DEFUN ("mswindows-locale-code-page", Fmswindows_locale_code_page,
       0, 1, 0, /*
Return the (ANSI) code page of the specified MS Windows locale.
If LOCALE is nil or omitted, the current locale is used.
*/
       (locale))
{
  LCID lcid = NILP (locale) ? current_locale : locale_to_lcid (locale);
  return make_int (mswindows_locale_to_code_page (lcid));
}

DEFUN ("mswindows-locale-oem-code-page", Fmswindows_locale_oem_code_page,
       0, 1, 0, /*
Return the OEM code page of the specified MS Windows locale.
If LOCALE is nil or omitted, the current locale is used.
*/
       (locale))
{
  LCID lcid = NILP (locale) ? current_locale : locale_to_lcid (locale);
  return make_int (mswindows_locale_to_oem_code_page (lcid));
}

static DWORD
int_from_hex (Char_ASCII *s)
{
  DWORD val = 0;
  static Char_ASCII hex[] = "0123456789abcdefABCDEF";
  Char_ASCII *p;

  while (*s && (p = strchr (hex, *s)) != NULL)
    {
      int digit = p - hex;
      if (digit > 15)
	digit -= 6;
      val = val * 16 + digit;
      s++;
    }
  return val;
}

/* We need to build a global list, since the EnumSystemLocale callback
   function isn't given a context pointer.  */
static Lisp_Object Vmswindows_valid_locales;

static BOOL CALLBACK
enum_locale_fn (Char_ASCII *localeNum)
{
  DWORD id = int_from_hex (localeNum);
  Vmswindows_valid_locales =
    Fcons (lcid_to_locale ((LCID) id), Vmswindows_valid_locales);
  return TRUE;
}

DEFUN ("mswindows-supported-locales", Fmswindows_supported_locales,
       0, 0, 0, /*
Return a list of the supported MS Windows locales on this system.
*/
       ())
{
  Vmswindows_valid_locales = Qnil;

  /* Use the ANSI version because the return value is just a hex number. */
  EnumSystemLocalesA (enum_locale_fn, LCID_SUPPORTED);

  Vmswindows_valid_locales = Fnreverse (Vmswindows_valid_locales);
  return Vmswindows_valid_locales;
}

/************************************************************************/
/*                            Mule functions                            */
/************************************************************************/

DEFUN ("mswindows-charset-code-page", 
       Fmswindows_charset_code_page, 1, 1, 0, /*
Return the code page for the CHARSET.

#### This function may be changed in the near future.

Currently defined Windows code pages include (along with their status
as Ansi, OEM, Mac, EBCDIC, or some combination):

EBCDIC      037 EBCDIC 
OEM         437 MS-DOS United States 
EBCDIC      500 EBCDIC "500V1" 
OEM         708 Arabic (ASMO 708) 
OEM         709 Arabic (ASMO 449+, BCON V4) 
OEM         710 Arabic (Transparent Arabic) 
OEM         720 Arabic (Transparent ASMO) 
OEM         737 Greek (formerly 437G) 
OEM         775 Baltic 
OEM         850 MS-DOS Multilingual (Latin I) 
OEM         852 MS-DOS Slavic (Latin II) 
OEM         855 IBM Cyrillic (primarily Russian) 
OEM         857 IBM Turkish 
OEM         860 MS-DOS Portuguese 
OEM         861 MS-DOS Icelandic 
OEM         862 Hebrew 
OEM         863 MS-DOS Canadian-French 
OEM         864 Arabic 
OEM         865 MS-DOS Nordic 
OEM         866 MS-DOS Russian 
OEM         869 IBM Modern Greek 
Ansi/OEM    874 Thai 
EBCDIC      875 EBCDIC 
Ansi/OEM    932 Japanese 
Ansi/OEM    936 Chinese (PRC, Singapore) 
Ansi/OEM    949 Korean 
Ansi/OEM    950 Chinese (Taiwan; Hong Kong SAR, PRC)  
EBCDIC      1026 EBCDIC 
ANSI        1200 Unicode (BMP of ISO 10646) 
ANSI        1250 Windows 3.1 Eastern European  
ANSI        1251 Windows 3.1 Cyrillic 
ANSI        1252 Windows 3.1 US (ANSI) 
ANSI        1253 Windows 3.1 Greek 
ANSI        1254 Windows 3.1 Turkish 
ANSI        1255 Hebrew 
ANSI        1256 Arabic 
ANSI        1257 Baltic 
ANSI        1258 VietNam
Ansi/OEM    1361 Korean (Johab) 
Mac         10000 Macintosh Roman 
Mac         10001 Macintosh Japanese 
Mac         10006 Macintosh Greek I 
Mac         10007 Macintosh Cyrillic 
Mac         10029 Macintosh Latin 2 
Mac         10079 Macintosh Icelandic 
Mac         10081 Macintosh Turkish

A code page is a set of characters, along with an enumeration of these
characters and an encoding of them in a byte stream.  Thus, in XEmacs
parlance it defines both a "charset" and a "coding system" for this
charset.  Traditional encodings are either simple one-byte encodings, or
combination one-byte/two-byte encodings (aka MBCS encodings, where MBCS
stands for "Multibyte Character Set") with the following properties:

-- all characters are encoded as a one-byte or two-byte sequence
-- the encoding is stateless (non-modal)
-- the lower 128 bytes are compatible with ASCII
-- in the higher bytes, the value of the first byte ("lead byte")
   determines whether a second byte follows
-- the values used for second bytes may overlap those used for first bytes,
   and (in some encodings) include values in the low half; thus, moving
   backwards is hard, and pure-ASCII algorithms (e.g. finding the next slash)
   will fail unless rewritten to be MBCS-aware (neither of these problems
   exist in UTF-8 or in the XEmacs internal string encoding)

Recent code pages, however, do not necessarily follow these properties --
code pages have been expanded to include arbitrary encodings, such as UTF-8
\(may have more than two bytes per character) and ISO-2022-JP (complex modal
encoding).

Every locale has four associated code pages: ANSI (an international
standard or some Microsoft-created approximation; the native code page
under Windows), OEM (a DOS encoding, still used in the FAT file system),
Mac (an encoding used on the Macintosh) and EBCDIC (a non-ASCII-compatible
encoding used on IBM mainframes, originally based on the BCD or
"binary-coded decimal" encoding of numbers).  All code pages associated
with a locale follow (as far as I know) the properties listed above for
traditional code pages.
*/
       (charset))
{
  charset = Fget_charset (charset);
  return Fgethash (charset, Vmswindows_charset_code_page_table, Qnil);
}

DEFUN ("mswindows-set-charset-code-page", 
       Fmswindows_set_charset_code_page, 2, 2, 0, /*
Set the CODE-PAGE for the CHARSET.

#### This function may be changed once full Unicode support is present.
*/
       (charset, code_page))
{
  charset = Fget_charset (charset);
  CHECK_INT (code_page);
  Fputhash (charset, code_page, Vmswindows_charset_code_page_table);
  return Qnil;
}

Lisp_Object 
mswindows_get_code_page_charset (int code_page)
{
  Lisp_Object charset_tail;
  Lisp_Object charset = Qunbound;

  LIST_LOOP (charset_tail, Fcharset_list ())
    {
      Lisp_Object charset_code_page;

      charset_code_page = Fmswindows_charset_code_page (XCAR (charset_tail));
      if (INTP (charset_code_page) &&
	  code_page == XINT (charset_code_page))
	{
	  charset = Fget_charset (XCAR (charset_tail));
	  break;
	}
    }
  return charset;
}



#if 0 /* #### from Emacs 20.6; consider porting */

xxDEFUN ("mswindows-get-locale-info", Fmswindows_get_locale_info, 1, 2, 0, /*
Return information about the Windows locale LCID.
By default, return a three letter locale code which encodes the default
language as the first two characters, and the country or regionial variant
as the third letter.  For example, ENU refers to `English (United States)',
while ENC means `English (Canadian)'.

If the optional argument LONGFORM is t, the long form of the locale
name is returned, e.g. `English (United States)' instead; if LONGFORM
is a number, it is interpreted as an LCTYPE constant and the corresponding
locale information is returned.

If LCID (a 16-bit number) is not a valid locale, the result is nil.
*/
       (lcid, longform))
{
  int got_abbrev;
  int got_full;
  char abbrev_name[32] = { 0 };
  char full_name[256] = { 0 };

  CHECK_INT (lcid);

  if (!IsValidLocale (XINT (lcid), LCID_SUPPORTED))
    return Qnil;

  if (NILP (longform))
    {
      got_abbrev = GetLocaleInfo (XINT (lcid),
				  LOCALE_SABBREVLANGNAME | LOCALE_USE_CP_ACP,
				  abbrev_name, sizeof (abbrev_name));
      if (got_abbrev)
	return build_string (abbrev_name);
    }
  else if (EQ (longform, Qt))
    {
      got_full = GetLocaleInfo (XINT (lcid),
				LOCALE_SLANGUAGE | LOCALE_USE_CP_ACP,
				full_name, sizeof (full_name));
      if (got_full)
	return build_string (full_name);
    }
  else if (NUMBERP (longform))
    {
      got_full = GetLocaleInfo (XINT (lcid),
				XINT (longform),
				full_name, sizeof (full_name));
      if (got_full)
	return make_unibyte_string (full_name, got_full);
    }

  return Qnil;
}

/* We need to build a global list, since the EnumCodePages callback
   function isn't given a context pointer.  */
Lisp_Object Vmswindows_valid_code_pages;

BOOL CALLBACK enum_code_page_fn (LPTSTR codepageNum)
{
  DWORD id = atoi (codepageNum);
  Vmswindows_valid_code_pages = Fcons (make_int (id), Vmswindows_valid_code_pages);
  return TRUE;
}

xxDEFUN ("mswindows-get-valid-code-pages", Fmswindows_get_valid_code_pages, 0, 0, 0, /*
Return list of all valid Windows code pages.
*/
       ())
{
  Vmswindows_valid_code_pages = Qnil;

  EnumSystemCodePages (enum_code_page_fn, CP_SUPPORTED);

  Vmswindows_valid_code_pages = Fnreverse (Vmswindows_valid_code_pages);
  return Vmswindows_valid_code_pages;
}

xxDEFUN ("mswindows-get-console-code-page", Fmswindows_get_console_code_page, 0, 0, 0, /*
Return current Windows code page for console input.
*/
       ())
{
  return make_int (GetConsoleCP ());
}

xxDEFUN ("mswindows-set-console-code-page", Fmswindows_set_console_code_page, 1, 1, 0, /*
Make Windows code page CP be the current code page setting for Emacs.
The code page setting affects keyboard input and display in tty mode.
If successful, the new CP is returned, otherwise nil.
*/
       (cp))
{
  CHECK_INT (cp);

  if (!IsValidCodePage (XINT (cp)))
    return Qnil;

  if (!SetConsoleCP (XINT (cp)))
    return Qnil;

  return make_int (GetConsoleCP ());
}

xxDEFUN ("mswindows-get-console-output-code-page", Fmswindows_get_console_output_code_page, 0, 0, 0, /*
Return current Windows code page for console output.
*/
       ())
{
  return make_int (GetConsoleOutputCP ());
}

xxDEFUN ("mswindows-set-console-output-code-page", Fmswindows_set_console_output_code_page, 1, 1, 0, /*
Make Windows code page CP be the current code page setting for Emacs.
The code page setting affects keyboard input and display in tty mode.
If successful, the new CP is returned, otherwise nil.
*/
       (cp))
{
  CHECK_INT (cp);

  if (!IsValidCodePage (XINT (cp)))
    return Qnil;

  if (!SetConsoleOutputCP (XINT (cp)))
    return Qnil;

  return make_int (GetConsoleOutputCP ());
}

xxDEFUN ("mswindows-get-code-page-charset", Fmswindows_get_code_page_charset, 1, 1, 0, /*
Return charset of code page CP.
Returns nil if the code page is not valid.
*/
       (cp))
{
  CHARSETINFO info;

  CHECK_INT (cp);

  if (!IsValidCodePage (XINT (cp)))
    return Qnil;

  if (TranslateCharsetInfo ((DWORD *) XINT (cp), &info, TCI_SRCCODEPAGE))
    return make_int (info.ciCharset);

  return Qnil;
}

xxDEFUN ("mswindows-get-valid-keyboard-layouts", Fmswindows_get_valid_keyboard_layouts, 0, 0, 0, /*
Return list of Windows keyboard languages and layouts.
The return value is a list of pairs of language id and layout id.
*/
       ())
{
  int num_layouts = GetKeyboardLayoutList (0, NULL);
  HKL * layouts = (HKL *) alloca (num_layouts * sizeof (HKL));
  Lisp_Object obj = Qnil;

  if (GetKeyboardLayoutList (num_layouts, layouts) == num_layouts)
    {
      while (--num_layouts >= 0)
	{
	  DWORD kl = (DWORD) layouts[num_layouts];

	  obj = Fcons (Fcons (make_int (kl & 0xffff),
			      make_int ((kl >> 16) & 0xffff)),
		       obj);
	}
    }

  return obj;
}

xxDEFUN ("mswindows-get-keyboard-layout", Fmswindows_get_keyboard_layout, 0, 0, 0, /*
Return current Windows keyboard language and layout.
The return value is the cons of the language id and the layout id.
*/
       ())
{
  DWORD kl = (DWORD) GetKeyboardLayout (dwWindowsThreadId);

  return Fcons (make_int (kl & 0xffff),
		make_int ((kl >> 16) & 0xffff));
}

xxDEFUN ("mswindows-set-keyboard-layout", Fmswindows_set_keyboard_layout, 1, 1, 0, /*
Make LAYOUT be the current keyboard layout for Emacs.
The keyboard layout setting affects interpretation of keyboard input.
If successful, the new layout id is returned, otherwise nil.
*/
       (layout))
{
  DWORD kl;

  CHECK_CONS (layout);
  CHECK_INT (XCAR (layout)));
  CHECK_INT (XCDR (layout)));

  kl = (XINT (XCAR (layout))) & 0xffff)
    | (XINT (XCDR (layout))) << 16);

  if (!ActivateKeyboardLayout ((HKL) kl, 0))
    return Qnil;

  return Fmswindows_get_keyboard_layout ();
}

#endif /* 0 */


/* input method functions. */

#ifdef HAVE_MS_WINDOWS

void
mswindows_start_ime_composition (struct frame *f)
{
  COMPOSITIONFORM form;
  HWND hwnd = FRAME_MSWINDOWS_HANDLE (f);
  HIMC himc = ImmGetContext (hwnd);

  /* Set a position of composition window. */
  xzero (form);
  form.dwStyle = CFS_POINT;
  form.ptCurrentPos.x = FRAME_MSWINDOWS_CURSOR_X (f);
  form.ptCurrentPos.y = FRAME_MSWINDOWS_CURSOR_Y (f);
  ImmSetCompositionWindow (himc, &form);

  /* Set composition window font same as current face one. */
  {
    LOGFONTW old_logfont;
    CHARSETINFO info;
    Lisp_Object charset;

    /* Get Mule charset from current ime font charset. */
    qxeImmGetCompositionFont (himc, &old_logfont);
    TranslateCharsetInfo ((DWORD *) (DWORD) old_logfont.lfCharSet, &info,
			  TCI_SRCCHARSET);
    charset = mswindows_get_code_page_charset (info.ciACP);

    if (CHARSETP (charset))
      {
	Lisp_Object window = FRAME_SELECTED_WINDOW (f);
	struct window *w = XWINDOW (window);
	face_index findex = FRAME_MSWINDOWS_CURSOR_FINDEX (f);
	struct face_cachel *cachel = WINDOW_FACE_CACHEL (w, findex);
	Lisp_Object face_font = FACE_CACHEL_FONT (cachel, charset);

	if (!FONT_INSTANCEP (face_font))
	  face_font = 
	    ensure_face_cachel_contains_charset (cachel, window, charset);

	if (!EQ (face_font, Vthe_null_font_instance))
	  {
	    LOGFONTW new_logfont;

	    /* Get LOGFONT from the face font */
	    if (qxeGetObject (FONT_INSTANCE_MSWINDOWS_HFONT_VARIANT
			      (XFONT_INSTANCE (face_font), 
			       cachel->underline, cachel->strikethru),
			      sizeof (LOGFONTW), (void*) &new_logfont))
	      qxeImmSetCompositionFont (himc, &new_logfont);
	  }
      }
  }
  ImmReleaseContext (hwnd, himc);
  return;
}

#endif /* HAVE_MS_WINDOWS */

#else /* not MULE */

int
mswindows_locale_to_code_page (LCID lcid)
{
  return CP_ACP;
}

#endif /* MULE */


#ifdef CYGWIN

/* based on newlib strncpy, strcpy */

wchar_t *
wcsncpy (wchar_t *dst0, const wchar_t *src0, size_t count)
{
  wchar_t *dscan;
  const wchar_t *sscan;

  dscan = dst0;
  sscan = src0;
  while (count > 0)
    {
      --count;
      if ((*dscan++ = *sscan++) == '\0')
	break;
    }
  while (count-- > 0)
    *dscan++ = '\0';

  return dst0;
}

wchar_t *
wcscpy (wchar_t *dst0, const wchar_t *src0)
{
  wchar_t *s = dst0;

  while ((*dst0++ = *src0++))
    ;

  return s;
}

wchar_t *
wcsdup (const wchar_t *str)
{
  int len = wcslen (str) + 1;
  void *val = xmalloc (len * sizeof (wchar_t));

  if (val == 0) return 0;
  return (wchar_t *) memcpy (val, str, len * sizeof (wchar_t));
}

#endif /* CYGWIN */


/************************************************************************/
/*                 MS Windows multibyte-to-unicode methods              */
/************************************************************************/

DEFINE_CODING_SYSTEM_TYPE (mswindows_multibyte_to_unicode);

enum mswindows_multibyte_cp_type
{
  MULTIBYTE_ANSI,
  MULTIBYTE_OEM,
  MULTIBYTE_EBCDIC,
  MULTIBYTE_MAC
};

enum mswindows_multibyte_locale_type
{
  MULTIBYTE_SPECIFIED_LOCALE,
  MULTIBYTE_SPECIFIED_CODE_PAGE,
  MULTIBYTE_CURRENT,
  MULTIBYTE_USER_DEFAULT,
  MULTIBYTE_SYSTEM_DEFAULT
};

struct mswindows_multibyte_to_unicode_coding_system
{
  enum mswindows_multibyte_cp_type cp_type;
  enum mswindows_multibyte_locale_type locale_type;
  LCID locale; /* if locale_type is MULTIBYTE_SPECIFIED_LOCALE */
  int cp; /* if locale_type is MULTIBYTE_SPECIFIED_CODE_PAGE */
};

struct mswindows_multibyte_to_unicode_coding_stream
{
  int partial_byte;
  int partial_byte_present;
  int cp;
};

static const struct lrecord_description
  mswindows_multibyte_to_unicode_coding_system_description[] = {
  { XD_END }
};

static void
mswindows_multibyte_to_unicode_init (Lisp_Object codesys)
{
  struct mswindows_multibyte_to_unicode_coding_system *data =
    XCODING_SYSTEM_TYPE_DATA (codesys, mswindows_multibyte_to_unicode);

  data->cp_type = MULTIBYTE_ANSI;
  data->locale_type = MULTIBYTE_CURRENT;
}

static Lisp_Object
lcid_to_locale_mule_or_no (LCID lcid)
{
#ifdef MULE
  return lcid_to_locale (lcid);
#else
  return Fcons (build_string ("NEUTRAL"), build_string ("DEFAULT"));
#endif
}

static int
determine_code_page (Lisp_Object codesys)
{
#ifdef MULE
  LCID locale;
  struct mswindows_multibyte_to_unicode_coding_system *data =
    XCODING_SYSTEM_TYPE_DATA (codesys, mswindows_multibyte_to_unicode);

  switch (data->locale_type)
    {
    case MULTIBYTE_SPECIFIED_CODE_PAGE:
      return data->cp;
    case MULTIBYTE_SPECIFIED_LOCALE:
      locale = data->locale; break;
    case MULTIBYTE_CURRENT:
      locale = mswindows_current_locale (); break;
    case MULTIBYTE_USER_DEFAULT:
      locale = GetUserDefaultLCID (); break;
    case MULTIBYTE_SYSTEM_DEFAULT:
      locale = GetSystemDefaultLCID (); break;
    default:
      abort (); locale = 0;
    }

  switch (data->cp_type)
    {
    case MULTIBYTE_ANSI:
      return mswindows_locale_to_code_page (locale);
    case MULTIBYTE_OEM:
      return mswindows_locale_to_oem_code_page (locale);
    case MULTIBYTE_EBCDIC:
#ifdef LOCALE_IDEFAULTEBCDICCODEPAGE /* Doesn't exist under Cygwin */
      {
	char codepagestr[10];
	GetLocaleInfoA (locale, LOCALE_IDEFAULTEBCDICCODEPAGE, codepagestr,
			10);
	return atoi (codepagestr);
      }
#else
      invalid_operation ("Unable to determine EBCDIC code page for locale",
			 lcid_to_locale (locale));
      return 0;
#endif
    case MULTIBYTE_MAC:
#ifdef LOCALE_IDEFAULTMACCODEPAGE /* Doesn't exist under Cygwin */
      {
	char codepagestr[10];
	GetLocaleInfoA (locale, LOCALE_IDEFAULTMACCODEPAGE, codepagestr,
			10);
	return atoi (codepagestr);
      }
#else
      invalid_operation ("Unable to determine Mac code page for locale",
			 lcid_to_locale (locale));
      return 0;
#endif
    default:
      abort (); return 0;
    }
#else /* not MULE */
  return CP_ACP;
#endif
}

static int
mswindows_multibyte_to_unicode_putprop (Lisp_Object codesys,
					Lisp_Object key,
					Lisp_Object value)
{
  struct mswindows_multibyte_to_unicode_coding_system *data =
    XCODING_SYSTEM_TYPE_DATA (codesys, mswindows_multibyte_to_unicode);

  if (EQ (key, Qcode_page))
    {
      if (EQ (value, Qansi))
	data->cp_type = MULTIBYTE_ANSI;
      else if (EQ (value, Qoem))
	data->cp_type = MULTIBYTE_OEM;
      else if (EQ (value, Qebcdic))
	data->cp_type = MULTIBYTE_EBCDIC;
      else if (EQ (value, Qmac))
	data->cp_type = MULTIBYTE_MAC;
      else
	{
	  CHECK_NATNUM (value);
	  data->locale_type = MULTIBYTE_SPECIFIED_CODE_PAGE;
	  data->cp = XINT (value);
	}
    }
  else if (EQ (key, Qlocale))
    {
      if (EQ (value, Qcurrent))
	data->locale_type = MULTIBYTE_CURRENT;
      else if (EQ (value, Quser_default))
	data->locale_type = MULTIBYTE_USER_DEFAULT;
      else if (EQ (value, Qsystem_default))
	data->locale_type = MULTIBYTE_SYSTEM_DEFAULT;
      else
	{
	  data->locale_type = MULTIBYTE_SPECIFIED_LOCALE;
#ifdef MULE
	  data->locale = locale_to_lcid (value);
#else
	  data->locale = 0;
#endif
	}
    }
  else
    return 0;
  return 1;
}

static Lisp_Object
mswindows_multibyte_to_unicode_getprop (Lisp_Object coding_system,
					Lisp_Object prop)
{
  struct mswindows_multibyte_to_unicode_coding_system *data =
    XCODING_SYSTEM_TYPE_DATA (coding_system, mswindows_multibyte_to_unicode);

  if (EQ (prop, Qcode_page))
    {
      if (data->locale_type == MULTIBYTE_SPECIFIED_CODE_PAGE)
	return make_int (data->cp);
      else
	switch (data->cp_type)
	  {
	  case MULTIBYTE_ANSI: return Qansi;
	  case MULTIBYTE_OEM: return Qoem;
	  case MULTIBYTE_EBCDIC: return Qebcdic;
	  case MULTIBYTE_MAC: return Qmac;
	  default: abort ();
	  }
    }
  else if (EQ (prop, Qlocale))
    {
      switch (data->locale_type)
	{
	case MULTIBYTE_CURRENT: return Qcurrent;
	case MULTIBYTE_USER_DEFAULT: return Quser_default;
	case MULTIBYTE_SYSTEM_DEFAULT: return Qsystem_default;
	case MULTIBYTE_SPECIFIED_LOCALE:
	  return lcid_to_locale_mule_or_no (data->locale);

	case MULTIBYTE_SPECIFIED_CODE_PAGE:
	  return Qnil;
	default: abort ();
	}
    }

  return Qunbound;
}

static void
mswindows_multibyte_to_unicode_print (Lisp_Object cs,
				      Lisp_Object printcharfun, int escapeflag)
{
  struct mswindows_multibyte_to_unicode_coding_system *data =
    XCODING_SYSTEM_TYPE_DATA (cs, mswindows_multibyte_to_unicode);

  write_c_string ("(", printcharfun);
  if (data->locale_type == MULTIBYTE_SPECIFIED_CODE_PAGE)
    print_internal (make_int (data->cp), printcharfun, 1);
  else
    {
      write_fmt_string_lisp (printcharfun, "%s, ", 1, mswindows_multibyte_to_unicode_getprop (cs, Qlocale));
      print_internal (mswindows_multibyte_to_unicode_getprop (cs, Qcode_page), printcharfun, 0);
    }
  write_c_string (")", printcharfun);
}

/* Convert multibyte to Unicode according to the specified code page
   and return the value as a malloc()ed string.  This currently exists
   because the TO_INTERNAL_FORMAT() mechanism -- the normal way to do
   such conversions -- has no way of passing in a parameter to control
   the operation.  We could use a global variable to pass this value
   in, but that runs the risk of causing problems due to reentrancy.
   (You might say, yeah, right, how can TO_INTERNAL_FORMAT() get
   called recursively merely when I'm doing a simple conversion
   operation?  It turns out this can and does happen, consistently, as
   a result of calling QUIT -- it happens consistently for complicated
   reasons outlined in event-msw.c, WM_KEYDOWN handling.) */

Extbyte *
convert_multibyte_to_unicode_malloc (const Extbyte *src, Bytecount n,
				     int cp, Bytecount *size_out)
{
  Bytecount nout = MultiByteToWideChar (cp, 0, src, n, 0, 0);
  Extbyte *outp = xnew_array (Extbyte, nout * sizeof (WCHAR));

  MultiByteToWideChar (cp, 0, src, n, (LPWSTR) outp, nout);
  if (size_out)
    *size_out = nout * sizeof (WCHAR);
  return outp;
}

/* Convert MS Windows multibyte to internal, with specified code page.
   See above for why this exists, and the TO_INTERNAL_FORMAT() macros
   aren't just used. */

Intbyte *
convert_multibyte_to_internal_malloc (const Extbyte *src, Bytecount n,
				      int cp, Bytecount *size_out)
{
  Bytecount size;
  Extbyte *unidata = convert_multibyte_to_unicode_malloc (src, n, cp, &size);
  Intbyte *intdata;

  TO_INTERNAL_FORMAT (DATA, (unidata, size), MALLOC, (intdata, size),
		      Qmswindows_unicode);

  xfree (unidata);

  if (size_out)
    *size_out = size;

  return intdata;
}

/* Convert multibyte to Unicode according to the specified code page
   and append the results onto the specified Dynarr.  See above. */

void
convert_multibyte_to_unicode_dynarr (const Extbyte *src, Bytecount n,
				     int cp, unsigned_char_dynarr *dst)
{
  Bytecount nout = MultiByteToWideChar (cp, 0, src, n, 0, 0);
  void *outp;

  Dynarr_add_many (dst, 0, nout * sizeof (WCHAR));
  /* dynarr's buffer may be realloc()ed by call above, so access it after */
  outp = Dynarr_atp (dst, Dynarr_length (dst) - nout * sizeof (WCHAR));
  MultiByteToWideChar (cp, 0, src, n, (LPWSTR) outp, nout);
}

/* Convert MS Windows multibyte to Unicode. */

static Bytecount
mswindows_multibyte_to_unicode_convert (struct coding_stream *str,
					const unsigned char *src,
					unsigned_char_dynarr *dst,
					Bytecount n)
{
  unsigned char *new_src = (unsigned char *) src;
  int i;
  struct mswindows_multibyte_to_unicode_coding_stream *data =
    CODING_STREAM_TYPE_DATA (str, mswindows_multibyte_to_unicode);
  Bytecount orign = n;

  if (data->cp == 0)
    data->cp = determine_code_page (str->codesys);
  if (data->partial_byte_present)
    {
      new_src = alloca_array (unsigned char, n + 1);
      memcpy (new_src + 1, src, n);
      new_src[0] =
	(unsigned char) data->partial_byte;
      n++;
    }

  if (str->direction == CODING_DECODE)
    {
      for (i = n - 1; i >= 0; i--)
	{
	  if (!IsDBCSLeadByteEx (data->cp, new_src[i]))
	    break;
	}

      i++;

      for (; i < n; i++)
	{
	  if (IsDBCSLeadByteEx (data->cp, new_src[i]))
	    i++;
	}

      if (i > n)
	{
	  /* a char is split across the boundary */
	  data->partial_byte = new_src[n - 1];
	  data->partial_byte_present = 1;
	  n--;
	}
      else
	data->partial_byte_present = 0;

      convert_multibyte_to_unicode_dynarr ((Extbyte *) new_src, n, data->cp,
					   dst);
    }
  else
    {
      if (n & 1)
	{
	  /* a char is split across the boundary */
	  data->partial_byte = new_src[n - 1];
	  data->partial_byte_present = 1;
	  n--;
	}
      else
	data->partial_byte_present = 0;

      {
	int nout = WideCharToMultiByte (data->cp, WC_COMPOSITECHECK,
					(LPWSTR) new_src, n / sizeof (WCHAR),
					0, 0, "~", 0);
	void *outp;

	Dynarr_add_many (dst, 0, nout);
	/* dynarr's buffer may be realloc()ed by call above, so access it
           after */
	outp = Dynarr_atp (dst, Dynarr_length (dst) - nout);
	WideCharToMultiByte (data->cp, WC_COMPOSITECHECK, (LPWSTR) new_src,
			     n / sizeof (WCHAR),
			     (LPSTR) outp, nout, "~", 0);
      }
    }
  return orign;
}

static enum source_sink_type
mswindows_multibyte_to_unicode_conversion_end_type (Lisp_Object codesys)
{
  return DECODES_BYTE_TO_BYTE;
}


/************************************************************************/
/*                       MS Windows Multibyte methods                   */
/************************************************************************/

DEFINE_CODING_SYSTEM_TYPE (mswindows_multibyte);

struct mswindows_multibyte_coding_system
{
  Lisp_Object code_page;
  Lisp_Object locale;
};

struct mswindows_multibyte_coding_stream
{
  int dummy;
};

static const struct lrecord_description
  mswindows_multibyte_coding_system_description[] = {
  { XD_LISP_OBJECT,
      coding_system_data_offset +
	offsetof (struct mswindows_multibyte_coding_system, code_page) },
  { XD_LISP_OBJECT,
      coding_system_data_offset +
	offsetof (struct mswindows_multibyte_coding_system, locale) },
  { XD_END }
};

static Bytecount
mswindows_multibyte_convert (struct coding_stream *str,
			     const UExtbyte *src,
			     unsigned_char_dynarr *dst, Bytecount n)
{
  Bytecount orign = n;
  /* should never be called; is preprocessed away in the
     canonicalize method */
  abort ();
  return orign;
}

static void
mswindows_multibyte_init (Lisp_Object codesys)
{
  struct mswindows_multibyte_coding_system *data =
    XCODING_SYSTEM_TYPE_DATA (codesys, mswindows_multibyte);

  data->code_page = Qnil;
  data->locale = Qnil;
}

static void
mswindows_multibyte_mark (Lisp_Object codesys)
{
  struct mswindows_multibyte_coding_system *data =
    XCODING_SYSTEM_TYPE_DATA (codesys, mswindows_multibyte);

  mark_object (data->code_page);
  mark_object (data->locale);
}

static int
mswindows_multibyte_putprop (Lisp_Object codesys,
			     Lisp_Object key,
			     Lisp_Object value)
{
  struct mswindows_multibyte_coding_system *data =
    XCODING_SYSTEM_TYPE_DATA (codesys, mswindows_multibyte);

  if (EQ (key, Qcode_page))
    data->code_page = value;
  else if (EQ (key, Qlocale))
    data->locale = value;
  else
    return 0;
  return 1;
}

static Lisp_Object
mswindows_multibyte_getprop (Lisp_Object coding_system,
			     Lisp_Object prop)
{
  struct mswindows_multibyte_coding_system *data =
    XCODING_SYSTEM_TYPE_DATA (coding_system, mswindows_multibyte);

  if (EQ (prop, Qcode_page))
    return data->code_page;
  else if (EQ (prop, Qlocale))
    return data->locale;
  else
    return Qunbound;
}

/* Convert this coding system into the proper chain. */

static Lisp_Object
mswindows_multibyte_canonicalize (Lisp_Object codesys)
{
  struct mswindows_multibyte_coding_system *data =
    XCODING_SYSTEM_TYPE_DATA (codesys, mswindows_multibyte);
  Lisp_Object m2u;

  m2u =
    make_internal_coding_system
      (Qnil,
       "internal-mswindows-multibyte-to-unicode",
       Qmswindows_multibyte_to_unicode,
       Qnil, NILP (data->locale) ?
       list2 (Qcode_page, data->code_page) :
       list4 (Qcode_page, data->code_page, Qlocale, data->locale));

  return make_internal_coding_system (codesys,
				      "internal-mswindows-multibyte-chain",
				      Qchain, Qunbound,
				      list4 (Qchain,
					     list2 (m2u, Qmswindows_unicode),
					     Qcanonicalize_after_coding,
					     codesys));
}


void
syms_of_intl_win32 (void)
{
#ifdef MULE
  DEFSUBR (Fmswindows_set_current_locale);
  DEFSUBR (Fmswindows_current_locale);
  DEFSUBR (Fmswindows_user_default_locale);
  DEFSUBR (Fmswindows_system_default_locale);
  DEFSUBR (Fmswindows_locale_code_page);
  DEFSUBR (Fmswindows_locale_oem_code_page);
  DEFSUBR (Fmswindows_supported_locales);
  DEFSUBR (Fmswindows_charset_code_page);
  DEFSUBR (Fmswindows_set_charset_code_page);

#if 0
  DEFSUBR (Fmswindows_get_locale_info);
  DEFSUBR (Fmswindows_get_current_locale_id);
  DEFSUBR (Fmswindows_get_default_locale_id);
  DEFSUBR (Fmswindows_get_valid_locale_ids);
  DEFSUBR (Fmswindows_set_current_locale);

  DEFSUBR (Fmswindows_get_console_code_page);
  DEFSUBR (Fmswindows_set_console_code_page);
  DEFSUBR (Fmswindows_get_console_output_code_page);
  DEFSUBR (Fmswindows_set_console_output_code_page);
  DEFSUBR (Fmswindows_get_valid_code_pages);
  DEFSUBR (Fmswindows_get_code_page_charset);

  DEFSUBR (Fmswindows_get_valid_keyboard_layouts);
  DEFSUBR (Fmswindows_get_keyboard_layout);
  DEFSUBR (Fmswindows_set_keyboard_layout);
#endif
#endif /* MULE */

  DEFSYMBOL (Qmswindows_tstr);
  DEFSYMBOL (Qmswindows_multibyte);
  DEFSYMBOL (Qmswindows_multibyte_to_unicode);
  DEFSYMBOL (Qmswindows_unicode);
  DEFSYMBOL (Qmswindows_multibyte_system_default);

  DEFSYMBOL (Qansi);
  DEFSYMBOL (Qoem);
  DEFSYMBOL (Qmac);
  DEFSYMBOL (Qebcdic);
}

void
coding_system_type_create_intl_win32 (void)
{
  INITIALIZE_CODING_SYSTEM_TYPE_WITH_DATA
    (mswindows_multibyte_to_unicode,
     "mswindows-multibyte-to-unicode-coding-system-p");
  CODING_SYSTEM_HAS_METHOD (mswindows_multibyte_to_unicode, init);
  CODING_SYSTEM_HAS_METHOD (mswindows_multibyte_to_unicode, print);
  CODING_SYSTEM_HAS_METHOD (mswindows_multibyte_to_unicode, convert);
  CODING_SYSTEM_HAS_METHOD (mswindows_multibyte_to_unicode, getprop);
  CODING_SYSTEM_HAS_METHOD (mswindows_multibyte_to_unicode, putprop);
  CODING_SYSTEM_HAS_METHOD (mswindows_multibyte_to_unicode,
			    conversion_end_type);

  INITIALIZE_CODING_SYSTEM_TYPE_WITH_DATA
    (mswindows_multibyte,
     "mswindows-multibyte-coding-system-p");
  CODING_SYSTEM_HAS_METHOD (mswindows_multibyte, convert);
  CODING_SYSTEM_HAS_METHOD (mswindows_multibyte, init);
  CODING_SYSTEM_HAS_METHOD (mswindows_multibyte, mark);
  CODING_SYSTEM_HAS_METHOD (mswindows_multibyte, getprop);
  CODING_SYSTEM_HAS_METHOD (mswindows_multibyte, putprop);
  CODING_SYSTEM_HAS_METHOD (mswindows_multibyte, canonicalize);
}

void
reinit_coding_system_type_create_intl_win32 (void)
{
  REINITIALIZE_CODING_SYSTEM_TYPE (mswindows_multibyte_to_unicode);
  REINITIALIZE_CODING_SYSTEM_TYPE (mswindows_multibyte);
}

void
vars_of_intl_win32 (void)
{
#ifdef MULE
  Vmswindows_charset_code_page_table =
    make_lisp_hash_table (50, HASH_TABLE_NON_WEAK, HASH_TABLE_EQ);
  staticpro (&Vmswindows_charset_code_page_table);
#endif /* MULE */
}

static void
determine_if_using_unicode (void)
{
  if (XEUNICODE_P)
    Fdefine_coding_system_alias (Qmswindows_tstr, Qmswindows_unicode);
  else
    Fdefine_coding_system_alias (Qmswindows_tstr,
				 Qmswindows_multibyte_system_default);
}

void
complex_vars_of_intl_win32 (void)
{
  Fmake_coding_system
    (Qmswindows_unicode, Qunicode,
     build_msg_string ("MS Windows Unicode"),
     nconc2 (list4 (Qdocumentation,
		    build_msg_string (
"Converts to the Unicode encoding for Windows API calls.\n"
"This encoding is equivalent to standard UTF16, little-endian."
),
		    Qmnemonic, build_string ("MSW-U")),
	     list4 (Qtype, Qutf_16,
		    Qlittle_endian, Qt)));

#ifdef MULE
  /* Just temporarily.  This will get fixed in mule-msw-init.el. */
  Fdefine_coding_system_alias (Qmswindows_multibyte_system_default,
			       Qraw_text);
#else
  /* Not temporarily.  These may be referenced by Lisp code so we need to
     define them. */
  Fdefine_coding_system_alias (Qmswindows_multibyte,
			       Qraw_text);
  Fdefine_coding_system_alias (Qmswindows_multibyte_system_default,
			       Qraw_text);
  Fdefine_coding_system_alias (intern ("mswindows-multibyte-user-default"),
			       Qraw_text);
  Fdefine_coding_system_alias (intern ("mswindows-multibyte-oem"),
			       Qraw_text);
  Fdefine_coding_system_alias (intern
			       ("mswindows-multibyte-oem-system-default"),
			       Qraw_text);
  Fdefine_coding_system_alias (intern ("mswindows-multibyte-oem-user-default"),
			       Qraw_text);
#endif /* MULE */

  determine_if_using_unicode ();
}

void
init_intl_win32 (void)
{
#ifdef MULE
  set_current_lcid (GetUserDefaultLCID ());
#endif /* MULE */

  if (initialized)
    /* If not initialized, we also call this, but early -- see the
       previous function. */
    determine_if_using_unicode ();
}
