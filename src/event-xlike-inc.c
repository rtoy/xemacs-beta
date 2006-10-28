/* Shared event code between X and GTK -- include file.
   Copyright (C) 1991-5, 1997 Free Software Foundation, Inc.
   Copyright (C) 1995 Sun Microsystems, Inc.
   Copyright (C) 1996, 2001, 2002, 2003 Ben Wing.

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

/* For some code it's reasonable to have only one copy and conditionalize
   at run-time.  For other code it isn't. #### Perhaps all code should be
   included here, not in event-xlike.c.  However, event-xlike.c is always
   X-specific, whereas the following code isn't, in the GTK case. */

static int
#ifdef THIS_IS_GTK
emacs_gtk_event_pending_p (int how_many)
#else
emacs_Xt_event_pending_p (int how_many)
#endif
{
  Lisp_Object event;
  int tick_count_val;

  /* If `how_many' is 0, then this function returns whether there are any
     X, timeout, or fd events pending (that is, whether
     emacs_Xt_next_event() would return immediately without blocking).

     If `how_many' is > 0, then this function returns whether there are
     that many *user generated* events available (keyboard, mouse click,
     etc.).  This also implies that emacs_Xt_next_event() would not block.
   */

  /* This function used to simply check whether there were any X events (or
     if user_p was 1, it iterated over all the pending X events using
     XCheckIfEvent(), looking for keystrokes and button events).  That
     worked in the old cheesoid event loop, which didn't go through
     XtAppDispatchEvent(), but it doesn't work any more -- X events may not
     result in anything.  For example, a button press in a blank part of
     the menubar appears as an X event but will not result in any Emacs
     events (a button press that activates the menubar results in an Emacs
     event through the stop_next_event mechanism).

     The only accurate way of determining whether these X events translate
     into Emacs events is to go ahead and dispatch them until there's
     something on the dispatch queue. */

  if (!how_many)
    {
      /* We're being asked for *ALL* events, not just user events. */

      /* (1) Any pending events in the dispatch queue? */
      if (!NILP (dispatch_event_queue))
	return 1;

      /* (2) Any TTY or process input available?

	 Note that formerly we just checked the value of XtAppPending() to
	 determine if there was file-desc input.  This doesn't work any
	 more with the signal_event_pipe; XtAppPending() will says "yes" in
	 this case but there isn't really any input.  So instead we keep
	 track of the file descriptors, and call select() ourselves.
	 Another way of fixing this problem is for the signal_event_pipe to
	 generate actual input in the form of an identity eval event or
	 something. (#### maybe this actually happens?) */

      if (poll_fds_for_input (non_fake_input_wait_mask))
	return 1;

#ifndef THIS_IS_GTK
      /* (3) Any timeout input available? */
      if (XtAppPending (Xt_app_con) & XtIMTimer)
	return 1;
#else
      /* #### Is there any way to do this in Gtk?  I don't think there
              is a 'peek' for events */
#endif
    }
  else
    {
      /* HOW_MANY > 0 */
      EVENT_CHAIN_LOOP (event, dispatch_event_queue)
	{
	  if (command_event_p (event))
	    {
	      how_many--;
	      if (how_many <= 0)
		return 1;
	    }
	}
    }

  /* XtAppPending() can be super-slow, esp. over a network connection.
     Quantify results have indicated that in some cases the call to
     detect_input_pending() completely dominates the running time of
     redisplay().  Fortunately, in a SIGIO world we can more quickly
     determine whether there are any X events: if an event has happened
     since the last time we checked, then a SIGIO will have happened.  On a
     machine with broken SIGIO, we'll still be in an OK state --
     quit_check_signal_tick_count will get ticked at least every 1/4
     second, so we'll be no more than that much behind reality. (In general
     it's OK if we erroneously report no input pending when input is
     actually pending() -- preemption is just a bit less efficient, that's
     all.  It's bad bad bad if you err the other way -- you've promised
     that `next-event' won't block but it actually will, and some action
     might get delayed until the next time you hit a key.)
     */

  if (!in_modal_loop)
    {
      /* quit_check_signal_tick_count is volatile so try to avoid race
	 conditions by using a temporary variable */
      tick_count_val = quit_check_signal_tick_count;
      if (last_quit_check_signal_tick_count != tick_count_val
#if !defined (THIS_IS_GTK) && (!defined (SIGIO) || defined (CYGWIN))
	  || (XtIMXEvent & XtAppPending (Xt_app_con))
#endif 
	  )
	{
	  last_quit_check_signal_tick_count = tick_count_val;

	  /* We need to drain the entire queue now -- if we only drain part of
	     it, we may later on end up with events actually pending but
	     detect_input_pending() returning false because there wasn't
	     another SIGIO. */
	  event_stream_drain_queue ();

	  if (!how_many)
	    return !NILP (dispatch_event_queue);

	  EVENT_CHAIN_LOOP (event, dispatch_event_queue)
	    {
	      if (command_event_p (event))
		{
		  how_many--;
		  if (how_many <= 0)
		    return 1;
		}
	    }

	  return 0;
	}
    }

  return 0;
}

#if defined(THIS_IS_X) || !defined(__GDK_KEYS_H__)

/* Use an appropriate map to Unicode within x_keysym_to_character. Arguments
   are evaluated multiple times.

   Breaks if an X11 keysym maps to zero in Unicode. */

#define USE_UNICODE_MAP(keysym, map)					\
  if (keysym >= FIRST_KNOWN_##map					\
      && (keysym < (FIRST_KNOWN_##map + countof(map)))			\
      && map[keysym - FIRST_KNOWN_##map ]) do				\
    {									\
      keysym -= FIRST_KNOWN_##map ;					\
      return Funicode_to_char(make_int(map[keysym]), Qnil);		\
    } while (0)

/* Maps to Unicode for X11 KeySyms, where we don't have a direct internal
   mapping based on a Mule character set, or whatever. Taken from Markus
   Kuhn's X11.keysyms--if you're ever comparing with that file, note the
   sequences of KeySyms often leave out entries, so you'll have to fill them
   in. Doesn't include support for Hangul, which it should, if the X11
   Hangul keysyms have ever been used anywhere.

   I'm not #ifdef'ing this based on wheter MULE is defined, because it's a
   matter of 324 bytes in a stripped executable, and I want the
   testing. :-P */

static UINT_16_BIT const TECHNICAL[] = 
  {
    0x23B7,	/* #x08A1 LEFT RADICAL	Technical */

#define FIRST_KNOWN_TECHNICAL 0x8A1

    0x0,	/* #x08A2 TOP LEFT RADICAL	Technical */
    0x0,	/* #x08A3 HORIZONTAL CONNECTOR	Technical */
    0x2320,	/* #x08A4 TOP INTEGRAL	Technical */
    0x2321,	/* #x08A5 BOTTOM INTEGRAL	Technical */
    0x0,	/* #x08A6 VERTICAL CONNECTOR	Technical */
    0x23A1,	/* #x08A7 TOP LEFT SQUARE BRACKET	Technical */
    0x23A3,	/* #x08A8 BOTTOM LEFT SQUARE BRACKET	Technical */
    0x23A4,	/* #x08A9 TOP RIGHT SQUARE BRACKET	Technical */
    0x23A6,	/* #x08AA BOTTOM RIGHT SQUARE BRACKET	Technical */
    0x239B,	/* #x08AB TOP LEFT PARENTHESIS	Technical */
    0x239D,	/* #x08AC BOTTOM LEFT PARENTHESIS	Technical */
    0x239E,	/* #x08AD TOP RIGHT PARENTHESIS	Technical */
    0x23A0,	/* #x08AE BOTTOM RIGHT PARENTHESIS	Technical */
    0x23A8,	/* #x08AF LEFT MIDDLE CURLY BRACE	Technical */
    0x23AC,	/* #x08B0 RIGHT MIDDLE CURLY BRACE	Technical */
    0x0,	/* #x08B1 TOP LEFT SUMMATION	Technical */
    0x0,	/* #x08B2 BOTTOM LEFT SUMMATION	Technical */
    0x0,	/* #x08B3 TOP VERTICAL SUMMATION CONNECTOR	Technical */
    0x0,	/* #x08B4 BOTTOM VERTICAL SUMMATION CONNECTOR	Technical */
    0x0,	/* #x08B5 TOP RIGHT SUMMATION	Technical */
    0x0,	/* #x08B6 BOTTOM RIGHT SUMMATION	Technical */
    0x0,	/* #x08B7 RIGHT MIDDLE SUMMATION	Technical */
    0x0,	/* #x08B8 */
    0x0,	/* #x08B9 */
    0x0,	/* #x08BA */
    0x0,	/* #x08BB */
    0x2264,	/* #x08BC LESS THAN OR EQUAL SIGN	Technical */
    0x2260,	/* #x08BD NOT EQUAL SIGN	Technical */
    0x2265,	/* #x08BE GREATER THAN OR EQUAL SIGN	Technical */
    0x222B,	/* #x08BF INTEGRAL	Technical */
    0x2234,	/* #x08C0 THEREFORE	Technical */
    0x221D,	/* #x08C1 VARIATION, PROPORTIONAL TO	Technical */
    0x221E,	/* #x08C2 INFINITY	Technical */
    0x0,	/* #x08C3 */
    0x0,	/* #x08C4 */
    0x2207,	/* #x08C5 NABLA, DEL	Technical */
    0x0,	/* #x08C6 */
    0x0,	/* #x08C7 */
    0x223C,	/* #x08C8 IS APPROXIMATE TO	Technical */
    0x2243,	/* #x08C9 SIMILAR OR EQUAL TO	Technical */
    0x0,	/* #x08CA */
    0x0,	/* #x08CB */
    0x0,	/* #x08CC */
    0x21D4,	/* #x08CD IF AND ONLY IF	Technical */
    0x21D2,	/* #x08CE IMPLIES	Technical */
    0x2261,	/* #x08CF IDENTICAL TO	Technical */
    0x0,	/* #x08D0 */
    0x0,	/* #x08D1 */
    0x0,	/* #x08D2 */
    0x0,	/* #x08D3 */
    0x0,	/* #x08D4 */
    0x0,	/* #x08D5 */
    0x221A,	/* #x08D6 RADICAL	Technical */
    0x0,	/* #x08D7 */
    0x0,	/* #x08D8 */
    0x0,	/* #x08D9 */
    0x2282,	/* #x08DA IS INCLUDED IN	Technical */
    0x2283,	/* #x08DB INCLUDES	Technical */
    0x2229,	/* #x08DC INTERSECTION	Technical */
    0x222A,	/* #x08DD UNION	Technical */
    0x2227,	/* #x08DE LOGICAL AND	Technical */
    0x2228,	/* #x08DF LOGICAL OR	Technical */
    0x0,	/* #x08E0 */
    0x0,	/* #x08E1 */
    0x0,	/* #x08E2 */
    0x0,	/* #x08E3 */
    0x0,	/* #x08E4 */
    0x0,	/* #x08E5 */
    0x0,	/* #x08E6 */
    0x0,	/* #x08E7 */
    0x0,	/* #x08E8 */
    0x0,	/* #x08E9 */
    0x0,	/* #x08Ea */
    0x0,	/* #x08Eb */
    0x0,	/* #x08Ec */
    0x0,	/* #x08Ed */
    0x0,	/* #x08Ee */
    0x2202,	/* #x08EF PARTIAL DERIVATIVE	Technical */
    0x0,	/* #x08F0 */
    0x0,	/* #x08F1 */
    0x0,	/* #x08F2 */
    0x0,	/* #x08F3 */
    0x0,	/* #x08F4 */
    0x0,	/* #x08F5 */
    0x0192,	/* #x08F6 FUNCTION	Technical */
    0x0,	/* #x08F7 */
    0x0,	/* #x08F8 */
    0x0,	/* #x08F9 */
    0x0,	/* #x08FA */
    0x2190,	/* #x08FB LEFT ARROW	Technical */
    0x2191,	/* #x08FC UPWARD ARROW	Technical */
    0x2192,	/* #x08FD RIGHT ARROW	Technical */
    0x2193,	/* #x08FE DOWNWARD ARROW	Technical */
  };

static UINT_16_BIT const SPECIAL[] = 
  {
    0x25C6,	/* #x09E0 SOLID DIAMOND	Special */

#define FIRST_KNOWN_SPECIAL 0x9E0

    0x2592,	/* #x09E1 CHECKERBOARD	Special */
    0x2409,	/* #x09E2 ``HT''	Special */
    0x240C,	/* #x09E3 ``FF''	Special */
    0x240D,	/* #x09E4 ``CR''	Special */
    0x240A,	/* #x09E5 ``LF''	Special */
    0x0,	/* #x09E6 */
    0x0,	/* #x09E7 */
    0x2424,	/* #x09E8 ``NL''	Special */
    0x240B,	/* #x09E9 ``VT''	Special */
    0x2518,	/* #x09EA LOWER-RIGHT CORNER	Special */
    0x2510,	/* #x09EB UPPER-RIGHT CORNER	Special */
    0x250C,	/* #x09EC UPPER-LEFT CORNER	Special */
    0x2514,	/* #x09ED LOWER-LEFT CORNER	Special */
    0x253C,	/* #x09EE CROSSING-LINES	Special */
    0x23BA,	/* #x09EF HORIZONTAL LINE, SCAN 1	Special */
    0x23BB,	/* #x09F0 HORIZONTAL LINE, SCAN 3	Special */
    0x2500,	/* #x09F1 HORIZONTAL LINE, SCAN 5	Special */
    0x23BC,	/* #x09F2 HORIZONTAL LINE, SCAN 7	Special */
    0x23BD,	/* #x09F3 HORIZONTAL LINE, SCAN 9	Special */
    0x251C,	/* #x09F4 LEFT ``T''	Special */
    0x2524,	/* #x09F5 RIGHT ``T''	Special */
    0x2534,	/* #x09F6 BOTTOM ``T''	Special */
    0x252C,	/* #x09F7 TOP ``T''	Special */
    0x2502	/* #x09F8 VERTICAL BAR	Special */
  };

static UINT_16_BIT const PUBLISHING[] = 
  {
    0x2003,	/* #x0AA1 EM SPACE	Publish */

#define FIRST_KNOWN_PUBLISHING 0xAA1

    0x2002,	/* #x0AA2 EN SPACE	Publish */
    0x2004,	/* #x0AA3 3/EM SPACE	Publish */
    0x2005,	/* #x0AA4 4/EM SPACE	Publish */
    0x2007,	/* #x0AA5 DIGIT SPACE	Publish */
    0x2008,	/* #x0AA6 PUNCTUATION SPACE	Publish */
    0x2009,	/* #x0AA7 THIN SPACE	Publish */
    0x200A,	/* #x0AA8 HAIR SPACE	Publish */
    0x2014,	/* #x0AA9 EM DASH	Publish */
    0x2013,	/* #x0AAA EN DASH	Publish */
    0x0,	/* #x0AAB */
    0x0,	/* #x0AAC SIGNIFICANT BLANK SYMBOL	Publish */
    0x0,	/* #x0AAD */
    0x2026,	/* #x0AAE ELLIPSIS	Publish */
    0x2025,	/* #x0AAF DOUBLE BASELINE DOT	Publish */
    0x2153,	/* #x0AB0 VULGAR FRACTION ONE THIRD	Publish */
    0x2154,	/* #x0AB1 VULGAR FRACTION TWO THIRDS	Publish */
    0x2155,	/* #x0AB2 VULGAR FRACTION ONE FIFTH	Publish */
    0x2156,	/* #x0AB3 VULGAR FRACTION TWO FIFTHS	Publish */
    0x2157,	/* #x0AB4 VULGAR FRACTION THREE FIFTHS	Publish */
    0x2158,	/* #x0AB5 VULGAR FRACTION FOUR FIFTHS	Publish */
    0x2159,	/* #x0AB6 VULGAR FRACTION ONE SIXTH	Publish */
    0x215A,	/* #x0AB7 VULGAR FRACTION FIVE SIXTHS	Publish */
    0x2105,	/* #x0AB8 CARE OF	Publish */
    0x0,	/* #x0AB9 */
    0x0,	/* #x0ABA */
    0x2012,	/* #x0ABB FIGURE DASH	Publish */
    0x3008,	/* #x0ABC LEFT ANGLE BRACKET	Publish */
    0x002E,	/* #x0ABD DECIMAL POINT	Publish */
    0x3009,	/* #x0ABE RIGHT ANGLE BRACKET	Publish */
    0x0,	/* #x0ABF MARKER	Publish */
    0x0,	/* #x0AC0 */
    0x0,	/* #x0AC1 */
    0x0,	/* #x0AC2 */
    0x215B,	/* #x0AC3 VULGAR FRACTION ONE EIGHTH	Publish */
    0x215C,	/* #x0AC4 VULGAR FRACTION THREE EIGHTHS	Publish */
    0x215D,	/* #x0AC5 VULGAR FRACTION FIVE EIGHTHS	Publish */
    0x215E,	/* #x0AC6 VULGAR FRACTION SEVEN EIGHTHS	Publish */
    0x0,	/* #x0AC7 */
    0x0,	/* #x0AC8 */
    0x2122,	/* #x0AC9 TRADEMARK SIGN	Publish */
    0x0,	/* #x0ACA SIGNATURE MARK	Publish */
    0x0,	/* #x0ACB TRADEMARK SIGN IN CIRCLE	Publish */
    0x0,	/* #x0ACC LEFT OPEN TRIANGLE	Publish */
    0x0,	/* #x0ACD RIGHT OPEN TRIANGLE	Publish */
    0x0,	/* #x0ACE EM OPEN CIRCLE	Publish */
    0x0,	/* #x0ACF EM OPEN RECTANGLE	Publish */
    0x2018,	/* #x0AD0 LEFT SINGLE QUOTATION MARK	Publish */
    0x2019,	/* #x0AD1 RIGHT SINGLE QUOTATION MARK	Publish */
    0x201C,	/* #x0AD2 LEFT DOUBLE QUOTATION MARK	Publish */
    0x201D,	/* #x0AD3 RIGHT DOUBLE QUOTATION MARK	Publish */
    0x211E,	/* #x0AD4 PRESCRIPTION, TAKE, RECIPE	Publish */
    0x0,	/* #x0AD5 */
    0x2032,	/* #x0AD6 MINUTES	Publish */
    0x2033,	/* #x0AD7 SECONDS	Publish */
    0x0,	/* #x0AD8 */
    0x271D,	/* #x0AD9 LATIN CROSS	Publish */
    0x0,	/* #x0ADA HEXAGRAM	Publish */
    0x0,	/* #x0ADB FILLED RECTANGLE BULLET	Publish */
    0x0,	/* #x0ADC FILLED LEFT TRIANGLE BULLET	Publish */
    0x0,	/* #x0ADD FILLED RIGHT TRIANGLE BULLET	Publish */
    0x0,	/* #x0ADE EM FILLED CIRCLE	Publish */
    0x0,	/* #x0ADF EM FILLED RECTANGLE	Publish */
    0x0,	/* #x0AE0 EN OPEN CIRCLE BULLET	Publish */
    0x0,	/* #x0AE1 EN OPEN SQUARE BULLET	Publish */
    0x0,	/* #x0AE2 OPEN RECTANGULAR BULLET	Publish */
    0x0,	/* #x0AE3 OPEN TRIANGULAR BULLET UP	Publish */
    0x0,	/* #x0AE4 OPEN TRIANGULAR BULLET DOWN	Publish */
    0x0,	/* #x0AE5 OPEN STAR	Publish */
    0x0,	/* #x0AE6 EN FILLED CIRCLE BULLET	Publish */
    0x0,	/* #x0AE7 EN FILLED SQUARE BULLET	Publish */
    0x0,	/* #x0AE8 FILLED TRIANGULAR BULLET UP	Publish */
    0x0,	/* #x0AE9 FILLED TRIANGULAR BULLET DOWN	Publish */
    0x0,	/* #x0AEA LEFT POINTER	Publish */
    0x0,	/* #x0AEB RIGHT POINTER	Publish */
    0x2663,	/* #x0AEC CLUB	Publish */
    0x2666,	/* #x0AED DIAMOND	Publish */
    0x2665,	/* #x0AEE HEART	Publish */
    0x0,	/* #x0AEF */
    0x2720,	/* #x0AF0 MALTESE CROSS	Publish */
    0x2020,	/* #x0AF1 DAGGER	Publish */
    0x2021,	/* #x0AF2 DOUBLE DAGGER	Publish */
    0x2713,	/* #x0AF3 CHECK MARK, TICK	Publish */
    0x2717,	/* #x0AF4 BALLOT CROSS	Publish */
    0x266F,	/* #x0AF5 MUSICAL SHARP	Publish */
    0x266D,	/* #x0AF6 MUSICAL FLAT	Publish */
    0x2642,	/* #x0AF7 MALE SYMBOL	Publish */
    0x2640,	/* #x0AF8 FEMALE SYMBOL	Publish */
    0x260E,	/* #x0AF9 TELEPHONE SYMBOL	Publish */
    0x2315,	/* #x0AFA TELEPHONE RECORDER SYMBOL	Publish */
    0x2117,	/* #x0AFB PHONOGRAPH COPYRIGHT SIGN	Publish */
    0x2038,	/* #x0AFC CARET	Publish */
    0x201A,	/* #x0AFD SINGLE LOW QUOTATION MARK	Publish */
    0x201E,	/* #x0AFE DOUBLE LOW QUOTATION MARK	Publish */
  };

static UINT_16_BIT const APL[] = 
  {
    0x22A5,	/* #x0BC2 DOWN TACK	APL */
#define FIRST_KNOWN_APL 0xBC2
    0x0,	/* #x0BC3 UP SHOE (CAP)	APL */
    0x230A,	/* #x0BC4 DOWN STILE	APL */
    0x0,	/* #x0BC5 */
    0x0,	/* #x0BC6 UNDERBAR	APL */
    0x0,	/* #x0BC7 */
    0x0,	/* #x0BC8 */
    0x0,	/* #x0BC9 */
    0x2218,	/* #x0BCA JOT	APL */
    0x0,	/* #x0BCB */
    0x2395,	/* #x0BCC QUAD	APL */
    0x0,	/* #x0BCD */
    0x22A4,	/* #x0BCE UP TACK	APL */
    0x25CB,	/* #x0BCF CIRCLE	APL */
    0x0,	/* #x0BD0 */
    0x0,	/* #x0BD1 */
    0x0,	/* #x0BD2 */
    0x2308,	/* #x0BD3 UP STILE	APL */
    0x0,	/* #x0BD4 */
    0x0,	/* #x0BD5 */
    0x0,	/* #x0BD6 DOWN SHOE (CUP)	APL */
    0x0,	/* #x0BD7 */
    0x0,	/* #x0BD8 RIGHT SHOE	APL */
    0x0,	/* #x0BD9 */
    0x0,	/* #x0BDA LEFT SHOE	APL */
    0x0,	/* #x0BDB */
    0x0,	/* #x0BDC */
    0x22A2,	/* #x0BDC LEFT TACK	APL */
    0x0,	/* #x0BDE */
    0x0,	/* #x0BDF */
    0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, /* 0x0BB0--0x0BBB */
    0x0, 0x0, 0x0, 0x0,
    0x22A3,	/* #x0BFC RIGHT TACK	APL */
  };

static UINT_16_BIT const CYRILLIC[] =
  {
    0x0452,	/* #x06A1 CYRILLIC SMALL LETTER DJE  */
#define FIRST_KNOWN_CYRILLIC 0x6A1
    0x0453,	/* #x06A2 CYRILLIC SMALL LETTER GJE  */
    0x0451,	/* #x06A3 CYRILLIC SMALL LETTER IO */
    0x0454,	/* #x06A4 CYRILLIC SMALL LETTER UKRAINIAN IE */
    0x0455,	/* #x06A5 CYRILLIC SMALL LETTER DZE */
    0x0456,	/* #x06A6 CYRILLIC SMALL LETTER BYELORUSSIAN-UKRAINIAN I */
    0x0457,	/* #x06A7 CYRILLIC SMALL LETTER YI */
    0x0458,	/* #x06A8 CYRILLIC SMALL LETTER JE */
    0x0459,	/* #x06A9 CYRILLIC SMALL LETTER LJE */
    0x045A,	/* #x06AA CYRILLIC SMALL LETTER NJE */
    0x045B,	/* #x06AB CYRILLIC SMALL LETTER TSHE */
    0x045C,	/* #x06AC CYRILLIC SMALL LETTER KJE */
    0x0491,	/* #x06AD CYRILLIC SMALL LETTER GHE WITH UPTURN */
    0x045E,	/* #x06AE CYRILLIC SMALL LETTER SHORT U */
    0x045F,	/* #x06AF CYRILLIC SMALL LETTER DZHE */
    0x2116,	/* #x06B0 NUMERO SIGN */
    0x0402,	/* #x06B1 CYRILLIC CAPITAL LETTER DJE */
    0x0403,	/* #x06B2 CYRILLIC CAPITAL LETTER GJE */
    0x0401,	/* #x06B3 CYRILLIC CAPITAL LETTER IO */
    0x0404,	/* #x06B4 CYRILLIC CAPITAL LETTER UKRAINIAN IE */
    0x0405,	/* #x06B5 CYRILLIC CAPITAL LETTER DZE */
    0x0406,	/* #x06B6 CYRILLIC CAPITAL LETTER BYELORUSSIAN-UKRAINIAN I */
    0x0407,	/* #x06B7 CYRILLIC CAPITAL LETTER YI */
    0x0408,	/* #x06B8 CYRILLIC CAPITAL LETTER JE */
    0x0409,	/* #x06B9 CYRILLIC CAPITAL LETTER LJE */
    0x040A,	/* #x06BA CYRILLIC CAPITAL LETTER NJE */
    0x040B,	/* #x06BB CYRILLIC CAPITAL LETTER TSHE */
    0x040C,	/* #x06BC CYRILLIC CAPITAL LETTER KJE */
    0x0490,	/* #x06BD CYRILLIC CAPITAL LETTER GHE WITH UPTURN */
    0x040E,	/* #x06BE CYRILLIC CAPITAL LETTER SHORT U */
    0x040F,	/* #x06BF CYRILLIC CAPITAL LETTER DZHE */
    0x044E,	/* #x06C0 CYRILLIC SMALL LETTER YU */
    0x0430,	/* #x06C1 CYRILLIC SMALL LETTER A */
    0x0431,	/* #x06C2 CYRILLIC SMALL LETTER BE */
    0x0446,	/* #x06C3 CYRILLIC SMALL LETTER TSE */
    0x0434,	/* #x06C4 CYRILLIC SMALL LETTER DE */
    0x0435,	/* #x06C5 CYRILLIC SMALL LETTER IE */
    0x0444,	/* #x06C6 CYRILLIC SMALL LETTER EF */
    0x0433,	/* #x06C7 CYRILLIC SMALL LETTER GHE */
    0x0445,	/* #x06C8 CYRILLIC SMALL LETTER HA */
    0x0438,	/* #x06C9 CYRILLIC SMALL LETTER I */
    0x0439,	/* #x06CA CYRILLIC SMALL LETTER SHORT I */
    0x043A,	/* #x06CB CYRILLIC SMALL LETTER KA */
    0x043B,	/* #x06CC CYRILLIC SMALL LETTER EL */
    0x043C,	/* #x06CD CYRILLIC SMALL LETTER EM */
    0x043D,	/* #x06CE CYRILLIC SMALL LETTER EN */
    0x043E,	/* #x06CF CYRILLIC SMALL LETTER O */
    0x043F,	/* #x06D0 CYRILLIC SMALL LETTER PE */
    0x044F,	/* #x06D1 CYRILLIC SMALL LETTER YA */
    0x0440,	/* #x06D2 CYRILLIC SMALL LETTER ER */
    0x0441,	/* #x06D3 CYRILLIC SMALL LETTER ES */
    0x0442,	/* #x06D4 CYRILLIC SMALL LETTER TE */
    0x0443,	/* #x06D5 CYRILLIC SMALL LETTER U */
    0x0436,	/* #x06D6 CYRILLIC SMALL LETTER ZHE */
    0x0432,	/* #x06D7 CYRILLIC SMALL LETTER VE */
    0x044C,	/* #x06D8 CYRILLIC SMALL LETTER SOFT SIGN */
    0x044B,	/* #x06D9 CYRILLIC SMALL LETTER YERU */
    0x0437,	/* #x06DA CYRILLIC SMALL LETTER ZE */
    0x0448,	/* #x06DB CYRILLIC SMALL LETTER SHA */
    0x044D,	/* #x06DC CYRILLIC SMALL LETTER E */
    0x0449,	/* #x06DD CYRILLIC SMALL LETTER SHCHA */
    0x0447,	/* #x06DE CYRILLIC SMALL LETTER CHE */
    0x044A,	/* #x06DF CYRILLIC SMALL LETTER HARD SIGN */
    0x042E,	/* #x06E0 CYRILLIC CAPITAL LETTER YU */
    0x0410,	/* #x06E1 CYRILLIC CAPITAL LETTER A */
    0x0411,	/* #x06E2 CYRILLIC CAPITAL LETTER BE */
    0x0426,	/* #x06E3 CYRILLIC CAPITAL LETTER TSE */
    0x0414,	/* #x06E4 CYRILLIC CAPITAL LETTER DE */
    0x0415,	/* #x06E5 CYRILLIC CAPITAL LETTER IE */
    0x0424,	/* #x06E6 CYRILLIC CAPITAL LETTER EF */
    0x0413,	/* #x06E7 CYRILLIC CAPITAL LETTER GHE */
    0x0425,	/* #x06E8 CYRILLIC CAPITAL LETTER HA */
    0x0418,	/* #x06E9 CYRILLIC CAPITAL LETTER I */
    0x0419,	/* #x06EA CYRILLIC CAPITAL LETTER SHORT I */
    0x041A,	/* #x06EB CYRILLIC CAPITAL LETTER KA */
    0x041B,	/* #x06EC CYRILLIC CAPITAL LETTER EL */
    0x041C,	/* #x06ED CYRILLIC CAPITAL LETTER EM */
    0x041D,	/* #x06EE CYRILLIC CAPITAL LETTER EN */
    0x041E,	/* #x06EF CYRILLIC CAPITAL LETTER O */
    0x041F,	/* #x06F0 CYRILLIC CAPITAL LETTER PE */
    0x042F,	/* #x06F1 CYRILLIC CAPITAL LETTER YA */
    0x0420,	/* #x06F2 CYRILLIC CAPITAL LETTER ER */
    0x0421,	/* #x06F3 CYRILLIC CAPITAL LETTER ES */
    0x0422,	/* #x06F4 CYRILLIC CAPITAL LETTER TE */
    0x0423,	/* #x06F5 CYRILLIC CAPITAL LETTER U */
    0x0416,	/* #x06F6 CYRILLIC CAPITAL LETTER ZHE */
    0x0412,	/* #x06F7 CYRILLIC CAPITAL LETTER VE */
    0x042C,	/* #x06F8 CYRILLIC CAPITAL LETTER SOFT SIGN */
    0x042B,	/* #x06F9 CYRILLIC CAPITAL LETTER YERU */
    0x0417,	/* #x06FA CYRILLIC CAPITAL LETTER ZE */
    0x0428,	/* #x06FB CYRILLIC CAPITAL LETTER SHA */
    0x042D,	/* #x06FC CYRILLIC CAPITAL LETTER E */
    0x0429,	/* #x06FD CYRILLIC CAPITAL LETTER SHCHA */
    0x0427,	/* #x06FE CYRILLIC CAPITAL LETTER CHE */
    0x042A,	/* #x06FF CYRILLIC CAPITAL LETTER HARD SIGN */
  };

/* For every key on the keyboard that has a known character correspondence,
   we define the character-of-keysym property of its XEmacs keysym, and make
   the default binding for the key be self-insert-command.

   The following magic is based on intimate knowledge of some of
   X11/keysymdef.h.  The keysym mappings defined by X11 are based on the
   iso8859 standards, except for Cyrillic and Greek.

   In a non-Mule world, a user can still have a multi-lingual editor, by
   doing (set-face-font "...-iso8859-2" (current-buffer)) for all their
   Latin-2 buffers, etc. and the X11 keysyms corresponding to characters in
   those character sets will still do the right thing (because of the
   make_char (code + 0x80) non-Mule case below.) Of course, X11 keysyms in
   other character sets will not do the right thing, because XEmacs won't
   support the right thing.

   This code is also called when a command lookup is about to fail, and the
   X11 platform code has worked out that it previously wasn't aware the
   keysym of that command could be generated by the user's keyboard; in that
   case, we bind its XEmacs keysym to self-insert-command if it has a
   character correspondence we know about, and tell the general event code
   that we've done so, so it can try the lookup again.

   Called from the GTK code because GTK 1 has no defined way of doing the
   same thing, and this works for it on X11. It should be moved back into
   event-Xt.c when and if the GTK port moves to GTK 2. */

#ifndef THIS_IS_GTK
static Lisp_Object
x_keysym_to_character(KeySym keysym)
#else
Lisp_Object
gtk_keysym_to_character(guint keysym)
#endif
{
  Lisp_Object charset = Qzero;
  int code = 0;

  /* Markus Kuhn's spec says keysyms in the range #x01000100 to #x0110FFFF
     and only those should correspond directly to Unicode code points, in
     the range #x100-#x10FFFF; actual implementations can have the Latin 1
     code points do the same thing with keysyms
     #x01000000-#x01000100. */

  if (keysym >= 0x01000000 && keysym <= 0x0110FFFF)
    return Funicode_to_char (make_int(keysym & 0xffffff), Qnil);

  if ((keysym & 0xff) < 0xa0)
    return Qnil;

  switch (keysym >> 8)
    {

#define USE_CHARSET(var,cs) \
  ((var) = charset_by_leading_byte (LEADING_BYTE_##cs))

    case 0: /* ASCII + Latin1 */
      USE_CHARSET (charset, LATIN_ISO8859_1);
      code = keysym & 0x7f;
      break;
    case 1: /* Latin2 */
      USE_CHARSET (charset, LATIN_ISO8859_2);
      code = keysym & 0x7f;
      break;
    case 2: /* Latin3 */
      USE_CHARSET (charset, LATIN_ISO8859_3);
      code = keysym & 0x7f;
      break;
    case 3: /* Latin4 */
      USE_CHARSET (charset, LATIN_ISO8859_4);
      code = keysym & 0x7f;
      break;
    case 4: /* Katakana */
      USE_CHARSET (charset, KATAKANA_JISX0201);
      if ((keysym & 0xff) > 0xa0)
	code = keysym & 0x7f;
      break;
    case 5: /* Arabic */
      USE_CHARSET (charset, ARABIC_ISO8859_6);
      code = keysym & 0x7f;
      break;
    case 6: /* Cyrillic */
      {
	USE_UNICODE_MAP(keysym, CYRILLIC);
	break;
      }
    case 7: /* Greek */
      {
	static UExtbyte const greek[] = /* 0x20 - 0x7f */
	{0x00, 0x36, 0x38, 0x39, 0x3a, 0x5a, 0x00, 0x3c,
	 0x3e, 0x5b, 0x00, 0x3f, 0x00, 0x00, 0x35, 0x2f,
	 0x00, 0x5c, 0x5d, 0x5e, 0x5f, 0x7a, 0x40, 0x7c,
	 0x7d, 0x7b, 0x60, 0x7e, 0x00, 0x00, 0x00, 0x00,
	 0x00, 0x41, 0x42, 0x43, 0x44, 0x45, 0x46, 0x47,
	 0x48, 0x49, 0x4a, 0x4b, 0x4c, 0x4d, 0x4e, 0x4f,
	 0x50, 0x51, 0x53, 0x00, 0x54, 0x55, 0x56, 0x57,
	 0x58, 0x59, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
	 0x00, 0x61, 0x62, 0x63, 0x64, 0x65, 0x66, 0x67,
	 0x68, 0x69, 0x6a, 0x6b, 0x6c, 0x6d, 0x6e, 0x6f,
	 0x70, 0x71, 0x73, 0x72, 0x74, 0x75, 0x76, 0x77,
	 0x78, 0x79, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00};
	USE_CHARSET (charset, GREEK_ISO8859_7);
	code = greek[(keysym & 0x7f) - 0x20];
	break;
      }
    case 8: 
      USE_UNICODE_MAP(keysym, TECHNICAL);
      break;
    case 9: 
      USE_UNICODE_MAP(keysym, SPECIAL);
      break;
    case 10:
      USE_UNICODE_MAP(keysym, PUBLISHING);
      break;
    case 11:
      USE_UNICODE_MAP(keysym, APL);
      break;
    case 12: /* Hebrew */
      USE_CHARSET (charset, HEBREW_ISO8859_8);
      code = keysym & 0x7f;
      break;
    case 13: /* Thai */
      /* #### This needs to deal with character composition.
                  Are you sure we can't leave it to the X server? */
      USE_CHARSET (charset, THAI_TIS620);
      code = keysym & 0x7f;
      break;
    case 14: /* Korean Hangul. Would like some information on whether this
		is worth doing--there don't appear to be any Korean keyboard
		layouts in the XKB data files. */
      break;

    case 19: /* Latin 9 - ISO8859-15. */
      USE_CHARSET (charset, LATIN_ISO8859_15);
      code = keysym & 0x7f;
      break;
    case 32: /* Currency. The lower sixteen bits of these keysyms happily
		correspond exactly to the Unicode code points of the
		associated characters */
      return Funicode_to_char(make_int(keysym & 0xffff), Qnil);
      break;
    default:
      break;
    }

  if (code == 0)
    return Qnil;

#ifdef MULE
  return make_char (make_ichar (charset, code, 0));
#else
  return make_char (code + 0x80);
#endif
}

#endif /* defined(THIS_IS_X) || !defined(__GDK_KEYS_H__) */
