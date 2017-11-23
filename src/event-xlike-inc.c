/* Common code between X and GTK -- event-related.
   Copyright (C) 1991-5, 1997 Free Software Foundation, Inc.
   Copyright (C) 1995 Sun Microsystems, Inc.
   Copyright (C) 1996, 2001, 2002, 2003, 2005, 2010 Ben Wing.

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

/* Synched up with: Not in FSF. */

/* Before including this file, you need to define either THIS_IS_X or
   THIS_IS_GTK.  */

/* See comment at top of redisplay-xlike-inc.c for an explanation of
   how this file works. */

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
      if (gtk_events_pending ())
	return 1;
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
#ifdef THIS_IS_GTK
	  || gdk_events_pending ()
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

#if defined (THIS_IS_X) || !defined (__GDK_KEYS_H__)

#ifdef MULE

/* Use an appropriate map to Unicode within x_keysym_to_character. Arguments
   are evaluated multiple times.

   Breaks if an X11 keysym maps to zero in Unicode. */

#define USE_UNICODE_MAP(keysym, map)					\
  if (keysym >= FIRST_KNOWN_##map					\
      && (keysym < (FIRST_KNOWN_##map + countof (map)))			\
      && map[keysym - FIRST_KNOWN_##map ]) do				\
    {									\
      keysym -= FIRST_KNOWN_##map ;					\
      return make_char (buffer_unicode_to_ichar ((int) map[keysym],	\
					  /* @@#### need to get some sort \
					     of buffer to compute this off; \
					     only applies in the old-Mule \
					     world */			\
					  current_buffer,		\
					  CONVERR_SUCCEED));		\
    } while (0)

/* Maps to Unicode for X11 KeySyms, where we don't have a direct internal
   mapping based on a Mule character set, or whatever. Taken from Markus
   Kuhn's X11.keysyms--if you're ever comparing with that file, note the
   sequences of KeySyms often leave out entries, so you'll have to fill them
   in. Doesn't include support for Hangul, which it should, if the X11
   Hangul keysyms have ever been used anywhere.
*/

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

static UINT_16_BIT const HANGUL[] = 
  {
#define FIRST_KNOWN_HANGUL 0xEA1
    0x3131,     /* #x0EA1 Hangul_Kiyeog */
    0x3132,     /* #x0EA2 Hangul_SsangKiyeog */
    0x3133,     /* #x0EA3 Hangul_KiyeogSios */
    0x3134,     /* #x0EA4 Hangul_Nieun */
    0x3135,     /* #x0EA5 Hangul_NieunJieuj */
    0x3136,     /* #x0EA6 Hangul_NieunHieuh */
    0x3137,     /* #x0EA7 Hangul_Dikeud */
    0x3138,     /* #x0EA8 Hangul_SsangDikeud */
    0x3139,     /* #x0EA9 Hangul_Rieul */
    0x313a,     /* #x0EAA Hangul_RieulKiyeog */
    0x313b,     /* #x0EAB Hangul_RieulMieum */
    0x313c,     /* #x0EAC Hangul_RieulPieub */
    0x313d,     /* #x0EAD Hangul_RieulSios */
    0x313e,     /* #x0EAE Hangul_RieulTieut */
    0x313f,     /* #x0EAF Hangul_RieulPhieuf */
    0x3140,     /* #x0EB0 Hangul_RieulHieuh */
    0x3141,     /* #x0EB1 Hangul_Mieum */
    0x3142,     /* #x0EB2 Hangul_Pieub */
    0x3143,     /* #x0EB3 Hangul_SsangPieub */
    0x3144,     /* #x0EB4 Hangul_PieubSios */
    0x3145,     /* #x0EB5 Hangul_Sios */
    0x3146,     /* #x0EB6 Hangul_SsangSios */
    0x3147,     /* #x0EB7 Hangul_Ieung */
    0x3148,     /* #x0EB8 Hangul_Jieuj */
    0x3149,     /* #x0EB9 Hangul_SsangJieuj */
    0x314a,     /* #x0EBA Hangul_Cieuc */
    0x314b,     /* #x0EBB Hangul_Khieuq */
    0x314c,     /* #x0EBC Hangul_Tieut */
    0x314d,     /* #x0EBD Hangul_Phieuf */
    0x314e,     /* #x0EBE Hangul_Hieuh */
    0x314f,     /* #x0EBF Hangul_A */
    0x3150,     /* #x0EC0 Hangul_AE */
    0x3151,     /* #x0EC1 Hangul_YA */
    0x3152,     /* #x0EC2 Hangul_YAE */
    0x3153,     /* #x0EC3 Hangul_EO */
    0x3154,     /* #x0EC4 Hangul_E */
    0x3155,     /* #x0EC5 Hangul_YEO */
    0x3156,     /* #x0EC6 Hangul_YE */
    0x3157,     /* #x0EC7 Hangul_O */
    0x3158,     /* #x0EC8 Hangul_WA */
    0x3159,     /* #x0EC9 Hangul_WAE */
    0x315a,     /* #x0ECA Hangul_OE */
    0x315b,     /* #x0ECB Hangul_YO */
    0x315c,     /* #x0ECC Hangul_U */
    0x315d,     /* #x0ECD Hangul_WEO */
    0x315e,     /* #x0ECE Hangul_WE */
    0x315f,     /* #x0ECF Hangul_WI */
    0x3160,     /* #x0ED0 Hangul_YU */
    0x3161,     /* #x0ED1 Hangul_EU */
    0x3162,     /* #x0ED2 Hangul_YI */
    0x3163,     /* #x0ED3 Hangul_I */
    0x11a8,     /* #x0ED4 Hangul_J_Kiyeog */
    0x11a9,     /* #x0ED5 Hangul_J_SsangKiyeog */
    0x11aa,     /* #x0ED6 Hangul_J_KiyeogSios */
    0x11ab,     /* #x0ED7 Hangul_J_Nieun */
    0x11ac,     /* #x0ED8 Hangul_J_NieunJieuj */
    0x11ad,     /* #x0ED9 Hangul_J_NieunHieuh */
    0x11ae,     /* #x0EDA Hangul_J_Dikeud */
    0x11af,     /* #x0EDB Hangul_J_Rieul */
    0x11b0,     /* #x0EDC Hangul_J_RieulKiyeog */
    0x11b1,     /* #x0EDD Hangul_J_RieulMieum */
    0x11b2,     /* #x0EDE Hangul_J_RieulPieub */
    0x11b3,     /* #x0EDF Hangul_J_RieulSios */
    0x11b4,     /* #x0EE0 Hangul_J_RieulTieut */
    0x11b5,     /* #x0EE1 Hangul_J_RieulPhieuf */
    0x11b6,     /* #x0EE2 Hangul_J_RieulHieuh */
    0x11b7,     /* #x0EE3 Hangul_J_Mieum */
    0x11b8,     /* #x0EE4 Hangul_J_Pieub */
    0x11b9,     /* #x0EE5 Hangul_J_PieubSios */
    0x11ba,     /* #x0EE6 Hangul_J_Sios */
    0x11bb,     /* #x0EE7 Hangul_J_SsangSios */
    0x11bc,     /* #x0EE8 Hangul_J_Ieung */
    0x11bd,     /* #x0EE9 Hangul_J_Jieuj */
    0x11be,     /* #x0EEA Hangul_J_Cieuc */
    0x11bf,     /* #x0EEB Hangul_J_Khieuq */
    0x11c0,     /* #x0EEC Hangul_J_Tieut */
    0x11c1,     /* #x0EED Hangul_J_Phieuf */
    0x11c2,     /* #x0EEE Hangul_J_Hieuh */
    0x316d,     /* #x0EEF Hangul_RieulYeorinHieuh */
    0x3171,     /* #x0EF0 Hangul_SunkyeongeumMieum */
    0x3178,     /* #x0EF1 Hangul_SunkyeongeumPieub */
    0x317f,     /* #x0EF2 Hangul_PanSios */
    0x3181,     /* #x0EF3 Hangul_KkogjiDalrinIeung */
    0x3184,     /* #x0EF4 Hangul_SunkyeongeumPhieuf */
    0x3186,     /* #x0EF5 Hangul_YeorinHieuh */
    0x318d,     /* #x0EF6 Hangul_AraeA */
    0x318e,     /* #x0EF7 Hangul_AraeAE */
    0x11eb,     /* #x0EF8 Hangul_J_PanSios */
    0x11f0,     /* #x0EF9 Hangul_J_KkogjiDalrinIeung */
    0x11f9,     /* #x0EFA Hangul_J_YeorinHieuh */
    0x0000,     /* #x0EFB */
    0x0000,     /* #x0EFC */
    0x0000,     /* #x0EFD */
    0x0000,     /* #x0EFE */
    0x20a9,     /* #x0EFF Korean_Won */
  };

static UINT_16_BIT const ARMENIAN[] = 
  {
#define FIRST_KNOWN_ARMENIAN 0x14A1
    0x0000,     /* #x14A1 Armenian_eternity */
    0x0587,     /* #x14A2 Armenian_ligature_ew */
    0x0589,     /* #x14A3 Armenian_verjaket */
    0x0029,     /* #x14A4 Armenian_parenright */
    0x0028,     /* #x14A5 Armenian_parenleft */
    0x00bb,     /* #x14A6 Armenian_guillemotright */
    0x00ab,     /* #x14A7 Armenian_guillemotleft */
    0x2014,     /* #x14A8 Armenian_em_dash */
    0x002e,     /* #x14A9 Armenian_mijaket */
    0x055d,     /* #x14AA Armenian_but */
    0x002c,     /* #x14AB Armenian_comma */
    0x2013,     /* #x14AC Armenian_en_dash */
    0x058a,     /* #x14AD Armenian_yentamna */
    0x2026,     /* #x14AE Armenian_ellipsis */
    0x055c,     /* #x14AF Armenian_amanak */
    0x055b,     /* #x14B0 Armenian_shesht */
    0x055e,     /* #x14B1 Armenian_paruyk */
    0x0531,     /* #x14B2 Armenian_AYB */
    0x0561,     /* #x14B3 Armenian_ayb */
    0x0532,     /* #x14B4 Armenian_BEN */
    0x0562,     /* #x14B5 Armenian_ben */
    0x0533,     /* #x14B6 Armenian_GIM */
    0x0563,     /* #x14B7 Armenian_gim */
    0x0534,     /* #x14B8 Armenian_DA */
    0x0564,     /* #x14B9 Armenian_da */
    0x0535,     /* #x14BA Armenian_YECH */
    0x0565,     /* #x14BB Armenian_yech */
    0x0536,     /* #x14BC Armenian_ZA */
    0x0566,     /* #x14BD Armenian_za */
    0x0537,     /* #x14BE Armenian_E */
    0x0567,     /* #x14BF Armenian_e */
    0x0538,     /* #x14C0 Armenian_AT */
    0x0568,     /* #x14C1 Armenian_at */
    0x0539,     /* #x14C2 Armenian_TO */
    0x0569,     /* #x14C3 Armenian_to */
    0x053a,     /* #x14C4 Armenian_ZHE */
    0x056a,     /* #x14C5 Armenian_zhe */
    0x053b,     /* #x14C6 Armenian_INI */
    0x056b,     /* #x14C7 Armenian_ini */
    0x053c,     /* #x14C8 Armenian_LYUN */
    0x056c,     /* #x14C9 Armenian_lyun */
    0x053d,     /* #x14CA Armenian_KHE */
    0x056d,     /* #x14CB Armenian_khe */
    0x053e,     /* #x14CC Armenian_TSA */
    0x056e,     /* #x14CD Armenian_tsa */
    0x053f,     /* #x14CE Armenian_KEN */
    0x056f,     /* #x14CF Armenian_ken */
    0x0540,     /* #x14D0 Armenian_HO */
    0x0570,     /* #x14D1 Armenian_ho */
    0x0541,     /* #x14D2 Armenian_DZA */
    0x0571,     /* #x14D3 Armenian_dza */
    0x0542,     /* #x14D4 Armenian_GHAT */
    0x0572,     /* #x14D5 Armenian_ghat */
    0x0543,     /* #x14D6 Armenian_TCHE */
    0x0573,     /* #x14D7 Armenian_tche */
    0x0544,     /* #x14D8 Armenian_MEN */
    0x0574,     /* #x14D9 Armenian_men */
    0x0545,     /* #x14DA Armenian_HI */
    0x0575,     /* #x14DB Armenian_hi */
    0x0546,     /* #x14DC Armenian_NU */
    0x0576,     /* #x14DD Armenian_nu */
    0x0547,     /* #x14DE Armenian_SHA */
    0x0577,     /* #x14DF Armenian_sha */
    0x0548,     /* #x14E0 Armenian_VO */
    0x0578,     /* #x14E1 Armenian_vo */
    0x0549,     /* #x14E2 Armenian_CHA */
    0x0579,     /* #x14E3 Armenian_cha */
    0x054a,     /* #x14E4 Armenian_PE */
    0x057a,     /* #x14E5 Armenian_pe */
    0x054b,     /* #x14E6 Armenian_JE */
    0x057b,     /* #x14E7 Armenian_je */
    0x054c,     /* #x14E8 Armenian_RA */
    0x057c,     /* #x14E9 Armenian_ra */
    0x054d,     /* #x14EA Armenian_SE */
    0x057d,     /* #x14EB Armenian_se */
    0x054e,     /* #x14EC Armenian_VEV */
    0x057e,     /* #x14ED Armenian_vev */
    0x054f,     /* #x14EE Armenian_TYUN */
    0x057f,     /* #x14EF Armenian_tyun */
    0x0550,     /* #x14F0 Armenian_RE */
    0x0580,     /* #x14F1 Armenian_re */
    0x0551,     /* #x14F2 Armenian_TSO */
    0x0581,     /* #x14F3 Armenian_tso */
    0x0552,     /* #x14F4 Armenian_VYUN */
    0x0582,     /* #x14F5 Armenian_vyun */
    0x0553,     /* #x14F6 Armenian_PYUR */
    0x0583,     /* #x14F7 Armenian_pyur */
    0x0554,     /* #x14F8 Armenian_KE */
    0x0584,     /* #x14F9 Armenian_ke */
    0x0555,     /* #x14FA Armenian_O */
    0x0585,     /* #x14FB Armenian_o */
    0x0556,     /* #x14FC Armenian_FE */
    0x0586,     /* #x14FD Armenian_fe */
    0x055a,     /* #x14FE Armenian_apostrophe */
    0x00a7,     /* #x14FF Armenian_section_sign */
  };

static UINT_16_BIT const GEORGIAN[] = 
  {
#define FIRST_KNOWN_GEORGIAN 0x15D0
    0x10d0,     /* #x15D0 Georgian_an */
    0x10d1,     /* #x15D1 Georgian_ban */
    0x10d2,     /* #x15D2 Georgian_gan */
    0x10d3,     /* #x15D3 Georgian_don */
    0x10d4,     /* #x15D4 Georgian_en */
    0x10d5,     /* #x15D5 Georgian_vin */
    0x10d6,     /* #x15D6 Georgian_zen */
    0x10d7,     /* #x15D7 Georgian_tan */
    0x10d8,     /* #x15D8 Georgian_in */
    0x10d9,     /* #x15D9 Georgian_kan */
    0x10da,     /* #x15DA Georgian_las */
    0x10db,     /* #x15DB Georgian_man */
    0x10dc,     /* #x15DC Georgian_nar */
    0x10dd,     /* #x15DD Georgian_on */
    0x10de,     /* #x15DE Georgian_par */
    0x10df,     /* #x15DF Georgian_zhar */
    0x10e0,     /* #x15E0 Georgian_rae */
    0x10e1,     /* #x15E1 Georgian_san */
    0x10e2,     /* #x15E2 Georgian_tar */
    0x10e3,     /* #x15E3 Georgian_un */
    0x10e4,     /* #x15E4 Georgian_phar */
    0x10e5,     /* #x15E5 Georgian_khar */
    0x10e6,     /* #x15E6 Georgian_ghan */
    0x10e7,     /* #x15E7 Georgian_qar */
    0x10e8,     /* #x15E8 Georgian_shin */
    0x10e9,     /* #x15E9 Georgian_chin */
    0x10ea,     /* #x15EA Georgian_can */
    0x10eb,     /* #x15EB Georgian_jil */
    0x10ec,     /* #x15EC Georgian_cil */
    0x10ed,     /* #x15ED Georgian_char */
    0x10ee,     /* #x15EE Georgian_xan */
    0x10ef,     /* #x15EF Georgian_jhan */
    0x10f0,     /* #x15F0 Georgian_hae */
    0x10f1,     /* #x15F1 Georgian_he */
    0x10f2,     /* #x15F2 Georgian_hie */
    0x10f3,     /* #x15F3 Georgian_we */
    0x10f4,     /* #x15F4 Georgian_har */
    0x10f5,     /* #x15F5 Georgian_hoe */
    0x10f6,     /* #x15F6 Georgian_fi */
  };

static UINT_16_BIT const AZERI_ETC[] = 
  {
#define FIRST_KNOWN_AZERI_ETC 0x16A2
    0x0000,     /* #x16A2 Ccedillaabovedot */
    0x1e8a,     /* #x16A3 Xabovedot */
    0x0000,     /* #x16A4 */
    0x0000,     /* #x16A5 Qabovedot */
    0x012c,     /* #x16A6 Ibreve */
    0x0000,     /* #x16A7 IE */
    0x0000,     /* #x16A8 UO */
    0x01b5,     /* #x16A9 Zstroke */
    0x01e6,     /* #x16AA Gcaron */
    0x0000,     /* #x16AB */
    0x0000,     /* #x16AC */
    0x0000,     /* #x16AD */
    0x0000,     /* #x16AE */
    0x019f,     /* #x16AF Obarred */
    0x0000,     /* #x16B0 */
    0x0000,     /* #x16B1 */
    0x0000,     /* #x16B2 ccedillaabovedot */
    0x1e8b,     /* #x16B3 xabovedot */
    0x0000,     /* #x16B4 Ocaron */
    0x0000,     /* #x16B5 qabovedot */
    0x012d,     /* #x16B6 ibreve */
    0x0000,     /* #x16B7 ie */
    0x0000,     /* #x16B8 uo */
    0x01b6,     /* #x16B9 zstroke */
    0x01e7,     /* #x16BA gcaron */
    0x0000,     /* #x16BB */
    0x0000,     /* #x16BC */
    0x01d2,     /* #x16BD ocaron */
    0x0000,     /* #x16BE */
    0x0275,     /* #x16BF obarred */
    0x0000,     /* #x16C0 */
    0x0000,     /* #x16C1 */
    0x0000,     /* #x16C2 */
    0x0000,     /* #x16C3 */
    0x0000,     /* #x16C4 */
    0x0000,     /* #x16C5 */
    0x018f,     /* #x16C6 SCHWA */
    0x0000,     /* #x16C7 */
    0x0000,     /* #x16C8 */
    0x0000,     /* #x16C9 */
    0x0000,     /* #x16CA */
    0x0000,     /* #x16CB */
    0x0000,     /* #x16CC */
    0x0000,     /* #x16CD */
    0x0000,     /* #x16CE */
    0x0000,     /* #x16CF */
    0x0000,     /* #x16D0 */
    0x1e36,     /* #x16D1 Lbelowdot */
    0x0000,     /* #x16D2 Lstrokebelowdot */
    0x0000,     /* #x16D3 Gtilde */
    0x0000,     /* #x16D4 */
    0x0000,     /* #x16D5 */
    0x0000,     /* #x16D6 */
    0x0000,     /* #x16D7 */
    0x0000,     /* #x16D8 */
    0x0000,     /* #x16D9 */
    0x0000,     /* #x16DA */
    0x0000,     /* #x16DB */
    0x0000,     /* #x16DC */
    0x0000,     /* #x16DD */
    0x0000,     /* #x16DE */
    0x0000,     /* #x16DF */
    0x0000,     /* #x16E0 */
    0x1e37,     /* #x16E1 lbelowdot */
    0x0000,     /* #x16E2 lstrokebelowdot */
    0x0000,     /* #x16E3 gtilde */
    0x0000,     /* #x16E4 */
    0x0000,     /* #x16E5 */
    0x0000,     /* #x16E6 */
    0x0000,     /* #x16E7 */
    0x0000,     /* #x16E8 */
    0x0000,     /* #x16E9 */
    0x0000,     /* #x16EA */
    0x0000,     /* #x16EB */
    0x0000,     /* #x16EC */
    0x0000,     /* #x16ED */
    0x0000,     /* #x16EE */
    0x0000,     /* #x16EF */
    0x0000,     /* #x16F0 */
    0x0000,     /* #x16F1 */
    0x0000,     /* #x16F2 */
    0x0000,     /* #x16F3 */
    0x0000,     /* #x16F4 */
    0x0000,     /* #x16F5 */
    0x0259,     /* #x16F6 schwa */
  };

static UINT_16_BIT const VIETNAMESE[] = 
  {
#define FIRST_KNOWN_VIETNAMESE 0x1E9F
    0x0303,     /* #x1E9F combining_tilde */
    0x1ea0,     /* #x1EA0 Abelowdot */
    0x1ea1,     /* #x1EA1 abelowdot */
    0x1ea2,     /* #x1EA2 Ahook */
    0x1ea3,     /* #x1EA3 ahook */
    0x1ea4,     /* #x1EA4 Acircumflexacute */
    0x1ea5,     /* #x1EA5 acircumflexacute */
    0x1ea6,     /* #x1EA6 Acircumflexgrave */
    0x1ea7,     /* #x1EA7 acircumflexgrave */
    0x1ea8,     /* #x1EA8 Acircumflexhook */
    0x1ea9,     /* #x1EA9 acircumflexhook */
    0x1eaa,     /* #x1EAA Acircumflextilde */
    0x1eab,     /* #x1EAB acircumflextilde */
    0x1eac,     /* #x1EAC Acircumflexbelowdot */
    0x1ead,     /* #x1EAD acircumflexbelowdot */
    0x1eae,     /* #x1EAE Abreveacute */
    0x1eaf,     /* #x1EAF abreveacute */
    0x1eb0,     /* #x1EB0 Abrevegrave */
    0x1eb1,     /* #x1EB1 abrevegrave */
    0x1eb2,     /* #x1EB2 Abrevehook */
    0x1eb3,     /* #x1EB3 abrevehook */
    0x1eb4,     /* #x1EB4 Abrevetilde */
    0x1eb5,     /* #x1EB5 abrevetilde */
    0x1eb6,     /* #x1EB6 Abrevebelowdot */
    0x1eb7,     /* #x1EB7 abrevebelowdot */
    0x1eb8,     /* #x1EB8 Ebelowdot */
    0x1eb9,     /* #x1EB9 ebelowdot */
    0x1eba,     /* #x1EBA Ehook */
    0x1ebb,     /* #x1EBB ehook */
    0x1ebc,     /* #x1EBC Etilde */
    0x1ebd,     /* #x1EBD etilde */
    0x1ebe,     /* #x1EBE Ecircumflexacute */
    0x1ebf,     /* #x1EBF ecircumflexacute */
    0x1ec0,     /* #x1EC0 Ecircumflexgrave */
    0x1ec1,     /* #x1EC1 ecircumflexgrave */
    0x1ec2,     /* #x1EC2 Ecircumflexhook */
    0x1ec3,     /* #x1EC3 ecircumflexhook */
    0x1ec4,     /* #x1EC4 Ecircumflextilde */
    0x1ec5,     /* #x1EC5 ecircumflextilde */
    0x1ec6,     /* #x1EC6 Ecircumflexbelowdot */
    0x1ec7,     /* #x1EC7 ecircumflexbelowdot */
    0x1ec8,     /* #x1EC8 Ihook */
    0x1ec9,     /* #x1EC9 ihook */
    0x1eca,     /* #x1ECA Ibelowdot */
    0x1ecb,     /* #x1ECB ibelowdot */
    0x1ecc,     /* #x1ECC Obelowdot */
    0x1ecd,     /* #x1ECD obelowdot */
    0x1ece,     /* #x1ECE Ohook */
    0x1ecf,     /* #x1ECF ohook */
    0x1ed0,     /* #x1ED0 Ocircumflexacute */
    0x1ed1,     /* #x1ED1 ocircumflexacute */
    0x1ed2,     /* #x1ED2 Ocircumflexgrave */
    0x1ed3,     /* #x1ED3 ocircumflexgrave */
    0x1ed4,     /* #x1ED4 Ocircumflexhook */
    0x1ed5,     /* #x1ED5 ocircumflexhook */
    0x1ed6,     /* #x1ED6 Ocircumflextilde */
    0x1ed7,     /* #x1ED7 ocircumflextilde */
    0x1ed8,     /* #x1ED8 Ocircumflexbelowdot */
    0x1ed9,     /* #x1ED9 ocircumflexbelowdot */
    0x1eda,     /* #x1EDA Ohornacute */
    0x1edb,     /* #x1EDB ohornacute */
    0x1edc,     /* #x1EDC Ohorngrave */
    0x1edd,     /* #x1EDD ohorngrave */
    0x1ede,     /* #x1EDE Ohornhook */
    0x1edf,     /* #x1EDF ohornhook */
    0x1ee0,     /* #x1EE0 Ohorntilde */
    0x1ee1,     /* #x1EE1 ohorntilde */
    0x1ee2,     /* #x1EE2 Ohornbelowdot */
    0x1ee3,     /* #x1EE3 ohornbelowdot */
    0x1ee4,     /* #x1EE4 Ubelowdot */
    0x1ee5,     /* #x1EE5 ubelowdot */
    0x1ee6,     /* #x1EE6 Uhook */
    0x1ee7,     /* #x1EE7 uhook */
    0x1ee8,     /* #x1EE8 Uhornacute */
    0x1ee9,     /* #x1EE9 uhornacute */
    0x1eea,     /* #x1EEA Uhorngrave */
    0x1eeb,     /* #x1EEB uhorngrave */
    0x1eec,     /* #x1EEC Uhornhook */
    0x1eed,     /* #x1EED uhornhook */
    0x1eee,     /* #x1EEE Uhorntilde */
    0x1eef,     /* #x1EEF uhorntilde */
    0x1ef0,     /* #x1EF0 Uhornbelowdot */
    0x1ef1,     /* #x1EF1 uhornbelowdot */
    0x0300,     /* #x1EF2 combining_grave */
    0x0301,     /* #x1EF3 combining_acute */
    0x1ef4,     /* #x1EF4 Ybelowdot */
    0x1ef5,     /* #x1EF5 ybelowdot */
    0x1ef6,     /* #x1EF6 Yhook */
    0x1ef7,     /* #x1EF7 yhook */
    0x1ef8,     /* #x1EF8 Ytilde */
    0x1ef9,     /* #x1EF9 ytilde */
  
    0x01a0,     /* #x1EFA Ohorn */
    0x01a1,     /* #x1EFB ohorn */
    0x01af,     /* #x1EFC Uhorn */
    0x01b0,     /* #x1EFD uhorn */
  
    0x0309,     /* #x1EFE combining_hook */
    0x0323,     /* #x1EFF combining_belowdot */
  };
#endif /* MULE */

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
x_keysym_to_character (KeySym keysym)
#else
Lisp_Object
gtk_keysym_to_character (guint keysym)
#endif
{
#ifdef MULE
  Lisp_Object charset = Qzero;
  int code = 0;
#endif /* MULE */

  /* @@#### Add support for 0xFE?? and 0xFF?? keysyms
     Add support for KOI8-U extensions in the 0x06?? range

     See http://www.cl.cam.ac.uk/~mgk25/ucs/keysyms.txt
 */

  /* Markus Kuhn's spec says keysyms in the range #x01000100 to #x0110FFFF
     and only those should correspond directly to Unicode code points, in
     the range #x100-#x10FFFF; actual implementations can have the Latin 1
     code points do the same thing with keysyms
     #x01000000-#x01000100. */

#ifndef MULE
  if (keysym >= 0x01000000 && keysym <= 0x010000FF)
    return make_char (keysym & 0xFFFFFF);
#else
  if (keysym >= 0x01000000 && keysym <= 0x0110FFFF)
    return make_char (buffer_unicode_to_ichar
		      ((int) (keysym & 0xFFFFFF),
		       /* @@#### need to get some sort of buffer to compute
			  this off; only applies in the old-Mule world */
		       current_buffer, CONVERR_SUCCEED));
#endif /* not MULE */

  if ((keysym & 0xff) < 0xa0)
    return Qnil;

#ifdef MULE
  switch (keysym >> 8)
    {
    case 0: /* ASCII + Latin1 */
      charset = Vcharset_latin_iso8859_1;
      code = keysym & 0xff;
      break;
    case 1: /* Latin2 */
      charset = Vcharset_latin_iso8859_2;
      code = keysym & 0xff;
      break;
    case 2: /* Latin3 */
      charset = Vcharset_latin_iso8859_3;
      code = keysym & 0xff;
      break;
    case 3: /* Latin4 */
      charset = Vcharset_latin_iso8859_4;
      code = keysym & 0xff;
      break;
    case 4: /* Katakana */
      charset = Vcharset_katakana_jisx0201;
      if ((keysym & 0xff) > 0xa0)
	code = keysym & 0xff;
      break;
    case 5: /* Arabic */
      charset = Vcharset_arabic_iso8859_6;
      code = keysym & 0xff;
      break;
    case 6: /* Cyrillic */
      {
	USE_UNICODE_MAP (keysym, CYRILLIC);
	break;
      }
    case 7: /* Greek */
      {
	static UExtbyte const greek[] = /* 0xa0 - 0xff */
	{0x00, 0xb6, 0xb8, 0xb9, 0xba, 0xda, 0x00, 0xbc,
	 0xbe, 0xdb, 0x00, 0xbf, 0x00, 0x00, 0xb5, 0xaf,
	 0x00, 0xdc, 0xdd, 0xde, 0xdf, 0xfa, 0xc0, 0xfc,
	 0xfd, 0xfb, 0xe0, 0xfe, 0x00, 0x00, 0x00, 0x00,
	 0x00, 0xc1, 0xc2, 0xc3, 0xc4, 0xc5, 0xc6, 0xc7,
	 0xc8, 0xc9, 0xca, 0xcb, 0xcc, 0xcd, 0xce, 0xcf,
	 0xd0, 0xd1, 0xd3, 0x00, 0xd4, 0xd5, 0xd6, 0xd7,
	 0xd8, 0xd9, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
	 0x00, 0xe1, 0xe2, 0xe3, 0xe4, 0xe5, 0xe6, 0xe7,
	 0xe8, 0xe9, 0xea, 0xeb, 0xec, 0xed, 0xee, 0xef,
	 0xf0, 0xf1, 0xf3, 0xf2, 0xf4, 0xf5, 0xf6, 0xf7,
	 0xf8, 0xf9, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00};
	charset = Vcharset_greek_iso8859_7;
	code = greek[(keysym & 0x7f) - 0x20];
	break;
      }
    case 8: 
      USE_UNICODE_MAP (keysym, TECHNICAL);
      break;
    case 9: 
      USE_UNICODE_MAP (keysym, SPECIAL);
      break;
    case 10:
      USE_UNICODE_MAP (keysym, PUBLISHING);
      break;
    case 11:
      USE_UNICODE_MAP (keysym, APL);
      break;
    case 12: /* Hebrew */
      charset = Vcharset_hebrew_iso8859_8;
      code = keysym & 0xff;
      break;
    case 13: /* Thai */
      /* #### This needs to deal with character composition.
	 Are you sure we can't leave it to the X server? */
      charset = Vcharset_thai_tis620;
      code = keysym & 0xff;
      break;
    case 14: /* Korean Hangul. */
      USE_UNICODE_MAP (keysym, HANGUL);
      break;
    case 18: /* Latin 8 - ISO8859-14. */
      charset = Ffind_charset (intern ("latin-iso8859-14"));
      code = keysym & 0xff;
      break;
    case 19: /* Latin 9 - ISO8859-15. */
      charset = Vcharset_latin_iso8859_15;
      code = keysym & 0xff;
      break;
    case 20: /* Armenian. */
      USE_UNICODE_MAP (keysym, ARMENIAN);
      break;
    case 21: /* Georgian. */
      USE_UNICODE_MAP (keysym, GEORGIAN);
      break;
    case 22: /* Azeri (and other Turkic or Caucasian languages of ex-USSR) */
      USE_UNICODE_MAP (keysym, AZERI_ETC);
      break;
    case 30: /* Vietnamese */
      USE_UNICODE_MAP (keysym, VIETNAMESE);
      break;
    case 32: /* Currency. The lower sixteen bits of these keysyms happily
		correspond exactly to the Unicode code points of the
		associated characters */
      return make_char (buffer_unicode_to_ichar
			((int) (keysym & 0xffff),
			 /* @@#### need to get some sort of buffer to
			    compute this off; only applies in the old-Mule
			    world */
			 current_buffer, CONVERR_SUCCEED));

/* @@#### Support me!

   Actually, these are somewhat already supported by x-init.el/x-compose.el,
   but only acute, grave, circum(flex), cedilla, diaeresis, tilde.  We
   should try to eliminate that code and use general Unicode support for
   converting to precomposed sequences.

0xfe50   U0300   f   # dead_grave
0xfe51   U0301   f   # dead_acute
0xfe52   U0302   f   # dead_circumflex
0xfe53   U0303   f   # dead_tilde
0xfe54   U0304   f   # dead_macron
0xfe55   U0306   f   # dead_breve
0xfe56   U0307   f   # dead_abovedot
0xfe57   U0308   f   # dead_diaeresis
0xfe58   U030a   f   # dead_abovering
0xfe59   U030b   f   # dead_doubleacute
0xfe5a   U030c   f   # dead_caron
0xfe5b   U0327   f   # dead_cedilla
0xfe5c   U0328   f   # dead_ogonek
0xfe5d   U0345   f   # dead_iota
0xfe5e   U3099   f   # dead_voiced_sound
0xfe5f   U309a   f   # dead_semivoiced_sound
0xfe60   U0323   f   # dead_belowdot
0xfe61   U0309   f   # dead_hook
0xfe62   U031b   f   # dead_horn

What about these?  We don't have to convert these to ASCII but make sure we
Handle all of the KP-foo things and get them to behave like plain foo when
KP-foo isn't bound (which includes self-inserting the associated character
if necessary). DOCUMENT the existing system that does this.

0xff08   U0008   f   # BackSpace	/- back space, back char -/
0xff09   U0009   f   # Tab
0xff0a   U000a   f   # Linefeed	/- Linefeed, LF -/
0xff0b   U000b   f   # Clear
0xff0d   U000d   f   # Return	/- Return, enter -/
0xff13   U0013   f   # Pause	/- Pause, hold -/
0xff14   U0014   f   # Scroll_Lock
0xff15   U0015   f   # Sys_Req
0xff1b   U001b   f   # Escape
0xff80   U0020   f   # KP_Space	/- space -/
0xff89   U0009   f   # KP_Tab
0xff8d   U000d   f   # KP_Enter	/- enter -/
0xffaa   U002a   f   # KP_Multiply
0xffab   U002b   f   # KP_Add
0xffac   U002c   f   # KP_Separator	/- separator, often comma -/
0xffad   U002d   f   # KP_Subtract
0xffae   U002e   f   # KP_Decimal
0xffaf   U002f   f   # KP_Divide
0xffb0   U0030   f   # KP_0
0xffb1   U0031   f   # KP_1
0xffb2   U0032   f   # KP_2
0xffb3   U0033   f   # KP_3
0xffb4   U0034   f   # KP_4
0xffb5   U0035   f   # KP_5
0xffb6   U0036   f   # KP_6
0xffb7   U0037   f   # KP_7
0xffb8   U0038   f   # KP_8
0xffb9   U0039   f   # KP_9
0xffbd   U003d   f   # KP_Equal	/- equals -/
*/
    default:
      break;
    }

  if (code == 0)
    return Qnil;

  /* #### Is this check on !NILP (charset) needed?  Maybe should be assert? */
  if (!NILP (charset))
    {
      /* First try to generate a unified character by converting through
	 Unicode, then try converting directly to an Ichar (only matters
	 when non-Unicode-internal, else we get same results both ways). */
      int ucs = charset_codepoint_to_unicode (charset, 0, code, CONVERR_FAIL);
      if (ucs >= 0)
	{
	  /* @@#### current_buffer dependency */
	  Ichar ich = buffer_unicode_to_ichar (ucs, current_buffer,
					       CONVERR_FAIL);
	  if (ich >= 0)
	    return make_char (ich);
	}
      else
	{
	  Ichar ich =
	    charset_codepoint_to_ichar (charset, 0, code, CONVERR_FAIL);
	  if (ich >= 0)
	    return make_char (ich);
	}
    }
  return Qnil;
#else /* not MULE */
  if (keysym >= 0x100)
    return Qnil;
  return make_char (keysym);
#endif /* (not) MULE */
}

#endif /* defined (THIS_IS_X) || !defined (__GDK_KEYS_H__) */
