/* strftime - custom formatting of date and/or time
   Copyright (C) 1989, 1991, 1992 Free Software Foundation, Inc.

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
/* Synched up with: FSF 19.30. */

/* This file has been ... uhhhhh ... Mule-ized.  Yeah.

   (Everything here is external format.  This is DANGEROUS and
   data-lossy, but fixing it is too much of a bother now.) --ben */

/* Note: this version of strftime lacks locale support,
   but it is standalone.

   Performs `%' substitutions similar to those in printf.  Except
   where noted, substituted fields have a fixed size; numeric fields are
   padded if necessary.  Padding is with zeros by default; for fields
   that display a single number, padding can be changed or inhibited by
   following the `%' with one of the modifiers described below.  Unknown
   field specifiers are copied as normal characters.  All other
   characters are copied to the output without change.

   Supports a superset of the ANSI C field specifiers.

   Literal character fields:
   %	%
   n	newline
   t	tab

   Numeric modifiers (a nonstandard extension):
   -	do not pad the field
   _	pad the field with spaces

   Time fields:
   %H	hour (00..23)
   %I	hour (01..12)
   %k	hour ( 0..23)
   %l	hour ( 1..12)
   %M	minute (00..59)
   %p	locale's AM or PM
   %r	time, 12-hour (hh:mm:ss [AP]M)
   %R	time, 24-hour (hh:mm)
   %s	time in seconds since 00:00:00, Jan 1, 1970 (a nonstandard extension)
   %S	second (00..61)
   %T	time, 24-hour (hh:mm:ss)
   %X	locale's time representation (%H:%M:%S)
   %z   time zone offset (e.g. +0530, -0800 etc)
   %Z	time zone (EDT), or nothing if no time zone is determinable

   Date fields:
   %a	locale's abbreviated weekday name (Sun..Sat)
   %A	locale's full weekday name, variable length (Sunday..Saturday)
   %b	locale's abbreviated month name (Jan..Dec)
   %B	locale's full month name, variable length (January..December)
   %c	locale's date and time (Sat Nov 04 12:02:33 EST 1989)
   %C	century (00..99)
   %d	day of month (01..31)
   %e	day of month ( 1..31)
   %D	date (mm/dd/yy)
   %G   year corresponding to the ISO 8601 week
   %g   Year of the ISO 8601 week within century (00 - 99)
   %h	same as %b
   %j	day of year (001..366)
   %m	month (01..12)
   %U	week number of year with Sunday as first day of week (00..53)
   %V   ISO 8601 week number (first week is the earliest one with Thu)
   %w	day of week (0..6)
   %W	week number of year with Monday as first day of week (00..53)
   %x	locale's date representation (mm/dd/yy)
   %y	last two digits of year (00..99)
   %Y	year (1970...)

   David MacKenzie <djm@gnu.ai.mit.edu> */

#ifdef HAVE_CONFIG_H
#include <config.h>
#include "lisp.h"
#endif

#include <stdio.h>
#include <sys/types.h>
#if defined(TM_IN_SYS_TIME) || (!defined(HAVE_TM_ZONE) && !defined(HAVE_TZNAME))
#include <sys/time.h>
#else
#include <time.h>
#endif

#ifndef STDC_HEADERS
time_t mktime ();
#endif

#if defined(WIN32_NATIVE) || defined(CYGWIN)
#include <time.h>
#else
#if defined(HAVE_TZNAME)
extern char *tzname[2];
#endif
#endif /* WIN32_NATIVE */

#ifdef emacs
#define strftime emacs_strftime
#endif

/* Types of padding for numbers in date and time. */
enum padding
{
  none, blank, zero
};

static char const* const days[] =
{
  "Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"
};

static char const * const months[] =
{
  "January", "February", "March", "April", "May", "June",
  "July", "August", "September", "October", "November", "December"
};

static char const * const roman_upper[] =
{
  "I", "II", "III", "IV", "V", "VI", "VII", "VIII", "IX", "X", "XI", "XII"
};

static char const * const roman_lower[] =
{
  "i", "ii", "iii", "iv", "v", "vi", "vii", "viii", "ix", "x", "xi", "xii"
};

/* Add character C to STRING and increment LENGTH,
   unless LENGTH would exceed MAX. */

#define add_char(c) do		\
{				\
  if (length + 1 <= max)	\
    string[length++] = (c);	\
} while (0)

/* Add a 2 digit number to STRING, padding if specified.
   Return the number of characters added, up to MAX. */

static int
add_num2 (char *string, int num, int max, enum padding pad)
{
  int top = num / 10;
  int length = 0;

  if (top == 0 && pad == blank)
    add_char (' ');
  else if (top != 0 || pad == zero)
    add_char (top + '0');
  add_char (num % 10 + '0');
  return length;
}

/* Add a 3 digit number to STRING, padding if specified.
   Return the number of characters added, up to MAX. */

static int
add_num3 (char *string, int num, int max, enum padding pad)
{
  int top = num / 100;
  int mid = (num - top * 100) / 10;
  int length = 0;

  if (top == 0 && pad == blank)
    add_char (' ');
  else if (top != 0 || pad == zero)
    add_char (top + '0');
  if (mid == 0 && top == 0 && pad == blank)
    add_char (' ');
  else if (mid != 0 || top != 0 || pad == zero)
    add_char (mid + '0');
  add_char (num % 10 + '0');
  return length;
}

/* Like strncpy except return the number of characters copied. */

static int
add_str (char *to, const char *from, int max)
{
  int i;

  for (i = 0; from[i] && i <= max; ++i)
    to[i] = from[i];
  return i;
}

static int
add_num_time_t (char *string, int max, time_t num)
{
  /* This buffer is large enough to hold the character representation
     (including the trailing NUL) of any unsigned decimal quantity
     whose binary representation fits in 128 bits.  */
  char buf[40];

  if (sizeof (num) > 16)
    ABORT ();
  sprintf (buf, "%lu", (unsigned long) num);
  return add_str (string, buf, max);
}

/* Return the week in the year of the time in TM, with the weeks
   starting on Sundays. */

static int
sun_week (const struct tm *tm)
{
  int dl;

  /* Set `dl' to the day in the year of the last day of the week previous
     to the one containing the day specified in TM.  If the day specified
     in TM is in the first week of the year, `dl' will be negative or 0.
     Otherwise, calculate the number of complete weeks before our week
     (dl / 7) and add any partial week at the start of the year (dl % 7). */
  dl = tm->tm_yday - tm->tm_wday;
  return dl <= 0 ? 0 : dl / 7 + (dl % 7 != 0);
}

/* Return the week in the year of the time in TM, with the weeks
   starting on Mondays. */

static int
mon_week (const struct tm *tm)
{
  int dl, wday;

  if (tm->tm_wday == 0)
    wday = 6;
  else
    wday = tm->tm_wday - 1;
  dl = tm->tm_yday - wday;
  return dl <= 0 ? 0 : dl / 7 + (dl % 7 != 0);
}

#ifndef __isleap
/* Nonzero if YEAR is a leap year (every 4 years,
   except every 100th isn't, and every 400th is).  */
# define __isleap(year)	\
  ((year) % 4 == 0 && ((year) % 100 != 0 || (year) % 400 == 0))
#endif

/* The number of days from the first day of the first ISO week of this
   year to the year day YDAY with week day WDAY.  ISO weeks start on
   Monday; the first ISO week has the year's first Thursday.  YDAY may
   be as small as YDAY_MINIMUM.  */
#define ISO_WEEK_START_WDAY 1 /* Monday */
#define ISO_WEEK1_WDAY 4 /* Thursday */
#define YDAY_MINIMUM (-366)
static int
iso_week_days (int yday, int wday)
{
  /* Add enough to the first operand of % to make it nonnegative.  */
  int big_enough_multiple_of_7 = (-YDAY_MINIMUM / 7 + 2) * 7;
  return (yday
	  - (yday - wday + ISO_WEEK1_WDAY + big_enough_multiple_of_7) % 7
	  + ISO_WEEK1_WDAY - ISO_WEEK_START_WDAY);
}

#if !defined(HAVE_TM_ZONE) && !defined(HAVE_TZNAME)
char *zone_name (const struct tm *tp);
char *
zone_name (const struct tm *tp)
{
  char *timezone ();
  struct timeval tv;
  struct timezone tz;

  gettimeofday (&tv, &tz);
  return timezone (tz.tz_minuteswest, tp->tm_isdst);
}
#endif

/* Format the time given in TM according to FORMAT, and put the
   results in STRING.
   Return the number of characters (not including terminating null)
   that were put into STRING, or 0 if the length would have
   exceeded MAX. */

size_t strftime (char *string, size_t max, const char *format,
		 const struct tm *tm);

size_t
strftime (char *string, size_t max, const char *fermat, const struct tm *tm)
{
  enum padding pad;		/* Type of padding to apply. */
  size_t length = 0;		/* Characters put in STRING so far. */

  for (; *fermat && length < max; ++fermat)
    {
      if (*fermat != '%')
	add_char (*fermat);
      else
	{
	  ++fermat;
	  /* Modifiers: */
	  if (*fermat == '-')
	    {
	      pad = none;
	      ++fermat;
	    }
	  else if (*fermat == '_')
	    {
	      pad = blank;
	      ++fermat;
	    }
	  else
	    pad = zero;

	  switch (*fermat)
	    {
	      /* Literal character fields: */
	    case 0:
	    case '%':
	      add_char ('%');
	      break;
	    case 'n':
	      add_char ('\n');
	      break;
	    case 't':
	      add_char ('\t');
	      break;
	    default:
	      add_char (*fermat);
	      break;

	      /* Time fields: */
	    case 'H':
	    case 'k':
	      length +=
		add_num2 (&string[length], tm->tm_hour, max - length,
			  *fermat == 'H' ? pad : blank);
	      break;
	    case 'I':
	    case 'l':
	      {
		int hour12;

		if (tm->tm_hour == 0)
		  hour12 = 12;
		else if (tm->tm_hour > 12)
		  hour12 = tm->tm_hour - 12;
		else
		  hour12 = tm->tm_hour;
		length +=
		  add_num2 (&string[length], hour12, max - length,
			    *fermat == 'I' ? pad : blank);
	      }
	      break;
	    case 'M':
	      length +=
		add_num2 (&string[length], tm->tm_min, max - length, pad);
	      break;
	    case 'p':
	      if (tm->tm_hour < 12)
		add_char ('A');
	      else
		add_char ('P');
	      add_char ('M');
	      break;
	    case 'r':
	      length +=
		strftime (&string[length], max - length, "%I:%M:%S %p", tm);
	      break;
	    case 'R':
	      length +=
		strftime (&string[length], max - length, "%H:%M", tm);
	      break;

	    case 's':
	      {
		struct tm writable_tm;
		writable_tm = *tm;
		length += add_num_time_t (&string[length], max - length,
					  mktime (&writable_tm));
	      }
	      break;

	    case 'S':
	      length +=
		add_num2 (&string[length], tm->tm_sec, max - length, pad);
	      break;
	    case 'T':
	      length +=
		strftime (&string[length], max - length, "%H:%M:%S", tm);
	      break;
              
            case 'V':
            case 'g':
            case 'G':
              {
                int year = tm->tm_year + 1900;
                int ndays = iso_week_days (tm->tm_yday, tm->tm_wday);

                if (ndays < 0)
                  {
                    /* This ISO week belongs to the previous year.  */
                    year--;
                    ndays =
                      iso_week_days (tm->tm_yday + (365 + __isleap (year)),
                                     tm->tm_wday);
                  }
                else
                  {
                    int d =
                      iso_week_days (tm->tm_yday - (365 + __isleap (year)),
                                     tm->tm_wday);
                    if (0 <= d)
                      {
                        /* This ISO week belongs to the next year.  */
                        year++;
                        ndays = d;
                      }
                  }

                switch (*fermat)
                  {
                    /*
                      #### FIXME
                      We really can't assume 1000 <= year <= 9999 
                      once time_t gets beyond 32 bits, but it's true
                      of the rest of the code here so get with the
                      program
                    */
                  case 'g':
                    length +=
                      add_num2 (&string[length], year % 100,
                                max - length, pad);
                    break;
                    
                  case 'G':
                    add_char (year / 1000 + '0');
                    length += add_num3 (&string[length], year % 1000,
                                        max - length, zero);
                    break;
                    
                  default:
                    length +=
                      add_num2 (&string[length], ndays / 7 + 1,
                                max - length, pad);
                    break;
                  }
              }
              break;
	    case 'X':
	      length +=
		strftime (&string[length], max - length, "%H:%M:%S", tm);
	      break;
            case 'z':
              {
                /*
                  #### FIXME: could use tm->tm_gmtoff if present. Since
                  the other code in xemacs does not do so we follow the
                  leaders (and don't add a autoconf macro to detect
                  its presence). 
                */
                long int offset;
                long int minutes;
                struct tm lt, *ut;
                time_t utc;

                lt = *tm;
                utc = mktime(&lt);
                ut = gmtime(&utc);
                /* assume that tm is valid so the others will be too! */
                assert( utc != (time_t) -1 && ut != NULL );
                
                /* tm diff code below is based on mktime.c, glibc 2.3.2 */
                {
                  int lt4, ut4, lt100, ut100, lt400, ut400;
                  int intervening_leap_days, years, ndays;

                  lt4 = (lt.tm_year >> 2) + (1900 >> 2) -
                    ! (lt.tm_year & 3);
                  ut4 = (ut->tm_year >> 2) + (1900 >> 2) -
                    ! (ut->tm_year & 3);
                  lt100 = lt4 / 25 - (lt4 % 25 < 0);
                  ut100 = ut4 / 25 - (ut4 % 25 < 0);
                  lt400 = lt100 >> 2;
                  ut400 = ut100 >> 2;
                  intervening_leap_days =
                    (lt4 - ut4) - (lt100 - ut100) + (lt400 - ut400);
                  years = lt.tm_year - ut->tm_year;
                  ndays = (365 * years + intervening_leap_days
                          + (lt.tm_yday - ut->tm_yday));
                  offset = (60 * (60 * (24 * ndays
					+ (lt.tm_hour - ut->tm_hour))
                                 + (lt.tm_min - ut->tm_min))
                           + (lt.tm_sec - ut->tm_sec));
                }

                minutes = offset / ( offset < 0 ? -60 : 60 );

                add_char ((offset < 0 ? '-' : '+'));
                
                if ( minutes / 600 != 0 )
                  add_char (minutes / 600 + '0');
                else if ( pad != none )
                  add_char ((pad == zero ? '0' : ' '));
                
                length +=
                  add_num3 (&string[length],
                            ((minutes / 60 ) % 10) * 100 + (minutes % 60),
                            max - length, pad);
                break;
              }
	    case 'Z':
#ifdef HAVE_TM_ZONE
	      length += add_str (&string[length], tm->tm_zone, max - length);
#else
#ifdef HAVE_TZNAME
	      if (tm->tm_isdst && tzname[1] && *tzname[1])
		length += add_str (&string[length], tzname[1], max - length);
	      else
		length += add_str (&string[length], tzname[0], max - length);
#else
	      length += add_str (&string[length], zone_name (tm), max - length);
#endif
#endif
	      break;

	      /* Date fields: */
	    case 'a':
	      add_char (days[tm->tm_wday][0]);
	      add_char (days[tm->tm_wday][1]);
	      add_char (days[tm->tm_wday][2]);
	      break;
	    case 'A':
	      length +=
		add_str (&string[length], days[tm->tm_wday], max - length);
	      break;
	    case 'b':
	    case 'h':
	      add_char (months[tm->tm_mon][0]);
	      add_char (months[tm->tm_mon][1]);
	      add_char (months[tm->tm_mon][2]);
	      break;
	    case 'B':
	      length +=
		add_str (&string[length], months[tm->tm_mon], max - length);
	      break;
	    case 'c':
	      length +=
		strftime (&string[length], max - length,
			  "%a %b %d %H:%M:%S %Z %Y", tm);
	      break;
	    case 'C':
	      length +=
		add_num2 (&string[length], (tm->tm_year + 1900) / 100,
			  max - length, pad);
	      break;
	    case 'd':
	      length +=
		add_num2 (&string[length], tm->tm_mday, max - length, pad);
	      break;
	    case 'e':
	      length +=
		add_num2 (&string[length], tm->tm_mday, max - length, blank);
	      break;
	    case 'D':
	      length +=
		strftime (&string[length], max - length, "%m/%d/%y", tm);
	      break;
	    case 'j':
	      length +=
		add_num3 (&string[length], tm->tm_yday + 1, max - length, pad);
	      break;
	    case 'm':
	      length +=
		add_num2 (&string[length], tm->tm_mon + 1, max - length, pad);
	      break;
	    case 'U':
	      length +=
		add_num2 (&string[length], sun_week (tm), max - length, pad);
	      break;
	    case 'w':
	      add_char (tm->tm_wday + '0');
	      break;
	    case 'W':
	      length +=
		add_num2 (&string[length], mon_week (tm), max - length, pad);
	      break;
	    case 'x':
	      length +=
		strftime (&string[length], max - length, "%m/%d/%y", tm);
	      break;
	    case 'y':
	      length +=
		add_num2 (&string[length], tm->tm_year % 100,
			  max - length, pad);
	      break;
	    case 'Y':
	      add_char ((tm->tm_year + 1900) / 1000 + '0');
	      length +=
		add_num3 (&string[length],
			  (1900 + tm->tm_year) % 1000, max - length, zero);
	      break;
	    case '\xe6':
	      length +=
		add_str (&string[length], roman_lower[tm->tm_mon],
			 max - length);
	      break;
	    case '\xC6':
	      length +=
		add_str (&string[length], roman_upper[tm->tm_mon],
			 max - length);
	      break;
	    }
	}
    }
  add_char (0);
  return length - 1;
}
