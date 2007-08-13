/* Merge parameters into a termcap entry string.
   Copyright (C) 1985, 1987, 1992, 1993, 1994 Free Software Foundation, Inc.

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

/* config.h may rename various library functions such as malloc.  */
#ifdef emacs

#include <config.h>

#include <string.h>

#define realloc xrealloc
#define malloc xmalloc
#define free xfree
extern void *xmalloc (int size);
extern void *xrealloc (void *, int size);

#else /* emacs */

#ifdef STDC_HEADERS
#include <stdlib.h>
#include <string.h>
#else
extern char *malloc ();
extern char *realloc ();
#endif

#endif /* !emacs */

/* Assuming STRING is the value of a termcap string entry
   containing `%' constructs to expand parameters,
   merge in parameter values and store result in block OUTSTRING points to.
   LEN is the length of OUTSTRING.  If more space is needed,
   a block is allocated with `malloc'.

   The value returned is the address of the resulting string.
   This may be OUTSTRING or may be the address of a block got with `malloc'.
   In the latter case, the caller must free the block.

   The fourth and following args to tparam serve as the parameter values.  */

static char *tparam1 (CONST char *string, char *outstring, int len,
                      CONST char *up, CONST char *left, 
                      int *argp);

/* XEmacs: renamed this function because just tparam() conflicts with
   ncurses */
char *emacs_tparam (CONST char *string, char *outstring, int len, int arg0,
		    int arg1, int arg2, int arg3);
char *
emacs_tparam (CONST char *string, char *outstring, int len, int arg0,
	      int arg1, int arg2, int arg3)
{
  int arg[4];
  arg[0] = arg0;
  arg[1] = arg1;
  arg[2] = arg2;
  arg[3] = arg3;
  return tparam1 (string, outstring, len, 0, 0, arg);
}

CONST char *BC;
CONST char *UP;

static char tgoto_buf[50];

char *tgoto (CONST char *cm, int hpos, int vpos);
char *
tgoto (CONST char *cm, int hpos, int vpos)
{
  int args[2];
  if (!cm)
    return 0;
  args[0] = vpos;
  args[1] = hpos;
  return tparam1 (cm, tgoto_buf, 50, UP, BC, args);
}

static char *
tparam1 (CONST char *string, char *outstring, int len, CONST char *up,
	 CONST char *left, int *argp)
{
  int c;
  CONST char *p = string;
  char *op = outstring;
  char *outend;
  int outlen = 0;

  int tem;
  int *old_argp = argp;
  int doleft = 0;
  int doup = 0;

  outend = outstring + len;

  while (1)
    {
      /* If the buffer might be too short, make it bigger.  */
      if (op + 5 >= outend)
	{
	  char *new;
	  if (outlen == 0)
	    {
	      outlen = len + 40;
	      new = (char *) malloc (outlen);
	      outend += 40;
	      memcpy (new, outstring, op - outstring);
	    }
	  else
	    {
	      outend += outlen;
	      outlen *= 2;
	      new = (char *) realloc (outstring, outlen);
	    }
	  op += new - outstring;
	  outend += new - outstring;
	  outstring = new;
	}
      c = *p++;
      if (!c)
	break;
      if (c == '%')
	{
	  c = *p++;
	  tem = *argp;
	  switch (c)
	    {
	    case 'd':		/* %d means output in decimal.  */
	      if (tem < 10)
		goto onedigit;
	      if (tem < 100)
		goto twodigit;
	    case '3':		/* %3 means output in decimal, 3 digits.  */
	      if (tem > 999)
		{
		  *op++ = tem / 1000 + '0';
		  tem %= 1000;
		}
	      *op++ = tem / 100 + '0';
	    case '2':		/* %2 means output in decimal, 2 digits.  */
	    twodigit:
	      tem %= 100;
	      *op++ = tem / 10 + '0';
	    onedigit:
	      *op++ = tem % 10 + '0';
	      argp++;
	      break;

	    case 'C':
	      /* For c-100: print quotient of value by 96, if nonzero,
		 then do like %+.  */
	      if (tem >= 96)
		{
		  *op++ = tem / 96;
		  tem %= 96;
		}
	    case '+':		/* %+x means add character code of char x.  */
	      tem += *p++;
	    case '.':		/* %. means output as character.  */
	      if (left)
		{
		  /* If want to forbid output of 0 and \n and \t,
		     and this is one of them, increment it.  */
		  while (tem == 0 || tem == '\n' || tem == '\t')
		    {
		      tem++;
		      if (argp == old_argp)
			doup++, outend -= strlen (up);
		      else
			doleft++, outend -= strlen (left);
		    }
		}
	      *op++ = tem | 0200;
	    case 'f':		/* %f means discard next arg.  */
	      argp++;
	      break;

	    case 'b':		/* %b means back up one arg (and re-use it). */
	      argp--;
	      break;

	    case 'r':		/* %r means interchange following two args. */
	      argp[0] = argp[1];
	      argp[1] = tem;
	      old_argp++;
	      break;

	    case '>':		/* %>xy means if arg is > char code of x, */
	      if (argp[0] > *p++) /* then add char code of y to the arg, */
		argp[0] += *p;	/* and in any case don't output. */
	      p++;		/* Leave the arg to be output later. */
	      break;

	    case 'a':		/* %a means arithmetic. */
	      /* Next character says what operation.
		 Add or subtract either a constant or some other arg. */
	      /* First following character is + to add or - to subtract
		 or = to assign.  */
	      /* Next following char is 'p' and an arg spec
		 (0100 plus position of that arg relative to this one)
		 or 'c' and a constant stored in a character. */
	      tem = p[2] & 0177;
	      if (p[1] == 'p')
		tem = argp[tem - 0100];
	      if (p[0] == '-')
		argp[0] -= tem;
	      else if (p[0] == '+')
		argp[0] += tem;
	      else if (p[0] == '*')
		argp[0] *= tem;
	      else if (p[0] == '/')
		argp[0] /= tem;
	      else
		argp[0] = tem;

	      p += 3;
	      break;

	    case 'i':		/* %i means add one to arg, */
	      argp[0] ++;	/* and leave it to be output later. */
	      argp[1] ++;	/* Increment the following arg, too!  */
	      break;

	    case '%':		/* %% means output %; no arg. */
	      goto ordinary;

	    case 'n':		/* %n means xor each of next two args with 140. */
	      argp[0] ^= 0140;
	      argp[1] ^= 0140;
	      break;

	    case 'm':		/* %m means xor each of next two args with 177. */
	      argp[0] ^= 0177;
	      argp[1] ^= 0177;
	      break;

	    case 'B':		/* %B means express arg as BCD char code. */
	      argp[0] += 6 * (tem / 10);
	      break;

	    case 'D':		/* %D means weird Delta Data transformation. */
	      argp[0] -= 2 * (tem % 16);
	      break;
	    }
	}
      else
	/* Ordinary character in the argument string.  */
      ordinary:
	*op++ = c;
    }
  *op = 0;
  while (doup-- > 0)
    strcat (op, up);
  while (doleft-- > 0)
    strcat (op, left);
  return outstring;
}

#ifdef DEBUG

main (argc, argv)
     int argc;
     char **argv;
{
  char buf[50];
  int args[3];
  args[0] = atoi (argv[2]);
  args[1] = atoi (argv[3]);
  args[2] = atoi (argv[4]);
  tparam1 (argv[1], buf, "LEFT", "UP", args);
  printf ("%s\n", buf);
  return 0;
}

#endif /* DEBUG */
