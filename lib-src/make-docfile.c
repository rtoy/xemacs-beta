/* Generate doc-string file for XEmacs from source files.
   Copyright (C) 1985, 86, 92, 93, 94, 97, 1999, 2000, 2001
   Free Software Foundation, Inc.
   Copyright (C) 1995 Board of Trustees, University of Illinois.
   Copyright (C) 1998, 1999 J. Kean Johnston.
   Copyright (C) 2001, 2002, 2010 Ben Wing.

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

/* Synched up with: FSF 21.3. */

/* The arguments given to this program are all the C and Lisp source files
 of XEmacs.  .elc and .el and .c files are allowed.
 A .o or .obj file can also be specified; the .c file it was made from is used.
 This helps the makefile pass the correct list of files.

 The results, which go to standard output or to a file
 specified with -a or -o (-a to append, -o to start from nothing),
 are entries containing function or variable names and their documentation.
 Each entry starts with a ^_ character.
 Then comes F for a function or V for a variable.
 Then comes the function or variable name, terminated with a newline.
 Then comes the documentation for that function or variable.

 Added 19.15/20.1:  `-i site-packages' allow installer to dump extra packages
 without modifying Makefiles, etc.
 */

#include <config.h>
#include <sysfile.h>

#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>

#include "compiler.h"

/* XEmacs addition */
#define C_IDENTIFIER_CHAR_P(c)			\
 (('A' <= c && c <= 'Z') ||			\
  ('a' <= c && c <= 'z') ||			\
  ('0' <= c && c <= '9') ||			\
  (c == '_'))

static void put_filename (const char *filename);
static int scan_file (const char *filename);
static int read_c_string (FILE *, int, int);
static void write_c_args (FILE *out, const char *func, char *buf, int minargs,
			  int maxargs);
static int scan_c_file (const char *filename, const char *mode);
static void skip_white (FILE *);
static void read_lisp_symbol (FILE *, char *);
static int scan_lisp_file (const char *filename, const char *mode);

/* Stdio stream for output to the DOC file.  */
static FILE *outfile;

/* XEmacs addition */
enum
{
  el_file,
  elc_file,
  c_file
} Current_file_type;

/* Name this program was invoked with.  */
char *progname;

/* XEmacs addition: set to 1 if this was invoked by ellcc */
int ellcc = 0;

/* Print error message.  `s1' is printf control string, `s2' is arg for it. */

static void
error (const char *s1, const char *s2)
{
  fprintf (stderr, "%s: ", progname);
  fprintf (stderr, s1, s2);
  fprintf (stderr, "\n");
}

/* Print error message and exit.  */

static void
fatal (const char *s1, const char *s2)
{
  error (s1, s2);
  exit (1);
}

/* Like malloc but get fatal error if memory is exhausted.  */

static long *
xmalloc (unsigned int size)
{
  long *result = (long *) malloc (size);
  if (result == NULL)
    fatal ("virtual memory exhausted", 0);
  return result;
}

/* XEmacs addition */
static char *
next_extra_elc (char *extra_elcs)
{
  static FILE *fp = NULL;
  static char line_buf[BUFSIZ];
  char *p = line_buf+1;

  if (!fp)
    {
      if (!extra_elcs)
	return NULL;
      else if (!(fp = fopen (extra_elcs, READ_BINARY)))
	{
	  /* It is not an error if this file doesn't exist. */
	  /*fatal ("error opening site package file list", 0);*/
	  return NULL;
	}
      fgets (line_buf, BUFSIZ, fp);
    }

  do
    {
      if (!fgets (line_buf, BUFSIZ, fp))
	{
	  fclose (fp);
	  fp = NULL;
	  return NULL;
	}
      line_buf[0] = '\0';
      /* reject too short or too long lines */
    } while (strlen (p) <= 2 || strlen (p) >= (BUFSIZ - 5));

  p[strlen (p) - 2] = '\0';
  strcat (p, ".elc");

  return p;
}


int
main (int argc, char **argv)
{
  int i;
  int err_count = 0;
  int first_infile;
  char *extra_elcs = NULL;	/* XEmacs addition */

  progname = argv[0];

  outfile = stdout;

  /* Don't put CRs in the DOC file.  */
#ifdef WIN32_NATIVE
  _fmode = O_BINARY;
  _setmode (fileno (stdout), O_BINARY);
#endif /* WIN32_NATIVE */

  /* If first two args are -o FILE, output to FILE.  */
  i = 1;
  if (argc > i + 1 && !strcmp (argv[i], "-o"))
    {
      outfile = fopen (argv[i + 1], WRITE_BINARY);
      i += 2;
    }
  if (argc > i + 1 && !strcmp (argv[i], "-a"))
    {
      outfile = fopen (argv[i + 1], APPEND_BINARY);
      i += 2;
    }
  if (argc > i + 1 && !strcmp (argv[i], "-d"))
    {
      chdir (argv[i + 1]);
      i += 2;
    }

  /* Additional command line arguments for XEmacs */
  if (argc > i + 1 && !strcmp (argv[i], "-E"))
    {
      outfile = fopen (argv[i + 1], APPEND_BINARY);
      i += 2;
      ellcc = 1;
    }
  if (argc > (i + 1) && !strcmp (argv[i], "-i"))
    {
      extra_elcs = argv[i + 1];
      i += 2;
    }

  if (outfile == 0)
    fatal ("No output file specified", "");

  /* XEmacs addition */
  if (ellcc)
    fprintf (outfile, "{\n");

  first_infile = i;
  for (; i < argc; i++)
    {
      /* XEmacs addition: the "if" clause is new; the "else" clause is the
	 original FSF Emacs code */
      if (argv[i][0] == '@')
	{
	  /* Allow a file containing files to process, for use w/MS Windows
	     (where command-line length limits are more problematic) */
	  FILE *argfile = fopen (argv[i] + 1, READ_TEXT);
	  char arg[QXE_PATH_MAX];

	  if (!argfile)
	    fatal ("Unable to open argument file %s", argv[i] + 1);
	  while (fgets (arg, QXE_PATH_MAX, argfile))
	    {
	      if (arg[strlen (arg) - 1] == '\n')
		arg[strlen (arg) - 1] = '\0'; /* chop \n */
	      err_count += scan_file (arg);
	    }
	}
      else if (argc > i + 1 && !strcmp (argv[i], "-d"))
        {
          /* XEmacs change; allow more than one chdir. 
             The idea is that the second chdir is to source-lisp, and that
             any Lisp files not under there have the full path specified.  */
          i += 1;
          chdir (argv[i]);
          continue;
        }
      else
	{
	  int j;
	  /* Don't process one file twice.  */
	  for (j = first_infile; j < i; j++)
	    if (! strcmp (argv[i], argv[j]))
	      break;
	  if (j == i)
	    err_count += scan_file (argv[i]);
	}
    }

  /* XEmacs addition */
  if (extra_elcs)
    {
      char *p;

      while ((p = next_extra_elc (extra_elcs)) != NULL)
	err_count += scan_file (p);
    }

  putc ('\n', outfile);
  if (ellcc)
    fprintf (outfile, "}\n\n");
  /* End XEmacs addition */

#ifndef VMS
  exit (err_count > 0);
#endif /* VMS */
  return err_count > 0;
}

/* Add a source file name boundary in the output file.  */
static void
put_filename (const char *filename)
{
  /* XEmacs change; don't strip directory information. */
#if 0
  const char *tmp;

  for (tmp = filename; *tmp; tmp++)
    {
      if (IS_DIRECTORY_SEP(*tmp))
	filename = tmp + 1;
    }
#endif 

  /* <= because sizeof includes the nul byte at the end. Not quite right,
     because it should include the length of the symbol + "\037[VF]" instead
     of simply 10. */
  assert(sizeof("\037S\n") + strlen(filename) + 10 
	 <= DOC_MAX_FILENAME_LENGTH);

  putc (037, outfile);
  putc ('S', outfile);
  fprintf (outfile, "%s\n", filename);
}

/* Read file FILENAME and output its doc strings to outfile.  */
/* Return 1 if file is not found, 0 if it is found.  */

static int
scan_file (const char *filename)
{
  int len = strlen (filename);

  /* XEmacs change: test ellcc and set Current_file_type in each case */
  if (ellcc == 0 && len > 4 && !strcmp (filename + len - 4, ".elc"))
    {
      Current_file_type = elc_file;
      return scan_lisp_file (filename, READ_BINARY);
    }
  else if (ellcc == 0 && len > 3 && !strcmp (filename + len - 3, ".el"))
    {
      Current_file_type = el_file;
      return scan_lisp_file (filename, READ_BINARY);
    }
  else
    {
      Current_file_type = c_file;
      return scan_c_file (filename, READ_BINARY);
    }
}

/* XEmacs addition: ISO 2022 handling */
static int
getc_skipping_iso2022 (FILE *file)
{
  register int c;
  /* #### Kludge -- Ignore any ISO2022 sequences */
  c = getc (file);
  while (c == 27)
    {
      c = getc (file);
      if (c == '$')
	c = getc (file);
      if (c >= '(' && c <= '/')
	c = getc (file);
      c = getc (file);
    }
  return c;
}

enum iso2022_state
{
  ISO_NOTHING,
  ISO_ESC,
  ISO_DOLLAR,
  ISO_FINAL_IS_NEXT,
  ISO_DOLLAR_AND_FINAL_IS_NEXT
};

static int non_ascii_p;

static int
getc_iso2022 (FILE *file)
{
  /* #### Kludge -- Parse ISO2022 sequences (more or less) */
  static enum iso2022_state state;
  static int prevc;
  register int c;
  c = getc (file);
  switch (state)
    {
    case ISO_NOTHING:
      if (c == 27)
	state = ISO_ESC;
      break;

    case ISO_ESC:
      if (c == '$')
	state = ISO_DOLLAR;
      else if (c >= '(' && c <= '/')
	state = ISO_FINAL_IS_NEXT;
      else
	state = ISO_NOTHING;
      break;

    case ISO_DOLLAR:
      if (c >= '(' && c <= '/')
	state = ISO_DOLLAR_AND_FINAL_IS_NEXT;
      else if (c >= '@' && c <= 'B') /* ESC $ @ etc */
	{
	  non_ascii_p = 1;
	  state = ISO_NOTHING;
	}
      else
	state = ISO_NOTHING;
      break;

    case ISO_FINAL_IS_NEXT:
      if (prevc == '(' && c == 'B') /* ESC ( B, invoke ASCII */
	non_ascii_p = 0;
      else if (prevc == '(' || prevc == ',') /* ESC ( x or ESC , x */
	non_ascii_p = 1;
      state = ISO_NOTHING;
      break;

    case ISO_DOLLAR_AND_FINAL_IS_NEXT:
      if (prevc == '(' || prevc == ',') /* ESC $ ( x or ESC $ , x */
	non_ascii_p = 1;
      state = ISO_NOTHING;
      break;
    }
      
  prevc = c;
  return c;
}


char globalbuf[128];

/* Skip a C string from INFILE,
 and return the character that follows the closing ".
 If printflag is positive, output string contents to outfile.
 If it is negative, store contents in buf.
 Convert escape sequences \n and \t to newline and tab;
 discard \ followed by newline.  */

#define MDGET do { prevc = c; c = getc_iso2022 (infile); } while (0)
static int
read_c_string (FILE *infile, int printflag, int c_docstring)
{
  register int prevc = 0, c = 0;
  char *p = globalbuf;
  int start = -1;	/* XEmacs addition */

  MDGET;
  while (c != EOF)
    {
      while ((c_docstring || c != '"' || non_ascii_p) && c != EOF)
	{
	  /* XEmacs addition: the first two "if" clauses are new */
	  if (c == '*' && !non_ascii_p)
	    {
	      int cc = getc (infile);
	      if (cc == '/')
		{
		  if (prevc != '\n')
		    {
		      if (printflag > 0)
			{
			  if (ellcc)
			    fprintf (outfile, "\\n\\");
			  putc ('\n', outfile);
			}
		      else if (printflag < 0)
			*p++ = '\n';
		    }
		  break;
		}
	      else
		ungetc (cc, infile);
	    }

	  if (start == 1)
	    {
	      if (printflag > 0)
		{
		  if (ellcc)
		    fprintf (outfile, "\\n\\");
		  putc ('\n', outfile);
		}
	      else if (printflag < 0)
		*p++ = '\n';
	    }
	  /* End XEmacs addition */

	  if (c == '\\' && !non_ascii_p)
	    {
	      MDGET;
	      if (c == '\n')
		{
		  MDGET;
		  start = 1;
		  continue;
		}
	      if (!c_docstring && c == 'n')
		c = '\n';
	      if (c == 't')
		c = '\t';
	    }

	  /* XEmacs change: the "if" clause is new; the "else" clause is
	     mostly the original FSF Emacs code */
	  if (c == '\n')
	    start = 1;
	  else
	    {
	      start = 0;
	      if (printflag > 0)
		{
		  if (ellcc && c == '"' && !non_ascii_p)
		    putc ('\\', outfile);
		  putc (c, outfile);
		}
	      else if (printflag < 0)
		*p++ = c;
	    }
	  MDGET;
	}
      /* XEmacs change: look for continuation of string */
      if (Current_file_type == c_file)
	{
	  do
	    {
	      MDGET;
	    }
	  while (isspace (c));
	  if (c != '"' || non_ascii_p)
	    break;
	}
      else
	{
	  MDGET;
	  if (c != '"' || non_ascii_p)
	    break;
	  /* If we had a "", concatenate the two strings.  */
	}
      MDGET;
    }

  if (printflag < 0)
    *p = 0;

  return c;
}

/* Write to file OUT the argument names of function FUNC, whose text is in BUF.
   MINARGS and MAXARGS are the minimum and maximum number of arguments.  */

#define SKIPWHITE do { while (isspace ((unsigned char) (*p))) p++; } while (0)

static void
write_c_args (FILE *out, const char *UNUSED (func), char *buf,
	      int minargs, int maxargs)
{
  register char *p;
  int in_ident = 0;
  int just_spaced = 0;
  int need_paren = 0;
#if 0
  int need_space = 1;

  fprintf (out, "(%s", func);
#else
  /* XEmacs - "arguments:" is for parsing the docstring.  FSF's help system
     doesn't parse the docstring for arguments like we do, so we're also
     going to omit the function name to preserve compatibility with elisp
     that parses the docstring.  Finally, not prefixing the arglist with
     anything is asking for trouble because it's not uncommon to have an
     unescaped parenthesis at the beginning of a line. --Stig */
  fprintf (out, "arguments: (");
#endif

  if (*buf == '(')
    ++buf;

  for (p = buf; *p; p++)
    {
      char c = *p;
#if 0
      int ident_start = 0;
#endif

      /* XEmacs addition:  used for ANSI prototypes and UNUSED macros. */
      static char uu [] = "UNUSED";
      static char ui [] = "USED_IF_";
      static char lo [] = "Lisp_Object";

      /* Notice when we enter or leave an identifier.  */
      if (C_IDENTIFIER_CHAR_P (c) != in_ident)
	{
	  if (!in_ident)
	    {
	      /* Entering identifier.  Print as we parse. */
	      char *here;     	/* Target for backtracking. */

	      /* XEmacs addition:  add support for ANSI prototypes and the
		 UNUSED macros.  Hop over them.  "Lisp_Object" is the only
		 C type allowed in DEFUNs.  For the UNUSED macros we need
		 to eat parens, too. */
	      /* Aren't these all vulnerable to buffer overrun?  I guess that
		 means that the .c is busted, so we may as well just die ... */

	      /* Skip over "Lisp_Object". */
	      if ((strncmp (p, lo, sizeof (lo) - 1) == 0) &&
		  isspace ((unsigned char) p[sizeof (lo) - 1]))
		{
		  p += (sizeof (lo) - 1);
		  SKIPWHITE;
		}
	      /* Skip over "UNUSED" or "USED_IF_*" invocation. */
	      need_paren = 1;
	      here = p;
	      if (strncmp (p, uu, sizeof (uu) - 1) == 0)
		p += (sizeof (uu) - 1);
	      else if (strncmp (p, ui, sizeof (ui) - 1) == 0)
		p += (sizeof (ui) - 1);
	      else
		need_paren = 0;

	      if (need_paren)
		{
		  /* Skip rest of macro name, open paren, whitespace. */
		  while (*p && C_IDENTIFIER_CHAR_P (*p))
		    p++;
		  SKIPWHITE;
		  if (*p++ == '(')
		    SKIPWHITE;
		  else
		    {
		      need_paren = 0;
		      p = here;
		    }
		}
	      c = *p;

	      /* Do bookkeeping.  Maybe output lambda keywords. */
	      in_ident = 1;
#if 0
	      /* XEmacs - This goes along with the change above. */
	      ident_start = 1;
	      if (need_space)
		putc (' ', out);
#endif
	      if (minargs == 0 && maxargs > 0)
		fprintf (out, "&optional ");
	      just_spaced = 1;

	      minargs--;
	      maxargs--;
	    }
	  else
	    {
	      /* Leaving identifier. */
	      in_ident = 0;
	      if (need_paren)
		{
		  SKIPWHITE;
		  if (*p == ')')
		    p++;
		  c = *p;
		  need_paren = 0;
		}
	    }
	}

      /* Print the C argument list as it would appear in lisp:
	 print underscores as hyphens, and print commas, tabs and newlines
	 as spaces.  Collapse adjacent spaces into one.  */
      if (c == '_')
	c = '-';
      else if (c == ',' || c == '\n' || c == '\t')
	c = ' ';
      /* XEmacs change: handle \n below for readability */

#if 0
      /* In C code, `default' is a reserved word, so we spell it
	 `defalt'; unmangle that here.  */
      if (ident_start
	  && strncmp (p, "defalt", 6) == 0
	  && ! (('A' <= p[6] && p[6] <= 'Z')
		|| ('a' <= p[6] && p[6] <= 'z')
		|| ('0' <= p[6] && p[6] <= '9')
		|| p[6] == '_'))
	{
	  fprintf (out, "DEFAULT");
	  p += 5;
	  in_ident = 0;
	  just_spaced = 0;
	}
#endif
      /* If the C argument name ends with `_', change it to ' ',
	 to allow use of C reserved words or global symbols as Lisp args. */
      if (c == '-' && ! C_IDENTIFIER_CHAR_P (p[1]))
	{
	  in_ident = 0;
	  just_spaced = 0;
	}
#if 0
      /* [[ XEmacs change: if the character is carriage return or linefeed,
	 escape it for the compiler ]] I doubt the clause with '\r' ever
	 worked right, and outputting newlines now screws up the regexp
	 in function-documentation-1, so don't do this; instead, we treat
	 newlines like spaces. --ben */
      else if (c == '\n')
	{
	  putc('\\', out);
	  putc('\n', out);
	  c = ' ';
	}
      else if (c == '\r')
	{
	  putc('\\', out);
	  putc('\r', out);
	}
#else
      else if (c == '\r') /* Just eat it, since we expect a newline to
			     follow */
	;
#endif /* (not) 0 */
      else if (c != ' ' || !just_spaced)
	{
	  if (c >= 'a' && c <= 'z')
	    /* Upcase the letter.  */
	    c += 'A' - 'a';
	  putc (c, out);
	}

      just_spaced = (c == ' ');
#if 0
      need_space = 0;
#endif
    }
  /* XEmacs addition */
  if (!ellcc)
    putc ('\n', out);
}
#undef SKIPWHITE


/* Read through a c file.  If a .o or .obj file is named,
   the corresponding .c file is read instead.
   Looks for DEFUN constructs such as are defined in ../src/lisp.h.
   Accepts any word starting DEF... so it finds DEFSIMPLE and DEFPRED ...
   which don't exist anymore! */

static int
scan_c_file (const char *filename, const char *mode)
{
  FILE *infile;
  register int c;
  register int commas;
  register int defunflag;
  register int defvarperbufferflag = 0;
  register int defvarflag;
  int minargs, maxargs;
  int l = strlen (filename);
  char f[QXE_PATH_MAX];

  /* XEmacs change: different method for checking filename extension */
  if (l > QXE_PATH_MAX - 1)
    {
#ifdef ENAMETOOLONG
      errno = ENAMETOOLONG;
#else
      errno = EINVAL;
#endif
      return 0;
    }

  strcpy (f, filename);
  if (l > 4 && !strcmp (f + l - 4, ".obj")) /* MS Windows */
    strcpy (f + l - 4, ".c");
  if (f[l - 1] == 'o')
    f[l - 1] = 'c';
  infile = fopen (f, mode);

  /* No error if non-ex input file */
  if (infile == NULL)
    {
      perror (f);
      return 0;
    }

#if 0
  /* Reset extension to be able to detect duplicate files. */
  filename[strlen (filename) - 1] = extension;
#endif

  c = '\n';
  while (!feof (infile))
    {
      if (c != '\n')
	{
	  c = getc (infile);
	  continue;
	}
      c = getc (infile);
      if (c == ' ')
	{
	  while (c == ' ')
	    c = getc (infile);
	  if (c != 'D')
	    continue;
	  c = getc (infile);
	  if (c != 'E')
	    continue;
	  c = getc (infile);
	  if (c != 'F')
	    continue;
	  c = getc (infile);
	  if (c != 'V')
	    continue;
	  c = getc (infile);
	  if (c != 'A')
	    continue;
	  c = getc (infile);
	  if (c != 'R')
	    continue;
	  c = getc (infile);
	  if (c != '_')
	    continue;

	  defvarflag = 1;
	  defunflag = 0;

	  c = getc (infile);
	  /* Note that this business doesn't apply under XEmacs.
	     DEFVAR_BUFFER_LOCAL in XEmacs behaves normally. */
	  defvarperbufferflag = (c == 'P');

	  c = getc (infile);
	}
      else if (c == 'D')
	{
	  c = getc (infile);
	  if (c != 'E')
	    continue;
	  c = getc (infile);
	  if (c != 'F')
	    continue;
	  c = getc (infile);
	  defunflag = (c == 'U');
	  defvarflag = 0;
	  c = getc (infile);	/* XEmacs addition */
	}
      else continue;

      while (c != '(')
	{
	  if (c < 0)
	    goto eof;
	  c = getc (infile);
	}

      c = getc (infile);
      if (c != '"')
	continue;
      c = read_c_string (infile, -1, 0);

      if (defunflag)
	commas = 4;
      else if (defvarperbufferflag)
	commas = 2;
      else if (defvarflag)
	commas = 1;
      else  /* For DEFSIMPLE and DEFPRED ... which now don't exist! */
	commas = 2;

      while (commas)
	{
	  if (c == ',')
	    {
	      commas--;
	      if (defunflag && (commas == 1 || commas == 2))
		{
		  do
		    c = getc (infile);
		  while (c == ' ' || c == '\n' || c == '\t');
		  if (c < 0)
		    goto eof;
		  ungetc (c, infile);
		  if (commas == 2) /* pick up minargs */
		    fscanf (infile, "%d", &minargs);
		  else /* pick up maxargs */
		    if (c == 'M' || c == 'U') /* MANY || UNEVALLED */
		      maxargs = -1;
		    else
		      fscanf (infile, "%d", &maxargs);
		}
	    }
	  if (c < 0)
	    goto eof;
	  c = getc (infile);
	}
      while (c == ' ' || c == '\n' || c == '\t')
	c = getc (infile);
      if (c == '"')
	c = read_c_string (infile, 0, 0);
      /* XEmacs change */
      if (defunflag | defvarflag)
	{
	  while (c != '/')
	    {
	      if (c < 0)
		goto eof;
	      if (defunflag && c == '(')
		fatal ("Missing doc string for DEFUN %s\n", globalbuf);
	      c = getc (infile);
	    }
	  c = getc (infile);
	  while (c == '*')
	    c = getc (infile);
	}
      else
	{
	  while (c != ',')
	    {
	      if (c < 0)
		goto eof;
	      c = getc (infile);
	    }
	  c = getc (infile);
	}
      /* End XEmacs change */
      while (c == ' ' || c == '\n' || c == '\t')
	c = getc (infile);
      /* XEmacs addition */
      if (defunflag | defvarflag)
	ungetc (c, infile);
      /* End XEmacs addition */

      if (defunflag || defvarflag || c == '"')
	{
	  /* XEmacs change: the original code is in the "else" clause */
	  /* XXX Must modify the documentation file name code to handle
	     ELLCCs */
	  if (ellcc)
	    fprintf (outfile, "  CDOC%s(\"%s\", \"\\\n",
		     defvarflag ? "SYM" : "SUBR", globalbuf);
	  else
	    {
	      put_filename (filename);	/* XEmacs addition */
	      putc (037, outfile);
	      putc (defvarflag ? 'V' : 'F', outfile);
	      fprintf (outfile, "%s\n", globalbuf);
	    }
	  c = read_c_string (infile, 1, defunflag || defvarflag);

	  /* If this is a defun, find the arguments and print them.  If
	     this function takes MANY or UNEVALLED args, then the C source
	     won't give the names of the arguments, so we shouldn't bother
	     trying to find them.  */
	  if (defunflag && maxargs != -1)
	    {
	      char argbuf[1024], *p = argbuf;
	      int paren_level = 1;
#if 0				/* For old DEFUN's only */
	      while (c != ')')
		{
		  if (c < 0)
		    goto eof;
		  c = getc (infile);
		}
#endif
	      /* Skip into arguments.  */
	      while (c != '(')
		{
		  if (c < 0)
		    goto eof;
		  c = getc (infile);
		}
	      /* Copy arguments into ARGBUF.  */
	      *p++ = c;
	      do
		{
		  *p++ = c = getc (infile);
		  if (c < 0)
		    goto eof;
		  /* XEmacs change: handle macros with args (eg, UNUSED) */
		  if (c == ')')
		    paren_level--;
		  if (c == '(')
		    paren_level++;
		}
	      while (paren_level > 0);
	      *p = '\0';
	      /* Output them.  */
	      if (ellcc)
		fprintf (outfile, "\\n\\\n\\n\\\n");
	      else
		fprintf (outfile, "\n\n");
	      write_c_args (outfile, globalbuf, argbuf, minargs, maxargs);
	    }
	  if (ellcc)
	    fprintf (outfile, "\\n\");\n\n");
	}
    }
 eof:
  fclose (infile);
  return 0;
}

/* Read a file of Lisp code, compiled or interpreted.
 Looks for
  (defun NAME ARGS DOCSTRING ...)
  (defmacro NAME ARGS DOCSTRING ...)
  (defsubst NAME ARGS DOCSTRING ...)
  (autoload (quote NAME) FILE DOCSTRING ...)
  (defvar NAME VALUE DOCSTRING)
  (defconst NAME VALUE DOCSTRING)
  (fset (quote NAME) (make-byte-code ... DOCSTRING ...))
  (fset (quote NAME) #[... DOCSTRING ...])
  (defalias (quote NAME) #[... DOCSTRING ...])
  (custom-declare-variable (quote NAME) VALUE DOCSTRING ...)
 starting in column zero.
 (quote NAME) may appear as 'NAME as well.

 We also look for #@LENGTH CONTENTS^_ at the beginning of the line.
 When we find that, we save it for the following defining-form,
 and we use that instead of reading a doc string within that defining-form.

 For defvar, defconst, and fset we skip to the docstring with a kludgy 
 formatting convention: all docstrings must appear on the same line as the
 initial open-paren (the one in column zero) and must contain a backslash 
 and a newline immediately after the initial double-quote.  No newlines
 must appear between the beginning of the form and the first double-quote.
 For defun, defmacro, and autoload, we know how to skip over the
 arglist, but the doc string must still have a backslash and newline
 immediately after the double quote. 
 The only source files that must follow this convention are preloaded
 uncompiled ones like loaddefs.el and bindings.el; aside
 from that, it is always the .elc file that we look at, and they are no
 problem because byte-compiler output follows this convention.
 The NAME and DOCSTRING are output.
 NAME is preceded by `F' for a function or `V' for a variable.
 An entry is output only if DOCSTRING has \ newline just after the opening "

 Adds the filename a symbol or function was found in before its docstring;
 there's no need for this with the load-history available, but we do it for
 consistency with the C parsing code. 
 */

static void
skip_white (FILE *infile)
{
  char c = ' ';
  while (c == ' ' || c == '\t' || c == '\n')
    c = getc (infile);
  ungetc (c, infile);
}

static void
read_lisp_symbol (FILE *infile, char *buffer)
{
  char c;
  char *fillp = buffer;

  skip_white (infile);
  while (1)
    {
      c = getc (infile);
      if (c == '\\')
	/* FSF has *(++fillp), which is wrong. */
	*fillp++ = getc (infile);
      else if (c == ' ' || c == '\t' || c == '\n' || c == '(' || c == ')')
	{
	  ungetc (c, infile);
	  *fillp = 0;
	  break;
	}
      else
	*fillp++ = c;
    }

  if (! buffer[0])
    fprintf (stderr, "## expected a symbol, got '%c'\n", c);
  
  skip_white (infile);
}

static int
scan_lisp_file (const char *filename, const char *mode)
{
  FILE *infile;
  register int c;
  char *saved_string = 0;

  infile = fopen (filename, mode);
  if (infile == NULL)
    {
      perror (filename);
      return 0;				/* No error */
    }

  c = '\n';
  while (!feof (infile))
    {
      char buffer[BUFSIZ];
      char type;
      int no_docstring = 0;

      /* If not at end of line, skip till we get to one.  */
      if (c != '\n')
	{
	  c = getc_skipping_iso2022 (infile);
	  continue;
	}
      /* Skip the line break.  */
      while (c == '\n')
	c = getc_skipping_iso2022 (infile);
      /* Detect a dynamic doc string and save it for the next expression.  */
      if (c == '#')
	{
	  c = getc_skipping_iso2022 (infile);
	  if (c == '@')
	    {
	      unsigned int nlength = 0, length = 0;
	      unsigned int i;

	      /* Read the length.  */
	      while ((c = getc_skipping_iso2022 (infile),
		      c >= '0' && c <= '9'))
		{
                  length = nlength;
		  nlength *= 10;
		  nlength += c - '0';

                  if (length <= nlength)
                    {
                      length = nlength;
                    }
                  else
                    {
                      break; /* Overflow, stick with the old value. */
                    }
		}

	      /* The next character is a space that is counted in the length
		 but not part of the doc string.
		 We already read it, so just ignore it.  */
	      length--;

	      /* Read in the contents.  */
	      if (saved_string != 0)
		free (saved_string);
	      saved_string = (char *) xmalloc (length);
	      for (i = 0; i < length; i++)
                {
                  saved_string[i] = c = getc (infile);
                  if (c == EOF)
                    {
                      saved_string[i] = '\0';
                      fatal ("docstring ran off end of file: `%.50s ...'",
                             saved_string);
                    }
                }
	      /* The last character is a ^_.
		 That is needed in the .elc file
		 but it is redundant in DOC.  So get rid of it here.  */
	      saved_string[length - 1] = 0;
	      /* Skip the line break.  */
	      while (c == '\n')
		c = getc_skipping_iso2022 (infile);
	      /* Skip the following line.  */
	      while (c != '\n' && c != EOF)
		c = getc_skipping_iso2022 (infile);
	    }
	  continue;
	}

      if (c != '(')
	continue;

      read_lisp_symbol (infile, buffer);

      if (! strcmp (buffer, "defun")
	  || ! strcmp (buffer, "defmacro")
	  || ! strcmp (buffer, "defsubst"))
	{
	  type = 'F';
	  read_lisp_symbol (infile, buffer);

	  /* Skip the arguments: either "nil" or a list in parens */

	  c = getc_skipping_iso2022 (infile);
	  if (c == 'n') /* nil */
	    {
	      if ((c = getc_skipping_iso2022 (infile)) != 'i' ||
		  (c = getc_skipping_iso2022 (infile)) != 'l')
		{
		  fprintf (stderr, "## unparsable arglist in %s (%s)\n",
			   buffer, filename);
		  continue;
		}
	    }
	  else if (c != '(')
	    {
	      fprintf (stderr, "## unparsable arglist in %s (%s)\n",
		       buffer, filename);
	      continue;
	    }
	  else
	    while (c != ')' && c != EOF)
	      {
		c = getc_skipping_iso2022 (infile);
		if (c < 0)
		  continue;
	      }
	  skip_white (infile);

	  /* If the next three characters aren't `dquote bslash newline'
	     then we're not reading a docstring.
	   */
	  if ((c = getc_skipping_iso2022 (infile)) != '"' ||
	      (c = getc_skipping_iso2022 (infile)) != '\\' ||
	      (c = getc_skipping_iso2022 (infile)) != '\n')
	    {
#ifdef DEBUG
	      fprintf (stderr, "## non-docstring in %s (%s)\n",
		       buffer, filename);
#endif
	      no_docstring = 1;
	    }
	}

      else if (! strcmp (buffer, "defvar")
	       || ! strcmp (buffer, "defconst"))
	{
	  char c1 = 0, c2 = 0;
	  type = 'V';
	  read_lisp_symbol (infile, buffer);

	  if (saved_string == 0)
	    {

	      /* Skip until the end of line; remember two previous chars.  */
	      while (c != '\n' && c >= 0)
		{
		  c2 = c1;
		  c1 = c;
		  c = getc_skipping_iso2022 (infile);
		}
	  
	      /* If two previous characters were " and \,
		 this is a doc string.  Otherwise, there is none.  */
	      if (c2 != '"' || c1 != '\\')
		{
#ifdef DEBUG
		  fprintf (stderr, "## non-docstring in %s (%s)\n",
			   buffer, filename);
#endif
                  no_docstring = 1;
		}
	    }
	}

      else if (! strcmp (buffer, "custom-declare-variable"))
	{
	  char c1 = 0, c2 = 0;
	  type = 'V';

	  c = getc (infile);
	  if (c == '\'')
	    read_lisp_symbol (infile, buffer);
	  else
	    {
	      if (c != '(')
		{
		  fprintf (stderr,
			   "## unparsable name in custom-declare-variable in %s\n",
			   filename);
		  continue;
		}
	      read_lisp_symbol (infile, buffer);
	      if (strcmp (buffer, "quote"))
		{
		  fprintf (stderr,
			   "## unparsable name in custom-declare-variable in %s\n",
			   filename);
		  continue;
		}
	      read_lisp_symbol (infile, buffer);
	      c = getc (infile);
	      if (c != ')')
		{
		  fprintf (stderr,
			   "## unparsable quoted name in custom-declare-variable in %s\n",
			   filename);
		  continue;
		}
	    }

	  if (saved_string == 0)
	    {
	      /* Skip to end of line; remember the two previous chars.  */
	      while (c != '\n' && c >= 0)
		{
		  c2 = c1;
		  c1 = c;
		  c = getc_skipping_iso2022 (infile);
		}
	  
	      /* If two previous characters were " and \,
		 this is a doc string.  Otherwise, there is none.  */
	      if (c2 != '"' || c1 != '\\')
		{
#ifdef DEBUG
		  fprintf (stderr, "## non-docstring in %s (%s)\n",
			   buffer, filename);
#endif
		  no_docstring = 1;
		}
	    }
	}

      else if (! strcmp (buffer, "fset") || ! strcmp (buffer, "defalias"))
	{
	  char c1 = 0, c2 = 0;
	  type = 'F';

	  c = getc_skipping_iso2022 (infile);
	  if (c == '\'')
	    read_lisp_symbol (infile, buffer);
	  else
	    {
	      if (c != '(')
		{
		  fprintf (stderr, "## unparsable name in fset in %s\n",
			   filename);
		  continue;
		}
	      read_lisp_symbol (infile, buffer);
	      if (strcmp (buffer, "quote"))
		{
		  fprintf (stderr, "## unparsable name in fset in %s\n",
			   filename);
		  continue;
		}
	      read_lisp_symbol (infile, buffer);
	      c = getc_skipping_iso2022 (infile);
	      if (c != ')')
		{
		  fprintf (stderr,
			   "## unparsable quoted name in fset in %s\n",
			   filename);
		  continue;
		}
	    }

	  if (saved_string == 0)
	    {
	      /* Skip to end of line; remember the two previous chars.  */
	      while (c != '\n' && c >= 0)
		{
		  c2 = c1;
		  c1 = c;
		  c = getc_skipping_iso2022 (infile);
		}
	  
	      /* If two previous characters were " and \,
		 this is a doc string.  Otherwise, there is none.  */
	      if (c2 != '"' || c1 != '\\')
		{
#ifdef DEBUG
		  fprintf (stderr, "## non-docstring in %s (%s)\n",
			   buffer, filename);
#endif
                  no_docstring = 1;
		}
	    }
	}

      else if (! strcmp (buffer, "autoload"))
	{
	  type = 'F';
	  c = getc_skipping_iso2022 (infile);
	  if (c == '\'')
	    read_lisp_symbol (infile, buffer);
	  else
	    {
	      if (c != '(')
		{
		  fprintf (stderr, "## unparsable name in autoload in %s\n",
			   filename);
		  continue;
		}
	      read_lisp_symbol (infile, buffer);
	      if (strcmp (buffer, "quote"))
		{
		  fprintf (stderr, "## unparsable name in autoload in %s\n",
			   filename);
		  continue;
		}
	      read_lisp_symbol (infile, buffer);
	      c = getc_skipping_iso2022 (infile);
	      if (c != ')')
		{
		  fprintf (stderr,
			   "## unparsable quoted name in autoload in %s\n",
			   filename);
		  continue;
		}
	    }
	  skip_white (infile);
	  if ((c = getc_skipping_iso2022 (infile)) != '\"')
	    {
	      fprintf (stderr, "## autoload of %s unparsable (%s)\n",
		       buffer, filename);
	      continue;
	    }
	  read_c_string (infile, 0, 0);
	  skip_white (infile);

	  if (saved_string == 0)
	    {
	      /* If the next three characters aren't `dquote bslash newline'
		 then we're not reading a docstring.  */
	      if ((c = getc_skipping_iso2022 (infile)) != '"'  ||
		  (c = getc_skipping_iso2022 (infile)) != '\\' ||
		  (c = getc_skipping_iso2022 (infile)) != '\n')
		{
#ifdef DEBUG
		  fprintf (stderr, "## non-docstring in %s (%s)\n",
			   buffer, filename);
#endif
                  no_docstring = 1;
		}
	    }
	}

#if 0				/* causes crash */
      else if (! strcmp (buffer, "if")
	       || ! strcmp (buffer, "byte-code"))
	;
#endif

      else
	{
#ifdef DEBUG
	  fprintf (stderr, "## unrecognized top-level form, %s (%s)\n",
		   buffer, filename);
#endif
	  continue;
	}

      /* At this point, we should either use the previous
	 dynamic doc string in saved_string
	 or gobble a doc string from the input file.

	 In the latter case, the opening quote (and leading
	 backslash-newline) have already been read.  */

      put_filename (filename);	/* XEmacs addition */
      putc (037, outfile);
      putc (type, outfile);
      fprintf (outfile, "%s\n", buffer);
      if (!no_docstring)
        {
          if (saved_string)
            {
              fputs (saved_string, outfile);
              /* Don't use one dynamic doc string twice.  */
              free (saved_string);
              saved_string = 0;
            }
          else
            read_c_string (infile, 1, 0);
        }
    }
  fclose (infile);
  return 0;
}
