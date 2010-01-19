/* #### Old code!  Replaced with make-msgfile.lex. */


/* Scan specified C and Lisp files, extracting the following messages:

     C files:
	GETTEXT (...)
	DEFER_GETTEXT (...)
	DEFUN interactive prompts
     Lisp files:
	(gettext ...)
	(dgettext "domain-name" ...)
	(defer-gettext ...)
	(interactive ...)

  The arguments given to this program are all the C and Lisp source files
  of XEmacs.  .el and .c files are allowed.  There is no support for .elc
  files at this time, but they may be specified; the corresponding .el file
  will be used.  Similarly, .o files can also be specified, and the corresponding
  .c file will be used.  This helps the makefile pass the correct list of files.

  The results, which go to standard output or to a file specified with -a or -o
  (-a to append, -o to start from nothing), are quoted strings wrapped in
  gettext(...).  The results can be passed to xgettext to produce a .po message
  file.
*/

#include <stdio.h>
#include <string.h>

#define LINESIZE 256
#define GET_LINE	fgets (line, LINESIZE, infile)
#define CHECK_EOL(p)	if (*(p) == '\0')  (p) = GET_LINE
#define SKIP_BLANKS(p)	while ((*p) == ' ' || (*p) == '\t')  (p)++

enum filetype { C_FILE, LISP_FILE, INVALID_FILE };
/* some brain-dead headers define this ... */
#undef FALSE
#undef TRUE
enum boolean { FALSE, TRUE };

FILE *infile;
FILE *outfile;
char line[LINESIZE];


void scan_file (char *filename);
void process_C_file (void);
void process_Lisp_file (void);
char *copy_up_to_paren (register char *p);
char *copy_quoted_string (register char *p);
enum boolean no_interactive_prompt (register char *q);
char *skip_blanks (register char *p);


main (int argc, char *argv[])
{
  register int i;

  outfile = stdout;

  /* If first two args are -o FILE, output to FILE. */
  i = 1;
  if (argc > i + 1 && strcmp (argv[i], "-o") == 0) {
    outfile = fopen (argv[++i], "w");
    ++i;
  }
  /* ...Or if args are -a FILE, append to FILE. */
  if (argc > i + 1 && strcmp (argv[i], "-a") == 0) {
    outfile = fopen (argv[++i], "a");
    ++i;
  }
  if (!outfile) {
    fprintf (stderr, "Unable to open output file %s\n", argv[--i]);
    return;
  }

  for (; i < argc; i++)
    scan_file (argv[i]);

  return 0;
}


void scan_file (char *filename)
{
  enum filetype type = INVALID_FILE;
  register char *p = filename + strlen (filename);

  if (strcmp (p - 4, ".elc") == 0) {
    *--p = '\0';				/* Use .el file instead */
    type = LISP_FILE;
  } else if (strcmp (p - 3, ".el") == 0)
    type = LISP_FILE;
  else if (strcmp (p - 2, ".o") == 0) {
    *--p = 'c';					/* Use .c file instead */
    type = C_FILE;
  } else if (strcmp (p - 2, ".c") == 0)
    type = C_FILE;

  if (type == INVALID_FILE) {
    fprintf (stderr, "File %s being ignored\n", filename);
    return;
  }
  infile = fopen (filename, "r");
  if (!infile) {
    fprintf (stderr, "Unable to open input file %s\n", filename);
    return;
  }

  fprintf (outfile, "/* %s */\n", filename);
  if (type == C_FILE)
    process_C_file ();
  else
    process_Lisp_file ();
  fputc ('\n', outfile);

  fclose (infile);
}


void process_C_file (void)
{
  register char *p;
  char *gettext, *defun;

  while (p = GET_LINE) {
    gettext = strstr (p, "GETTEXT");
    defun = strstr (p, "DEFUN");
    if (gettext || defun) {
      if (gettext) {
	p = gettext;
	p += 7;			/* Skip over "GETTEXT" */
      }
      else if (defun) {
	p = defun;
	p += 5;			/* Skip over "DEFUN" */
      }

      p = skip_blanks (p);
      if (*p++ != '(')
	continue;

      if (defun) {
	register int i;

	for (i = 0; i < 5; i++)	/* Skip over commas to doc string */
	  while (*p++ != ',')
	    CHECK_EOL (p);
	if (*p == '\n')
	  p = GET_LINE;
      }

      p = skip_blanks (p);
      if (*p != '\"')		/* Make sure there is a quoted string */
	continue;

      if (defun && no_interactive_prompt (p))
	continue;

      fprintf (outfile, "gettext(");
      if (gettext)
	p = copy_up_to_paren (p);
      else
	p = copy_quoted_string (p);
      fprintf (outfile, ")\n");
    }
  }
}


void process_Lisp_file (void)
{
  register char *p;
  char *gettext, *interactive;
  enum boolean dgettext = FALSE;

  while (p = GET_LINE) {
    gettext = strstr (p, "gettext");
    interactive = strstr (p, "(interactive");
    if (gettext || interactive) {
      if (!interactive)
	p = gettext;
      else if (!gettext)
	p = interactive;
      else if (gettext < interactive) {
	p = gettext;
	interactive = NULL;
      } else {
	p = interactive;
	gettext = NULL;
      }

      if (gettext) {
	if (p > line && *(p-1) == 'd')
	  dgettext = TRUE;
	p += 7;		/* Skip over "gettext" */
      } else
	p += 12;	/* Skip over "(interactive" */

      p = skip_blanks (p);
      if (*p != '\"')		/* Make sure there is a quoted string */
	continue;

      if (dgettext) {		/* Skip first quoted string (domain name) */
	while (*++p != '"')
	  ;  /* null statement */
	++p;
	p = skip_blanks (p);
	if (*p != '\"')		/* Check for second quoted string (message) */
	  continue;
      }

      if (interactive && no_interactive_prompt (p))
	continue;

      fprintf (outfile, "gettext(");
      p = copy_up_to_paren (p);
      fprintf (outfile, ")\n");
    }
  }
}


/* Assuming p points to some character beyond an opening parenthesis, copy
   everything to outfile up to but not including the closing parenthesis.
*/
char *copy_up_to_paren (register char *p)
{
  for (;;) {
    SKIP_BLANKS (p);	/* We don't call skip_blanks() in order to */
    CHECK_EOL (p);	/* preserve blanks at the beginning of the line */
    if (*p == ')')
      break;

    if (*p == '\"')
      p = copy_quoted_string (p);
    else
      fputc (*p++, outfile);
  }
  return p;
}


/* Assuming p points to a quote character, copy the quoted string to outfile.
*/
char *copy_quoted_string (register char *p)
{
  do {
    if (*p == '\\')
      fputc (*p++, outfile);
    fputc (*p++, outfile);
    CHECK_EOL (p);
  } while (*p != '\"');

  fputc (*p++, outfile);
  return p;
}


/* Return TRUE if the interactive specification consists only
   of code letters and no prompt.
*/
enum boolean no_interactive_prompt (register char *q)
{
  while (++q, *q == '*' || *q == '@')
    ; /* null statement */
  if (*q == '\"')
    return TRUE;
 skip_code_letter:
  if (*++q == '\"')
    return TRUE;
  if (*q == '\\' && *++q == 'n') {
    ++q;
    goto skip_code_letter;
  }
  return FALSE;
}


char *skip_blanks (register char *p)
{
  while (*p == ' ' || *p == '\t' || *p == '\n') {
    p++;
    CHECK_EOL (p);
  }
  return p;
}
