%{

/* This is a Lex file. */

/* Localizable-message snarfing.
   Copyright (C) 1994, 1995 Amdahl Corporation.

This file is part of XEmacs.

XEmacs is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

XEmacs is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with XEmacs; see the file COPYING.  If not, write to
the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  */

/* Written by Ben Wing, November 1994.  Some code based on earlier
   make-msgfile.c. */

/* See text.c for a proposal about how this whole system should work. */

/* Note: there is still much work to be done on this.

   1) Definition of Arg below won't handle a generalized argument
      as might appear in a function call.  This is fine for DEFUN
      and friends, because only simple arguments appear there; but
      it might run into problems if Arg is used for other sorts
      of functions.
   2) snarf() should be modified so that it doesn't output null
      strings and non-textual strings (see the comment at the top
      of make-msgfile.c).
   3) parsing of (insert) should snarf all of the arguments.
   4) need to add set-keymap-prompt and deal with gettext of that.
   5) parsing of arguments should snarf all strings anywhere within
      the arguments, rather than just looking for a string as the
      argument.  This allows if statements as arguments to get parsed.
   6) begin_paren_counting() et al. should handle recursive entry.
   7) handle set-window-buffer and other such functions that take
      a buffer as the other-than-first argument.
   8) there is a fair amount of work to be done on the C code.
      Look through the code for #### comments associated with
      '#ifdef I18N3' or with an I18N3 nearby.
   9) Deal with `get-buffer-process' et al.
   10) Many of the changes in the Lisp code marked
       'rewritten for I18N3 snarfing' should be undone once (5) is
       implemented.
   11) Go through the Lisp code in prim and make sure that all
       strings are gettexted as necessary.  This may reveal more
       things to implement.
   12) Do the equivalent of (8) for the Lisp code.
   13) Deal with parsing of menu specifications.

	--ben
   
*/

/* Some notes:

-- {Arg} below could get confused by commas inside of quotes.
-- {LispToken} below can match some things that are not tokens (e.g.
   numbers) but for all practical purposes it should be fine.
*/

#include <stdio.h>

int snarf_return_state;

%}

%p 6000
%e 2000
%n 1000
%a 4000
%s C_QUOTE C_COMMENT LQUO LCOM
%s CSNARF LSNARF
%s DO_C DO_LISP DEFUN
%s DEFUN2 DEFUN3 LDEF

W	[ \t\n]
Any	(.|"\n")
Q	"\""
NQ	[^"]
NT	[^A-Za-z_0-9]
LP	"("
RP	")"
BS	"\\"
Esc	({BS}{Any})
Wh	({W}*)
LCom	(";"({Esc}|.)*)
LWh	(({W}|{Lcom})*)
Open	({Wh}{LP})
OpWQ	({Open}{Wh}{Q})
String	({Q}({Esc}|{NQ})*{Q})
Arg	([^,]*",")
StringArg	({Wh}{String}{Wh}",")
OpenString	({Open}{StringArg})
LispToken	(({Esc}|[-A-Za-z0-9!@$%^&*_=+|{}`~,<.>/?])+)
%%

<DO_C>{NT}"GETTEXT"{OpWQ} { snarf (); }
<DO_C>{NT}"DEFER_GETTEXT"{OpWQ} { snarf (); }
<DO_C>{NT}"build_translated_string"{OpWQ} { snarf (); }
<DO_C>{NT}"insert_string"{OpWQ} { snarf (); }
<DO_C>{NT}"message"{OpWQ} { snarf (); }
<DO_C>{NT}"warn_when_safe"{OpWQ} { snarf (); }
<DO_C>{NT}"error"{OpWQ} { snarf (); }
<DO_C>{NT}"continuable_error"{OpWQ} { snarf (); }
<DO_C>{NT}"signal_simple_error"{OpWQ} { snarf (); }
<DO_C>{NT}"signal_simple_error_2"{OpWQ} { snarf (); }
<DO_C>{NT}"signal_simple_continuable_error"{OpWQ} { snarf (); }
<DO_C>{NT}"signal_simple_continuable_error_2"{OpWQ} { snarf (); }
<DO_C>{NT}"report_file_error"{OpWQ} { snarf (); }
<DO_C>{NT}"signal_file_error"{OpWQ} { snarf (); }
<DO_C>{NT}"signal_double_file_error"{OpWQ} { snarf (); }
<DO_C>{NT}"signal_double_file_error_2"{OpWQ} { snarf (); }
<DO_C>{NT}"syntax_error"{OpWQ} { snarf (); }
<DO_C>{NT}"continuable_syntax_error"{OpWQ} { snarf (); }
<DO_C>{NT}"CTB_ERROR"{OpWQ} { snarf (); }
<DO_C>{NT}"fatal"{OpWQ} { snarf (); }
<DO_C>{NT}"stdout_out"{OpWQ} { snarf (); }
<DO_C>{NT}"stderr_out"{OpWQ} { snarf (); }
<DO_C>{NT}"with_output_to_temp_buffer"{OpWQ} { snarf (); }

<DO_C>{NT}"DEFVAR_BOOL"{OpenString}{Arg}{Wh}{Q} { snarf (); }
<DO_C>{NT}"DEFVAR_LISP"{OpenString}{Arg}{Wh}{Q} { snarf (); }
<DO_C>{NT}"DEFVAR_SPECIFIER"{OpenString}{Arg}{Wh}{Q} { snarf (); }
<DO_C>{NT}"DEFVAR_INT"{OpenString}{Arg}{Wh}{Q} { snarf (); }
<DO_C>{NT}"DEFVAR_BUFFER_LOCAL"{OpenString}{Arg}{Wh}{Q} { snarf (); }
<DO_C>{NT}"DEFVAR_BUFFER_DEFAULTS"{OpenString}{Arg}{Wh}{Q} { snarf (); }
<DO_C>{NT}"deferror"{Open}{Arg}{StringArg}{Wh}{Q} { snarf (); }

<DO_C>{NT}"barf_or_query_if_file_exists"{Open}{Arg}{Wh}{Q} {
  /* #### see comment above about use of Arg */
  snarf ();
}

<DO_C>{NT}"DEFUN"{Open} { BEGIN DEFUN; }

<DO_C>"/*" {
  /* This is hateful, but doc strings are sometimes put inside of comments
     (to get around limits in cpp), so we can't ignore stuff inside of
     comments. */
  /* BEGIN C_COMMENT; */
}
<DO_C>{Q} { BEGIN C_QUOTE; }
<DO_C>{Any} { }

<DEFUN>{StringArg}{Arg}{Arg}{Arg}{Arg}{Wh} { BEGIN DEFUN2; }
<DEFUN>{Any} { bad_c_defun (); }

<DEFUN2>{Q} {
  /* We found an interactive specification. */
  snarf_return_state = DEFUN3;
  snarf ();
}
<DEFUN2>[^,]* {
  /* This function doesn't have an interactive specification.
     Don't use {Arg} in the specification because DEFUN3 looks
     for the comma. */
  BEGIN DEFUN3;
}

<DEFUN3>{Wh}","{Wh}{Q} {
  snarf_return_state = DO_C;
  snarf ();
}
<DEFUN3>{Any} { bad_c_defun (); }

<C_QUOTE>{Esc} { }
<C_QUOTE>{Q} { BEGIN DO_C; }
<C_QUOTE>{Any} { }

<C_COMMENT>"*/" { BEGIN DO_C; }
<C_COMMENT>{Any} { }

<DO_LISP>{LP}{LWh}"gettext"{LWh}{Q} { inc_paren (); snarf (); }
<DO_LISP>{LP}{LWh}"purecopy"{LWh}{Q} { inc_paren (); snarf (); }
<DO_LISP>{LP}{LWh}"interactive"{LWh}{Q} { inc_paren (); snarf (); }
<DO_LISP>{LP}{LWh}"message"{LWh}{Q} { inc_paren (); snarf (); }
<DO_LISP>{LP}{LWh}"error"{LWh}{Q} { inc_paren (); snarf (); }
<DO_LISP>{LP}{LWh}"warn"{LWh}{Q} { inc_paren (); snarf (); }
<DO_LISP>{LP}{LWh}"format"{LWh}{Q} { inc_paren (); snarf (); }
<DO_LISP>{LP}{LWh}"substitute-command-keys"{LWh}{Q} { inc_paren (); snarf (); }
<DO_LISP>{LP}{LWh}"temp-minibuffer-message"{LWh}{Q} { inc_paren (); snarf (); }
<DO_LISP>{LP}{LWh}"momentary-string-display"{LWh}{Q} { inc_paren (); snarf (); }
<DO_LISP>{LP}{LWh}"princ"{LWh}{Q} { inc_paren (); snarf (); }
<DO_LISP>{LP}{LWh}"prin1"{LWh}{Q} { inc_paren (); snarf (); }
<DO_LISP>{LP}{LWh}"prin1-to-string"{LWh}{Q} { inc_paren (); snarf (); }
<DO_LISP>{LP}{LWh}"print"{LWh}{Q} { inc_paren (); snarf (); }
<DO_LISP>{LP}{LWh}"insert"{LWh}{Q} { inc_paren (); snarf (); }
<DO_LISP>{LP}{LWh}"insert-before-markers"{LWh}{Q} { inc_paren (); snarf (); }

<DO_LISP>{LP}{LWh}"get-buffer"{LWh}{Q} { inc_paren (); snarf (); }
<DO_LISP>{LP}{LWh}"get-buffer-create"{LWh}{Q} { inc_paren (); snarf (); }
<DO_LISP>{LP}{LWh}"generate-new-buffer-name"{LWh}{Q} { inc_paren (); snarf (); }
<DO_LISP>{LP}{LWh}"rename-buffer"{LWh}{Q} { inc_paren (); snarf (); }
<DO_LISP>{LP}{LWh}"set-buffer"{LWh}{Q} { inc_paren (); snarf (); }
<DO_LISP>{LP}{LWh}"switch-to-buffer"{LWh}{Q} { inc_paren (); snarf (); }
<DO_LISP>{LP}{LWh}"pop-to-buffer"{LWh}{Q} { inc_paren (); snarf (); }
<DO_LISP>{LP}{LWh}"with-output-to-temp-buffer"{LWh}{Q} { inc_paren (); snarf (); }
<DO_LISP>{LP}{LWh}"buffer-enable-undo"{LWh}{Q} { inc_paren (); snarf (); }
<DO_LISP>{LP}{LWh}"buffer-disable-undo"{LWh}{Q} { inc_paren (); snarf (); }
<DO_LISP>{LP}{LWh}"get-buffer-window"{LWh}{Q} { inc_paren (); snarf (); }
<DO_LISP>{LP}{LWh}"delete-windows-on"{LWh}{Q} { inc_paren (); snarf (); }
<DO_LISP>{LP}{LWh}"replace-buffer-in-windows"{LWh}{Q} { inc_paren (); snarf (); }
<DO_LISP>{LP}{LWh}"display-buffer"{LWh}{Q} { inc_paren (); snarf (); }
<DO_LISP>{LP}{LWh}"other-buffer"{LWh}{Q} { inc_paren (); snarf (); }

<DO_LISP>{LP}{LWh}"read-from-minibuffer"{LWh}{Q} { inc_paren (); snarf (); }
<DO_LISP>{LP}{LWh}"read-shell-command"{LWh}{Q} { inc_paren (); snarf (); }
<DO_LISP>{LP}{LWh}"read-file-name"{LWh}{Q} { inc_paren (); snarf (); }
<DO_LISP>{LP}{LWh}"read-buffer"{LWh}{Q} { inc_paren (); snarf (); }
<DO_LISP>{LP}{LWh}"read-variable"{LWh}{Q} { inc_paren (); snarf (); }
<DO_LISP>{LP}{LWh}"read-command"{LWh}{Q} { inc_paren (); snarf (); }
<DO_LISP>{LP}{LWh}"read-function"{LWh}{Q} { inc_paren (); snarf (); }
<DO_LISP>{LP}{LWh}"read-directory-name"{LWh}{Q} { inc_paren (); snarf (); }
<DO_LISP>{LP}{LWh}"read-string"{LWh}{Q} { inc_paren (); snarf (); }
<DO_LISP>{LP}{LWh}"read-number"{LWh}{Q} { inc_paren (); snarf (); }
<DO_LISP>{LP}{LWh}"read-minibuffer"{LWh}{Q} { inc_paren (); snarf (); }
<DO_LISP>{LP}{LWh}"read-quoted-char"{LWh}{Q} { inc_paren (); snarf (); }
<DO_LISP>{LP}{LWh}"read-face-name"{LWh}{Q} { inc_paren (); snarf (); }
<DO_LISP>{LP}{LWh}"read-itimer"{LWh}{Q} { inc_paren (); snarf (); }
<DO_LISP>{LP}{LWh}"completing-read"{LWh}{Q} { inc_paren (); snarf (); }
<DO_LISP>{LP}{LWh}"y-or-n-p"{LWh}{Q} { inc_paren (); snarf (); }
<DO_LISP>{LP}{LWh}"yes-or-no-p"{LWh}{Q} { inc_paren (); snarf (); }
<DO_LISP>{LP}{LWh}"query-replace-read-args"{LWh}{Q} { inc_paren (); snarf (); }
<DO_LISP>{LP}{LWh}"eval-minibuffer"{LWh}{Q} { inc_paren (); snarf (); }
<DO_LISP>{LP}{LWh}"edit-and-eval-command"{LWh}{Q} { inc_paren (); snarf (); }

<DO_LISP>{LP}{LWh}"defvar"{LWh}{LispToken}{LWh} {
  inc_paren (); begin_paren_counting (LDEF);
}
<DO_LISP>{LP}{LWh}"defconst"{LWh}{LispToken}{LWh} {
  inc_paren (); begin_paren_counting (LDEF);
}
<DO_LISP>{LP}{LWh}"defun"{LWh}{LispToken}{LWh} {
  inc_paren (); begin_paren_counting (LDEF);
}
<DO_LISP>{LP}{LWh}"defmacro"{LWh}{LispToken}{LWh} {
  inc_paren (); begin_paren_counting (LDEF);
}
<DO_LISP>{LP}{LWh}"defsubst"{LWh}{LispToken}{LWh} {
  inc_paren (); begin_paren_counting (LDEF);
}

<DO_LISP>{Q} { BEGIN LQUO; }
<DO_LISP>";" { BEGIN LCOM; }
<DO_LISP>{LP} { inc_paren (); }
<DO_LISP>{RP} { dec_paren (); }
<DO_LISP>{Esc} { }
<DO_LISP>{W} { lisp_whitespace (); }
<DO_LISP>{Any} { }

<LQUO>{Esc} { }
<LQUO>{Q} { BEGIN DO_LISP; }
<LQUO>{Any} { }

<LCOM>"\n" { BEGIN DO_LISP; }
<LCOM>{Any} { }

<LDEF>{LWh}{Q} { snarf (); }
<LDEF>{Any} { BEGIN DO_LISP; }

<CSNARF>{Esc} { ECHO; }
<CSNARF>{Q} { ECHO; fprintf (yyout, ")\n"); BEGIN snarf_return_state; }
<CSNARF>{Any} { ECHO; }

<LSNARF>{Esc} { ECHO; }
<LSNARF>"\n" { fprintf (yyout, "\\n\\\n"); }
<LSNARF>{Q} { ECHO; fprintf (yyout, ")\n"); BEGIN snarf_return_state; }
<LSNARF>{Any} { ECHO; }

%%

enum filetype { C_FILE, LISP_FILE, INVALID_FILE };
/* some brain-dead headers define this ... */
#undef FALSE
#undef TRUE
enum boolean { FALSE, TRUE };

void scan_file (char *filename);
void process_C_file (void);
void process_Lisp_file (void);

int in_c;
int in_paren_counting, paren_count;
int paren_return_state;

snarf ()
{
  fprintf (yyout, "gettext(\"");
  if (in_c)
    BEGIN CSNARF;
  else
    BEGIN LSNARF;
}

bad_c_defun ()
{
  fprintf (stderr, "Warning: Invalid DEFUN encountered in C, line %d.\n",
	   yylineno);
  snarf_return_state = DO_C;
  BEGIN DO_C;
  /* REJECT; Sun's lex is broken!  Use Flex! */
}

bad_lisp_def ()
{
  fprintf (stderr,
	   "Warning: Invalid defmumble encountered in Lisp, line %d.\n",
	   yylineno);
  snarf_return_state = DO_LISP;
  BEGIN DO_LISP;
  /* REJECT; Sun's lex is broken!  Use Flex! */
}

inc_paren ()
{
  if (in_paren_counting)
    paren_count++;
}

dec_paren ()
{
  if (in_paren_counting)
    {
      /* If we find a right paren without a matching left paren, it usually
	 just indicates a statement like

	 (defvar foo-mumble nil)

	 where 'nil' is the sexp we are skipping over, and there's no
	 doc string. */
      if (paren_count > 0)
	paren_count--;
      else
	unput (')');	
      if (paren_count == 0)
	{
	  in_paren_counting = 0;
	  BEGIN paren_return_state;
	}
    }
}

/* #### begin_paren_counting () does not handle recursive entries */

begin_paren_counting (int return_state)
{
  in_paren_counting = 1;
  paren_count = 0;
  paren_return_state = return_state;
}

lisp_whitespace ()
{
  if (in_paren_counting && !paren_count)
    {
      /* We got to the end of a token and we're not in a parenthesized
	 expression, so we're at the end of an sexp. */
      in_paren_counting = 0;
      BEGIN paren_return_state;
    }
}

yywrap ()
{
  return 1;
}

main (int argc, char *argv[])
{
  register int i;

  yyout = stdout;

  /* If first two args are -o FILE, output to FILE. */
  i = 1;
  if (argc > i + 1 && strcmp (argv[i], "-o") == 0) {
    yyout = fopen (argv[++i], "w");
    ++i;
  }
  /* ...Or if args are -a FILE, append to FILE. */
  if (argc > i + 1 && strcmp (argv[i], "-a") == 0) {
    yyout = fopen (argv[++i], "a");
    ++i;
  }
  if (!yyout) {
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
  yyin = fopen (filename, "r");
  if (!yyin) {
    fprintf (stderr, "Unable to open input file %s\n", filename);
    return;
  }

  fprintf (yyout, "/* %s */\n", filename);
  if (type == C_FILE)
    process_C_file ();
  else
    process_Lisp_file ();
  fputc ('\n', yyout);
  
  fclose (yyin);
}

void process_C_file ()
{
  snarf_return_state = DO_C;
  in_c = 1;
  BEGIN DO_C;
  yylex ();
}

void process_Lisp_file ()
{
  snarf_return_state = DO_LISP;
  in_c = 0;
  BEGIN DO_LISP;
  yylex ();
}

