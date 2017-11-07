/* Header for CCL (Code Conversion Language) interpreter.
   Copyright (C) 1995 Electrotechnical Laboratory, JAPAN.
   Copyright (C) 2010 Ben Wing.
   Licensed to the Free Software Foundation.

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

#ifndef INCLUDED_mule_ccl_h_
#define INCLUDED_mule_ccl_h_

/* Macros for exit status of CCL program.  */
enum ccl_status
  {
    CCL_STAT_SUCCESS,		/* Terminated successfully.  */
    CCL_STAT_SUSPEND_BY_SRC,	/* Terminated by empty input.  */
    CCL_STAT_SUSPEND_BY_DST,	/* Terminated by output buffer full.  */
    CCL_STAT_QUIT,		/* Terminated because of quit.  */
    CCL_STAT_INVALID_CMD,	/* Terminated because of invalid
				   command.  */
    CCL_STAT_CONVERSION_ERROR,	/* Terminated because of error in
				   converting to/from characters
				   or Unicode codepoints */
    CCL_STAT_INVALID_CHARSET,	/* Terminated because of an invalid charset
				   (unrecognizable ID, not "7-bit" when a
				   7-bit charset is wanted, etc.). */
  };

enum ccl_coding_eol
  {
    CCL_CODING_EOL_LF,		/* Line-feed only, same as Emacs' */
    CCL_CODING_EOL_CRLF,	/* Sequence of carriage-return and
				   line-feed.  */
    CCL_CODING_EOL_CR,		/* Carriage-return only.  */
  };

/* Structure to hold information about running CCL code.  Read
   comments in the file ccl.c for the detail of each field.  */
struct ccl_program {
  Elemcount size;		/* Size of the compiled code.  */
  Lisp_Object *prog;		/* Pointer into the compiled code.  */
  EMACS_INT ic;			/* Instruction Counter (index for PROG).  */
  EMACS_INT eof_ic;		/* Instruction Counter for end-of-file
				   processing code.  */
  EMACS_INT reg[8];		/* CCL registers, reg[7] is used for
				   condition flag of relational
				   operations.  */
  /* Not used in XEmacs: */
  /* int private_state; */      /* CCL instruction may use this
				   for private use, mainly for saving
				   internal states on suspending.
				   This variable is set to 0 when ccl is 
				   set up.  */

  Boolint last_block;		/* Set to 1 while processing the last
				   block. */
  enum ccl_status status;	/* Exit status of the CCL program.  */
  EMACS_INT buf_magnification;	/* Output buffer magnification.  How
				   many times bigger the output buffer
				   should be than the input buffer.  */
  int stack_idx;		/* How deep the call of CCL_Call is nested.  */
  enum ccl_coding_eol eol_type; /* When the CCL program is used for
				   encoding by a coding system, set to
				   the eol_type of the coding
				   system.  */
  /* Not used in XEmacs: */
  /* int multibyte; */		/* 1 if the source text is multibyte.  */
};

#define CCL_MODE_ENCODING 0
#define CCL_MODE_DECODING 1

#ifdef DEBUG_XEMACS
#define CCL_DEBUG
#endif

/* If OBJECT is symbol designating a registered CCL program, return it.
   Else if OBJECT is a vector CCL program with no unresolved symbols, return
   it.
   Else, if OBJECT is a vector CCL program with unresolved symbols, return a
   newly-created vector reflecting the CCL program with all symbols
   resolved, if that is currently possible in this XEmacs.

   Otherwise, signal `invalid-argument'. */
Lisp_Object get_ccl_program (Lisp_Object object);

/* Set up fields of the structure pointed by CCL appropriately for the
   execution of ccl program CCL_PROG (a symbol or a vector).

   If CCL_PROG is a vector and contains unresolved symbols, this function
   will throw an assertion failure. To avoid this, call get_ccl_program at
   the point that you receive the CCL program from Lisp, and use and store
   its (resolved) result instead. */
int setup_ccl_program (struct ccl_program *, Lisp_Object ccl_prog);

int ccl_driver (struct ccl_program *, const unsigned char *,
		struct buffer *, unsigned_char_dynarr *, int, int *,
		int);

void mark_ccl_program (struct ccl_program *ccl);

extern const struct sized_memory_description ccl_program_description;

EXFUN (Fregister_ccl_program, 2);

extern Lisp_Object Qccl_program;

/* Vector of CCL program names vs corresponding program data.  */
extern Lisp_Object Vccl_program_table;

/* Symbols of ccl program have this property, a value of the property
   is an index for Vccl_program_table. */
extern Lisp_Object Qccl_program_idx;

#endif /* INCLUDED_mule_ccl_h_ */
