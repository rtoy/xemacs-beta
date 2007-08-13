/* CCL -- Code Conversion Language Interpreter
   Copyright (C) 1992, 1995 Free Software Foundation, Inc.
   Copyright (C) 1995 Sun Microsystems, Inc.

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

/* Synched up with: Mule 2.3.  Not in FSF. */

#include <config.h>
#include "lisp.h"

#include "buffer.h"
#include "mule-coding.h"

/* CCL operators */
#define CCL_SetCS		0x00
#define CCL_SetCL		0x01
#define CCL_SetR		0x02
#define CCL_SetA		0x03
#define CCL_Jump		0x04
#define CCL_JumpCond		0x05
#define CCL_WriteJump		0x06
#define CCL_WriteReadJump	0x07
#define CCL_WriteCJump		0x08
#define CCL_WriteCReadJump	0x09
#define CCL_WriteSJump		0x0A
#define CCL_WriteSReadJump	0x0B
#define CCL_WriteAReadJump	0x0C
#define CCL_Branch		0x0D
#define CCL_Read1		0x0E
#define CCL_Read2		0x0F
#define CCL_ReadBranch		0x10
#define CCL_Write1		0x11
#define CCL_Write2		0x12
#define CCL_WriteC		0x13
#define CCL_WriteS		0x14
#define CCL_WriteA		0x15
#define CCL_End			0x16
#define CCL_SetSelfCS		0x17
#define CCL_SetSelfCL		0x18
#define CCL_SetSelfR		0x19
#define CCL_SetExprCL		0x1A
#define CCL_SetExprR		0x1B
#define CCL_JumpCondC		0x1C
#define CCL_JumpCondR		0x1D
#define CCL_ReadJumpCondC	0x1E
#define CCL_ReadJumpCondR	0x1F

#define CCL_PLUS	0x00
#define CCL_MINUS	0x01
#define CCL_MUL		0x02
#define CCL_DIV		0x03
#define CCL_MOD		0x04
#define CCL_AND		0x05
#define CCL_OR		0x06
#define CCL_XOR		0x07
#define CCL_LSH		0x08
#define CCL_RSH		0x09
#define CCL_LSH8	0x0A
#define CCL_RSH8	0x0B
#define CCL_DIVMOD	0x0C
#define CCL_LS		0x10
#define CCL_GT		0x11
#define CCL_EQ		0x12
#define CCL_LE		0x13
#define CCL_GE		0x14
#define CCL_NE		0x15

/* Header of CCL compiled code */
#define CCL_HEADER_EOF		0
#define CCL_HEADER_MAIN		1

#define CCL_STAT_SUCCESS	0
#define CCL_STAT_SUSPEND	1
#define CCL_STAT_INVALID_CMD	2

#define CCL_SUCCESS			\
  ccl->status = CCL_STAT_SUCCESS;	\
  goto ccl_finish
#define CCL_SUSPEND			\
  ccl->ic = --ic;			\
  ccl->status = CCL_STAT_SUSPEND;	\
  goto ccl_finish
#define CCL_INVALID_CMD			\
  ccl->status = CCL_STAT_INVALID_CMD;	\
  goto ccl_error_handler

#define CCL_WRITE_CHAR(ch) do					\
{								\
  if (!src)							\
    {								\
      CCL_INVALID_CMD;						\
    }								\
  else								\
    {								\
      /* !!#### is this correct for both directions????? */	\
      Bufbyte __buf__[MAX_EMCHAR_LEN];				\
      int __len__;						\
      __len__ = set_charptr_emchar (__buf__, ch);		\
      Dynarr_add_many (dst, __buf__, __len__);			\
    }								\
} while (0)

#define CCL_WRITE_STRING(len) do			\
{							\
  if (!src)						\
    {							\
      CCL_INVALID_CMD;					\
    }							\
  else							\
    {							\
      for (j = 0; j < len; j++)				\
	Dynarr_add (dst, XINT (prog[ic + 1 + j]));	\
    }							\
} while (0)

#define CCL_READ_CHAR(r) do		\
{					\
  if (!src)				\
    {					\
      CCL_INVALID_CMD;			\
    }					\
  else if (s < s_end)			\
    r = *s++;				\
  else if (end_flag)			\
    {					\
      ic = XINT (prog[CCL_HEADER_EOF]);	\
      continue;				\
    }					\
  else					\
    {					\
      CCL_SUSPEND;			\
    }					\
} while (0)


/* Run a CCL program.  The initial state and program are contained in
   CCL.  SRC, if non-zero, specifies a source string (of size N)
   to read bytes from, and DST, of non-zero, specifies a destination
   Dynarr to write bytes to.  If END_FLAG is set, it means that
   the end section of the CCL program should be run rather than
   the normal section.

   For CCL programs that do not involve code conversion (e.g.
   converting a single character into a font index), all parameters
   but the first will usually be 0. */

int
ccl_driver (struct ccl_program *ccl, CONST unsigned char *src,
	    unsigned_char_dynarr *dst, int n, int end_flag)
{
  int code, op, rrr, cc, i, j;
  CONST unsigned char *s = NULL, *s_end = NULL;
  int   ic = ccl->ic;
  int *reg = ccl->reg;
  Lisp_Object *prog = ccl->prog;

  if (!ic)
    ic = CCL_HEADER_MAIN;

  if (src)
    {
      s = src;
      s_end = s + n;
    }

  while (1)
    {
      code = XINT (prog[ic++]);
      op = code & 0x1F;
      rrr = (code >> 5) & 0x7;
      cc = code >> 8;

      switch (op)
	{
	case CCL_SetCS:
	  reg[rrr] = cc; continue;
	case CCL_SetCL:
	  reg[rrr] = XINT (prog[ic++]); continue;
	case CCL_SetR:
	  reg[rrr] = reg[cc]; continue;
	case CCL_SetA:
	  cc = reg[cc];
	  i = XINT (prog[ic++]);
	  if (cc >= 0 && cc < i)
	    reg[rrr] = XINT (prog[ic + cc]);
	  ic += i;
	  continue;
	case CCL_Jump:
	  ic = cc; continue;
	case CCL_JumpCond:
	  if (!reg[rrr])
	    ic = cc;
	  continue;
	case CCL_WriteJump:
	  CCL_WRITE_CHAR (reg[rrr]);
	  ic = cc;
	  continue;
	case CCL_WriteReadJump:
	  if (ccl->status != CCL_STAT_SUSPEND)
	    {
	      CCL_WRITE_CHAR (reg[rrr]);
	    }
	  else
	    ccl->status = CCL_STAT_SUCCESS;
	  CCL_READ_CHAR (reg[rrr]);
	  ic = cc;
	  continue;
	case CCL_WriteCJump:
	  CCL_WRITE_CHAR (XINT (prog[ic]));
	  ic = cc;
	  continue;
	case CCL_WriteCReadJump:
	  if (ccl->status != CCL_STAT_SUSPEND)
	    {
	      CCL_WRITE_CHAR (XINT (prog[ic]));
	    }
	  else
	    ccl->status = CCL_STAT_SUCCESS;
	  CCL_READ_CHAR (reg[rrr]);
	  ic = cc;
	  continue;
	case CCL_WriteSJump:
	  i = XINT (prog[ic]);
	  CCL_WRITE_STRING (i);
	  ic = cc;
	  continue;
	case CCL_WriteSReadJump:
	  if (ccl->status != CCL_STAT_SUSPEND)
	    {
	      i = XINT (prog[ic]);
	      CCL_WRITE_STRING (i);
	    }
	  else
	    ccl->status = CCL_STAT_SUCCESS;
	  CCL_READ_CHAR (reg[rrr]);
	  ic = cc;
	  continue;
	case CCL_WriteAReadJump:
	  if (ccl->status != CCL_STAT_SUSPEND)
	    {
	      i = XINT (prog[ic]);
	      if (reg[rrr] >= 0 && reg[rrr] < i)
		CCL_WRITE_CHAR (XINT (prog[ic + 1 + reg[rrr]]));
	    }
	  else
	    ccl->status = CCL_STAT_SUCCESS;
	  CCL_READ_CHAR (reg[rrr]);
	  ic = cc;
	  continue;
	case CCL_ReadBranch:
	  CCL_READ_CHAR (reg[rrr]);
	case CCL_Branch:
	  ic = XINT (prog[ic + (((unsigned int) reg[rrr] < cc)
				? reg[rrr] : cc)]);
	  continue;
	case CCL_Read1:
	  CCL_READ_CHAR (reg[rrr]);
	  continue;
	case CCL_Read2:
	  CCL_READ_CHAR (reg[rrr]);
	  CCL_READ_CHAR (reg[cc]);
	  continue;
	case CCL_Write1:
	  CCL_WRITE_CHAR (reg[rrr]);
	  continue;
	case CCL_Write2:
	  CCL_WRITE_CHAR (reg[rrr]);
	  CCL_WRITE_CHAR (reg[cc]);
	  continue;
	case CCL_WriteC:
	  i = XINT (prog[ic++]);
	  CCL_WRITE_CHAR (i);
	  continue;
	case CCL_WriteS:
	  cc = XINT (prog[ic]);
	  CCL_WRITE_STRING (cc);
	  ic += cc + 1;
	  continue;
	case CCL_WriteA:
	  i = XINT (prog[ic++]);
	  cc = reg[rrr];
	  if (cc >= 0 && cc < i)
	    CCL_WRITE_CHAR (XINT (prog[ic + cc]));
	  ic += i;
	  continue;
	case CCL_End:
	  CCL_SUCCESS;
	case CCL_SetSelfCS:
	  i = cc;
	  op = XINT (prog[ic++]);
	  goto ccl_set_self;
	case CCL_SetSelfCL:
	  i = XINT (prog[ic++]);
	  op = XINT (prog[ic++]);
	  goto ccl_set_self;
	case CCL_SetSelfR:
	  i = reg[cc];
	  op = XINT (prog[ic++]);
	  ccl_set_self:
	  switch (op)
	    {
	    case CCL_PLUS:   reg[rrr] += i;  break;
	    case CCL_MINUS:  reg[rrr] -= i;  break;
	    case CCL_MUL:    reg[rrr] *= i;  break;
	    case CCL_DIV:    reg[rrr] /= i;  break;
	    case CCL_MOD:    reg[rrr] %= i;  break;
	    case CCL_AND:    reg[rrr] &= i;  break;
	    case CCL_OR:     reg[rrr] |= i;  break;
	    case CCL_XOR:    reg[rrr] ^= i;  break;
	    case CCL_LSH:    reg[rrr] <<= i; break;
	    case CCL_RSH:    reg[rrr] >>= i; break;
	    case CCL_LSH8:   reg[rrr] <<= 8; reg[rrr] |= i; break;
	    case CCL_RSH8:   reg[7] = reg[rrr] & 0xFF; reg[rrr] >>= 8; break;
	    case CCL_DIVMOD: reg[7] = reg[rrr] % i;    reg[rrr] /= i;  break;
	    case CCL_LS:     reg[rrr] = reg[rrr] < i;  break;
	    case CCL_GT:     reg[rrr] = reg[rrr] > i;  break;
	    case CCL_EQ:     reg[rrr] = reg[rrr] == i; break;
	    case CCL_LE:     reg[rrr] = reg[rrr] <= i; break;
	    case CCL_GE:     reg[rrr] = reg[rrr] >= i; break;
	    case CCL_NE:     reg[rrr] = reg[rrr] != i; break;
	    default: CCL_INVALID_CMD;
	    }
	    continue;
	case CCL_SetExprCL:
	  i = reg[cc];
	  j = XINT (prog[ic++]);
	  op = XINT (prog[ic++]);
	  cc = 0;
	  goto ccl_set_expr;
	case CCL_SetExprR:
	  i = reg[cc];
	  j = reg[XINT (prog[ic++])];
	  op = XINT (prog[ic++]);
	  cc = 0;
	  goto ccl_set_expr;
	case CCL_ReadJumpCondC:
	  CCL_READ_CHAR (reg[rrr]);
	case CCL_JumpCondC:
	  i = reg[rrr];
	  j = XINT (prog[ic++]);
	  rrr = 7;
	  op = XINT (prog[ic++]);
	  goto ccl_set_expr;
	case CCL_ReadJumpCondR:
	  CCL_READ_CHAR (reg[rrr]);
	case CCL_JumpCondR:
	  i = reg[rrr];
	  j = reg[XINT (prog[ic++])];
	  rrr = 7;
	  op = XINT (prog[ic++]);
	  ccl_set_expr:
	  switch (op)
	    {
	    case CCL_PLUS:   reg[rrr] = i + j;  break;
	    case CCL_MINUS:  reg[rrr] = i - j;  break;
	    case CCL_MUL:    reg[rrr] = i * j;  break;
	    case CCL_DIV:    reg[rrr] = i / j;  break;
	    case CCL_MOD:    reg[rrr] = i % j;  break;
	    case CCL_AND:    reg[rrr] = i & j;  break;
	    case CCL_OR:     reg[rrr] = i | j;  break;
	    case CCL_XOR:    reg[rrr] = i ^ j;; break;
	    case CCL_LSH:    reg[rrr] = i << j; break;
	    case CCL_RSH:    reg[rrr] = i >> j; break;
	    case CCL_LSH8:   reg[rrr] = (i << 8) | j; break;
	    case CCL_RSH8:   reg[rrr] = i >> 8; reg[7] = i & 0xFF; break;
	    case CCL_DIVMOD: reg[rrr] = i / j;  reg[7] = i % j;    break;
	    case CCL_LS:     reg[rrr] = i < j;  break;
	    case CCL_GT:     reg[rrr] = i > j;  break;
	    case CCL_EQ:     reg[rrr] = i == j; break;
	    case CCL_LE:     reg[rrr] = i <= j; break;
	    case CCL_GE:     reg[rrr] = i >= j; break;
	    case CCL_NE:     reg[rrr] = i != j; break;
	    default: CCL_INVALID_CMD;
	    }
	    if (cc && !reg[rrr])
	      ic = cc;
	    continue;
	default:
	  CCL_INVALID_CMD;
	}
    }

  ccl_error_handler:
  if (dst)
    {
      char buf[200];
      switch (ccl->status)
	{
	case CCL_STAT_INVALID_CMD:
	  sprintf (buf, "CCL: Invalid command (%x).\n", op);
	  break;
	default:
	  sprintf (buf, "CCL: Unknown error type (%d).\n", ccl->status);
	}
      Dynarr_add_many (dst, (unsigned char *) buf, strlen (buf));
    }

  ccl_finish:
  ccl->ic = ic;
  if (dst)
    return Dynarr_length (dst);
  else
    return 0;
}

/* Set up CCL to execute CCL program VAL, with initial register values
   coming from REGS (NUMREGS of them are specified) and initial
   instruction counter coming from INITIAL_IC (a value of 0 means
   start at the beginning of the program, wherever that is).
   */

void
set_ccl_program (struct ccl_program *ccl, Lisp_Object val, int *regs,
		 int numregs, int initial_ic)
{
  int i;

  ccl->saved_vector = val;
  ccl->prog = XVECTOR_DATA (val);
  ccl->size = XVECTOR_LENGTH (val);
  if (initial_ic == 0)
    ccl->ic = CCL_HEADER_MAIN;
  else
    ccl->ic = initial_ic;
  for (i = 0; i < numregs; i++)
    ccl->reg[i] = regs[i];
  for (; i < 8; i++)
    ccl->reg[i] = 0;
  ccl->end_flag = 0;
  ccl->status = 0;
}

#ifdef emacs

static void
set_ccl_program_from_lisp_values (struct ccl_program *ccl,
				  Lisp_Object prog,
				  Lisp_Object status)
{
  int i;
  int intregs[8];
  int ic;

  CHECK_VECTOR (prog);
  CHECK_VECTOR (status);

  if (XVECTOR_LENGTH (status) != 9)
    signal_simple_error ("Must specify values for the eight registers and IC",
			 status);
  for (i = 0; i < 8; i++)
    {
      Lisp_Object regval = XVECTOR_DATA (status)[i];
      if (NILP (regval))
	intregs[i] = 0;
      else
	{
	  CHECK_INT (regval);
	  intregs[i] = XINT (regval);
	}
    }

  {
    Lisp_Object lic = XVECTOR_DATA (status)[8];
    if (NILP (lic))
      ic = 0;
    else
      {
	CHECK_NATNUM (lic);
	ic = XINT (lic);
      }
  }

  set_ccl_program (ccl, prog, intregs, 8, ic);
}

static void
set_lisp_status_from_ccl_program (Lisp_Object status,
				  struct ccl_program *ccl)
{
  int i;

  for (i = 0; i < 8; i++)
    XVECTOR_DATA (status)[i] = make_int (ccl->reg[i]);
  XVECTOR_DATA (status)[8] = make_int (ccl->ic);
}


DEFUN ("execute-ccl-program", Fexecute_ccl_program, 2, 2, 0, /*
Execute CCL-PROGRAM with registers initialized by STATUS.
CCL-PROGRAM is a vector of compiled CCL code created by `ccl-compile'.
STATUS must be a vector of nine values, specifying the initial value
 for the R0, R1 .. R7 registers and for the instruction counter IC.
A nil value for a register initializer causes the register to be set
to 0.  A nil value for the IC initializer causes execution to start
 at the beginning of the program.
When the program is done, STATUS is modified (by side-effect) to contain
 the ending values for the corresponding registers and IC.
*/
       (ccl_program, status))
{
  struct ccl_program ccl;

  set_ccl_program_from_lisp_values (&ccl, ccl_program, status);
  ccl_driver (&ccl, 0, 0, 0, 0);
  set_lisp_status_from_ccl_program (status, &ccl);
  return Qnil;
}

DEFUN ("execute-ccl-program-string", Fexecute_ccl_program_string, 3, 3, 0, /*
Execute CCL-PROGRAM with initial STATUS on STRING.
CCL-PROGRAM is a vector of compiled CCL code created by `ccl-compile'.
STATUS must be a vector of nine values, specifying the initial value
 for the R0, R1 .. R7 registers and for the instruction counter IC.
A nil value for a register initializer causes the register to be set
to 0.  A nil value for the IC initializer causes execution to start
 at the beginning of the program.
When the program is done, STATUS is modified (by side-effect) to contain
 the ending values for the corresponding registers and IC.
Returns the resulting string.
*/
       (ccl_program, status, str))
{
  struct ccl_program ccl;
  Lisp_Object val;
  int len;
  unsigned_char_dynarr *outbuf;

  set_ccl_program_from_lisp_values (&ccl, ccl_program, status);
  CHECK_STRING (str);

  outbuf = Dynarr_new (unsigned char);
  len = ccl_driver (&ccl, XSTRING_DATA (str), outbuf, XSTRING_LENGTH (str), 0);
  ccl_driver (&ccl, (unsigned char *) "", outbuf, 0, 1);
  set_lisp_status_from_ccl_program (status, &ccl);

  val = make_string (Dynarr_atp (outbuf, 0), len);
  Dynarr_free (outbuf);
  return val;
}

DEFUN ("ccl-reset-elapsed-time", Fccl_reset_elapsed_time, 0, 0, 0, /*
Reset the internal value which holds the time elapsed by CCL interpreter.
*/
       ())
{
  error ("Not yet implemented; use `current-process-time'");
  return Qnil;
}

DEFUN ("ccl-elapsed-time", Fccl_elapsed_time, 0, 0, 0, /*
Return the time elapsed by CCL interpreter as cons of user and system time.
This measures processor time, not real time.  Both values are floating point
numbers measured in seconds.  If only one overall value can be determined,
the return value will be a cons of that value and 0.
*/
       ())
{
  error ("Not yet implemented; use `current-process-time'");
  return Qnil;
}

void
syms_of_mule_ccl (void)
{
  DEFSUBR (Fexecute_ccl_program);
  DEFSUBR (Fexecute_ccl_program_string);
  DEFSUBR (Fccl_reset_elapsed_time);
  DEFSUBR (Fccl_elapsed_time);
}

#else  /* not emacs */
#ifdef standalone

#define INBUF_SIZE 1024
#define MAX_CCL_CODE_SIZE 4096

void
main (int argc, char **argv)
{
  FILE *progf;
  char inbuf[INBUF_SIZE];
  unsigned_char_dynarr *outbuf;
  struct ccl_program ccl;
  int i;
  Lisp_Object ccl_prog = make_vector (MAX_CCL_CODE_SIZE);

  if (argc < 2)
    {
      fprintf (stderr,
	       "Usage: %s ccl_program_file_name <infile >outfile\n",
	       argv[0]);
      exit (1);
    }

  if ((progf = fopen (argv[1], "r")) == NULL)
    {
      fprintf (stderr, "%s: Can't read file %s", argv[0], argv[1]);
      exit (1);
    }

  XVECTOR_LENGTH (ccl_prog) = 0;
  while (fscanf (progf, "%x", &i) == 1)
    XVECTOR_DATA (ccl_prog)[XVECTOR_LENGTH (ccl_prog)++] = make_int (i);
  set_ccl_program (&ccl, ccl_prog, 0, 0, 0);

  outbuf = Dynarr_new (unsigned char);

  while ((i = fread (inbuf, 1, INBUF_SIZE, stdin)) == INBUF_SIZE)
    {
      i = ccl_driver (&ccl, inbuf, outbuf, INBUF_SIZE, 0);
      fwrite (Dynarr_atp (outbuf, 0), 1, i, stdout);
    }
  if (i)
    {
      i = ccl_driver (&ccl, inbuf, outbuf, i, 1);
      fwrite (Dynarr_atp (outbuf, 0), 1, i, stdout);
    }

  fclose (progf);
  exit (0);
}
#endif  /* standalone */
#endif  /* not emacs */
