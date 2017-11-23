/* Copyright (C) 2010 Ben Wing.
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

#include <config.h>
#include "lisp.h"

/* This file used to include debugging hooks for malloc(), back when we
   shipped our own copy of gmalloc.c. Now we just use the system malloc, and
   this file has code to debug GCPROs. */

#if defined(DEBUG_INPUT_BLOCKING) || defined (DEBUG_GCPRO)

/* Note: There is no more input blocking in XEmacs */
typedef enum {
  block_type, unblock_type, totally_type,
  gcpro1_type, gcpro2_type, gcpro3_type, gcpro4_type, gcpro5_type,
  ungcpro_type
} blocktype;

struct block_input_history_struct
{
  char *file;
  int line;
  blocktype type;
  int value;
};

typedef struct block_input_history_struct block_input_history;

#endif /* DEBUG_INPUT_BLOCKING || DEBUG_GCPRO */

#ifdef DEBUG_INPUT_BLOCKING

int blhistptr;

#define BLHISTLIMIT 1000

block_input_history blhist[BLHISTLIMIT];

note_block_input (char *file, int line)
{
  note_block (file, line, block_type);
  assert (interrupt_input_blocked <= 2);
}

note_unblock_input (char* file, int line)
{
  note_block (file, line, unblock_type);
}

note_totally_unblocked (char* file, int line)
{
  note_block (file, line, totally_type);
}

note_block (char *file, int line, blocktype type)
{
  blhist[blhistptr].file = file;
  blhist[blhistptr].line = line;
  blhist[blhistptr].type = type;
  blhist[blhistptr].value = interrupt_input_blocked;

  blhistptr++;
  if (blhistptr >= BLHISTLIMIT)
    blhistptr = 0;
}

#endif /* DEBUG_INPUT_BLOCKING */


#ifdef DEBUG_GCPRO

int gcprohistptr;
#define GCPROHISTLIMIT 1000
block_input_history gcprohist[GCPROHISTLIMIT];

static void
log_gcpro (char *file, int line, struct gcpro *value, blocktype type)
{
  if (type == ungcpro_type)
    {
      if (value == gcprolist) goto OK;
      assert (gcprolist);
      if (value == gcprolist->next) goto OK;
      assert (gcprolist->next);
      if (value == gcprolist->next->next) goto OK;
      assert (gcprolist->next->next);
      if (value == gcprolist->next->next->next) goto OK;
      assert (gcprolist->next->next->next);
      if (value == gcprolist->next->next->next->next) goto OK;
      ABORT ();
    OK:;
    }
  gcprohist[gcprohistptr].file = file;
  gcprohist[gcprohistptr].line = line;
  gcprohist[gcprohistptr].type = type;
  gcprohist[gcprohistptr].value = (int) value;
  gcprohistptr++;
  if (gcprohistptr >= GCPROHISTLIMIT)
    gcprohistptr = 0;
}

void
debug_gcpro1 (char *file, int line, struct gcpro *gcpro1, Lisp_Object *var)
{
  gcpro1->next = gcprolist; gcpro1->var = var; gcpro1->nvars = 1;
  gcprolist = gcpro1;
  log_gcpro (file, line, gcpro1, gcpro1_type);
}

void
debug_gcpro2 (char *file, int line, struct gcpro *gcpro1, struct gcpro *gcpro2,
	      Lisp_Object *var1, Lisp_Object *var2)
{
  gcpro1->next = gcprolist; gcpro1->var = var1; gcpro1->nvars = 1;
  gcpro2->next = gcpro1; gcpro2->var = var2; gcpro2->nvars = 1;
  gcprolist = gcpro2;
  log_gcpro (file, line, gcpro2, gcpro2_type);
}

void
debug_gcpro3 (char *file, int line, struct gcpro *gcpro1, struct gcpro *gcpro2,
	      struct gcpro *gcpro3, Lisp_Object *var1, Lisp_Object *var2,
	      Lisp_Object *var3)
{
  gcpro1->next = gcprolist; gcpro1->var = var1; gcpro1->nvars = 1;
  gcpro2->next = gcpro1; gcpro2->var = var2; gcpro2->nvars = 1;
  gcpro3->next = gcpro2; gcpro3->var = var3; gcpro3->nvars = 1;
  gcprolist = gcpro3;
  log_gcpro (file, line, gcpro3, gcpro3_type);
}

void
debug_gcpro4 (char *file, int line, struct gcpro *gcpro1, struct gcpro *gcpro2,
	      struct gcpro *gcpro3, struct gcpro *gcpro4, Lisp_Object *var1,
	      Lisp_Object *var2, Lisp_Object *var3, Lisp_Object *var4)
{
  log_gcpro (file, line, gcpro4, gcpro4_type);
  gcpro1->next = gcprolist; gcpro1->var = var1; gcpro1->nvars = 1;
  gcpro2->next = gcpro1; gcpro2->var = var2; gcpro2->nvars = 1;
  gcpro3->next = gcpro2; gcpro3->var = var3; gcpro3->nvars = 1;
  gcpro4->next = gcpro3; gcpro4->var = var4; gcpro4->nvars = 1;
  gcprolist = gcpro4;
}

void
debug_gcpro5 (char *file, int line, struct gcpro *gcpro1, struct gcpro *gcpro2,
	      struct gcpro *gcpro3, struct gcpro *gcpro4, struct gcpro *gcpro5,
	      Lisp_Object *var1, Lisp_Object *var2, Lisp_Object *var3,
	      Lisp_Object *var4, Lisp_Object *var5)
{
  log_gcpro (file, line, gcpro5, gcpro5_type);
  gcpro1->next = gcprolist; gcpro1->var = var1; gcpro1->nvars = 1;
  gcpro2->next = gcpro1; gcpro2->var = var2; gcpro2->nvars = 1;
  gcpro3->next = gcpro2; gcpro3->var = var3; gcpro3->nvars = 1;
  gcpro4->next = gcpro3; gcpro4->var = var4; gcpro4->nvars = 1;
  gcpro5->next = gcpro4; gcpro5->var = var5; gcpro5->nvars = 1;
  gcprolist = gcpro5;
}

void
debug_ungcpro (char *file, int line, struct gcpro *gcpro1)
{
  log_gcpro (file, line, gcpro1, ungcpro_type);
  gcprolist = gcpro1->next;
}


/* To be called from the debugger */
void show_gcprohist (void);
void
show_gcprohist (void)
{
  int i, j;
  for (i = 0, j = gcprohistptr;
       i < GCPROHISTLIMIT;
       i++, j++)
    {
      if (j >= GCPROHISTLIMIT)
	j = 0;
      printf ("%3d  %s		%d	%s	0x%x\n",
	      j, gcprohist[j].file, gcprohist[j].line,
	      (gcprohist[j].type == gcpro1_type ? "GCPRO1" :
	       gcprohist[j].type == gcpro2_type ? "GCPRO2" :
	       gcprohist[j].type == gcpro3_type ? "GCPRO3" :
	       gcprohist[j].type == gcpro4_type ? "GCPRO4" :
	       gcprohist[j].type == gcpro5_type ? "GCPRO5" :
	       gcprohist[j].type == ungcpro_type ? "UNGCPRO" : "???"),
	      gcprohist[j].value);
    }
  fflush (stdout);
}

#endif /* DEBUG_GCPRO */
