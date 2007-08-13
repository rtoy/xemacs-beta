/* MS-DOS specific C utilities, interface.
   Copyright (C) 1993 Free Software Foundation, Inc.

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

/* Synched up with: FSF 19.28. */

#include <time.h>
#include <dpmi.h>

int dos_ttraw ();
int dos_ttcooked ();
int getdefdir (int, char*);
void unixtodos_filename (char *);
void dostounix_filename (char *);
void sleep_or_kbd_hit (int, int);
char *rootrelativepath (char *);
void init_environment ();
void internal_terminal_init ();
#ifndef _stdio_h_
int internal_flush (FILE *);
#endif
void ctrl_break_func (_go32_dpmi_registers *);
void install_ctrl_break_check ();

extern int have_mouse;
int mouse_init1 ();
void mouse_init ();
void mouse_on ();
void mouse_off ();
void mouse_moveto (int, int);
void mouse_check_moved ();
int mouse_pressed (int, int *, int *);
int mouse_released (int, int *, int *);
void init_gettimeofday ();
