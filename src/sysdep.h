/* System-dependent prototypes
   Copyright (C) 1985, 1993, 1994 Free Software Foundation, Inc.
   Copyright (C) 2001, 2002 Ben Wing.

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

/* Synched up with: FSF 19.30.  Split out of sysdep.c/emacs.c. */

#ifndef INCLUDED_sysdep_h_
#define INCLUDED_sysdep_h_

#include <setjmp.h>

#ifndef WIN32_NATIVE
extern char **environ;
#endif

#ifdef PDUMP
int pdump_read_file (char **pdump_start_pos, size_t *pdump_length);
#endif

int eight_bit_tty (struct device *d);

void stuff_char (struct console *con, int c);

void init_baud_rate (struct device *d);

void set_exclusive_use (int fd);

void set_descriptor_non_blocking (int fd);

int get_pty_max_bytes (int fd);
Ibyte get_eof_char (int fd);

/* Wait for subprocess with process id `pid' to terminate and
   make sure it will get eliminated (not remain forever as a zombie) */
void wait_for_termination (int pid);

/* flush any pending output
 * (may flush input as well; it does not matter the way we use it)
 */
void flush_pending_output (int channel);

void child_setup_tty (int out);

/* Suspend the Emacs process; give terminal to its superior.  */
void sys_suspend (void);
/* Suspend a process if possible; give terminal to its superior. */
void sys_suspend_process (int process);

void request_sigio (void);
void unrequest_sigio (void);

void stop_interrupts (void);
void start_interrupts (void);
void slow_down_interrupts (void);
void speed_up_interrupts (void);
void init_poll_for_quit (void);

/* Used so that signals can break out of system calls that aren't
   naturally interruptible. */

extern JMP_BUF break_system_call_jump;
extern volatile int can_break_system_calls;

/* Call these functions if you want to change some terminal parameter --
   reset the console, change the parameter, and init it again. */
void init_one_console (struct console *c);
void reset_one_console (struct console *c);
void init_one_device (struct device *d);
void reset_one_device (struct device *d);

/* Prepare all terminals for exiting Emacs; move the cursor to the
   bottom of the frame, turn off special modes, etc.  Called at exit.
   This calls reset_one_console() on all consoles and does some other
   stuff (e.g. fix the foreground pgroup). */

void reset_all_consoles (void);

/* Call these functions if you are going to temporarily exit back to
   the shell (e.g. when suspending).  This calls reset_one_console()
   on the initial console and does some other stuff (e.g. fix the
   foreground pgroup). */

void reset_initial_console (void);
void reinit_initial_console (void);

/* We muck around with our process group.  This function needs
   to be called at startup.  The rest of the mucking is done as
   part of the functions reset_all_consoles(), reset_initial_console(),
   and reinit_initial_console(). */

void init_process_group (void);
void munge_tty_process_group (void);
void unmunge_tty_process_group (void);

void disconnect_controlling_terminal (void);

/* Return nonzero if safe to use tabs in output.
   At the time this is called, init_sys_modes has not been done yet.  */
int tabs_safe_p (struct device *d);

/* Get terminal size from system.
   If zero or a negative number is stored, the value is not valid.  */
void get_tty_device_size (struct device *d, int *widthp, int *heightp);
/* Set the logical window size associated with descriptor FD */
int set_window_size (int fd, int height, int width);

/* Set up the proper status flags for use of a pty.  */
void setup_pty (int fd);

/* Return the address of the start of the text segment prior to unexec. */
char *start_of_text (void);
/* Return the address of the start of the data segment prior to unexec. */
void *start_of_data (void);

/* Get_system_name returns as its value a string for system-name to return. */
void init_system_name (void);

#ifdef WIN32_NATIVE
void *sbrk (unsigned long increment);
#endif

Bytecount total_data_usage (void);

#endif /* INCLUDED_sysdep_h_ */
