/* Definitions for asynchronous process control in XEmacs.
   Copyright (C) 1985, 1992, 1993, 1994 Free Software Foundation, Inc.
   Copyright (C) 2002 Ben Wing.

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

#ifndef INCLUDED_process_h_
#define INCLUDED_process_h_

BEGIN_C_DECLS

/* struct Lisp_Process is defined in procimpl.h; only process-*.c need
   to know about the guts of it. */

DECLARE_LRECORD (process, Lisp_Process);
#define XPROCESS(x) XRECORD (x, process, Lisp_Process)
#define wrap_process(p) wrap_record (p, process)
#define PROCESSP(x) RECORDP (x, process)
#define CHECK_PROCESS(x) CHECK_RECORD (x, process)
#define PROCESS_LIVE_P(x) (EQ ((x)->status_symbol, Qrun))
#define PROCESS_READABLE_P(x) (!NILP ((x)->pipe_instream))

#define CHECK_LIVE_PROCESS(x) do {			\
  CHECK_PROCESS (x);					\
  if (! PROCESS_LIVE_P (XPROCESS (x)))			\
    dead_wrong_type_argument (Qprocess_live_p, (x));	\
} while (0)

#define CHECK_READABLE_PROCESS(x) do {			\
  CHECK_PROCESS (x);					\
  if (! PROCESS_READABLE_P (XPROCESS (x)))		\
    dead_wrong_type_argument (Qprocess_readable_p, (x));	\
} while (0)

EXFUN (Fdelete_process, 1);
EXFUN (Fget_buffer_process, 1);
EXFUN (Fget_process, 1);
EXFUN (Fprocess_status, 1);
EXFUN (Fprocess_kill_without_query, 2);
EXFUN (Fprocess_id, 1);

MODULE_API
DECLARE_DOESNT_RETURN (report_process_error (const char *, Lisp_Object));
DECLARE_DOESNT_RETURN (report_network_error (const char *, Lisp_Object));
extern Lisp_Object Vlisp_EXEC_SUFFIXES;

MODULE_API Ibyte *egetenv (const CIbyte *var);
MODULE_API void eputenv (const CIbyte *var, const CIbyte *value);
extern int env_initted;

extern Lisp_Object Qprocess_live_p;

Lisp_Object connect_to_file_descriptor (Lisp_Object name,
					Lisp_Object buffer,
					Lisp_Object infd,
					Lisp_Object outfd);
int connected_via_filedesc_p (Lisp_Process *p);
void kill_buffer_processes (Lisp_Object buffer);
void close_process_descs (void);
void set_process_filter (Lisp_Object proc, Lisp_Object filter,
			 int filter_does_read,
			 int set_stderr);
void update_process_status (Lisp_Object p,
			    Lisp_Object status_symbol,
			    int exit_code, int core_dumped);
void get_process_streams (Lisp_Process *p,
			  Lisp_Object *instr, Lisp_Object *outstr,
			  Lisp_Object *errstr);
int get_process_selected_p (Lisp_Process *p, int do_err);
void set_process_selected_p (Lisp_Process *p, int in_selected,
			     int err_selected);
Lisp_Process *get_process_from_usid (USID usid);

#ifdef HAVE_SOCKETS
int network_connection_p (Lisp_Object process);
#else
#define network_connection_p(x) 0
#endif

extern Lisp_Object Qclosed, Qmulticast, Qopen, Qrun, Qstop, Qtcp, Qudp;
extern Lisp_Object Vprocess_connection_type, Vprocess_list;

/* Report all recent events of a change in process status
   (either run the sentinel or output a message).
   This is done while Emacs is waiting for keyboard input.  */
void status_notify (void);
void kick_status_notify (void);
void deactivate_process (Lisp_Object proc);
Charcount read_process_output (Lisp_Object proc, int read_stderr);
int process_has_separate_stderr (Lisp_Object proc);
const char *signal_name (int signum);
Lisp_Object canonicalize_host_name (Lisp_Object host);

END_C_DECLS

/* The name of the file open to get a null file, or a data sink.
   MS-DOS, and OS/2 redefine this.  */
#ifndef NULL_DEVICE
#define NULL_DEVICE "/dev/null"
#endif

/* A string listing the possible suffixes used for executable files,
   separated by colons.  MS-DOS, and OS/2 redefine this.  */
#ifndef EXEC_SUFFIXES
#define EXEC_SUFFIXES ""
#endif

#endif /* INCLUDED_process_h_ */
