/* Processes implementation header
   Copyright (C) 1985, 1992, 1993, 1994 Free Software Foundation, Inc.

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

/* This file must be only included by the process implementation files:
   process-unix.c, process-msw.c etc. The Lisp_Process structure and other
   contents of this file is not exported to the rest of the world */

#ifndef _XEMACS_PROCIMPL_H_
#define _XEMACS_PROCIMPL_H_

struct Lisp_Process;

/*
 * Structure which keeps methods of the process implementation.
 * There is only one object of this class exists in a particular
 * XEmacs implementation.
 */

/* #### Comment me... */

struct process_methods
{
  void (*mark_process_data) (struct Lisp_Process *proc,
			     void (*markobj) (Lisp_Object));
  void (*print_process_data) (struct Lisp_Process *proc,
			      Lisp_Object printcharfun);
  void (*finalize_process_data) (struct Lisp_Process *proc, int for_disksave);
  void (*alloc_process_data) (struct Lisp_Process *p);
  void (*init_process_io_handles) (struct Lisp_Process *p,
				   void* in, void* out, int flags);
  int  (*create_process) (struct Lisp_Process *p,
			  Lisp_Object *argv, int nargv,
			  Lisp_Object program, Lisp_Object cur_dir);
  int  (*tooltalk_connection_p) (struct Lisp_Process *p);
#ifdef HAVE_SOCKETS
  void (*open_network_stream) (Lisp_Object name, Lisp_Object host,
			       Lisp_Object service, Lisp_Object family,
			       void** vinfd, void** voutfd);
#ifdef HAVE_MULTICAST
  void (*open_multicast_group) (Lisp_Object name, Lisp_Object dest,
				Lisp_Object port, Lisp_Object ttl,
				void** vinfd, void** voutfd);
#endif /* HAVE_MULTICAST */
#endif /* HAVE_SOCKETS */
  Lisp_Object (*canonicalize_host_name) (Lisp_Object host);
  int  (*set_window_size) (struct Lisp_Process* p, int height, int width);
  void (*send_process) (Lisp_Object proc, struct lstream* lstream);
  void (*reap_exited_processes) (void);
  void (*update_status_if_terminated) (struct Lisp_Process* p);
  void (*kill_child_process) (Lisp_Object proc, int signo,
			      int current_group, int nomsg);
  int  (*kill_process_by_pid) (int pid, int sigcode);
  int  (*process_send_eof) (Lisp_Object proc);
  Lisp_Object (*get_tty_name) (struct Lisp_Process *p);
  USID (*deactivate_process) (struct Lisp_Process *p);
  void (*init_process) (void);
};

extern struct process_methods the_process_methods;

/*
 * Accessors for process_methods
 */

#define HAS_PROCMETH_P(name) (the_process_methods.name != 0)
#define PROCMETH(name, par) ((the_process_methods.name) par)
#define PROCMETH_OR_GIVEN(name, par, given) (HAS_PROCMETH_P(name) ? PROCMETH(name, par) : (given))
#define MAYBE_PROCMETH(name, par) do { if (HAS_PROCMETH_P(name)) PROCMETH(name, par); } while (0);
#define MAYBE_LISP_PROCMETH(name, par) PROCMETH_OR_GIVEN(name, par, Qnil)
#define MAYBE_INT_PROCMETH(name, par) PROCMETH_OR_GIVEN(name, par, 0)
#define PROCESS_HAS_METHOD(os, name) the_process_methods.name = os##_##name

/*
 * Structure records pertinent information about open channels.
 * There is one channel associated with each process.
 */

struct Lisp_Process
{
  struct lcrecord_header header;
  /* Name of this process */
  Lisp_Object name;
  /* List of command arguments that this process was run with */
  Lisp_Object command;
  /* (funcall FILTER PROC STRING)  (if FILTER is non-nil)
     to dispose of a bunch of chars from the process all at once */
  Lisp_Object filter;
  /* (funcall SENTINEL PROCESS) when process state changes */
  Lisp_Object sentinel;
  /* Buffer that output is going to */
  Lisp_Object buffer;
  /* Marker set to end of last buffer-inserted output from this process */
  Lisp_Object mark;
  /* Lisp_Int of subprocess' PID, or a cons of
     service/host if this is really a network connection */
  Lisp_Object pid;

  /* Symbol indicating status of process.
     This may be a symbol: run, stop, exit, signal */
  Lisp_Object status_symbol;

  /* Exit code if process has terminated,
     signal which stopped/interrupted process
     or 0 if process is running */
  int exit_code;
  /* Non-false if process has exited and "dumped core" on its way down */
  char core_dumped;

  /* This next field is only actually used #ifdef ENERGIZE */
  /* if this flag is not NIL, then filter will do the read on the
     channel, rather than having a call to make_string.
     This only works if the filter is a subr. */
  char filter_does_read;
  /* Non-nil means kill silently if Emacs is exited.  */
  char kill_without_query;
  char selected;
  /* Event-count of last event in which this process changed status.  */
  volatile int tick;
  /* Event-count of last such event reported.  */
  int update_tick;
  /* Low level streams used in input and output, connected to child */
  Lisp_Object pipe_instream;
  Lisp_Object pipe_outstream;
#ifdef FILE_CODING
  /* Data end streams, decoding and encoding pipe_* streams */
  Lisp_Object coding_instream;
  Lisp_Object coding_outstream;
#endif

  /* Implementation dependent data */
  void *process_data;
};

/* Macros to refer to data connection streams */
#ifdef FILE_CODING
#define DATA_INSTREAM(p) (p)->coding_instream
#define DATA_OUTSTREAM(p) (p)->coding_outstream
#else
#define DATA_INSTREAM(p) (p)->pipe_instream
#define DATA_OUTSTREAM(p) (p)->pipe_outstream
#endif

/* Random externs from process.c */
extern Lisp_Object Qrun, Qstop, Qopen, Qclosed;
extern Lisp_Object Qtcpip;
extern Lisp_Object Vprocess_connection_type;
extern Lisp_Object Vprocess_list;

extern struct hash_table *usid_to_process;

extern volatile int process_tick;

extern int windowed_process_io;

#ifdef HAVE_MULTICAST
extern Lisp_Object Qmulticast;
#endif

#ifdef PROCESS_IO_BLOCKING
extern Lisp_Object network_stream_blocking_port_list;
#endif  /* PROCESS_IO_BLOCKING */

Lisp_Object make_process_internal (Lisp_Object name);
void init_process_io_handles (struct Lisp_Process *p, void* in,
			      void* out, int flags);
void send_process (Lisp_Object proc,
		   Lisp_Object relocatable,
		   CONST Bufbyte *nonrelocatable,
		   int start, int len);

#endif /* _XEMACS_PROCIMPL_H_ */
