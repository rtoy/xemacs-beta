/* Asynchronous subprocess control for XEmacs.
   Copyright (C) 1985, 1986, 1987, 1988, 1992, 1993, 1994, 1995
   Free Software Foundation, Inc.
   Copyright (C) 1995 Sun Microsystems, Inc.
   Copyright (C) 1995, 1996 Ben Wing.

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

/* Synched up with: Mule 2.0, FSF 19.30. */

/* This file has been Mule-ized except for `start-process-internal'
   and `open-network-stream-internal'. */

#include <config.h>

#if !defined (NO_SUBPROCESSES)

/* The entire file is within this conditional */

#include "lisp.h"

#include "buffer.h"
#include "commands.h"
#include "events.h"
#include "frame.h"
#include "insdel.h"
#include "lstream.h"
#include "opaque.h"
#include "process.h"
#include "sysdep.h"
#include "window.h"

#include <setjmp.h>
#include "sysfile.h"
#include "sysproc.h"
#include "systime.h"
#include "syssignal.h" /* Always include before systty.h */

#include "systty.h"
#include "syswait.h"

/* a process object is a network connection when its pid field a cons
   (name of name of port we are connected to . foreign host name) */

/* Valid values of process->status_symbol */
Lisp_Object Qrun, Qstop; /* Qexit from eval.c, Qsignal from data.c. */
/* Qrun => Qopen, Qexit => Qclosed for "network connection" processes */
Lisp_Object Qopen, Qclosed;

/* t means use pty, nil means use a pipe,
   maybe other values to come.  */
static Lisp_Object Vprocess_connection_type;

/* FSFmacs says:

   These next two vars are non-static since sysdep.c uses them in the
   emulation of `select'.  */
/* Number of events of change of status of a process.  */
static volatile int process_tick;

/* Number of events for which the user or sentinel has been notified.  */
static int update_tick;

/* Nonzero means delete a process right away if it exits.  */
int delete_exited_processes;

/* Indexed by descriptor, gives the process (if any) for that descriptor */
Lisp_Object descriptor_to_process[MAXDESC];

/* List of process objects. */
Lisp_Object Vprocess_list;

Lisp_Object Qprocessp;

/* Buffered-ahead input char from process, indexed by channel.
   -1 means empty (no char is buffered).
   Used on sys V where the only way to tell if there is any
   output from the process is to read at least one char.
   Always -1 on systems that support FIONREAD.  */

/* FSFmacs says:
   Don't make static; need to access externally.  */
static int proc_buffered_char[MAXDESC];

#ifdef HAVE_PTYS
/* The file name of the pty opened by allocate_pty.  */

static char pty_name[24];
#endif


/************************************************************************/
/*                        the process Lisp object                       */
/************************************************************************/

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
  /* Non-0 if this is really a ToolTalk channel. */
  int connected_via_filedesc_p;
#if 0 /* FSFmacs */
  /* Perhaps it's cleaner this way, but FSFmacs
     provides no way of retrieving this value, so I'll
     leave this info with PID. */
  /* Non-nil if this is really a child process */
  Lisp_Object childp;
#endif

  /* Symbol indicating status of process.
     This may be a symbol: run, stop, exit, signal */
  Lisp_Object status_symbol;


  /* Exit code if process has terminated,
     signal which stopped/interrupted process
     or 0 if process is running */
  int exit_code;
  /* Number of this process */
  /* Non-false if process has exited and "dumped core" on its way down */
  char core_dumped;
  /* Descriptor by which we read from this process.  -1 for dead process */
  int infd;
  /* Descriptor by which we write to this process. -1 for dead process */
  int outfd;
  /* Descriptor for the tty which this process is using.
     -1 if we didn't record it (on some systems, there's no need).  */
  int subtty;
  /* Name of subprocess terminal. */
  Lisp_Object tty_name;
  /* Non-false if communicating through a pty.  */
  char pty_flag;
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
  /* streams used in input and output */
  Lisp_Object instream;
  Lisp_Object outstream;
  /* The actual filedesc stream used for output; may be different
     than OUTSTREAM under Mule */
  Lisp_Object filedesc_stream;
};

static Lisp_Object mark_process (Lisp_Object, void (*) (Lisp_Object));
static void print_process (Lisp_Object, Lisp_Object, int);
static void finalize_process (void *, int);
DEFINE_LRECORD_IMPLEMENTATION ("process", process,
                               mark_process, print_process, finalize_process,
                               0, 0, struct Lisp_Process);

static Lisp_Object
mark_process (Lisp_Object obj, void (*markobj) (Lisp_Object))
{
  struct Lisp_Process *proc = XPROCESS (obj);
  ((markobj) (proc->name));
  ((markobj) (proc->command));
  ((markobj) (proc->filter));
  ((markobj) (proc->sentinel));
  ((markobj) (proc->buffer));
  ((markobj) (proc->mark));
  ((markobj) (proc->pid));
  ((markobj) (proc->tty_name));
  ((markobj) (proc->instream));
  ((markobj) (proc->outstream));
  ((markobj) (proc->filedesc_stream));
  return (proc->status_symbol);
}

static void
print_process (Lisp_Object obj, Lisp_Object printcharfun, int escapeflag)
{
  struct Lisp_Process *proc = XPROCESS (obj);
  
  if (print_readably)
    error ("printing unreadable object #<process %s>",
           XSTRING_DATA (proc->name));
      
  if (!escapeflag)
    {
      print_internal (proc->name, printcharfun, 0);
    }
  else
    {
      int netp = network_connection_p (obj);
      write_c_string (((netp) ? GETTEXT ("#<network connection ") :
		       GETTEXT ("#<process ")), printcharfun);
      print_internal (proc->name, printcharfun, 1);
      write_c_string (((netp) ? " " : " pid "), printcharfun);
      print_internal (proc->pid, printcharfun, 1);
      write_c_string (" state:", printcharfun);
      print_internal (proc->status_symbol, printcharfun, 1);
      write_c_string (">", printcharfun);
    }
}

#ifdef HAVE_WINDOW_SYSTEM
extern void debug_process_finalization (struct Lisp_Process *p);
#endif /* HAVE_WINDOW_SYSTEM */

static void
finalize_process (void *header, int for_disksave)
{
  if (for_disksave) return; /* hmm, what would this do anyway? */
  /* #### this probably needs to be tied into the tty event loop */
  /* #### when there is one */
#ifdef HAVE_WINDOW_SYSTEM
  {
    struct Lisp_Process *p = (struct Lisp_Process *) header;
    debug_process_finalization (p);
  }
#endif /* HAVE_WINDOW_SYSTEM */
}


/************************************************************************/
/*                       basic process accessors                        */
/************************************************************************/

static SIGTYPE
close_safely_handler (int signo)
{
  EMACS_REESTABLISH_SIGNAL (signo, close_safely_handler);
  SIGRETURN;
}

static void
close_safely (int fd)
{
  stop_interrupts ();
  signal (SIGALRM, close_safely_handler);
  alarm (1);
  close (fd);
  alarm (0);
  start_interrupts ();
}

static void
close_descriptor_pair (int in, int out)
{
  if (in >= 0)
    close (in);
  if (out != in && out >= 0)
    close (out);
}

/* Close all descriptors currently in use for communication
   with subprocess.  This is used in a newly-forked subprocess
   to get rid of irrelevant descriptors.  */

void
close_process_descs (void)
{
#ifndef WINDOWSNT
  int i;
  for (i = 0; i < MAXDESC; i++)
    {
      Lisp_Object process;
      process = descriptor_to_process[i];
      if (!NILP (process))
	{
	  close_descriptor_pair (XPROCESS (process)->infd,
				 XPROCESS (process)->outfd);
	}
    }
#endif
}

void
get_process_file_descriptors (struct Lisp_Process *p, int *infd,
			      int *outfd)
{
  if (! p) abort ();
  /* the cast of MAXDESC is needed for some versions of Linux */
  assert (p->infd  >= -1 && p->infd  < ((int) (MAXDESC)));
  assert (p->outfd >= -1 && p->outfd < ((int) (MAXDESC)));
  *infd = p->infd;
  *outfd = p->outfd;
}

struct Lisp_Process *
get_process_from_input_descriptor (int infd)
{
  Lisp_Object proc;

  if ((infd  < 0) || (infd  >= ((int) (MAXDESC)))) abort ();
  proc = descriptor_to_process[infd];
  if (NILP (proc))
    return 0;
  else
    return XPROCESS (proc);
}

int
get_process_selected_p (struct Lisp_Process *p)
{
  return p->selected;
}

void
set_process_selected_p (struct Lisp_Process *p, int selected_p)
{
  p->selected = !!selected_p;
}

#ifdef HAVE_SOCKETS
int
network_connection_p (Lisp_Object process)
{
  return (GC_CONSP (XPROCESS (process)->pid));
}
#endif

int
connected_via_filedesc_p (struct Lisp_Process *p)
{
  return p->connected_via_filedesc_p;
}

DEFUN ("processp", Fprocessp, 1, 1, 0, /*
Return t if OBJECT is a process.
*/
       (obj))
{
  return ((PROCESSP (obj)) ? Qt : Qnil);
}

DEFUN ("process-list", Fprocess_list, 0, 0, 0, /*
Return a list of all processes.
*/
       ())
{
  return Fcopy_sequence (Vprocess_list);
}

DEFUN ("get-process", Fget_process, 1, 1, 0, /*
Return the process named NAME, or nil if there is none.
*/
       (name))
{
  Lisp_Object tail;

  if (GC_PROCESSP (name))
    return (name);

  if (!gc_in_progress)
    /* this only gets called during GC when emacs is going away as a result
       of a signal or crash. */
    CHECK_STRING (name);

  for (tail = Vprocess_list; GC_CONSP (tail); tail = XCDR (tail))
    {
      Lisp_Object proc = XCAR (tail);
      QUIT;
      if (!NILP (Fequal (name, XPROCESS (proc)->name)))
        return (XCAR (tail));
    }
  return Qnil;
}

DEFUN ("get-buffer-process", Fget_buffer_process, 1, 1, 0, /*
Return the (or, a) process associated with BUFFER.
BUFFER may be a buffer or the name of one.
*/
       (name))
{
  Lisp_Object buf, tail, proc;

  if (GC_NILP (name)) return Qnil;
  buf = Fget_buffer (name);
  if (GC_NILP (buf)) return Qnil;

#ifdef ENERGIZE
  {
    Lisp_Object p = energize_get_buffer_process (buf);
    if (!GC_NILP (p)) return p;
  }
#endif

  for (tail = Vprocess_list; GC_CONSP (tail); tail = XCDR (tail))
    {
      /* jwz: do not quit here - it isn't necessary, as there is no way for
	 Vprocess_list to get circular or overwhelmingly long, and this
	 function is called from layout_mode_element under redisplay. */
      /* QUIT; */
      proc = XCAR (tail);
      if (GC_PROCESSP (proc) && EQ (XPROCESS (proc)->buffer, buf))
	return proc;
    }
  return Qnil;
}

/* This is how commands for the user decode process arguments.  It
   accepts a process, a process name, a buffer, a buffer name, or nil.
   Buffers denote the first process in the buffer, and nil denotes the
   current buffer.  */

static Lisp_Object
get_process (Lisp_Object name)
{
  Lisp_Object proc;

#ifdef I18N3
  /* #### Look more closely into translating process names. */
#endif

  /* This may be called during a GC from process_send_signal() from
     kill_buffer_processes() if emacs decides to abort(). */
  if (GC_PROCESSP (name))
    return name;

  if (GC_NILP (name))
    proc = Fget_buffer_process (Fcurrent_buffer ());
  else
    {
      proc = Fget_process (name);
      if (GC_NILP (proc))
	proc = Fget_buffer_process (Fget_buffer (name));
    }

  if (!GC_NILP (proc))
    return proc;

  if (GC_NILP (name))
    error ("Current buffer has no process");
  else
    error ("Process %s does not exist", XSTRING_DATA (name));
  /* NOTREACHED */
  return Qnil; /* warning suppression */
}

DEFUN ("process-id", Fprocess_id, 1, 1, 0, /*
Return the process id of PROCESS.
This is the pid of the Unix process which PROCESS uses or talks to.
For a network connection, this value is a cons of
 (foreign-network-port . foreign-host-name).
*/
       (proc))
{
  Lisp_Object pid;
  CHECK_PROCESS (proc);

  pid = XPROCESS (proc)->pid;
  if (network_connection_p (proc))
    /* return (Qnil); */
    return (Fcons (Fcar (pid), Fcdr (pid)));
  else
    return (pid);
}

DEFUN ("process-name", Fprocess_name, 1, 1, 0, /*
Return the name of PROCESS, as a string.
This is the name of the program invoked in PROCESS,
possibly modified to make it unique among process names.
*/
       (proc))
{
  CHECK_PROCESS (proc);
  return XPROCESS (proc)->name;
}

DEFUN ("process-command", Fprocess_command, 1, 1, 0, /*
Return the command that was executed to start PROCESS.
This is a list of strings, the first string being the program executed
and the rest of the strings being the arguments given to it.
*/
       (proc))
{
  CHECK_PROCESS (proc);
  return XPROCESS (proc)->command;
}


/************************************************************************/
/*                          creating a process                          */
/************************************************************************/

static Lisp_Object
make_process_internal (Lisp_Object name)
{
  Lisp_Object val, name1;
  int i;
  struct Lisp_Process *p
    = alloc_lcrecord (sizeof (struct Lisp_Process), lrecord_process);

  /* If name is already in use, modify it until it is unused.  */
  name1 = name;
  for (i = 1; ; i++)
    {
      char suffix[10];
      Lisp_Object tem = Fget_process (name1);
      if (NILP (tem)) 
        break;
      sprintf (suffix, "<%d>", i);
      name1 = concat2 (name, build_string (suffix));
    }
  name = name1;
  p->name = name;

  p->command  = Qnil;
  p->filter   = Qnil;
  p->sentinel = Qnil;
  p->buffer   = Qnil;
  p->mark = Fmake_marker ();
  p->pid = Qnil;
  p->status_symbol = Qrun;
  p->connected_via_filedesc_p = 0;
  p->exit_code = 0;
  p->core_dumped = 0;
  p->infd   = -1;
  p->outfd  = -1;
  p->subtty = -1;
  p->tty_name = Qnil;
  p->pty_flag = 0;
  p->filter_does_read = 0;
  p->kill_without_query = 0;
  p->selected = 0;
  p->tick = 0;
  p->update_tick = 0;
  p->instream  = Qnil;
  p->outstream = Qnil;

  XSETPROCESS (val, p);

  Vprocess_list = Fcons (val, Vprocess_list);
  return (val);
}

#ifdef HAVE_PTYS

/* Open an available pty, returning a file descriptor.
   Return -1 on failure.
   The file name of the terminal corresponding to the pty
   is left in the variable pty_name.  */

static int
allocate_pty (void)
{
  struct stat stb;
  int c, i;
  int fd;

  /* Some systems name their pseudoterminals so that there are gaps in
     the usual sequence - for example, on HP9000/S700 systems, there
     are no pseudoterminals with names ending in 'f'.  So we wait for
     three failures in a row before deciding that we've reached the
     end of the ptys.  */
  int failed_count = 0;

#ifdef PTY_ITERATION
  PTY_ITERATION
#else
  for (c = FIRST_PTY_LETTER; c <= 'z'; c++)
    for (i = 0; i < 16; i++)
#endif
      {
#ifdef PTY_NAME_SPRINTF
	PTY_NAME_SPRINTF
#else
	sprintf (pty_name, "/dev/pty%c%x", c, i);
#endif /* no PTY_NAME_SPRINTF */

#ifdef PTY_OPEN
	PTY_OPEN;
#else /* no PTY_OPEN */
#ifdef IRIS
	/* Unusual IRIS code */
 	*ptyv = open ("/dev/ptc", O_RDWR | O_NDELAY, 0);
 	if (fd < 0)
 	  return -1;
	if (fstat (fd, &stb) < 0)
	  return -1;
#else /* not IRIS */
	if (stat (pty_name, &stb) < 0)
	  {
	    failed_count++;
	    if (failed_count >= 3)
	      return -1;
	  }
	else
	  failed_count = 0;
#ifdef O_NONBLOCK
	fd = open (pty_name, O_RDWR | O_NONBLOCK, 0);
#else
	fd = open (pty_name, O_RDWR | O_NDELAY, 0);
#endif
#endif /* not IRIS */
#endif /* no PTY_OPEN */

	if (fd >= 0)
	  {
	    /* check to make certain that both sides are available
	       this avoids a nasty yet stupid bug in rlogins */
#ifdef PTY_TTY_NAME_SPRINTF
	    PTY_TTY_NAME_SPRINTF
#else
            sprintf (pty_name, "/dev/tty%c%x", c, i);
#endif /* no PTY_TTY_NAME_SPRINTF */
#ifndef UNIPLUS
	    if (access (pty_name, 6) != 0)
	      {
		close (fd);
#if !defined(IRIS) && !defined(__sgi)
		continue;
#else
		return -1;
#endif /* IRIS */
	      }
#endif /* not UNIPLUS */
	    setup_pty (fd);
	    return fd;
	  }
      }
  return -1;
}
#endif /* HAVE_PTYS */

static int
create_bidirectional_pipe (int *inchannel, int *outchannel,
			   volatile int *forkin, volatile int *forkout)
{
  int sv[2];

#ifdef SKTPAIR
  if (socketpair (AF_UNIX, SOCK_STREAM, 0, sv) < 0)
    return -1;
  *outchannel = *inchannel = sv[0];
  *forkout = *forkin = sv[1];
#else /* not SKTPAIR */
#ifdef WINDOWSNT
  pipe_with_inherited_out (sv);
  *inchannel = sv[0];
  *forkout = sv[1];

  pipe_with_inherited_in (sv);
  *forkin = sv[0];
  *outchannel = sv[1];
#else /* not WINDOWSNT */
  int temp;
  temp = pipe (sv);
  if (temp < 0) return -1;
  *inchannel = sv[0];
  *forkout = sv[1];
  temp = pipe (sv);
  if (temp < 0) return -1;
  *outchannel = sv[1];
  *forkin = sv[0];
#endif /* not WINDOWSNT */
#endif /* not SKTPAIR */
  return 0;
}

#ifndef VMS /* VMS version of this function is in vmsproc.c.  */

static Bufbyte
get_eof_char (struct Lisp_Process *p)
{
  /* Figure out the eof character for the outfd of the given process.
   * The following code is similar to that in process_send_signal, and 
   * should probably be merged with that code somehow. */

  CONST Bufbyte ctrl_d = (Bufbyte) '\004';
  
  if (!isatty (p->outfd))
    return ctrl_d;
#ifdef HAVE_TERMIOS
  {
    struct termios t;
    tcgetattr (p->outfd, &t);
#if 0
    /* What is the following line designed to do??? -mrb */
    if (strlen ((CONST char *) t.c_cc) < (unsigned int) (VEOF + 1))
      return ctrl_d;
    else
      return (Bufbyte) t.c_cc[VEOF];
#endif
    return t.c_cc[VEOF] == CDISABLE ? ctrl_d : (Bufbyte) t.c_cc[VEOF];
  }
#else /* ! HAVE_TERMIOS */
  /* On Berkeley descendants, the following IOCTL's retrieve the
    current control characters.  */
#if defined (TIOCGETC)
  {
    struct tchars c;
    ioctl (p->outfd, TIOCGETC, &c);
    return (Bufbyte) c.t_eofc;
  }
#else /* ! defined (TIOCGLTC) && defined (TIOCGETC) */
  /* On SYSV descendants, the TCGETA ioctl retrieves the current control
     characters.  */
#ifdef TCGETA
  {
    struct termio t;
    ioctl (p->outfd, TCGETA, &t);
    if (strlen ((CONST char *) t.c_cc) < (unsigned int) (VINTR + 1))
      return ctrl_d;
    else
      return (Bufbyte) t.c_cc[VINTR];
  }
#else /* ! defined (TCGETA) */
  /* Rather than complain, we'll just guess ^D, which is what 
   * earlier emacsen always used. */
  return ctrl_d;
#endif /* ! defined (TCGETA) */
#endif /* ! defined (TIOCGETC) */
#endif /* ! defined (HAVE_TERMIOS) */
}

static int
get_pty_max_bytes (struct Lisp_Process *p)
{
  int pty_max_bytes;

#if defined (HAVE_FPATHCONF) && defined (_PC_MAX_CANON)
  pty_max_bytes = fpathconf (p->outfd, _PC_MAX_CANON);
  if (pty_max_bytes < 0)
    pty_max_bytes = 250;
#else
  pty_max_bytes = 250;
#endif
  /* Deduct one, to leave space for the eof.  */
  pty_max_bytes--;

  return pty_max_bytes;
}

static void
init_process_fds (struct Lisp_Process *p, int in, int out)
{
  p->infd = in;
  p->outfd = out;
  p->instream = make_filedesc_input_stream (in, 0, -1, 0);
  p->outstream = make_filedesc_output_stream (out, 0, -1,
					      LSTR_BLOCKED_OK
					      | (p->pty_flag ?
						 LSTR_PTY_FLUSHING : 0));
  p->filedesc_stream = p->outstream;
  if (p->pty_flag)
    {
      Bufbyte eof_char = get_eof_char (p);
      int pty_max_bytes = get_pty_max_bytes (p);
      filedesc_stream_set_pty_flushing (XLSTREAM (p->outstream),
					pty_max_bytes, eof_char);
    }
}

static void
create_process (Lisp_Object process, 
                char **new_argv, CONST char *current_dir)
{
  /* This function rewritten by wing@666.com. */

  int pid, inchannel, outchannel;
  /* Use volatile to protect variables from being clobbered by longjmp.  */
  volatile int forkin, forkout;
  volatile int pty_flag = 0;
  char **env;
  struct Lisp_Process *p = XPROCESS (process);

  env = environ;

  inchannel = outchannel = forkin = forkout = -1;

#ifdef HAVE_PTYS
  if (!NILP (Vprocess_connection_type))
    {
      /* find a new pty, open the master side, return the opened
	 file handle, and store the name of the corresponding slave
	 side in global variable pty_name. */
      outchannel = inchannel = allocate_pty ();
    }

  if (inchannel >= 0)
    {
      /* You're "supposed" to now open the slave in the child.
	 On some systems, we can open it here; this allows for
	 better error checking. */
#ifndef USG
      /* On USG systems it does not work to open the pty's tty here
	       and then close and reopen it in the child.  */
#ifdef O_NOCTTY
      /* Don't let this terminal become our controlling terminal
	 (in case we don't have one).  */
      forkout = forkin = open (pty_name, O_RDWR | O_NOCTTY, 0);
#else
      forkout = forkin = open (pty_name, O_RDWR, 0);
#endif
      if (forkin < 0)
	goto io_failure;
#endif /* not USG */
      p->pty_flag = pty_flag = 1;
    }
  else
#endif /* HAVE_PTYS */
    if (create_bidirectional_pipe (&inchannel, &outchannel,
				   &forkin, &forkout) < 0)
      goto io_failure;

#if 0
  /* Replaced by close_process_descs */
  set_exclusive_use (inchannel);
  set_exclusive_use (outchannel);
#endif

  set_descriptor_non_blocking (inchannel);

  /* Record this as an active process, with its channels.
     As a result, child_setup will close Emacs's side of the pipes.  */
  descriptor_to_process[inchannel] = process;
  init_process_fds (p, inchannel, outchannel);
  /* Record the tty descriptor used in the subprocess.  */
  p->subtty = forkin;
  p->status_symbol = Qrun;
  p->exit_code = 0;

  {
    /* child_setup must clobber environ on systems with true vfork.
       Protect it from permanent change.  */
    char **save_environ = environ;

#ifdef EMACS_BTL
    /* when performance monitoring is on, turn it off before the vfork(),
       as the child has no handler for the signal -- when back in the
       parent process, turn it back on if it was really on when you "turned
       it off" */
    int logging_on = cadillac_stop_logging ();	/* #### rename me */
#endif

#ifndef WINDOWSNT
    pid = vfork ();
    if (pid == 0)
#endif /* not WINDOWSNT */
      {
	/**** Now we're in the child process ****/
	int xforkin = forkin;
	int xforkout = forkout;

	if (!pty_flag)
	  EMACS_SEPARATE_PROCESS_GROUP ();
#ifdef HAVE_PTYS
	else
	  {
	    /* Disconnect the current controlling terminal, pursuant to
	       making the pty be the controlling terminal of the process.
	       Also put us in our own process group. */

	    disconnect_controlling_terminal ();

	    /* Open the pty connection and make the pty's terminal
	       our controlling terminal.

	       On systems with TIOCSCTTY, we just use it to set
	       the controlling terminal.  On other systems, the
	       first TTY we open becomes the controlling terminal.
	       So, we end up with four possibilities:

	       (1) on USG and TIOCSCTTY systems, we open the pty
	           and use TIOCSCTTY.
	       (2) on other USG systems, we just open the pty.
	       (3) on non-USG systems with TIOCSCTTY, we
	           just use TIOCSCTTY. (On non-USG systems, we
		   already opened the pty in the parent process.)
	       (4) on non-USG systems without TIOCSCTTY, we
	           close the pty and reopen it.

	       This would be cleaner if we didn't open the pty
	       in the parent process, but doing it that way
	       makes it possible to trap error conditions.
	       It's harder to convey an error from the child
	       process, and I don't feel like messing with
	       this now. */

	    /* There was some weirdo, probably wrong,
	       conditionalization on RTU and UNIPLUS here.
	       I deleted it.  So sue me. */

	    /* SunOS has TIOCSCTTY but the close/open method
	       also works. */

#  if defined (USG) || !defined (TIOCSCTTY)
	    /* Now close the pty (if we had it open) and reopen it.
	       This makes the pty the controlling terminal of the
	       subprocess.  */
	    /* I wonder if close (open (pty_name, ...)) would work?  */
	    if (xforkin >= 0)
	      close (xforkin);
	    xforkout = xforkin = open (pty_name, O_RDWR, 0);
	    if (xforkin < 0)
	      {
		write (1, "Couldn't open the pty terminal ", 31);
		write (1, pty_name, strlen (pty_name));
		write (1, "\n", 1);
		_exit (1);
	      }
#  endif /* USG or not TIOCSCTTY */

	    /* Miscellaneous setup required for some systems.
               Must be done before using tc* functions on xforkin.
               This guarantees that isatty(xforkin) is true. */
            
# ifdef SETUP_SLAVE_PTY
	    SETUP_SLAVE_PTY;
# endif /* SETUP_SLAVE_PTY */
            
#  ifdef TIOCSCTTY
	    /* We ignore the return value
	       because faith@cs.unc.edu says that is necessary on Linux.  */
            assert (isatty (xforkin));
	    ioctl (xforkin, TIOCSCTTY, 0);
#  endif /* TIOCSCTTY */

	    /* Change the line discipline. */

# if defined (HAVE_TERMIOS) && defined (LDISC1)
	    {
	      struct termios t;
              assert (isatty (xforkin));
	      tcgetattr (xforkin, &t);
	      t.c_lflag = LDISC1;
	      if (tcsetattr (xforkin, TCSANOW, &t) < 0)
		perror ("create_process/tcsetattr LDISC1 failed\n");
	    }
# elif defined (NTTYDISC) && defined (TIOCSETD)
	    {
	      /* Use new line discipline.  TIOCSETD is accepted and
                 ignored on Sys5.4 systems with ttcompat. */
	      int ldisc = NTTYDISC;
              assert (isatty (xforkin));
	      ioctl (xforkin, TIOCSETD, &ldisc);
	    }
# endif /* TIOCSETD & NTTYDISC */

	    /* Make our process group be the foreground group
	       of our new controlling terminal. */

	    {
	      int piddly = EMACS_GET_PROCESS_GROUP ();
	      EMACS_SET_TTY_PROCESS_GROUP (xforkin, &piddly);
	    }

# ifdef AIX
	    /* On AIX, we've disabled SIGHUP above once we start a
	       child on a pty.  Now reenable it in the child, so it
	       will die when we want it to.  */
	    signal (SIGHUP, SIG_DFL);
# endif /* AIX */
	  }
#endif /* HAVE_PTYS */

	signal (SIGINT, SIG_DFL);
	signal (SIGQUIT, SIG_DFL);

#ifndef MSDOS
	if (pty_flag)
	  {
	    /* Set up the terminal characteristics of the pty. */
	    child_setup_tty (xforkout);
	  }

#ifdef WINDOWSNT
	pid = child_setup (xforkin, xforkout, xforkout,
			   new_argv, current_dir);
#else  /* not WINDOWSNT */	
	child_setup (xforkin, xforkout, xforkout, new_argv, current_dir);
#endif /* not WINDOWSNT */
#endif /* not MSDOS */
      }
#ifdef EMACS_BTL
    else if (logging_on)
      cadillac_start_logging ();	/* #### rename me */
#endif

    environ = save_environ;
  }

  if (pid < 0)
    {
      close_descriptor_pair (forkin, forkout);
      report_file_error ("Doing vfork", Qnil);
    }

  p->pid = make_int (pid);
  /* p->subtty = -1; */

#ifdef WINDOWSNT
  register_child (pid, inchannel);
#endif /* WINDOWSNT */

  /* If the subfork execv fails, and it exits,
     this close hangs.  I don't know why.
     So have an interrupt jar it loose.  */
  if (forkin >= 0)
    close_safely (forkin);
  if (forkin != forkout && forkout >= 0)
    close (forkout);

#ifdef HAVE_PTYS
  if (pty_flag)
    XPROCESS (process)->tty_name = build_string (pty_name);
  else
#endif
    XPROCESS (process)->tty_name = Qnil;

  /* Notice that SIGCHLD was not blocked. (This is not possible on
     some systems.) No biggie if SIGCHLD occurs right around the
     time that this call happens, because SIGCHLD() does not actually
     deselect the process (that doesn't occur until the next time
     we're waiting for an event, when status_notify() is called). */
  event_stream_select_process (XPROCESS (process));

  return;

io_failure:
  {
    int temp = errno;
    close_descriptor_pair (forkin, forkout);
    close_descriptor_pair (inchannel, outchannel);
    errno = temp;
    report_file_error ("Opening pty or pipe", Qnil);
  }
}
#endif /* not VMS */

/* This function is the unwind_protect form for Fstart_process_internal.  If
   PROC doesn't have its pid set, then we know someone has signalled
   an error and the process wasn't started successfully, so we should
   remove it from the process list.  */
static void remove_process (Lisp_Object proc);
static Lisp_Object
start_process_unwind (Lisp_Object proc)
{
  /* Was PROC started successfully?  */
  if (EQ (XPROCESS (proc)->pid, Qnil))
    remove_process (proc);
  return Qnil;
}

DEFUN ("start-process-internal", Fstart_process_internal, 3, MANY, 0, /*
Start a program in a subprocess.  Return the process object for it.
Args are NAME BUFFER PROGRAM &rest PROGRAM-ARGS
NAME is name for process.  It is modified if necessary to make it unique.
BUFFER is the buffer or (buffer-name) to associate with the process.
 Process output goes at end of that buffer, unless you specify
 an output stream or filter function to handle the output.
 BUFFER may be also nil, meaning that this process is not associated
 with any buffer
Third arg is program file name.  It is searched for as in the shell.
Remaining arguments are strings to give program as arguments.
INCODE and OUTCODE specify the coding-system objects used in input/output
 from/to the process.
*/
       (int nargs, Lisp_Object *args))
{
  /* !!#### This function has not been Mule-ized */
  /* This function can GC */
  Lisp_Object buffer, name, program, proc, current_dir;
  Lisp_Object tem;
  int speccount = specpdl_depth ();
#ifdef VMS
  char *new_argv;
  int len;
#else
  char **new_argv;
#endif
  int i;

  buffer = args[1];
  if (!NILP (buffer))
    buffer = Fget_buffer_create (buffer);

  CHECK_STRING (args[0]);    /* name */
  CHECK_STRING (args[2]);    /* program */

  /* Make sure that the child will be able to chdir to the current
     buffer's current directory, or its unhandled equivalent.  We
     can't just have the child check for an error when it does the
     chdir, since it's in a vfork.

     We have to GCPRO around this because Fexpand_file_name and
     Funhandled_file_name_directory might call a file name handling
     function.  The argument list is protected by the caller, so all
     we really have to worry about is buffer.  */
  {
    struct gcpro gcpro1, gcpro2; /* Caller gc-protects args[] */

    current_dir = current_buffer->directory;

    GCPRO2 (buffer, current_dir);

    current_dir = 
      expand_and_dir_to_file (Funhandled_file_name_directory (current_dir),
			      Qnil);
#if 0	/* This loser breaks ange-ftp */
    if (NILP (Ffile_accessible_directory_p (current_dir)))
      report_file_error ("Setting current directory",
			 list1 (current_buffer->directory));
#endif /* 0 */

    UNGCPRO;
  }

  name = args[0];
  program = args[2];

#ifdef VMS
  /* Make a one member argv with all args concatenated
     together separated by a blank.  */
  len = XSTRING_LENGTH (program) + 2;
  for (i = 3; i < nargs; i++)
    {
      tem = args[i];
      CHECK_STRING (tem);
      len += XSTRING_LENGTH (tem) + 1;	/* count the blank */
    }
  new_argv = (char *) alloca (len);
  strcpy (new_argv, XSTRING_DATA (program));
  for (i = 3; i < nargs; i++)
    {
      tem = args[i];
      CHECK_STRING (tem);
      strcat (new_argv, " ");
      strcat (new_argv, XSTRING_DATA (tem));
    }
  /* Need to add code here to check for program existence on VMS */

#else /* not VMS */
  new_argv = (char **)
    alloca ((nargs - 1) * sizeof (char *));

  new_argv[0] = (char *) XSTRING_DATA (program);

  /* If program file name is not absolute, search our path for it */
  if (!IS_DIRECTORY_SEP (XSTRING_BYTE (program, 0))
      && !(XSTRING_LENGTH (program) > 1
	  && IS_DEVICE_SEP (XSTRING_BYTE (program, 1))))
    {
      struct gcpro gcpro1, gcpro2, gcpro3, gcpro4; /* Caller protects args[] */
      GCPRO4 (buffer, current_dir, name, program);

      tem = Qnil;
      locate_file (Vexec_path, program, EXEC_SUFFIXES, &tem,
		   X_OK);
      UNGCPRO;
      if (NILP (tem))
	report_file_error ("Searching for program", list1 (program));
      tem = Fexpand_file_name (tem, Qnil);
      new_argv[0] = (char *) XSTRING_DATA (tem);
    }
  else
    {
      if (!NILP (Ffile_directory_p (program)))
	error ("Specified program for new process is a directory");
    }

  for (i = 3; i < nargs; i++)
    {
      tem = args[i];
      CHECK_STRING (tem);
      new_argv[i - 2] = (char *) XSTRING_DATA (tem);
    }
  new_argv[i - 2] = 0;

#endif /* not VMS */

  proc = make_process_internal (name);

  XPROCESS (proc)->buffer = buffer;
  XPROCESS (proc)->command = Flist (nargs - 2,
				    args + 2);

  /* Make the process marker point into the process buffer (if any).  */
  if (!NILP (buffer))
    Fset_marker (XPROCESS (proc)->mark,
		 make_int (BUF_ZV (XBUFFER (buffer))), buffer);

  /* If an error occurs and we can't start the process, we want to
     remove it from the process list.  This means that each error
     check in create_process doesn't need to call remove_process
     itself; it's all taken care of here.  */
  record_unwind_protect (start_process_unwind, proc);

  create_process (proc, new_argv, (char *) XSTRING_DATA (current_dir));

  return unbind_to (speccount, proc);
}


/* connect to an existing file descriptor.  This is very similar to
   open-network-stream except that it assumes that the connection has
   already been initialized.  It is currently used for ToolTalk
   communication. */

/* This function used to be visible on the Lisp level, but there is no
   real point in doing that.  Here is the doc string:

  "Connect to an existing file descriptor.\n\
Returns a subprocess-object to represent the connection.\n\
Input and output work as for subprocesses; `delete-process' closes it.\n\
Args are NAME BUFFER INFD OUTFD.\n\
NAME is name for process.  It is modified if necessary to make it unique.\n\
BUFFER is the buffer (or buffer-name) to associate with the process.\n\
 Process output goes at end of that buffer, unless you specify\n\
 an output stream or filter function to handle the output.\n\
 BUFFER may be also nil, meaning that this process is not associated\n\
 with any buffer\n\
INFD and OUTFD specify the file descriptors to use for input and\n\
 output, respectively."
*/

Lisp_Object
connect_to_file_descriptor (Lisp_Object name, Lisp_Object buffer,
			    Lisp_Object infd, Lisp_Object outfd)
{
  /* This function can GC */
  Lisp_Object proc;
  int inch;

  CHECK_STRING (name);
  CHECK_INT (infd);
  CHECK_INT (outfd);

  inch = XINT (infd);
  if (!NILP (descriptor_to_process[inch]))
    error ("There is already a process connected to fd %d", inch);
  if (!NILP (buffer))
    buffer = Fget_buffer_create (buffer);
  proc = make_process_internal (name);

  descriptor_to_process[inch] = proc;

  XPROCESS (proc)->pid = Fcons (infd, name);
  XPROCESS (proc)->buffer = buffer;
  init_process_fds (XPROCESS (proc), inch, XINT (outfd));
  XPROCESS (proc)->connected_via_filedesc_p = 1;

  event_stream_select_process (XPROCESS (proc));

  return proc;
}


#ifdef HAVE_SOCKETS

static int
get_internet_address (Lisp_Object host, struct sockaddr_in *address,
		      Error_behavior errb)
{
  struct hostent *host_info_ptr;
#ifdef TRY_AGAIN
  int count = 0;
#endif

#ifndef HAVE_TERM
  memset (address, 0, sizeof (*address));

  while (1)
    {
#ifdef TRY_AGAIN
      if (count++ > 10) break;
      h_errno = 0;
#endif
      /* Some systems can't handle SIGIO/SIGALARM in gethostbyname. */
      slow_down_interrupts ();
      host_info_ptr = gethostbyname ((char *) XSTRING_DATA (host));
      speed_up_interrupts ();
#ifdef TRY_AGAIN
      if (! (host_info_ptr == 0 && h_errno == TRY_AGAIN))
#endif
	break;
      Fsleep_for (make_int (1));
    }
  if (host_info_ptr)
    {
      address->sin_family = host_info_ptr->h_addrtype;
      memcpy (&address->sin_addr, host_info_ptr->h_addr, host_info_ptr->h_length);
    }
  else
    {
      IN_ADDR numeric_addr;
      /* Attempt to interpret host as numeric inet address */
      numeric_addr = inet_addr ((char *) XSTRING_DATA (host));
      if (NUMERIC_ADDR_ERROR)
	{
	  maybe_error (Qprocess, errb,
		       "Unknown host \"%s\"", XSTRING_DATA (host));
	  return 0;
	}

      /* There was some broken code here that called strlen() here
	 on (char *) &numeric_addr and even sometimes accessed
	 uninitialized data. */
      address->sin_family = AF_INET;
      * (IN_ADDR *) &address->sin_addr = numeric_addr;
    }

  return 1;
}

/* open a TCP network connection to a given HOST/SERVICE.  Treated
   exactly like a normal process when reading and writing.  Only
   differences are in status display and process deletion.  A network
   connection has no PID; you cannot signal it.  All you can do is
   deactivate and close it via delete-process */

DEFUN ("open-network-stream-internal", Fopen_network_stream_internal, 4, 4, 0, /*
Open a TCP connection for a service to a host.
Returns a subprocess-object to represent the connection.
Input and output work as for subprocesses; `delete-process' closes it.
Args are NAME BUFFER HOST SERVICE.
NAME is name for process.  It is modified if necessary to make it unique.
BUFFER is the buffer (or buffer-name) to associate with the process.
 Process output goes at end of that buffer, unless you specify
 an output stream or filter function to handle the output.
 BUFFER may be also nil, meaning that this process is not associated
 with any buffer
Third arg is name of the host to connect to, or its IP address.
Fourth arg SERVICE is name of the service desired, or an integer
 specifying a port number to connect to.
*/
       (name, buffer, host, service))
{
  /* !!#### This function has not been Mule-ized */
  /* This function can GC */
  Lisp_Object proc;
  struct sockaddr_in address;
  int s, outch, inch;
  int port;
  struct gcpro gcpro1, gcpro2, gcpro3, gcpro4;
  volatile int retry = 0;
  int retval;

  GCPRO4 (name, buffer, host, service);
  CHECK_STRING (name);
  CHECK_STRING (host);
  if (INTP (service))
    port = htons ((unsigned short) XINT (service));
  else
    {
      struct servent *svc_info;
      CHECK_STRING (service);
      svc_info = getservbyname ((char *) XSTRING_DATA (service), "tcp");
      if (svc_info == 0)
#ifdef WIN32
	error ("Unknown service \"%s\" (%d)",
	       XSTRING_DATA (service), WSAGetLastError ());
#else
	error ("Unknown service \"%s\"", XSTRING_DATA (service));
#endif
      port = svc_info->s_port;
    }

  get_internet_address (host, &address, ERROR_ME);
  address.sin_port = port;

  s = socket (address.sin_family, SOCK_STREAM, 0);
  if (s < 0) 
    report_file_error ("error creating socket", list1 (name));

  /* Turn off interrupts here -- see comments below.  There used to
     be code which called bind_polling_period() to slow the polling
     period down rather than turn it off, but that seems rather
     bogus to me.  Best thing here is to use a non-blocking connect
     or something, to check for QUIT. */

  /* Comments that are not quite valid: */

  /* Kernel bugs (on Ultrix at least) cause lossage (not just EINTR)
     when connect is interrupted.  So let's not let it get interrupted.
     Note we do not turn off polling, because polling is only used
     when not interrupt_input, and thus not normally used on the systems
     which have this bug.  On systems which use polling, there's no way
     to quit if polling is turned off.  */

  /* Slow down polling.  Some kernels have a bug which causes retrying
     connect to fail after a connect.  */

  slow_down_interrupts ();

 loop:

  /* A system call interrupted with a SIGALRM or SIGIO comes back
     here, with can_break_system_calls reset to 0. */
  SETJMP (break_system_call_jump);
  if (QUITP)
    {
      speed_up_interrupts ();
      REALLY_QUIT;
      /* In case something really weird happens ... */
      slow_down_interrupts ();
    }

  /* Break out of connect with a signal (it isn't otherwise possible).
     Thus you don't get screwed with a hung network. */
  can_break_system_calls = 1;
  retval = connect (s, (struct sockaddr *) &address, sizeof (address));
  can_break_system_calls = 0;
  if (retval == -1 && errno != EISCONN)
    {
      int xerrno = errno;
      if (errno == EINTR)
	goto loop;
      if (errno == EADDRINUSE && retry < 20)
	{
	  /* A delay here is needed on some FreeBSD systems,
	     and it is harmless, since this retrying takes time anyway
	     and should be infrequent.
             `sleep-for' allowed for quitting this loop with interrupts
             slowed down so it can't be used here.  Async timers should
             already be disabled at this point so we can use `sleep'. */
          sleep (1);
	  retry++;
	  goto loop;
	}

      close (s);

      speed_up_interrupts ();

      errno = xerrno;
      report_file_error ("connection failed", list2 (host, name));
    }

  speed_up_interrupts ();

#else /* HAVE_TERM */
  s = connect_server (0);
  if (s < 0)
    report_file_error ("error creating socket", Fcons (name, Qnil));
  send_command (s, C_PORT, 0, "%s:%d", XSTRING_DATA (host), ntohs (port));
  send_command (s, C_DUMB, 1, 0);
#endif /* HAVE_TERM */

  inch = s;
  outch = dup (s);
  if (outch < 0)
    {
      close (s); /* this used to be leaked; from Kyle Jones */
      report_file_error ("error duplicating socket", list1 (name));
    }

  if (!NILP (buffer))
    buffer = Fget_buffer_create (buffer);
  proc = make_process_internal (name);

  descriptor_to_process[inch] = proc;

  set_descriptor_non_blocking (inch);

  XPROCESS (proc)->pid = Fcons (service, host);
  XPROCESS (proc)->buffer = buffer;
  init_process_fds (XPROCESS (proc), inch, outch);
  XPROCESS (proc)->connected_via_filedesc_p = 0;

  event_stream_select_process (XPROCESS (proc));

  UNGCPRO;
  return proc;
}

#endif	/* HAVE_SOCKETS */

Lisp_Object
canonicalize_host_name (Lisp_Object host)
{
#ifdef HAVE_SOCKETS
  /* #### for HAVE_TERM, you probably have to do something else. */
  struct sockaddr_in address;

  if (!get_internet_address (host, &address, ERROR_ME_NOT))
    return host;

  if (address.sin_family == AF_INET)
    return build_string (inet_ntoa (address.sin_addr));
  else
    /* #### any clue what to do here? */
    return host;
#else
  return host;
#endif
}


DEFUN ("set-process-window-size", Fset_process_window_size, 3, 3, 0, /*
Tell PROCESS that it has logical window size HEIGHT and WIDTH.
*/
       (proc, height, width))
{
  CHECK_PROCESS (proc);
  CHECK_NATNUM (height);
  CHECK_NATNUM (width);
  if (set_window_size (XPROCESS (proc)->infd, XINT (height), XINT (width))
      <= 0)
    return Qnil;
  else
    return Qt;
}


/************************************************************************/
/*                              Process I/O                             */
/************************************************************************/

/*  (Faccept_process_output is now in event-stream.c) */

/* Some FSFmacs error handlers here.  We handle this
   in call2_trapping_errors(). */

/* Read pending output from the process channel,
   starting with our buffered-ahead character if we have one.
   Yield number of characters read.

   This function reads at most 1024 bytes.
   If you want to read all available subprocess output,
   you must call it repeatedly until it returns zero.  */

Charcount
read_process_output (Lisp_Object proc)
{
  /* This function can GC */
  Bytecount nbytes, nchars;
#ifdef VMS
  char *chars;
#else
  Bufbyte chars[1024];
#endif
  Lisp_Object outstream;
  struct Lisp_Process *p = XPROCESS (proc);

  /* If there is a lot of output from the subprocess, the loop in
     execute_internal_event() might call read_process_output() more
     than once.  If the filter that was executed from one of these
     calls set the filter to t, we have to stop now.  Return -1 rather
     than 0 so execute_internal_event() doesn't close the process.
     Really, the loop in execute_internal_event() should check itself
     for a process-filter change, like in status_notify(); but the
     struct Lisp_Process is not exported outside of this file. */
  if (p->infd < 0)
    return -1; /* already closed */

  if (!NILP (p->filter) && (p->filter_does_read))
    {
      Lisp_Object filter_result;

      /* Some weird FSFmacs crap here with
	 Vdeactivate_mark and current_buffer->keymap */
      running_asynch_code = 1;
      filter_result = call2_trapping_errors ("Error in process filter",
					     p->filter, proc, Qnil);
      running_asynch_code = 0;
      restore_match_data ();
      CHECK_INT (filter_result);
      return XINT (filter_result);
    }

#ifdef VMS
  VMS_PROC_STUFF *vs, *get_vms_process_pointer();

  vs = get_vms_process_pointer (XINT (p->pid));
  if (vs)
    {
      if (!vs->iosb[0])
	return(0);		/* Really weird if it does this */
      if (!(vs->iosb[0] & 1))
	return -1;		/* I/O error */
    }
  else
    error ("Could not get VMS process pointer");
  chars = vs->inputBuffer;
  nbytes = clean_vms_buffer (chars, vs->iosb[1]);
  if (nbytes <= 0)
    {
      start_vms_process_read (vs); /* Crank up the next read on the process */
      return 1;			/* Nothing worth printing, say we got 1 */
    }
#else /* not VMS */

#if 0 /* FSFmacs */
  /* #### equivalent code from FSFmacs.  Would need some porting
     for Windows NT. */
  if (proc_buffered_char[channel] < 0)
#ifdef WINDOWSNT
    nchars = read_child_output (channel, chars, sizeof (chars));
#else
    nchars = read (channel, chars, sizeof chars);
#endif
  else
    {
      chars[0] = proc_buffered_char[channel];
      proc_buffered_char[channel] = -1;
#ifdef WINDOWSNT
      nchars = read_child_output (channel, chars + 1, sizeof (chars) - 1);
#else
      nchars = read (channel, chars + 1, sizeof chars - 1);
#endif
      if (nchars < 0)
	nchars = 1;
      else
	nchars = nchars + 1;
    }
#endif /* FSFmacs */

  nbytes = Lstream_read (XLSTREAM (p->instream), chars, sizeof (chars));
  if (nbytes <= 0) return nbytes;
#endif /* not VMS */

  nchars = bytecount_to_charcount (chars, nbytes);
  outstream = p->filter;
  if (!NILP (outstream))
    {
      /* We used to bind inhibit-quit to t here, but
	 call2_trapping_errors() does that for us. */
      running_asynch_code = 1;
      call2_trapping_errors ("Error in process filter",
			     outstream, proc, make_string (chars, nbytes));
      running_asynch_code = 0;
      restore_match_data ();
#ifdef VMS
      start_vms_process_read (vs);
#endif
      return (nchars);
    }

  /* If no filter, write into buffer if it isn't dead.  */
  if (!NILP (p->buffer) && BUFFER_LIVE_P (XBUFFER (p->buffer)))
    {
      Lisp_Object old_read_only = Qnil;
      Bufpos old_point;
      Bufpos old_begv;
      Bufpos old_zv;
      int old_zmacs_region_stays = zmacs_region_stays;
      struct gcpro gcpro1, gcpro2;
      struct buffer *buf = XBUFFER (p->buffer);

      GCPRO2 (proc, old_read_only);

      old_point = BUF_PT (buf);
      old_begv = BUF_BEGV (buf);
      old_zv = BUF_ZV (buf);
      old_read_only = buf->read_only;
      buf->read_only = Qnil;

      /* Insert new output into buffer
	 at the current end-of-output marker,
	 thus preserving logical ordering of input and output.  */
      if (XMARKER (p->mark)->buffer)
	BUF_SET_PT (buf,
		    bufpos_clip_to_bounds (old_begv, marker_position (p->mark),
					   old_zv));
      else
	BUF_SET_PT (buf, old_zv);

      /* If the output marker is outside of the visible region, save
	 the restriction and widen.  */
      if (! (BUF_BEGV (buf) <= BUF_PT (buf) &&
	     BUF_PT (buf) <= BUF_ZV (buf)))
	Fwiden (p->buffer);

      /* Make sure opoint floats ahead of any new text, just as point
	 would.  */
      if (BUF_PT (buf) <= old_point)
	old_point += nchars;

      /* Insert after old_begv, but before old_zv.  */
      if (BUF_PT (buf) < old_begv)
	old_begv += nchars;
      if (BUF_PT (buf) <= old_zv)
	old_zv += nchars;

#if 0
      /* This screws up intial display of the window.  jla */

      /* Insert before markers in case we are inserting where
	 the buffer's mark is, and the user's next command is Meta-y.  */
      buffer_insert_raw_string_1 (buf, -1, chars,
				  nbytes, INSDEL_BEFORE_MARKERS);
#else
      buffer_insert_raw_string (buf, chars, nbytes);
#endif

      Fset_marker (p->mark, make_int (BUF_PT (buf)), p->buffer);

      MARK_MODELINE_CHANGED;

      /* If the restriction isn't what it should be, set it.  */
      if (old_begv != BUF_BEGV (buf) || old_zv != BUF_ZV (buf))
	Fnarrow_to_region (make_int (old_begv), make_int (old_zv),
			   p->buffer);

      /* Handling the process output should not deactivate the mark.  */
      zmacs_region_stays = old_zmacs_region_stays;
      buf->read_only = old_read_only;
      BUF_SET_PT (buf, old_point);

      UNGCPRO;
    }
#ifdef VMS
  start_vms_process_read (vs);
#endif
  return (nchars);
}

/* Sending data to subprocess */

static JMP_BUF send_process_frame;

static SIGTYPE
send_process_trap (int signum)
{
  EMACS_REESTABLISH_SIGNAL (signum, send_process_trap);
  EMACS_UNBLOCK_SIGNAL (signum);
  LONGJMP (send_process_frame, 1);
}

/* send some data to process PROC.  If NONRELOCATABLE is non-NULL, it
   specifies the address of the data.  Otherwise, the data comes from the
   object RELOCATABLE (either a string or a buffer).  START and LEN
   specify the offset and length of the data to send.

   Note that START and LEN are in Bufpos's if RELOCATABLE is a buffer,
   and in Bytecounts otherwise. */

static void
send_process (volatile Lisp_Object proc,
              Lisp_Object relocatable, CONST Bufbyte *nonrelocatable,
              int start, int len)
{
  /* This function can GC */
  /* Use volatile to protect variables from being clobbered by longjmp.  */
  struct gcpro gcpro1, gcpro2;
  SIGTYPE (*volatile old_sigpipe) (int) = 0;
  Lisp_Object lstream = Qnil;
  volatile struct Lisp_Process *p = XPROCESS (proc);
#if defined (NO_UNION_TYPE) /* || !defined (__GNUC__) GCC bug only??? */
  /* #### ugh! There must be a better solution. */
  Lisp_Object defeat_volatile_kludge = (Lisp_Object) proc;
#else
  Lisp_Object defeat_volatile_kludge = proc;
#endif

#ifdef VMS
  VMS_PROC_STUFF *vs, *get_vms_process_pointer (int);
#endif /* VMS */

  GCPRO2 (defeat_volatile_kludge, lstream);

  if (p->outfd < 0)
    signal_simple_error ("Process not open for writing", proc);

#ifdef VMS
  vs = get_vms_process_pointer (XINT (p->pid));
  if (vs == 0)
    error ("Could not find this process: %x",
	   XINT (p->pid));
  else if (write_to_vms_process (vs, buf, len))
    ;
#else

  if (nonrelocatable)
    lstream =
      make_fixed_buffer_input_stream (nonrelocatable + start, len);
  else if (GC_BUFFERP (relocatable))
    lstream = make_lisp_buffer_input_stream (XBUFFER (relocatable),
					     start, start + len, 0);
  else
    lstream = make_lisp_string_input_stream (relocatable, start, len);

  if (!SETJMP (send_process_frame))
    {
      /* use a reasonable-sized buffer (somewhere around the size of the
	 stream buffer) so as to avoid inundating the stream with blocked
	 data. */
      Bufbyte chunkbuf[512];
      Bytecount chunklen;

      while (1)
	{
	  int writeret;

	  chunklen = Lstream_read (XLSTREAM (lstream), chunkbuf, 512);
	  if (chunklen <= 0)
	    break; /* perhaps should abort() if < 0?
		      This should never happen. */
	  old_sigpipe =
	    (SIGTYPE (*) (int)) signal (SIGPIPE, send_process_trap);
	  /* Lstream_write() will never successfully write less than
	     the amount sent in.  In the worst case, it just buffers
	     the unwritten data. */
	  writeret = Lstream_write (XLSTREAM (p->outstream), chunkbuf,
				    chunklen);
	  signal (SIGPIPE, old_sigpipe);
	  if (writeret < 0)
	    /* This is a real error.  Blocking errors are handled
	       specially inside of the filedesc stream. */
	    report_file_error ("writing to process",
			       list1 (proc));
	  while (filedesc_stream_was_blocked (XLSTREAM (p->filedesc_stream)))
	    {
	      /* Buffer is full.  Wait, accepting input; 
		 that may allow the program
		 to finish doing output and read more.  */
	      Faccept_process_output (Qnil, make_int (1), Qnil);
	      old_sigpipe =
		(SIGTYPE (*) (int)) signal (SIGPIPE, send_process_trap);
	      Lstream_flush (XLSTREAM (p->filedesc_stream));
	      signal (SIGPIPE, old_sigpipe);
	    }
	}
    }
#endif /* !VMS */
  else
    { /* We got here from a longjmp() from the SIGPIPE handler */
      signal (SIGPIPE, old_sigpipe);
      p->status_symbol = Qexit;
      p->exit_code = 256; /* #### SIGPIPE ??? */
      p->core_dumped = 0;
      p->tick++;
      process_tick++;
      deactivate_process (proc);
#ifdef VMS
      error ("Error writing to process %s; closed it",
	    XSTRING_DATA (p->name));
#else
      error ("SIGPIPE raised on process %s; closed it",
	    XSTRING_DATA (p->name));
#endif
    }
  Lstream_flush (XLSTREAM (p->outstream));
  UNGCPRO;
}

DEFUN ("process-tty-name", Fprocess_tty_name, 1, 1, 0, /*
Return the name of the terminal PROCESS uses, or nil if none.
This is the terminal that the process itself reads and writes on,
not the name of the pty that Emacs uses to talk with that terminal.
*/
       (proc))
{
  CHECK_PROCESS (proc);
  return XPROCESS (proc)->tty_name;
}

DEFUN ("set-process-buffer", Fset_process_buffer, 2, 2, 0, /*
Set buffer associated with PROCESS to BUFFER (a buffer, or nil).
*/
       (proc, buffer))
{
  CHECK_PROCESS (proc);
  if (!NILP (buffer))
    CHECK_BUFFER (buffer);
  XPROCESS (proc)->buffer = buffer;
  return buffer;
}

DEFUN ("process-buffer", Fprocess_buffer, 1, 1, 0, /*
Return the buffer PROCESS is associated with.
Output from PROCESS is inserted in this buffer
unless PROCESS has a filter.
*/
       (proc))
{
  CHECK_PROCESS (proc);
  return XPROCESS (proc)->buffer;
}

DEFUN ("process-mark", Fprocess_mark, 1, 1, 0, /*
Return the marker for the end of the last output from PROCESS.
*/
       (proc))
{
  CHECK_PROCESS (proc);
#ifdef ENERGIZE
  if (EQ (proc, Venergize_process)) /* per buffer rather than per process */
    return Fenergize_user_input_buffer_mark (Qnil); /* ## current_buffer ok? */
#endif
  return XPROCESS (proc)->mark;
}

void
set_process_filter (Lisp_Object proc, Lisp_Object filter, int filter_does_read)
{
  CHECK_PROCESS (proc);
  if (PROCESS_LIVE_P (proc))
    if (EQ (filter, Qt))
      event_stream_unselect_process (XPROCESS (proc));
    else
      event_stream_select_process (XPROCESS (proc));

  XPROCESS (proc)->filter = filter;
  XPROCESS (proc)->filter_does_read = filter_does_read;
}

DEFUN ("set-process-filter", Fset_process_filter, 2, 2, 0, /*
Give PROCESS the filter function FILTER; nil means no filter.
t means stop accepting output from the process.
When a process has a filter, each time it does output
the entire string of output is passed to the filter.
The filter gets two arguments: the process and the string of output.
If the process has a filter, its buffer is not used for output.
*/
       (proc, filter))
{
  set_process_filter (proc, filter, 0);
  return filter;
}

DEFUN ("process-filter", Fprocess_filter, 1, 1, 0, /*
Return the filter function of PROCESS; nil if none.
See `set-process-filter' for more info on filter functions.
*/
       (proc))
{
  CHECK_PROCESS (proc);
  return XPROCESS (proc)->filter;
}

DEFUN ("process-send-region", Fprocess_send_region, 3, 3, 0, /*
Send current contents of region as input to PROCESS.
PROCESS may be a process name or an actual process.
Called from program, takes three arguments, PROCESS, START and END.
If the region is more than 500 or so characters long,
it is sent in several bunches.  This may happen even for shorter regions.
Output from processes can arrive in between bunches.
*/
       (process, start, end))
{
  /* This function can GC */
  Lisp_Object proc = get_process (process);
  Bufpos st, en;

  get_buffer_range_char (current_buffer, start, end, &st, &en, 0);

  send_process (proc, Fcurrent_buffer (), 0,
                st, en - st);
  return (Qnil);
}

DEFUN ("process-send-string", Fprocess_send_string, 2, 4, 0, /*
Send PROCESS the contents of STRING as input.
PROCESS may be a process name or an actual process.
Optional arguments FROM and TO specify part of STRING, see `substring'.
If STRING is more than 500 or so characters long,
it is sent in several bunches.  This may happen even for shorter strings.
Output from processes can arrive in between bunches.
*/
       (process, string, from, to))
{
  /* This function can GC */
  Lisp_Object proc;
  Bytecount len;
  Bytecount bfr, bto;

  proc = get_process (process);
  CHECK_STRING (string);
  get_string_range_byte (string, from, to, &bfr, &bto,
			 GB_HISTORICAL_STRING_BEHAVIOR);
  len = bto - bfr;

  send_process (proc, string, 0, bfr, len);
  return (Qnil);
}


/************************************************************************/
/*                             process status                           */
/************************************************************************/

/* Some FSFmacs error handlers here.  We handle this
   in call2_trapping_errors(). */

static Lisp_Object
exec_sentinel_unwind (Lisp_Object datum)
{
  struct Lisp_Cons *d = XCONS (datum);
  XPROCESS (d->car)->sentinel = d->cdr;
  free_cons (d);
  return Qnil;
}

static void
exec_sentinel (Lisp_Object proc, Lisp_Object reason)
{
  /* This function can GC */
  Lisp_Object sentinel;
  struct Lisp_Process *p = XPROCESS (proc);
  int speccount = specpdl_depth ();

  sentinel = p->sentinel;
  if (NILP (sentinel))
    return;

  /* Some weird FSFmacs crap here with
     Vdeactivate_mark and current_buffer->keymap */

  /* Zilch the sentinel while it's running, to avoid recursive invocations;
     assure that it gets restored no matter how the sentinel exits.  */
  p->sentinel = Qnil;
  record_unwind_protect (exec_sentinel_unwind, noseeum_cons (proc, sentinel));
  /* We used to bind inhibit-quit to t here, but call2_trapping_errors()
     does that for us. */
  running_asynch_code = 1;
  call2_trapping_errors ("Error in process sentinel",
			 sentinel, proc, reason);
  running_asynch_code = 0;
  restore_match_data ();
  unbind_to (speccount, Qnil);
}

DEFUN ("set-process-sentinel", Fset_process_sentinel, 2, 2, 0, /*
Give PROCESS the sentinel SENTINEL; nil for none.
The sentinel is called as a function when the process changes state.
It gets two arguments: the process, and a string describing the change.
*/
       (proc, sentinel))
{
  CHECK_PROCESS (proc);
  XPROCESS (proc)->sentinel = sentinel;
  return sentinel;
}

DEFUN ("process-sentinel", Fprocess_sentinel, 1, 1, 0, /*
Return the sentinel of PROCESS; nil if none.
See `set-process-sentinel' for more info on sentinels.
*/
       (proc))
{
  CHECK_PROCESS (proc);
  return XPROCESS (proc)->sentinel;
}


CONST char *
signal_name (int signum)
{
  if (signum >= 0 && signum < NSIG)
#ifndef VMS
    return ((CONST char *) sys_siglist[signum]);
#else
    return ((CONST char *) sys_errlist[signum]);
#endif
  return ((CONST char *) GETTEXT ("unknown signal"));
}

/* Compute the Lisp form of the process status from
   the numeric status that was returned by `wait'.  */

static void
update_status_from_wait_code (struct Lisp_Process *p, WAITTYPE *w_fmh)
{
  /* C compiler lossage when attempting to pass w directly */
  WAITTYPE w = *w_fmh;

  if (WIFSTOPPED (w))
    {
      p->status_symbol = Qstop;
      p->exit_code = WSTOPSIG (w);
      p->core_dumped = 0;
    }
  else if (WIFEXITED (w))
    {
      p->status_symbol = Qexit;
      p->exit_code = WRETCODE (w);
      p->core_dumped = ((WCOREDUMP (w)) ? 1 : 0);
    }
  else if (WIFSIGNALED (w))
    {
      p->status_symbol = Qsignal;
      p->exit_code = (int) WTERMSIG (w);
      p->core_dumped = ((WCOREDUMP (w)) ? 1 : 0);
    }
  else
    {
      p->status_symbol = Qrun;
      p->exit_code = 0;
    }
}

void
update_process_status (Lisp_Object p,
		       Lisp_Object status_symbol,
		       int exit_code,
		       int core_dumped)
{
  XPROCESS (p)->tick++;
  process_tick++;
  XPROCESS (p)->status_symbol = status_symbol;
  XPROCESS (p)->exit_code = exit_code;
  XPROCESS (p)->core_dumped = core_dumped;
}

#ifdef SIGCHLD

#define MAX_EXITED_PROCESSES 1000
static volatile pid_t exited_processes[MAX_EXITED_PROCESSES];
static volatile WAITTYPE exited_processes_status[MAX_EXITED_PROCESSES];
static volatile int exited_processes_index;

static volatile int sigchld_happened;

/* For any processes that have changed status and are recorded
   and such, update the corresponding struct Lisp_Process.
   We separate this from record_exited_processes() so that
   we never have to call this function from within a signal
   handler.  We block SIGCHLD in case record_exited_processes()
   is called from a signal handler. */

static void
reap_exited_processes (void)
{
  int i;
  struct Lisp_Process *p;

  if (exited_processes_index <= 0)
      return;
  EMACS_BLOCK_SIGNAL (SIGCHLD);
  for (i = 0; i < exited_processes_index; i++)
    {
      int pid = exited_processes[i];
      WAITTYPE w = exited_processes_status[i];

      /* Find the process that signaled us, and record its status.  */

      p = 0;
      {
        Lisp_Object tail;
	LIST_LOOP (tail, Vprocess_list)
	  {
	    Lisp_Object proc = XCAR (tail);
	    p = XPROCESS (proc);
	    if (INTP (p->pid) && XINT (p->pid) == pid)
	      break;
	    p = 0;
	  }
      }

      if (p)
	{
	  /* Change the status of the process that was found.  */
	  p->tick++;
	  process_tick++;
	  update_status_from_wait_code (p, &w);
	  
          /* If process has terminated, stop waiting for its output.  */
	  if (WIFSIGNALED (w) || WIFEXITED (w))
	    {
	      if (p->infd >= 0)
		{
		  /* We can't just call event_stream->unselect_process_cb (p)
		     here, because that calls XtRemoveInput, which is not
		     necessarily reentrant, so we can't call this at interrupt
		     level.
		   */
		}
	    }
	}
      else
	{
          /* There was no asynchronous process found for that id.  Check
	     if we have a synchronous process. Only set sync process status
	     if there is one, so we work OK with the waitpid() call in
	     wait_for_termination(). */
	  if (synch_process_alive != 0)
	    { /* Set the global sync process status variables. */
	      synch_process_alive = 0;

	      /* Report the status of the synchronous process.  */
	      if (WIFEXITED (w))
		synch_process_retcode = WRETCODE (w);
	      else if (WIFSIGNALED (w))
		synch_process_death = signal_name (WTERMSIG (w));
	    }
        }
    }

  exited_processes_index = 0;
      
  EMACS_UNBLOCK_SIGNAL (SIGCHLD);
}

/* On receipt of a signal that a child status has changed,
 loop asking about children with changed statuses until
 the system says there are no more.  All we do is record
 the processes and wait status.

 This function could be called from within the SIGCHLD
 handler, so it must be completely reentrant.  When
 not called from a SIGCHLD handler, BLOCK_SIGCHLD should
 be non-zero so that SIGCHLD is blocked while this
 function is running. (This is necessary so avoid
 race conditions with the SIGCHLD_HAPPENED flag). */

static void
record_exited_processes (int block_sigchld)
{
  if (!sigchld_happened)
      return;
  if (block_sigchld)
    EMACS_BLOCK_SIGNAL (SIGCHLD);

  while (sigchld_happened)
    {
      int pid;
      WAITTYPE w;
      
      /* Keep trying to get a status until we get a definitive result.  */
      do 
	{
	  errno = 0;
#ifdef WNOHANG
#  ifndef WUNTRACED
#    define WUNTRACED 0
#  endif /* not WUNTRACED */
#  ifdef HAVE_WAITPID
	  pid = waitpid ((pid_t) -1, &w, WNOHANG | WUNTRACED);
#  else
	  pid = wait3 (&w, WNOHANG | WUNTRACED, 0);
#  endif
#else /* not WNOHANG */
	  pid = wait (&w);
#endif /* not WNOHANG */
	}
      while (pid <= 0 && errno == EINTR);
      
      if (pid <= 0)
	break;
      
      if (exited_processes_index < MAX_EXITED_PROCESSES)
	{
	  exited_processes[exited_processes_index] = pid;
	  exited_processes_status[exited_processes_index] = w;
	  exited_processes_index++;
	}
      
      /* On systems with WNOHANG, we just ignore the number
	 of times that SIGCHLD was signalled, and keep looping
	 until there are no more processes to wait on.  If we
	 don't have WNOHANG, we have to rely on the count in
	 SIGCHLD_HAPPENED. */
#ifndef WNOHANG
      sigchld_happened--;
#endif /* not WNOHANG */
    }

  sigchld_happened = 0;

  if (block_sigchld)
    EMACS_UNBLOCK_SIGNAL (SIGCHLD);
}

/** USG WARNING:  Although it is not obvious from the documentation
 in signal(2), on a USG system the SIGCLD handler MUST NOT call
 signal() before executing at least one wait(), otherwise the handler
 will be called again, resulting in an infinite loop.  The relevant
 portion of the documentation reads "SIGCLD signals will be queued
 and the signal-catching function will be continually reentered until
 the queue is empty".  Invoking signal() causes the kernel to reexamine
 the SIGCLD queue.   Fred Fish, UniSoft Systems Inc.
 
 (Note that now this only applies in SYS V Release 2 and before.
 On SYS V Release 3, we use sigset() to set the signal handler for
 the first time, and so we don't have to reestablish the signal handler
 in the handler below.  On SYS V Release 4, we don't get this weirdo
 behavior when we use sigaction(), which we do use.) */

static SIGTYPE
sigchld_handler (int signo)
{
#ifdef OBNOXIOUS_SYSV_SIGCLD_BEHAVIOR
  int old_errno = errno;

  sigchld_happened++;
  record_exited_processes (0);
  errno = old_errno;
#else
  sigchld_happened++;
#endif
  signal_fake_event ();
  /* WARNING - must come after wait3() for USG systems */
  EMACS_REESTABLISH_SIGNAL (signo, sigchld_handler);
  SIGRETURN;
}

#endif /* SIGCHLD */

/* Return a string describing a process status list.  */

static Lisp_Object 
status_message (struct Lisp_Process *p)
{
  Lisp_Object symbol = p->status_symbol;
  int code = p->exit_code;
  int coredump = p->core_dumped;
  Lisp_Object string, string2;

  if (EQ (symbol, Qsignal) || EQ (symbol, Qstop))
    {
      string = build_string (signal_name (code));
      if (coredump)
	string2 = build_translated_string (" (core dumped)\n");
      else
	string2 = build_string ("\n");
      set_string_char (XSTRING (string), 0,
		       DOWNCASE (current_buffer,
				 string_char (XSTRING (string), 0)));
      return concat2 (string, string2);
    }
  else if (EQ (symbol, Qexit))
    {
      if (code == 0)
	return build_translated_string ("finished\n");
      string = Fnumber_to_string (make_int (code));
      if (coredump)
	string2 = build_translated_string (" (core dumped)\n");
      else
	string2 = build_string ("\n");
      return concat2 (build_translated_string ("exited abnormally with code "),
		      concat2 (string, string2));
    }
  else
    return Fcopy_sequence (Fsymbol_name (symbol));
}

/* Tell status_notify() to check for terminated processes.  We do this
   because on some systems we sometimes miss SIGCHLD calls. (Not sure
   why.) */

void
kick_status_notify (void)
{
  process_tick++;
}

/* Report all recent events of a change in process status
   (either run the sentinel or output a message).
   This is done while Emacs is waiting for keyboard input.  */

void
status_notify (void)
{
  /* This function can GC */
  Lisp_Object tail = Qnil;
  Lisp_Object symbol = Qnil;
  Lisp_Object msg = Qnil;
  struct gcpro gcpro1, gcpro2, gcpro3;
  /* process_tick is volatile, so we have to remember it now.
     Otherwise, we get a race condition is SIGCHLD happens during
     this function.

     (Actually, this is not the case anymore.  The code to
     update the process structures has been moved out of the
     SIGCHLD handler.  But for the moment I'm leaving this
     stuff in -- it can't hurt.) */
  int temp_process_tick;

#ifdef SIGCHLD
#ifndef OBNOXIOUS_SYSV_SIGCLD_BEHAVIOR
  record_exited_processes (1);
#endif
  reap_exited_processes ();
#endif

  temp_process_tick = process_tick;

  if (update_tick == temp_process_tick)
    return;

  /* We need to gcpro tail; if read_process_output calls a filter
     which deletes a process and removes the cons to which tail points
     from Vprocess_alist, and then causes a GC, tail is an unprotected
     reference.  */
  GCPRO3 (tail, symbol, msg);

  for (tail = Vprocess_list; CONSP (tail); tail = XCDR (tail))
    {
      Lisp_Object proc = XCAR (tail);
      struct Lisp_Process *p = XPROCESS (proc);
      /* p->tick is also volatile.  Same thing as above applies. */
      int this_process_tick;

#ifdef HAVE_WAITPID
      /* #### extra check for terminated processes, in case a SIGCHLD
	 got missed (this seems to happen sometimes, I'm not sure why).
       */
      {
	WAITTYPE w;
#ifdef SIGCHLD
	EMACS_BLOCK_SIGNAL (SIGCHLD);
#endif
	if (INTP (p->pid) &&
	    waitpid (XINT (p->pid), &w, WNOHANG) == XINT (p->pid))
	  {
	    p->tick++;
	    update_status_from_wait_code (p, &w);
	  }
#ifdef SIGCHLD
	EMACS_UNBLOCK_SIGNAL (SIGCHLD);
#endif
      }
#endif
      this_process_tick = p->tick;
      if (this_process_tick != p->update_tick)
	{
	  p->update_tick = this_process_tick;

	  /* If process is still active, read any output that remains.  */
          while (!EQ (p->filter, Qt)
		 && read_process_output (proc) > 0)
            ;

	  /* Get the text to use for the message.  */
	  msg = status_message (p);

	  /* If process is terminated, deactivate it or delete it.  */
	  symbol = p->status_symbol;

	  if (EQ (symbol, Qsignal) 
              || EQ (symbol, Qexit))
	    {
	      if (delete_exited_processes)
		remove_process (proc);
	      else
		deactivate_process (proc);
	    }

	  /* Now output the message suitably.  */
	  if (!NILP (p->sentinel))
	    exec_sentinel (proc, msg);
	  /* Don't bother with a message in the buffer
	     when a process becomes runnable.  */
	  else if (!EQ (symbol, Qrun) && !NILP (p->buffer))
	    {
	      Lisp_Object old_read_only = Qnil;
	      Lisp_Object old = Fcurrent_buffer ();
	      Bufpos opoint;
              struct gcpro ngcpro1, ngcpro2;

	      /* Avoid error if buffer is deleted
		 (probably that's why the process is dead, too) */
	      if (!BUFFER_LIVE_P (XBUFFER (p->buffer)))
		continue;

              NGCPRO2 (old, old_read_only);
	      Fset_buffer (p->buffer);
	      opoint = BUF_PT (current_buffer);
	      /* Insert new output into buffer
		 at the current end-of-output marker,
		 thus preserving logical ordering of input and output.  */
	      if (XMARKER (p->mark)->buffer)
		BUF_SET_PT (current_buffer, marker_position (p->mark));
	      else
		BUF_SET_PT (current_buffer, BUF_ZV (current_buffer));
	      if (BUF_PT (current_buffer) <= opoint)
		opoint += (string_char_length (XSTRING (msg))
                           + string_char_length (XSTRING (p->name))
                           + 10);

	      old_read_only = current_buffer->read_only;
	      current_buffer->read_only = Qnil;
	      buffer_insert_c_string (current_buffer, "\nProcess ");
	      Finsert (1, &p->name);
	      buffer_insert_c_string (current_buffer, " ");
	      Finsert (1, &msg);
	      current_buffer->read_only = old_read_only;
	      Fset_marker (p->mark, make_int (BUF_PT (current_buffer)),
			   p->buffer);

	      BUF_SET_PT (current_buffer, opoint);
	      Fset_buffer (old);
              NUNGCPRO;
	    }
	}
    } /* end for */

  /* in case buffers use %s in modeline-format */
  MARK_MODELINE_CHANGED;
  redisplay ();

  update_tick = temp_process_tick;

  UNGCPRO;
}

DEFUN ("process-status", Fprocess_status, 1, 1, 0, /*
Return the status of PROCESS.
This is a symbol, one of these:

run  -- for a process that is running.
stop -- for a process stopped but continuable.
exit -- for a process that has exited.
signal -- for a process that has got a fatal signal.
open -- for a network stream connection that is open.
closed -- for a network stream connection that is closed.
nil -- if arg is a process name and no such process exists.
PROCESS may be a process, a buffer, the name of a process or buffer, or
nil, indicating the current buffer's process.
*/
       (proc))
{
  Lisp_Object status;

  if (STRINGP (proc))
    proc = Fget_process (proc);
  else
    proc = get_process (proc);

  if (NILP (proc))
    return proc;

  status = XPROCESS (proc)->status_symbol;
  if (network_connection_p (proc))
    {
      if (EQ (status, Qrun))
	status = Qopen;
      else if (EQ (status, Qexit))
	status = Qclosed;
    }
  return (status);
}

DEFUN ("process-exit-status", Fprocess_exit_status, 1, 1, 0, /*
Return the exit status of PROCESS or the signal number that killed it.
If PROCESS has not yet exited or died, return 0.
*/
       (proc))
{
  CHECK_PROCESS (proc);
  return (make_int (XPROCESS (proc)->exit_code));
}


#ifdef SIGNALS_VIA_CHARACTERS
/* Get signal character to send to process if SIGNALS_VIA_CHARACTERS */

static int
process_signal_char (int tty_fd, int signo)
{
  /* If it's not a tty, pray that these default values work */
  if (!isatty(tty_fd)) {
#define CNTL(ch) (037 & (ch))
    switch (signo)
      {
      case SIGINT:  return CNTL('C');
      case SIGQUIT: return CNTL('\\');
#ifdef SIGTSTP
      case SIGTSTP: return CNTL('Z');
#endif
      }
  }

#ifdef HAVE_TERMIOS
  /* TERMIOS is the latest and bestest, and seems most likely to work.
     If the system has it, use it. */
  {
    struct termios t;
    tcgetattr (tty_fd, &t);
    switch (signo)
      {
      case SIGINT:  return t.c_cc[VINTR];
      case SIGQUIT: return t.c_cc[VQUIT];
#  if defined (VSWTCH) && !defined (PREFER_VSUSP)
      case SIGTSTP: return t.c_cc[VSWTCH];
#  else
      case SIGTSTP: return t.c_cc[VSUSP];
#  endif
      }
  }

# elif defined (TIOCGLTC) && defined (TIOCGETC) /* not HAVE_TERMIOS */
  {
    /* On Berkeley descendants, the following IOCTL's retrieve the
       current control characters.  */
    struct tchars c;
    struct ltchars lc;
    switch (signo)
      {
      case SIGINT:  ioctl (tty_fd, TIOCGETC, &c);  return c.t_intrc;
      case SIGQUIT: ioctl (tty_fd, TIOCGETC, &c);  return c.t_quitc;
#  ifdef SIGTSTP
      case SIGTSTP: ioctl (tty_fd, TIOCGLTC, &lc); return lc.t_suspc;
#  endif /* SIGTSTP */
      }
  }

# elif defined (TCGETA) /* ! defined (TIOCGLTC) && defined (TIOCGETC) */
  {
    /* On SYSV descendants, the TCGETA ioctl retrieves the current
       control characters.  */
    struct termio t;
    ioctl (tty_fd, TCGETA, &t);
    switch (signo) {
    case SIGINT:  return t.c_cc[VINTR];
    case SIGQUIT: return t.c_cc[VQUIT];
#  ifdef SIGTSTP
    case SIGTSTP: return t.c_cc[VSWTCH];
#  endif /* SIGTSTP */
    }
  }
# else /* ! defined (TCGETA) */
#error ERROR! Using SIGNALS_VIA_CHARACTERS, but not (TIOCGLTC && TIOCGETC) || TCGETA
  /* If your system configuration files define SIGNALS_VIA_CHARACTERS,
     you'd better be using one of the alternatives above!  */
# endif /* ! defined (TCGETA) */
  return '\0';
}
#endif /* SIGNALS_VIA_CHARACTERS */


/* send a signal number SIGNO to PROCESS.
   CURRENT_GROUP means send to the process group that currently owns
   the terminal being used to communicate with PROCESS.
   This is used for various commands in shell mode.
   If NOMSG is zero, insert signal-announcements into process's buffers
   right away.

   If we can, we try to signal PROCESS by sending control characters
   down the pty.  This allows us to signal inferiors who have changed
   their uid, for which killpg would return an EPERM error.  */

static void
process_send_signal (Lisp_Object process0, int signo,
                     int current_group, int nomsg)
{
  /* This function can GC */
  Lisp_Object proc = get_process (process0);
  struct Lisp_Process *p = XPROCESS (proc);
  int gid;
  int no_pgrp = 0;

  if (network_connection_p (proc))
    error ("Network connection %s is not a subprocess",
	  XSTRING_DATA (p->name));
  if (p->infd < 0)
    error ("Process %s is not active",
	  XSTRING_DATA (p->name));

  if (!p->pty_flag)
    current_group = 0;

  /* If we are using pgrps, get a pgrp number and make it negative.  */
  if (current_group)
    {
#ifdef SIGNALS_VIA_CHARACTERS
      /* If possible, send signals to the entire pgrp
	 by sending an input character to it.  */
      {
        char sigchar = process_signal_char(p->subtty, signo);
        if (sigchar) {
          send_process (proc, Qnil, (Bufbyte *) &sigchar, 0, 1);
          return;
        }
      }
#endif /* ! defined (SIGNALS_VIA_CHARACTERS) */

#ifdef TIOCGPGRP 
      /* Get the pgrp using the tty itself, if we have that.
	 Otherwise, use the pty to get the pgrp.
	 On pfa systems, saka@pfu.fujitsu.co.JP writes:
	 "TIOCGPGRP symbol defined in sys/ioctl.h at E50.
	 But, TIOCGPGRP does not work on E50 ;-P works fine on E60"
	 His patch indicates that if TIOCGPGRP returns an error, then
	 we should just assume that p->pid is also the process group id.  */
      {
	int err;

        err = ioctl ( (p->subtty != -1 ? p->subtty : p->infd), TIOCGPGRP, &gid);

#ifdef pfa
	if (err == -1)
	  gid = - XINT (p->pid);
#endif /* ! defined (pfa) */
      }
      if (gid == -1)
	no_pgrp = 1;
      else
	gid = - gid;
#else /* ! defined (TIOCGPGRP ) */
      /* Can't select pgrps on this system, so we know that
	 the child itself heads the pgrp.  */
      gid = - XINT (p->pid);
#endif /* ! defined (TIOCGPGRP ) */
    }
  else
    gid = - XINT (p->pid);

  switch (signo)
    {
#ifdef SIGCONT
    case SIGCONT:
      p->status_symbol = Qrun;
      p->exit_code = 0;
      p->tick++;
      process_tick++;
      if (!nomsg)
	status_notify ();
      break;
#endif /* ! defined (SIGCONT) */
    case SIGINT:
#ifdef VMS
      send_process (proc, Qnil, (Bufbyte *) "\003", 0,
		    1); /* ^C */
      goto whoosh;
#endif
    case SIGQUIT:
#ifdef VMS
      send_process (proc, Qnil, (Bufbyte *) "\031", 0,
		    1); /* ^Y */
      goto whoosh;
#endif
    case SIGKILL:
#ifdef VMS
      sys$forcex (&(XINT (p->pid)), 0, 1);
      whoosh:
#endif
      flush_pending_output (p->infd);
      break;
    }

  /* If we don't have process groups, send the signal to the immediate
     subprocess.  That isn't really right, but it's better than any
     obvious alternative.  */
  if (no_pgrp)
    {
      kill (XINT (p->pid), signo);
      return;
    }

  /* gid may be a pid, or minus a pgrp's number */
#ifdef TIOCSIGSEND
  if (current_group)
    ioctl (p->infd, TIOCSIGSEND, signo);
  else
    {
      gid = - XINT (p->pid);
      kill (gid, signo);
    }
#else /* ! defined (TIOCSIGSEND) */
  EMACS_KILLPG (-gid, signo);
#endif /* ! defined (TIOCSIGSEND) */
}

DEFUN ("interrupt-process", Finterrupt_process, 0, 2, 0, /*
Interrupt process PROCESS.  May be process or name of one.
Nil or no arg means current buffer's process.
Second arg CURRENT-GROUP non-nil means send signal to
the current process-group of the process's controlling terminal
rather than to the process's own process group.
If the process is a shell, this means interrupt current subjob
rather than the shell.
*/
       (process, current_group))
{
  /* This function can GC */
  process_send_signal (process, SIGINT, !NILP (current_group), 0);
  return process;
}

DEFUN ("kill-process", Fkill_process, 0, 2, 0, /*
Kill process PROCESS.  May be process or name of one.
See function `interrupt-process' for more details on usage.
*/
       (process, current_group))
{
  /* This function can GC */
  process_send_signal (process, SIGKILL, !NILP (current_group),
		       0);
  return process;
}

DEFUN ("quit-process", Fquit_process, 0, 2, 0, /*
Send QUIT signal to process PROCESS.  May be process or name of one.
See function `interrupt-process' for more details on usage.
*/
       (process, current_group))
{
  /* This function can GC */
  process_send_signal (process, SIGQUIT, !NILP (current_group),
		       0);
  return process;
}

DEFUN ("stop-process", Fstop_process, 0, 2, 0, /*
Stop process PROCESS.  May be process or name of one.
See function `interrupt-process' for more details on usage.
*/
       (process, current_group))
{
  /* This function can GC */
#ifndef SIGTSTP
  error ("no SIGTSTP support");
#else
  process_send_signal (process, SIGTSTP, !NILP (current_group),
		       0);
#endif
  return process;
}

DEFUN ("continue-process", Fcontinue_process, 0, 2, 0, /*
Continue process PROCESS.  May be process or name of one.
See function `interrupt-process' for more details on usage.
*/
       (process, current_group))
{
  /* This function can GC */
#ifdef SIGCONT
    process_send_signal (process, SIGCONT, !NILP (current_group),
			 0);
#else
    error ("no SIGCONT support");
#endif
  return process;
}

DEFUN ("signal-process", Fsignal_process, 2, 2,
       "nProcess number: \nnSignal code: ", /*
Send the process with process id PID the signal with code SIGCODE.
PID must be an integer.  The process need not be a child of this Emacs.
SIGCODE may be an integer, or a symbol whose name is a signal name.
*/
       (pid, sigcode))
{
  CHECK_INT (pid);

#define handle_signal(NAME, VALUE)			\
  else if (!strcmp ((CONST char *) name, NAME))		\
    XSETINT (sigcode, VALUE)

  if (INTP (sigcode))
    ;
  else
    {
      Bufbyte *name;

      CHECK_SYMBOL (sigcode);
      name = string_data (XSYMBOL (sigcode)->name);

      if (0)
	;
#ifdef SIGHUP
      handle_signal ("SIGHUP", SIGHUP);
#endif
#ifdef SIGINT
      handle_signal ("SIGINT", SIGINT);
#endif
#ifdef SIGQUIT
      handle_signal ("SIGQUIT", SIGQUIT);
#endif
#ifdef SIGILL
      handle_signal ("SIGILL", SIGILL);
#endif
#ifdef SIGABRT
      handle_signal ("SIGABRT", SIGABRT);
#endif
#ifdef SIGEMT
      handle_signal ("SIGEMT", SIGEMT);
#endif
#ifdef SIGKILL
      handle_signal ("SIGKILL", SIGKILL);
#endif
#ifdef SIGFPE
      handle_signal ("SIGFPE", SIGFPE);
#endif
#ifdef SIGBUS
      handle_signal ("SIGBUS", SIGBUS);
#endif
#ifdef SIGSEGV
      handle_signal ("SIGSEGV", SIGSEGV);
#endif
#ifdef SIGSYS
      handle_signal ("SIGSYS", SIGSYS);
#endif
#ifdef SIGPIPE
      handle_signal ("SIGPIPE", SIGPIPE);
#endif
#ifdef SIGALRM
      handle_signal ("SIGALRM", SIGALRM);
#endif
#ifdef SIGTERM
      handle_signal ("SIGTERM", SIGTERM);
#endif
#ifdef SIGURG
      handle_signal ("SIGURG", SIGURG);
#endif
#ifdef SIGSTOP
      handle_signal ("SIGSTOP", SIGSTOP);
#endif
#ifdef SIGTSTP
      handle_signal ("SIGTSTP", SIGTSTP);
#endif
#ifdef SIGCONT
      handle_signal ("SIGCONT", SIGCONT);
#endif
#ifdef SIGCHLD
      handle_signal ("SIGCHLD", SIGCHLD);
#endif
#ifdef SIGTTIN
      handle_signal ("SIGTTIN", SIGTTIN);
#endif
#ifdef SIGTTOU
      handle_signal ("SIGTTOU", SIGTTOU);
#endif
#ifdef SIGIO
      handle_signal ("SIGIO", SIGIO);
#endif
#ifdef SIGXCPU
      handle_signal ("SIGXCPU", SIGXCPU);
#endif
#ifdef SIGXFSZ
      handle_signal ("SIGXFSZ", SIGXFSZ);
#endif
#ifdef SIGVTALRM
      handle_signal ("SIGVTALRM", SIGVTALRM);
#endif
#ifdef SIGPROF
      handle_signal ("SIGPROF", SIGPROF);
#endif
#ifdef SIGWINCH
      handle_signal ("SIGWINCH", SIGWINCH);
#endif
#ifdef SIGINFO
      handle_signal ("SIGINFO", SIGINFO);
#endif
#ifdef SIGUSR1
      handle_signal ("SIGUSR1", SIGUSR1);
#endif
#ifdef SIGUSR2
      handle_signal ("SIGUSR2", SIGUSR2);
#endif
      else
	error ("Undefined signal name %s", name);
    }

#undef handle_signal

#ifdef WINDOWSNT
  /* Only works for kill-type signals */
  return make_int (win32_kill_process (XINT (pid), XINT (sigcode)));
#else
  return make_int (kill (XINT (pid), XINT (sigcode)));
#endif
}

DEFUN ("process-send-eof", Fprocess_send_eof, 0, 1, 0, /*
Make PROCESS see end-of-file in its input.
PROCESS may be a process, a buffer, the name of a process or buffer, or
nil, indicating the current buffer's process.
If PROCESS is a network connection, or is a process communicating
through a pipe (as opposed to a pty), then you cannot send any more
text to PROCESS after you call this function.
*/
       (process))
{
  /* This function can GC */
  Lisp_Object proc;

  proc = get_process (process);

  /* Make sure the process is really alive.  */
  if (! EQ (XPROCESS (proc)->status_symbol, Qrun))
    error ("Process %s not running", XSTRING_DATA (XPROCESS (proc)->name));

#ifdef VMS
  send_process (proc, Qnil, (Bufbyte *) "\032", 0, 1);   /* ^Z */
#else
  if (XPROCESS (proc)->pty_flag)
    {
      /* #### get_eof_char simply doesn't return the correct character
         here.  Maybe it is needed to determine the right eof
         character in init_process_fds but here it simply screws
         things up. */
#if 0
      Bufbyte eof_char = get_eof_char (XPROCESS (proc));
      send_process (proc, Qnil, &eof_char, 0, 1);
#else
      send_process (proc, Qnil, (CONST Bufbyte *) "\004", 0, 1);
#endif
    }
  else
    {
      close (XPROCESS (proc)->outfd);
      XPROCESS (proc)->outfd = open (NULL_DEVICE, O_WRONLY, 0);
    }
#endif /* !VMS */
  return process;
}


/************************************************************************/
/*                          deleting a process                          */
/************************************************************************/

void
deactivate_process (Lisp_Object proc)
{
  int inchannel, outchannel;
  struct Lisp_Process *p = XPROCESS (proc);
  SIGTYPE (*old_sigpipe) (int) = 0;

  inchannel = p->infd;
  outchannel = p->outfd;

  /* closing the outstream could result in SIGPIPE, so ignore it. */
  old_sigpipe =
    (SIGTYPE (*) (int)) signal (SIGPIPE, SIG_IGN);
  if (!NILP (p->instream))
    Lstream_close (XLSTREAM (p->instream));
  if (!NILP (p->outstream))
    Lstream_close (XLSTREAM (p->outstream));
  signal (SIGPIPE, old_sigpipe);

  if (inchannel >= 0)
    {
      /* Beware SIGCHLD hereabouts. */
      flush_pending_output (inchannel);
      close_descriptor_pair (inchannel, outchannel);
      if (!NILP (p->pid))
	{
	  /* It's possible that we got as far in the process-creation
	     process as creating the descriptors but didn't get so
	     far as selecting the process for input.  In this
	     case, p->pid is nil: p->pid is set at the same time that
	     the process is selected for input. */
#ifdef VMS
	  {
	    VMS_PROC_STUFF *get_vms_process_pointer (), *vs;
	    if (outchannel >= 0) 
	      sys$dassgn (outchannel);
	    vs = get_vms_process_pointer (XINT (p->pid));
	    if (vs)
	      give_back_vms_process_stuff (vs);
	  }
#endif /* VMS */
	  /* Must call this before setting the file descriptors to 0 */
	  event_stream_unselect_process (p);
	}

      p->infd  = -1;
      p->outfd = -1;
      descriptor_to_process[inchannel] = Qnil;
    }
}

static void
remove_process (Lisp_Object proc)
{
  Vprocess_list = delq_no_quit (proc, Vprocess_list);
  Fset_marker (XPROCESS (proc)->mark, Qnil, Qnil);

  deactivate_process (proc);
}

DEFUN ("delete-process", Fdelete_process, 1, 1, 0, /*
Delete PROCESS: kill it and forget about it immediately.
PROCESS may be a process or the name of one, or a buffer name.
*/
       (proc))
{
  /* This function can GC */
  struct Lisp_Process *p;
  proc = get_process (proc);
  p = XPROCESS (proc);
  if (network_connection_p (proc))
    {
      p->status_symbol = Qexit;
      p->exit_code = 0;
      p->core_dumped = 0;
      p->tick++;
      process_tick++;
    }
  else if (p->infd >= 0)
    {
      Fkill_process (proc, Qnil);
      /* Do this now, since remove_process will make sigchld_handler do nothing.  */
      p->status_symbol = Qsignal;
      p->exit_code = SIGKILL;
      p->core_dumped = 0;
      p->tick++;
      process_tick++;
      status_notify ();
    }
  remove_process (proc);
  return Qnil;
}

/* Kill all processes associated with `buffer'.
 If `buffer' is nil, kill all processes  */

void
kill_buffer_processes (Lisp_Object buffer)
{
  Lisp_Object tail;

  for (tail = Vprocess_list; GC_CONSP (tail);
       tail = XCDR (tail))
    {
      Lisp_Object proc = XCAR (tail);
      if (GC_PROCESSP (proc)
	  && (GC_NILP (buffer) || GC_EQ (XPROCESS (proc)->buffer, buffer)))
	{
	  if (network_connection_p (proc))
	    Fdelete_process (proc);
	  else if (XPROCESS (proc)->infd >= 0)
	    process_send_signal (proc, SIGHUP, 0, 1);
	}
    }
}

#if 0 /* Unused */
int
count_active_processes (void)
{
  Lisp_Object tail;
  int count = 0;

  for (tail = Vprocess_list; CONSP (tail); tail = XCDR (tail))
    {
      Lisp_Object status = XPROCESS (XCAR (tail))->status_symbol;
      if ((EQ (status, Qrun) || EQ (status, Qstop)))
	count++;
    }

  return count;
}
#endif /* Unused */

DEFUN ("process-kill-without-query", Fprocess_kill_without_query, 1, 2, 0, /*
Say no query needed if PROCESS is running when Emacs is exited.
Optional second argument if non-nil says to require a query.
Value is t if a query was formerly required.
*/
       (proc, require_query_p))
{
  int tem;

  CHECK_PROCESS (proc);
  tem = XPROCESS (proc)->kill_without_query;
  XPROCESS (proc)->kill_without_query = NILP (require_query_p);

  return (tem ? Qnil : Qt);
}

DEFUN ("process-kill-without-query-p", Fprocess_kill_without_query_p, 1, 1, 0, /*
Whether PROC will be killed without query if running when emacs is exited.
*/
       (proc))
{
  CHECK_PROCESS (proc);
  return (XPROCESS (proc)->kill_without_query ? Qt : Qnil);
}


/* This is not named init_process in order to avoid a conflict with NS 3.3 */
void
init_xemacs_process (void)
{
  int i;

#ifdef SIGCHLD
# ifndef CANNOT_DUMP
  if (! noninteractive || initialized)
# endif
    signal (SIGCHLD, sigchld_handler);
#endif /* SIGCHLD */

  Vprocess_list = Qnil;
  for (i = 0; i < MAXDESC; i++)
    {
      descriptor_to_process[i] = Qnil;
      proc_buffered_char[i] = -1;
    }
}
#if 0

xxDEFUN ("process-connection", Fprocess_connection, Sprocess_connection,
	 0, 1, 0 /*
Return the connection type of `PROCESS'.  This can be nil (pipe),
t or pty (pty) or stream (socket connection).
*/ )
  (process)
     Lisp_Object process;
{
  return XPROCESS (process)->type;
}

#endif /* 0 */

void
syms_of_process (void)
{
  defsymbol (&Qprocessp, "processp");
  defsymbol (&Qrun, "run");
  defsymbol (&Qstop, "stop");
  defsymbol (&Qsignal, "signal");
  /* Qexit is already defined by syms_of_eval
   * defsymbol (&Qexit, "exit"); 
   */
  defsymbol (&Qopen, "open");
  defsymbol (&Qclosed, "closed");

  DEFSUBR (Fprocessp);
  DEFSUBR (Fget_process);
  DEFSUBR (Fget_buffer_process);
  DEFSUBR (Fdelete_process);
  DEFSUBR (Fprocess_status);
  DEFSUBR (Fprocess_exit_status);
  DEFSUBR (Fprocess_id);
  DEFSUBR (Fprocess_name);
  DEFSUBR (Fprocess_tty_name);
  DEFSUBR (Fprocess_command);
  DEFSUBR (Fset_process_buffer);
  DEFSUBR (Fprocess_buffer);
  DEFSUBR (Fprocess_mark);
  DEFSUBR (Fset_process_filter);
  DEFSUBR (Fprocess_filter);
  DEFSUBR (Fset_process_window_size);
  DEFSUBR (Fset_process_sentinel);
  DEFSUBR (Fprocess_sentinel);
  DEFSUBR (Fprocess_kill_without_query);
  DEFSUBR (Fprocess_kill_without_query_p);
  DEFSUBR (Fprocess_list);
  DEFSUBR (Fstart_process_internal);
#ifdef HAVE_SOCKETS
  DEFSUBR (Fopen_network_stream_internal);
#endif /* HAVE_SOCKETS */
  DEFSUBR (Fprocess_send_region);
  DEFSUBR (Fprocess_send_string);
  DEFSUBR (Finterrupt_process);
  DEFSUBR (Fkill_process);
  DEFSUBR (Fquit_process);
  DEFSUBR (Fstop_process);
  DEFSUBR (Fcontinue_process);
  DEFSUBR (Fprocess_send_eof);
  DEFSUBR (Fsignal_process);
/*  DEFSUBR (Fprocess_connection); */
}

void
vars_of_process (void)
{
  Fprovide (intern ("subprocesses"));
#ifdef HAVE_SOCKETS
  Fprovide (intern ("network-streams"));
#endif
  staticpro (&Vprocess_list);

  DEFVAR_BOOL ("delete-exited-processes", &delete_exited_processes /*
*Non-nil means delete processes immediately when they exit.
nil means don't delete them until `list-processes' is run.
*/ );

  delete_exited_processes = 1;

  DEFVAR_LISP ("process-connection-type", &Vprocess_connection_type /*
Control type of device used to communicate with subprocesses.
Values are nil to use a pipe, or t or `pty' to use a pty.
The value has no effect if the system has no ptys or if all ptys are busy:
then a pipe is used in any case.
The value takes effect when `start-process' is called.
*/ );
  Vprocess_connection_type = Qt;
}

#endif /* not NO_SUBPROCESSES */
