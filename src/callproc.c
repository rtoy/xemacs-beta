/* Old synchronous subprocess invocation for XEmacs.
   Copyright (C) 1985, 86, 87, 88, 93, 94, 95 Free Software Foundation, Inc.
   Copyright (C) 2000, 2001, 2002 Ben Wing.

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
/* Partly sync'ed with 19.36.4 */


/* #### Everything in this file should go.  As soon as I merge my
   stderr-proc WS, it will.
*/

#include <config.h>
#include "lisp.h"

#include "buffer.h"
#include "commands.h"
#include "file-coding.h"
#include "insdel.h"
#include "lstream.h"
#include "process.h"
#include "sysdep.h"
#include "window.h"

#include "sysdir.h"
#include "sysfile.h"
#include "sysproc.h"
#include "syssignal.h"
#include "systime.h"
#include "systty.h"

/* True iff we are about to fork off a synchronous process or if we
   are waiting for it.  */
volatile int synch_process_alive;

/* Nonzero => this is a string explaining death of synchronous subprocess.  */
const char *synch_process_death;

/* If synch_process_death is zero,
   this is exit code of synchronous subprocess.  */
int synch_process_retcode;

/* Clean up when exiting Fcall_process_internal.
   On Windows, delete the temporary file on any kind of termination.
   On Unix, kill the process and any children on termination by signal.  */

/* Nonzero if this is termination due to exit.  */
static int call_process_exited;

static Lisp_Object
call_process_kill (Lisp_Object fdpid)
{
  Lisp_Object fd = Fcar (fdpid);
  Lisp_Object pid = Fcdr (fdpid);

  if (!NILP (fd))
    retry_close (XINT (fd));

  if (!NILP (pid))
    EMACS_KILLPG (XINT (pid), SIGKILL);

  synch_process_alive = 0;
  return Qnil;
}

static Lisp_Object
call_process_cleanup (Lisp_Object fdpid)
{
  int fd  = XINT (Fcar (fdpid));
  int pid = XINT (Fcdr (fdpid));

  if (!call_process_exited &&
      EMACS_KILLPG (pid, SIGINT) == 0)
  {
    int speccount = specpdl_depth ();

    record_unwind_protect (call_process_kill, fdpid);
    /* #### "c-G" -- need non-consing Single-key-description */
    message ("Waiting for process to die...(type C-g again to kill it instantly)");

    wait_for_termination (pid);

    /* "Discard" the unwind protect.  */
    XCAR (fdpid) = Qnil;
    XCDR (fdpid) = Qnil;
    unbind_to (speccount);

    message ("Waiting for process to die... done");
  }
  synch_process_alive = 0;
  retry_close (fd);
  return Qnil;
}

DEFUN ("old-call-process-internal", Fold_call_process_internal, 1, MANY, 0, /*
Call PROGRAM synchronously in separate process, with coding-system specified.
Arguments are
 (PROGRAM &optional INFILE BUFFER DISPLAY &rest ARGS).
The program's input comes from file INFILE (nil means `/dev/null').
Insert output in BUFFER before point; t means current buffer;
 nil for BUFFER means discard it; 0 means discard and don't wait.
BUFFER can also have the form (REAL-BUFFER STDERR-FILE); in that case,
REAL-BUFFER says what to do with standard output, as above,
while STDERR-FILE says what to do with standard error in the child.
STDERR-FILE may be nil (discard standard error output),
t (mix it with ordinary output), or a file name string.

Fourth arg DISPLAY non-nil means redisplay buffer as output is inserted.
Remaining arguments are strings passed as command arguments to PROGRAM.

If BUFFER is 0, `call-process' returns immediately with value nil.
Otherwise it waits for PROGRAM to terminate and returns a numeric exit status
 or a signal description string.
If you quit, the process is killed with SIGINT, or SIGKILL if you
 quit again.
*/
       (int nargs, Lisp_Object *args))
{
  /* This function can GC */
  Lisp_Object infile, buffer, current_dir, display, path;
  int fd[2];
  int filefd;
  int pid;
  char buf[16384];
  char *bufptr = buf;
  int bufsize = 16384;
  int speccount = specpdl_depth ();
  struct gcpro gcpro1, gcpro2, gcpro3;
  Intbyte **new_argv = alloca_array (Intbyte *, max (2, nargs - 2));

  /* File to use for stderr in the child.
     t means use same as standard output.  */
  Lisp_Object error_file;

  CHECK_STRING (args[0]);

  error_file = Qt;

#if defined (NO_SUBPROCESSES)
  /* Without asynchronous processes we cannot have BUFFER == 0.  */
  if (nargs >= 3 && !INTP (args[2]))
    signal_error (Qunimplemented, "Operating system cannot handle asynchronous subprocesses", Qunbound);
#endif /* NO_SUBPROCESSES */

  /* Do all filename munging before building new_argv because GC in
   *  Lisp code called by various filename-hacking routines might
   *  relocate strings */
  locate_file (Vexec_path, args[0], Vlisp_EXEC_SUFFIXES, &path, X_OK);

  /* Make sure that the child will be able to chdir to the current
     buffer's current directory, or its unhandled equivalent. [[ We
     can't just have the child check for an error when it does the
     chdir, since it's in a vfork. ]] -- not any more, we don't use
     vfork. -ben

     Note: These calls are spread out to insure that the return values
     of the calls (which may be newly-created strings) are properly
     GC-protected. */
  {
    struct gcpro ngcpro1, ngcpro2;
    NGCPRO2 (current_dir, path);   /* Caller gcprotects args[] */
    current_dir = current_buffer->directory;
    /* If the current dir has no terminating slash, we'll get undesirable
       results, so put the slash back. */
    current_dir = Ffile_name_as_directory (current_dir);
    current_dir = Funhandled_file_name_directory (current_dir);
    current_dir = expand_and_dir_to_file (current_dir, Qnil);

#if 0
    /* This is in FSF, but it breaks everything in the presence of
       ange-ftp-visited files, so away with it.  */
    if (NILP (Ffile_accessible_directory_p (current_dir)))
      signal_error (Qprocess_error, "Setting current directory",
		    current_buffer->directory);
#endif /* 0 */
    NUNGCPRO;
  }

  GCPRO2 (current_dir, path);

  if (nargs >= 2 && ! NILP (args[1]))
    {
      struct gcpro ngcpro1;
      NGCPRO1 (current_buffer->directory);
      infile = Fexpand_file_name (args[1], current_buffer->directory);
      NUNGCPRO;
      CHECK_STRING (infile);
    }
  else
    infile = build_string (NULL_DEVICE);

  UNGCPRO;

  GCPRO3 (infile, current_dir, path);  	/* Fexpand_file_name might trash it */

  if (nargs >= 3)
    {
      buffer = args[2];

      /* If BUFFER is a list, its meaning is
	 (BUFFER-FOR-STDOUT FILE-FOR-STDERR).  */
      if (CONSP (buffer))
	{
	  if (CONSP (XCDR (buffer)))
	    {
	      Lisp_Object file_for_stderr = XCAR (XCDR (buffer));

	      if (NILP (file_for_stderr) || EQ (Qt, file_for_stderr))
		error_file = file_for_stderr;
	      else
		error_file = Fexpand_file_name (file_for_stderr, Qnil);
	    }

	  buffer = XCAR (buffer);
	}

      if (!(EQ (buffer, Qnil)
	    || EQ (buffer, Qt)
	    || ZEROP (buffer)))
	{
	  Lisp_Object spec_buffer = buffer;
	  buffer = Fget_buffer (buffer);
	  /* Mention the buffer name for a better error message.  */
	  if (NILP (buffer))
	    CHECK_BUFFER (spec_buffer);
	  CHECK_BUFFER (buffer);
	}
    }
  else
    buffer = Qnil;

  UNGCPRO;

  display = ((nargs >= 4) ? args[3] : Qnil);

  /* From here we assume we won't GC (unless an error is signaled). */
  {
    REGISTER int i;
    for (i = 4; i < nargs; i++)
      {
	CHECK_STRING (args[i]);
	new_argv[i - 3] = XSTRING_DATA (args[i]);
      }
  }
  new_argv[max(nargs - 3,1)] = 0;

  if (NILP (path))
    signal_error (Qprocess_error, "Searching for program",
		  Fcons (args[0], Qnil));
  new_argv[0] = XSTRING_DATA (path);

  filefd = qxe_open (XSTRING_DATA (infile), O_RDONLY | OPEN_BINARY, 0);
  if (filefd < 0)
    report_process_error ("Opening process input file", infile);

  if (INTP (buffer))
    {
      fd[1] = qxe_open ((Intbyte *) NULL_DEVICE, O_WRONLY | OPEN_BINARY, 0);
      fd[0] = -1;
    }
  else
    {
      pipe (fd);
#if 0
      /* Replaced by close_process_descs */
      set_exclusive_use (fd[0]);
#endif
    }

  {
    REGISTER int fd1 = fd[1];
    int fd_error = fd1;

    /* Record that we're about to create a synchronous process.  */
    synch_process_alive = 1;

    /* These vars record information from process termination.
       Clear them now before process can possibly terminate,
       to avoid timing error if process terminates soon.  */
    synch_process_death = 0;
    synch_process_retcode = 0;

    if (NILP (error_file))
      fd_error = qxe_open ((Intbyte *) NULL_DEVICE, O_WRONLY | OPEN_BINARY);
    else if (STRINGP (error_file))
      {
	fd_error = qxe_open (XSTRING_DATA (error_file),
			     O_WRONLY | O_TRUNC | O_CREAT | OPEN_BINARY,
			     CREAT_MODE);
      }

    if (fd_error < 0)
      {
	int save_errno = errno;
	retry_close (filefd);
	retry_close (fd[0]);
	if (fd1 >= 0)
	  retry_close (fd1);
	errno = save_errno;
	report_process_error ("Cannot open", Fcons (error_file, Qnil));
      }

    pid = fork ();

    if (pid == 0)
      {
	if (fd[0] >= 0)
	  retry_close (fd[0]);
	/* This is necessary because some shells may attempt to
	   access the current controlling terminal and will hang
	   if they are run in the background, as will be the case
	   when XEmacs is started in the background.  Martin
	   Buchholz observed this problem running a subprocess
	   that used zsh to call gzip to uncompress an info
	   file. */
	disconnect_controlling_terminal ();
	child_setup (filefd, fd1, fd_error, new_argv, current_dir);
      }
    if (fd_error >= 0)
      retry_close (fd_error);

    /* Close most of our fd's, but not fd[0]
       since we will use that to read input from.  */
    retry_close (filefd);
    if (fd1 >= 0)
      retry_close (fd1);
  }

  if (pid < 0)
    {
      int save_errno = errno;
      if (fd[0] >= 0)
	retry_close (fd[0]);
      errno = save_errno;
      report_process_error ("Doing fork", Qunbound);
    }

  if (INTP (buffer))
    {
      if (fd[0] >= 0)
	retry_close (fd[0]);
#if defined (NO_SUBPROCESSES)
      /* If Emacs has been built with asynchronous subprocess support,
	 we don't need to do this, I think because it will then have
	 the facilities for handling SIGCHLD.  */
      wait_without_blocking ();
#endif /* NO_SUBPROCESSES */
      return Qnil;
    }

  {
    int nread;
    int total_read = 0;
    Lisp_Object instream;
    struct gcpro ngcpro1;

    /* Enable sending signal if user quits below.  */
    call_process_exited = 0;

    record_unwind_protect (call_process_cleanup,
                           Fcons (make_int (fd[0]), make_int (pid)));

    /* FSFmacs calls Fset_buffer() here.  We don't have to because
       we can insert into buffers other than the current one. */
    if (EQ (buffer, Qt))
      buffer = wrap_buffer (current_buffer);
    instream = make_filedesc_input_stream (fd[0], 0, -1, LSTR_ALLOW_QUIT);
    instream =
      make_coding_input_stream
	(XLSTREAM (instream),
	 get_coding_system_for_text_file (Vcoding_system_for_read, 1),
	 CODING_DECODE, 0);
    NGCPRO1 (instream);
    while (1)
      {
	QUIT;
	/* Repeatedly read until we've filled as much as possible
	   of the buffer size we have.  But don't read
	   less than 1024--save that for the next bufferfull.  */

	nread = 0;
	while (nread < bufsize - 1024)
	  {
	    Bytecount this_read
	      = Lstream_read (XLSTREAM (instream), bufptr + nread,
			      bufsize - nread);

	    if (this_read < 0)
	      goto give_up;

	    if (this_read == 0)
	      goto give_up_1;

	    nread += this_read;
	  }

      give_up_1:

	/* Now NREAD is the total amount of data in the buffer.  */
	if (nread == 0)
	  break;

#if 0
	/* [[check Vbinary_process_output]] */
#endif

	total_read += nread;

	if (!NILP (buffer))
	  buffer_insert_raw_string (XBUFFER (buffer), (Intbyte *) bufptr,
				    nread);

	/* Make the buffer bigger as we continue to read more data,
	   but not past 64k.  */
	if (bufsize < 64 * 1024 && total_read > 32 * bufsize)
	  {
	    bufsize *= 2;
	    bufptr = (char *) alloca (bufsize);
	  }

	if (!NILP (display) && INTERACTIVE)
	  {
	    redisplay ();
	  }
      }
  give_up:
    Lstream_close (XLSTREAM (instream));
    NUNGCPRO;

    QUIT;
    /* Wait for it to terminate, unless it already has.  */
    wait_for_termination (pid);

    /* Don't kill any children that the subprocess may have left behind
       when exiting.  */
    call_process_exited = 1;
    unbind_to (speccount);

    if (synch_process_death)
      return build_msg_string (synch_process_death);
    return make_int (synch_process_retcode);
  }
}



/* Move the file descriptor FD so that its number is not less than MIN. *
   The original file descriptor remains open.  */
static int
relocate_fd (int fd, int min)
{
  if (fd >= min)
    return fd;
  else
    {
      int newfd = dup (fd);
      if (newfd == -1)
	{
	  Intbyte *errmess;
	  GET_STRERROR (errmess, errno);
	  stderr_out ("Error while setting up child: %s\n", errmess);
	  _exit (1);
	}
      return relocate_fd (newfd, min);
    }
}

/* This is the last thing run in a newly forked inferior
   either synchronous or asynchronous.
   Copy descriptors IN, OUT and ERR
   as descriptors STDIN_FILENO, STDOUT_FILENO, and STDERR_FILENO.
   Initialize inferior's priority, pgrp, connected dir and environment.
   then exec another program based on new_argv.

   XEmacs: We've removed the SET_PGRP argument because it's already
   done by the callers of child_setup.

   CURRENT_DIR is an elisp string giving the path of the current
   directory the subprocess should have.  Since we can't really signal
   a decent error from within the child (not quite correct in
   XEmacs?), this should be verified as an executable directory by the
   parent.  */

void
child_setup (int in, int out, int err, Intbyte **new_argv,
	     Lisp_Object current_dir)
{
  Intbyte **env;
  Intbyte *pwd;

#ifdef SET_EMACS_PRIORITY
  if (emacs_priority != 0)
    nice (- emacs_priority);
#endif

  /* Under Windows, we are not in a child process at all, so we should
     not close handles inherited from the parent -- we are the parent
     and doing so will screw up all manner of things!  Similarly, most
     of the rest of the cleanup done in this function is not done
     under Windows.

     #### This entire child_setup() function is an utter and complete
     piece of shit.  I would rewrite it, at the very least splitting
     out the Windows and non-Windows stuff into two completely
     different functions; but instead I'm trying to make it go away
     entirely, using the Lisp definition in process.el.  What's left
     is to fix up the routines in event-msw.c (and in event-Xt.c and
     event-tty.c) to allow for stream devices to be handled correctly.
     There isn't much to do, in fact, and I'll fix it shortly.  That
     way, the Lisp definition can be used non-interactively too. */
#if !defined (NO_SUBPROCESSES)
  /* Close Emacs's descriptors that this process should not have.  */
  close_process_descs ();
#endif /* not NO_SUBPROCESSES */
  close_load_descs ();

  /* [[Note that use of alloca is always safe here.  It's obvious for systems
     that do not have true vfork or that have true (stack) alloca.
     If using vfork and C_ALLOCA it is safe because that changes
     the superior's static variables as if the superior had done alloca
     and will be cleaned up in the usual way.]] -- irrelevant because
     XEmacs does not use vfork. */
  {
    REGISTER Bytecount i;

    i = XSTRING_LENGTH (current_dir);
    pwd = alloca_array (Intbyte, i + 6);
    memcpy (pwd, "PWD=", 4);
    memcpy (pwd + 4, XSTRING_DATA (current_dir), i);
    i += 4;
    if (!IS_DIRECTORY_SEP (pwd[i - 1]))
      pwd[i++] = DIRECTORY_SEP;
    pwd[i] = 0;

    /* [[We can't signal an Elisp error here; we're in a vfork.  Since
       the callers check the current directory before forking, this
       should only return an error if the directory's permissions
       are changed between the check and this chdir, but we should
       at least check.]] -- irrelevant because XEmacs does not use vfork. */
    if (qxe_chdir (pwd + 4) < 0)
      {
	/* Don't report the chdir error, or ange-ftp.el doesn't work. */
	/* (FSFmacs does _exit (errno) here.) */
	pwd = 0;
      }
    else
      {
	/* Strip trailing "/".  Cretinous *[]&@$#^%@#$% Un*x */
	/* leave "//" (from FSF) */
	while (i > 6 && IS_DIRECTORY_SEP (pwd[i - 1]))
	  pwd[--i] = 0;
      }
  }

  /* Set `env' to a vector of the strings in Vprocess_environment.  */
  /* + 2 to include PWD and terminating 0.  */
  env = alloca_array (Intbyte *, XINT (Flength (Vprocess_environment)) + 2);
  {
    REGISTER Lisp_Object tail;
    Intbyte **new_env = env;

    /* If we have a PWD envvar and we know the real current directory,
       pass one down, but with corrected value.  */
    if (pwd && egetenv ("PWD"))
      *new_env++ = pwd;

    /* Copy the Vprocess_environment strings into new_env.  */
    for (tail = Vprocess_environment;
	 CONSP (tail) && STRINGP (XCAR (tail));
	 tail = XCDR (tail))
      {
      Intbyte **ep = env;
      Intbyte *envvar = XSTRING_DATA (XCAR (tail));

      /* See if envvar duplicates any string already in the env.
	 If so, don't put it in.
	 When an env var has multiple definitions,
	 we keep the definition that comes first in process-environment.  */
      for (; ep != new_env; ep++)
	{
	  Intbyte *p = *ep, *q = envvar;
	  while (1)
	    {
	      if (*q == 0)
		/* The string is malformed; might as well drop it.  */
		goto duplicate;
	      if (*q != *p)
		break;
	      if (*q == '=')
		goto duplicate;
	      p++, q++;
	    }
	}
      if (pwd && !qxestrncmp ((Intbyte *) "PWD=", envvar, 4))
	{
	  *new_env++ = pwd;
	  pwd = 0;
	}
      else
        *new_env++ = envvar;

    duplicate: ;
    }

    *new_env = 0;
  }

  /* Make sure that in, out, and err are not actually already in
     descriptors zero, one, or two; this could happen if Emacs is
     started with its standard in, out, or error closed, as might
     happen under X.  */
  in  = relocate_fd (in,  3);
  out = relocate_fd (out, 3);
  err = relocate_fd (err, 3);

  /* Set the standard input/output channels of the new process.  */
  retry_close (STDIN_FILENO);
  retry_close (STDOUT_FILENO);
  retry_close (STDERR_FILENO);

  dup2 (in,  STDIN_FILENO);
  dup2 (out, STDOUT_FILENO);
  dup2 (err, STDERR_FILENO);

  retry_close (in);
  retry_close (out);
  retry_close (err);

  /* I can't think of any reason why child processes need any more
     than the standard 3 file descriptors.  It would be cleaner to
     close just the ones that need to be, but the following brute
     force approach is certainly effective, and not too slow. */
  {
    int fd;
    for (fd=3; fd<=64; fd++)
      retry_close (fd);
  }

#ifdef vipc
  something missing here;
#endif /* vipc */

  /* we've wrapped execve; it translates its arguments */
  qxe_execve (new_argv[0], new_argv, env);

  stdout_out ("Can't exec program %s\n", new_argv[0]);
  _exit (1);
}

void
syms_of_callproc (void)
{
  DEFSUBR (Fold_call_process_internal);
}
