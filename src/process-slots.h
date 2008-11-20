/* Definitions of marked slots in processes
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

/* Synched up with: ????.  Split out of procimpl.h. */

/* We define the Lisp_Objects in the process structure in a separate file
   because there are numerous places we want to iterate over them, such
   as when defining them in the structure, initializing them, or marking
   them.

   To use, define MARKED_SLOT before including this file.  No need to
   undefine; that happens automatically.  */

  /* Name of this process */
  MARKED_SLOT (name)
  /* List of command arguments that this process was run with */
  MARKED_SLOT (command)
  /* (funcall FILTER PROC STRING)  (if FILTER is non-nil)
     to dispose of a bunch of chars from the process all at once */
  MARKED_SLOT (filter)
  /* (funcall FILTER PROC STRING)  (if FILTER is non-nil)
     to dispose of a bunch of chars from the stderr of process all at once */
  MARKED_SLOT (stderr_filter)
  /* (funcall SENTINEL PROCESS) when process state changes */
  MARKED_SLOT (sentinel)
  /* Buffer that output or stderr output is going to */
  MARKED_SLOT (buffer)
  MARKED_SLOT (stderr_buffer)
  /* Marker set to end of last buffer-inserted output from this process */
  MARKED_SLOT (mark)
  MARKED_SLOT (stderr_mark)
  /* Lisp_Int of subprocess' PID, or a cons of
     service/host if this is really a network connection */
  MARKED_SLOT (pid)

  /* Symbol indicating status of process.
     This may be a symbol: run, stop, exit, signal */
  MARKED_SLOT (status_symbol)
  /* Low level streams used in input and output, connected to child */
  MARKED_SLOT (pipe_instream)
  MARKED_SLOT (pipe_outstream)
  MARKED_SLOT (pipe_errstream)
  /* Data end streams, decoding and encoding pipe_* streams */
  MARKED_SLOT (coding_instream)
  MARKED_SLOT (coding_outstream)
  MARKED_SLOT (coding_errstream)

  /* Name of subprocess terminal.  Only needed for Unix but we put it
     here to avoid complications with KKCC, which needs to know about
     all of the Lisp objects, including in process-type-specific data. */
  MARKED_SLOT (tty_name)

#undef MARKED_SLOT
