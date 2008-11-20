/* Virtual diry bit implementation for XEmacs.
   Copyright (C) 2005 Marcus Crestani.

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

/* Synched up with: Not in FSF. */

#include <config.h>
#include "lisp.h"
#include "gc.h"
#include "mc-alloc.h"
#include "vdb.h"

#include <errno.h>
#include <signal.h>
#include <sys/mman.h>
#include <sys/time.h>
#include <sys/resource.h>
#include <unistd.h>
#include <mach/mach.h>
#include <mach/mach_error.h>
#include <architecture/ppc/cframe.h>


/* the structure of an exception msg and its reply */
typedef struct rep_msg {
  mach_msg_header_t head;
  NDR_record_t NDR;
  kern_return_t ret_code;
} mach_reply_msg_t;

typedef struct exc_msg {
  mach_msg_header_t head;
  /* start of the kernel processed data */
  mach_msg_body_t msgh_body;
  mach_msg_port_descriptor_t thread;
  mach_msg_port_descriptor_t task;
  /* end of the kernel processed data */
  NDR_record_t NDR;
  exception_type_t exception;
  mach_msg_type_number_t code_cnt;
  exception_data_t code;
  /* some padding */
  char pad[512];
} mach_exc_msg_t;

/* this is a neat little mach callback */
extern boolean_t exc_server(mach_msg_header_t *in, mach_msg_header_t *out);

/* these are the globals everyone needs */
static size_t page_size = 16384;
static mach_port_t task_self = NULL;
static mach_port_t exc_port = NULL;

/* these are some less neat mach callbacks */
kern_return_t
catch_exception_raise_state 
(mach_port_t UNUSED (port),
 exception_type_t UNUSED (exception_type),
 exception_data_t UNUSED (exception_data),
 mach_msg_type_number_t UNUSED (data_cnt),
 thread_state_flavor_t *UNUSED (flavor),
 thread_state_t UNUSED (in_state),
 mach_msg_type_number_t UNUSED (is_cnt),
 thread_state_t UNUSED (out_state),
 mach_msg_type_number_t UNUSED (os_cnt))
{
  return KERN_FAILURE;
}

kern_return_t 
catch_exception_raise_state_identitity 
(mach_port_t UNUSED (port),
 mach_port_t UNUSED (thread_port),
 mach_port_t UNUSED (task_port),
 exception_type_t UNUSED (exception_type),
 exception_data_t UNUSED (exception_data),
 mach_msg_type_number_t UNUSED (data_count),
 thread_state_flavor_t *UNUSED (state_flavor),
 thread_state_t UNUSED (in_state),
 mach_msg_type_number_t UNUSED (in_state_count),
 thread_state_t UNUSED (out_state),
 mach_msg_type_number_t UNUSED (out_state_count))
{
  return KERN_FAILURE;
}

kern_return_t 
catch_exception_raise
(mach_port_t UNUSED (port),
 mach_port_t UNUSED (thread_port),
 mach_port_t UNUSED (task_port),
 exception_type_t UNUSED (exception_type),
 exception_data_t exception_data,
 mach_msg_type_number_t UNUSED (data_count))
{
  /* kernel return value is in exception_data[0], faulting address in
     exception_data[1] */
  if (write_barrier_enabled
      && (fault_on_protected_page ((void *) exception_data[1]))
      && exception_data[0] == KERN_PROTECTION_FAILURE)
    {
      vdb_designate_modified ((void *) exception_data[1]);
      unprotect_page_and_mark_dirty ((void *) exception_data[1]);
      return KERN_SUCCESS;
    }
  else  /* default sigsegv handler */
    {
      fprintf (stderr, "\n\nFatal Error: Received %s (%d) for address 0x%x\n",
	       "EXC_BAD_ACCESS", exception_data[0], (int) exception_data[1]);
      return KERN_FAILURE;
    }
}

/* this is the thread which forwards of exceptions read from the exception
   server off to our exception catchers and then back out to the other
   thread */
void 
exception_thread(void)
{
  mach_msg_header_t *message;
  mach_msg_header_t *reply;
  kern_return_t retval;
  
  /* allocate the space for the message and reply */
  message = (mach_msg_header_t *) malloc (sizeof (mach_exc_msg_t));
  reply = (mach_msg_header_t *) malloc (sizeof (mach_reply_msg_t));
  /* do this loop forever */
  while (1) 
    {
      /* block until we get an exception message */
      retval = mach_msg (message, MACH_RCV_MSG, 0, sizeof (mach_exc_msg_t), 
			 exc_port, MACH_MSG_TIMEOUT_NONE, MACH_PORT_NULL);
      /* forward off the handling of this message */
      if (!exc_server (message, reply)) 
	{
	  fprintf (stderr, "INTERNAL ERROR: exc_server() failed.\n");
	  ABORT ();
	}
      /* send the message back out to the thread */
      retval = mach_msg (reply, MACH_SEND_MSG, sizeof (mach_reply_msg_t), 0, 
			 MACH_PORT_NULL, MACH_MSG_TIMEOUT_NONE,
			 MACH_PORT_NULL);
    }
}

/* this initializes the subsystem (sets the exception port, starts the
   exception handling thread, etc) */
void 
vdb_install_signal_handler (void) 
{
  mach_port_t thread_self, exc_port_s, exc_thread;
  ppc_thread_state_t *exc_thread_state;
  mach_msg_type_name_t type;
  void *subthread_stack;
  kern_return_t retval;

  /* get ids for ourself */
  if (!task_self) 
    task_self = mach_task_self ();
  thread_self = mach_thread_self ();

  /* allocate the port we're going to get exceptions on */
  retval = mach_port_allocate (task_self, MACH_PORT_RIGHT_RECEIVE, &exc_port);
  if (retval != KERN_SUCCESS) 
    {
      fprintf (stderr, "Couldn't allocate exception port: %s\n", 
	       mach_error_string (retval));
      ABORT ();
    }

  /* extract out the send rights for that port, which the OS needs */
  retval = mach_port_extract_right (task_self, exc_port, 
				    MACH_MSG_TYPE_MAKE_SEND,
				    &exc_port_s, &type);
  if(retval != KERN_SUCCESS)
    {
      fprintf (stderr, "Couldn't extract send rights: %s\n",
	       mach_error_string (retval));
      ABORT ();
    }

  /* set the exception ports for this thread to the above */
  retval = thread_set_exception_ports(thread_self, EXC_MASK_BAD_ACCESS, 
				      exc_port_s, EXCEPTION_DEFAULT, 
				      PPC_THREAD_STATE);
  if(retval != KERN_SUCCESS)
    {
      fprintf (stderr, "Couldn't set exception ports: %s\n",
	       mach_error_string (retval));
      ABORT ();
    }

  /* set up the subthread */
  retval = thread_create(task_self, &exc_thread);
  if(retval != KERN_SUCCESS) 
    {
      fprintf (stderr , "Couldn't create exception thread: %s\n",
	       mach_error_string (retval));
      ABORT ();
    }
  subthread_stack = (void *) malloc (page_size);
  subthread_stack = 
    (char *) subthread_stack + (page_size - C_ARGSAVE_LEN - C_RED_ZONE);
  exc_thread_state = 
    (ppc_thread_state_t *) malloc (sizeof (ppc_thread_state_t));
  exc_thread_state->srr0 = (unsigned int) exception_thread;
  exc_thread_state->r1 = (unsigned int) subthread_stack;
  retval = thread_set_state (exc_thread, PPC_THREAD_STATE,
			     (thread_state_t) exc_thread_state,
			     PPC_THREAD_STATE_COUNT);
  if (retval != KERN_SUCCESS)
    {
      fprintf (stderr, "Couldn't set subthread state: %s\n",
	       mach_error_string (retval));
      ABORT ();
    }
  retval = thread_resume (exc_thread);
  if (retval != KERN_SUCCESS) 
    {
      fprintf (stderr, "Couldn't resume subthread: %s\n",
	       mach_error_string (retval));
      ABORT ();
    }
  allow_incremental_gc = 1;
}

void
vdb_protect (void *ptr, EMACS_INT len)
{
  if (mprotect (ptr, len, PROT_READ))
    {
      perror ("Couldn't mprotect");
      ABORT ();
    }
}

void
vdb_unprotect (void *ptr, EMACS_INT len)
{
  if (mprotect (ptr, len, PROT_READ | PROT_WRITE))
    {
      perror ("Couldn't mprotect");
      ABORT ();
    }
}
