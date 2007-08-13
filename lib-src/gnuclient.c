/* -*-C-*-
 Client code to allow local and remote editing of files by XEmacs.
 Copyright (C) 1989 Free Software Foundation, Inc.
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
Boston, MA 02111-1307, USA.

 Author: Andy Norman (ange@hplb.hpl.hp.com), based on 
         'etc/emacsclient.c' from the GNU Emacs 18.52 distribution.

 Please mail bugs and suggestions to the author at the above address.
*/

/*
 * This file incorporates new features added by Bob Weiner <weiner@mot.com>,
 * Darrell Kindred <dkindred@cmu.edu> and Arup Mukherjee <arup@cmu.edu>.
 * GNUATTACH support added by Ben Wing <wing@xemacs.org>.
 * Please see the note at the end of the README file for details.
 *
 * (If gnuserv came bundled with your emacs, the README file is probably
 * ../etc/gnuserv.README relative to the directory containing this file)
 */

#if 0
/* Hand-munged RCS header */
static char rcsid [] = "!Header: gnuclient.c,v 2.2 95/12/12 01:39:21 wing nene !";
#endif

#include "gnuserv.h"
#include "getopt.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/types.h>
#include <unistd.h>

#if !defined(SYSV_IPC) && !defined(UNIX_DOMAIN_SOCKETS) && \
    !defined(INTERNET_DOMAIN_SOCKETS)
int
main (int argc, char *argv[])
{
  fprintf (stderr, "Sorry, the Emacs server is only "
	   "supported on systems that have\n");
  fprintf (stderr, "Unix Domain sockets, Internet Domain "
	   "sockets or System V IPC.\n");
  exit (1);
} /* main */
#else /* SYSV_IPC || UNIX_DOMAIN_SOCKETS || INTERNET_DOMAIN_SOCKETS */

static char cwd[MAXPATHLEN+2];	/* current working directory when calculated */
static char *cp = NULL;		/* ptr into valid bit of cwd above */

#ifdef GNUATTACH
#include <signal.h>

static pid_t emacs_pid;			/* Process id for emacs process */

void tell_emacs_to_resume(int sig)
{
    char buffer[GSERV_BUFSZ+1];
    int s;					/* socket / msqid to server */
    int connect_type;           			/* CONN_UNIX, CONN_INTERNET, or
						 * CONN_IPC */

  /* Why is SYSV so retarded? */
  /* We want emacs to realize that we are resuming */
    signal(SIGCONT, tell_emacs_to_resume); 

    connect_type = make_connection (NULL, (u_short) 0, &s);

    sprintf(buffer,"(server-eval '(resume-pid-console %d))", getpid());
    send_string(s, buffer);
  
#ifdef SYSV_IPC
  if (connect_type == (int) CONN_IPC)
    disconnect_from_ipc_server (s, msgp, FALSE);
#else /* !SYSV_IPC */
  if (connect_type != (int) CONN_IPC)
    disconnect_from_server (s, FALSE);
#endif /* !SYSV_IPC */
}

void pass_signal_to_emacs(int sig)
{
  if (kill(emacs_pid, sig) == -1) {
    fprintf(stderr, "gnuattach: Could not pass signal to emacs process\n");
    exit(1);
  }
}

void initialize_signals()
{
  /* Set up signal handler to pass relevant signals to emacs process */
  signal(SIGHUP, pass_signal_to_emacs);
  signal(SIGQUIT, pass_signal_to_emacs);
  signal(SIGILL, pass_signal_to_emacs);
  signal(SIGTRAP, pass_signal_to_emacs);
  signal(SIGSEGV, pass_signal_to_emacs);
  signal(SIGPIPE, pass_signal_to_emacs);
  signal(SIGTERM, pass_signal_to_emacs);
#ifdef SIGBUS
  signal(SIGBUS, pass_signal_to_emacs);
#endif
#ifdef SIGIOT
  signal(SIGIOT, pass_signal_to_emacs);
#endif

  /* We want emacs to realize that we are resuming */
  signal(SIGCONT, tell_emacs_to_resume);
}

#endif /* GNUATTACH */


/*
  get_current_working_directory -- return the cwd.
*/
static char *
get_current_working_directory (void)
{
  if (cp == NULL)
    {				/* haven't calculated it yet */
#ifdef BSD
      if (getwd (cwd) == 0)
#else /* !BSD */
      if (getcwd (cwd,MAXPATHLEN) == NULL)
#endif /* !BSD */
	{
	  perror (progname);
	  fprintf (stderr, "%s: unable to get current working directory\n",
		   progname);
	  exit (1);
	} /* if */
    
      /* on some systems, cwd can look like '@machine/' ... */
      /* ignore everything before the first '/' */
      for (cp = cwd; *cp && *cp != '/'; ++cp)
	;

    } /* if */

  return cp;

} /* get_current_working_directory */


/*
  filename_expand -- try to convert the given filename into a fully-qualified
  		     pathname.
*/
static void
filename_expand (char *fullpath, char *filename)
  /* fullpath - returned full pathname */
  /* filename - filename to expand */
{
  int len;

  fullpath[0] = '\0';
  
  if (filename[0] && filename[0] != '/')
    {	/* relative filename */
      strcat (fullpath, get_current_working_directory ());
      len = strlen (fullpath);

      if (len > 0 && fullpath[len-1] == '/')	/* trailing slash already? */
	;					/* yep */
      else
	strcat (fullpath, "/");		/* nope, append trailing slash */
    } /* if */

  strcat (fullpath,filename);

} /* filename_expand */

int
main (int argc, char *argv[])
{
  int starting_line = 1;			/* line to start editing at */
  char command[MAXPATHLEN+50];		/* emacs command buffer */
  char fullpath[MAXPATHLEN+1];			/* full pathname to file */
#ifndef GNUATTACH
  int qflg = 0;					/* quick edit, don't wait for 
						 * user to finish */
#endif
  int errflg = 0;				/* option error */
  int c;					/* char from getopt */
  int s;					/* socket / msqid to server */
  int connect_type;           			/* CONN_UNIX, CONN_INTERNET, or
						 * CONN_IPC */
#ifdef INTERNET_DOMAIN_SOCKETS
  char *hostarg = NULL;				/* remote hostname */
  char thishost[HOSTNAMSZ];			/* this hostname */
  char remotepath[MAXPATHLEN+1];		/* remote pathname */
  int rflg = 0;					/* pathname given on cmdline */
  u_short portarg = 0;				/* port to server */
  char *ptr;					/* return from getenv */
#endif /* INTERNET_DOMAIN_SOCKETS */
#ifdef SYSV_IPC
  struct msgbuf *msgp;				/* message */
#endif /* SYSV_IPC */
#ifdef GNUATTACH
  char *tty;
  char buffer[GSERV_BUFSZ+1];		/* buffer to read pid */
#endif

#ifdef INTERNET_DOMAIN_SOCKETS
  memset (remotepath, 0, sizeof (remotepath));
#endif /* INTERNET_DOMAIN_SOCKETS */

  progname = argv[0];

  while ((c = getopt (argc, argv,

#ifdef INTERNET_DOMAIN_SOCKETS
		      "h:p:r:q"
#else /* !INTERNET_DOMAIN_SOCKETS */
# ifdef GNUATTACH
		      ""
# else
		      "q"
# endif
#endif /* !INTERNET_DOMAIN_SOCKETS */

		      )) != EOF)
    switch (c)
      {
#ifndef GNUATTACH
      case 'q':					/* quick mode specified */
	qflg++;
	break;
#endif

#ifdef INTERNET_DOMAIN_SOCKETS
      case 'h':				/* server host name specified */
	hostarg = optarg;
	break;
      case 'r':				/* remote path from server specifed */
	strcpy (remotepath,optarg);
	rflg++;
	break;
      case 'p':				/* port number specified */
	portarg = atoi (optarg);
	break;
#endif /* INTERNET_DOMAIN_SOCKETS */

      case '?':
	errflg++;
      } /* switch */

  if (errflg)
    {
      fprintf (stderr,
#ifdef INTERNET_DOMAIN_SOCKETS
	       "usage: %s [-q] [-h hostname] [-p port] [-r pathname] "
	       "[[+line] path] ...\n",
#else /* !INTERNET_DOMAIN_SOCKETS */
# ifdef GNUATTACH
	       "usage: %s [[+line] path] ...\n",
# else
	       "usage: %s [-q] [[+line] path] ...\n",
# endif
#endif /* !INTERNET_DOMAIN_SOCKETS */
	       progname);
      exit (1);
    } /* if */

#ifdef GNUATTACH
  tty = ttyname (0);
  if (!tty)
    {
      fprintf (stderr, "%s: Not connected to a tty", progname);
      exit (1);
    }

  /* This next stuff added in an attempt to make handling of
     the tty do the right thing when dealing with signals.
     Idea is to pass all the appropriate signals to the emacs process
     */

  connect_type = make_connection (NULL, (u_short) 0, &s);

  send_string(s,"(server-eval '(emacs-pid))");
  send_string(s,EOT_STR);
  
  if (read_line(s,buffer) == 0) {
    fprintf(stderr, "%s: Could not establish emacs procces id\n",progname);
    exit(1);
  }
  /* don't do disconnect_from_server becasue we have already read data,
     and disconnect doesn't do anything else
   */
#ifdef SYSV_IPC
  if (connect_type == (int) CONN_IPC)
    disconnect_from_ipc_server (s, msgp, FALSE);
#endif /* !SYSV_IPC */

  emacs_pid = (pid_t)atol(buffer);
  initialize_signals();
      
#endif /*GNUATTACH */ 

#if defined(INTERNET_DOMAIN_SOCKETS) && !defined(GNUATTACH)
  connect_type = make_connection (hostarg, portarg, &s);
#else
  connect_type = make_connection (NULL, (u_short) 0, &s);
#endif

#ifdef INTERNET_DOMAIN_SOCKETS
  if (connect_type == (int) CONN_INTERNET)
    {
      gethostname (thishost, HOSTNAMSZ);
      if (!rflg)
	{				/* attempt to generate a path 
					 * to this machine */
	  if ((ptr = getenv ("GNU_NODE")) != NULL)
	    /* user specified a path */
	    strcpy (remotepath, ptr);
	}
#if 0  /* This is really bogus... re-enable it if you must have it! */
#if defined (hp9000s300) || defined (hp9000s800)
      else if (strcmp (thishost,hostarg))
	{	/* try /net/thishost */
	  strcpy (remotepath, "/net/");		/* (this fails using internet 
						   addresses) */
	  strcat (remotepath, thishost);
	}
#endif
#endif
    }
  else
    {					/* same machines, no need for path */
      remotepath[0] = '\0';		/* default is the empty path */
    }
#endif /* INTERNET_DOMAIN_SOCKETS */

#ifdef SYSV_IPC
  if ((msgp = (struct msgbuf *) 
       malloc (sizeof *msgp + GSERV_BUFSZ)) == NULL)
    {
      fprintf (stderr, "%s: not enough memory for message buffer\n", progname);
      exit (1);
    } /* if */

  msgp->mtext[0] = '\0';			/* ready for later strcats */
#endif /* SYSV_IPC */

#ifdef GNUATTACH
  ptr = getenv ("TERM");
  if (!ptr)
    {
      fprintf (stderr, "%s: unknown terminal type\n", progname);
      exit (1);
    }
  sprintf (command, "(server-tty-edit-files \"%s\" \"%s\" %d '(", 
	   tty, ptr, getpid());
  send_string (s, command);
#else
  if (qflg)
    {
      send_string (s, "(server-edit-files-quickly '(");
    }
  else
    {
      send_string (s, "(server-edit-files '(");
    }
#endif

  for (; optind < argc; optind++)
    {
      if (*argv[optind] == '+')
	starting_line = atoi (argv[optind]);
      else
	{
	  filename_expand (fullpath, argv[optind]);
	  sprintf (command, "(%d . \"%s%s\")", starting_line,

#ifdef INTERNET_DOMAIN_SOCKETS
		   remotepath,
#else /* !INTERNET_DOMAIN_SOCKETS */
		   "",
#endif
		   fullpath);
	  send_string (s,command);
	  starting_line = 1;
	} /* else */
    } /* for */

  send_string (s,"))");

#ifdef SYSV_IPC
  if (connect_type == (int) CONN_IPC)
    disconnect_from_ipc_server (s, msgp, FALSE);
#else /* !SYSV_IPC */
  if (connect_type != (int) CONN_IPC)
    disconnect_from_server (s, FALSE);
#endif /* !SYSV_IPC */

  return 0;

} /* main */

#endif /* SYSV_IPC || UNIX_DOMAIN_SOCKETS || INTERNET_DOMAIN_SOCKETS */
