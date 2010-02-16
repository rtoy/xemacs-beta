/* DDE client for XEmacs.
   Copyright (C) 2002 Alastair J. Houghton

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

/* -- Includes -------------------------------------------------------------- */

#ifdef HAVE_CONFIG_H
# include <config.h>
#endif
#include <windows.h>
#include <ddeml.h>
#include <stdlib.h>
#include <stdio.h>
#include <ctype.h>
#include <errno.h>

#ifdef __CYGWIN__
#include <stdlib.h>
#include <unistd.h>
#endif

static void error (const char* s1, const char* s2);
static void fatal (const char *s1, const char *s2);
static void * xmalloc (size_t size);
static char * getNextArg (const char **ptr, unsigned *len);

/* -- Post-Include Defines -------------------------------------------------- */

/* Timeouts & delays */
#define CONNECT_RETRIES		20
#define CONNECT_DELAY		500		/* ms */
#define TRANSACTION_TIMEOUT	5000		/* ms */
#define MAX_INPUT_IDLE_WAIT     INFINITE	/* ms */

/* DDE Strings */
#define SERVICE_NAME	"XEmacs"
#define TOPIC_NAME	"System"
#define COMMAND_FORMAT	"[open(\"%s%s\")]"

/* XEmacs program name */
#define GENERIC_PROGRAM		EMACS_PROGNAME ".exe"
#define VERSIONED_PROGRAM	EMACS_PROGNAME "-" EMACS_VERSION ".exe"

/* -- Constants ------------------------------------------------------------- */

/* -- Global Variables ------------------------------------------------------ */

HINSTANCE hInstance;
DWORD     idInst = 0;

/* -- Function Declarations ------------------------------------------------- */

HDDEDATA CALLBACK ddeCallback (UINT uType, UINT uFmt, HCONV hconv,
			       HSZ hsz1, HSZ hsz2, HDDEDATA hdata,
			       DWORD dwData1, DWORD dwData2);

int WINAPI WinMain (HINSTANCE hInst,
		    HINSTANCE hPrev,
		    LPSTR     lpCmdLine,
		    int       nCmdShow);

static HCONV openConversation (void);
static void closeConversation (HCONV hConv);
static int doFile (HCONV hConv, LPSTR lpszFileName1, LPSTR lpszFileName2);
static int parseCommandLine (HCONV hConv, LPSTR lpszCommandLine);

/* -- Function Definitions -------------------------------------------------- */

/*
 * Name    : ddeCallback
 * Function: Gets called by DDEML.
 *
 */

HDDEDATA CALLBACK
ddeCallback (UINT uType, UINT uFmt, HCONV hconv,
	     HSZ hsz1, HSZ hsz2, HDDEDATA hdata,
	     DWORD dwData1, DWORD dwData2)
{
  return (HDDEDATA) NULL;
}

/*
 * Name    : WinMain
 * Function: The program's entry point function.
 *
 */

int WINAPI
WinMain (HINSTANCE hInst,
	 HINSTANCE hPrev,
	 LPSTR     lpCmdLine,
	 int       nCmdShow)
{
  HCONV hConv;
  int   ret = 0;
  UINT  uiRet;

  /* Initialise the DDEML library */
  uiRet = DdeInitialize (&idInst,
			 (PFNCALLBACK) ddeCallback,
			 APPCMD_CLIENTONLY
			 |CBF_FAIL_ALLSVRXACTIONS,
			 0);

  if (uiRet != DMLERR_NO_ERROR)
    {
      MessageBox (NULL, "Could not initialise DDE management library.",
		  "winclient", MB_ICONEXCLAMATION | MB_OK);

      return 1;
    }

  /* Open a conversation */
  hConv = openConversation ();

  if (hConv)
    {
      /* OK. Next, we need to parse the command line. */
      ret = parseCommandLine (hConv, lpCmdLine);

      /* Close the conversation */
      closeConversation (hConv);
    }

  DdeUninitialize (idInst);

  return ret;
}

/*
 * Name    : openConversation
 * Function: Start a conversation.
 *
 */

static HCONV
openConversation (void)
{
  HSZ             hszService = NULL, hszTopic = NULL;
  HCONV           hConv = NULL;

  /* Get the application (service) name */
  hszService = DdeCreateStringHandle (idInst,
				      SERVICE_NAME,
				      CP_WINANSI);

  if (!hszService)
    {
      MessageBox (NULL, "Could not create string handle for service.",
		  "winclient", MB_ICONEXCLAMATION | MB_OK);

      goto error;
    }

  /* Get the topic name */
  hszTopic = DdeCreateStringHandle (idInst,
				    TOPIC_NAME,
				    CP_WINANSI);

  if (!hszTopic)
    {
      MessageBox (NULL, "Could not create string handle for topic.",
		  "winclient", MB_ICONEXCLAMATION | MB_OK);

      goto error;
    }

  /* Try to connect */
  hConv = DdeConnect (idInst, hszService, hszTopic, NULL);

  if (!hConv)
    {
      STARTUPINFO         sti;
      PROCESS_INFORMATION pi;
      int                 n;

      /* Try to start the program */
      ZeroMemory (&sti, sizeof (sti));
      sti.cb = sizeof (sti);
      if (!CreateProcess (NULL, GENERIC_PROGRAM, NULL, NULL, FALSE, 0,
			  NULL, NULL, &sti, &pi) &&
	  !CreateProcess (NULL, VERSIONED_PROGRAM, NULL, NULL, FALSE, 0,
			  NULL, NULL, &sti, &pi))
	{
	  MessageBox (NULL, "Could not start process.",
		      "winclient", MB_ICONEXCLAMATION | MB_OK);

	  goto error;
	}

      /* Wait for the process to enter an idle state */
      WaitForInputIdle (pi.hProcess, MAX_INPUT_IDLE_WAIT);

      /* Close the handles */
      CloseHandle (pi.hThread);
      CloseHandle (pi.hProcess);

      /* Try to connect */
      for (n = 0; n < CONNECT_RETRIES; n++)
	{
	  Sleep (CONNECT_DELAY);
	
	  hConv = DdeConnect (idInst, hszService, hszTopic, NULL);

	  if (hConv)
	    break;
	}

      if (!hConv)
	{
	  /* Still couldn't connect. */
	  MessageBox (NULL, "Could not connect to DDE server.",
		      "winclient", MB_ICONEXCLAMATION | MB_OK);

	  goto error;
	}
    }

  /* Release the string handles */
  DdeFreeStringHandle (idInst, hszService);
  DdeFreeStringHandle (idInst, hszTopic);

  return hConv;

 error:
  if (hConv)
    DdeDisconnect (hConv);
  if (hszService)
    DdeFreeStringHandle (idInst, hszService);
  if (hszTopic)
    DdeFreeStringHandle (idInst, hszTopic);

  return NULL;
}

/*
 * Name    : closeConversation
 * Function: Close a conversation.
 *
 */

static void
closeConversation (HCONV hConv)
{
  /* Shut down */
  DdeDisconnect (hConv);
}

/*
 * Name    : doFile
 * Function: Process a file.
 *
 */

int
doFile (HCONV hConv, LPSTR lpszFileName1, LPSTR lpszFileName2)
{
  char            *buf = NULL;
  unsigned        len;

  /* Calculate the buffer length */
  len = strlen (lpszFileName1) + strlen (lpszFileName2)
    + strlen (COMMAND_FORMAT);

  /* Allocate a buffer */
  buf = (char *) xmalloc (len);

  if (!buf)
    {
      MessageBox (NULL, "Not enough memory.",
		  "winclient", MB_ICONEXCLAMATION | MB_OK);

      return 1;
    }

  /* Build the command */
  len = wsprintf (buf, COMMAND_FORMAT, lpszFileName1, lpszFileName2);
  len++;

  /* OK. We're connected. Send the message. */
  DdeClientTransaction (buf, len, hConv, NULL,
			0, XTYP_EXECUTE, TRANSACTION_TIMEOUT, NULL);

  free (buf);

  return 0;
}

/*
 * Name    : getNextArg
 * Function: Retrieve the next command line argument.
 *
 */

static char *
getNextArg (const char **ptr, unsigned *len)
{
  int        in_quotes = 0, quit = 0, all_in_quotes = 0;
  const char *p = *ptr, *start;
  char       *buf = NULL;
  unsigned   length = 0;

  /* Skip whitespace */
  while (*p && isspace (*p))
    p++;

  /* If this is the end, return NULL */
  if (!*p)
    return NULL;

  /* Remember where we are */
  start = p;

  /* Find the next whitespace character outside quotes */
  if (*p == '"')
    all_in_quotes = 1;

  while (*p && !quit)
    {
      switch (*p)
	{
	case '"':
	  in_quotes = 1 - in_quotes;
	  p++;
	  break;

	case '\\':
	  if (!in_quotes)
	    all_in_quotes = 0;
	
	  p++;

	  if (!*p)
	    break;

	  p++;
	  break;

	default:
	  if (isspace (*p) && !in_quotes)
	    quit = 1;
	  else if (!in_quotes)
	    all_in_quotes = 0;

	  if (!quit)
	    p++;
	}
    }

  /* Work out the length */
  length = p - start;

  /* Strip quotes if the argument is completely quoted */
  if (all_in_quotes)
    {
      start++;
      length -= 2;
    }

  /* Copy */
  buf = (char *) xmalloc (length + 1);

  if (!buf)
    return NULL;

  strncpy (buf, start, length);
  buf[length] = '\0';

  /* Return the pointer and length */
  *ptr = p;
  *len = length;

  return buf;
}

/*
 * Name    : parseCommandLine
 * Function: Process the command line. This program accepts a list of strings
 *         : (which may contain wildcards) representing filenames.
 *
 */

int
parseCommandLine (HCONV hConv, LPSTR lpszCommandLine)
{
  char            *fullpath, *filepart;
  char            *arg;
  unsigned        len, pathlen;
  int             ret = 0;
  HANDLE          hFindFile = NULL;
  WIN32_FIND_DATA wfd;

  /* Retrieve arguments */
  while ((arg = getNextArg ((const char**)&lpszCommandLine, &len)) != NULL)
    {
      fullpath = NULL;
#ifdef __CYGWIN__
      /* If the filename is not an absolute path,
	 add the current directory to the pathname */
      if (*arg != '/')
	{
	  len = pathconf(".", _PC_PATH_MAX);
	  fullpath = (char *) xmalloc (len+1);
	  if (!fullpath)
	    {
		MessageBox (NULL, "Not enough memory.", "winclient",
			    MB_ICONEXCLAMATION | MB_OK);
		ret = 1;
		break;
	    }
	  if (!getcwd(fullpath, (size_t)len))
	    {
	      MessageBox (NULL, "Could not retrieve current directory.",
			  "winclient", MB_ICONEXCLAMATION | MB_OK);
	      ret = 1;
	      break;
	    }
          /* Append trailing slash */
	  strcat(fullpath, "/");
	  ret = doFile (hConv, fullpath, arg);
	}
      else
	{
	  /* The arg has already been expanded, so pass it as it is */
	  ret = doFile (hConv, "", arg);
	}
#else
      /* First find the canonical path name */
      fullpath = filepart = NULL;
      pathlen = GetFullPathName (arg, 0, fullpath, &filepart);

      fullpath = (char *) xmalloc (pathlen);

      if (!fullpath)
	{
	  MessageBox (NULL, "Not enough memory.", "winclient",
		      MB_ICONEXCLAMATION | MB_OK);
	  ret = 1;
	  break;
	}

      GetFullPathName (arg, pathlen, fullpath, &filepart);

      /* Find the first matching file */
      hFindFile = FindFirstFile (arg, &wfd);

      if (hFindFile == INVALID_HANDLE_VALUE)
	ret = doFile (hConv, fullpath, "");
      else
	{
	  /* Chop off the file part from the full path name */
	  if (filepart)
	    *filepart = '\0';

	  /* For each matching file */
	  do
	    {
	      /* Process it */
	      ret = doFile (hConv, fullpath, wfd.cFileName);

	      if (ret)
		break;
	    }
	  while (FindNextFile (hFindFile, &wfd));

	  FindClose (hFindFile);
	}
#endif
      /* Release the path name buffers */
      if (fullpath)
	free (fullpath);
      free (arg);

      if (ret)
	break;
    }

  return ret;
}

static void
fatal (const char *s1, const char *s2)
{
  error (s1, s2);
  exit (1);
}

/* Print error message.  `s1' is printf control string, `s2' is arg for it. */
static void
error (const char* s1, const char* s2)
{
  fprintf (stderr, "winclient: ");
  fprintf (stderr, s1, s2);
  fprintf (stderr, "\n");
}

/* Like malloc but get fatal error if memory is exhausted.  */

static void *
xmalloc (size_t size)
{
  void *result = malloc (size);
  if (result == NULL)
    fatal ("virtual memory exhausted", (char *) 0);
  return result;
}
