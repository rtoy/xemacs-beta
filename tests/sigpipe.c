/* code is all from loser.c and loser.el by Mly

Copyright (C) 2002 Richard Mlynarik <mly@pobox.com>

This is part of XEmacs

Compile this file.  Run it in the background giving it a command line
argument PORT which is a positive integer 1024 < PORT < 32768 (avoid the
numbers assigned in /etc/services).

Then start up a fresh (you're going to crash) XEmacs.  Execute the following

(defun lose (port)
  (interactive "nUrk: ")
  (require 'comint)
  (while t
    (condition-case e
        (let* ((name "*lose*")
	       (b (get-buffer-create name)))
          (switch-to-buffer b)
          (comint-mode)
          (comint-exec b name (cons "127.0.0.1" port) nil '())
          (process-send-string (get-buffer-process b) "\377\373\001")
          (process-send-string (get-buffer-process b) "\377\373\001"))
      (error (message "URK: %s" e)) (sit-for 1))))

Then M-x lose RET PORT RET and you lose big (in XEmacs 21.1, anyway).
Note: the error messages are proper functioning.  What should eventually
happen after a number of SIGPIPEs is that you get a SIGSEGV and life is
bad and XEmacs is dead.
*/

#include <arpa/inet.h>

int
main (int argc, char **argv)
{
  struct sockaddr_in junk;
  int s;

  memset (&junk, 0, sizeof (junk));

  junk.sin_family = AF_INET;
  junk.sin_addr.s_addr = htonl (INADDR_ANY); /* un*x sucks */
  junk.sin_port = htons (atoi (argv[1])); /* un*x blows */

  s = socket (PF_INET, SOCK_STREAM, 0);

  bind (s, (struct sockaddr *)&junk, sizeof (junk));

  listen (s, 1);
  
  for (;;)
  {
    int loser = accept (s, NULL, 0);
    close (loser);
  }
}

