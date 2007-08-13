;; -*-Emacs-Lisp-*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; File:         efs.el
;; Release:      $efs release: 1.15 $
;; Version:      #Revision: 1.56 $
;; RCS:          
;; Description:  Transparent FTP support for the original GNU Emacs
;;               from FSF and Lucid Emacs
;; Authors:      Andy Norman <ange@hplb.hpl.hp.com>,
;;               Sandy Rutherford <sandy@ibm550.sissa.it>
;; Created:      Thu Oct 12 14:00:05 1989 (as ange-ftp)
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; The following restrictions apply to all of the files in the efs
;;; distribution.
;;; 
;;; Copyright (C) 1993  Andy Norman / Sandy Rutherford
;;;
;;; Authors:
;;;          Andy Norman (ange@hplb.hpl.hp.com)
;;;          Sandy Rutherford (sandy@ibm550.sissa.it)
;;;          
;;;          The authors of some of the sub-files of efs are different
;;;          from the above.  We are very grateful to people who have
;;;          contributed code to efs.
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 1, or (at your option)
;;; any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; A copy of the GNU General Public License can be obtained from this
;;; program's authors (send electronic mail to ange@hplb.hpl.hp.com) or
;;; from the Free Software Foundation, Inc., 675 Mass Ave, Cambridge,
;;; MA 02139, USA.

;;; Description:
;;;
;;; This package attempts to make accessing files and directories on
;;; remote computers from within GNU Emacs as simple and transparent
;;; as possible.  Currently all remote files are accessed using FTP.
;;; The goal is to make the entire internet accessible as a virtual
;;; file system.

;;; Acknowledgements: << please add to this list >>
;;;
;;; Corny de Souza for writing efs-mpe.el.
;;; Jamie Zawinski for writing efs-ti-twenex.el and efs-ti-explorer.el
;;; Joe Wells for writing the first pass at vms support for ange-ftp.el.
;;; Sebastian Kremer for helping with dired support.
;;; Ishikawa Ichiro for MULE support.
;;; 
;;; Many other people have contributed code, advice, and beta testing
;;; (sometimes without even realizing it) to both ange-ftp and efs:
;;;
;;; Rob Austein, Doug Bagley, Andy Caiger, Jim Franklin, Noah
;;; Friedman, Aksnes Knut-Havard, Elmar Heeb, John Interrante, Roland
;;; McGrath, Jeff Morgenthaler, Mike Northam, Jens Petersen, Jack
;;; Repenning, Joerg-Martin Schwarz, Michael Sperber, Svein Tjemsland,
;;; Andy Whitcroft, Raymond A. Wiker
;;;
;;; Also, thank you to all the people on the efs-testers mailing list.
;;; 

;;; --------------------------------------------------------------
;;; Documentation:
;;; --------------------------------------------------------------
;;;
;;; If you have any problems with efs, please read this section
;;; *before* submitting a bug report.

;;; Installation:
;;;
;;; For byte compiling the efs package, a Makefile is provided.
;;; You should follow the instructions at the top of the Makefile.
;;; If you have any problems, please let us know so that we can fix
;;; them for other users. Don't even consider using efs without
;;; byte compiling it. It will be far too slow.
;;;
;;; If you decide to byte compile efs by hand, it is important that
;;; the file efs-defun.el be byte compiled first, followed by efs.el.
;;; The other files may be byte compiled in any order.
;;;
;;; To use efs, simply put the byte compiled files in your load path
;;; and add
;;;
;;;        (require 'efs)
;;;
;;; in your .emacs file.  Note this takes awhile, and some users have
;;; found this to  be unbearably slow.  Therefore ...
;;;
;;; If you would like efs to be autoloaded when you attempt to access
;;; a remote file, put
;;;
;;;        (require 'efs-auto)
;;;
;;; in your .emacs file. Note that there are some limitations associated
;;; with autoloading efs. A discussion of them is given at the top of
;;; efs-auto.el.

;;; Configuration variables:
;;;
;;; It is important that you read through the section on user customization
;;; variables (search forward for the string ">>>"). If your local network
;;; is not fully connected to the internet, but accesses the internet only
;;; via a gateway, then it is vital to set the appropriate variables to
;;; inform efs about the geometry of your local network. Also, see the
;;; paragraph on gateways below.

;;; Usage:
;;;
;;; Once installed, efs operates largely transparently. All files
;;; normally accessible to you on the internet, become part of a large
;;; virtual file system. These files are accessed using an extended
;;; file name syntax. To access file <path> on remote host <host> by
;;; logging in as user <user>, you simply specify the full path of the
;;; file as /<user>@<host>:<path>. Nearly all GNU Emacs file handling
;;; functions work for remote files. It is not possible to access
;;; remote files using shell commands in an emacs *shell* buffer, as such
;;; commands are passed directly to the shell, and not handled by emacs.
;;; FTP is the underlying utility that efs uses to operate on remote files.
;;;
;;; For example, if find-file is given a filename of:
;;;
;;;   /ange@anorman:/tmp/notes
;;;
;;; then efs will spawn an FTP process, connect to the host 'anorman' as
;;; user 'ange', get the file '/tmp/notes' and pop up a buffer containing the
;;; contents of that file as if it were on the local file system.  If efs
;;; needed a password to connect then it would prompt the user in the
;;; minibuffer. For further discussion of the efs path syntax, see the
;;; paragraph on extended file name syntax below.

;;; Ports:
;;;
;;; efs supports the use of nonstandard ports on remote hosts.
;;; To specify that port <port> should be used, give the host name as
;;; host#<port>. Host names may be given in this form anywhere that efs
;;; normally expects a host name. This includes in the .netrc file.
;;; Logically, efs treats different ports to correspond to different
;;; remote hosts.

;;; Extended filename syntax:
;;;
;;; The default full efs path syntax is
;;;
;;;            /<user>@<host>#<port>:<path>
;;;
;;; Both the `#<port>' and `<user>@' may be omitted.
;;;
;;; If the `#<port>' is omitted, then the default port is taken to be 21,
;;; the usual FTP port. For most users, the port syntax will only
;;; very rarely be necessary.
;;;
;;; If the `<user>@' is omitted, then efs will use a default user.  If a
;;; login token is specified in your .netrc file, then this will be used as
;;; the default user for <host>.  Otherwise, it is determined based on the
;;; value of the variable efs-default-user.
;;; 
;;; This efs path syntax can be customised to a certain extent by
;;; changing a number of variables in the subsection Internal Variables.
;;; To undertake such a customization requires some knowledge about the
;;; internal workings of efs.

;;; Passwords:
;;;
;;; A password is required for each host / user pair.  This will be
;;; prompted for when needed, unless already set by calling
;;; efs-set-passwd, or specified in a *valid* ~/.netrc file.
;;;
;;; When efs prompts for a password, it provides defaults from its
;;; cache of currently known passwords.  The defaults are ordered such
;;; that passwords for accounts which have the same user name as the
;;; login which is currently underway have priority. You can cycle
;;; through your list of defaults with C-n to cycle forwards and C-p
;;; to cycle backwards. The list is circular.

;;; Passwords for user "anonymous":
;;;
;;; Passwords for the user "anonymous" (or "ftp") are handled
;;; specially.  The variable efs-generate-anonymous-password controls
;;; what happens. If the value of this variable is a string, then this
;;; is used as the password; if non-nil, then a password is created
;;; from the name of the user and the hostname of the machine on which
;;; GNU Emacs is running; if nil (the default) then the user is
;;; prompted for a password as normal.

;;; "Dumb" UNIX hosts:
;;;
;;; The FTP servers on some UNIX machines have problems if the "ls"
;;; command is used.  efs will try to correct for this automatically,
;;; and send the "dir" command instead.  If it fails, you can call the
;;; function efs-add-host, and give the host type as dumb-unix.  Note
;;; that this change will take effect for the current GNU Emacs
;;; session only. To make this specification for future emacs
;;; sessions, put
;;;
;;; (efs-add-host 'dumb-unix "hostname")
;;;
;;; in your .emacs file. Also, please report any failure to automatically
;;; recognize dumb unix to the "bugs" address given below, so that we can
;;; fix the auto recognition code.

;;; File name completion:
;;;
;;; Full file-name completion is supported on every type of remote
;;; host.  To do filename completion, efs needs a listing from the
;;; remote host.  Therefore, for very slow connections, it might not
;;; save any time. However, the listing is cached, so subsequent uses
;;; of file-name completion will be just as fast as for local file
;;; names.

;;; FTP processes:
;;;
;;; When efs starts up an FTP process, it leaves it running for speed
;;; purposes.  Some FTP servers will close the connection after a period of
;;; time, but efs should be able to quietly reconnect the next time that
;;; the process is needed.
;;;
;;; The FTP process will be killed should the associated "*ftp user@host*"
;;; buffer be deleted.  This should not cause efs any grief.

;;; Showing background FTP activity on the mode-line:
;;; 
;;; After efs is loaded, the command efs-display-ftp-activity will cause
;;; background FTP activity to be displayed on the mode line. The variable
;;; efs-mode-line-format is used to determine how this data is displayed.
;;; efs does not continuously track the number of active sessions, as this
;;; would cause the display to change too rapidly. Rather, it uses a heuristic
;;; algorithm to determine when there is a significant change in FTP activity.

;;; File types:
;;;
;;; By default efs will assume that all files are ASCII. If a file
;;; being transferred matches the value of efs-binary-file-name-regexp
;;; then the file will be assumed to be a binary file, and efs will
;;; transfer it using "type image". ASCII files will be transferred
;;; using a transfer type which efs computes to be correct according
;;; to its knowledge of the file system of the remote host. The
;;; command `efs-prompt-for-transfer-type' toggles the variable
;;; `efs-prompt-for-transfer-type'. When this variable is non-nil, efs
;;; will prompt the user for the transfer type to use for every FTP
;;; transfer.  Having this set all the time is annoying, but it is
;;; useful to give special treatment to a small set of files.
;;; There is also variable efs-text-file-name-regexp.  This is tested before
;;; efs-binary-file-name-regexp, so if you set efs-text-file-name-regexp
;;; to a non-trivial regular expression, and efs-binary-file-name-regexp
;;; to ".*", the result will to make image the default tranfer type.
;;;
;;; Also, if you set efs-treat-crlf-as-nl, then efs will use type image
;;; to transfer files between hosts whose file system differ only in that
;;; one specifies end of line as CR-LF, and the other as NL.  This is useful
;;; if you are transferring files between UNIX and DOS machines, and have a
;;; package such as dos-mode.el, that handles the extra ^M's.

;;; Account passwords:
;;;
;;; Some FTP servers require an additional password which is sent by
;;; the ACCOUNT command.  efs will detect this and prompt the user for
;;; an account password if the server expects one.  Also, an account
;;; password can be set by calling efs-set-account, or by specifying
;;; an account token in the .netrc file.
;;;
;;; Some operating systems, such as CMS, require that ACCOUNT be used to
;;; give a write access password for minidisks. efs-set-account can be used
;;; to set a write password for a specific minidisk. Also, tokens of the form
;;;     minidisk <minidisk name> <password>
;;; may be added to host lines in your .netrc file. Minidisk tokens must be
;;; at the end of the host line, however there may be an arbitrary number of
;;; them for any given host.

;;; Preloading:
;;;
;;; efs can be preloaded, but must be put in the site-init.el file and
;;; not the site-load.el file in order for the documentation strings for the
;;; functions being overloaded to be available.

;;; Status reports:
;;;
;;; Most efs commands that talk to the FTP process output a status
;;; message on what they are doing.  In addition, efs can take advantage
;;; of the FTP client's HASH command to display the status of transferring
;;; files and listing directories.  See the documentation for the variables
;;; efs-hash-mark-size, efs-send-hash and efs-verbose for more details.

;;; Caching of directory information:
;;; 
;;; efs keeps an internal cache of file listings from remote hosts.
;;; If this cache gets out of synch, it can be renewed by reverting a
;;; dired buffer for the appropriate directory (dired-revert is usually
;;; bound to "g").
;;;
;;; Alternatively, you can add the following two lines to your .emacs file
;;; if you want C-r to refresh efs's cache whilst doing filename
;;; completion.
;;; (define-key minibuffer-local-completion-map "\C-r" 'efs-re-read-dir)
;;; (define-key minibuffer-local-must-match-map "\C-r" 'efs-re-read-dir)

;;; Gateways:
;;;
;;; Sometimes it is necessary for the FTP process to be run on a different
;;; machine than the machine running GNU Emacs.  This can happen when the
;;; local machine has restrictions on what hosts it can access.
;;;
;;; efs has support for running the ftp process on a different (gateway)
;;; machine.  The way it works is as follows:
;;;
;;;  1) Set the variable 'efs-gateway-host' to the name of a machine
;;;     that doesn't have the access restrictions.  If you need to use
;;;     a nonstandard port to access this host for gateway use, then
;;;     specify efs-gateway-host as "<hostname>#<port>".
;;;
;;;  2) Set the variable 'efs-ftp-local-host-regexp' to a regular expression
;;;     that matches hosts that can be contacted from running a local ftp
;;;     process, but fails to match hosts that can't be accessed locally.  For
;;;     example:
;;;
;;;     "\\.hp\\.com$\\|^[^.]*$"
;;;
;;;     will match all hosts that are in the .hp.com domain, or don't have an
;;;     explicit domain in their name, but will fail to match hosts with
;;;     explicit domains or that are specified by their ip address.
;;;
;;;  3) Set the variable `efs-local-host-regexp' to machines that you have
;;;     direct TCP/IP access.  In other words, you must be able to ping these
;;;     hosts.  Usually, efs-ftp-local-host-regexp and efs-local-host-regexp
;;;     will be the same.  However, they will differ for so-called transparent
;;;     gateways.  See #7 below for more details.
;;;
;;;  4) Set the variable 'efs-gateway-tmp-name-template' to the name of
;;;     a directory plus an identifying filename prefix for making temporary
;;;     files on the gateway.  For example: "/tmp/hplose/ange/efs"
;;;
;;;  5) If the gateway and the local host share cross-mounted directories,
;;;     set the value of `efs-gateway-mounted-dirs-alist' accordingly. It
;;;     is particularly useful, but not mandatory, that the directory
;;;     of `efs-gateway-tmp-name-template' be cross-mounted.
;;;
;;;  6) Set the variable `efs-gateway-type' to the type gateway that you have.
;;;     This variable is a list, the first element of which is a symbol
;;;     denoting the type of gateway.  Following elements give further
;;;     data on the gateway.
;;;
;;;  Supported gateway types:
;;;
;;;  a) local:
;;;     This means that your local host is itself the gateway.  However,
;;;     it is necessary to use a different FTP client to gain access to
;;;     the outside world.  If the name of the FTP client were xftp, you might
;;;     set efs-gateway-type to
;;;
;;;              (list 'local "xftp" efs-ftp-program-args)
;;;
;;;     If xftp required special arguments, then give them in place of
;;;     efs-ftp-program-args.  See the documentation for efs-ftp-program-args
;;;     for the syntax.
;;;
;;;  b) proxy:
;;;     This indicates that your gateway works by first FTP'ing to it, and
;;;     then issuing a USER command of the form
;;;
;;;                          USER <username>@<host>
;;;
;;;     In this case, you might set efs-gateway-type to
;;;
;;;            (list 'proxy "ftp" efs-ftp-program-args)
;;;             
;;;     If you need to use a nonstandard client, such as iftp, give this
;;;     instead of "ftp".  If this client needs to take special arguments,
;;;     give them instead of efs-ftp-program-args.
;;;
;;;  c) remsh:
;;;     For this type of gateway, you need to start a remote shell on
;;;     your gateway, using either remsh or rsh.  You should set
;;;     efs-gateway-type to something like
;;;
;;;            (list 'remsh "remsh" nil "ftp" efs-ftp-program-args)
;;;
;;;    If you use rsh instead of remsh, change the second element from
;;;    "remsh" to "rsh".  Note that the symbol indicating the gateway
;;;    type should still be 'remsh.  If you want to pass arguments
;;;    to the remsh program, give them as the third element.  For example,
;;;    if you need to specify a user, make this (list "-l" "sandy").
;;;    If you need to use a nonstandard FTP client, specify that as the fourth
;;;    element.  If your FTP client needs to be given special arguments,
;;;    give them instead of efs-ftp-program-args.
;;;
;;; d) interactive:
;;;    This indicates that you need to establish a login on the gateway,
;;;    using either telnet or rlogin.
;;;    You should set efs-gateway-type to something like
;;;
;;;      (list 'interactive "rlogin" nil "exec ftp" efs-ftp-program-args)
;;;
;;;    If you need to use telnet, then give "telnet" in place of the second
;;;    element "rlogin".  If your login program needs to be given arguments,
;;;    then they should be given in the third slot.  The fourth element
;;;    is for the name of the FTP client program.  Giving this as "exec ftp",
;;;    instead of "ftp", ensures that you are logged out if the FTP client
;;;    dies.  If the FTP client takes special arguments, give these instead
;;;    of efs-ftp-program-args.  Furthermore, you should see the documentation
;;;    at the top of efs-gwp.el.  You may need to set the variables
;;;    efs-gwp-setup-term-command, and efs-gwp-prompt-pattern.
;;;
;;; e) raptor:
;;;    This is a type of gateway where efs is expected to specify a gateway
;;;    user, and send a password for this user using the ACCOUNT command.
;;;    For example, to log in to foobar.edu as sandy, while using the account
;;;    ange on the gateway, the following commands would be sent:
;;;
;;;    open raptorgate.com
;;;    quote USER sandy@foobar.edu ange
;;;    quote pass <sandy's password on foobar>
;;;    quote account <ange's password on raptorgate>
;;;
;;;    For such a gateway, you would set efs-gateway-type to
;;;
;;;      (list 'raptor efs-ftp-program efs-ftp-program-args <GATEWAY USER>)
;;;
;;;    where <GATEWAY USER> is the name of your account on the gateway.  In
;;;    the above example, this would be "ange".  You can set your gateway
;;;    password by simply setting an account password for the gateway host.
;;;    This can be done with either efs-set-account, or within your .netrc
;;;    file.  If no password is set, you will be prompted for one.
;;;
;;; f) interlock:
;;;    This is a type of gateway where you are expected to send a PASS
;;;    command after opening the connection to the gateway.
;;;    The precise login sequence is
;;;
;;;    open interlockgate
;;;    quote PASS <sandy's password on interlockgate>
;;;    quote USER sandy@foobar.edu
;;;    quote PASS <sandy's password on foobar.edu>
;;;
;;;    For such a gateway, you should set efs-gateway-type to
;;;
;;;       (list 'interlock efs-ftp-program efs-ftp-program-args)
;;;    
;;;    If you need to use a nonstandard name for your FTP client,
;;;    then replace efs-ftp-program with this name.  If your FTP client
;;;    needs to take nonstandard arguments, then replace efs-ftp-program-args
;;;    with these arguments.  See efs-ftp-program-args <V> for the required
;;;    syntax.
;;;
;;;    If your gateway returns both a 220 code and a 331 code to the
;;;    "open interlockgate" command, then you should add a regular
;;;    expression to efs-skip-msgs <V> that matches the 220 response.
;;;    Returning two response codes to a single FTP command is not permitted
;;;    in RFC 959.  It is not possible for efs to ignore the 220 by default,
;;;    because than it would hang for interlock installations which do not
;;;    require a password.
;;;
;;; g) kerberos:
;;;    With this gateway, you need to authenticate yourself by getting a
;;;    kerberos "ticket" first.  Usually, this is done with the kinit program.
;;;    Once authenticated, you connect to foobar.com as user sandy with the
;;;    sequence: (Note that the "-n" argument inhibits automatic login.
;;;    Although, in manual use you probably don't use it, efs always uses it.)
;;;
;;;    iftp -n
;;;    open foobar.com
;;;    user sandy@foobar.com
;;;
;;;    You should set efs-gateway-type to something like
;;;
;;;      (list 'kerberos "iftp" efs-ftp-program-args "kinit" <KINIT-ARGS>)
;;;    
;;;    If you use an FTP client other than iftp, insert its name instead
;;;    of "iftp" above.  If your FTP client needs special arguments, give
;;;    them as a list of strings in place of efs-ftp-program-args.  If
;;;    the program that you use to collect a ticket in not called "kinit",
;;;    then give its name in place of "kinit" above.  <KINIT-ARGS> should be
;;;    any arguments that you need to pass to your kinit program, given as a
;;;    list of strings.  Most likely, you will give this as nil.
;;;    
;;;    See the file efs-kerberos.el for more configuration variables.  If you
;;;    need to adjust any of these variables, please report this to us so that
;;;    we can fix them for other users.
;;;
;;;    If efs detects that you are not authenticated to use the gateway, it
;;;    will run the kinit program automatically, prompting you for a password.
;;;    If you give a password in your .netrc file for login the value of
;;;    efs-gateway-host <V> and user kerberos, then efs will use this to
;;;    obtain gateway authentication.
;;;
;;; 7) Transparent gateways:
;;;
;;;    If your gateway is completely transparent (for example it uses
;;;    socks), then you should set efs-gateway-type to nil.  Also,
;;;    set efs-ftp-local-host-regexp to ".*".  However, efs-local-host-regexp,
;;;    must still be set to a regular expression matching hosts in your local
;;;    domain.  efs uses this to determine which machines that it can
;;;    open-network-stream to.  Furthermore, you should still set
;;;    efs-gateway-host to the name of your gateway machine.  That way efs
;;;    will know that this is a special machine having direct TCP/IP access
;;;    to both hosts in the outside world, and hosts in your local domain.
;;;
;;; 8) Common Problems with Gateways:
;;;
;;; a) Spurious 220 responses:
;;;    Some proxy-style gateways (eg gateway type 'proxy or 'raptor),
;;;    return two 3-digit FTP reply codes to the USER command.
;;;    For example:
;;;
;;;    open gateway.weird
;;;    220 Connected to gateway.weird
;;;    quote USER sandy@foobar
;;;    220 Connected to foobar
;;;    331 Password required for sandy
;;;    
;;;    This is wrong, according to the FT Protocol.  Each command must return
;;;    exactly one 3-digit reply code.  It may be preceded by continuation
;;;    lines.  What should really be returned is:
;;;
;;;    quote USER sandy@foobar
;;;    331-Connected to foobar.
;;;    331 Password required for sandy.
;;;
;;;    or even
;;;
;;;    quote USER sandy@foobar
;;;    331-220 Connected to foobar.
;;;    331 Password required for sandy.
;;;
;;;    Even though the "331-220" looks strange, it is correct protocol, and
;;;    efs will parse it properly.
;;;
;;;    If your gateway is returning a spurious 220 to USER, a work-around
;;;    is to add a regular expression to `efs-skip-msgs' that matches
;;;    this line.  It must not match the 220 line returned to the open
;;;    command.  This work-around may not work, as some system FTP clients
;;;    also get confused by the spurious 220.  In this case, the only
;;;    solution is to patch the gateway server.  In either case, please
;;;    send a bug report to the author of your gateway software.
;;;   
;;; b) Case-sensitive parsing of FTP commands:
;;;    Some gateway servers seem to treat FTP commands case-sensitively.
;;;    This is incorrect, as RFC 959 clearly states that FTP commands
;;;    are always to be case-insensitive.  If this is a problem with your
;;;    gateway server, you should send a bug report to its author.
;;;    If efs is using a case for FTP commands that does not suit your server,
;;;    a possible work-around is to edit the efs source so that the required
;;;    case is used.  However, we will not be making any changes to the
;;;    standard efs distribution to support this type of server behaviour.
;;;    If you need help changing the efs source, you should enquire with the
;;;    efs-help mailing list.
;;;    

;;; ---------------------------------------------------------------
;;; Tips for using efs:
;;; ---------------------------------------------------------------

;;; 1) Beware of compressing files on non-UNIX hosts. efs will do it by
;;;    copying the file to the local machine, compressing it there, and then
;;;    sending it back. Binary file transfers between machines of different
;;;    architectures can be a risky business. Test things out first on some
;;;    test files. See "Bugs" below. Also, note that efs sometimes
;;;    copies files by moving them through the local machine. Again,
;;;    be careful when doing this with binary files on non-Unix
;;;    machines.
;;;
;;; 2) Beware that dired over ftp will use your setting of dired-no-confirm
;;;    (list of dired commands for which confirmation is not asked).
;;;    You might want to reconsider your setting of this variable,
;;;    because you might want confirmation for more commands on remote
;;;    direds than on local direds. For example, I strongly recommend
;;;    that you not include compress in this list. If there is enough
;;;    demand it might be a good idea to have an alist
;;;    efs-dired-no-confirm of pairs ( TYPE . LIST ), where TYPE is an
;;;    operating system type and LIST is a list of commands for which
;;;    confirmation would be suppressed.  Then remote dired listings
;;;    would take their (buffer-local) value of dired-no-confirm from
;;;    this alist. Who votes for this?
;;;    
;;; 3) Some combinations of FTP clients and servers break and get out of sync
;;;    when asked to list a non-existent directory.  Some of the ai.mit.edu
;;;    machines cause this problem for some FTP clients. Using
;;;    efs-kill-ftp-process can be used to restart the ftp process, which
;;;    should get things back in synch.
;;;
;;; 4) Some ftp servers impose a length limit on the password that can
;;;    be sent. If this limit is exceeded they may bomb in an
;;;    incomprehensible way. This sort of behaviour is common with
;;;    MVS servers. Therefore, you should beware of this possibility
;;;    if you are generating a long password (like an email address)
;;;    with efs-generate-anonymous-password.
;;;
;;; 5) Some antiquated FTP servers hang when asked for an RNFR command.
;;;    efs sometimes uses this to test whether its local cache is stale.
;;;    If your server for HOST hangs when asked for this command, put
;;;    (efs-set-host-property HOST 'rnfr-failed t)
;;;    in your efs-ftp-startup-function-alist entry for HOST.
;;;   

;;; -----------------------------------------------------------------------
;;; Where to get the latest version of efs:
;;; -----------------------------------------------------------------------
;;;
;;; The authors are grateful to anyone or any organization which
;;; provides anonymous FTP distribution for efs.
;;;
;;;
;;; Europe:
;;;
;;; Switzerland
;;; /anonymous@itp.ethz.ch:/sandy/efs/
;;; 
;;; North America:
;;;
;;; Massachusetts, USA
;;; /anonymous@alpha.gnu.ai.mit.edu:/efs/
;;;
;;; California, USA
;;; /anonymous@ftp.hmc.edu:/pub/emacs/packages/efs/
;;;
;;; Australia and New Zealand:
;;;
;;; ????????????
;;;
;;; Japan:
;;;
;;; ????????????

;;; ---------------------------------------------------------------------
;;; Non-UNIX support:
;;; ---------------------------------------------------------------------

;;; efs has full support, incuding file name completion and tree dired
;;; for:
;;;
;;; VMS, CMS, MTS, MVS, ti-twenex, ti-explorer (the last two are lisp
;;; machines), TOPS-20, DOS (running the Distinct, Novell, FTP
;;; software, NCSA, Microsoft in both unix and DOS mode, Super TCP, and
;;; Hellsoft FTP servers), unix descriptive listings (dl), KA9Q, OS/2,
;;; VOS, NOS/VE, CMS running the KNET server, Tandem's Guardian OS, COKE
;;;
;;; efs should be able to automatically recognize any of the operating
;;; systems and FTP servers that it supports. Please report any
;;; failure to do so to the "bugs" address below. You can specify a
;;; certain host as being of a given host type with the command
;;;
;;; (efs-add-host <host-type> <host>)
;;;
;;; <host-type> is a symbol, <host> is a string. If this command is
;;; used interactively, then <host-type> is prompted for with
;;; completion. Some host types have regexps that can be used to
;;; specify a class of host names as being of a certain type. Note
;;; that if you specify a host as being of a certain type, efs does
;;; not verify that that is really the type of the host. This calls
;;; for caution when using regexps to specify host types, as an
;;; inadvertent match to a regexp might have unpleasant consequences.
;;;
;;; See the respective efs-TYPE.el files for more information.
;;; When or if we get a tex info file, it should contain some more
;;; details on the non-unix support.

;;; ------------------------------------------------------------------
;;; Bugs and other things that go clunk in the night:
;;; ------------------------------------------------------------------

;;; How to report a bug:
;;; --------------------
;;; 
;;; Type M-x efs-report-bug
;;; or
;;; send mail to efs-bugs@cuckoo.hpl.hp.com.
;;;
;;; efs is a "free" program. This means that you didn't (or shouldn't
;;; have) paid anything for it. It also means that nobody is paid to
;;; maintain it, and the authors weren't paid for writing it.
;;; Therefore, please try to write your bug report in a clear and
;;; complete fashion. It will greatly enhance the probability that
;;; something will be done about your problem.
;;;
;;; Note that efs relies heavily in cached information, so the bug may
;;; depend in a complicated fashion on commands that were performed on
;;; remote files from the beginning of your emacs session. Trying to
;;; reproduce your bug starting from a fresh emacs session is usually
;;; a good idea.
;;;

;;; Fan/hate mail:
;;; --------------
;;;
;;; efs has its own mailing list called efs-help.  All users of efs
;;; are welcome to subscribe (see below) and to discuss aspects of
;;; efs.  New versions of efs are posted periodically to the mailing
;;; list.
;;;
;;; To [un]subscribe to efs-help, or to report mailer problems with the
;;; list, please mail one of the following addresses:
;;;
;;;     efs-help-request@cuckoo.hpl.hp.com
;;; or
;;;     efs-help-request%cuckoo.hpl.hp.com@hplb.hpl.hp.com
;;;
;;; Please don't forget the -request part.
;;;
;;; For mail to be posted directly to efs-help, send to one of the
;;; following addresses:
;;; 
;;;     efs-help@cuckoo.hpl.hp.com
;;; or
;;;     efs-help%cuckoo.hpl.hp.com@hplb.hpl.hp.com
;;;
;;; Alternatively, there is a mailing list that only gets
;;; announcements of new efs releases.  This is called efs-announce,
;;; and can be subscribed to by e-mailing to the -request address as
;;; above.  Please make it clear in the request which mailing list you
;;; wish to join.
;;;

;;; Known bugs:
;;; -----------
;;;
;;; If you hit a bug in this list, please report it anyway. Most of
;;; the bugs here remain unfixed because they are considered too
;;; esoteric to be a high priority. If one of them gets reported
;;; enough, we will likely change our view on that.
;;; 
;;;  1) efs does not check to make sure that when creating a new file,
;;;     you provide a valid filename for the remote operating system.
;;;     If you do not, then the remote FTP server will most likely
;;;     translate your filename in some way. This may cause efs to
;;;     get confused about what exactly is the name of the file.
;;;
;;;  2) For CMS support, we send too many cd's. Since cd's are cheap, I haven't
;;;     worried about this too much. Eventually, we should have some caching
;;;     of the current minidisk. This is complicated by the fact that some
;;;     CMS servers lie about the current minidisk, so sending redundant
;;;     cd's helps us recover in this case.
;;;    
;;;  3) The code to do compression of files over ftp is not as careful as it
;;;     should be. It deletes the old remote version of the file, before
;;;     actually checking if the local to remote transfer of the compressed
;;;     file succeeds. Of course to delete the original version of the file
;;;     after transferring the compressed version back is also dangerous,
;;;     because some OS's have severe restrictions on the length of filenames,
;;;     and when the compressed version is copied back the "-Z" or ".Z" may be
;;;     truncated. Then, efs would delete the only remaining version of
;;;     the file.  Maybe efs should make backups when it compresses files
;;;     (of course, the backup "~" could also be truncated off, sigh...).
;;;     Suggestions?
;;;
;;;  4) If a dir listing is attempted for an empty directory on (at least
;;;     some) VMS hosts, an ftp error is given. This is really an ftp bug, and
;;;     I don't know how to get efs work to around it.
;;; 
;;;  5) efs gets confused by directories containing file names with
;;;     embedded newlines. A temporary solution is to add "q" to your
;;;     dired listing switches. As long as your dired listing switches
;;;     also contain "l" and either "a" or "A", efs will use these
;;;     switches to get listings for its internal cache. The "q" switch
;;;     should force listings to be exactly one file per line. You
;;;     still will not be able to access a file with embedded newlines,
;;;     but at least it won't mess up the parsing of the rest of the files.
;;;
;;;  6) efs cannot parse symlinks which have an embedded " -> "
;;;     in their name. It's alright to have an embedded " -> " in the name
;;;     of any other type of file. A fix is possible, but probably not worth
;;;     the trouble. If you disagree, send us a bug report.
;;;
;;;  7) efs doesn't handle context-dep. files in H-switch listings on
;;;     HP's. It wouldn't be such a big roaring deal to fix this. I'm
;;;     waiting until I get an actual bug report though.
;;;
;;;  8) If a hard link is added or deleted, efs will not update its
;;;     internal cache of the link count for other names of the file.
;;;     This may cause file-nlinks to return incorrectly. Reverting
;;;     any dired buffer containing other names for the file will
;;;     cause the file data to be updated, including the link counts.
;;;     A fix for this problem is known and will be eventually
;;;     implemented. How it is implemented will depend on how we decide
;;;     to handle inodes. See below.
;;;
;;;  9) efs is unable to parse R-switch listings from remote unix hosts.
;;;     This is inefficient, because efs will insist on doing individual
;;;     listings of the subdirectories to get its file information.
;;;     This may be fixed if there is enough demand.
;;;
;;; 10) In file-attributes, efs returns a fake inode number. Of course
;;;     this is necessary, but this inode number is not even necessarily
;;;     unique.  It is simply the sum of the characters (treated as
;;;     integers) in the host name, user name, and file name. Possible
;;;     ways to get a unique inode number are:
;;;     a) Simply keep a count of all remote file in the cache, and
;;;        return the file's position in this count as a negative number.
;;;     b) For unix systems, we could actually get at the real inode
;;;        number on the remote host, by adding an "i" to the ls switches.
;;;        The inode numbers would then be removed from the listing
;;;        returned by efs-ls, if the caller hadn't requested the "i"
;;;        switch. We could then make a unique number out of the host name
;;;        and the real inode number.
;;;
;;; 11) efs tries to determine if a file is readable or writable by comparing
;;;     the file modes, file owner, and user name under which it is logged
;;;     into the remote host. This does not take into account groups.
;;;     We simply assume that the user belongs to all groups. As a result
;;;     we may assume that a file is writable, when in fact it is not.
;;;     Groups are tough to handle correctly over FTP. Suggestions?
;;;     (For new FTP servers, can do a "QUOTE SITE EXEC groups" to
;;;     handle this.)

;;; -----------------------------------------------------------
;;; Technical information on this package:
;;; -----------------------------------------------------------

;;; efs hooks onto the following functions using the
;;; file-name-handler-alist.  Depending on which version of emacs you
;;; are using, not all of these functions may access this alist. In
;;; this case, efs overloads the definitions of these functions with
;;; versions that do access the file-name-handler-alist. These
;;; overloads are done in efs's version-specific files.
;;;
;;; abbreviate-file-name
;;; backup-buffer
;;; copy-file
;;; create-file-buffer
;;; delete-directory
;;; delete-file
;;; directory-file-name
;;; directory-files
;;; file-attributes
;;; file-directory-p
;;; file-exists-p
;;; file-local-copy
;;; file-modes
;;; file-name-all-completions
;;; file-name-as-directory
;;; file-name-completion
;;; file-name-directory
;;; file-name-nondirectory
;;; file-name-sans-versions
;;; file-newer-than-file-p
;;; file-readable-p
;;; file-executable-p
;;; file-accessible-directory-p
;;; file-symlink-p
;;; file-writable-p
;;; get-file-buffer
;;; insert-directory
;;; insert-file-contents
;;; list-directory
;;; make-directory-internal
;;; rename-file
;;; set-file-modes
;;; set-visited-file-modtime
;;; substitute-in-file-name
;;; verify-visited-file-modtime
;;; write-region
;;; 
;;; The following functions are overloaded in efs.el, because they cannot
;;; be handled via the file-name-handler-alist.
;;;
;;; expand-file-name
;;; load
;;; read-file-name-internal (Emacs 18, only)
;;; require
;;; 
;;; The following dired functions are handled by hooking them into the
;;; the file-name-handler-alist. This is done in efs-dired.el.
;;; 
;;; efs-dired-compress-file
;;; eds-dired-print-file
;;; efs-dired-make-compressed-filename
;;; efs-compress-file
;;; efs-dired-print-file
;;; efs-dired-create-directory
;;; efs-dired-recursive-delete-directory
;;; efs-dired-uncache
;;; efs-dired-call-process
;;; 
;;; In efs-dired.el, the following dired finctions are overloaded.
;;;
;;; dired-collect-file-versions
;;; dired-find-file
;;; dired-flag-backup-files
;;; dired-get-filename
;;; dired-insert-headerline
;;; dired-move-to-end-of-filename
;;; dired-move-to-filename
;;; dired-run-shell-command
;;;
;;; efs makes use of the following hooks
;;;
;;; diff-load-hook
;;; dired-before-readin-hook
;;; find-file-hooks
;;; dired-grep-load-hook

;;; LISPDIR ENTRY for the Elisp Archive:
;;; 
;;;    LCD Archive Entry:
;;;    efs|Andy Norman and Sandy Rutherford
;;;    |ange@hplb.hpl.hp.com and sandy@ibm550.sissa.it
;;;    |transparent FTP Support for GNU Emacs
;;;    |$Date: 94/08/25 $|$efs release: 1.15 beta $|

;;; Host and listing type notation:
;;;
;;; The functions efs-host-type and efs-listing-type, and the
;;; variable efs-dired-host-type follow the following conventions
;;; for remote host types.
;;;
;;; nil = local host type, whatever that is (probably unix).
;;;       Think nil as in "not a remote host". This value is used by
;;;       efs-dired-host-type for local buffers.
;;;       (efs-host-type nil) => nil
;;;
;;; 'type = a remote host of TYPE type.
;;;
;;; 'type:list = a remote host using listing type 'type:list.
;;;              This is currently used for Unix dl (descriptive
;;;              listings), when efs-dired-host-type is set to
;;;              'unix:dl, and to support the myriad of DOS FTP
;;;              servers.

;;; Supported host and listing types:
;;;
;;; unknown, unix, dumb-unix, bsd-unix, sysV-unix, next-unix,
;;; super-dumb-unix, dumb-apollo-unix,
;;; apollo-unix, unix:dl, dos-distinct, ka9q, dos, dos:ftp, dos:novell,
;;; dos:ncsa, dos:winsock, vos, hell, dos:microsoft, super-dumb-unix
;;; vms, cms, mts, mvs, mvs:tcp mvs:nih tops-20, mpe, ti-twenex,
;;; ti-explorer, os2, vos,
;;; vms:full, guardian, ms-unix (This is the Microsoft NT Windows server
;;; in unix mode.), plan9, unix:unknown, nos-ve (actually NOS/VE).

;;; Host and listing type hierarchy:
;;;
;;; unknown: unix, dumb-unix, sysV-unix, bsd-unix, next-unix, apollo-unix,
;;;          ka9q, dos-distinct, unix:dl, hell, 
;;;          super-dumb-unix, dumb-apollo-unix
;;; unix:    sysV-unix, bsd-unix, next-unix, apollo-unix, unix:dl
;;; dos:     dos:ftp, dos:novell, dos:ncsa, dos:microsoft, dos:winsock
;;; dumb-unix:
;;; bsd-unix:
;;; sysV-unix:
;;; next-unix:
;;; apollo-unix:
;;; dumb-apollo-unix:
;;; unix:dl:
;;; unix:unknown: unix:dl, unix
;;; super-dumb-unix:
;;; dos-distinct:
;;; dos:ftp:
;;; dos:novell:
;;; dos:microsoft
;;; ka9q:
;;; vms: vms:full
;;; cms:
;;; mts:
;;; mvs: mvs:tcp, mvs:nih
;;; mvs:tcp:
;;; mvs:nih:
;;; tops-20:
;;; ti-twenex:
;;; ti-explorer:
;;; os2:
;;; vos:
;;; vms:full:
;;; dos:ncsa:
;;; dos:winsock:
;;; vos:
;;; hell:
;;; guardian:
;;; ms-unix:
;;; plan9:
;;; nos-ve:
;;; coke:
;;; 


;;;; ================================================================
;;;; >0
;;;; Table of Contents for efs.el
;;;; ================================================================
;;
;;   Each section of efs.el is labelled by >#, where # is the number of
;;   the section.
;;
;;    1. Provisions, requirements, and autoloads.
;;    2. Variable definitions.
;;    3. Utilities.
;;    4. Hosts, users, accounts, and passwords.
;;    5. FTP client process and server responses.
;;    6. Sending commands to the FTP server.
;;    7. Parsing and storing remote file system data.
;;    8. Redefinitions of standard GNU Emacs functions.
;;    9. Multiple host type support.
;;   10. Attaching onto the appropriate emacs version.


;;;; ================================================================
;;;; >1
;;;; General provisions, requirements, and autoloads.
;;;; Host type, and local emacs type dependent loads, and autoloads
;;;; are in the last two sections of this file.
;;;; ================================================================

;;;; ----------------------------------------------------------------
;;;; Provide the package (Do this now to avoid an infinite loop)
;;;; ----------------------------------------------------------------

(provide 'efs)

;;;; ----------------------------------------------------------------
;;;; Our requirements.
;;;; ----------------------------------------------------------------

(require 'backquote)
(require 'comint)
(require 'efs-defun)
(require 'efs-netrc)
(require 'efs-cu)
(require 'efs-ovwrt)
;; Do this last, as it installs efs into the file-name-handler-alist.
(require 'efs-fnh)

(autoload 'efs-report-bug "efs-report" "Submit a bug report for efs." t)
(autoload 'efs-gwp-start "efs-gwp" ; For interactive gateways.
	  "Login to the gateway machine and fire up an FTP client.")
(autoload 'efs-kerberos-login "efs-kerberos")
(autoload 'efs-insert-directory "efs-dired" "Insert a directory listing.")
(autoload 'efs-set-mdtm-of "efs-cp-p")
(autoload 'diff-latest-backup-file "diff")
(autoload 'read-passwd "passwd" "Read a password from the minibuffer." t)


;;;; ============================================================
;;;; >2
;;;;           Variable Definitions
;;;; **** The user configuration variables are in  ****
;;;; **** the second subsection of this section.   ****
;;;; ============================================================

;;;; ------------------------------------------------------------
;;;; Constant Definitions
;;;; ------------------------------------------------------------

(defconst efs-version
  (concat (substring "$efs release: 1.15 $" 14 -2)
	  "/"
	  (substring "#Revision: 1.56 $" 11 -2)))

(defconst efs-time-zero 1970) ; we count time from midnight, Jan 1, 1970 GMT.

(defconst efs-dumb-host-types
  '(dumb-unix super-dumb-unix vms cms mts ti-twenex ti-explorer dos mvs
	      tops-20 mpe ka9q dos-distinct os2 vos hell guardian
	      netware cms-knet nos-ve coke dumb-apollo-unix)
  "List of host types that can't take UNIX ls-style listing options.")
;; dos-distinct only ignores ls switches; it doesn't barf.
;; Still treat it as dumb.

(defconst efs-unix-host-types
  '(unix sysV-unix bsd-unix next-unix apollo-unix dumb-unix
	 dumb-apollo-unix super-dumb-unix)
  "List of unix host types.")

(defconst efs-version-host-types '(vms tops-20 ti-twenex ti-explorer)
  "List of host-types which associated a version number to all files.
This is not the same as associating version numbers to only backup files.")
;; Note that on these systems, 
;;  (file-name-sans-versions EXISTING-FILE) does not exist as a file.

(defconst efs-single-extension-host-types
  '(vms tops-20 ti-twenex ti-explorer cms mvs dos ka9q dos-distinct hell
	netware ms-unix plan9 cms-knet nos-ve)
  "List of host types which allow at most one extension on a file name.
Extensions are deliminated by \".\". In addition, these host-types must
allow \"-\" in file names, because it will be used to add additional extensions
to indicate compressed files.")

(defconst efs-idle-host-types
  (append '(coke unknown) efs-unix-host-types))
;; List of host types for which it is possible that the SITE IDLE command
;; is supported.

(defconst efs-listing-types
  '(unix:dl unix:unknown
    dos:novell dos:ftp dos:ncsa dos:microsoft dos:stcp dos:winsock
    mvs:nih mvs:tcp mvs:tcp
    vms:full)
  "List of supported listing types")

(defconst efs-nlist-listing-types
  '(vms:full))
;; Listing types which give a long useless listing when asked for a
;; LIST. For these, use an NLST instead. This can only be done
;; when there is some way to distinguish directories from
;; plain files in an NLST.

(defconst efs-opaque-gateways '(remsh interactive))
;; List of gateway types for which we need to do explicit file handling on
;; the gateway machine.

;;;; ------------------------------------------------------------------
;;;; User customization variables. Please read through these carefully.
;;;; ------------------------------------------------------------------

;;;>>>>  If you are not fully connected to the internet,        <<<< 
;;;>>>>  and need to use a gateway (no matter how transparent)  <<<<
;;;>>>>  you will need to set some of the following variables.  <<<<
;;;>>>>  Read the documentation carefully.                      <<<<

(defvar efs-local-host-regexp ".*"
  "Regexp to match names of local hosts.
These are hosts to which it is possible to obtain a direct internet
connection.  Even if the host is accessible by a very transparent FTP gateway,
it does not qualify as a local host.  The test to determine if machine A is
local to your machine is if it is possible to ftp from A _back_ to your
local machine.  Also, open-network-stream must be able to reach the host 
in question.")

(defvar efs-ftp-local-host-regexp ".*"
  "Regexp to match the names of hosts reachable by a direct ftp connection.
This regexp should match the names of hosts which can be reached using ftp,
without requiring any explicit connection to a gateway. If you have a smart
ftp client which is able to transparently go through a gateway, this will
differ from `efs-local-host-regexp'.")

(defvar efs-gateway-host nil
  "If non-nil, this must be the name of your ftp gateway machine.
If your net world is divided into two domains according to
`efs-local-ftp-host-regexp', set this variable to the name of the
gateway machine.")

(defvar efs-gateway-type nil
  "Specifies which type of gateway you wish efs to use.
This should be a list, the first element of which is a symbol denoting the
gateway type, and following elements give data on how to use the gateway.

The following possibilities are supported:

  '(local FTP-PROGRAM FTP-PROGRAM-ARGS)
  This means that your local host is itself the gateway.  However,
  you need to run a special FTP client to access outside hosts.
  FTP-PROGRAM should be the name of this FTP client,  and FTP-PROGRAM-ARGS
  is a list of arguments to pass to it \(probably set this to the value of
  efs-ftp-program-args <V>\).  Note that if your gateway is of this type,
  then you would set efs-gateway-host to nil.

  '(proxy FTP-PROGRAM FTP-PROGRAM-ARGS)
  This indicates that your gateway works by first FTP'ing to it, and
  then giving a  USER command of the form \"USER <username>@<host>\".
  FTP-PROGRAM is the FTP program to use to connect to the gateway; this
  is most likely \"ftp\".  FTP-PROGRAM-ARGS is a list of arguments to 
  pass to it.  You likely want this to be set to the value of
  efs-ftp-program-args <V>.  If the connection to the gateway FTP server
  is to be on a port different from 21, set efs-gateway-host to 
  \"<host>#<port>\".

  '(raptor FTP-PROGRAM FTP-PROGRAM-ARGS USER)
  This is for the gateway called raptor by Eagle.  After connecting to the
  the gateway, the command \"user <user>@host USER\" is issued to login
  as <user> on <host>, where USER is an authentication username for the 
  gateway.  After issuing the password for the remote host, efs will
  send the password for USER on efs-gateway-host <V> as an account command.

 '(interlock FTP-PROGRAM FTP-PROGRAM-ARGS)
  This is for the interlock gateway.  The exact login sequence is to
  connect to the gateway specified by efs-gateway-host <V>, send the
  gateway password with a PASS command, send the command
  \"user <user>@<host>\" to connect to remote host <host> as user <user>,
  and finally to send the password for <user> on <host> with a second
  PASS command.

  '(kerberos FTP-PROGRAM FTP-PROGRAM-ARGS KINIT-PROGRAM KINIT-PROGRAM-ARGS)
  This is for the kerberos gateway where you need to run a program (kinit) to
  obtain a ticket for gateway authroization first.  FTP-PROGRAM should be
  the name of the FTP client that you use to connect to the gateway.  This
  may likely be \"iftp\".  FTP-PROGRAM-ARGS are the arguments that you need
  to pass to FTP-PROGRAM.  This is probably the value of
  efs-ftp-program-args <V>.  KINIT-PROGRAM is the name of the program to
  run in order to obtain a ticket.  This is probably \"kinit\".
  KINIT-PROGRAM-ARGS is a list og strings indicating any arguments that you
  need to pass to KINIT-PROGRAM.  Most likely this is nil.

  '(remsh GATEWAY-PROGRAM GATEWAY-PROGRAM-ARGS FTP-PROGRAM FTP-PROGRAM-ARGS)
  This indicates that you wish to run FTP on your gateway using a remote shell.
  GATEWAY-PROGRAM is the name of the program to use to start a remote shell.
  It is assumed that it is not necessary to provide a password to start
  this remote shell.  Likely values are \"remsh\" or \"rsh\".
  GATEWAY-PROGRAM-ARGS is a list of arguments to pass to GATEWAY-PROGRAM.
  FTP-PROGRAM is the name of the FTP program on the gateway.  A likely setting
  of this is \"ftp\".  FTP-PROGRAM-ARGS is a list of arguments to pass to 
  FTP-PROGRAM.  Most likely these should be set to the value of
  efs-ftp-program-args <V>.

  '(interactive GATEWAY-PROGRAM GATEWAY-PROGRAM-ARGS FTP-PROGRAM 
      FTP-PROGRAM-ARGS)
  This indicates that you need to start an interactive login on your gatway,
  using rlogin, telnet, or something similar.  GATEWAY-PROGRAM is the name
  of the program to use to log in to the gateway, and GATEWAY-PROGRAM-ARGS
  is a list of arguments to pass to it.  FTP-PROGRAM is the name of the FTP
  program on the gateway.  A likely setting for this variable would be
  \"exec ftp\".  FTP-PROGRAM-ARGS is a list of arguments to pass
  to FTP-PROGRAM.  You probably want to set these to the same value as
  efs-ftp-program-args <V>.  If you are using this option, read the
  documentation at the top of efs-gwp.el, and see 
  efs-gwp-setup-term-command <V>.")

(defvar efs-gateway-hash-mark-size nil
  "*Value of `efs-hash-mark-size' for FTP clients on `efs-gateway-host'.
See the documentation of these variables for more information.")

(defvar efs-gateway-incoming-binary-hm-size nil
  "*Value of `efs-incoming-binary-hm-size' for `efs-gateway-host'.
See documentation of these variables for more information.")

(defvar efs-gateway-tmp-name-template "/tmp/efs"
  "Template used to create temporary files when ftp-ing through a gateway.
This should be the name of the file on the gateway, and not necessarily
the name on the local host.")

(defvar efs-gateway-mounted-dirs-alist nil
  "An alist of directories cross-mounted between the gateway and local host.
Each entry is of the form \( DIR1 . DIR2 \), where DIR1 is the name of the
directory on the local host, and DIR2 is its name on the remote host. Both
DIR1 and DIR2 must be specified in directory syntax, i.e. end in a slash.
Note that we will assume that subdirs of DIR1 and DIR2 are also accessible
on both machines.")

(defvar efs-gateway-ftp-prompt-regexp "^\\(ftp\\|Ftp\\|FTP\\)> *"
  "*Regular expression to match the prompt of the gateway FTP client.")

;;; End of gateway config variables.

(defvar efs-tmp-name-template "/tmp/efs"
  "Template used to create temporary files.
If you are worried about security, make this a directory in some
bomb-proof cave somewhere. efs does clean up its temp files, but
they do live for short periods of time.")

(defvar efs-generate-anonymous-password t
  "*If t, use a password of `user@host' when logging in as the anonymous user.
`host' is generated by the function `efs-system-fqdn'. If `system name' returns
a fully qualified domain name, `efs-system-fqdn' will return this. Otherwise,
it will attempt to use nslookup to obtain a fully qualified domain name. If
this is unsuccessful, the returned value will be the same as `system-name',
whether this is a fully qualified domain name or not.

If a string then use that as the password.

If nil then prompt the user for a password.

Beware that some operating systems, such as MVS, restrict substantially
the password length. The login will fail with a weird error message
if you exceed it.")

(defvar efs-high-security-hosts nil
  "*Indicates host user pairs for which passwords should not be cached.
If non-nil, should be a regexp matching user@host constructions for which
efs should not store passwords in its internal cache.")

;; The following regexps are tested in the following order:
;; efs-binary-file-host-regexp, efs-36-bit-binary-file-name-regexp,
;; efs-binary-file-name-regexp, efs-text-file-name-regexp.
;; File names which match nothing are transferred in 'image mode.

;; If we're not careful, we're going to blow the regexp stack here.
;; Probably should move to a list of regexps. Slower, but safer.
;; This is not a problem in Emacs 19.
(defvar efs-binary-file-name-regexp
  (concat "\\." ; the dot
	  ;; extensions
	  "\\([zZ]\\|t?gz\\|lzh\\|arc\\|zip\\|zoo\\|ta[rz]\\|dvi\\|sit\\|"
	  "ps\\|elc\\|gif\\|Z-part-..\\|tpz\\|exe\\|[jm]pg\\|TZ[a-z]?\\|lib\\)"
	  "\\(~\\|~[0-9]+~\\)?$" ; backups
	  "\\|"
	  ;; UPPER CASE LAND
	  "\\."
	  "\\(ARC\\|ELC\\|TAGS\\|EXE\\|ZIP\\|DVI\|ZOO\\|GIF\\|T?GZ\\|"
	  "[JM]PG\\)"
	  "\\([.#;][0-9]+\\)?$" ; versions
	  )
  "*Files whose names  match this regexp will be considered to be binary.
By binary here, we mean 8-bit binary files (the usual unix binary files).
If nil, no files will be considered to be binary.")

(defvar efs-binary-file-host-regexp nil
  "*All files on hosts matching this regexp are treated as 8-bit binary.
Setting this to nil, inhibits this feature.")

(defvar efs-36-bit-binary-file-name-regexp nil
  "*Files whose names match this regexp will be considered to PDP 10 binaries.
These are 36-bit word-aligned binary files. This is really only relevant for
files on PDP 10's, and similar machines. If nil, no files will be considered
to be PDP 10 binaries.")

(defvar efs-text-file-name-regexp ".*"
  "*Files whose names match this regexp will be considered to be text files.")

(defvar efs-prompt-for-transfer-type nil
  "*If non-nil, efs will prompt for the transfer type for each file transfer.
The command efs-prompt-for-transfer-type can be used to toggle its value.")

(defvar efs-treat-crlf-as-nl nil
  "*Controls how file systems using CRLF as end of line are treated.
If non-nil, such file systems will be considered equivalent to those which use
LF as end of line.  This is particularly relevant to transfers between DOS
systems and UNIX.  Setting this to be non-nil will cause all file transfers
between DOS and UNIX systems to use be image or binary transfers.")

(defvar efs-send-hash t
  "*If non-nil, send the HASH command to the FTP client.")

(defvar efs-hash-mark-size nil
  "*Default size, in bytes, between hash-marks when transferring a file.
If this is nil then efs will attempt to assign a value based on the
output of the HASH command. Also, if this variable is incorrectly set,
then efs will try to correct it based on the size of the last file
transferred, and the number hashes outputed by the client during the
transfer.

The variable `efs-gateway-hash-mark-size' defines the corresponding value
for the FTP client on the gateway, if you are using a gateway. 

Some client-server combinations do not correctly compute the number of hash
marks for incoming binary transfers. In this case, a separate variable
`efs-incoming-binary-hm-size' can be used to set a default value of the
hash mark size for incoming binary transfers.")

(defvar efs-incoming-binary-hm-size nil
  "*Default hash mark size for incoming binary transfers.
If this is nil, incoming binary transfers will use `efs-hash-mark-size' as 
the default. See the documentation of this variable for more details.")

(defvar efs-verbose t
  "*If non-NIL then be chatty about interaction with the FTP process.
If 0 do not give % transferred reports for asynchronous commands and status
reports for commands verifying file modtimes, but report on everything else.")

(defvar efs-message-interval 0
  "*Defines the minimum time in seconds between status messages.
A new status message is not displayed, if one has already been given
within this period of time.")

(defvar efs-max-ftp-buffer-size 3000
  "*Maximum size in characters of FTP process buffer, before it is trimmed.
The buffer is trimmed to approximately half this size. Setting this to nil
inhibits trimming of FTP process buffers.")

(defvar efs-ls-cache-max 5
  "*Maximum number of directory listings to be cached in efs-ls-cache.")

(defvar efs-mode-line-format " ftp(%d)"
  "Format string used to determine how FTP activity is shown on the mode line.
It is passed to format, with second argument the number of active FTP
sessions as an integer.")

(defvar efs-show-host-type-in-dired t
  "If non-nil, show the system type on the mode line of remote dired buffers.")

(defvar efs-ftp-activity-function nil
  "Function called to indicate FTP activity. 
It must have exactly one argument, the number of active FTP sessions as an
integer.")

(defvar efs-ftp-program-name "ftp"
  "Name of FTP program to run.")

(defvar efs-ftp-program-args '("-i" "-n" "-g" "-v")
  "*A list of arguments passed to the FTP program when started.")

(defvar efs-ftp-prompt-regexp "^\\(ftp\\|Ftp\\|FTP\\)> *"
  "*Regular expression to match the prompt of your FTP client.")

(defvar efs-nslookup-program "nslookup"
  "*If non-NIL then a string naming nslookup program." )

(defvar efs-nslookup-on-connect nil
  "*If non-NIL then use nslookup to resolve the host name before connecting.")

(defvar efs-nslookup-threshold 1000
  "How many iterations efs waits on the nslookup program.
Applies when nslookup is used to compute a fully qualified domain name
for the local host, in the case when `system-name' does not return one.
If you set this to nil, efs will wait an arbitrary amount of time to get
output.")

(defvar efs-make-backup-files efs-unix-host-types
  "*A list of operating systems for which efs will make Emacs backup files.
The backup files are made on the remote host.

For example:
'\(unix sysV-unix bsd-unix apollo-unix dumb-unix\) makes sense, but
'\(unix vms\) would be silly, since vms makes its own backups.")

;; Is this variable really useful? We should try to figure a way to
;; do local copies on a remote machine that doesn't take forever.
(defvar efs-backup-by-copying nil
  "*Version of `backup by copying' for remote files.
If non-nil, remote files will be backed up by copying, instead of by renaming.
Note the copying will be done by moving the file through the local host -- a 
very time consuming operation.")

;;; Auto-save variables. Relevant for auto-save.el

(defvar efs-auto-save 0
  "*If 1, allows efs files to be auto-saved.
If 0, suppresses auto-saving of efs files.
Don't use any other value.")

(defvar efs-auto-save-remotely nil
  "*Determines where remote files are auto-saved.

If nil, auto-saves for remote files will be written in `auto-save-directory'
or `auto-save-directory-fallback' if this isn't defined.

If non-nil, causes the auto-save file for an efs file to be written in
the remote directory containing the file, rather than in a local directory.
For remote files, this overrides a non-nil `auto-save-directory'. Local files
are unaffected. If you want to use this feature, you probably only want to 
set this true in a few buffers, rather than globally.  You might want to give
each buffer its own value using `make-variable-buffer-local'. It is usually
a good idea to auto-save remote files locally, because it is not only faster,
but provides protection against a connection going down.

See also variable `efs-auto-save'.")

(defvar efs-short-circuit-to-remote-root nil
  "*Defines whether \"//\" short-circuits to the remote or local root.")

;; Can we somehow grok this from system type?  No.
(defvar efs-local-apollo-unix
  (eq 0 (string-match "//" (or (getenv "HOME") (getenv "SHELL") "")))
  "*Defines whether the local machine is an apollo running Domain.
This variable has nothing to do with efs, and should be basic to all 
of emacs.")

(defvar efs-root-umask nil
  "*umask to use for root logins.")

(defvar efs-anonymous-umask nil
  "*umask to use for anonymous logins.")

(defvar efs-umask nil
  "*umask to use for efs sessions.
If this is nil, then the setting of umask on the local host is used.")

;; Eliminate these variables when Sun gets around to getting its FTP server
;; out of the stone age.
(defvar efs-ding-on-umask-failure t
  "*Ring the bell if the umask command fails on a unix host. Many servers don't
support this command, so if you get a lot of annoying failures, set this
to nil.")

(defvar efs-ding-on-chmod-failure t
  "*Ring the bell if the chmod command fails on a unix host. Some servers don't
support this command, so if you get a lot of annoying failures, set this
to nil.")

;; Please let us know if you can contribute more entries to this guessing game.
(defvar efs-nlist-cmd
  (cond
   ;; Covers Ultrix, SunOS, and NeXT.
   ((eq system-type 'berkeley-unix)
    "ls")
   ((memq system-type '(hpux aix-v3 silicon-graphics-unix))
    "nlist")
   ;; Blind guess
   ("ls"))
  "*FTP client command for getting a brief listing (NLST) from the FTP server. 
We try to guess this based on the local system-type, but obviously if you
are using a gateway, you'll have to set it yourself.")

(defvar efs-compute-remote-buffer-file-truename nil
  "*If non-nil, `buffer-file-truename' will be computed for remote buffers.
In emacs 19, each buffer has a local variable, `buffer-file-truename',
which is used to ensure that symbolic links will not confuse emacs into
visiting the same file with two buffers. This variable is computed by
chasing all symbolic links in `buffer-file-name', both at the level of the
file and at the level of all parent directories. Since this operation can be
very time-consuming over FTP, this variable can be used to inhibit it.")

(defvar efs-buffer-name-case nil
  "*Selects the case used for buffer names of case-insensitive file names.
Case-insensitive file names are files on hosts whose host type is in
`efs-case-insensitive-host-types'.

If this is 'up upper case is used, if it is 'down lower case is used. 
If this has any other value, the case is inherited from the name used 
to access the file.")

(defvar efs-fancy-buffer-names "%s@%s"
  "Format used to compute names of buffers attached to remote files.

If this is nil, buffer names are computed in the usual way.

If it is a string, then the it is passed to format with second and third
arguments the host name and file name.

Otherwise, it is assumed to be function taking three arguments, the host name,
the user name, and the truncated file name.  It should returns the name to
be used for the buffer.")

(defvar efs-verify-anonymous-modtime nil
  "*Determines if efs checks modtimes for remote files on anonymous logins.
If non-nil, efs runs `verify-visited-file-modtime' for remote files on 
anonymous ftp logins. Since verify-visited-file-modtime slows things down,
and most people aren't editing files on anonymous ftp logins, this is nil
by default.")

(defvar efs-verify-modtime-host-regexp ".*"
  "*Regexp to match host names for which efs checks file modtimes.
If non-nil, efs will run `verify-visited-file-modtime' for remote
files on hosts matching this regexp. If nil, verify-visited-file-modtime
is supressed for all remote hosts. This is tested before
`efs-verify-anonymous-modtime'.")

(defvar efs-maximize-idle nil
  "*If non-nil, efs will attempt to maximize the idle time out period.
At some idle moment in the connection after login, efs will attempt to
set the idle time out period to the maximum amount allowed by the server.
It applies only to non-anonymous logins on unix hosts.")

(defvar efs-expire-ftp-buffers t
  "*If non-nil ftp buffers will be expired.
The buffers will be killed either after `efs-ftp-buffer-expire-time' has
elapsed with no activity, or the remote FTP server has timed out.")

(defvar efs-ftp-buffer-expire-time nil
  "*If non-nil, the time after which ftp buffers will be expired.
If nil, ftp buffers will be expired only when the remote server has timed out.
If an integer, ftp buffers will be expired either when the remote server
has timed out, or when this many seconds on inactivity has elapsed.")

;; If you need to increase this variable much, it is likely that
;; the true problem is timing errors between the efs process filter
;; and the FTP server. This could either be caused by the server
;; not following RFC959 response codes, or a bug in efs. In either
;; case please report the problem to us. If it's a bug, we'll fix it.
;; If the server is at fault we may try to do something. Our rule
;; of thumb is that we will support non-RFC959 behaviour, as long as
;; it doesn't risk breaking efs for servers which behave properly.

(defvar efs-retry-time 5
  "*Number of seconds to wait before retrying if data doesn't arrive.
The FTP command isn't retried, rather efs just takes a second look
for the data file. This might need to be increased for very slow FTP
clients.")

(defvar efs-pty-check-threshold 1000
  "*How long efs waits before deciding that it doesn't have a pty.
Specifically it is the number of iterations through `accept-process-output'
that `efs-pty-p' waits before deciding that the pty is really a pipe.
Set this to nil to inhibit checking for pty's. If efs seems to be
mistaking some pty's for pipes, try increasing this number.")

(defvar efs-pty-check-retry-time 5
  "*Number of seconds that efs waits before retrying a pty check.
This can be lengthened, if your FTP client is slow to start.")

(defvar efs-suppress-abort-recursive-edit-and-then nil
  "*If non-nil, `efs-abort-recursive-edit-and-then' will not run its function.
This means that when a recursive edit is in progress, automatic popping of the
FTP process buffer, and automatic popping of the bug report buffer will not
work. `efs-abort-recursive-edit-and-then' works by forking a \"sleep 0\"
process. On some unix implementations the forked process might be of the same
size as the original GNU Emacs process. Forking such a large process just to
do a \"sleep 0\" is probably not good.")

(defvar efs-ftp-buffer-format "*ftp %s@%s*"
  "Format to construct the name of FTP process buffers.
This string is fed to `format' with second and third arguments the user
name and host name.")
;; This does not affect the process name of the FTP client process.
;; That is always *ftp USER@HOST*

(defvar efs-debug-ftp-connection nil
  "*If non-nil, the user will be permitted to debug the FTP connection.
This means that typing a C-g to the FTP process filter will give the user
the option to type commands at the FTP connection.  Normally, the connection
is killed first.  Note that doing this may result in the FTP process filter
getting out of synch with the FTP client, so using this feature routinely
isn't recommended.")

;;; Hooks and crooks.

(defvar efs-ftp-startup-hook nil
  "Hook to run immediately after starting the FTP client.
This hook is run before the FTP OPEN command is sent.")

(defvar efs-ftp-startup-function-alist nil
  "Association list of functions to running after FTP login.
This should be an alist of the form '\(\(REGEXP . FUNCTION\) ...\), where
REGEXP is a regular expression matched against the name of the remote host,
and FUNCTION is a function of two arguments, HOST and USER. REGEXP is
compared to the host name with `case-fold-search' bound to t. Only the first
match in the alist is run.")

(defvar efs-load-hook nil
  "Hook to run immediately after loading efs.el.
You can use it to alter definitions in efs.el, but why would you want 
to do such a thing?")

;;;; -----------------------------------------------------------
;;;; Regexps for parsing FTP server responses.
;;;; -----------------------------------------------------------
;;;
;;;  If you have to tune these variables, please let us know, so that
;;;  we can get them right in the next release.

(defvar efs-multi-msgs
  ;; RFC959 compliant codes
  "^[1-5][0-5][0-7]-")
;; Regexp to match the start of an FTP server multiline reply.

(defvar efs-skip-msgs
  ;; RFC959 compliant codes
  (concat
   "^110 \\|" ; Restart marker reply.
   "^125 \\|" ; Data connection already open; transfer starting.
   "^150 ")) ; File status OK; about to open connection.
;; Regexp to match an FTP server response which we wish to ignore.

(defvar efs-cmd-ok-msgs
  ;; RFC959 compliant
  "^200 \\|^227 ")
;; Regexp to match the server command OK response.
;; Because PORT commands return this we usually ignore it. However, it is
;; a valid response for TYPE, SITE, and a few other commands (cf. RFC 959).
;; If we are explicitly sending a PORT, or one of these other commands, 
;; then we don't want to ignore this response code.  Also use this to match
;; the return code for PASV, as some clients burp these things out at odd
;; times.

(defvar efs-pending-msgs
  ;; RFC959 compliant
  "^350 ") ; Requested file action, pending further information.
;; Regexp to match the \"requested file action, pending further information\"
;; message. These are usually ignored, except if we are using RNFR to test for
;; file existence.

(defvar efs-cmd-ok-cmds
  (concat
   "^quote port \\|^type \\|^quote site \\|^chmod \\|^quote noop\\|"
   "^quote pasv"))
;; Regexp to match commands for which efs-cmd-ok-msgs is a valid server
;; response for success.

(defvar efs-passwd-cmds
  "^quote pass \\|^quote acct \\|^quote site gpass ")
;; Regexp to match commands for sending passwords.
;; All text following (match-end 0) will be replaced by "Turtle Power!"

(defvar efs-bytes-received-msgs
  ;; Strictly a client response
  "^[0-9]+ bytes ")
;; Regexp to match the reply from the FTP client that it has finished
;; receiving data.

(defvar efs-server-confused-msgs
  ;; ka9q uses this to indicate an incorrectly set transfer mode, and
  ;; then does send a second completion code for the command. This does
  ;; *not* conform to RFC959.
  "^100 Warning: type is ")
;; Regexp to match non-standard response from the FTP server. This can 
;; sometimes be the result of an incorrectly set transfer mode. In this case
;; we do not rely on the server to tell us when the data transfer is complete,
;; but check with the client.

(defvar efs-good-msgs
  (concat
   ;; RFC959 compliant codes
   "^2[01345][0-7] \\|" ; 2yz = positive completion reply
   "^22[02-7] \\|"      ; 221 = successful logout
			; (Sometimes get this with a timeout,
			; so treat as fatal.)
   "^3[0-5][0-7] \\|"    ; 3yz = positive intermediate reply
   ;; client codes
   "^[Hh]ash mark "))
;; Response to indicate that the requested action was successfully completed.

(defvar efs-failed-msgs
  (concat
   ;; RFC959 compliant codes
   "^120 \\|"       ; Service ready in nnn minutes.
   "^450 \\|"       ; File action not taken; file is unavailable, or busy.
   "^452 \\|"       ; Insufficient storage space on system.
   "^5[0-5][0-7] \\|" ; Permanent negative reply codes.
   ;; When clients tell us that	a file doesn't exist, or can't access.
   "^\\(local: +\\)?/[^ ]* +"
   "\\([Nn]o such file or directory\\|[Nn]ot a plain file\\|"
   "The file access permissions do not allow \\|Is a directory\\b\\)"))
;; Regexp to match responses for failed commands. However, the ftp connection
;; is assumed to be good.

(defvar efs-fatal-msgs
  (concat
   ;; RFC959 codes
   "^221 \\|" ; Service closing control connection.
   "^421 \\|" ; Service not available.
   "^425 \\|" ; Can't open data connection.
   "^426 \\|" ; Connection closed, transfer aborted.
   "^451 \\|" ; Requested action aborted, local error in processing.
   ;; RFC959 non-compliant codes
   "^552 Maximum Idle Time Exceded\\.$\\|" ; Hellsoft server uses this to
					   ; indicate a timeout. 552 is
					   ; supposed to be used for exceeded
					   ; storage allocation. Note that
					   ; they also misspelled the error
					   ; message.
   ;; client problems
   "^ftp: \\|^Not connected\\|^rcmd: \\|^No control connection\\|"
   "^unknown host\\|: unknown host$\\|^lost connection\\|"
   "^[Ss]egmentation fault\\|"
   ;; Make sure that the "local: " isn't just a message about a file.
   "^local: [^/]\\|"
   ;; Gateways
   "^iftp: cannot authenticate to server\\b"
   ))
;; Regexp to match responses that something has gone drastically wrong with
;; either the client, server, or connection. We kill the ftp process, and start
;; anew.

(defvar efs-unknown-response-msgs
  "^[0-9][0-9][0-9] ")
;; Regexp to match server response codes that we don't understand. This
;; is tested after all the other regexp, so it can match everything.

(defvar efs-pasv-msgs
  ;; According to RFC959.
  "^227 .*(\\([0-9]+,[0-9]+,[0-9]+,[0-9]+,[0-9]+,[0-9]+\\))$")
;; Matches the output of a PASV. (match-beginning 1) and (match-end 1)
;; must bracket the IP address and port.

(defvar efs-syst-msgs "^215 \\|^210 ")
;; 215 is RFC959. Plan 9 FTP server returns a 210. 210 is not assigned in
;; RFC 959.
;; The plan 9 people tell me that they fixed this. -- sr 18/4/94
;; Matches the output of a SYST.

(defvar efs-mdtm-msgs
  (concat
   "^213 [0-9][0-9][0-9][0-9][0-9][0-9][0-9]"
   "[0-9][0-9][0-9][0-9][0-9][0-9][0-9]$"))
;; Regexp to match the output of a quote mdtm command.

(defvar efs-idle-msgs
  "^200 [^0-9]+ \\([0-9]+\\)[^0-9]* max \\([0-9]+\\)")
;; Regexp to match the output of a SITE IDLE command.
;; Match 1 should refer to the current idle time, and match 2 the maximum 
;; idle time.

(defvar efs-write-protect-msgs "^532 ") ; RFC959
;; Regexp to match a server ressponse to indicate that a STOR failed
;; because of insufficient write privileges.

(defvar efs-hash-mark-msgs
  "[hH]ash mark [^0-9]*\\([0-9]+\\)")
;; Regexp matching the FTP client's output upon doing a HASH command.

(defvar efs-xfer-size-msgs
  (concat
   ;; UN*X
   "^150 .* connection for .* (\\([0-9]+\\) bytes)\\|"
   ;; Wollongong VMS server.
   "^125 .* transfer started for .* (\\([0-9]+\\) bytes)\\|"
   ;; TOPS-20 server
   "^150 .* retrieve of .* ([0-9]+ pages?, \\([0-9]+\\) 7-bit bytes)"))
;; Regular expression used to determine the number of bytes
;; in a FTP transfer. The first (match-beginning #) which is non-nil is assumed
;; to give the size.

(defvar efs-expand-dir-msgs "^550 \\([^: ]+\\):")
;; Regexp to match the error response from a "get ~sandy".
;; By parsing the error, we can get a quick expansion of ~sandy
;; According to RFC 959, should be a 550.

(defvar efs-gateway-fatal-msgs
  "No route to host\\|Connection closed\\|No such host\\|Login incorrect")
;; Regular expression matching messages from the rlogin / telnet process that
;; indicates that logging in to the gateway machine has gone wrong.

(defvar efs-too-many-users-msgs
  ;; The test for "two many" is because some people can't spell.
  ;; I allow for up to two adjectives before "users".
  (concat
   "\\b[Tt][wo]o many\\( +[^ \n]+\\)?\\( +[^ \n]+\\)? +users\\b\\|"
   "\\btry back later\\b"))
;; Regular expresion to match what servers output when there are too many
;; anonymous logins.  It is assumed that this is part of a 530 or 530- response
;; to USER or PASS.

;;;; -------------------------------------------------------------
;;;; Buffer local FTP process variables
;;;; -------------------------------------------------------------

;;; Variables buffer local to the process buffers are
;;; named with the prefix efs-process-

(defvar efs-process-q nil)
;; List of functions to be performed asynch.
(make-variable-buffer-local 'efs-process-q)

(defvar efs-process-cmd-waiting nil)
;; Set to t if a process has a synchronous cmd waiting to execute.
;; In this case, it will allow the synch. cmd to run before returning to
;;  the cmd queue.
(make-variable-buffer-local 'efs-process-cmd-waiting)

(defvar efs-process-server-confused nil)
(make-variable-buffer-local 'efs-process-server-confused)

(defvar efs-process-cmd nil)
;; The command currently being executed, as a string.
(make-variable-buffer-local 'efs-process-cmd)

(defvar efs-process-xfer-size 0)
(make-variable-buffer-local 'efs-process-xfer-size)

(defvar efs-process-umask nil)
;; nil if the umask hash not been set
;; an integer (the umask) if the umask has been set
(make-variable-buffer-local 'efs-process-umask)

(defvar efs-process-idle-time nil)
;; If non-nil, the idle time of the server in seconds.
(make-variable-buffer-local 'efs-process-idle-time)

(defvar efs-process-busy nil)
(make-variable-buffer-local 'efs-process-busy)

(defvar efs-process-result-line "")
(make-variable-buffer-local 'efs-process-result-line)

(defvar efs-process-result nil)
(make-variable-buffer-local 'efs-process-result)

(defvar efs-process-result-cont-lines "")
(make-variable-buffer-local 'efs-process-result-cont-lines)

(defvar efs-process-msg "")
(make-variable-buffer-local 'efs-process-msg)

(defvar efs-process-nowait nil)
(make-variable-buffer-local 'efs-process-nowait)

(defvar efs-process-string "")
(make-variable-buffer-local 'efs-process-string)

(defvar efs-process-continue nil)
(make-variable-buffer-local 'efs-process-continue)

(defvar efs-process-hash-mark-count 0)
(make-variable-buffer-local 'efs-process-hash-mark-count)

(defvar efs-process-hash-mark-unit nil)
(make-variable-buffer-local 'efs-process-hash-mark-unit)

(defvar efs-process-last-percent -1)
(make-variable-buffer-local 'efs-process-last-percent)

(defvar efs-process-host nil)
(make-variable-buffer-local 'efs-process-host)

(defvar efs-process-user nil)
(make-variable-buffer-local 'efs-process-user)

(defvar efs-process-host-type nil)
;; Holds the host-type as a string, for showing it on the mode line.
(make-variable-buffer-local 'efs-process-host-type)

(defvar efs-process-xfer-type nil)
;; Set to one of 'ascii, 'ebcdic, 'image, 'tenex, or nil to indicate
;; the current setting of the transfer type for the connection. nil means
;; that we don't know.
(make-variable-buffer-local 'efs-process-xfer-type)

(defvar efs-process-client-altered-xfer-type nil)
;; Sometimes clients alter the xfer type, such as doing
;; an ls it is changed to ascii. If we are using quoted commands
;; to do xfers the client doesn't get a chance to set it back.
(make-variable-buffer-local 'efs-process-client-altered-xfer-type)

(defvar efs-process-prompt-regexp nil)
;; local value of prompt of FTP client.
(make-variable-buffer-local 'efs-process-prompt-regexp)

(defvar efs-process-cmd-counter 0)
;; Counts FTP commands, mod 16.
(make-variable-buffer-local 'efs-process-cmd-counter)

;;;; ------------------------------------------------------------
;;;; General Internal Variables.
;;;; ------------------------------------------------------------

;;; For the byte compiler
;;
;;  These variables are usually unbound.  We are just notifying the
;;  byte compiler that we know what we are doing.

(defvar bv-length) ; getting file versions.
(defvar default-file-name-handler-alist) ; for file-name-handler-alist
(defvar efs-completion-dir) ; for file name completion predicates
(defvar dired-directory) ; for default actions in interactive specs
(defvar dired-local-variables-file) ; for inhibiting child look ups
(defvar dired-in-query) ; don't clobber dired queries with stat messages
(defvar after-load-alist) ; in case we're in emacs 18.
(defvar comint-last-input-start)
(defvar comint-last-input-end)
(defvar explicit-shell-file-name)

;;; fluid vars

(defvar efs-allow-child-lookup t)
;; let-bind to nil, if want to inhibit child lookups.

(defvar efs-nested-cmd nil)
;; let-bound to t, when a cmd is executed by a cont or pre-cont.
;; Such cmds will never end by looking at the next item in the queue,
;; if they are run synchronously, but rely on their calling function
;; to do this.

;;; polling ftp buffers

(defvar efs-ftp-buffer-poll-time 300
  "Period, in seconds, which efs will poll ftp buffers for activity.
Used for expiring \(killing\) inactive ftp buffers.")

(defconst efs-ftp-buffer-alist nil)
;; alist of ftp buffers, and the total number of seconds that they
;; have been idle.

;;; load extensions

(defvar efs-load-lisp-extensions '(".elc" ".el" "")
  "List of extensions to try when loading lisp files.")

;;; mode-line

(defvar efs-mode-line-string "")
;; Stores the string that efs displays on the mode line.

;;; data & temporary buffers

(defvar efs-data-buffer-name " *ftp data*")
;; Buffer name to hold directory listing data received from ftp process.

(defvar efs-data-buffer-name-2 " *ftp data-2*")
;; A second buffer name in which to hold directory listings.
;; Used for listings which are made during another directory listing.

;;; process names

(defvar efs-ctime-process-name-format "*efs ctime %s*")
;; Passed to format with second arg the host name.

;;; For temporary files.

;; This is a list of symbols.
(defconst efs-tmp-name-files ())
;; Here is where these symbols live:
(defconst efs-tmp-name-obarray (make-vector 7 0))
;; We put our version of the emacs PID here:
(defvar efs-pid nil)

;;; For abort-recursive-edit

(defvar efs-abort-recursive-edit-data nil)
(defvar efs-abort-recursive-edit-delay 5)
;; Number of seconds after which efs-abort-recursive-edit-and-then
;; will decide not to runs its sentinel. The assumption is that something
;; went wrong.

;;; hashtables (Use defconst's to clobber any user silliness.)

(defconst efs-files-hashtable (efs-make-hashtable 97))
;; Hash table for storing directories and their respective files.

(defconst efs-expand-dir-hashtable (efs-make-hashtable))
;; Hash table of tilde expansions for remote directories.

(defconst efs-ls-converter-hashtable (efs-make-hashtable 37))
;; Hashtable for storing functions to convert listings from one
;; format to another.  Keys are the required switches, and the values
;; are alist of the form ((SWITCHES . CONVERTER)...) where is SWITCHES
;; are the listing switches for the original listing, and CONVERTER is a
;; function of one-variable, the listing-type, to do the conversion
;; on data in the current buffer. SWITCHES is either a string, or nil.
;; nil means that the listing can be converted from cache in
;; efs-files-hashtable, a string from cache in efs-ls-cache.  For the latter,
;; listings with no switches (dumb listings), represent SWITCHES as a string
;; consisting only of the ASCII null character.

;;; cache variables (Use defconst's to clobber any user sillines.)

(defconst efs-ls-cache nil
  "List of results from efs-ls.
Each entry is a list of four elements, the file listed, the switches used
\(nil if none\), the listing string, and whether this string has already been
parsed.")

(defvar efs-ls-uncache nil)
;; let-bind this to t, if you want to be sure that efs-ls will replace any
;; cache entries.

;; This is a cache to see if the user has changed
;; completion-ignored-extensions.
(defconst efs-completion-ignored-extensions completion-ignored-extensions
  "This variable is internal to efs. Do not set.
See completion-ignored-extensions, instead.")

;; We cache the regexp we use for completion-ignored-extensions. This
;; saves building a string every time we do completion. String construction
;; is costly in emacs.
(defconst efs-completion-ignored-pattern
  (mapconcat (function
	      (lambda (s) (if (stringp s)
			      (concat (regexp-quote s) "$")
			    "/"))) ; / never in filename
	     efs-completion-ignored-extensions
	     "\\|")
  "This variable is internal to efs. Do not set.
See completion-ignored-extensions, instead.")

(defvar efs-system-fqdn nil
  "Cached value of the local systems' fully qualified domain name.")

;;; The file-type-alist

;; efs-file-type-alist is an alist indexed by host-type
;; which stores data on how files are structured on the given
;; host-type. Each entry is a list of three elements. The first is the
;; definition of a `byte', the second the native character representation,
;; and the third, the file structure.
;;
;; Meanings of the symbols:
;; ------------------------
;; The byte symbols:
;; 8-bit     = bytes of 8-bits
;; 36-bit-wa = 36-bit word aligned. Precisely, the addressing unit is that
;;             of a PDP-10 using the "<440700,,0> byte pointer".
;;
;; The native character set symbols:
;; 8-ascii = 8-bit NVT-ASCII
;; 7-ascii = 7-bit ascii as on a PDP-10
;; ebcdic  = EBCDIC as on an IBM mainframe
;; lispm   = the native character set on a lispm (Symbolics and LMI)
;; mts     = native character representation in the Michigan Terminal System
;;           (which runs on IBM and Amdal mainframes), similar to ebcdic
;;
;; The file structure symbols:
;;
;; file-nl    = data is stored as a contiguous sequence of data bytes
;;              with EOL denoted by <NL>.
;; file-crlf  = data is stored as a contiguous sequence of data bytes
;;              with EOL denoted by <CR-LF>
;; record     = data is stored as a sequence of records
;; file-lispm = data as stored on a lispm. i.e. a sequence of bits
;;              with EOL denoted by character code 138 (?)
;;
;; If we've messed anything up here, please let us know.

(defvar efs-file-type-alist
  '((unix . (8-bit 8-ascii file-nl))
    (sysV-unix . (8-bit 8-ascii file-nl))
    (bsd-unix . (8-bit 8-ascii file-nl))
    (apollo-unix . (8-bit 8-ascii file-nl))
    (dumb-apollo-unix . (8-bit 8-ascii file-nl))
    (dumb-unix . (8-bit 8-ascii file-nl))
    (super-dumb-unix . (8-bit 8-ascii file-nl))
    (guardian . (8-bit ascii file-nl))
    (plan9 . (8-bit 8-ascii file-nl))
    (dos . (8-bit 8-ascii file-crlf))
    (ms-unix . (8-bit 8-ascii file-crlf))
    (netware . (8-bit 8-ascii file-crlf))
    (os2 . (8-bit 8-ascii file-crlf))
    (tops-20 . (36-bit-wa 7-ascii file-crlf))
    (mpe . (8-bit 8-ascii record))
    (mvs . (8-bit ebcdic record))
    (cms . (8-bit ebcdic record))
    (cms-knet . (8-bit ebcdic record))
    (mts . (8-bit mts record)) ; mts seems to have its own char rep.
			       ; Seems to be close to ebcdic, but not the same.
    (dos-distinct . (8-bit 8-ascii file-crlf))
    (ka9q . (8-bit 8-ascii file-crlf))
    (vms . (8-bit 8-ascii record)) ; The mysteries of VMS's RMS.
    (hell . (8-bit 8-ascii file-crlf))
    (vos . (8-bit 8-ascii record))
    (ti-explorer . (8-bit lispm file-lispm)) ; lispms use a file structure, but
					     ; use an out of range char to
					     ; indicate EOL.
    (ti-twenex . (8-bit lispm file-lispm))
    (nos-ve . (8-bit 8-ascii record))
    (coke . (8-bit 8-ascii file-nl)) ; only support 8-bit beverages
    (nil . (8-bit 8-ascii file-nl)))) ; the local host

;;; Status messages

(defvar efs-last-message-time -86400) ; yesterday
;; The time of the last efs status message. c.f. efs-message-interval

;;; For handling dir listings

;; This MUST match all the way to to the start of the filename.
;; This version corresponds to what dired now uses (sandy, 14.1.93)
(defvar efs-month-and-time-regexp
  (concat
   " \\([0-9]+\\) +" ; file size
   "\\(Jan\\|Feb\\|Mar\\|Apr\\|May\\|June?\\|July?\\|Aug\\|Sep\\|Oct"
					; June and July are for HP-UX 9.0
   "\\|Nov\\|Dec\\) \\([ 0-3][0-9]\\)\\("
   " [012][0-9]:[0-6][0-9] \\|"  ; time
   "  [12][90][0-9][0-9] \\|"    ; year on IRIX, NeXT, SunOS, ULTRIX, Apollo
				 ; HP-UX, A/UX
   " [12][90][0-9][0-9]  \\)"    ; year on AIX
   ))

(defvar efs-month-alist
  '(("Jan" . 1) ("Feb". 2) ("Mar" . 3) ("Apr" . 4) ("May" . 5) ("Jun" . 6)
    ("June" . 6) ("Jul" . 7) ("July" . 7) ("Aug" . 8) ("Sep" . 9) ("Oct" . 10)
    ("Nov" . 11) ("Dec" . 12)))

;; Matches the file modes, link number, and owner string.
;; The +/- is for extended file access permissions.
(defvar efs-modes-links-owner-regexp
  (concat
   "\\([^ ][-r][-w][^ ][-r][-w][^ ][-r][-w][^ ]\\)[-+]? *\\([0-9]+\\)"
   " +\\([^ ]+\\) "))
  
;;;; ---------------------------------------------------------------
;;;; efs-dired variables
;;;; ---------------------------------------------------------------

;; These variables must be here, instead of in efs-dired.el, because
;; the efs-HOST-TYPE.el files need to add to it.
(defvar efs-dired-re-exe-alist nil
  "Association list of regexps which match file lines of executable files.")

(defvar efs-dired-re-dir-alist nil
  "Association list of regexps which match file lines of subdirectories.")

(defvar efs-dired-host-type nil
  "Host type of a dired buffer. \(buffer local\)")
(make-variable-buffer-local 'efs-dired-host-type)

(defvar efs-dired-listing-type nil
  "Listing type of a dired buffer. \(buffer local\)")
(make-variable-buffer-local 'efs-dired-listing-type)

(defvar efs-dired-listing-type-string nil)
(make-variable-buffer-local 'efs-dired-listing-type-string)

;;;; -------------------------------------------------------------
;;;; New error symbols.
;;;; -------------------------------------------------------------

(put 'ftp-error 'error-conditions '(ftp-error file-error error))
;; (put 'ftp-error 'error-message "FTP error")


;;;; =============================================================
;;;; >3
;;;; Utilities
;;;; =============================================================

;;; -------------------------------------------------------------------
;;; General Macros (Make sure that macros are defined before they're
;;;                 used, for the byte compiler.
;;; -------------------------------------------------------------------

(defmacro efs-kbd-quit-protect (proc &rest body)
  ;; When an efs function controlling an FTP connection gets a kbd-quit
  ;; this tries to make sure that everything unwinds consistently.
  (let ((temp (make-symbol "continue")))
    (list 'let
	  (list '(quit-flag nil)
		'(inhibit-quit nil)
		(list temp t))
	  (list
	   'while temp
	   (list 'setq temp nil)
	   (list
	    'condition-case nil
	    (cons 'progn
		  body)
	    (list 'quit
		  (list 'setq temp
			(list 'efs-kbd-quit-protect-cover-quit proc))))))))

(defun efs-kbd-quit-protect-cover-quit (proc)
  ;; This function exists to keep the macro expansion of the
  ;; efs-kbd-quit-protect down to a reasonable size.
  (let ((pop-up-windows t)
	(buff (get-buffer (process-buffer proc)))
	res)
    (if (save-window-excursion
	  (if buff
	      (progn
		(pop-to-buffer buff)
		(goto-char (point-max))
		(recenter (- (window-height)
			     2))))
	  (setq res (efs-kill-ftp-buffer-with-prompt proc buff)))
	(progn
	  (if (eq res 0)
	      (if (eq (selected-window)
		      (minibuffer-window))
		  (efs-abort-recursive-edit-and-then
		   (function
		    (lambda (buff)
		      (if (get-buffer buff)
			  (display-buffer buff))))
		   buff)
		(if (get-buffer buff)
		    (display-buffer buff))
		(signal 'quit nil))
	    (if (eq (selected-window) (minibuffer-window))
		(abort-recursive-edit)
	      (signal (quote quit) nil)))
	  nil)
      (sit-for 0)
      (message "Waiting on %s..." (or (car (efs-parse-proc-name proc))
				      "a whim"))
      t)))

(put 'efs-kbd-quit-protect 'lisp-indent-hook 1)

(defmacro efs-save-buffer-excursion (&rest forms)
  "Execute FORMS, restoring the current buffer afterwards.
Unlike, save-excursion, this does not restore the point."
  (let ((temp (make-symbol "saved-buff")))
    (list 'let
	  (list (list temp '(current-buffer)))
	  (list 'unwind-protect
		(cons 'progn forms)
		(list 'condition-case nil
		      (list 'set-buffer temp)
		      '(error nil))))))

(put 'efs-save-buffer-excursion 'lisp-indent-hook 0)

(defmacro efs-unquote-dollars (string)
  ;; Unquote $$'s to $'s in STRING.
  (` (let ((string (, string))
	   (start 0)
	   new)
       (while (string-match "\\$\\$" string start)
	 (setq new (concat new (substring
				string start (1+ (match-beginning 0))))
	       start (match-end 0)))
       (if new
	   (concat new (substring string start))
	 string))))

(defmacro efs-get-file-part (path)
  ;; Given PATH, return the file part used for looking up the file's entry
  ;; in a hashtable.
  ;; This need not be the same thing as file-name-nondirectory.
  (` (let ((file (file-name-nondirectory (, path))))
       (if (string-equal file "")
	   "."
	 file))))

(defmacro efs-ftp-path-macro (path)
  ;; Just a macro version of efs-ftp-path, for speed critical
  ;; situations. Could use (inline ...) instead, but not everybody
  ;; uses the V19 byte-compiler. Also, doesn't call efs-save-match-data,
  ;; but assumes that the calling function does it.
  (`
   (let ((path (, path)))
     (or (string-equal path efs-ftp-path-arg)
	 (setq efs-ftp-path-res
	       (and (string-match efs-path-regexp path)
		    (let ((host (substring path (match-beginning 2)
					   (match-end 2)))
			  (user (and (match-beginning 1)
				     (substring path (match-beginning 1)
						(1- (match-end 1)))))
			  (rpath (substring path (1+ (match-end 2)))))
		      (list (if (string-equal host "")
				(setq host (system-name))
			      host)
			    (or user (efs-get-user host))
			    rpath)))
	       ;; Set this last, in case efs-get-user calls this function,
	       ;; which would modify an earlier setting.
	       efs-ftp-path-arg path))
     efs-ftp-path-res)))

(defmacro efs-canonize-switches (switches)
  ;; Converts a switches string, into a lexographically ordered string,
  ;; omitting - and spaces.  Should we remove duplicate characters too?
  (` (if (, switches)
	 (mapconcat
	  'char-to-string
	  (sort (delq ?- (delq ?\  (mapcar 'identity (, switches)))) '<) "")
       ;; For the purpose of interning in a hashtable, represent the nil
       ;; switches, as a string consisting of the ascii null character.
       (char-to-string 0))))

(defmacro efs-canonize-file-name (fn)
  ;; Canonizes the case of file names.
  (` (let ((parsed (efs-ftp-path (, fn))))
       (if parsed
	   (let ((host (car parsed)))
	     (if (memq (efs-host-type host) efs-case-insensitive-host-types)
		 (downcase (, fn))
	       (format efs-path-format-string (nth 1 parsed) (downcase host)
		       (nth 2 parsed))))
	 (, fn)))))

(defmacro efs-get-files-hashtable-entry (fn)
  (` (efs-get-hash-entry (efs-canonize-file-name (, fn)) efs-files-hashtable)))

;;;; ------------------------------------------------------------
;;;; Utility Functions
;;;; ------------------------------------------------------------

(defun efs-kill-ftp-buffer-with-prompt (proc buffer)
  ;; Does a 3-way prompt to kill a ftp PROC and BUFFER.
  ;; Returns t if buffer was killed, 0 if only process, nil otherwise.
  (let ((inhibit-quit t)
	(cursor-in-echo-area t)
	char)
    (message
     (if efs-debug-ftp-connection
	 "Kill ftp process and buffer (y[es], n[o], c[lose], d[ebug] ) "
       "Kill ftp process and buffer? (y or n, c to only close process) "))
    (setq char (read-char))
    (prog1
	(cond
	 ((memq char '(?y ?Y ?\ ))
	  (set-process-sentinel proc nil)
	  (condition-case nil
	      (kill-buffer buffer)
	    (error nil))
	  t)
	 ((memq char '(?c ?C))
	  (set-process-sentinel proc nil)
	  (condition-case nil
	      (save-excursion
		(set-buffer buffer)
		(setq efs-process-busy nil
		      efs-process-q nil)
		(delete-process proc))
	    (error nil))
	  0)
	 ((memq char '(?n ?N))
	  (message "")
	  nil)
	 ((and efs-debug-ftp-connection
	       (memq char '(?d ?D)))
	  (condition-case nil
	      (save-excursion
		(set-buffer buffer)
		(setq efs-process-busy nil
		      efs-process-q nil))
	    (error nil))
	  0)
	 (t
	  (message
	   (if efs-debug-ftp-connection
	       "Type one of y, n, c or d."
	     "Type one of y, n or c."))
	  (ding)
	  (sit-for 1)
	  (setq quit-flag nil)
	  (efs-kill-ftp-buffer-with-prompt proc buffer))))))

(defun efs-barf-if-not-directory (directory)
  ;; Signal an error if DIRECTORY is not one.
  (or (file-directory-p directory)
      (signal 'file-error
	      (list "Opening directory"
		    (if (file-exists-p directory)
			"not a directory"
		      "no such file or directory")
		    directory))))

(defun efs-call-cont (cont &rest args)
  "Call the function specified by CONT.
CONT can be either a function or a list of a function and some args.
The first parameters passed to the function will be ARGS.  The remaining
args will be taken from CONT if a list was passed."
  (if cont
      (let ((efs-nested-cmd t)) ; let-bound so that conts don't pop any queues
	(efs-save-buffer-excursion
	  (if (and (listp cont)
		   (not (eq (car cont) 'lambda)))
	      (apply (car cont) (append args (cdr cont)))
	    (apply cont args))))))

(defun efs-replace-path-component (fullpath path)
  "For FULLPATH matching efs-path-regexp replace the path component with PATH."
  (efs-save-match-data
    (if (string-match efs-path-root-regexp fullpath)
	(concat (substring fullpath 0 (match-end 0)) path)
      path)))

(defun efs-abort-recursive-edit-and-then (fun &rest args)
  ;; Does an abort-recursive-edit, and runs fun _after_ emacs returns to
  ;; top level.
  (if (get-process "efs-abort-recursive-edit")
      ;; Don't queue these things. Clean them out.
      (delete-process "efs-abort-recursive-edit"))
  (or efs-suppress-abort-recursive-edit-and-then
      (progn
	(setq efs-abort-recursive-edit-data (cons (nth 1 (current-time))
						  (cons fun args)))
	(condition-case nil
	    (set-process-sentinel
	     (let ((default-directory exec-directory)
		   (process-connection-type nil))
	       (start-process "efs-abort-recursive-edit" nil "sleep" "0"))
	     (function
	      (lambda (proc string)
		(let ((data efs-abort-recursive-edit-data))
		  (setq efs-abort-recursive-edit-data)
		  (if (and data
			   (integerp (car data))
			   (<= (- (nth 1 (current-time)) (car data))
			       efs-abort-recursive-edit-delay))
		      (apply (nth 1 data) (nthcdr 2 data)))))))
	  (error nil))))
  (abort-recursive-edit))

(defun efs-occur-in-string (char string)
  ;; Return the number of occurrences of CHAR in STRING.
  (efs-save-match-data
    (let ((regexp (regexp-quote (char-to-string char)))
	  (count 0)
	  (start 0))
      (while (string-match regexp string start)
	(setq start (match-end 0)
	      count (1+ count)))
      count)))

(defun efs-parse-proc-name (proc)
  ;; Parses the name of process to return a list \(host user\).
  (efs-save-match-data
    (let ((name (process-name proc)))
      (and name
	   (string-match "^\\*ftp \\([^@]*\\)@\\([^*]+\\)\\*$" name)
	   (list (substring name (match-beginning 2) (match-end 2))
		 (substring name (match-beginning 1) (match-end 1)))))))

;;;; ------------------------------------------------------------
;;;; Of Geography, connectivity, and the internet... Gateways.
;;;; ------------------------------------------------------------

(defun efs-use-gateway-p (host &optional opaque-p)
;; Returns whether to access this host via a gateway.
;; Returns the gateway type as a symbol.  See efs-gateway-type <V>.
;; If optional OPAQUE-P is non-nil, only returns non-nil if the gateway
;; type is in the list efs-opaque-gateways <V>.
  (and efs-gateway-type
       host  ;local host is nil
       (efs-save-match-data
	 (and (not (string-match efs-ftp-local-host-regexp host))
	      (let ((type (car efs-gateway-type)))
		(if opaque-p
		    (and (memq type efs-opaque-gateways) type)
		  type))))))

(defun efs-local-to-gateway-filename (filename &optional reverse)
  ;; Converts a FILENAME on the local host to its name on the gateway,
  ;; using efs-gateway-mounted-dirs-alist. If REVERSE is non-nil, does just
  ;; that. If the there is no corresponding name because non of its parent
  ;; directories are mounted, returns nil.
  (if efs-gateway-mounted-dirs-alist
      (let ((len (length filename))
	    (alist efs-gateway-mounted-dirs-alist)
	    result elt elt-len)
	(if reverse
	    (while (setq elt (car alist))
	      (if (and (>= len (setq elt-len (length (cdr elt))))
		       (string-equal (cdr elt) (substring filename 0 elt-len)))
		  (setq result (concat (car elt)
				       (substring filename elt-len))
			alist nil)
		(setq alist (cdr alist))))
	  (while (setq elt (car alist))
	    (if (and (>= len (setq elt-len (length (car elt))))
		     (string-equal (car elt) (substring filename 0 elt-len)))
		(setq result (concat (cdr elt)
				     (substring filename elt-len))
		      alist nil)
	      (setq alist (cdr alist)))))
	result)))

;;; ------------------------------------------------------------
;;; Enhanced message support.
;;; ------------------------------------------------------------

(defun efs-message (fmt &rest args)
  "Output the given message, truncating to the size of the minibuffer window."
  (let ((msg (apply (function format) fmt args))
	(max (window-width (minibuffer-window))))
    (if (>= (length msg) max)
	(setq msg (concat "> " (substring msg (- 3 max)))))
    (message "%s" msg)))

(defun efs-message-p ()
  ;; Returns t, if efs is allowed to display a status message.
  (not
   (or (and (boundp 'dired-in-query) dired-in-query)
       (boundp 'search-message)
       cursor-in-echo-area
       (and (/= efs-message-interval 0)
	    (let ((diff (- efs-last-message-time
			   (setq efs-last-message-time
				 (nth 1 (current-time))))))
	      (and
	       (> diff (- efs-message-interval))
	       (< diff 0))))))) ; in case the clock wraps.

(efs-define-fun efs-relativize-filename (file &optional dir new)
  "Abbreviate the given filename relative to DIR .
If DIR is nil, use the value of `default-directory' for the currently selected
window. If the optional parameter NEW is given and the 
non-directory parts match, only return the directory part of the file."
  (let* ((dir (or dir (save-excursion
			(set-buffer (window-buffer (selected-window)))
			default-directory)))
	 (dlen (length dir))
	 (result file))
    (and (> (length file) dlen)
	 (string-equal (substring file 0 dlen) dir)
	 (setq result (substring file dlen)))
    (and new
	 (string-equal (file-name-nondirectory result)
		       (file-name-nondirectory new))
	 (or (setq result (file-name-directory result))
	     (setq result "./")))
    (abbreviate-file-name result)))

;;; ------------------------------------------------------------
;;; Temporary file location and deletion...
;;; ------------------------------------------------------------

(defun efs-get-pid ()
  ;; Half-hearted attempt to get the current process's id.
  (setq efs-pid (substring (make-temp-name "") 1)))

(defun efs-make-tmp-name (host1 host2)
  ;; Returns the name of a new temp file, for moving data between HOST1
  ;; and HOST2. This temp file must be directly accessible to the
  ;; FTP client connected to HOST1. Using nil for either HOST1 or
  ;; HOST2 means the local host. The return value is actually a list
  ;; whose car is the name of the temp file wrto to the local host
  ;; and whose cdr is the name of the temp file wrto to the host
  ;; on which the client connected to HOST1 is running. If the gateway
  ;; is only accessible by FTP, then the car of this may be in efs extended
  ;; file name syntax.
  (let ((pid (or efs-pid (efs-get-pid)))
	(start ?a)
	file entry template rem-template template-len)
    ;; Compute the templates.
    (if (null (and host1 (efs-use-gateway-p host1 t)))
	;; file must be local
	(if (null (and host2 (efs-use-gateway-p host2 t)))
	    (setq template efs-tmp-name-template)
	  (setq template (or (efs-local-to-gateway-filename
			      efs-gateway-tmp-name-template t)
			     efs-tmp-name-template)))
      ;; file must be on the gateway -- make sure that the gateway
      ;; configuration is sensible.
      (efs-save-match-data
	(or (string-match efs-ftp-local-host-regexp efs-gateway-host)
	    (error "Gateway %s must be directly ftp accessible."
		   efs-gateway-host)))
      (setq rem-template efs-gateway-tmp-name-template
	    template (or (efs-local-to-gateway-filename
			  efs-gateway-tmp-name-template t)
			 (format efs-path-format-string
				 (efs-get-user efs-gateway-host)
				 efs-gateway-host
				 efs-gateway-tmp-name-template))
	    template-len (length template)))
    ;; Compute a new file name.
    (while (let (efs-verbose)
	     (setq file (format "%s%c%s" template start pid)
		   entry (intern file efs-tmp-name-obarray))
	     (or (memq entry efs-tmp-name-files)
		 (file-exists-p file)))
      (if (> (setq start (1+ start)) ?z)
	  (progn
	    (setq template (concat template "X"))
	    (setq start ?a))))
    (setq efs-tmp-name-files
	  (cons entry efs-tmp-name-files))
    (if rem-template
	(cons file (concat rem-template (substring file template-len)))
      (cons file file))))

(defun efs-del-tmp-name (temp)
  ;; Deletes file TEMP, a string.
  (setq efs-tmp-name-files
	(delq (intern temp efs-tmp-name-obarray)
	      efs-tmp-name-files))
  (condition-case ()
      (let (efs-verbose)
	(delete-file temp))
    (error nil)))


;;;; ==============================================================
;;;; >4
;;;; Hosts, Users, Accounts, and Passwords
;;;; ==============================================================
;;;
;;; A lot of the support for this type of thing is in efs-netrc.el.

;;;; ------------------------------------------------------------
;;;; Password support.
;;;; ------------------------------------------------------------

(defun efs-lookup-passwd (host user)
  ;; Look up the password for HOST and USER.
  (let ((ent (efs-get-host-user-property host user 'passwd)))
    (and ent (efs-code-string ent))))

(defun efs-system-fqdn ()
  "Returns a fully qualified domain name for the current host, if possible."
  (or efs-system-fqdn
      (setq efs-system-fqdn
	    (let ((sys (system-name)))
	      (if (string-match "\\." sys)
		  sys
		(if efs-nslookup-program
		    (let ((proc (let ((default-directory exec-directory)
				      (process-connection-type nil))
				  (start-process " *nslookup*" " *nslookup*"
						 efs-nslookup-program sys)))
			  (res sys)
			  (n 0))
		      (process-kill-without-query proc)
		      (save-excursion
			(set-buffer (process-buffer proc))
			(let ((quit-flag nil)
			      (inhibit-quit nil))
			  (if efs-nslookup-threshold
			      (progn
				(while (and (memq (process-status proc)
						  '(run open))
					    (< n efs-nslookup-threshold))
				  (accept-process-output)
				  (setq n (1+ n)))
				(if (>= n efs-nslookup-threshold)
				    (progn
				      (with-output-to-temp-buffer "*Help*"
					(princ (format "\
efs is unable to determine a fully qualified domain name
for the local host to send as an anonymous ftp password.

The function `system-name' is not returning a fully qualified
domain name. An attempt to obtain a fully qualified domain name
with `efs-nslookup-program' (currently set to \"%s\") has
elicited no response from that program. Consider setting 
`efs-generate-anonymous-password' to an email address for anonymous
ftp passwords.

For more information see the documentation (use C-h v) for the
variables `efs-nslookup-program' and `efs-nslookup-threshold'."
						       efs-nslookup-program)))
				      (error "No response from %s"
					     efs-nslookup-program))))
			    (while (memq (process-status proc) '(run open))
			      (accept-process-output proc)))
			  (goto-char (point-min))
			  (if (re-search-forward
			       (format "^Name: *\\(%s\\.[^ \n\t]+\\)"
				       sys) nil t)
			      (setq res (buffer-substring
					 (match-beginning 1)
					 (match-end 1)))
			    (kill-buffer (current-buffer)))))
		      res)
		  sys))))))

(defun efs-passwd-unique-list (alist)
  ;; Preserving the relative order of ALIST, remove all entries with duplicate 
  ;; cars.
  (let (result)
    (while alist
      (or (assoc (car alist) result)
	  (setq result (cons (car alist) result)))
      (setq alist (cdr alist)))
    (nreverse result)))

(defun efs-get-passwd-list (user host)
  ;; Returns an alist of the form '((pass host user) ...).
  ;; The order is essentially arbitrary, except that entries with user
  ;; equal to USER will appear first. Followed by entries with host equal to
  ;; HOST. Also, there will be no entries with duplicate values of pass.
  (efs-parse-netrc)
  (let* ((user-template (concat "/" user))
	 (ulen (length user-template))
	 (hlen (length host))
	 primaries secondaries tertiaries)
    (efs-save-match-data
      (efs-map-hashtable
       (function
	(lambda (key passwd)
	  (cond ((null passwd) nil)
		((and (> (length key) ulen)
		      (string-equal user-template
				    (substring key (- ulen))))
		 (setq primaries (cons (list (efs-code-string passwd)
					     (substring key 0 (- ulen))
					     (substring user-template 1))
				       primaries)))
		((and (> (length key) hlen)
		      (string-equal host (substring key 0 hlen))
		      (memq (aref key hlen) '(?/ ?.)))
		 (if (string-match "/" key hlen)
		     (setq secondaries
			   (cons (list (efs-code-string passwd)
				       (substring key 0 (match-beginning 0))
				       (substring key (match-end 0)))
				 secondaries))))
		((string-match "/" key)
		 (setq tertiaries
		       (cons (list (efs-code-string passwd)
				   (substring key 0 (match-beginning 0))
				   (substring key (match-end 0)))
			     tertiaries))))))
       efs-host-user-hashtable 'passwd))
    (efs-passwd-unique-list (nconc primaries secondaries tertiaries))))
    
(defun efs-get-passwd (host user)
  "Given a HOST and USER, return the FTP password, prompting if it was not
previously set."
  (efs-parse-netrc)

  ;; look up password in the hash table first; user might have overriden the
  ;; defaults.
  (cond ((efs-lookup-passwd host user))
	
	;; see if default user and password set from the .netrc file.
	((and (stringp efs-default-user)
	      efs-default-password
	      (string-equal user efs-default-user))
	 (copy-sequence efs-default-password))
	
	;; anonymous ftp password is handled specially since there is an
	;; unwritten rule about how that is used on the Internet.
	((and (efs-anonymous-p user)
	      efs-generate-anonymous-password)
	 (if (stringp efs-generate-anonymous-password)
	     (copy-sequence efs-generate-anonymous-password)
	   (concat (user-login-name) "@" (efs-system-fqdn))))
	
	;; see if same user has logged in to other hosts; if so then prompt
	;; with the password that was used there.
	(t
	 (let (others defaults passwd)
	   (unwind-protect
	       (progn
		 (setq others (efs-get-passwd-list user host)
		       defaults (mapcar
				 (function
				  (lambda (x)
				    (cons
				     (format
				      "Passwd for %s@%s (same as %s@%s): "
				      user host (nth 2 x) (nth 1 x))
				     (car x))))
				 others))
		 (setq passwd
		       (read-passwd
			(or defaults
			    (format "Password for %s@%s: " user host)))))
	     (while others
	       (fillarray (car (car others)) 0)
	       (setq others (cdr others))))
	   (or (null passwd)
	       (and efs-high-security-hosts
		    (efs-save-match-data
		      (string-match efs-high-security-hosts
				    (format "%s@%s" user host))))
	       (efs-set-passwd host user passwd))
	   passwd))))

;;;; ------------------------------------------------------------
;;;; Account support
;;;; ------------------------------------------------------------

(defun efs-get-account (host user &optional minidisk really)
  "Given a HOST, USER, and optional MINIDISK return the FTP account password.
If the optional REALLY argument is given, prompts the user if it can't find
one."
  (efs-parse-netrc)
  (let ((account (if minidisk
		     (efs-get-hash-entry
		      (concat (downcase host) "/" user "/" minidisk)
		      efs-minidisk-hashtable
		      (memq (efs-host-type host)
			    efs-case-insensitive-host-types))
		   (efs-get-host-user-property host user 'account))))
    (if account
	(efs-code-string account)
      ;; Do we really want to send the default-account passwd for all
      ;; minidisks?
      (if (and (stringp efs-default-user)
	       (string-equal user efs-default-user)
	       efs-default-account)
	  efs-default-account
	(and really
	     (let ((acct
		    (read-passwd
		     (if minidisk
			 (format
			  "Write access password for minidisk %s on %s@%s: "
			  minidisk user host)
		       (format
			"Account password for %s@%s: " user host)))))
	       (or (and efs-high-security-hosts
			(efs-save-match-data
			  efs-high-security-hosts
			  (format "%s@%s" user host)))
		   (efs-set-account host user minidisk acct))
	       acct))))))

;;;; -------------------------------------------------------------
;;;; Special classes of users.
;;;; -------------------------------------------------------------

(defun efs-anonymous-p (user)
  ;; Returns t if USER should be treated as an anonymous FTP login.
  (let ((user (downcase user)))
    (or (string-equal user "anonymous") (string-equal user "ftp"))))


;;;; =============================================================
;;;; >5
;;;; FTP client process, and server responses
;;;; =============================================================

;;;; ---------------------------------------------------------
;;;; Support for asynch process queues.
;;;; ---------------------------------------------------------

(defun efs-add-to-queue (host user item)
  "To the end of the command queue for HOST and USER, adds ITEM.
Does nothing if there is no process buffer for HOST and USER."
  (let ((buff (efs-ftp-process-buffer host user)))
    (if (get-buffer buff)
	(save-excursion
	  (set-buffer buff)
	  (setq efs-process-q
		(nconc efs-process-q (list item)))))))

;;;; -------------------------------------------------------
;;;; Error recovery for the process filter.
;;;; -------------------------------------------------------

;;; Could make this better, but it's such an unlikely error to hit.
(defun efs-process-scream-and-yell (line)
  (let* ((buff (buffer-name (current-buffer)))
	 (host (and (string-match "@\\(.*\\)\\*$" buff)
		    (substring buff (match-beginning 1) (match-end 1)))))
    (with-output-to-temp-buffer "*Help*"
      (princ
       (concat
	"efs is unable to identify the following reply code
from the ftp server " host ":\n\n" line "

Please send a bug report to ange@hplb.hpl.hp.com.
In your report include a transcript of your\n"
buff " buffer."))))
  (error "Unable to identify server code."))

(defun efs-error (host user msg)
  "Signal \'ftp-error for the FTP connection for HOST and USER.
The error gives the string MSG as text. The process buffer for the FTP
is popped up in another window."
  (let ((cur (selected-window))
	(pop-up-windows t)
	(buff (get-buffer (efs-ftp-process-buffer host user))))
    (if buff
	(progn
	  (pop-to-buffer buff)
	  (goto-char (point-max))
	  (select-window cur))))
  (signal 'ftp-error (list (format "FTP Error: %s" msg))))

;;;; --------------------------------------------------------------------
;;;; Process filter and supporting functions for handling FTP codes.
;;;; --------------------------------------------------------------------

(defun efs-process-handle-line (line proc)
  ;; Look at the given LINE from the ftp process PROC and try to catagorize it.
  (cond ((string-match efs-xfer-size-msgs line)
	 (let ((n 1))
	   ;; this loop will bomb with an args out of range error at 10
	   (while (not (match-beginning n))
	     (setq n (1+ n)))
	   (setq efs-process-xfer-size
		 (ash (string-to-int (substring line
						(match-beginning n)
						(match-end n)))
		    -10))))
	
	((string-match efs-multi-msgs line)
	 (setq efs-process-result-cont-lines
	       (concat efs-process-result-cont-lines line "\n")))
	
	((string-match efs-skip-msgs line))

	((string-match efs-cmd-ok-msgs line)
	 (if (string-match efs-cmd-ok-cmds efs-process-cmd)
	     (setq efs-process-busy nil
		   efs-process-result nil
		   efs-process-result-line line)))

	((string-match efs-pending-msgs line)
	 (if (string-match "^quote rnfr " efs-process-cmd)
	     (setq efs-process-busy nil
		   efs-process-result nil
		   efs-process-result-line line)))
	
	((string-match efs-bytes-received-msgs line)
	 (if efs-process-server-confused
	     (setq efs-process-busy nil
		   efs-process-result nil
		   efs-process-result-line line)))
	
	((string-match efs-server-confused-msgs line)
	 (setq efs-process-server-confused t))

	((string-match efs-good-msgs line)
	 (setq efs-process-busy nil
	       efs-process-result nil
	       efs-process-result-line line))

	((string-match efs-fatal-msgs line)
	 (set-process-sentinel proc nil)
	 (delete-process proc)
	 (setq efs-process-busy nil
	       efs-process-result 'fatal
	       efs-process-result-line line))
	
	((string-match efs-failed-msgs line)
	 (setq efs-process-busy nil
	       efs-process-result 'failed
	       efs-process-result-line line))
	
	((string-match efs-unknown-response-msgs line)
	 (setq efs-process-busy nil
	       efs-process-result 'weird
	       efs-process-result-line line)
	 (efs-process-scream-and-yell line))))

(efs-define-fun efs-process-log-string (proc str)
  ;; For a given PROCESS, log the given STRING at the end of its
  ;; associated buffer.
  (let ((buff (get-buffer (process-buffer proc))))
    (if buff
	(efs-save-buffer-excursion
	  (set-buffer buff)
	  (comint-output-filter proc str)))))

(defun efs-process-filter (proc str)
  ;; Build up a complete line of output from the ftp PROCESS and pass it
  ;; on to efs-process-handle-line to deal with.
  (let ((inhibit-quit t)
	(buffer (get-buffer (process-buffer proc)))
	(efs-default-directory default-directory))

    ;; see if the buffer is still around... it could have been deleted.
    (if buffer
	(efs-save-buffer-excursion
	  (set-buffer (process-buffer proc))
	  (efs-save-match-data

	    ;; handle hash mark printing
	    (if efs-process-busy
		(setq str (efs-process-handle-hash str)
		      efs-process-string (concat efs-process-string str)))
	    (efs-process-log-string proc str)
	    (while (and efs-process-busy
			(string-match "\n" efs-process-string))
	      (let ((line (substring efs-process-string
				     0
				     (match-beginning 0))))
		(setq efs-process-string (substring
					  efs-process-string
					  (match-end 0)))
		;; If we are in synch with the client, we should
		;; never get prompts in the wrong place. Just to be safe,
		;; chew them off.
		(while (string-match efs-process-prompt-regexp line)
		  (setq line (substring line (match-end 0))))
		(efs-process-handle-line line proc)))
	    
	    ;; has the ftp client finished?  if so then do some clean-up
	    ;; actions.
	    (if (not efs-process-busy)
		(progn
		  (efs-correct-hash-mark-size)
		  ;; reset process-kill-without-query
		  (process-kill-without-query proc)
		  ;; issue the "done" message since we've finished.
		  (if (and efs-process-msg
			   (efs-message-p)
			   (null efs-process-result))
		      (progn

			(efs-message "%s...done" efs-process-msg)
			(setq efs-process-msg nil)))
		  
		  (if (and efs-process-nowait
			   (null efs-process-cmd-waiting))
		      
		      (progn
			;; Is there a continuation we should be calling?
			;; If so, we'd better call it, making sure we
			;; only call it once.
			(if efs-process-continue
			    (let ((cont efs-process-continue))
			      (setq efs-process-continue nil)
			      (efs-call-cont
			       cont
			       efs-process-result
			       efs-process-result-line
			       efs-process-result-cont-lines)))
			;; If the cmd was run asynch, run the next
			;; cmd from the queue. For synch cmds, this
			;; is done by efs-send-cmd. For asynch
			;; cmds we don't care about
			;; efs-nested-cmd, since nothing is
			;; waiting for the cmd to complete. If
			;; efs-process-cmd-waiting is t, exit
			;; to let this command run.
			(if (and efs-process-q
				 ;; Be careful to check efs-process-busy
				 ;; again, because the cont may have started
				 ;; some new ftp action.
				 ;; wheels within wheels...
				 (null efs-process-busy))
			    (let ((next (car efs-process-q)))
			      (setq efs-process-q
				      (cdr efs-process-q))
			      (apply 'efs-send-cmd
				     efs-process-host
				     efs-process-user
				     next))))
		    
		    (if efs-process-continue
			(let ((cont efs-process-continue))
			  (setq efs-process-continue nil)
			  (efs-call-cont
			   cont
			   efs-process-result
			   efs-process-result-line
			   efs-process-result-cont-lines))))
		  
		  ;; Update the mode line
		  ;; We can't test nowait to see if we changed the
		  ;; modeline in the first place, because conts
		  ;; may be running now, which will confuse the issue.
		  ;; The logic is simpler if we update the modeline
		  ;; before the cont, but then the user sees the
		  ;; modeline track the cont execution. It's dizzying.
		  (if (and (or efs-mode-line-format
			       efs-ftp-activity-function)
			   (null efs-process-busy))
		      (efs-update-mode-line)))))

	  ;; Trim buffer, if required.
	  (and efs-max-ftp-buffer-size
	       (zerop efs-process-cmd-counter)
	       (> (point-max) efs-max-ftp-buffer-size)
	       (= (point-min) 1) ; who knows, the user may have narrowed.
	       (null (get-buffer-window (current-buffer)))
	       (save-excursion
		 (goto-char (/ efs-max-ftp-buffer-size 2))
		 (forward-line 1)
		 (delete-region (point-min) (point))))))))

;;;; ------------------------------------------------------------------
;;;; Functions for counting hashes and reporting on bytes transferred.
;;;; ------------------------------------------------------------------

(defun efs-set-xfer-size (host user bytes)
  ;; Set the size of the next FTP transfer in bytes.
  (let ((proc (efs-get-process host user)))
    (if proc
	(let ((buf (process-buffer proc)))
	  (if buf
	      (save-excursion
		(set-buffer buf)
		(setq efs-process-xfer-size (ash bytes -10))))))))

(defun efs-guess-incoming-bin-hm-size ()
  ;; Guess at the hash mark size for incoming binary transfers by taking
  ;; the average value for such transfers to other hosts.
  (let ((total 0)
	(n 0))
    (efs-map-hashtable
     (function
      (lambda (host hm-size)
	(if hm-size (setq total (+ total hm-size)
			  n (1+ n)))))
     efs-host-hashtable
     'incoming-bin-hm-size)
    (and (> n 0) (/ total n))))

(defun efs-set-hash-mark-unit (host user &optional incoming)
  ;; Sets the value of efs-process-hash-mark-unit according to the xfer-type.
  ;; efs-hash-mark-unit is the number of bytes represented by a hash mark,
  ;; in units of 16. If INCOMING is non-nil, the xfer will be a GET.
  (if efs-send-hash
      (let ((buff (efs-ftp-process-buffer host user))
	    (gate-p (efs-use-gateway-p host t)))
	(if buff
	    (save-excursion
	      (set-buffer buff)
	      (setq efs-process-hash-mark-unit
		    (ash (or
			  (and incoming (eq efs-process-xfer-type 'image)
			       (or (efs-get-host-property
				    host 'incoming-bin-hm-size)
				   (if gate-p
				       efs-gateway-incoming-binary-hm-size
				     efs-incoming-binary-hm-size)
				   (let ((guess
					  (efs-guess-incoming-bin-hm-size)))
				     (and guess
					  (efs-set-host-property
					   host 'incoming-bin-hm-size
					   guess)))))
			  (if gate-p
			      efs-gateway-hash-mark-size
			    efs-hash-mark-size)
			  1024) ; make sure that we have some integer
			 -4)))))))

(defun efs-correct-hash-mark-size ()
  ;; Corrects the value of efs-{ascii,binary}-hash-mark-size.
  ;; Must be run in the process buffer.
  (and efs-send-hash
       efs-process-hash-mark-unit
       (> efs-process-xfer-size 0)
       (< efs-process-xfer-size 524288) ; 2^19, prevent overflows
       (> efs-process-hash-mark-count 0)
       (or (> efs-process-last-percent 100)
	   (< (ash (* efs-process-hash-mark-unit
		      (1+ efs-process-hash-mark-count )) -6)
	      efs-process-xfer-size))
       (let ((val (ash (/ (ash efs-process-xfer-size 6)
			  efs-process-hash-mark-count) 4)))
	 (if (and (eq efs-process-xfer-type 'image)
		  (>= (length efs-process-cmd) 4)
		  (string-equal (downcase (substring efs-process-cmd 0 4))
				"get "))
	     (efs-set-host-property efs-process-host 'incoming-bin-hm-size val)
	   (set (if (efs-use-gateway-p efs-process-host t)
		    'efs-gateway-hash-mark-size
		  'efs-hash-mark-size)
		val)))))

(defun efs-process-handle-hash (str)
  ;; Remove hash marks from STRING and display count so far.
  (if (string-match "^#+$" str)
      (progn
	(setq efs-process-hash-mark-count
	      (+ efs-process-hash-mark-count
		 (- (match-end 0) (match-beginning 0))))
	(and
	 efs-process-msg
	 efs-process-hash-mark-unit
	 (not (and efs-process-nowait
		   (or (eq efs-verbose 0)
		       (eq (selected-window) (minibuffer-window)))))
	 (efs-message-p)
	 (let* ((big (> efs-process-hash-mark-count 65536)) ; 2^16
		(kbytes (if big
			    (* efs-process-hash-mark-unit
			       (ash efs-process-hash-mark-count -6))
			  (ash (* efs-process-hash-mark-unit
				  efs-process-hash-mark-count)
			       -6))))
	   (if (zerop efs-process-xfer-size)
	       (or (zerop kbytes)
		   (efs-message "%s...%dk" efs-process-msg kbytes))
	     (let ((percent (if big
				(/ (* 100 (ash kbytes -7))
				   (ash efs-process-xfer-size -7))
			      (/ (* 100 kbytes) efs-process-xfer-size))))
	       ;; Don't display %'s betwwen 100 and 110
	       (and (> percent 100) (< percent 110) (setq percent 100))
	       ;; cut out the redisplay of identical %-age messages.
	       (or (eq percent efs-process-last-percent)
		   (progn
		     (setq efs-process-last-percent percent)
		     (efs-message "%s...%d%%" efs-process-msg percent)))))))
	(concat (substring str 0 (match-beginning 0))
		(and (/= (length str) (match-end 0))
		     (substring str (1+ (match-end 0))))))
    str))

;;;; ------------------------------------------------------------------
;;;; Keeping track of the number of active background connections.
;;;; ------------------------------------------------------------------

(defun efs-ftp-processes-active ()
  ;; Return the number of FTP processes busy.
  (save-excursion
    (length
     (delq nil
	   (mapcar
	    (function
	     (lambda (buff)
	       (set-buffer buff)
	       (and (boundp 'efs-process-busy)
		    efs-process-busy)))
	    (buffer-list))))))

(defun efs-update-mode-line ()
  ;; Updates the mode with FTP activity, and runs `efs-ftp-activity-function'.
  (let ((num (efs-ftp-processes-active)))
    (if efs-mode-line-format
	(progn
	  (if (zerop num)
	      (setq efs-mode-line-string "")
	    (setq efs-mode-line-string (format efs-mode-line-format num)))
	  ;; fake emacs into re-calculating all the mode lines.
	  (save-excursion (set-buffer (other-buffer)))
	  (set-buffer-modified-p (buffer-modified-p))))
    (if efs-ftp-activity-function
	(funcall efs-ftp-activity-function num))))

;;;###autoload
(defun efs-display-ftp-activity ()
  "Displays the number of active background ftp sessions in the modeline.
Uses the variable `efs-mode-line-format' to determine how this will be
displayed."
  (interactive)
  (or (memq 'efs-mode-line-string global-mode-string)
      (if global-mode-string
	  (nconc global-mode-string '(efs-mode-line-string))
	(setq global-mode-string '("" efs-mode-line-string)))))

;;;; -------------------------------------------------------------------
;;;; Expiring inactive ftp buffers.
;;;; -------------------------------------------------------------------

(defun efs-start-polling ()
  ;; Start polling FTP buffers, to look for idle ones.
  (or (null efs-expire-ftp-buffers)
      (let ((proc (get-process "efs poll")))
	(or (and proc (eq (process-status proc) 'run))))
      (let ((default-directory exec-directory)
	    (process-connection-type nil)
	    new-proc)
	(condition-case nil
	    (delete-process "efs poll")
	  (error nil))
	(setq new-proc (start-process
			"efs poll" nil
			(concat exec-directory "wakeup")
			(int-to-string efs-ftp-buffer-poll-time)))
	(set-process-filter new-proc (function efs-expire-ftp-buffers-filter))
	(process-kill-without-query new-proc))))

(defun efs-connection-visited-p (host user)
  ;; Returns t if there are any buffers visiting files on HOST and USER.
  (save-excursion
    (let ((list (buffer-list))
	  (case-fold (memq (efs-host-type host)
			   efs-case-insensitive-host-types))
	  (visited nil)
	  parsed)
      (setq host (downcase host))
      (if case-fold (setq user (downcase user)))
      (while list
	(set-buffer (car list))
	(if (or (and buffer-file-name
		     (setq parsed (efs-ftp-path buffer-file-name))
		     (string-equal host (downcase (car parsed)))
		     (string-equal user (if case-fold
					    (downcase (nth 1 parsed))
					  (nth 1 parsed))))
		(and (boundp 'dired-directory)
		     (stringp dired-directory)
		     efs-dired-host-type
		     (setq parsed (efs-ftp-path dired-directory))
		     (string-equal host (downcase (car parsed)))
		     (string-equal user (if case-fold
					    (downcase (nth 1 parsed))
					  (nth 1 parsed)))))
	    (setq visited t
		  list nil)
	  (setq list (cdr list))))
      visited)))

(defun efs-expire-ftp-buffers-filter (proc string)
  ;; Check all ftp buffers, and kill them if they have been inactive
  ;; for the minimum of efs-ftp-buffer-expire-time and their local
  ;; time out time.
  (if efs-expire-ftp-buffers
      (let ((list (buffer-list))
	    new-alist)
	(save-excursion
	  (while list
	    (set-buffer (car list))
	    (if (eq major-mode 'efs-mode)
		(let* ((proc (get-buffer-process (current-buffer)))
		       (proc-p (and proc (memq (process-status proc)
					       '(run open)))))
		  (if (or efs-ftp-buffer-expire-time
			  efs-process-idle-time
			  (null proc-p))
		      (let ((elt (assq (car list) efs-ftp-buffer-alist))
			    (wind-p (get-buffer-window (car list))))
			(if (or (null elt) (buffer-modified-p)
				efs-process-busy wind-p)
			    (progn
			      (setq new-alist (cons (cons (car list) 0)
						    new-alist))
			      (or wind-p (set-buffer-modified-p nil)))
			  (let ((idle (+ (cdr elt)
					 efs-ftp-buffer-poll-time)))
			    (if (and proc-p
				     (< idle
					(if efs-ftp-buffer-expire-time
					    (if efs-process-idle-time
						(min efs-ftp-buffer-expire-time
						     efs-process-idle-time)
					      efs-ftp-buffer-expire-time)
					  efs-process-idle-time)))
				(progn
				  (setq new-alist (cons (cons (car list) idle)
							new-alist))
				  (set-buffer-modified-p nil))
			      ;; If there are still buffers for host & user,
			      ;; don't wipe the cache.
			      (and proc
				   (efs-connection-visited-p
				    efs-process-host efs-process-user)
				   (set-process-sentinel proc nil))
			      (kill-buffer (car list)))))))))
	    (setq list (cdr list))))
	(setq efs-ftp-buffer-alist new-alist))
    (condition-case nil
	(delete-process "efs poll")
      (error nil))))

;;;; -------------------------------------------------------------------
;;;; When the FTP client process dies...
;;;; -------------------------------------------------------------------

(defun efs-process-sentinel (proc str)
  ;; When ftp process changes state, nuke all file-entries in cache.
  (let ((buff (process-buffer proc)))
    ;; If the client dies, make sure that efs doesn't think that
    ;; there is a running process.
    (save-excursion
      (condition-case nil
	  (progn
	    (set-buffer buff)
	    (setq efs-process-busy nil))
	(error nil)))
    (let ((parsed (efs-parse-proc-name proc)))
      (if parsed
	  (progn
	    (apply 'efs-wipe-file-entries parsed)
	    (apply 'efs-wipe-from-ls-cache parsed))))
    (if (or efs-mode-line-format efs-ftp-activity-function)
	(efs-update-mode-line))))

(defun efs-kill-ftp-process (buffer)
  "Kill an FTP connection and its associated process buffer.
If the BUFFER's visited file name or default-directory is an efs remote
file name, it is the connection for that file name that is killed."
  (interactive "bKill FTP process associated with buffer: ")
  (or buffer (setq buffer (current-buffer)))
  (save-excursion
    (set-buffer buffer)
    (if (eq major-mode 'efs-mode)
	(kill-buffer buffer)
      (let ((file (or (buffer-file-name) default-directory)))
	(if file
	    (let ((parsed (efs-ftp-path (expand-file-name file))))
	      (if parsed
		  (let ((host (nth 0 parsed))
			(user (nth 1 parsed)))
		    (kill-buffer
		     (efs-ftp-process-buffer host user))))))))))

(defun efs-close-ftp-process (buffer)
  "Close an FTP connection.
This kills the FTP client process, but unlike `efs-kill-ftp-process' this
neither kills the process buffer, nor deletes cached data for the connection."
  (interactive "bClose FTP process associated with buffer: ")
  (or buffer (setq buffer (current-buffer)))
  (save-excursion
    (set-buffer buffer)
    (if (eq major-mode 'efs-mode)
	(let ((process (get-buffer-process buffer)))
	  (if process
	      (progn
		(set-process-sentinel process nil)
		(setq efs-process-busy nil
		      efs-process-q nil)
		(if (or efs-mode-line-format efs-ftp-activity-function)
		    (efs-update-mode-line))
		(delete-process process))))
      (let ((file (or (buffer-file-name) default-directory)))
	(if file
	    (let ((parsed (efs-ftp-path (expand-file-name file))))
	      (if parsed
		  (let ((process (get-process
				  (format "*ftp %s@%s*"
					  (nth 1 parsed) (car parsed)))))
		    (if process
			(progn
			  (set-buffer (process-buffer process))
			  (set-process-sentinel process nil)
			  (setq efs-process-busy nil
				efs-process-q nil)
			  (if (or efs-mode-line-format
				  efs-ftp-activity-function)
			      (efs-update-mode-line))
			  (delete-process process)))))))))))

(defun efs-ping-ftp-connection (buffer)
  "Ping a connection by sending a NOOP command.
Useful for waking up a possible expired connection."
  (interactive "bPing FTP connection associated with buffer: ")
  (or buffer (setq buffer (current-buffer)))
  (efs-save-buffer-excursion
    (set-buffer buffer)
    (let (file host user parsed)
      (if (or (and (eq major-mode 'efs-mode)
		   (setq host efs-process-host
			 user efs-process-user))
	      (and (setq file (or (buffer-file-name) default-directory))
		   (setq parsed (efs-ftp-path file))
		   (setq host (car parsed)
			 user (nth 1 parsed))))
	  (or (car
	       (efs-send-cmd
		host user '(quote noop)
		(format "Pinging connection %s@%s" user host)))
	      (message "Connection %s@%s is alive." user host))))))

(defun efs-display-ftp-process-buffer (buffer)
  "Displays the FTP process buffer associated with the current buffer."
  (interactive "bDisplay FTP buffer associated with buffer: ")
  (if (null buffer) (setq buffer (current-buffer)))
  (let ((file (or (buffer-file-name) default-directory))
	parsed proc-buffer)
    (if (and file (setq parsed (efs-ftp-path file))
	     (setq proc-buffer (get-buffer (efs-ftp-process-buffer
					    (car parsed)
					    (nth 1 parsed)))))
	(display-buffer proc-buffer)
      (error "Buffer %s not associated with an FTP process" buffer))))

;;;; -------------------------------------------------------------------
;;;; Starting the FTP client process
;;;; -------------------------------------------------------------------

(defun efs-ftp-process-buffer (host user)
  "Return name of the process buffer for ftp process for HOST and USER."
  ;; Host names on the internet are case-insensitive.
  (format efs-ftp-buffer-format user (downcase host)))

(defun efs-pty-check (proc threshold)
  ;; Checks to see if PROC is a pty. Beware, it clobbers the process
  ;; filter, so run this before you set the filter.
  ;; THRESHOLD is an integer to tell it how long to wait for output.
  (sit-for 0)   ; Update the display before doing any waiting.
  (let ((efs-pipe-p t)
	(n 0))
    (set-process-filter proc (function (lambda (proc string)
					 (setq efs-pipe-p nil))))
    (while (and (< n threshold) efs-pipe-p)
      (accept-process-output)
      (setq n (1+ n)))
    (if efs-pipe-p
	(progn
	  (sit-for 0) ; update display
	  ;; Use a sleep-for as I don't want pty-checking to depend
	  ;; on pending input.
	  (sleep-for efs-pty-check-retry-time)))
    (accept-process-output)
    (if efs-pipe-p
	(if (or noninteractive
		(progn
		  ;; in case the user typed something during the wait.
		  (discard-input)
		  (y-or-n-p
		   (format "%s seems not a pty. Kill? " proc))))
	    (progn
	      (kill-buffer (process-buffer proc))
	      (if (eq (selected-window) (minibuffer-window))
		  (abort-recursive-edit)
		(signal 'quit nil))))
      ;; Need to send a \n to make sure, because sometimes we get the startup
      ;; prompt from a pipe.
      (sit-for 0)
      (process-send-string proc "\n")
      (setq efs-pipe-p t
	    n 0)
      (while (and (< n threshold) efs-pipe-p)
	(accept-process-output)
	(setq n (1+ n)))
      (if efs-pipe-p
	  (progn
	    (sit-for 0)
	    (sleep-for efs-pty-check-retry-time)))
      (accept-process-output)
      (if (and efs-pipe-p
	       (or noninteractive
		   (progn
		     ;; in case the user typed something during the wait.
		     (discard-input)
		     (y-or-n-p
		      (format "%s seems not a pty. Kill? " proc)))))
	  (progn
	    (kill-buffer (process-buffer proc))
	    (if (eq (selected-window) (minibuffer-window))
		(abort-recursive-edit)
	      (signal 'quit nil)))))))

(defun efs-start-process (host user name)
  "Spawn a new ftp process ready to connect to machine HOST as USER.
If HOST is only ftp-able through a gateway machine then spawn a shell
on the gateway machine to do the ftp instead. NAME is the name of the
process."
  (let* ((use-gateway (efs-use-gateway-p host))
	 (buffer (get-buffer-create (efs-ftp-process-buffer host user)))
	 (process-connection-type t)
	 (opaque-p (memq use-gateway efs-opaque-gateways))
	 proc)
    (save-excursion
      (set-buffer buffer)
      (efs-mode host user (if opaque-p
			      efs-gateway-ftp-prompt-regexp
			    efs-ftp-prompt-regexp)))
    (cond
     ((null use-gateway)
      (message "Opening FTP connection to %s..." host)
      (setq proc (apply 'start-process name buffer efs-ftp-program-name
			efs-ftp-program-args)))
     ((eq use-gateway 'interactive)
      (setq proc (efs-gwp-start host user name)))
     ((eq use-gateway 'remsh)
      (message "Opening FTP connection to %s via %s..." host efs-gateway-host)
      (setq proc (apply 'start-process name buffer (nth 1 efs-gateway-type)
			(append (list efs-gateway-host)
				(nth 2 efs-gateway-type)
				(list (nth 3 efs-gateway-type))
				(nth 4 efs-gateway-type)))))
     ((memq use-gateway '(proxy raptor interlock kerberos))
      (message "Opening FTP connection to %s via %s..." host efs-gateway-host)
      (setq proc (apply 'start-process name buffer (nth 1 efs-gateway-type)
			(nth 2 efs-gateway-type))))
     ((eq use-gateway 'local)
      (message "Opening FTP connection to %s..." host)
      (setq proc (apply 'start-process name buffer (nth 1 efs-gateway-type)
			(nth 2 efs-gateway-type))))
     ((error "Never heard of gateway type %s" use-gateway)))
    (process-kill-without-query proc)
    (if opaque-p
	(accept-process-output proc)
      (if efs-pty-check-threshold
	  (efs-pty-check proc efs-pty-check-threshold)
	(accept-process-output proc)))
    (set-process-sentinel proc (function efs-process-sentinel))
    (set-process-filter proc (function efs-process-filter))
    (efs-start-polling)
    (save-excursion
      (set-buffer buffer)
      (goto-char (point-max))
      (set-marker (process-mark proc) (point)))
    proc))

(defun efs-get-process-internal (host user)
  ;; Get's the first process for HOST and USER.  If HOST runs a
  ;; a case insignificant OS, then case is not considered in USER.
  (let ((list (process-list))
	(case-fold (memq (efs-host-type host)
			 efs-case-insensitive-host-types))
	(len (+ (length host) (length user) 7))
	fmt name found)
    (setq host (downcase host))
    (if case-fold (setq user (downcase user)))
    (while (and (not found) list)
      (setq name (process-name (car list)))
      (if (and (= (length name) len)
	       (string-equal (substring name 0 5) "*ftp ")
	       (string-equal
		(if case-fold (downcase (substring name 5)) (substring name 5))
		(or fmt (setq fmt (format "%s@%s*" user host))))
	       (memq (process-status (car list)) '(run open)))
	  (setq found (car list))
	(setq list (cdr list))))
    found))

;; efs-guess-host-type calls this
;; function recursively. The (if (and proc... avoids an infinite
;; loop. We should make sure that this won't hang things if the
;; connection goes wrong.

(defun efs-get-process (host user)
  "Return the process object for the FTP process for HOST and USER.
Create a new process if needed."

  (let ((proc (efs-get-process-internal host user)))
    (if (and proc (memq (process-status proc) '(run open)))
	proc
      
      ;; Make sure that the process isn't around in some strange state.

      (setq host (downcase host))
      (let ((name (concat "*ftp " user "@" host "*")))
	(if proc (condition-case nil (delete-process proc) (error nil)))
	
	;; grab a suitable process.
	(setq proc (efs-start-process host user name))
	
	(efs-save-match-data
	  (efs-save-buffer-excursion
	    (set-buffer (process-buffer proc))
	    
	    ;; Run any user-specified hooks.
	    (run-hooks 'efs-ftp-startup-hook)
	    
	    ;; login to FTP server.
	    (efs-login host user proc)

	    ;; Beware, the process may have died if the login went bad.
	    (if (memq (process-status proc) '(run open))

		(progn
		  ;; Tell client to send back hash-marks as progress.  It isn't
		  ;; usually fatal if this command fails.
		  (efs-guess-hash-mark-size proc)
		  
		  ;; Run any user startup functions
		  (let ((alist efs-ftp-startup-function-alist)
			(case-fold-search t))
		    (while alist
		      (if (string-match (car (car alist)) host)
			  (progn
			    (funcall (cdr (car alist)) host user)
			    (setq alist nil))
			(setq alist (cdr alist)))))
		  
		  ;; Guess at the host type.
		  (efs-guess-host-type host user)
		  
		  ;; Check the idle time.
		  (efs-check-idle host user)

		  proc)

	      ;; Hopefully a recursive retry worked.
	      (or (efs-get-process-internal host user)
		  (error "No FTP process for %s@%s" user host)))))))))

(defun efs-guess-hash-mark-size (proc)
  ;; Doesn't run efs-save-match-data. You must do that yourself.
  (if efs-send-hash
      (save-excursion
	(set-buffer (process-buffer proc))
	(let ((line (nth 1 (efs-raw-send-cmd proc "hash")))
	      (gate-p (efs-use-gateway-p efs-process-host t)))
	  ;; Don't guess if the hash-mark-size is already set.
	  (or (if gate-p efs-gateway-hash-mark-size efs-hash-mark-size)
	      (if (string-match efs-hash-mark-msgs line)
		  (let ((size (substring line (match-beginning 1)
					 (match-end 1))))
		    (if (string-match "^[0-9]+$" size)
			(set (if gate-p
				 'efs-gateway-hash-mark-size
			       'efs-hash-mark-size)
			     (string-to-int size))))))))))

;;;; ------------------------------------------------------------
;;;; Simple FTP process shell support.
;;;; ------------------------------------------------------------

(defun efs-mode (host user prompt)
  "Major mode for interacting with an FTP process.
The user interface for sending commands to the FTP process is `comint-mode'.
For more information see the documentation for `comint-mode'.  This command
is not intended for interactive use.
Takes arguments: HOST USER PROMPT

Runs efs-mode-hook if it is not nil.

Key map:
\\{comint-mode-map}"
  (let ((proc (get-buffer-process (current-buffer))))
    ;; Running comint-mode will kill-all-local-variables.
    (comint-mode)
    ;; All these variables are buffer local.
    (setq major-mode 'efs-mode
	  mode-name "efs"
	  default-directory (file-name-directory efs-tmp-name-template)
	  comint-prompt-regexp prompt
	  efs-process-host host
	  efs-process-user user
	  efs-process-prompt-regexp prompt)
    (set (make-local-variable 'paragraph-start) comint-prompt-regexp)
    ;; Old versions of comint don't have this.  It does no harm for
    ;; the newer ones.
    (set (make-local-variable 'comint-last-input-start) (make-marker))
    (goto-char (point-max))
    ;; in case there is a running process
    (if proc (set-marker (process-mark proc) (point)))
    (run-hooks 'efs-mode-hook)))


;;;; =============================================================
;;;; >6
;;;; Sending commands to the FTP server.
;;;; =============================================================

;;;; -------------------------------------------------------------
;;;; General purpose functions for sending commands.
;;;; -------------------------------------------------------------

(defun efs-raw-send-cmd (proc cmd &optional msg pre-cont cont nowait)
;; Low-level routine to send the given ftp CMD to the ftp PROCESS.
;; MSG is an optional message to output before and after the command.
;; If PRE-CONT is non-nil, it is called immediately after execution
;; of the command starts, but without waiting for it to finish.
;; If CONT is non-NIL then it is either a function or a list of function and
;; some arguments.  The function will be called when the ftp command has 
;; completed.
;; If CONT is NIL then this routine will return \( RESULT . LINE \) where
;; RESULT is whether the command was successful, and LINE is the line from
;; the FTP process that caused the command to complete.
;; If NOWAIT is nil then we will wait for the command to complete before 
;; returning. If NOWAIT is 0, then we will wait until the command starts,
;; executing before returning. NOWAIT of 1 is like 0, except that the modeline
;; will indicate an asynch FTP command.
;; If NOWAIT has any other value, then we will simply queue the
;; command. In all cases, CONT will still be called

  (if (memq (process-status proc) '(run open))
      (efs-save-buffer-excursion
	(set-buffer (process-buffer proc))
	
	(if efs-process-busy
	    ;; This function will always wait on a busy process.
	    ;; Queueing is done by efs-send-cmd.
	    (let ((efs-process-cmd-waiting t))
	      (efs-kbd-quit-protect proc
		(while efs-process-busy
		  (accept-process-output)))))

	(setq efs-process-string ""
	      efs-process-result-line ""
	      efs-process-result-cont-lines ""
	      efs-process-busy t
	      efs-process-msg (and efs-verbose msg)
	      efs-process-continue cont
	      efs-process-server-confused nil
	      efs-process-nowait nowait
	      efs-process-hash-mark-count 0
	      efs-process-last-percent -1
	      efs-process-xfer-size 0
	      efs-process-cmd-counter (% (1+ efs-process-cmd-counter) 16))
	(process-kill-without-query proc t)
	(and efs-process-msg
	     (efs-message-p)
	     (efs-message "%s..." efs-process-msg))
	(goto-char (point-max))
	(move-marker comint-last-input-start (point))
	(move-marker comint-last-input-end (point))
	;; don't insert the password into the buffer on the USER command.
	(efs-save-match-data
	  (if (string-match efs-passwd-cmds cmd)
	      (insert (setq efs-process-cmd
			    (substring cmd 0 (match-end 0)))
		      " Turtle Power!\n")
	    (setq efs-process-cmd cmd)
	    (insert cmd "\n")))
	(process-send-string proc (concat cmd "\n"))
	(set-marker (process-mark proc) (point))
	;; Update the mode-line
	(if (and (or efs-mode-line-format efs-ftp-activity-function)
		 (memq nowait '(t 1)))
	    (efs-update-mode-line))
	(if pre-cont
	    (let ((efs-nested-cmd t))
	      (save-excursion
		(apply (car pre-cont) (cdr pre-cont)))))
	(prog1
	    (if nowait 
		nil
	      ;; hang around for command to complete
	      ;; Some clients die after the command is sent, if the server
	      ;; times out. Don't wait on dead processes.
	      (efs-kbd-quit-protect proc
		(while (and efs-process-busy
			    ;; Need to recheck nowait, since it may get reset
			    ;; in a cont.
			    (null efs-process-nowait)
			    (memq (process-status proc) '(run open)))
		  (accept-process-output proc)))
	      
	      ;; cont is called by the process filter
	      (if cont
		  ;; Return nil if a cont was called.
		  ;; Can't return process-result
		  ;; and process-line since executing
		  ;; the cont may have changed
		  ;; the state of the process buffer.
		  nil
		(list efs-process-result
		      efs-process-result-line
		      efs-process-result-cont-lines)))
	  
	  ;; If the process died, the filter would have never got the chance
	  ;; to call the cont. Try to jump start things.
	  
	  (if (and (not (memq (process-status proc) '(run open)))
		   (string-equal efs-process-result-line "")
		   cont
		   (equal cont efs-process-continue))
	      (progn
		(setq efs-process-continue nil
		      efs-process-busy nil)
		;; The process may be in some strange state. Get rid of it.
		(condition-case nil (delete-process proc) (error nil))
		(efs-call-cont cont 'fatal "" "")))))
    
    (error "FTP process %s has died." (process-name proc))))

(efs-defun efs-quote-string nil (string &optional not-space)
  "Quote any characters in STRING that may confuse the ftp process.
If NOT-SPACE is non-nil, then blank characters are not quoted, because
it is assumed that the string will be surrounded by \"'s."
  (apply (function concat)
	 (mapcar (function
		   (lambda (char)
		     (if (or (< char ?\ )
			     (and (null not-space) (= char ?\ ))
			     (> char ?\~)
		             (= char ?\")
			     (= char ?\\))
			 (vector ?\\ char)
		       (vector char))))
		 string)))

(efs-defun efs-fix-path nil (path &optional reverse)
  "Convert PATH from a unix format to a non-unix format.
If optional REVERSE, convert in the opposite direction."
  (identity path))

(efs-defun efs-fix-dir-path nil (dir-path)
  "Convert DIR-PATH from unix format to a non-unix format for a dir listing"
  ;; The default def runs for dos-distinct, ka9q, and all the unix's.
  ;; To be more careful about distinguishing dirs from plain files,
  ;; we append a ".".
  (let ((len (length dir-path)))
    (if (and (not (zerop len)) (= (aref dir-path (1- len)) ?/))
	(concat dir-path ".")
      dir-path)))

(defun efs-send-cmd (host user cmd
			       &optional msg pre-cont cont nowait noretry)
  "Find an ftp process connected to HOST logged in as USER and send it CMD.
MSG is an optional status message to be output before and after issuing the
command.

See the documentation for efs-raw-send-cmd for a description of CONT, PRE-CONT
and NOWAIT. Normally, if the command fails it is retried. If NORETRY is
non-nil, this is not done."
  ;; Handles conversion to remote pathname syntax and remote ls option
  ;; capability. Also, sends umask if nec.

  (let ((proc (efs-get-process host user)))
    
    (if (and
	 (eq nowait t)
	 (save-excursion
	   (set-buffer (process-buffer proc))
	   (or efs-process-busy
	       efs-process-cmd-waiting)))
	
	(progn
	  (efs-add-to-queue
	   host user
	   ;; Not nec. to store host and user, because the queue is for
	   ;; a specific host user pair anyway. Because the queue is always
	   ;; examined when efs-process-busy
	   ;; is nil, it should be impossible to get into a loop
	   ;; where we keep re-queueing over and over. To be on the safe
	   ;; side, store nowait as 1.
	   (list cmd msg pre-cont cont 1 noretry))
	  nil)
      
      ;; Send a command.

      (let (cmd-string afsc-result afsc-line afsc-cont-lines)

	(let ((efs-nested-cmd t)
	      (cmd0 (car cmd))
	      (cmd1 (nth 1 cmd))
	      (cmd2 (nth 2 cmd))
	      (cmd3 (nth 3 cmd)))
	  
	  (cond
	   
	   ((eq cmd0 'quote)
	    ;; QUOTEd commands
	    (cond
	     
	     ((eq cmd1 'site)
	      ;; SITE commands
	      (cond
	       ((memq cmd2 '(umask idle dos exec nfs group gpass))
		;; For UMASK cmd3 = value of umask
		;; For IDLE cmd3 = idle setting, or nil if we're querying.
		;; For DOS and NFS cmd3 is nil.
		;; For EXEC cmd3 is the command to be exec'ed -- a string.
		(if cmd3 (setq cmd3 (concat " " cmd3)))
		(setq cmd-string (concat "quote site " (symbol-name cmd2)
					 cmd3)))
	       ((eq cmd2 'chmod)
		(let* ((host-type (efs-host-type host user))
		       (cmd4 (efs-quote-string
			      host-type (efs-fix-path host-type (nth 4 cmd)))))
		  (setq cmd-string (concat "quote site chmod " cmd3 " "
					   cmd4))))
	       (t (error "efs: Don't know how to send %s %s %s %s"
			 cmd0 cmd1 cmd2 cmd3))))
	     
	     ((memq cmd1 '(pwd xpwd syst pasv noop))
	      (setq cmd-string (concat "quote " (symbol-name cmd1))))
	     
	     ;; PORT command (cmd2 is IP + port address)
	     ((eq cmd1 'port)
	      (setq cmd-string (concat "quote port " cmd2)))

	     ((memq cmd1 '(appe retr))
	      (let ((host-type (efs-host-type host user)))
		;; Set an xfer type
		(if cmd3 (efs-set-xfer-type host user cmd3 t))
		(setq cmd2 (efs-quote-string host-type
					     (efs-fix-path host-type cmd2))
		      cmd-string (concat "quote " (symbol-name cmd1) " "
					 cmd2))))
	     
	     ((eq cmd1 'stor)
	      (let ((host-type (efs-host-type host user)))
		(if (memq host-type efs-unix-host-types)
		    (efs-set-umask host user))
		;; Set an xfer type
		(if cmd3 (efs-set-xfer-type host user cmd3 t))
		(setq cmd2 (efs-quote-string host-type
					     (efs-fix-path host-type cmd2))
		      cmd-string (concat "quote stor " cmd2))))
	     
	     ((memq cmd1 '(size mdtm rnfr))
	      (let ((host-type (efs-host-type host user)))
		(setq cmd2 (efs-quote-string host-type
					     (efs-fix-path host-type cmd2))
		      cmd-string (concat "quote "
					 (symbol-name cmd1) " " cmd2))))

	     ((memq cmd1 '(pass user))
	      (setq cmd-string (concat "quote " (symbol-name cmd1) " " cmd2)))
	     
	     (t
	      (error "efs: Don't know how to send %s %s %s %s"
		     cmd0 cmd1 cmd2 cmd3))))
	   
	   ;; TYPE command
	   ((eq cmd0 'type)
	    (setq cmd-string (concat "type " (symbol-name cmd1))))
     
	   ;; DIR command
	   ;; cmd == 'dir "remote-path" "local-path" "ls-switches"
	   ((memq cmd0 '(dir nlist))
	    (let ((host-type (efs-host-type host user))
		  (listing-type (efs-listing-type host user)))
	      (setq cmd1 (efs-fix-dir-path host-type cmd1))
	      (cond
	       ((memq listing-type efs-nlist-listing-types)
		(setq cmd-string (concat efs-nlist-cmd " "
					 (efs-quote-string host-type cmd1)
					 " " cmd2)))
	       ((or (memq host-type efs-dumb-host-types)
		    (null cmd3))
		(setq cmd-string (format "%s %s %s"
					 (if (eq cmd0 'nlist)
					     efs-nlist-cmd
					   "dir")
					 (efs-quote-string host-type cmd1)
					 cmd2)))
	       ((setq cmd-string
		      (format "%s \"%s %s\" %s"
			      (if (eq cmd0 'nlist)
				  efs-nlist-cmd
				"ls")
			      cmd3 (efs-quote-string host-type cmd1 t)
			      ;; cmd2 is a temp file, not nec. to quote.
			      cmd2))))))
	   
	   ;; First argument is the remote pathname
	   ((memq cmd0 '(delete mkdir rmdir cd))
	    (let ((host-type (efs-host-type host user)))
	      (setq cmd1 (efs-quote-string host-type
					   (efs-fix-path host-type cmd1))
		    cmd-string (concat (symbol-name cmd0) " " cmd1))))
	   
	   ;; GET command
	   ((eq cmd0 'get)
	    (let ((host-type (efs-host-type host user)))
	      (if cmd3 (efs-set-xfer-type host user cmd3))
	      (efs-set-hash-mark-unit host user t)
	      (setq cmd1 (efs-quote-string host-type
					   (efs-fix-path host-type cmd1))
		    cmd2 (efs-quote-string host-type cmd2)
		    cmd-string (concat "get " cmd1 " " cmd2))))
	   
	   ;; PUT command
	   ((eq cmd0 'put)
	    (let ((host-type (efs-host-type host user)))
	      (if (memq host-type efs-unix-host-types)
		  (efs-set-umask host user))
	      (if cmd3 (efs-set-xfer-type host user cmd3))
	      (efs-set-hash-mark-unit host user)
	      (setq cmd2 (efs-quote-string host-type
					   (efs-fix-path host-type cmd2))
		    cmd1 (efs-quote-string host-type cmd1)
		    cmd-string (concat "put " cmd1 " " cmd2))))

	   ;; APPEND command
	   ((eq cmd0 'append)
	    (let ((host-type (efs-host-type host user)))
	      (if cmd3 (efs-set-xfer-type host user cmd3))
	      (efs-set-hash-mark-unit host user)
	      (setq cmd2 (efs-quote-string host-type
					   (efs-fix-path host-type cmd2))
		    cmd1 (efs-quote-string host-type cmd1)
		    cmd-string (concat "append " cmd1 " " cmd2))))
	   
	   ;; CHMOD command
	   ((eq cmd0 'chmod)
	    (let ((host-type (efs-host-type host user)))
	      (setq cmd2 (efs-quote-string host-type
					   (efs-fix-path host-type cmd2))
		    cmd-string (concat "chmod " cmd1 " " cmd2))))
	   
	   ;; Both arguments are remote pathnames
	   ((eq cmd0 'rename)
	    (let ((host-type (efs-host-type host user)))
	      (setq cmd1 (efs-quote-string host-type
					   (efs-fix-path host-type cmd1))
		    cmd2 (efs-quote-string host-type
					   (efs-fix-path host-type cmd2))
		    cmd-string (concat "rename " cmd1 " " cmd2))))
	   
	   (t
	    (error "efs: Don't know how to send %s %s %s %s"
		   cmd0 cmd1 cmd2 cmd3))))
	  
	;; Actually send the resulting command.
	;; Why do we use this complicated binding of afsc-{result,line},
	;; rather then use the fact that efs-raw-send-cmd returns?
	;; Because efs-raw-send-cmd returns the result of the first
	;; attempt only. efs-send-cmd should return the result of
	;; the retry, if one was necessary.
	;; Maybe it would be better if efs-raw-send-cmd returned
	;; the result of cont, if nowait was nil? Or maybe still return
	;; \(result  line \)? As long as nowait is nil, it should
	;; return something useful.

	;; Beware, if some of the above FTP commands had to restart
	;; the process, PROC won't be set to the right process object.
	(setq proc (efs-get-process host user))
	
	(efs-raw-send-cmd
	 proc
	 cmd-string
	 msg
	 pre-cont
	 (efs-cont (result line cont-lines) (host user proc cmd msg pre-cont
						  cont nowait noretry)
	   (cond ((and (null noretry) (eq result 'fatal))
		  (let ((retry
			 (efs-send-cmd
			  host user cmd msg pre-cont cont
			  (if (eq nowait t) 1 nowait) t)))
		    (or cont nowait
			(setq afsc-result (car retry)
			      afsc-line (nth 1 retry)
			      afsc-cont-lines (nth 2 retry)))))
		 ((and (eq result 'failed)
		       (or (memq (car cmd) '(append rename put))
			   (and (eq (car cmd) 'quote)
				(eq (nth 1 cmd) 'stor)))
		       (efs-save-match-data
			 (string-match efs-write-protect-msgs line)))
		  (let ((retry (efs-write-recover
				(efs-host-type host)
				line cont-lines host user cmd msg pre-cont
				cont nowait noretry)))
		    (or cont nowait
			(setq afsc-result (car retry)
			      afsc-line (nth 1 retry)
			      afsc-cont-lines (nth 2 retry)))))
		 
		 (t (if cont
			(efs-call-cont cont result line cont-lines)
		      (or nowait
			  (setq afsc-result result
				afsc-line line
				afsc-cont-lines cont-lines))))))
	 nowait)
	
	(prog1
	    (if (or nowait cont)
		nil
	      (list afsc-result afsc-line afsc-cont-lines))
	  
	  ;; Check the queue
	  (or nowait
	      efs-nested-cmd
	      (let ((buff (efs-ftp-process-buffer host user)))
		(if (get-buffer buff)
		    (save-excursion
		      (set-buffer buff)
		      (if efs-process-q
			  (let ((next (car efs-process-q)))
			    (setq efs-process-q (cdr efs-process-q))
			    (apply 'efs-send-cmd host user next))))))))))))

(efs-defun efs-write-recover nil
  (line cont-lines host user cmd msg pre-cont cont nowait noretry)
  "Called when a write command fails with `efs-write-protect-msgs'.
Should return \(result  line cont-lines\), like `efs-raw-send-cmd'."
  ;; This default version doesn't do anything.
  (if cont
      (progn
	(efs-call-cont cont 'failed line cont-lines)
	nil)
    (if nowait nil (list 'failed line cont-lines))))

;;;; ---------------------------------------------------------------------
;;;; The login sequence. (The follows RFC959 rather tightly. If a server
;;;;                      can't even get the login codes right, it is
;;;;                      pretty much scrap metal.)
;;;; ---------------------------------------------------------------------

;;;###autoload
(defun efs-nslookup-host (host)
  "Attempt to resolve the given HOSTNAME using nslookup if possible."
  (interactive "sHost: ")
  (if efs-nslookup-program
      (let* ((default-directory exec-directory)
	     (default-major-mode 'fundamental-mode)
	     (process-connection-type nil)
	     (proc (start-process " *nslookup*" " *nslookup*"
				  efs-nslookup-program host))
	     (res host))
	(process-kill-without-query proc)
	(save-excursion
	  (set-buffer (process-buffer proc))
	  (let ((quit-flag nil)
		(inhibit-quit nil))
	    (while (memq (process-status proc) '(run open))
	      (accept-process-output proc)))
	  (goto-char (point-min))
	  (if (re-search-forward
	       "Name:.*\nAddress\\(es\\)?: *\\([.0-9]+\\)$" nil t)
	      (setq res (buffer-substring (match-beginning 2)
					  (match-end 2))))
	  (kill-buffer (current-buffer)))
	(if (interactive-p)
	    (message "%s: %s" host res))
	res)
    (if (interactive-p)
	(message
	 "No nslookup program. See the variable efs-nslookup-program."))
    host))

(defun efs-login (host user proc)
  "Connect to the FTP-server on HOST as USER.
PROC is the process to the FTP-client. Doesn't call efs-save-match-data.
You must do that yourself."
  (let ((gate (efs-use-gateway-p host)))
    (if (eq gate 'kerberos)
	(progn
	  (setq proc (efs-kerberos-login host user proc))
	  (efs-login-send-user host user proc gate))
      (let ((to (if (memq gate '(proxy local raptor))
		    efs-gateway-host
		  host))
	    port cmd result)
	(if (string-match "#" to)
	    (setq port (substring to (match-end 0))
		  to (substring to 0 (match-beginning 0))))
	(and efs-nslookup-on-connect
	     (string-match "[^0-9.]" to)
	     (setq to (efs-nslookup-host to)))
	(setq cmd (concat "open " to))
	(if port (setq cmd (concat cmd " " port)))
	
	;; Send OPEN command.
	(setq result (efs-raw-send-cmd proc cmd nil))
	
	(and (eq gate 'interlock) (string-match "^331 " (nth 1 result))
	     (setq result (efs-login-send-pass
			   efs-gateway-host
			   (efs-get-user efs-gateway-host) proc)))
	
	;; Analyze result of OPEN.
	(if (car result)
	    (progn
	      (condition-case nil (delete-process proc) (error nil))
	      (efs-error host user (concat "OPEN request failed: "
					   (nth 1 result))))
	  (efs-login-send-user host user proc gate))))))

(defun efs-login-send-user (host user proc &optional gate retry)
  "Send user command to HOST and USER. PROC is the ftp client process.
Optional argument GATE specifies which type of gateway is being used.
RETRY argument specifies to try twice if we get a 421 response."
  (let ((cmd (cond
	      ((memq gate '(local proxy interlock))
	       (format "quote USER \"%s\"@%s" user
		       (if (and efs-nslookup-on-connect
				(string-match "[^0-9.]" host))
			   (efs-nslookup-host host)
			 host)))
	      ((eq gate 'raptor)
	       (format "quote USER \"%s\"@%s %s" user
		       (if (and efs-nslookup-on-connect
				(string-match "[^0-9.]" host))
			   (efs-nslookup-host host)
			 host)
		       (nth 3 efs-gateway-type)))
	      ((eq gate 'kerberos)
	       (let ((to host)
		     port)
		 (if (string-match "#" host)
		     (progn
		       (setq to (substring host 0 (match-beginning 0))
			     port (substring host (match-end 0)))
		       (and efs-nslookup-on-connect
			    (string-match "[^0-9.]" to)
			    (efs-nslookup-host to))
		       (setq to (concat to "@" port))))
		 (format "quote user \"%s\"@%s" user to)))
	      (t
	       (format "quote user \"%s\"" user))))
	(msg (format "Logging in as user %s%s..." user
		     (if (memq gate '(proxy local raptor kerberos))
			 (concat "@" host) "")))  
	result code)	 
	
    ;; Send the message by hand so that we can report on the size
    ;; of the MOTD.
    (message msg)
    
    ;; Send USER command.
    (setq result (efs-raw-send-cmd proc cmd nil))
    
    ;; Analyze result of USER (this follows RFC959 strictly)
    (if (< (length (nth 1 result)) 4)
	(progn
	  (condition-case nil (delete-process proc) (error nil))
	  (efs-error host user
		     (concat "USER request failed: " (nth 1 result))))

      (setq code (substring (nth 1 result) 0 4))
      (cond
       
       ((string-equal "331 " code)
	;; Need password
	(setq result (efs-login-send-pass host user proc gate)))
     
       ((string-equal "332 " code)
	;; Need an account, but no password
	(setq result (efs-login-send-acct host user proc gate)))
     
       ((null (car result))
	;; logged in proceed
	nil)

       ((and (or (string-equal "530 " code) (string-equal "421 " code))
	     (efs-anonymous-p user)
	     (or (string-match efs-too-many-users-msgs (nth 1 result))
		 (string-match efs-too-many-users-msgs (nth 2 result))))
	(if (save-window-excursion
	      (condition-case nil
		  (display-buffer (process-buffer proc))
		(error nil))
	      (y-or-n-p (format
			 "Too many users for %s@%s. Try again? "
			 user host)))
	    (progn
	      ;; Set result to nil if we are doing a retry, so done
	      ;; message only gets sent once.
	      (setq result nil)
	      (if (string-equal code "530 ")
		  (efs-login-send-user host user proc gate t)
		(efs-get-process host user)))
	  (signal 'quit nil)))
       
       ((and retry (string-equal code "421 "))
	(setq result nil)
	(efs-get-process host user))
       
       (t  ; bombed
	(condition-case nil (delete-process proc) (error nil))
	;; Wrong username?
	(efs-set-user host nil)
	(efs-error host user
		   (concat "USER request failed: " (nth 1 result)))))
      (and (null (car result))
	   (stringp (nth 2 result))
	   (message "%sdone%s" msg
		    (let ((n (efs-occur-in-string ?\n (nth 2 result))))
		      (if (> n 1)
			  (format "; MOTD of %d lines" n)
			"")))))))

(defun efs-login-send-pass (host user proc &optional gate)
  "Sends password to HOST and USER. PROC is the ftp client process.
Doesn't call efs-save-match data. You must do that yourself."
  ;; Note that efs-get-password always returns something.
  ;; It prompts the user if necessary. Even if the returned password is
  ;; \"\", send it, because we wouldn't be running this function
  ;; if the server wasn't insisting on a password.
  (let* ((pass "")
	 (qpass "")
	 (cmd "")
	 (result (unwind-protect
		     (progn
		       (condition-case nil
			   (setq pass (efs-get-passwd host user))
			 (quit (condition-case nil
				   (kill-buffer (process-buffer proc))
				 (error nil))
			       (signal 'quit nil)))
		       (setq cmd (concat
				  "quote pass "
				  (setq qpass (efs-quote-string nil pass t))))
		       (efs-raw-send-cmd proc cmd))
		   (fillarray pass 0)
		   (fillarray qpass 0)
		   (fillarray cmd 0)))
	 (code (and (>= (length (nth 1 result)) 4)
		    (substring (nth 1 result) 0 4))))
    (or code (setq code ""))
    ;; Analyze the result.
    (cond
     ((string-equal code "332 ")
      ;; require an account passwd
      (setq result (efs-login-send-acct host user proc gate)))
     ((null (car result))
      ;; logged in proceed
      nil)
     ((or (string-equal code "530 ") (string-equal code "421 "))
      ;; Give the user another chance
      (condition-case nil
	  (if (efs-anonymous-p user)
	      (if (or (string-match efs-too-many-users-msgs (nth 1 result))
		      (string-match efs-too-many-users-msgs (nth 2 result)))
		  (if (save-window-excursion
			(condition-case nil
			    (display-buffer (process-buffer proc))
			  (error nil))
			(y-or-n-p (format
				   "Too many users for %s@%s. Try again? "
				   user host)))
		      (progn
			;; Return nil if we are doing a retry, so done
			;; message only gets sent once.
			(setq result nil)
			(if (string-equal code "530 ")
			    (efs-login-send-user host user proc gate)
			  (efs-get-process host user)))
		    (signal 'quit nil))
		(unwind-protect
		    (efs-set-passwd
		     host user
		     (save-window-excursion
		       (condition-case nil
			   (display-buffer (process-buffer proc))
			 (error nil))
		       (setq pass
			     (read-passwd
			      (format
			       "Password for %s@%s failed. Try again: "
			       user host)))))
		  (fillarray pass 0))
		(setq result nil)
		(efs-login-send-user host user proc gate))
	    (unwind-protect
		(efs-set-passwd
		 host user
		 (setq pass
		       (read-passwd
			(format "Password for %s@%s failed. Try again: "
				user host))))
	      (fillarray pass 0))
	    (setq result nil)
	    (efs-login-send-user host user proc gate))
	(quit (condition-case nil (delete-process proc) (error nil))
	      (efs-set-user host nil)
	      (efs-set-passwd host user nil)
	      (signal 'quit nil))
	(error (condition-case nil (delete-process proc) (error nil))
	       (efs-set-user host nil)
	       (efs-set-passwd host user nil)
	       (efs-error host user "PASS request failed."))))
     (t ; bombed for unexplained reasons
      (condition-case nil (delete-process proc) (error nil))
      (efs-error host user (concat "PASS request failed: " (nth 1 result)))))
    result))

(defun efs-login-send-acct (host user proc &optional gate)
  "Sends account password to HOST and USER. PROC is the ftp client process.
Doesn't call efs-save-match data. You must do that yourself."
  (let* ((acct "")
	 (qacct "")
	 (cmd "")
	 (result (unwind-protect
		     (progn
		       ;; The raptor gateway requires us to send a gateway
		       ;; authentication password for account.  What if the
		       ;; remote server wants one too?
		       (setq acct (if (eq gate 'raptor)
				      (efs-get-account
				       efs-gateway-host
				       (nth 3 efs-gateway-type) nil t)
				    (efs-get-account host user nil t))
			     qacct (efs-quote-string nil acct t)
			     cmd (concat "quote acct " qacct))
		       (efs-raw-send-cmd proc cmd))
		   (fillarray acct 0)
		   (fillarray qacct 0)
		   (fillarray cmd 0))))
    ;; Analyze the result
    (cond
     ((null (car result))
      ;; logged in proceed
      nil)
     ((eq (car result) 'failed)
      ;; Give the user another chance
      (condition-case nil
	  (progn
	    (unwind-protect
		(progn
		  (setq acct (read-passwd
			      (format
			       "Account password for %s@%s failed. Try again: "
			       user host)))
		  (or (and efs-high-security-hosts
			   (string-match efs-high-security-hosts
					 (format "%s@%s" user host)))
		      (efs-set-account host user nil acct)))
	      (fillarray acct 0))
	    (setq result (efs-login-send-user host user proc gate)))
	(quit (condition-case nil (delete-process proc) (error nil)))
	(error (condition-case nil (delete-process proc) (error nil))
	       (efs-error host user "ACCT request failed."))))
     (t ; bombed for unexplained reasons
      (condition-case nil (delete-process proc) (error nil))
      (efs-error host user (concat "ACCT request failed: " (nth 1 result)))))
    result))

;;;; ----------------------------------------------------------------------
;;;; Changing working directory.
;;;; ----------------------------------------------------------------------

(defun efs-raw-send-cd (host user dir &optional no-error)
  ;; If NO-ERROR, doesn't barf, but just returns success (t) or failure (nil).
  ;; This does not use efs-send-cmd.
  ;; Also DIR must be in the syntax of the remote host-type.
  (let* ((cmd (concat "cd " dir))
	 cd-result cd-line)
    (efs-raw-send-cmd
     (efs-get-process host user)
     cmd nil nil
     (efs-cont (result line cont-lines) (cmd)
       (if (eq result 'fatal)
	   (efs-raw-send-cmd
	    (efs-get-process host user)
	    cmd nil nil
	    (function (lambda (result line cont-lines)
			(setq cd-result result
			      cd-line line))))
	 (setq cd-result result
	       cd-line line))))
    (if no-error
	(null cd-result)
      (if cd-result
	  (efs-error host user (concat "CD failed: " cd-line))))))

;;;; --------------------------------------------------------------
;;;; Getting a PWD.
;;;; --------------------------------------------------------------

(defun efs-unquote-quotes (string)
  ;; Unquote \"\"'s in STRING to \".
  (let ((start 0)
	new)
    (while (string-match "\"\"" string start)
      (setq new (concat new (substring
			     string start (1+ (match-beginning 0))))
	    start (match-end 0)))
    (if new
	(concat new (substring string start))
      string)))

(efs-defun efs-send-pwd nil (host user &optional xpwd)
  "Attempts to get the current working directory for the given HOST/USER pair.
Returns \( DIR . LINE \) where DIR is either the directory or NIL if not found,
and LINE is the relevant success or fail line from the FTP-server. If the
optional arg XPWD is given, uses this server command instead of PWD."
  (let* ((result (efs-send-cmd host user
			       (list 'quote (if xpwd 'xpwd 'pwd))
			       "Getting pwd"))
	 (line (nth 1 result))
	 dir)
    (or (car result)
	(efs-save-match-data
	  (if (string-match "\"\\(.*\\)\"[^\"]*$" line)
	      (setq dir (efs-unquote-quotes (substring line (match-beginning 1)
						       (match-end 1))))
	    (if (string-match " \\([^ ]+\\) " line) ; stone-age servers!
		(setq dir (substring line
				     (match-beginning 1)
				     (match-end 1)))))))
    (cons dir line)))

(efs-defun efs-send-pwd super-dumb-unix (host user &optional xpwd)
  ;; Guess at the pwd for a unix host that doesn't support pwd.
  (if (efs-anonymous-p user)
      ;; guess
      (cons "/" "")
    ;; Who knows?
    (message "Can't obtain pwd for %s" host)
    (ding)
    (sleep-for 2)
    (message "All file names must be specified as full paths.")
    (cons nil "")))

;;;; --------------------------------------------------------
;;;; Getting the SIZE of a remote file.
;;;; --------------------------------------------------------

(defun efs-send-size (host user file)
  "For HOST and USER, get the size of FILE in bytes.
This returns a list \( SIZE . LINE \), where SIZE is the file size in bytes,
or nil if this couldn't be determined, and LINE is the output line of the 
FTP server."
  (efs-save-match-data
    (let ((result (efs-send-cmd host user (list 'quote 'size file))))
      (setcar result
	      (and (null (car result))
		   (string-match "^213 +\\([0-9]+\\)$" (nth 1 result))
		   (string-to-int
		    (substring
		     (cdr result)
		     (match-beginning 1) (match-end 1)))))
      result)))

;;;; ------------------------------------------------------------
;;;; umask support
;;;; ------------------------------------------------------------

(defun efs-umask (user)
  "Returns the umask that efs will use for USER.
If USER is root or anonymous, then the values of efs-root-umask
and efs-anonymous-umask, respectively, take precedence, to be followed
by the value of efs-umask, and if this is nil, it returns your current
umask on the local machine. Returns nil if this can't be determined."
  (or
   (and (string-equal user "root") efs-root-umask)
   (and (efs-anonymous-p user)
	efs-anonymous-umask)
   efs-umask
   (let* ((shell (or (and (boundp 'explicit-shell-file-name)
			  explicit-shell-file-name)
		     (getenv "ESHELL")
		     (getenv "SHELL")
		     "/bin/sh"))
	  (default-major-mode 'fundamental-mode)
	  (default-directory exec-directory)
	  (buff (get-buffer-create " *efs-umask-data*")))
     (unwind-protect
	 (save-excursion
	   (set-buffer buff)
	   (call-process shell nil buff nil "-c" "umask")
	   (goto-char (point-min))
	   (if (re-search-forward "[0-7]?[0-7]?[0-7]" nil t)
	       (string-to-int (buffer-substring (match-beginning 0)
						(match-end 0)))))
       (kill-buffer buff)))))

(defun efs-send-umask (host user mask)
  "Sets the umask on HOST for USER to MASK.
Returns t for success, nil for failure."
  (interactive
   (let* ((path (or buffer-file-name
		    (and (eq major-mode 'dired-mode)
			 dired-directory)))
	  (parsed (and path (efs-ftp-path path)))
	  (default-host (car parsed))
	  (default-user (nth 1 parsed))
	  (default-mask (efs-umask default-user)))
     (list
      (read-string "Host: " default-host)
      (read-string "User: " default-user)
      (read-string "Umask: " (int-to-string default-mask)))))
  (let (int-mask)
    (if (integerp mask)
	(setq int-mask mask
	      mask (int-to-string mask))
      (setq int-mask (string-to-int mask)))
    (or (string-match "^ *[0-7]?[0-7]?[0-7] *$" mask)
	(error "Invalid umask %s" mask))
    (efs-send-cmd host user
		  (list 'quote 'site 'umask mask)
		  (concat "Setting umask to " mask)
		  (list
		   (function
		    (lambda (int-mask)
		      (let ((buff (efs-ftp-process-buffer host user)))
			(if (get-buffer buff)
			    (save-excursion
			      (set-buffer buff)
			      (setq efs-process-umask int-mask))))))
		   int-mask)
		  (efs-cont (result line cont-lines) (host user mask)
		    (if result
			(let ((buff (efs-ftp-process-buffer host user)))
			  (efs-set-host-property host 'umask-failed t)
			  (if (get-buffer buff)
			      (save-excursion
				(set-buffer buff)
				(setq efs-process-umask nil)))
			  (message
			   "Unable to set umask to %s on %s" mask host)
			  (if efs-ding-on-umask-failure
			      (progn
				(ding)
				(sit-for 1))))))
		  0))) ; Do this NOWAIT = 0

(defun efs-set-umask (host user)
  "Sets the umask for HOST and USER, if it has not already been set."
  (save-excursion
    (set-buffer (process-buffer (efs-get-process host user)))
    (if (or efs-process-umask (efs-get-host-property host 'umask-failed))
	nil
      (let ((umask (efs-umask user)))
	(efs-send-umask host user umask)
	t))))  ; Tell the caller that we did something.
	
(defun efs-modes-from-umask (umask)
  ;; Given the 3 digit octal integer umask, returns the decimal integer
  ;; according to chmod that a file would be written with.
  ;; Assumes only ordinary files, so ignores x bits.
  (let* ((others (% umask 10))
	 (umask (/ umask 10))
	 (group (% umask 10))
	 (umask (/ umask 10))
	 (owner (% umask 10))
	 (factor 1))
    (apply '+
	   (mapcar
	    (function
	     (lambda (x)
	       (prog1
		   (* factor (- 6 (- x (% x 2))))
		 (setq factor (* factor 8)))))
	    (list others group owner)))))

;;;; ------------------------------------------------------------
;;;; Idle time manipulation.
;;;; ------------------------------------------------------------

(defun efs-check-idle (host user)
  ;; We just toss it in the queue to run whenever there's time.
  ;; Just fail quietly if this doesn't work.
  (if (and (or efs-maximize-idle efs-expire-ftp-buffers)
	   (memq (efs-host-type host) efs-idle-host-types)
	   (null (efs-get-host-property host 'idle-failed)))
      (let ((buffname (efs-ftp-process-buffer host user)))
	(efs-add-to-queue
	 host user
	 (list '(quote site idle)
	       nil nil
	       (efs-cont (result line cont-lines) (host user buffname)
		 (efs-save-match-data
		   (if (and (null result)
			    (string-match efs-idle-msgs line))
		       (let ((max (substring line (match-beginning 2)
					     (match-end 2))))
			 (if (get-buffer buffname)
			     (save-excursion
			       (set-buffer buffname)
			       (setq efs-process-idle-time
				     (string-to-int
				      (substring line (match-beginning 1)
						 (match-end 1))))))
			 (if (and efs-maximize-idle
				  (not (efs-anonymous-p user)))
			     (efs-add-to-queue
			      host user
			      (list
			       (list 'quote 'site 'idle max)
			       nil nil
			       (efs-cont (result line cont-lines) (buffname
								   max)
				 (and (null result)
				      (get-buffer buffname)
				      (save-excursion
					(set-buffer buffname)
					(setq efs-process-idle-time
					      (string-to-int max)))))
			       0))))
		     (efs-set-host-property host 'idle-failed t))))
	       0 nil))))) ; Using NOWAIT = 0 inhibits mode line toggling.


;;;; ------------------------------------------------------------
;;;; Sending the SYST command for system type.
;;;; ------------------------------------------------------------

(defun efs-get-syst (host user)
  "Use SYST to get the remote system type.
Returns the system type as a string if this succeeds, otherwise nil."
  (let* ((result (efs-send-cmd host user '(quote syst)))
	 (line (nth 1 result)))
    (efs-save-match-data
      (and (null (car result))
	   (string-match efs-syst-msgs line)
	   (substring line (match-end 0))))))
  
;;;; ------------------------------------------------------------
;;;; File transfer representation type support
;;;; ------------------------------------------------------------

;;; Legal representation types are: image, ascii, ebcdic, tenex

(efs-defun efs-file-type nil (path)
  ;; Returns the file type for PATH, the full efs path, with filename FILE.
  ;; The return value is one of 'text, '8-binary, or '36-binary.
  (let ((parsed (efs-ftp-path path)))
    (efs-save-match-data
      (cond
       ;; There is no special significance to temp names, but we assume that
       ;; they exist on an 8-bit byte machine.
       ((or (null path)
	    (let ((temp (intern-soft path efs-tmp-name-obarray)))
	      (and temp (memq temp efs-tmp-name-files))))
	'8-binary)
       ((and (null parsed) (file-exists-p path))
	(efs-local-file-type path))
       ;; test special hosts
       ((and parsed
	     efs-binary-file-host-regexp
	     (let ((case-fold-search t))
	       (string-match efs-binary-file-host-regexp (car parsed))))
	'8-binary)
       (t
	;; Test file names
	(let ((file (efs-internal-file-name-nondirectory
		     (or (nth 2 parsed) path))))
	  (cond
	   ;; test for PDP-10 binaries
	   ((and efs-36-bit-binary-file-name-regexp
		 (string-match efs-36-bit-binary-file-name-regexp file))
	    '36-binary)
	   ((and efs-binary-file-name-regexp
		 (string-match efs-binary-file-name-regexp file))
	    '8-binary)
	   ((and efs-text-file-name-regexp
		 (string-match efs-text-file-name-regexp file))
	    'text)
	   ;; by default
	   (t
	    '8-binary))))))))

(efs-define-fun efs-local-file-type (file)
  ;; Looks at the beginning (magic-cookie) of a local file to determine
  ;; if it is a text file or not.  If it's not a text file, it doesn't care
  ;; about what type of binary file, so this doesn't really look for a magic
  ;; cookie.
  ;; Doesn't call efs-save-match-data.  The caller should do so.
  (save-excursion
    (set-buffer (get-buffer-create efs-data-buffer-name))
    (erase-buffer)
    (insert-file-contents file nil 0 16)
    (if (looking-at "[ -~\n\r\C-L]*\\'")
	'text
      '8-binary)))

(defun efs-rationalize-file-type (f-type t-type)
  ;; When the original and new names for a file indicate
  ;; different file types, this function applies an ad hoc heuristic
  ;; to return a single file type.
  (cond
   ((eq f-type t-type)
    f-type)
   ((memq '36-binary (list f-type t-type))
    '36-binary)
   ((memq '8-binary (list f-type t-type))
    '8-binary)
   (t
    'text)))

(defun efs-prompt-for-transfer-type (arg)
  "Toggles value of efs-prompt-for-transfer-type.
With prefix arg, turns prompting on if arg is positive, otherwise turns
prompting off."
  (interactive "P")
  (if (if arg
	  (> (prefix-numeric-value arg) 0)
	(null efs-prompt-for-transfer-type))
      ;; turn prompting on
      (prog1
	  (setq efs-prompt-for-transfer-type t)
	(message "Prompting for FTP transfer TYPE is on."))
    (prog1
	(setq efs-prompt-for-transfer-type nil)
      (message "Prompting for FTP transfer TYPE is off."))))

(defun efs-read-xfer-type (path)
  ;; Prompt for the transfer type to use for PATH
  (let ((type
	 (completing-read
	  (format "FTP transfer TYPE for %s: " (efs-relativize-filename path))
	  '(("binary") ("image") ("ascii") ("ebcdic") ("tenex"))
	  nil t)))
    (if (string-equal type "binary")
	'image
      (intern type))))

(defun efs-xfer-type (f-host-type f-path t-host-type t-path
				  &optional via-local)
  ;; Returns the transfer type for transferring a file.
  ;; F-HOST-TYPE = the host type of the machine on which the file is from.
  ;; F-PATH = path, in full efs-syntax, of the original file
  ;; T-HOST-TYPE = host-type of the machine to which the file is being
  ;;               transferred.
  ;; VIA-LOCAL = non-nil of the file is being moved through the local, or
  ;;             a gateway machine.
  ;; Set F-PATH or T-PATH to nil, to indicate that the file is being
  ;; transferred from/to a temporary file, whose name has no significance.
  (let (temp)
    (and f-path
	 (setq temp (intern-soft f-path efs-tmp-name-obarray))
	 (memq temp efs-tmp-name-files)
	 (setq f-path nil))
    (and t-path
	 (setq temp (intern-soft t-path efs-tmp-name-obarray))
	 (memq temp efs-tmp-name-files)
	 (setq t-path nil)))
  (if (or (null (or f-host-type t-host-type)) (null (or f-path t-path)))
      'image ; local copy?
    (if efs-prompt-for-transfer-type
	(efs-read-xfer-type (if f-path f-path t-path))
      (let ((f-fs (cdr (assq f-host-type efs-file-type-alist)))
	    (t-fs (cdr (assq t-host-type efs-file-type-alist))))
	(if (and f-fs t-fs
		 (if efs-treat-crlf-as-nl
		     (and (eq (car f-fs) (car t-fs))
			  (eq (nth 1 f-fs) (nth 1 t-fs))
			  (let ((f2-fs (nth 2 f-fs))
				(t2-fs (nth 2 t-fs)))
			    (or (eq f2-fs t2-fs)
				(and (memq f2-fs '(file-crlf file-nl))
				     (memq t2-fs '(file-crlf file-nl))))))
		   (equal f-fs t-fs)))
	    'image
	  (let ((type (cond
		       ((and f-path t-path)
			(efs-rationalize-file-type
			 (efs-file-type t-host-type t-path)
			 (efs-file-type f-host-type f-path)))
		       (f-path
			(efs-file-type f-host-type f-path))
		       (t-path
			(efs-file-type t-host-type t-path)))))
	    (cond
	     ((eq type '36-binary)
	      'image)
	     ((eq type '8-binary)
	      (if (or (eq (car f-fs) '36-bit-wa)
		      (eq (car t-fs) '36-bit-wa))
		  'tenex
		'image))
	     (t ; handles 'text
	      (if (and t-fs f-fs (eq (nth 1 f-fs) 'ebcdic)
		       (eq (nth 1 t-fs) 'ebcdic) (null via-local))
		  'ebcdic
		'ascii)))))))))

(defun efs-set-xfer-type (host user type &optional clientless)
  ;; Sets the xfer type for HOST and USER to TYPE.
  ;; If the connection is already using the required type, does nothing.
  ;; If clientless is non-nil, we are using a quoted xfer command, and
  ;; need to check if the client has changed things.
  (save-excursion
    (let ((buff (process-buffer (efs-get-process host user))))
      (set-buffer buff)
      (or (if (and clientless efs-process-client-altered-xfer-type)
	      (or (eq type efs-process-client-altered-xfer-type)
		  (setq efs-process-client-altered-xfer-type nil))
	    ;; We are sending a non-clientless command, so the client
	    ;; gets back in synch.
	    (setq efs-process-client-altered-xfer-type nil)
	    (and efs-process-xfer-type
		 (eq type efs-process-xfer-type)))
	  (let ((otype efs-process-xfer-type))
	    ;; Set this now in anticipation that the TYPE command will work,
	    ;; in case other commands, such as efs-set-hash-mark-unit want to
	    ;; grok this before the TYPE command completes.
	    (setq efs-process-xfer-type type)
	    (efs-send-cmd
	     host user (list 'type type)
	     nil nil
	     (efs-cont (result line cont-lines) (host user type otype buff)
	       (if result
		   (unwind-protect
		       (efs-error host user (format "TYPE %s failed: %s"
						    (upcase (symbol-name type))
						    line))
		     (if (get-buffer buff)
			 (save-excursion
			   (set-buffer buff)
			   (setq efs-process-xfer-type otype))))))
	     0)))))) ; always send type commands NOWAIT = 0


;;;; ------------------------------------------------------------
;;;; Obtaining DIR listings.
;;;; ------------------------------------------------------------

(defun efs-ls-guess-switches ()
  ;; Tries to determine what would be the most useful switches
  ;; to use for a DIR listing.
  (if (and (boundp 'dired-listing-switches)
	   (stringp dired-listing-switches)
	   (efs-parsable-switches-p dired-listing-switches t))
      dired-listing-switches
    "-al"))

(efs-defun efs-ls-dumb-check nil (line host file path lsargs msg noparse
				       noerror nowait cont)
  nil)

(efs-defun efs-ls-dumb-check unknown (line host file path lsargs
			       msg noparse noerror nowait cont)
  ;; Checks to see if the host type might be dumb unix. If so, returns the 
  ;; listing otherwise nil.
  (and
   lsargs
   (string-match
    ;; Some CMU servers return a 530 here.  550 is correct.
    (concat "^5[35]0 \\(The file \\)?"
	    (regexp-quote (concat lsargs " " path)))
    ;; 550 is for a non-accessible file -- RFC959
    line)
   (progn
     (if (eq (efs-host-type host) 'apollo-unix)
	 (efs-add-host 'dumb-apollo-unix host)
       (efs-add-host 'dumb-unix host))
     ;; try again
     (if nowait
	 t ; return t if asynch
	   ; This is because dumb-check can't run asynch.
	   ; This means that we can't recognize dumb hosts asynch.
	   ; Shouldn't be a problem.
       (efs-ls file nil
	       (if (eq msg t)
		   (format "Relisting %s" (efs-relativize-filename file))
		 msg)
	       noparse noerror nowait cont)))))

;; With no-error nil, this function returns:
;; an error if file is not an efs-path
;;                      (This should never happen.) 
;; an error if either the listing is unreadable or there is an ftp error.
;; the listing (a string), if everything works.
;; 
;; With no-error t, it returns:
;; an error if not an efs-path
;; error if listing is unreable (most likely caused by a slow connection)
;; nil if ftp error (this is because although asking to list a nonexistent
;;                   directory on a remote unix machine usually (except
;;                   maybe for dumb hosts) returns an ls error, but no
;;                   ftp error, if the same is done on a VMS machine,
;;                   an ftp error is returned. Need to trap the error
;;                   so we can go on and try to list the parent.)
;; the listing, if everything works.

(defun efs-ls (file lsargs msg &optional noparse noerror nowait cont nlist)
  "Return the output of a `DIR' or `ls' command done over ftp.
FILE is the full name of the remote file, LSARGS is any args to pass to the
`ls' command. MSG is a message to be displayed while listing, if MSG is given
as t, a suitable message will be computed. If nil, no message will be
displayed. If NOPARSE is non-nil, then the listing will not be parsed and
stored in internal cache. Otherwise, the listing will be parsed, if LSARGS
allow it. If NOERROR is non-nil, then we return nil if the listing fails,
rather than signal an error. If NOWAIT is non-nil, we do the listing
asynchronously, returning nil. If CONT is non-nil it is called with first
argument the listing string."
  ;; If lsargs are nil, this forces a one-time only dumb listing using dir.
  (setq file (efs-expand-file-name file))
  (let ((parsed (efs-ftp-path file)))
    (if parsed
	(let* ((host (nth 0 parsed))
	       (user (nth 1 parsed))
	       (path (nth 2 parsed))
	       (host-type (efs-host-type host user))
	       (listing-type (efs-listing-type host user))
	       (parse (cond
		       ((null noparse)
			(efs-parsable-switches-p lsargs t))
		       ((eq noparse 'parse)
			t)
		       (t nil)))
	       (switches lsargs)
	       cache)
	  
	  (if (memq host-type efs-dumb-host-types)
	      (setq lsargs nil))
	  (if (and (null efs-ls-uncache)
		   (setq cache
			 (or (efs-get-from-ls-cache file switches)
			     (and switches
				  (efs-convert-from-ls-cache
				   file switches host-type listing-type)))))
	      ;; The listing is in the mail, errr... cache.
	      (let (listing)
		(if (stringp cache)
		    (setq listing cache)
		  (setq listing (car cache))
		  (if (and parse (null (nth 1 cache)))
		      (save-excursion
			(set-buffer
			 (let ((default-major-mode 'fundamental-mode))
			   (get-buffer-create
			    efs-data-buffer-name)))
			(erase-buffer)
			(insert listing)
			(goto-char (point-min))
			(efs-set-files
			 file
			 (efs-parse-listing listing-type
					    host user path
					    file lsargs))
			;; Note that we have parsed it now.
			(setcar (cdr cache) t))))
		(if cont (efs-call-cont cont listing))
		listing)
	    
	    (if cache
		(efs-del-from-ls-cache file nil nil))
	    ;; Need to get the listing via FTP.
	    (let* ((temp (efs-make-tmp-name host nil))
		   (temp-file (car temp))
		   listing-result)
	      (efs-send-cmd
	       host user
	       (list (if nlist 'nlist 'dir) path (cdr temp) lsargs)
	       (if (eq msg t)
		   (format "Listing %s" (efs-relativize-filename file))
		 msg)
	       nil
	       (efs-cont (result line cont-lines)
		   (host-type listing-type host user temp-file path
			      switches file lsargs noparse parse noerror
			      msg nowait cont)
		 ;; The client flipped to ascii, remember this.
		 (let ((buff (get-buffer
			      (efs-ftp-process-buffer host user))))
		   (if buff
		       (efs-save-buffer-excursion
			 (set-buffer buff)
			 (setq efs-process-client-altered-xfer-type
			       'ascii))))
		 (unwind-protect
		     (if result
			 (or (setq listing-result
				   (efs-ls-dumb-check
				    (and (or (eq host-type 'unknown)
					     (eq listing-type 'unix:unknown))
					 'unknown)
				    line host file path lsargs msg
				    noparse noerror nowait cont))
			     ;; If dumb-check returns non-nil
			     ;; then it would have handled any error recovery
			     ;; and conts. listing-result would only be set to
			     ;; t if nowait was non-nil. Therefore, the final
			     ;; return for efs-ls could never be t, even if I
			     ;; set listing-result to t here.
			     (if noerror
				 (if cont
				     (efs-call-cont cont nil))
			       (efs-error host user
					  (concat "DIR failed: "
						  line))))
		       
		       ;; listing worked
		       (if (efs-ftp-path temp-file)
			   (efs-add-file-entry (efs-host-type efs-gateway-host)
					       temp-file nil nil nil))
		       (save-excursion
			 ;; A hack to get around a jka-compr problem.
			 ;; Do we still need it?
			 (let ((default-major-mode 'fundamental-mode)
			       efs-verbose jka-compr-enabled)
			   (set-buffer (get-buffer-create
					efs-data-buffer-name))
			   (erase-buffer)
			   (if (or (file-readable-p temp-file)
				   (sleep-for efs-retry-time)
				   (file-readable-p temp-file))
			       (insert-file-contents temp-file)
			     (efs-error host user
					(format
					 "list data file %s not readable"
					 temp-file))))
			 (if parse
			     (progn
			       (efs-set-files
				file
				(efs-parse-listing listing-type host user path
						   file lsargs))
			       ;; Parsing may update the host type.
			       (and lsargs (memq (efs-host-type host)
						 efs-dumb-host-types)
				    (setq lsargs nil))))
			 (let ((listing (buffer-string)))
			   (efs-add-to-ls-cache file lsargs listing parse)
			   (if (and (null lsargs) switches)
			       ;; Try to convert
			       (let ((conv (efs-get-ls-converter switches)))
				 (and conv
				      (setq conv (assoc
						  (char-to-string 0)
						  conv))
				      (funcall (cdr conv) listing-type nil)
				      (setq listing (buffer-string)))))
			   (or nowait (setq listing-result listing))
			   ;; Call the ls cont, with first arg the
			   ;; listing string.
			   (if cont
			       (efs-call-cont cont listing)))))
		   (efs-del-tmp-name temp-file)))
	       nowait)
	      (and (null nowait) listing-result))))
      (error "Attempt to get a remote listing for the local file %s" file))))


;;;; ===============================================================
;;;; >7
;;;; Parsing and storing remote file system data.
;;;; ===============================================================

;;; The directory listing parsers do some host type guessing.
;;; Most of the host type guessing is done when the PWD output
;;; is parsed. A bit is done when the error codes for DIR are
;;; analyzed.

;;;; -----------------------------------------------------------
;;;; Caching directory listings.
;;;; -----------------------------------------------------------

;;;  Aside from storing files data in a hashtable, a limited number
;;;  of listings are stored in complete form in `efs-ls-cache'.

(defun efs-del-from-ls-cache (file &optional parent-p dir-p)
  ;; Deletes from the ls cache the listing for FILE.
  ;; With optional PARENT-P, deletes any entry for the parent
  ;; directory of FILE too.
  ;; If DIR-P is non-nil, then the directory listing of FILE is to be deleted.
  (if dir-p
      (setq file (file-name-as-directory file))
    (setq file (directory-file-name file)))
  (setq file (efs-canonize-file-name file))
  (if parent-p
      (setq parent-p (file-name-directory
		      (if dir-p
			  (directory-file-name file)
			file))))
  (setq efs-ls-cache
	(delq nil
	      (mapcar
	       (if parent-p
		   (function
		    (lambda (x)
		      (let ((f-ent (car x)))
			(and (not (string-equal file f-ent))
			     (not (string-equal parent-p f-ent)) 
			     x))))
		 (function
		  (lambda (x)
		    (and (not (string-equal file (car x)))
			 x))))
	       efs-ls-cache))))

(defun efs-wipe-from-ls-cache (host user)
  ;; Remove from efs-ls-cache all listings for HOST and USER.
  (let ((host (downcase host))
	(case-insens (memq (efs-host-type host)
			   efs-case-insensitive-host-types)))
    (if case-insens (setq user (downcase user)))
    (setq efs-ls-cache
	(delq nil
	      (mapcar
	       (function
		(lambda (x)
		  (let ((parsed (efs-ftp-path (car x))))
		    (and (not
			  (and (string-equal (car parsed) host)
			       (string-equal (if case-insens
						 (downcase (nth 1 parsed))
					       (nth 1 parsed))
					     user)))
			 x))))
	       efs-ls-cache)))))

(defun efs-get-from-ls-cache (file switches)
  ;; Returns the value in `ls-cache' for FILE and SWITCHES.
  ;; Returns a list consisting of the listing string, and whether its
  ;; already been parsed. This list is eq to the nthcdr 2 of the actual
  ;; cache entry, so you can setcar it.
  ;; For dumb listings, SWITCHES will be nil.
  (let ((list efs-ls-cache)
	(switches (efs-canonize-switches switches))
	(file (efs-canonize-file-name file)))
    (catch 'done
      (while list
	(if (and (string-equal file (car (car list)))
		 (string-equal switches (nth 1 (car list))))
	    (throw 'done (nthcdr 2 (car list)))
	  (setq list (cdr list)))))))

(defun efs-add-to-ls-cache (file switches listing parsed)
  ;; Only call after efs-get-from-cache returns nil, to avoid duplicate
  ;; entries. PARSED should be t, if the listing has already been parsed.
  (and (> efs-ls-cache-max 0)
       (let ((switches (efs-canonize-switches switches))
	     (file (efs-canonize-file-name file)))
	 (if (= efs-ls-cache-max 1)
	     (setq efs-ls-cache
		   (list (list file switches listing parsed)))
	   (if (>= (length efs-ls-cache) efs-ls-cache-max)
	       (setcdr (nthcdr (- efs-ls-cache-max 2) efs-ls-cache) nil))
	   (setq efs-ls-cache (cons (list file switches listing parsed)
				    efs-ls-cache))))))

;;;; --------------------------------------------------------------
;;;; Converting listings from cache.
;;;; --------------------------------------------------------------

(defun efs-get-ls-converter (to-switches)
  ;; Returns converter alist for TO-SWITCHES
  (efs-get-hash-entry (efs-canonize-switches to-switches)
		      efs-ls-converter-hashtable))

(defun efs-add-ls-converter (to-switches  from-switches converter)
  ;; Adds an entry to `efs-ls-converter-hashtable'.
  ;; If from-switches is t, the converter converts from internal files
  ;; hashtable.
  (let* ((to-switches (efs-canonize-switches to-switches))
	 (ent (efs-get-hash-entry to-switches efs-ls-converter-hashtable))
	 (add (cons (or (eq from-switches t)
			(efs-canonize-switches from-switches))
		    converter)))
    (if ent
	(or (member add ent)
	    (nconc ent (list add)))
      (efs-put-hash-entry to-switches (list add) efs-ls-converter-hashtable))))

(defun efs-convert-from-ls-cache (file switches host-type listing-type)
  ;; Returns a listing by converting the switches from a cached listing.
  (let ((clist (efs-get-ls-converter switches))
	(dir-p (= ?/ (aref file (1- (length file)))))
	elt listing result regexp alist)
    (while file ; this loop will iterate at most twice.
      (setq alist clist)
      (while alist
	(setq elt (car alist))
	(if (eq (car elt) t)
	    (if (and dir-p (setq result (funcall (cdr elt) host-type
						 (let ((efs-ls-uncache t))
						   (efs-get-files file))
						 regexp)))
		(setq alist nil
		      file nil)
	      (setq alist (cdr alist)))
	  (if (and (setq listing
			 (efs-get-from-ls-cache file (car elt)))
		   (save-excursion
		     (set-buffer
		      (let ((default-major-mode 'fundamental-mode))
			(get-buffer-create efs-data-buffer-name)))
		     (erase-buffer)
		     (insert (car listing))
		     (and (funcall (cdr elt) listing-type regexp)
			  (setq result (buffer-string)))))
	      (setq alist nil
		    file nil)
	    (setq alist (cdr alist)))))
      ;; Look for wildcards.
      (if (and file (null dir-p) (null regexp))
	  (setq regexp (efs-shell-regexp-to-regexp
			(file-name-nondirectory file))
		file (file-name-directory file)
		dir-p t)
	(setq file nil)))
    result))

;;; Define some converters

(defun efs-unix-t-converter-sort-pred (elt1 elt2)
  (let* ((data1 (car elt1))
	 (data2 (car elt2))
	 (year1 (car data1))
	 (year2 (car data2))
	 (month1 (nth 1 data1))
	 (month2 (nth 1 data2))
	 (day1 (nth 2 data1))
	 (day2 (nth 2 data2))
	 (hour1 (nth 3 data1))
	 (hour2 (nth 3 data2))
	 (minutes1 (nth 4 data1))
	 (minutes2 (nth 4 data2)))
    (if year1
	(and year2
	     (or (> year1 year2)
		 (and (= year1 year2)
		      (or (> month1 month2)
			  (and (= month1 month2)
			       (> day1 day2))))))
      (if year2
	  t
	(or (> month1 month2)
	    (and (= month1 month2)
		 (or (> day1 day2)
		     (and (= day1 day2)
			  (or (> hour1 hour2)
			      (and (= hour1 hour2)
				   (> minutes1 minutes2)))))))))))

(defun efs-unix-t-converter (&optional regexp reverse)
  (if regexp
      nil
    (goto-char (point-min))
    (efs-save-match-data
      (if (re-search-forward efs-month-and-time-regexp nil t)
	  (let ((current-month (cdr (assoc (substring
					    (current-time-string) 4 7)
					   efs-month-alist)))
		list-start start end list year month day hour minutes)
	    (beginning-of-line)
	    (setq list-start (point))
	    (while (progn
		     (setq start (point))
		     (forward-line 1)
		     (setq end (point))
		     (goto-char start)
		     (re-search-forward efs-month-and-time-regexp end t))
	      ;; Need to measure wrto the current month
	      ;; There is a bug here if because of time-zone shifts, the
	      ;; local machine and the remote one are on different months.
	      (setq month (% (+ (- 11 current-month)
				(cdr (assoc
				      (buffer-substring (match-beginning 2)
							(match-end 2))
				      efs-month-alist))) 12)
		    day (string-to-int
			 (buffer-substring (match-beginning 3) (match-end 3)))
		    year (buffer-substring (match-beginning 4) (match-end 4)))
	      (if (string-match ":" year)
		  (setq hour (string-to-int (substring year 0
						       (match-beginning 0)))
			minutes (string-to-int (substring year (match-end 0)))
			year nil)
		(setq hour nil
		      minutes nil
		      year (string-to-int year)))
	      (setq list (cons
			  (cons
			   (list year month day hour minutes)
			   (buffer-substring start end))
			  list))
	      (goto-char end))
	    (setq list
		  (mapcar 'cdr
			  (sort list 'efs-unix-t-converter-sort-pred)))
	    (if reverse (setq list (nreverse list)))
	    (delete-region list-start (point))
	    (apply 'insert list)
	    t)))))

(efs-defun efs-t-converter nil (&optional regexp reverse)
  ;; Converts listing without the t-switch, to ones with it.
  nil) ; by default assume that we cannot work.

(efs-fset 'efs-t-converter 'unix 'efs-unix-t-converter)
(efs-fset 'efs-t-converter 'sysV-unix 'efs-unix-t-converter)
(efs-fset 'efs-t-converter 'apollo-unix 'efs-unix-t-converter)
(efs-fset 'efs-t-converter 'bsd-unix 'efs-unix-t-converter)
(efs-fset 'efs-t-converter 'dumb-unix 'efs-unix-t-converter)
(efs-fset 'efs-t-converter 'dumb-apollo-unix 'efs-unix-t-converter)
(efs-fset 'efs-t-converter 'super-dumb-unix 'efs-unix-t-converter)

(defun efs-rt-converter (listing-type &optional regexp)
  ;; Reverse time sorting
  (efs-t-converter listing-type regexp t))

(defun efs-unix-alpha-converter (&optional regexp reverse)
  (if regexp
      nil
    (goto-char (point-min))
    (efs-save-match-data
      (if (re-search-forward efs-month-and-time-regexp nil t)
	  (let (list list-start end start next)
	    (beginning-of-line)
	    (setq list-start (point))
	    (while (progn
		     (setq start (point))
		     (end-of-line)
		     (setq end (point)
			   next (1+ end))
		     (goto-char start)
		     (re-search-forward efs-month-and-time-regexp end t))
	      ;; Need to measure wrto the current month
	      ;; There is a bug here if because of time-zone shifts, the
	      ;; local machine and the remote one are on different months.
	      (setq list
		    (cons
		     (cons (buffer-substring (point) end)
			   (buffer-substring start next))
		     list))
	      (goto-char next))
	    (delete-region list-start (point))
	    (apply 'insert
		   (mapcar 'cdr
			   (sort list (if reverse
					  (function
					   (lambda (x y)
					     (string< (car y) (car x))))
					(function
					 (lambda (x y)
					   (string< (car x) (car y))))))))
	    t)))))

(efs-defun efs-alpha-converter nil (&optional regexp reverse)
  ;; Converts listing to lexigraphical order.
  nil) ; by default assume that we cannot work.

(efs-fset 'efs-alpha-converter 'unix 'efs-unix-alpha-converter)
(efs-fset 'efs-alpha-converter 'sysV-unix 'efs-unix-alpha-converter)
(efs-fset 'efs-alpha-converter 'apollo-unix 'efs-unix-alpha-converter)
(efs-fset 'efs-alpha-converter 'bsd-unix 'efs-unix-alpha-converter)
(efs-fset 'efs-alpha-converter 'dumb-unix 'efs-unix-alpha-converter)
(efs-fset 'efs-alpha-converter 'dumb-apollo-unix 'efs-unix-alpha-converter)
(efs-fset 'efs-alpha-converter 'super-dumb-unix 'efs-unix-alpha-converter)

(defun efs-ralpha-converter (listing-type &optional regexp)
  ;; Reverse alphabetic
  (efs-alpha-converter listing-type regexp t))

(defun efs-unix-S-converter (&optional regexp reverse)
  (if regexp
      nil
    (goto-char (point-min))
    (efs-save-match-data
      (if (re-search-forward efs-month-and-time-regexp nil t)
	  (let (list list-start start next)
	    (beginning-of-line)
	    (setq list-start (point))
	    (while (progn
		     (setq start (point))
		     (forward-line 1)
		     (setq next (point))
		     (goto-char start)
		     (re-search-forward efs-month-and-time-regexp next t))
	      ;; Need to measure wrto the current month
	      ;; There is a bug here if because of time-zone shifts, the
	      ;; local machine and the remote one are on different months.
	      (setq list
		    (cons
		     (cons (string-to-int
			    (buffer-substring (match-beginning 1)
					      (match-end 1)))
			   (buffer-substring start next))
		     list))
	      (goto-char next))
	    (delete-region list-start (point))
	    (apply 'insert
		   (mapcar 'cdr
			   (sort list (if reverse
					  (function
					   (lambda (x y)
					     (< (car x) (car y))))
					(function
					 (lambda (x y)
					   (> (car x) (car y))))))))
	    t)))))

(efs-defun efs-S-converter nil (&optional regexp reverse)
  ;; Converts listing without the S-switch, to ones with it.
  nil) ; by default assume that we cannot work.

(efs-fset 'efs-S-converter 'unix 'efs-unix-S-converter)
(efs-fset 'efs-S-converter 'sysV-unix 'efs-unix-S-converter)
(efs-fset 'efs-S-converter 'apollo-unix 'efs-unix-S-converter)
(efs-fset 'efs-S-converter 'bsd-unix 'efs-unix-S-converter)
(efs-fset 'efs-S-converter 'dumb-unix 'efs-unix-S-converter)
(efs-fset 'efs-S-converter 'dumb-apollo-unix 'efs-unix-S-converter)
(efs-fset 'efs-S-converter 'super-dumb-unix 'efs-unix-S-converter)

(defun efs-rS-converter (listing-type &optional regexp)
  ;; Reverse S switch.
  (efs-S-converter listing-type regexp t))

(defun efs-unix-X-converter (&optional regexp reverse)
  (if regexp
      nil
    (goto-char (point-min))
    (efs-save-match-data
      (if (re-search-forward efs-month-and-time-regexp nil t)
	  (let (next list list-start fnstart eol start end link-p)
	    (beginning-of-line)
	    (setq list-start (point))
	    (while (progn
		     (setq start (point))
		     (skip-chars-forward "0-9 ")
		     (setq link-p (= (following-char) ?l))
		     (end-of-line)
		     (setq eol (point)
			   next (1+ eol))
		     (goto-char start)
		     (re-search-forward efs-month-and-time-regexp eol t))
	      ;; Need to measure wrto the current month
	      ;; There is a bug here if because of time-zone shifts, the
	      ;; local machine and the remote one are on different months.
	      (setq fnstart (point))
	      (or (and link-p (search-forward " -> " eol t)
		       (goto-char (match-beginning 0)))
		  (goto-char eol))
	      (setq end (point))
	      (skip-chars-backward "^." fnstart)
	      (setq list
		    (cons
		     (cons
		      (if (= (point) fnstart)
			  ""
			(buffer-substring (point) end))
		      (buffer-substring start next))
		     list))
	      (goto-char next))
	    (delete-region list-start (point))
	    (apply 'insert
		   (mapcar 'cdr
			   (sort list (if reverse
					  (function
					   (lambda (x y)
					     (string< (car y) (car x))))
					(function
					 (lambda (x y)
					   (string< (car x) (car y))))))))
	    t)))))

(efs-defun efs-X-converter nil (&optional regexp reverse)
  ;; Sort on file name extension.  By default do nothing
  nil)

(defun efs-rX-converter (listing-type &optional regexp)
  (efs-X-converter listing-type regexp t))

(efs-fset 'efs-X-converter 'unix 'efs-unix-X-converter)
(efs-fset 'efs-X-converter 'sysV-unix 'efs-unix-X-converter)
(efs-fset 'efs-X-converter 'apollo-unix 'efs-unix-X-converter)
(efs-fset 'efs-X-converter 'bsd-unix 'efs-unix-X-converter)
(efs-fset 'efs-X-converter 'dumb-unix 'efs-unix-X-converter)
(efs-fset 'efs-X-converter 'dumb-apollo-unix 'efs-unix-X-converter)
(efs-fset 'efs-X-converter 'super-dumb-unix 'efs-unix-X-converter)

;;; Brief listings

;;; The following functions do a heap better at packing than
;;; the usual ls listing.  A variable column width is used.
(defun efs-column-widths (columns list &optional across)
  ;; Returns the column widths for breaking LIST into
  ;; COLUMNS number of columns.
  (cond
   ((null list)
    nil)
   ((= columns 1)
    (list (apply 'max (mapcar 'length list))))
   ((let* ((len (length list))
	   (col-length (/ len columns))
	   (remainder (% len columns))
	   (i 0)
	   (j 0)
	   (max-width 0)
	   widths padding)
      (if (zerop remainder)
	  (setq padding 0)
	(setq col-length (1+ col-length)
	      padding (- columns remainder)))
      (setq list (nconc (copy-sequence list) (make-list padding nil)))
      (setcdr (nthcdr (1- (+ len padding)) list) list)
      (while (< i columns)
	(while (< j col-length)
	  (setq max-width (max max-width (length (car list)))
		list (if across (nthcdr columns list) (cdr list))
		j (1+ j)))
	(setq widths (cons (+ max-width 2) widths)
	      max-width 0
	      j 0
	      i (1+ i))
	(if across (setq list (cdr list))))
      (setcar widths (- (car widths) 2))
      (nreverse widths)))))
  
(defun efs-calculate-columns (list &optional across)
  ;; Returns a list of integers which are the column widths that best pack
  ;; LIST, a list of strings, onto the screen.
  (and list
       (let* ((width (1- (window-width)))
	      (columns (max 1 (/ width
				 (+ 2 (apply 'max (mapcar 'length list))))))
	      col-list last-col-list)
	 (while (<= (apply '+ (setq col-list
				    (efs-column-widths columns list across)))
		    width)
	   (setq columns (1+ columns)
		 last-col-list col-list))
	 (or last-col-list col-list))))

(defun efs-format-columns-of-files (files &optional across)
  ;; Returns the number of lines used.
  ;; If ACROSS is non-nil, sorts across rather than down the buffer, like
  ;; ls -x
  ;; A beefed up version of the function in dired. Thanks Sebastian.
  (and files
       (let* ((columns (efs-calculate-columns files across))
	      (ncols (length columns))
	      (ncols1 (1- ncols))
	      (nfiles (length files))
	      (nrows (+ (/ nfiles ncols)
			(if (zerop (% nfiles ncols)) 0 1)))
	      (space-left (- (window-width) (apply '+ columns) 1))
	      (stretch (/ space-left ncols1))
	      (float-stretch (if (zerop ncols1) 0 (% space-left ncols1)))
	      (i 0)
	      (j 0)
	      (result "")
	      file padding)
	 (setq files (nconc (copy-sequence files) ; fill up with empty fns
			    (make-list (- (* ncols nrows) nfiles) "")))
	 (setcdr (nthcdr (1- (length files)) files) files) ; make circular
	 (while (< j nrows)
	   (while (< i ncols)
	     (setq result (concat result (setq file (car files))))
	     (setq padding (- (nth i columns) (length file)))
	     (or (= i ncols1)
		 (progn
		   (setq padding (+ padding stretch))
		   (if (< i float-stretch) (setq padding (1+ padding)))))
	     (setq result (concat result (make-string padding ?\ )))
	     (setq files (if across (cdr files) (nthcdr nrows files))
		   i (1+ i)))
	   (setq result (concat result "\n"))
	   (setq i 0
		 j (1+ j))
	   (or across (setq files (cdr files))))
	 result)))

(defun efs-brief-converter (host-type file-table F a A p x C &optional regexp)
  ;; Builds a brief directory listing for file cache, with
  ;; possible switches F, a, A, p, x.
  (efs-save-match-data
   (let (list ent modes)
     (efs-map-hashtable
      (function
       (lambda (key val)
	 (if (and
	      (efs-really-file-p host-type key val)
	      (or a
		  (and A (not (or (string-equal "." key)
				  (string-equal ".." key))))
		  (/= (string-to-char key) ?.))
	      (or (null regexp)
		  (string-match regexp key)))
	     (setq ent (car val)
		   modes (nth 3 val)
		   list (cons
			 (cond ((null (or F p))
				key)
			       ((eq t ent)
			      (concat key "/"))
			       ((cond
				 ((null F)
				  key)
				 ((stringp ent)
				  (concat key "@"))
				 ((null modes)
				  key)
				 ((eq (string-to-char modes) ?s)
				  ;; a socket
				  (concat key "="))
				 ((or
				   (memq (elt modes 3) '(?x ?s ?t))
				   (memq (elt modes 6) '(?x ?s ?t))
				   (memq (elt modes 9) '(?x ?s ?t)))
				  (concat key "*"))
				 (t
				  key))))
			 list)))))
      file-table)
     (setq list (sort list 'string<))
     (if (or C x)
	 (efs-format-columns-of-files list x)
       (concat (mapconcat 'identity list "\n") "\n")))))

;;; Store converters.

;; The cheaters.
(efs-add-ls-converter "-al" nil (function
				 (lambda (listing-type &optional regexp)
				   (null regexp))))
(efs-add-ls-converter "-Al" nil (function
				 (lambda (listing-type &optional regexp)
				   (null regexp))))
(efs-add-ls-converter "-alF" nil (function
				  (lambda (listing-type &optional regexp)
				    (null regexp))))
(efs-add-ls-converter "-AlF" nil (function
				  (lambda (listing-type &optional regexp)
				    (null regexp))))

(efs-add-ls-converter "-alt" "-al" 'efs-t-converter)
(efs-add-ls-converter "-Alt" "-Al" 'efs-t-converter)
(efs-add-ls-converter "-lt" "-l" 'efs-t-converter)
(efs-add-ls-converter "-altF" "-alF" 'efs-t-converter)
(efs-add-ls-converter "-AltF" "-AlF" 'efs-t-converter)
(efs-add-ls-converter "-ltF" "-lF" 'efs-t-converter)
(efs-add-ls-converter "-alt" nil 'efs-t-converter)
(efs-add-ls-converter "-altF" nil 'efs-t-converter) 
(efs-add-ls-converter "-Alt" nil 'efs-t-converter)  ; cheating a bit
(efs-add-ls-converter "-AltF" nil 'efs-t-converter) ; cheating a bit

(efs-add-ls-converter "-altr" "-al" 'efs-rt-converter)
(efs-add-ls-converter "-Altr" "-Al" 'efs-rt-converter)
(efs-add-ls-converter "-ltr" "-l" 'efs-rt-converter)
(efs-add-ls-converter "-altFr" "-alF" 'efs-rt-converter)
(efs-add-ls-converter "-AltFr" "-AlF" 'efs-rt-converter)
(efs-add-ls-converter "-ltFr" "-lF" 'efs-rt-converter)
(efs-add-ls-converter "-altr" nil 'efs-rt-converter)
(efs-add-ls-converter "-Altr" nil 'efs-rt-converter)

(efs-add-ls-converter "-alr" "-alt" 'efs-alpha-converter)
(efs-add-ls-converter "-Alr" "-Alt" 'efs-alpha-converter)
(efs-add-ls-converter "-lr" "-lt" 'efs-alpha-converter)
(efs-add-ls-converter "-alFr" "-alFt" 'efs-alpha-converter)
(efs-add-ls-converter "-AlFr" "-AlFt" 'efs-alpha-converter)
(efs-add-ls-converter "-lFr" "-lFt" 'efs-alpha-converter)

(efs-add-ls-converter "-al" "-alt" 'efs-alpha-converter)
(efs-add-ls-converter "-Al" "-Alt" 'efs-alpha-converter)
(efs-add-ls-converter "-l" "-lt" 'efs-alpha-converter)
(efs-add-ls-converter "-alF" "-alFt" 'efs-alpha-converter)
(efs-add-ls-converter "-AlF" "-AlFt" 'efs-alpha-converter)
(efs-add-ls-converter "-lF" "-lFt" 'efs-alpha-converter)
(efs-add-ls-converter nil "-alt" 'efs-alpha-converter)

(efs-add-ls-converter "-alr" "-al" 'efs-ralpha-converter)
(efs-add-ls-converter "-Alr" "-Al" 'efs-ralpha-converter)
(efs-add-ls-converter "-lr" "-l" 'efs-ralpha-converter)
(efs-add-ls-converter "-alFr" "-alF" 'efs-ralpha-converter)
(efs-add-ls-converter "-lAFr" "-lAF" 'efs-ralpha-converter)
(efs-add-ls-converter "-lFr" "-lF" 'efs-ralpha-converter)
(efs-add-ls-converter "-alr" nil 'efs-ralpha-converter)

(efs-add-ls-converter "-alr" "-alt" 'efs-ralpha-converter)
(efs-add-ls-converter "-Alr" "-Alt" 'efs-ralpha-converter)
(efs-add-ls-converter "-lr" "-lt" 'efs-ralpha-converter)
(efs-add-ls-converter "-alFr" "-alFt" 'efs-ralpha-converter)
(efs-add-ls-converter "-lAFr" "-lAFt" 'efs-ralpha-converter)
(efs-add-ls-converter "-lFr" "-lFt" 'efs-ralpha-converter)

(efs-add-ls-converter "-alS" "-al" 'efs-S-converter)
(efs-add-ls-converter "-AlS" "-Al" 'efs-S-converter)
(efs-add-ls-converter "-lS" "-l" 'efs-S-converter)
(efs-add-ls-converter "-alSF" "-alF" 'efs-S-converter)
(efs-add-ls-converter "-AlSF" "-AlF" 'efs-S-converter)
(efs-add-ls-converter "-lSF" "-lF" 'efs-S-converter)
(efs-add-ls-converter "-alS" nil 'efs-S-converter)

(efs-add-ls-converter "-alSr" "-al" 'efs-rS-converter)
(efs-add-ls-converter "-AlSr" "-Al" 'efs-rS-converter)
(efs-add-ls-converter "-lSr" "-l" 'efs-rS-converter)
(efs-add-ls-converter "-alSFr" "-alF" 'efs-rS-converter)
(efs-add-ls-converter "-AlSFr" "-AlF" 'efs-rS-converter)
(efs-add-ls-converter "-lSFr" "-lF" 'efs-rS-converter)
(efs-add-ls-converter "-alSr" nil 'efs-rS-converter)

(efs-add-ls-converter "-alS" "-alt" 'efs-S-converter)
(efs-add-ls-converter "-AlS" "-Alt" 'efs-S-converter)
(efs-add-ls-converter "-lS" "-lt" 'efs-S-converter)
(efs-add-ls-converter "-alSF" "-alFt" 'efs-S-converter)
(efs-add-ls-converter "-AlSF" "-AlFt" 'efs-S-converter)
(efs-add-ls-converter "-lSF" "-lFt" 'efs-S-converter)

(efs-add-ls-converter "-alSr" "-alt" 'efs-rS-converter)
(efs-add-ls-converter "-AlSr" "-Alt" 'efs-rS-converter)
(efs-add-ls-converter "-lSr" "-lt" 'efs-rS-converter)
(efs-add-ls-converter "-alSFr" "-alFt" 'efs-rS-converter)
(efs-add-ls-converter "-AlSFr" "-AlFt" 'efs-rS-converter)
(efs-add-ls-converter "-lSFr" "-lFt" 'efs-rS-converter)

(efs-add-ls-converter "-AlX" nil 'efs-X-converter)
(efs-add-ls-converter "-alX" nil 'efs-X-converter)
(efs-add-ls-converter "-AlXr" nil 'efs-rX-converter)
(efs-add-ls-converter "-alXr" nil 'efs-rX-converter)

(efs-add-ls-converter "-alX" "-al" 'efs-X-converter)
(efs-add-ls-converter "-AlX" "-Al" 'efs-X-converter)
(efs-add-ls-converter "-lX" "-l" 'efs-X-converter)
(efs-add-ls-converter "-alXF" "-alF" 'efs-X-converter)
(efs-add-ls-converter "-AlXF" "-AlF" 'efs-X-converter)
(efs-add-ls-converter "-lXF" "-lF" 'efs-X-converter)

(efs-add-ls-converter "-alXr" "-al" 'efs-rX-converter)
(efs-add-ls-converter "-AlXr" "-Al" 'efs-rX-converter)
(efs-add-ls-converter "-lXr" "-l" 'efs-rX-converter)
(efs-add-ls-converter "-alXFr" "-alF" 'efs-rX-converter)
(efs-add-ls-converter "-AlXFr" "-AlF" 'efs-rX-converter)
(efs-add-ls-converter "-lXFr" "-lF" 'efs-rX-converter)

;;; Converters for efs-files-hashtable

(efs-add-ls-converter
 "" t (function
       (lambda (host-type files &optional regexp)
	 (efs-brief-converter host-type files
			      nil nil nil nil nil nil regexp))))
(efs-add-ls-converter
 "-C" t (function
	 (lambda (host-type files &optional regexp)
	   (efs-brief-converter host-type files
				nil nil nil nil nil t regexp))))
(efs-add-ls-converter
 "-F" t (function
	 (lambda (host-type files &optional regexp)
	   (efs-brief-converter host-type files
				t nil nil nil nil nil regexp))))
(efs-add-ls-converter
 "-p" t (function
	 (lambda (host-type files &optional regexp)
	   (efs-brief-converter host-type files
				nil nil nil t nil nil regexp))))
(efs-add-ls-converter
 "-CF" t (function
	  (lambda (host-type files &optional regexp)
	     (efs-brief-converter host-type files
				  t nil nil nil nil t regexp))))
(efs-add-ls-converter
 "-Cp" t (function
	  (lambda (host-type files &optional regexp)
	    (efs-brief-converter host-type files nil nil nil t nil t regexp))))
(efs-add-ls-converter
 "-x" t (function
	 (lambda (host-type files &optional regexp)
	   (efs-brief-converter host-type files
				nil nil nil nil t nil regexp))))
(efs-add-ls-converter
 "-xF" t (function
	  (lambda (host-type files &optional regexp)
	    (efs-brief-converter host-type files t nil nil nil t nil regexp))))
(efs-add-ls-converter
 "-xp" t (function
	  (lambda (host-type files &optional regexp)
	    (efs-brief-converter host-type files nil nil nil t t nil regexp))))
(efs-add-ls-converter
 "-Ca" t (function
	  (lambda (host-type files &optional regexp)
	    (efs-brief-converter host-type files nil t nil nil nil t regexp))))
(efs-add-ls-converter
 "-CFa" t (function
	   (lambda (host-type files &optional regexp)
	     (efs-brief-converter host-type files t t nil nil nil t regexp))))
(efs-add-ls-converter
 "-Cpa" t (function
	   (lambda (host-type files &optional regexp)
	     (efs-brief-converter host-type files nil t nil t nil t regexp))))
(efs-add-ls-converter
 "-xa" t (function
	  (lambda (host-type files &optional regexp)
	    (efs-brief-converter host-type files nil t nil nil t nil regexp))))
(efs-add-ls-converter
 "-xFa" t (function
	   (lambda (host-type files &optional regexp)
	     (efs-brief-converter host-type files t t nil nil t nil regexp))))
(efs-add-ls-converter
 "-xpa" t (function
	   (lambda (host-type files &optional regexp)
	     (efs-brief-converter host-type files nil t nil t t nil regexp))))
(efs-add-ls-converter
 "-CA" t (function
	  (lambda (host-type files &optional regexp)
	    (efs-brief-converter host-type files nil nil t nil nil t regexp))))
(efs-add-ls-converter
 "-CFA" t (function
	   (lambda (host-type files &optional regexp)
	     (efs-brief-converter host-type files t nil t nil nil t regexp))))
(efs-add-ls-converter
 "-CpA" t (function
	   (lambda (host-type files &optional regexp)
	     (efs-brief-converter host-type files nil nil t t nil t regexp))))
(efs-add-ls-converter
 "-xA" t (function
	  (lambda (host-type files &optional regexp)
	    (efs-brief-converter host-type files nil nil t nil t nil regexp))))
(efs-add-ls-converter
 "-xFA" t (function
	   (lambda (host-type files &optional regexp)
	     (efs-brief-converter host-type files t nil t nil t nil regexp))))
(efs-add-ls-converter
 "-xpA" t (function
	   (lambda (host-type files &optional regexp)
	     (efs-brief-converter host-type files nil nil t t t nil regexp))))

;;;; ------------------------------------------------------------
;;;; Directory Listing Parsers
;;;; ------------------------------------------------------------

(defconst efs-unix:dl-listing-regexp
  "^[^ \n\t]+\n? +\\([0-9]+\\|-\\|=\\) ")

;; Note to progammers:
;; Below are a series of macros and functions used for parsing unix
;; file listings. They are intended only to be used together, so be careful
;; about using them out of context.

(defmacro efs-ls-parse-file-line ()
  ;; Extract the filename, size, and permission string from the current
  ;; line of a dired-like listing. Assumes that the point is at
  ;; the beginning of the line, leaves it just before the size entry.
  ;; Returns a list (name size perm-string nlinks owner).
  ;; If there is no file on the line, returns nil.
  (` (let ((eol (save-excursion (end-of-line) (point)))
	   name size modes nlinks owner)
       (skip-chars-forward " 0-9" eol)
       (and
	(looking-at efs-modes-links-owner-regexp)
	(setq modes (buffer-substring (match-beginning 1)
				      (match-end 1))
	      nlinks (string-to-int (buffer-substring (match-beginning 2)
						      (match-end 2)))
	      owner (buffer-substring (match-beginning 3) (match-end 3)))
	(re-search-forward efs-month-and-time-regexp eol t)
	(setq name (buffer-substring (point) eol)
	      size (string-to-int (buffer-substring (match-beginning 1)
						    (match-end 1))))
	(list name size modes nlinks owner)))))
	      
(defun efs-relist-symlink (host user symlink path switches)
  ;; Does a re-list of a single symlink in efs-data-buffer-name-2,
  ;; HOST = remote host
  ;; USER = remote username
  ;; SYMLINK = symbolic link name as a remote fullpath
  ;; PATH = efs full path syntax for the dir. being listed
  ;; SWITCHES = ls switches to use for the re-list
  ;; Returns (symlink-name symlink-target), as given by the listing. Returns
  ;; nil if the listing fails.
  ;; Does NOT correct for any symlink marking.
  (let* ((temp (efs-make-tmp-name host nil))
	 (temp-file (car temp))
	 (default-major-mode 'fundamental-mode)
	 spot)
    (unwind-protect
	(and
	 (prog1
	     (null
	      (car
	       (efs-send-cmd host user
			     (list 'dir symlink (cdr temp) switches)
			     (format "Listing %s"
				     (efs-relativize-filename
				      (efs-replace-path-component
				       path symlink))))))
	   ;; Put the old message back.
	   (if (and efs-verbose
		    (not (and (boundp 'dired-in-query) dired-in-query)))
	       (message "Listing %s..."
			(efs-relativize-filename path))))
	 (save-excursion
	   (if (efs-ftp-path temp-file)
	       (efs-add-file-entry (efs-host-type efs-gateway-host)
				   temp-file nil nil nil))
	   (set-buffer (get-buffer-create efs-data-buffer-name-2))
	   (erase-buffer)
	   (if (or (file-readable-p temp-file)
		   (sleep-for efs-retry-time)
		   (file-readable-p temp-file))
	       (let (efs-verbose)
		 (insert-file-contents temp-file))
	     (efs-error host user
			(format
			 "list data file %s not readable" temp-file)))
	   (skip-chars-forward " 0-9")
	   (and
	    (eq (following-char) ?l)
	    (re-search-forward efs-month-and-time-regexp nil t)
	    (setq spot (point))
	    (re-search-forward " -> " nil t)
	    (progn
	      (end-of-line)
	      (list
	       ;; We might get the full path in the listing.
	       (file-name-nondirectory
		(buffer-substring spot (match-beginning 0)))
	       (buffer-substring (match-end 0) (point)))))))
      (efs-del-tmp-name temp-file))))

(defun efs-ls-sysV-p (host user dir linkname path)
  ;; Returns t if the symlink is listed in sysV style. i.e. The
  ;; symlink name is marked with an @.
  ;; HOST = remote host name
  ;; USER = remote user name
  ;; DIR = directory being listed as a remote full path.
  ;; LINKNAME = relative name of symbolic link as derived from an ls -..F...
  ;;            this is assumed to end with an @
  ;; PATH = efs full path synatx for the directory
  (let ((link (car (efs-relist-symlink
		    host user
		    (concat dir (substring linkname 0 -1))
		    path "-lFd" ))))
    (and link (string-equal link linkname))))

(defun efs-ls-next-p (host user dir linkname target path)
  ;; Returns t is the symlink is marked in the NeXT style.
  ;; i.e. The symlink destination is marked with an @.
  ;; This assumes that the host-type has already been identified
  ;; as NOT sysV-unix, and that target ends in an "@".
  ;; HOST = remote host name
  ;; USER = remote user name
  ;; DIR = remote directory being listed, as a remore full path
  ;; LINKNAME = relative name of symbolic link
  ;;            Since we've eliminated sysV, it won't be marked with an @
  ;; TARGET = target of symbolic link, as derived from an ls -..F..
  ;; PATH = directory being listed in full efs path syntax.
  (let ((no-F-target (nth 1 (efs-relist-symlink
			     host user
			     (concat dir linkname)
			     path "-ld"))))
    (and no-F-target
	 (string-equal (concat no-F-target "@") target))))

;; This deals with the F switch. Should also do something about
;; unquoting names obtained with the SysV b switch and the GNU Q
;; switch. See Sebastian's dired-get-filename.

(defun efs-ls-parser (host-type host user dir path switches)
  ;; Meant to be called by efs-parse-listing.
  ;; Assumes that point is at the beginning of the first file line.
  ;; Assumes that SWITCHES has already been bound to nil for a dumb host.
  ;; HOST-TYPE is the remote host-type
  ;; HOST is the remote host name
  ;; USER is the remote user name
  ;; DIR is the remote directory as a full path
  ;; PATH is the directory in full efs syntax, and directory syntax.
  ;; SWITCHES is the ls listing switches
  (let ((tbl (efs-make-hashtable))
	(used-F (and switches (string-match "F" switches)))
	(old-tbl (efs-get-files-hashtable-entry path))
	file-type symlink directory file size modes nlinks owner)
    (while (setq file (efs-ls-parse-file-line))
      (setq size (nth 1 file)
	    modes (nth 2 file)
	    nlinks (nth 3 file)
	    owner (nth 4 file)
	    file (car file)
	    file-type (string-to-char modes)
	    directory (eq file-type ?d))
      (if (eq file-type ?l)
	  (if (string-match " -> " file)
	      (setq symlink (substring file (match-end 0))
		    file (substring file 0 (match-beginning 0)))
	    ;; Shouldn't happen
	    (setq symlink ""))
	(setq symlink nil))
      (if used-F
	  ;; The F-switch jungle
	  (let ((socket (eq file-type ?s))
		(fifo (eq file-type ?p))
		(executable
		 (and (not symlink) ; x bits don't mean a thing for symlinks
		      (or (memq (elt modes 3) '(?x ?s ?t))
			  (memq (elt modes 6) '(?x ?s ?t))
			  (memq (elt modes 9) '(?x ?s ?t))))))
	    ;; Deal with marking of directories, executables, and sockets.
	    (if (or (and executable (string-match "*$" file))
		    (and socket (string-match "=$" file))
		    (and fifo (string-match "|$" file)))
		(setq file (substring file 0 -1))
	      ;; Do the symlink dance.
	      (if symlink
		  (let ((fat-p (string-match "@$" file))
			(sat-p (string-match "@$" symlink)))
		    (cond
		     ;; Those that mark the file
		     ((and (memq host-type '(sysV-unix apollo-unix)) fat-p)
		      (setq file (substring file 0 -1)))
		     ;; Those that mark nothing
		     ((memq host-type '(bsd-unix dumb-unix)))
		     ;; Those that mark the target
		     ((and (eq host-type 'next-unix) sat-p)
		      (setq symlink (substring symlink 0 -1)))
		     ;; We don't know
		     ((eq host-type 'unix)
		      (if fat-p
			  (cond
			   ((efs-ls-sysV-p host user dir
					   file path)
			    (setq host-type 'sysV-unix
				  file (substring file 0 -1))
			    (efs-add-host 'sysV-unix host)
			    (efs-add-listing-type 'sysV-unix host user))
			   ((and sat-p
				 (efs-ls-next-p host user dir file symlink
						path))
			    (setq host-type 'next-unix
				  symlink (substring symlink 0 -1))
			    (efs-add-host 'next-unix host)
			    (efs-add-listing-type 'next-unix host user))
			   (t
			    (setq host-type 'bsd-unix)
			    (efs-add-host 'bsd-unix host)
			    (efs-add-listing-type 'bsd-unix host user)))
			(if (and sat-p
				 (efs-ls-next-p host user dir file
						symlink path))
			    (progn
			      (setq host-type 'next-unix
				    symlink (substring symlink 0 -1))
			      (efs-add-host 'next-unix host)
			      (efs-add-listing-type 'next-unix host user))
			  (setq host-type 'bsd-unix)
			  (efs-add-host 'bsd-unix host)
			  (efs-add-listing-type 'bsd-unix host user)))))
		    ;; Look out for marking of symlink
		    ;; If we really wanted to, at this point we
		    ;; could distinguish aix from hp-ux, ultrix, irix and a/ux,
		    ;; allowing us to skip the re-list in the future, for the
		    ;; later 4 host types. Another version...
		    (if (string-match "[=|*]$" symlink)
			(let ((relist (efs-relist-symlink
				       host user (concat dir file)
				       path "-dl")))
			  (if relist (setq symlink (nth 1 relist))))))))))
      ;; Strip / off the end unconditionally.  It's not a valid file character
      ;; anyway.
      (if (string-match "/$" file) (setq file (substring file 0 -1)))
      (let ((mdtm (and old-tbl (nth 5 (efs-get-hash-entry file old-tbl)))))
	(if mdtm
	    (efs-put-hash-entry file (list (or symlink directory) size owner
					   modes nlinks mdtm) tbl)
	  (efs-put-hash-entry file (list (or symlink directory) size owner
					 modes nlinks) tbl)))
      (forward-line 1))
    (efs-put-hash-entry "." '(t) tbl)
    (efs-put-hash-entry ".." '(t) tbl)
    tbl))

(efs-defun efs-parse-listing nil (host user dir path &optional switches)
  ;; Parse the a listing which is assumed to be from some type of unix host.
  ;; Note that efs-key will be bound to the actual host type.
  ;; HOST = remote host name
  ;; USER = remote user name
  ;; DIR = directory as a remote full path
  ;; PATH = directory in full efs path syntax
  ;; SWITCHES = ls switches used for the listing
  (efs-save-match-data
    (cond
     ;; look for total line
     ((looking-at "^total [0-9]+$")
      (forward-line 1)
      ;; Beware of machines that put a blank line after the totals line.
      (skip-chars-forward " \t\n")
      (efs-ls-parser efs-key host user dir path switches))
     ;; look for errors
     ((looking-at "[^\n]+\\( not found\\|: Not a directory\\)\n\\'")
      ;; It's an ls error message.
      nil)
     ((eobp) ; i.e. zerop buffer-size
      nil) ; assume an ls error message
     ;; look for listings without total lines
     ((re-search-forward efs-month-and-time-regexp nil t)
      (beginning-of-line)
      (efs-ls-parser efs-key host user dir path switches))
     (t nil))))

(efs-defun efs-parse-listing unix:unknown
  (host user dir path &optional switches)
  ;; Parse the a listing which is assumed to be from some type of unix host,
  ;; possibly one doing a dl listing.
  ;; HOST = remote host name
  ;; USER = remote user name
  ;; DIR = directory as a remote full path
  ;; PATH = directory in full efs path syntax
  ;; SWITCHES = ls switches used for the listing
 (efs-save-match-data
   (cond
    ;; look for total line
    ((looking-at "^total [0-9]+$")
     (forward-line 1)
     ;; Beware of machines that put a blank line after the totals line.
     (skip-chars-forward " \t\n")
     ;; This will make the listing-type track the host-type.
     (efs-add-listing-type nil host user)
     (efs-ls-parser 'unix host user dir path switches))
    ;; look for errors
    ((looking-at "[^\n]+\\( not found\\|: Not a directory\\)\n\\'")
     ;; It's an ls error message.
     nil)
    ((eobp) ; i.e. zerop buffer-size
     nil) ; assume an ls error message
    ;; look for listings without total lines
    ((and (re-search-forward efs-month-and-time-regexp nil t)
	  (progn
	    (beginning-of-line)
	    (looking-at efs-modes-links-owner-regexp)))
     (efs-add-listing-type nil host user)
     (efs-ls-parser 'unix host user dir path switches))
    ;; look for dumb listings
    ((re-search-forward
      (concat (regexp-quote switches)
	      " not found\\|\\(^ls: +illegal option -- \\)")
      (save-excursion (end-of-line) (point)) t)
     (if (eq (efs-host-type host) 'apollo-unix)
	 (progn
	   (efs-add-host 'dumb-apollo-unix host)
	   (efs-add-listing-type 'dumb-apollo-unix host user))
       (efs-add-host 'dumb-unix host)
       (efs-add-listing-type 'dumb-unix host user))
     (if (match-beginning 1)
	 ;; Need to try to list again.
	 (let ((efs-ls-uncache t))
	   (efs-ls
	    path nil (format "Relisting %s" (efs-relativize-filename path)) t)
	   (goto-char (point-min))
	   (efs-parse-listing nil host user dir path switches))
       (if (re-search-forward "^total [0-9]+$" nil t)
	   (progn
	     (beginning-of-line)
	     (delete-region (point-min) (point))
	     (forward-line 1)
	     (efs-ls-parser 'dumb-unix host user dir path switches)))))
    ;; Look for dl listings.
    ((re-search-forward  efs-unix:dl-listing-regexp nil t)
     (efs-add-host 'unix host)
     (efs-add-listing-type 'unix:dl host user)
     (efs-parse-listing 'unix:dl host user dir path switches))
    ;; don't know, return nil
    (t nil))))

(defun efs-ls-parse-1-liner (filename buffer &optional symlink)
  ;; Parse a 1-line listing for FILENAME in BUFFER, and update
  ;; the cached info for FILENAME.
  ;; Optional SYMLINK arg gives the expected target of a symlink.
  ;; Since one-line listings are usually used to update info for
  ;; newly created files, we usually know what sort of a file to expect.
  ;; Actually trying to parse out the symlink target could be impossible
  ;; for some types of switches.
  (efs-save-buffer-excursion
    (set-buffer buffer)
    (goto-char (point-min))
    (skip-chars-forward " 0-9")
    (efs-save-match-data
      (let (modes nlinks owner size)
      (and
       (looking-at efs-modes-links-owner-regexp)
       (setq modes (buffer-substring (match-beginning 1) (match-end 1))
	     nlinks (string-to-int (buffer-substring (match-beginning 2)
						     (match-end 2)))
	     owner (buffer-substring (match-beginning 3) (match-end 3)))
       (re-search-forward efs-month-and-time-regexp nil t)
       (setq size (string-to-int (buffer-substring (match-beginning 1)
						   (match-end 1))))
       (let* ((filename (directory-file-name filename))
	      (files (efs-get-files-hashtable-entry
		      (file-name-directory filename))))
	 (if files
	     (let* ((key (efs-get-file-part filename))
		    (ignore-case (memq (efs-host-type
					(car (efs-ftp-path filename)))
				       efs-case-insensitive-host-types))
		    (ent (efs-get-hash-entry key files ignore-case))
		    (mdtm (nth 5 ent))
		    type)
	       (if (= (string-to-char modes) ?l)
		   (setq type
			 (cond
			  ((stringp symlink)
			   symlink)
			  ((stringp (car ent))
			   (car ent))
			  (t ; something weird happened.
			   "")))
		 (if (= (string-to-char modes) ?d)
		     (setq type t)))
	       (efs-put-hash-entry
		key (list type size owner modes nlinks mdtm)
		files ignore-case)))))))))

(efs-defun efs-update-file-info nil (file buffer &optional symlink)
  "For FILE, update cache information from a single file listing in BUFFER."
  ;; By default, this does nothing.
  nil)

(efs-defun efs-update-file-info unix (file buffer &optional symlink)
  (efs-ls-parse-1-liner file buffer))
(efs-defun efs-update-file-info sysV-unix (file buffer &optional symlink)
  (efs-ls-parse-1-liner file buffer))
(efs-defun efs-update-file-info bsd-unix (file buffer &optional symlink)
  (efs-ls-parse-1-liner file buffer))
(efs-defun efs-update-file-info next-unix (file buffer &optional symlink)
  (efs-ls-parse-1-liner file buffer))
(efs-defun efs-update-file-info apollo-unix (file buffer &optional symlink)
  (efs-ls-parse-1-liner file buffer))
(efs-defun efs-update-file-info dumb-unix (file buffer &optional symlink)
  (efs-ls-parse-1-liner file buffer))
(efs-defun efs-update-file-info dumb-apollo-unix
  (file buffer &optional symlink)
  (efs-ls-parse-1-liner file buffer))
(efs-defun efs-update-file-info super-dumb-unix (file buffer &optional symlink)
  (efs-ls-parse-1-liner file buffer))

;;;; ----------------------------------------------------------------
;;;; The 'unknown listing parser. This does some host-type guessing.
;;;; ----------------------------------------------------------------

;;; Regexps for host and listing type guessing from the listing syntax.

(defconst efs-ka9q-listing-regexp
  (concat
   "^\\([0-9,.]+\\|No\\) files\\. [0-9,.]+ bytes free\\. "
   "Disk size [0-9,]+ bytes\\.$"))
;; This version of the regexp is really for hosts which allow some switches,
;; but not ours. Rather than determine which switches we could be using
;; we just assume that it's dumb.
(defconst efs-dumb-unix-listing-regexp
  (concat
   "^[Uu]sage: +ls +-[a-zA-Z0-9]+[ \n]\\|"
   ;; Unitree server
   "^Error getting stats for \"-[a-zA-Z0-9]+\""))

(defconst efs-dos-distinct-date-and-time-regexp
  (concat
   " \\(Jan\\|Feb\\|Mar\\|Apr\\|May\\|Jun\\|Jul\\|Aug\\|Sep\\|Oct"
   "\\|Nov\\|Dec\\) [ 0-3][0-9],[12][90][0-9][0-9]  "
   "[ 12][0-9]:[0-5][0-9]  "))
;; Regexp to match the output from the hellsoft ftp server to an
;; ls -al. Unfortunately, this looks a lot like some unix ls error
;; messages.
(defconst efs-hell-listing-regexp
  (concat
   "ls: file or directory not found\n\\'\\|"
   "[-d]\\[[-A-Z][-A-Z][-A-Z][-A-Z][-A-Z][-A-Z][-A-Z]\\]"))

(efs-defun efs-parse-listing unknown
  (host user dir path &optional switches)
  "Parse the current buffer which is assumed to contain a dir listing.
Return a hashtable as the result. If the listing is not really a
directory listing, then return nil.

HOST is the remote host's name.
USER is the remote user name.
DIR is the directory as a full remote path.
PATH is the directory in full efs path synatx.
SWITCHES are the switches passed to ls. If SWITCHES is nil, then a
dumb \(with dir\) listing has been done."
  (efs-save-match-data
    (cond
     
     ;; look for total line
     ((looking-at "^total [0-9]+$")
      (efs-add-host 'unix host)
      (forward-line 1)
      ;; Beware of machines that put a blank line after the totals line.
      (skip-chars-forward " \t\n")
      (efs-ls-parser 'unix host user dir path switches))

     ;; Look for hellsoft. Need to do this before looking
     ;; for ls errors, since the hellsoft output looks a lot like an ls error.
     ((looking-at efs-hell-listing-regexp)
      (if (null (car (efs-send-cmd host user '(quote site dos))))
	  (let* ((key (concat host "/" user "/~"))
		 (tilde (efs-get-hash-entry
			 key efs-expand-dir-hashtable)))
	    (efs-add-host 'hell host)
	    ;; downcase the expansion of ~
	    (if (and tilde (string-match "^[^a-z]+$" tilde))
		(efs-put-hash-entry key (downcase tilde)
				    efs-expand-dir-hashtable))
	    ;; Downcase dir, in case its got some upper case stuff in it.
	    (setq dir (downcase dir)
		  path (efs-replace-path-component path dir))
	    (let ((efs-ls-uncache t))
	      ;; This will force the data buffer to be re-filled
	      (efs-ls path nil (format "Relisting %s"
				       (efs-relativize-filename path))
		      t))
	    (efs-parse-listing 'hell host user dir path))
	;; Don't know, give unix a try.
	(efs-add-host 'unix host)
	nil))
     
     ;; look for ls errors
     ((looking-at "[^\n]+\\( not found\\|: Not a directory\\)\n\\'")
      ;; It's an ls error message.
      (efs-add-host 'unix host)
      nil)
     
     ((eobp) ; i.e. (zerop (buffer-size))
      ;; This could be one of:
      ;; (1) An Ultrix ls error message
      ;; (2) A listing with the A switch of an empty directory
      ;;     on a machine which doesn't give a total line.
      ;; (3) The result of an attempt at an nlist. (This would mean a
      ;;     dumb host.)
      ;; (4) The twilight zone.
      (cond
       ((save-excursion
	  (set-buffer (process-buffer
		       (efs-get-process host user)))
	  (save-excursion
	    (goto-char (point-max))
	    (and
	     ;; The dir ftp output starts with a 200 cmd.
	     (re-search-backward "^150 " nil t)
	     ;; We never do an nlist (it's a short listing).
	     ;; If the machine thinks that we did, it's dumb.
	     (looking-at "[^\n]+ NLST "))))
	;; It's dumb-unix or ka9q. Anything else?
	;; This will re-fill the data buffer with a dumb listing.
	(let ((efs-ls-uncache t))
	  (efs-ls path nil (format "Relisting %s"
				   (efs-relativize-filename path))
		  t))
	(cond
	 ;; check for dumb-unix
	 ((re-search-forward efs-month-and-time-regexp nil t)
	  (efs-add-host 'dumb-unix host)
	  (beginning-of-line)
	  (efs-parse-listing 'dumb-unix host user dir path))
	 ;; check for ka9q
	 ((save-excursion
	    (goto-char (point-max))
	    (forward-line -1)
	    (looking-at efs-ka9q-listing-regexp))
	  (efs-add-host 'ka9q host)
	  (efs-parse-listing 'ka9q host user dir path))
	 (t ; Don't know, try unix.
	  (efs-add-host 'unix host)
	  nil)))
       ;; check for Novell Netware
       ((null (car (efs-send-cmd host user '(quote site nfs))))
	(efs-add-host 'netware host)
	(let ((efs-ls-uncache t))
	  (efs-ls path nil (format "Relisting %s"
				   (efs-relativize-filename path))
		  t))
	(efs-parse-listing 'netware host user dir path))
       (t
	;; Assume (1), an Ultrix error message.
	(efs-add-host 'unix host)
	nil)))
     
     ;; unix without a total line
     ((re-search-forward efs-month-and-time-regexp nil t)
      (efs-add-host 'unix host)
      (beginning-of-line)
      (efs-ls-parser 'unix host user dir path switches))
     
     ;; Now we look for host-types, or listing-types which are auto-rec
     ;; by the listing parser, because it's not possible to pick them out
     ;; from a pwd.
     
     ;; check for dumb-unix
     ;; (Guessing of dumb-unix hosts which return an ftp error message is
     ;; done in efs-ls.)
     ((re-search-forward efs-dumb-unix-listing-regexp nil t)
      (efs-add-host 'dumb-unix host)
      ;; This will force the data buffer to be re-filled
      (let ((efs-ls-uncache t))
	(efs-ls path nil (format "Relisting %s"
				 (efs-relativize-filename path))
		t))
      (efs-parse-listing 'dumb-unix host user dir path))
     
     ;; check for Distinct's DOS ftp server
     ((re-search-forward efs-dos-distinct-date-and-time-regexp nil t)
      (efs-add-host 'dos-distinct host)
      (efs-parse-listing 'dos-distinct host user dir path))
     
     ;; check for KA9Q pseudo-unix (LINUX?)
     ((save-excursion
	(goto-char (point-max))
	(forward-line -1)
	(looking-at efs-ka9q-listing-regexp))
      (efs-add-host 'ka9q host)
      ;; This will re-fill the data buffer.
      ;; Need to do this because ka9q is a dumb host.
      (let ((efs-ls-uncache t))
	(efs-ls path nil (format "Relisting %s"
				 (efs-relativize-filename path))
		t))
      (efs-parse-listing 'ka9q host user dir path))
     
     ;; Check for a unix descriptive (dl) listing
     ;; Do this last, because it's hard to guess.
     ((re-search-forward  efs-unix:dl-listing-regexp nil t)
      (efs-add-host 'unix host)
      (efs-add-listing-type 'unix:dl host user)
      (efs-parse-listing 'unix:dl host user dir path switches))

     ;; Don't know what's going on. Return nil, and assume unix.
     (t
      (efs-add-host 'unix host)
      nil))))

;;;; ------------------------------------------------------------
;;;; Directory information hashtable.
;;;; ------------------------------------------------------------

(efs-defun efs-really-file-p nil (file ent)
  ;; efs-files-hashtable sometimes contains fictitious entries, when
  ;; some OS's allow a file to be accessed by another name. For example,
  ;; in VMS the highest version of a file may be accessed by omitting the
  ;; the file version number. This function should return t if the
  ;; filename FILE is really a file. ENT is the hash entry of the file.
  t)

(efs-defun efs-add-file-entry nil (path type size owner
					&optional modes nlinks mdtm)
  ;; Add a new file entry for PATH
  ;; TYPE is nil for a plain file, t for a directory, and a string
  ;;   (the target of the link) for a symlink.
  ;; SIZE is the size of the file in bytes.
  ;; OWNER is the owner of the file, as a string.
  ;; MODES is the file modes, as a string.  In Unix, this will be 10 cars.
  ;; NLINKS is the number of links for the file.
  ;; MDTM is the last modtime obtained for the file.  This is for
  ;;   short-term cache only, as emacs often has sequences of functions
  ;;   doing modtime lookup.  If you really want to be sure of the modtime,
  ;;   use efs-get-file-mdtm, which asks the remote server.
  
  (and (eq type t)
       (setq path (directory-file-name path)))
  (let ((files (efs-get-files-hashtable-entry (file-name-directory path))))
    (if files
	(efs-put-hash-entry
	 (efs-get-file-part path)
	 (cond (mdtm
		(list type size owner modes nlinks
		      mdtm))
	       (nlinks
		(list type size owner modes nlinks))
	       (modes (list type size owner modes))
	       (t (list type size owner)))
	 files
	 (memq efs-key efs-case-insensitive-host-types)))
    (efs-del-from-ls-cache path t nil)))

(efs-defun efs-delete-file-entry nil (path &optional dir-p)
  "Delete the file entry for PATH, if its directory info exists."
  (if dir-p
      (progn 
	(setq path (file-name-as-directory path))
	(efs-del-hash-entry (efs-canonize-file-name path)
			    efs-files-hashtable)
	;; Note that file-name-as-directory followed by
	;; (substring path 0 -1) 
	;; serves to canonicalize directory file names to their unix form.
	;; i.e. in VMS, FOO.DIR -> FOO/ -> FOO
	;; PATH is supposed to be s fully expanded efs-style path.
	(setq path (substring path 0 -1))))
  (let ((files (efs-get-files-hashtable-entry (file-name-directory path))))
    (if files
	(efs-del-hash-entry
	 (efs-get-file-part path)
	 files
	 (memq (efs-host-type (car (efs-ftp-path path)))
	       efs-case-insensitive-host-types))))
  (efs-del-from-ls-cache path t nil)
  (if dir-p (efs-del-from-ls-cache path nil t)))

(defun efs-set-files (directory files)
  "For DIRECTORY, set or change the associated FILES hashtable."
  (if files
      (efs-put-hash-entry
       (efs-canonize-file-name (file-name-as-directory directory))
       files efs-files-hashtable)))

(defun efs-parsable-switches-p (switches &optional full-dir)
  ;; Returns non-nil if SWITCHES would give an ls listing suitable for parsing
  ;; If FULL-DIR is non-nil, the switches must be suitable for parsing a full
  ;; ditectory.
  (or (null switches)
      (efs-save-match-data
	(and (string-match "[aA]" switches)
	     ;; g is not good enough, need l or o for owner.
	     (string-match "[lo]" switches)
	     ;; L shows link target, rather than link. We need both.
	     (not (string-match "[RfL]" switches))
	     (not (and full-dir (string-match "d" switches)))))))

(defun efs-get-files (directory &optional no-error)
  "For DIRECTORY, return a hashtable of file entries.
This will give an error or return nil, depending on the value of
NO-ERROR, if a listing for DIRECTORY cannot be obtained."
  (let ((directory (file-name-as-directory directory)))
    (or (efs-get-files-hashtable-entry directory)
	(and (efs-ls directory (efs-ls-guess-switches) t 'parse no-error)
	     (efs-get-files-hashtable-entry directory)))))

(efs-defun efs-allow-child-lookup nil (host user dir file)
  ;; Returns non-nil if in directory DIR,  FILE could possibly be a subdir
  ;; according to its file-name syntax, and therefore a child listing should
  ;; be attempted. Note that DIR is in directory syntax.
  ;; i.e. /foo/bar/, not /foo/bar.
  ;; Deal with dired. Anything else?
  (not (and (boundp 'dired-local-variables-file)
	    (stringp dired-local-variables-file)
	    (string-equal dired-local-variables-file file))))

(defmacro efs-ancestral-check (host-type path ignore-case)
  ;; Checks to see if something in a path's ancient parentage
  ;; would make it impossible for the path to exist in the directory
  ;; tree. In this case it returns nil. Otherwise returns t (there
  ;; is essentially no information returned in this case, the file
  ;; may exist or not).
  ;; This macro should make working with RCS more efficient.
  ;; It also helps with FTP servers that go into fits if we ask to
  ;; list a non-existent dir.
  ;; Yes, I know that the function mapped over the hashtable can
  ;; be written more cleanly with a concat, but this is faster.
  ;; concat's cause a lot of consing. So do regexp-quote's, but we can't
  ;; avoid it.
  ;; Probably doesn't make much sense for this to be an efs-defun, since
  ;; the host-type dependence is very mild.
  (`
   (let ((path (, path)) ; expand once
	 (ignore-case (, ignore-case))
	 str)
     ;; eliminate flat file systems -- should have a constant for this
     (or (memq (, host-type) '(mts cms mvs cms-knet))
	 (efs-save-match-data
	   (catch 'foo
	     (efs-map-hashtable
	      (function
	       (lambda (key val)
		 (and (eq (string-match (regexp-quote key) path) 0)
		      (setq str (substring path (match-end 0)))
		      (string-match "^[^/]+" str)
		      (not (efs-hash-entry-exists-p
			    (substring str 0 (match-end 0))
			    val ignore-case))
		      (throw 'foo nil))))
	      efs-files-hashtable)
	     t))))))

(defun efs-file-entry-p (path)
  ;; Return whether there is a file entry for PATH.
  ;; Under no circumstances does this cause FTP activity.
  (let* ((path (directory-file-name (efs-canonize-file-name path)))
	 (dir (file-name-directory path))
	 (file (efs-get-file-part path))
	 (tbl (efs-get-files-hashtable-entry dir)))
    (and tbl (efs-hash-entry-exists-p
	      file tbl
	      (memq (efs-host-type (car (efs-ftp-path dir)))
		    efs-case-insensitive-host-types)) t)))

(defun efs-get-file-entry (path)
  "Return the given file entry for PATH.
This is a list of the form \(type size owner modes nlinks modtm\), 
where type is nil for a normal file, t for a directory, and a string for a
symlink, size is the size of the file in bytes, if known, and modes are 
the permission modes of the file as a string. modtm is short-term the
cache of the file modtime.  It is not used by `verify-visited-file-modtime'.
If the file isn't in the hashtable, this returns nil."
  (let* ((path (directory-file-name (efs-canonize-file-name path)))
	 (dir (file-name-directory path))
	 (file (efs-get-file-part path))
	 (parsed (efs-ftp-path dir))
	 (host (car parsed))
	 (host-type (efs-host-type host))
	 (ent (efs-get-files-hashtable-entry dir))
	 (ignore-case (memq host-type efs-case-insensitive-host-types)))
    (if ent
	(efs-get-hash-entry file ent ignore-case)
      (let ((user (nth 1 parsed))
	    (r-dir (nth 2 parsed)))
	(and (efs-ancestral-check host-type path ignore-case)
	     (or (and efs-allow-child-lookup
		      (efs-allow-child-lookup host-type
						   host user r-dir file)
		      (setq ent (efs-get-files path t))
		      (efs-get-hash-entry "." ent))
		 ;; i.e. it's a directory by child lookup
		 (efs-get-hash-entry
		  file (efs-get-files dir) ignore-case)))))))

(defun efs-wipe-file-entries (host user)
  "Remove cache data for all files on HOST and USER.
This replaces the file entry information hashtable with one that
doesn't have any entries for the given HOST, USER pair."
  (let ((new-tbl (efs-make-hashtable (length efs-files-hashtable)))
	(host (downcase host))
	(case-fold (memq (efs-host-type host)
			 efs-case-insensitive-host-types)))
    (if case-fold (setq user (downcase user)))
    (efs-map-hashtable
     (function
      (lambda (key val)
	(let ((parsed (efs-ftp-path key)))
	  (if parsed
	      (let ((h (nth 0 parsed))
		    (u (nth 1 parsed)))
		(or (and (string-equal host (downcase h))
			 (string-equal user (if case-fold (downcase u) u)))
		    (efs-put-hash-entry key val new-tbl)))))))
     efs-files-hashtable)
    (setq efs-files-hashtable new-tbl)))


;;;; ============================================================
;;;; >8
;;;; Redefinitions of standard GNU Emacs functions.
;;;; ============================================================

;;;; ------------------------------------------------------------
;;;; expand-file-name and friends...
;;;; ------------------------------------------------------------

;; New filename expansion code for efs.
;; The overall structure is based around the following internal
;; functions and macros. Since these are internal, they do NOT
;; call efs-save-match-data. This is done by their calling
;; function.
;; 
;; efs-expand-tilde
;;   - expands all ~ constructs, both local and remote.
;; efs-short-circuit-file-name
;;   - short-circuits //'s and /~'s, for both local and remote paths.
;; efs-de-dot-file-name
;;   - canonizes /../ and /./'s in both local and remote paths.
;;
;; The following two functions overload existing emacs functions.
;; They are the entry points to this filename expansion code, and as such
;; call efs-save-match-data.
;; 
;; efs-expand-file-name
;; efs-substitute-in-file-name

;;; utility macros

(defmacro efs-short-circuit-file-name (filename)
  ;; Short-circuits //'s and /~'s in filenames.
  ;; Returns a list consisting of the local path,
  ;; host-type, host, user. For local hosts,
  ;; host-type, host, and user are all nil.
  (`
   (let ((start 0)
	 (string (, filename))
	 backskip regexp lbackskip
	 lregexp parsed host-type host user)

     (if efs-local-apollo-unix
	 (setq lregexp ".//+"
	       lbackskip 2)
       (setq lregexp "//+"
	     lbackskip 1))
     
     ;; Short circuit /user@mach: roots. It is important to do this
     ;; now to avoid unnecessary ftp connections.
     
     (while (string-match efs-path-root-short-circuit-regexp string start)
       (setq start (1+ (match-beginning 0))))
     (or (zerop start) (setq string (substring string start)
			     start 0))
     
     ;; identify remote root
     
     (if (setq parsed (efs-ftp-path-macro string))
	 (if (memq (setq string (nth 2 parsed)
		       host-type
		       (efs-host-type (setq host (car parsed))
				      (setq user (nth 1 parsed))))
		   '(apollo-unix dumb-apollo-unix))
	     (setq regexp ".//+"
		   backskip 2)
	   (setq regexp "//+"
		 backskip 1))
       (setq regexp lregexp
	     backskip lbackskip))
     
     ;; Now short-circuit in an apollo and efs sensitive way.
     
     (while (cond ((string-match regexp string start)
		   (setq start (- (match-end 0) backskip)))
		  ((string-match  "/~" string start)
		   (setq start (1- (match-end 0)))))
       
       (and host-type
	    (null efs-short-circuit-to-remote-root)
	    (setq host-type nil
		  regexp lregexp
		  backskip lbackskip)))
     (or (zerop start) (setq string (substring string start)))
     (list string host-type (and host-type host) (and host-type user)))))

(defmacro efs-expand-tilde (tilde host-type host user)
  ;; Expands a TILDE (~ or ~sandy type construction)
  ;; Takes as an arg a filename (not directory name!)
  ;; and returns a filename. HOST-TYPE is the type of remote host.
  ;; nil is the type of the local host.
  (`
   (if (, host-type) ; nil host-type is the local machine
       (let* ((host (downcase (, host)))
	      (host-type (, host-type))
	      (ignore-case (memq host-type
				 efs-case-insensitive-host-types))
	      (tilde (, tilde))
	      (user (, user))
	      (key (concat host "/" user "/" tilde))
	      (res (efs-get-hash-entry
		    key efs-expand-dir-hashtable ignore-case)))
	 (or res
	     ;; for real accounts on unix systems, use the get trick
	     (and (not (efs-anonymous-p user))
		  (memq host-type efs-unix-host-types)
		  (let ((line (nth 1 (efs-send-cmd
				      host user
				      (list 'get tilde "/dev/null")
				      (format "expanding %s" tilde)))))
		    (setq res
			  (and (string-match efs-expand-dir-msgs line)
			       (substring line 
					  (match-beginning 1)
					  (match-end 1))))
		    (if res
			(progn
			  (setq res (efs-internal-directory-file-name res))
			  (efs-put-hash-entry
			   key res efs-expand-dir-hashtable ignore-case)
			  res))))
	     (progn
	       (setq res
		     (if (string-equal tilde "~")
			 (car (efs-send-pwd
			       host-type host user))
		       (let* ((home-key (concat host "/" user "/~"))
			      (home (efs-get-hash-entry
				     home-key efs-expand-dir-hashtable
				     ignore-case))
			      pwd-result)
			 (if home
			     (setq home
				   (efs-fix-path
				    host-type
				    (efs-internal-file-name-as-directory
				     host-type home)))
			   (if (setq home
				     (car
				      (setq pwd-result
					    (efs-send-pwd
					     host-type
					     host user))))
			       (efs-put-hash-entry
				home-key
				(efs-internal-directory-file-name
				 (efs-fix-path host-type home 'reverse))
				efs-expand-dir-hashtable ignore-case)
			     (efs-error host user
					     (concat "PWD failed: "
						     (cdr pwd-result)))))
			 (unwind-protect
			     (and (efs-raw-send-cd host user
					       (efs-fix-path
						host-type tilde) t)
				  (car
				   (efs-send-pwd
				    host-type host user)))
			   (efs-raw-send-cd host user home)))))
	       (if res
		   (progn
		     (setq res (efs-internal-directory-file-name
				(efs-fix-path host-type res 'reverse)))
		     (efs-put-hash-entry
		      key res efs-expand-dir-hashtable ignore-case)
		     res)))
	     (if (string-equal tilde "~")
		 (error "Cannot get home directory on %s" host)
	       (error "User %s is not known on %s" (substring tilde 1) host))))
     ;; local machine
     (efs-real-expand-file-name (, tilde)))))

(defmacro efs-de-dot-file-name (string)
  ;; Takes a string as arguments, and removes /../'s and /./'s.
  (`
   (let ((string (, string))
	 (start 0)
	 new make-dir)
     ;; to make the regexp's simpler, canonicalize to directory name.
     (if (setq make-dir (string-match "/\\.\\.?$" string))
	 (setq string (concat string "/")))
     (while (string-match "/\\./" string start)
       (setq new (concat new
			 (substring string
				    start (match-beginning 0)))
	     start (1- (match-end 0))))
     
     (if new (setq string (concat new (substring string start))))
     
     (while (string-match "/[^/]+/\\.\\./" string)
       ;; Is there a way to avoid all this concating and copying?
       (setq string (concat (substring string 0 (1+ (match-beginning 0)))
			    (substring string (match-end 0)))))
     
     ;; Do /../ and //../ special cases. They should expand to
     ;; / and //, respectively.
     (if (string-match "^\\(/+\\)\\.\\./" string)
	 (setq string (concat (substring string 0 (match-end 1))
			      (substring string (match-end 0)))))
     
     (if (and make-dir
	      (not (string-match "^/+$" string)))
	 (substring string 0 -1)
       string))))

(defun efs-substitute-in-file-name (string)
  "Documented as original."
  ;; Because of the complicated interaction between short-circuiting
  ;; and environment variable substitution, this can't call the macro
  ;; efs-short-circuit-file-name.
  (efs-save-match-data
    (let ((start 0)
	  var new root backskip regexp lbackskip
	  lregexp parsed fudge-host-type rstart error)
     
      (if efs-local-apollo-unix
	  (setq lregexp ".//+"
		lbackskip 2)
	(setq lregexp "//+"
	      lbackskip 1))
      
      ;; Subst. existing env variables
      (while (string-match "\\$" string start)
	(setq new (concat new (substring string start (match-beginning 0)))
	      start (match-end 0))
	(cond ((eq (string-match "\\$" string start) start)
	       (setq start (1+ start)
		     new (concat new "$$")))
	      ((eq (string-match "{" string start) start)
	       (if (and (string-match "}" string start)
			(setq var (getenv
				   (substring string (1+ start)
					      (1- (match-end 0))))))
		   (setq start (match-end 0)
			 new (concat new var))
		 (setq new (concat new "$"))))
	      ((eq (string-match "[a-zA-Z0-9]+" string start) start)
	       (if (setq var (getenv
			      (substring string start (match-end 0))))
		   (setq start (match-end 0)
			 new (concat new var))
		 (setq new (concat new "$"))))
	      ((setq new (concat new "$")))))
      (if new (setq string (concat new (substring string start))
		    start 0))

      ;; Short circuit /user@mach: roots. It is important to do this
      ;; now to avoid unnecessary ftp connections.
      
      (while (string-match efs-path-root-short-circuit-regexp
			   string start)
	(setq start (1+ (match-beginning 0))))
      (or (zerop start) (setq string (substring string start)
			      start 0))

      ;; Look for invalid environment variables in the root. If one is found,
      ;; we set the host-type to 'unix. Since we can't login in to determine
      ;; it. There is a good chance that we will bomb later with an error,
      ;; but the day may yet be saved if the root is short-circuited off.

      (if (string-match efs-path-root-regexp string)
	  (progn
	    (setq root (substring string 0 (match-end 0))
		  start (match-end 0))
	    (if (string-match "[^$]\\(\\$\\$\\)*\\$[^$]" root)
		(progn
		  (setq rstart (1- (match-end 0))
			fudge-host-type t)
		  (cond
		   ((eq (elt root rstart) ?{)
		    (setq
		     error
		     (if (string-match "}" root rstart)
			 (concat
			  "Subsituting non-existent environment variable "
			  (substring root (1+ rstart) (match-beginning 0)))
		       "Missing \"}\" in environment-variable substitution")))
		   ((eq (string-match "[A-Za-z0-9]+" root rstart) rstart)
		    (setq
		     error
		     (concat
		      "Subsituting non-existent environment variable "
		      (substring root rstart (match-beginning 0)))))
		   (t
		    (setq
		     error
		     "Bad format environment-variable substitution")))))
	    (setq root (efs-unquote-dollars root)
		  parsed (efs-ftp-path root))

	    (if (and (not fudge-host-type)
		     ;; This may trigger an FTP connection
		     (memq (efs-host-type (car parsed) (nth 1 parsed))
			   '(apollo-unix dumb-apollo-unix)))
		(setq regexp ".//+"
		      backskip 2)
	      (setq regexp "//+"
		    backskip 1)))
	;; no root, we're local
	(setq regexp lregexp
	      backskip lbackskip))
     
      ;; Now short-circuit in an apollo and efs sensitive way.
      
      (while (cond ((string-match regexp string start)
		    (setq start (- (match-end 0) backskip)))
		   ((string-match  "/~" string start)
		    (setq start (1- (match-end 0)))))
	
	(and root
	     (null efs-short-circuit-to-remote-root)
	     (setq root nil
		   regexp lregexp
		   backskip lbackskip)))
      
      ;; If we still have a bad root, barf.
      (if (and root error) (error error))

      ;; look for non-existent evironment variables in the path
      
      (if (string-match
	   "\\([^$]\\|^\\)\\(\\$\\$\\)*\\$\\([^$]\\|$\\)" string start)
	  (progn
	    (setq start (match-beginning 3))
	    (cond
	     ((eq (length string) start)
	      (error "Empty string is an invalid environment variable"))
	     ((eq (elt string start) ?{)
	      (if (string-match "}" string start)
		  (error
		   "Subsituting non-existent environment variable %s"
		   (substring string (1+ start) (match-end 0)))
		(error
		 "Missing \"}\" in environment-variable substitution")))
	     ((eq (string-match "[A-Za-z0-9]+" string start) start)
	      (error
	       "Subsituting non-existent environment variable %s"
	       (substring string start (match-end 0))))
	     (t
	      (error
	       "Bad format environment-variable substitution")))))

      (if root
	  (concat root
		  (efs-unquote-dollars
		   (if (zerop start)
		       string
		     (substring string start))))
	(efs-unquote-dollars
	 (if (zerop start)
	     string
	   (substring string start)))))))
	      
(defun efs-expand-file-name (name &optional default)
  "Documented as original."
  (let (s-c-res path host user host-type)
    (efs-save-match-data
      (or (file-name-absolute-p name)
	  (setq name (concat
		      (file-name-as-directory
		       (or default default-directory))
		      name)))
      (setq s-c-res (efs-short-circuit-file-name name)
	    path (car s-c-res)
	    host-type (nth 1 s-c-res)
	    host (nth 2 s-c-res)
	    user (nth 3 s-c-res))
      (cond ((string-match "^~[^/]*" path)
	     (let ((start (match-end 0)))
	       (setq path (concat
			   (efs-expand-tilde
			    (substring path 0 start)
			    host-type host user)
			   (substring path start)))))
	    ((and host-type (not (file-name-absolute-p path)))
	     ;; We expand the empty string to a directory.
	     ;; This can be more efficient for filename
	     ;; completion. It's also consistent with non-unix.
	     (let ((tilde (efs-expand-tilde
			   "~" host-type host user)))
	       (if (string-equal tilde "/")
		   (setq path (concat "/" path))
		 (setq path (concat tilde "/" path))))))
      
      (setq  path (efs-de-dot-file-name path))
      (if host-type
	  (format efs-path-format-string user host path)
	path))))

;;;; ------------------------------------------------------------
;;;; Other functions for manipulating file names.
;;;; ------------------------------------------------------------

(defun efs-internal-file-name-extension (filename)
  ;; Returns the extension for file name FN.
  (save-match-data
    (let ((file (file-name-sans-versions (file-name-nondirectory filename))))
      (if (string-match "\\.[^.]*\\'" file)
	  (substring file (match-beginning 0))
	""))))

(defun efs-file-name-as-directory (name)
  ;; version of file-name-as-directory for remote files.
  ;; Usually just appends a / if there isn't one already.
  ;; For some systems, it may also remove .DIR like extensions.
  (let* ((parsed (efs-ftp-path name))
	 (file (nth 2 parsed)))
    (if (string-equal file "")
	name
      (efs-internal-file-name-as-directory
       (efs-host-type (car parsed) (nth 1 parsed)) name))))

(efs-defun efs-internal-file-name-as-directory nil (name)
  ;; By default, simply adds a trailing /, if there isn't one.
  ;; Note that for expanded filenames, it pays to call this rather
  ;; than efs-file-name-as-directory.
  (let (file-name-handler-alist)
    (file-name-as-directory name)))
 	
(defun efs-file-name-directory (name)
  ;; file-name-directory for remote files. Takes care not to
  ;; turn /user@host: into /.
  (let ((path (nth 2 (efs-ftp-path name)))
	file-name-handler-alist)
    (if (or (string-equal path "")
	    (and (= (string-to-char path) ?~)
		 (not
		  (efs-save-match-data
		    (string-match "/" path 1)))))
	name
      (if (efs-save-match-data
	    (not (string-match "/" path)))
	  (efs-replace-path-component name "")
	(file-name-directory name)))))

(defun efs-file-name-nondirectory (name)
  ;; Computes file-name-nondirectory for remote files.
  ;; For expanded filenames, can just call efs-internal-file-name-nondirectory.
  (let ((file (nth 2 (efs-ftp-path name))))
    (if (or (string-equal file "")
	    (and (= (string-to-char file) ?~)
		 (not
		  (efs-save-match-data
		    (string-match "/" file 1)))))
	""
      (if (efs-save-match-data
	    (not (string-match "/" file)))
	  file
	(efs-internal-file-name-nondirectory name)))))

(defun efs-internal-file-name-nondirectory (name)
  ;; Version of file-name-nondirectory, without the efs-file-handler-function.
  ;; Useful to call this, if we have already decomposed the filename.
  (let (file-name-handler-alist)
    (file-name-nondirectory name)))

(defun efs-directory-file-name (dir)
  ;; Computes directory-file-name for remote files.
  ;; Needs to be careful not to turn /foo@bar:/ into /foo@bar:
  (let ((parsed (efs-ftp-path dir)))
    (if (string-equal "/" (nth 2 parsed))
	dir
      (efs-internal-directory-file-name dir))))

(defun efs-internal-directory-file-name (dir)
  ;; Call this if you want to apply directory-file-name to the remote
  ;; part of a efs-style path. Don't call for non-efs-style paths,
  ;; as this short-circuits the file-name-handler-alist completely.
  (let (file-name-handler-alist)
    (directory-file-name dir)))

(efs-defun efs-remote-directory-file-name nil (dir)
  "Returns the file name on the remote system of directory DIR.
If the remote system is not unix, this may not be the same as the file name
of the directory in efs's internal cache."
  (directory-file-name dir))

(defun efs-file-name-sans-versions (filename &optional keep-backup-versions)
  ;; Version of file-name-sans-versions for remote files.
  (or (file-name-absolute-p filename)
      (setq filename (expand-file-name filename)))
  (let ((parsed (efs-ftp-path filename)))
    (efs-internal-file-name-sans-versions
     (efs-host-type (car parsed) (nth 1 parsed))
     filename keep-backup-versions)))

(efs-defun efs-internal-file-name-sans-versions nil
  (filename &optional keep-backup-versions)
  (let (file-name-handler-alist)
    (file-name-sans-versions filename keep-backup-versions)))

(defun efs-diff-latest-backup-file (fn)
  ;; Version of diff latest backup file for remote files.
  ;; Accomodates non-unix.
  ;; Returns the latest backup for fn, according to the numbering
  ;; of the backups. Does not check file-newer-than-file-p.
  (let ((parsed (efs-ftp-path fn)))
    (efs-internal-diff-latest-backup-file
     (efs-host-type (car parsed) (nth 1 parsed)) fn)))

(efs-defun efs-internal-diff-latest-backup-file nil (fn)
  ;; Default behaviour is the behaviour in diff.el
  (let (file-name-handler-alist)
    (diff-latest-backup-file fn)))

(defun efs-unhandled-file-name-directory (filename)
  ;; Calculate a default unhandled directory for an efs buffer.
  ;; This is used to compute directories in which to execute
  ;; processes. This is relevant to V19 only. Doesn't do any harm for
  ;; older versions though. It would be nice if this wasn't such a
  ;; kludge.
  (file-name-directory efs-tmp-name-template))

(defun efs-file-truename (filename)
  ;; Calculates a remote file's truename, if this isn't inhibited.
  (let ((filename (expand-file-name filename)))
    (if (and efs-compute-remote-buffer-file-truename
	     (memq (efs-host-type (car (efs-ftp-path filename)))
		   efs-unix-host-types))
	(efs-internal-file-truename filename)
      filename)))

(defun efs-internal-file-truename (filename)
  ;; Internal function so that we don't keep checking
  ;; efs-compute-remote-buffer-file-truename, etc, as we recurse.
  (let ((dir (efs-file-name-directory filename))
	target dirfile)
    ;; Get the truename of the directory.
    (setq dirfile (efs-directory-file-name dir))
    ;; If these are equal, we have the (or a) root directory.
    (or (string= dir dirfile)
	(setq dir (efs-file-name-as-directory
		   (efs-internal-file-truename dirfile))))
    (if (equal ".." (efs-file-name-nondirectory filename))
	(efs-directory-file-name (efs-file-name-directory
				  (efs-directory-file-name dir)))
      (if (equal "." (efs-file-name-nondirectory filename))
	  (efs-directory-file-name dir)
	;; Put it back on the file name.
	(setq filename (concat dir (efs-file-name-nondirectory filename)))
	;; Is the file name the name of a link?
	(setq target (efs-file-symlink-p filename))
	(if target
	    ;; Yes => chase that link, then start all over
	    ;; since the link may point to a directory name that uses links.
	    ;; We can't safely use expand-file-name here
	    ;; since target might look like foo/../bar where foo
	    ;; is itself a link.  Instead, we handle . and .. above.
	    (if (file-name-absolute-p target)
		(efs-internal-file-truename target)
	      (efs-internal-file-truename (concat dir target)))
	  ;; No, we are done!
	  filename)))))


;;;; ----------------------------------------------------------------
;;;; I/O functions
;;;; ----------------------------------------------------------------

(efs-define-fun efs-set-buffer-file-name (filename)
  ;; Sets the buffer local variables for filename appropriately.
  ;; A special function because Lucid and FSF do this differently.
  ;; This default behaviour is the lowest common denominator.
  (setq buffer-file-name filename))

(defun efs-write-region (start end filename &optional append visit &rest args)
  ;; write-region for remote files.
  ;; This version accepts the V19 interpretation for the arg VISIT.
  ;; However, making use of this within V18 may cause errors to crop up.
  ;; ARGS should catch the MULE coding-system argument.
  (if (stringp visit) (setq visit (expand-file-name visit)))
  (setq filename (expand-file-name filename))
  (let ((parsed (efs-ftp-path filename))
	;; Make sure that the after-write-region-hook isn't called inside
	;; the file-handler-alist
	(after-write-region-hook nil))
    (if parsed
	(let* ((host (car parsed))
	       (user (nth 1 parsed))
	       (host-type (efs-host-type host user))
	       (temp (car (efs-make-tmp-name nil host)))
	       (type (efs-xfer-type nil nil host-type filename))
	       (abbr (and (or (stringp visit) (eq t visit) (null visit))
			  (efs-relativize-filename
			   (if (stringp visit) visit filename))))
	       (buffer (current-buffer))
	       (b-file-name buffer-file-name)
	       (mod-p (buffer-modified-p)))
	  (unwind-protect
	      (progn
		(condition-case err
		    (progn
		      (unwind-protect
			  (let ((executing-macro t))
			    ;; let-bind executing-macro to inhibit messaging.
			    ;; Setting VISIT to 'quiet is more elegant.
			    ;; But in Emacs 18, doing it this way allows
			    ;; us to modify the visited file modtime, so
			    ;; that undo's show the buffer modified.
			    (apply 'write-region start end
				   temp nil visit args))
			;; buffer-modified-p is now correctly set
			(setq buffer-file-name b-file-name)
			;; File modtime is bogus, so clear.
			(clear-visited-file-modtime))
		      (efs-copy-file-internal
		       temp nil filename parsed (if append 'append t)
		       nil (and abbr (format "Writing %s" abbr))
		       ;; cont
		       (efs-cont (result line cont-lines) (filename buffer
								    visit)
			 (if result
			     (signal 'ftp-error
				     (list "Opening output file"
					   (format "FTP Error: \"%s\"" line)
					   filename)))
			 ;; The new file entry will be added by
			 ;; efs-copy-file-internal.
			 (cond
			  ((eq visit t)
			   ;; This will run asynch.
			   (efs-save-buffer-excursion
			     (set-buffer buffer)
			     (efs-set-buffer-file-name filename)
			     (efs-set-visited-file-modtime)))
			  ((stringp visit)
			   (efs-save-buffer-excursion
			     (set-buffer buffer)
			     (efs-set-buffer-file-name visit)
			     (set-visited-file-modtime)))))
		       nil type))
		  (error
		   ;; restore buffer-modified-p
		   (let (file-name-handler-alist)
		     (set-buffer-modified-p mod-p))
		   (signal (car err) (cdr err))))
		(if (or (eq visit t)
			(and (stringp visit)
			     (efs-ftp-path visit)))
		    (efs-set-buffer-mode)))
	    (efs-del-tmp-name temp))
	  (and abbr (efs-message "Wrote %s" abbr)))
      (if (and (stringp visit) (efs-ftp-path visit))
	  (progn
	    (apply 'write-region start end filename append visit args)
	    (efs-set-buffer-file-name visit)
	    (efs-set-visited-file-modtime)
	    (efs-set-buffer-mode))
	(error "efs-write-region called for a local file")))))

(defun efs-insert-file-contents (filename &optional visit &rest args)
  ;; Inserts file contents for remote files.
  ;; The additional ARGS covers V19 BEG and END. Should also handle the
  ;; CODING-SYSTEM arg for mule. Hope the two don't trip over each other.
  (barf-if-buffer-read-only)
  (unwind-protect
      (let* ((filename (expand-file-name filename))
	     (parsed (efs-ftp-path filename))
	     (host (car parsed))
	     (host-type (efs-host-type host))
	     (user (nth 1 parsed))
	     (path (nth 2 parsed))
	     (buffer (current-buffer)))
	
	(if (or (file-exists-p filename)
		(let* ((res (and
			     (not (efs-get-host-property host 'rnfr-failed))
			     (efs-send-cmd
			      host user (list 'quote 'rnfr path))))
		       (line (nth 1 res)))
		  ;; RNFR returns a 550 if the file doesn't exist.
		  (if (and line (>= (length line) 4)
			   (string-equal "550 " (substring line 0 4)))
		      nil
		    (if (car res) (efs-set-host-property host 'rnfr-failed t))
		    (efs-del-from-ls-cache filename t nil)
		    (efs-del-hash-entry
		     (efs-canonize-file-name (file-name-directory filename))
		     efs-files-hashtable)
		    (file-exists-p filename))))
	    
	    (let ((temp (concat
			 (car (efs-make-tmp-name nil host))
			 (efs-internal-file-name-extension filename)))
		  (type (efs-xfer-type host-type filename nil nil))
		  (abbr (efs-relativize-filename filename))
		  (i-f-c-size 0))
	      
	      (unwind-protect
		  (efs-copy-file-internal
		   filename parsed temp nil t nil
		   (format "Retrieving %s" abbr)
		   (efs-cont (result line cont-lines) (filename visit buffer
								host-type
								temp args)
		     (if result
			 (signal 'ftp-error
				 (list "Opening input file"
				       (format "FTP Error: \"%s\""
					       line)
				       filename))
		       (if (eq host-type 'coke)
			   (efs-coke-insert-beverage-contents buffer filename
							      line)
			 (efs-save-buffer-excursion
			   (set-buffer buffer)
			   (if (or (file-readable-p temp)
				   (sleep-for efs-retry-time)
				   ;; Wait for file to hopefully appear.
				   (file-readable-p temp))
			       
			       (setq i-f-c-size
				   (nth 1 (apply 'insert-file-contents
						 temp visit args)))
			     (signal 'ftp-error
				     (list
				      "Opening input file:"
				      (format
				       "FTP Error: %s not arrived or readable"
				       filename))))
			   ;; This is done asynch
			   (if visit
			       (let ((buffer-file-name filename))
				 (efs-set-visited-file-modtime)))))))
		   nil type)
		(efs-del-tmp-name temp))
	      ;; Return (FILENAME SIZE)
	      (list filename i-f-c-size))
	  (signal 'file-error (list "Opening input file" filename))))
    ;; Set buffer-file-name at the very last, so if anything bombs, we're
    ;; not visiting.
    (if visit
	(efs-set-buffer-file-name filename))))

(defun efs-revert-buffer (arg noconfirm)
  "Revert this buffer from a remote file using ftp."
  (let ((opoint (point)))
    (cond ((null buffer-file-name)
	   (error "Buffer does not seem to be associated with any file"))
	  ((or noconfirm
	       (yes-or-no-p (format "Revert buffer from file %s? "
				    buffer-file-name)))
	   (let ((buffer-read-only nil))
	     ;; Set buffer-file-name to nil
	     ;; so that we don't try to lock the file.
	     (let ((buffer-file-name nil))
	       (unlock-buffer)
	       (erase-buffer))
	     (insert-file-contents buffer-file-name t))
	   (goto-char (min opoint (point-max)))
	   (after-find-file nil)
	   t))))

(defun efs-recover-file (file)
  ;; Version of recover file for remote files, and remote autosave files too.
  (if (auto-save-file-name-p file) (error "%s is an auto-save file" file))
  (let* ((file-name (let ((buffer-file-name file)) (make-auto-save-file-name)))
	 (file-name-parsed (efs-ftp-path file-name))
	 (file-parsed (efs-ftp-path file))
	 (efs-ls-uncache t))
    (cond ((not (file-newer-than-file-p file-name file))
	   (error "Auto-save file %s not current" file-name))
	  ((save-window-excursion
	     (or (eq system-type 'vax-vms)
		 (progn
		   (with-output-to-temp-buffer "*Directory*"
		     (buffer-disable-undo standard-output)
		     (if file-parsed
			 (progn
			   (princ (format "On the host %s:\n"
					  (car file-parsed)))
			   (princ
			    (let ((default-directory exec-directory))
			      (efs-ls file (if (file-symlink-p file)
					       "-lL" "-l")
				      t t))))
		       (princ "On the local host:\n")
		       (let ((default-directory exec-directory))
			 (call-process "ls" nil standard-output nil
				       (if (file-symlink-p file) "-lL" "-l")
				       file)))
		     (princ "\nAUTO SAVE FILE on the ")
		     (if file-name-parsed
			 (progn
			   (princ (format "host %s:\n"
					  (car file-name-parsed)))
			   (princ
			    (efs-ls file-name
				    (if (file-symlink-p file-name) "-lL" "-l")
				    t t)))
		       (princ "local host:\n")
		       (let ((default-directory exec-directory))
			 (call-process "ls" nil standard-output nil
				       "-l" file-name)))
		     (princ "\nFile modification times are given in ")
		     (princ "the local time of each host.\n"))
		   (save-excursion
		     (set-buffer "*Directory*")
		     (goto-char (point-min))
		     (while (not (eobp))
		       (end-of-line)
		       (if (> (current-column) (window-width))
			   (progn
			     (skip-chars-backward " \t")
			     (skip-chars-backward "^ \t\n")
			     (if (> (current-column) 12)
				 (progn
				   (delete-horizontal-space)
				   (insert "\n           ")))))
		       (forward-line 1))
		     (set-buffer-modified-p nil)
		     (goto-char (point-min)))))
	     (yes-or-no-p (format "Recover using this auto save file? ")))
	   (switch-to-buffer (find-file-noselect file t))
	   (let ((buffer-read-only nil))
	     (erase-buffer)
	     (insert-file-contents file-name nil))
	   (after-find-file nil))
	  (t (error "Recover-file cancelled."))))
  ;; This is no longer done in V19. However, I like the caution for
  ;; remote files, where file-newer-than-file-p may lie.
  (setq buffer-auto-save-file-name nil)
  (message "Auto-save off in this buffer till you do M-x auto-save-mode."))

;;;; ------------------------------------------------------------------
;;;; Attributes of files.
;;;; ------------------------------------------------------------------

(defun efs-file-symlink-p (file)
  ;; Version of file-symlink-p for remote files.
  ;; Call efs-expand-file-name rather than the normal
  ;; expand-file-name to stop loops when using a package that
  ;; redefines both file-symlink-p and expand-file-name.
  ;; Do not use efs-get-file-entry, because a child-lookup won't do.
  (let* ((file (efs-expand-file-name file))
	 (ignore-case (memq (efs-host-type (car (efs-ftp-path file)))
			    efs-case-insensitive-host-types))
	 (file-type (car (efs-get-hash-entry
			  (efs-get-file-part file)
			  (efs-get-files (file-name-directory file))
			  ignore-case))))
    (and (stringp file-type)
	 (if (file-name-absolute-p file-type)
	     (efs-replace-path-component file file-type)
	   file-type))))

(defun efs-file-exists-p (path)
  ;; file-exists-p for remote file. Uses the cache if possible.
  (let* ((path (expand-file-name path))
	 (parsed (efs-ftp-path path)))
    (efs-internal-file-exists-p (efs-host-type (car parsed) (nth 1 parsed))
				path)))

(efs-defun efs-internal-file-exists-p nil (path)
  (and (efs-get-file-entry path) t))

(defun efs-file-directory-p (file)
  (let* ((file (expand-file-name file))
	 (parsed (efs-ftp-path file)))
    (efs-internal-file-directory-p (efs-host-type (car parsed) (nth 1 parsed))
				   file)))

(efs-defun efs-internal-file-directory-p nil (path)
  ;; Version of file-directory-p for remote files.
  (let ((parsed (efs-ftp-path path)))
    (or (string-equal (nth 2 parsed) "/")  ; root is always a directory
	(let ((file-ent (car (efs-get-file-entry
			      (efs-internal-file-name-as-directory
			       (efs-host-type (car parsed) (nth 1 parsed))
			       path)))))
	  ;; We do a file-name-as-directory on path here because some
	  ;; machines (VMS) use a .DIR to indicate the filename associated
	  ;; with a directory. This needs to be canonicalized.
	  (if (stringp file-ent)
	      (efs-internal-file-directory-p
	       nil
	       (efs-chase-symlinks
		;; efs-internal-directory-file-name
		;; only loses for paths where the remote file
		;; is /. This has been eliminated.
		(efs-internal-directory-file-name path)))
	    file-ent)))))

(defun efs-file-attributes (file)
  ;; Returns file-file-attributes for a remote file.
  ;; For the file modtime does not return efs's cached value, as that
  ;; corresponds to buffer-file-modtime (i.e. the modtime of the file
  ;; the last time the buffer was vsisted or saved). Caching modtimes
  ;; does not make much sense, as they are usually used to determine
  ;; if a cache is stale. The modtime if a remote file can be obtained with
  ;; efs-get-file-mdtm. This is _not_ returned for the 5th entry here,
  ;; because it requires an FTP transaction, and a priori we don't know
  ;; if the caller actually cares about this info. Having file-attributes
  ;; return such a long list of info is not well suited to remote files,
  ;; as some of this info may be costly to obtain.
  (let* ((file (expand-file-name file))
	 (ent (efs-get-file-entry file)))
    (if ent
	(let* ((parsed (efs-ftp-path file))
	       (host (nth 0 parsed))
	       (user (nth 1 parsed))
	       (path (nth 2 parsed))
	       (type (car ent))
	       (size (or (nth 1 ent) -1))
	       (owner (nth 2 ent))
	       (modes (nth 3 ent))
	       ;; Hack to give remote files a "unique" "inode number".
	       ;; It's actually the sum of the characters in its name.
	       ;; It's not even really unique.
	       (inode (apply '+
			     (nconc (mapcar 'identity host)
				    (mapcar 'identity user)
				    (mapcar 'identity
					    (efs-internal-directory-file-name
					     path)))))
	       (nlinks (or (nth 4 ent) -1))) ; return -1 if we don't know
	  (list
	   (if (and (stringp type) (file-name-absolute-p type))
	       (efs-replace-path-component file type)
	     type)		        ;0 file type
	   nlinks	        	;1 link count
	   (if owner                    ;2 uid
	       ;; Not really a unique integer,
	       ;; just a half-hearted attempt
	       (apply '+ (mapcar 'identity owner))
	     -1)
	   -1	                  	;3 gid
	   '(0 0)	        	;4 atime
	   '(0 0)	        	;5 mtime
	   '(0 0)	         	;6 ctime
	   size 		        ;7 size
	   (or modes                    ;8 mode
	       (concat
		(cond ((stringp type) "l")
		      (type "d")
		      (t "-"))
		"?????????"))
	   nil    		        ;9 gid weird (Who knows if the gid
					;             would be changed?)
	   inode                        ;10 inode
	   -1	         	        ;11 device number [v19 only]
	   )))))

(defun efs-file-writable-p (file)
  ;; file-writable-p for remote files.
  ;; Does not attempt to open the file, but just looks at the cached file
  ;; modes.
  (let* ((file (expand-file-name file))
	 (ent (efs-get-file-entry file)))
    (if (and ent (or (not (stringp (car ent)))
		     (setq file (efs-chase-symlinks file)
			   ent (efs-get-file-entry file))))
	(let* ((owner (nth 2 ent))
	       (modes (nth 3 ent))
	       (parsed (efs-ftp-path file))
	       (host-type (efs-host-type (car parsed)))
	       (user (nth 1 parsed)))
	  (if (memq host-type efs-unix-host-types)
	      (setq host-type 'unix))
	  (efs-internal-file-writable-p host-type user owner modes))
      (let ((dir (file-name-directory file)))
	(and
	 (not (string-equal dir file))
	 (file-directory-p dir)
	 (file-writable-p dir))))))

(efs-defun efs-internal-file-writable-p nil (user owner modes)
  ;; By default, we'll just guess yes.
  t)

(efs-defun efs-internal-file-writable-p unix (user owner modes)
  (if (and modes
	   (not (string-equal user "root")))
      (null
       (null
	(if (string-equal user owner)
	    (memq ?w (list (aref modes 2) (aref modes 5)
			   (aref modes 8)))
	  (memq ?w (list (aref modes 5) (aref modes 8))))))
    t)) ; guess

(defun efs-file-readable-p (file)
  ;; Version of file-readable-p that works for remote files.
  ;; Works by checking efs's cache of the file modes.
  (let* ((file (expand-file-name file))
	 (ent (efs-get-file-entry file)))
    (and ent
	 (or (not (stringp (car ent)))
	     (setq ent (efs-get-file-entry (efs-chase-symlinks file))))
	 ;; file exists
	 (let* ((parsed (efs-ftp-path file))
		(owner (nth 2 ent))
		(modes (nth 3 ent))
		(host-type (efs-host-type (car parsed)))
		(user (nth 1 parsed)))
	   (if (memq host-type efs-unix-host-types)
	       (setq host-type 'unix))
	   (efs-internal-file-readable-p host-type user owner modes)))))

(efs-defun efs-internal-file-readable-p nil (user owner modes)
  ;; Guess t by default
  t)

(efs-defun efs-internal-file-readable-p unix (user owner modes)
  (if (and modes
	   (not (string-equal user "root")))
      (null
       (null
	(if (string-equal user owner)
	    (memq ?r (list (aref modes 1) (aref modes 4)
			   (aref modes 7)))
	  (memq ?r (list (aref modes 4) (aref modes 7))))))
    t)) ; guess

(defun efs-file-executable-p (file)
  ;; Version of file-executable-p for remote files.
  (let ((ent (efs-get-file-entry file)))
    (and ent
	 (or (not (stringp (car ent)))
	     (setq ent (efs-get-file-entry (efs-chase-symlinks file))))
	 ;; file exists
	 (let* ((parsed (efs-ftp-path file))
		(owner (nth 2 ent))
		(modes (nth 3 ent))
		(host-type (efs-host-type (car parsed)))
		(user (nth 1 parsed)))
	   (if (memq host-type efs-unix-host-types)
	       (setq host-type 'unix))
	   (efs-internal-file-executable-p host-type user owner modes)))))

(efs-defun efs-internal-file-executable-p nil (user owner modes)
  ;; Guess t by default
  t)

(efs-defun efs-internal-file-executable-p unix (user owner modes)
  (if (and modes
	   (not (string-equal user "root")))
      (null
       (null
	(if (string-equal user owner)
	    (memq ?x (list (aref modes 3) (aref modes 6)
			   (aref modes 9)))
	  (memq ?x (list (aref modes 6) (aref modes 9))))))
    t)) ; guess

(defun efs-file-accessible-directory-p (dir)
  ;; Version of file-accessible-directory-p for remote directories.
  (let ((file (directory-file-name dir)))
    (and (efs-file-directory-p file) (efs-file-executable-p file))))

;;;; --------------------------------------------------------------
;;;; Listing directories.
;;;; --------------------------------------------------------------

(defun efs-shell-regexp-to-regexp (regexp)
  ;; Converts a shell regexp to an emacs regexp.
  ;; Probably full of bugs. Tries to follow csh globbing.
  (let ((curly 0)
	backslash)
    (concat "^"
	    (mapconcat
	     (function
	      (lambda (char)
		(cond
		 (backslash
		  (setq backslash nil)
		  (regexp-quote (char-to-string char)))
		 ((and (> curly 0) (eq char ?,))
		  "\\|")
		 ((memq char '(?[ ?]))
		  (char-to-string char))
		 ((eq char ??)
		  ".")
		 ((eq char ?\\)
		  (setq backslash t)
		  "")
		 ((eq char ?*)
		  ".*")
		 ((eq char ?{)
		  (setq curly (1+ curly))
		  "\\(")
		 ((and (eq char ?}) (> curly 0))
		  (setq curly (1- curly))
		  "\\)")
		 (t (regexp-quote (char-to-string char))))))
	     regexp nil)
	    "$")))


;;; Getting directory listings.

(defun efs-directory-files (directory &optional full match nosort)
  ;; Returns directory-files for remote directories.
  ;; NOSORT is a V19 arg.
  (let* ((directory (expand-file-name directory))
	 (parsed (efs-ftp-path directory))
	 (directory (efs-internal-file-name-as-directory
		     (efs-host-type (car parsed) (nth 1 parsed)) directory))
	 files)
    (efs-barf-if-not-directory directory)
    (setq files (efs-hash-table-keys (efs-get-files directory) nosort))
    (cond
     ((null (or full match))
      files)
     (match ; this is slow case
      (let (res f)
	(efs-save-match-data
	  (while files
	    (setq f (if full (concat directory (car files)) (car files))
		  files (cdr files))
	    (if (string-match match f)
		(setq res (nconc res (list f))))))
	res))
     (full
      (mapcar (function
	       (lambda (fn)
		 (concat directory fn)))
	      files)))))

(defun efs-list-directory (dirname &optional verbose)
  ;; Version of list-directory for remote directories.
  ;; If verbose is nil, it gets its information from efs's
  ;; internal cache.
  (let* ((dirname (expand-file-name (or dirname default-directory)))
	 header)
    (if (file-directory-p dirname)
	(setq dirname (file-name-as-directory dirname)))
    (setq header dirname)
    (with-output-to-temp-buffer "*Directory*"
      (buffer-disable-undo standard-output)
      (princ "Directory ")
      (princ header)
      (terpri)
      (princ
       (efs-ls dirname (if verbose
			   list-directory-verbose-switches
			 list-directory-brief-switches)
	       t)))))

;;;; -------------------------------------------------------------------
;;;; Manipulating buffers.
;;;; -------------------------------------------------------------------

(defun efs-get-file-buffer (file)
  ;; Version of get-file-buffer for remote files. Needs to fuss over things
  ;; like OS's which are case-insens. for file names.
  (let ((file (efs-canonize-file-name (expand-file-name file)))
	(buff-list (buffer-list))
	buff-name)
    (catch 'match
      (while buff-list
	(and (setq buff-name (buffer-file-name (car buff-list)))
	     (= (length buff-name) (length file)) ; efficiency hack
	     (string-equal (efs-canonize-file-name buff-name) file)
	     (throw 'match (car buff-list)))
	(setq buff-list (cdr buff-list))))))

(defun efs-create-file-buffer (filename)
  ;; Version of create-file-buffer for remote file names.
  (let* ((parsed (efs-ftp-path (expand-file-name filename)))
	 (file (nth 2 parsed))
	 (host (car parsed))
	 (host-type (efs-host-type host))
	 (buff (cond
		((null efs-fancy-buffer-names)
		 (if (string-equal file "/")
		     "/"
		   (efs-internal-file-name-nondirectory
		    (efs-internal-directory-file-name file))))
		((stringp efs-fancy-buffer-names)
		 (format efs-fancy-buffer-names
			 (if (string-equal file "/")
			     "/"
			   (efs-internal-file-name-nondirectory
			    (efs-internal-directory-file-name file)))
			 (substring host 0 (string-match "\\." host 1))))
		(t ; efs-fancy-buffer-names had better be a function
		 (funcall efs-fancy-buffer-names host
			  (nth 1 parsed) file)))))
    (if (memq host-type efs-case-insensitive-host-types)
	(cond ((eq efs-buffer-name-case 'down)
	       (setq buff (downcase buff)))
	      ((eq efs-buffer-name-case 'up)
	       (setq buff (upcase buff)))))
    (get-buffer-create (generate-new-buffer-name buff))))
    
(defun efs-set-buffer-mode ()
  "Set correct modes for the current buffer if it is visiting a remote file."
  (if (and (stringp buffer-file-name)
	   (efs-ftp-path buffer-file-name))
      (progn
	(auto-save-mode efs-auto-save)
	(set (make-local-variable 'revert-buffer-function)
	     'efs-revert-buffer)
	(set (make-local-variable 'default-directory-function)
	     'efs-default-dir-function))))

;;;; ---------------------------------------------------------
;;;; Functions for doing backups.
;;;; ---------------------------------------------------------

(defun efs-backup-buffer ()
  ;; Version of backup-buffer for buffers visiting remote files.
  (if efs-make-backup-files
      (let* ((parsed (efs-ftp-path buffer-file-name))
	     (host (car parsed))
	     (host-type (efs-host-type (car parsed))))
	(if (or (not (listp efs-make-backup-files))
		(memq host-type efs-make-backup-files))
	    (efs-internal-backup-buffer
	     host host-type (nth 1 parsed) (nth 2 parsed))))))

(defun efs-internal-backup-buffer (host host-type user remote-path)
  ;; This is almost a copy of the function in files.el, modified
  ;; to check to see if the backup file exists, before deleting it.
  ;; It also supports efs-backup-by-copying, and tries to do the
  ;; right thing about backup-by-copying-when-mismatch. Only called
  ;; for remote files.
  ;; Set the umask now, so that `setmodes' knows about it.
  (efs-set-umask host user)
  (let ((ent (efs-get-file-entry (expand-file-name buffer-file-name)))
	;; Never do version-control if the remote operating system is doing it.
	(version-control (if (memq host-type efs-version-host-types)
			     'never
			   version-control))
	modstring)
    (and make-backup-files
	 (not buffer-backed-up)
	 ent ; i.e. file-exists-p
	 (not (eq t (car ent)))
	 (or (null (setq modstring (nth 3 ent)))
	     (not (memq host-type efs-unix-host-types))
	     (memq (aref modstring 0) '(?- ?l)))
	 (or (< (length remote-path) 5)
	     (not (string-equal "/tmp/" (substring remote-path 0 5))))
	 (condition-case ()
	     (let* ((backup-info (find-backup-file-name buffer-file-name))
		    (backupname (car backup-info))
		    (targets (cdr backup-info))
		    (links (nth 4 ent))
		    setmodes)
	       (condition-case ()
		   (if (or file-precious-flag
			   (stringp (car ent)) ; symlinkp
			   efs-backup-by-copying
			   (and backup-by-copying-when-linked
				links (> links 1))
			   (and backup-by-copying-when-mismatch
				(not
				 (if (memq
				      host-type
				      efs-case-insensitive-host-types)
				     (string-equal
				      (downcase user) (downcase (nth 2 ent)))
				   (string-equal user (nth 2 ent))))))
		       (copy-file buffer-file-name backupname t t)
		     (condition-case ()
			 (if (file-exists-p backupname)
			     (delete-file backupname))
		       (file-error nil))
		     (rename-file buffer-file-name backupname t)
		     (setq setmodes (file-modes backupname)))
		 (file-error
		  ;; If trouble writing the backup, write it in ~.
		  (setq backupname (expand-file-name "~/%backup%~"))
		  (message
		   "Cannot write backup file; backing up in ~/%%backup%%~")
		  (sleep-for 1)
		  (copy-file buffer-file-name backupname t t)))
	       (setq buffer-backed-up t)
	       ;; Starting with 19.26, trim-versions-without-asking
	       ;; has been renamed to delete-old-verions.
	       (if (and targets
			(or (if (boundp 'trim-versions-without-asking)
				trim-versions-without-asking
			      (and
			       (boundp 'delete-old-versions)
			       delete-old-versions))
			    (y-or-n-p (format
				       "Delete excess backup versions of %s? "
				       buffer-file-name))))
		   (while targets
		     (condition-case ()
			 (delete-file (car targets))
		       (file-error nil))
		     (setq targets (cdr targets))))
	       ;; If the file was already written with the right modes,
	       ;; don't return set-modes.
	       (and setmodes
		    (null
		     (let ((buff (get-buffer
				  (efs-ftp-process-buffer host user))))
		       (and buff
			    (save-excursion
			      (set-buffer buff)
			      (and (integerp efs-process-umask)
				   (= (efs-modes-from-umask efs-process-umask)
				      setmodes))))))
		    setmodes))
	   (file-error nil)))))

;;;; ------------------------------------------------------------
;;;; Redefinition for Emacs file mode support
;;;; ------------------------------------------------------------

(defmacro efs-build-mode-string-element (int suid-p sticky-p)
  ;; INT is between 0 and 7.
  ;; If SUID-P is non-nil, we are building the 3-char string for either
  ;; the owner or group, and the s[ug]id bit is set.
  ;; If STICKY-P is non-nil, we are building the string for other perms,
  ;; and the sticky bit is set.
  ;; It doesn't make sense for both SUID-P and STICKY-P be non-nil!
  (` (let* ((int (, int))
	    (suid-p (, suid-p))
	    (sticky-p (, sticky-p))
	    (read-bit (if (memq int '(4 5 6 7)) "r" "-"))
	    (write-bit (if (memq int '(2 3 6 7)) "w" "-"))
	    (x-bit (if (memq int '(1 3 5 7))
		       (cond (suid-p "s") (sticky-p "t") ("x"))
		     (cond (suid-p "S") (sticky-p "T") ("-")))))
       (concat read-bit write-bit x-bit))))
       
(defun efs-mode-string (int)
  ;; Takes an octal integer between 0 and 7777, and returns the 9 character
  ;; mode string.
  (let* ((other-int (% int 10))
	 (int (/ int 10))
	 (group-int (% int 10))
	 (int (/ int 10))
	 (owner-int (% int 10))
	 (int (/ int 10))
	 (suid (memq int '(4 5 6 7)))
	 (sgid (memq int '(2 3 6 7)))
	 (sticky (memq int '(1 3 5 7))))
    (concat (efs-build-mode-string-element owner-int suid nil)
	    (efs-build-mode-string-element group-int sgid nil)
	    (efs-build-mode-string-element other-int nil sticky))))
  
(defun efs-set-file-modes (file mode)
  ;; set-file-modes for remote files.
  ;; For remote files, if mode is nil, does nothing.
  ;; This is because efs-file-modes returns nil if the modes
  ;; of a remote file couldn't be determined, even if the file exists.
  (and mode
       (let* ((file (expand-file-name file))
	      (parsed (efs-ftp-path file))
	      (host (car parsed))
	      (user (nth 1 parsed))
	      (r-file (nth 2 parsed))
	      ;; convert to octal, and keep only 12 lowest order bits.
	      (omode (format "%o" (- mode (lsh (lsh mode -12) 12)))))
	 (if (or (efs-get-host-property host 'chmod-failed)
		 (null (memq (efs-host-type host user) efs-unix-host-types)))
	     (message "Unable to set file modes for %s to %s." file omode)
	   (efs-send-cmd
	    host user
	    (list 'quote 'site 'chmod omode r-file)
	    nil nil
	    (efs-cont (result line cont-lines) (host file r-file omode)
	      (if result
		  (progn
		    (efs-set-host-property host 'chmod-failed t)
		    (message "CHMOD %s failed for %s on %s." omode r-file host)
		    (if efs-ding-on-chmod-failure
			(progn (ding) (sit-for 1))))
		(let ((ent (efs-get-file-entry file)))
		  (if ent
		      (let* ((type
			      (cond
			       ((null (car ent)) "-")
			       ((eq (car ent) t) "d")
			       ((stringp (car ent)) "s")
			       (t
				(error
				 "Weird error in efs-set-file-modes"))))
			     (mode-string (concat
					   type
					   (efs-mode-string
					    (string-to-int omode))))
			     (tail (nthcdr 3 ent)))
			(if (consp tail)
			    (setcar tail mode-string)
			  (efs-add-file-entry nil file (car ent) (nth 1 ent)
					      (nth 2 ent) mode-string)))))))
	    0)))) ; It should be safe to do this NOWAIT = 0
  ;; set-file-modes returns nil
  nil)

(defmacro efs-parse-mode-element (modes)
  ;; Parses MODES, a string of three chars, and returns an integer
  ;; between 0 and 7 according to how unix file modes are represented
  ;; for chmod.
  (` (if (= (length (, modes)) 3)
	 (let ((list (mapcar
		      (function (lambda (char)
				  (if (memq char '( ?- ?S ?T)) 0 1)))
		      (, modes))))
	   ;; Convert to octal
	   (+ (* (car list) 4) (* (nth 1 list) 2) (nth 2 list)))
       (error "Can't parse modes %s" (, modes)))))

(defun efs-parse-mode-string (string)
  ;; Parse a 9-character mode string, and return what it represents
  ;; as a decimal integer.
  (let ((owner (efs-parse-mode-element (substring string 0 3)))
	(group (efs-parse-mode-element (substring string 3 6)))
	(other (efs-parse-mode-element (substring string 6 9)))
	(owner-x (elt string 2))
	(group-x (elt string 5))
	(other-x (elt string 8)))
    (+ (* (+ (if (memq owner-x '(?s ?S)) 4 0)
	     (if (memq group-x '(?s ?S)) 2 0)
	     (if (memq other-x '(?t ?T)) 1 0))
	  512)
       (* owner 64)
       (* group 8)
       other)))

(defun efs-file-modes (file)
  ;; Version of file-modes for remote files.
  ;; Returns nil if the file modes can't be determined, either because
  ;; the file doesn't exist, or for any other reason.
  (let* ((file (expand-file-name file))
	 (parsed (efs-ftp-path file)))
    (and (memq (efs-host-type (car parsed)) efs-unix-host-types)
	 ;; Someday we should cache mode strings for non-unix, but they
	 ;; won't be in unix format. Also, CHMOD doesn't work for non-unix
	 ;; hosts, so returning this info to emacs is a waste.
	 (let* ((ent (efs-get-file-entry file))
		(modes (nth 3 ent)))
	   (and modes
		(efs-parse-mode-string (substring modes 1)))))))

;;;; ------------------------------------------------------------
;;;; Redefinition of Emacs file modtime support.
;;;; ------------------------------------------------------------

(defun efs-day-number (year month day)
  ;; Returns the day number within year of date. Taken from calendar.el,
  ;; by Edward Reingold. Thanks.
  ;; An explanation of the calculation can be found in PascAlgorithms by
  ;; Edward and Ruth Reingold, Scott-Foresman/Little, Brown, 1988.
  (let ((day-of-year (+ day (* 31 (1- month)))))
    (if (> month 2)
	(progn
	  (setq day-of-year (- day-of-year (/ (+ 23 (* 4 month)) 10)))
	  (if (zerop (% year 4))
	      (setq day-of-year (1+ day-of-year)))))
    day-of-year))

(defun efs-days-elapsed (year month day)
  ;; Number of days elapsed since Jan 1, `efs-time-zero'
  (+ (efs-day-number year month day)         ; days this year
     (* 365 (- year efs-time-zero))          ; days in prior years
     (- (/ (max (1- year) efs-time-zero) 4)
	(/ efs-time-zero 4))                 ; leap years
     -1 ))                                   ; don't count today

;; 2^16 = 65536
;; Use this to avoid overflows

(defun efs-seconds-elapsed (year month day hours minutes seconds)
  ;; Computes the seconds elapsed from `efs-time-zero', in emacs'
  ;; format of a list of two integers, the first the higher 16-bits,
  ;; the second the lower 16-bits.
  (let* ((days (efs-days-elapsed year month day))
	 ;; compute hours
	 (hours (+ (* 24 days) hours))
	 (high (lsh hours -16))
	 (low (- hours (lsh high 16)))
	 ;; compute minutes
	 (low (+ (* low 60) minutes))
	 (carry (lsh low -16))
	 (high (+ (* high 60) carry))
	 (low (- low (lsh carry 16)))
	 ;; compute seconds
	 (low (+ (* low 60) seconds))
	 (carry (lsh low -16))
	 (high (+ (* high 60) carry))
	 (low (- low (lsh carry 16))))
    (list high low)))

(defun efs-parse-mdtime (string)
  ;; Parse a string, which is assumed to be the result of an ftp MDTM command.
  (efs-save-match-data
    (if (string-match efs-mdtm-msgs string)
	(efs-seconds-elapsed
	 (string-to-int (substring string 4 8))
	 (string-to-int (substring string 8 10))
	 (string-to-int (substring string 10 12))
	 (string-to-int (substring string 12 14))
	 (string-to-int (substring string 14 16))
	 (string-to-int (substring string 16 18))))))

(defun efs-parse-ctime (string)
  ;; Parse STRING which is assumed to be the result of a query over port 37.
  ;; Returns the number of seconds since the turn of the century, as a
  ;; list of two 16-bit integers.
  (and (= (length string) 4)
       (list (+ (lsh (aref string 0) 8) (aref string 1))
	     (+ (lsh (aref string 2) 8) (aref string 3)))))

(defun efs-time-minus (time1 time2)
  ;; Subtract 32-bit integers, represented as two 16-bit integers.
  (let ((high (- (car time1) (car time2)))
	(low (- (nth 1 time1) (nth 1 time2))))
    (cond
     ((and (< high 0) (> low 0))
      (setq high (1+ high)
	    low (- low 65536)))
     ((and (> high 0) (< low 0))
      (setq high (1- high)
	    low (+ 65536 low))))
    (list high low)))

(defun efs-time-greater (time1 time2)
  ;; Compare two 32-bit integers, each represented as a list of two 16-bit
  ;; integers.
  (or (> (car time1) (car time2))
      (and (= (car time1) (car time2))
	   (> (nth 1 time1) (nth 1 time2)))))

(defun efs-century-time (host &optional nowait cont)
  ;; Treat nil as the local host.
  ;; Returns the # of seconds since the turn of the century, according
  ;; to the system clock on host.
  ;; CONT is called with first arg HOST and second the # of seconds.
  (or host (setq host (system-name)))
  (efs-set-host-property host 'last-ctime nil)
  (efs-set-host-property host 'ctime-cont cont)
  (let ((name (format efs-ctime-process-name-format host))
	proc)
    (condition-case nil (delete-process name) (error nil))
    (if (and
	 (or (efs-save-match-data (string-match efs-local-host-regexp host))
	     (string-equal host (system-name)))
	 (setq proc (condition-case nil
			(open-network-stream name nil host 37)
		      (error nil))))
	(progn
	  (set (intern name) "")
	  (set-process-filter
	   proc
	   (function
	    (lambda (proc string)
	      (let ((name (process-name proc))
		    result)
		(set (intern name) (concat (symbol-value (intern name))
					   string))
		(setq result (efs-parse-ctime
			      (symbol-value (intern name))))
		(if result
		    (let* ((host (substring name 11 -1))
			   (cont (efs-get-host-property host 'ctime-cont)))
		      (efs-set-host-property host 'last-ctime result)
		      (condition-case nil (delete-process proc) (error nil))
		      (if cont
			  (progn
			    (efs-set-host-property host 'ctime-cont nil)
			    (efs-call-cont cont host result)))))))))
	  (set-process-sentinel
	   proc
	   (function
	    (lambda (proc state)
	      (let* ((name (process-name proc))
		     (host (substring name 11 -1))
		     (cont (efs-get-host-property host 'ctime-cont)))
		(makunbound (intern name))
		(or (efs-get-host-property host 'last-ctime)
		    (if cont
			(progn
			  (efs-set-host-property host 'ctime-cont nil)
			  (efs-call-cont cont host 'failed))))))))
	  (if nowait
	      nil
	    (let ((quit-flag nil)
		  (inhibit-quit nil))
	      (while (memq (process-status proc) '(run open))
		(accept-process-output)))
	    (accept-process-output)
	    (or (efs-get-host-property host 'last-ctime)
		'failed)))
      (if cont
	  (progn
	    (efs-set-host-property host 'ctime-cont nil)
	    (efs-call-cont cont host 'failed)))
      (if nowait nil 'failed))))

(defun efs-clock-difference (host &optional nowait)
  ;; clock difference with the local host
  (let ((result (efs-get-host-property host 'clock-diff)))
    (or
     result
     (progn
       (efs-century-time
	host nowait
	(efs-cont (host result) (nowait)
	  (if (eq result 'failed)
	      (efs-set-host-property host 'clock-diff 'failed)
	    (efs-century-time
	     nil nowait
	     (efs-cont (lhost lresult) (host result)
	       (if (eq lresult 'failed)
		   (efs-set-host-property host 'clock-diff 'failed)
		 (efs-set-host-property host 'clock-diff
					(efs-time-minus result lresult))))))))
       (and (null nowait)
	    (or (efs-get-host-property host 'clock-diff)
		'failed))))))

(defun efs-get-file-mdtm (host user file path)
  "For HOST and USER, return FILE's last modification time.
PATH is the file name in full efs syntax.
Returns a list of two six-digit integers which represent the 16 high order
bits, and 16 low order bits of the number of elapsed seconds since
`efs-time-zero'"
  (and (null (efs-get-host-property host 'mdtm-failed))
       (let ((result (efs-send-cmd host user (list 'quote 'mdtm file)
				   (and (eq efs-verbose t)
					"Getting modtime")))
	     parsed)
	 (if (and (null (car result))
		  (setq parsed (efs-parse-mdtime (nth 1 result))))
	     (let ((ent (efs-get-file-entry path)))
	       (if ent
		   (setcdr ent (list (nth 1 ent) (nth 2 ent)
				     (nth 3 ent) (nth 4 ent)
				     parsed)))
	       parsed)
	   (efs-save-match-data
	     ;; The 550 error is for a nonexistent file.  Actually implies
	     ;; that MDTM works.
	     (if (string-match "^550 " (nth 1 result))
		 '(0 0)
	       (efs-set-host-property host 'mdtm-failed t)
	       nil))))))

(efs-define-fun efs-set-emacs-bvf-mdtm (buffer mdtm)
  ;; Sets cached value for the buffer visited file modtime.
  (if (get-buffer buffer)
      (save-excursion
	(set-buffer buffer)
	(let (file-name-handler-alist)
	  (set-visited-file-modtime mdtm)))))

;; (defun efs-set-visited-file-modtime (&optional time)
;;   ;; For remote files sets the modtime for a buffer to be that of the
;;   ;; visited file. With arg TIME sets the modtime to TIME. TIME must be a list
;;   ;; of two 16-bit integers.
;;   ;; The function set-visited-file-modtime is for emacs-19. It doesn't
;;   ;; exist in emacs 18. If you're running efs, it will work in emacs 18 for
;;   ;; remote files only.
;;   (if time
;;       (efs-set-emacs-bvf-mdtm (current-buffer) time)
;;     (let* ((path buffer-file-name)
;; 	   (parsed (efs-ftp-path path))
;; 	   (host (car parsed))
;; 	   (user (nth 1 parsed))
;; 	   (file (nth 2 parsed))
;; 	   (buffer (current-buffer)))
;;       (if (efs-save-match-data
;; 	    (and efs-verify-modtime-host-regexp
;; 		 (string-match efs-verify-modtime-host-regexp host)
;; 		 (or efs-verify-anonymous-modtime
;; 		     (not (efs-anonymous-p user)))
;; 		 (not (efs-get-host-property host 'mdtm-failed))))
;; 	  (efs-send-cmd
;; 	   host user (list 'quote 'mdtm file)
;; 	   nil nil
;; 	   (efs-cont (result line cont-lines) (host user path buffer)
;; 	     (let (modtime)
;; 	       (if (and (null result)
;; 			(setq modtime (efs-parse-mdtime line)))
;; 		   (let ((ent (efs-get-file-entry path)))
;; 		     (if ent
;; 			 (setcdr ent (list (nth 1 ent) (nth 2 ent)
;; 					   (nth 3 ent) (nth 4 ent)
;; 					   modtime)))
;; 		     (setq buffer (and (setq buffer (get-buffer buffer))
;; 				       (buffer-name buffer)))
;; 		     ;; Beware that since this is happening asynch, the buffer
;; 		     ;; may have disappeared.
;; 		     (and buffer (efs-set-emacs-bvf-mdtm buffer modtime)))
;; 		 (efs-save-match-data
;; 		   (or (string-match "^550 " line)
;; 		       (efs-set-host-property host 'mdtm-failed t)))
;; 		 (efs-set-emacs-bvf-mdtm buffer 0)))) ; store dummy values
;; 	   0) ; Always do this NOWAIT = 0
;; 	(efs-set-emacs-bvf-mdtm buffer 0))
;;       nil) ; return NIL
;;     ))

(defvar efs-set-modtimes-synchronously nil
  "*Whether efs uses a synchronous FTP command to set the visited file modtime.
Setting this variable to non-nil means that efs will set visited file modtimes
synchronously.

Asynchronous setting of visited file modtimes leaves a very small
window where Emacs may fail to detect a super session.  However, it gives
faster user access to newly visited files.")


(defun efs-set-visited-file-modtime (&optional time)
  ;; For remote files sets the modtime for a buffer to be that of the
  ;; visited file. With arg TIME sets the modtime to TIME. TIME must be a list
  ;; of two 16-bit integers.
  ;; The function set-visited-file-modtime is for emacs-19. It doesn't
  ;; exist in emacs 18. If you're running efs, it will work in emacs 18 for
  ;; remote files only.
  (if time
      (efs-set-emacs-bvf-mdtm (current-buffer) time)
    (let* ((path buffer-file-name)
	   (parsed (efs-ftp-path path))
	   (host (car parsed))
	   (user (nth 1 parsed))
	   (file (nth 2 parsed))
	   (buffer (current-buffer)))
      (if (efs-save-match-data
	    (and efs-verify-modtime-host-regexp
		 (string-match efs-verify-modtime-host-regexp host)
		 (or efs-verify-anonymous-modtime
		     (not (efs-anonymous-p user)))
		 (not (efs-get-host-property host 'mdtm-failed))))
	  (progn
	    (or efs-set-modtimes-synchronously (clear-visited-file-modtime))
	    (efs-send-cmd
	     host user (list 'quote 'mdtm file)
	     nil nil
	     (efs-cont (result line cont-lines) (host user path buffer)
	       (let (modtime)
		 (if (and (null result)
			  (setq modtime (efs-parse-mdtime line)))
		     (let ((ent (efs-get-file-entry path)))
		       (if ent
			   (setcdr ent (list (nth 1 ent) (nth 2 ent)
					     (nth 3 ent) (nth 4 ent)
					     modtime)))
		       (setq buffer (and (setq buffer (get-buffer buffer))
					 (buffer-name buffer)))
		       ;; Beware that since might be happening asynch,
		       ;; the buffer may have disappeared.
		       (and buffer (efs-set-emacs-bvf-mdtm buffer modtime)))
		   (efs-save-match-data
		     (or (string-match "^550 " line)
			 (efs-set-host-property host 'mdtm-failed t)))
		   (efs-set-emacs-bvf-mdtm buffer '(0 0))))) ; store dummy values
	     (and (null efs-set-modtimes-synchronously) 0)))
	(efs-set-emacs-bvf-mdtm buffer '(0 0)))
      nil))) ; return NIL

(defun efs-file-newer-than-file-p (file1 file2)
  ;; Version of file-newer-than-file-p for remote files.
  (let* ((file1 (expand-file-name file1))
	 (file2 (expand-file-name file2))
	 (parsed1 (efs-ftp-path file1))
	 (parsed2 (efs-ftp-path file2))
	 (host1 (car parsed1))
	 (host2 (car parsed2))
	 (user1 (nth 1 parsed1))
	 (user2 (nth 1 parsed2)))
    (cond
     ;; If the first file doedn't exist, or is remote but
     ;; we're not supposed to check modtimes on it, return nil.
     ((or (null (file-exists-p file1))
	  (and parsed1
	       (or
		(null efs-verify-modtime-host-regexp)
		(efs-get-host-property host1 'mdtm-failed)
		(not (string-match efs-verify-modtime-host-regexp host1))
		(and (null efs-verify-anonymous-modtime)
		     (efs-anonymous-p user1)))))
      nil)
     ;; If the same is true for the second file, return t.
     ((or (null (file-exists-p file2))
	  (and parsed2
	       (or
		(null efs-verify-modtime-host-regexp)
		(efs-get-host-property host2 'mdtm-failed)
		(not (string-match efs-verify-modtime-host-regexp host2))
		(and (null efs-verify-anonymous-modtime)
		     (efs-anonymous-p user2)))))
      t)
     ;; Calculate modtimes. If we get here, any remote files should
     ;; have a file entry.
     (t
      (let (mod1 mod2 shift1 shift2)
	(if parsed1
	    (let ((ent (efs-get-file-entry file1)))
	      (setq mod1 (nth 5 ent)
		    shift1 (efs-clock-difference host1))
	      (or mod1
		  (setq mod1 (efs-get-file-mdtm
			      host1 user1 (nth 2 parsed1) file1))))
	  (setq mod1 (nth 5 (file-attributes file1))))
	(if parsed2
	    (let ((ent (efs-get-file-entry file2)))
	      (setq mod2 (nth 5 ent)
		    shift2 (efs-clock-difference host2))
	      (or mod2
		  (setq mod2 (efs-get-file-mdtm
				host2 user2 (nth 2 parsed2) file2))))
	  (setq mod2 (nth 5 (file-attributes file2))))
	;; If we can't compute clock shifts, we act as if we don't
	;; even know the modtime. Should we have more faith in ntp?
	(cond
	 ((or (null mod1) (eq shift1 'failed))
	  nil)
	 ((or (null mod2) (eq shift2 'failed))
	  t)
	 ;; We get to compute something!
	 (t
	  (efs-time-greater
	   (if shift1 (efs-time-minus mod1 shift1) mod1)
	   (if shift2 (efs-time-minus mod2 shift2) mod2)))))))))

(defun efs-verify-visited-file-modtime (buff)
  ;; Verifies the modtime for buffers visiting remote files.
  ;; Won't get called for buffer not visiting any file.
  (let ((buff (get-buffer buff)))
    (null 
     (and buff ; return t if no buffer?  Need to beware of multi-threading.
	  (buffer-file-name buff) ; t if no file
	  (let ((mdtm (save-excursion
			(set-buffer buff)
			(visited-file-modtime))))
	    (and
	     (not (eq mdtm 0))
	     (not (equal mdtm '(0 0)))
	     efs-verify-modtime-host-regexp
	     (let* ((path (buffer-file-name buff))
		    (parsed (efs-ftp-path path))
		    (host (car parsed))
		    (user (nth 1 parsed))
		    nmdtm)
	       (and
		(null (efs-get-host-property host 'mdtm-failed))
		(efs-save-match-data
		  (string-match
		   efs-verify-modtime-host-regexp host))
		(or  efs-verify-anonymous-modtime
		     (not (efs-anonymous-p user)))
		(setq nmdtm (efs-get-file-mdtm host user (nth 2 parsed) path))
		(progn
		  (or (equal nmdtm '(0 0))
		      (file-exists-p path) ; Make sure that there is an entry.
		      (null
		       (efs-get-files
			(file-name-directory
			 (efs-internal-directory-file-name path))))
		      (efs-add-file-entry
		       (efs-host-type host) path nil nil nil nil nil nmdtm))
		  (null (and (eq (cdr mdtm) (nth 1 nmdtm))
			     (eq (car mdtm) (car nmdtm)))))))))))))

;;;; -----------------------------------------------------------
;;;; Redefinition of Emacs file name completion
;;;; -----------------------------------------------------------

(defmacro efs-set-completion-ignored-pattern ()
  ;; Set regexp efs-completion-ignored-pattern
  ;; to use for filename completion.
  (`
   (or (equal efs-completion-ignored-extensions
	      completion-ignored-extensions)
       (setq efs-completion-ignored-extensions
	     completion-ignored-extensions
	     efs-completion-ignored-pattern
	     (mapconcat (function
			 (lambda (s) (if (stringp s)
					 (concat (regexp-quote s) "$")
				       "/"))) ; / never in filename
			efs-completion-ignored-extensions
			"\\|")))))

(defun efs-file-entry-active-p (sym)
  ;; If the file entry is a symlink, returns whether the file pointed to
  ;; exists.
  ;; Note that DIR is dynamically bound.
  (let ((file-type (car (get sym 'val))))
    (or (not (stringp file-type))
	(file-exists-p (efs-chase-symlinks
			(expand-file-name file-type efs-completion-dir))))))

(defun efs-file-entry-not-ignored-p (sym)
  ;; If the file entry is not a directory (nor a symlink pointing to a
  ;; directory) returns whether the file (or file pointed to by the symlink)
  ;; is ignored by completion-ignored-extensions.
  (let ((file-type (car (get sym 'val)))
	(symname (symbol-name sym)))
    (if (stringp file-type)
	;; Maybe file-truename would be better here, but it is very costly
	;; to chase symlinks at every level over FTP.
	(let ((file (efs-chase-symlinks (expand-file-name
					 file-type efs-completion-dir))))
	  (or (file-directory-p file)
	      (and (file-exists-p file)
		   (not (string-match efs-completion-ignored-pattern
				      symname)))))
      (or file-type ; is a directory name
	  (not (string-match efs-completion-ignored-pattern symname))))))

(defun efs-file-name-all-completions (file dir)
  ;; Does file-name-all-completions in remote directories.
  (efs-barf-if-not-directory dir)
  (let* ((efs-completion-dir (file-name-as-directory (expand-file-name dir)))
	 (completion-ignore-case
	  (memq (efs-host-type (car (efs-ftp-path efs-completion-dir)))
		efs-case-insensitive-host-types))
	 (tbl (efs-get-files efs-completion-dir))
	 (completions
	  (all-completions file tbl
			   (function efs-file-entry-active-p))))
    ;; see whether each matching file is a directory or not...
    (mapcar
     ;; Since the entries in completions will match the case
     ;; of the entries in tbl, don't need to case-fold
     ;; in efs-get-hash-entry below.
     (function
      (lambda (file)
	(let ((ent (car (efs-get-hash-entry file tbl))))
	  (if (or (eq ent t)
		  (and (stringp ent)
		       (file-directory-p (efs-chase-symlinks
					  (expand-file-name
					   ent efs-completion-dir)))))
	      (concat file "/")
	    file))))
     completions)))

(defun efs-file-name-completion (file dir)
  ;; Does file name expansion in remote directories.
  (efs-barf-if-not-directory dir)
  (if (equal file "")
      ""
    (let* ((efs-completion-dir (file-name-as-directory (expand-file-name dir)))
	   (completion-ignore-case
	    (memq (efs-host-type (car (efs-ftp-path efs-completion-dir)))
		  efs-case-insensitive-host-types))
	   (tbl (efs-get-files efs-completion-dir)))
      (efs-set-completion-ignored-pattern)
      (efs-save-match-data
	(or (efs-file-name-completion-1
	     file tbl efs-completion-dir
	     (function efs-file-entry-not-ignored-p))
	    (efs-file-name-completion-1
	     file tbl efs-completion-dir
	     (function efs-file-entry-active-p)))))))

(defun efs-file-name-completion-1 (file tbl dir predicate)
  ;; Internal subroutine for efs-file-name-completion.  Do not call this.
  (let ((bestmatch (try-completion file tbl predicate)))
    (if bestmatch
	(if (eq bestmatch t)
	    (if (file-directory-p (expand-file-name file dir))
		(concat file "/")
	      t)
	  (if (and (eq (try-completion bestmatch tbl predicate) t)
		   (file-directory-p
		    (expand-file-name bestmatch dir)))
	      (concat bestmatch "/")
	    bestmatch)))))

;;;; ----------------------------------------------------------
;;;; Functions for loading lisp.
;;;; ----------------------------------------------------------

;;; jka-load provided ideas here. Thanks, Jay.

(defun efs-load-openp (str suffixes)
  ;; Given STR, searches load-path and efs-load-lisp-extensions
  ;; for the name of a file to load. Returns the full path, or nil
  ;; if none found.
  (let ((path-list (if (file-name-absolute-p str) t load-path))
	root result)
    ;; If there is no load-path, at least try the default directory.
    (or path-list
	(setq path-list (list default-directory)))
    (while (and path-list (null result))
      (if (eq path-list t)
	  (setq path-list nil
		root str)
	(setq root (expand-file-name str (car path-list))
	      path-list (cdr path-list))
	(or (file-name-absolute-p root)
	    (setq root (expand-file-name root default-directory))))
      (let ((suff-list suffixes))
	(while (and suff-list (null result))
	  (let ((try (concat root (car suff-list))))
	    (if (or (not (file-readable-p try))
		    (file-directory-p try))
		(setq suff-list (cdr suff-list))
	      (setq result try))))))
    result))

(defun efs-load (file &optional noerror nomessage nosuffix)
  "Documented as original."
  (let ((filename (efs-load-openp
		   file
		   (if nosuffix '("") efs-load-lisp-extensions))))
    (if (not filename)
	(and (null noerror) (error "Cannot open load file %s" file))
      (let ((parsed (efs-ftp-path filename))
	    (after-load (and (boundp 'after-load-alist)
			     (assoc file after-load-alist))))
	(if parsed
	    (let ((temp (car (efs-make-tmp-name nil (car parsed)))))
	      (unwind-protect
		  (progn
		    (efs-copy-file-internal
		     filename parsed temp nil t nil
		     (format "Getting %s" filename))
		    (or (file-readable-p temp)
			(error
			 "efs-load: temp file %s is unreadable" temp))
		    (or nomessage
			(message "Loading %s..." file))
		    ;; temp is an absolute filename, so load path
		    ;; won't be searched.
		    (let (after-load-alist)
		      (efs-real-load temp t t t))
		    (or nomessage
			(message "Loading %s...done" file))
		    (if after-load (mapcar 'eval (cdr after-load)))
		    t) ; return t if everything worked
		(efs-del-tmp-name temp)))
	  (prog2
	   (or nomessage
	       (message "Loading %s..." file))
	   (let (after-load-alist)
	     (or (efs-real-load filename noerror t t)
		 (setq after-load nil)))
	   (or nomessage
	       (message "Loading %s...done" file))
	   (if after-load (mapcar 'eval (cdr after-load)))))))))

(defun efs-require (feature &optional filename)
  "Documented as original."
  (if (eq feature 'ange-ftp) (efs-require-scream-and-yell))
  (if (featurep feature)
      feature
    (or filename (setq filename (symbol-name feature)))
    (let ((fullpath (efs-load-openp filename
				    efs-load-lisp-extensions)))
      (if (not fullpath)
	  (error "Cannot open load file: %s" filename)
	(let ((parsed (efs-ftp-path fullpath)))
	  (if parsed
	      (let ((temp (car (efs-make-tmp-name nil (car parsed)))))
		(unwind-protect
		    (progn
		      (efs-copy-file-internal
		       fullpath parsed temp nil t nil
		       (format "Getting %s" fullpath))
		      (or (file-readable-p temp)
			  (error
			   "efs-require: temp file %s is unreadable" temp))
		      (efs-real-require feature temp))
		  (efs-del-tmp-name temp)))
	    (efs-real-require feature fullpath)))))))

(defun efs-require-scream-and-yell ()
  ;; Complain if something attempts to load ange-ftp.
  (with-output-to-temp-buffer "*Help*"
    (princ
     "Something tried to load ange-ftp.
EFS AND ANGE-FTP DO NOT WORK TOGETHER.

If the culprit package does need to access ange-ftp internal functions,
then it should be adequate to simply remove the \(require 'ange-ftp\)
line and let efs handle remote file access.  Otherwise, it will need to
be ported to efs.  This may already have been done, and you can find out
by sending an enquiry to efs-help@cuckoo.hpl.hp.com.

Signalling an error with backtrace will allow you to determine which
package was requiring ange-ftp.\n"))
  (select-window (get-buffer-window "*Help*"))
  (enlarge-window (- (count-lines (point-min) (point-max))
		     (window-height) -1))
  (if (y-or-n-p "Signal error with backtrace? ")
      (let ((stack-trace-on-error t))
	(error "Attempt to require ange-ftp"))))

;;;; -----------------------------------------------------------
;;;; Redefinition of Emacs functions for reading file names.
;;;; -----------------------------------------------------------

(defun efs-unexpand-parsed-filename (host user path)
  ;; Replaces the home directory in path with "~". Returns the unexpanded
  ;; full-path.
  (let* ((path-len (length path))
	 (def-user (efs-get-user host))
	 (host-type (efs-host-type host user))
	 (ignore-case (memq host-type efs-case-insensitive-host-types)))
    (if (> path-len 1)
      (let* ((home (efs-expand-tilde "~" host-type host user))
	     (home-len (length home)))
	(if (and (> path-len home-len)
		 (if ignore-case (string-equal (downcase home)
					       (downcase
						(substring path
							   0 home-len)))
		   (string-equal home (substring path 0 home-len)))
		 (= (aref path home-len) ?/))
	    (setq path (concat "~" (substring path home-len))))))
    (if (if ignore-case (string-equal (downcase user)
				      (downcase def-user))
	  (string-equal user def-user))
	(format efs-path-format-without-user host path)
      (format efs-path-format-string user host path))))

(efs-define-fun efs-abbreviate-file-name (filename)
  ;; Version of abbreviate-file-name for remote files.
  (efs-save-match-data
    (let ((tail directory-abbrev-alist))
      (while tail
	(if (string-match (car (car tail)) filename)
	    (setq filename
		  (concat (cdr (car tail))
			  (substring filename (match-end 0)))))
	(setq tail (cdr tail)))
      (apply 'efs-unexpand-parsed-filename (efs-ftp-path filename)))))

(defun efs-default-dir-function ()
  (let ((parsed (efs-ftp-path default-directory))
	(dd default-directory))
    (if parsed
	(efs-save-match-data
	  (let ((tail directory-abbrev-alist))
	    (while tail
	      (if (string-match (car (car tail)) dd)
		  (setq dd (concat (cdr (car tail))
				   (substring dd (match-end 0)))
			parsed nil))
	      (setq tail (cdr tail)))
	    (apply 'efs-unexpand-parsed-filename
		   (or parsed (efs-ftp-path dd)))))
      default-directory)))

(defun efs-re-read-dir (&optional dir)
  "Forces a re-read of the directory DIR.
If DIR is omitted then it defaults to the directory part of the contents
of the current buffer. This is so this function can be caled from the
minibuffer."
  (interactive)
  (if dir
      (setq dir (expand-file-name dir))
    (setq dir (file-name-directory (expand-file-name (buffer-string)))))
  (let ((parsed (efs-ftp-path dir)))
    (if parsed
	(let ((efs-ls-uncache t))
	  (efs-del-hash-entry (efs-canonize-file-name dir)
			      efs-files-hashtable)
	  (efs-get-files dir t)))))

;;;; ---------------------------------------------------------------
;;;; Creation and deletion of files and directories.
;;;; ---------------------------------------------------------------

(defun efs-delete-file (file)
  ;; Deletes remote files.
  (let* ((file (expand-file-name file))
	 (parsed (efs-ftp-path file))
	 (host (car parsed))
	 (user (nth 1 parsed))
	 (host-type (efs-host-type host user))
	 (path (nth 2 parsed))
	 (abbr (efs-relativize-filename file))
	 (result (efs-send-cmd host user (list 'delete path)
			       (format "Deleting %s" abbr))))
    (if (car result)
	(signal 'ftp-error
		(list "Removing old name"
		      (format "FTP Error: \"%s\"" (nth 1 result))
		      file)))
    (efs-delete-file-entry host-type file)))

(defun efs-make-directory-internal (dir)
  ;; version of make-directory-internal for remote directories.
  (if (file-exists-p dir)
      (error "Cannot make directory %s: file already exists" dir)
    (let* ((parsed (efs-ftp-path dir))
	   (host (nth 0 parsed))
	   (user (nth 1 parsed))
	   (host-type (efs-host-type host user))
	   ;; Some ftp's on unix machines (at least on Suns)
	   ;; insist that mkdir take a filename, and not a
	   ;; directory-name name as an arg. Argh!! This is a bug.
	   ;; Non-unix machines will probably always insist
	   ;; that mkdir takes a directory-name as an arg
	   ;; (as the ftp man page says it should).
	   (path (if (or (memq host-type efs-unix-host-types)
			 (memq host-type '(os2 dos)))
		     (efs-internal-directory-file-name (nth 2 parsed))
		   (efs-internal-file-name-as-directory
		    host-type (nth 2 parsed))))
	   (abbr (efs-relativize-filename dir))
	   (result (efs-send-cmd host user
				 (list 'mkdir path)
				 (format "Making directory %s"
					 abbr))))
      (if (car result)
	  (efs-error host user
		     (format "Could not make directory %s: %s" dir
			     (nth 1 result))))
      (efs-add-file-entry host-type dir t nil user))))

;; V19 calls this function delete-directory. It used to be called
;; remove-directory.

(defun efs-delete-directory (dir)
  ;; Version of delete-directory for remote directories.
  (if (file-directory-p dir)
      (let* ((parsed (efs-ftp-path dir))
	     (host (nth 0 parsed))
	     (user (nth 1 parsed))
	     (host-type (efs-host-type host user))
	     ;; Some ftp's on unix machines (at least on Suns)
	     ;; insist that rmdir take a filename, and not a
	     ;; directory-name name as an arg. Argh!! This is a bug.
	     ;; Non-unix machines will probably always insist
	     ;; that rmdir takes a directory-name as an arg
	     ;; (as the ftp man page says it should).
	     (path 
	      (if (or (memq host-type efs-unix-host-types)
		      (memq host-type '(os2 dos)))
		  (efs-internal-directory-file-name (nth 2 parsed))
		(efs-internal-file-name-as-directory
		 host-type (nth 2 parsed))))
	     (abbr (efs-relativize-filename dir))
	     (result (efs-send-cmd host user
				   (list 'rmdir path)
				   (format "Deleting directory %s" abbr))))
	(if (car result)
	    (efs-error host user
		       (format "Could not delete directory %s: %s"
			       dir (nth 1 result))))
	(efs-delete-file-entry host-type dir t))
    (error "Not a directory: %s" dir)))

(defun efs-file-local-copy (file)
  ;; internal function for diff.el (dired 6.3 or later)
  ;; Makes a temp file containing the contents of file.
  ;; returns the name of the tmp file created, or nil if none is.
  ;; This function should have optional cont and nowait args.
  (let* ((file (expand-file-name file))
	 (tmp (car (efs-make-tmp-name nil  (car (efs-ftp-path file))))))
    (efs-copy-file-internal file (efs-ftp-path file)
			    tmp nil t nil (format "Getting %s" file))
    tmp))

(defun efs-diff/grep-del-temp-file (temp)
  ;; internal function for diff.el and grep.el
  ;; if TEMP is non-nil, deletes the temp file TEMP.
  ;; if TEMP is nil, does nothing.
  (and temp
       (efs-del-tmp-name temp)))
	    
;;;; ------------------------------------------------------------
;;;; File copying support...
;;;; ------------------------------------------------------------

;;;  - totally re-written 6/24/92.
;;;  - re-written again 9/3/93
;;;  - and again 14/4/93
;;;  - and again 17/8/93

(defun efs-barf-or-query-if-file-exists (absname querystring interactive)
  (if (file-exists-p absname)
      (if (not interactive)
	  (signal 'file-already-exists (list absname))
	(if (not (yes-or-no-p (format "File %s already exists; %s anyway? "
				      absname querystring)))
	    (signal 'file-already-exists (list absname))))))

(defun efs-concatenate-files (file1 file2)
  ;; Concatenates file1 to file2. Both must be local files.
  ;; Needed because the efs version of copy-file understands
  ;; ok-if-already-exists = 'append
  (or (file-readable-p file1)
      (signal 'file-error
	      (list (format "Input file %s not readable." file1))))
  (or (file-writable-p file2)
      (signal 'file-error
	      (list (format "Output file %s not writable." file2))))
  (let ((default-directory exec-directory))
    (call-process "sh" nil nil nil "-c" (format "cat %s >> %s" file1 file2))))

(defun efs-copy-add-file-entry (newname host-type user size append)
  ;; Add an entry in `efs-files-hashtable' for a file newly created via a copy.
  (if (eq size -1) (setq size nil))
  (if append
      (let ((ent (efs-get-file-entry newname)))
	(if (and ent (null (car ent)))
	    (if (and size (numberp (nth 1 ent)))
		(setcar (cdr ent) (+ size (nth 1 ent)))
	      (setcar (cdr ent) nil))
	  ;; If the ent is a symlink or directory, don't overwrite that entry.
	  (if (null ent)
	      (efs-add-file-entry host-type newname nil nil nil))))
    (efs-add-file-entry host-type newname nil size user)))
  
(defun efs-copy-remote-to-remote (f-host-type f-host f-user f-path filename
					      t-host-type t-host t-user
					      t-path newname append msg cont
					      nowait xfer-type)
;; Use a 3rd data connection to copy from F-HOST for F-USER to T-HOST
;; for T-USER.
  (if (efs-get-host-property t-host 'pasv-failed)
      ;; PASV didn't work before, don't try again.
      (if cont (efs-call-cont cont 'failed "" ""))
    (or xfer-type
	(setq xfer-type (efs-xfer-type f-host-type filename
				       t-host-type newname)))
    (efs-send-cmd
     t-host t-user '(quote pasv) nil nil
     (efs-cont (pasv-result pasv-line pasv-cont-lines)
	 (cont nowait f-host-type f-host f-user f-path filename
	       t-host-type t-host t-user t-path newname xfer-type msg append)
       (efs-save-match-data
	 (if (or pasv-result
		 (not (string-match efs-pasv-msgs pasv-line)))
	     (progn
	       (efs-set-host-property t-host 'pasv-failed t)
	       (if cont
		   (efs-call-cont
		    cont (or pasv-result 'failed) pasv-line pasv-cont-lines)))
	   (let ((address (substring pasv-line (match-beginning 1)
				     (match-end 1))))
	     (efs-send-cmd
	      f-host f-user
	      (list 'quote 'port address) nil nil
	      (efs-cont (port-result port-line port-cont-lines)
		  (cont f-host f-user f-host-type f-path filename
			xfer-type msg)
		(if port-result
		    (if cont
			(efs-call-cont
			 cont port-result port-line port-cont-lines)
		      (efs-error f-host f-user
				 (format "PORT failed for %s: %s"
					 filename port-line)))
		  (efs-send-cmd
		   f-host f-user
		   (list 'quote 'retr f-path xfer-type)
		   msg nil
		   (efs-cont (retr-result retr-line retr-cont-lines)
		       (cont f-host f-user f-path)
		     (and retr-result
			  (null cont)
			  (efs-error
			   f-host f-user
			   (format "RETR failed for %s: %s"
				   f-path retr-line)))
		     (if cont (efs-call-cont
			       cont retr-result retr-line retr-cont-lines)))
		   (if (eq nowait t) 1 nowait))))
	      1) ; can't ever wait on this command.
	     (efs-send-cmd
	      t-host t-user
	      (list 'quote (if append 'appe 'stor) t-path xfer-type)
	      nil nil
	      (efs-cont (stor-result stor-line stor-cont-lines)
		  (t-host t-user t-path t-host-type newname filename
			  append)
		(if stor-result
		    (efs-error
		     t-host t-user (format "%s failed for %s: %s"
					   (if append "APPE" "STOR")
					   t-path stor-line))
		  (efs-copy-add-file-entry
		   newname t-host-type t-user
		   (nth 1 (efs-get-file-entry filename)) append)))
	      (if (eq nowait t) 1 nowait))))))
     nowait)))

(defun efs-copy-on-remote (host user host-type filename newname filename-parsed
				newname-parsed keep-date append-p msg cont
				nowait xfer-type)
  ;; Uses site exec to copy the file on a remote host
  (let ((exec-cp (efs-get-host-property host 'exec-cp)))
    (if (or append-p
	    (not (memq host-type efs-unix-host-types))
	    (efs-get-host-property host 'exec-failed)
	    (eq exec-cp 'failed))
	(efs-copy-via-temp filename filename-parsed newname newname-parsed
			   append-p keep-date msg cont nowait xfer-type)
      (if (eq exec-cp 'works)
	  (efs-send-cmd
	   host user
	   (list 'quote 'site 'exec
		 (format "cp %s%s %s" (if keep-date "-p " "")
			 (nth 2 filename-parsed) (nth 2 newname-parsed)))
	   msg nil
	   (efs-cont (result line cont-lines) (host user filename newname
						    host-type filename-parsed
						    newname-parsed
						    keep-date append-p msg cont
						    xfer-type nowait)
	     (if result
		 (progn
		   (efs-set-host-property host 'exec-failed t)
		   (efs-copy-via-temp filename filename-parsed newname
				      newname-parsed append-p keep-date
				      nil cont nowait xfer-type))
	       (efs-save-match-data
		 (if (string-match "\n200-\\([^\n]*\\)" cont-lines)
		     (let ((err (substring cont-lines (match-beginning 1)
					   (match-end 1))))
		       (if cont
			   (efs-call-cont cont 'failed err cont-lines)
			 (efs-error host user err)))
		   (efs-copy-add-file-entry
		    newname host-type user
		    (nth 7 (efs-file-attributes filename)) nil)
		   (if cont (efs-call-cont cont nil line cont-lines))))))
	   nowait)
	(message "Checking for cp executable on %s..." host)
	(efs-send-cmd
	 host user (list 'quote 'site 'exec "cp / /") nil nil
	 (efs-cont (result line cont-lines) (host user filename newname
						  host-type filename-parsed
						  newname-parsed
						  keep-date append-p msg cont
						  xfer-type nowait)
	   (efs-save-match-data
	     (if (string-match "\n200-" cont-lines)
		 (efs-set-host-property host 'exec-cp 'works)
	       (efs-set-host-property host 'exec-cp 'failed)))
	   (efs-copy-on-remote host user host-type filename newname
			       filename-parsed newname-parsed keep-date
			       append-p msg cont nowait xfer-type))
	 nowait)))))

(defun efs-copy-via-temp (filename filename-parsed newname newname-parsed
				   append keep-date msg cont nowait xfer-type)
  ;; Copies from FILENAME to NEWNAME via a temp file.
  (let* ((temp (car (if (efs-use-gateway-p (car filename-parsed) t)
			(efs-make-tmp-name (car filename-parsed)
					   (car newname-parsed))
		      (efs-make-tmp-name (car newname-parsed)
					 (car filename-parsed)))))
	 (temp-parsed (efs-ftp-path temp)))
    (or xfer-type (setq xfer-type
			(efs-xfer-type
			 (efs-host-type (car filename-parsed)) filename
			 (efs-host-type (car newname-parsed)) newname
			 t)))
    (efs-copy-file-internal
     filename filename-parsed temp temp-parsed t nil (if (eq 0 msg) 2 msg)
     (efs-cont (result line cont-lines) (newname newname-parsed temp
						 temp-parsed append msg cont
						 nowait xfer-type)
       (if result
	   (progn
	     (efs-del-tmp-name temp)
	     (if cont
		 (efs-call-cont cont result line cont-lines)
	       (signal 'ftp-error
		       (list "Opening input file"
			     (format "FTP Error: \"%s\" " line) filename))))
	 (efs-copy-file-internal
	  temp temp-parsed newname newname-parsed (if append 'append t) nil
	  (if (eq msg 0) 1 msg)
	  (efs-cont (result line cont-lines) (temp newname cont)
	    (efs-del-tmp-name temp)
	    (if cont
		(efs-call-cont cont result line cont-lines)
	      (if result
		  (signal 'ftp-error
			  (list "Opening output file"
				(format "FTP Error: \"%s\" " line) newname)))))
	  nowait xfer-type)))
     nowait xfer-type)))

(defun efs-copy-file-internal (filename filename-parsed newname newname-parsed
					ok-if-already-exists keep-date
					&optional msg cont nowait xfer-type)
  ;; Internal function for copying a file from FILENAME to NEWNAME.
  ;; FILENAME-PARSED and NEWNAME-PARSED are the lists obtained by parsing
  ;; FILENAME and NEWNAME with efs-ftp-path.
  ;; If OK-IF-ALREADY-EXISTS is nil, then existing files will not be
  ;; overwritten.
  ;; If it is a number, then the user will be prompted about overwriting.
  ;; If it eq 'append, then an existing file will be appended to.
  ;; If it has anyother value, then existing files will be silently
  ;; overwritten.
  ;; If KEEP-DATE is t then we will attempt to reatin the date of the
  ;; original copy of the file. If this is a string, the modtime of the
  ;; NEWNAME will be set to this date. Must be in touch -t format.
  ;; If MSG is nil, then the copying will be done silently.
  ;; If it is a string, then that will be the massage displayed while copying.
  ;; If it is 0, then a suitable default message will be computed.
  ;; If it is 1, then a suitable default will be computed, assuming
  ;; that FILENAME is a temporary file, whose name is not suitable to use
  ;; in a status message.
  ;; If it is 2, then a suitable default will be used, assuming that
  ;; NEWNAME is a temporary file.
  ;; CONT is a continuation to call after completing the copy.
  ;; The first two args are RESULT and LINE, the result symbol and status
  ;; line of the FTP command. If more than one ftp command has been used,
  ;; then these values for the last FTP command are given.
  ;; NOWAIT can be either nil, 0, 1, t. See `efs-send-cmd' for an explanation.
  ;; XFER-TYPE is the transfer type to use for transferring the files.
  ;; If this is nil, than a suitable transfer type is computed.
  ;; Does not call expand-file-name. Do that yourself.

  ;; check to see if we can overwrite
  (if (or (not ok-if-already-exists)
	  (numberp ok-if-already-exists))
      (efs-barf-or-query-if-file-exists
       newname "copy to it" (numberp ok-if-already-exists)))
  (if (null (or filename-parsed newname-parsed))
      ;; local to local copy
      (progn
	(if (eq ok-if-already-exists 'append)
	    (efs-concatenate-files filename newname)
	  (copy-file filename newname ok-if-already-exists keep-date))
	(if cont
	    (efs-call-cont cont nil "Copied locally" "")))
    (let* ((f-host (car filename-parsed))
	   (f-user (nth 1 filename-parsed))
	   (f-path (nth 2 filename-parsed))
	   (f-host-type (efs-host-type f-host f-user))
	   (f-gate-p (efs-use-gateway-p f-host t))
	   (t-host (car newname-parsed))
	   (t-user (nth 1 newname-parsed))
	   (t-path (nth 2 newname-parsed))
	   (t-host-type (efs-host-type t-host t-user))
	   (t-gate-p (efs-use-gateway-p t-host t))
	   (append-p (eq ok-if-already-exists 'append))
	   gatename)

      (if (and (eq keep-date t) (null newname-parsed))
	  ;; f-host must be remote now.
	  (setq keep-date filename))
	  
      (cond
       
       ;; Check to see if we can do a PUT
       ((or
	 (and (null f-host)
	      (or (null t-gate-p)
		  (setq gatename (efs-local-to-gateway-filename filename))))
	 (and t-gate-p
	      f-host
	      (string-equal (downcase f-host) (downcase efs-gateway-host))
	      (if (memq f-host-type efs-case-insensitive-host-types)
		  (string-equal (downcase f-user)
				(downcase (efs-get-user efs-gateway-host)))
		(string-equal f-user (efs-get-user efs-gateway-host)))))
	(or f-host (let (file-name-handler-alist)
		     (if (file-exists-p filename)
			 (cond
			  ((file-directory-p filename)
			   (signal 'file-error
				   (list "Non-regular file"
					 "is a directory" filename)))
			  ((not (file-readable-p filename))
			   (signal 'file-error
				   (list "Opening input file"
					 "permission denied" filename))))
		       (signal 'file-error
			       (list "Opening input file"
				     "no such file or directory" filename)))))
	(or xfer-type
	    (setq xfer-type
		  (efs-xfer-type f-host-type filename t-host-type newname)))
	(let ((size (and (or (null f-host-type)
			     (efs-file-entry-p filename))
			 (nth 7 (file-attributes filename)))))
	  ;; -1 is a bogus size for remote files
	  (if (eq size -1) (setq size nil))
	  (efs-send-cmd
	   t-host t-user
	   (list (if append-p 'append 'put)
		 (if f-host
		     f-path
		   (or gatename filename))
		 t-path
		 xfer-type)
	   (cond ((eq msg 2)
		  (concat (if append-p "Appending " "Putting ")
			  (efs-relativize-filename filename)))
		 ((eq msg 1)
		  (concat (if append-p "Appending " "Putting ")
			  (efs-relativize-filename newname)))
		 ((eq msg 0)
		  (concat (if append-p "Appending " "Copying ")
			  (efs-relativize-filename filename)
			  " to "
			  (efs-relativize-filename
			   newname (file-name-directory filename) filename)))
		 (t msg))
	   (and size (list 'efs-set-xfer-size t-host t-user size))
	   (efs-cont (result line cont-lines) (newname t-host-type t-user size
						       append-p cont)
	     (if result
		 (if cont
		     (efs-call-cont cont result line cont-lines)
		   (signal 'ftp-error
			   (list "Opening output file"
				 (format "FTP Error: \"%s\" " line) newname)))
	       ;; add file entry
	       (efs-copy-add-file-entry newname t-host-type t-user
					size append-p)
	       (if cont
		   (efs-call-cont cont result line cont-lines))))
	   nowait)))
       
       ;; Check to see if we can do a GET
       ((and
	 ;; I think that giving the append arg, will cause this function
	 ;; to make a temp file, recursively call itself, and append the temp
	 ;; file to the local file. Hope it works out...
	 (null append-p)
	 (or
	  (and (null t-host)
	       (or (null f-gate-p)
		   (setq gatename (efs-local-to-gateway-filename newname))))
	  (and f-gate-p
	       t-host
	       (string-equal (downcase t-host) (downcase efs-gateway-host))
	       (if (memq t-host-type efs-case-insensitive-host-types)
		   (string-equal (downcase t-user)
				 (downcase (efs-get-user efs-gateway-host)))
		 (string-equal t-user (efs-get-user efs-gateway-host))))))
	(or t-host (let (file-name-handler-alist)
		     (cond ((not (file-writable-p newname))
			    (signal 'file-error
				    (list "Opening output file"
					  "permission denied" newname)))
			   ((file-directory-p newname)
			    (signal 'file-error
				    (list "Opening output file"
					  "is a directory" newname))))))
	(or xfer-type
	    (setq xfer-type
		  (efs-xfer-type f-host-type filename t-host-type newname)))
	(let ((size (and (or (null f-host-type)
			     (efs-file-entry-p filename))
			 (nth 7 (file-attributes filename)))))
	  ;; -1 is a bogus size for remote files.
	  (if (eq size -1) (setq size nil))
	  (efs-send-cmd
	   f-host f-user
	   (list 'get
		 f-path
		 (if t-host
		     t-path
		   (or gatename newname))
		 xfer-type)
	   (cond ((eq msg 0)
		  (concat "Copying "
			  (efs-relativize-filename filename)
			  " to "
			  (efs-relativize-filename
			   newname (file-name-directory filename) filename)))
		 ((eq msg 2)
		  (concat "Getting " (efs-relativize-filename filename)))
		 ((eq msg 1)
		  (concat "Getting " (efs-relativize-filename newname)))
		 (t msg))
	   ;; If the server emits a efs-xfer-size-msgs, it will over-ride this.
	   ;; With no xfer msg, this is will do the job.
	   (and size (list 'efs-set-xfer-size f-host f-user size))
	   (efs-cont (result line cont-lines) (filename newname size
							t-host-type t-user
							cont keep-date)
	     (if result
		 (if cont
		     (efs-call-cont cont result line cont-lines)
		   (signal 'ftp-error
			   (list "Opening input file"
				 (format "FTP Error: \"%s\" " line) filename)))
	       ;; Add a new file entry, if relevant.
	       (if t-host-type
		   ;; t-host will be equal to efs-gateway-host, if t-host-type
		   ;; is non-nil.
		   (efs-copy-add-file-entry newname t-host-type
					    t-user size nil))
	       (if (and (null t-host-type) (stringp keep-date))
		   (efs-set-mdtm-of
		    filename newname
		    (and cont
			 (efs-cont (result1 line1 cont-lines1) (result
								line cont-lines
								cont)
			   (efs-call-cont cont result line cont-lines))))
		 (if cont
		     (efs-call-cont cont result line cont-lines)))))
	   nowait)))

       ;; Can we do a EXEC cp?
       ((and t-host f-host
	     (string-equal (downcase t-host) (downcase f-host))
	     (if (memq t-host-type efs-case-insensitive-host-types)
		 (string-equal (downcase t-user) (downcase f-user))
	       (string-equal t-user f-user)))
	(efs-copy-on-remote
	 t-host t-user t-host-type filename newname filename-parsed
	 newname-parsed keep-date append-p
	 (cond ((eq msg 0)
		(concat "Copying "
			(efs-relativize-filename filename)
			" to "
			(efs-relativize-filename
			 newname (file-name-directory filename) filename)))
	       ((eq msg 1)
		(concat "Copying " (efs-relativize-filename newname)))
	       ((eq msg 2)
		(concat "Copying " (efs-relativize-filename filename)))
	       (t msg))
	 cont nowait xfer-type))

       ;; Try for a copy with PASV
       ((and t-host f-host
	     (not (and (string-equal (downcase t-host) (downcase f-host))
		       (if (memq t-host-type efs-case-insensitive-host-types)
			   (string-equal (downcase t-user) (downcase f-user))
			 (string-equal t-user f-user))))
	     (or
	      (and efs-gateway-host
		   ;; The gateway should be able to talk to anything.
		   (let ((gh (downcase efs-gateway-host)))
		     (or (string-equal (downcase t-host) gh)
			 (string-equal (downcase f-host) gh))))
	      (efs-save-match-data
		(eq (null (string-match efs-local-host-regexp t-host))
		    (null (string-match efs-local-host-regexp f-host))))))
	(efs-copy-remote-to-remote
	 f-host-type f-host f-user f-path filename
	 t-host-type t-host t-user t-path newname
	 append-p
	 (cond ((eq msg 0)
		(concat "Copying "
			(efs-relativize-filename filename)
			" to "
			(efs-relativize-filename
			 newname (file-name-directory filename) filename)))
	       ((eq msg 1)
		(concat "Copying " (efs-relativize-filename newname)))
	       ((eq msg 2)
		(concat "Copying " (efs-relativize-filename filename)))
	       (t msg))
	 (efs-cont (result line cont-lines)
	     (filename filename-parsed newname newname-parsed
		       append-p keep-date msg cont nowait xfer-type)
	   (if result
	       ;; PASV didn't work. Do things the old-fashioned
	       ;; way.
	       (efs-copy-via-temp
		filename filename-parsed newname newname-parsed
		append-p keep-date msg cont nowait xfer-type)
	     (if cont
		 (efs-call-cont cont result line cont-lines))))
	 nowait xfer-type))
       
       ;; Can't do anything direct. Divide and conquer.
       (t
	(efs-copy-via-temp filename filename-parsed newname newname-parsed
			   append-p keep-date msg cont nowait xfer-type))))))

(defun efs-copy-file (filename newname &optional ok-if-already-exists
			       keep-date nowait)
  ;; Version of copy file for remote files. Actually, will also work
  ;; for local files too, since efs-copy-file-internal can copy anything.
  ;; If called interactively, copies asynchronously.
  (setq filename (expand-file-name filename)
	newname (expand-file-name newname))
  (if (eq ok-if-already-exists 'append)
      (setq ok-if-already-exists t))
  (efs-copy-file-internal filename (efs-ftp-path filename)
			  newname (efs-ftp-path newname)
			  ok-if-already-exists keep-date 0 nil nowait))

;;;; ------------------------------------------------------------
;;;; File renaming support.
;;;; ------------------------------------------------------------

(defun efs-rename-get-file-list (dir ent)
  ;; From hashtable ENT for DIR returns a list of all files except "."
  ;; and "..".
  (let (list)
    (efs-map-hashtable
     (function
      (lambda (key val)
	(or (string-equal "." key) (string-equal ".." key)
	    (setq list
		  (cons (expand-file-name key dir) list)))))
     ent)
    list))

(defun efs-rename-get-files (dir cont nowait)
  ;; Obtains a list of files in directory DIR (except . and ..), and applies
  ;; CONT to the list. Doesn't return anything useful.
  (let* ((dir (file-name-as-directory dir))
	 (ent (efs-get-files-hashtable-entry dir)))
    (if ent
	(efs-call-cont cont (efs-rename-get-file-list dir ent))
      (efs-ls
       dir (efs-ls-guess-switches) t nil t nowait
       (efs-cont (listing) (dir cont)
	 (efs-call-cont
	  cont (and listing
		    (efs-rename-get-file-list
		     dir (efs-get-files-hashtable-entry dir)))))))))

(defun efs-rename-get-local-file-tree (dir)
  ;; Returns a list of the full directory tree under DIR, for DIR on the
  ;; local host.  The list is in tree order.
  (let ((res (list dir)))
    (mapcar
     (function
      (lambda (file)
	(if (file-directory-p file)
	    (nconc res (delq nil (mapcar
				  (function
				   (lambda (f)
				     (and (not (string-equal "." f))
					  (not (string-equal ".." f))
					  (expand-file-name f file))))
				  (directory-files file)))))))
     res)
    res))

(defun efs-rename-get-remote-file-tree (next curr total cont nowait)
  ;; Builds a hierarchy of files.
  ;; NEXT is the next level so far.
  ;; CURR are unprocessed files in the current level.
  ;; TOTAL is the processed files so far.
  ;; CONT is a cont. function called on the total list after all files
  ;;      are processed.
  ;; NOWAIT non-nil means run asynch.
  (or curr (setq curr next
		 next nil))
  (if curr
      (let ((file (car curr)))
	(setq curr (cdr curr)
	      total (cons file total))
	(if (file-directory-p file)
	    (efs-rename-get-files
	     file
	     (efs-cont (list) (next curr total cont nowait)
	       (efs-rename-get-remote-file-tree (nconc next list)
					   curr total cont nowait))
	     nowait)
	  (efs-rename-get-remote-file-tree next curr total cont nowait)))
    (efs-call-cont cont (nreverse total))))

(defun efs-rename-make-targets (files from-dir-len to-dir host user host-type
				      cont nowait)
  ;; Make targets (copy a file or make a subdir) on local or host
  ;; for the files in list. Afterwhich, call CONT.
  (if files
      (let* ((from (car files))
	     (files (cdr files))
	     (to (concat to-dir (substring from from-dir-len))))
	(if (file-directory-p from)
	    (if host-type
		(let ((dir (nth 2 (efs-ftp-path to))))
		  (or (memq host-type efs-unix-host-types)
		      (memq host-type '(dos os2))
		      (setq dir (efs-internal-file-name-as-directory nil dir)))
		  (efs-send-cmd
		   host user (list 'mkdir dir)
		   (format "Making directory %s" (efs-relativize-filename to))
		   nil
		   (efs-cont (res line cont-lines) (to files from-dir-len
						       to-dir host user
						       host-type cont nowait)
		     (if res
			 (if cont
			     (efs-call-cont cont res line cont-lines)
			   (signal 'ftp-error
				   (list "Making directory"
					 (format "FTP Error: \"%s\"" line)
					 to)))
		       (efs-rename-make-targets
			files from-dir-len to-dir host user
			host-type cont nowait)))
		   nowait))
	      (condition-case nil
		  (make-directory-internal to)
		(error (efs-call-cont
			cont 'failed (format "Failed to mkdir %s" to) "")))
	      (efs-rename-make-targets
	       files from-dir-len to-dir host user host-type cont nowait))
	  (efs-copy-file-internal
	   from (efs-ftp-path from) to (and host-type (efs-ftp-path to)) nil t
	   (format "Renaming %s to %s" (efs-relativize-filename from)
		   (efs-relativize-filename to))
	   (efs-cont (res line cont-lines) (from to files from-dir-len to-dir
						 host user host-type cont
						 nowait)
	     (if res
		 (if cont
		     (efs-call-cont cont res line cont-lines)
		   (signal 'ftp-error
			   (list "Renaming"
				 (format "FTP Error: \"%s\"" line) from to)))
	       (efs-rename-make-targets
		files from-dir-len to-dir host user host-type cont nowait)))
	   nowait)))
    (if cont (efs-call-cont cont nil "" ""))))

(defun efs-rename-delete-on-local (files)
  ;; Delete the files FILES, and then run CONT.
  ;; FILES are assumed to be in inverse tree order.
  (message "Deleting files...")
  (mapcar
   (function
    (lambda (f)
      (condition-case nil
	  (if (file-directory-p f)
	      (delete-directory f)
	    (delete-file f))
	(file-error nil)))) ; don't complain if the file is already gone.
   files)
  (message "Deleting files...done"))

(defun efs-rename-delete-on-remote (files host user host-type cont nowait)
  ;; Deletes the list FILES on a remote host.  When done calls CONT.
  ;; FILES is assumed to be in reverse tree order.
  (if files
      (let* ((f (car files))
	     (rf (nth 2 (efs-ftp-path f))))
	(progn
	  (setq files (cdr files))
	  (if (file-directory-p f)
	      (let ((rf (if (memq host-type (append efs-unix-host-types
						    '(dos os2)))
			    (efs-internal-directory-file-name f)
			  (efs-internal-file-name-as-directory nil f))))
		
		(efs-send-cmd
		 host user (list 'rmdir rf)
		 (concat "Deleting directory " (efs-relativize-filename f))
		 nil
		 (efs-cont (res line cont-lines) (f files host user host-type
						    cont nowait)
		   (if (and res
			    (efs-save-match-data
			      (not (string-match "^550 " line))))
		       (if cont
			   (efs-call-cont cont res line cont-lines)
			 (signal 'ftp-error
				 (list "Deleting directory"
				       (format "FTP Error: \"%s\"" line)
				       f)))
		     (efs-rename-delete-on-remote
		      files host user host-type cont nowait)))
		 nowait))
	    (efs-send-cmd
	     host user (list 'delete rf)
	     (concat "Deleting " rf)
	     nil
	     (efs-cont (res line cont-lines) (f files host user host-type
						cont nowait)
	       (if (and res
			(efs-save-match-data
			  (not (string-match "^550 " line))))
		   (if cont
		       (efs-call-cont cont res line cont-lines)
		     (signal 'ftp-error
			     (list "Deleting"
				   (format "FTP Error: \"%s\"" line)
				   f)))
		 (efs-rename-delete-on-remote
		  files host user host-type cont nowait)))
	     nowait))))
    (if cont (efs-call-cont cont nil "" ""))))

(defun efs-rename-on-remote (host user old-path new-path old-file new-file
				  msg nowait cont)
  ;; Run a rename command on the remote server.
  ;; OLD-PATH and NEW-PATH are in full efs syntax.
  ;; OLD-FILE and NEW-FILE are the remote full pathnames, not in efs syntax.
  (efs-send-cmd
   host user (list 'rename old-file new-file) msg nil
   (efs-cont (result line cont-lines) (cont old-path new-path host)
     (if result
	 (progn
	   (or (and (>= (length line) 4)
		    (string-equal "550 " (substring line 0 4)))
	       (efs-set-host-property host 'rnfr-failed t))
	   (if cont
	       (efs-call-cont cont result line cont-lines)
	     (signal 'ftp-error
		     (list "Renaming"
			   (format "FTP Error: \"%s\"" line)
			   old-path new-path))))
       (let ((entry (efs-get-file-entry old-path))
	     (host-type (efs-host-type host))
	     ;; If no file entry, do extra work on the hashtable,
	     ;; rather than force a listing.
	     (dir-p (or (not (efs-file-entry-p old-path))
			(file-directory-p old-path))))
	 (apply 'efs-add-file-entry host-type new-path
		(eq (car entry) t) (cdr entry))
	 (efs-delete-file-entry host-type old-path)
	 (if dir-p
	     (let* ((old (efs-canonize-file-name
			  (file-name-as-directory old-path)))
		    (new (efs-canonize-file-name
			  (file-name-as-directory new-path)))
		    (old-len (length old))
		    (new-tbl (efs-make-hashtable
			      (length efs-files-hashtable))))
	       (efs-map-hashtable
		(function
		 (lambda (key val)
		   (if (and (>= (length key) old-len)
			    (string-equal (substring key 0 old-len)
					  old))
		       (efs-put-hash-entry
			(concat new (substring key old-len)) val new-tbl)
		     (efs-put-hash-entry key val new-tbl))))
		efs-files-hashtable)
	       (setq efs-files-hashtable new-tbl)))
	 (if cont (efs-call-cont cont result line cont-lines)))))
   nowait))

(defun efs-rename-local-to-remote (filename newname newname-parsed
					    msg cont nowait)
  ;; Renames a file from the local host to a remote host.
  (if (file-directory-p filename)
      (let* ((files (efs-rename-get-local-file-tree filename))
	     (to-dir (directory-file-name newname))
	     (filename (directory-file-name filename))
	     (len (length filename))
	     (t-parsed (efs-ftp-path to-dir))
	     (host (car t-parsed))
	     (user (nth 1 t-parsed))
	     (host-type (efs-host-type host)))
	;; MSG is never passed here, instead messages are constructed
	;; internally.  I don't know how to use a single message
	;; in a function which sends so many FTP commands.
	(efs-rename-make-targets
	 files len to-dir host user host-type
	 (efs-cont (result line cont-lines) (files filename newname cont)
	   (if result
	       (if cont
		   (efs-call-cont cont result line cont-lines)
		 (signal 'ftp-error
			 (list "Renaming" (format "FTP Error: \"%s\"" line)
			       filename newname)))
	     (efs-rename-delete-on-local (nreverse files))
	     (if cont (efs-call-cont cont result line cont-lines))))
	 nowait))
    (efs-copy-file-internal
     filename nil newname newname-parsed t t msg
     (efs-cont (result line cont-lines) (filename cont)
       (if result
	   (if cont
	       (efs-call-cont cont result line cont-lines)
	     (signal 'ftp-error
		     (list "Renaming"
			   (format "FTP Error: \"%s\"" line)
			   filename newname)))
	 (condition-case nil
	     (delete-file filename)
	   (error nil))
	 (if cont (efs-call-cont cont result line cont-lines))))
     nowait)))

(defun efs-rename-from-remote (filename filename-parsed newname newname-parsed
					msg cont nowait)
  (let ((f-host (car filename-parsed))
	(f-user (nth 1 filename-parsed))
	(fast-nowait (if (eq nowait t) 1 nowait)))
    (if (file-directory-p filename)
	(let* ((t-host (car newname-parsed))
	       (t-user (nth 1 newname-parsed))
	       (t-host-type (and t-host (efs-host-type t-host)))
	       (f-host-type (efs-host-type f-host)))
	  (efs-rename-get-remote-file-tree
	   nil (list filename) nil
	   (efs-cont (list) (filename filename-parsed newname t-host t-user
				      t-host-type f-host f-user f-host-type
				      cont fast-nowait)
	     (efs-rename-make-targets
	      list (length filename) newname t-host t-user t-host-type
	      (efs-cont (res line cont-lines) (filename newname f-host f-user
							f-host-type list cont
							fast-nowait)
		(if res
		    (if cont
			(efs-call-cont cont res line cont-lines)
		      (signal 'ftp-error
			      (list "Renaming"
				    (format "FTP Error: \"%s\"" line)
				    filename newname)))
		  (efs-rename-delete-on-remote
		   (nreverse list) f-host f-user f-host-type cont
		   fast-nowait)))
	      fast-nowait)) nowait))
      ;; Do things the simple way.
      (let ((f-path (nth 2 filename-parsed))
	    (f-abbr (efs-relativize-filename filename)))
	(efs-copy-file-internal
	 filename filename-parsed newname newname-parsed t t msg
	 (efs-cont (result line cont-lines) (filename newname f-host f-user
						      f-path f-abbr
						      cont fast-nowait)
	   (if result
	       (if cont
		   (efs-call-cont cont result line cont-lines)
		 (signal 'ftp-error
			 (list "Renaming"
			       (format "FTP Error: \"%s\"" line)
			       filename newname)))
	     (efs-send-cmd
	      f-host f-user (list 'delete f-path)
	      (format "Removing %s" f-abbr) nil
	      (efs-cont (result line cont-lines) (filename f-host cont)
		(if result
		    (if cont
			(efs-call-cont cont result line cont-lines)
		      (signal 'ftp-error
			      (list "Renaming"
				    (format "Failed to remove %s"
					    filename)
				    "FTP Error: \"%s\"" line)))
		  (efs-delete-file-entry (efs-host-type f-host)
					 filename)
		  (if cont
		      (efs-call-cont cont result line cont-lines))))
	      fast-nowait))) nowait)))))

(defun efs-rename-file-internal (filename newname ok-if-already-exists
					  &optional msg cont nowait)
  ;; Internal version of rename-file for remote files.
  ;; Takes CONT and NOWAIT args.
  (let ((filename (expand-file-name filename))
	(newname (expand-file-name newname)))
    (let ((f-parsed (efs-ftp-path filename))
	  (t-parsed (efs-ftp-path newname)))
      (if (null (or f-parsed t-parsed))
	  (progn
	    ;; local rename
	    (rename-file filename newname ok-if-already-exists)
	    (if cont
		(efs-call-cont cont nil "Renamed locally" "")))

	;; check to see if we can overwrite
	(if (or (not ok-if-already-exists)
		(numberp ok-if-already-exists))
	    (efs-barf-or-query-if-file-exists
	     newname "rename to it" (numberp ok-if-already-exists)))

	(let ((f-abbr (efs-relativize-filename filename))
	      (t-abbr (efs-relativize-filename newname
					       (file-name-directory filename)
					       filename)))
	  (or msg (setq msg (format "Renaming %s to %s" f-abbr t-abbr)))
	  (if f-parsed
	      (let* ((f-host (car f-parsed))
		     (f-user (nth 1 f-parsed))
		     (f-path (nth 2 f-parsed))
		     (f-host-type (efs-host-type f-host)))
		(if (and t-parsed
			 (string-equal (downcase f-host)
				       (downcase (car t-parsed)))
			 (not (efs-get-host-property f-host 'rnfr-failed))
			 (if (memq f-host-type efs-case-insensitive-host-types)
			     (string-equal (downcase f-user)
					   (downcase (nth 1 t-parsed)))
			   (string-equal f-user (nth 1 t-parsed))))
		    ;; Can run a RENAME command on the server.
		    (efs-rename-on-remote
		     f-host f-user filename newname f-path (nth 2 t-parsed)
		     msg nowait
		     (efs-cont (result line cont-lines) (f-host
							 filename
							 newname
							 ok-if-already-exists
							 msg cont nowait)
		       (if result
			   (progn
			     (efs-set-host-property f-host 'rnfr-failed t)
			     (efs-rename-file-internal
			      filename newname ok-if-already-exists msg cont
			      (if (eq nowait t) 1 nowait)))
			 (if cont
			     (efs-call-cont cont result line cont-lines)))))
		  ;; remote to remote
		  (efs-rename-from-remote filename f-parsed newname t-parsed
					  msg cont nowait)))
	    ;; local to remote
	    (efs-rename-local-to-remote 
	     filename newname t-parsed msg cont nowait)))))))

(defun efs-rename-file (filename newname &optional ok-if-already-exists nowait)
  ;; Does file renaming for remote files.
  (efs-rename-file-internal filename newname ok-if-already-exists
			    nil nil nowait))

;;;; ------------------------------------------------------------
;;;; Making symbolic and hard links.
;;;; ------------------------------------------------------------

;;;  These functions require that the remote FTP server understand
;;;  SITE EXEC and that ln is in its the ftp-exec path.

(defun efs-try-ln (host user cont nowait)
  ;; Do some preemptive testing to see if exec ln works
  (if (efs-get-host-property host 'exec-failed)
      (signal 'ftp-error (list "Unable to exec ln on host" host)))
  (let ((exec-ln (efs-get-host-property host 'exec-ln)))
    (cond
     ((eq exec-ln 'failed)
      (signal 'ftp-error (list "ln is not in FTP exec path on host" host)))
     ((eq exec-ln 'works)
      (efs-call-cont cont))
     (t
      (message "Checking for ln executable on %s..." host)
      (efs-send-cmd
       host user '(quote site exec "ln / /")
       nil nil
       (efs-cont (result line cont-lines) (host user cont)
	 (if result
	     (progn
	       (efs-set-host-property host 'exec-failed t)
	       (efs-error host user (format "exec: %s" line)))
	   ;; Look for an error message
	   (if (efs-save-match-data
		 (string-match "\n200-" cont-lines))
	       (progn
		 (efs-set-host-property host 'exec-ln 'works)
		 (efs-call-cont cont))
	     (efs-set-host-property host 'exec-ln 'failed)
	     (efs-error host user
			(format "ln not in FTP exec path on host %s" host)))))
       nowait)))))

(defun efs-make-symbolic-link-internal
  (target linkname &optional ok-if-already-exists cont nowait)
  ;; Makes remote symbolic links. Assumes that linkname is already expanded.
  (let* ((parsed (efs-ftp-path linkname))
	 (host (car parsed))
	 (user (nth 1 parsed))
	 (linkpath (nth 2 parsed))
	 (abbr (efs-relativize-filename linkname
					(file-name-directory target) target))
	 (tparsed (efs-ftp-path target))
	 (com-target target)
	 cmd-string)
    (if (null (file-directory-p
	       (file-name-directory linkname)))
	(if cont
	    (efs-call-cont cont 'failed
			   (format "no such file or directory, %s" linkname)
			   "")
	  (signal 'file-error (list "no such file or directory" linkname)))
      (if (or (not ok-if-already-exists)
	      (numberp ok-if-already-exists))
	  (efs-barf-or-query-if-file-exists
	   linkname "make symbolic link" (numberp ok-if-already-exists)))
      ;; Do this after above, so that hopefully the host type is sorted out
      ;; by now.
      (let ((host-type (efs-host-type host)))
	(if (or (not (memq host-type efs-unix-host-types))
		(memq host-type efs-dumb-host-types)
		(efs-get-host-property host 'exec-failed))
	    (error "Unable to make symbolic links on %s." host)))
      ;; Be careful not to spoil relative links, or symlinks to other
      ;; machines, which maybe symlink-fix.el can sort out.
      (if (and tparsed
	       (string-equal (downcase (car tparsed)) (downcase host))
	       (string-equal (nth 1 tparsed) user))
	  (setq com-target (nth 2 tparsed)))
      ;; symlinks only work for unix, so don't need to
      ;; convert pathnames. What about VOS?
      (setq cmd-string (concat "ln -sf "  com-target " " linkpath))
      (efs-try-ln
       host user
       (efs-cont () (host user cmd-string target linkname com-target
			  abbr cont nowait)
	 (efs-send-cmd
	  host user (list 'quote 'site 'exec cmd-string)
	  (format "Symlinking %s to %s" target abbr)
	  nil
	  (efs-cont (result line cont-lines) (host user com-target linkname
						   cont)
	    (if result
		(progn
		  (efs-set-host-property host 'exec-failed t)
		  (efs-error host user (format "exec: %s" line)))
	      (efs-save-match-data
		(if (string-match "\n200-\\([^\n]*\\)" cont-lines)
		    (let ((err (substring cont-lines (match-beginning 1)
					  (match-end 1))))
		      (if cont
			  (efs-call-cont cont 'failed err cont-lines)
			(efs-error host user err)))
		  (efs-add-file-entry nil linkname com-target nil user)
		  (if cont (efs-call-cont cont nil line cont-lines))))))
	  nowait))
       nowait))))

(defun efs-make-symbolic-link (target linkname &optional ok-if-already-exists)
  ;; efs version of make-symbolic-link
  (let* ((linkname (expand-file-name linkname))
	 (parsed (efs-ftp-path linkname)))
    (if parsed
	(efs-make-symbolic-link-internal target linkname ok-if-already-exists)
      ;; Handler will match on either target or linkname. We are only
      ;; interested in the linkname.
      (let ((file-name-handler-alist (efs-file-name-handler-alist-sans-fn
				      'efs-file-handler-function)))
	(make-symbolic-link target linkname ok-if-already-exists)))))

(defun efs-add-name-to-file-internal
  (file newname &optional ok-if-already-exists cont nowait)
  ;; Makes remote symbolic links. Assumes that linkname is already expanded.
  (let* ((parsed (efs-ftp-path file))
	 (host (car parsed))
	 (user (nth 1 parsed))
	 (path (nth 2 parsed))
	 (nparsed (efs-ftp-path newname))
	 (nhost (car nparsed))
	 (nuser (nth 1 nparsed))
	 (npath (nth 2 nparsed))
	 (abbr (efs-relativize-filename newname
					(file-name-directory file)))
	 (ent (efs-get-file-entry file))
	 cmd-string)
    (or (and (string-equal (downcase host) (downcase nhost))
	     (string-equal user nuser))
	(error "Cannot create hard links between different host user pairs."))
    (if (or (null ent) (stringp (car ent))
	    (not (file-directory-p
		  (file-name-directory newname))))
	(if cont
	    (efs-call-cont cont 'failed
			   (format "no such file or directory, %s %s"
				   file newname) "")
	  (signal 'file-error
		  (list "no such file or directory"
			file newname)))
      (if (or (not ok-if-already-exists)
	      (numberp ok-if-already-exists))
	  (efs-barf-or-query-if-file-exists
	   newname "make hard link" (numberp ok-if-already-exists)))
      ;; Do this last, so that hopefully the host type is known.
      (let ((host-type (efs-host-type host)))
	(if (or (not (memq host-type efs-unix-host-types))
		(memq host-type efs-dumb-host-types)
		(efs-get-host-property host 'exec-failed))
	    (error "Unable to make hard links on %s." host)))
      (setq cmd-string (concat "ln -f "  path " " npath))
      (efs-try-ln
       host user
       (efs-cont () (host user cmd-string file newname abbr cont nowait)
	 (efs-send-cmd
	  host user (list 'quote 'site 'exec cmd-string)
	  (format "Adding to %s name %s" file abbr)
	  nil
	  (efs-cont (result line cont-lines) (host user file newname cont)
	    (if result
		(progn
		  (efs-set-host-property host 'exec-failed t)
		  (efs-error host user (format "exec: %s" line)))
	      (efs-save-match-data
		(if (string-match "\n200-\\([^\n]*\\)" cont-lines)
		    (let ((err (substring cont-lines (match-beginning 1)
					  (match-end 1))))
		      (if cont
			  (efs-call-cont cont 'failed err cont-lines)
			(efs-error host user err)))
		  (let ((ent (efs-get-file-entry file)))
		    (if ent
			(let ((nlinks (nthcdr 4 ent))
			      new-nlinks)
			  (and (integerp (car nlinks))
			       (setq new-nlinks (1+ (car nlinks)))
			       (setcar nlinks new-nlinks))
			  (apply 'efs-add-file-entry nil newname ent)
			  (if cont (efs-call-cont cont nil line cont-lines)))
		      (let ((tbl (efs-get-files-hashtable-entry
				  (file-name-directory
				   (directory-file-name newname)))))
			(if tbl
			    (efs-ls
			     newname
			     (concat (efs-ls-guess-switches) "d") t t nil
			     nowait
			     (efs-cont (listing) (newname cont line cont-lines)
			       (efs-update-file-info
				newname efs-data-buffer-name)
			       (if cont
				   (efs-call-cont cont nil line cont-lines))))
			  (if cont
			      (efs-call-cont cont nil line cont-lines))))))))))
	  nowait))
       nowait))))

(defun efs-add-name-to-file (file newname &optional ok-if-already-exists)
  ;; efs version of add-name-to-file
  (efs-add-name-to-file-internal file newname ok-if-already-exists))


;;;; ==============================================================
;;;; >9
;;;; Multiple Host Type Support.
;;;; The initial host type guessing is done in the PWD code below.
;;;; If necessary, further guessing is done in the listing parser.
;;;; ==============================================================


;;;; --------------------------------------------------------------
;;;; Functions for setting and retrieving host types.
;;;; --------------------------------------------------------------

(defun efs-add-host (type host)
  "Sets the TYPE of the remote host HOST.
The host type is read with completion so this can be used to obtain a
list of supported host types. HOST must be a string, giving the name of
the host, exactly as given in file names. Setting the host type with
this function is preferable to setting the efs-TYPE-host-regexp, as look up
will be faster. Returns TYPE."
  ;; Since internet host names are always case-insensitive, we will cache
  ;; them in lower case.
  (interactive
   (list
    (intern
     (completing-read "Host type: "
		      (mapcar
		       (function (lambda (elt)
				   (list (symbol-name (car elt)))))
		       efs-host-type-alist)
		      nil t))
    (read-string "Host: "
		 (let ((name (or (buffer-file-name)
				 (and (eq major-mode 'dired-mode)
				      dired-directory))))
		   (and name (car (efs-ftp-path name)))))))
  (setq host (downcase host))
  (efs-set-host-property host 'host-type type)
  (prog1
      (setq efs-host-cache host
	    efs-host-type-cache type)
    (efs-set-process-host-type host)))

(defun efs-set-process-host-type (host &optional user)
  ;; Sets the value of efs-process-host-type so that it is shown
  ;; on the mode-line.
  (let ((buff-list (buffer-list)))
    (save-excursion
      (while buff-list
	(set-buffer (car buff-list))
	(if (equal efs-process-host host)
	    (setq efs-process-host-type (concat " " (symbol-name
						     (efs-host-type host))))
	  (and efs-show-host-type-in-dired
	       (eq major-mode 'dired-mode)
	       efs-dired-host-type
	       (string-equal (downcase
			      (car (efs-ftp-path default-directory)))
			     (downcase host))
	       (if user
		   (setq efs-dired-listing-type-string
			 (concat
			  " "
			  (symbol-name (efs-listing-type host user))))
		 (or efs-dired-listing-type-string
		     (setq efs-dired-listing-type-string
			   (concat " " (symbol-name (efs-host-type host))))))))
	(setq buff-list (cdr buff-list))))))

;;;; ----------------------------------------------------------------
;;;; Functions for setting and retrieving listings types.
;;;; ----------------------------------------------------------------

;;;  listing types??
;;;  These are distinguished from host types, in case some OS's have two
;;;  breeds of listings. e.g. Unix descriptive listings.
;;;  We also use this to support the myriad of DOS ftp servers.


(defun efs-listing-type (host user)
  "Returns the type of listing used on HOST by USER.
If there is no entry for a specialized listing, returns the host type."
  (or
   (efs-get-host-user-property host user 'listing-type)
   (efs-host-type host user)))

(defun efs-add-listing-type (type host user)
  "Interactively adds the specialized listing type TYPE for HOST and USER
to the listing type cache."
  (interactive
   (let ((name (or (buffer-file-name)
		   (and (eq major-mode 'dired-mode)
			dired-directory))))
     (list
      (intern
       (completing-read "Listing type: "
			(mapcar
			 (function (lambda (elt)
				     (list (symbol-name elt))))
			 efs-listing-types)
			nil t))
      (read-string "Host: "
		   (and name (car (efs-ftp-path name))))
      (read-string "User: "
		   (and name (nth 1 (efs-ftp-path name)))))))
  (efs-set-host-user-property host user 'listing-type type)
  (efs-set-process-host-type host user))

;;;; --------------------------------------------------------------
;;;; Auotomagic bug reporting for unrecognized host types.
;;;; --------------------------------------------------------------

(defun efs-scream-and-yell-1 (host user)
  ;; Internal for efs-scream-and-yell.
  (with-output-to-temp-buffer "*Help*"
    (princ
     (format
      "efs is unable to identify the remote host type of %s.

Please report this as a bug. It would be very helpful
if your bug report contained at least the PWD command
within the *ftp %s@%s* buffer.
If you know them, also send the operating system 
and ftp server types of the remote host." host user host)))
  (if (y-or-n-p "Would you like to submit a bug report now? ")
      (efs-report-bug host user
		      "Bug occurred during efs-guess-host-type." t)))

(defun efs-scream-and-yell (host user)
  ;; Advertises that something has gone wrong in identifying the host type.
  (if (eq (selected-window) (minibuffer-window))
      (efs-abort-recursive-edit-and-then 'efs-scream-and-yell-1 host user)
    (efs-scream-and-yell-1 host user)
    (error "Unable to identify remote host type")))

;;;; --------------------------------------------------------
;;;; Guess at the host type using PWD syntax.
;;;; --------------------------------------------------------

;; host-type path templates. These should match a pwd performed
;; as the first command after connecting. They should be as tight
;; as possible

(defconst efs-unix-path-template "^/")
(defconst efs-apollo-unix-path-template "^//")
(defconst efs-cms-path-template
  (concat
   "^[-A-Z0-9$*][-A-Z0-9$*]?[-A-Z0-9$*]?[-A-Z0-9$*]?[-A-Z0-9$*]?"
   "[-A-Z0-9$*]?[-A-Z0-9$*]?[-A-Z0-9$*]?\\.[0-9][0-9][0-9A-Z]$\\|"
   ;; For the SFS version of CMS
   "^[-A-Z0-9]+:[-A-Z0-9$*]+\\.$"))
   
(defconst efs-mvs-path-template "^'?[A-Z][0-9][0-9]?[0-9]?[0-9]?[0-9]?\\.'?")

(defconst efs-guardian-path-template
  (concat
   "^\\("
   "\\\\[A-Z0-9][A-Z0-9]?[A-Z0-9]?[A-Z0-9]?[A-Z0-9]?[A-Z0-9]?[A-Z0-9]?\\."
   "\\)?"
   "\\$[A-Z0-9][A-Z0-9]?[A-Z0-9]?[A-Z0-9]?[A-Z0-9]?[A-Z0-9]?[A-Z0-9]?\\."
   "[A-Z][A-Z0-9]?[A-Z0-9]?[A-Z0-9]?[A-Z0-9]?[A-Z0-9]?[A-Z0-9]?$"))
;; guardian and cms are very close to overlapping (they don't). Be careful.
(defconst efs-vms-path-template
  "^[-A-Z0-9_$]+:\\[[-A-Z0-9_$]+\\(\\.[-A-Z0-9_$]+\\)*\\]$")
(defconst efs-mts-path-template
  "^[A-Z0-9._][A-Z0-9._][A-Z0-9._][A-Z0-9._]:$")
(defconst efs-ms-unix-path-template "^[A-Za-z0-9]:/")

;; Following two are for TI lisp machines. Note that lisp machines
;; do not have a default directory, but only a default pathname against
;; which relative pathnames are merged (Jamie tells me).
(defconst efs-ti-explorer-pwd-line-template
  (let* ((excluded-chars ":;<>.#\n\r\t\\/a-z ")
	 (token  (concat "[^" excluded-chars "]+")))
    (concat "^250 "
	    token ": "					; host name
	    token "\\(\\." token "\\)*; "		; directory
	    "\\(\\*.\\*\\|\\*\\)#\\(\\*\\|>\\)"	; name, ext, version
	    "$")))	; "*.*#*" or "*.*#>" or "*#*" or "*#>" or "#*" ...
(defconst efs-ti-twenex-path-template
  (let* ((excluded-chars ":;<>.#\n\r\t\\/a-z ")
	 (token  (concat "[^" excluded-chars "]+")))
    (concat "^"
	    token ":"					; host name
	    "<\\(" token "\\)?\\(\\." token "\\)*>"	; directory
	    "\\(\\*.\\*\\|\\*\\)"			; name and extension
	    "$")))

(defconst efs-tops-20-path-template
  "^[-A-Z0-9_$]+:<[-A-Z0-9_$]\\(.[-A-Z0-9_$]+\\)*>$")
(defconst efs-pc-path-template
  "^[a-zA-Z0-9]:\\\\\\([-_+=a-zA-Z0-9.]+\\\\\\)*[-_+=a-zA-Z0-9.]*$")
(defconst efs-mpe-path-template
  (let ((token (concat  "[A-Z][A-Z0-9]?[A-Z0-9]?[A-Z0-9]?[A-Z0-9]?"
			"[A-Z0-9]?[A-Z0-9]?[A-Z0-9]?")))
    (concat
     ;; optional session name
     "^\\(" token "\\)?,"
     ;; username
     token "."
     ;; account
     token ","
     ;; group
     token "$")))
(defconst efs-vos-path-template
  (let ((token "[][@\\^`{}|~\"$+,---./:_a-zA-Z0-9]+"))
    (concat
     "%" token           ; host
     "#" token           ; disk
     "\\(>" token "\\)+" ; directories
     )))
(defconst efs-netware-path-template "^[-A-Z0-9_][-A-Z0-9_/]*:/")
;; Sometimes netware doesn't return a device to a PWD. Then it will be
;; recognized by the listing parser.

(defconst efs-nos-ve-path-template "^:[A-Z0-9]")
;; Matches the path for NOS/VE

(defconst efs-mvs-pwd-line-template
  ;; Not sure how the PWD parser will do with empty strings, so treate
  ;; this as a line regexp.
  "^257 \\([Nn]o prefix defined\\|\"\" is working directory\\)")
(defconst efs-cms-pwd-line-template
  "^450 No current working directory defined$")
(defconst efs-tops-20-pwd-line-template
  "^500 I never heard of the \\(XPWD\\|PWD\\) command\\. Try HELP\\.$")
(defconst efs-dos:ftp-pwd-line-template
  "^250 Current working directory is +")
(defconst efs-coke-pwd-line-template "^257 Current balance \\$[0-9]")

(defconst efs-super-dumb-unix-tilde-regexp
  "^550 /.*: No such file or directory\\.?$")
(defconst efs-cms-knet-tilde-regexp
  "^501 Invalid CMS fileid: ~$")


;; It might be nice to message users about the host type identified,
;; but there is so much other messaging going on, it would not be
;; seen. No point in slowing things down just so users can read
;; a host type message.

(defun efs-guess-host-type (host user)
  "Guess the host type of HOST.
Does a PWD and examines the directory syntax. The PWD is then cached for use
in file name expansion."
  (let ((host-type (efs-host-type host))
	(key (concat host "/" user "/~"))
	syst)
    (efs-save-match-data
      (if (eq host-type 'unknown)
	  ;; Note that efs-host-type returns unknown as the default.
	  ;; Since we don't yet know the host-type, we use the default
	  ;; version of efs-send-pwd. We compensate if necessary
	  ;; by looking at the entire line of output.
	  (let* ((result (efs-send-pwd nil host user))
		 (dir (car result))
		 (line (cdr result)))
	    (cond
	     
	     ;; First sift through process lines to see if we recognize
	     ;; any pwd errors, or full line messages.
	     
	     ;; CMS
	     ((string-match efs-cms-pwd-line-template line)
	      (setq host-type (efs-add-host 'cms host)
		    dir (concat "/" (if (> (length user) 8)
					(substring user 0 8)
				      user)
				".191"))
	      (message
	       "Unable to determine a \"home\" CMS minidisk.  Assuming %s"
	       dir)
	      (sit-for 1))
	     
	     ;; TOPS-20
	     ((string-match efs-tops-20-pwd-line-template line)
	      (setq host-type (efs-add-host 'tops-20 host)
		    dir (car (efs-send-pwd 'tops-20 host user))))
	     
	     ;; TI-EXPLORER lisp machine. pwd works here, but the output
	     ;; needs to be specially parsed since spaces separate
	     ;; hostnames from dirs from filenames.
	     ((string-match efs-ti-explorer-pwd-line-template line)
	      (setq host-type (efs-add-host 'ti-explorer host)
		    dir (substring line 4)))

	     ;; FTP Software's DOS Server
	     ((string-match efs-dos:ftp-pwd-line-template line)
	      (setq host-type (efs-add-host 'dos host)
		    dir (substring line (match-end 0)))
	      (efs-add-listing-type 'dos:ftp host user))

	     ;; MVS
	     ((string-match efs-mvs-pwd-line-template line)
	      (setq host-type (efs-add-host 'mvs host)
		    dir "")) ; "" will convert to /, which is always
			     ; the mvs home dir.

	     ;; COKE
	     ((string-match efs-coke-pwd-line-template line)
	      (setq host-type (efs-add-host 'coke host)
		    dir "/"))
	     
	     ;; Try to get tilde.
	     ((null dir)
	      (let ((tilde (nth 1 (efs-send-cmd
				   host user (list 'get "~" "/dev/null")))))
		(cond
		 ;; super dumb unix
		 ((string-match efs-super-dumb-unix-tilde-regexp tilde)
		  (setq dir (car (efs-send-pwd 'super-dumb-unix host user))
			host-type (efs-add-host 'super-dumb-unix host)))

		 ;; Try for cms-knet
		 ((string-match efs-cms-knet-tilde-regexp tilde)
		  (setq dir (car (efs-send-pwd 'cms-knet host user))
			host-type (efs-add-host 'cms-knet host)))
		 
		 ;; We don't know. Scream and yell.
		 (efs-scream-and-yell host user))))
	     
	     ;; Now look at dir to determine host type
	     
	     ;; try for UN*X-y type stuff
	     ((string-match efs-unix-path-template dir)
	      (if
		  ;; Check for apollo, so we know not to short-circuit //.
		  (string-match efs-apollo-unix-path-template dir)
		  (progn
		    (setq host-type (efs-add-host 'apollo-unix host))
		    (efs-add-listing-type 'unix:unknown host user))
		;; could be ka9q, dos-distinct, plus any of the unix breeds,
		;; except apollo.
		(if (setq syst (efs-get-syst host user))
		    (let ((case-fold-search t))
		      (cond
		       ((string-match "\\bNetware\\b" syst)
			(setq host-type (efs-add-host 'netware host)))
		       ((string-match "^Plan 9" syst)
			(setq host-type (efs-add-host 'plan9 host)))
		       ((string-match "^UNIX" syst)
			(setq host-type (efs-add-host 'unix host))
			(efs-add-listing-type 'unix:unknown host user)))))))
	     
	     ;; try for VMS
	     ((string-match efs-vms-path-template dir)
	      (setq host-type (efs-add-host 'vms host)))
	     
	     ;; try for MTS
	     ((string-match efs-mts-path-template dir)
	      (setq host-type (efs-add-host 'mts host)))
	     
	     ;; try for CMS
	     ((string-match efs-cms-path-template dir)
	      (setq host-type (efs-add-host 'cms host)))

	     ;; try for Tandem's guardian OS
	     ((string-match efs-guardian-path-template dir)
	      (setq host-type (efs-add-host 'guardian host)))
	     
	     ;; Try for TOPS-20. pwd doesn't usually work for tops-20
	     ;; But who knows???
	     ((string-match efs-tops-20-path-template dir)
	      (setq host-type (efs-add-host 'tops-20 host)))
	     
	     ;; Try for DOS or OS/2.
	     ((string-match efs-pc-path-template dir)
	      (let ((syst (efs-get-syst host user))
		    (case-fold-search t))
		(if (and syst (string-match "^OS/2 " syst))
		    (setq host-type (efs-add-host 'os2 host))
		  (setq host-type (efs-add-host 'dos host)))))
	     
	     ;; try for TI-TWENEX lisp machine
	     ((string-match efs-ti-twenex-path-template dir)
	      (setq host-type (efs-add-host 'ti-twenex host)))

	     ;; try for MPE
	     ((string-match efs-mpe-path-template dir)
	      (setq host-type (efs-add-host 'mpe host)))

	     ;; try for VOS
	     ((string-match efs-vos-path-template dir)
	      (setq host-type (efs-add-host 'vos host)))

	     ;; try for the microsoft server in unix mode
	     ((string-match efs-ms-unix-path-template dir)
	      (setq host-type (efs-add-host 'ms-unix host)))

	     ;; Netware?
	     ((string-match efs-netware-path-template dir)
	      (setq host-type (efs-add-host 'netware host)))

	     ;; Try for MVS
	     ((string-match efs-mvs-path-template dir)
	      (if (string-match "^'.+'$" dir)
		  ;; broken MVS PWD quoting
		  (setq dir (substring dir 1 -1)))
	      (setq host-type (efs-add-host 'mvs host)))

	     ;; Try for NOS/VE
	     ((string-match efs-nos-ve-path-template dir)
	      (setq host-type (efs-add-host 'nos-ve host)))
	     
	     ;; We don't know. Scream and yell.
	     (t
	      (efs-scream-and-yell host user)))
	    
	    ;; Now that we have done a pwd, might as well put it in
	    ;; the expand-dir hashtable.
	    (if dir
		(efs-put-hash-entry
		 key
		 (efs-internal-directory-file-name
		  (efs-fix-path host-type dir 'reverse))
		 efs-expand-dir-hashtable
		 (memq host-type efs-case-insensitive-host-types))))

	;; host-type has been identified by regexp, set the mode-line.
	(efs-set-process-host-type host user)
	
	;; Some special cases, where we need to store the cwd on login.
	(if (not (efs-hash-entry-exists-p
		  key efs-expand-dir-hashtable))
	    (cond
	     ;; CMS: We will be doing cd's, so we'd better make sure that
	     ;; we know where home is.
	     ((eq host-type 'cms)
	      (let* ((res (efs-send-pwd 'cms host user))
		     (dir (car res))
		     (line (cdr res)))
		(if (and dir (not (string-match
				   efs-cms-pwd-line-template line)))
		    (setq dir (concat "/" dir))
		  (setq dir (concat "/" (if (> (length user) 8)
					    (substring user 0 8)
					  user)
				    ".191"))
		  (message
		   "Unable to determine a \"home\" CMS minidisk. Assuming %s"
		   dir))
		(efs-put-hash-entry
		 key dir efs-expand-dir-hashtable
		 (memq 'cms efs-case-insensitive-host-types))))
	     ;; MVS: pwd doesn't work in the root directory, so we stuff this
	     ;; into the hashtable manually.
	     ((eq host-type 'mvs)
	      (efs-put-hash-entry key "/" efs-expand-dir-hashtable))
	     ))))))


;;;; -----------------------------------------------------------
;;;; efs-autoloads
;;;; These provide the entry points for the non-unix packages.
;;;; -----------------------------------------------------------

(efs-autoload 'efs-fix-path vms "efs-vms")
(efs-autoload 'efs-fix-path mts "efs-mts")
(efs-autoload 'efs-fix-path cms "efs-cms")
(efs-autoload 'efs-fix-path ti-explorer "efs-ti-explorer")
(efs-autoload 'efs-fix-path ti-twenex "efs-ti-twenex")
(efs-autoload 'efs-fix-path dos "efs-pc")
(efs-autoload 'efs-fix-path mvs "efs-mvs")
(efs-autoload 'efs-fix-path tops-20 "efs-tops-20")
(efs-autoload 'efs-fix-path mpe "efs-mpe")
(efs-autoload 'efs-fix-path os2 "efs-pc")
(efs-autoload 'efs-fix-path vos "efs-vos")
(efs-autoload 'efs-fix-path ms-unix "efs-ms-unix")
(efs-autoload 'efs-fix-path netware "efs-netware")
(efs-autoload 'efs-fix-path cms-knet "efs-cms-knet")
(efs-autoload 'efs-fix-path guardian "efs-guardian")
(efs-autoload 'efs-fix-path nos-ve "efs-nos-ve")

(efs-autoload 'efs-fix-dir-path vms "efs-vms")
(efs-autoload 'efs-fix-dir-path mts "efs-mts")
(efs-autoload 'efs-fix-dir-path cms "efs-cms")
(efs-autoload 'efs-fix-dir-path ti-explorer "efs-ti-explorer")
(efs-autoload 'efs-fix-dir-path ti-twenex "efs-ti-twenex")
(efs-autoload 'efs-fix-dir-path dos "efs-pc")
(efs-autoload 'efs-fix-dir-path mvs "efs-mvs")
(efs-autoload 'efs-fix-dir-path tops-20 "efs-tops-20")
(efs-autoload 'efs-fix-dir-path mpe "efs-mpe")
(efs-autoload 'efs-fix-dir-path os2 "efs-pc")
(efs-autoload 'efs-fix-dir-path vos "efs-vos")
(efs-autoload 'efs-fix-dir-path hell "efs-hell")
(efs-autoload 'efs-fix-dir-path ms-unix "efs-ms-unix")
(efs-autoload 'efs-fix-dir-path netware "efs-netware")
(efs-autoload 'efs-fix-dir-path plan9 "efs-plan9")
(efs-autoload 'efs-fix-dir-path cms-knet "efs-cms-knet")
(efs-autoload 'efs-fix-dir-path guardian "efs-guardian")
(efs-autoload 'efs-fix-dir-path nos-ve "efs-nos-ve")
(efs-autoload 'efs-fix-dir-path coke "efs-coke")

;; A few need to autoload a pwd function
(efs-autoload 'efs-send-pwd tops-20 "efs-tops-20")
(efs-autoload 'efs-send-pwd cms-knet "efs-cms-knet")
(efs-autoload 'efs-send-pwd ti-explorer "efs-ti-explorer")
(efs-autoload 'efs-send-pwd hell "efs-hell")
(efs-autoload 'efs-send-pwd mvs "efs-mvs")
(efs-autoload 'efs-send-pwd coke "efs-coke")

;; A few packages are loaded by the listing parser.
(efs-autoload 'efs-parse-listing ka9q "efs-ka9q")
(efs-autoload 'efs-parse-listing unix:dl "efs-dl")
(efs-autoload 'efs-parse-listing dos-distinct "efs-dos-distinct")
(efs-autoload 'efs-parse-listing hell "efs-hell")
(efs-autoload 'efs-parse-listing netware "efs-netware")

;; Packages that need to autoload for child-lookup
(efs-autoload 'efs-allow-child-lookup plan9 "efs-plan9")
(efs-autoload 'efs-allow-child-lookup coke "efs-coke")

;; Packages that need to autoload for file-exists-p and file-directory-p
(efs-autoload 'efs-internal-file-exists-p guardian "efs-guardian")
(efs-autoload 'efs-internal-file-directory-p guardian "efs-guardian")



;;;; ============================================================
;;;; >10
;;;; Attaching onto the appropriate Emacs version
;;;; ============================================================

;;;; -------------------------------------------------------------------
;;;; Connect to various hooks.
;;;; -------------------------------------------------------------------

(or (memq 'efs-set-buffer-mode find-file-hooks)
    (setq find-file-hooks
	  (cons 'efs-set-buffer-mode find-file-hooks)))

;;; We are using our own dired.el, so this doesn't depend on Emacs flavour.

(if (featurep 'dired)
    (require 'efs-dired)
  (add-hook 'dired-load-hook (function
			      (lambda ()
				(require 'efs-dired)))))

;;;; ------------------------------------------------------------
;;;; Add to minor-mode-alist.
;;;; ------------------------------------------------------------

(or (assq 'efs-process-host-type minor-mode-alist)
    (if (assq 'dired-sort-mode minor-mode-alist)
	(let ((our-list
	       (nconc
		(delq nil
		      (list (assq 'dired-sort-mode minor-mode-alist)
			    (assq 'dired-subdir-omit minor-mode-alist)
			    (assq 'dired-marker-stack minor-mode-alist)))
		(list '(efs-process-host-type efs-process-host-type)
		      '(efs-dired-listing-type
			efs-dired-listing-type-string))))
	      (old-list (delq
			 (assq 'efs-process-host-type minor-mode-alist)
			 (delq
			  (assq 'efs-dired-listing-type minor-mode-alist)
			  minor-mode-alist))))
	  (setq minor-mode-alist nil)
	  (while old-list
	    (or (assq (car (car old-list)) our-list)
		(setq minor-mode-alist (nconc minor-mode-alist
					      (list (car old-list)))))
	    (setq old-list (cdr old-list)))
	  (setq minor-mode-alist (nconc our-list minor-mode-alist)))
      (setq minor-mode-alist
	    (nconc
	     (list '(efs-process-host-type efs-process-host-type)
		   '(efs-dired-listing-type efs-dired-listing-type-string))
	     minor-mode-alist))))

;;;; ------------------------------------------------------------
;;;; File name handlers
;;;; ------------------------------------------------------------

;;;###autoload
(defun efs-file-handler-function (operation &rest args)
  "Function to call special file handlers for remote files."
  (let ((handler (and (if (boundp 'allow-remote-paths)
			  allow-remote-paths
			t)
		      (get operation 'efs))))
    (if handler
	(apply handler args)
      (let ((inhibit-file-name-handlers
	     (cons 'efs-file-handler-function
		   (and (eq inhibit-file-name-operation operation)
			inhibit-file-name-handlers)))
	    (inhibit-file-name-operation operation))
	(apply operation args)))))

(defun efs-sifn-handler-function (operation &rest args)
  ;; Handler function for substitute-in-file-name
  (if (and (if (boundp 'allow-remote-paths)
			  allow-remote-paths
			t)
	   (eq operation 'substitute-in-file-name))
      (apply 'efs-substitute-in-file-name args)
    (let ((inhibit-file-name-handlers
	   (cons 'efs-sifn-handler-function
		 (and (eq operation inhibit-file-name-operation)
		      inhibit-file-name-handlers)))
	  (inhibit-file-name-operation operation))
      (apply operation args))))

;; Yes, this is what it looks like.  I'm defining the handler to run our
;; version whenever there is an environment variable.

(nconc file-name-handler-alist
       (list
	(cons "\\(^\\|[^$]\\)\\(\\$\\$\\)*\\$[{a-zA-Z0-9]"
	      'efs-sifn-handler-function)))

;;;; ------------------------------------------------------------
;;;; Necessary overloads.
;;;; ------------------------------------------------------------

;;;  The following functions are overloaded, instead of extended via
;;;  the file-name-handler-alist. For various reasons, the
;;;  file-name-handler-alist doesn't work for them. It would be nice if
;;;  this list could be shortened in the future.

;; File name exansion. It is not until _after_ a file name has been
;; expanded that it is reasonable to test it for a file name handler.
(efs-overwrite-fn "efs" 'expand-file-name)

;; Loading lisp files. The problem with using the file-name-handler-alist
;; here is that we don't know what is to be handled, until after searching
;; the load-path. The solution is to change the C code for Fload.
;; A patch to do this has been written by Jay Adams <jka@ece.cmu.edu>.
(efs-overwrite-fn "efs" 'load)
(efs-overwrite-fn "efs" 'require)

;;;; ------------------------------------------------------------
;;;; Install the file handlers for efs-file-handler-function.
;;;; ------------------------------------------------------------

;; I/O
(put 'insert-file-contents 'efs 'efs-insert-file-contents)
(put 'write-region 'efs 'efs-write-region)
(put 'directory-files 'efs 'efs-directory-files)
(put 'list-directory 'efs 'efs-list-directory)
(put 'insert-directory 'efs 'efs-insert-directory)
(put 'recover-file 'efs 'efs-recover-file)
;; file properties
(put 'file-directory-p 'efs 'efs-file-directory-p)
(put 'file-writable-p 'efs 'efs-file-writable-p)
(put 'file-readable-p 'efs 'efs-file-readable-p)
(put 'file-executable-p 'efs 'efs-file-executable-p)
(put 'file-symlink-p 'efs 'efs-file-symlink-p)
(put 'file-attributes 'efs 'efs-file-attributes)
(put 'file-exists-p 'efs 'efs-file-exists-p)
(put 'file-accessible-directory-p 'efs 'efs-file-accessible-directory-p)
;; manipulating file names
(put 'file-name-directory 'efs 'efs-file-name-directory)
(put 'file-name-nondirectory 'efs 'efs-file-name-nondirectory)
(put 'file-name-as-directory 'efs 'efs-file-name-as-directory)
(put 'directory-file-name 'efs 'efs-directory-file-name)
(put 'abbreviate-file-name 'efs 'efs-abbreviate-file-name)
(put 'file-name-sans-versions 'efs 'efs-file-name-sans-versions)
(put 'unhandled-file-name-directory 'efs 'efs-unhandled-file-name-directory)
(put 'diff-latest-backup-file 'efs 'efs-diff-latest-backup-file)
(put 'file-truename 'efs 'efs-file-truename)
;; modtimes
(put 'verify-visited-file-modtime 'efs 'efs-verify-visited-file-modtime)
(put 'file-newer-than-file-p 'efs 'efs-file-newer-than-file-p)
(put 'set-visited-file-modtime 'efs 'efs-set-visited-file-modtime)
;; file modes
(put 'set-file-modes 'efs 'efs-set-file-modes)
(put 'file-modes 'efs 'efs-file-modes)
;; buffers
(put 'backup-buffer 'efs 'efs-backup-buffer)
(put 'get-file-buffer 'efs 'efs-get-file-buffer)
(put 'create-file-buffer 'efs 'efs-create-file-buffer)
;; creating and removing files
(put 'delete-file 'efs 'efs-delete-file)
(put 'copy-file 'efs 'efs-copy-file)
(put 'rename-file 'efs 'efs-rename-file)
(put 'file-local-copy 'efs 'efs-file-local-copy)
(put 'make-directory-internal 'efs 'efs-make-directory-internal)
(put 'delete-directory 'efs 'efs-delete-directory)
(put 'add-name-to-file 'efs 'efs-add-name-to-file)
(put 'make-symbolic-link 'efs 'efs-make-symbolic-link)
;; file name completion
(put 'file-name-completion 'efs 'efs-file-name-completion)
(put 'file-name-all-completions 'efs 'efs-file-name-all-completions)

;;;; ------------------------------------------------------------
;;;; Finally run any load-hooks.
;;;; ------------------------------------------------------------

(run-hooks 'efs-load-hook)

;;; end of efs.el
