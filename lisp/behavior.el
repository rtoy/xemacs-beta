;;; behavior.el --- consistent interface onto packages

;; Copyright (C) 2000, 2001, 2002, 2003 Ben Wing.

;; Author: Ben Wing
;; Maintainer: XEmacs Development Team
;; Keywords: internal, dumped

;; This file is part of XEmacs.

;; XEmacs is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; XEmacs is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with XEmacs; see the file COPYING.  If not, write to the 
;; Free Software Foundation, 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Synched up with: Not in FSF.

;;; Authorship:

;; Created July 2000 by Ben Wing.

;;; Commentary:

;; This file is dumped with XEmacs.

;; This file is part of the "Behaviors" project and is a work in progress.
;; The purpose of the project is to provide (a) a consistent interface (at
;; the API level) onto the functionality provided by packages, and (b) an
;; easy-to-use user interface for this functionality, something that
;; *really works*.
;;
;; First, what characteristics do/should packages have? (NOTE: In this
;; discussion below, `package' and `behavior' are being used more or less
;; interchangeably.  Eventually this will get resolved.)

;; 1) A file, or one or more file, containing the code of the package.  In
;;    addition, a "head" file in the case that the package needs to be
;;    loaded in order to get its functionality (e.g. "load-to-enable"
;;    packages -- an abomination that is tolerated only with severe
;;    displeasure).
;; 2) A Lisp name -- a fairly short symbol (2-3 words max), uncapitalized,
;;    without use of excessive abbreviation and with words set off by
;;    dashes.  This should be the same as the name of the topmost custom
;;    group associated with the package (see next item), and preferably the
;;    same as the common prefix used for variables defined by your package
;;    and the name of the head file of the package.
;; 3) Associated custom group listing the settings associated with the package.
;; 4) Enable and disable methods for turning on or off the functionality of
;;    the package, if it's amenable to such a model.  Most packages are of two
;;    types:
;; 
;;    (a) They add some functionality to XEmacs, which is incorporated
;;    into and makes changes to the normal functionality of XEmacs.  Once the
;;    package is enabled, the user doesn't have to do anything specific for
;;    the package to do its thing -- it happens automatically if the user is
;;    using the area whose behavior has been changed.  These include packages
;;    such as `avoid' (which makes the mouse poointer move when the cursor
;;    gets too close), EFS (which adds the ability to treat an FTP site as
;;    part of the local file system), the packages that supply the
;;    mode-specific handling for various files, etc
;; 
;;    (b) They provide functionality in the form of specific command to be
;;    invoked.  This can be as simple as the `hippie-expand' command (tries
;;    lots of different expansion methods for the text before point, to
;;    try and get a result) and as complicated as GNUS or VM.
;; 
;;    Some packages might provide both -- you can enable them and they
;;    incorporate some functionality into the XEmacs base, but while
;;    they're enabled they provide certain commands.  #### We need some
;;    thought here, and case-by-case analysis, to determine if this really
;;    makes sense -- can the enable/disable be removed and whatever needs
;;    to happen incorporated as part of the command?  can the
;;    enable/disable just added to the commands?
;; 
;; 5) Packages of type (b) just above will have a list of commands that can be
;;    run.  They should be in standard menubar format -- i.e. just like a
;;    submenu, but without the initial string indidicating the name of the
;;    menu.
;; 6) Short doc string, for use in a menu item.  *NOT* necessarily the same
;;    as the documentation for the Custom group, which is often too long.
;; 7) Long documentation.
;; 
;; Good package etiquette:
;; 
;; 
;; -- Never mess with the menu yourself, or try to "cheat" by putting yourself
;;    high up in the hierarchy, e.g. at the top-level or directly off a
;;    top-level group that expects to contain only groups of groups, not
;;    end-level groups.
;; 
;; -- Never use the `override-behavior' commands or the like for specifying
;; (in an overriding fashion) the exact appearance of the hierarchies.
;; 
;; -- For type (a), with enable/disable methods:
;; 
;;    (a) Loading the file should NOT DO ANYTHING.  Not enable, not add hooks,
;;        nothing.
;;    (b) Both enable and disable hooks must exist.  The disable hook must
;;        completely reset the environment to how it was before the package
;;        was enabled.  This includes restoring the prior bindings for
;;        modified key bindings.  #### We need some helper function to assist
;;        with remembering the old key bindings and putting them back only
;;        when new key bindings haven't been made -- but recognize when those
;;        new key bondings were attached as a result of loading another
;;        package, so that after any order of loading and unloading a series
;;        of packages, the original bindings will eventually occur. (Something
;;        like `advice' for key definitions.) Replacement of functions should
;;        happen through `advice'.
;; 
;;    We recognize that many packages out there don't follow these precepts at
;;    all.  Many or most of them are install-only, often happening
;;    automatically when the file is loaded.  Converting these will be a step
;;    at a time: First, redo the latter type so that the initialization code
;;    is put into a function and not run automatically upon load.  Next step,
;;    try to provide some sort of disable.  Third step, work on making sure
;;    that disable removes *everything* and enable puts it all back.  Fourth
;;    step, work on properly advising keys and functions.
;; 

;; Comparison/Integration with Custom:

;; Custom only handles variable settings, and has no concept of standard
;; enable/disable methods for a package, a standard way of specifying
;; package documentation, or a list of commands associated with a package.
;; Also, its groups do not always map very well onto packages and the
;; resulting hierarchy is too big, confusing, difficult-to-navigate, and
;; incoherent.  More generally it does not address at all the basic problem
;; that a hierarchy created in a decentralized fashion -- and by a large
;; number of authors, some more competent than others -- will inevitably be
;; incoherent when put together.
;;

;; In general, ease-of-use was not the overarching goal of Custom.  The
;; primary goal of Custom seems to have been to provide a consistent interface
;; and get all the packages to use it.  Ease-of-use -- or even following
;; established user-interface standards -- has taken a far-distant second, and
;; appears in many respects to be an afterthought that never had any serious
;; effort investigated into it.
;; 
;; The eventual intent of this project is to integrate with custom.  The final
;; intent of integration is that this project subsumes Custom completely,
;; making Custom the unified, user-friendly means of controlling XEmacs that
;; has never properly existed.  However, that will take a lot of work.  For
;; the meantime, the plan is to develop the Behavior subsystem independent of
;; Custom, with ease-of-use as the primary goal, and get it to the point where
;; it encompasses most packages out there, has stabilized its interface, and
;; works well.  At that point, we will consider integration with Custom. (Note
;; that the hard part of the Behavior work is not actually behaviorizing the
;; packages, but developing the interface itself.)
;; 
;; As for integrating with Custom -- ideally that would mean simply extending
;; defgroup, but that is not really possible given that backward-compatibility
;; would not work -- existing versions of `defgroup' give an error when
;; presented with an unknown keyword.  In practice, then, this might mean that
;; a separate `define-behavior' command (or `defpackage', or the like) will
;; still exist.

;;; Code:

;; Hash table mapping behavior names to property lists, with entries for
;; :group, :custom-group, :short-doc, :require, :enable, :disable,
;; and :commands.
(defconst behavior-hash-table (make-hash-table))
;; Hash table mapping groups to property lists (entries for :group, :children,
;; :short-doc).
(defconst behavior-group-hash-table (make-hash-table))
;; Hash table with override information for groups.
;; :short-doc).
(defconst behavior-override-hash-table (make-hash-table))

(defvar within-behavior-enabling-disabling nil)

(defgroup behaviors nil
  "Behaviors -- high-level functionality interface.")

;; List of enabled behaviors.
(defcustom enabled-behavior-list nil
  "List of currently enabled behaviors.
Normally, don't set it directly; use `enable-behavior' or `disable-behavior'."
  :initialize #'set-default
  :set #'(lambda (sym val)
	   (if within-behavior-enabling-disabling
	       (set sym val)
	     (let* ((old-val enabled-behavior-list)
		    (disable-list (set-difference old-val val))
		    (enable-list (set-difference val old-val)))
	       (dolist (b disable-list)
		 (disable-behavior b t))
	       (dolist (b enable-list)
		 (enable-behavior b t))
	       (assert (equal (sort (copy-sequence enabled-behavior-list) 'string-lessp)
			      (sort (copy-sequence val) 'string-lessp))))))
  :type '(repeat (symbol :tag "Behavior"))
  :group 'behaviors)


(defvar behavior-history nil
  "History of entered behaviors.")

(defun behavior-group-p (group)
  "Non-nil if GROUP is the name of a valid behavior group."
  (not (null (gethash group behavior-group-hash-table))))

(defun check-behavior-group (group)
  "Verify that GROUP is a valid behavior group, or nil.
Return GROUP if so."
  (or (behavior-group-p group)
      (error 'invalid-argument "Invalid behavior group" group))
  group)

(defun* define-behavior (name doc-string &key
			 group custom-group
			 (short-doc
			  (capitalize-string-as-title
			   (replace-in-string (symbol-name name) "-" " ")))
			 require enable disable commands
			 &allow-other-keys)
  ;; We allow other keys to allow for the possibility of extensions by
  ;; later versions of XEmacs.  Packages should be able to support those
  ;; extensions without worrying about causing problems with older versions
  ;; of XEmacs.
  "Define a behavior named NAME.
DOC-STRING must be specified, a description of what the behavior does
when it's enabled and how to further control it (typically through
custom variables).  Accepted keywords are

:group	    Symbol naming the behavior group this behavior is within.
:custom-group Symbol naming the custom group containing the options that
            can be set in association with this behavior.  If not specified,
            the custom group with the same name as the behavior will be
            used, if it exists.
:short-doc  A \"pretty\" version of the name, for use in menus.  If omitted
              a prettified name will be generated.
:require    A single symbol or a list of such symbols, which need to be
              present at enable time, or will be loaded using `require'.
:enable     A function of no variables, which turns the behavior on.
:disable    A function of no variables, which turns the behavior off.
:commands   A list of interactive commands that can be invoked in
            conjunction with the behavior.  These will appear in a submenu
            along with the rest of the items for the behavior.

Behaviors are assumed to be global, and to take effect immediately; if
the underlying package is per-buffer, it may have to scan all existing
buffers and frob them.  When a behavior is disabled, it should completely
go away *everywhere*, as if it were never invoked at all.

The :disable keyword can be missing, although this is considered bad
practice.  In such a case, attempting to disable the behavior will signal
an error unless you use the `force' option.

The :enable keyword can be missing.  This is useful for behaviors that
are really a series of related commands without anything semantically
corresponding to \"turning on\" or \"turning off\" the behavior.

A behavior with no :enable and no :command is possible.  This might be
used, for example, by a behavior that encapsulates a series of related
Lisp functions.  Such behaviors may be handled specially, e.g. not
displayed in the menus or displayed in a separate location, since they
have no user-invocable behavior."
  (let ((entry (list :group (check-behavior-group group)
		     :custom-group custom-group
		     :short-doc short-doc :require require
		     :enable enable :disable disable
		     :commands commands)))
    (puthash name entry behavior-hash-table))
  ;; update the children list of the group we're in (maybe nil).
  (unless (member name (getf (gethash group behavior-group-hash-table)
			     :children))
    (push name (getf (gethash group behavior-group-hash-table) :children))))

(defun* override-behavior (name &key
			   short-doc
			   group
			   include
			   demote-others)
  "Override the default properties of a behavior group NAME.
Normally, groups are created and assigned properties by individual packages.
The resulting hierarchy may not make much sense globally.  This function
allows the hierarchy and appearance of a group to be specified globally,
and will take precendence over the properties assigned by `define-behavior-group'.  This allows a global organization to be imposed on groups, while still allowing for graceful handling of new or unknown groups.

NAME can be a symbol specifying a group name, or a list of
\(PARENT [...] NAME), where a path from a particular parent is explicitly
given. (This latter form allows the same name to be assigned to more than one
group.)

Accepted keywords are

:short-doc  A \"pretty\" version of the name, for use in menus.
:group      Parent group, if any.  Should not be given if the parents are
            explicitly specified in NAME.
:include    A list of behaviors that are specifically included in this
            group, in addition to those that are included by the behaviors
            themselves.
:demote-others If non-nil, exclude all behaviors not specified in the :include
            list and put them instead (i.e. \"demote\" them) to another group,
            usually a subgroup."
  (let ((entry (list :group (check-behavior-group group)
		     :short-doc short-doc
		     :include include
		     :demote-others demote-others)))
    (puthash name entry behavior-override-hash-table)))

(defun* define-behavior-group (name &key
			       (short-doc
				(capitalize-string-as-title
				 (replace-in-string (symbol-name name) "-"
						    " ")))
			       group)
  "Define a behavior group NAME.

NAME can be a symbol specifying a group name, or a list of
\(PARENT [...] NAME), where a path from a particular parent is explicitly
given. (This latter form allows the same name to be assigned to more than one
group.)

Accepted keywords are

:short-doc  A \"pretty\" version of the name, for use in menus.  If omitted
              a prettified name will be generated.
:group      Parent group, if any.  Should not be given if the parents are
            explicitly specified in NAME."
  (let ((entry (list :group (check-behavior-group group)
		     :short-doc short-doc)))
    (puthash name entry behavior-group-hash-table))
  ;; update the children list of the parent (maybe nil).
  (push name (getf (gethash group behavior-group-hash-table) :children)))

(defun read-behavior (prompt &optional must-match initial-contents history
		      default-value)
  "Return a behavior symbol from the minibuffer, prompting with string PROMPT.
If non-nil, optional second arg INITIAL-CONTENTS is a string to insert
 in the minibuffer before reading.
Third arg HISTORY, if non-nil, specifies a history list. (It defaults to
`behavior-history'.)
Fourth arg DEFAULT-VALUE is the default value.  If non-nil, it is used
 for history command, and as the value to return if the user enters the
 empty string."
  (let ((result
	 (completing-read
	  prompt
	  (let ((table (let (lis)
			 (maphash #'(lambda (key val)
				      (push (cons key val) lis))
				  behavior-hash-table)
			 (nreverse lis))))
	    (mapc #'(lambda (aentry)
		      (setcar aentry (symbol-name (car aentry))))
		  table)
	    table)
	  nil must-match initial-contents (or history 'behavior-history)
	  default-value)))
    (if (and result (stringp result))
	(intern result)
      result)))

(defun behavior-enabled-p (behavior)
  "Non-nil if BEHAVIOR (a symbol) if currently enabled."
  (memq behavior enabled-behavior-list))

(defun enable-behavior (behavior &optional force)
  "Enable the specified behavior."
  (interactive (list (read-behavior "Enable Behavior: " t) current-prefix-arg))
  (let ((plist (gethash behavior behavior-hash-table)))
    (or plist (error 'invalid-argument "Not a behavior" behavior))
    (or force (not (memq behavior enabled-behavior-list))
	(error 'invalid-change "Behavior already enabled" behavior))
    (let ((require (getf plist :require))
	  (enable (getf plist :enable)))
      (cond ((listp require)
	     (mapc #'(lambda (sym) (require sym)) require))
	    ((symbolp require)
	     (require require))
	    ((null require))
	    (t (error 'invalid-argument "Invalid :require spec" require)))
      (message "Enabling behavior %s..." behavior)
      (if enable (funcall enable))
      (message "Enabling behavior %s...done" behavior)
      (let ((within-behavior-enabling-disabling t))
	(customize-set-variable 'enabled-behavior-list
				(cons behavior enabled-behavior-list))))))

(defun disable-behavior (behavior &optional force)
  "Disable the specified behavior."
  (interactive (list (read-behavior "Disable Behavior: " t)
		     current-prefix-arg))
  (let ((plist (gethash behavior behavior-hash-table)))
    (or plist (error 'invalid-argument "Not a behavior" behavior))
    (or force (memq behavior enabled-behavior-list)
	(error 'invalid-change "Behavior not enabled" behavior))
    (let ((require (getf plist :require))
	  (disable (getf plist :disable)))
      (cond ((listp require)
	     (mapc #'(lambda (sym) (require sym)) require))
	    ((symbolp require)
	     (require require))
	    ((null require))
	    (t (error 'invalid-argument "Invalid :require spec" require)))
      (message "Disabling behavior %s..." behavior)
      (if disable (funcall disable))
      (message "Disabling behavior %s...done" behavior)
      (let ((within-behavior-enabling-disabling t))
	(customize-set-variable 'enabled-behavior-list
				(delq behavior enabled-behavior-list))))))

(defun compute-behavior-group-children (group hash)
  "Compute the actual children for GROUP and its subgroups.
This takes into account the override information specified."
  (let* ((group-plist (gethash group behavior-group-hash-table))
	 (override (gethash group behavior-override-hash-table))
	 (children (getf group-plist :children)))
    )
  )

(defun behavior-menu-filter-1 (menu group)
  (submenu-generate-accelerator-spec
   (let* (
	  ;;options
	  ;;help
	  (enable
	   (menu-split-long-menu
	    (menu-sort-menu
	     (let ((group-plist (gethash group behavior-group-hash-table)))
	       (loop for behavior in (getf group-plist :children)
		 nconc (if (behavior-group-p behavior)
			   (list
			    (cons (getf
				   (gethash behavior behavior-group-hash-table)
				   :short-doc)
				  (behavior-menu-filter-1 menu behavior)))
			 (let* ((plist (gethash behavior behavior-hash-table))
				(commands (getf plist :commands)))
			   (nconc
			    (if (getf plist :enable)
				`([,(format "%s (%s) [toggle]"
					    (getf plist :short-doc)
					    behavior)
				   (if (memq ',behavior
					     enabled-behavior-list)
				       (disable-behavior ',behavior)
				     (enable-behavior ',behavior))
				   :active ,(if (getf plist :disable) t
					      (not (memq
						    ',behavior
						    enabled-behavior-list)))
				   :style toggle
				   :selected (memq ',behavior
						   enabled-behavior-list)]))
			    (cond ((null commands) nil)
				  ((and (eq (length commands) 1)
					(vectorp (elt commands 0)))
				   (let ((comm (copy-sequence
						(elt commands 0))))
				     (setf (elt comm 0)
					   (format "%s (%s)"
						   (elt comm 0) behavior))
				     (list comm)))
				  (t (list
				      (cons (format "%s (%s) Commands"
						    (getf plist :short-doc)
						    behavior)
					    commands)))))))))
	     ))
	   )
	  )
     enable)
   '(?p)))

(defun behavior-menu-filter (menu)
  (append
   '(("%_Package Utilities"
       ("%_Set Download Site"
	("%_Official Releases"
	 :filter (lambda (&rest junk)
		   (menu-split-long-menu
		    (submenu-generate-accelerator-spec
		     (package-ui-download-menu)))))
	("%_Pre-Releases"
	 :filter (lambda (&rest junk)
		   (menu-split-long-menu
		    (submenu-generate-accelerator-spec
		     (package-ui-pre-release-download-menu)))))
	("%_Site Releases"
	 :filter (lambda (&rest junk)
		   (menu-split-long-menu
		    (submenu-generate-accelerator-spec
		     (package-ui-site-release-download-menu))))))
       "--:shadowEtchedIn"
      ["%_Update Package Index" package-get-update-base]
      ["%_List and Install" pui-list-packages]
      ["U%_pdate Installed Packages" package-get-update-all]
      ["%_Help" (Info-goto-node "(xemacs)Packages")])
     "----")
   (behavior-menu-filter-1 menu nil)))

;; Initialize top-level group.
(puthash nil '(:children nil :short-doc "Root") behavior-group-hash-table)

(provide 'behavior)

;;; finder-inf.el ends here
