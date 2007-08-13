;;; about.el --- the About The Authors page (shameless self promotion).
;;;

;; Copyright (c) 1995, 1996, 1997 XEmacs Advocacy Organization.

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

;;; This is kind of a kludge.  We were going to use W3 to do this, but
;;; it's too slow to load, and HTML gives you too little control over
;;; the layout (especially indentation and inter-paragraph spacing).
;;; Maybe the text should have been written in limited HTML anyway,
;;; and I should have hacked up a simple and fast parser for it, but
;;; it's done now...
;;;
;;; Code: Jamie Zawinski <jwz@netscape.com>
;;; Text: Ben Wing <wing@666.com>, Jamie Zawinski <jwz@netscape.com>
;;; Hard: Amiga 1000, Progressive Peripherals Frame Grabber.
;;; Soft: FG 2.0, DigiPaint 3.0, pbmplus (dec 91), xv 3.0.
;;; Modified for 19.11 by Eduardo Pelegri-Llopart <pelegri@eng.sun.com>
;;;		      and Chuck Thompson <cthomp@xemacs.org>
;;; More hacking for 19.12 by Chuck Thompson and Ben Wing.
;;; 19.13 and 19.14 updating done by Chuck Thompson.
;;; 19.15 and 20.0 updating done by Steve Baur and Martin Buchholz.

(require 'browse-url)
(defvar about-xref-map (let ((map (make-sparse-keymap)))
			 (define-key map 'button1 'about-xemacs-xref)
			 (define-key map 'button2 'about-xemacs-xref)
			 (define-key map '(return) 'about-xemacs-xref)
			 map))

;; This historically significant variable has been removed from service.
(defvar what-are-we-calling-it-today "XEmacs")

(defun about-face (text face)
  (let ((p (point))
	e)
    (insert text)
    (setq e (make-extent p (point)))
    ;;(set-extent-property e 'start-open t)
    (set-extent-face e face)
    e))

(defun about-xref (text xref help)
  (let ((e (about-face text 'bold)))
    (set-extent-property e 'keymap about-xref-map)
    (set-extent-property e 'mouse-face 'highlight)
    (set-extent-property e 'xref xref)
    (set-extent-property e 'help-echo help)
    e))

;;;###autoload
(defun about-xemacs ()
  (interactive)
  (switch-to-buffer (get-buffer-create "About XEmacs"))
  (delete-other-windows)
  (buffer-disable-undo (current-buffer))
  (widen)
  (set (make-local-variable 'tab-width) 8)
  (setq buffer-read-only t)
  (view-mode nil 'kill-buffer)		;; assume the new view-less
  (let* ((buffer-read-only nil)
         (emacs-short-version (concat emacs-major-version "." emacs-minor-version))
         (emacs-about-version (format "version %s; February 1997" emacs-short-version))
	 (indent-tabs-mode t)
	 )
    (erase-buffer)
    (insert "\n")
    (indent-to (startup-center-spaces xemacs-logo))
    (let ((e (make-extent (point) (point))))
      (set-extent-begin-glyph e xemacs-logo))
    (insert "\n\n")
    (indent-to (startup-center-spaces "(formerly known as Lucid Emacs)"))
    (insert "(formerly known as Lucid Emacs)")
    (insert "\n\n")
    (indent-to (startup-center-spaces emacs-about-version))
    (about-xref emacs-about-version 'news "The latest NEWS of XEmacs")
    (insert "\n\n")

    (insert "\n\t")
    (about-face "XEmacs" 'italic)
    (insert " is a powerful, extensible text editor with full GUI
	support, initially based on an early version of GNU Emacs 19 from
	the Free Software Foundation and since kept up to date with recent
	versions of that product.  XEmacs stems from a ")
    (about-xref "collaboration" 'history "An XEmacs History Lesson")
    (insert "\n\tof Lucid, Inc. with Sun Microsystems, Inc. and the University
	of Illinois with additional support having been provided by
	Amdahl Corporation and INS Engineering Corporation.\n\n\t")

	(insert "In almost all circumstances, Emacs-Lisp code written for
	GNU Emacs versions 18 and 19 will run under XEmacs without
	requiring any modifications, or at most will require small
	changes to accommodate an improved functional interface.\n\n\t")

    (insert "XEmacs provides a great number of ")
    (about-xref "new features" 'features "See a list of the new features.")
    (insert ".  More details
	on XEmacs's functionality, including bundled packages can be
	obtained through the ")

    (about-xref "`info`" 'info "Look at the info pages")
    (insert " on-line information system.

	The WWW page for XEmacs can be browsed, using any WWW browser, at\n\t\t")
    (about-xref "http://www.xemacs.org/" 'w3-xemacs "Go to the XEmacs World Wide Web page")
    (insert "\n\n\tNote that w3 (XEmacs's own browser), might need customization
	(due to firewalls) in order to work correctly.\n\n\t")

    (insert "XEmacs is the result of the time and effort of many people.
	The developers responsible for the " emacs-short-version " release are:

		 * ") (about-xref "Steve Baur" 'steve "Find out more about Steve Baur") (insert "  <steve@altair.xemacs.org>
		 * ") (about-xref "Martin Buchholz" 'mrb "Find out more about Martin Buchholz") (insert "  <mrb@eng.sun.com>
		 * ") (about-xref "Chuck Thompson" 'cthomp "Find out more about Chuck Thompson") (insert "  <cthomp@xemacs.org>
		 * ") (about-xref "Ben Wing" 'wing "Find out more about Ben Wing") (insert "  <wing@xemacs.org>

		 * ") (about-xref "And many other contributors..." 'others "Read about the legion of XEmacs hackers") (insert "

	Chuck Thompson was Mr. XEmacs from 19.11 through 19.14.  Ben Wing
	was crucial to each of those releases.

	Jamie Zawinski was Mr. Lucid Emacs from 19.0 through 19.10,
	the last release actually named Lucid Emacs.  Richard Mlynarik
	was crucial to most of those releases.

		 * ") (about-xref "Jamie Zawinski" 'jwz "Find out more about Jamie Zawinski") (insert "  <jwz@netscape.com>
		 * ")  (about-xref "Richard Mlynarik" 'mly "Find out more about Richard Mlynarik")  (insert "  <mly@adoc.xerox.com>")
   (insert "\n\n\tClick ")
   (about-xref "here" 'kill-buffer "Exit the About page")
   (insert " to remove (kill) this buffer.")
   (goto-char (point-min)))
  )

(defun about-load-mosaic (&optional who-to-load)
  (save-excursion
    (set-buffer (get-buffer-create "About XEmacs"))
    (toggle-read-only 0)

    (let ((rest (if who-to-load (list who-to-load)
		  '(steve mrb cthomp wing stig jwz mly vladimir baw piper bw wmperry kyle larsi)))
	  (got-error nil))
      (while rest
	(let* ((who (car rest))
	       (who-xpm (expand-file-name
			 (concat (symbol-name who)
				 (if (memq (device-class (selected-device))
					   '(color grayscale))
				     ""
				   "m")
				 ".xpm")
			 data-directory)))
	  (or (file-exists-p who-xpm) (setq who-xpm (concat who-xpm ".Z")))
	  (if (eq nil (assoc who (buffer-local-variables)))
	      (make-local-variable who))
	  (if (and (boundp who)
		   (glyphp (symbol-value who)))
	      nil
	    (message "One moment please...")
	    (condition-case c
		(save-restriction
		  (set who nil)
		  (narrow-to-region (point) (point))
		  (insert-file-contents who-xpm)
		  (if (looking-at "\037\235") ;may already be decompressed...
		      (call-process-region (point-min) (point-max)
					   "zcat" t t nil))
		  (set who (make-glyph
			    (prog1 (buffer-string)
			      (delete-region (point-min) (point-max)))))
		  )
	      (error
	       (setq got-error t)
	       (message nil)
	       (display-error c nil)
	       (sit-for 2)))))
	(setq rest (cdr rest)))
      (or got-error (message nil)))
    (toggle-read-only 1)
    ))

(defun about-add-mosaic ()
  (goto-char (point-min))
  (about-load-mosaic)

  ;; HERE TO PLACE ADDITIONAL MUGSHOTS

  (goto-char (point-max))
  (insert "\n   ")

  (let ((rest '(steve mrb cthomp wing stig linebreak jwz mly vladimir linebreak baw piper bw linebreak wmperry kyle larsi))
	(got-error nil))
    (while rest
      (if (eq (car rest) 'linebreak)
	  (insert "\n\n  ")
	(let* ((who (car rest))
	       (b (get-buffer "About XEmacs"))
	       (p (symbol-value-in-buffer who b nil)))
	  (or (glyphp p) (setq p nil))
	  (and p
	       (let ((e (make-extent (point) (point))))
		 (set-extent-begin-glyph e p)
		 (set-extent-property e 'keymap about-xref-map)
		 (set-extent-property e 'xref who)))
	  (insert " ")
	  (sit-for 0)))
      (setq rest (cdr rest)))
    (insert "\n")
    (goto-char (point-min))
    (or got-error (message nil)))
  )

(defun about-xemacs-xref ()
  (interactive "@")
  (let* ((e (or current-mouse-event last-input-event))
	 (extent (or (and (null e) (extent-at (point)))
		     (and (mouse-event-p e) (event-glyph-extent e))
 		     (extent-at (if (mouse-event-p e)
				    (event-point e)
				  (point))
				(if (mouse-event-p e)
				    (event-buffer e)
				  (current-buffer))
				'xref)))
	 (xref (extent-property extent 'xref))
	 prev-page)
    ;; prev-page is used for the core people's pages since they can be
    ;; reached from two different locations
    (if (equal (buffer-name) "About XEmacs")
	(setq prev-page 'about)
      ;; Kill the sub-buffers when going back to the top, so that we
      ;; don't hold pointers to the bitmaps longer than necessary.
      (if (not (eq xref 'w3-jamie))
	  (progn
	    (kill-buffer (current-buffer))
	    (setq prev-page 'others))))
    (cond
     ((eq xref 'about)
      (about-xemacs))
     ((eq xref 'info)
      (info))
     ((or (eq xref 'w3-xemacs) (eq xref 'w3-jamie))
      (funcall browse-url-browser-function
	       (if (eq xref 'w3-xemacs)
		   "http://www.xemacs.org/"
		 "http://www.netscape.com/people/jwz/")))
     ((eq xref 'kill-buffer)
      (kill-buffer (current-buffer)))
     ((eq xref 'news)
      (view-emacs-news)
      (view-mode nil 'kill-buffer)		;; assume the new view-less
      (save-excursion
	(goto-char (point-min))
	(let ((buffer-read-only nil))
	  (insert "\nClick ")
	  (about-xref "here" prev-page "Return to previous page")
	  (insert " to go back to the previous page.\n\n")
	  (set-buffer-modified-p nil)
	  )))
     (t
      (switch-to-buffer
       (get-buffer-create
	(case xref
	  ('jwz "About Jamie Zawinski")
	  ('cthomp "About Chuck Thompson")
	  ('wing "About Ben Wing")
	  ('mly "About Richard Mlynarik")
	  ('vladimir "About Vladimir Ivanovic")
	  ('baw "About Barry Warsaw")
	  ('wmperry "About William Perry")
	  ('bw "About Bob Weiner")
	  ('piper "About Andy Piper")
	  ('stig "About Jonathan Stigelman")
	  ('steve "About Steve Baur")
	  ('mrb "About Martin Buchholz")
	  ('kyle "About Kyle Jones")
	  ('larsi "About Lars Magne Ingebrigtsen")
	  ('others "About Everyone")
	  ('features "New XEmacs Features")
	  ('history "XEmacs History")
	  )))
      (delete-other-windows)
      (buffer-disable-undo (current-buffer))
      (widen)
      (setq buffer-read-only t)
      (view-mode nil 'kill-buffer)		;; assume the new view-less
      (let ((buffer-read-only nil)
	    (case-fold-search nil)
	    )
	(if (and (not (eq xref 'others)) (not (eq xref 'history))
		 (not (eq xref 'features)))
	    (about-load-mosaic xref))
	(erase-buffer)
	(let* ((b (get-buffer "About XEmacs"))
	       (p (and b (symbol-value-in-buffer xref b nil))))
	  (or (glyphp p) (setq p nil))
	  (cond (p
		 (insert "\n\t")
		 (set-extent-begin-glyph (make-extent (point) (point)) p)
		 (insert "\n\t"))
		(t
		 (insert "\n\t"))))
	(cond
	 ((eq xref 'history)
	  (insert "Click ")
	  (about-xref "here" prev-page "Return to previous page")
	  (insert " to go back to the previous page.\n\n\t")

	  (about-face "XEmacs" 'bold)
	  (insert "\n\n\n\t")
	  (about-face "The Lucid, Inc. Point of View" 'italic)
	  (insert "

	At the time of the inception of Lucid Emacs (the former name
	of XEmacs), Lucid's latest product was Energize, a C/C++
	development environment.  Rather than invent (and force our
	users to learn) a new user interface, we chose to build part
	of our environment on top of the world's best editor, GNU
	Emacs.  (Though our product is commercial, the work we did on
	GNU Emacs is free software, and is useful in its own right.)

	We needed a version of Emacs with mouse-sensitive regions,
	multiple fonts, the ability to mark sections of a buffer as
	read-only, the ability to detect which parts of a buffer have
	been modified, and many other features.

	For our purposes, the existing version of Epoch was not
	sufficient; it did not allow us to put arbitrary pixmaps/icons
	in buffers, `undo' did not restore changes to regions, regions
	did not overlap and merge their attributes in the way we needed,
	and several other things.

	We could have devoted our time to making Epoch do what we needed
	(and, in fact, we spent some time doing that in 1990) but, since
	the FSF planned to include Epoch-like features in their version
	19, we decided that our efforts would be better spent improving
	Emacs 19 instead of Epoch.

	Our original hope was that our changes to Emacs would be
	incorporated into the \"official\" v19.  However, scheduling
	conflicts arose, and we found that, given the amount of work
	still remaining to be done, we didn't have the time or manpower
	to do the level of coordination that would be necessary to get
	our changes accepted by the FSF.  Consequently, we released our
	work as a forked branch of Emacs, instead of delaying any
	longer.

	Roughly a year after Lucid Emacs 19.0 was released, a beta
	version of the FSF branch of Emacs 19 was released.  The FSF
	version is better in some areas, and worse in others, as
	reflects the differing focus of our development efforts.

	We plan to continue developing and supporting Lucid Emacs, and
	merging in bug fixes and new features from the FSF branch as
	appropriate; we do not plan to discard any of the functionality
	that we implemented which RMS has chosen not to include in his
	version.

	Certain elements of Lucid Emacs, or derivatives of them, have
	been ported to the FSF version.  We have not been doing work in
	this direction, because we feel that Lucid Emacs has a cleaner
	and more extensible substrate, and that any kind of merger
	between the two branches would be far easier by merging the FSF
	changes into our version than the other way around.

	We have been working closely with the Epoch developers to merge
	in the remaining Epoch functionality which Lucid Emacs does not
	yet have.  Epoch and Lucid Emacs will soon be one and the same
	thing.  Work is being done on a compatibility package which will
	allow Epoch 4 code to run in XEmacs with little or no change.")

	  (insert "\n\n\n\t")
	  (about-face "The Sun Microsystems, Inc. Point of View" 'italic)
	  (insert "

	Emacs 18 has been around for a long, long time.  Version 19
	was supposed to be the successor to v18 with X support.  It
	was going to be available \"real soon\" for a long time (some
	people remember hearing about v19 as early as 1984!), but it
	never came out.  v19 development was going very, very slowly,
	and from the outside it seemed that it was not moving at all.
	In the meantime other people gave up waiting for v19 and
	decided to build their own X-aware Emacsen.  The most
	important of these was probably Epoch, which came from the
	University of Illinois (\"UofI\") and was based on v18.

	Around 1990, the Developer Products group within Sun
	Microsystems Inc., decided that it wanted an integrated
	editor.  (This group is now known as DevPro.  It used to be
	known as SunPro - the name was changed in mid-1994.)  They
	contracted with the University of Illinois to provide a number
	of basic enhancements to the functionality in Epoch.  UofI
	initially was planning to deliver this on top of Epoch code.

	In the meantime, (actually some time before they talked with
	UofI) Lucid had decided that it also wanted to provide an
	integrated environment with an integrated editor.  Lucid
	decided that the Version 19 base was a better one than Version
	18 and thus decided not to use Epoch but instead to work with
	Richard Stallman, the head of the Free Software Foundation and
	principal author of Emacs, on getting v19 out.  At some point
	Stallman and Lucid parted ways.  Lucid kept working and got a
	v19 out that they called Lucid Emacs 19.

	After Lucid's v19 came out it became clear to us (the UofI and
	Sun) that the right thing to do was to push for an integration
	of both Lucid Emacs and Epoch, and to get the deliverables
	that Sun was asking from the University of Illinois on top of
	this integrated platform.  Until 1994, Sun and Lucid both
	actively supported XEmacs as part of their product suite and
	invested a comparable amount of effort into it.  Substantial
	portions of the current code have originated under the support
	of Sun, either directly within Sun, or at UofI but paid for by
	Sun.  This code was kept away from Lucid for a while, but
	later was made available to them.  Initially Lucid didn't know
	that Sun was supporting UofI, but later Sun was open about it.

	Around 1992 DevPro-originated code started showing up in Lucid
	Emacs, starting with the infusion of the Epoch redisplay code.
	The separate code bases at Lucid, Sun, and the University of
	Illinois were merged, allowing a single XEmacs to evolve from
	that point on.

	Sun originally called the integrated product \"ERA\", for
	\"Emacs Rewritten Again\".  Sun and Lucid eventually came
	to an agreement to find a name for the product that was not
	specific to either company.  An additional constraint that
	Lucid placed on the name was that it must contain the word
	\"Emacs\" in it -- thus \"ERA\" was not acceptable.  The
	tentatively agreed-upon name was \"XEmacs\", and this has been
	the name of the program since version 19.11.)

	As of 1997, Sun is shipping XEmacs as part of its Developer
	Products integrated programming environment \"Sun WorkShop\".
	Sun is continuing to support XEmacs development, with focus on
	internationalization and quality improvement.")

	  (insert "\n\n\n\t")
	  (about-face "Lucid goes under\n" 'italic)
	  (insert "
	Around mid-'94, Lucid went out of business.  Lucid founder
	Richard Gabriel's book \"Patterns of Software\", which is
	highly recommended reading in any case, documents the demise
	of Lucid and suggests lessons to be learned for the whole
	software development community.

	Development on XEmacs, however, has continued unabated under
	the auspices of Sun Microsystems and the University of
	Illinois, with help from Amdahl Corporation and INS
	Engineering Corporation.  Sun plans to continue to support
	XEmacs into the future.")

	  (insert "\n\n\n\t")
	  (about-face "The Amdahl Corporation point of view" 'italic)
	  (insert "

	Amdahl Corporation's Storage Products Group (SPG) uses XEmacs
	as the focal point of a environment for development of the
	microcode used in Amdahl's large-scale disk arrays, or DASD's.
	SPG has joint ventures with Japanese companies, and decided
	in late 1994 to contract out for work on XEmacs in order
	to hasten the development of Mule support (i.e. support for
	Japanese, Chinese, etc.) in XEmacs and as a gesture of goodwill
	towards the XEmacs community for all the work they have done
	on making a powerful, modern, freely available text editor.
	Through this contract, Amdahl provided a large amount of work
	in XEmacs in the form of rewriting the basic text-processing
	mechanisms to allow for Mule support and writing a large
	amount of the support for multiple devices.
	
	Although Amdahl is no longer hiring a full-time contractor,
	they are still funding part-time work on XEmacs and providing
	resources for further XEmacs development.")

	  (insert "\n\n\n\t")
	  (about-face "The INS Engineering point of view" 'italic)
	  (insert "

	INS Engineering Corporation, based in Tokyo, bought rights
	to sell Energize when Lucid went out of business.  Unhappy
	with the performance of the Japanese support in XEmacs 19.11,
	INS also contributed to the XEmacs development from late 1994
	to early 1995.")

	  (insert "\n\n\n\t")
	  (insert "Click ")
	  (about-xref "here" prev-page "Return to previous page")
	  (insert " to go back to the previous page.\n\n\t")
	  )
	 ((eq xref 'jwz)
	  (about-face "Jamie Zawinski" 'bold)
	  (insert "\t\t\"")
	  (about-face "So much to do, so little time." 'italic)
	  (insert "\"\n")
	  (insert "\n
	Jamie Zawinski was primarily to blame for Lucid Emacs from its
	inception in 1991, to 1994 when Lucid Inc. finally died.  He is
	now to be found at Netscape Communications, hacking on Netscape
	Navigator (he did the first Unix version and the mail reader).
	Thankfully his extensive sleep deprivation experiments conducted
	during 1994 and 1995 are now a thing of the past, but his
	predilection for dark, Gothic music remains unabated.

	Come visit his glorified .plan file at

		")
	  (about-xref "http://www.netscape.com/people/jwz/" 'w3-jamie "Visit Jamie's WWW page")
	  (insert "\n\n\tClick ")
	  (about-xref "here" prev-page "Return to previous page")
	  (insert " to go back to the previous page.\n")
	  )
	 ((eq xref 'steve)
	  (about-face "Steve Baur" 'bold)
	  (insert " <steve@altair.xemacs.org>

	Steve took over the maintenance of XEmacs in November of 1996
	(it seemed like a good idea at the time ...).  In real life he is a
	network administrator and Unix systems programmer for Miranova
	Systems, Inc.

	Steve's main contributions to XEmacs have been reviving the FAQ,
	testing and integrating patches, tracking down and fixing bugs, and
	answering hundreds of questions on Usenet.")

	  (insert "\n\n\tClick ")
	  (about-xref "here" prev-page "Return to previous page")
	  (insert " to go back to the previous page.\n")
	  )
	 ((eq xref 'mrb)
	  (about-face "Martin Buchholz" 'bold)
	  (insert " <mrb@eng.sun.com>

	Martin is the XEmacs guy at DevPro, a part of Sun Microsystems.
	Martin used to do XEmacs as a `hobby' while at IBM, and was
	crazy enough to try to make a living doing it at Sun.

	Martin starting using Emacs originally not to edit files, but
	to get the benefit of shell mode. He actually used to run
	nothing but a shell buffer, and use `xterm -e vi' to edit
	files.  But then he saw the light.  He dreams of rewriting
	shell mode from scratch.  Stderr should show up in red!!

	Martin is currently working mostly on Internationalization.
	He spends most of his waking hours inside a Japanized XEmacs.")

	  (insert "\n\n\tClick ")
	  (about-xref "here" prev-page "Return to previous page")
	  (insert " to go back to the previous page.\n")
	  )
	 ((eq xref 'cthomp)
	  (about-face "Chuck Thompson" 'bold)
	  (insert " <cthomp@xemacs.org>

	Chuck, through being in the wrong place at the right time, has
	gotten stuck with being Jamie's replacement as the primary
	maintainer of XEmacs.  This has caused his hair to begin
	falling out and quadrupled his daily coffee dosage.  Though he
	works at and for the University of Illinois his funding for
	XEmacs work actually came from Sun Microsystems.

	He has worked on XEmacs since November 1992, which fact
	occasionally gives him nightmares.  As of October 1995, he no
	longer works full-time on XEmacs, though he does continue as
	an active maintainer.  His main contributions have been the
	greatly enhanced redisplay engine, scrollbar support, the
	toolbars, configure support and numerous other minor features
	and fixes.

	Rumors that Chuck is aka Black Francis aka Frank Black are
	completely unfounded.")

	  (insert "\n\n\tClick ")
	  (about-xref "here" prev-page "Return to previous page")
	  (insert " to go back to the previous page.\n")
	  )
	 ((eq xref 'wing)
	  (about-face "Ben Wing" 'bold)
	  (insert " <wing@666.com>

	I'm not a thug -- I just play one on video.
	My roommate says I'm a San Francisco \"Mission Critter\".\n\n\t")
	  (about-face "Gory stuff follows:" 'italic)
	  (insert "

	In 1992 I left a stuffy East-Coast university, set out into the
	real world, and ended up a co-founder of Pearl Software.  As
	part of this company, I became the principal architect of
	Win-Emacs, a port of Lucid Emacs to Microsoft Windows and
	Windows NT (for more info, e-mail to ")
	  (about-face "info@pearlsoft.com" 'italic)
	  (insert ").

	Since April 1993, I've worked on XEmacs as a contractor
	for various companies, changing hats faster than Ronald Reagan's
	hair color (oops, did I just show my age?).  My main contributions
	to XEmacs include rewriting large parts of the internals and the
	gory Xt/Xlib interfacing, adding the Mule support, implementing
	the external client widget, improving the documentation (especially
	the Emacs Lisp manual), and being a general nuisance ... er,
	brainstormer for many of the new features of XEmacs.

	Recently I took a job at Dimension X, where I'm working on a
	Java-based toolkit for developing VRML applications.")
	  (insert "\n\n\tClick ")
	  (about-xref "here" prev-page "Return to previous page")
	  (insert " to go back to the previous page.\n")
	  )
	 ((eq xref 'mly)
	  (about-face "Richard Mlynarik" 'bold)
	  (insert " <mly@adoc.xerox.com>

	Cars are Evil.  Ride a bike.")

	  (insert "\n\n\tClick ")
	  (about-xref "here" prev-page "Return to previous page")
	  (insert " to go back to the previous page.\n")
	  )
	 ((eq xref 'vladimir)
	  (about-face "Vladimir Ivanovic" 'bold)
	  (insert " <vladimir@mri.com>

	Former technical lead for XEmacs at Sun.  He is now with
	Microtec Research Inc., working on embedded systems
	development tools.")

	  (insert "\n\n\tClick ")
	  (about-xref "here" prev-page "Return to previous page")
	  (insert " to go back to the previous page.\n")
	  )

	 ((eq xref 'baw)
	  (about-face "Barry Warsaw" 'bold)
	  (insert " <bwarsaw@cnri.reston.va.us>

	Author of cc-mode for C++, C, and Objective-C editing, and
	Supercite for mail and news citing.  Also various and sundry other
	Emacs utilities, fixes, enhancements and kludgery as whimsy,
	boredom, and ToT dictate (but not necessarily in that order).


	Daddy
	© 1994 Warsaw
	========
	Drive me Daddy, drive me quick
	Push my pedal, shift my stick
	Fill me up with golden gas
	My rubber squeals, I go real fast

	Milk me Daddy, milk me now
	Milk me like a big ol' cow
	I've got milk inside my udder
	Churn it up and make some butter")

	  (insert "\n\n\tClick ")
	  (about-xref "here" prev-page "Return to previous page")
	  (insert " to go back to the previous page.\n")
	  )

	 ((eq xref 'bw)
	  (about-face "Bob Weiner" 'bold)
	  (insert " <weiner@infodock.com>

	Author of the Hyperbole everyday information management
	hypertext system and the OO-Browser multi-language code
	browser.  He also designed the InfoDock integrated tool
	framework for software engineers.  It runs atop XEmacs and is
	available from his firm, InfoDock Associates, which offers custom
	development and support packages for corporate users of XEmacs,
	GNU Emacs and InfoDock.  See \"http://www.infodock.com\".
	His interests include user interfaces, information management,
	CASE tools, communications and enterprise integration.")

	  (insert "\n\n\tClick ")
	  (about-xref "here" prev-page "Return to previous page")
	  (insert " to go back to the previous page.\n")
	  )

	 ((eq xref 'piper)
	  (about-face "Andy Piper" 'bold)
	  (insert " <andyp@parallax.co.uk>

	Author of the original \"fake\" XEmacs toolbar, and outl-mouse for
	mouse gesture based outlining.  Accomplished kludge contributor.")

	  (insert "\n\n\tClick ")
	  (about-xref "here" prev-page "Return to previous page")
	  (insert " to go back to the previous page.\n")
	  )

	 ((eq xref 'stig)
	  (about-face "Jonathan Stigelman" 'bold)
	  (insert " <stig@hackvan.com>

	Stig is sort of a tool fetishist.  He has a hate/love relationship
	with computers and he hacks on XEmacs because it's a good tool that
	makes computers somewhat less of a nuisance.  Besides XEmacs, Stig
	especially likes his Leatherman, his Makita, and his lockpicks.
	Stig wants a MIG welder and air tools.

	Stig likes to perch, hang from the ceiling, and climb on the walls.
	Stig has a cool van.  Stig would like to be able to telecommute
	from, say, the north rim of the Grand Canyon or the midst of Baja.")

	  (insert "\n\n\tClick ")
	  (about-xref "here" prev-page "Return to previous page")
	  (insert " to go back to the previous page.\n")
	  )

	 ((eq xref 'wmperry)
	  (about-face "William Perry" 'bold)
	  (insert " <wmperry@aventail.com>

	Author of Emacs-w3, the builtin web browser that comes with XEmacs,
	and various additions to the C code (e.g. the database support,
	the PNG support, some of the GIF/JPEG support, the strikethru
	face attribute support).

	He is currently working at Aventail, Corp. on SOCKS v5 servers.")

	  (insert "\n\n\tClick ")
	  (about-xref "here" prev-page "Return to previous page")
	  (insert " to go back to the previous page.\n")
	  )

	 ((eq xref 'kyle)
	  (about-face "Kyle Jones" 'bold)
	  (insert " <kyle_jones@wonderworks.com>

	Author of VM, a mail-reading package that is included in
	the standard XEmacs distribution, and contributor of many
	improvements and bug fixes.  Unlike RMAIL and MH-E, VM
	uses the standard UNIX mailbox format for its folders;
	thus, you can use VM concurrently with other UNIX mail
	readers such as Berkeley Mail and ELM.
	See \"http://www.wonderworks.com/kyle/\".")

	  (insert "\n\n\tClick ")
	  (about-xref "here" prev-page "Return to previous page")
	  (insert " to go back to the previous page.\n")
	  )

	 ((eq xref 'larsi)
	  (about-face "Lars Magne Ingebrigtsen" 'bold)
	  (insert " <larsi@ifi.uio.no>

	Author of Gnus the Usenet news and Mail reading package in
	the standard XEmacs distribution, and contributor of various
	enhancements and portability fixes.  Lars is a student at the
	Institute of Informatics at the University of Oslo.  He is
	currently plumbing away at his majors work at the Institute of
	Physics, working on an SCI project connected with CASCADE and
	CERN and stuff.
	See \"http://www.ifi.uio.no/~larsi/\".")

	  (insert "\n\n\tClick ")
	  (about-xref "here" prev-page "Return to previous page")
	  (insert " to go back to the previous page.\n")
	  )

	 ((eq xref 'others)
	  (insert "Click ")
	  (about-xref "here" 'about "Return to previous page")
	  (insert " to go back to the previous page\n\n\t")

	  (about-face "Other Contributors to XEmacs" 'italic)

	  (insert "

	Like most free software, XEmacs is a collaborative effort.
	These are some of the contributors; we have no doubt forgotten
	someone; we apologize!  You can see some of our faces further below.

	") (about-xref "Vladimir Ivanovic" 'vladimir "Find out more about Vladimir Ivanovic") (insert " <vladimir@mri.com>
	Former technical lead for XEmacs at Sun Microsystems.  He is
	now with Microtec Research Inc., working on embedded systems
	development tools.

	") (about-xref "Jonathan Stigelman" 'stig "Find out more about Jonathan Stigelman") (insert " <stig@hackvan.com>
	Peripatetic uninominal Emacs hacker.  Stig sometimes operates
	out of a big white van set up for nomadic living and hacking.
	Implemented the faster stay-up Lucid menus and hyper-apropos.
	Contributor of many dispersed improvements in the core Lisp code,
	and back-seat contributor for several of it's major packages.

	") (about-xref "Barry Warsaw" 'baw "Find out more about Barry Warsaw") (insert " <bwarsaw@cnri.reston.va.us>
	Author of cc-mode for C++, C, and Objective-C editing, and
	Supercite for mail and news citing.  Also various and sundry other
	Emacs utilities, fixes, enhancements and kludgery as whimsy,
	boredom, and ToT dictate (but not necessarily in that order).

	") (about-xref "Andy Piper" 'piper "Find out more about Andy Piper") (insert " <andyp@parallax.co.uk>
	Created the prototype for the toolbars.  Has been the first to make
	use of many of the new XEmacs graphics features.

	") (about-xref "Bob Weiner" 'bw "Find out more about Bob Weiner") (insert " <weiner@infodock.com>
	Author of the Hyperbole everyday information management
	hypertext system and the OO-Browser multi-language code
	browser.  He also designed the InfoDock integrated tool
	framework for software engineers.  It runs atop XEmacs and is
	available from his firm, InfoDock Associates, which offers custom
	development and support packages for corporate users of XEmacs,
	GNU Emacs and InfoDock.  See \"http://www.infodock.com\".
	His interests include user interfaces, information management,
	CASE tools, communications and enterprise integration.

	") (about-xref "William Perry" 'wmperry "Find out more about Bill Perry") (insert " <wmperry@aventail.com>
	Author of Emacs-w3, the builtin web browser that comes with XEmacs,
	and various additions to the C code (e.g. the database support,
	the PNG support, some of the GIF/JPEG support, the strikethru
	face attribute support).

	") (about-xref "Kyle Jones" 'kyle "Find out more about Kyle Jones") (insert " <kyle_jones@wonderworks.com>
	Author of VM, a mail-reading package that is included in
	the standard XEmacs distribution, and contributor of many
	improvements and bug fixes.  Unlike RMAIL and MH-E, VM
	uses the standard UNIX mailbox format for its folders;
	thus, you can use VM concurrently with other UNIX mail
	readers such as Berkeley Mail and ELM.
	See \"http://www.wonderworks.com/kyle/\"

	") (about-xref "Lars Magne Ingebrigtsen" 'larsi "Find out more about Lars Magne Ingebrigtsen") (insert " <larsi@ifi.uio.no>
	Author of Gnus the Usenet news and Mail reading package in
	the standard XEmacs distribution, and contributor of various
	enhancements and portability fixes.  Lars is a student at the
	Institute of Informatics at the University of Oslo.  He is
	currently plumbing away at his majors work at the Institute of
	Physics, working on an SCI project connected with CASCADE and
	CERN and stuff.
	See \"http://www.ifi.uio.no/~larsi/\"

	Darrell Kindred <Darrell.Kindred@cmu.edu>
	Unofficial maintainer of the xemacs-beta list of extant bugs
	and contributor of an extraordinary number of important bug
	fixes, many of them in areas that neither Chuck nor Ben were
	particularly enthusiastic about investigating.

	Eduardo Pelegri-Llopart <pelegri@eng.sun.com>
	Author of EOS, a package included in the standard XEmacs
	distribution that integrates XEmacs with the SPARCworks
	development environment from Sun.  Past lead for XEmacs at
	Sun; advocated the validity of using Epoch, and later Lemacs,
	at Sun through several early prototypes.

	Matthieu Devin <devin@rs.com>
	Part of the original (pre-19.0) Lucid Emacs development team.
	Matthieu wrote the initial Energize interface, designed the
	toolkit-independent Lucid Widget library, and fixed enough
	redisplay bugs to last a lifetime.  The features in Lucid
	Emacs were largely inspired by Matthieu's initial prototype of
	an Energize interface using Epoch.

	Harlan Sexton <hbs@odi.com>
	Part of the original (pre-19.0) Lucid Emacs development team.
	Harlan designed and implemented many of the low level data
	structures which are original to the Lucid version of Emacs,
	including extents and hash tables.

	Eric Benson <eb@kaleida.com>
	Part of the original (pre-19.0) Lucid Emacs development team.
	Eric played a big part in the design of many aspects of the
	system, including the new command loop and keymaps, fixed
	numerous bugs, and has been a reliable beta tester ever since.

	John Rose <john.rose@sun.com>
	Author of many extensions to the `extents' code, including
	the initial implementation of `duplicable' properties.

	Hans Muller <hmuller@eng.sun.com>
	Author of the code used to connect XEmacs with ToolTalk, and
	of an early client of the external Emacs widget.

	David Moore <dmoore@UCSD.EDU>
	David has contributed greatly to the quest to speed up XEmacs.
	He is a student in the Computer Systems Laboratory at UCSD.
	When he manages to have free time, he usually spends it on 200
	mile bicycle rides, learning german or showing people the best
	mail & news environment he's found in 10 years.  (That'd be
	XEmacs, Gnus and bbdb, of course.)  He can be found at
	`druidmuck.egbt.org 4201' at various hours of the day.

	Hrvoje Niksic <hniksic@srce.hr>
	Hrvoje is currently a student at the Faculty of Electrical
	Engineering and Computing in Zagreb, Croatia.  He works part-
	time at SRCE, where he helps run the network machines.
	In his free time he is helping develop free software (especially
	XEmacs, as well as GNU software) and is writing his own -- he has
	written a small network mirroring utility Wget, see
	\"ftp://gnjilux.cc.fer.hr/pub/unix/util/wget/\".

	In addition to those just mentioned, the following people have
	spent a great deal of effort providing feedback, testing beta
	versions of XEmacs, providing patches to the source code, or
	doing all of the above.  We couldn't have done it without them.

	  Nagi M. Aboulenein <aboulene@ponder.csci.unt.edu>
	  Gary Adams <gra@zeppo.East.Sun.COM>
	  Gennady Agranov <agranov@csa.CS.Technion.Ac.IL>
	  Mark Allender <allender@vnet.IBM.COM>
	  Butch Anton <butch@zaphod.uchicago.edu>
	  Fred Appelman <Fred.Appelman@cv.ruu.nl>
	  Erik \"The Pope\" Arneson <lazarus@mind.net>
	  Tor Arntsen <tor@spacetec.no>
	  Mike Battaglia <mbattagl@dsccc.com>
	  Neal Becker <neal@ctd.comsat.com>
	  Paul Bibilo <peb@delcam.com>
	  Jan Borchers <job@tk.uni-linz.ac.at>
	  Mark Borges <mdb@cdc.noaa.gov>
	  David P. Boswell <daveb@tau.space.thiokol.com>
	  Tim Bradshaw <tfb@edinburgh.ac.uk>
	  Rick Braumoeller <rickb@mti.sgi.com>
	  Matthew J. Brown <mjb@doc.ic.ac.uk>
	  Alastair Burt <burt@dfki.uni-kl.de>
	  Rick Campbell <rickc@lehman.com>
	  Richard Caley <rjc@cstr.edinburgh.ac.uk>
	  Stephen Carney <carney@gvc.dec.com>
	  Philippe Charton <charton@lmd.ens.fr>
	  Peter Cheng <peter.cheng@sun.com>
	  Jin S. Choi <jin@atype.com>
	  Tomasz J. Cholewo <tjchol01@mecca.spd.louisville.edu>
	  Serenella Ciongoli <czs00@ladybug.oes.amdahl.com>
	  Richard Cognot <cognot@ensg.u-nancy.fr>
	  Andy Cohen <cohen@andy.bu.edu>
	  Andrew J Cosgriff <ajc@bing.wattle.id.au>
	  Nick J. Crabtree <nickc@scopic.com>
	  Christopher Davis <ckd@kei.com>
	  Soren Dayton <csdayton@cs.uchicago.edu>
	  Michael Diers <mdiers@logware.de>
	  William G. Dubuque <wgd@martigny.ai.mit.edu>
	  Samuel J. Eaton <samuele@cogs.susx.ac.uk>
	  Carl Edman <cedman@Princeton.EDU>
	  Dave Edmondson <davided@sco.com>
	  Jonathan Edwards <edwards@intranet.com>
	  Eric Eide <eeide@asylum.cs.utah.edu>
	  EKR <ekr@terisa.com>
	  Oscar Figueiredo <Oscar.Figueiredo@di.epfl.ch>
	  David Fletcher <frodo@tsunami.com>
	  Paul Flinders <ptf@delcam.co.uk>
	  Jered J Floyd <jered@mit.edu>
	  Jerry Frain <jerry@sneffels.tivoli.com>
	  Benjamin Fried <bf@morgan.com>
	  Barry Friedman <friedman@bnr.ca>
	  Lew Gaiter III <lew@StarFire.com>
	  Itay Gat <itay@cs.huji.ac.il>
	  Tim Geisler <Tim.Geisler@informatik.uni-muenchen.de>
	  Dave Gillespie <daveg@synaptics.com>
	  Christian F. Goetze <cg@bigbook.com>
	  Wolfgang Grieskamp <wg@cs.tu-berlin.de>
	  James Grinter <jrg@demon.net>
	  Ben Gross <bgross@uiuc.edu>
	  Dirk Grunwald <grunwald@foobar.cs.Colorado.EDU>
	  Dipankar Gupta <dg@hplb.hpl.hp.com>
	  Markus Gutschke <gutschk@GOEDEL.UNI-MUENSTER.DE>
	  Adam Hammer <hammer@cs.purdue.edu>
	  Magnus Hammerin <magnush@epact.se>
	  ChangGil Han <cghan@phys401.phys.pusan.ac.kr>
	  Derek Harding <dharding@lssec.bt.co.uk>
	  Michael Harnois <mharnois@sbt.net>
	  John Haxby <J.Haxby@isode.com>
	  Jareth \"JHod\" Hein <jhod@po.iijnet.or.jp>
	  Benedikt Heinen <beh@icemark.thenet.ch>
	  Stephan Herrmann <sh@first.gmd.de>
	  Charles Hines <chuck_hines@VNET.IBM.COM>
	  David Hughes <djh@harston.cv.com>
	  Andrew Innes <andrewi@harlequin.co.uk>
	  Markku Jarvinen <Markku.Jarvinen@simpukka.funet.fi>
	  Robin Jeffries <robin.jeffries@sun.com>
	  Philip Johnson <johnson@uhics.ics.Hawaii.Edu>
	  J. Kean Johnston <jkj@paradigm-sa.com>
	  Andreas Kaempf <andreas@sccon.com>
	  Doug Keller <dkeller@vnet.ibm.com>
	  Hunter Kelly <retnuh@corona>
	  Gregor Kennedy <gregork@dadd.ti.com>
	  Michael Kifer <kifer@cs.sunysb.edu>
	  Yasuhiko Kiuchi <kiuchi@dsp.ksp.fujixerox.co.jp>
	  Greg Klanderman <greg@alphatech.com>
	  Valdis Kletnieks <Valdis.Kletnieks@vt.edu>
	  Jens Krinke <krinke@ips.cs.tu-bs.de>
	  Mats Larsson <Mats.Larsson@uab.ericsson.se>
	  Jens Lautenbacher <jens@lemcbed.lem.uni-karlsruhe.de>
	  Simon Leinen <simon@instrumatic.ch>
	  Carsten Leonhardt <leo@arioch.tng.oche.de>
	  James LewisMoss <moss@cs.sc.edu>
	  Mats Lidell <mats.lidell@contactor.se>
	  Matt Liggett <mliggett@seven.ucs.indiana.edu>
	  Christian Limpach <Christian.Limpach@nice.ch>
	  Robert Lipe <robertl@arnet.com>
	  Damon Lipparelli <lipp@aa.net>
	  Hamish Macdonald <hamish@bnr.ca>
	  Ian MacKinnon <imackinnon@telia.co.uk>
	  Patrick MacRoberts <macro@hpcobr30.cup.hp.com>
	  Tonny Madsen <Tonny.Madsen@netman.dk>
	  Ketil Z Malde <ketil@ii.uib.no>
	  Steve March <smarch@quaver.urbana.mcd.mot.com>
	  Pekka Marjola <pema@iki.fi>
	  Simon Marshall <simon@gnu.ai.mit.edu>
	  Dave Mason <dmason@plg.uwaterloo.ca>
	  Jaye Mathisen <mrcpu@cdsnet.net>
	  Michael Meissner <meissner@osf.org>
	  David M. Meyer <meyer@ns.uoregon.edu>
	  Brad Miller <bmiller@cs.umn.edu>
	  Jeff Miller <jmiller@bay1.bayserve.net>
	  John Morey <jmorey@crl.com>
	  Rob Mori <rob.mori@sun.com>
	  Heiko Muenkel <muenkel@tnt.uni-hannover.de>
	  Arup Mukherjee <arup+@cs.cmu.edu>
	  Colas Nahaboo <Colas.Nahaboo@sophia.inria.fr>
	  Lynn D. Newton <lynn@ives.phx.mcd.mot.com>
	  Casey Nielson <knielson@joule.elee.calpoly.edu>
	  Georg Nikodym <Georg.Nikodym@canada.sun.com>
	  Andy Norman <ange@hplb.hpl.hp.com>
	  Joseph J. Nuspl Jr. <nuspl@cc.purdue.edu>
	  Kim Nyberg <kny@tekla.fi>
	  David Ofelt <ofelt@getalife.Stanford.EDU>
	  Tore Olsen <toreo@colargol.idb.hist.no>
	  Greg Onufer <Greg.Onufer@eng.sun.com>
	  Achim Oppelt <aoppelt@theorie3.physik.uni-erlangen.de>
	  Sudeep Kumar Palat <palat@idt.unit.no>
	  Marc Paquette <Marc.Paquette@Softimage.com>
	  Jens-U H Petersen <petersen@kurims.kyoto-u.ac.jp>
	  Joel Peterson <tarzan@aosi.com>
	  Thomas A. Peterson <tap@src.honeywell.com>
	  Peter Pezaris <pez@dwwc.com>
	  Tibor Polgar <tlp00@eng.amdahl.com>
	  Frederic Poncin <fp@info.ucl.ac.be>
	  E. Rehmi Post <rehmi@asylum.sf.ca.us>
	  Colin Rafferty <craffert@spspme.ml.com>
	  Paul M Reilly <pmr@pajato.com>
	  Jack Repenning <jackr@sgi.com>
	  Daniel Rich <drich@cisco.com>
	  Roland Rieke <rol@darmstadt.gmd.de>
	  Russell Ritchie <ritchier@msc.ie>
	  Roland <rol@darmstadt.gmd.de>
	  Mike Russell <mjruss@rchland.vnet.ibm.com>
	  Jan Sandquist <etxquist@iqa.ericsson.se>
	  Marty Sasaki <sasaki@spdcc.com>
	  Mike Scheidler <c23mts@eng.delcoelect.com>
	  Darrel Schneider <darrel@slc.com>
	  Hayden Schultz <haydens@ll.mit.edu>
	  Cotton Seed <cottons@cybercom.net>
	  Axel Seibert <seiberta@informatik.tu-muenchen.de>
	  Odd-Magne Sekkingstad <oddms@ii.uib.no>
	  Vinnie Shelton <shelton@icd.teradyne.com>
	  John Shen <zfs60@cas.org>
	  Murata Shuuichirou <mrt@mickey.ai.kyutech.ac.jp>
	  Jeffrey Sparkes <jsparkes@bnr.ca>
	  Michael Sperber <sperber@informatik.uni-tuebingen.de>
	  Manoj Srivastava <srivasta@pilgrim.umass.edu>
	  Francois Staes <frans@kiwi.uia.ac.be>
	  Jason Stewart <jasons@cs.unm.edu>
	  Rick Tait <rickt@gnu.ai.mit.edu>
	  James Thompson <thompson@wg2.waii.com>
	  Morioka Tomohiko <morioka@jaist.ac.jp>
	  Raymond L. Toy <toy@rtp.ericsson.se>
	  John Turner <turner@xdiv.lanl.gov>
	  Juan E. Villacis <jvillaci@cs.indiana.edu>
	  Vladimir Vukicevic <vladimir@intrepid.com>
	  Peter Ware <ware@cis.ohio-state.edu>
	  Yoav Weiss <yoav@zeus.datasrv.co.il>
	  Rod Whitby <rwhitby@asc.corp.mot.com>
	  Rich Williams <rdw@hplb.hpl.hp.com>
	  David C Worenklein <dcw@gcm.com>
	  Takeshi Yamada <yamada@sylvie.kecl.ntt.jp>
	  Jason Yanowitz <yanowitz@eternity.cs.umass.edu>
	  La Monte Yarroll <piggy@hilbert.maths.utas.edu.au>
	  Blair Zajac <blair@olympia.gps.caltech.edu>
	  Daniel Zivkovic <daniel@canada.sun.com>
	  Karel Zuiderveld <Karel.Zuiderveld@cv.ruu.nl>
	  and the makers of Jolt Cola (tm)")
	  (goto-char (point-min))
	  (while (re-search-forward "^[ \t]*\\([^<>\n]+\\) <[^>\n]+>$"
				    nil t)
	    (set-extent-face (make-extent (match-beginning 1) (match-end 1))
			     'bold))
	  (goto-char (point-min))
	  (while (re-search-forward "^[ \t]*<\\([^>\n]+\\)>$" nil t)
	    (set-extent-face (make-extent (match-beginning 1) (match-end 1))
			     'bold))

	  (goto-char (point-max))
	  (insert "\n")
	  (about-add-mosaic)
	  (goto-char (point-max))
	  (insert "\n\n\tClick ")
	  (about-xref "here" 'about "Return to previous page")
	  (insert " to go back to the previous page.\n")
	  )
	 ((eq xref 'features)
	  (insert "Click ")
	  (about-xref "here" 'about "Return to previous page")
	  (insert " to go back to the previous page\n\n\t")

	  (about-face "New Features in XEmacs" 'bold-italic)

	  (insert "\n
	* MULE (Multi-Lingual Emacs) support.  Display of multiple
	  simultaneous character sets is possible.
	* Text for complex languages can be entered using the XIM mechanism.
	* Localization of menubar text for the Japanese locale.
	* A real toolbar.
	* Proper integration with Xt and Motif (including Motif menubars
	  and scrollbars).  Motif look-alike menubars and scrollbars
	  are provided for those systems without real Motif support.
	* Face support on TTY's, including color.
	* Horizontal and vertical scrollbars in all windows.
	* Support for variable-width and variable height fonts.
	* Support for display on multiple simultaneous X and/or TTY devices.
	* Support for arbitrary pixmaps in a buffer.
	* Access to the ToolTalk API.
	* Support for using XEmacs frames as Xt widgets.
	* Support for overlapping regions (or extents) and efficient handling
	  of a large number of such extents in a single buffer.
	* Powerful, flexible control over the display characteristics
	  of most of the visual aspects of XEmacs through the use
	  of specifiers, which allow separate values to be specified
	  for individual buffers, windows, frames, devices, device classes,
	  and device types.
	* A clean interface to the menubar, window-system events, and key
	  combinations."))
	 ))
      (goto-char (point-min))
      ))))
