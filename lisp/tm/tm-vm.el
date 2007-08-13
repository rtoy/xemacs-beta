;;; tm-vm.el --- tm-MUA (MIME Extension module) for VM

;; Copyright (C) 1994,1995,1996 Free Software Foundation, Inc.

;; Author: MASUTANI Yasuhiro <masutani@me.es.osaka-u.ac.jp>
;;         Kenji Wakamiya <wkenji@flab.fujitsu.co.jp>
;;         MORIOKA Tomohiko <morioka@jaist.ac.jp>
;;         Shuhei KOBAYASHI <shuhei-k@jaist.ac.jp>
;;         Oscar Figueiredo <Oscar.Figueiredo@di.epfl.ch>
;; Maintainer: Oscar Figueiredo <Oscar.Figueiredo@di.epfl.ch>
;; Created: 1994/10/29
;; Version: $Revision: 1.7 $
;; Keywords: mail, MIME, multimedia, multilingual, encoded-word

;; This file is part of tm (Tools for MIME).

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;;      Plese insert `(require 'tm-vm)' in your ~/.vm file.

;;; Code:

(eval-when-compile
  (require 'tm-mail)
  (require 'vm)
  (require 'vm-window))

(require 'tm-edit)
(require 'tm-view)
(require 'vm-reply)
(require 'vm-summary)
(require 'vm-menu)
(require 'vm-toolbar)
(require 'vm-mime)

;;; @ Variables

;;; @@ User customization variables

(defvar tm-vm/use-vm-bindings t
  "*If t, use VM compatible keybindings in MIME Preview buffers. 
Otherwise TM generic bindings for content extraction/playing are 
made available.")
 
(defvar tm-vm/attach-to-popup-menus t
  "*If t append MIME specific commands to VM's popup menus.")

(defvar tm-vm/use-original-url-button t
  "*If it is t, use original URL button instead of tm's.")

(defvar tm-vm/automatic-mime-preview (or (and (boundp 'vm-display-using-mime)
					      vm-display-using-mime)
					 t)
  "*If non-nil, automatically process and show MIME messages.")

(defvar tm-vm/strict-mime t
  "*If nil, do MIME processing even if there is no MIME-Version field.")

(defvar tm-vm/use-ps-print (not (featurep 'mule))
  "*Use Postscript printing (ps-print) to print MIME messages.")

(defvar tm-vm-load-hook nil
  "*List of functions called after tm-vm is loaded.")

(defvar tm-vm/select-message-hook nil
  "*List of functions called every time a message is selected.
tm-vm uses `vm-select-message-hook', use tm-vm/select-message-hook instead.
When the hooks are run current buffer is either VM folder buffer with
the current message delimited by (point-min) and (point-max) or the MIME
Preview buffer.")

(defvar tm-vm/forward-message-hook vm-forward-message-hook
  "*List of functions called after a Mail mode buffer has been
created to forward a message in message/rfc822 type format.
If `vm-forwarding-digest-type' is \"rfc1521\", tm-vm runs this
hook instead of `vm-forward-message-hook'.")

(defvar tm-vm/send-digest-hook nil
  "*List of functions called after a Mail mode buffer has been
created to send a digest in multipart/digest type format.
If `vm-digest-send-type' is \"rfc1521\", tm-vm runs this hook
instead of `vm-send-digest-hook'.")

(defvar tm-vm/build-mime-preview-buffer-hook nil
  "*List of functions called each time a MIME Preview buffer is built.
These hooks are run in the MIME-Preview buffer.")

;;; @@ System/Information variables

(defconst tm-vm/RCS-ID
  "$Id: tm-vm.el,v 1.7 1997/06/21 20:03:15 steve Exp $")
(defconst tm-vm/version (get-version-string tm-vm/RCS-ID))

; Ensure vm-menu-mail-menu gets properly defined *before* tm-vm/vm-emulation-map
; since it contains a call to vm-menu-initialize-vm-mode-menu-map
(setq vm-menu-mail-menu
  (let ((title (if (vm-menu-fsfemacs-menus-p)
		   (list "Mail Commands"
			 "Mail Commands"
			 "---"
			 "---")
		 (list "Mail Commands"))))
    (append
     title
     (list ["Send and Exit" vm-mail-send-and-exit (vm-menu-can-send-mail-p)]
	   ["Send, Keep Composing" vm-mail-send (vm-menu-can-send-mail-p)]
	   ["Cancel" kill-buffer t]
	   "----"
	   "Go to Field:"
	   "----"
	   ["      To:" mail-to t]
	   ["      Subject:" mail-subject	t]
	   ["      CC:" mail-cc t]
	   ["      BCC:" mail-bcc t]
	   ["      Reply-To:" mail-reply-to t]
	   ["      Text" mail-text t]
	   "----"
	   ["Yank Original" vm-menu-yank-original vm-reply-list]
	   ["Fill Yanked Message" mail-fill-yanked-message t]
	   ["Insert Signature"	mail-signature t]
	   ["Insert File..." insert-file t]
	   ["Insert Buffer..."	insert-buffer t])
     (if tm-vm/attach-to-popup-menus
	 (list "----"
	       (cons "MIME Commands" 
		     (mapcar (function (lambda (item)
					 (vector (nth 1 item)
						 (nth 2 item)
						 t)))
			     mime-editor/menu-list))))
     )))

(defvar tm-vm/vm-emulation-map
  (let ((map (make-sparse-keymap)))
    (define-key map "h" 'vm-summarize)
    ;(define-key map "\M-n" 'vm-next-unread-message)
    ;(define-key map "\M-p" 'vm-previous-unread-message)
    (define-key map "n" 'vm-next-message)
    (define-key map "p" 'vm-previous-message)
    (define-key map "N" 'vm-next-message-no-skip)
    (define-key map "P" 'vm-previous-message-no-skip)
    ;(define-key map "\C-\M-n" 'vm-move-message-forward)
    ;(define-key map "\C-\M-p" 'vm-move-message-backward)
    ;(define-key map "\t" 'vm-goto-message-last-seen)
    ;(define-key map "\r" 'vm-goto-message)
    (define-key map "^" 'vm-goto-parent-message)
    (define-key map "t" 'vm-expose-hidden-headers)
    (define-key map " " 'vm-scroll-forward)
    (define-key map "b" 'vm-scroll-backward)
    (define-key map "\C-?" 'vm-scroll-backward)
    (define-key map "d" 'vm-delete-message)
    (define-key map "\C-d" 'vm-delete-message-backward)
    (define-key map "u" 'vm-undelete-message)
    (define-key map "U" 'vm-unread-message)
    (define-key map "e" 'vm-edit-message)
    ;(define-key map "a" 'vm-set-message-attributes)
    ;(define-key map "j" 'vm-discard-cached-data)
    ;(define-key map "k" 'vm-kill-subject)
    (define-key map "f" 'vm-followup)
    (define-key map "F" 'vm-followup-include-text)
    (define-key map "r" 'vm-reply)
    (define-key map "R" 'vm-reply-include-text)
    (define-key map "\M-r" 'vm-resend-bounced-message)
    (define-key map "B" 'vm-resend-message)
    (define-key map "z" 'vm-forward-message)
    ;(define-key map "c" 'vm-continue-composing-message)
    (define-key map "@" 'vm-send-digest)
    ;(define-key map "*" 'vm-burst-digest)
    (define-key map "m" 'vm-mail)
    (define-key map "g" 'vm-get-new-mail)
    ;(define-key map "G" 'vm-sort-messages)
    (define-key map "v" 'vm-visit-folder)
    (define-key map "s" 'vm-save-message)
    ;(define-key map "w" 'vm-save-message-sans-headers)
    ;(define-key map "A" 'vm-auto-archive-messages)
    (define-key map "S" 'vm-save-folder)
    ;(define-key map "|" 'vm-pipe-message-to-command)
    (define-key map "#" 'vm-expunge-folder)
    (define-key map "q" 'vm-quit)
    (define-key map "x" 'vm-quit-no-change)
    (define-key map "i" 'vm-iconify-frame)
    (define-key map "?" 'vm-help)
    (define-key map "\C-_" 'vm-undo)
    (define-key map "\C-xu" 'vm-undo)
    (define-key map "!" 'shell-command)
    (define-key map "<" 'vm-beginning-of-message)
    (define-key map ">" 'vm-end-of-message)
    ;(define-key map "\M-s" 'vm-isearch-forward)
    (define-key map "=" 'vm-summarize)
    (define-key map "L" 'vm-load-init-file)
    ;(define-key map "l" (make-sparse-keymap))
    ;(define-key map "la" 'vm-add-message-labels)
    ;(define-key map "ld" 'vm-delete-message-labels)
    ;(define-key map "V" (make-sparse-keymap))
    ;(define-key map "VV" 'vm-visit-virtual-folder)
    ;(define-key map "VC" 'vm-create-virtual-folder)
    ;(define-key map "VA" 'vm-apply-virtual-folder)
    ;(define-key map "VM" 'vm-toggle-virtual-mirror)
    ;(define-key map "V?" 'vm-virtual-help)
    ;(define-key map "M" (make-sparse-keymap))
    ;(define-key map "MN" 'vm-next-command-uses-marks)
    ;(define-key map "Mn" 'vm-next-command-uses-marks)
    ;(define-key map "MM" 'vm-mark-message) 
    ;(define-key map "MU" 'vm-unmark-message)
    ;(define-key map "Mm" 'vm-mark-all-messages)
    ;(define-key map "Mu" 'vm-clear-all-marks)
    ;(define-key map "MC" 'vm-mark-matching-messages)
    ;(define-key map "Mc" 'vm-unmark-matching-messages)
    ;(define-key map "MT" 'vm-mark-thread-subtree)
    ;(define-key map "Mt" 'vm-unmark-thread-subtree)
    ;(define-key map "MS" 'vm-mark-messages-same-subject)
    ;(define-key map "Ms" 'vm-unmark-messages-same-subject)
    ;(define-key map "MA" 'vm-mark-messages-same-author)
    ;(define-key map "Ma" 'vm-unmark-messages-same-author)
    ;(define-key map "M?" 'vm-mark-help)
    ;(define-key map "W" (make-sparse-keymap))
    ;(define-key map "WW" 'vm-apply-window-configuration)
    ;(define-key map "WS" 'vm-save-window-configuration)
    ;(define-key map "WD" 'vm-delete-window-configuration)
    ;(define-key map "W?" 'vm-window-help)
    (define-key map "\C-t" 'vm-toggle-threads-display)
    (define-key map "\C-x\C-s" 'vm-save-buffer)
    (define-key map "\C-x\C-w" 'vm-write-file)
    (define-key map "\C-x\C-q" 'vm-toggle-read-only)
    ;(define-key map "%" 'vm-change-folder-type)
    (define-key map "\M-C" 'vm-show-copying-restrictions)
    (define-key map "\M-W" 'vm-show-no-warranty)
    ;; suppress-keymap provides these, but now that we don't use
    ;; suppress-keymap anymore...
    (define-key map "0" 'digit-argument)
    (define-key map "1" 'digit-argument)
    (define-key map "2" 'digit-argument)
    (define-key map "3" 'digit-argument)
    (define-key map "4" 'digit-argument)
    (define-key map "5" 'digit-argument)
    (define-key map "6" 'digit-argument)
    (define-key map "7" 'digit-argument)
    (define-key map "8" 'digit-argument)
    (define-key map "9" 'digit-argument)
    (define-key map "-" 'negative-argument)
    (if mouse-button-2
	(define-key map mouse-button-2 (function tm:button-dispatcher)))
    (if (vm-menu-fsfemacs-menus-p)
	(progn
	  (vm-menu-initialize-vm-mode-menu-map)
	  (define-key map [menu-bar]
	    (lookup-key vm-mode-menu-map [rootmenu vm]))))
    map)
  "VM emulation keymap for MIME-Preview buffers.")

(defvar tm-vm/popup-menu 
  (let (fsfmenu
	(dummy (make-sparse-keymap))
	(menu (append vm-menu-dispose-menu
		      (list "----" 
			    (cons mime-viewer/menu-title
				  (mapcar (function
					   (lambda (item)
					     (vector (nth 1 item)(nth 2 item) t)))
					  mime-viewer/menu-list))))))
    (if running-xemacs
	menu
      (vm-easy-menu-define fsfmenu (list dummy) nil menu)
      fsfmenu))
  "VM's popup menu + MIME specific commands")



(define-key vm-mode-map "Z" 'tm-vm/view-message)
(define-key vm-mode-map "T" 'tm-vm/decode-message-header)
(define-key vm-mode-map "\et" 'tm-vm/toggle-preview-mode)

; Disable VM 6 built-in MIME handling
(setq vm-display-using-mime nil
      vm-send-using-mime nil)

;;; @ MIME encoded-words

(defvar tm-vm/use-tm-patch nil
  "Does not decode encoded-words in summary buffer if it is t.
If you use tiny-mime patch for VM (by RIKITAKE Kenji
<kenji@reseau.toyonaka.osaka.jp>), please set it t [tm-vm.el]")

(or tm-vm/use-tm-patch
    (progn
(defadvice vm-compile-format (around tm activate)
  "MIME decoding support through TM added."
  (let ((vm-display-using-mime t))
    ad-do-it))

(defadvice vm-tokenized-summary-insert (around tm activate)
  "MIME decoding support through TM added."
  (let ((vm-display-using-mime t))
    ad-do-it))

(fset 'vm-decode-mime-encoded-words-in-string 'mime-eword/decode-string)
(fset 'vm-reencode-mime-encoded-words-in-string 'mime-eword/encode-string)

))

(defun tm-vm/decode-message-header (&optional count)
  "Decode MIME header of current message.
Numeric prefix argument COUNT means to decode the current message plus
the next COUNT-1 messages.  A negative COUNT means decode the current
message and the previous COUNT-1 messages.
When invoked on marked messages (via vm-next-command-uses-marks),
all marked messages are affected, other messages are ignored."
  (interactive "p")
  (or count (setq count 1))
  (vm-follow-summary-cursor)
  (vm-select-folder-buffer)
  (vm-check-for-killed-summary)
  (vm-error-if-folder-empty)
  (vm-error-if-folder-read-only)
  (let ((mlist (vm-select-marked-or-prefixed-messages count))
        (realm nil)
        (vlist nil)
        (vbufs nil))
    (save-excursion
      (while mlist
        (setq realm (vm-real-message-of (car mlist)))
        ;; Go to real folder of this message.
        ;; But maybe this message is already real message...
        (set-buffer (vm-buffer-of realm))
        (let ((buffer-read-only nil))
          (vm-save-restriction
           (narrow-to-region (vm-headers-of realm) (vm-text-of realm))
           (mime/decode-message-header))
          (let ((vm-message-pointer (list realm))
                (last-command nil))
            (vm-discard-cached-data))
          ;; Mark each virtual and real message for later summary
          ;; update.
          (setq vlist (cons realm (vm-virtual-messages-of realm)))
          (while vlist
            (vm-mark-for-summary-update (car vlist))
            ;; Remember virtual and real folders related this message,
            ;; for later display update.
            (or (memq (vm-buffer-of (car vlist)) vbufs)
                (setq vbufs (cons (vm-buffer-of (car vlist)) vbufs)))
            (setq vlist (cdr vlist)))
          (if (eq vm-flush-interval t)
              (vm-stuff-virtual-attributes realm)
            (vm-set-modflag-of realm t)))
        (setq mlist (cdr mlist)))
      ;; Update mail-buffers and summaries.
      (while vbufs
        (set-buffer (car vbufs))
        (vm-preview-current-message)
        (setq vbufs (cdr vbufs))))))

(defun tm-vm/header-filter ()
  "Filter headers in current buffer according to vm-visible-headers and vm-invisible-header-regexp.
Current buffer is assumed to have a message-like structure."
  (goto-char (point-min))
  (let ((visible-headers vm-visible-headers))
    (if (or vm-use-lucid-highlighting
	    vm-display-xfaces)
	(setq visible-headers (cons "X-Face:" vm-visible-headers)))
    (vm-reorder-message-headers nil
				visible-headers
				vm-invisible-header-regexp)
    (mime/decode-message-header)))

(setq mime-viewer/content-header-filter-alist 
      (append '((vm-mode . tm-vm/header-filter)
                (vm-virtual-mode . tm-vm/header-filter)) 
              mime-viewer/content-header-filter-alist))



;;; @ MIME Viewer

(setq mime-viewer/code-converter-alist 
      (append
       (list (cons 'vm-mode 'mime-charset/decode-buffer)
	     (cons 'vm-virtual-mode 'mime-charset/decode-buffer))
       mime-viewer/code-converter-alist))

;;; @@ MIME-Preview buffer management

(defvar tm-vm/system-state nil)

(defun tm-vm/system-state ()
  (save-excursion
    (if mime::preview/article-buffer
        (set-buffer mime::preview/article-buffer)
      (vm-select-folder-buffer))
    tm-vm/system-state))

(defun tm-vm/build-preview-buffer ()
  "Build the MIME Preview buffer for the current VM message. 
Current buffer should be VM's folder buffer."

  (set (make-local-variable 'tm-vm/system-state) 'mime-viewing)
  (setq vm-system-state 'reading)

  ;; Update message flags and store them in folder buffer before 
  ;; entering MIME viewer
  (tm-vm/update-message-status)

  ;; We need to save window configuration because we may be working 
  ;; in summary window
  (save-window-excursion
    (save-restriction
      (save-excursion
	(widen)
	(goto-char (vm-start-of (car vm-message-pointer)))
	(forward-line)
	(narrow-to-region (point)
			  (vm-end-of (car vm-message-pointer)))
    
	(let ((ml vm-message-list)
	      (mp vm-message-pointer))
	  (mime/viewer-mode nil nil nil nil nil nil)
	  (setq vm-mail-buffer mime::preview/article-buffer)
	  (setq vm-message-list ml
		vm-message-pointer mp))
	;; Install VM toolbar for MIME-Preview buffer if not installed
	(tm-vm/check-for-toolbar)
	(if tm-vm/use-vm-bindings
	    (progn 
	      (define-key tm-vm/vm-emulation-map "\C-c" (current-local-map))
	      (use-local-map tm-vm/vm-emulation-map)
	      (vm-menu-install-menubar)
	      (if (and vm-use-menus
		       (vm-menu-support-possible-p))
		  (setq mode-popup-menu tm-vm/popup-menu))))

	;; Highlight message (and display XFace if supported)
	(if (or vm-highlighted-header-regexp
		(and running-xemacs vm-use-lucid-highlighting))
	    (vm-highlight-headers))
	;; Display XFaces with VM internal support if appropriate
	(if (and vm-display-xfaces
		 running-xemacs
		 (vm-multiple-frames-possible-p)
		 (featurep 'xface))
	    (let ((highlight-headers-hack-x-face-p t)
		  (highlight-headers-regexp nil)
		  (highlight-headers-citation-regexp nil)
		  (highlight-headers-citation-header-regexp nil))
	      (highlight-headers (point-min) (point-max) t)))
        ;; Energize URLs and buttons
	(if (and tm-vm/use-original-url-button
		 vm-use-menus (vm-menu-support-possible-p))
	    (progn (vm-energize-headers)
		   (vm-energize-urls)))
	(run-hooks 'tm-vm/build-mime-preview-buffer-hook)
	))))

(defun tm-vm/sync-preview-buffer ()
  "Ensure that the MIME preview buffer, if it exists, actually corresponds to the current message. 
If no MIME Preview buffer is needed then kill it. If no
MIME Preview buffer exists nothing is done."
  ;; Current buffer should be message buffer when calling this function
  (let* ((mbuf (current-buffer))
         (pbuf (and mime::article/preview-buffer
                    (get-buffer mime::article/preview-buffer))))
    (if pbuf
	;; A MIME Preview buffer exists then it may need to be synch'ed
	(save-excursion
	  (set-buffer mbuf)
	  (if (and tm-vm/strict-mime
		   (not (vm-get-header-contents (car vm-message-pointer)
						"MIME-Version:")))
	      (progn
		(setq mime::article/preview-buffer nil
		      tm-vm/system-state nil)
		(if pbuf (kill-buffer pbuf)))
	    (tm-vm/build-preview-buffer)))
          ;; Return to previous frame
          )))

(defun tm-vm/toggle-preview-mode ()
  "Toggle automatic MIME preview on or off. 
In automatic MIME Preview mode each newly selected article is MIME processed if
it has MIME content without need for an explicit request from the user. This
behaviour is controlled by the variable tm-vm/automatic-mime-preview."

  (interactive)
  (if tm-vm/automatic-mime-preview
      (progn
        (tm-vm/quit-view-message)
        (setq tm-vm/automatic-mime-preview nil)
        (message "Automatic MIME Preview is now disabled."))
    ;; Enable Automatic MIME Preview
    (tm-vm/view-message)
    (setq tm-vm/automatic-mime-preview t)
    (message "Automatic MIME Preview is now enabled.")
    ))

;;; @@ Display functions

(defun tm-vm/update-message-status ()
  "Update current message display and summary. 
Remove 'unread' and 'new' flags.  The MIME Preview buffer is not displayed,
tm-vm/display-preview-buffer should be called for that. This function is
display-configuration safe."
  (if mime::preview/article-buffer
      (set-buffer mime::preview/article-buffer)
    (vm-select-folder-buffer))
  (if (or (and mime::article/preview-buffer
	       (get-buffer mime::article/preview-buffer)
	       (vm-get-visible-buffer-window mime::article/preview-buffer))
	  (vm-get-visible-buffer-window (current-buffer)))
      (progn
        (if (vm-new-flag (car vm-message-pointer))
            (vm-set-new-flag (car vm-message-pointer) nil))
        (if (vm-unread-flag (car vm-message-pointer))
            (vm-set-unread-flag (car vm-message-pointer) nil))
        (vm-update-summary-and-mode-line)
        (tm-vm/howl-if-eom))
    (vm-update-summary-and-mode-line)))

(defun tm-vm/display-preview-buffer ()
  "Replace the VM message buffer with the MIME-Preview buffer if the VM message buffer is currently displayed or undisplay it if tm-vm/system-state is nil."
  (let* ((mbuf (current-buffer))
         (mwin (vm-get-visible-buffer-window mbuf))
         (pbuf (and mime::article/preview-buffer
                    (get-buffer mime::article/preview-buffer)))
         (pwin (and pbuf (vm-get-visible-buffer-window pbuf)))) 
    (if (and pbuf (tm-vm/system-state))
        ;; display preview buffer if preview-buffer exists
        (cond
         ((and mwin pwin)
          (vm-undisplay-buffer mbuf)
          (tm-vm/update-message-status))
         ((and mwin (not pwin))
          (set-window-buffer mwin pbuf)
          (tm-vm/update-message-status))
         (pwin
          (tm-vm/update-message-status))
         (t
          ;; don't display if neither mwin nor pwin was displayed before.
          ))
      ;; display folder buffer
      (cond
       ((and mwin pwin)
        (vm-undisplay-buffer pbuf))
       ((and (not mwin) pwin)
        (set-window-buffer pwin mbuf))
       (mwin
        ;; folder buffer is already displayed.
        )
       (t
        ;; don't display if neither mwin nor pwin was displayed before.
        )))))

(defun tm-vm/preview-current-message ()
  "Either preview message (view first lines only) or MIME-Preview it.
The message is previewed if message previewing is enabled see vm-preview-lines.
If not, MIME-Preview current message (ie. parse MIME
contents and display appropriately) if it has MIME contents and
tm-vm/automatic-mime-preview is non nil. Installed on vm-visit-folder-hook and
vm-select-message-hook."
  ;; assumed current buffer is folder buffer.
  (setq tm-vm/system-state nil)
  (if (get-buffer mime/output-buffer-name)
      (vm-undisplay-buffer mime/output-buffer-name))
  (if (and vm-message-pointer
	   tm-vm/automatic-mime-preview
	   (or (null vm-preview-lines)
	       (not (eq vm-system-state 'previewing))
	       (and (not vm-preview-read-messages)
		    (not (vm-new-flag (car vm-message-pointer)))
		    (not (vm-unread-flag (car vm-message-pointer))))))
      (if (or (not tm-vm/strict-mime)
              (vm-get-header-contents (car vm-message-pointer)
                                      "MIME-Version:"))
          ;; do MIME processing.
	  (progn 
	    (tm-vm/build-preview-buffer)
	    (save-excursion
	      (set-buffer mime::article/preview-buffer)
	      (run-hooks 'tm-vm/select-message-hook)))
        ;; don't do MIME processing. decode header only.
        (let (buffer-read-only)
          (mime/decode-message-header)
	  (run-hooks 'tm-vm/select-message-hook))
        )
    ;; don't preview; do nothing.
    (run-hooks 'tm-vm/select-message-hook))
  (tm-vm/display-preview-buffer))

(defun tm-vm/view-message ()
  "Decode and view the current VM message as a MIME encoded message. 
A MIME Preview buffer using mime/viewer-mode is created.
See mime/viewer-mode for more information"
  (interactive)
  (vm-follow-summary-cursor)
  (vm-select-folder-buffer)
  (vm-check-for-killed-summary)
  (vm-error-if-folder-empty)
  (vm-display (current-buffer) t '(tm-vm/view-message 
                                   tm-vm/toggle-preview-mode)
              '(tm-vm/view-message reading-message))
  (let ((tm-vm/automatic-mime-preview t))
    (tm-vm/preview-current-message))
)

(defun tm-vm/quit-view-message ()
  "Quit MIME-Viewer and go back to normal VM. 
MIME Preview buffer is killed. This function is called by `mime-viewer/quit'
command via `mime-viewer/quitting-method-alist'."
  (if (get-buffer mime/output-buffer-name)
      (vm-undisplay-buffer mime/output-buffer-name))
  (vm-select-folder-buffer)
  (let* ((mbuf (current-buffer))
         (pbuf (and mime::article/preview-buffer
                    (get-buffer mime::article/preview-buffer)))
         (pwin (and pbuf (vm-get-visible-buffer-window pbuf))))
    (if pbuf (kill-buffer pbuf))
    (and pwin
         (select-window pwin)
         (switch-to-buffer mbuf)))
  (setq tm-vm/system-state nil)
  (vm-display (current-buffer) t (list this-command)
              (list 'reading-message)))

(add-hook 'vm-select-message-hook 'tm-vm/preview-current-message)
(add-hook 'vm-visit-folder-hook   'tm-vm/preview-current-message)





;;; @@ for tm-view

;;; based on vm-do-reply [vm-reply.el]
(defun tm-vm/do-reply (buf to-all include-text)
  (save-excursion
    (set-buffer buf)
    (let ((dir default-directory)
          to cc subject in-reply-to references newsgroups)
      (cond ((setq to
                   (let ((reply-to (std11-field-body "Reply-To")))
                     (if (vm-ignored-reply-to reply-to)
                         nil
                       reply-to))))
            ((setq to (std11-field-body "From")))
            ;; (t (error "No From: or Reply-To: header in message"))
            )
      (if to-all
          (setq cc (delq nil (cons cc (std11-field-bodies '("To" "Cc"))))
                cc (mapconcat 'identity cc ","))
        )
      (setq subject (std11-field-body "Subject"))
      (and subject vm-reply-subject-prefix
           (let ((case-fold-search t))
             (not
              (equal
               (string-match (regexp-quote vm-reply-subject-prefix)
                             subject)
               0)))
           (setq subject (concat vm-reply-subject-prefix subject)))
      (setq in-reply-to (std11-field-body "Message-Id")
            references (nconc
                        (std11-field-bodies '("References" "In-Reply-To"))
                        (list in-reply-to))
            newsgroups (list (or (and to-all
                                      (std11-field-body "Followup-To"))
                                 (std11-field-body "Newsgroups"))))
      (setq to (vm-parse-addresses to)
            cc (vm-parse-addresses cc))
      (if vm-reply-ignored-addresses
          (setq to (vm-strip-ignored-addresses to)
                cc (vm-strip-ignored-addresses cc)))
      (setq to (vm-delete-duplicates to nil t))
      (setq cc (vm-delete-duplicates
                (append (vm-delete-duplicates cc nil t)
                        to (copy-sequence to))
                t t))
      (and to (setq to (mapconcat 'identity to ",\n ")))
      (and cc (setq cc (mapconcat 'identity cc ",\n ")))
      (and (null to) (setq to cc cc nil))
      (setq references (delq nil references)
            references (mapconcat 'identity references " ")
            references (vm-parse references "[^<]*\\(<[^>]+>\\)")
            references (vm-delete-duplicates references)
            references (if references (mapconcat 'identity references "\n\t")))
      (setq newsgroups (delq nil newsgroups)
            newsgroups (mapconcat 'identity newsgroups ",")
            newsgroups (vm-parse newsgroups "[ \t\f\r\n,]*\\([^ \t\f\r\n,]+\\)")
            newsgroups (vm-delete-duplicates newsgroups)
            newsgroups (if newsgroups (mapconcat 'identity newsgroups ",")))
      (vm-mail-internal
       (if to
           (format "reply to %s%s"
                   (std11-full-name-string
                    (car (std11-parse-address-string to)))
                   (if cc ", ..." "")))
       to subject in-reply-to cc references newsgroups)
      (setq mail-reply-buffer buf
            ;; vm-system-state 'replying
            default-directory dir))
    (if include-text
        (save-excursion
          (goto-char (point-min))
          (let ((case-fold-search nil))
            (re-search-forward
             (concat "^" (regexp-quote mail-header-separator) "$") nil 0))
          (forward-char 1)
          (tm-vm/yank-content)))
    (run-hooks 'vm-reply-hook)
    (run-hooks 'vm-mail-mode-hook)
    ))

(defun tm-vm/following-method (buf)
  (tm-vm/do-reply buf 'to-all 'include-text)
  )

(defun tm-vm/yank-content ()
  (interactive)
  (let ((this-command 'vm-yank-message))
    (vm-display nil nil '(vm-yank-message)
                '(vm-yank-message composing-message))
    (save-restriction
      (narrow-to-region (point)(point))
      (insert-buffer mail-reply-buffer)
      (goto-char (point-max))
      (push-mark)
      (goto-char (point-min)))
    (cond (mail-citation-hook (run-hooks 'mail-citation-hook))
          (mail-yank-hooks (run-hooks 'mail-yank-hooks))
          (t (mail-indent-citation)))
    ))

(set-alist 'mime-viewer/following-method-alist
           'vm-mode
           (function tm-vm/following-method))
(set-alist 'mime-viewer/following-method-alist
           'vm-virtual-mode
           (function tm-vm/following-method))

(set-alist 'mime-viewer/quitting-method-alist
           'vm-mode
           'tm-vm/quit-view-message)
(set-alist 'mime-viewer/quitting-method-alist
           'vm-virtual-mode
           'tm-vm/quit-view-message)

;;; @@ Motion commands

(defmacro tm-vm/save-window-excursion (&rest forms)
  (list 'let '((tm-vm/selected-window (selected-window)))
        (list 'unwind-protect
              (cons 'progn forms)
              '(if (window-live-p tm-vm/selected-window)
                   (select-window tm-vm/selected-window)))))

(defmacro tm-vm/save-frame-excursion (&rest forms)
  (list 'let '((tm-vm/selected-frame (vm-selected-frame)))
	(list 'unwind-protect
	      (cons 'progn forms)
	      '(if (frame-live-p tm-vm/selected-frame)
		   (vm-select-frame tm-vm/selected-frame)))))

(defadvice vm-scroll-forward (around tm-aware activate)
  "Made TM-aware (handles the MIME-Preview buffer)."
  (if (and 
       (not (save-excursion 
	      (if mime::preview/article-buffer
		  (set-buffer mime::preview/article-buffer))
	      (vm-select-folder-buffer)
	      (eq vm-system-state 'previewing)))
       (not (tm-vm/system-state)))
      (progn 
	ad-do-it
	(tm-vm/display-preview-buffer))
    (let* ((mp-changed (vm-follow-summary-cursor))
	   (mbuf (or (vm-select-folder-buffer) (current-buffer)))
	   (mwin (vm-get-buffer-window mbuf))
	   (pbuf (and mime::article/preview-buffer
		      (get-buffer mime::article/preview-buffer)))
	   (pwin (and pbuf (vm-get-buffer-window pbuf)))
	   )
      (vm-check-for-killed-summary)
      (vm-error-if-folder-empty)
      (cond
	; A new message was selected 
	; => leave it to tm-vm/preview-current-message
       (mp-changed
	nil)
       ((eq vm-system-state 'previewing)
	(vm-display (current-buffer) t (list this-command) '(reading-message))
	(vm-show-current-message)
	(tm-vm/preview-current-message))
	; Preview buffer was killed
       ((null pbuf)
	(tm-vm/preview-current-message))
	; Preview buffer was undisplayed
       ((null pwin)
	(if (null mwin)
	    (vm-display mbuf t '(vm-scroll-forward vm-scroll-backward)
			(list this-command 'reading-message)))
	(tm-vm/display-preview-buffer))
	; Preview buffer is displayed => scroll
       (t
	(tm-vm/save-window-excursion
	 (select-window pwin)
	 (set-buffer pbuf)
	 (if (pos-visible-in-window-p (point-max) pwin)
	     (if vm-auto-next-message
		 (vm-next-message))
	   ;; not at the end of message. scroll preview buffer only.
	   (scroll-up)
	   (tm-vm/howl-if-eom))
	 ))))
    )
)

(defadvice vm-scroll-backward (around tm-aware activate)
  "Made TM-aware (handles the MIME-Preview buffer)."
  (if (and
       (not (save-excursion 
	      (if mime::preview/article-buffer
		  (set-buffer mime::preview/article-buffer))
	      (vm-select-folder-buffer)
	      (eq vm-system-state 'previewing)))	 
       (not (tm-vm/system-state)))
      ad-do-it
    (let* ((mp-changed (vm-follow-summary-cursor))
	   (mbuf (or (vm-select-folder-buffer) (current-buffer)))
	   (mwin (vm-get-buffer-window mbuf))
	   (pbuf (and mime::article/preview-buffer
		      (get-buffer mime::article/preview-buffer)))
	   (pwin (and pbuf (vm-get-buffer-window pbuf)))
	   )
      (vm-check-for-killed-summary)
      (vm-error-if-folder-empty)
      (cond
	; A new message was selected 
	; => leave it to tm-vm/preview-current-message
       (mp-changed
	nil)
       ((eq vm-system-state 'previewing)
	(tm-vm/update-message-status)
	(setq vm-system-state 'reading)
	(tm-vm/preview-current-message))
	; Preview buffer was killed
       ((null pbuf)
	(tm-vm/preview-current-message))
	; Preview buffer was undisplayed
       ((null pwin)
	(if (null mwin)
	    (vm-display mbuf t '(vm-scroll-forward vm-scroll-backward)
			(list this-command 'reading-message)))
	(tm-vm/display-preview-buffer))
	; Preview buffer is displayed => scroll
       (t
	(tm-vm/save-window-excursion
	 (select-window pwin)
	 (if (pos-visible-in-window-p (point-min) pwin)
	     nil
	   ;; not at the end of message. scroll preview buffer only.
	   (scroll-down))
	 ))))
    ))

(defadvice vm-beginning-of-message (around tm-aware activate)
  "Made TM-aware, works properly in MIME-Preview buffers."
  (if (not (tm-vm/system-state))
      ad-do-it
    (vm-follow-summary-cursor)
    (vm-select-folder-buffer)
    (vm-check-for-killed-summary)
    (vm-error-if-folder-empty)
    (let ((pbuf (and mime::article/preview-buffer
                     (get-buffer mime::article/preview-buffer))))
      (if (null pbuf)
          (progn
            (tm-vm/preview-current-message)
            (setq pbuf (get-buffer mime::article/preview-buffer))
            ))
      (vm-display (current-buffer) t '(vm-beginning-of-message)
                  '(vm-beginning-of-message reading-message))
      (tm-vm/display-preview-buffer)
      (tm-vm/save-window-excursion
       (select-window (vm-get-visible-buffer-window pbuf))
       (push-mark)
       (goto-char (point-min))
       (vm-display (current-buffer) t '(vm-beginning-of-message)
		   '(vm-beginning-of-message reading-message))
       ))))

(defadvice vm-end-of-message (around tm-aware activate)
  "Made TM-aware, works properly in MIME-Preview buffers."
  (interactive)
  (if (not (tm-vm/system-state))
      ad-do-it
    (vm-follow-summary-cursor)
    (vm-select-folder-buffer)
    (vm-check-for-killed-summary)
    (vm-error-if-folder-empty)
    (let ((pbuf (and mime::article/preview-buffer
                     (get-buffer mime::article/preview-buffer))))
      (if (null pbuf)
          (progn
            (tm-vm/preview-current-message)
            (setq pbuf (get-buffer mime::article/preview-buffer))
            ))
      (vm-display (current-buffer) t '(vm-end-of-message)
                  '(vm-end-of-message reading-message))
      (tm-vm/display-preview-buffer)
      (tm-vm/save-window-excursion
       (select-window (vm-get-buffer-window pbuf))
       (push-mark)
       (goto-char (point-max))
       (vm-display (current-buffer) t '(vm-end-of-message)
		   '(vm-end-of-message reading-message))
       ))))

;;; based on vm-howl-if-eom [vm-page.el]
(defun tm-vm/howl-if-eom ()
  (let* ((pbuf (or mime::article/preview-buffer (current-buffer)))
         (pwin (and (vm-get-visible-buffer-window pbuf))))
    (and pwin
         (save-excursion
           (save-window-excursion
             (condition-case ()
                 (let ((next-screen-context-lines 0))
                   (tm-vm/save-frame-excursion
		    (vm-select-frame (vm-window-frame pwin))
		    (save-selected-window
		      (select-window pwin)
		      (save-excursion
			(let ((scroll-in-place-replace-original nil))
			  (scroll-up)))))
		    nil)
               (error t))))
         (vm-emit-eom-blurb)
         )))

(defadvice vm-emit-eom-blurb (around tm-aware activate)
  "Made TM-aware, works properly in MIME-Preview buffers."
  (save-excursion
    (if mime::preview/article-buffer
        (set-buffer mime::preview/article-buffer))
    ad-do-it))

(defadvice vm-next-message (around tm-aware activate)
  "Made TM-aware, works properly in MIME-Preview buffers."
  (if mime::preview/article-buffer
      (set-buffer mime::preview/article-buffer))
  (tm-vm/save-window-excursion
   ad-do-it))

(defadvice vm-previous-message (around tm-aware activate)
  "Made TM-aware, works properly in MIME-Preview buffers."
  (if mime::preview/article-buffer
      (set-buffer mime::preview/article-buffer))
  (tm-vm/save-window-excursion
   ad-do-it))

(defadvice vm-next-message-no-skip (around tm-aware activate)
  "Made TM-aware, works properly in MIME-Preview buffers."
  (if mime::preview/article-buffer
      (set-buffer mime::preview/article-buffer))
  (tm-vm/save-window-excursion
   ad-do-it))

(defadvice vm-previous-message-no-skip (around tm-aware activate)
  "TM wrapper for vm-previous-message-no-skip (which see)."
  (if mime::preview/article-buffer
      (set-buffer mime::preview/article-buffer))
  (tm-vm/save-window-excursion
     ad-do-it))

(defadvice vm-next-unread-message (around tm-aware activate)
  "Made TM-aware, works properly in MIME-Preview buffers."
  (if mime::preview/article-buffer
      (set-buffer mime::preview/article-buffer))
  (tm-vm/save-window-excursion
   ad-do-it))

(defadvice vm-previous-unread-message (around tm-aware activate)
  "Made TM-aware, works properly in MIME-Preview buffers."
  (if mime::preview/article-buffer
      (set-buffer mime::preview/article-buffer))
  (tm-vm/save-window-excursion
   ad-do-it))


(set-alist 'mime-viewer/over-to-previous-method-alist
           'vm-mode 'vm-previous-message)
(set-alist 'mime-viewer/over-to-next-method-alist
           'vm-mode 'vm-next-message)
(set-alist 'mime-viewer/over-to-previous-method-alist
           'vm-virtual-mode 'vm-previous-message)
(set-alist 'mime-viewer/over-to-next-method-alist
           'vm-virtual-mode 'vm-next-message)






;;; @ MIME Editor

;;; @@ vm-yank-message


(defvar tm-vm/yank:message-to-restore nil
  "For internal use by tm-vm only.")

(defun vm-yank-message (&optional message)
  "Yank message number N into the current buffer at point.
When called interactively N is always read from the minibuffer.  When
called non-interactively the first argument is expected to be a
message struct.

This function originally provided by vm-reply has been patched for TM
in order to provide better citation of MIME messages : if a MIME
Preview buffer exists for the message then its contents are inserted
instead of the raw message.

This command is meant to be used in VM created Mail mode buffers; the
yanked message comes from the mail buffer containing the message you
are replying to, forwarding, or invoked VM's mail command from.

All message headers are yanked along with the text.  Point is
left before the inserted text, the mark after.  Any hook
functions bound to mail-citation-hook are run, after inserting
the text and setting point and mark.  For backward compatibility,
if mail-citation-hook is set to nil, `mail-yank-hooks' is run
instead.

If mail-citation-hook and mail-yank-hooks are both nil, this
default action is taken: the yanked headers are trimmed as
specified by vm-included-text-headers and
vm-included-text-discard-header-regexp, and the value of
vm-included-text-prefix is prepended to every yanked line."
  (interactive
   (list
    ;; What we really want for the first argument is a message struct,
    ;; but if called interactively, we let the user type in a message
    ;; number instead.
    (let (mp default
             (result 0)
             prompt
             (last-command last-command)
             (this-command this-command))
      (if (bufferp vm-mail-buffer)
          (save-excursion
            (vm-select-folder-buffer)
            (setq default (and vm-message-pointer
                               (vm-number-of (car vm-message-pointer)))
                  prompt (if default
                             (format "Yank message number: (default %s) "
                                     default)
                           "Yank message number: "))
            (while (zerop result)
              (setq result (read-string prompt))
              (and (string= result "") default (setq result default))
              (setq result (string-to-int result)))
            (if (null (setq mp (nthcdr (1- result) vm-message-list)))
                (error "No such message."))
            (setq tm-vm/yank:message-to-restore (string-to-int default))
            (save-selected-window
              (vm-goto-message result))
            (car mp))
        nil))))
  (if (null message)
      (if mail-reply-buffer
          (tm-vm/yank-content)
        (error "This is not a VM Mail mode buffer."))
    (if (null (buffer-name vm-mail-buffer))
        (error "The folder buffer containing message %d has been killed."
               (vm-number-of message)))
    (vm-display nil nil '(vm-yank-message)
                '(vm-yank-message composing-message))
    (let ((b (current-buffer)) (start (point)) end)
      (save-restriction
        (widen)
        (save-excursion
          (set-buffer (vm-buffer-of message))
          (let (pbuf)
            (tm-vm/sync-preview-buffer)
            (setq pbuf (and mime::article/preview-buffer
                            (get-buffer mime::article/preview-buffer)))
            (if (and pbuf
		     (not (eq this-command 'vm-forward-message)))
		;; Yank contents of MIME Preview buffer
                (if running-xemacs
                    (let ((tmp (generate-new-buffer "tm-vm/tmp")))
                      (set-buffer pbuf)
                      (append-to-buffer tmp (point-min) (point-max))
                      (set-buffer tmp)
                      (map-extents
                       '(lambda (ext maparg) 
                          (set-extent-property ext 'begin-glyph nil)))
                      (append-to-buffer b (point-min) (point-max))
                      (setq end (vm-marker
                                 (+ start (length (buffer-string))) b))
                      (kill-buffer tmp))
                  (set-buffer pbuf)
                  (append-to-buffer b (point-min) (point-max))
                  (setq end (vm-marker
                             (+ start (length (buffer-string))) b)))
	      ;; Yank contents of raw VM message
              (save-restriction
                (setq message (vm-real-message-of message))
                (set-buffer (vm-buffer-of message))
                (widen)
                (append-to-buffer
                 b (vm-headers-of message) (vm-text-end-of message))
                (setq end
                      (vm-marker (+ start (- (vm-text-end-of message)
                                             (vm-headers-of message))) b))))))
        (push-mark end)
        (cond (mail-citation-hook (run-hooks 'mail-citation-hook))
              (mail-yank-hooks (run-hooks 'mail-yank-hooks))
              (t (vm-mail-yank-default message)))
        ))
    (if tm-vm/yank:message-to-restore
        (save-selected-window
          (vm-goto-message tm-vm/yank:message-to-restore)
          (setq tm-vm/yank:message-to-restore nil)))
    ))

;;; @@ for tm-partial
;;;

(call-after-loaded
 'tm-partial
 (function
  (lambda ()
    (set-atype 'mime/content-decoding-condition
               '((type . "message/partial")
                 (method . mime-article/grab-message/partials)
                 (major-mode . vm-mode)
                 (summary-buffer-exp . vm-summary-buffer)
                 ))
    (set-alist 'tm-partial/preview-article-method-alist
               'vm-mode
               (function
                (lambda ()
                  (tm-vm/view-message)
                  )))
    )))


;;; @@ for tm-edit
;;;

(call-after-loaded
 'mime-setup
 (function
  (lambda ()
    (setq vm-forwarding-digest-type "rfc1521")
    (setq vm-digest-send-type "rfc1521")
    )))

;;; @@@ multipart/digest

(if (not (fboundp 'vm-unsaved-message))
    (fset 'vm-unsaved-message 'message))

(defun tm-vm/enclose-messages (mlist &optional preamble)
  "Enclose the messages in MLIST as multipart/digest.
The resulting digest is inserted at point in the current buffer.

MLIST should be a list of message structs (real or virtual).
These are the messages that will be enclosed."
  (if mlist
      (let ((digest (consp (cdr mlist)))
            (mp mlist)
            m)
        (save-restriction
          (narrow-to-region (point) (point))
          (while mlist
            (setq m (vm-real-message-of (car mlist)))
            (mime-editor/insert-tag "message" "rfc822")
            (tm-mail/insert-message m)
            (goto-char (point-max))
            (setq mlist (cdr mlist)))
          (if preamble
              (progn
                (goto-char (point-min))
                (mime-editor/insert-tag "text" "plain")
                (vm-unsaved-message "Building digest preamble...")
                (while mp
                  (let ((vm-summary-uninteresting-senders nil))
                    (insert
                     (vm-sprintf 'vm-digest-preamble-format (car mp)) "\n"))
                  (if vm-digest-center-preamble
                      (progn
                        (forward-char -1)
                        (center-line)
                        (forward-char 1)))
                  (setq mp (cdr mp)))))
          (if digest
              (mime-editor/enclose-digest-region (point-min) (point-max)))
          ))))

(defadvice vm-forward-message (around tm-aware activate)
  "Extended to support rfc1521 digests (roughly equivalent to what
VM does when vm-forwarding-digest-type is 'mime but using message/rfc822
when appropriate."
  (if (not (equal vm-forwarding-digest-type "rfc1521"))
      ad-do-it
    (if mime::preview/article-buffer
	(set-buffer mime::preview/article-buffer))
    (vm-follow-summary-cursor)
    (vm-select-folder-buffer)
    (vm-check-for-killed-summary)
    (vm-error-if-folder-empty)
    (if (eq last-command 'vm-next-command-uses-marks)
        (let ((vm-digest-send-type vm-forwarding-digest-type))
          (setq this-command 'vm-next-command-uses-marks)
          (command-execute 'tm-vm/send-digest))
      (let ((dir default-directory)
            (mp vm-message-pointer))
        (save-restriction
          (widen)
          (vm-mail-internal
           (format "forward of %s's note re: %s"
                   (vm-su-full-name (car vm-message-pointer))
                   (vm-su-subject (car vm-message-pointer)))
           nil
           (and vm-forwarding-subject-format
                (let ((vm-summary-uninteresting-senders nil))
                  (vm-sprintf 'vm-forwarding-subject-format (car mp)))))
          (make-local-variable 'vm-forward-list)
          (setq vm-system-state 'forwarding
                vm-forward-list (list (car mp))
                default-directory dir)
          (goto-char (point-min))
          (re-search-forward
           (concat "^" (regexp-quote mail-header-separator) "\n") nil 0)
          (tm-vm/enclose-messages vm-forward-list)
          (mail-position-on-field "To"))
        (run-hooks 'tm-vm/forward-message-hook)
        (run-hooks 'vm-mail-mode-hook)))))

(defun tm-vm/send-digest (&optional arg)
  "Send a digest of all messages in the current folder to recipients.
The type of the digest is specified by the variable vm-digest-send-type.
You will be placed in a Mail mode buffer as is usual with replies, but you
must fill in the To: and Subject: headers manually.

If invoked on marked messages (via vm-next-command-uses-marks),
only marked messages will be put into the digest."
  (interactive "P")
  (if (not (equal vm-digest-send-type "rfc1521"))
      (vm-send-digest arg)
    (vm-select-folder-buffer)
    (vm-check-for-killed-summary)
    (vm-error-if-folder-empty)
    (let ((dir default-directory)
          (vm-forward-list (if (eq last-command 'vm-next-command-uses-marks)
                               (vm-select-marked-or-prefixed-messages 0)
                             vm-message-list)))
      (save-restriction
        (widen)
        (vm-mail-internal (format "digest from %s" (buffer-name)))
        (setq vm-system-state 'forwarding
              default-directory dir)
        (goto-char (point-min))
        (re-search-forward (concat "^" (regexp-quote mail-header-separator)
                                   "\n"))
        (goto-char (match-end 0))
        (vm-unsaved-message "Building %s digest..." vm-digest-send-type)
        (tm-vm/enclose-messages vm-forward-list arg)
        (mail-position-on-field "To")
        (message "Building %s digest... done" vm-digest-send-type)))
    (run-hooks 'tm-vm/send-digest-hook)
    (run-hooks 'vm-mail-mode-hook)))

(substitute-key-definition 'vm-send-digest
                           'tm-vm/send-digest vm-mode-map)

;;; @@@ Menus


(call-after-loaded
 'tm-edit
 (function
  (lambda ()
    (autoload 'tm-mail/insert-message "tm-mail")
    (set-alist 'mime-editor/message-inserter-alist
               'mail-mode (function tm-mail/insert-message))
    (set-alist 'mime-editor/split-message-sender-alist
               'mail-mode (function
                           (lambda ()
                             (interactive)
                             (funcall send-mail-function)
                             )))
    )))



;;; @ VM Integration

(add-hook 'vm-quit-hook 'tm-vm/quit-view-message)

;;; @@ Wrappers for miscellaneous VM functions

(defadvice vm-summarize (around tm-aware activate)
  "Made TM aware. Callable from the MIME Preview buffer."
  (if mime::preview/article-buffer
      (set-buffer mime::preview/article-buffer))
  ad-do-it
  (save-excursion
    (set-buffer vm-summary-buffer)
    (tm-vm/check-for-toolbar))
  (tm-vm/preview-current-message))

(defadvice vm-expose-hidden-headers (around tm-aware activate)
  "Made TM aware. Callable from the MIME Preview buffer."
  (if mime::preview/article-buffer
      (set-buffer mime::preview/article-buffer))
  (let ((visible-headers vm-visible-headers))
    (tm-vm/quit-view-message)
    ad-do-it
    (let ((vm-visible-headers visible-headers))
      (if (= (point-min) (vm-start-of (car vm-message-pointer)))
	  (setq vm-visible-headers '(".*")))
      (tm-vm/preview-current-message))))

(if (vm-mouse-fsfemacs-mouse-p)
    (progn
      (define-key tm-vm/vm-emulation-map [mouse-3] 'ignore)
      (define-key tm-vm/vm-emulation-map [down-mouse-3] 'vm-mouse-button-3)
      (defadvice vm-mouse-button-3 (after tm-aware activate)
	"Made TM aware. Works in MIME-Preview buffers."
	(if (and 
	     vm-use-menus
	     (eq major-mode 'mime/viewer-mode))
	    (vm-menu-popup-mode-menu event))))
)

(defadvice vm-save-message (around tm-aware activate)
  "Made TM aware. Callable from the MIME Preview buffer."
  (if mime::preview/article-buffer
      (save-excursion
	(set-buffer mime::preview/article-buffer)
	ad-do-it)
    ad-do-it))

(defadvice vm-expunge-folder (around tm-aware activate)
  "Made TM aware. Callable from the MIME Preview buffer."
  (if mime::preview/article-buffer
      (save-excursion
	(set-buffer mime::preview/article-buffer)
	ad-do-it)
    ad-do-it))

(defadvice vm-save-folder (around tm-aware activate)
  "Made TM aware. Callable from the MIME Preview buffer."
  (if mime::preview/article-buffer
      (save-excursion
	(set-buffer mime::preview/article-buffer)
	ad-do-it)
    ad-do-it))

(defadvice vm-goto-parent-message (around tm-aware activate)
  "Made TM aware. Callable from the MIME Preview buffer."
  (if mime::preview/article-buffer
      (save-excursion
	(set-buffer mime::preview/article-buffer)
	ad-do-it)
    ad-do-it))

(defadvice vm-delete-message (around tm-aware activate)
  "Made TM aware. Callable from the MIME Preview buffer."
  (interactive "p")
  (if (interactive-p)
      (vm-follow-summary-cursor))
  (if mime::preview/article-buffer
      (save-excursion
	(set-buffer mime::preview/article-buffer)
	ad-do-it)
    ad-do-it))

(defadvice vm-delete-message-backward (around tm-aware activate)
  "Made TM aware. Callable from the MIME Preview buffer."
  (interactive "p")
  (if (interactive-p)
      (vm-follow-summary-cursor))
  (if mime::preview/article-buffer
      (save-excursion
	(set-buffer mime::preview/article-buffer)
	ad-do-it)
    ad-do-it))

(defadvice vm-undelete-message (around tm-aware activate)
  "Made TM aware. Callable from the MIME Preview buffer."
  (interactive "p")
  (if (interactive-p)
      (vm-follow-summary-cursor))
  (if mime::preview/article-buffer
      (save-excursion
	(set-buffer mime::preview/article-buffer)
	ad-do-it)
    ad-do-it))

(defadvice vm-unread-message (around tm-aware activate)
  "Made TM aware. Callable from the MIME Preview buffer."
  (if mime::preview/article-buffer
      (save-excursion
	(set-buffer mime::preview/article-buffer)
	ad-do-it)
    ad-do-it))

(defadvice vm-edit-message (around tm-aware activate)
  "Made TM aware. Callable from the MIME Preview buffer."
  (if mime::preview/article-buffer
      (save-excursion
	(set-buffer mime::preview/article-buffer)
	ad-do-it)
    ad-do-it))


  
;;; @@ VM Toolbar Integration

;;; based on vm-toolbar-any-messages-p [vm-toolbar.el]
(defun tm-vm/check-for-toolbar ()
  "Install VM toolbar if necessary."
  (if (and running-xemacs
	   vm-toolbar-specifier)
      (progn
	(if (null (specifier-instance vm-toolbar-specifier))
	    (vm-toolbar-install-toolbar))
	(vm-toolbar-update-toolbar))))

(defun vm-toolbar-any-messages-p ()
  (save-excursion
    (if mime::preview/article-buffer
	(set-buffer mime::preview/article-buffer))
    (vm-check-for-killed-folder)
    (vm-select-folder-buffer)
    vm-message-list))


;;; @ BBDB Integration
;;;

(call-after-loaded
 'bbdb
 (function
  (lambda ()
    (require 'bbdb-vm)
    (require 'tm-bbdb)
    (defun tm-bbdb/vm-update-record (&optional offer-to-create)
      (save-excursion
	(vm-select-folder-buffer)
	(if (and (tm-vm/system-state)
		 mime::article/preview-buffer
		 (get-buffer mime::article/preview-buffer))
	    (let ((tm-bbdb/auto-create-p bbdb/mail-auto-create-p))
	      (tm-bbdb/update-record offer-to-create))
	  (or (bbdb/vm-update-record offer-to-create)
	      (delete-windows-on (get-buffer "*BBDB*")))
	  )))
    (remove-hook 'vm-select-message-hook 'bbdb/vm-update-record)
    (remove-hook 'vm-show-message-hook 'bbdb/vm-update-record)
    (add-hook 'tm-vm/select-message-hook 'tm-bbdb/vm-update-record)
    )))

;;; @ ps-print (Suggested by Anders Stenman <stenman@isy.liu.se>)
;;;

(if tm-vm/use-ps-print
    (progn
      (autoload 'ps-print-buffer-with-faces "ps-print" "Postscript Print" t)
      (add-hook 'vm-mode-hook 'tm-vm/ps-print-setup)
      (add-hook 'mime-viewer/define-keymap-hook 'tm-vm/ps-print-setup)
      (fset 'vm-toolbar-print-command 'tm-vm/print-message)))

(defun tm-vm/ps-print-setup ()
  "Set things up for printing MIME messages with ps-print. Set binding to 
the [Print Screen] key."
  (local-set-key (if running-xemacs
		     'f22
		   [f22]) 
		 'tm-vm/print-message)
  (make-local-variable 'ps-header-lines)
  (make-local-variable 'ps-left-header)
  (setq ps-header-lines 3)
  (setq ps-left-header
        (list 'ps-article-subject 'ps-article-author 'buffer-name)))

(defun tm-vm/print-message ()
  "Print current message with ps-print if it's a MIME message. 
Value of tm-vm/strict-mime is also taken into consideration."
  (interactive)
  (vm-follow-summary-cursor)
  (vm-select-folder-buffer)
  (tm-vm/sync-preview-buffer)
  (let ((pbuf (and mime::article/preview-buffer
		  (get-buffer mime::article/preview-buffer))))
    (if pbuf
        (save-excursion
          (set-buffer pbuf)
          (require 'ps-print)
          (ps-print-buffer-with-faces))
      (vm-print-message))))


;;; @ end

(provide 'tm-vm)
(run-hooks 'tm-vm-load-hook)

;;; tm-vm.el ends here.
