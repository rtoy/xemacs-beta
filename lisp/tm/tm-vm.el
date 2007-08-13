;;; tm-vm.el --- tm-MUA (MIME Extension module) for VM

;; Copyright (C) 1994,1995,1996 Free Software Foundation, Inc.

;; Author: MASUTANI Yasuhiro <masutani@me.es.osaka-u.ac.jp>
;;         Kenji Wakamiya <wkenji@flab.fujitsu.co.jp>
;;         MORIOKA Tomohiko <morioka@jaist.ac.jp>
;;         Shuhei KOBAYASHI <shuhei-k@jaist.ac.jp>
;;         Oscar Figueiredo <Oscar.Figueiredo@di.epfl.ch>
;; Maintainer: Oscar Figueiredo <Oscar.Figueiredo@di.epfl.ch>
;; Created: 1994/10/29
;; Version: $Revision: 1.1.1.1 $
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

(require 'tm-view)
(require 'vm)

(defconst tm-vm/RCS-ID
  "$Id: tm-vm.el,v 1.1.1.1 1996/12/18 22:43:38 steve Exp $")
(defconst tm-vm/version (get-version-string tm-vm/RCS-ID))

(define-key vm-mode-map "Z" 'tm-vm/view-message)
(define-key vm-mode-map "T" 'tm-vm/decode-message-header)
(define-key vm-mode-map "\et" 'tm-vm/toggle-preview-mode)

(defvar tm-vm/use-original-url-button nil
  "*If it is t, use original URL button instead of tm's.")

(defvar tm-vm-load-hook nil
  "*List of functions called after tm-vm is loaded.")


;;; @ for MIME encoded-words
;;;

(defvar tm-vm/use-tm-patch nil
  "Does not decode encoded-words in summary buffer if it is t.
If you use tiny-mime patch for VM (by RIKITAKE Kenji
<kenji@reseau.toyonaka.osaka.jp>), please set it t [tm-vm.el]")

(or tm-vm/use-tm-patch
    (progn
;;;
(defvar tm-vm/chop-full-name-function 'tm-vm/default-chop-full-name)
(setq vm-chop-full-name-function tm-vm/chop-full-name-function)

(defun tm-vm/default-chop-full-name (address)
  (let* ((ret (vm-default-chop-full-name address))
         (full-name (car ret))
         )
    (if (stringp full-name)
        (cons (mime-eword/decode-string full-name)
              (cdr ret))
      ret)))

(require 'vm-summary)
(or (fboundp 'tm:vm-su-subject)
    (fset 'tm:vm-su-subject (symbol-function 'vm-su-subject))
    )
(defun vm-su-subject (m)
  (mime-eword/decode-string (tm:vm-su-subject m))
  )

(or (fboundp 'tm:vm-su-full-name)
    (fset 'tm:vm-su-full-name (symbol-function 'vm-su-full-name))
    )
(defun vm-su-full-name (m)
  (mime-eword/decode-string (tm:vm-su-full-name m))
  )

(or (fboundp 'tm:vm-su-to-names)
    (fset 'tm:vm-su-to-names (symbol-function 'vm-su-to-names))
    )
(defun vm-su-to-names (m)
  (mime-eword/decode-string (tm:vm-su-to-names m))
  )
;;;
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


;;; @ automatic MIME preview
;;;

(defvar tm-vm/automatic-mime-preview t
  "*If non-nil, automatically process and show MIME messages.")

(defvar tm-vm/strict-mime t
  "*If nil, do MIME processing even if there is no MIME-Version field.")

(defvar tm-vm/select-message-hook nil
  "*List of functions called every time a message is selected.
tm-vm uses `vm-select-message-hook', use this hook instead.")

(defvar tm-vm/system-state nil)

(setq mime-viewer/content-header-filter-alist 
      (append '((vm-mode . tm-vm/header-filter)
                (vm-virtual-mode . tm-vm/header-filter)) 
              mime-viewer/content-header-filter-alist))

(defun tm-vm/header-filter ()
  "Filter headers in current buffer (assumed to be a message-like buffer)
according to vm-visible-headers and vm-invisible-header-regexp"
  (beginning-of-buffer)
  (let ((visible-headers vm-visible-headers))
    (if (or vm-use-lucid-highlighting
	    vm-display-xfaces)
	(setq visible-headers (cons "X-Face:" vm-visible-headers)))
    (vm-reorder-message-headers nil
				visible-headers
				vm-invisible-header-regexp)
    (mime/decode-message-header)))

(defun tm-vm/system-state ()
  (save-excursion
    (if mime::preview/article-buffer
        (set-buffer mime::preview/article-buffer)
      (vm-select-folder-buffer))
    tm-vm/system-state))

(defun tm-vm/sync-preview-buffer ()
  "Ensure that the MIME preview buffer, if it exists actually corresponds to 
the current message. If no MIME Preview buffer is needed, delete it. If no
MIME Preview buffer exists nothing is done."
  ;; Current buffer should be message buffer when calling this function
  (let* ((mbuf (current-buffer))
         (pbuf (and mime::article/preview-buffer
                    (get-buffer mime::article/preview-buffer)))
         (win (or (and pbuf (vm-get-buffer-window pbuf))
                  (vm-get-buffer-window mbuf)))
         (frame (selected-frame)))
    (if pbuf
        ;; Go to the frame where pbuf or mbuf is (frame-per-composition t)
        (save-excursion
          (if win
              (vm-select-frame (vm-window-frame win)))
          ;; Rebuild MIME Preview buffer to ensure it corresponds to
          ;; current message
          (save-window-excursion
            (save-selected-window
              (save-excursion
                (set-buffer mbuf)
                (setq mime::article/preview-buffer nil)
                (if pbuf (kill-buffer pbuf)))
              (tm-vm/view-message)))
          ;; Return to previous frame
          (vm-select-frame frame)))))

(defun tm-vm/display-preview-buffer ()
  (let* ((mbuf (current-buffer))
         (mwin (vm-get-visible-buffer-window mbuf))
         (pbuf (and mime::article/preview-buffer
                    (get-buffer mime::article/preview-buffer)))
         (pwin (and pbuf (vm-get-visible-buffer-window pbuf)))) 
    (if (and pbuf (tm-vm/system-state))
        ;; display preview buffer
        (cond
         ((and mwin pwin)
          (vm-undisplay-buffer mbuf)
          (tm-vm/show-current-message))
         ((and mwin (not pwin))
          (set-window-buffer mwin pbuf)
          (tm-vm/show-current-message))
         (pwin
          (tm-vm/show-current-message))
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
        )))
    (set-buffer mbuf)))

(defun tm-vm/preview-current-message ()
  "Preview current message if it has a MIME contents and 
tm-vm/automatic-mime-preview is non nil. Installed on 
vm-visit-folder-hook and vm-select-message-hook."
  ;; assumed current buffer is folder buffer.
  (setq tm-vm/system-state nil)
  (if (get-buffer mime/output-buffer-name)
      (vm-undisplay-buffer mime/output-buffer-name))
  (if (and vm-message-pointer tm-vm/automatic-mime-preview)
      (if (or (not tm-vm/strict-mime)
              (vm-get-header-contents (car vm-message-pointer)
                                      "MIME-Version:"))
          ;; do MIME processing.
          (progn
	    ;; Consider message as shown => update its flags and store them
	    ;; in folder buffer before entering MIME viewer
	    (tm-vm/show-current-message)
            (set (make-local-variable 'tm-vm/system-state) 'previewing)
            (save-window-excursion
              (vm-widen-page)
              (goto-char (point-max))
              (widen)
              (narrow-to-region (point)
                                (save-excursion
                                  (goto-char
                                   (vm-start-of (car vm-message-pointer))
                                   )
                                  (forward-line)
                                  (point)
                                  ))

              (mime/viewer-mode nil nil nil nil nil vm-mode-map)
              ;; Highlight message (and display XFace if supported)
              (if (or vm-highlighted-header-regexp
                      (and (vm-xemacs-p) vm-use-lucid-highlighting))
                  (vm-highlight-headers))
              ;; Energize URLs and buttons
              (if (and tm-vm/use-original-url-button
                       vm-use-menus (vm-menu-support-possible-p))
                  (progn
                    (vm-energize-urls)
                    (vm-energize-headers)))
              (goto-char (point-min))
              (narrow-to-region (point) (search-forward "\n\n" nil t))
              ))
        ;; don't do MIME processing. decode header only.
        (let (buffer-read-only)
          (mime/decode-message-header))
        )
    ;; don't preview; do nothing.
    )
  (tm-vm/display-preview-buffer)
  (run-hooks 'tm-vm/select-message-hook))

(defun tm-vm/show-current-message ()
  "Update current message display and summary. Remove 'unread' and 'new' flags. "
  (if mime::preview/article-buffer
      (set-buffer mime::preview/article-buffer)
    (vm-select-folder-buffer))
  (if mime::article/preview-buffer
      (save-excursion
	(set-buffer mime::article/preview-buffer)
	(goto-char (point-min))
	(widen)))
  (if (or (and mime::article/preview-buffer
	       (vm-get-visible-buffer-window mime::article/preview-buffer))
	  (vm-get-visible-buffer-window (current-buffer)))
      (progn
        (setq tm-vm/system-state 'reading)
        (if (vm-new-flag (car vm-message-pointer))
            (vm-set-new-flag (car vm-message-pointer) nil))
        (if (vm-unread-flag (car vm-message-pointer))
            (vm-set-unread-flag (car vm-message-pointer) nil))
        (vm-update-summary-and-mode-line)
        (tm-vm/howl-if-eom))
    (vm-update-summary-and-mode-line)))

(defun tm-vm/toggle-preview-mode ()
  "Toggle automatic MIME preview on or off. In automatic MIME Preview mode 
each newly selected article is MIME processed if it has MIME content without
need for an explicit request from the user. This behaviour is controlled by the 
variable tm-vm/automatic-mime-preview."
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

(add-hook 'vm-select-message-hook 'tm-vm/preview-current-message)
(add-hook 'vm-visit-folder-hook   'tm-vm/preview-current-message)

;;; tm-vm move commands
;;;

(defmacro tm-vm/save-window-excursion (&rest forms)
  (list 'let '((tm-vm/selected-window (selected-window)))
        (list 'unwind-protect
              (cons 'progn forms)
              '(if (window-live-p tm-vm/selected-window)
                   (select-window tm-vm/selected-window)))))

;;; based on vm-scroll-forward [vm-page.el]
(defun tm-vm/scroll-forward (&optional arg)
  (interactive "P")
  (let ((this-command 'vm-scroll-forward))
    (if (not (tm-vm/system-state))
        (progn 
          (vm-scroll-forward arg)
          (tm-vm/display-preview-buffer))
      (let* ((mp-changed (vm-follow-summary-cursor))
             (mbuf (or (vm-select-folder-buffer) (current-buffer)))
             (mwin (vm-get-buffer-window mbuf))
             (pbuf (and mime::article/preview-buffer
                        (get-buffer mime::article/preview-buffer)))
             (pwin (and pbuf (vm-get-buffer-window pbuf)))
             (was-invisible (and (null mwin) (null pwin)))
             )
        ;; now current buffer is folder buffer.
        (tm-vm/save-window-excursion
         (if (or mp-changed was-invisible)
             (vm-display mbuf t '(vm-scroll-forward vm-scroll-backward)
                         (list this-command 'reading-message)))
         (tm-vm/display-preview-buffer)
         (setq mwin (vm-get-buffer-window mbuf)
               pwin (and pbuf (vm-get-buffer-window pbuf)))
         (cond
          ((or mp-changed was-invisible)
           nil
           )
          ((null pbuf)
           ;; preview buffer is killed.
           (tm-vm/preview-current-message)
           (vm-update-summary-and-mode-line))
          ((eq (tm-vm/system-state) 'previewing)
           (tm-vm/show-current-message))
          (t
           (select-window pwin)
           (set-buffer pbuf)
           (if (pos-visible-in-window-p (point-max) pwin)
               (tm-vm/next-message)
             ;; not end of message. scroll preview buffer only.
             (scroll-up)
             (tm-vm/howl-if-eom)
             (set-buffer mbuf))
           ))))
      )))

;;; based on vm-scroll-backward [vm-page.el]
(defun tm-vm/scroll-backward (&optional arg)
  (interactive "P")
  (let ((this-command 'vm-scroll-backward))
    (if (not (tm-vm/system-state))
        (vm-scroll-backward arg)
      (let* ((mp-changed (vm-follow-summary-cursor))
             (mbuf (or (vm-select-folder-buffer) (current-buffer)))
             (mwin (vm-get-buffer-window mbuf))
             (pbuf (and mime::article/preview-buffer
                        (get-buffer mime::article/preview-buffer)))
             (pwin (and pbuf (vm-get-buffer-window pbuf)))
             (was-invisible (and (null mwin) (null pwin)))
             )
        ;; now current buffer is folder buffer.
        (if (or mp-changed was-invisible)
            (vm-display mbuf t '(vm-scroll-forward vm-scroll-backward)
                        (list this-command 'reading-message)))
        (tm-vm/save-window-excursion
         (tm-vm/display-preview-buffer)
         (setq mwin (vm-get-buffer-window mbuf)
               pwin (and pbuf (vm-get-buffer-window pbuf)))
         (cond
          (was-invisible
           nil
           )
          ((null pbuf)
           ;; preview buffer is killed.
           (tm-vm/preview-current-message)
           (vm-update-summary-and-mode-line))
          ((eq (tm-vm/system-state) 'previewing)
           (tm-vm/show-current-message))
          (t
           (select-window pwin)
           (set-buffer pbuf)
           (if (pos-visible-in-window-p (point-min) pwin)
               nil
             ;; scroll preview buffer only.
             (scroll-down)
             (set-buffer mbuf))
           ))))
      )))

;;; based on vm-beginning-of-message [vm-page.el]
(defun tm-vm/beginning-of-message ()
  "Moves to the beginning of the current message."
  (interactive)
  (if (not (tm-vm/system-state))
      (progn
        (setq this-command 'vm-beginning-of-message)
        (vm-beginning-of-message))
    (vm-follow-summary-cursor)
    (vm-select-folder-buffer)
    (vm-check-for-killed-summary)
    (vm-error-if-folder-empty)
    (let ((mbuf (current-buffer))
          (pbuf (and mime::article/preview-buffer
                     (get-buffer mime::article/preview-buffer))))
      (if (null pbuf)
          (progn
            (tm-vm/preview-current-message)
            (setq pbuf (get-buffer mime::article/preview-buffer))
            ))
      (vm-display mbuf t '(vm-beginning-of-message)
                  '(vm-beginning-of-message reading-message))
      (tm-vm/display-preview-buffer)
      (set-buffer pbuf)
      (tm-vm/save-window-excursion
       (select-window (vm-get-buffer-window pbuf))
       (push-mark)
       (goto-char (point-min))
       ))))

;;; based on vm-end-of-message [vm-page.el]
(defun tm-vm/end-of-message ()
  "Moves to the end of the current message."
  (interactive)
  (if (not (tm-vm/system-state))
      (progn
        (setq this-command 'vm-end-of-message)
        (vm-end-of-message))
    (vm-follow-summary-cursor)
    (vm-select-folder-buffer)
    (vm-check-for-killed-summary)
    (vm-error-if-folder-empty)
    (let ((mbuf (current-buffer))
          (pbuf (and mime::article/preview-buffer
                     (get-buffer mime::article/preview-buffer))))
      (if (null pbuf)
          (progn
            (tm-vm/preview-current-message)
            (setq pbuf (get-buffer mime::article/preview-buffer))
            ))
      (vm-display mbuf t '(vm-end-of-message)
                  '(vm-end-of-message reading-message))
      (tm-vm/display-preview-buffer)
      (set-buffer pbuf)
      (tm-vm/save-window-excursion
       (select-window (vm-get-buffer-window pbuf))
       (push-mark)
       (goto-char (point-max))
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
                   (select-window pwin)
                   (save-excursion
                     (save-window-excursion
                       (let ((scroll-in-place-replace-original nil))
                         (scroll-up))))
                   nil)
               (error t))))
         (tm-vm/emit-eom-blurb)
         )))

;;; based on vm-emit-eom-blurb [vm-page.el]
(defun tm-vm/emit-eom-blurb ()
  (save-excursion
    (if mime::preview/article-buffer
        (set-buffer mime::preview/article-buffer))
    (vm-emit-eom-blurb)))

;;; based on vm-quit [vm-folder.el]
(defun tm-vm/quit ()
  "Quit VM saving the folder buffer and killing the MIME Preview buffer if any"
  (interactive)
  (save-excursion
    (vm-select-folder-buffer)
    (if (and mime::article/preview-buffer
             (get-buffer mime::article/preview-buffer))
        (kill-buffer mime::article/preview-buffer)))
  (vm-quit))

(defun tm-vm/quit-no-change ()
  "Quit VM without saving the folder buffer but killing the MIME Preview buffer
if any"
  (interactive)
  (save-excursion
    (vm-select-folder-buffer)
    (if (and mime::article/preview-buffer
             (get-buffer mime::article/preview-buffer))
        (kill-buffer mime::article/preview-buffer)))
  (vm-quit-no-change))

(substitute-key-definition 'vm-scroll-forward
                           'tm-vm/scroll-forward vm-mode-map)
(substitute-key-definition 'vm-scroll-backward
                           'tm-vm/scroll-backward vm-mode-map)
(substitute-key-definition 'vm-beginning-of-message
                           'tm-vm/beginning-of-message vm-mode-map)
(substitute-key-definition 'vm-end-of-message
                           'tm-vm/end-of-message vm-mode-map)
(substitute-key-definition 'vm-quit
                           'tm-vm/quit vm-mode-map)
(substitute-key-definition 'vm-quit-no-change
                           'tm-vm/quit-no-change vm-mode-map)

;;; based on vm-next-message [vm-motion.el]                        
(defun tm-vm/next-message ()
  (set-buffer mime::preview/article-buffer)
  (let ((this-command 'vm-next-message)
        (owin (selected-window))
        (vm-preview-lines nil)
        )
    (vm-next-message 1 nil t)
    (if (window-live-p owin)
        (select-window owin))))

;;; based on vm-previous-message [vm-motion.el]
(defun tm-vm/previous-message ()
  (set-buffer mime::preview/article-buffer)
  (let ((this-command 'vm-previous-message)
        (owin (selected-window))
        (vm-preview-lines nil)
        )
    (vm-previous-message 1 nil t)
    (if (window-live-p owin)
        (select-window owin))))

(set-alist 'mime-viewer/over-to-previous-method-alist
           'vm-mode 'tm-vm/previous-message)
(set-alist 'mime-viewer/over-to-next-method-alist
           'vm-mode 'tm-vm/next-message)
(set-alist 'mime-viewer/over-to-previous-method-alist
           'vm-virtual-mode 'tm-vm/previous-message)
(set-alist 'mime-viewer/over-to-next-method-alist
           'vm-virtual-mode 'tm-vm/next-message)

;;; @@ vm-yank-message
;;;
;; 1996/3/28 by Oscar Figueiredo <figueire@lspsun16.epfl.ch>

(require 'vm-reply)

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
          (let* ((mbuf (current-buffer))
                 pbuf)
            (tm-vm/sync-preview-buffer)
            (setq pbuf (and mime::article/preview-buffer
                            (get-buffer mime::article/preview-buffer)))
            (if pbuf
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


;;; @ for tm-view
;;;

;;; based on vm-do-reply [vm-reply.el]
(defun tm-vm/do-reply (buf to-all include-text)
  (save-excursion
    (set-buffer buf)
    (let ((dir default-directory)
          to cc subject mp in-reply-to references newsgroups)
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


(defun tm-vm/quit-view-message ()
  "Quit MIME-Viewer and go back to normal VM. MIME Preview buffer 
is killed. This function is called by `mime-viewer/quit' command 
via `mime-viewer/quitting-method-alist'."
  (if (get-buffer mime/output-buffer-name)
      (vm-undisplay-buffer mime/output-buffer-name))
  (vm-select-folder-buffer)
  (let* ((mbuf (current-buffer))
         (pbuf (and mime::article/preview-buffer
                    (get-buffer mime::article/preview-buffer)))
         (pwin (and pbuf (vm-get-visible-buffer-window pbuf))))
    (kill-buffer pbuf)
    (and pwin
         (select-window pwin)
         (switch-to-buffer mbuf)))
  (setq tm-vm/system-state nil)
  (vm-display (current-buffer) t (list this-command)
              (list 'reading-message))
  )

(defun tm-vm/view-message ()
  "Decode and view a MIME encoded message under VM. 
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

(set-alist 'mime-viewer/quitting-method-alist
           'vm-mode
           'tm-vm/quit-view-message)

(set-alist 'mime-viewer/quitting-method-alist
           'vm-virtual-mode
           'tm-vm/quit-view-message)


;;; @ for tm-partial
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


;;; @ for tm-edit
;;;

;;; @@ for multipart/digest
;;;

(defvar tm-vm/forward-message-hook nil
  "*List of functions called after a Mail mode buffer has been
created to forward a message in message/rfc822 type format.
If `vm-forwarding-digest-type' is \"rfc1521\", tm-vm runs this
hook instead of `vm-forward-message-hook'.")

(defvar tm-vm/send-digest-hook nil
  "*List of functions called after a Mail mode buffer has been
created to send a digest in multipart/digest type format.
If `vm-digest-send-type' is \"rfc1521\", tm-vm runs this hook
instead of `vm-send-digest-hook'.")

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

(defun tm-vm/forward-message ()
  "Forward the current message to one or more recipients.
You will be placed in a Mail mode buffer as you would with a
reply, but you must fill in the To: header and perhaps the
Subject: header manually."
  (interactive)
  (if (not (equal vm-forwarding-digest-type "rfc1521"))
      (vm-forward-message)
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
                             vm-message-list))
          start)
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

(substitute-key-definition 'vm-forward-message
                           'tm-vm/forward-message vm-mode-map)
(substitute-key-definition 'vm-send-digest
                           'tm-vm/send-digest vm-mode-map)


;;; @@ setting
;;;

(defvar tm-vm/use-xemacs-popup-menu t)

;;; modified by Steven L. Baur <steve@miranova.com>
;;;     1995/12/6 (c.f. [tm-en:209])
(defun mime-editor/attach-to-vm-mode-menu ()
  "Arrange to attach MIME editor's popup menu to VM's"
  (if (boundp 'vm-menu-mail-menu)
      (progn
        (setq vm-menu-mail-menu
              (append vm-menu-mail-menu
                      (list "----"
                            mime-editor/popup-menu-for-xemacs)))
        (remove-hook 'vm-mail-mode-hook 'mime-editor/attach-to-vm-mode-menu)
        )))

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
                             (sendmail-send-it)
                             )))
    (if (and (string-match "XEmacs\\|Lucid" emacs-version)
             tm-vm/use-xemacs-popup-menu)
        (add-hook 'vm-mail-mode-hook 'mime-editor/attach-to-vm-mode-menu)
      )
    )))

(call-after-loaded
 'mime-setup
 (function
  (lambda ()
    (setq vm-forwarding-digest-type "rfc1521")
    (setq vm-digest-send-type "rfc1521")
    )))


;;; @ for BBDB
;;;

(call-after-loaded
 'bbdb
 (function
  (lambda ()
    (require 'bbdb-vm)
    (require 'tm-bbdb)
    (defun tm-bbdb/vm-update-record (&optional offer-to-create)
      (vm-select-folder-buffer)
      (if (and (tm-vm/system-state)
               mime::article/preview-buffer
               (get-buffer mime::article/preview-buffer))
          (let ((tm-bbdb/auto-create-p bbdb/mail-auto-create-p))
	    (tm-bbdb/update-record offer-to-create))
        (or (bbdb/vm-update-record offer-to-create)
            (delete-windows-on (get-buffer "*BBDB*")))
        ))
    (remove-hook 'vm-select-message-hook 'bbdb/vm-update-record)
    (remove-hook 'vm-show-message-hook 'bbdb/vm-update-record)
    (add-hook 'tm-vm/select-message-hook 'tm-bbdb/vm-update-record)
    )))

;;; @ for ps-print (Suggestted by Anders Stenman <stenman@isy.liu.se>)
;;;

(require 'ps-print)

(add-hook 'vm-mode-hook 'tm-vm/ps-print-setup)
(add-hook 'mime-viewer/define-keymap-hook 'tm-vm/ps-print-setup)
(fset 'vm-toolbar-print-command 'tm-vm/print-message)

(defun tm-vm/ps-print-setup ()
  "Set things up for printing MIME messages with ps-print. Set binding to 
the [Print Screen] key."
  (local-set-key (ps-prsc) 'tm-vm/print-message)
  (setq ps-header-lines 3)
  (setq ps-left-header
        (list 'ps-article-subject 'ps-article-author 'buffer-name)))

(defun tm-vm/print-message ()
  "Print current message with ps-print if it's a MIME message. 
Value of tm-vm/strict-mime is also taken into consideration."
  (interactive)
  (vm-follow-summary-cursor)
  (let* ((mbuf (or (vm-select-folder-buffer) (current-buffer)))
         pbuf)
    (tm-vm/sync-preview-buffer)
    (setq pbuf (and mime::article/preview-buffer
                    (get-buffer mime::article/preview-buffer)))
    (if pbuf
        (save-excursion
          (set-buffer pbuf)
          (require 'ps-print)
          (ps-print-buffer-with-faces))
      (vm-print-message))))

;;; @ end
;;;

(provide 'tm-vm)

(run-hooks 'tm-vm-load-hook)

;;; tm-vm.el ends here.

