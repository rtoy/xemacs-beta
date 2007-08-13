;;; isearch-ext.el --- incremental search with front-end inputting method

;; Author: SAKAI Kiyotaka <ksakai@mtl.t.u-tokyo.ac.jp>
;; Keywords: search

;; !Id: isearch-ext.el,v 1.41 1994/12/16 15:33:34 ksakai Exp !

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
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; This program is extension of isearch.el to support multi-lingal
;; incremental search with front-end input method.
;;
;; If you want to use this program, simply put C-\ or C-o or C-[ when
;; doing incremental search, and you can input search words with
;; inputting method.
;;
;; For backward compatibility with mule-1.x, you can also use C-k, but
;; isearch-edit-string may be more suitable for this use.  If you
;; think so, put the following code in your .emacs.
;;
;;   (define-key isearch-mode-map "\C-k" 'isearch-edit-string)
;;

;; Following people contributed modifications to isearch-ext.el:
;;   Kenichi Handa <handa@etlken.etl.go.jp>
;;   YAMAMOTO Mitsuharu <mituharu@is.s.u-tokyo.ac.jp>
;;   A. Sasaki <beckun@cis.canon.co.jp>
;;   Atsuo Ohki <ohki@gssm.otsuka.tsukuba.ac.jp>

;;; Code:

;; #### This is far from working in XEmacs.

(eval-when-compile (require 'quail))


;;;###autoload
(defvar search-string-char-prompt "*Enter string... ")

(defvar isearch-fep-prompt "" "Prompt for isearch-fep mode.")
(defvar isearch-fep-mode nil "If t, isearch-fep-mode is invoked.")

(defconst isearch-fep-table
  '((isearch-fep-string isearch-fep-prompt-string isearch-fep-read-string)
    (isearch-fep-egg    isearch-fep-prompt-egg    isearch-fep-read-egg)
    (isearch-fep-canna  isearch-fep-prompt-canna  isearch-fep-read-canna)
    (isearch-fep-quail  isearch-fep-prompt-quail  isearch-fep-read-quail)))

;; the followings are defined in isearch.el
(define-key isearch-mode-map "\C-k"  'isearch-fep-string)
(define-key isearch-mode-map "\C-\\" 'isearch-fep-egg)
(define-key isearch-mode-map "\M-k"  'isearch-fep-egg)
(define-key isearch-mode-map "\C-o"  'isearch-fep-canna)
;;  (define-key isearch-mode-map "\C-\]" 'isearch-fep-quail)

(defun isearch-fep-mode ()
  (let ((command this-command)
	(isearch-fep-mode t)
	table str)
    (while isearch-fep-mode
      (setq table (assq command isearch-fep-table))
      (setq isearch-fep-prompt (funcall (car (cdr table))))
      (message "%s%s" isearch-fep-prompt (isearch-message))
      (if (eq command 'isearch-fep-string)    ;; \C-k
	  (progn
	    (setq str (funcall (nth 2 table)))
	    (setq isearch-fep-mode nil)
	    (isearch-process-search-string str str))
	(let* ((keys (read-key-sequence nil))
	       (current-command (key-binding keys t)))
	  (setq isearch-fep-mode (not (eq command current-command)))
	  (if isearch-fep-mode
	      (if (assq current-command isearch-fep-table)
		  (setq command current-command)
		(cond ((eq current-command 'isearch-printing-char)
		       (setq str (funcall (nth 2 table) keys))
		       (isearch-process-search-string str str))
		      ((or (eq current-command 'isearch-other-control-char)
			   (eq current-command 'isearch-other-meta-char))
		       (call-interactively current-command)
		       (setq isearch-fep-mode nil))
		      ((eq current-command 'isearch-exit)
		       (setq isearch-fep-mode nil)
		       (message "%s%s"
				(isearch-message-prefix) isearch-message))
		      (t
		       (call-interactively current-command))))
	    (setq isearch-fep-prompt nil)
	    (message "%s%s" (isearch-message-prefix) isearch-message)))))))

;;
;;  Read string from minibuffer for incremental search.
;;

;;;###autoload
(defun isearch-fep-string ()
  "Read string from minibuffer for incremental search."
  (interactive)
  (isearch-fep-mode))

(defun isearch-fep-prompt-string ()
  search-string-char-prompt)

(defun exit-minibuffer-and-isearch-backward ()
  (interactive)
  (setq unread-command-events
	(nconc unread-command-events
	       (list (character-to-event ?\r) (character-to-event ?\r))))
  (exit-minibuffer))

(defun isearch-fep-read-string ()
  (save-excursion
    (set-buffer (window-buffer (minibuffer-window)))
    (let* ((overriding-local-map nil)
	   (minibuffer-local-map (cons 'keymap minibuffer-local-map)))
      ;; Some program overwrites "\C-m"'s default binding.
      (define-key minibuffer-local-map "\C-m" 'exit-minibuffer)
      (define-key minibuffer-local-map "\C-s" 'exit-minibuffer)
      (define-key minibuffer-local-map "\C-r"
	'exit-minibuffer-and-isearch-backward)
      (condition-case condition
	  (read-from-minibuffer (concat isearch-fep-prompt (isearch-message)))
	(quit
	 (isearch-abort))))))


;;
;;  For EGG
;;

;;;###autoload
(defun isearch-fep-egg ()
  "Read string for incremental search by using egg."
  (interactive)
  (isearch-fep-mode))

(defun isearch-fep-prompt-egg ()
  (if (featurep 'egg)
      (format "[%s]" (map-indicator its:*current-map*))
    (setq isearch-fep-mode nil)
    (message "No EGG!! ")
    (sit-for 1)
    ""))

(defun isearch-exit-minibuffer-egg (from to)
  (exit-minibuffer))

(defvar isearch-fep-egg-its-map nil)
(defvar isearch-fep-egg-server-type nil)

(defun isearch-minibuffer-setup-egg ()
  (setq its:*current-map* isearch-fep-egg-its-map)
  (setq wnn-server-type isearch-fep-egg-server-type))

(defun isearch-fep-read-egg (first-str)
  (if (and (featurep 'egg) (= (minibuffer-depth) 0))
      (let ((isearch-fep-egg-its-map its:*current-map*)
	    (isearch-fep-egg-server-type wnn-server-type)
	    (minibuffer-setup-hook 'isearch-minibuffer-setup-egg))
	(save-excursion
	  (set-buffer (window-buffer (minibuffer-window)))
	  (let ((display-minibuffer-mode-in-minibuffer t)
		(egg:*input-mode* t)
		(egg:*mode-on* t)
		(self-insert-after-hook 'isearch-exit-minibuffer-egg))
	    (setq unread-command-events (listify-key-sequence first-str))
	    (unwind-protect
		(read-from-minibuffer (isearch-message))
	      (setq egg:henkan-mode-in-use nil)
	      (setq disable-undo nil)))))
    ""))


;;
;;  For Canna
;;

;;;###autoload
(defun isearch-fep-canna ()
  "Read string for incremental search by using canna."
  (interactive)
  (isearch-fep-mode))

(defun isearch-fep-prompt-canna ()
  (if (and (featurep 'canna) canna:*initialized*)
      (format "%s" canna:*kanji-mode-string*)
    (setq isearch-fep-mode nil)
    (message "No Canna!! ")
    (sit-for 1)
    ""))

(defun isearch-exit-minibuffer-canna (from to)
  (exit-minibuffer))

(defun isearch-fep-read-canna (first-str)
  (if (and (featurep 'canna) (= (minibuffer-depth) 0))
      (save-excursion
	(set-buffer (window-buffer (minibuffer-window)))
	(let ((display-minibuffer-mode-in-minibuffer t)
	      (canna:*japanese-mode* t)
	      (canna:*japanese-mode-in-minibuffer* t)
	      (canna:*fence-mode* nil)
	      (self-insert-after-hook 'isearch-exit-minibuffer-canna))
	  (setq unread-command-events (listify-key-sequence first-str))
	  (unwind-protect
	      (read-from-minibuffer (isearch-message))
	    ;XEmacs change:
	    (buffer-enable-undo (current-buffer)))))
    ""))


;;
;;  For QUAIL
;;

;;;###autoload
(defun isearch-fep-quail ()
  "Read string for incremental search by using quail."
  (interactive)
  (require 'quail)
  (isearch-fep-mode))

(defun isearch-fep-prompt-quail ()
  "[QUAIL]")

(defun isearch-exit-minibuffer-quail ()
  (if (or quail-current-key quail-current-str)
      nil
    (exit-minibuffer)))

(defun isearch-fep-read-quail (first-str)
  (let ((quail-self-insert-after-hook 'isearch-exit-minibuffer-quail))
    (setq unread-command-events
	  (nconc unread-command-events
		 (cons (character-to-event ?\\)
		       (listify-key-sequence first-str)))
    (unwind-protect
	(read-from-minibuffer
	 (concat isearch-fep-prompt (isearch-message)))
      ;; XEmacs change:
      (buffer-enable-undo (current-buffer))
      ))))


(provide 'isearch-ext)
;;; isearch-ext.el ends here

