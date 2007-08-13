;;; Quail <-> EGG interface

(require 'quail)

(or (featurep 'egg)
    (error "You don't have EGG feature."))

(defconst quail-mode-egg-map (make-keymap))
(set-keymap-parents quail-mode-egg-map (list quail-mode-map))
(define-key quail-mode-egg-map " " 'quail-henkan-region))

(defvar quail-henkan-start nil)
(make-variable-buffer-local 'quail-henkan-start)

(defvar quail-henkan-start-char ?◇)

(defconst quail-henkan-mode-map (make-keymap))
(set-keymap-parents quail-henkan-mode-map (list henkan-mode-map))

(substitute-key-definition 'self-insert-command
			   'quail-henkan-kakutei-and-self-insert
			   quail-henkan-mode-map)

(define-key quail-henkan-mode-map "\C-k" 'quail-henkan-kakutei)
(define-key quail-henkan-mode-map "\C-l" 'quail-henkan-kakutei)
(define-key quail-henkan-mode-map "\C-m" 'quail-henkan-kakutei)
(define-key quail-henkan-mode-map "\C-c" 'quail-henkan-quit)
(define-key quail-henkan-mode-map "\C-g" 'quail-henkan-quit)
(define-key quail-henkan-mode-map "\177" 'quail-henkan-quit)
(define-key quail-henkan-mode-map "\eg" 'quail-henkan-quit))

(defun quail-henkan-mark ()
  "Set mark at the current position to indicate starting point of henkan."
  (interactive)
  (quail-delete-region)
  (quail-init-state)
  (setq quail-henkan-start (point-marker))
  (insert quail-henkan-start-char))

(defun quail-henkan-region ()
  (interactive)
  (quail-delete-region)
  (quail-init-state)
  (if quail-henkan-start
      (let ((pos (point-marker)))
	(goto-char quail-henkan-start)
	(if (and (= (following-char) quail-henkan-start-char)
		 (progn (delete-char 1)
			(< quail-henkan-start pos)))
	    (let ((henkan-mode-map quail-henkan-mode-map))
	      (henkan-region quail-henkan-start pos))
	  (goto-char pos))
	(setq quail-henkan-start nil))))

(defun quail-henkan-kakutei-and-self-insert ()
  (interactive)
  (setq unread-command-events (list last-command-event))
  (quail-henkan-kakutei))

(defun quail-henkan-reset ()
  (egg:bunsetu-attribute-off bunsetu-number)
  (egg:henkan-attribute-off)
  (goto-char egg:region-start)
  (delete-region (- egg:region-start (length egg:henkan-open))
		 (+ egg:region-end (length egg:henkan-close)))
  (set-marker egg:region-start nil) 
  (set-marker egg:region-end nil) 
  (setq quail-henkan-start nil)
  (quail-init-state)
  (use-local-map (quail-mode-map))
  (egg:mode-line-display))

(defun quail-henkan-kakutei ()
  (interactive)
  (quail-henkan-reset)
  (let ((i 0) (max (wnn-server-bunsetu-suu)))
    (while (< i max)
      (insert (car (wnn-server-bunsetu-kanji i )))
      (if (not overwrite-mode)
	  (undo-boundary))
      (setq i (1+ i))
      ))
  (wnn-server-hindo-update)
  (egg:mode-line-display))

(defun quail-henkan-quit ()
  (interactive)
  (quail-henkan-reset)
  (insert egg:kanji-kanabuff)
  (wnn-server-henkan-quit)
  (egg:mode-line-display))
