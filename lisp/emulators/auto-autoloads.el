;;; DO NOT MODIFY THIS FILE
(if (featurep 'emulators-autoloads) (error "Already loaded"))

;;;### (autoloads (edt-emulation-on) "edt" "emulators/edt.el")

(autoload 'edt-emulation-on "edt" "\
Turn on EDT Emulation." t nil)

;;;***

;;;### (autoloads (teco-command) "teco" "emulators/teco.el")

(autoload 'teco-command "teco" "\
Read and execute a Teco command string." t nil)

;;;***

;;;### (autoloads (tpu-edt-on) "tpu-edt" "emulators/tpu-edt.el")

(fset 'tpu-edt-mode 'tpu-edt-on)

(fset 'tpu-edt 'tpu-edt-on)

(autoload 'tpu-edt-on "tpu-edt" "\
Turn on TPU/edt emulation." t nil)

;;;***

;;;### (autoloads (tpu-set-cursor-bound tpu-set-cursor-free tpu-set-scroll-margins) "tpu-extras" "emulators/tpu-extras.el")

(autoload 'tpu-set-scroll-margins "tpu-extras" "\
Set scroll margins." t nil)

(autoload 'tpu-set-cursor-free "tpu-extras" "\
Allow the cursor to move freely about the screen." t nil)

(autoload 'tpu-set-cursor-bound "tpu-extras" "\
Constrain the cursor to the flow of the text." t nil)

;;;***

;;;### (autoloads (wordstar-mode) "ws-mode" "emulators/ws-mode.el")

(autoload 'wordstar-mode "ws-mode" "\
Major mode with WordStar-like key bindings.

BUGS:
 - Help menus with WordStar commands (C-j just calls help-for-help)
   are not implemented
 - Options for search and replace
 - Show markers (C-k h) is somewhat strange
 - Search and replace (C-q a) is only available in forward direction

No key bindings beginning with ESC are installed, they will work
Emacs-like.

The key bindings are:

  C-a		backward-word
  C-b		fill-paragraph
  C-c		scroll-up-line
  C-d		forward-char
  C-e		previous-line
  C-f		forward-word
  C-g		delete-char
  C-h		backward-char
  C-i		indent-for-tab-command
  C-j		help-for-help
  C-k		ordstar-C-k-map
  C-l		ws-repeat-search
  C-n		open-line
  C-p		quoted-insert
  C-r		scroll-down-line
  C-s		backward-char
  C-t		kill-word
  C-u		keyboard-quit
  C-v		overwrite-mode
  C-w		scroll-down
  C-x		next-line
  C-y		kill-complete-line
  C-z		scroll-up

  C-k 0		ws-set-marker-0
  C-k 1		ws-set-marker-1
  C-k 2		ws-set-marker-2
  C-k 3		ws-set-marker-3
  C-k 4		ws-set-marker-4
  C-k 5		ws-set-marker-5
  C-k 6		ws-set-marker-6
  C-k 7		ws-set-marker-7
  C-k 8		ws-set-marker-8
  C-k 9		ws-set-marker-9
  C-k b		ws-begin-block
  C-k c		ws-copy-block
  C-k d		save-buffers-kill-emacs
  C-k f		find-file
  C-k h		ws-show-markers
  C-k i		ws-indent-block
  C-k k		ws-end-block
  C-k p		ws-print-block
  C-k q		kill-emacs
  C-k r		insert-file
  C-k s		save-some-buffers
  C-k t		ws-mark-word
  C-k u		ws-exdent-block
  C-k C-u	keyboard-quit
  C-k v		ws-move-block
  C-k w		ws-write-block
  C-k x		kill-emacs
  C-k y		ws-delete-block

  C-o c		wordstar-center-line
  C-o b		switch-to-buffer
  C-o j		justify-current-line
  C-o k		kill-buffer
  C-o l		list-buffers
  C-o m		auto-fill-mode
  C-o r		set-fill-column
  C-o C-u	keyboard-quit
  C-o wd	delete-other-windows
  C-o wh	split-window-horizontally
  C-o wo	other-window
  C-o wv	split-window-vertically

  C-q 0		ws-find-marker-0
  C-q 1		ws-find-marker-1
  C-q 2		ws-find-marker-2
  C-q 3		ws-find-marker-3
  C-q 4		ws-find-marker-4
  C-q 5		ws-find-marker-5
  C-q 6		ws-find-marker-6
  C-q 7		ws-find-marker-7
  C-q 8		ws-find-marker-8
  C-q 9		ws-find-marker-9
  C-q a		ws-query-replace
  C-q b		ws-to-block-begin
  C-q c		end-of-buffer
  C-q d		end-of-line
  C-q f		ws-search
  C-q k		ws-to-block-end
  C-q l		ws-undo
  C-q p		ws-last-cursorp
  C-q r		beginning-of-buffer
  C-q C-u	keyboard-quit
  C-q w		ws-last-error
  C-q y		ws-kill-eol
  C-q DEL	ws-kill-bol
" t nil)

;;;***

(provide 'emulators-autoloads)
