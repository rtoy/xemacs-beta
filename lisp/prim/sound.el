;;; sound.el --- Loading sound files in XEmacs

;; Copyright (C) 1985, 1986, 1992, 1993, 1994 Free Software Foundation, Inc.
;; Copyright (C) 1995 Tinker Systems and INS Engineering Corp.

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

;;; Synched up with: Not in FSF.

;;;###autoload
(or sound-alist
    ;; these should be silent until sounds are loaded
    (setq sound-alist '((ready nil) (warp nil))))

;;;###autoload
(defun load-sound-file (filename sound-name &optional volume)
  "Read in an audio-file and add it to the sound-alist.

You can only play sound files if you are running on display 0 of the
console of a machine with native sound support or running a NetAudio
server and XEmacs has the necessary sound support compiled in.

The sound file must be in the Sun/NeXT U-LAW format."
  (interactive "fSound file name: \n\
SSymbol to name this sound: \n\
nVolume (0 for default): ")
  (or (symbolp sound-name) (error "sound-name not a symbol"))
  (or (null volume) (integerp volume) (error "volume not an integer or nil"))
  (let (buf data)
    (unwind-protect
	(save-excursion
	  (set-buffer (setq buf (get-buffer-create " *sound-tmp*")))
	  (buffer-disable-undo (current-buffer))
	  (erase-buffer)
	  (insert-file-contents filename)
	  (setq data (buffer-string))
	  (erase-buffer))
      (and buf (kill-buffer buf)))
    (let ((old (assq sound-name sound-alist)))
      ;; some conses in sound-alist might have been dumped with emacs.
      (if old (setq sound-alist (delq old (copy-sequence sound-alist)))))
    (setq sound-alist (cons
			(purecopy
			 (nconc (list sound-name)
				(if (and volume (not (eq 0 volume)))
				    (list ':volume volume))
			       (list ':sound data)))
			sound-alist)))
  sound-name)

;;;###autoload
(defun load-default-sounds ()
  "Load and install some sound files as beep-types.
This only works if you're on display 0 of a Sun SparcStation, SGI machine,
or HP9000s700, or running a NetAudio server."
  (interactive)
  ;; #### - this should do NOTHING if the sounds can't be played.  
  (message "Loading sounds...")
  (setq sound-alist nil)
  (let ((default-directory data-directory))
    (load-sound-file "sounds/drum-beep.au"	'drum)
    (load-sound-file "sounds/quiet-beep.au"	'quiet)
    (load-sound-file "sounds/bass-snap.au"	'bass 80)
    (load-sound-file "sounds/whip.au"		'whip 70)
    (load-sound-file "sounds/cuckoo.au"		'cuckoo)
    (load-sound-file "sounds/yeep.au"		'yeep)
    (load-sound-file "sounds/hype.au"		'hype 100)
    )
  (setq sound-alist
	(append
	 '((default		:sound bass)
	   (undefined-key	:sound drum)
	   (undefined-click	:sound drum)
	   ;; beginning-of-buffer or end-of-buffer errors.
	   (buffer-bound	:sound drum)
	   ;; buffer-read-only error
	   (read-only	        :sound drum)
	   ;; non-interactive function or lambda called
	   (command-error	:sound bass)
	   (y-or-n-p		:sound quiet)
	   (yes-or-no-p		:sound quiet)
	   (auto-save-error	:sound whip :volume 100)
	   (no-completion	:sound whip)
	   (isearch-failed	:sound quiet)
	   (isearch-quit	:sound bass)
	   ;; QUIT: sound generated by ^G and it's variants.
	   (quit		:sound quiet :volume 75)
	   ;; READY: time-consuming task has completed...  compile,
	   ;; cvs-update, etc.
	   (ready		:sound cuckoo)
	   ;; WARP: XEmacs has changed the selected-window or frame
	   ;; asynchronously...  Especially when it's done by an
	   ;; asynchronous process filter.  Perhaps by a debugger breakpoint
	   ;; has been hit?
	   (warp		:sound yeep :volume 75)
	   ;; ALARM: used for reminders...
	   (alarm		:sound cuckoo :volume 100)
	   )
	 sound-alist))
  (message "Loading sounds...done")
  ;; (beep nil 'quiet)
  )

;; sound.el ends here.
