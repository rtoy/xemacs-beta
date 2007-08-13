;;; DO NOT MODIFY THIS FILE
(if (not (featurep 'auctex-autoloads))
    (progn

;;;### (autoloads (BibTeX-auto-store) "latex" "auctex/latex.el")

(autoload 'BibTeX-auto-store "latex" "\
This function should be called from bibtex-mode-hook.
It will setup BibTeX to store keys in an auto file." nil nil)

;;;***

;;;### (autoloads nil "tex-info" "auctex/tex-info.el")

;;;***

;;;### (autoloads (japanese-latex-mode japanese-plain-tex-mode) "tex-jp" "auctex/tex-jp.el")

(autoload 'japanese-plain-tex-mode "tex-jp" "\
Major mode for editing files of input for Japanese plain TeX.
Set japanese-TeX-mode to t, and enters plain-tex-mode." t nil)

(autoload 'japanese-latex-mode "tex-jp" "\
Major mode for editing files of input for Japanese plain TeX.
Set japanese-TeX-mode to t, and enters latex-mode." t nil)

;;;***

;;;### (autoloads (TeX-submit-bug-report TeX-insert-quote TeX-auto-generate-global TeX-auto-generate ams-tex-mode) "tex" "auctex/tex.el")

(autoload 'ams-tex-mode "tex" "\
Major mode for editing files of input for AmS TeX.
See info under AUC TeX for documentation.

Special commands:
\\{TeX-mode-map}
 
Entering AmS-tex-mode calls the value of text-mode-hook,
then the value of TeX-mode-hook, and then the value
of AmS-TeX-mode-hook." t nil)

(autoload 'TeX-auto-generate "tex" "\
Generate style file for TEX and store it in AUTO.  
If TEX is a directory, generate style files for all files in the directory." t nil)

(autoload 'TeX-auto-generate-global "tex" "\
Create global auto directory for global TeX macro definitions." t nil)

(autoload 'TeX-insert-quote "tex" "\
Insert the appropriate quote marks for TeX.
Inserts the value of `TeX-open-quote' (normally ``) or `TeX-close-quote'
\(normally '') depending on the context.  If `TeX-quote-after-quote'
is non-nil, this insertion works only after \". 
With prefix argument, always inserts \" characters." t nil)

(autoload 'TeX-submit-bug-report "tex" "\
Submit via mail a bug report on AUC TeX" t nil)

;;;***

(provide 'auctex-autoloads)
))
