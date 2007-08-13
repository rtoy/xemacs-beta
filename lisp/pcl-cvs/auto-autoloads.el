;;; DO NOT MODIFY THIS FILE
(if (not (featurep 'pcl-cvs-autoloads))
    (progn

;;;### (autoloads (pcl-cvs-fontify) "pcl-cvs-xemacs" "pcl-cvs/pcl-cvs-xemacs.el")

(autoload 'pcl-cvs-fontify "pcl-cvs-xemacs" nil nil nil)

;;;***

;;;### (autoloads (cvs-update-other-window cvs-update) "pcl-cvs" "pcl-cvs/pcl-cvs.el")

(autoload 'cvs-update "pcl-cvs" "\
Run a 'cvs update' in the current working directory.  Feed the
output to a *cvs* buffer and run cvs-mode on it.
If optional prefix argument LOCAL is non-nil, 'cvs update -l' is run." t nil)

(autoload 'cvs-update-other-window "pcl-cvs" "\
Run a 'cvs update' in the current working directory.  Feed the
output to a *cvs* buffer, display it in the other window, and run
cvs-mode on it.

If optional prefix argument LOCAL is non-nil, 'cvs update -l' is run." t nil)

;;;***

(provide 'pcl-cvs-autoloads)
))
