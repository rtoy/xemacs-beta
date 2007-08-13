;;; DO NOT MODIFY THIS FILE
(if (featurep 'hm--html-menus-autoloads) (error "Already loaded"))

;;;### (autoloads (hm--html-minor-mode hm--html-mode) "hm--html-mode" "hm--html-menus/hm--html-mode.el")

(autoload 'hm--html-mode "hm--html-mode" "\
Major mode for editing HTML hypertext documents.  
Special commands:\\{hm--html-mode-map}
Turning on hm--html-mode calls the value of the variable hm--html-mode-hook,
if that value is non-nil." t nil)

(autoload 'hm--html-minor-mode "hm--html-mode" "\
Toggle hm--html-minor-mode.
With arg, turn hm--html-minor-mode on iff arg is positive." t nil)

;;;***

;;;### (autoloads (html-view-get-display html-view-goto-url html-view-view-buffer html-view-view-file html-view-start-mosaic) "html-view" "hm--html-menus/html-view.el")

(autoload 'html-view-start-mosaic "html-view" "\
Start Mosaic." t nil)

(autoload 'html-view-view-file "html-view" "\
View an html file with Mosaic." t nil)

(autoload 'html-view-view-buffer "html-view" "\
View html buffer with Mosaic.
If BUFFER-TO-VIEW is nil, then the current buffer is used." t nil)

(autoload 'html-view-goto-url "html-view" "\
Goto an URL in Mosaic." t nil)

(autoload 'html-view-get-display "html-view" "\
Get the display for Mosaic." t nil)

;;;***

;;;### (autoloads (tmpl-insert-template-file tmpl-insert-template-file-from-fixed-dirs tmpl-expand-templates-in-buffer tmpl-expand-templates-in-region) "tmpl-minor-mode" "hm--html-menus/tmpl-minor-mode.el")

(autoload 'tmpl-expand-templates-in-region "tmpl-minor-mode" "\
Expands the templates in the region from BEGIN to END.
If BEGIN and END are nil, then the current region is used." t nil)

(autoload 'tmpl-expand-templates-in-buffer "tmpl-minor-mode" "\
Expands all templates in the current buffer." t nil)

(autoload 'tmpl-insert-template-file-from-fixed-dirs "tmpl-minor-mode" "\
Inserts a template FILE and expands it, if `tmpl-automatic-expand' is t.
This command tries to read the template file from a list of
predefined directories (look at `tmpl-template-dir-list') and it filters
the contents of these directories with the regular expression
`tmpl-filter-regexp' (look also at this variable). 
The command uses a history variable, which could be changed with the
variable `tmpl-history-variable-name'.

The user of the command is able to change interactively to another
directory by entering at first the string \"Change the directory\".
This may be too difficult for the user. Therefore another command
called `tmpl-insert-template-file' exist, which doesn't use fixed
directories and filters." t nil)

(autoload 'tmpl-insert-template-file "tmpl-minor-mode" "\
Inserts a template FILE and expand it, if `tmpl-automatic-expand' is t.
Look also at `tmpl-template-dir-list', to specify a default template directory.
You should also take a look at `tmpl-insert-template-file-from-fixed-dirs'
which has additional advantages (and disadvantages :-).

ATTENTION: The interface of this function has changed. The old 
function had the argument list (&optional TEMPLATE-DIR AUTOMATIC-EXPAND).
The variables `tmpl-template-dir-list' and `tmpl-automatic-expand' must
now be used instead of the args TEMPLATE-DIR and AUTOMATIC-EXPAND." t nil)

;;;***

(provide 'hm--html-menus-autoloads)
