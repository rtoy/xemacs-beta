;;; DO NOT MODIFY THIS FILE
(if (not (featurep 'gnats-autoloads))
    (progn

;;;### (autoloads (gnats:summ-pr gnats:query-pr gnats:edit-pr gnats:view-pr gnats:gnats-mode) "gnats" "gnats/gnats.el")

(defvar gnats::mode-name nil "\
Name of the GNATS mode.")

(setq gnats::mode-name 'gnats:gnats-mode)

(fset 'gnats-mode gnats::mode-name)

(autoload 'gnats:gnats-mode "gnats" "\
Major mode for editing problem reports.
For information about the form see gnats(1) and pr_form(5).

When you are finished editing the buffer, type \\[gnats:submit-pr] to commit
your changes to the PR database.  To abort the edit, type
\\[gnats:unlock-buffer].

Special commands:
\\{gnats-mode-map}
Turning on gnats-mode calls the value of the variable gnats-mode-hook,
if it is not nil." nil nil)

(fset 'view-pr 'gnats:view-pr)

(autoload 'gnats:view-pr "gnats" "\
Visit the problem report named by the string ID.  While viewing, press
'e' to edit the currently viewed PR." t nil)

(fset 'edit-pr 'gnats:edit-pr)

(autoload 'gnats:edit-pr "gnats" "\
Edit the problem report named by the string ID." t nil)

(fset 'query-pr 'gnats:query-pr)

(autoload 'gnats:query-pr "gnats" "\
Run query-pr, with user-specified args, and collect output in a buffer.
While query-pr runs asynchronously, you can use the \\[next-error] command
to find the text that the hits refer to." t nil)

(fset 'summ-pr 'gnats:summ-pr)

(autoload 'gnats:summ-pr "gnats" "\
Run query-pr, with user-specified args, and display a pretty summary.
Well, display a summary, at least." t nil)

;;;***

;;;### (autoloads (send-pr:send-pr-mode send-pr:send-pr) "send-pr" "gnats/send-pr.el")

(fset 'send-pr 'send-pr:send-pr)

(autoload 'send-pr:send-pr "send-pr" "\
Create a buffer and read in the result of `send-pr -P'.
When finished with editing the problem report use \\[send-pr:submit-pr]
to send the PR with `send-pr -b -f -'." t nil)

(fset 'send-pr-mode 'send-pr:send-pr-mode)

(autoload 'send-pr:send-pr-mode "send-pr" "\
Major mode for submitting problem reports.
For information about the form see gnats(1) and send-pr(1).
Special commands: \\{send-pr-mode-map}
Turning on send-pr-mode calls the value of the variable send-pr-mode-hook,
if it is not nil." t nil)

;;;***

(provide 'gnats-autoloads)
))
