;;; DO NOT MODIFY THIS FILE
(if (featurep 'ediff-autoloads) (error "Already loaded"))

;;;### (autoloads (ediff-show-registry) "ediff-mult" "ediff/ediff-mult.el")

(autoload 'ediff-show-registry "ediff-mult" "\
Display Ediff's registry." t nil)

(defalias 'eregistry 'ediff-show-registry)

;;;***

;;;### (autoloads (ediff-toggle-use-toolbar ediff-toggle-multiframe) "ediff-util" "ediff/ediff-util.el")

(autoload 'ediff-toggle-multiframe "ediff-util" "\
Switch from multiframe display to single-frame display and back.
To change the default, set the variable `ediff-window-setup-function',
which see." t nil)

(autoload 'ediff-toggle-use-toolbar "ediff-util" "\
Enable or disable Ediff toolbar.
Works only in versions of Emacs that support toolbars.
To change the default, set the variable `ediff-use-toolbar-p', which see." t nil)

;;;***

;;;### (autoloads (ediff-documentation ediff-version ediff-revision ediff-patch-buffer ediff-patch-file run-ediff-from-cvs-buffer ediff-merge-revisions-with-ancestor ediff-merge-revisions ediff-merge-buffers-with-ancestor ediff-merge-buffers ediff-merge-files-with-ancestor ediff-merge-files ediff-regions-linewise ediff-regions-wordwise ediff-windows-linewise ediff-windows-wordwise ediff-merge-directory-revisions-with-ancestor ediff-merge-directory-revisions ediff-merge-directories-with-ancestor ediff-merge-directories ediff-directories3 ediff-directory-revisions ediff-directories ediff-buffers3 ediff-buffers ediff-files3 ediff-files) "ediff" "ediff/ediff.el")

(autoload 'ediff-files "ediff" "\
Run Ediff on a pair of files, FILE-A and FILE-B." t nil)

(autoload 'ediff-files3 "ediff" "\
Run Ediff on three files, FILE-A, FILE-B, and FILE-C." t nil)

(defalias 'ediff3 'ediff-files3)

(defalias 'ediff 'ediff-files)

(autoload 'ediff-buffers "ediff" "\
Run Ediff on a pair of buffers, BUFFER-A and BUFFER-B." t nil)

(defalias 'ebuffers 'ediff-buffers)

(autoload 'ediff-buffers3 "ediff" "\
Run Ediff on three buffers, BUFFER-A, BUFFER-B, and BUFFER-C." t nil)

(defalias 'ebuffers3 'ediff-buffers3)

(autoload 'ediff-directories "ediff" "\
Run Ediff on a pair of directories, DIR1 and DIR2, comparing files that have
the same name in both. The third argument, REGEXP, is a regular expression that
can be used to filter out certain file names." t nil)

(defalias 'edirs 'ediff-directories)

(autoload 'ediff-directory-revisions "ediff" "\
Run Ediff on a directory, DIR1, comparing its files with their revisions.
The second argument, REGEXP, is a regular expression that filters the file
names. Only the files that are under revision control are taken into account." t nil)

(defalias 'edir-revisions 'ediff-directory-revisions)

(autoload 'ediff-directories3 "ediff" "\
Run Ediff on three directories, DIR1, DIR2, and DIR3, comparing files that
have the same name in all three. The last argument, REGEXP, is a regular
expression that can be used to filter out certain file names." t nil)

(defalias 'edirs3 'ediff-directories3)

(autoload 'ediff-merge-directories "ediff" "\
Run Ediff on a pair of directories, DIR1 and DIR2, merging files that have
the same name in both. The third argument, REGEXP, is a regular expression that
can be used to filter out certain file names." t nil)

(defalias 'edirs-merge 'ediff-merge-directories)

(autoload 'ediff-merge-directories-with-ancestor "ediff" "\
Merge files in directories DIR1 and DIR2 using files in ANCESTOR-DIR as ancestors.
Ediff merges files that have identical names in DIR1, DIR2. If a pair of files
in DIR1 and DIR2 doesn't have an ancestor in ANCESTOR-DIR, Ediff will merge
without ancestor. The fourth argument, REGEXP, is a regular expression that
can be used to filter out certain file names." t nil)

(autoload 'ediff-merge-directory-revisions "ediff" "\
Run Ediff on a directory, DIR1, merging its files with their revisions.
The second argument, REGEXP, is a regular expression that filters the file
names. Only the files that are under revision control are taken into account." t nil)

(defalias 'edir-merge-revisions 'ediff-merge-directory-revisions)

(autoload 'ediff-merge-directory-revisions-with-ancestor "ediff" "\
Run Ediff on a directory, DIR1, merging its files with their revisions and ancestors.
The second argument, REGEXP, is a regular expression that filters the file
names. Only the files that are under revision control are taken into account." t nil)

(defalias 'edir-merge-revisions-with-ancestor 'ediff-merge-directory-revisions-with-ancestor)

(defalias 'edirs-merge-with-ancestor 'ediff-merge-directories-with-ancestor)

(autoload 'ediff-windows-wordwise "ediff" "\
Compare WIND-A and WIND-B, which are selected by clicking, wordwise.
With prefix argument, DUMB-MODE, or on a non-windowing display, works as
follows:
If WIND-A is nil, use selected window.
If WIND-B is nil, use window next to WIND-A." t nil)

(autoload 'ediff-windows-linewise "ediff" "\
Compare WIND-A and WIND-B, which are selected by clicking, linewise.
With prefix argument, DUMB-MODE, or on a non-windowing display, works as
follows:
If WIND-A is nil, use selected window.
If WIND-B is nil, use window next to WIND-A." t nil)

(autoload 'ediff-regions-wordwise "ediff" "\
Run Ediff on a pair of regions in two different buffers.
Regions (i.e., point and mark) are assumed to be set in advance.
This function is effective only for relatively small regions, up to 200
lines. For large regions, use `ediff-regions-linewise'." t nil)

(autoload 'ediff-regions-linewise "ediff" "\
Run Ediff on a pair of regions in two different buffers.
Regions (i.e., point and mark) are assumed to be set in advance.
Each region is enlarged to contain full lines.
This function is effective for large regions, over 100-200
lines. For small regions, use `ediff-regions-wordwise'." t nil)

(defalias 'ediff-merge 'ediff-merge-files)

(autoload 'ediff-merge-files "ediff" "\
Merge two files without ancestor." t nil)

(autoload 'ediff-merge-files-with-ancestor "ediff" "\
Merge two files with ancestor." t nil)

(defalias 'ediff-merge-with-ancestor 'ediff-merge-files-with-ancestor)

(autoload 'ediff-merge-buffers "ediff" "\
Merge buffers without ancestor." t nil)

(autoload 'ediff-merge-buffers-with-ancestor "ediff" "\
Merge buffers with ancestor." t nil)

(autoload 'ediff-merge-revisions "ediff" "\
Run Ediff by merging two revisions of a file.
The file is the optional FILE argument or the file visited by the current
buffer." t nil)

(autoload 'ediff-merge-revisions-with-ancestor "ediff" "\
Run Ediff by merging two revisions of a file with a common ancestor.
The file is the the optional FILE argument or the file visited by the current
buffer." t nil)

(autoload 'run-ediff-from-cvs-buffer "ediff" "\
Run Ediff-merge on appropriate revisions of the selected file.
First run after `M-x cvs-update'. Then place the cursor on a line describing a
file and then run `run-ediff-from-cvs-buffer'." t nil)

(autoload 'ediff-patch-file "ediff" "\
Run Ediff by patching SOURCE-FILENAME." t nil)

(autoload 'ediff-patch-buffer "ediff" "\
Run Ediff by patching BUFFER-NAME." t nil)

(defalias 'epatch 'ediff-patch-file)

(defalias 'epatch-buffer 'ediff-patch-buffer)

(autoload 'ediff-revision "ediff" "\
Run Ediff by comparing versions of a file.
The file is an optional FILE argument or the file visited by the current
buffer. Use `vc.el' or `rcs.el' depending on `ediff-version-control-package'." t nil)

(defalias 'erevision 'ediff-revision)

(autoload 'ediff-version "ediff" "\
Return string describing the version of Ediff.
When called interactively, displays the version." t nil)

(autoload 'ediff-documentation "ediff" "\
Display Ediff's manual.
With optional NODE, goes to that node." t nil)

;;;***

(provide 'ediff-autoloads)
