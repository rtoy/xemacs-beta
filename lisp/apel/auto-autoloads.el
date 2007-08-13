;;; DO NOT MODIFY THIS FILE
(if (featurep 'apel-autoloads) (error "Already loaded"))

;;;### (autoloads (module-installed-p exec-installed-p file-installed-p get-latest-path add-latest-path add-path) "file-detect" "apel/file-detect.el")

(autoload 'add-path "file-detect" "\
Add PATH to `load-path' if it exists under `default-load-path'
directories and it does not exist in `load-path'.

You can use following PATH styles:
	load-path relative: \"PATH/\"
			(it is searched from `defaul-load-path')
	home directory relative: \"~/PATH/\" \"~USER/PATH/\"
	absolute path: \"/HOO/BAR/BAZ/\"

You can specify following OPTIONS:
	'all-paths	search from `load-path'
			instead of `default-load-path'
	'append		add PATH to the last of `load-path'" nil nil)

(autoload 'add-latest-path "file-detect" "\
Add latest path matched by PATTERN to `load-path'
if it exists under `default-load-path' directories
and it does not exist in `load-path'.

If optional argument ALL-PATHS is specified, it is searched from all
of load-path instead of default-load-path. [file-detect.el]" nil nil)

(autoload 'get-latest-path "file-detect" "\
Return latest directory in default-load-path
which is matched to regexp PATTERN.
If optional argument ALL-PATHS is specified,
it is searched from all of load-path instead of default-load-path." nil nil)

(autoload 'file-installed-p "file-detect" "\
Return absolute-path of FILE if FILE exists in PATHS.
If PATHS is omitted, `load-path' is used." nil nil)

(defvar exec-suffix-list '("") "\
*List of suffixes for executable.")

(autoload 'exec-installed-p "file-detect" "\
Return absolute-path of FILE if FILE exists in PATHS.
If PATHS is omitted, `exec-path' is used.
If suffixes is omitted, `exec-suffix-list' is used." nil nil)

(autoload 'module-installed-p "file-detect" "\
Return t if module is provided or exists in PATHS.
If PATHS is omitted, `load-path' is used." nil nil)

;;;***

;;;### (autoloads (richtext-decode richtext-encode) "richtext" "apel/richtext.el")

(autoload 'richtext-encode "richtext" nil nil nil)

(autoload 'richtext-decode "richtext" nil nil nil)

;;;***

(provide 'apel-autoloads)
