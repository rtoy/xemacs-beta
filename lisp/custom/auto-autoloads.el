;;; DO NOT MODIFY THIS FILE
(if (featurep 'custom-autoloads) (error "Already loaded"))

;;;### (autoloads (customize-menu-create custom-menu-create custom-save-all customize-save-customized customize-browse custom-buffer-create-other-window custom-buffer-create customize-apropos-groups customize-apropos-faces customize-apropos-options customize-apropos customize-saved customize-customized customize-face-other-window customize-face customize-option-other-window customize-option customize-group-other-window customize-group customize customize-save-variable customize-set-variable customize-set-value) "cus-edit" "custom/cus-edit.el")

(autoload 'customize-set-value "cus-edit" "\
Set VARIABLE to VALUE.  VALUE is a Lisp object.

If VARIABLE has a `variable-interactive' property, that is used as if
it were the arg to `interactive' (which see) to interactively read the value.

If VARIABLE has a `custom-type' property, it must be a widget and the
`:prompt-value' property of that widget will be used for reading the value." t nil)

(autoload 'customize-set-variable "cus-edit" "\
Set the default for VARIABLE to VALUE.  VALUE is a Lisp object.

If VARIABLE has a `custom-set' property, that is used for setting
VARIABLE, otherwise `set-default' is used.

The `customized-value' property of the VARIABLE will be set to a list
with a quoted VALUE as its sole list member.

If VARIABLE has a `variable-interactive' property, that is used as if
it were the arg to `interactive' (which see) to interactively read the value.

If VARIABLE has a `custom-type' property, it must be a widget and the
`:prompt-value' property of that widget will be used for reading the value. " t nil)

(autoload 'customize-save-variable "cus-edit" "\
Set the default for VARIABLE to VALUE, and save it for future sessions.
If VARIABLE has a `custom-set' property, that is used for setting
VARIABLE, otherwise `set-default' is used.

The `customized-value' property of the VARIABLE will be set to a list
with a quoted VALUE as its sole list member.

If VARIABLE has a `variable-interactive' property, that is used as if
it were the arg to `interactive' (which see) to interactively read the value.

If VARIABLE has a `custom-type' property, it must be a widget and the
`:prompt-value' property of that widget will be used for reading the value. " t nil)

(autoload 'customize "cus-edit" "\
Select a customization buffer which you can use to set user options.
User options are structured into \"groups\".
Initially the top-level group `Emacs' and its immediate subgroups
are shown; the contents of those subgroups are initially hidden." t nil)

(autoload 'customize-group "cus-edit" "\
Customize GROUP, which must be a customization group." t nil)

(autoload 'customize-group-other-window "cus-edit" "\
Customize SYMBOL, which must be a customization group." t nil)

(defalias 'customize-variable 'customize-option)

(autoload 'customize-option "cus-edit" "\
Customize SYMBOL, which must be a user option variable." t nil)

(defalias 'customize-variable-other-window 'customize-option-other-window)

(autoload 'customize-option-other-window "cus-edit" "\
Customize SYMBOL, which must be a user option variable.
Show the buffer in another window, but don't select it." t nil)

(autoload 'customize-face "cus-edit" "\
Customize SYMBOL, which should be a face name or nil.
If SYMBOL is nil, customize all faces." t nil)

(autoload 'customize-face-other-window "cus-edit" "\
Show customization buffer for FACE in other window." t nil)

(autoload 'customize-customized "cus-edit" "\
Customize all user options set since the last save in this session." t nil)

(autoload 'customize-saved "cus-edit" "\
Customize all already saved user options." t nil)

(autoload 'customize-apropos "cus-edit" "\
Customize all user options matching REGEXP.
If ALL is `options', include only options.
If ALL is `faces', include only faces.
If ALL is `groups', include only groups.
If ALL is t (interactively, with prefix arg), include options which are not
user-settable, as well as faces and groups." t nil)

(autoload 'customize-apropos-options "cus-edit" "\
Customize all user options matching REGEXP.
With prefix arg, include options which are not user-settable." t nil)

(autoload 'customize-apropos-faces "cus-edit" "\
Customize all user faces matching REGEXP." t nil)

(autoload 'customize-apropos-groups "cus-edit" "\
Customize all user groups matching REGEXP." t nil)

(autoload 'custom-buffer-create "cus-edit" "\
Create a buffer containing OPTIONS.
Optional NAME is the name of the buffer.
OPTIONS should be an alist of the form ((SYMBOL WIDGET)...), where
SYMBOL is a customization option, and WIDGET is a widget for editing
that option." nil nil)

(autoload 'custom-buffer-create-other-window "cus-edit" "\
Create a buffer containing OPTIONS.
Optional NAME is the name of the buffer.
OPTIONS should be an alist of the form ((SYMBOL WIDGET)...), where
SYMBOL is a customization option, and WIDGET is a widget for editing
that option." nil nil)

(autoload 'customize-browse "cus-edit" "\
Create a tree browser for the customize hierarchy." t nil)

(defcustom custom-file (if (boundp 'emacs-user-extension-dir) (concat "~" init-file-user emacs-user-extension-dir "options.el") "~/.emacs") "File used for storing customization information.\nIf you change this from the default \"~/.emacs\" you need to\nexplicitly load that file for the settings to take effect." :type 'file :group 'customize)

(autoload 'customize-save-customized "cus-edit" "\
Save all user options which have been set in this session." t nil)

(autoload 'custom-save-all "cus-edit" "\
Save all customizations in `custom-file'." nil nil)

(autoload 'custom-menu-create "cus-edit" "\
Create menu for customization group SYMBOL.
The menu is in a format applicable to `easy-menu-define'." nil nil)

(autoload 'customize-menu-create "cus-edit" "\
Return a customize menu for customization group SYMBOL.
If optional NAME is given, use that as the name of the menu. 
Otherwise the menu will be named `Customize'.
The format is suitable for use with `easy-menu-define'." nil nil)

;;;***

;;;### (autoloads (custom-set-faces custom-initialize-frame custom-declare-face) "cus-face" "custom/cus-face.el")

(autoload 'custom-declare-face "cus-face" "\
Like `defface', but FACE is evaluated as a normal argument." nil nil)

(autoload 'custom-initialize-frame "cus-face" "\
Initialize local faces for FRAME if necessary.
If FRAME is missing or nil, the first member of (frame-list) is used." nil nil)

(autoload 'custom-set-faces "cus-face" "\
Initialize faces according to user preferences.
The arguments should be a list where each entry has the form:

  (FACE SPEC [NOW])

SPEC will be stored as the saved value for FACE.  If NOW is present
and non-nil, FACE will also be created according to SPEC.

See `defface' for the format of SPEC." nil nil)

;;;***

;;;### (autoloads (widget-minor-mode widget-browse-other-window widget-browse widget-browse-at) "wid-browse" "custom/wid-browse.el")

(autoload 'widget-browse-at "wid-browse" "\
Browse the widget under point." t nil)

(autoload 'widget-browse "wid-browse" "\
Create a widget browser for WIDGET." t nil)

(autoload 'widget-browse-other-window "wid-browse" "\
Show widget browser for WIDGET in other window." t nil)

(autoload 'widget-minor-mode "wid-browse" "\
Togle minor mode for traversing widgets.
With arg, turn widget mode on if and only if arg is positive." t nil)

;;;***

;;;### (autoloads (widget-delete widget-create widget-prompt-value widget-apply) "wid-edit" "custom/wid-edit.el")

(autoload 'widget-apply "wid-edit" "\
Apply the value of WIDGET's PROPERTY to the widget itself.
ARGS are passed as extra arguments to the function." nil nil)

(autoload 'widget-prompt-value "wid-edit" "\
Prompt for a value matching WIDGET, using PROMPT.
The current value is assumed to be VALUE, unless UNBOUND is non-nil." nil nil)

(autoload 'widget-create "wid-edit" "\
Create widget of TYPE.  
The optional ARGS are additional keyword arguments." nil nil)

(autoload 'widget-delete "wid-edit" "\
Delete WIDGET." nil nil)

;;;***

(provide 'custom-autoloads)
