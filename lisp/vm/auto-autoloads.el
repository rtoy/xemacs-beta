;;; DO NOT MODIFY THIS FILE
(if (not (featurep 'vm-autoloads))
    (progn

;;;### (autoloads (vm-easy-menu-create-keymaps vm-easy-menu-define) "vm-easymenu" "vm/vm-easymenu.el")

(autoload 'vm-easy-menu-define "vm-easymenu" "\
Define a menu bar submenu in maps MAPS, according to MENU.
The menu keymap is stored in symbol SYMBOL, both as its value
and as its function definition.   DOC is used as the doc string for SYMBOL.

The first element of MENU must be a string.  It is the menu bar item name.
The rest of the elements are menu items.

A menu item is usually a vector of three elements:  [NAME CALLBACK ENABLE]

NAME is a string--the menu item name.

CALLBACK is a command to run when the item is chosen,
or a list to evaluate when the item is chosen.

ENABLE is an expression; the item is enabled for selection
whenever this expression's value is non-nil.

Alternatively, a menu item may have the form: 

   [ NAME CALLBACK [ KEYWORD ARG ] ... ]

Where KEYWORD is one of the symbol defined below.

   :keys KEYS

KEYS is a string; a complex keyboard equivalent to this menu item.
This is normally not needed because keyboard equivalents are usually
computed automatically.

   :active ENABLE

ENABLE is an expression; the item is enabled for selection
whenever this expression's value is non-nil.

   :suffix NAME

NAME is a string; the name of an argument to CALLBACK.

   :style 
   
STYLE is a symbol describing the type of menu item.  The following are
defined:  

toggle: A checkbox.  
        Currently just prepend the name with the string \"Toggle \".
radio: A radio button. 
nil: An ordinary menu item.

   :selected SELECTED

SELECTED is an expression; the checkbox or radio button is selected
whenever this expression's value is non-nil.
Currently just disable radio buttons, no effect on checkboxes.

A menu item can be a string.  Then that string appears in the menu as
unselectable text.  A string consisting solely of hyphens is displayed
as a solid horizontal line.

A menu item can be a list.  It is treated as a submenu.
The first element should be the submenu name.  That's used as the
menu item in the top-level menu.  The cdr of the submenu list
is a list of menu items, as above." nil 'macro)

(autoload 'vm-easy-menu-create-keymaps "vm-easymenu" nil nil nil)

;;;***

(provide 'vm-autoloads)
))
