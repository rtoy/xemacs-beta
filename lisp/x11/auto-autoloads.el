;;; DO NOT MODIFY THIS FILE
(if (featurep 'x11-autoloads) (error "Already loaded"))

;;;### (autoloads (font-menu-weight-constructor font-menu-size-constructor font-menu-family-constructor reset-device-font-menus) "x-font-menu" "x11/x-font-menu.el")

(defvar font-menu-ignore-scaled-fonts t "\
*If non-nil, then the font menu will try to show only bitmap fonts.")

(defvar font-menu-this-frame-only-p nil "\
*If non-nil, then changing the default font from the font menu will only
affect one frame instead of all frames.")

(fset 'install-font-menus 'reset-device-font-menus)

(autoload 'reset-device-font-menus "x-font-menu" "\
Generates the `Font', `Size', and `Weight' submenus for the Options menu.
This is run the first time that a font-menu is needed for each device.
If you don't like the lazy invocation of this function, you can add it to
`create-device-hook' and that will make the font menus respond more quickly
when they are selected for the first time.  If you add fonts to your system, 
or if you change your font path, you can call this to re-initialize the menus." nil nil)

(autoload 'font-menu-family-constructor "x-font-menu" nil nil nil)

(autoload 'font-menu-size-constructor "x-font-menu" nil nil nil)

(autoload 'font-menu-weight-constructor "x-font-menu" nil nil nil)

;;;***

(provide 'x11-autoloads)
