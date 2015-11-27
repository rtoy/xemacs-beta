;; gtk-widget-accessors.el --- GTK wrappers for widgets
;;
;; Copyright (C) 2000, 2001 William M. Perry
;;
;; This file is part of XEmacs.

;; XEmacs is free software: you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the
;; Free Software Foundation, either version 3 of the License, or (at your
;; option) any later version.

;; XEmacs is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;; FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
;; for more details.

;; You should have received a copy of the GNU General Public License
;; along with XEmacs.  If not, see <http://www.gnu.org/licenses/>.

(globally-declare-fboundp
 '(gtk-fundamental-type))

(require 'gtk-ffi)

(defconst G_TYPE_INVALID 0)
(defconst G_TYPE_NONE 1)
(defconst G_TYPE_INTERFACE 2)
(defconst G_TYPE_CHAR 3)
(defconst G_TYPE_UCHAR 4)
(defconst G_TYPE_BOOLEAN 5)
(defconst G_TYPE_INT 6)
(defconst G_TYPE_UINT 7)
(defconst G_TYPE_LONG 8)
(defconst G_TYPE_ULONG 9)
(defconst G_TYPE_INT64 10)
(defconst G_TYPE_UINT64 11)
(defconst G_TYPE_ENUM 12)
(defconst G_TYPE_FLAGS 13)
(defconst G_TYPE_FLOAT 14)
(defconst G_TYPE_DOUBLE 15)
(defconst G_TYPE_STRING 16)
(defconst G_TYPE_POINTER 17)
(defconst G_TYPE_BOXED 18)
(defconst G_TYPE_PARAM 19)
(defconst G_TYPE_OBJECT 20)

(defconst gtk-value-accessor-names
  '("INVALID" "NONE" "INTERFACE" "CHAR" "UCHAR" "BOOLEAN" "INT" "UINT"
    "LONG" "ULONG" "INT64" "UINT64" "ENUM" "FLAGS" "FLOAT" "DOUBLE"
    "STRING" "POINTER" "BOXED" "PARAM" "OBJECT"))

(defun define-widget-accessors (gtk-class
				wrapper
				prefix args)
  "Output stub C code to access parts of a widget from lisp.
GTK-CLASS is the GTK class to grant access to.
WRAPPER is a fragment to construct GTK C macros for typechecking/etc. (ie: WIDGET)
ARGS is a list of (type . name) cons cells.
Defines a whole slew of functions to access & set the slots in the
structure."
  (set-buffer (get-buffer-create "emacs-widget-accessors.c"))
  (goto-char (point-max))
  (let ((arg)
	(base-arg-type nil)
	(lisp-func-name nil)
	(c-func-name nil)
	(func-names nil))
    (setq gtk-class (symbol-name gtk-class)
	  wrapper (upcase wrapper))
    (while (setq arg (pop args))
      (setq lisp-func-name (format "gtk-%s-%s" prefix (cdr arg))
	    lisp-func-name (replace-in-string lisp-func-name "_" "-")
	    c-func-name (concat "F" (replace-in-string lisp-func-name "-" "_")))
      (insert
       "DEFUN (\"" lisp-func-name "\", " c-func-name ", 1, 1, 0, /*\n"
       "Access the `" (symbol-name (cdr arg)) "' slot of OBJ, a " gtk-class " object.\n"
       "*/\n"
       "\t(obj))\n"
       "{\n"
       (format "\t%s *the_obj = NULL;\n" gtk-class)
       "\tGValue arg;\n"
       "\n"
       "\tCHECK_GTK_OBJECT (obj);\n"
       "\n"
       (format "\tif (!GTK_IS_%s (XGTK_OBJECT (obj)->object))\n" wrapper)
       "\t{\n"
       (format "\t\twtaerror (\"Object is not a %s\", obj);\n" gtk-class)
       "\t};\n"
       "\n"
       (format "\tthe_obj = G_%s (XGTK_OBJECT (obj)->object);\n" wrapper)

       ;;(format "\targ.type = g_type_from_name (\"%s\");\n" (symbol-name (car arg)))
       )
;       (format "\targ.type = GTK_TYPE_%s;\n" (or
;					       (nth (gtk-fundamental-type (car arg))
;						    gtk-value-accessor-names)
;					       (case (car arg)
;						 (GtkListOfString "STRING_LIST")
;						 (GtkListOfObject "OBJECT_LIST")
;						 (otherwise
;						  "POINTER")))))

      (setq base-arg-type (gtk-fundamental-type (car arg)))
      (cond
       ((= base-arg-type G_TYPE_OBJECT)
	(insert
	 (format "\tG_VALUE_OBJECT (arg) = G_OBJECT (the_obj->%s);"
		 (cdr arg))))
       ((or (= base-arg-type G_TYPE_POINTER)
	    (= base-arg-type G_TYPE_BOXED))
	(insert
	 (format "\tG_VALUE_%s (arg) = (void *)the_obj->%s;"
		 (nth (gtk-fundamental-type (car arg)) gtk-value-accessor-names)
		 (cdr arg))))
       (t
	(insert
	 (format "\tG_VALUE_%s (arg) = the_obj->%s;"
		 (or (nth (gtk-fundamental-type (car arg)) gtk-value-accessor-names) "POINTER")
		 (cdr arg)))))
      (insert
       "\n"
       "\treturn (g_type_to_lisp (&arg));\n"
       "}\n\n")
      (push c-func-name func-names))
    func-names))

(defun import-widget-accessors (file syms-function-name &rest description)
  "Import multiple widgets, and emit a suitable vars_of_foo() function for them.\n"
  (declare (special c-mode-common-hook c-mode-hook))
  (let ((c-mode-common-hook nil)
	(c-mode-hook nil))
    (find-file file))
  (erase-buffer)
  (insert "/* This file was automatically generated by ../lisp/gtk-widget-accessors.el */\n"
	  "/* DO NOT EDIT BY HAND!!! */\n")
  (let ((c-funcs nil))
    (while description
      (setq c-funcs (nconc (define-widget-accessors
			     (pop description) (pop description)
			     (pop description) (pop description)) c-funcs)))
    (goto-char (point-max))
    (insert "void " syms-function-name " (void)\n"
	    "{\n\t"
	    (mapconcat (lambda (x)
			 (concat "DEFSUBR (" x ");")) c-funcs "\n\t")
	    "\n}"))
  (save-buffer))

;; Because the new FFI layer imports GTK types lazily, we need to load
;; up all of the gtk types we know about, or we get errors about
;; unknown GTK types later on.
(mapatoms (lambda (sym)
	    (if (string-match "gtk-[^-]+-get-type" (symbol-name sym))
		(funcall sym))))

(import-widget-accessors
 "../src/emacs-widget-accessors.c"
 "syms_of_widget_accessors "

 'GtkAdjustment "ADJUSTMENT" "adjustment"
 '((gfloat . lower)
   (gfloat . upper)
   (gfloat . value)
   (gfloat . step_increment)
   (gfloat . page_increment)
   (gfloat . page_size))

 'GtkWidget "WIDGET" "widget"
 '(;(GtkStyle     . style)
   ;;(GdkWindow    . window)
   (gpointer	. window)      
   (gchararray    . name)
   (GtkWidget    . parent))

 'GtkButton "BUTTON" "button"
 '((GtkWidget  . child)
   (gboolean   . in_button)
   (gboolean   . button_down))

 'GtkCombo "COMBO" "combo"
 '((GtkWidget  . entry)
   (GtkWidget  . button)
   (GtkWidget  . popup)
   (GtkWidget  . popwin)
   (GtkWidget  . list))

 'GtkGammaCurve "GAMMA_CURVE" "gamma-curve"
 '((GtkWidget  . table)
   (GtkWidget  . curve)
   (gfloat      . gamma)
   (GtkWidget  . gamma_dialog)
   (GtkWidget  . gamma_text))

 'GtkCheckMenuItem "CHECK_MENU_ITEM" "check-menu-item"
 '((gboolean   . active))

 'GtkNotebook "NOTEBOOK" "notebook"
 '((GtkPositionType . tab_pos))

 'GtkText "TEXT" "text"
 '((GtkAdjustment . hadj)
   (GtkAdjustment . vadj))

 'GtkFileSelection "FILE_SELECTION" "file-selection"
 '((GtkWidget . dir_list)
   (GtkWidget . file_list)
   (GtkWidget . selection_entry)
   (GtkWidget . selection_text)
   (GtkWidget . main_vbox)
   (GtkWidget . ok_button)
   (GtkWidget . cancel_button)
   (GtkWidget . help_button)
   (GtkWidget . action_area))

 'GtkFontSelectionDialog "FONT_SELECTION_DIALOG" "font-selection-dialog"
 '((GtkWidget . fontsel)
   (GtkWidget . main_vbox)
   (GtkWidget . action_area)
   (GtkWidget . ok_button)
   (GtkWidget . apply_button)
   (GtkWidget . cancel_button))

 'GtkColorSelectionDialog "COLOR_SELECTION_DIALOG" "color-selection-dialog"
 '((GtkWidget . colorsel)
   (GtkWidget . main_vbox)
   (GtkWidget . ok_button)
   (GtkWidget . reset_button)
   (GtkWidget . cancel_button)
   (GtkWidget . help_button))

 'GtkDialog "DIALOG" "dialog"
 '((GtkWidget . vbox)
   (GtkWidget . action_area))

 'GtkInputDialog "INPUT_DIALOG" "input-dialog"
 '((GtkWidget . close_button)
   (GtkWidget . save_button))

 'GtkPlug "PLUG" "plug"
 '((GdkWindow . socket_window)
   (gint      . same_app))

 'GtkObject "OBJECT" "object"
 '((guint     . flags)
   (guint     . ref_count))

 'GtkPaned "PANED" "paned"
 '((GtkWidget . child1)
   (GtkWidget . child2)
   (gboolean  . child1_resize)
   (gboolean  . child2_resize)
   (gboolean  . child1_shrink)
   (gboolean  . child2_shrink))

;;  'GtkCList "CLIST" "clist"
;;  '((gint . rows)
;;    (gint . columns)
;;    (GtkAdjustment . hadjustment)
;;    (GtkAdjustment . vadjustment)
;;    (GtkSortType   . sort_type)
;;    (gint . focus_row)
;;    (gint . sort_column))

;;  'GtkList "LIST" "list"
;;  '((GtkListOfObject . children)
;;    (GtkListOfObject . selection))

;;  'GtkTree "TREE" "tree"
;;  '((GtkListOfObject . children)
;;    (GtkTree         . root_tree)
;;    (GtkWidget       . tree_owner)
;;    (GtkListOfObject . selection))

 'GtkTreeItem "TREE_ITEM" "tree-item"
 '((GtkWidget       . subtree))

 'GtkScrolledWindow "SCROLLED_WINDOW" "scrolled-window"
 '((GtkWidget . hscrollbar)
   (GtkWidget . vscrollbar)
   (gboolean  . hscrollbar_visible)
   (gboolean  . vscrollbar_visible))

 )
