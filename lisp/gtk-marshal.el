(defconst name-to-return-type
  '(("INT" . "guint")
    ("CALLBACK" . "GtkCallback")
    ("OBJECT" . "GtkObject *")
    ("POINTER" . "void *")
    ("STRING" . "gchar *")
    ("BOOL" . "gboolean")
    ("DOUBLE" . "gdouble")
    ("FLOAT" . "gfloat")
    ("LIST"  . "void *")
    ("NONE" . nil)))

(defvar defined-marshallers nil)

(defun get-marshaller-name (rval args)
  (concat "emacs_gtk_marshal_" rval "__"
	  (mapconcat 'identity (or args '("NONE")) "_")))

(defun define-marshaller (rval &rest args)
  (let ((name nil)
	(internal-rval (assoc rval  name-to-return-type))
	(ctr 0)
	(func-proto (format "__%s_fn" rval)))
    (if (not internal-rval)
	(error "Do not know return type of `%s'" rval))
    (setq name (get-marshaller-name rval args))

    (if (member name defined-marshallers)
	(error "Attempe to define the same marshaller more than once! %s" name))

    (set-buffer (get-buffer-create "emacs-marshals.c"))
    (goto-char (point-max))

    (if (or (member "FLOAT" args) (member "DOUBLE" args))
	;; We need to special case anything with FLOAT in the argument
	;; list or the parameters get screwed up royally.
	(progn
	  (setq func-proto (concat (format "__%s__" rval)
				   (mapconcat 'identity args "_")
				   "_fn"))
	  (insert "typedef "
		  (or (cdr internal-rval) "void")
		  " (*"
		  func-proto ")("
		  (mapconcat (lambda (x)
			       (cdr (assoc x name-to-return-type))) args ", ")
		  ");\n")))

    (insert "\n"
	    "static void\n"
	    name " (ffi_actual_function func, GtkArg *args)\n"
	    "{\n"
	    (format "  %s rfunc = (%s) func;\n" func-proto func-proto))

    (if (string= "LIST" rval) (setq rval "POINTER"))

    (if (cdr internal-rval)
	;; It has a return type to worry about
	(insert "  " (cdr internal-rval) " *return_val;\n\n"
		(format "  return_val = GTK_RETLOC_%s (args[%d]);\n" rval (length args))
		"  *return_val = ")
      (insert "  "))
    (insert "(*rfunc) (")
    (while args
      (if (/= ctr 0)
	  (insert ", "))
      (insert (format "GTK_VALUE_%s (args[%d])" (car args) ctr))
      (setq args (cdr args)
	    ctr (1+ ctr)))
    (insert ");\n")
    (insert "}\n")))

(save-excursion
  (find-file "../src/emacs-marshals.c")
  (erase-buffer)
  (setq defined-marshallers nil)

  (insert "/* This file was automatically generated by ../lisp/gtk-marshal.el */\n"
	  "/* DO NOT EDIT BY HAND!!! */\n")
  (insert "#define GTK_VALUE_ARRAY(x) GTK_VALUE_POINTER(x)\n\n")
  (insert "#define GTK_VALUE_LIST(x) GTK_VALUE_POINTER(x)\n\n")

  (let ((todo '(
		("BOOL" "OBJECT" "INT")
		("BOOL" "OBJECT" "OBJECT" "OBJECT")
		("BOOL" "OBJECT" "OBJECT")
		("BOOL" "OBJECT" "POINTER")
		("BOOL" "OBJECT" "STRING")
		("BOOL" "OBJECT")
		("BOOL" "POINTER" "BOOL")
		("BOOL" "POINTER")
		("BOOL")
		("FLOAT" "OBJECT" "FLOAT")
		("FLOAT" "OBJECT")
		("INT" "BOOL")
		("INT" "OBJECT" "ARRAY")
		("INT" "OBJECT" "INT" "ARRAY")
		("INT" "OBJECT" "INT" "INT")
		("INT" "OBJECT" "INT" "STRING")
		("INT" "OBJECT" "INT")
		("INT" "OBJECT" "OBJECT")
		("INT" "OBJECT" "POINTER" "INT" "INT")
		("INT" "OBJECT" "POINTER" "INT")
		("INT" "OBJECT" "POINTER")
		("INT" "OBJECT" "STRING")
		("INT" "OBJECT")
		("INT" "POINTER" "INT")
		("INT" "POINTER" "STRING" "INT")
		("INT" "POINTER" "STRING" "STRING")
		("INT" "POINTER" "STRING")
		("INT" "POINTER")
		("INT" "STRING" "STRING" "INT" "ARRAY")
		("INT" "STRING")
		("INT")
		("LIST" "OBJECT")
		("LIST")
		("NONE" "BOOL")
		("NONE" "INT" "INT" "INT" "INT")
		("NONE" "INT" "INT")
		("NONE" "INT")
		("NONE" "OBJECT" "BOOL" "INT")
		("NONE" "OBJECT" "BOOL")
		("NONE" "OBJECT" "FLOAT" "FLOAT" "FLOAT" "BOOL")
		("NONE" "OBJECT" "FLOAT" "FLOAT" "FLOAT" "FLOAT")
		("NONE" "OBJECT" "FLOAT" "FLOAT" "FLOAT")
		("NONE" "OBJECT" "FLOAT" "FLOAT")
		("NONE" "OBJECT" "FLOAT")
		("NONE" "OBJECT" "INT" "BOOL")
		("NONE" "OBJECT" "INT" "FLOAT" "BOOL")
		("NONE" "OBJECT" "INT" "FLOAT")
		("NONE" "OBJECT" "INT" "INT" "ARRAY" "ARRAY" "ARRAY" "ARRAY" "ARRAY" "ARRAY")
		("NONE" "OBJECT" "INT" "INT" "ARRAY")
		("NONE" "OBJECT" "INT" "INT" "FLOAT" "FLOAT")
		("NONE" "OBJECT" "INT" "INT" "INT" "INT")
		("NONE" "OBJECT" "INT" "INT" "INT")
		("NONE" "OBJECT" "INT" "INT" "POINTER" "POINTER")
		("NONE" "OBJECT" "INT" "INT" "POINTER")
		("NONE" "OBJECT" "INT" "INT" "STRING" "INT" "POINTER" "POINTER")
		("NONE" "OBJECT" "INT" "INT" "STRING")
		("NONE" "OBJECT" "INT" "INT")
		("NONE" "OBJECT" "INT" "OBJECT")
		("NONE" "OBJECT" "INT" "POINTER")
		("NONE" "OBJECT" "INT" "STRING")
		("NONE" "OBJECT" "INT")
		("NONE" "OBJECT" "LIST" "INT")
		("NONE" "OBJECT" "LIST")
		("NONE" "OBJECT" "OBJECT" "BOOL" "BOOL" "INT" "INT")
		("NONE" "OBJECT" "OBJECT" "BOOL" "BOOL" "INT")
		("NONE" "OBJECT" "OBJECT" "BOOL" "BOOL")
		("NONE" "OBJECT" "OBJECT" "FLOAT" "INT")
		("NONE" "OBJECT" "OBJECT" "INT" "INT" "INT" "INT" "INT" "INT" "INT" "INT")
		("NONE" "OBJECT" "OBJECT" "INT" "INT" "INT" "INT")
		("NONE" "OBJECT" "OBJECT" "INT" "INT" "INT")
		("NONE" "OBJECT" "OBJECT" "INT" "INT")
		("NONE" "OBJECT" "OBJECT" "INT")
		("NONE" "OBJECT" "OBJECT" "OBJECT" "INT")
		("NONE" "OBJECT" "OBJECT" "OBJECT" "OBJECT" "INT")
		("NONE" "OBJECT" "OBJECT" "OBJECT" "OBJECT")
		("NONE" "OBJECT" "OBJECT" "OBJECT" "POINTER" "POINTER" "INT" "INT")
		("NONE" "OBJECT" "OBJECT" "OBJECT")
		("NONE" "OBJECT" "OBJECT" "POINTER")
		("NONE" "OBJECT" "OBJECT" "STRING" "INT" "INT" "INT" "INT" "INT")
		("NONE" "OBJECT" "OBJECT" "STRING" "STRING" "INT")
		("NONE" "OBJECT" "OBJECT" "STRING" "STRING")
		("NONE" "OBJECT" "OBJECT" "STRING")
		("NONE" "OBJECT" "OBJECT")
		("NONE" "OBJECT" "POINTER" "BOOL")
		("NONE" "OBJECT" "POINTER" "INT" "FLOAT" "FLOAT")
		("NONE" "OBJECT" "POINTER" "INT" "INT" "INT")
		("NONE" "OBJECT" "POINTER" "INT" "INT")
		("NONE" "OBJECT" "POINTER" "INT" "POINTER" "POINTER")
		("NONE" "OBJECT" "POINTER" "INT" "POINTER")
		("NONE" "OBJECT" "POINTER" "INT" "STRING" "INT" "POINTER" "POINTER")
		("NONE" "OBJECT" "POINTER" "INT" "STRING")
		("NONE" "OBJECT" "POINTER" "INT")
		("NONE" "OBJECT" "POINTER" "POINTER" "INT" "INT" "INT" "INT" "INT" "INT")
		("NONE" "OBJECT" "POINTER" "POINTER" "POINTER" "STRING" "INT")
		("NONE" "OBJECT" "POINTER" "POINTER" "POINTER")
		("NONE" "OBJECT" "POINTER" "POINTER")
		("NONE" "OBJECT" "POINTER" "STRING" "INT" "POINTER" "POINTER" "POINTER" "POINTER" "BOOL" "BOOL")
		("NONE" "OBJECT" "POINTER")
		("NONE" "OBJECT" "STRING" "BOOL")
		("NONE" "OBJECT" "STRING" "INT" "INT" "INT")
		("NONE" "OBJECT" "STRING" "POINTER" "INT" "INT" "INT")
		("NONE" "OBJECT" "STRING" "POINTER" "INT" "INT")
		("NONE" "OBJECT" "STRING" "STRING")
		("NONE" "OBJECT" "STRING")
		("NONE" "OBJECT")
		("NONE" "POINTER" "INT" "INT")
		("NONE" "POINTER" "INT")
		("NONE" "POINTER" "POINTER" "BOOL" "INT" "INT" "INT" "INT" "INT" "INT")
		("NONE" "POINTER" "POINTER" "BOOL" "INT" "INT" "INT" "INT")
		("NONE" "POINTER" "POINTER" "INT" "INT" "INT" "INT")
		("NONE" "POINTER" "POINTER" "INT" "INT")
		("NONE" "POINTER" "POINTER" "POINTER" "INT" "INT" "STRING" "INT")
		("NONE" "POINTER" "POINTER" "POINTER" "INT" "INT" "STRING")
		("NONE" "POINTER" "POINTER" "POINTER" "POINTER")
		("NONE" "POINTER" "POINTER")
		("NONE" "POINTER" "STRING" "STRING")
		("NONE" "POINTER" "STRING")
		("NONE" "POINTER")
		("NONE")
		("OBJECT" "BOOL" "BOOL" "INT")
		("OBJECT" "BOOL" "INT")
		("OBJECT" "FLOAT" "FLOAT" "FLOAT" "FLOAT" "FLOAT" "FLOAT")
		("OBJECT" "FLOAT" "FLOAT" "FLOAT" "FLOAT" "FLOAT")
		("OBJECT" "FLOAT" "FLOAT" "FLOAT" "FLOAT")
		("OBJECT" "INT" "ARRAY")
		("OBJECT" "INT" "BOOL" "BOOL")
		("OBJECT" "INT" "INT" "ARRAY")
		("OBJECT" "INT" "INT" "BOOL")
		("OBJECT" "INT" "INT" "STRING")
		("OBJECT" "INT" "INT")
		("OBJECT" "INT")
		("OBJECT" "OBJECT" "FLOAT" "INT")
		("OBJECT" "OBJECT" "INT")
		("OBJECT" "OBJECT" "OBJECT")
		("OBJECT" "OBJECT" "STRING" "INT" "INT" "INT" "INT" "INT")
		("OBJECT" "OBJECT" "STRING" "INT" "INT" "INT" "INT")
		("OBJECT" "OBJECT" "STRING" "INT" "INT")
		("OBJECT" "OBJECT" "STRING")
		("OBJECT" "OBJECT")
		("OBJECT" "POINTER" "POINTER")
		("OBJECT" "POINTER" "STRING")
		("OBJECT" "POINTER")
		("OBJECT" "STRING" "FLOAT" "FLOAT" "FLOAT" "BOOL")
		("OBJECT" "STRING" "INT" "STRING" "STRING")
		("OBJECT" "STRING" "OBJECT")
		("OBJECT" "STRING" "STRING" "STRING" "ARRAY" "STRING" "STRING")
		("OBJECT" "STRING" "STRING")
		("OBJECT" "STRING")
		("OBJECT")
		("POINTER" "INT" "INT")
		("POINTER" "INT")
		("POINTER" "OBJECT" "INT" "INT")
		("POINTER" "OBJECT" "INT")
		("POINTER" "OBJECT" "POINTER" "INT")
		("POINTER" "OBJECT" "POINTER" "POINTER" "ARRAY" "INT" "POINTER" "POINTER" "POINTER" "POINTER" "BOOL" "BOOL")
		("POINTER" "OBJECT" "POINTER")
		("POINTER" "OBJECT")
		("POINTER" "POINTER")
		("POINTER" "STRING" "INT")
		("POINTER")
		("STRING" "INT" "INT" "INT")
		("STRING" "INT")
		("STRING" "OBJECT" "BOOL")
		("STRING" "OBJECT" "FLOAT")
		("STRING" "OBJECT" "INT" "INT")
		("STRING" "OBJECT" "INT")
		("STRING" "OBJECT")
		("STRING" "POINTER" "STRING")
		("STRING" "POINTER")
		("STRING")
		)
	      )
	)
    (mapc (lambda (x) (apply 'define-marshaller x)) todo)

    (insert "\n
#include \"hash.h\"
static int
our_string_eq (const void *st1, const void *st2)
{
  if (!st1)
    return st2 ? 0 : 1;
  else if (!st2)
    return 0;
  else
    return !strcmp ( (const char *) st1, (const char *) st2);
}

static unsigned long
our_string_hash (const void *xv)
{
  unsigned int h = 0;
  unsigned const char *x = (unsigned const char *) xv;

  if (!x) return 0;

  while (*x)
    {
      unsigned int g;
      h = (h << 4) + *x++;
      if ((g = h & 0xf0000000) != 0)
	h = (h ^ (g >> 24)) ^ g;
    }

  return h;
}

static struct hash_table *marshaller_hashtable;

static void initialize_marshaller_storage (void)
{
	if (!marshaller_hashtable)
	{
		marshaller_hashtable = make_general_hash_table (100, our_string_hash, our_string_eq);
")
    
    (mapc (lambda (x)
	    (let ((name (get-marshaller-name (car x) (cdr x))))
	      (insert (format "\t\tputhash (\"%s\", (void *) %s, marshaller_hashtable);\n" name name))))
	  todo)
    (insert "\t};\n"
	    "}\n"
	    "
static void *find_marshaller (const char *func_name)
{
	void *fn = NULL;
	initialize_marshaller_storage ();

	if (gethash (func_name, marshaller_hashtable, (const void **)&fn))
	{
		return (fn);
	}

	return (NULL);
}
"))

  (save-buffer)
  (kill-buffer "emacs-marshals.c"))
