/* Various function declarations for XEmacs.
   Used to be part of lisp.h
   Copyright (C) 1985-1987, 1992-1994 Free Software Foundation, Inc.

This file is part of XEmacs.

XEmacs is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 2, or (at your option) any
later version.

XEmacs is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with XEmacs; see the file COPYING.  If not, write to
the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  */

/* Synched up with: Mule 2.0.  Divergent from FSF. */

#ifndef _EMACSFNS_H_
#define _EMACSFNS_H_


/* Defined in abbrev.c */
extern Lisp_Object Vfundamental_mode_abbrev_table;
Lisp_Object Fexpand_abbrev (void);


/* Defined in alloc.c */
void release_breathing_space (void);
Lisp_Object Fcons (Lisp_Object car, Lisp_Object cdr);
Lisp_Object noseeum_cons (Lisp_Object car, Lisp_Object cdr);
Lisp_Object Flist (int nargs, Lisp_Object *args);
Lisp_Object Fmake_list (Lisp_Object length, Lisp_Object init);
Lisp_Object Fmake_vector (Lisp_Object length, Lisp_Object init);
Lisp_Object make_vector (EMACS_INT length, Lisp_Object init);
Lisp_Object Fvector (int nargs, Lisp_Object *args);
Lisp_Object vector1 (Lisp_Object);
Lisp_Object vector2 (Lisp_Object, Lisp_Object);
Lisp_Object vector3 (Lisp_Object, Lisp_Object, Lisp_Object);
Lisp_Object vector4 (Lisp_Object, Lisp_Object, Lisp_Object,
		     Lisp_Object);
Lisp_Object vector5 (Lisp_Object, Lisp_Object, Lisp_Object,
		     Lisp_Object, Lisp_Object);
Lisp_Object vector6 (Lisp_Object, Lisp_Object, Lisp_Object,
		     Lisp_Object, Lisp_Object, Lisp_Object);
Lisp_Object vector7 (Lisp_Object, Lisp_Object, Lisp_Object,
		     Lisp_Object, Lisp_Object, Lisp_Object,
		     Lisp_Object);
Lisp_Object vector8 (Lisp_Object, Lisp_Object, Lisp_Object,
		     Lisp_Object, Lisp_Object, Lisp_Object,
		     Lisp_Object, Lisp_Object);
Lisp_Object make_bit_vector (EMACS_INT length, Lisp_Object init);
Lisp_Object make_bit_vector_from_byte_vector (unsigned char *bytevec,
					      EMACS_INT length);
Lisp_Object Fmake_bit_vector (Lisp_Object length, Lisp_Object init);
Lisp_Object Fmake_symbol (Lisp_Object name);
Lisp_Object Fmake_marker (void);
Lisp_Object noseeum_make_marker (void);
Lisp_Object Fmake_string (Lisp_Object length, Lisp_Object init);
void garbage_collect_1 (void);
Lisp_Object Fgarbage_collect (void);
Lisp_Object list1 (Lisp_Object);
Lisp_Object list2 (Lisp_Object, Lisp_Object);
Lisp_Object list3 (Lisp_Object, Lisp_Object, Lisp_Object);
Lisp_Object list4 (Lisp_Object, Lisp_Object, Lisp_Object, Lisp_Object);
Lisp_Object list5 (Lisp_Object, Lisp_Object, Lisp_Object, Lisp_Object,
		   Lisp_Object);
Lisp_Object list6 (Lisp_Object, Lisp_Object, Lisp_Object, Lisp_Object,
		   Lisp_Object, Lisp_Object);

void malloc_warning (CONST char *);
DECLARE_DOESNT_RETURN (memory_full (void));
void *xmalloc (int size);
void *xmalloc_and_zero (int size);
void *xrealloc (void *, int size);
#ifdef ERROR_CHECK_MALLOC
void xfree_1 (void *);
#else
void xfree (void *);
#endif
char *xstrdup (CONST char *);
void disksave_object_finalization (void);
extern int purify_flag;
extern int gc_currently_forbidden;
Lisp_Object restore_gc_inhibit (Lisp_Object);
extern EMACS_INT gc_generation_number[1];
int purified (Lisp_Object);

Lisp_Object build_string (CONST char *);
Lisp_Object build_ext_string (CONST char *str,
			      enum external_data_format fmt);
Lisp_Object build_translated_string (CONST char *);
Lisp_Object make_string (CONST Bufbyte *, Bytecount);
Lisp_Object make_ext_string (CONST Extbyte *contents, EMACS_INT length,
			     enum external_data_format fmt);
Lisp_Object make_uninit_string (Bytecount length);

Lisp_Object make_float (double float_value);

Lisp_Object Fmake_byte_code (int nargs, Lisp_Object *args);

Lisp_Object Fpurecopy (Lisp_Object);
int purespace_usage (void);
void report_pure_usage (int report_impurities,
			int die_if_pure_storage_exceeded);
Lisp_Object make_pure_string (CONST Bufbyte *, Bytecount len,
			      Lisp_Object plist, int nocopy);
Lisp_Object make_pure_pname (CONST Bufbyte *, Bytecount len,
			     int nocopy);
Lisp_Object pure_cons (Lisp_Object, Lisp_Object);
Lisp_Object pure_list (int nargs, Lisp_Object *args);
Lisp_Object make_pure_vector (EMACS_INT len, Lisp_Object init);

void free_cons (struct Lisp_Cons *ptr);
void free_list (Lisp_Object list);
void free_alist (Lisp_Object alist);
void mark_conses_in_list (Lisp_Object obj);

void free_marker (struct Lisp_Marker *ptr);

#ifdef LISP_FLOAT_TYPE
Lisp_Object make_pure_float (double float_value);
/* void free_float (struct Lisp_Float *); */
#endif

int object_dead_p (Lisp_Object);

#ifdef MEMORY_USAGE_STATS
int malloced_storage_size (void *ptr, int claimed_size,
			   struct overhead_stats *stats);
int fixed_type_block_overhead (int size);
#endif


/* Defined in buffer.c */
Lisp_Object make_buffer (struct buffer *buf);
Lisp_Object Fset_buffer_left_margin_width (Lisp_Object width,
					   Lisp_Object buffer);
Lisp_Object Fset_buffer_right_margin_width (Lisp_Object width,
					    Lisp_Object buffer);
Lisp_Object Fbuffer_left_margin_width (Lisp_Object buffer);
Lisp_Object Fbuffer_right_margin_width (Lisp_Object buffer);
Lisp_Object Ferase_buffer (Lisp_Object buffer);
Lisp_Object Fbuffer_disable_undo (Lisp_Object buffer);
Lisp_Object Fkill_buffer (Lisp_Object buffer);
Lisp_Object Fbuffer_name (Lisp_Object buffer);
Lisp_Object Fget_buffer (Lisp_Object name);
Lisp_Object Fget_buffer_create (Lisp_Object name);
Lisp_Object Fget_file_buffer (Lisp_Object fn);
Lisp_Object get_truename_buffer (REGISTER Lisp_Object filename);
Lisp_Object Fset_buffer (Lisp_Object buffer);
Lisp_Object Fbarf_if_buffer_read_only (Lisp_Object buffer,
				       Lisp_Object s, Lisp_Object e);
Lisp_Object Fcurrent_buffer (void);
void switch_to_buffer (Lisp_Object buf, Lisp_Object norecord);
Lisp_Object Frecord_buffer (Lisp_Object);
Lisp_Object Fother_buffer (Lisp_Object buffer, Lisp_Object frame,
			   Lisp_Object visible_ok);
Lisp_Object Fbuffer_list (Lisp_Object frame);
Lisp_Object Fset_buffer_modified_p (Lisp_Object flag,
				    Lisp_Object buffer);
extern Lisp_Object QSscratch;   /* "*scratch*" */
extern Lisp_Object Qbuffer_file_name, Qbuffer_undo_list;
extern Lisp_Object Qdefault_directory;
extern int find_file_compare_truenames;
extern int find_file_use_truenames;
Lisp_Object Fbuffer_modified_p (Lisp_Object buffer);
Lisp_Object Fgenerate_new_buffer_name (Lisp_Object name,
				       Lisp_Object ignore);
Lisp_Object Frename_buffer (Lisp_Object name, Lisp_Object unique);

/* Functions to call before and after each text change. */
extern Lisp_Object Vbefore_change_functions;
extern Lisp_Object Qbefore_change_functions;
extern Lisp_Object Vafter_change_functions;
extern Lisp_Object Qafter_change_functions;

/* #### Obsolete, for compatibility */
extern Lisp_Object Vbefore_change_function;
extern Lisp_Object Qbefore_change_function;
extern Lisp_Object Vafter_change_function;
extern Lisp_Object Qafter_change_function;

extern Lisp_Object Vfirst_change_hook;
extern Lisp_Object Qfirst_change_hook;
extern Lisp_Object Vinhibit_read_only;

extern Lisp_Object Qpermanent_local, Qprotected_field;


/* Defined in bytecode.c */
extern Lisp_Object Qbyte_code;
Lisp_Object Fbyte_code (Lisp_Object bytestr, 
			Lisp_Object constants_vector, 
			Lisp_Object maxdepth);


/* Defined in callint.c */
extern Lisp_Object Vcommand_history;
extern Lisp_Object Qcall_interactively;
Lisp_Object Fcall_interactively (Lisp_Object fn, Lisp_Object record,
				 Lisp_Object keys);
Lisp_Object Fprefix_numeric_value (Lisp_Object prefix);
extern Lisp_Object Qread_from_minibuffer;
extern Lisp_Object Qenable_recursive_minibuffers;
extern Lisp_Object Qcompleting_read;
extern Lisp_Object Qread_file_name;
extern Lisp_Object Qread_directory_name;
extern Lisp_Object Qread_buffer;
extern Lisp_Object Qmouse_leave_buffer_hook;


/* Defined in callproc.c */
extern Lisp_Object Vexec_path, Vexec_directory, Vdata_directory,
  Vdata_directory_list, Vdoc_directory, Vsite_directory;


/* Defined in casefiddle.c */
Lisp_Object Fupcase (Lisp_Object obj, Lisp_Object buffer);
Lisp_Object Fdowncase (Lisp_Object obj, Lisp_Object buffer);
Lisp_Object Fcapitalize (Lisp_Object obj, Lisp_Object buffer);
Lisp_Object Fupcase_initials (Lisp_Object obj, Lisp_Object buffer);
Lisp_Object Fupcase_region (Lisp_Object b, Lisp_Object e,
			    Lisp_Object buffer);
Lisp_Object Fdowncase_region (Lisp_Object b, Lisp_Object e,
			      Lisp_Object buffer);
Lisp_Object Fcapitalize_region (Lisp_Object b, Lisp_Object e,
				Lisp_Object buffer);
Lisp_Object Fupcase_initials_region (Lisp_Object b, Lisp_Object e,
				     Lisp_Object buffer);
Lisp_Object Fupcase_word (Lisp_Object arg, Lisp_Object buffer);
Lisp_Object Fdowncase_word (Lisp_Object arg, Lisp_Object buffer);
Lisp_Object Fcapitalize_word (Lisp_Object arg, Lisp_Object buffer);

extern Lisp_Object Vascii_downcase_table, Vascii_upcase_table;
extern Lisp_Object Vascii_canon_table, Vascii_eqv_table;
extern Lisp_Object Vmirror_ascii_downcase_table, Vmirror_ascii_upcase_table;
extern Lisp_Object Vmirror_ascii_canon_table, Vmirror_ascii_eqv_table;


/* Defined in chartab.c */
Lisp_Object Fmake_char_table (Lisp_Object type);
Lisp_Object Fput_char_table (Lisp_Object from, Lisp_Object to,
			     Lisp_Object table);
Lisp_Object Fcopy_char_table (Lisp_Object table);


/* Defined in cmdloop.c */
Lisp_Object Frecursive_edit (void);
extern Lisp_Object Qdisabled, Qtop_level;
extern Lisp_Object Vdisabled_command_hook;
Lisp_Object Fcommand_loop_1 (void);
extern Lisp_Object Qreally_early_error_handler;


/* Defined in cmds.c */
Lisp_Object Fforward_char (Lisp_Object n, Lisp_Object buffer);
Lisp_Object Fforward_line (Lisp_Object n, Lisp_Object buffer);
Lisp_Object Fend_of_line (Lisp_Object n, Lisp_Object buffer);
Lisp_Object Fbeginning_of_line (Lisp_Object n, Lisp_Object buffer);
extern Lisp_Object Qself_insert_command;


/* Defined in console.c */
Lisp_Object Fset_input_mode (Lisp_Object interrupt,
			     Lisp_Object flow, 
			     Lisp_Object meta,
			     Lisp_Object quit,
			     Lisp_Object console);
Lisp_Object Fselect_console (Lisp_Object);
Lisp_Object Fselected_console (void);
Lisp_Object Fdelete_console (Lisp_Object console, Lisp_Object force);
Lisp_Object Fconsole_type (Lisp_Object);
Lisp_Object Fconsole_name (Lisp_Object);
Lisp_Object Fconsole_enable_input (Lisp_Object console);
Lisp_Object Fconsole_disable_input (Lisp_Object console);
void stuff_buffered_input (Lisp_Object stuffstring);
extern Lisp_Object Qx, Qns, Qtty, Qstream;
extern Lisp_Object Qcreate_console_hook, Qdelete_console_hook;


/* Defined in data.c */
extern Lisp_Object Qnil, Qt, Qquote, Qlambda, Qunbound;
extern Lisp_Object Qerror_conditions, Qerror_message, Qtop_level, Qsignal;
extern Lisp_Object Qerror, Qquit, Qwrong_type_argument, Qargs_out_of_range;
extern Lisp_Object Qvoid_function, Qvoid_variable;
extern Lisp_Object Qcyclic_function_indirection, Qcyclic_variable_indirection;
extern Lisp_Object Qsetting_constant, Qinvalid_read_syntax;
extern Lisp_Object Qmalformed_list, Qmalformed_property_list;
extern Lisp_Object Qcircular_list, Qcircular_property_list;
extern Lisp_Object Qinvalid_function, Qwrong_number_of_arguments, Qno_catch;
extern Lisp_Object Qio_error;
extern Lisp_Object Qend_of_file, Qarith_error;
extern Lisp_Object Qrange_error, Qdomain_error, Qsingularity_error;
extern Lisp_Object Qsingularity_error, Qoverflow_error, Qunderflow_error;
extern Lisp_Object Qbeginning_of_buffer, Qend_of_buffer, Qbuffer_read_only;

extern Lisp_Object Qintegerp, Qnatnump, Qsymbolp, Qlistp, Qconsp;
extern Lisp_Object Qstringp, Qarrayp, Qsequencep, Qbufferp, Qsubrp;
extern Lisp_Object Qcharacterp, Qchar_or_string_p, Qmarkerp, Qvectorp;
extern Lisp_Object Qbitp, Qbit_vectorp;
extern Lisp_Object Qinteger_or_char_p;
extern Lisp_Object Qinteger_or_marker_p, Qboundp, Qfboundp;
extern Lisp_Object Qcons;
extern Lisp_Object Qcdr, Qignore;

extern Lisp_Object Qnumberp, Qnumber_or_marker_p;
extern Lisp_Object Qnumber_char_or_marker_p;

extern Lisp_Object Qvalues;
extern Lisp_Object Qprogn;
extern Lisp_Object Qstring_or_buffer_p;

extern Lisp_Object Qfloatp;
Lisp_Object Ftruncate (Lisp_Object n);

Lisp_Object Flistp (Lisp_Object x);

Lisp_Object Fcar (Lisp_Object cons), Fcar_safe (Lisp_Object cons);
Lisp_Object Fcdr (Lisp_Object cons), Fcdr_safe (Lisp_Object cons);
Lisp_Object Fsetcar (Lisp_Object cons, Lisp_Object val);
Lisp_Object Fsetcdr (Lisp_Object cons, Lisp_Object val);
Lisp_Object Faref (Lisp_Object array, Lisp_Object idx);
Lisp_Object Faset (Lisp_Object array, Lisp_Object idx, Lisp_Object x);
Lisp_Object Farray_length (Lisp_Object array);
Lisp_Object Felt (Lisp_Object seq, Lisp_Object idx);

Lisp_Object Fzerop (Lisp_Object);
Lisp_Object Fnumber_to_string (Lisp_Object num);
Lisp_Object Fstring_to_number (Lisp_Object str, Lisp_Object base);
Lisp_Object Fsubr_min_args (Lisp_Object subr);
Lisp_Object Fsubr_max_args (Lisp_Object subr);

#ifndef make_int
Lisp_Object make_int (EMACS_INT);
#endif
Lisp_Object make_char (Emchar num);
DECLARE_DOESNT_RETURN (pure_write_error (void));
DECLARE_DOESNT_RETURN (args_out_of_range (Lisp_Object, Lisp_Object));
DECLARE_DOESNT_RETURN (args_out_of_range_3 (Lisp_Object, Lisp_Object,
					    Lisp_Object));
Lisp_Object wrong_type_argument (Lisp_Object pred, Lisp_Object value);
DECLARE_DOESNT_RETURN (dead_wrong_type_argument (Lisp_Object predicate,
						 Lisp_Object value));
void check_int_range (int val, int min, int max);

Lisp_Object word_to_lisp (unsigned int);
unsigned int lisp_to_word (Lisp_Object);

Lisp_Object Fcompiled_function_instructions (Lisp_Object function);
Lisp_Object Fcompiled_function_constants (Lisp_Object function);
Lisp_Object Fcompiled_function_stack_depth (Lisp_Object function);
Lisp_Object Fcompiled_function_arglist (Lisp_Object function);
Lisp_Object Fcompiled_function_interactive (Lisp_Object function);
Lisp_Object Fcompiled_function_domain (Lisp_Object function);
Lisp_Object Fcompiled_function_annotation (Lisp_Object function);

Lisp_Object Fquo (int nargs, Lisp_Object *args);
Lisp_Object Fsub1 (Lisp_Object num);
Lisp_Object Fadd1 (Lisp_Object num);

Lisp_Object Fgtr (Lisp_Object num1, Lisp_Object num2);
Lisp_Object Flss (Lisp_Object num1, Lisp_Object num2);
Lisp_Object Fleq (Lisp_Object num1, Lisp_Object num2);
Lisp_Object Fgeq (Lisp_Object num1, Lisp_Object num2);

Lisp_Object Fminus (int nargs, Lisp_Object *args);
Lisp_Object Fplus  (int nargs, Lisp_Object *args);
Lisp_Object Fmin   (int nargs, Lisp_Object *args);
Lisp_Object Fmax   (int nargs, Lisp_Object *args);
Lisp_Object Ftimes (int nargs, Lisp_Object *args);
Lisp_Object Frem (Lisp_Object num1, Lisp_Object num2);


/* Defined in device.c */
Lisp_Object Fmake_device (Lisp_Object type, Lisp_Object connection,
			  Lisp_Object params);
Lisp_Object Fselect_device (Lisp_Object device);
Lisp_Object Fset_device_selected_frame (Lisp_Object device, Lisp_Object frame);
Lisp_Object Fselected_device (Lisp_Object);
Lisp_Object Fdelete_device (Lisp_Object device, Lisp_Object force);
Lisp_Object Fdevice_console (Lisp_Object);
Lisp_Object Fdevice_name (Lisp_Object);
extern Lisp_Object Qgrayscale, Qmono;
extern Lisp_Object Qcreate_device_hook, Qdelete_device_hook;

/* Defined in device-tty.c */
extern Lisp_Object Qinit_post_tty_win;

/* Defined in dialog.c */
Lisp_Object Fpopup_dialog_box (Lisp_Object dbox_desc);


/* Defined in dired.c */
Lisp_Object make_directory_hash_table (char *path);
Lisp_Object wasteful_word_to_lisp (unsigned int item);


/* Defined in doc.c */
extern Lisp_Object Vdoc_file_name;
Lisp_Object Fsubstitute_command_keys (Lisp_Object string);
Lisp_Object Fdocumentation (Lisp_Object fun, Lisp_Object raw);
Lisp_Object Fdocumentation_property (Lisp_Object sym, Lisp_Object prop,
				     Lisp_Object raw);
Lisp_Object unparesseuxify_doc_string (int fd, EMACS_INT position,
				       char *name_nonreloc,
				       Lisp_Object name_reloc);
Lisp_Object read_doc_string (Lisp_Object filepos);


/* Defined in doprnt.c */
Bytecount emacs_doprnt_c (Lisp_Object stream,
			  CONST Bufbyte *format_nonreloc,
			  Lisp_Object format_reloc,
			  Bytecount format_length,
			  ...);
Bytecount emacs_doprnt_va (Lisp_Object stream,
			   CONST Bufbyte *format_nonreloc,
			   Lisp_Object format_reloc,
			   Bytecount format_length,
			   va_list vargs);
Bytecount emacs_doprnt_lisp (Lisp_Object stream,
			     CONST Bufbyte *format_nonreloc,
			     Lisp_Object format_reloc,
			     Bytecount format_length,
			     int nargs, CONST Lisp_Object *largs);
Bytecount emacs_doprnt_lisp_2 (Lisp_Object stream,
			       CONST Bufbyte *format_nonreloc,
			       Lisp_Object format_reloc,
			       Bytecount format_length,
			       int nargs, ...);
Lisp_Object emacs_doprnt_string_c (CONST Bufbyte *format_nonreloc,
				   Lisp_Object format_reloc,
				   Bytecount format_length,
				   ...);
Lisp_Object emacs_doprnt_string_va (CONST Bufbyte *format_nonreloc,
				    Lisp_Object format_reloc,
				    Bytecount format_length,
				    va_list vargs);
Lisp_Object emacs_doprnt_string_lisp (CONST Bufbyte *format_nonreloc,
				      Lisp_Object format_reloc,
				      Bytecount format_length,
				      int nargs,
				      CONST Lisp_Object *largs);
Lisp_Object emacs_doprnt_string_lisp_2 (CONST Bufbyte *format_nonreloc,
					Lisp_Object format_reloc,
					Bytecount format_length,
					int nargs, ...);


/* Defined in editfns.c */
Bufpos bufpos_clip_to_bounds (Bufpos lower, Bufpos num, Bufpos upper);
Bytind bytind_clip_to_bounds (Bytind lower, Bytind num, Bytind upper);
Lisp_Object time_to_lisp (time_t the_time);
int lisp_to_time (Lisp_Object specified_time, time_t *result);
Lisp_Object Fwiden (Lisp_Object buffer);
Lisp_Object Fnarrow_to_region (Lisp_Object b, Lisp_Object e,
			       Lisp_Object buffer);
extern Lisp_Object Vprefix_arg, Vcurrent_prefix_arg;
extern Lisp_Object Qcurrent_prefix_arg;
Lisp_Object Fgoto_char (Lisp_Object pos, Lisp_Object buffer);
Lisp_Object Fpoint_min_marker (Lisp_Object buffer);
Lisp_Object Fpoint_max_marker (Lisp_Object buffer);
Lisp_Object Fpoint_min (Lisp_Object buffer);
Lisp_Object Fpoint_max (Lisp_Object buffer);
Lisp_Object Fpoint (Lisp_Object buffer);
Lisp_Object Fpoint_marker (Lisp_Object dont_copy_p, Lisp_Object buffer);
Lisp_Object Fmark_marker (Lisp_Object inactive_p, Lisp_Object buffer);
Lisp_Object Ffollowing_char (Lisp_Object buffer);
Lisp_Object Fpreceding_char (Lisp_Object buffer);
Lisp_Object Fchar_after (Lisp_Object pos, Lisp_Object buffer);
Lisp_Object Finsert (int nargs, Lisp_Object *args);
Lisp_Object Finsert_string (Lisp_Object string, Lisp_Object buffer);
Lisp_Object Finsert_char (Lisp_Object ch, Lisp_Object count,
			  Lisp_Object ignored, Lisp_Object buffer);
void buffer_insert1 (struct buffer *buf, Lisp_Object arg);
Lisp_Object Finsert_before_markers (int nargs, Lisp_Object *args);
Lisp_Object Finsert_buffer_substring (Lisp_Object buffer, 
				      Lisp_Object b, Lisp_Object e);
Lisp_Object Fdelete_region (Lisp_Object b, Lisp_Object e,
			    Lisp_Object buffer);
Lisp_Object Feolp (Lisp_Object buffer);
Lisp_Object Feobp (Lisp_Object buffer);
Lisp_Object Fbolp (Lisp_Object buffer);
Lisp_Object Fbobp (Lisp_Object buffer);
Lisp_Object Fformat (int nargs, Lisp_Object *args);
Lisp_Object Fbuffer_substring (Lisp_Object start, Lisp_Object end,
			       Lisp_Object buffer);
Lisp_Object make_string_from_buffer (struct buffer *buf,
				     int pos, int length);
Lisp_Object save_excursion_save (void), save_restriction_save (void);
Lisp_Object save_excursion_restore (Lisp_Object info);
Lisp_Object save_restriction_restore (Lisp_Object info);
Lisp_Object Fchar_to_string (Lisp_Object ch);
Lisp_Object Fcurrent_time_seconds (Lisp_Object cons);
Lisp_Object Fgetenv (Lisp_Object var, Lisp_Object interactivep);
extern Lisp_Object Qpoint, Qmark, Qregion_beginning, Qregion_end;
extern Lisp_Object Qformat;


/* Defined in emacsfns.c */
Lisp_Object save_current_buffer_restore (Lisp_Object buffer);



/* Defined in elhash.c */
Lisp_Object Fhashtablep (Lisp_Object obj);
Lisp_Object Fmake_hashtable (Lisp_Object size, Lisp_Object test_fun);
Lisp_Object Fcopy_hashtable (Lisp_Object old_table);
Lisp_Object Fgethash (Lisp_Object key, Lisp_Object table,
		      Lisp_Object def);
Lisp_Object Fremhash (Lisp_Object key, Lisp_Object table);
Lisp_Object Fputhash (Lisp_Object key, Lisp_Object val,
		      Lisp_Object table);
Lisp_Object Fclrhash (Lisp_Object table);
Lisp_Object Fhashtable_fullness (Lisp_Object table);
Lisp_Object Fmaphash (Lisp_Object function, Lisp_Object table);

extern Lisp_Object Vcharacter_set_property;


/* Defined in emacs.c */
DECLARE_DOESNT_RETURN_GCC__ATTRIBUTE__SYNTAX_SUCKS (fatal (CONST char *fmt,
							   ...),
						    1, 2);
int stderr_out (CONST char *fmt, ...) PRINTF_ARGS (1, 2);
int stdout_out (CONST char *fmt, ...) PRINTF_ARGS (1, 2);
SIGTYPE fatal_error_signal (int sig);
Lisp_Object make_arg_list (int argc, char **argv);
void make_argc_argv (Lisp_Object argv_list, int *argc, char ***argv);
void free_argc_argv (char **argv);
Lisp_Object decode_env_path (CONST char *evarname, CONST char *default_);
Lisp_Object decode_path (CONST char *path);
/* Nonzero means don't do interactive redisplay and don't change tty modes */
extern int noninteractive;
Lisp_Object Fkill_emacs (Lisp_Object arg);
extern int preparing_for_armageddon;

extern Lisp_Object Vcommand_line_args;
extern Lisp_Object Vinvocation_name;
extern Lisp_Object Vinvocation_directory;

extern int emacs_priority;
extern int running_asynch_code;
extern int suppress_early_backtrace;

extern Lisp_Object Qsave_buffers_kill_emacs;
extern Lisp_Object Qkill_emacs_hook;
extern Lisp_Object Frunning_temacs_p (void);

/* Defined in eval.c */
extern Lisp_Object Qautoload, Qexit, Qinteractive, Qcommandp, Qdefun, Qmacro;
extern Lisp_Object Vinhibit_quit, Vquit_flag, Qinhibit_quit, Qrun_hooks;
extern Lisp_Object Vautoload_queue;
Lisp_Object Fuser_variable_p (Lisp_Object);
Lisp_Object Finteractive_p (void);
Lisp_Object Fsignal (Lisp_Object signame, Lisp_Object data);
DECLARE_DOESNT_RETURN (signal_error (Lisp_Object sig, Lisp_Object data));
void maybe_signal_error (Lisp_Object sig, Lisp_Object data, Lisp_Object class,
			 Error_behavior errb);
Lisp_Object maybe_signal_continuable_error (Lisp_Object sig, Lisp_Object data,
					    Lisp_Object class,
					    Error_behavior errb);
DECLARE_DOESNT_RETURN_GCC__ATTRIBUTE__SYNTAX_SUCKS (error (CONST char *fmt,
							   ...),
						    1, 2);
void maybe_error (Lisp_Object class, Error_behavior errb,
		  CONST char *fmt, ...) PRINTF_ARGS (3, 4);
Lisp_Object continuable_error (CONST char *fmt, ...) PRINTF_ARGS (1, 2);
Lisp_Object maybe_continuable_error (Lisp_Object class, Error_behavior errb,
				     CONST char *fmt, ...) PRINTF_ARGS (3, 4);
DECLARE_DOESNT_RETURN (signal_simple_error (CONST char *, Lisp_Object));
void maybe_signal_simple_error (CONST char *, Lisp_Object,
				Lisp_Object class, Error_behavior errb);
Lisp_Object signal_simple_continuable_error (CONST char *,
					     Lisp_Object);
Lisp_Object maybe_signal_simple_continuable_error (CONST char *reason,
						   Lisp_Object frob,
						   Lisp_Object class,
						   Error_behavior errb);
DECLARE_DOESNT_RETURN_GCC__ATTRIBUTE__SYNTAX_SUCKS (error_with_frob
						    (Lisp_Object frob,
						     CONST char *fmt, ...),
						    2, 3);
void maybe_error_with_frob (Lisp_Object frob, Lisp_Object class,
			    Error_behavior errb,
			    CONST char *fmt, ...) PRINTF_ARGS (4, 5);
Lisp_Object continuable_error_with_frob (Lisp_Object frob, CONST char *fmt,
					 ...) PRINTF_ARGS (2, 3);
Lisp_Object maybe_continuable_error_with_frob (Lisp_Object frob,
					       Lisp_Object class,
					       Error_behavior errb,
					       CONST char *fmt,
					       ...) PRINTF_ARGS (4, 5);
DECLARE_DOESNT_RETURN (signal_simple_error_2 (CONST char *,
					      Lisp_Object, Lisp_Object));
void maybe_signal_simple_error_2 (CONST char *reason, Lisp_Object frob0,
				  Lisp_Object frob1, Lisp_Object class,
				  Error_behavior errb);
Lisp_Object signal_simple_continuable_error_2 (CONST char *,
					       Lisp_Object,
					       Lisp_Object);
Lisp_Object maybe_signal_simple_continuable_error_2 (CONST char *reason,
						     Lisp_Object frob0,
						     Lisp_Object frob1,
						     Lisp_Object class,
						     Error_behavior errb);
Lisp_Object Fprogn (Lisp_Object args);
Lisp_Object Fcommandp (Lisp_Object obj);
Lisp_Object Feval (Lisp_Object form);
Lisp_Object Fapply (int nargs, Lisp_Object *args);
Lisp_Object funcall_recording_as (Lisp_Object recorded_as, int nargs,
				  Lisp_Object *args);
Lisp_Object Ffuncall (int nargs, Lisp_Object *args);
Lisp_Object Fbacktrace (Lisp_Object stream, Lisp_Object detailed);
Lisp_Object run_hook_with_args_in_buffer (struct buffer *buf, int nargs,
					  Lisp_Object *args,
					  enum run_hooks_condition cond);
Lisp_Object run_hook_with_args (int nargs, Lisp_Object *args,
				enum run_hooks_condition cond);
void va_run_hook_with_args (Lisp_Object hook_var, int nargs, ...);
void va_run_hook_with_args_in_buffer (struct buffer *buf, Lisp_Object hook_var,
				      int nargs, ...);
Lisp_Object run_hook (Lisp_Object hook);
Lisp_Object apply1 (Lisp_Object fn, Lisp_Object args);
Lisp_Object call0 (Lisp_Object fn);
Lisp_Object call1 (Lisp_Object fn, Lisp_Object a0);
Lisp_Object call2 (Lisp_Object fn, Lisp_Object a0, Lisp_Object a1);
Lisp_Object call3 (Lisp_Object fn,
		   Lisp_Object a0, Lisp_Object a1, Lisp_Object a2);
Lisp_Object call4 (Lisp_Object fn,
		   Lisp_Object a0, Lisp_Object a1, Lisp_Object a2,
		   Lisp_Object a3);
Lisp_Object call5 (Lisp_Object fn,
		   Lisp_Object a0, Lisp_Object a1, Lisp_Object a2,
		   Lisp_Object a3, Lisp_Object a4);
Lisp_Object call6 (Lisp_Object fn,
		   Lisp_Object a0, Lisp_Object a1, Lisp_Object a2,
		   Lisp_Object a3, Lisp_Object a4, Lisp_Object a5);
Lisp_Object call7 (Lisp_Object fn,
		   Lisp_Object a0, Lisp_Object a1, Lisp_Object a2,
		   Lisp_Object a3, Lisp_Object a4, Lisp_Object a5,
		   Lisp_Object a6);
Lisp_Object call8 (Lisp_Object fn,
		   Lisp_Object a0, Lisp_Object a1, Lisp_Object a2,
		   Lisp_Object a3, Lisp_Object a4, Lisp_Object a5,
		   Lisp_Object a6, Lisp_Object a7);
Lisp_Object call0_in_buffer (struct buffer *buf, Lisp_Object fn);
Lisp_Object call1_in_buffer (struct buffer *buf, Lisp_Object fn,
			     Lisp_Object a0);
Lisp_Object call2_in_buffer (struct buffer *buf, Lisp_Object fn,
			     Lisp_Object a0, Lisp_Object a1);
Lisp_Object call3_in_buffer (struct buffer *buf, Lisp_Object fn,
			     Lisp_Object a0, Lisp_Object a1,
			     Lisp_Object a2);
Lisp_Object call4_in_buffer (struct buffer *buf, Lisp_Object fn,
			     Lisp_Object a0, Lisp_Object a1,
			     Lisp_Object a2, Lisp_Object a3);
Lisp_Object call5_in_buffer (struct buffer *buf, Lisp_Object fn,
			     Lisp_Object a0, Lisp_Object a1,
			     Lisp_Object a2, Lisp_Object a3,
			     Lisp_Object a4);
Lisp_Object call6_in_buffer (struct buffer *buf, Lisp_Object fn,
			     Lisp_Object a0, Lisp_Object a1,
			     Lisp_Object a2, Lisp_Object a3,
			     Lisp_Object a4, Lisp_Object a5);
Lisp_Object eval_in_buffer (struct buffer *buf, Lisp_Object form);
Lisp_Object call0_with_handler (Lisp_Object handler, Lisp_Object fn);
Lisp_Object call1_with_handler (Lisp_Object handler, Lisp_Object fn,
				Lisp_Object a0);
Lisp_Object eval_in_buffer_trapping_errors (CONST char *warning_string,
					    struct buffer *buf,
					    Lisp_Object form);
Lisp_Object run_hook_trapping_errors (CONST char *warning_string,
					     Lisp_Object hook_symbol);
Lisp_Object safe_run_hook_trapping_errors (CONST char *warning_string,
					   Lisp_Object hook_symbol,
					   int allow_quit);
Lisp_Object call0_trapping_errors (CONST char *warning_string,
					  Lisp_Object function);
Lisp_Object call1_trapping_errors (CONST char *warning_string,
					  Lisp_Object function,
					  Lisp_Object object);
Lisp_Object call2_trapping_errors (CONST char *warning_string,
				   Lisp_Object function,
				   Lisp_Object object1,
				   Lisp_Object object2);
Lisp_Object call_with_suspended_errors (lisp_fn_t fun,
					volatile Lisp_Object retval,
					Lisp_Object class,
					Error_behavior errb,
					int nargs, ...);
/* C Code should be using internal_catch, record_unwind_p, condition_case_1 */
/* Lisp_Object Fcatch (Lisp_Object args); */
/* Lisp_Object Funwind_protect (Lisp_Object args); */
/* Lisp_Object Fcondition_case (Lisp_Object args); */
Lisp_Object Fthrow (Lisp_Object tag, Lisp_Object val);
Lisp_Object internal_catch (Lisp_Object tag, 
			    Lisp_Object (*func) (Lisp_Object arg),
			    Lisp_Object arg,
			    int *threw);
Lisp_Object condition_case_1 (Lisp_Object handlers,
			      Lisp_Object (*bfun) (Lisp_Object barg),
			      Lisp_Object barg,
			      Lisp_Object (*hfun) (Lisp_Object val,
						   Lisp_Object harg),
			      Lisp_Object harg);
Lisp_Object Fcondition_case_3 (Lisp_Object bodyform, 
			       Lisp_Object var, 
			       Lisp_Object handlers);
Lisp_Object unbind_to (int n, Lisp_Object val);
void specbind (Lisp_Object symbol, Lisp_Object value);
void record_unwind_protect (Lisp_Object (*function) (Lisp_Object arg),
			    Lisp_Object arg);
void do_autoload (Lisp_Object fundef, Lisp_Object funname);
Lisp_Object un_autoload (Lisp_Object oldqueue);
void warn_when_safe_lispobj (Lisp_Object class, Lisp_Object level,
			     Lisp_Object obj);
void warn_when_safe (Lisp_Object class, Lisp_Object level,
		     CONST char *fmt, ...) PRINTF_ARGS (3, 4);
Lisp_Object Fcommand_execute (Lisp_Object cmd, Lisp_Object record,
			      Lisp_Object keys);
Lisp_Object Ffetch_bytecode (Lisp_Object object);


/* Defined in event-stream.c */
Lisp_Object Fread_key_sequence (Lisp_Object prompt, Lisp_Object continue_echo,
				Lisp_Object dont_downcase_last);
Lisp_Object Fsit_for (Lisp_Object seconds, Lisp_Object nodisp);
Lisp_Object Fsleep_for (Lisp_Object seconds);
Lisp_Object Faccept_process_output (Lisp_Object process,
				    Lisp_Object timeout_secs,
				    Lisp_Object timeout_msecs);
Lisp_Object Fnext_event (Lisp_Object event, Lisp_Object prompt);
Lisp_Object Fnext_command_event (Lisp_Object event, Lisp_Object prompt);
Lisp_Object Fdispatch_event (Lisp_Object event);
void wait_delaying_user_input (int (*predicate) (void *arg),
			       void *predicate_arg);
int detect_input_pending (void);
void enqueue_command_event (Lisp_Object event);
Lisp_Object dequeue_command_event (void);
Lisp_Object Fadd_timeout (Lisp_Object secs, 
			  Lisp_Object function, Lisp_Object object, 
			  Lisp_Object resignal);
Lisp_Object Fdisable_timeout (Lisp_Object id); 
void reset_this_command_keys (Lisp_Object console, int clear_echo_area_p);
Lisp_Object Fenqueue_eval_event (Lisp_Object function,
				 Lisp_Object object);
Lisp_Object enqueue_misc_user_event (Lisp_Object channel,
				     Lisp_Object function,
				     Lisp_Object object);
extern Lisp_Object Qpre_command_hook, Qpost_command_hook;
Lisp_Object Fdiscard_input (void), Finput_pending_p (void);


/* Defined in event-Xt.c */
void signal_special_Xt_user_event (Lisp_Object channel,
				   Lisp_Object function,
				   Lisp_Object object);


/* Defined in events.c */
void clear_event_resource (void);
Lisp_Object Fmake_event (void);
Lisp_Object Fdeallocate_event (Lisp_Object event);
Lisp_Object Fcopy_event (Lisp_Object from, Lisp_Object to);
Lisp_Object allocate_event (void);
int event_to_character (struct Lisp_Event *event,
			int allow_extra_modifiers,
			int allow_meta,
			int allow_non_ascii);
Lisp_Object Fcharacter_to_event (Lisp_Object ch, Lisp_Object event,
				 Lisp_Object console,
				 Lisp_Object use_console_meta_flag);
Lisp_Object Fevent_to_character (Lisp_Object e,
				 Lisp_Object allow_extra_modifiers,
				 Lisp_Object allow_meta,
				 Lisp_Object allow_non_ascii);
Lisp_Object Fevent_over_text_area_p (Lisp_Object event);
Lisp_Object Fevent_over_modeline_p (Lisp_Object event);
Lisp_Object Fevent_over_border_p (Lisp_Object event);
Lisp_Object Fevent_over_toolbar_p (Lisp_Object event);
Lisp_Object Fevent_window (Lisp_Object event);
Lisp_Object Fevent_buffer (Lisp_Object event);
Lisp_Object Fevent_button (Lisp_Object event);
Lisp_Object Fevent_function (Lisp_Object event);
Lisp_Object Fevent_glyph_extent (Lisp_Object event);
Lisp_Object Fevent_modeline_position (Lisp_Object event);
Lisp_Object Fevent_key (Lisp_Object event);
Lisp_Object Fevent_modifiers (Lisp_Object event);
Lisp_Object Fevent_modifier_bits (Lisp_Object event);
Lisp_Object Fevent_object (Lisp_Object event);
Lisp_Object Fevent_point (Lisp_Object event);
Lisp_Object Fevent_process (Lisp_Object event);
Lisp_Object Fevent_timestamp (Lisp_Object event);
Lisp_Object Fevent_x (Lisp_Object event);
Lisp_Object Fevent_y (Lisp_Object event);
Lisp_Object Fevent_x_pixel (Lisp_Object event);
Lisp_Object Fevent_y_pixel (Lisp_Object event);

extern Lisp_Object QKbackspace, QKtab, QKlinefeed, QKreturn;
extern Lisp_Object QKescape, QKspace, QKdelete, QKnosymbol;


/* Defined in extents.c */
Lisp_Object Fextentp (Lisp_Object obj);
Lisp_Object Fextent_object (Lisp_Object ext);
Lisp_Object Fextent_start_position (Lisp_Object ext);
Lisp_Object Fextent_end_position (Lisp_Object ext);
Lisp_Object Fextent_length (Lisp_Object ext);
Lisp_Object Fmake_extent (Lisp_Object from, Lisp_Object to,
			  Lisp_Object buffer);
Lisp_Object Fset_extent_endpoints (Lisp_Object, Lisp_Object,
				   Lisp_Object, Lisp_Object);
Lisp_Object Fdelete_extent (Lisp_Object extent);
Lisp_Object Fdetach_extent (Lisp_Object);
Lisp_Object Fmap_extents (Lisp_Object function, 
			  Lisp_Object buffer, 
			  Lisp_Object from, Lisp_Object to,
			  Lisp_Object maparg,
			  Lisp_Object flags,
			  Lisp_Object property,
			  Lisp_Object value);
Lisp_Object Fextent_at (Lisp_Object pos, Lisp_Object buffer, 
			Lisp_Object flag, Lisp_Object before,
			Lisp_Object at_flag);
Lisp_Object Fextent_face (Lisp_Object);
Lisp_Object Fset_extent_face (Lisp_Object, Lisp_Object);
Lisp_Object Fset_extent_begin_glyph (Lisp_Object extent,
				     Lisp_Object begin_glyph,
				     Lisp_Object layout);
Lisp_Object Fset_extent_end_glyph (Lisp_Object extent_obj,
				   Lisp_Object glyph,
				   Lisp_Object layout);
Lisp_Object Fset_extent_begin_glyph_layout (Lisp_Object extent,
					    Lisp_Object layout);
Lisp_Object Fset_extent_end_glyph_layout (Lisp_Object extent,
					  Lisp_Object layout);
Lisp_Object Fextent_begin_glyph_layout (Lisp_Object extent);
Lisp_Object Fextent_end_glyph_layout (Lisp_Object extent);
Lisp_Object Fset_extent_priority (Lisp_Object extent, Lisp_Object pri);
Lisp_Object Fset_extent_property (Lisp_Object,Lisp_Object,Lisp_Object);
Lisp_Object Fextent_property (Lisp_Object extent, Lisp_Object,
			      Lisp_Object default_);
Lisp_Object Fextent_properties (Lisp_Object extent);
Lisp_Object Fforce_highlight_extent (Lisp_Object extent,
				     Lisp_Object flag);
Lisp_Object Fhighlight_extent (Lisp_Object extent, Lisp_Object flag);
Lisp_Object Fset_extent_parent (Lisp_Object, Lisp_Object);

Lisp_Object Fnext_single_property_change (Lisp_Object pos,
					  Lisp_Object prop,
					  Lisp_Object buffer,
					  Lisp_Object limit);
Lisp_Object Fprevious_single_property_change (Lisp_Object pos,
					      Lisp_Object prop,
					      Lisp_Object buffer,
					      Lisp_Object limit);
Lisp_Object Fput_text_property (Lisp_Object start,
				Lisp_Object end,
				Lisp_Object prop,
				Lisp_Object value,
				Lisp_Object object);
Lisp_Object Fextent_in_region_p(Lisp_Object extent,
				Lisp_Object from,
				Lisp_Object to,
				Lisp_Object flags);

extern Lisp_Object Qdetached, Qdestroyed, Qbegin_glyph, Qend_glyph;
extern Lisp_Object Qstart_open, Qend_open, Qread_only;
extern Lisp_Object Qunique, Qduplicable;
extern Lisp_Object Qoutside_margin, Qinside_margin, Qwhitespace;
extern Lisp_Object Qglyph_invisible;


/* Defined in faces.c */
extern Lisp_Object Vbuilt_in_face_specifiers;
Lisp_Object Fface_name (Lisp_Object);


/* Defined in fileio.c */
extern Lisp_Object Qfile_name_handler_alist;
extern Lisp_Object Qfile_error;
Lisp_Object Ffile_name_as_directory (Lisp_Object fn);
Lisp_Object Fexpand_file_name (Lisp_Object fn, Lisp_Object def);
Lisp_Object Ffile_name_nondirectory (Lisp_Object fn);
Lisp_Object Fsubstitute_in_file_name (Lisp_Object fn);
Lisp_Object Ffile_symlink_p (Lisp_Object fn);
Lisp_Object Ffile_truename (Lisp_Object name, Lisp_Object def);
Lisp_Object Ffile_name_nondirectory (Lisp_Object fn);
Lisp_Object Ffile_name_directory (Lisp_Object fn);
Lisp_Object Fdirectory_file_name (Lisp_Object fn);
Lisp_Object Ffile_directory_p (Lisp_Object fn);
Lisp_Object Ffile_readable_p (Lisp_Object fn);
Lisp_Object Ffile_name_absolute_p (Lisp_Object fn);
Lisp_Object Ffile_exists_p (Lisp_Object fn);
Lisp_Object Ffile_executable_p (Lisp_Object filename);
Lisp_Object Ffile_accessible_directory_p (Lisp_Object fn);
void record_auto_save (void);
void force_auto_save_soon (void);
Lisp_Object Ffind_file_name_handler (Lisp_Object filename,
				     Lisp_Object operation);
DECLARE_DOESNT_RETURN (report_file_error (CONST char *string,
					  Lisp_Object data));
void maybe_report_file_error (CONST char *string, Lisp_Object data,
			      Lisp_Object class, Error_behavior errb);
DECLARE_DOESNT_RETURN (signal_file_error (CONST char *string,
					  Lisp_Object data));
void maybe_signal_file_error (CONST char *string, Lisp_Object data,
			      Lisp_Object class, Error_behavior errb);
DECLARE_DOESNT_RETURN (signal_double_file_error (CONST char *string1,
						 CONST char *string2,
						 Lisp_Object data));
void maybe_signal_double_file_error (CONST char *string1, CONST char *string2,
				     Lisp_Object data, Lisp_Object class,
				     Error_behavior errb);
DECLARE_DOESNT_RETURN (signal_double_file_error_2 (CONST char *string1,
						   CONST char *string2,
						   Lisp_Object data1,
						   Lisp_Object data2));
void maybe_signal_double_file_error_2 (CONST char *string1,
				       CONST char *string2, Lisp_Object data1,
				       Lisp_Object data2, Lisp_Object class,
				       Error_behavior errb);
Lisp_Object lisp_strerror (int errnum);
Lisp_Object expand_and_dir_to_file (Lisp_Object fn, Lisp_Object def);
Lisp_Object Finsert_file_contents_internal (Lisp_Object filename,
					    Lisp_Object visit,
					    Lisp_Object beg,
					    Lisp_Object end,
					    Lisp_Object replace,
					    Lisp_Object codesys,
					    Lisp_Object used_codesys);
Lisp_Object Fdo_auto_save (Lisp_Object nomsg, Lisp_Object current_only);
Lisp_Object Fverify_visited_file_modtime (Lisp_Object buffer);
Lisp_Object Funhandled_file_name_directory (Lisp_Object filename);
Lisp_Object Fset_buffer_modtime (Lisp_Object buf, Lisp_Object in_time);
int read_allowing_quit (int fildes, void *buf, unsigned int nbyte);
int write_allowing_quit (int fildes, CONST void *buf,
			 unsigned int nbyte);
int internal_delete_file (Lisp_Object filename);


/* Defined in filelock.c */
void lock_file (Lisp_Object fn);
void unlock_file (Lisp_Object fn);
void unlock_all_files (void);
Lisp_Object Flock_buffer (Lisp_Object fn);
Lisp_Object Funlock_buffer (void);
void unlock_buffer (struct buffer *buffer);
Lisp_Object Ffile_locked_p (Lisp_Object fn);


/* Defined in filemode.c */
void filemodestring (struct stat *statp, char *str);


/* Defined in floatfns.c */
double extract_float (Lisp_Object);
Lisp_Object Ffloat (Lisp_Object n);


/* Defined in fns.c */
Lisp_Object list_sort (Lisp_Object list, 
		       Lisp_Object lisp_arg,
		       int (*pred_fn) (Lisp_Object first,
				       Lisp_Object second,
				       Lisp_Object lisp_arg));
Lisp_Object Fsort (Lisp_Object list, 
		   Lisp_Object pred);
Lisp_Object merge (Lisp_Object org_l1, Lisp_Object org_l2,
		   Lisp_Object pred);

extern Lisp_Object Qstring_lessp, Qidentity, Qyes_or_no_p;
extern Lisp_Object Vfeatures;
Lisp_Object Fidentity (Lisp_Object x);
Lisp_Object Frandom (Lisp_Object arg);
Lisp_Object Flength (Lisp_Object seq);
Lisp_Object Fstring_equal (Lisp_Object s1, Lisp_Object s2);
Lisp_Object Fstring_lessp (Lisp_Object s1, Lisp_Object s2);
Lisp_Object string_getprop (struct Lisp_String *s,
			    Lisp_Object property,
			    Lisp_Object default_);
void string_putprop (struct Lisp_String *s, Lisp_Object property,
		     Lisp_Object value);
void bump_string_modiff (Lisp_Object str);
Lisp_Object Fappend (int nargs, Lisp_Object *args);
Lisp_Object Fconcat (int nargs, Lisp_Object *args);
Lisp_Object Fvconcat (int nargs, Lisp_Object *args);
Lisp_Object Fcopy_sequence (Lisp_Object seq);
Lisp_Object Fsubstring (Lisp_Object str, Lisp_Object s, Lisp_Object e);
Lisp_Object Fnthcdr (Lisp_Object n, Lisp_Object list);
Lisp_Object Fnth (Lisp_Object n, Lisp_Object list);
Lisp_Object Fmember (Lisp_Object elt, Lisp_Object list);
Lisp_Object Fold_member (Lisp_Object measles_mumps, Lisp_Object and_rubella);
Lisp_Object Fmemq (Lisp_Object elt, Lisp_Object list);
Lisp_Object Fold_memq (Lisp_Object typhoid, Lisp_Object dysentery);
Lisp_Object memq_no_quit (Lisp_Object elt, Lisp_Object list);
Lisp_Object Fassoc (Lisp_Object elt, Lisp_Object list);
Lisp_Object assoc_no_quit (Lisp_Object key, Lisp_Object alist);
Lisp_Object Fassq (Lisp_Object key, Lisp_Object alist);
Lisp_Object Fold_assq (Lisp_Object syphilis, Lisp_Object gonorrhea);
Lisp_Object assq_no_quit (Lisp_Object key, Lisp_Object alist);
Lisp_Object Frassoc (Lisp_Object key, Lisp_Object alist);
Lisp_Object Frassq (Lisp_Object key, Lisp_Object alist);
Lisp_Object rassq_no_quit (Lisp_Object key, Lisp_Object alist);
Lisp_Object Fdelete (Lisp_Object elt, Lisp_Object list);
Lisp_Object Fdelq (Lisp_Object elt, Lisp_Object list);
Lisp_Object delq_no_quit (Lisp_Object elt, Lisp_Object list);
Lisp_Object delq_no_quit_and_free_cons (Lisp_Object elt, Lisp_Object list);
Lisp_Object Fremassoc (Lisp_Object elt, Lisp_Object list);
Lisp_Object remassoc_no_quit (Lisp_Object key, Lisp_Object list);
Lisp_Object Fremassq (Lisp_Object key, Lisp_Object alist);
Lisp_Object remassq_no_quit (Lisp_Object key, Lisp_Object alist);
Lisp_Object Fremrassoc (Lisp_Object key, Lisp_Object alist);
Lisp_Object Fremrassq (Lisp_Object key, Lisp_Object alist);
Lisp_Object remrassq_no_quit (Lisp_Object key, Lisp_Object alist);
Lisp_Object Freverse (Lisp_Object list), Fnreverse (Lisp_Object list);
Lisp_Object Fget (Lisp_Object sym, Lisp_Object prop, Lisp_Object def);
Lisp_Object Fput (Lisp_Object sym, Lisp_Object prop, Lisp_Object val);

void pure_put (Lisp_Object sym, Lisp_Object prop, Lisp_Object val);
Lisp_Object Fremprop (Lisp_Object sym, Lisp_Object prop);
int plists_differ (Lisp_Object a, Lisp_Object b, int nil_means_not_present,
		   int laxp, int depth);
Lisp_Object internal_plist_get (Lisp_Object plist, Lisp_Object property);
void internal_plist_put (Lisp_Object *plist, Lisp_Object property,
			 Lisp_Object value);
int internal_remprop (Lisp_Object *plist, Lisp_Object property);
Lisp_Object external_plist_get (Lisp_Object *plist, Lisp_Object property,
				int laxp, Error_behavior errb);
void external_plist_put (Lisp_Object *plist, Lisp_Object property,
			 Lisp_Object value, int laxp, Error_behavior errb);
int external_remprop (Lisp_Object *plist, Lisp_Object property,
		      int laxp, Error_behavior errb);
Lisp_Object Fequal (Lisp_Object one, Lisp_Object two);
Lisp_Object Fold_equal (Lisp_Object ebola, Lisp_Object marburg);
int internal_equal (Lisp_Object, Lisp_Object, int depth);
Lisp_Object Ffillarray (Lisp_Object array, Lisp_Object init);
Lisp_Object Fnconc (int nargs, Lisp_Object *args);
Lisp_Object Fmapcar (Lisp_Object fn, Lisp_Object seq);
Lisp_Object Ffeaturep (Lisp_Object name);
Lisp_Object Frequire (Lisp_Object name, Lisp_Object filename);
Lisp_Object Fprovide (Lisp_Object name);
Lisp_Object concat2 (Lisp_Object s1, Lisp_Object s2);
Lisp_Object concat3 (Lisp_Object s1, Lisp_Object s2, Lisp_Object s3);
Lisp_Object vconcat2 (Lisp_Object s1, Lisp_Object s2);
Lisp_Object vconcat3 (Lisp_Object s1, Lisp_Object s2, Lisp_Object s3);
Lisp_Object nconc2 (Lisp_Object l1, Lisp_Object l2);
Lisp_Object Fcopy_alist (Lisp_Object alist);
Lisp_Object Fcopy_tree (Lisp_Object arg, Lisp_Object vecp);
Lisp_Object Fplist_put (Lisp_Object plist, Lisp_Object prop, Lisp_Object val);
Lisp_Object Fplist_get (Lisp_Object plist, Lisp_Object prop,
			Lisp_Object default_);
Lisp_Object Fcanonicalize_plist (Lisp_Object plist,
				 Lisp_Object infected_with_hemorrhagic_fever);
Lisp_Object Flax_plist_put (Lisp_Object plist, Lisp_Object prop,
			    Lisp_Object val);
Lisp_Object Flax_plist_get (Lisp_Object plist, Lisp_Object prop,
			    Lisp_Object default_);
Lisp_Object Flax_plist_remprop (Lisp_Object plist, Lisp_Object prop);
Lisp_Object Fcanonicalize_lax_plist (Lisp_Object plist,
				     Lisp_Object
				     infected_with_hemorrhagic_fever);
Lisp_Object Fdestructive_alist_to_plist (Lisp_Object alist);
Lisp_Object Fcheck_valid_plist (Lisp_Object);
Lisp_Object Fvalid_plist_p (Lisp_Object);
void check_losing_bytecode (CONST char *function, Lisp_Object seq);


/* Defined in font-lock.c */
extern Lisp_Object Qcomment, Qblock_comment;
extern Lisp_Object Qbeginning_of_defun, Qend_of_defun;


/* Defined in frame.c */
Lisp_Object Fframep (Lisp_Object obj);
Lisp_Object Fframe_live_p (Lisp_Object obj);
Lisp_Object Fselect_frame (Lisp_Object scr);
Lisp_Object Fselected_frame (Lisp_Object device);
Lisp_Object Fmake_frame (Lisp_Object props, Lisp_Object device);
Lisp_Object Fwindow_frame (Lisp_Object window);
Lisp_Object Fframe_root_window (Lisp_Object frame);
Lisp_Object Fframe_selected_window (Lisp_Object frame);
Lisp_Object Fframe_list (Lisp_Object device);
Lisp_Object Fframe_name (Lisp_Object frame);
Lisp_Object Fnext_frame (Lisp_Object frame, Lisp_Object miniframe,
			 Lisp_Object device);
Lisp_Object Fmouse_position (Lisp_Object device);
Lisp_Object Fmouse_pixel_position (Lisp_Object device);
Lisp_Object Fset_mouse_position (Lisp_Object window,
				 Lisp_Object x, Lisp_Object y);
Lisp_Object Fset_mouse_pixel_position (Lisp_Object window,
				       Lisp_Object x, Lisp_Object y);
Lisp_Object Fraise_frame (Lisp_Object frame);
Lisp_Object Fmake_frame_visible (Lisp_Object frame);
Lisp_Object Fmake_frame_invisible (Lisp_Object frame,
				   Lisp_Object ignored);
Lisp_Object Ficonify_frame (Lisp_Object frame);
Lisp_Object Fdeiconify_frame (Lisp_Object frame);
Lisp_Object Fframe_visible_p (Lisp_Object frame);
Lisp_Object Fframe_iconified_p (Lisp_Object frame);
Lisp_Object Fvisible_frame_list (Lisp_Object device);
Lisp_Object Fset_frame_height (Lisp_Object frame,
			       Lisp_Object rows, Lisp_Object pretend);
Lisp_Object Fset_frame_width  (Lisp_Object frame,
			       Lisp_Object cols, Lisp_Object pretend);
Lisp_Object Fset_frame_size (Lisp_Object frame, 
			     Lisp_Object cols, Lisp_Object rows, 
			     Lisp_Object pretend);
Lisp_Object Fset_frame_position (Lisp_Object frame,
				 Lisp_Object xoffset, 
				 Lisp_Object yoffset);
Lisp_Object Fdelete_frame (Lisp_Object frame, Lisp_Object force);
Lisp_Object Fset_frame_properties (Lisp_Object frame, Lisp_Object plist);
Lisp_Object Fframe_property (Lisp_Object frame, Lisp_Object property,
			     Lisp_Object default_);
Lisp_Object Fset_frame_pointer (Lisp_Object frame, Lisp_Object pointer);

extern Lisp_Object Vcreate_frame_hook, Qcreate_frame_hook;
extern Lisp_Object Vmouse_enter_frame_hook, Qmouse_enter_frame_hook;
extern Lisp_Object Vmouse_leave_frame_hook, Qmouse_leave_frame_hook;
extern Lisp_Object Vmap_frame_hook, Qmap_frame_hook;
extern Lisp_Object Vunmap_frame_hook, Qunmap_frame_hook;
extern Lisp_Object Vmouse_motion_handler;
extern Lisp_Object Vsynchronize_minibuffers;

extern Lisp_Object Qdrag_and_drop_functions;

extern Lisp_Object Qframep, Qframe_live_p, Qdelete_frame;
extern Lisp_Object Qselect_frame_hook, Qdeselect_frame_hook;
extern Lisp_Object Qsynchronize_minibuffers;
extern Lisp_Object Qbuffer_predicate;

extern Lisp_Object Qminibuffer;
extern Lisp_Object Qunsplittable;
extern Lisp_Object Qinternal_border_width;
extern Lisp_Object Qtop_toolbar_shadow_color;
extern Lisp_Object Qbottom_toolbar_shadow_color;
extern Lisp_Object Qbackground_toolbar_color;
extern Lisp_Object Qtop_toolbar_shadow_pixmap;
extern Lisp_Object Qbottom_toolbar_shadow_pixmap;
extern Lisp_Object Qtoolbar_shadow_thickness;
extern Lisp_Object Qscrollbar_placement;
extern Lisp_Object Qinter_line_space;
extern Lisp_Object Qvisual_bell;
extern Lisp_Object Qbell_volume;
extern Lisp_Object Qpointer_background;
extern Lisp_Object Qpointer_color;
extern Lisp_Object Qtext_pointer;
extern Lisp_Object Qspace_pointer;
extern Lisp_Object Qmodeline_pointer;
extern Lisp_Object Qgc_pointer;
extern Lisp_Object Qinitially_unmapped;
extern Lisp_Object Quse_backing_store;
extern Lisp_Object Qborder_color;
extern Lisp_Object Qborder_width;

extern Lisp_Object Qvisible, Qiconic, Qinvisible, Qvisible_iconic;
extern Lisp_Object Qinvisible_iconic;
extern Lisp_Object Qnomini, Qvisible_nomini, Qiconic_nomini, Qinvisible_nomini;
extern Lisp_Object Qvisible_iconic_nomini, Qinvisible_iconic_nomini;



/* Defined in frame-x.c */
Lisp_Object Fx_window_id (Lisp_Object frame);
extern Lisp_Object Qpopup;


/* Defined in general.c */
extern Lisp_Object Qactually_requested;
extern Lisp_Object Qafter;
extern Lisp_Object Qall;
extern Lisp_Object Qand;
extern Lisp_Object Qassoc;
extern Lisp_Object Qat;
extern Lisp_Object Qautodetect;
extern Lisp_Object Qautomatic_conversion;
extern Lisp_Object Qbad_variable;
extern Lisp_Object Qbefore;
extern Lisp_Object Qbinary;
extern Lisp_Object Qblack;
extern Lisp_Object Qboolean;
extern Lisp_Object Qbottom;
extern Lisp_Object Qbuffer;
extern Lisp_Object Qbutton;
extern Lisp_Object Qcategory;
extern Lisp_Object Qcase;
extern Lisp_Object Qchannel;
extern Lisp_Object Qchar;
extern Lisp_Object Qcharacter;
extern Lisp_Object Qchars;
extern Lisp_Object Qcolor;
extern Lisp_Object Qcolumns;
extern Lisp_Object Qcommand;
extern Lisp_Object Qconsole;
extern Lisp_Object Qcritical;
extern Lisp_Object Qdata;
extern Lisp_Object Qdead;
extern Lisp_Object Qdelete;
extern Lisp_Object Qdelq;
extern Lisp_Object Qdefault;
extern Lisp_Object Qdevice;
extern Lisp_Object Qdimension;
extern Lisp_Object Qdisplay;
extern Lisp_Object Qdoc_string;
extern Lisp_Object Qdynarr_overhead;
extern Lisp_Object Qempty;
extern Lisp_Object Qeq;
extern Lisp_Object Qequal;
extern Lisp_Object Qeql;
extern Lisp_Object Qeval;
extern Lisp_Object Qextents;
extern Lisp_Object Qface;
extern Lisp_Object Qfont;
extern Lisp_Object Qframe;
extern Lisp_Object Qfunction;
extern Lisp_Object Qgap_overhead;
extern Lisp_Object Qgeneric;
extern Lisp_Object Qgeometry;
extern Lisp_Object Qglobal;
extern Lisp_Object Qheight;
extern Lisp_Object Qhighlight;
extern Lisp_Object Qid;
extern Lisp_Object Qimage;
extern Lisp_Object Qinfo;
extern Lisp_Object Qinherit;
extern Lisp_Object Qinteger;
extern Lisp_Object Qinternal;
extern Lisp_Object Qkey;
extern Lisp_Object Qkey_assoc;
extern Lisp_Object Qkeyboard;
extern Lisp_Object Qkeymap;
extern Lisp_Object Qleft;
extern Lisp_Object Qlist;
extern Lisp_Object Qmagic;
extern Lisp_Object Qmalloc_overhead;
extern Lisp_Object Qmarkers;
extern Lisp_Object Qmax;
extern Lisp_Object Qmemory;
extern Lisp_Object Qmenubar;
extern Lisp_Object Qmessage;
extern Lisp_Object Qminus;
extern Lisp_Object Qmodifiers;
extern Lisp_Object Qmotion;
extern Lisp_Object Qname;
extern Lisp_Object Qnone;
extern Lisp_Object Qnot;
extern Lisp_Object Qnothing;
extern Lisp_Object Qnotice;
extern Lisp_Object Qobject;
extern Lisp_Object Qonly;
extern Lisp_Object Qor;
extern Lisp_Object Qother;
extern Lisp_Object Qpath;
extern Lisp_Object Qpointer;
extern Lisp_Object Qprint;
extern Lisp_Object Qprocess;
extern Lisp_Object Qprovide;
extern Lisp_Object Qrassoc;
extern Lisp_Object Qrassq;
extern Lisp_Object Qrequire;
extern Lisp_Object Qresource;
extern Lisp_Object Qreturn;
extern Lisp_Object Qreverse;
extern Lisp_Object Qright;
extern Lisp_Object Qold_assoc;
extern Lisp_Object Qold_delete;
extern Lisp_Object Qold_delq;
extern Lisp_Object Qold_rassoc;
extern Lisp_Object Qold_rassq;
extern Lisp_Object Qsearch;
extern Lisp_Object Qsimple;
extern Lisp_Object Qspace;
extern Lisp_Object Qspecifier;
extern Lisp_Object Qstream;
extern Lisp_Object Qstring;
extern Lisp_Object Qsymbol;
extern Lisp_Object Qsyntax;
extern Lisp_Object Qtext;
extern Lisp_Object Qtimeout;
extern Lisp_Object Qtimestamp;
extern Lisp_Object Qtoolbar;
extern Lisp_Object Qtop;
extern Lisp_Object Qtty;
extern Lisp_Object Qtype;
extern Lisp_Object Qundefined;
extern Lisp_Object Qunimplemented;
extern Lisp_Object Qvalue_assoc;
extern Lisp_Object Qvector;
extern Lisp_Object Qwarning;
extern Lisp_Object Qwhite;
extern Lisp_Object Qwidth;
extern Lisp_Object Qwindow;
extern Lisp_Object Qwindow_system;
extern Lisp_Object Qx;
extern Lisp_Object Qy;


/* Defined in getloadavg.c */
int getloadavg (double loadavg[], int nelem);


/* Defined in glyphs.c */
Lisp_Object Fmake_glyph_internal (Lisp_Object type);
Lisp_Object Fmake_image_instance (Lisp_Object data,
				  Lisp_Object device,
				  Lisp_Object force_mono,
				  Lisp_Object no_error);
Lisp_Object Fimage_instance_p (Lisp_Object obj);
Lisp_Object Fimage_instance_type (Lisp_Object image_instance);
Lisp_Object Fglyph_type (Lisp_Object glyph);
extern Lisp_Object Qnothing, Qmono_pixmap, Qcolor_pixmap;
extern Lisp_Object Qsubwindow;
extern Lisp_Object Qformatted_string;
extern Lisp_Object Qicon;
extern Lisp_Object Qconst_glyph_variable;
extern Lisp_Object Qdisplay_table;
Error_behavior decode_error_behavior_flag (Lisp_Object no_error);
Lisp_Object encode_error_behavior_flag (Error_behavior errb);


/* Defined in glyphs-x.c */
Lisp_Object Fcolorize_image_instance (Lisp_Object, Lisp_Object,
				      Lisp_Object);


/* Defined in indent.c */
Lisp_Object Fvertical_motion (Lisp_Object lines, Lisp_Object window);
Lisp_Object Findent_to (Lisp_Object col, Lisp_Object mincol,
			Lisp_Object buffer);
Lisp_Object Fcurrent_column (Lisp_Object buffer); 
int bi_spaces_at_point (struct buffer *b, Bytind pos);
int column_at_point (struct buffer *buf, Bufpos pos, int cur_col);
int current_column (struct buffer *buf);
void invalidate_current_column (void);
Bufpos vmotion (struct window *w, Bufpos orig, int vtarget,
		int *ret_vpos);


/* Defined in intl.c */
Lisp_Object Fignore_defer_gettext (Lisp_Object obj);
Lisp_Object Fgettext (Lisp_Object string);
Lisp_Object Fdgettext (Lisp_Object domain, Lisp_Object string);
extern Lisp_Object Qdefer_gettext;
#ifdef I18N3
extern Lisp_Object Vfile_domain;
#endif


/* Defined in keymap.c */
Lisp_Object Fcurrent_local_map (Lisp_Object buffer);
Lisp_Object Fkeymapp (Lisp_Object);
Lisp_Object Fmake_sparse_keymap (Lisp_Object);
Lisp_Object Fkeymap_fullness (Lisp_Object keymap);
Lisp_Object Fkey_description (Lisp_Object key);
Lisp_Object Fsingle_key_description (Lisp_Object key);
Lisp_Object Ftext_char_description (Lisp_Object c);
Lisp_Object Fdefine_key (Lisp_Object keymap, Lisp_Object key,
			 Lisp_Object definition);
extern Lisp_Object Qmodeline_map;
extern Lisp_Object Vsingle_space_string;
extern Lisp_Object Qcontrol, Qctrl, Qmeta, Qsuper, Qhyper, Qalt, Qshift;
extern Lisp_Object Qkeymap, Qkeymapp;
void where_is_to_char (Lisp_Object definition, char *buffer);


/* Defined in lread.c */
extern Lisp_Object Qvariable_documentation, Qstandard_input, Qread_char;
extern Lisp_Object Qvariable_domain; /* I18N3 */
extern Lisp_Object Qload;
extern Lisp_Object Vstandard_input;
extern Lisp_Object Vvalues;
extern Lisp_Object Vcurrent_compiled_function_annotation;
extern Lisp_Object Vload_file_name_internal;
extern Lisp_Object Vload_file_name_internal_the_purecopy;
Lisp_Object Fread (Lisp_Object readcharfun);
Lisp_Object Fread_from_string (Lisp_Object string, 
			       Lisp_Object start, Lisp_Object end);
Lisp_Object Fload_internal (Lisp_Object filename,
			    Lisp_Object missing_ok,
			    Lisp_Object nomessage,
			    Lisp_Object nosuffix,
			    Lisp_Object codesys,
			    Lisp_Object used_codesys);
void ebolify_bytecode_constants (Lisp_Object vector);
void close_load_descs (void);
int locate_file (Lisp_Object path, 
		 Lisp_Object str, CONST char *suffix, 
		 Lisp_Object *storeptr, int mode);
Lisp_Object Flocate_file_clear_hashing (Lisp_Object path);
int isfloat_string (CONST char *);

/* Well, I've decided to enable this. -- ben */
#define LOADHIST

#ifdef LOADHIST /* this is just a stupid idea */
#define LOADHIST_ATTACH(x) \
 do { if (initialized) Vcurrent_load_list = Fcons (x, Vcurrent_load_list); } \
 while (0)
extern Lisp_Object Vcurrent_load_list;
extern Lisp_Object Vload_history;
#else /*! LOADHIST */
# define LOADHIST_ATTACH(x)
#endif /*! LOADHIST */


/* Defined in macros.c */
Lisp_Object Fexecute_kbd_macro (Lisp_Object macro, 
				Lisp_Object prefixarg);


/* Defined in marker.c */
Bytind bi_marker_position (Lisp_Object marker);
Bufpos marker_position (Lisp_Object marker);
void set_bi_marker_position (Lisp_Object marker, Bytind pos);
void set_marker_position (Lisp_Object marker, Bufpos pos);
void unchain_marker (Lisp_Object marker);
Lisp_Object Fset_marker (Lisp_Object marker, 
			 Lisp_Object pos, Lisp_Object buffer);
Lisp_Object Fmarker_position (Lisp_Object m);
Lisp_Object Fmarker_buffer (Lisp_Object m);
Lisp_Object Fcopy_marker (Lisp_Object m, Lisp_Object type);
Lisp_Object noseeum_copy_marker (Lisp_Object m, Lisp_Object type);
Lisp_Object set_marker_restricted (Lisp_Object marker,
				   Lisp_Object pos, Lisp_Object buf);
#ifdef MEMORY_USAGE_STATS
int compute_buffer_marker_usage (struct buffer *b,
				 struct overhead_stats *ovstats);
#endif


/* Defined in menubar.c */
Lisp_Object Fpopup_menu (Lisp_Object menu_desc, Lisp_Object event);
extern Lisp_Object Qcurrent_menubar;
extern Lisp_Object Qactivate_menubar_hook;
extern Lisp_Object Qmenu_no_selection_hook;
extern Lisp_Object Vactivate_menubar_hook;
extern int popup_menu_up_p;
extern Lisp_Object Qmouse_event_p; /* events.c */
extern Lisp_Object Q_active, Q_suffix, Q_keys, Q_style, Q_selected;
extern Lisp_Object Q_filter, Q_config, Q_included;
extern Lisp_Object Q_accelerator;
extern Lisp_Object Qtoggle, Qradio;
extern Lisp_Object Vmenubar_configuration;
extern int menubar_show_keybindings;
extern Lisp_Object Vblank_menubar;
extern int popup_menu_titles;


/* Defined in minibuf.c */
extern int minibuf_level;
Charcount scmp_1 (CONST Bufbyte *s1, CONST Bufbyte *s2, Charcount len,
		  int ignore_case);
#define scmp(s1, s2, len) scmp_1 (s1, s2, len, completion_ignore_case)
Lisp_Object Fread_from_minibuffer (Lisp_Object prompt, 
				   Lisp_Object init,
				   Lisp_Object keymap,
				   Lisp_Object read_crock,
				   Lisp_Object hist);
extern int completion_ignore_case;
extern Lisp_Object Qcompletion_ignore_case;
extern Lisp_Object Vcompletion_regexp_list;
int regexp_ignore_completion_p (CONST Bufbyte *nonreloc,
				Lisp_Object reloc, Bytecount offset,
				Bytecount length);

extern Lisp_Object Vminibuffer_zero;

extern Lisp_Object Vecho_area_buffer;
Lisp_Object clear_echo_area (struct frame *f, Lisp_Object label, 
			     int no_restore);
Lisp_Object clear_echo_area_from_print (struct frame *f, 
					Lisp_Object label, 
					int no_restore);
void echo_area_append (struct frame *f, CONST Bufbyte *nonreloc,
		       Lisp_Object reloc, Bytecount offset,
		       Bytecount length, Lisp_Object type);
void echo_area_message (struct frame *f, CONST Bufbyte *nonreloc,
			Lisp_Object reloc, Bytecount offset,
			Bytecount length, Lisp_Object type);
Lisp_Object echo_area_status (struct frame *f);
int echo_area_active (struct frame *f);
Lisp_Object echo_area_contents (struct frame *f);
void message_internal (CONST Bufbyte *nonreloc, Lisp_Object reloc,
		       Bytecount offset, Bytecount length);
void message_append_internal (CONST Bufbyte *nonreloc, Lisp_Object reloc,
			      Bytecount offset, Bytecount length);
void message (CONST char *fmt, ...) PRINTF_ARGS (1, 2);
void message_append (CONST char *fmt, ...) PRINTF_ARGS (1, 2);
void message_no_translate (CONST char *fmt, ...) PRINTF_ARGS (1, 2);
void clear_message (void);


/* Defined in mocklisp.c */
extern Lisp_Object Vmocklisp_arguments, Qmocklisp, Qmocklisp_arguments;
Lisp_Object ml_apply (Lisp_Object function, Lisp_Object args);


/* Defined in mule-*.c */
Lisp_Object Ffind_charset (Lisp_Object charset);
Lisp_Object Fget_coding_system (Lisp_Object coding_system);
Lisp_Object Ffind_coding_system (Lisp_Object coding_system);
Lisp_Object Fcoding_system_property (Lisp_Object coding_system,
				     Lisp_Object property);
extern Lisp_Object Qmnemonic;


/* Defined in objects-tty.c */
extern Lisp_Object Vtty_color_alist;
#if 0
Lisp_Object Vtty_dynamic_color_fg;
Lisp_Object Vtty_dynamic_color_bg;
#endif


/* Defined in print.c */
void write_string_to_stdio_stream (FILE *stream, struct console *con,
				   CONST Bufbyte *str, Bytecount offset,
				   Bytecount len,
				   enum external_data_format fmt);
extern Lisp_Object Vprin1_to_string_buffer;
Lisp_Object Fprin1 (Lisp_Object obj, Lisp_Object printcharfun);
Lisp_Object Fprinc (Lisp_Object obj, Lisp_Object printcharfun);
Lisp_Object Fprint (Lisp_Object obj, Lisp_Object printcharfun);
Lisp_Object Fprin1_to_string (Lisp_Object obj, Lisp_Object noescape);
Lisp_Object Fterpri (Lisp_Object printcharfun);
extern Lisp_Object Vstandard_output, Qstandard_output;
extern Lisp_Object Qexternal_debugging_output;
void debug_backtrace (void);
void debug_short_backtrace (int length);
void temp_output_buffer_setup (CONST char *bufname);
void temp_output_buffer_show (Lisp_Object buf, Lisp_Object same_scrn);
/* NOTE: Do not call this with the data of a Lisp_String.  Use princ.
 * Note: stream should be defaulted before calling
 *  (eg Qnil means stdout, not Vstandard_output, etc) */
void write_c_string (CONST char *s, Lisp_Object printcharfun);
/* Same goes for this function. */
void write_string_1 (CONST Bufbyte *s, Bytecount size, 
		     Lisp_Object printcharfun);
void print_internal (Lisp_Object obj, 
		     Lisp_Object printcharfun, 
		     int escapeflag);
extern Lisp_Object Vprint_level;
extern Lisp_Object Vprint_length;
extern int print_escape_newlines;
extern int print_readably;
extern Lisp_Object Qprint_escape_newlines;
Lisp_Object internal_with_output_to_temp_buffer  (CONST char *bufname, 
						  Lisp_Object (*function)
						  (Lisp_Object args),
						  Lisp_Object args,
						  Lisp_Object same_frame);
void float_to_string (char *buf, double data);
void print_symbol (Lisp_Object, Lisp_Object stream, int escapeflag);
void print_compiled_function (Lisp_Object, Lisp_Object stream, int escapeflag);
void print_float (Lisp_Object, Lisp_Object stream, int escapeflag);
extern Lisp_Object Qprint_length, Qprint_string_length;
void internal_object_printer (Lisp_Object obj, Lisp_Object printcharfun,
			      int escapeflag);
extern Lisp_Object Ferror_message_string(Lisp_Object obj);

/* Defined in process.c */
Lisp_Object Fprocess_send_eof (Lisp_Object process);


/* Defined in profile.c */
void mark_profiling_info (void (*markfun) (Lisp_Object));
extern int profiling_active;
extern int profiling_redisplay_flag;


/* Defined in rangetab.c */
Lisp_Object Fmake_range_table (void);
Lisp_Object Fput_range_table (Lisp_Object start, Lisp_Object end,
			      Lisp_Object val, Lisp_Object table);
Lisp_Object Fget_range_table (Lisp_Object pos, Lisp_Object table,
			      Lisp_Object default_);
Lisp_Object Fclear_range_table (Lisp_Object table);
void put_range_table (Lisp_Object table, EMACS_INT first,
		      EMACS_INT last, Lisp_Object val);
int unified_range_table_bytes_needed (Lisp_Object rangetab);
int unified_range_table_bytes_used (void *unrangetab);
void unified_range_table_copy_data (Lisp_Object rangetab, void *dest);
Lisp_Object unified_range_table_lookup (void *unrangetab, EMACS_INT pos,
					Lisp_Object default_);
int unified_range_table_nentries (void *unrangetab);
void unified_range_table_get_range (void *unrangetab, int offset,
				    EMACS_INT *min, EMACS_INT *max,
				    Lisp_Object *val);


/* Defined in redisplay.c */
Lisp_Object Fredraw_frame (Lisp_Object frame, Lisp_Object no_preempt);
Lisp_Object Fmessage_displayed_p (Lisp_Object return_string);
extern Lisp_Object Voverlay_arrow_position, Voverlay_arrow_string;
extern Lisp_Object Vbar_cursor;
extern Lisp_Object Qbar_cursor;
extern Lisp_Object Vwindow_system;
extern Lisp_Object Qcursor_in_echo_area;


/* Defined in search.c */
struct re_pattern_buffer;
struct re_registers;
Lisp_Object Fstring_match (Lisp_Object regexp,
			   Lisp_Object string, Lisp_Object start,
			   Lisp_Object buffer);
Lisp_Object Fmatch_beginning (Lisp_Object n);
Lisp_Object Fmatch_end (Lisp_Object n);
Lisp_Object Fskip_chars_forward (Lisp_Object string, Lisp_Object lim,
				 Lisp_Object buffer);
Lisp_Object Fskip_chars_backward (Lisp_Object string, Lisp_Object lim,
				  Lisp_Object buffer);
Bufpos scan_buffer (struct buffer *buf, Emchar target, Bufpos start,
		    Bufpos end, int cnt, int *shortage,
		    int allow_quit);
Bufpos find_next_newline (struct buffer *buf, Bufpos from, int cnt);
Bufpos find_next_newline_no_quit (struct buffer *buf, Bufpos from, int cnt);
Bytind bi_find_next_newline_no_quit (struct buffer *buf, Bytind from, int cnt);
Bufpos find_before_next_newline (struct buffer *buf, Bufpos from,
				 Bufpos to, int cnt);
struct re_pattern_buffer *compile_pattern (Lisp_Object pattern,
					   struct re_registers *regp,
					   char *translate, int posix,
					   Error_behavior errb);
Bytecount fast_string_match (Lisp_Object regexp,  CONST Bufbyte *nonreloc,
			     Lisp_Object reloc, Bytecount offset,
			     Bytecount length, int case_fold_search,
			     Error_behavior errb, int no_quit);
Bytecount fast_lisp_string_match (Lisp_Object regex, Lisp_Object string);
Lisp_Object Fre_search_forward (Lisp_Object string, Lisp_Object bound,
				Lisp_Object no_error,
				Lisp_Object count,
				Lisp_Object buffer);
void restore_match_data (void);


/* Defined in signal.c */
void init_interrupts_late (void);
extern int dont_check_for_quit;
void begin_dont_check_for_quit (void);
void emacs_sleep (int secs);


/* Defined in sound.c */
Lisp_Object Fplay_sound (Lisp_Object sound, Lisp_Object volume,
			 Lisp_Object device);
Lisp_Object Fding (Lisp_Object arg, Lisp_Object sound,
		   Lisp_Object device);
void init_device_sound (struct device *d);
extern Lisp_Object Qnas;


/* Defined in specifier.c */
Lisp_Object Fspecifier_instance (Lisp_Object specifier,
				 Lisp_Object locale,
				 Lisp_Object default_,
				 Lisp_Object no_fallback);
Lisp_Object Fspecifier_specs (Lisp_Object specifier,
			      Lisp_Object locale,
			      Lisp_Object tag_set,
			      Lisp_Object exact_p);
Lisp_Object Fspecifier_spec_list (Lisp_Object specifier,
				  Lisp_Object locale,
				  Lisp_Object tag_set,
				  Lisp_Object exact_p);
Lisp_Object Fmake_specifier (Lisp_Object type);
Lisp_Object Fadd_spec_to_specifier (Lisp_Object specifier,
				    Lisp_Object locale,
				    Lisp_Object instantiator,
				    Lisp_Object tag,
				    Lisp_Object how_to_add);
Lisp_Object Fvalid_specifier_locale_p (Lisp_Object locale);
Lisp_Object Fcopy_specifier (Lisp_Object specifier,
			     Lisp_Object dest,
			     Lisp_Object locale,
			     Lisp_Object tag_set,
			     Lisp_Object exact_p,
			     Lisp_Object how_to_add);
Lisp_Object Fremove_specifier (Lisp_Object specifier,
			       Lisp_Object locale,
			       Lisp_Object tag_set,
			       Lisp_Object exact_p);
Lisp_Object specifier_instance (Lisp_Object specifier,
				Lisp_Object matchspec,
				Lisp_Object domain,
				Error_behavior errb,
				int no_quit,
				int no_fallback,
				Lisp_Object depth);
Lisp_Object specifier_instance_no_quit (Lisp_Object specifier,
					Lisp_Object matchspec,
					Lisp_Object domain,
					Error_behavior errb,
					int no_fallback,
					Lisp_Object depth);
Lisp_Object Fset_specifier_dirty_flag (Lisp_Object specifier);
extern Lisp_Object Qfallback;
extern Lisp_Object Qnatnum;


/* Defined in symbols.c */
extern Lisp_Object Vobarray;
Lisp_Object Fintern (Lisp_Object str, Lisp_Object obarray);
Lisp_Object Fintern_soft (Lisp_Object str, Lisp_Object obarray);
int hash_string (CONST Bufbyte *, Bytecount len);
Lisp_Object intern (CONST char *);
Lisp_Object oblookup (Lisp_Object obarray,
		      CONST Bufbyte *, Bytecount len);
void map_obarray (Lisp_Object obarray, 
		  void (*fn) (Lisp_Object sym, Lisp_Object arg),
		  Lisp_Object arg);
Lisp_Object Fboundp (Lisp_Object sym);
Lisp_Object Ffboundp (Lisp_Object);
Lisp_Object Ffset (Lisp_Object sym, Lisp_Object val);
Lisp_Object Fsymbol_plist (Lisp_Object sym);
Lisp_Object Fsetplist (Lisp_Object sym, Lisp_Object val);
Lisp_Object Fsymbol_function (Lisp_Object sym);
Lisp_Object Fsymbol_value (Lisp_Object sym);
Lisp_Object Fdefault_value (Lisp_Object sym);
Lisp_Object Fdefault_boundp (Lisp_Object sym);
Lisp_Object Fset (Lisp_Object sym, Lisp_Object val);
Lisp_Object Fset_default (Lisp_Object sym, Lisp_Object val);
Lisp_Object Fsymbol_name (Lisp_Object sym);
Lisp_Object Findirect_function (Lisp_Object object);
Lisp_Object indirect_function (Lisp_Object object, int errorp);
Lisp_Object symbol_value_in_buffer (Lisp_Object sym, Lisp_Object buf);
void kill_buffer_local_variables (struct buffer *buf);
Lisp_Object Fmake_local_variable (Lisp_Object object);
int symbol_value_buffer_local_info (Lisp_Object symbol, struct buffer *);
Lisp_Object find_symbol_value (Lisp_Object symbol);
Lisp_Object find_symbol_value_quickly (Lisp_Object symbol_cons, int find_it_p);
Lisp_Object top_level_value (Lisp_Object symbol);
Lisp_Object Fkill_local_variable (Lisp_Object symbol);
Lisp_Object Fmake_variable_buffer_local (Lisp_Object variable);
Lisp_Object Fbuilt_in_variable_type (Lisp_Object sym);
extern Lisp_Object Qconst_specifier;

/* Defined in syntax.c */
int scan_words (struct buffer *buf, int from, int count);
Lisp_Object Fforward_word (Lisp_Object n, Lisp_Object buffer);


/* Defined in sysdep.c, also declared in sysdep.h.
   (These may be called via the open, etc macros instead...)
 */
#ifdef ENCAPSULATE_OPEN
int sys_open (CONST char *path, int oflag, ...);
#endif
#ifdef ENCAPSULATE_CLOSE
int sys_close (int fd);
#endif
#ifdef ENCAPSULATE_READ
int sys_read (int fildes, void *buf, unsigned int nbyte);
#endif
#ifdef ENCAPSULATE_WRITE
int sys_write (int fildes, CONST void *buf, unsigned int nbyte);
#endif

#ifdef ENCAPSULATE_FOPEN
FILE *sys_fopen (CONST char *path, CONST char *type);
#endif
#ifdef ENCAPSULATE_FCLOSE
int sys_fclose (FILE *stream);
#endif
#ifdef ENCAPSULATE_FREAD
size_t sys_fread (void *ptr, size_t size, size_t nitem, FILE *stream);
#endif
#ifdef ENCAPSULATE_FWRITE
size_t sys_fwrite (CONST void *ptr, size_t size, size_t nitem,
		   FILE *stream);
#endif

unsigned int sys_getuid (void);

char *egetenv (CONST char *);
/* extern char *getenv (CONST char *); */


/* Defined in undo.c */
Lisp_Object Fundo_boundary (void);
Lisp_Object truncate_undo_list (Lisp_Object list, int min, int max);
void record_extent (Lisp_Object extent, int attached);
void record_insert (struct buffer *b, Bufpos beg, Charcount length);
void record_delete (struct buffer *b, Bufpos beg, Charcount length);
void record_change (struct buffer *b, Bufpos beg, Charcount length);
extern Lisp_Object Qinhibit_read_only;


/* Defined in unex*.c */
int unexec (char *new_name, char *a_name,
	    uintptr_t data_start, 
	    uintptr_t bss_start, 
	    uintptr_t entry_address);
#ifdef RUN_TIME_REMAP
int run_time_remap (char *);
#endif


/* Defined in vm-limit.c */
void memory_warnings (void *start, void (*warnfun) (CONST char *));


/* Defined in window.c */
extern Lisp_Object Qvisible;
extern Lisp_Object Qscroll_up, Qscroll_down;
Lisp_Object Fselected_window (Lisp_Object device);
Lisp_Object Fwindow_buffer (Lisp_Object window);
Lisp_Object Fwindow_lowest_p (Lisp_Object window);
Lisp_Object Fwindow_highest_p (Lisp_Object window);
Lisp_Object Fget_buffer_window (Lisp_Object buffer, 
				Lisp_Object frame,
				Lisp_Object ignored);
Lisp_Object Fsave_window_excursion (Lisp_Object body);
Lisp_Object Fset_window_configuration (Lisp_Object config);
Lisp_Object save_window_excursion_unwind (Lisp_Object window_config);
Lisp_Object Fcurrent_window_configuration (Lisp_Object frame);
Lisp_Object display_buffer (Lisp_Object buffer, 
			    Lisp_Object notthiswindow, 
			    Lisp_Object overrideframe);
Lisp_Object Freplace_buffer_in_windows (Lisp_Object buffer);
Lisp_Object Fwindow_dedicated_p (Lisp_Object window);
Lisp_Object Fnext_window (Lisp_Object window, 
			  Lisp_Object minibuf, 
			  Lisp_Object all_frames,
			  Lisp_Object device);
Lisp_Object Fdelete_window (Lisp_Object window, Lisp_Object force);
Lisp_Object Fselect_window (Lisp_Object window);
Lisp_Object Fset_window_buffer (Lisp_Object window, 
				Lisp_Object buffer);
Lisp_Object Fsplit_window (Lisp_Object window, 
			   Lisp_Object chsize, 
			   Lisp_Object horflag);
Lisp_Object Frecenter (Lisp_Object arg, Lisp_Object window);
Lisp_Object Fmove_to_window_line (Lisp_Object arg, Lisp_Object window);
Lisp_Object Fbuffer_left_margin_pixwidth (Lisp_Object buffer);
Lisp_Object Fbuffer_right_margin_pixwidth (Lisp_Object buffer);
Lisp_Object Fset_window_hscroll (Lisp_Object window, Lisp_Object ncol);
Lisp_Object Fwindow_point (Lisp_Object window);
Lisp_Object Fset_window_point (Lisp_Object window, Lisp_Object pos);
Lisp_Object Fset_window_start (Lisp_Object window, Lisp_Object pos,
			       Lisp_Object noforce);
Lisp_Object Fwindow_start (Lisp_Object window);
Lisp_Object Fwindow_end (Lisp_Object window, Lisp_Object guarantee);

#endif /* _EMACSFNS_H_ */
