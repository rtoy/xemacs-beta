/* Various initialization function prototypes.
   Copyright (C) 1995 Board of Trustees, University of Illinois.
   Copyright (C) 2001, 2002 Ben Wing.

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

/* Synched up with: Not in FSF. */

#ifndef INCLUDED_symsinit_h_
#define INCLUDED_symsinit_h_

/* Earliest environment initializations (dump-time and run-time). */

void init_win32_very_very_early (void);

void init_data_very_early (void);
void init_floatfns_very_early (void);
void init_free_hook (void);
void init_process_times_very_early (void);
void init_ralloc (void);
void init_signals_very_early (void);
void init_mswindows_dde_very_early (void);

/* Early Lisp-engine initialization -- dump-time only for init, dump-time
   and post-pdump-load-time for reinit.  We call the reinit() routine
   ourselves at post-pdump-load-time, but the init_() routine calls the
   reinit() routine itself. (This is because sometimes the timing of when
   to call the routine is tricky -- the init routine might need to do some
   stuff, call the reinit() routine, and do some more stuff.) */

void init_alloc_once_early (void);
void reinit_alloc_early (void);
void init_symbols_once_early (void);
void reinit_symbols_early (void);
void init_errors_once_early (void);
void reinit_opaque_early (void);
void init_opaque_once_early (void);
void init_elhash_once_early (void);
void init_eistring_once_early (void);
void reinit_eistring_early (void);

/* Reset the Lisp engine.  Called both at dump-time, run-time and
   run-temacs-time; at dump-time, it's called early, before any of the
   vars() or complex_vars() routines.  Currently does almost nothing. */

void init_alloc_early (void);

/* Called somewhat randomly -- at dump-time, in the middle of the vars()
   calls, and at run-time, just before the late initializations. */

void init_eval_semi_early (void);

/* Declare the built-in symbols and primitives (dump-time only). */

void syms_of_abbrev (void);
void syms_of_alloc (void);
void syms_of_balloon_x (void);
void syms_of_buffer (void);
void syms_of_bytecode (void);
void syms_of_callint (void);
void syms_of_casefiddle (void);
void syms_of_casetab (void);
void syms_of_chartab (void);
void syms_of_cmdloop (void);
void syms_of_cmds (void);
void syms_of_console (void);
void syms_of_console_mswindows (void);
void syms_of_console_tty (void);
void syms_of_data (void);
void syms_of_database (void);
void syms_of_debug (void);
void syms_of_device (void);
void syms_of_device_mswindows (void);
void syms_of_device_tty (void);
void syms_of_device_x (void);
void syms_of_dialog (void);
void syms_of_dialog_mswindows (void);
void syms_of_dialog_x (void);
void syms_of_dired (void);
void syms_of_dired_mswindows (void);
void syms_of_doc (void);
void syms_of_dragdrop (void);
void syms_of_editfns (void);
EXTERN_C void syms_of_eldap (void);
void syms_of_elhash (void);
void syms_of_emacs (void);
void syms_of_eval (void);
void syms_of_event_Xt (void);
void syms_of_event_mswindows (void);
void syms_of_event_stream (void);
void syms_of_events (void);
void syms_of_extents (void);
void syms_of_faces (void);
void syms_of_file_coding (void);
void syms_of_fileio (void);
void syms_of_filelock (void);
void syms_of_floatfns (void);
void syms_of_fns (void);
#ifdef USE_C_FONT_LOCK
void syms_of_font_lock (void);
#endif /* USE_C_FONT_LOCK */
void syms_of_frame (void);
void syms_of_frame_mswindows (void);
void syms_of_frame_tty (void);
void syms_of_frame_x (void);
void syms_of_free_hook (void);
void syms_of_general (void);
void syms_of_glyphs (void);
void syms_of_glyphs_eimage (void);
void syms_of_glyphs_mswindows (void);
void syms_of_glyphs_shared (void);
void syms_of_glyphs_widget (void);
void syms_of_glyphs_x (void);
void syms_of_gpmevent (void);
void syms_of_gui (void);
void syms_of_gui_mswindows (void);
void syms_of_gui_x (void);
void syms_of_gutter (void);
void syms_of_indent (void);
void syms_of_input_method_xlib (void);
void syms_of_intl (void);
void syms_of_intl_win32 (void);
void syms_of_intl_x (void);
void syms_of_keymap (void);
void syms_of_lread (void);
void syms_of_macros (void);
void syms_of_marker (void);
#ifdef NEW_GC
void syms_of_mc_alloc (void);
#endif /* NEW_GC */
void syms_of_md5 (void);
void syms_of_menubar (void);
void syms_of_menubar_mswindows (void);
void syms_of_menubar_x (void);
void syms_of_minibuf (void);
void syms_of_module (void);
EXTERN_C void syms_of_canna_api (void);
void syms_of_mule_ccl (void);
void syms_of_mule_charset (void);
void syms_of_mule_coding (void);
void syms_of_mule_wnn (void);
void syms_of_nt (void);
void syms_of_number (void);
void syms_of_objects (void);
void syms_of_objects_mswindows (void);
void syms_of_objects_tty (void);
void syms_of_objects_x (void);
void syms_of_font_mgr (void);
EXTERN_C void syms_of_postgresql (void);
void syms_of_print (void);
void syms_of_process (void);
void syms_of_process_nt (void);
void syms_of_profile (void);
void syms_of_ralloc (void);
void syms_of_rangetab (void);
void syms_of_redisplay (void);
void syms_of_scrollbar (void);
void syms_of_scrollbar_mswindows(void);
void syms_of_search (void);
void syms_of_select (void);
void syms_of_select_mswindows (void);
void syms_of_select_x (void);
void syms_of_signal (void);
void syms_of_sound (void);
void syms_of_specifier (void);
void syms_of_sunpro (void);
void syms_of_symbols (void);
void syms_of_syntax (void);
void syms_of_tests (void);
void syms_of_text (void);
void syms_of_toolbar (void);
void syms_of_tooltalk (void);
void syms_of_undo (void);
void syms_of_unicode (void);
void syms_of_widget (void);
void syms_of_win32 (void);
void syms_of_window (void);

/* Initialize the console types (dump-time only for console_type_(),
   post-pdump-load-time only for reinit_). */

void console_type_create (void);
void console_type_create_stream (void);
void reinit_console_type_create_stream (void);
void console_type_create_tty (void);
void reinit_console_type_create_tty (void);
void console_type_create_device_tty (void);
void console_type_create_frame_tty (void);
void console_type_create_objects_tty (void);
void console_type_create_redisplay_tty (void);
void console_type_create_x (void);
void reinit_console_type_create_x (void);
void console_type_create_device_x (void);
void reinit_console_type_create_device_x (void);
void console_type_create_frame_x (void);
void console_type_create_glyphs_x (void);
void console_type_create_menubar_x (void);
void console_type_create_objects_x (void);
void console_type_create_redisplay_x (void);
void console_type_create_scrollbar_x (void);
void console_type_create_select_x (void);
void console_type_create_toolbar_x (void);
void console_type_create_dialog_x (void);
void console_type_create_mswindows (void);
void reinit_console_type_create_mswindows (void);
void console_type_create_device_mswindows (void);
void console_type_create_frame_mswindows (void);
void console_type_create_menubar_mswindows (void);
void console_type_create_objects_mswindows (void);
void console_type_create_redisplay_mswindows (void);
void console_type_create_scrollbar_mswindows (void);
void console_type_create_toolbar_mswindows (void);
void console_type_create_glyphs_mswindows (void);
void console_type_create_dialog_mswindows (void);
void console_type_create_select_mswindows (void);

/* Initialize the specifier types (dump-time only for specifier_type_(),
   post-pdump-load-time only for reinit_). */

void specifier_type_create (void);
void reinit_specifier_type_create (void);
void specifier_type_create_image (void);
void reinit_specifier_type_create_image (void);
void specifier_type_create_gutter (void);
void reinit_specifier_type_create_gutter (void);
void specifier_type_create_objects (void);
void reinit_specifier_type_create_objects (void);
void specifier_type_create_toolbar (void);
void reinit_specifier_type_create_toolbar (void);

/* Initialize the coding system types (dump-time only for
   coding_system_type_(), post-pdump-load-time only for reinit_). */

void coding_system_type_create (void);
void reinit_coding_system_type_create (void);
void coding_system_type_create_unicode (void);
void reinit_coding_system_type_create_unicode (void);
void coding_system_type_create_intl_win32 (void);
void reinit_coding_system_type_create_intl_win32 (void);
void coding_system_type_create_mule_coding (void);
void reinit_coding_system_type_create_mule_coding (void);

/* Initialize the structure types (dump-time only). */

void structure_type_create (void);
void structure_type_create_chartab (void);
void structure_type_create_faces (void);
void structure_type_create_rangetab (void);
void structure_type_create_hash_table (void);

/* Initialize the image instantiator types (dump-time only). */

void image_instantiator_format_create (void);
void image_instantiator_format_create_glyphs_eimage (void);
void image_instantiator_format_create_glyphs_widget (void);
void image_instantiator_format_create_glyphs_x (void);
void image_instantiator_format_create_glyphs_mswindows (void);
void image_instantiator_format_create_glyphs_tty (void);

/* Initialize the lstream types (dump-time only). */

void lstream_type_create (void);
void lstream_type_create_file_coding (void);
void lstream_type_create_print (void);
void lstream_type_create_mswindows_selectable (void);

/* Initialize process types */

void process_type_create_nt (void);
void process_type_create_unix (void);

/* Allow for Fprovide() (dump-time only). */

void init_provide_once (void);

/* Initialize most variables (dump-time for vars_, dump-time and
   post-pdump-load-time for reinit_vars). */

void vars_of_abbrev (void);
void vars_of_alloc (void);
void vars_of_balloon_x (void);
void vars_of_buffer (void);
void reinit_vars_of_buffer (void);
void vars_of_bytecode (void);
void vars_of_callint (void);
void vars_of_chartab (void);
void vars_of_cmdloop (void);
void vars_of_cmds (void);
void vars_of_console (void);
void reinit_vars_of_console (void);
void vars_of_console_stream (void);
void vars_of_console_mswindows (void);
void vars_of_console_tty (void);
void vars_of_console_x (void);
void vars_of_data (void);
void vars_of_database (void);
void vars_of_debug (void);
void reinit_vars_of_debug (void);
void vars_of_device (void);
void reinit_vars_of_device (void);
void vars_of_device_mswindows (void);
void vars_of_device_x (void);
void reinit_vars_of_device_x (void);
void vars_of_dialog (void);
void vars_of_dialog_x (void);
void vars_of_dialog_mswindows (void);
void vars_of_dired (void);
void vars_of_dired_mswindows (void);
void vars_of_doc (void);
void vars_of_dragdrop (void);
void vars_of_editfns (void);
void vars_of_emacs (void);
void vars_of_eval (void);
void reinit_vars_of_eval (void);
void vars_of_event_stream (void);
void reinit_vars_of_event_stream (void);
void vars_of_event_tty (void);
void reinit_vars_of_event_tty (void);
void vars_of_event_mswindows (void);
void reinit_vars_of_event_mswindows (void);
void vars_of_event_Xt (void);
void reinit_vars_of_event_Xt (void);
void vars_of_events (void);
void reinit_vars_of_events (void);
void vars_of_extents (void);
void reinit_vars_of_extents (void);
void vars_of_faces (void);
void vars_of_file_coding (void);
void reinit_vars_of_file_coding (void);
void vars_of_fileio (void);
void reinit_vars_of_fileio (void);
void vars_of_filelock (void);
void vars_of_floatfns (void);
void vars_of_fns (void);
#ifdef USE_C_FONT_LOCK
void vars_of_font_lock (void);
void reinit_vars_of_font_lock (void);
#endif /* USE_C_FONT_LOCK */
void vars_of_frame_tty (void);
void vars_of_frame_mswindows (void);
void reinit_vars_of_frame_mswindows (void);
void vars_of_frame_x (void);
void vars_of_frame (void);
void vars_of_glyphs_x (void);
void vars_of_glyphs_eimage (void);
void vars_of_glyphs_widget (void);
void reinit_vars_of_glyphs_widget (void);
void vars_of_glyphs_mswindows (void);
void vars_of_glyphs (void);
void reinit_vars_of_glyphs (void);
void vars_of_gui_x (void);
void reinit_vars_of_gui_x (void);
void vars_of_gui (void);
void vars_of_gutter (void);
void vars_of_input_method_motif (void);
void vars_of_input_method_xlib (void);
void vars_of_indent (void);
void vars_of_insdel (void);
void reinit_vars_of_insdel (void);
void vars_of_intl (void);
void vars_of_intl_win32 (void);
void vars_of_keymap (void);
void vars_of_lread (void);
void reinit_vars_of_lread (void);
void vars_of_lstream (void);
void reinit_vars_of_lstream (void);
void vars_of_macros (void);
void vars_of_md5 (void);
void vars_of_menubar_x (void);
void reinit_vars_of_menubar_x (void);
void vars_of_menubar (void);
void vars_of_menubar_mswindows (void);
void vars_of_minibuf (void);
void reinit_vars_of_minibuf (void);
void vars_of_module (void);
void reinit_vars_of_module (void);
EXTERN_C void vars_of_canna_api (void);
void vars_of_mule_ccl(void);
void vars_of_mule_charset (void);
void vars_of_mule_coding (void);
void reinit_vars_of_mule_coding (void);
void vars_of_mule_wnn (void);
void reinit_vars_of_mule_wnn (void);
void vars_of_nt (void);
void vars_of_number (void);
void vars_of_objects (void);
void vars_of_font_mgr (void);
void reinit_vars_of_font_mgr (void);
void reinit_vars_of_objects (void);
void vars_of_objects_tty (void);
void vars_of_objects_mswindows (void);
void reinit_vars_of_object_mswindows (void);
void vars_of_objects_x (void);
void vars_of_print (void);
void reinit_vars_of_print (void);
void vars_of_process (void);
void vars_of_process_nt (void);
void vars_of_process_unix (void);
void vars_of_profile (void);
void vars_of_ralloc (void);
void vars_of_realpath (void);
void vars_of_redisplay (void);
void vars_of_regex (void);
void vars_of_scrollbar_x (void);
void reinit_vars_of_scrollbar_x (void);
void vars_of_scrollbar (void);
void vars_of_scrollbar_mswindows (void);
void vars_of_search (void);
void reinit_vars_of_search (void);
void vars_of_select (void);
void vars_of_select_mswindows (void);
void vars_of_sound (void);
void vars_of_specifier (void);
void vars_of_sunpro (void);
void vars_of_symbols (void);
void vars_of_syntax (void);
void vars_of_tests (void);
void vars_of_text (void);
void reinit_vars_of_text (void);
void vars_of_toolbar (void);
void vars_of_tooltalk (void);
void vars_of_undo (void);
void reinit_vars_of_undo (void);
void vars_of_unicode (void);
void vars_of_window (void);
void reinit_vars_of_window (void);
void vars_of_win32 (void);
void vars_of_select_x (void);
void reinit_vars_of_select_x (void);
EXTERN_C void vars_of_eldap (void);
EXTERN_C void vars_of_postgresql (void);
void vars_of_gpmevent (void);

/* Initialize specifier variables (dump-time only). */

void specifier_vars_of_glyphs (void);
void specifier_vars_of_glyphs_widget (void);
void specifier_vars_of_gutter (void);
void specifier_vars_of_menubar (void);
void specifier_vars_of_redisplay (void);
void specifier_vars_of_scrollbar (void);
void specifier_vars_of_toolbar (void);
void specifier_vars_of_window (void);

/* Initialize variables with complex dependencies on other variables
   (dump-time for complex_vars_, dump-time and post-pdump-load-time
   for reinit_(), pdump-load-time-only for reinit_..._runtime_only()).
   #### The reinit_() functions should be called from emacs.c, not the
   corresponding complex_vars_of_(). */

void complex_vars_of_faces (void);
void complex_vars_of_mule_charset (void);
void complex_vars_of_file_coding (void);
void complex_vars_of_intl_win32 (void);
void complex_vars_of_glyphs (void);
void complex_vars_of_glyphs_x (void);
void complex_vars_of_glyphs_mswindows (void);
void complex_vars_of_alloc (void);
void complex_vars_of_menubar (void);
void complex_vars_of_scrollbar (void);
void complex_vars_of_frame (void);
void complex_vars_of_syntax (void);
void complex_vars_of_casetab (void);
void complex_vars_of_chartab (void);
void complex_vars_of_buffer (void);
void reinit_complex_vars_of_buffer_runtime_only (void);
void complex_vars_of_console (void);
void reinit_complex_vars_of_console_runtime_only (void);
void complex_vars_of_emacs (void);
void complex_vars_of_minibuf (void);
void reinit_complex_vars_of_minibuf (void);
void complex_vars_of_keymap (void);
void complex_vars_of_font_mgr (void);

/* Late initialization -- stuff pertaining only to interactive usage,
   I/O, or Lisp reading. (Dump-time and run-time, but the code itself
   may conditionalize on this by checking the `initialized' variable.) */

void init_buffer_1 (void);
void init_buffer_2 (void);
void init_console_stream (int reinit);
void init_device_tty (void);
void init_editfns (void);
void init_event_Xt_late (void);
void init_event_mswindows_late (void);
void init_event_stream (void);
void init_event_tty_late (void);
void init_event_unixoid (void);
void init_file_coding (void);
void init_hpplay (void);
void init_intl (void);
void init_intl_win32 (void);
void init_lread (void);
void init_macros (void);
void init_mswindows_environment (void);
void init_nt (void);
void init_postgresql_from_environment (void);
void init_redisplay (void);
void init_select_mswindows (void);
void init_sunpro (void);
void init_win32 (void);
void init_xemacs_process (void);

void syms_of_device_gtk (void);
void syms_of_dialog_gtk (void);
void syms_of_event_gtk (void);
void syms_of_frame_gtk (void);
void syms_of_glyphs_gtk (void);
void syms_of_gui_gtk (void);
void syms_of_menubar_gtk (void);
void syms_of_objects_gtk (void);
void syms_of_select_gtk (void);
void syms_of_ui_gtk (void);
void syms_of_widget_accessors (void);
void syms_of_ui_byhand (void);
void console_type_create_gtk (void);
void reinit_console_type_create_gtk (void);
void console_type_create_device_gtk (void);
void console_type_create_frame_gtk (void);
void console_type_create_glyphs_gtk (void);
void console_type_create_menubar_gtk (void);
void console_type_create_objects_gtk (void);
void console_type_create_redisplay_gtk (void);
void console_type_create_scrollbar_gtk (void);
void console_type_create_toolbar_gtk (void);
void console_type_create_dialog_gtk (void);
void image_instantiator_format_create_glyphs_gtk (void);
void vars_of_device_gtk (void);
void vars_of_dialog_gtk (void);
void vars_of_event_gtk (void);
void reinit_vars_of_event_gtk (void);
void vars_of_frame_gtk (void);
void vars_of_glyphs_gtk (void);
void vars_of_gui_gtk (void);
void vars_of_menubar_gtk (void);
void reinit_vars_of_menubar_gtk (void);
void vars_of_objects_gtk (void);
void vars_of_scrollbar_gtk (void);
void vars_of_select_gtk (void);
void vars_of_ui_gtk (void);
void complex_vars_of_glyphs_gtk (void);
void init_event_gtk_late (void);
void console_type_create_select_gtk (void);

/* Enhanced number initialization: must be done only at runtime due to complex
   interactions with the supporting libraries. */
void init_number (void);

#endif /* INCLUDED_symsinit_h_ */
