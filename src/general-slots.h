/* Commonly-used symbols -- include file
   Copyright (C) 1995 Sun Microsystems.
   Copyright (C) 1995, 1996, 2000, 2001, 2002, 2003, 2010 Ben Wing.

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

/* The purpose of this file is as a central place to stick symbols
   that don't have any obvious connection to any particular module
   and might be used in many different contexts.

   Four types of declarations are allowed here:

   SYMBOL (Qfoo); declares a symbol "foo"
   SYMBOL_MODULE_API (Qfoo); declares a symbol "foo" that is visible to modules
   SYMBOL_KEYWORD (Q_foo); declares a keyword symbol ":foo"
   SYMBOL_GENERAL (Qfoo, "bar"); declares a symbol named "bar" but stored in
     the variable Qfoo

To sort the crap in this file, use the following:

(sort-regexp-fields nil
		    "^.*(Q_?\\(.*\\));$" "\\1"
		    (progn
		      (search-forward "SYMBOL")
		      (match-beginning 0))
		    (point-max))
*/

SYMBOL (Qabort);
SYMBOL_KEYWORD (Q_accelerator);
SYMBOL_KEYWORD (Q_active);
SYMBOL (Qafter);
SYMBOL (Qall);
SYMBOL_KEYWORD (Q_allow_other_keys);
SYMBOL (Qand);
SYMBOL (Qappend);
SYMBOL (Qascii);
SYMBOL (Qassoc);
SYMBOL (Qat);
SYMBOL (Qautodetect);
SYMBOL (Qbad_variable);
SYMBOL (Qbefore);
SYMBOL (Qbigfloat);
SYMBOL (Qbinary);
SYMBOL (Qbitmap);
SYMBOL (Qboolean);
SYMBOL_KEYWORD (Q_border);
SYMBOL (Qbottom);
SYMBOL (Qbottom_margin);
SYMBOL (Qbuffer);
SYMBOL (Qbuffers);
SYMBOL (Qbuilt_in);
SYMBOL (Qbutton);
SYMBOL_KEYWORD (Q_buttons);
SYMBOL_KEYWORD (Q_callback);
SYMBOL_KEYWORD (Q_callback_ex);
SYMBOL (Qcancel);
SYMBOL (Qcar);
SYMBOL (Qcategory);
SYMBOL (Qccl_program);
SYMBOL (Qcenter);
SYMBOL (Qchain);
SYMBOL (Qchange);
SYMBOL (Qchannel);
SYMBOL (Qchar);
SYMBOL (Qcharacter);
SYMBOL (Qchars);
SYMBOL (Qcode_page);
SYMBOL (Qcoding_system);
SYMBOL (Qcoerce);
SYMBOL (Qcolor);
SYMBOL (Qcolumns);
SYMBOL (Qcommand);
SYMBOL_KEYWORD (Q_config);
SYMBOL (Qconsole);
SYMBOL (Qcontrol_1);
SYMBOL (Qcopies);
SYMBOL_MODULE_API (Qcritical);
SYMBOL (Qctext);
SYMBOL (Qcurrent);
SYMBOL (Qcursor);
SYMBOL (Qdata);
SYMBOL (Qdde);
SYMBOL (Qdead);
SYMBOL (Qdebug);
SYMBOL (Qdefault);
SYMBOL_MODULE_API (Qdelete);
SYMBOL (Qdelq);
SYMBOL (Qdescription);
SYMBOL_KEYWORD (Q_descriptor);
SYMBOL (Qdevice);
SYMBOL (Qdevices);
SYMBOL_KEYWORD (Q_device);
SYMBOL (Qdialog);
SYMBOL (Qdirectory);
SYMBOL (Qdimension);
SYMBOL (Qdisplay);
SYMBOL (Qdoc_string);
SYMBOL (Qdocumentation);
SYMBOL (Qduplex);
SYMBOL (Qemergency);
SYMBOL (Qempty);
SYMBOL_KEYWORD (Q_end);
SYMBOL (Qencode_as_utf_8);
SYMBOL (Qeval);
SYMBOL (Qevent);
SYMBOL (Qextents);
SYMBOL (Qexternal);
SYMBOL (Qface);
SYMBOL (Qfaces);
SYMBOL (Qfallback);
SYMBOL (Qfile);
SYMBOL_MODULE_API (Qfile_name);
SYMBOL_KEYWORD (Q_filter);
SYMBOL (Qfinal);
SYMBOL (Qfixnum);
SYMBOL_MODULE_API (Qfixnump);
SYMBOL (Qfloat);
SYMBOL (Qfont);
SYMBOL (Qframe);
SYMBOL (Qframes);
SYMBOL (Qfrom_page);
SYMBOL (Qfrom_unicode);
SYMBOL (Qfull_assoc);
SYMBOL (Qfuncall);
SYMBOL (Qfunction);
SYMBOL (Qgarbage_collection);
SYMBOL (Qgeneric);
SYMBOL (Qgeometry);
SYMBOL (Qglobal);
SYMBOL (Qglyph);
SYMBOL (Qgtk);
SYMBOL (Qgui_item);
SYMBOL (Qgutter);
SYMBOL (Qheight);
SYMBOL_KEYWORD (Q_height);
SYMBOL (Qhelp);
SYMBOL (Qhigh);
SYMBOL (Qhighlight);
SYMBOL (Qhorizontal);
SYMBOL (Qicon);
SYMBOL (Qid);
SYMBOL (Qignore);
SYMBOL (Qinitial);
SYMBOL (Qimage);
SYMBOL_KEYWORD (Q_image);
SYMBOL_KEYWORD (Q_included);
SYMBOL (Qinfo);
SYMBOL (Qinherit);
SYMBOL_KEYWORD (Q_initial_focus);
SYMBOL (Qinteger);
SYMBOL (Qinternal);
SYMBOL_KEYWORD (Q_items);
SYMBOL_KEYWORD (Q_justify);
SYMBOL_KEYWORD (Q_vertically_justify);
SYMBOL_KEYWORD (Q_horizontally_justify);
SYMBOL_KEYWORD (Q_key);
SYMBOL (Qkey);
SYMBOL (Qkey_assoc);
SYMBOL (Qkey_mapping);
SYMBOL_KEYWORD (Q_key_sequence);
SYMBOL (Qkeyboard);
SYMBOL (Qkeymap);
SYMBOL_KEYWORD (Q_keys);
SYMBOL_KEYWORD (Q_label);
SYMBOL (Qlandscape);
SYMBOL (Qlast_command);
SYMBOL (Qleft);
SYMBOL (Qleft_margin);
SYMBOL (Qlet);
SYMBOL (Qlevel);
SYMBOL (Qlist);
SYMBOL (Qlittle_endian);
SYMBOL (Qlocale);
SYMBOL (Qlow);
SYMBOL_GENERAL (Qlss, "<");
SYMBOL (Qmagic);
SYMBOL_KEYWORD (Q_margin_width);
SYMBOL (Qmarkers);
SYMBOL (Qmax);
SYMBOL (Qmemory);
SYMBOL (Qmenubar);
SYMBOL (Qmessage);
SYMBOL_GENERAL (Qminus, "-");
SYMBOL (Qmodifiers);
SYMBOL (Qmotion);
SYMBOL (Qmsprinter);
SYMBOL (Qmswindows);
SYMBOL (Qname);
SYMBOL_MODULE_API (Qnative);
SYMBOL (Qnatnum);
SYMBOL (Qno);
SYMBOL (Qnone);
SYMBOL (Qnot);
SYMBOL (Qnothing);
SYMBOL_MODULE_API (Qnotice);
SYMBOL (Qobject);
SYMBOL (Qok);
SYMBOL (Qold_assoc);
SYMBOL (Qold_delete);
SYMBOL (Qold_delq);
SYMBOL (Qold_rassoc);
SYMBOL (Qold_rassq);
SYMBOL (Qonly);
SYMBOL (Qor);
SYMBOL (Qorientation);
SYMBOL_KEYWORD (Q_orientation);
SYMBOL (Qother);
SYMBOL (Qpage_setup);
SYMBOL (Qpages);
SYMBOL (Qpeer);
SYMBOL (Qpointer);
SYMBOL (Qpopup);
SYMBOL (Qportrait);
SYMBOL (Qprepend);
SYMBOL (Qprint);
SYMBOL (Qprinter);
SYMBOL_KEYWORD (Q_printer_settings);
SYMBOL (Qprocess);
SYMBOL_KEYWORD (Q_properties);
SYMBOL (Qprovide);
SYMBOL (Qquery_coding_clear_highlights);
SYMBOL (Qquery_coding_warning_face);
SYMBOL (Qquestion);
SYMBOL_KEYWORD (Q_question);
SYMBOL (Qquote);
SYMBOL (Qradio);
SYMBOL (Qrassoc);
SYMBOL (Qrassq);
SYMBOL (Qratio);
SYMBOL (Qredisplay);
SYMBOL (Qremove_all);
SYMBOL (Qrequire);
SYMBOL (Qresource);
SYMBOL (Qretry);
SYMBOL (Qreturn);
SYMBOL (Qreverse);
SYMBOL (Qright);
SYMBOL (Qright_margin);
SYMBOL_MODULE_API (Qsearch);
SYMBOL (Qselected);
SYMBOL_KEYWORD (Q_selected);
SYMBOL (Qselection);
SYMBOL (Qset_glyph_image);
SYMBOL (Qsignal);
SYMBOL_MODULE_API (Qsimple);
SYMBOL (Qsize);
SYMBOL (Qsound);
SYMBOL (Qspace);
SYMBOL (Qspecifier);
SYMBOL (Qstandard);
SYMBOL_KEYWORD (Q_start);
SYMBOL (Qstream);
SYMBOL (Qstring);
SYMBOL_KEYWORD (Q_style);
SYMBOL_KEYWORD (Q_suffix);
SYMBOL (Qsubtype);
SYMBOL (Qsucceeded);
SYMBOL (Qsymbol);
SYMBOL (Qsyntax);
SYMBOL (Qsystem_default);
SYMBOL (Qterminal);
SYMBOL (Qtest);
SYMBOL (Qtext);
SYMBOL_KEYWORD (Q_text);
SYMBOL (Qthis_command);
SYMBOL (Qtimeout);
SYMBOL (Qtimestamp);
SYMBOL_KEYWORD (Q_title);
SYMBOL (Qto_page);
SYMBOL (Qtoggle);
SYMBOL (Qtoolbar);
SYMBOL (Qtop);
SYMBOL (Qtop_margin);
SYMBOL (Qtty);
SYMBOL (Qtype);
SYMBOL (Qundecided);
SYMBOL (Qundefined);
SYMBOL (Qunencodable);
SYMBOL (Qinvalid_sequence);
SYMBOL (Qunimplemented);
SYMBOL (Qunicode_registries);
SYMBOL (Qunicode_type);
SYMBOL (Quser_default);
SYMBOL_KEYWORD (Q_value);
SYMBOL (Qvalue_assoc);
SYMBOL (Qvertical);
SYMBOL (Qwarning);
SYMBOL (Qwidget);
SYMBOL (Qwidth);
SYMBOL_KEYWORD (Q_width);
SYMBOL (Qwindow);
SYMBOL (Qwindows);
SYMBOL (Qwindow_id);
SYMBOL (Qwindow_system);
SYMBOL (Qx);
SYMBOL (Qy);
SYMBOL (Qyes);
