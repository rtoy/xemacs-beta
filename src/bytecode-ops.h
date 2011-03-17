/* Execution of byte code produced by bytecomp.el.
   Implementation of compiled-function objects.
   Copyright (C) 1992, 1993 Free Software Foundation, Inc.
   Copyright (C) 1995, 2002, 2010 Ben Wing.

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

/* Synched up with: Mule 2.0, FSF 19.30. */

/* There is more than one place in bytecode.c that may want to do something
   with the list of all the opcodes.  To handle this, we extract them into
   a separate file that can get included after defining OPCODE(sym, val)
   appropriately.  No need to undefine OPCODE; that happens automatically.
*/

  OPCODE (varref,  			010)
  OPCODE (varset,  			020)
  OPCODE (varbind, 			030)
  OPCODE (call,    			040)
  OPCODE (unbind,  			050)

  OPCODE (nth,     			070)
  OPCODE (symbolp, 			071)
  OPCODE (consp,   			072)
  OPCODE (stringp, 			073)
  OPCODE (listp,   			074)
  OPCODE (old_eq,  			075)
  OPCODE (old_memq, 			076)
  OPCODE (not,    			077)
  OPCODE (car,    			0100)
  OPCODE (cdr, 	  			0101)
  OPCODE (cons,   			0102)
  OPCODE (list1,  			0103)
  OPCODE (list2,  			0104)
  OPCODE (list3,  			0105)
  OPCODE (list4,  			0106)
  OPCODE (length, 			0107)
  OPCODE (aref,   			0110)
  OPCODE (aset,   			0111)
  OPCODE (symbol_value, 		0112)
  OPCODE (symbol_function, 		0113)
  OPCODE (set,    			0114)
  OPCODE (fset,   			0115)
  OPCODE (get,    			0116)
  OPCODE (subseq, 			0117)
  OPCODE (concat2, 			0120)
  OPCODE (concat3, 			0121)
  OPCODE (concat4, 			0122)
  OPCODE (sub1, 			0123)
  OPCODE (add1, 			0124)
  OPCODE (eqlsign, 			0125)
  OPCODE (gtr, 				0126)
  OPCODE (lss, 				0127)
  OPCODE (leq, 				0130)
  OPCODE (geq, 				0131)
  OPCODE (diff, 			0132)
  OPCODE (negate, 			0133)
  OPCODE (plus, 			0134)
  OPCODE (max, 				0135)
  OPCODE (min, 				0136)
  OPCODE (mult, 			0137)

  OPCODE (point, 			0140)
  OPCODE (eq, 				0141) /* was Bmark, but no longer
						 generated as of v18 */
  OPCODE (goto_char, 			0142)
  OPCODE (insert, 			0143)
  OPCODE (point_max, 			0144)
  OPCODE (point_min, 			0145)
  OPCODE (char_after, 			0146)
  OPCODE (following_char, 		0147)
  OPCODE (preceding_char, 		0150)
  OPCODE (current_column, 		0151)
  OPCODE (indent_to, 			0152)
  OPCODE (equal, 			0153) /* was Bscan_buffer, but no
						 longer generated as of
						 v18 */
  OPCODE (eolp, 			0154)
  OPCODE (eobp, 			0155)
  OPCODE (bolp, 			0156)
  OPCODE (bobp, 			0157)
  OPCODE (current_buffer, 		0160)
  OPCODE (set_buffer, 			0161)
  OPCODE (save_current_buffer, 		0162) /* was Bread_char, but no
						 longer generated as of
						 v19 */
  OPCODE (memq, 			0163) /* was Bset_mark, but no
						 longer generated as of
						 v18 */
  OPCODE (interactive_p, 		0164) /* Needed since interactive-p
						 takes unevalled args */
  OPCODE (forward_char, 		0165)
  OPCODE (forward_word, 		0166)
  OPCODE (skip_chars_forward, 		0167)
  OPCODE (skip_chars_backward, 		0170)
  OPCODE (forward_line, 		0171)
  OPCODE (char_syntax, 			0172)
  OPCODE (buffer_substring, 		0173)
  OPCODE (delete_region, 		0174)
  OPCODE (narrow_to_region, 		0175)
  OPCODE (widen, 			0176)
  OPCODE (end_of_line, 			0177)

  OPCODE (constant2, 			0201)
  OPCODE (goto, 			0202)
  OPCODE (gotoifnil, 			0203)
  OPCODE (gotoifnonnil, 		0204)
  OPCODE (gotoifnilelsepop, 		0205)
  OPCODE (gotoifnonnilelsepop, 		0206)
  OPCODE (return, 			0207)
  OPCODE (discard, 			0210)
  OPCODE (dup, 				0211)

  OPCODE (save_excursion, 		0212)
  OPCODE (save_window_excursion,	0213)
  OPCODE (save_restriction, 		0214)
  OPCODE (catch, 			0215)

  OPCODE (unwind_protect, 		0216)
  OPCODE (condition_case, 		0217)
  OPCODE (temp_output_buffer_setup, 	0220)
  OPCODE (temp_output_buffer_show,  	0221)

  OPCODE (unbind_all,			0222)

  OPCODE (set_marker,			0223)
  OPCODE (match_beginning,		0224)
  OPCODE (match_end,			0225)
  OPCODE (upcase,			0226)
  OPCODE (downcase,			0227)

  OPCODE (string_equal, 		0230)
  OPCODE (string_lessp,     		0231)
  OPCODE (old_equal, 	 		0232)
  OPCODE (nthcdr, 	 		0233)
  OPCODE (elt, 		 		0234)
  OPCODE (old_member, 	 		0235)
  OPCODE (old_assq, 	 		0236)
  OPCODE (nreverse, 	 		0237)
  OPCODE (setcar, 	 		0240)
  OPCODE (setcdr, 	 		0241)
  OPCODE (car_safe, 	 		0242)
  OPCODE (cdr_safe, 	 		0243)
  OPCODE (nconc, 	 		0244)
  OPCODE (quo, 		 		0245)
  OPCODE (rem, 		 		0246)
  OPCODE (numberp, 	 		0247)
  OPCODE (fixnump, 	 		0250) /* Was Bintegerp. */

  OPCODE (Rgoto, 			0252)
  OPCODE (Rgotoifnil, 			0253)
  OPCODE (Rgotoifnonnil, 		0254)
  OPCODE (Rgotoifnilelsepop, 		0255)
  OPCODE (Rgotoifnonnilelsepop, 	0256)

  OPCODE (listN, 			0257)
  OPCODE (concatN, 			0260)
  OPCODE (insertN, 			0261)

  OPCODE (bind_multiple_value_limits,   0262) /* New in 21.5. */
  OPCODE (multiple_value_list_internal, 0263) /* New in 21.5. */
  OPCODE (multiple_value_call,          0264) /* New in 21.5. */
  OPCODE (throw,                        0265) /* New in 21.5. */

  OPCODE (member, 			0266) /* new in v20 */
  OPCODE (assq, 			0267) /* new in v20 */

  OPCODE (constant,			0300)

#undef OPCODE
