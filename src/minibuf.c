/* Minibuffer input and completion.
   Copyright (C) 1985, 1986, 1992-1995 Free Software Foundation, Inc.
   Copyright (C) 1995 Sun Microsystems, Inc.
   Copyright (C) 2002, 2010 Ben Wing.

This file is part of XEmacs.

XEmacs is free software: you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation, either version 3 of the License, or (at your
option) any later version.

XEmacs is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with XEmacs.  If not, see <http://www.gnu.org/licenses/>. */

/* Synched up with: Mule 2.0, FSF 19.28.  Mule-ized except as noted.
   Substantially different from FSF. */

/* #### dmoore - All sorts of things in here can call lisp, like message.
   Track all this stuff. */

#include <config.h>
#include "lisp.h"

#include "buffer.h"
#include "commands.h"
#include "console-stream.h"
#include "events.h"
#include "frame-impl.h"
#include "insdel.h"
#include "redisplay.h"
#include "window-impl.h"
#include "elhash.h"

/* Depth in minibuffer invocations.  */
int minibuf_level;

Lisp_Object Qcompletion_ignore_case;

/* Nonzero means completion ignores case.  */
int completion_ignore_case;

/* List of regexps that should restrict possible completions.  */
Lisp_Object Vcompletion_regexp_list;

/* The echo area buffer. */
Lisp_Object Vecho_area_buffer;

/* Prompt to display in front of the minibuffer contents */
Lisp_Object Vminibuf_prompt;

/* Added on 97/3/14 by Jareth Hein (jhod@po.iijnet.or.jp) for input system support */
/* String to be displayed in front of prompt of the minibuffer contents */
Lisp_Object Vminibuf_preprompt;

/* Hook to run just after entry to minibuffer. */
Lisp_Object Qminibuffer_setup_hook, Vminibuffer_setup_hook;

Lisp_Object Qappend_message, Qcurrent_message_label,
            Qclear_message, Qdisplay_message;


DEFUN ("minibuffer-depth", Fminibuffer_depth, 0, 0, 0, /*
Return current depth of activations of minibuffer, a nonnegative integer.
*/
       ())
{
  return make_fixnum (minibuf_level);
}

/* The default buffer to use as the window-buffer of minibuffer windows */
/*  Note there is special code in kill-buffer to make this unkillable */
Lisp_Object Vminibuffer_zero;


/* Actual minibuffer invocation. */

static Lisp_Object
read_minibuffer_internal_unwind (Lisp_Object unwind_data)
{
  Lisp_Object frame;
  XWINDOW (minibuf_window)->last_modified[CURRENT_DISP] = Qzero;
  XWINDOW (minibuf_window)->last_modified[DESIRED_DISP] = Qzero;
  XWINDOW (minibuf_window)->last_modified[CMOTION_DISP] = Qzero;
  XWINDOW (minibuf_window)->last_facechange[CURRENT_DISP] = Qzero;
  XWINDOW (minibuf_window)->last_facechange[DESIRED_DISP] = Qzero;
  XWINDOW (minibuf_window)->last_facechange[CMOTION_DISP] = Qzero;
  Vminibuf_prompt = Felt (unwind_data, Qzero);
  minibuf_level = XFIXNUM (Felt (unwind_data, make_fixnum (1)));
  while (CONSP (unwind_data))
    {
      Lisp_Object victim = unwind_data;
      unwind_data = XCDR (unwind_data);
      free_cons (victim);
    }

  /* If cursor is on the minibuffer line,
     show the user we have exited by putting it in column 0.  */
  frame = Fselected_frame (Qnil);
  if (!noninteractive
      && !NILP (frame)
      && !NILP (XFRAME (frame)->minibuffer_window))
    {
      struct window *w = XWINDOW (XFRAME (frame)->minibuffer_window);
      redisplay_move_cursor (w, 0, 0);
    }

  return Qnil;
}

/* 97/4/13 jhod: Added for input methods */
DEFUN ("set-minibuffer-preprompt", Fset_minibuffer_preprompt, 1, 1, 0, /*
Set the minibuffer preprompt string to PREPROMPT. This is used by language
input methods to relay state information to the user.
*/
       (preprompt))
{
  if (NILP (preprompt))
    {
      Vminibuf_preprompt = Qnil;
    }
  else
    {
      CHECK_STRING (preprompt);

      Vminibuf_preprompt = LISP_GETTEXT (preprompt);
    }
  return Qnil;
}

DEFUN_NORETURN ("read-minibuffer-internal", Fread_minibuffer_internal,
		1, 1, 0, /*
Lowest-level interface to minibuffers.  Don't call this.
*/
       (prompt))
{
  /* This function can GC */

  /* We used to record the specpdl_depth here, but since call_command_loop
     never returns, we can never call unbind_to_1.  Since we exit via a throw,
     we let whoever catches unbind for us. */

  CHECK_STRING (prompt);

  single_console_state ();

  record_unwind_protect (read_minibuffer_internal_unwind,
                         noseeum_cons
			 (Vminibuf_prompt,
			  noseeum_cons (make_fixnum (minibuf_level), Qnil)));
  Vminibuf_prompt = LISP_GETTEXT (prompt);

  /* NOTE: Here (or somewhere around here), in FSFmacs 19.30,
     choose_minibuf_frame() is called.  This is the only
     place in FSFmacs that it's called any more -- there's
     also a call in xterm.c, but commented out, and 19.28
     had the calls in different places.

     choose_minibuf_frame() does the following:

  if (!EQ (minibuf_window, selected_frame()->minibuffer_window))
    {
      Fset_window_buffer (selected_frame()->minibuffer_window,
			  XWINDOW (minibuf_window)->buffer);
      minibuf_window = selected_frame()->minibuffer_window;
    }

  #### Note that we don't do the set-window-buffer.  This call is
  similar, but not identical, to a set-window-buffer call made
  in `read-from-minibuffer' in minibuf.el.  I hope it's close
  enough, because minibuf_window isn't really exported to Lisp.

  The comment above choose_minibuf_frame() reads:

  Put minibuf on currently selected frame's minibuffer.
  We do this whenever the user starts a new minibuffer
  or when a minibuffer exits.  */

  minibuf_window = FRAME_MINIBUF_WINDOW (selected_frame ());

  run_hook (Qminibuffer_setup_hook);

  minibuf_level++;
  clear_echo_area (selected_frame (), Qnil, 0);

  call_command_loop (Qt);

  RETURN_NOT_REACHED (Qnil);
}



/* Completion hair */

/* Compare exactly LEN chars of strings at S1 and S2,
   ignoring case if appropriate.
   Return -1 if strings match,
   else number of chars that match at the beginning.  */

/* Note that this function works in Charcounts, unlike most functions.
   This is necessary for many reasons, one of which is that two
   strings may match even if they have different numbers of bytes,
   if IGNORE_CASE is true. */

Charcount
scmp_1 (const Ibyte *s1, const Ibyte *s2, Charcount len,
	int ignore_case)
{
  Charcount l = len;

  if (ignore_case)
    {
      while (l)
        {
          Ichar c1 = CANONCASE (0, itext_ichar (s1));
          Ichar c2 = CANONCASE (0, itext_ichar (s2));

          if (c1 == c2)
            {
              l--;
              INC_IBYTEPTR (s1);
              INC_IBYTEPTR (s2);
            }
          else
            break;
        }
    }
  else
    {
      while (l && itext_ichar (s1) == itext_ichar (s2))
	{
	  l--;
	  INC_IBYTEPTR (s1);
	  INC_IBYTEPTR (s2);
	}
    }

  if (l == 0)
    return -1;
  else return len - l;
}

/* Map FUNCTION, a C function, across LISZT, a pseudo-alist, calling
   it with three args, ELTSTRING (the car of the element if a cons,
   otherwise the element itself), ELT (the element, always) and
   EXTRA_ARG. Stop if FUNCTION returns non-zero. */
static void
map_completion_list (maphash_function_t function, Lisp_Object liszt,
                     void *extra_arg)
{
  Lisp_Object eltstring;

  GC_EXTERNAL_LIST_LOOP_2 (elt, liszt)
    {
      eltstring = CONSP (elt) ? XCAR (elt) : elt;
      if (function (eltstring, elt, extra_arg))
        {
          XUNGCPRO (elt);
          return;
        }
    }
  END_GC_EXTERNAL_LIST_LOOP (elt);
}

static void
map_completion (maphash_function_t function, Lisp_Object collection,
                void *extra_arg, Lisp_Object predicate)
{
  if (LISTP (collection))
    {
      map_completion_list (function, collection, extra_arg);
    }
  else if (VECTORP (collection))
    {
      map_obarray (collection, function, extra_arg);
    }
  else if (NILP (predicate))
    {
      /* This can't call Lisp, no need to copy and compress the hash
         table entries. */
      elisp_maphash_unsafe (function, collection, extra_arg);
    }
  else
    {
      elisp_maphash (function, collection, extra_arg);
    }
}

int
regexp_ignore_completion_p (const Ibyte *nonreloc,
			    Lisp_Object reloc, Bytecount offset,
			    Bytecount length)
{
  /* Ignore this element if it fails to match all the regexps.  */
  if (!NILP (Vcompletion_regexp_list))
    {
      EXTERNAL_LIST_LOOP_2 (re, Vcompletion_regexp_list)
	{
	  CHECK_STRING (re);
	  if (fast_string_match (re, nonreloc, reloc, offset,
				 length, 0, ERROR_ME, 0) < 0)
	    return 1;
	}
    }
  return 0;
}

/* Callers should GCPRO, since this may call eval */
static int
ignore_completion_p (Lisp_Object completion_string,
                     Lisp_Object pred, Lisp_Object completion,
                     Boolint hash_tablep)
{
  Lisp_Object tem;

  if (regexp_ignore_completion_p (0, completion_string, 0, -1))
    return 1;

  if (NILP (pred))
    {
      return 0;
    }

  /* Ignore this element if there is a predicate and the predicate doesn't
     like it. */
  if (hash_tablep)
    {
      tem = call2 (pred, completion_string, completion);
    }
  else if (EQ (pred, Qcommandp))
    {
      tem = Fcommandp (completion);
    }
  else
    {
      tem = call1 (pred, completion);
    }

  return NILP (tem);
}

struct try_completion_closure 
{
  Lisp_Object string;
  Charcount slength;
  Lisp_Object predicate;
  Lisp_Object bestmatch;
  Charcount blength;
  Charcount bestmatchsize;
  Boolint hash_tablep;
  int matchcount;
};

static int
try_completion_mapper (Lisp_Object eltstring, Lisp_Object value,
                       void *arg)
{
  struct try_completion_closure *tcc = (struct try_completion_closure *) arg;
  Charcount eltlength;

  if (SYMBOLP (eltstring))
    {
      eltstring = XSYMBOL_NAME (eltstring);
    }

  if (!STRINGP (eltstring))
    {
      return 0;
    }

  /* Is this element a possible completion? */
  eltlength = string_char_length (eltstring);
  if (tcc->slength <= eltlength
      && (0 > scmp (XSTRING_DATA (eltstring), XSTRING_DATA (tcc->string),
                    tcc->slength)))
    {
      struct gcpro gcpro1, gcpro2, gcpro3;
      int loser;
      GCPRO3 (tcc->string, eltstring, tcc->bestmatch);
      loser = ignore_completion_p (eltstring, tcc->predicate, value,
                                   tcc->hash_tablep);
      UNGCPRO;
      if (loser)      /* reject this one */
        {
          return 0;
        }

      /* Update computation of how much all possible completions
         match */

      tcc->matchcount++;
      if (NILP (tcc->bestmatch))
        {
          tcc->bestmatch = eltstring;
          tcc->blength = eltlength;
          tcc->bestmatchsize = eltlength;
        }
      else
        {
          Charcount compare = min (tcc->bestmatchsize, eltlength);
          Charcount matchsize =
            scmp (XSTRING_DATA (tcc->bestmatch), XSTRING_DATA (eltstring),
                  compare);
          if (matchsize < 0)
            matchsize = compare;
          if (completion_ignore_case)
            {
              /* If this is an exact match except for case, use it as
                 the best match rather than one that is not an exact
                 match.  This way, we get the case pattern of the
                 actual match.  */
              if ((matchsize == eltlength
                   && matchsize < tcc->blength)
                  ||
                  /* If there is more than one exact match ignoring
                     case, and one of them is exact including case,
                     prefer that one.  */
                  /* If there is no exact match ignoring case,
                     prefer a match that does not change the case of
                     the input.  */
                  ((matchsize == eltlength)
                   ==
                   (matchsize == tcc->blength)
                   && 0 > scmp_1 (XSTRING_DATA (eltstring),
                                  XSTRING_DATA (tcc->string),
                                  tcc->slength, 0)
                   && 0 <= scmp_1 (XSTRING_DATA (tcc->bestmatch),
                                   XSTRING_DATA (tcc->string),
                                   tcc->slength, 0)))
                {
                  tcc->bestmatch = eltstring;
                  tcc->blength = eltlength;
                }
            }
          tcc->bestmatchsize = matchsize;
        }
    }

  return 0;
}

DEFUN ("try-completion", Ftry_completion, 2, 3, 0, /*
Return common substring of all completions of STRING in COLLECTION.
COLLECTION must be a list, a hash table, an obarray, or a function.

Each string (or symbol) in COLLECTION is tested to see if it (or its
name) begins with STRING.  All that match are compared together; the
longest initial sequence common to all matches is returned as a
string.  If there is no match at all, nil is returned.  For an exact
match, t is returned.

If COLLECTION is a list, the elements of the list that are not cons
cells and the cars of the elements of the list that are cons cells
\(which must be strings or symbols) form the set of possible
completions.

If COLLECTION is a hash table, all the keys that are strings or symbols
are the possible completions.

If COLLECTION is an obarray, the names of all symbols in the obarray
are the possible completions.

If COLLECTION is a function, it is called with three arguments: the
values STRING, PREDICATE and nil.  Whatever it returns becomes the
value of `try-completion'.

If optional third argument PREDICATE is non-nil, it is used to test
each possible match.  The match is a candidate only if PREDICATE
returns non-nil.  The argument given to PREDICATE is the alist element
or the symbol from the obarray.  If COLLECTION is a hash table,
PREDICATE is passed two arguments, the key and the value of the hash
table entry.
*/
       (string, collection, predicate))
{
  /* This function can GC */
  struct try_completion_closure tcc;

  CHECK_STRING (string);

  if (!NILP (Ffunctionp (collection)))
    {
      return call3 (collection, string, predicate, Qnil);
    }

  if (!(LISTP (collection) || VECTORP (collection) || HASH_TABLEP (collection)))
    {
      signal_error (Qwrong_type_argument,
                    "must be a list, vector, hash table or function",
                    collection);
    }

  tcc.string = string;
  tcc.slength = string_char_length (string);
  tcc.bestmatch = Qnil;
  tcc.blength = 0;
  tcc.bestmatchsize = 0;
  tcc.predicate = predicate;
  tcc.hash_tablep = HASH_TABLEP (collection);
  tcc.matchcount = 0;

  map_completion (try_completion_mapper, collection, &tcc, predicate);

  if (NILP (tcc.bestmatch))
    return Qnil;		/* No completions found */

  /* If we are ignoring case, and there is no exact match, and no
     additional text was supplied, don't change the case of what the
     user typed.  */
  if (completion_ignore_case && tcc.bestmatchsize == tcc.slength
      && tcc.blength > tcc.bestmatchsize)
    return string;

  /* Return t if the supplied string is an exact match (counting
     case); it does not require any change to be made.  */
  if (tcc.matchcount == 1 && tcc.bestmatchsize == tcc.slength
      && 0 > scmp_1 (XSTRING_DATA (tcc.bestmatch), XSTRING_DATA (tcc.string),
		     tcc.bestmatchsize, 0))
    return Qt;

  /* Else extract the part in which all completions agree */
  return Fsubseq (tcc.bestmatch, Qzero, make_fixnum (tcc.bestmatchsize));
}

struct all_completions_closure
{
  Lisp_Object string;
  Charcount slength;
  Lisp_Object predicate;
  Lisp_Object allmatches;
  Boolint hash_tablep;
};

static int
all_completions_mapper (Lisp_Object eltstring, Lisp_Object value, void *arg)
{
  struct all_completions_closure *acc = (struct all_completions_closure *) arg;
  /* Is this element a possible completion? */

  if (SYMBOLP (eltstring))
    {
      eltstring = XSYMBOL_NAME (eltstring);
    }

  if (STRINGP (eltstring) && (acc->slength <= string_char_length (eltstring))
      /* Reject alternatives that start with space unless the input
         starts with space.  */
      && ((acc->slength > 0 && string_ichar (acc->string, 0) == ' ')
          || string_ichar (eltstring, 0) != ' ')
      && (0 > scmp (XSTRING_DATA (eltstring), XSTRING_DATA (acc->string),
                    acc->slength)))
    {
      /* Yes.  Now check whether predicate likes it. */
      struct gcpro gcpro1, gcpro2;
      int loser;
      GCPRO2 (eltstring, acc->string);
      loser = ignore_completion_p (eltstring, acc->predicate, value,
                                   acc->hash_tablep);
      UNGCPRO;
      if (!loser)
        {
          /* Ok => put it on the list. */
          XSETCDR (acc->allmatches, Fcons (eltstring, Qnil));
          acc->allmatches = XCDR (acc->allmatches);
        }
    }

  return 0;
}

DEFUN ("all-completions", Fall_completions, 2, 3, 0, /*
Search for partial matches to STRING in COLLECTION.
COLLECTION must be an list, a hash table, an obarray, or a function.

Each string (or symbol) in COLLECTION is tested to see if it (or its
name) begins with STRING.  The value is a list of all the strings from
COLLECTION that match.

If COLLECTION is a list, the elements of the list that are not cons
cells and the cars of the elements of the list that are cons cells
\(which must be strings or symbols) form the set of possible
completions.

If COLLECTION is a hash-table, all the keys that are strings or symbols
are the possible completions.

If COLLECTION is an obarray, the names of all symbols in the obarray
are the possible completions.

If COLLECTION is a function, it is called with three arguments: the
values STRING, PREDICATE and t.  Whatever it returns becomes the
value of `all-completions'.

If optional third argument PREDICATE is non-nil, it is used to test
each possible match.  The match is a candidate only if PREDICATE
returns non-nil.  The argument given to PREDICATE is the alist element
or the symbol from the obarray.  If COLLECTION is a hash table,
PREDICATE is passed two arguments, the key and the value of the hash
table entry.
*/
       (string, collection, predicate))
{
  /* This function can GC */
  struct all_completions_closure acc;
  Lisp_Object allmatches = noseeum_cons (Qnil, Qnil);
  struct gcpro gcpro1;

  CHECK_STRING (string);

  if (!NILP (Ffunctionp (collection)))
    {
      return call3 (collection, string, predicate, Qt);
    }

  if (!(LISTP (collection) || VECTORP (collection) || HASH_TABLEP (collection)))
    {
      signal_error (Qwrong_type_argument,
                    "must be a list, vector, hash table or function",
                    collection);
    }
  GCPRO1 (allmatches);
  acc.string = string;
  acc.slength = string_char_length (string);
  acc.predicate = predicate;
  acc.allmatches = allmatches;
  acc.hash_tablep = HASH_TABLEP (collection);

  map_completion (all_completions_mapper, collection, &acc, predicate);

  acc.allmatches = XCDR (allmatches);
  free_cons (allmatches);
  UNGCPRO;
  return acc.allmatches;
}

struct test_completion_closure
{
  Lisp_Object string;
  Lisp_Object predicate;
  Lisp_Object result;
  Boolint hash_tablep;
};

static int
test_completion_mapper (Lisp_Object eltstring, Lisp_Object value, void *arg)
{
  struct test_completion_closure *tcc = (struct test_completion_closure *) arg;

  if (SYMBOLP (eltstring))
    {
      eltstring = XSYMBOL_NAME (eltstring);
    }

  if (!STRINGP (eltstring))
    {
      return 0;
    }

  if (completion_ignore_case ?
      0 == qxetextcasecmp (XSTRING_DATA (tcc->string),
                           XSTRING_LENGTH (tcc->string),
                           XSTRING_DATA (eltstring),
                           XSTRING_LENGTH (eltstring))
      : 0 == qxememcmp4 (XSTRING_DATA (tcc->string),
                         XSTRING_LENGTH (tcc->string),
                         XSTRING_DATA (eltstring),
                         XSTRING_LENGTH (eltstring)))
    {
      struct gcpro gcpro1, gcpro2, gcpro3;
      int loser;
      GCPRO3 (eltstring, tcc->string, tcc->predicate);
      loser = ignore_completion_p (eltstring, tcc->predicate, value,
                                   tcc->hash_tablep);
      UNGCPRO;
      if (!loser)
        {
          tcc->result = Qt;
          return 1;
        }
    }

  return 0;
}

DEFUN ("test-completion", Ftest_completion, 2, 3, 0, /*
Return non-nil if STRING is a valid completion in COLLECTION.

COLLECTION must be a list, a hash table, an obarray, or a function.

Each string (or symbol) in COLLECTION is tested to see if it (or its
name) begins with STRING.  The value is a list of all the strings from
COLLECTION that match.

If COLLECTION is a list, the elements of the list that are not cons
cells and the cars of the elements of the list that are cons cells
\(which must be strings or symbols) form the set of possible
completions.

If COLLECTION is a hash-table, all the keys that are strings or symbols
are the possible completions.

If COLLECTION is an obarray, the names of all symbols in the obarray
are the possible completions.

If COLLECTION is a function, it is called with three arguments: the
values STRING, PREDICATE and the symbol `lambda'.  Whatever it returns
is passed back by `test-completion'.

If optional third argument PREDICATE is non-nil, it is used to test
for possible matches.  The match is a candidate only if PREDICATE
returns non-nil.  The argument given to PREDICATE is the alist element
or the symbol from the obarray.  If COLLECTION is a hash table,
PREDICATE is passed two arguments, the key and the value of the hash
table entry.
*/
       (string, collection, predicate))
{
  struct test_completion_closure tcc;

  CHECK_STRING (string);

  if (!NILP (Ffunctionp (collection)))
    {
      return call3 (collection, string, predicate, Qlambda);
    }

  if (!(LISTP (collection) || VECTORP (collection) || HASH_TABLEP (collection)))
    {
      signal_error (Qwrong_type_argument,
                    "must be a list, vector, hash table or function",
                    collection);
    }

  tcc.string = string;
  tcc.predicate = predicate;
  tcc.result = Qnil;
  tcc.hash_tablep = HASH_TABLEP (collection);

  if (VECTORP (collection) && !completion_ignore_case)
    {
      /* We're case sensitive -> no need for a linear search. */
      Lisp_Object lookup = Fintern_soft (string, collection, Qzero);

      if (ZEROP (lookup))
        {
          return Qnil;
        }

      return ignore_completion_p (XSYMBOL_NAME (lookup), tcc.predicate,
                                  lookup, 0) ? Qnil : Qt;

      /* It would be reasonable to do something similar for the hash
         tables, except, both symbol and string keys are vaild
         completions there. So a negative #'gethash for the string
         (with #'equal as the hash table tests) still means you have
         to do the linear search, for any symbols with that string
         name, which hash very differently; returning t is a little
         quicker, but returning nil is just as slow, so our average
         performance barely changes, at the cost of code
         complexity. */
    }
  else
    {
      map_completion (test_completion_mapper, collection, &tcc, predicate);
    }

  return tcc.result;
}


/* Useless FSFmacs functions */
/* More than useless.  I've nuked minibuf_prompt_width so they won't
   function at all in XEmacs at the moment.  They are used to
   implement some braindamage in FSF which we aren't including. --cet */

#if 0
DEFUN ("minibuffer-prompt", Fminibuffer_prompt, 0, 0, 0, /*
Return the prompt string of the currently-active minibuffer.
If no minibuffer is active, return nil.
*/
	 ())
{
  return Fcopy_sequence (Vminibuf_prompt);
}

DEFUN ("minibuffer-prompt-width", Fminibuffer_prompt_width, 0, 0, 0, /*
Return the display width of the minibuffer prompt.
*/
	 ())
{
  return make_fixnum (minibuf_prompt_width);
}
#endif /* 0 */


/************************************************************************/
/*                              echo area                               */
/************************************************************************/

extern int stdout_needs_newline;

static Lisp_Object
clear_echo_area_internal (struct frame *f, Lisp_Object label, int from_print,
			  int no_restore)
{
  /* This function can call lisp */
  if (!NILP (Ffboundp (Qclear_message)))
    {
      Lisp_Object frame = wrap_frame (f);

      return call4 (Qclear_message, label, frame, from_print ? Qt : Qnil,
		    no_restore ? Qt : Qnil);
    }
  else
    {
      stderr_out ("\n");
      return Qnil;
    }
}

Lisp_Object
clear_echo_area (struct frame *f, Lisp_Object label, int no_restore)
{
  /* This function can call lisp */
  return clear_echo_area_internal (f, label, 0, no_restore);
}

Lisp_Object
clear_echo_area_from_print (struct frame *f, Lisp_Object label, int no_restore)
{
  /* This function can call lisp */
  return clear_echo_area_internal (f, label, 1, no_restore);
}

void
echo_area_append (struct frame *f, const Ibyte *nonreloc, Lisp_Object reloc,
		  Bytecount offset, Bytecount length,
		  Lisp_Object label)
{
  /* This function can call lisp */
  Lisp_Object obj;
  struct gcpro gcpro1;
  Lisp_Object frame;

  /* There is an inlining bug in egcs-20000131 c++ that can be worked
     around as follows:  */
#if defined (__GNUC__) && defined (__cplusplus)
  alloca (4);
#endif

  /* some callers pass in a null string as a way of clearing the echo area.
     check for length == 0 now; if this case, neither nonreloc nor reloc
     may be valid.  */
  if (length == 0)
    return;

  fixup_internal_substring (nonreloc, reloc, offset, &length);

  /* also check it here, in case the string was really blank. */
  if (length == 0)
    return;

  if (!NILP (Ffboundp (Qappend_message)))
    {
      if (STRINGP (reloc) && offset == 0 && length == XSTRING_LENGTH (reloc))
	obj = reloc;
      else
	{
	  if (STRINGP (reloc))
	    nonreloc = XSTRING_DATA (reloc);
	  obj = make_string (nonreloc + offset, length);
	}

      frame = wrap_frame (f);
      GCPRO1 (obj);
      call4 (Qappend_message, label, obj, frame,
	     EQ (label, Qprint) ? Qt : Qnil);
      UNGCPRO;
    }
  else
    {
      if (STRINGP (reloc))
	nonreloc = XSTRING_DATA (reloc);
      write_string_1 (Qexternal_debugging_output, nonreloc + offset, length);
    }
}

void
echo_area_message (struct frame *f, const Ibyte *nonreloc,
		   Lisp_Object reloc, Bytecount offset, Bytecount length,
		   Lisp_Object label)
{
  /* This function can call lisp */
  clear_echo_area (f, label, 1);
  echo_area_append (f, nonreloc, reloc, offset, length, label);
}

int
echo_area_active (struct frame *UNUSED (f))
{
  /* By definition, the echo area is active if the echo-area buffer
     is not empty.  No need to call Lisp code. (Anyway, this function
     is called from redisplay.) */
  struct buffer *echo_buffer = XBUFFER (Vecho_area_buffer);
  return BUF_BEGV (echo_buffer) != BUF_ZV (echo_buffer);
}

Lisp_Object
echo_area_status (struct frame *f)
{
  /* This function can call lisp */
  if (!NILP (Ffboundp (Qcurrent_message_label)))
    {
      Lisp_Object frame = wrap_frame (f);

      return call1 (Qcurrent_message_label, frame);
    }
  else
    return stdout_needs_newline ? Qmessage : Qnil;
}

Lisp_Object
echo_area_contents (struct frame *UNUSED (f))
{
  /* See above.  By definition, the contents of the echo-area buffer
     are the contents of the echo area. */
  return Fbuffer_substring (Qnil, Qnil, Vecho_area_buffer);
}

/* Dump an informative message to the echo area.  This function takes a
   string in internal format. */
void
message_internal (const Ibyte *nonreloc, Lisp_Object reloc,
		  Bytecount offset, Bytecount length)
{
  /* This function can call lisp  */
  if (NILP (Vexecuting_macro))
    echo_area_message (selected_frame (), nonreloc, reloc, offset, length,
		       Qmessage);
}

void
message_append_internal (const Ibyte *nonreloc, Lisp_Object reloc,
			 Bytecount offset, Bytecount length)
{
  /* This function can call lisp  */
  if (NILP (Vexecuting_macro))
    echo_area_append (selected_frame (), nonreloc, reloc, offset, length,
		      Qmessage);
}

/* The next three functions are interfaces to message_internal() that
   take strings in external format.  message() does I18N3 translating
   on the format string; message_no_translate() does not. */

static void
message_1 (const CIbyte *fmt, va_list args)
{
  /* This function can call lisp */
  if (fmt)
    {
      struct gcpro gcpro1;
      /* message_internal() might GC, e.g. if there are after-change-hooks
	 on the echo area buffer */
      Lisp_Object obj = emacs_vsprintf_string (fmt, args);
      GCPRO1 (obj);
      message_internal (0, obj, 0, -1);
      UNGCPRO;
    }
  else
    message_internal (0, Qnil, 0, 0);
}

static void
message_append_1 (const CIbyte *fmt, va_list args)
{
  /* This function can call lisp */
  if (fmt)
    {
      struct gcpro gcpro1;
      /* message_internal() might GC, e.g. if there are after-change-hooks
	 on the echo area buffer */
      Lisp_Object obj = emacs_vsprintf_string (fmt, args);
      GCPRO1 (obj);
      message_append_internal (0, obj, 0, -1);
      UNGCPRO;
    }
  else
    message_append_internal (0, Qnil, 0, 0);
}

void
clear_message (void)
{
  /* This function can call lisp */
  message_internal (0, Qnil, 0, 0);
}

void
message (const char *fmt, ...)
{
  /* This function can call lisp */
  /* I think it's OK to pass the data of Lisp strings as arguments to
     this function.  No GC'ing will occur until the data has already
     been copied. */
  va_list args;

  va_start (args, fmt);
  if (fmt)
    fmt = GETTEXT (fmt);
  message_1 (fmt, args);
  va_end (args);
}

void
message_append (const char *fmt, ...)
{
  /* This function can call lisp */
  va_list args;

  va_start (args, fmt);
  if (fmt)
    fmt = GETTEXT (fmt);
  message_append_1 (fmt, args);
  va_end (args);
}

void
message_no_translate (const char *fmt, ...)
{
  /* This function can call lisp */
  /* I think it's OK to pass the data of Lisp strings as arguments to
     this function.  No GC'ing will occur until the data has already
     been copied. */
  va_list args;

  va_start (args, fmt);
  message_1 (fmt, args);
  va_end (args);
}


/************************************************************************/
/*                            initialization                            */
/************************************************************************/

void
syms_of_minibuf (void)
{
  DEFSYMBOL (Qminibuffer_setup_hook);

  DEFSYMBOL (Qcompletion_ignore_case);

  DEFSUBR (Fminibuffer_depth);
#if 0
  DEFSUBR (Fminibuffer_prompt);
  DEFSUBR (Fminibuffer_prompt_width);
#endif
  DEFSUBR (Fset_minibuffer_preprompt);
  DEFSUBR (Fread_minibuffer_internal);

  DEFSUBR (Ftry_completion);
  DEFSUBR (Fall_completions);
  DEFSUBR (Ftest_completion);

  DEFSYMBOL (Qappend_message);
  DEFSYMBOL (Qclear_message);
  DEFSYMBOL (Qdisplay_message);
  DEFSYMBOL (Qcurrent_message_label);
}

void
reinit_vars_of_minibuf (void)
{
  minibuf_level = 0;
}

void
vars_of_minibuf (void)
{
  staticpro (&Vminibuf_prompt);
  Vminibuf_prompt = Qnil;

  /* Added by Jareth Hein (jhod@po.iijnet.or.jp) for input system support */
  staticpro (&Vminibuf_preprompt);
  Vminibuf_preprompt = Qnil;

  DEFVAR_LISP ("minibuffer-setup-hook", &Vminibuffer_setup_hook /*
Normal hook run just after entry to minibuffer.
*/ );
  Vminibuffer_setup_hook = Qnil;

  DEFVAR_BOOL ("completion-ignore-case", &completion_ignore_case /*
Non-nil means don't consider case significant in completion.
*/ );
  completion_ignore_case = 0;

  DEFVAR_LISP ("completion-regexp-list", &Vcompletion_regexp_list /*
List of regexps that should restrict possible completions.
Each completion has to match all regexps in this list.
*/ );
  Vcompletion_regexp_list = Qnil;
}

void
reinit_complex_vars_of_minibuf (void)
{
  /* This function can GC */
#ifdef I18N3
  /* #### This needs to be fixed up so that the gettext() gets called
     at runtime instead of at load time. */
#endif
  Vminibuffer_zero
    = Fget_buffer_create (build_ascstring (" *Minibuf-0*"));
  staticpro_nodump (&Vminibuffer_zero);
  Vecho_area_buffer
    = Fget_buffer_create (build_ascstring (" *Echo Area*"));
  staticpro_nodump (&Vecho_area_buffer);
}

void
complex_vars_of_minibuf (void)
{
  reinit_complex_vars_of_minibuf ();
}
