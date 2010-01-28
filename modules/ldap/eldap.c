/* LDAP client interface for XEmacs.
   Copyright (C) 1998 Free Software Foundation, Inc.
   Copyright (C) 2004 Ben Wing.
   

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

/* Author: Oscar Figueiredo with lots of support from Hrvoje Niksic */

/* This file provides lisp primitives for access to an LDAP library
   conforming to the API defined in RFC 1823.
   It has been tested with:
   - UMich LDAP 3.3 (http://www.umich.edu/~dirsvcs/ldap/)
   - OpenLDAP 1.2 (http://www.openldap.org/)
   - Netscape's LDAP SDK (http://developer.netscape.com/) */


#include <config.h>
#include "lisp.h"
#include "opaque.h"
#include "sysdep.h"
#include "buffer.h"
#include "process.h"		/* for report_process_error */
#ifdef HAVE_SHLIB
# include "emodules.h"
#endif

#include <errno.h>

#include "eldap.h"

static Fixnum ldap_default_port;
static Lisp_Object Vldap_default_base;

static Lisp_Object Qeldap;

/* Needed by the lrecord definition */
Lisp_Object Qldapp;

/* ldap-open plist keywords */
static Lisp_Object Qport, Qauth, Qbinddn, Qpasswd, Qderef, Qtimelimit, Qsizelimit;
/* Search scope limits */
static Lisp_Object Qbase, Qonelevel, Qsubtree;
/* Authentication methods */
static Lisp_Object Qkrbv41, Qkrbv42;
/* Deref policy */
static Lisp_Object Qnever, Qalways, Qfind;
/* Modification types (Qdelete is defined in general.c) */
static Lisp_Object Qadd, Qreplace;


/************************************************************************/
/*                         Utility Functions                            */
/************************************************************************/

static DECLARE_DOESNT_RETURN (signal_ldap_error (LDAP *, LDAPMessage *, int));

static DOESNT_RETURN
signal_ldap_error (LDAP *ld,
#if defined HAVE_LDAP_PARSE_RESULT || defined HAVE_LDAP_RESULT2ERROR
		   LDAPMessage *res,
#else
		   LDAPMessage *UNUSED (res),
#endif
		   int ldap_err)
{
  if (ldap_err <= 0)
    {
#if defined HAVE_LDAP_PARSE_RESULT
      int err;
      ldap_err = ldap_parse_result (ld, res,
                                    &err,
                                    NULL, NULL, NULL, NULL, 0);
      if (ldap_err == LDAP_SUCCESS)
        ldap_err = err;
#elif defined HAVE_LDAP_GET_LDERRNO
      ldap_err = ldap_get_lderrno (ld, NULL, NULL);
#elif defined HAVE_LDAP_RESULT2ERROR
      ldap_err = ldap_result2error (ld, res, 0);
#else
      ldap_err = ld->ld_errno;
#endif
    }
  invalid_operation ("LDAP error",
		     build_extstring (ldap_err2string (ldap_err), Qnative));
}


/************************************************************************/
/*                        ldap lrecord basic functions                  */
/************************************************************************/

static Lisp_Object
make_ldap (Lisp_LDAP *ldap)
{
  return wrap_ldap (ldap);
}

static const struct memory_description ldap_description [] = {
  { XD_LISP_OBJECT, offsetof (struct Lisp_LDAP, host) },
  { XD_END }
};

static Lisp_Object
mark_ldap (Lisp_Object obj)
{
  return XLDAP (obj)->host;
}

static void
print_ldap (Lisp_Object obj, Lisp_Object printcharfun, int UNUSED (escapeflag))
{
  Lisp_LDAP *ldap = XLDAP (obj);

  if (print_readably)
    printing_unreadable_object ("#<ldap %s>", XSTRING_DATA (ldap->host));

  write_fmt_string_lisp (printcharfun, "#<ldap %S", 1, ldap->host);
  if (!ldap->ld)
    write_ascstring (printcharfun,"(dead) ");
  write_fmt_string (printcharfun, " 0x%lx>", (long)ldap);
}

static Lisp_LDAP *
allocate_ldap (void)
{
  Lisp_LDAP *ldap = ALLOC_LCRECORD_TYPE (Lisp_LDAP, &lrecord_ldap);

  ldap->ld = NULL;
  ldap->host = Qnil;
  return ldap;
}

static void
finalize_ldap (void *header, int for_disksave)
{
  Lisp_LDAP *ldap = (Lisp_LDAP *) header;

  if (for_disksave)
    invalid_operation ("Can't dump an emacs containing LDAP objects",
			 make_ldap (ldap));

  if (ldap->ld)
    ldap_unbind (ldap->ld);
  ldap->ld = NULL;
}

DEFINE_LRECORD_IMPLEMENTATION ("ldap", ldap, 0,
                               mark_ldap, print_ldap, finalize_ldap,
                               NULL, NULL, ldap_description, Lisp_LDAP);


/************************************************************************/
/*                        Basic ldap accessors                          */
/************************************************************************/

/* ###autoload */
DEFUN ("ldapp", Fldapp, 1, 1, 0, /*
Return t if OBJECT is a LDAP connection.
*/
       (object))
{
  return LDAPP (object) ? Qt : Qnil;
}

DEFUN ("ldap-host", Fldap_host, 1, 1, 0, /*
Return the server host of the connection LDAP, as a string.
*/
       (ldap))
{
  CHECK_LDAP (ldap);
  return (XLDAP (ldap))->host;
}

DEFUN ("ldap-live-p", Fldap_live_p, 1, 1, 0, /*
Return t if LDAP is an active LDAP connection.
*/
       (ldap))
{
  CHECK_LDAP (ldap);
  return (XLDAP (ldap))->ld ? Qt : Qnil;
}

/************************************************************************/
/*                  Opening/Closing a LDAP connection                   */
/************************************************************************/


/* ###autoload */
DEFUN ("ldap-open", Fldap_open, 1, 2, 0, /*
Open a LDAP connection to HOST.
PLIST is a plist containing additional parameters for the connection.
Valid keys in that list are:
  `port' the TCP port to use for the connection if different from
`ldap-default-port'.
  `auth' is the authentication method to use, possible values depend on
the LDAP library XEmacs was compiled with: `simple', `krbv41' and `krbv42'.
  `binddn' is the distinguished name of the user to bind as (in RFC 1779 syntax).
  `passwd' is the password to use for simple authentication.
  `deref' is one of the symbols `never', `always', `search' or `find'.
  `timelimit' is the timeout limit for the connection in seconds.
  `sizelimit' is the maximum number of matches to return.
*/
       (host, plist))
{
  /* This function can GC */
  Lisp_LDAP *ldap;
  LDAP *ld;
  int  ldap_port = 0;
  int  ldap_auth = LDAP_AUTH_SIMPLE;
  Extbyte *ldap_binddn = NULL;
  Extbyte *ldap_password = NULL;
  int  ldap_deref = LDAP_DEREF_NEVER;
  int  ldap_timelimit = 0;
  int  ldap_sizelimit = 0;
  int  err;

  CHECK_STRING (host);

  {
    EXTERNAL_PROPERTY_LIST_LOOP_3 (keyword, value, plist)
      {
	/* TCP Port */
	if (EQ (keyword, Qport))
	  {
	    CHECK_INT (value);
	    ldap_port = XINT (value);
	  }
	/* Authentication method */
	if (EQ (keyword, Qauth))
	  {
	    if (EQ (value, Qsimple))
	      ldap_auth = LDAP_AUTH_SIMPLE;
#ifdef LDAP_AUTH_KRBV41
	    else if (EQ (value, Qkrbv41))
	      ldap_auth = LDAP_AUTH_KRBV41;
#endif
#ifdef LDAP_AUTH_KRBV42
	    else if (EQ (value, Qkrbv42))
	      ldap_auth = LDAP_AUTH_KRBV42;
#endif
	    else
	      invalid_constant ("Invalid authentication method", value);
	  }
	/* Bind DN */
	else if (EQ (keyword, Qbinddn))
	  {
	    CHECK_STRING (value);
	    LISP_STRING_TO_EXTERNAL (value, ldap_binddn, Qnative);
	  }
	/* Password */
	else if (EQ (keyword, Qpasswd))
	  {
	    CHECK_STRING (value);
	    LISP_STRING_TO_EXTERNAL (value, ldap_password, Qnative);
	  }
	/* Deref */
	else if (EQ (keyword, Qderef))
	  {
	    if (EQ (value, Qnever))
	      ldap_deref = LDAP_DEREF_NEVER;
	    else if (EQ (value, Qsearch))
	      ldap_deref = LDAP_DEREF_SEARCHING;
	    else if (EQ (value, Qfind))
	      ldap_deref = LDAP_DEREF_FINDING;
	    else if (EQ (value, Qalways))
	      ldap_deref = LDAP_DEREF_ALWAYS;
	    else
	      invalid_constant ("Invalid deref value", value);
	  }
	/* Timelimit */
	else if (EQ (keyword, Qtimelimit))
	  {
	    CHECK_INT (value);
	    ldap_timelimit = XINT (value);
	  }
	/* Sizelimit */
	else if (EQ (keyword, Qsizelimit))
	  {
	    CHECK_INT (value);
	    ldap_sizelimit = XINT (value);
	  }
      }
  }

  if (ldap_port == 0)
    {
      ldap_port = ldap_default_port;
    }

  /* Connect to the server and bind */
  slow_down_interrupts ();
  ld = ldap_open (NEW_LISP_STRING_TO_EXTERNAL (host, Qnative), ldap_port);
  speed_up_interrupts ();

  if (ld == NULL )
    report_process_error ("Failed connecting to host", host);

#ifdef HAVE_LDAP_SET_OPTION
  if ((err = ldap_set_option (ld, LDAP_OPT_DEREF,
                              (void *)&ldap_deref)) != LDAP_SUCCESS)
    signal_ldap_error (ld, NULL, err);
  if ((err = ldap_set_option (ld, LDAP_OPT_TIMELIMIT,
                              (void *)&ldap_timelimit)) != LDAP_SUCCESS)
    signal_ldap_error (ld, NULL, err);
  if ((err = ldap_set_option (ld, LDAP_OPT_SIZELIMIT,
                              (void *)&ldap_sizelimit)) != LDAP_SUCCESS)
    signal_ldap_error (ld, NULL, err);
  if ((err = ldap_set_option (ld, LDAP_OPT_REFERRALS,
                              LDAP_OPT_ON)) != LDAP_SUCCESS)
    signal_ldap_error (ld, NULL, err);
  if ((err = ldap_set_option (ld, LDAP_OPT_RESTART,
                              LDAP_OPT_ON)) != LDAP_SUCCESS)
    signal_ldap_error (ld, NULL, err);
#else  /* not HAVE_LDAP_SET_OPTION */
  ld->ld_deref = ldap_deref;
  ld->ld_timelimit = ldap_timelimit;
  ld->ld_sizelimit = ldap_sizelimit;
#ifdef LDAP_REFERRALS
  ld->ld_options = LDAP_OPT_REFERRALS;
#else /* not LDAP_REFERRALS */
  ld->ld_options = 0;
#endif /* not LDAP_REFERRALS */
  /* XEmacs uses interrupts (SIGIO,SIGALRM), LDAP calls need to ignore them */
  ld->ld_options |= LDAP_OPT_RESTART;
#endif /* not HAVE_LDAP_SET_OPTION */

  err = ldap_bind_s (ld, ldap_binddn, ldap_password, ldap_auth);
  if (err != LDAP_SUCCESS)
    {
      signal_error (Qprocess_error, "Failed binding to the server",
		    build_extstring (ldap_err2string (err), Qnative));
    }

  ldap = allocate_ldap ();
  ldap->ld = ld;
  ldap->host = host;

  return make_ldap (ldap);
}



DEFUN ("ldap-close", Fldap_close, 1, 1, 0, /*
Close an LDAP connection.
*/
      (ldap))
{
  Lisp_LDAP *lldap;
  CHECK_LIVE_LDAP (ldap);
  lldap = XLDAP (ldap);
  ldap_unbind (lldap->ld);
  lldap->ld = NULL;
  return Qnil;
}



/************************************************************************/
/*                  Working on a LDAP connection                        */
/************************************************************************/
struct ldap_unwind_struct
{
  LDAPMessage *res;
  struct berval **vals;
};

static Lisp_Object
ldap_search_unwind (Lisp_Object unwind_obj)
{
  struct ldap_unwind_struct *unwind =
    (struct ldap_unwind_struct *) get_opaque_ptr (unwind_obj);
  if (unwind->res)
    ldap_msgfree (unwind->res);
  if (unwind->vals)
    ldap_value_free_len (unwind->vals);
  return Qnil;
}

/* The following function is called `ldap-search-basic' instead of      */
/* plain `ldap-search' to maintain compatibility with the XEmacs 21.1   */
/* API where `ldap-search' was the name of the high-level search        */
/* function                                                             */

DEFUN ("ldap-search-basic", Fldap_search_basic, 2, 8, 0, /*
Perform a search on an open LDAP connection.
LDAP is an LDAP connection object created with `ldap-open'.
FILTER is a filter string for the search as described in RFC 1558.
BASE is the distinguished name at which to start the search.
SCOPE is one of the symbols `base', `onelevel' or `subtree' indicating
the scope of the search.
ATTRS is a list of strings indicating which attributes to retrieve
 for each matching entry. If nil return all available attributes.
If ATTRSONLY is non-nil then only the attributes are retrieved, not
the associated values.
If WITHDN is non-nil each entry in the result will be prepended with
its distinguished name DN.
If VERBOSE is non-nil progress messages will be echoed.
The function returns a list of matching entries.  Each entry is itself
an alist of attribute/value pairs optionally preceded by the DN of the
entry according to the value of WITHDN.
*/
       (ldap, filter, base, scope, attrs, attrsonly, withdn, verbose))
{
  /* This function can GC */

  /* Vars for query */
  LDAP *ld;
  LDAPMessage *e;
  BerElement *ptr;
  Extbyte *a, *dn, *bs, *filt;
  int i, rc;
  int  matches;
  struct ldap_unwind_struct unwind;

  int  ldap_scope = LDAP_SCOPE_SUBTREE;
  Extbyte **ldap_attributes = NULL;

  int speccount = specpdl_depth ();

  Lisp_Object list   = Qnil;
  Lisp_Object entry  = Qnil;
  Lisp_Object result = Qnil;
  struct gcpro gcpro1, gcpro2, gcpro3;

  GCPRO3 (list, entry, result);

  unwind.res = NULL;
  unwind.vals = NULL;

  /* Do all the parameter checking  */
  CHECK_LIVE_LDAP (ldap);
  ld = XLDAP (ldap)->ld;

  /* Filter */
  CHECK_STRING (filter);

  /* Search base */
  if (NILP (base))
    {
      base = Vldap_default_base;
    }
  if (!NILP (base))
    {
      CHECK_STRING (base);
    }

  /* Search scope */
  if (!NILP (scope))
    {
      if (EQ (scope, Qbase))
        ldap_scope = LDAP_SCOPE_BASE;
      else if (EQ (scope, Qonelevel))
        ldap_scope = LDAP_SCOPE_ONELEVEL;
      else if (EQ (scope, Qsubtree))
        ldap_scope = LDAP_SCOPE_SUBTREE;
      else
        invalid_constant ("Invalid scope", scope);
    }

  /* Attributes to search */
  if (!NILP (attrs))
    {
      CHECK_CONS (attrs);
      ldap_attributes = alloca_array (char *, 1 + XINT (Flength (attrs)));

      i = 0;
      {
	EXTERNAL_LIST_LOOP_2 (current, attrs)
	  {
	    CHECK_STRING (current);
	    LISP_STRING_TO_EXTERNAL (current, ldap_attributes[i], Qnative);
	    ++i;
	  }
      }
      ldap_attributes[i] = NULL;
    }

  /* Attributes only ? */
  CHECK_SYMBOL (attrsonly);

  /* Perform the search */
  bs = NILP (base) ? (Extbyte *) "" :
    NEW_LISP_STRING_TO_EXTERNAL (base, Qnative);
  filt = NILP (filter) ? (Extbyte *) "" :
    NEW_LISP_STRING_TO_EXTERNAL (filter, Qnative);
  if (ldap_search (ld, bs, ldap_scope, filt, ldap_attributes,
		   NILP (attrsonly) ? 0 : 1)
      == -1)
    {
      signal_ldap_error (ld, NULL, 0);
    }

  /* Ensure we don't exit without cleaning up */
  record_unwind_protect (ldap_search_unwind,
                         make_opaque_ptr (&unwind));

  /* Build the results list */
  matches = 0;

  rc = ldap_result (ld, LDAP_RES_ANY, 0, NULL, &unwind.res);

  while (rc == LDAP_RES_SEARCH_ENTRY)
    {
      QUIT;
      matches ++;
      e = ldap_first_entry (ld, unwind.res);
      /* #### This call to message() is pretty fascist, because it
         destroys the current echo area contents, even when invoked
         from Lisp.  It should use echo_area_message() instead, and
         restore the old echo area contents later.  */
      if (! NILP (verbose))
        message ("Parsing ldap results... %d", matches);
      entry = Qnil;
      /* Get the DN if required */
      if (! NILP (withdn))
        {
          dn = ldap_get_dn (ld, e);
          if (dn == NULL)
            signal_ldap_error (ld, e, 0);
          entry = Fcons (build_extstring (dn, Qnative), Qnil);
        }
      for (a = ldap_first_attribute (ld, e, &ptr);
           a != NULL;
           a = ldap_next_attribute (ld, e, ptr))
        {
          list = Fcons (build_extstring (a, Qnative), Qnil);
          unwind.vals = ldap_get_values_len (ld, e, a);
          if (unwind.vals != NULL)
            {
              for (i = 0; unwind.vals[i] != NULL; i++)
                {
                  list = Fcons (make_extstring ((Extbyte *) unwind.vals[i]->bv_val,
                                                 unwind.vals[i]->bv_len,
                                                 Qnative),
                                list);
                }
            }
          entry = Fcons (Fnreverse (list),
                         entry);
          ldap_value_free_len (unwind.vals);
          unwind.vals = NULL;
        }
      result = Fcons (Fnreverse (entry),
                      result);
      ldap_msgfree (unwind.res);
      unwind.res = NULL;

      rc = ldap_result (ld, LDAP_RES_ANY, 0, NULL, &(unwind.res));
    }

#if defined HAVE_LDAP_PARSE_RESULT
  {
    int rc2 = ldap_parse_result (ld, unwind.res,
				 &rc,
				 NULL, NULL, NULL, NULL, 0);
    if (rc2 != LDAP_SUCCESS)
      rc = rc2;
  }
#else
  if (rc == 0)
    signal_ldap_error (ld, NULL, LDAP_TIMELIMIT_EXCEEDED);

  if (rc == -1)
    signal_ldap_error (ld, unwind.res, (unwind.res==NULL) ? ld->ld_errno : 0);

#if defined HAVE_LDAP_RESULT2ERROR
  rc = ldap_result2error (ld, unwind.res, 0);
#endif
#endif

  if (rc != LDAP_SUCCESS)
    signal_ldap_error (ld, NULL, rc);

  ldap_msgfree (unwind.res);
  unwind.res = (LDAPMessage *)NULL;

  /* #### See above for calling message().  */
  if (! NILP (verbose))
    message ("Parsing ldap results... done");

  unbind_to (speccount);
  UNGCPRO;
  return Fnreverse (result);
}

DEFUN ("ldap-add", Fldap_add, 3, 3, 0, /*
Add an entry to an LDAP directory.
LDAP is an LDAP connection object created with `ldap-open'.
DN is the distinguished name of the entry to add.
ENTRY is an entry specification, i.e., a list of cons cells
containing attribute/value string pairs.
*/
       (ldap, dn, entry))
{
  LDAP *ld;
  LDAPMod *ldap_mods, **ldap_mods_ptrs;
  struct berval *bervals;
  int rc;
  int i, j;
  Elemcount len;

  Lisp_Object values  = Qnil;
  struct gcpro gcpro1;

  GCPRO1 (values);

  /* Do all the parameter checking  */
  CHECK_LIVE_LDAP (ldap);
  ld = XLDAP (ldap)->ld;

  /* Check the DN */
  CHECK_STRING (dn);

  /* Check the entry */
  CHECK_CONS (entry);
  if (NILP (entry))
    invalid_operation ("Cannot add void entry", entry);

  /* Build the ldap_mods array */
  len = (Elemcount) XINT (Flength (entry));
  ldap_mods = alloca_array (LDAPMod, len);
  ldap_mods_ptrs = alloca_array (LDAPMod *, 1 + len);
  i = 0;

  {
    EXTERNAL_LIST_LOOP_2 (current, entry)
      {
	CHECK_CONS (current);
	CHECK_STRING (XCAR (current));
	ldap_mods_ptrs[i] = &(ldap_mods[i]);
	LISP_STRING_TO_EXTERNAL (XCAR (current), ldap_mods[i].mod_type,
				 Qnative);
	ldap_mods[i].mod_op = LDAP_MOD_ADD | LDAP_MOD_BVALUES;
	values = XCDR (current);
	if (CONSP (values))
	  {
	    len = (Elemcount) XINT (Flength (values));
	    bervals = alloca_array (struct berval, len);
	    ldap_mods[i].mod_vals.modv_bvals =
	      alloca_array (struct berval *, 1 + len);
	    j = 0;
	    {
	      EXTERNAL_LIST_LOOP_2 (cur2, values)
		{
		  CHECK_STRING (cur2);
		  ldap_mods[i].mod_vals.modv_bvals[j] = &(bervals[j]);
		  TO_EXTERNAL_FORMAT (LISP_STRING, cur2,
				      ALLOCA, (bervals[j].bv_val,
					       bervals[j].bv_len),
				      Qnative);
		  j++;
		}
	    }
	    ldap_mods[i].mod_vals.modv_bvals[j] = NULL;
	  }
	else
	  {
	    CHECK_STRING (values);
	    bervals = alloca_array (struct berval, 1);
	    ldap_mods[i].mod_vals.modv_bvals = alloca_array (struct berval *,
							     2);
	    ldap_mods[i].mod_vals.modv_bvals[0] = &(bervals[0]);
	    TO_EXTERNAL_FORMAT (LISP_STRING, values,
				ALLOCA, (bervals[0].bv_val,
					 bervals[0].bv_len),
				Qnative);
	    ldap_mods[i].mod_vals.modv_bvals[1] = NULL;
	  }
	i++;
      }
  }
  ldap_mods_ptrs[i] = NULL;
  rc = ldap_add_s (ld, NEW_LISP_STRING_TO_EXTERNAL (dn, Qnative),
		   ldap_mods_ptrs);
  if (rc != LDAP_SUCCESS)
    signal_ldap_error (ld, NULL, rc);

  UNGCPRO;
  return Qnil;
}

DEFUN ("ldap-modify", Fldap_modify, 3, 3, 0, /*
Add an entry to an LDAP directory.
LDAP is an LDAP connection object created with `ldap-open'.
DN is the distinguished name of the entry to modify.
MODS is a list of modifications to apply.
A modification is a list of the form (MOD-OP ATTR VALUE1 VALUE2 ...)
MOD-OP and ATTR are mandatory, VALUEs are optional depending on MOD-OP.
MOD-OP is the type of modification, one of the symbols `add', `delete'
or `replace'. ATTR is the LDAP attribute type to modify.
*/
       (ldap, dn, mods))
{
  LDAP *ld;
  LDAPMod *ldap_mods, **ldap_mods_ptrs;
  struct berval *bervals;
  int i, j, rc;
  Lisp_Object mod_op;
  Elemcount len;

  Lisp_Object values  = Qnil;
  struct gcpro gcpro1;

  /* Do all the parameter checking  */
  CHECK_LIVE_LDAP (ldap);
  ld = XLDAP (ldap)->ld;

  /* Check the DN */
  CHECK_STRING (dn);

  /* Check the entry */
  CHECK_CONS (mods);
  if (NILP (mods))
    return Qnil;

  /* Build the ldap_mods array */
  len = (Elemcount) XINT (Flength (mods));
  ldap_mods = alloca_array (LDAPMod, len);
  ldap_mods_ptrs = alloca_array (LDAPMod *, 1 + len);
  i = 0;

  GCPRO1 (values);
  {
    EXTERNAL_LIST_LOOP_2 (current, mods)
      {
	CHECK_CONS (current);
	CHECK_SYMBOL (XCAR (current));
	mod_op = XCAR (current);
	ldap_mods_ptrs[i] = &(ldap_mods[i]);
	ldap_mods[i].mod_op = LDAP_MOD_BVALUES;
	if (EQ (mod_op, Qadd))
	  ldap_mods[i].mod_op |= LDAP_MOD_ADD;
	else if (EQ (mod_op, Qdelete))
	  ldap_mods[i].mod_op |= LDAP_MOD_DELETE;
	else if (EQ (mod_op, Qreplace))
	  ldap_mods[i].mod_op |= LDAP_MOD_REPLACE;
	else
	  invalid_constant ("Invalid LDAP modification type", mod_op);
	current = XCDR (current);
	CHECK_STRING (XCAR (current));
	LISP_STRING_TO_EXTERNAL (XCAR (current), ldap_mods[i].mod_type,
				 Qnative);
	values = XCDR (current);
	len = (Elemcount) XINT (Flength (values));
	bervals = alloca_array (struct berval, len);
	ldap_mods[i].mod_vals.modv_bvals =
	  alloca_array (struct berval *, 1 + len);
	j = 0;
	{
	  EXTERNAL_LIST_LOOP_2 (cur2, values)
	    {
	      CHECK_STRING (cur2);
	      ldap_mods[i].mod_vals.modv_bvals[j] = &(bervals[j]);
	      TO_EXTERNAL_FORMAT (LISP_STRING, cur2,
				  ALLOCA, (bervals[j].bv_val,
					   bervals[j].bv_len),
				  Qnative);
	      j++;
	    }
	  ldap_mods[i].mod_vals.modv_bvals[j] = NULL;
	  i++;
	}
      }
  }
  ldap_mods_ptrs[i] = NULL;
  rc = ldap_modify_s (ld, NEW_LISP_STRING_TO_EXTERNAL (dn, Qnative),
		      ldap_mods_ptrs);
  if (rc != LDAP_SUCCESS)
    signal_ldap_error (ld, NULL, rc);

  UNGCPRO;
  return Qnil;
}


DEFUN ("ldap-delete", Fldap_delete, 2, 2, 0, /*
Delete an entry to an LDAP directory.
LDAP is an LDAP connection object created with `ldap-open'.
DN is the distinguished name of the entry to delete.
*/
       (ldap, dn))
{
  LDAP *ld;
  int rc;

  /* Check parameters */
  CHECK_LIVE_LDAP (ldap);
  ld = XLDAP (ldap)->ld;
  CHECK_STRING (dn);

  rc = ldap_delete_s (ld, NEW_LISP_STRING_TO_EXTERNAL (dn, Qnative));
  if (rc != LDAP_SUCCESS)
    signal_ldap_error (ld, NULL, rc);

  return Qnil;
}

void
syms_of_eldap (void)
{
  INIT_LRECORD_IMPLEMENTATION (ldap);

  DEFSYMBOL (Qeldap);
  DEFSYMBOL (Qldapp);
  DEFSYMBOL (Qport);
  DEFSYMBOL (Qauth);
  DEFSYMBOL (Qbinddn);
  DEFSYMBOL (Qpasswd);
  DEFSYMBOL (Qderef);
  DEFSYMBOL (Qtimelimit);
  DEFSYMBOL (Qsizelimit);
  DEFSYMBOL (Qbase);
  DEFSYMBOL (Qonelevel);
  DEFSYMBOL (Qsubtree);
  DEFSYMBOL (Qkrbv41);
  DEFSYMBOL (Qkrbv42);
  DEFSYMBOL (Qnever);
  DEFSYMBOL (Qalways);
  DEFSYMBOL (Qfind);
  DEFSYMBOL (Qadd);
  DEFSYMBOL (Qreplace);

  DEFSUBR (Fldapp);
  DEFSUBR (Fldap_host);
  DEFSUBR (Fldap_live_p);
  DEFSUBR (Fldap_open);
  DEFSUBR (Fldap_close);
  DEFSUBR (Fldap_search_basic);
  DEFSUBR (Fldap_add);
  DEFSUBR (Fldap_modify);
  DEFSUBR (Fldap_delete);
}

void
vars_of_eldap (void)
{

  Fprovide (Qeldap);

  ldap_default_port = LDAP_PORT;
  Vldap_default_base =  Qnil;

  DEFVAR_INT ("ldap-default-port", &ldap_default_port /*
Default TCP port for LDAP connections.
Initialized from the LDAP library. Default value is 389.
*/ );

  DEFVAR_LISP ("ldap-default-base", &Vldap_default_base /*
Default base for LDAP searches.
This is a string using the syntax of RFC 1779.
For instance, "o=ACME, c=US" limits the search to the
Acme organization in the United States.
*/ );

}

#ifdef HAVE_SHLIB
EXTERN_C void unload_eldap (void);
void
unload_eldap (void)
{
  /* Remove defined types */
  UNDEF_LRECORD_IMPLEMENTATION (ldap);

  /* Remove staticpro'ing of symbols */
  unstaticpro_nodump (&Qeldap);
  unstaticpro_nodump (&Qldapp);
  unstaticpro_nodump (&Qport);
  unstaticpro_nodump (&Qauth);
  unstaticpro_nodump (&Qbinddn);
  unstaticpro_nodump (&Qpasswd);
  unstaticpro_nodump (&Qderef);
  unstaticpro_nodump (&Qtimelimit);
  unstaticpro_nodump (&Qsizelimit);
  unstaticpro_nodump (&Qbase);
  unstaticpro_nodump (&Qonelevel);
  unstaticpro_nodump (&Qsubtree);
  unstaticpro_nodump (&Qkrbv41);
  unstaticpro_nodump (&Qkrbv42);
  unstaticpro_nodump (&Qnever);
  unstaticpro_nodump (&Qalways);
  unstaticpro_nodump (&Qfind);
  unstaticpro_nodump (&Qadd);
  unstaticpro_nodump (&Qreplace);
}
#endif /* HAVE_SHLIB */
