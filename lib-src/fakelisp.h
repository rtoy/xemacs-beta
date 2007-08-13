#ifndef _FAKELISP_H
#define _FAKELISP_H

#include <config.h>

/* Cancel substitutions made by config.h for Emacs.  */
#undef open
#undef read
#undef write
#undef close

/* We used to test for `BSTRING' here, but only GCC and Emacs define
   `BSTRING', as far as I know, and neither of them use this code.  */
#if HAVE_STRING_H || STDC_HEADERS
#include <string.h>
#ifndef bcmp
#define bcmp(s1, s2, n) memcmp ((s1), (s2), (n))
#endif
#ifndef bcopy
#define bcopy(s, d, n)  memcpy ((d), (s), (n))
#endif
#ifndef bzero
#define bzero(s, n)     memset ((s), 0, (n))
#endif
#else
#include <strings.h>
#endif

typedef unsigned int Lisp_Object;

enum Lisp_Type {
  Lisp_Int,
  Lisp_Symbol,
  Lisp_String,
  Lisp_Vector
};

#ifndef VALBITS					/* hir, 1994.12.19 */
#define VALBITS 24
#endif
#define VALMASK ((1 << VALBITS) - 1)

#define XTYPE(x) ((enum Lisp_Type)((x)>>VALBITS))

struct Lisp_Vector {
  int size;
  Lisp_Object *contents;
};

struct Lisp_String {
  int size;
  unsigned char *data;
};

struct Lisp_Symbol {
  unsigned char *name;
  Lisp_Object value;
};

#define Qnil (Lisp_Object)(Lisp_Symbol << VALBITS)
#define Qt   (Lisp_Object)((Lisp_Symbol << VALBITS) | 1)

#define XFASTINT(x) (x)
#define XVECTOR(x) ((struct Lisp_Vector *)Lisp_Object_Table[(x)&VALMASK])
#define XSTRING(x) ((struct Lisp_String *)Lisp_Object_Table[(x)&VALMASK])
#define XSYMBOL(x) ((struct Lisp_Symbol *)Lisp_Object_Table[(x)&VALMASK])

extern void *Lisp_Object_Table[4096];

extern Lisp_Object make_vector(), make_string(), make_symbol();
extern Lisp_Object Fsymbol_value();

#define GLYPH unsigned int

#endif /* _FAKELISP_H */
