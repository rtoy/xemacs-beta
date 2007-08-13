#include "fakelisp.h"

void *Lisp_Object_Table[4096];
int Lisp_Object_Index = 0;

Lisp_Object
make_vector(len)
     int len;
{
  Lisp_Object val = Lisp_Object_Index++;

  Lisp_Object_Table[val] = (char *) malloc (sizeof (struct Lisp_Vector));
  XVECTOR (val)->size = len;
  XVECTOR (val)->contents =
    (Lisp_Object *) malloc ((sizeof (Lisp_Object)) * len);
  return (val | (Lisp_Vector << VALBITS));
}

Lisp_Object
make_string(str)
     char *str;
{
  Lisp_Object val = Lisp_Object_Index++;
  int len = strlen (str);

  Lisp_Object_Table[val] = (char *) malloc (sizeof (struct Lisp_String));
  XSTRING (val)->size = len;
  XSTRING (val)->data = (unsigned char *) malloc (len + 1);
  memcpy (XSTRING (val)->data, str, len + 1);
  return (val | (Lisp_String << VALBITS));
}

Lisp_Object
make_symbol(name)
     char *name;
{
  Lisp_Object val = Lisp_Object_Index++;
  int len = strlen (name);

  Lisp_Object_Table[val] = (char *) malloc (sizeof (struct Lisp_Symbol));
  XSYMBOL (val)->name = (unsigned char *) malloc (len + 1);
  memcpy (XSYMBOL (val)->name, name, len + 1);
  XSYMBOL (val)->value = Qnil;
  return (val | (Lisp_String << VALBITS));
}

Lisp_Object
Fsymbol_value(obj)
     Lisp_Object obj;
{
  return XSYMBOL (obj)->value;
}
