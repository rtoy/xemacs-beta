#include <stdio.h>
#include <sys/param.h>
#include "mulelib.h"

#ifdef STDC_HEADERS
#include <stdlib.h>
#endif

#ifdef USG
#include <string.h>
#else
#include <strings.h>
#endif

#include <ctype.h>

char *mule_library_version = "2.2";

int mule_error;
char mule_error_msg[256];

static int mulelib_initialized = 0;

#ifdef MSDOS
#define READ_TEXT "rt"
#else /* not MSDOS */
#define READ_TEXT "r"
#endif /* not MSDOS */

/********************
 * General routines *
 ********************/

#define LINE_SIZE 4096

FILE *
open_file(dirs, file)
     char *dirs, *file;
{
  char path[MAXPATHLEN], *tail;
  int len;
  FILE *fp;

  if (file[0] == '/'
#ifdef MSDOS
      || file[0] && file[1] == ':'
#endif
      )
    return fopen(file, READ_TEXT);

  while (*dirs) {
    (tail = index(dirs, ','))
#ifdef MSDOS
      || (tail = index(dirs, ';'))
#else
      || (tail = index(dirs, ':'))
#endif
      || (tail = index(dirs, '\0'));

    len = tail - dirs;
    strncpy(path, dirs, len);
    while (path[--len] == '/'); /* back to non-slash */
    path[++len] = '/';
    strcpy(&path[++len], file);

    fp = fopen (path, READ_TEXT);
    if (fp || *tail == '\0') return fp;
 
    dirs = tail + 1;
  }
}

get_line(buf, size, fp)
     char *buf;
     int size;
     FILE *fp;
{
  int len;

  if (fgets(buf, size, fp) != NULL) {
    len = strlen(buf);
    if (buf[len - 1] == '\n')
      buf[--len] = '\0';
    return len;
  } else
    return -1;
}

#define PROCEED_CHAR(c) \
  if (!(p1 = (char *)index(p0, c))) goto invalid_entry

warning1(format, arg1)
     char *format;
     int arg1;
{
  fprintf(stderr, format, arg1);
}

warning2(format, arg1, arg2)
     char *format;
     int arg1, arg2;
{
  fprintf(stderr, format, arg1, arg2);
}

warning3(format, arg1, arg2, arg3)
     char *format;
     int arg1, arg2, arg3;
{
  fprintf(stderr, format, arg1, arg2, arg3);
}

fatal1(arg)
     char *arg;
{
  fprintf(stderr, "%s", arg);
  exit(1);
}

/*****************************
 * Reading CHARSETS database *
 *****************************/

/* extra information table */
char *font_name[128];
int font_encoding[128];

set_charsets_param(line)
     char *line;
{
  char *p0 = line, *p1;
  int len;
  unsigned char lc, bytes, clm, type, graphic, final, direction;
  char *doc;

  lc = atoi(p0);

  PROCEED_CHAR(':');
  p0 = p1 + 1;
  bytes = atoi(p0);

  PROCEED_CHAR(':');
  p0 = p1 + 1;
  clm = atoi(p0);

  PROCEED_CHAR(':');
  p0 = p1 + 1;
  type = atoi(p0);

  PROCEED_CHAR(':');
  p0 = p1 + 1;
  graphic = atoi(p0);

  PROCEED_CHAR(':');
  p0 = p1 + 1;
  final = atoi(p0);

  PROCEED_CHAR(':');
  p0 = p1 + 1;
  direction = atoi(p0);

  PROCEED_CHAR(':');
  p0 = p1 + 1;
  (p1 = index(p0, ':')) || (p1 = index(p0, '\0'));
  len = p1 - p0;
  doc = (char *)malloc(len + 1);
  bcopy(p0, doc, len);
  doc[len] = '\0';

  update_mc_table(lc, bytes, clm, type, graphic, final, direction, doc);

  if (*p1) {
    p0 = p1 + 1;
    (p1 = index(p0, ':')) || (p1 = index(p0, '\0'));
    len = p1 - p0;
    font_name[lc & 0x7F] = (char *)malloc(len + 1);
    bcopy(p0, font_name[lc & 0x7F], len);
    font_name[lc & 0x7F][len] = '\0';

    font_encoding[lc & 0x7F] = *p1 ? atoi(p1) : 0;
  }

  return 0;

 invalid_entry:
  mule_error = MULE_ERROR_INVALID_CHARSETS;
  sprintf(mule_error_msg, "Invalid line in CHARSETS file: %s", line);
  return -1;
}
 
set_ccl_program_param(line)
     char *line;
{
  int lc, len, i, j;
  char *p, *p1;
  Lisp_Object ccl;

  lc = atoi(line) & 0x7F;
  p = p1 = index(line, ':') + 1;
  if (!p) return -1;
  len = 0;
  while (*p1) if (*p1++ == ' ') len++;
  ccl = make_vector(len);
  for (i = 0; i < len; i++) {
    sscanf(p, "%x", XVECTOR (ccl)->contents + i);
    p = index(p + 1, ' ');
  }
  x_ccl_programs[lc] = (CCL_PROGRAM *)malloc(sizeof (CCL_PROGRAM));
  set_ccl_program(x_ccl_programs[lc], ccl);
  return 0;
}

charsets_initialize(charsets)
     char *charsets;
{
  FILE *fp;
  char line[LINE_SIZE];
  int i, status;

  if (!charsets) charsets = CHARSETS;
  fp = open_file(PATH_DATA, charsets);
  if (!fp) {
    mule_error = MULE_ERROR_NO_CHARSETS;
    sprintf(mule_error_msg, "File %s not in the path %s", charsets, PATH_DATA);
    return -1;
  }
  init_charset_once();
  for (i = 0; i < 128; i++)
    x_ccl_programs[i] = NULL;

  status = 0;
  while (get_line(line, sizeof line, fp) >= 0) {
    if (line[0] == '#') {
      if (status > 0) status++;
      continue;
    }
    if (status <= 1) {
      status = 1;
      if (set_charsets_param(line) < 0) return -1;
    } else {
      if (set_ccl_program_param(line) < 0) return -1;
    }
  }
  return 0;
}

/*****************************
 * Reading CODINGS database *
 *****************************/

coding_type coding_system_table[CODING_SYSTEM_COUNT];
int n_base_coding_system = 0;
int n_coding_system;

set_coding_system_param(line, cs)
     char *line;
     coding_type *cs;
{
  char *p0 = line, *p1;
  int len, i;

  PROCEED_CHAR(':');
  len = p1 - p0;
  cs->name = (char *)malloc(len + 1);
  bcopy(p0, cs->name, len);
  cs->name[len] = '\0';

  p0 = p1 + 1;
  i = atoi(p0);
  CODE_TYPE_SET(cs, i);

  PROCEED_CHAR(':');
  p0 = p1 + 1;
  cs->mnemonic = *p0;

  PROCEED_CHAR(':');
  p0 = p1 + 1;
  i = atoi(p0);
  CODE_FORM (cs)
    = (i==0) ? CODE_EOL_AUTO : (i==1) ? CODE_EOL_LF
      : (i==2) ? CODE_EOL_CRLF : CODE_EOL_CR;
  CODE_LF (cs) = CODE_CRLF (cs) = CODE_CR (cs) = Qnil;

  PROCEED_CHAR(':');
  p0 = p1 + 1;
  if (CODE_TYPE (cs) == ISO2022)  {
    int flags[12];
    for (i = 0; i < 11; i++) {
      if (!(flags[i] = atoi(p0))) flags[i] = Qnil;
      PROCEED_CHAR(',');
      p0 = p1 + 1;
    }
    flags[i] = atoi(p0);
    CODE_LC_SET(cs, flags[0], flags[1], flags[2], flags[3]);
    CODE_FORM_SET(cs, (Lisp_Object)flags[4], (Lisp_Object)flags[5],
		  (Lisp_Object)flags[6], (Lisp_Object)flags[7],
		  (Lisp_Object)flags[8], (Lisp_Object)flags[9],
		  (Lisp_Object)flags[10], (Lisp_Object)flags[11]);
  } else if (CODE_TYPE (cs) == BIG5) {
    CODE_FORM_SET(cs, Qnil, Qnil, Qnil, Qnil, Qnil, Qnil, Qnil, Qnil);
  } else if (CODE_TYPE (cs) == CCL) {
    int len;
    char *start = p0, c;
    Lisp_Object ccl;

    len = 0;
    while ((c = *p0++) != ',') if (c == ' ') len++;
    ccl = make_vector (len);
    for (i = 0; i < len; i++) {
      sscanf(start, "%x", XVECTOR (ccl)->contents + i);
      start = index (start + 1, ' ');
    }
    set_ccl_program (&CODE_CCL_ENCODE (cs), ccl);

    start = p0;
    len = 0;
    while ((c = *p0++) != ':') if (c == ' ') len++;
    ccl = make_vector (len);
    for (i = 0; i < len; i++) {
      sscanf(start, "%x", XVECTOR (ccl)->contents + i);
      start = index (start + 1, ' ');
    }
    set_ccl_program (&CODE_CCL_DECODE (cs), ccl);
    p0 -= 2;
  }

  PROCEED_CHAR(':');
  p0 = p1 + 1;
  len = strlen(p0);
  cs->doc = (char *)malloc(len + 1);
  bcopy(p0, cs->doc, len + 1);
  return 0;

 invalid_entry:
  mule_error = MULE_ERROR_INVALID_CODING;
  sprintf(mule_error_msg, "Invalid line in CODINGS file: %s", line);
  return -1;
}

char *coding_category_names[IDX_BIN + 1] = {
  "*coding-category-internal*",
  "*coding-category-sjis*",
  "*coding-category-iso-7*",
  "*coding-category-iso-8-1*",
  "*coding-category-iso-8-2*",
  "*coding-category-iso-else*",
  "*coding-category-big5*",
  "*coding-category-bin*"};

set_coding_category(line, priority)
     char *line;
     int priority;
{
  char *p0 = line, *p1;
  int i, len;

  PROCEED_CHAR(':');
  len = p1 - p0;
  for (i = 0; i <= IDX_BIN; i++) {
    if (!strncmp(p0, XSYMBOL (code_category[i])->name, len))
      break;
  }
  if (i <= IDX_BIN) {
    code_priority_category[priority] = i;
    p0 = p1 + 1;
    XSYMBOL (code_category[i])->value = make_symbol(p0);
    return 0;
  }

 invalid_entry:
  mule_error = MULE_ERROR_INVALID_CODING;
  sprintf(mule_error_msg, "Invalid line in CODINGS file: %s", line);
  return -1;
} 

codings_initialize(codings)
     char *codings;
{
  FILE *fp;
  char line[LINE_SIZE];
  char *coding_category_header = "## LIST OF CODING CATEGORIES";
  coding_type *cs;
  int i;

  if (!codings) codings = CODINGS;
  fp = open_file(PATH_DATA, codings);
  if (!fp) {
    mule_error = MULE_ERROR_NO_CODING;
    sprintf(mule_error_msg, "File %s not in the path %s!", codings, PATH_DATA);
    return -1;
  }
  n_coding_system = 0;
  while (get_line(line, sizeof line, fp) >= 0
	 && strncmp(line, coding_category_header,
		    strlen(coding_category_header))) {
    if (line[0] != '#') {
      if (set_coding_system_param(line, coding_system_table + n_coding_system)
	  < 0)
	return -1;
      n_coding_system++;
    }
  }

  n_base_coding_system = n_coding_system;

  for (i = 0; i < n_base_coding_system; i++) {
    if (CODE_TYPE (&coding_system_table[i]) != CCL) {
      sprintf(line, "%sunix", coding_system_table[i].name);
      coding_system_table[i].eol_lf = make_symbol(line);
      cs = &coding_system_table[n_coding_system++];
      *cs = coding_system_table[i];
      cs->name = XSYMBOL (coding_system_table[i].eol_lf)->name;
      CODE_FORM (cs) |= CODE_EOL_LF;
      CODE_LF (cs) = CODE_CRLF (cs) = CODE_CR (cs) = Qnil;

      sprintf(line, "%sdos", coding_system_table[i].name);
      coding_system_table[i].eol_crlf = make_symbol(line);
      cs = &coding_system_table[n_coding_system++];
      *cs = coding_system_table[i];
      cs->name = XSYMBOL (coding_system_table[i].eol_crlf)->name;
      CODE_FORM (cs) |= CODE_EOL_CRLF;
      CODE_LF (cs) = CODE_CRLF (cs) = CODE_CR (cs) = Qnil;

      sprintf(line, "%smac", coding_system_table[i].name);
      coding_system_table[i].eol_cr = make_symbol(line);
      cs = &coding_system_table[n_coding_system++];
      *cs = coding_system_table[i];
      cs->name = XSYMBOL (coding_system_table[i].eol_cr)->name;
      CODE_FORM (cs) |= CODE_EOL_CR;
      CODE_LF (cs) = CODE_CRLF (cs) = CODE_CR (cs) = Qnil;
    }
  }

  for (i = 0; i <= IDX_BIN; i++)
    code_category[i] = make_symbol(coding_category_names[i]);
  i = 0;
  while (get_line(line, sizeof line, fp) >= 0) {
    if (line[0] && line[0] != '#') {
      if (set_coding_category(line, i) < 0)
	return -1;
      i++;
    }
  }
  return 0;
}

static
find_coding(str)
     char *str;
{
  int i;

  if (*str == '*') {
    for (i = 0; i < n_coding_system; i++)
      if (!strcmp(str, coding_system_table[i].name)) return i;
  } else {
    unsigned int eol_type;
    switch (str[1]) {
    case '\0': eol_type = CODE_EOL_AUTO; break;
    case '.': eol_type = CODE_EOL_LF; break;
    case ':': eol_type = CODE_EOL_CRLF; break;
    case '\'': eol_type = CODE_EOL_CR; break;
    default: return -1;
    }
    for (i = 0; i < n_coding_system; i++)
      if (coding_system_table[i].mnemonic == *str
	  && (CODE_FORM (&coding_system_table[i]) & CODE_EOL_MASK) == eol_type)
	return i;
  }
  return -1;
}

encode_code(code, mccode)
     Lisp_Object code;
     coding_type *mccode;
{
  int i;

  if ((i = find_coding(XSYMBOL (code)->name)) >= 0)
    *mccode = coding_system_table[i];
  else
    CODE_TYPE_SET (mccode, NOCONV);
}

set_coding_system(inname, incode, outname, outcode)
     char *inname, *outname;
     coding_type *incode, *outcode;
{
  int i;

  if (inname == NULL) inname = "*autoconv*";
  if ((i = find_coding(inname)) < 0) {
    mule_error = MULE_ERROR_UNKNOWN_CODE;
    sprintf(mule_error_msg, "Unknown code: %s", inname);
    return -1;
  }
  *incode = coding_system_table[i];

  if (outname == NULL) outname = "*internal*";
  if ((i = find_coding(outname)) < 0) {
    mule_error = MULE_ERROR_UNKNOWN_CODE;
    sprintf(mule_error_msg, "Unknown code: %s", outname);
    return -1;
  }
  *outcode = coding_system_table[i];

  return 0;
}

code_conversion(incode, inbuf, insize, outcode, outbuf, outsize)
     coding_type *incode, *outcode;
     char *inbuf, *outbuf;
     int insize, outsize;
{
  Lisp_Object found = Qnil;
  char *buf1, *buf2;
  int buf1_used = 0, buf2_used = 0, n;

  if (insize > 0) {
    if (CODE_TYPE (incode) == ITNCODE) {
      buf1 = inbuf;
      n = insize;
    } else {
      buf1 = (char *)malloc(ENCODE_BUF_SIZE(insize, incode));
      buf1_used = 1;
      n = encode(incode, inbuf, buf1, insize, &found);
      if (found != Qnil)
	incode->name = (char *)XSYMBOL (found)->name;
    }
  } else {
    buf1 = inbuf;
    n = 0;
    CODE_CNTL(outcode) |= CC_END;
  }

  if (CODE_TYPE (outcode) == ITNCODE) {
    buf2 = buf1;
  } else {
    if ((CODE_FORM (outcode) & CODE_EOL_MASK) == CODE_EOL_AUTO
	&& (CODE_FORM (incode) & CODE_EOL_MASK) != CODE_EOL_AUTO) {
      unsigned int eol = CODE_FORM (incode) & CODE_EOL_MASK;

      CODE_FORM (outcode) |= eol;
      switch (eol) {
      case CODE_EOL_LF:
	outcode->name = (char *)XSYMBOL (outcode->eol_lf)->name; break;
      case CODE_EOL_CRLF:
	outcode->name = (char *)XSYMBOL (outcode->eol_crlf)->name; break;
      case CODE_EOL_CR:
	outcode->name = (char *)XSYMBOL (outcode->eol_cr)->name; break;
      }
    }
    buf2 = (char *)malloc(DECODE_BUF_SIZE(n, outcode));
    buf2_used = 1;
    n = decode(outcode, buf1, buf2, n);
  }

  if (n <= outsize) {
    bcopy(buf2, outbuf, n);
  } else {
    bcopy(buf2, outbuf, outsize);
    mule_error = MULE_ERROR_OVERFLOW;
    sprintf(mule_error_msg, "Output buffer overflow");
  }

  if (buf2_used) free(buf2);
  if (buf1_used) free(buf1);
  return (n <= outsize ? n : -1);
}

/****************
 * MSDOS staffs *
 ****************/

#ifdef MSDOS

static char emacsroot[MAXPATHLEN];

void
init_environment (argc, argv, skip_args)
     int argc;
     char **argv;
     int skip_args;
{
  char *s, *t, *root;
  int len;

  /* Find our root from argv[0].  Assuming argv[0] is, say,
     "c:/emacs/bin/emacs.exe" our root will be "c:/emacs".  */
  len = strlen (argv[0]);
  root = alloca (len + 10);  /* A little extra space for the stuff below.  */
  strcpy (root, argv[0]);
  while (len > 0 && root[len] != '/' && root[len] != ':')
    len--;
  root[len] = '\0';
  if (len > 4 && strcmp (root + len - 4, "/bin") == 0)
    root[len - 4] = '\0';
  else
    strcpy (root, "c:/emacs");  /* Only under debuggers, I think.  */
  len = strlen (root);
  strcpy (emacsroot, root);
}

char *
rootrelativepath (rel)
     char *rel;
{
  static char result[MAXPATHLEN + 10];

  strcpy (result, emacsroot);
  strcat (result, "/");
  strcat (result, rel);
  return result;
}

#endif /* MSDOS */

/***************
 * INITIALIZER *
 ***************/

mulelib_initialize(argc, argv, charsets, codings)
     int argc;
     char **argv, *charsets, *codings;
{
  if (!mulelib_initialized) {
    int i;

#ifdef MSDOS
    init_environment(argc, argv, 0);
#endif

    for (i = 0; i < 128; i++) {
      font_name[i] = NULL;
      font_encoding[i] = 0;
    }

    if (charsets_initialize(charsets) < 0) return -1;
    if (codings_initialize(codings) < 0) return -1;
    mulelib_initialized = 1;
  }
  return 0;
}
