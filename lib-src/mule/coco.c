#include <stdio.h>
#include "mulelib.h"

#define IN_BUF_SIZE 4096
#define OUT_BUF_SIZE (IN_BUF_SIZE * 5 + CONV_BUF_EXTRA)

static char *coco_version = "2.2";

static int
strpcmp(p0, p1)
     char **p0, **p1;
{
  return strcmp(*p0, *p1);
}

coco_list(stream)
     FILE *stream;
{
  int i, j, k, clm, len, maxlen;
  char **names;

  names = (char **)malloc((sizeof names[0]) * n_base_coding_system);
  maxlen = 0;
  for (i = 0; i < n_base_coding_system; i++) {
    names[i] = coding_system_table[i].name;
    len = strlen(names[i]);
    if (len > maxlen) maxlen = len;
  }
  qsort (names, n_base_coding_system, sizeof names[0], strpcmp);

  maxlen += 2;
  clm = 79 / maxlen;
  k = n_base_coding_system / clm;
  if (n_base_coding_system % clm) k++;
  fprintf (stream, "You can specify the following coding-systems:\n");
  for (i = 0; i < k; i++) {
    len = 0;
    for (j = 0; j < clm; j++) {
      if (i + j * k < n_base_coding_system) {
	if (len < maxlen * j)
	  while (len < maxlen * j) {
	    putc(' ', stream);
	    len++;
	  }
	fprintf (stream, "  %s", names[i + j * k]);
	len += strlen(names[i + j * k]) + 2;
      }
    }
    putc ('\n', stream);
  }
  fprintf (stream, "You can add \"unix*\", \"dos*\", or \"mac*\"");
  fprintf (stream, " at the tail of these names\n");
  fprintf (stream, " to specify type of end-of-line");
  fprintf (stream, "(LF[default], CRLF, or CR).\n");

  free(names);

  return 0;
}

main(argc, argv)
     int argc;
     char **argv;
{
  int help = 0, list = 0, verbose = 0, query = 0;
  int in_total = 0, out_total = 0, n;
  char inbuf[IN_BUF_SIZE], outbuf[OUT_BUF_SIZE];
  char *inname = NULL, *outname = NULL;
  coding_type incode, outcode;

  if (mulelib_initialize(argc, argv, NULL, NULL) < 0) {
    fprintf(stderr, "Initialization of Mule libarary failed.\n");
    exit(1);
  }

  while (--argc > 0) {
    argv++;
    if (!strcmp(*argv, "-h")) { help = list = 1; break; }
    if (!strcmp(*argv, "-l")) { list = 1; break; }
    if (!strcmp(*argv, "-v")) { verbose = 1; continue; }
    if (!strcmp(*argv, "-q")) { query = 1; continue; }
    if (!query) {
      if (outname != NULL) inname = outname;
      outname = *argv;
    }
  }

  if (help) {
    printf("Version %s (mulelib version %s)\n",
	   coco_version, mule_library_version);
    printf("Usage: coco [-h|-l|-v|-q] [[INCODE] OUTCODE] <INFILE >OUTFILE\n");
    printf("  -h: print this message\n");
    printf("  -l: list valid INCODE and OUTCODE\n");
    printf("  -v: print information about conversion\n");
    printf("  -q: just investigate how INFILE is encoded\n");
    printf("  INCODE: coding-system of INFILE, defaults to `*autoconv*'\n");
    printf("  OUTCODE: coding-system of OUTFILE, defaults to `*internal*'\n");
  }

  if (list) {
    coco_list(stdout);
    exit(0);
  }

  if (set_coding_system(inname, &incode, outname, &outcode) < 0) {
    fprintf(stderr, "%s\n", mule_error_msg);
    exit(1);
  }

  while ((n = read(0, inbuf, sizeof inbuf)) > 0) {
    in_total += n;
    if ((n = code_conversion(&incode, inbuf, n, &outcode, outbuf,
			     OUT_BUF_SIZE)) < 0) {
      if (!query) write(1, outbuf, OUT_BUF_SIZE);
      exit(1);
    }
    if (!query && n > 0) write(1, outbuf, n);
    out_total += n;
  }

  if (query) {
    fprintf(stderr, "In (%s): %6d bytes\n", incode.name, in_total);
    exit(0);
  }

  /* Flush out an unfinished state (e.g. terminating with "ESC ( B"). */
  n = code_conversion(&incode, inbuf, 0, &outcode, outbuf, OUT_BUF_SIZE);
  if (n > 0) write(1, outbuf, n);
  out_total += n;

  if (verbose)
    fprintf(stderr, "In :%6d bytes of %s\nOut:%6d bytes of %s\n",
	    in_total, incode.name, out_total, outcode.name);
  exit(0);
}
