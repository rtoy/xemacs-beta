/* Copies the dump file inside the xemacs executable */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

static const unsigned char key[] = {
  255,
  6,
  1,
  2,
  3,
  4,
  255,
  3,
  9,
  62,
  255,
  10,
  4,
  61,
  255
};

int main(int argc, char **argv)
{
  FILE *te, *xe, *dump;
  unsigned char *xed, *p;
  long size, size_dump, size1, i;
  long max_size, offset;

  char msg[65536];

  if(argc != 6 && (argc != 3 || strcmp(argv[1], "-s"))) {
    fprintf(stderr, "Usage:\n%s temacs xemacs.dmp xemacs size offset\n%s -s xemacs.dmp\n", argv[0], argv[0]);
    exit(1);
  }

  if(argc == 3) {
    sprintf(msg, "Opening %s failed", argv[2]);
    dump = fopen(argv[2], "rb+");
    if(!dump) {
      perror(msg);
      exit(1);
    }

    if(fseek(dump, 0, SEEK_END)) {
      perror("fseek end dump");
      exit(1);
    }

    size = ftell(dump);
    if(size == -1) {
      perror("ftell dump");
      exit(1);
    }

    printf("%ld\n", size);
    exit(0);
  }


  max_size = strtol(argv[4], 0, 10);
  offset   = strtol(argv[5], 0, 10);

  sprintf(msg, "Opening %s failed", argv[1]);
  te = fopen(argv[1], "rb");
  if(!te) {
    perror(msg);
    exit(1);
  }

  if(fseek(te, 0, SEEK_END)) {
    perror("fseek end");
    exit(1);
  }

  size = ftell(te);
  if(size == -1) {
    perror("ftell");
    exit(1);
  }

  if(fseek(te, 0, SEEK_SET)) {
    perror("fseek beginning");
    exit(1);
  }

  xed = malloc(size);
  if(!xed) {
    perror("malloc");
    exit(1);
  }

  size1 = fread(xed, 1, size, te);
  if(size1 != size) {
    if(ferror(te)) {
      perror("fread temacs");
      exit(1);
    }
    fprintf(stderr, "Fread returned %ld, expected %ld ?\n", size1, size);
    exit(1);
  }

  if(fclose(te)) {
    perror("fclose temacs");
    exit(1);
  }

  p = xed;
  for(i=0; i<size-(long)sizeof(key); i++) {
    if(!memcmp(p, key, sizeof(key)))
      goto found;
    p++;
  }

  fprintf(stderr, "dumped_data key not found in executable.\n");
  exit(1);

 found:
  fprintf(stderr, "dumped_data found at offset 0x%lx, patching.\n", i);

  sprintf(msg, "Opening %s failed", argv[2]);
  dump = fopen(argv[2], "r");
  if(!dump) {
    perror(msg);
    exit(1);
  }

  if(fseek(dump, 0, SEEK_END)) {
    perror("fseek end dump");
    exit(1);
  }

  size_dump = ftell(dump);
  if(size_dump == -1) {
    perror("ftell dump");
    exit(1);
  }

  if(size_dump > max_size) {
    fprintf(stderr, "Dump file too big for available space (max=%ld, dump=%ld)\n", max_size, size_dump);
    exit(2);
  }

  if(fseek(dump, 0, SEEK_SET)) {
    perror("fseek beginning dump");
    exit(1);
  }

  size1 = fread(xed+i+offset, 1, size_dump, dump);
  if(size1 != size_dump) {
    if(ferror(dump)) {
      perror("fread dump");
      exit(1);
    }
    fprintf(stderr, "Fread dump returned %ld, expected %ld ?\n", size1, size_dump);
    exit(1);
  }

  if(fclose(dump)) {
    perror("fclose dump");
    exit(1);
  }

  memset(xed+i, 0, offset);

  xed[i  ] = size_dump;
  xed[i+1] = size_dump >> 8;
  xed[i+2] = size_dump >> 16;
  xed[i+3] = size_dump >> 24;

  fprintf(stderr, "dumped_data found at offset 0x%lx, patching.\n", i);

  sprintf(msg, "Opening %s failed", argv[3]);
  xe = fopen(argv[3], "wb");
  if(!xe) {
    perror(msg);
    exit(1);
  }

  size1 = fwrite(xed, 1, size, xe);
  if(size1 != size) {
    if(ferror(xe)) {
      perror("fwrite xemacs");
      exit(1);
    }
    fprintf(stderr, "Fwrite xemacs returned %ld, expected %ld ?\n", size1, size);
    exit(1);
  }

  if(fclose(xe)) {
    perror("fclose xemacs");
    exit(1);
  }

  exit(0);
}

