/* -*- C -*- */

#include <stdio.h>

extern long external_long;
extern long * external_pointer;
extern long (* (procedure_table []))();

struct two_word_struct
{
  long foo;
  long bar;
};

struct two_word_struct
reveal_structure_convention (x, y)
     long x;
     long y;
{
  struct two_word_struct result;

  result.foo = (x + y);
  result.bar = (x - y);
  return (result);
}

long
dummy_proc_1 (x, y)
     long x;
     long y;
{
  return (x + y);
}

long
dummy_proc_2 (x, y)
     long x;
     long y;
{
  return (x - y);
}

long (* (procedure_table [])) () =
{
  dummy_proc_1,
  dummy_proc_2,
  dummy_proc_1,
  dummy_proc_2,
  dummy_proc_1,
  dummy_proc_2
};

long
reveal_register_partition (x0, x1, x2, x3, x4, x5, x6, x7, x8, x9,
			   y0, y1, y2, y3, y4, y5, y6, y7, y8, y9,
			   z0, z1, z2, z3, z4, z5, z6, z7, z8, z9,
			   q0, q1)
     long x0; long x1; long x2; long x3; long x4;
     long x5; long x6; long x7; long x8; long x9;
     long y0; long y1; long y2; long y3; long y4;
     long y5; long y6; long y7; long y8; long y9;
     long z0; long z1; long z2; long z3; long z4;
     long z5; long z6; long z7; long z8; long z9;
     long q0; long q1;
{
  return (  (x0 * y0) + (x1 * y1) + (x2 * y2) + (x3 * y3) + (x4 * y4)
	  + (x5 * y5) + (x6 * y6) + (x7 * y7) + (x8 * y8) + (x9 * y9)
	  + (y0 * z0) + (y1 * z1) + (y2 * z2) + (y3 * z3) + (y4 * z4)
	  + (y5 * z5) + (y6 * z6) + (y7 * z7) + (y8 * z8) + (y9 * z9)
	  + (z0 * q0) + (z1 * q1) + (z2 * x2) + (z3 * x3) + (z4 * x4)
	  + (z5 * x5) + (z6 * x6) + (z7 * x7) + (z8 * x8) + (z9 * x9)
	  + (q0 * x0) + (q1 * x1));
}

long external_long, * external_pointer;

main (argc, argv)
     int argc;
     char **argv;
{
  extern int atoi ();
  long values[33];
  int count;
  struct two_word_struct temp;

  if (argc != 33)
  {
    fprintf (stderr, "usage: Don't run this program!\n");
    exit (1);
  }
  for (count = 1; count < argc; count++)
    values[count] = ((long) (atoi (argv[count])));

  printf ("result = %ld",
	  (reveal_register_partition
	   (values[ 1], values[ 2], values[ 3], values[ 4], values[ 5],
	    values[ 6], values[ 7], values[ 8], values[ 9], values[10],
	    values[11], values[12], values[13], values[14], values[15],
	    values[16], values[17], values[18], values[19], values[20],
	    values[21], values[22], values[23], values[24], values[25],
	    values[26], values[27], values[28], values[29], values[30],
	    values[31], values[32])));

  temp = (reveal_structure_convention (values[31], values[32]));
  printf ("temp = {foo = %ld, bar = %ld}\n", temp.foo, temp.bar);
  external_long = 42;
  external_pointer = &external_long;
  printf ("*external_pointer = %ld; external_pointer = 0x%lx\n",
	  *external_pointer, external_pointer);
  printf ("value = %ld\n",
	  ((* (procedure_table[((int) (values[1]))])) (values[2], values[3])));
  exit (0);
}
