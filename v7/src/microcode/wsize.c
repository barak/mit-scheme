/* -*-C-*-

Copyright (c) 1987 Massachusetts Institute of Technology

This material was developed by the Scheme project at the Massachusetts
Institute of Technology, Department of Electrical Engineering and
Computer Science.  Permission to copy this software, to redistribute
it, and to use it for any purpose is granted, subject to the following
restrictions and understandings.

1. Any copy made of this software must include this copyright notice
in full.

2. Users of this software agree to make their best efforts (a) to
return to the MIT Scheme project any improvements or extensions that
they make, so that these may be included in future releases; and (b)
to inform MIT of noteworthy uses of this software.

3. All materials developed as a consequence of the use of this
software shall duly acknowledge such use, in accordance with the usual
standards of acknowledging credit in academic research.

4. MIT has made no warrantee or representation that the operation of
this software will be error-free, and MIT is under no obligation to
provide any services, by way of maintenance, update, or otherwise.

5. In conjunction with products arising from the use of this material,
there shall be no use of the name of the Massachusetts Institute of
Technology nor of any adaptation thereof in any advertising,
promotional, or sales literature without prior written consent from
MIT in each case. */

/* $Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/microcode/Attic/wsize.c,v 9.22 1987/07/07 02:22:55 jinx Rel $ */

#include <stdio.h>
#include <math.h>
#include <errno.h>

extern int errno;
extern char *malloc();
extern free();

/* Some machines do not set ERANGE by default. */
/* This attempts to fix this. */

#ifdef celerity
#define hack_signal
#endif

#ifdef hack_signal

#define setup_error() signal(SIGFPE, range_error)

range_error()
{
  setup_error();
  errno = ERANGE;
}
#else
#define setup_error()
#endif


#define ARR_SIZE 20000
#define MEM_SIZE 400000

/* Force program data to be relatively large. */

static long dummy[ARR_SIZE];

/* Note: comments are printed in a weird way because some
   C compilers eliminate them even from strings.
*/

main()
{
  double accum, delta;
  int count, expt_size, char_size, mant_size;
  unsigned long to_be_shifted;
  unsigned bogus;
  char *temp;

  setup_error();

  for(bogus = ((unsigned) -1), count = 0;
      bogus != 0;
      count += 1)
    bogus >>= 1;

  char_size = count/(sizeof(unsigned));
  temp = malloc(MEM_SIZE*sizeof(long));
  if (temp == NULL)
    printf("/%c Cannot allocate %d Pointers. %c/\n",
           '*', MEM_SIZE, '*');
  else count = free(temp);

  if (((unsigned long) temp) < (1 << ((char_size*sizeof(long))-8)))
    printf("#define Heap_In_Low_Memory\n");
  else
    printf("/%c Heap is not in Low Memory. %c/\n", '*', '*');
  	
  to_be_shifted = -1;
  if ((to_be_shifted >> 1) != to_be_shifted)
    printf("#define UNSIGNED_SHIFT\n");
  else
    printf("/%c unsigned longs use arithmetic shifting. %c/\n", 
           '*', '*');

  printf("#define CHAR_SIZE            %d\n",
	 char_size);

  printf("#define USHORT_SIZE          %d\n",
	 (sizeof(unsigned short) * char_size));

  printf("#define ULONG_SIZE           %d\n",
	 (sizeof(unsigned long) * char_size));

  printf("/%c Flonum (double) size is %d bits. %c/\n",
	 '*', (char_size*sizeof(double)), '*');
  
  for(mant_size = 0, accum = 1.0, delta = 0.5;
      ((accum + delta) != accum);
      accum = accum + delta,
      delta /= 2.0,
      mant_size += 1) ;

  for(errno = 0, expt_size = 0, bogus = 1, accum = 0.0;
      errno != ERANGE;
      expt_size += 1, bogus <<= 1)
  {
    delta = accum;
    accum = pow(2.0, ((double) bogus));
    if (accum == delta)
      break;
  }

  expt_size -= 1;

  printf("#define FLONUM_EXPT_SIZE     %d\n", expt_size);
  printf("#define FLONUM_MANTISSA_BITS %d\n", mant_size);
  printf("#define MAX_FLONUM_EXPONENT  %d\n", ((1 << expt_size) - 1));
  printf("/%c Representation %s hidden bit. %c/\n", '*',
	 (((2+expt_size+mant_size) > (char_size*sizeof(double))) ?
	  "uses" :
	  "does not use"), '*');
  return;	
}
