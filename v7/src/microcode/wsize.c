/* -*-C-*-

Copyright (c) 1989 Massachusetts Institute of Technology

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

/* $Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/microcode/Attic/wsize.c,v 9.25 1989/02/15 18:47:04 jinx Exp $ */

#include <stdio.h>
#include <math.h>
#include <errno.h>

#define false 0
#define true 1

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

/* Structure used to find double alignment constraints. */

struct double_probe {
  long field_1;
  double field_2;
} proble_double[2];

/* Note: comments are printed in a weird way because some
   C compilers eliminate them even from strings.
*/

main()
{
  double accum[3], delta, dtemp;
  int count, expt_size, char_size, mant_size, double_size, extra;
  unsigned long to_be_shifted;
  unsigned bogus;
  char buffer[sizeof(long)];
  char *temp;

  setup_error();

  for(bogus = ((unsigned) -1), count = 0;
      bogus != 0;
      count += 1)
    bogus >>= 1;

  char_size = count / (sizeof(unsigned));

  temp = malloc(MEM_SIZE*sizeof(long));
  if (temp == NULL)
  {
    printf("/%c CONFUSION: Can't allocate %d Pointers. %c/\n",
           '*', MEM_SIZE, '*');
    printf("/%c Will not assume that the Heap is in Low Memory. %c/\n",
	   '*', '*');
  }
  else
  {
    count = free(temp);
    if (((unsigned long) temp) < (1 << ((char_size * sizeof(long)) - 8)))
      printf("#define Heap_In_Low_Memory\n");
    else
      printf("/%c Heap is not in Low Memory. %c/\n", '*', '*');
  }
  	
  to_be_shifted = -1;
  if ((to_be_shifted >> 1) != to_be_shifted)
    printf("#define UNSIGNED_SHIFT\n");
  else
    printf("/%c unsigned longs use arithmetic shifting. %c/\n", 
           '*', '*');

  if (sizeof(long) == sizeof(char))
  {
    printf("/%c sizeof(long) == sizeof(char); no byte order problems! %c/\n",
	   '*', '*');
  }
  else
  {
    buffer[0] = 1;
    for (count = 1; count < sizeof(long); )
      buffer[count++] = 0;
    if (*((long *) &buffer[0]) == 1)
      printf("#define VAX_BYTE_ORDER\n");
    else
      printf("/%c VAX_BYTE_ORDER not used. %c/\n", '*', '*');
  }

  double_size = (char_size*sizeof(double));

  printf("#define CHAR_SIZE            %d\n",
	 char_size);

  printf("#define USHORT_SIZE          %d\n",
	 (sizeof(unsigned short) * char_size));

  printf("#define ULONG_SIZE           %d\n",
	 (sizeof(unsigned long) * char_size));

  printf("/%c Flonum (double) size is %d bits. %c/\n",
	 '*', double_size, '*');
  
  if (sizeof(struct double_probe) == (sizeof(double) + sizeof(long)))
  {
    printf("/%c Flonums have no special alignment constraints. %c/\n",
	   '*', '*');
  }
  else if ((sizeof(struct double_probe) != (2 * sizeof(double))) ||
	   ((sizeof(double) % sizeof(long)) != 0))
  {
    printf("/%c CONFUSION: Can't determine float alignment constraints! %c/\n",
	   '*', '*');
    printf("/%c Please define FLOATING_ALIGNMENT by hand. %c/\n", '*', '*');
  }
  else
  {
    printf("#define FLOATING_ALIGNMENT   0x%lx\n", (sizeof(double)-1));
  }

  mant_size = 1;
  accum[0] = 1.0;
  accum[1] = 0.0;
  delta = 0.5;

  while(true)
  {
    accum[2] = accum[1];
    accum[1] = (accum[0] + delta);
    if ((accum[1] == accum[0]) ||
	(accum[2] == accum[1]) ||
	(mant_size == double_size))
      break;
    delta = (delta / ((double) 2.0));
    mant_size += 1;
  }

  printf("#define FLONUM_MANTISSA_BITS %d\n", mant_size);

  for(errno = 0, expt_size = 0, bogus = 1, dtemp = 0.0;
      ((errno != ERANGE) && (expt_size <= double_size));
      expt_size += 1, bogus <<= 1)
  {
    delta = dtemp;
    dtemp = pow(((double) 2.0), ((double) bogus));
    if (dtemp == delta)
      break;
  }

  expt_size -= 1;

  printf("#define FLONUM_EXPT_SIZE     %d\n", expt_size);
  printf("#define MAX_FLONUM_EXPONENT  %d\n", ((1 << expt_size) - 1));

  extra = ((2 + expt_size + mant_size) - double_size);

  if (extra > 1)
  {
    printf("/%c CONFUSION: Can't determine floating parameters! %c/\n",
	   '*', '*');
    printf("/%c Please fix above three parameters by hand. %c/\n", '*', '*');
  }
  else
  {
    printf("/%c Floating point representation %s hidden bit. %c/\n", '*',
	   ((extra == 1) ? "uses" : "does not use"), '*');
  }
  return;	
}
