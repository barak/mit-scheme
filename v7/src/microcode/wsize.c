/* -*-C-*-

$Id: wsize.c,v 9.35 2002/11/20 19:46:16 cph Exp $

Copyright (c) 1989-1999 Massachusetts Institute of Technology

This file is part of MIT Scheme.

MIT Scheme is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 2 of the License, or (at your
option) any later version.

MIT Scheme is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with MIT Scheme; if not, write to the Free Software Foundation,
Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.

*/

#include <stdio.h>
#include <math.h>
#include <errno.h>
#include <signal.h>
#include "ansidecl.h"
/* #include "config.h" */

#ifndef TYPE_CODE_LENGTH
/* This MUST match object.h */
#define TYPE_CODE_LENGTH	8
#endif

#define ASCII_LOWER_A		0141
#define ASCII_UPPER_A		0101

#define boolean			int
#define false			0
#define true			1

extern int errno;
extern PTR EXFUN (malloc, ());
extern void EXFUN (free, ());

/* The following hanky-panky courtesy of some buggy compilers. */

int
mul (x, y)
     int x; int y;
{
  return (x * y);
}

double
itod (n)
     int n;
{
  return ((double) (mul (n, 1)));
}

double
power (base, expo)
     double base;
     unsigned expo;
{
  double result = (itod (1));
  while (expo != 0)
  {
    if ((expo & 1) == 1)
    {
      result *= base;
      expo -= 1;
    }
    else
    {
      base *= base;
      expo >>= 1;
    }
  }
  return (result);
}

/* Some machines do not set ERANGE by default. */
/* This attempts to fix this. */

#ifdef SIGFPE

#  define setup_error()		signal(SIGFPE, range_error)

void
range_error()
{
  setup_error();
  errno = ERANGE;
  return;
}

#else /* not SIGFPE */

#  define setup_error()

#endif /* SIGFPE */

/* Force program data to be relatively large. */

#define ARR_SIZE		20000
#define MEM_SIZE		400000

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
  double accum[3], delta, dtemp, zero, one, two;
  int count, expt_size, char_size, mant_size, double_size, extra;
  unsigned long to_be_shifted;
  unsigned bogus;
  struct { long pad; char real_buffer[sizeof(long)]; } padding_buf;
  char * buffer, * temp;
  boolean confused;

  buffer = &padding_buf.real_buffer[0];
  confused = false;
  setup_error ();

  printf ("/%c CSCHEME configuration parameters. %c/\n", '*', '*');
  printf ("/%c REMINDER: Insert these definitions in config.h. %c/\n\n",
	  '*', '*');

  printf ("/%c REMINDER: Change the following definitions! %c/\n",
	  '*', '*');
  printf ("#define MACHINE_TYPE          \"Unknown machine, fix config.h\"\n");
  printf ("#define FASL_INTERNAL_FORMAT   FASL_UNKNOWN\n\n");

  if ((((int) 'a') == ASCII_LOWER_A) &&
      (((int) 'A') == ASCII_UPPER_A))
  {
    printf ("/%c The ASCII character set is used. %c/\n", '*', '*');
  }
  else
  {
    printf ("/%c The ASCII character set is NOT used. %c/\n", '*', '*');
    printf ("/%c REMINDER: Change the following definition! %c/\n",
	    '*', '*');
  }

  for (bogus = ((unsigned) -1), count = 0;
       bogus != 0;
       count += 1)
  {
    bogus >>= 1;
  }

  char_size = (count / (sizeof(unsigned)));

  temp = (malloc (MEM_SIZE * (sizeof(long))));
  if (temp == NULL)
  {
    confused = true;
    printf ("/%c CONFUSION: Could not allocate %d Objects. %c/\n",
	    '*', MEM_SIZE, '*');
    printf ("/%c Will not assume that the Heap is in Low Memory. %c/\n",
	    '*', '*');
  }
  else
  {
    free (temp);
    if (((unsigned long) temp) <
	(1 << ((char_size * sizeof(long)) - TYPE_CODE_LENGTH)))
      printf ("#define HEAP_IN_LOW_MEMORY     1\n");
    else
      printf ("/%c Heap is not in Low Memory. %c/\n", '*', '*');
  }

  to_be_shifted = -1;
  if ((to_be_shifted >> 1) == to_be_shifted)
  {
    printf ("/%c unsigned longs use arithmetic shifting. %c/\n", '*', '*');
    printf ("#define UNSIGNED_SHIFT_BUG\n");
  }
  else
  {
    printf ("/%c unsigned longs use logical shifting. %c/\n", '*', '*');
  }

  if ((sizeof(long)) == (sizeof(char)))
  {
    printf ("/%c sizeof(long) == sizeof(char); no byte order problems! %c/\n",
	    '*', '*');
  }
  else
  {
    buffer[0] = 1;
    for (count = 1; count < sizeof(long); )
    {
      buffer[count++] = 0;
    }
    if (*((long *) &buffer[0]) == 1)
    {
      printf("#define VAX_BYTE_ORDER         1\n\n");
    }
    else
    {
      printf("/%c VAX_BYTE_ORDER not used. %c/\n\n", '*', '*');
    }
  }

  double_size = (char_size*sizeof(double));

  printf ("#define CHAR_BIT               %d\n",
	  char_size);

  if (sizeof(struct double_probe) == (sizeof(double) + sizeof(long)))
  {
    printf ("/%c Flonums have no special alignment constraints. %c/\n",
	    '*', '*');
  }
  else if ((sizeof(struct double_probe) != (2 * sizeof(double))) ||
	   ((sizeof(double) % sizeof(long)) != 0))
  {
    confused = true;
    printf ("/%c CONFUSION: Can't determine float alignment constraints! %c/\n",
	    '*', '*');
    printf ("/%c Please define FLOATING_ALIGNMENT by hand. %c/\n", '*', '*');
  }
  else
  {
    printf ("#define FLOATING_ALIGNMENT     0x%lx\n", (sizeof(double)-1));
  }

  mant_size = 1;

  zero = (itod (0));
  one = (itod (1));
  two = (itod (2));

  accum[0] = one;
  accum[1] = zero;
  delta = (one / two);

  while (true)
  {
    accum[2] = accum[1];
    accum[1] = (accum[0] + delta);
    if ((accum[1] == accum[0]) ||
	(accum[2] == accum[1]) ||
	(mant_size == double_size))
      break;
    delta = (delta / two);
    mant_size += 1;
  }

  printf ("#define FLONUM_MANTISSA_BITS   %d\n", mant_size);

  for (errno = 0, expt_size = 0, bogus = 1, dtemp = zero;
       ((errno != ERANGE) && (expt_size <= double_size));
       expt_size += 1, bogus <<= 1)
  {
    delta = dtemp;
    dtemp = (power (two, bogus));
    if (dtemp == delta)
      break;
  }

  expt_size -= 1;

  printf ("#define FLONUM_EXPT_SIZE       %d\n", expt_size);
  printf ("#define MAX_FLONUM_EXPONENT    %d\n", ((1 << expt_size) - 1));

  extra = ((2 + expt_size + mant_size) - double_size);

  if (extra > 1)
  {
    confused = true;
    printf ("/%c CONFUSION: Can't determine floating parameters! %c/\n",
	    '*', '*');
    printf ("/%c Please fix above three parameters by hand. %c/\n", '*', '*');
  }
  else
  {
    printf ("/%c Floating point representation %s hidden bit. %c/\n", '*',
	    ((extra == 1) ? "uses" : "does not use"), '*');
  }
  if (confused)
  {
    fprintf (stderr, "Please examine carefully the \"confused\" parameters.\n");
    exit(1);
  }
  return;
}
