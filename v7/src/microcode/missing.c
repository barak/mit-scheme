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

/* $Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/microcode/missing.c,v 9.21 1987/01/22 14:29:02 jinx Rel $
 * This file contains utilities potentially missing from the math library
 */

#ifdef DEBUG_MISSING
#include "config.h"
#endif

static Boolean floating_table_initialized = false;
static double floating_table[(2*FLONUM_EXPT_SIZE)-1];
static int exponent_table[(2*FLONUM_EXPT_SIZE)-1];

void initialize_floating_table()
{ register int index, exponent;
  register int *int_table = &exponent_table[FLONUM_EXPT_SIZE-1];
  register double *the_table = &floating_table[FLONUM_EXPT_SIZE-1];
  register double x;
  the_table[0] = 1.0;
  int_table[0] = 0;
  for (x = 2.0, index = 1, exponent = 1;
       index < FLONUM_EXPT_SIZE;
       x *= x, index += 1, exponent += exponent)
  { the_table[index] = x;
    int_table[index] = exponent;
  }
  for (x = 0.5, index = -1, exponent = -1;
       index > -FLONUM_EXPT_SIZE;
       x *= x, index -= 1, exponent += exponent)
  { the_table[index] = x;
    int_table[index] = exponent;
  }
  floating_table_initialized = true;
  return;
}

double frexp(value, eptr)
double value;
int *eptr;
{ register double mant;
  register int exponent, index;
  register double *the_table = &floating_table[FLONUM_EXPT_SIZE-1];
  register int *int_table = &exponent_table[FLONUM_EXPT_SIZE-1];

  if (value == 0.0)
  { *eptr = 0;
    return 0.0;
  }
  if (!floating_table_initialized) initialize_floating_table();
  mant = ((value < 0.0) ? -value : value);
  exponent = 0;
  while (mant < 0.5)
  { for (index = -FLONUM_EXPT_SIZE+1;
	 the_table[index] < mant;
	 index += 1) ;
    exponent += int_table[index];
    mant /= the_table[index];
  }
  if (mant >= 1.0)
  { while (mant >= 2.0)
    { for (index = FLONUM_EXPT_SIZE-1;
	   the_table[index] > mant;
	   index -= 1) ;
      exponent += int_table[index];
      mant /= the_table[index];
    }
    mant /= 2.0;
    exponent += 1;
  }
  *eptr = exponent;
  return ((value < 0.0) ? -mant : mant);
}

double ldexp(value, exponent)
register double value;
register int exponent;
{ register int index;
  register double *the_table = &floating_table[FLONUM_EXPT_SIZE-1];
  register int *int_table = &exponent_table[FLONUM_EXPT_SIZE-1];

  if (value == 0.0) return 0.0;
  if (!floating_table_initialized) initialize_floating_table();
  while (exponent > 0)
  { for(index = FLONUM_EXPT_SIZE-1;
	int_table[index] > exponent;
	index -= 1) ;
    exponent -= int_table[index];
    value *= the_table[index];
  }
  while (exponent < 0)
  { for(index = -FLONUM_EXPT_SIZE+1;
	int_table[index] < exponent;
	index += 1) ;
    exponent -= int_table[index];
    value *= the_table[index];
  }
  return value;
}


#ifdef DEBUG_MISSING

#include <stdio.h>

main()
{ double input, output;
  int exponent;

  while (true)
  { printf("Number -> ");
    scanf("%F", &input);
    output = frexp(input, &exponent);
    printf("Input = %G; Output = %G; Exponent = %d\n",
	   input, output, exponent);
    printf("Result = %G\n", ldexp(output, exponent));
  }
}
#endif

