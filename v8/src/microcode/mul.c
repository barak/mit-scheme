/* -*-C-*-

Copyright (c) 1987, 1988, 1989 Massachusetts Institute of Technology

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

/* $Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v8/src/microcode/mul.c,v 9.25 1989/02/17 15:05:19 jinx Exp $
 *
 * This file contains the portable fixnum multiplication procedure.
 * Returns NIL if the result does not fit in a fixnum.
 * Note: This has only been tried on machines with long = 32 bits.
 * This file is included in the appropriate os file if needed.
 */

#define HALF_WORD_SIZE	((sizeof(long)*CHAR_SIZE)/2)
#define HALF_WORD_MASK	(1<<HALF_WORD_SIZE)-1
#define MAX_MIDDLE	(1<<((ADDRESS_LENGTH-1)-HALF_WORD_SIZE))
#define MAX_FIXNUM	(1<<ADDRESS_LENGTH)
#define	ABS(x)		(((x) < 0) ? -(x) : (x))

Pointer
Mul(Arg1, Arg2)
     Pointer Arg1, Arg2;
{
  long A, B, C;
  fast unsigned long Hi_A, Hi_B, Lo_A, Lo_B, Lo_C, Middle_C;
  Boolean Sign;

  Sign_Extend(Arg1, A);
  Sign_Extend(Arg2, B);
  Sign = ((A < 0) == (B < 0));
  A = ABS(A);
  B = ABS(B);
  Hi_A = ((A >> HALF_WORD_SIZE) & HALF_WORD_MASK);
  Hi_B = ((B >> HALF_WORD_SIZE) & HALF_WORD_MASK);
  if ((Hi_A > 0) && (Hi_B > 0))
    return (NIL);
  Lo_A = (A & HALF_WORD_MASK);
  Lo_B = (B & HALF_WORD_MASK);
  Lo_C = (Lo_A * Lo_B);
  if (Lo_C >= FIXNUM_SIGN_BIT)
    return (NIL);
  Middle_C = (Lo_A * Hi_B) + (Hi_A * Lo_B);
  if (Middle_C >= MAX_MIDDLE)
    return (NIL);
  C = Lo_C + (Middle_C << HALF_WORD_SIZE);
  if (Fixnum_Fits(C))
  {
    if (Sign || (C == 0))
      return (MAKE_UNSIGNED_FIXNUM(C));
    else
      return (MAKE_UNSIGNED_FIXNUM(MAX_FIXNUM - C));
  }
  return (NIL);
}
