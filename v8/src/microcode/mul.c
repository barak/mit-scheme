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

/* $Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v8/src/microcode/mul.c,v 9.26 1989/02/19 17:51:47 jinx Rel $
 *
 * This file contains the fixnum multiplication procedure.
 * Returns NIL if the result does not fit in a fixnum.
 * Note: The portable version has only been tried on machines with
 * long = 32 bits.  This file is included in the appropriate os file.
 */

extern Pointer Mul();

#if defined(vax) && defined(bsd)

#define MUL_HANDLED

/* Note that "register" is used here (not "fast") since the
   assembly code requires knowledge of the location of
   the variables and they therefore must be in registers.
   This is a kludge.  It depends on what register variables
   get assigned to what registers.  It should be entirely
   coded in assembly language.  -- JINX
*/

Pointer
Mul(Arg1, Arg2)
     Pointer Arg1, Arg2;
{
  register long A, B, C;

  Sign_Extend(Arg1, A);
  Sign_Extend(Arg2, B);
  asm("	emul	r11,r10,$0,r10");  /* A is in 11, B in 10 */
  C = A;
  A = B;	/* What is all this shuffling? -- JINX */
  B = C;
  /* B should have high order result, A low order */
  if (((B == 0)  && (A & (-1 << 23)) == 0) ||
      ((B == -1) && (A & (-1 << 23)) == (-1 << 23)))
  {
    return (MAKE_SIGNED_FIXNUM(A));
  }
  else
  {
    return (NIL);
  }
}

#endif

/* 68k family code.  Uses hp9000s200 conventions for the new compiler. */

#if defined(hp9000s200) && !defined(old_cc) && !defined(__GNUC__)
#define MUL_HANDLED

/* The following constants are hard coded in the assembly language
 * code below.  The code assumes that d0 and d1 are scratch registers
 * for the compiler.
 */

#if (NIL != 0) || (TC_FIXNUM != 0x1A)
#include "Error: types changed.  Change assembly language appropriately"
#endif

#if defined(MC68020)

static long Fixnum_Range[2] = {SMALLEST_FIXNUM , BIGGEST_FIXNUM};

	asm("	text");
	asm("	global _Mul");
	asm("_Mul:");
	asm("	bfexts	4(%sp){&8:&24},%d0");
	asm("	bfexts	8(%sp){&8:&24},%d1");
	asm("	muls.l	%d1,%d0");
	asm("	bvs.b	result_is_nil");
	asm("	cmp2.l	%d0,_Fixnum_Range");
	asm("	bcs.b	result_is_nil");
	asm("	moveq	&0x1A,%d1");
	asm("	bfins	%d1,%d0{&0:&8}");
	asm("	rts");
	asm("result_is_nil:");
	asm("	clr.l	%d0");
	asm("	rts");
	asm("	data");

#else	/* not MC68020, but 68k family */

	/* 20(sp) = arg0; 24(sp) = arg1 because of movem */

	asm("	text");
	asm("	global _Mul");
	asm("_Mul:");
	asm("	movem.l	%d2-%d5,-(%sp)");
	asm("	clr.b	%d5");
	asm("	tst.b	21(%sp)");
	asm("	slt	20(%sp)");
	asm("	bge.b	coerce_1");
	asm("	moveq	&1,%d5");
	asm("	neg.l	20(%sp)");

	asm("coerce_1:");
	asm("	tst.b	25(%sp)");
	asm("	slt	24(%sp)");
	asm("	bge.b	after_coerce");
	asm("	eori.b	&1,%d5");
	asm("	neg.l	24(%sp)");
	asm("after_coerce:");
	asm("	move.l	20(%sp),%d0");
	asm("	move.l	24(%sp),%d1");
	asm("	move.w	%d0,%d2");
	asm("	mulu	%d1,%d2");
	asm("	move.w	%d1,%d4");
	asm("	swap	%d1");
	asm("	move.w	%d1,%d3");
	asm("	mulu	%d0,%d3");
	asm("	swap	%d0");
	asm("	mulu	%d0,%d4");
	asm("	add.l	%d4,%d3");
	asm("	bcs.b	result_is_nil");
	asm("	mulu	%d0,%d1");
	asm("	bne.b	result_is_nil");
	asm("	swap	%d2");
	asm("	add.w	%d3,%d2");
	asm("	bcs.b	result_is_nil");
	asm("	swap	%d3");
	asm("	tst.w	%d3");
	asm("	bne.b	result_is_nil");
	asm("	cmpi.w	%d2,&0x7F");
	asm("	bgt.b	result_is_nil");
	asm("	swap	%d2");
	asm("	tst.b	%d5");
	asm("	beq.b	sign_is_right");
	asm("	neg.l	%d2");
	asm("sign_is_right:");
	asm("	move.l	%d2,-(%sp)");
	asm("	move.b	&0x1A,(%sp)");
	asm("	move.l	(%sp)+,%d0");
	asm("	movem.l	(%sp)+,%d2-%d5");
	asm("	rts");
	asm("result_is_nil:");
	asm("	clr.l	%d0");
	asm("	movem.l	(%sp)+,%d2-%d5");
	asm("	rts");
	asm("	data");

#endif	/* not MC68020 */
#endif  /* hp9000s200 */

#ifndef MUL_HANDLED

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

#endif /* not MUL_HANDLED */
