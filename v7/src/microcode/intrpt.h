/* -*-C-*-

$Id: intrpt.h,v 1.10 1992/09/14 20:34:20 cph Exp $

Copyright (c) 1987-92 Massachusetts Institute of Technology

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

/* Interrupt manipulation utilities. */

/* The interrupt control registers. */

/* interrupts requesting */
#define IntCode ((long) (Registers[REGBLOCK_INT_CODE]))
#define set_IntCode(code)						\
  (Registers[REGBLOCK_INT_CODE]) = ((SCHEME_OBJECT) (code))

/* interrupts enabled */
#define IntEnb ((long) (Registers[REGBLOCK_INT_MASK]))
#define set_IntEnb(mask)						\
  (Registers[REGBLOCK_INT_MASK]) = ((SCHEME_OBJECT) (mask))

/* Interrupt bits -- scanned from LSB (1) to MSB (16) */

#define INT_Stack_Overflow	1	/* Local interrupt */
#define INT_Global_GC		2
#define INT_GC			4	/* Local interrupt */
#define INT_Global_1		8
#define INT_Character		16	/* Local interrupt */
#define INT_Global_2		32
#define INT_Timer		64	/* Local interrupt */
#define INT_Global_3		128
#define INT_Suspend		256	/* Local interrupt */
#define INT_Global_Mask		\
  (INT_Global_GC | INT_Global_1 | INT_Global_2 | INT_Global_3)

#define Global_GC_Level		1
#define Global_1_Level		3
#define Global_2_Level		5
#define Global_3_Level		7
#define MAX_INTERRUPT_NUMBER	8

#define INT_Mask		((1 << (MAX_INTERRUPT_NUMBER + 1)) - 1)

/* Utility macros. */

#define PENDING_INTERRUPTS() (IntEnb & IntCode)

#define INTERRUPT_QUEUED_P(mask) ((IntCode & (mask)) != 0)

#define INTERRUPT_ENABLED_P(mask) ((IntEnb & (mask)) != 0)

#define INTERRUPT_PENDING_P(mask) (((PENDING_INTERRUPTS()) & (mask)) != 0)

#define COMPILER_SETUP_INTERRUPT()					\
{									\
  (Regs [REGBLOCK_MEMTOP]) =						\
    ((INTERRUPT_PENDING_P (INT_Mask))					\
     ? ((SCHEME_OBJECT) -1)						\
     : ((SCHEME_OBJECT) MemTop));					\
}

#define FETCH_INTERRUPT_MASK()		(IntEnb)

#define SET_INTERRUPT_MASK(mask)					\
{									\
  set_IntEnb (mask);							\
  COMPILER_SETUP_INTERRUPT ();						\
}

#define FETCH_INTERRUPT_CODE()		(IntCode)

#define CLEAR_INTERRUPT(code)						\
{									\
  set_IntCode (IntCode &~ (code));					\
  COMPILER_SETUP_INTERRUPT ();						\
}

#define REQUEST_INTERRUPT(code)						\
{									\
  set_IntCode (IntCode | (code));					\
  COMPILER_SETUP_INTERRUPT ();						\
}

#define INITIALIZE_INTERRUPTS()						\
{									\
  set_IntEnb (0);							\
  set_IntCode (0);							\
  SET_INTERRUPT_MASK (INT_Mask);					\
  CLEAR_INTERRUPT (INT_Mask);						\
}

/* Compatibility */

#define COMPILER_SET_MEMTOP COMPILER_SETUP_INTERRUPT
