/* -*-C-*-

$Id: intrpt.h,v 1.19 1997/04/02 07:43:58 cph Exp $

Copyright (c) 1987-97 Massachusetts Institute of Technology

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

/* Interrupt bits -- scanned from LSB (1) to MSB (16) */

#define INT_Stack_Overflow	1	/* Local interrupt */
#define INT_Global_GC		2
#define INT_GC			4	/* Local interrupt */
#define INT_Global_1		8
#define INT_Character		16	/* Local interrupt */
#define INT_AFTER_GC		32	/* Local interrupt */
#define INT_Timer		64	/* Local interrupt */
#define INT_Global_3		128
#define INT_Suspend		256	/* Local interrupt */
#define INT_Global_Mask		\
  (INT_Global_GC | INT_Global_1 | INT_Global_3)

/* Descartes profiling interrupts */

#define INT_IPPB_Flush		512	/* Local interrupt */
#define INT_IPPB_Extend	       1024	/* Local interrupt */
#define INT_PCBPB_Flush	       2048	/* Local interrupt */
#define INT_PCBPB_Extend       4096	/* Local interrupt */
#define INT_HCBPB_Flush	       8192	/* Local interrupt */
#define INT_HCBPB_Extend      16384	/* Local interrupt */

#define INT_Step_CC	      32768		

#define Global_GC_Level		1
#define Global_1_Level		3
#define Global_3_Level		7
#define MAX_INTERRUPT_NUMBER   15	/* 2^15 = INT_Step_CC */

#define INT_Mask		((1 << (MAX_INTERRUPT_NUMBER + 1)) - 1)

/* Utility macros. */

#define PENDING_INTERRUPTS()						\
  ((FETCH_INTERRUPT_MASK ()) & (FETCH_INTERRUPT_CODE ()))

#define INTERRUPT_QUEUED_P(mask) (((FETCH_INTERRUPT_CODE ()) & (mask)) != 0)

#define INTERRUPT_ENABLED_P(mask) (((FETCH_INTERRUPT_MASK ()) & (mask)) != 0)

#define INTERRUPT_PENDING_P(mask) (((PENDING_INTERRUPTS ()) & (mask)) != 0)

#define COMPILER_SETUP_INTERRUPT() do					\
{									\
  (Registers[REGBLOCK_MEMTOP]) =					\
    (((PENDING_INTERRUPTS ()) != 0)					\
     ? ((SCHEME_OBJECT) -1)						\
     : (INTERRUPT_ENABLED_P (INT_GC))					\
     ? ((SCHEME_OBJECT) (ADDR_TO_SCHEME_ADDR (MemTop)))			\
     : ((SCHEME_OBJECT) (ADDR_TO_SCHEME_ADDR (Heap_Top))));		\
  (Registers[REGBLOCK_STACK_GUARD]) =					\
    ((INTERRUPT_ENABLED_P (INT_Stack_Overflow))				\
     ? ((SCHEME_OBJECT) (ADDR_TO_SCHEME_ADDR (Stack_Guard)))		\
     : ((SCHEME_OBJECT) (ADDR_TO_SCHEME_ADDR (Stack_Bottom))));		\
} while (0)

#define FETCH_INTERRUPT_MASK() ((long) (Registers[REGBLOCK_INT_MASK]))

#define SET_INTERRUPT_MASK(mask)					\
{									\
  GRAB_INTERRUPT_REGISTERS ();						\
  (Registers[REGBLOCK_INT_MASK]) = ((SCHEME_OBJECT) (mask));		\
  COMPILER_SETUP_INTERRUPT ();						\
  RELEASE_INTERRUPT_REGISTERS ();					\
}

#define FETCH_INTERRUPT_CODE() ((long) (Registers[REGBLOCK_INT_CODE]))

#define REQUEST_INTERRUPT(code)						\
{									\
  GRAB_INTERRUPT_REGISTERS ();						\
  (Registers[REGBLOCK_INT_CODE]) =					\
    ((SCHEME_OBJECT) ((FETCH_INTERRUPT_CODE ()) | (code)));		\
  COMPILER_SETUP_INTERRUPT ();						\
  RELEASE_INTERRUPT_REGISTERS ();					\
}

#define CLEAR_INTERRUPT_NOLOCK(code)					\
{									\
  (Registers[REGBLOCK_INT_CODE]) =					\
    ((SCHEME_OBJECT) ((FETCH_INTERRUPT_CODE ()) &~ (code)));		\
  COMPILER_SETUP_INTERRUPT ();						\
}

#define CLEAR_INTERRUPT(code)						\
{									\
  GRAB_INTERRUPT_REGISTERS ();						\
  CLEAR_INTERRUPT_NOLOCK (code);					\
  RELEASE_INTERRUPT_REGISTERS ();					\
}

#define INITIALIZE_INTERRUPTS()						\
{									\
  GRAB_INTERRUPT_REGISTERS ();						\
  (Registers[REGBLOCK_INT_MASK]) = ((SCHEME_OBJECT) INT_Mask);		\
  (Registers[REGBLOCK_INT_CODE]) = ((SCHEME_OBJECT) 0);			\
  COMPILER_SETUP_INTERRUPT ();						\
  RELEASE_INTERRUPT_REGISTERS ();					\
}

#if defined(_OS2) || defined(WINNT)

#define GRAB_INTERRUPT_REGISTERS() OS_grab_interrupt_registers ()
#define RELEASE_INTERRUPT_REGISTERS() OS_release_interrupt_registers ()

extern void OS_grab_interrupt_registers (void);
extern void OS_release_interrupt_registers (void);

#else /* not (_OS2 or WINNT) */

#define GRAB_INTERRUPT_REGISTERS()
#define RELEASE_INTERRUPT_REGISTERS()

#endif /* not (_OS2 or WINNT) */
