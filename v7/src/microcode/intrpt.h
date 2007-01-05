/* -*-C-*-

$Id: intrpt.h,v 1.25 2007/01/05 15:33:06 cph Exp $

Copyright 1987,1992,1993,1994,1997,2000 Massachusetts Institute of Technology
Copyright 2003 Massachusetts Institute of Technology

This file is part of MIT/GNU Scheme.

MIT/GNU Scheme is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or (at
your option) any later version.

MIT/GNU Scheme is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with MIT/GNU Scheme; if not, write to the Free Software
Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA 02110-1301,
USA.

*/

/* Interrupt manipulation utilities. */

/* Interrupt bits -- scanned from LSB (1) to MSB (16) */

#define INT_Stack_Overflow	0x0001	/* Local interrupt */
#define INT_Global_GC		0x0002
#define INT_GC			0x0004	/* Local interrupt */
#define INT_Global_1		0x0008
#define INT_Character		0x0010	/* Local interrupt */
#define INT_AFTER_GC		0x0020	/* Local interrupt */
#define INT_Timer		0x0040	/* Local interrupt */
#define INT_Global_3		0x0080
#define INT_Suspend		0x0100	/* Local interrupt */

/* Descartes profiling interrupts */

#define INT_IPPB_Flush		0x0200	/* Local interrupt */
#define INT_IPPB_Extend		0x0400	/* Local interrupt */
#define INT_PCBPB_Flush		0x0800	/* Local interrupt */
#define INT_PCBPB_Extend	0x1000	/* Local interrupt */
#define INT_HCBPB_Flush		0x2000	/* Local interrupt */
#define INT_HCBPB_Extend	0x4000	/* Local interrupt */

#define INT_Step_CC		0x8000

#define INT_Global_Mask (INT_Global_GC | INT_Global_1 | INT_Global_3)

#define Global_GC_Level		0x1
#define Global_1_Level		0x3
#define Global_3_Level		0x7
#define MAX_INTERRUPT_NUMBER	0xF	/* 2^15 = INT_Step_CC */

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

#if defined(__OS2__) || defined(__WIN32__)
   extern void OS_grab_interrupt_registers (void);
   extern void OS_release_interrupt_registers (void);
#  define GRAB_INTERRUPT_REGISTERS() OS_grab_interrupt_registers ()
#  define RELEASE_INTERRUPT_REGISTERS() OS_release_interrupt_registers ()
#else
#  define GRAB_INTERRUPT_REGISTERS()
#  define RELEASE_INTERRUPT_REGISTERS()
#endif
