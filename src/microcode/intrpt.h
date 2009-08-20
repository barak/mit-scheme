/* -*-C-*-

$Id$

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008 Massachusetts Institute of Technology

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

#define INT_Stack_Overflow	0x0001UL /* Local interrupt */
#define INT_Global_GC		0x0002UL
#define INT_GC			0x0004UL /* Local interrupt */
#define INT_Global_1		0x0008UL
#define INT_Character		0x0010UL /* Local interrupt */
#define INT_AFTER_GC		0x0020UL /* Local interrupt */
#define INT_Timer		0x0040UL /* Local interrupt */
#define INT_Global_3		0x0080UL
#define INT_Suspend		0x0100UL /* Local interrupt */

/* Descartes profiling interrupts */

#define INT_IPPB_Flush		0x0200UL /* Local interrupt */
#define INT_IPPB_Extend		0x0400UL /* Local interrupt */
#define INT_PCBPB_Flush		0x0800UL /* Local interrupt */
#define INT_PCBPB_Extend	0x1000UL /* Local interrupt */
#define INT_HCBPB_Flush		0x2000UL /* Local interrupt */
#define INT_HCBPB_Extend	0x4000UL /* Local interrupt */

#define INT_Step_CC		0x8000UL

#define INT_Global_Mask (INT_Global_GC | INT_Global_1 | INT_Global_3)

#define Global_GC_Level		0x1UL
#define Global_1_Level		0x3UL
#define Global_3_Level		0x7UL
#define MAX_INTERRUPT_NUMBER	0xFUL	/* 2^15 = INT_Step_CC */

#define INT_Mask		((1UL << (MAX_INTERRUPT_NUMBER + 1)) - 1)

/* Utility macros. */

#define PENDING_INTERRUPTS() (GET_INT_MASK & GET_INT_CODE)
#define PENDING_INTERRUPTS_P ((PENDING_INTERRUPTS ()) != 0)
#define INTERRUPT_QUEUED_P(mask) ((GET_INT_CODE & (mask)) != 0)
#define INTERRUPT_ENABLED_P(mask) ((GET_INT_MASK & (mask)) != 0)
#define INTERRUPT_PENDING_P(mask) (((PENDING_INTERRUPTS ()) & (mask)) != 0)

#define COMPILER_SETUP_INTERRUPT() do					\
{									\
  SET_MEMTOP								\
    (((PENDING_INTERRUPTS ()) != 0)					\
     ? memory_block_start						\
     : (GC_ENABLED_P ())						\
     ? heap_alloc_limit							\
     : heap_end);							\
  SET_STACK_GUARD							\
    ((INTERRUPT_ENABLED_P (INT_Stack_Overflow))				\
     ? stack_guard							\
     : STACK_TOP);							\
} while (0)

#define SET_INTERRUPT_MASK(mask) do					\
{									\
  GRAB_INTERRUPT_REGISTERS ();						\
  SET_INT_MASK (mask);							\
  COMPILER_SETUP_INTERRUPT ();						\
  RELEASE_INTERRUPT_REGISTERS ();					\
} while (0)

#define REQUEST_INTERRUPT(code) do					\
{									\
  GRAB_INTERRUPT_REGISTERS ();						\
  SET_INT_CODE (GET_INT_CODE | (code));					\
  COMPILER_SETUP_INTERRUPT ();						\
  RELEASE_INTERRUPT_REGISTERS ();					\
} while (0)

#define CLEAR_INTERRUPT_NOLOCK(code) do					\
{									\
  SET_INT_CODE (GET_INT_CODE &~ (code));				\
  COMPILER_SETUP_INTERRUPT ();						\
} while (0)

#define CLEAR_INTERRUPT(code) do					\
{									\
  GRAB_INTERRUPT_REGISTERS ();						\
  CLEAR_INTERRUPT_NOLOCK (code);					\
  RELEASE_INTERRUPT_REGISTERS ();					\
} while (0)

#define INITIALIZE_INTERRUPTS(mask) do					\
{									\
  GRAB_INTERRUPT_REGISTERS ();						\
  SET_INT_MASK (mask);							\
  SET_INT_CODE (0);							\
  COMPILER_SETUP_INTERRUPT ();						\
  RELEASE_INTERRUPT_REGISTERS ();					\
} while (0)

#if defined(__OS2__) || defined(__WIN32__)
   extern void OS_grab_interrupt_registers (void);
   extern void OS_release_interrupt_registers (void);
#  define GRAB_INTERRUPT_REGISTERS() OS_grab_interrupt_registers ()
#  define RELEASE_INTERRUPT_REGISTERS() OS_release_interrupt_registers ()
#else
#  define GRAB_INTERRUPT_REGISTERS()
#  define RELEASE_INTERRUPT_REGISTERS()
#endif
