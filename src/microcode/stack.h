/* -*-C-*-

$Id: stack.h,v 9.48 2008/01/30 20:02:20 cph Exp $

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

/* Stack abstraction */

#define SET_STACK_LIMITS(addr, n_words) do				\
{									\
  stack_start = (addr);							\
  stack_end = (stack_start + (n_words));				\
} while (0)

#define STACK_BOTTOM stack_end
#define STACK_TOP stack_start

#define INITIALIZE_STACK() do						\
{									\
  stack_pointer = STACK_BOTTOM;						\
  (*STACK_TOP) = (MAKE_BROKEN_HEART (STACK_TOP));			\
  stack_guard = (STACK_TOP + STACK_GUARD_SIZE);				\
  COMPILER_SETUP_INTERRUPT ();						\
} while (0)

#define STACK_OVERFLOWED_P()						\
  ((*STACK_TOP) != (MAKE_BROKEN_HEART (STACK_TOP)))

#ifndef STACK_RESET
#  define STACK_RESET() do {} while (0)
#endif

#define STACK_CHECK(n) do						\
{									\
  if (!CAN_PUSH_P (n))							\
    {									\
      STACK_CHECK_FATAL ("STACK_CHECK");				\
      REQUEST_INTERRUPT (INT_Stack_Overflow);				\
    }									\
} while (0)

#define STACK_CHECK_FATAL(s) do						\
{									\
  if (STACK_OVERFLOWED_P ())						\
    stack_death (s);							\
} while (false)

#define CAN_PUSH_P(n) (SP_OK_P (STACK_LOC (- (n))))
#define SP_OK_P(sp) ((sp) >= stack_guard)

#define STACK_LOCATIVE_DECREMENT(locative) (-- (locative))
#define STACK_LOCATIVE_INCREMENT(locative) ((locative) ++)
#define STACK_LOCATIVE_OFFSET(locative, offset) ((locative) + (offset))
#define STACK_LOCATIVE_REFERENCE(locative, offset) ((locative) [(offset)])
#define STACK_LOCATIVE_DIFFERENCE(newer, older) ((older) - (newer))
#define STACK_LOCATIVE_LESS_P(loc1, loc2) ((loc1) < (loc2))

#define ADDRESS_IN_STACK_REGION_P(address, lower_limit, upper_limit)	\
  (((address) >= (lower_limit)) && ((address) < (upper_limit)))

#define STACK_N_PUSHED (stack_end - stack_pointer)
#define SP_TO_N_PUSHED(sp, start, end) ((end) - (sp))
#define N_PUSHED_TO_SP(np, start, end) ((end) - (np))

#define STACK_LOCATIVE_PUSH(locative) (* (STACK_LOCATIVE_DECREMENT (locative)))
#define STACK_LOCATIVE_POP(locative) (* (STACK_LOCATIVE_INCREMENT (locative)))

#define STACK_PUSH(object) (STACK_LOCATIVE_PUSH (stack_pointer)) = (object)
#define STACK_POP() (STACK_LOCATIVE_POP (stack_pointer))
#define STACK_LOC(offset) (STACK_LOCATIVE_OFFSET (stack_pointer, (offset)))
#define STACK_REF(offset) (STACK_LOCATIVE_REFERENCE (stack_pointer, (offset)))
