/* -*-C-*-

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016,
    2017, 2018, 2019, 2020, 2021, 2022 Massachusetts Institute of
    Technology

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

#ifndef SCM_STACK_H
#define SCM_STACK_H 1

#include "object.h"
#include "memmag.h"

static inline void
stack_push (SCHEME_OBJECT object)
{
  *--stack_pointer = object;
}

static inline SCHEME_OBJECT
stack_pop (void)
{
  return *stack_pointer++;
}

static inline SCHEME_OBJECT*
stack_loc (unsigned long offset)
{
  return stack_pointer + offset;
}

static inline SCHEME_OBJECT
stack_ref (unsigned long offset)
{
  return *stack_loc (offset);
}

static inline void
stack_set (unsigned long offset, SCHEME_OBJECT object)
{
  *stack_loc (offset) = object;
}

static inline void
decrement_sp (unsigned long offset)
{
  stack_pointer -= offset;
}

static inline void
increment_sp (unsigned long offset)
{
  stack_pointer += offset;
}

static inline unsigned long
stack_n_pushed (void)
{
  return stack_end - stack_pointer;
}

static inline void
set_stack_limits (SCHEME_OBJECT* start, unsigned long size)
{
  stack_start = start;
  stack_end = start + size;
}

static inline bool
stack_overflowed_p (void)
{
  return *stack_start != MAKE_BROKEN_HEART (stack_start);
}

static inline bool
stack_can_push_p (unsigned long n)
{
  return stack_pointer - n >= stack_guard;
}

#define STACK_BOTTOM stack_end
#define STACK_TOP stack_start

#define INITIALIZE_STACK() do						\
{									\
  stack_pointer = STACK_BOTTOM;						\
  (*STACK_TOP) = (MAKE_BROKEN_HEART (STACK_TOP));			\
  stack_guard = (STACK_TOP + STACK_GUARD_SIZE);				\
  COMPILER_SETUP_INTERRUPT ();						\
} while (0)

#ifndef STACK_RESET
#  define STACK_RESET() do {} while (0)
#endif

#define SP_OK_P(sp) ((sp) >= stack_guard)

#define STACK_LOCATIVE_DECREMENT(locative) (-- (locative))
#define STACK_LOCATIVE_INCREMENT(locative) ((locative) ++)
#define STACK_LOCATIVE_OFFSET(locative, offset) ((locative) + (offset))
#define STACK_LOCATIVE_REFERENCE(locative, offset) ((locative) [(offset)])
#define STACK_LOCATIVE_DIFFERENCE(newer, older) ((older) - (newer))
#define STACK_LOCATIVE_ABOVE_P(loc1, loc2) ((loc1) < (loc2))
#define STACK_LOCATIVE_PUSH(locative) (* (STACK_LOCATIVE_DECREMENT (locative)))
#define STACK_LOCATIVE_POP(locative) (* (STACK_LOCATIVE_INCREMENT (locative)))

#define ADDRESS_IN_STACK_REGION_P(address, lower_limit, upper_limit)	\
  (((address) >= (lower_limit)) && ((address) < (upper_limit)))

#define SP_TO_N_PUSHED(sp, start, end) ((end) - (sp))
#define N_PUSHED_TO_SP(np, start, end) ((end) - (np))

#endif                          // SCM_STACK_H
