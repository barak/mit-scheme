/* -*-C-*-

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008, 2009, 2010, 2011, 2012 Massachusetts Institute
    of Technology

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

/* Memory management */

#ifndef SCM_MEMMAG_H
#define SCM_MEMMAG_H 1

/* Overflow detection, various cases */

#define GC_ENABLED_P() (INTERRUPT_ENABLED_P (INT_GC))

#define HEAP_AVAILABLE							\
  ((unsigned long) ((FREE_OK_P (Free)) ? (heap_alloc_limit - Free) : 0))

#define FREE_OK_P(free)							\
  (((free) >= heap_start) && ((free) < heap_alloc_limit))

#define HEAP_AVAILABLE_P(n_words)					\
  ((FREE_OK_P (Free)) && ((Free + (n_words)) <= heap_alloc_limit))

#define GC_NEEDED_P(n_words)						\
  ((!HEAP_AVAILABLE_P (n_words)) && (GC_ENABLED_P ()))

#define SPACE_BEFORE_GC()						\
  ((GC_ENABLED_P ())							\
   ? HEAP_AVAILABLE							\
   : (ADDRESS_IN_HEAP_P (Free))						\
   ? ((unsigned long) (heap_end - Free))				\
   : 0)

#define REQUEST_GC(n_words) do						\
{									\
  REQUEST_INTERRUPT (INT_GC);						\
  gc_space_needed = (n_words);						\
} while (0)

#define RESET_HEAP_ALLOC_LIMIT() do					\
{									\
  heap_alloc_limit = (heap_end - heap_reserved);			\
  COMPILER_SETUP_INTERRUPT ();						\
} while (0)

#define ARG_HEAP_RESERVED(n)						\
  (arg_ulong_index_integer ((n), ((heap_end - heap_start) / 2)))

#define ADDRESS_IN_MEMORY_BLOCK_P(address)				\
  (((address) >= memory_block_start) && ((address) < memory_block_end))

#define ADDRESS_IN_HEAP_P(address)					\
  (((address) >= heap_start) && ((address) < heap_end))

#define ADDRESS_IN_STACK_P(address)					\
  (((address) >= stack_start) && ((address) < stack_end))

#define ADDRESS_IN_CONSTANT_P(address)					\
  (((address) >= constant_start) && ((address) < constant_end))

/* buffer for impurify, etc. */
#ifndef CONSTANT_SPACE_FUDGE
#  define CONSTANT_SPACE_FUDGE 128
#endif

extern bool allocations_ok_p (unsigned long, unsigned long, unsigned long);
extern void reset_allocator_parameters (unsigned long, unsigned long);
extern bool object_in_heap_p (SCHEME_OBJECT);
extern void std_gc_pt1 (void);
extern void std_gc_pt2 (void);
extern void stack_death (const char *) NORETURN;

#endif /* SCM_MEMMAG_H */
