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

/* Copy objects into constant/pure space.  */

#include "scheme.h"
#include "prims.h"
#include "gccode.h"

static void purify (SCHEME_OBJECT);

/* Purify increases the size of constant space at the expense of the
   heap.  A GC-like relocation is performed with the object being
   purified as the root.  The object is copied and relocated from the
   heap to the area adjacent to constant space.  Then a normal GC is
   finished after changing the end of constant-space marker.  */

DEFINE_PRIMITIVE ("PRIMITIVE-PURIFY", Prim_primitive_purify, 3, 3,
		  "(OBJECT PURE? SAFETY-MARGIN)\n\
Copy OBJECT from the heap into constant/pure space.\n\
PURE? is ignored.")
{
  PRIMITIVE_HEADER (3);
  canonicalize_primitive_context ();

  stack_check_fatal ("PURIFY");

  SCHEME_OBJECT object = ARG_REF (1);
  unsigned long safety_margin = ARG_HEAP_RESERVED (3);
  pop_primitive_frame (3);

  ENTER_CRITICAL_SECTION ("purify");
  heap_reserved = safety_margin;
  purify (object);

  stack_check (CONT_SIZE);
  push_cont (RC_NORMAL_GC_DONE,
             cons (SHARP_T,
                   ULONG_TO_FIXNUM ((HEAP_AVAILABLE > gc_space_needed)
                                    ? HEAP_AVAILABLE - gc_space_needed
                                    : 0)));

  RENAME_CRITICAL_SECTION ("purify daemon");
  SCHEME_OBJECT daemon = vector_ref (fixed_objects, GC_DAEMON);
  if (daemon == SHARP_F)
    PRIMITIVE_ABORT (PRIM_POP_RETURN);
  stack_check (2);
  stack_push (daemon);
  stack_push (make_apply_frame_header (1));
  PRIMITIVE_ABORT (PRIM_APPLY);
  /*NOTREACHED*/
  PRIMITIVE_RETURN (UNSPECIFIC);
}

static void
purify (SCHEME_OBJECT object)
{
  SCHEME_OBJECT * start_copy;
  SCHEME_OBJECT * new_constant_alloc_next;
  SCHEME_OBJECT * heap_copy_start;

  stack_check_fatal ("PURIFY");

  open_tospace (constant_alloc_next);
  initialize_weak_chain ();

  start_copy = (get_newspace_ptr ());
  add_to_tospace (object);

  current_gc_table = (std_gc_table ());
  gc_scan_tospace (start_copy, 0);

  new_constant_alloc_next = (get_newspace_ptr ());
  increment_tospace_ptr (CONSTANT_SPACE_FUDGE);
  heap_copy_start = (get_newspace_ptr ());

  std_gc_pt1 ();

  constant_alloc_next = new_constant_alloc_next;
  constant_end = heap_copy_start;
  heap_start = constant_end;

  std_gc_pt2 ();

  resize_tospace (heap_end - heap_start);
}
