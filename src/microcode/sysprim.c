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

/* Random system primitives.  Most are implemented in terms of
   utilities in os.c */

#include "scheme.h"
#include "prims.h"
#include "osscheme.h"
#include "ostty.h"
#include "ostop.h"

extern long OS_set_trap_state (long);
extern double arg_flonum (int);

/* Pretty random primitives */

DEFINE_PRIMITIVE ("EXIT", Prim_non_restartable_exit, 0, 0,
  "Exit Scheme with no option to restart.")
{
  PRIMITIVE_HEADER (0);
  termination_normal (0);
  PRIMITIVE_RETURN (UNSPECIFIC);
}

DEFINE_PRIMITIVE ("EXIT-WITH-VALUE",
		  Prim_non_restartable_exit_with_value, 1, 1,
  "Exit Scheme with no option to restart, returning integer argument\n\
as exit status.")
{
  PRIMITIVE_HEADER (1);
  termination_normal ((int) arg_integer (1));
  PRIMITIVE_RETURN (UNSPECIFIC);
}

DEFINE_PRIMITIVE ("HALT", Prim_restartable_exit, 0, 0,
  "Exit Scheme, suspending it to that it can be restarted.")
{
  PRIMITIVE_HEADER (0);
  OS_restartable_exit ();
  PRIMITIVE_RETURN (UNSPECIFIC);
}

DEFINE_PRIMITIVE ("UNDER-EMACS?", Prim_under_emacs_p, 0, 0, 0)
{
  PRIMITIVE_HEADER (0);
  PRIMITIVE_RETURN (BOOLEAN_TO_OBJECT (OS_under_emacs_p ()));
}

DEFINE_PRIMITIVE ("SET-TRAP-STATE!", Prim_set_trap_state, 1, 1, 0)
{
  long result;
  PRIMITIVE_HEADER (1);

  result = (OS_set_trap_state (arg_nonnegative_integer (1)));
  if (result < 0)
  {
    error_bad_range_arg (1);
    /*NOTREACHED*/
  }
  PRIMITIVE_RETURN (LONG_TO_UNSIGNED_FIXNUM (result));
}

DEFINE_PRIMITIVE ("HEAP-AVAILABLE?", Prim_heap_available_p, 1, 1,
  "(N-WORDS)\n\
Tests to see if there are at least N-WORDS words of heap storage available")
{
  PRIMITIVE_HEADER (1);
  PRIMITIVE_RETURN
    (BOOLEAN_TO_OBJECT (HEAP_AVAILABLE_P (arg_ulong_integer (1))));
}

DEFINE_PRIMITIVE ("PRIMITIVE-GET-FREE", Prim_get_free, 1, 1,
  "(TYPE-CODE)\n\
Return the value of the free pointer tagged with TYPE-CODE")
{
  PRIMITIVE_HEADER (1);
  PRIMITIVE_RETURN
    (make_pointer_object ((arg_ulong_index_integer (1, N_TYPE_CODES)), Free));
}

DEFINE_PRIMITIVE ("PRIMITIVE-INCREMENT-FREE", Prim_increment_free, 1, 1,
  "(N-WORDS)\n\
Advance the free pointer by N-WORDS words.")
{
  PRIMITIVE_HEADER (1);
  Free += (arg_ulong_integer (1));
  PRIMITIVE_RETURN (UNSPECIFIC);
}

#define CONVERT_ADDRESS(address)					\
  (ulong_to_integer (address_to_datum (address)))

DEFINE_PRIMITIVE ("GC-SPACE-STATUS", Prim_gc_space_status, 0, 0, 0)
{
  PRIMITIVE_HEADER (0);
  {
    SCHEME_OBJECT v = (make_vector (12, SHARP_F, true));
    vector_set (v, 0, (ULONG_TO_FIXNUM (sizeof (SCHEME_OBJECT))));
    vector_set (v, 1, (CONVERT_ADDRESS (constant_start)));
    vector_set (v, 2, (CONVERT_ADDRESS (constant_alloc_next)));
    vector_set (v, 3, (CONVERT_ADDRESS (constant_end)));
    vector_set (v, 4, (CONVERT_ADDRESS (heap_start)));
    vector_set (v, 5, (CONVERT_ADDRESS (Free)));
    vector_set (v, 6, (CONVERT_ADDRESS (heap_alloc_limit)));
    vector_set (v, 7, (CONVERT_ADDRESS (heap_end)));
    vector_set (v, 8, (CONVERT_ADDRESS (stack_start)));
    vector_set (v, 9, (CONVERT_ADDRESS (stack_pointer)));
    vector_set (v, 10, (CONVERT_ADDRESS (stack_guard)));
    vector_set (v, 11, (CONVERT_ADDRESS (stack_end)));
    PRIMITIVE_RETURN (v);
  }
}

DEFINE_PRIMITIVE ("SCHEME-PROGRAM-NAME", Prim_scheme_program_name, 0, 0, 0)
{
  PRIMITIVE_HEADER (0);
  PRIMITIVE_RETURN (char_pointer_to_string (scheme_program_name));
}

DEFINE_PRIMITIVE ("READ-BYTE-FROM-MEMORY", Prim_read_byte_from_memory, 1, 1,
  "(ADDRESS)\n\
Read a byte from memory at ADDRESS and return it as an unsigned integer.")
{
  PRIMITIVE_HEADER (1);
  PRIMITIVE_RETURN
    (ulong_to_integer (* ((unsigned char *) (arg_ulong_integer (1)))));
}

DEFINE_PRIMITIVE ("READ-WORD-FROM-MEMORY", Prim_read_word_from_memory, 1, 1,
  "(ADDRESS)\n\
Read a word from memory at ADDRESS and return it as an unsigned integer.")
{
  PRIMITIVE_HEADER (1);
  PRIMITIVE_RETURN
    (ulong_to_integer (* ((unsigned long *) (arg_ulong_integer (1)))));
}

DEFINE_PRIMITIVE ("READ-FLOAT-FROM-MEMORY", Prim_read_float_from_memory, 1, 1,
  "(ADDRESS)\n\
Read a float from memory at ADDRESS and return it as a flonum.")
{
  PRIMITIVE_HEADER (1);
  PRIMITIVE_RETURN (double_to_flonum (* ((double *) (arg_ulong_integer (1)))));
}

DEFINE_PRIMITIVE ("WRITE-BYTE-TO-MEMORY", Prim_write_byte_to_memory, 2, 2,
  "(BYTE ADDRESS)\n\
Write BYTE to memory at ADDRESS.")
{
  PRIMITIVE_HEADER (2);
  (* ((unsigned char *) (arg_ulong_integer (2))))
    = (arg_index_integer (1, 0x100));
  PRIMITIVE_RETURN (UNSPECIFIC);
}

DEFINE_PRIMITIVE ("WRITE-WORD-TO-MEMORY", Prim_write_word_to_memory, 2, 2,
  "(WORD ADDRESS)\n\
Write WORD to memory at ADDRESS.")
{
  PRIMITIVE_HEADER (2);
  (* ((unsigned long *) (arg_ulong_integer (2)))) = (arg_ulong_integer (1));
  PRIMITIVE_RETURN (UNSPECIFIC);
}

DEFINE_PRIMITIVE ("WRITE-FLOAT-TO-MEMORY", Prim_write_float_to_memory, 2, 2,
  "(FLOAT ADDRESS)\n\
Write FLOAT to memory at ADDRESS.")
{
  PRIMITIVE_HEADER (2);
  (* ((double *) (arg_ulong_integer (2)))) = (arg_flonum (1));
  PRIMITIVE_RETURN (UNSPECIFIC);
}

DEFINE_PRIMITIVE ("CC-BLOCK-LINKAGE-INFO", Prim_cc_block_linkage_info, 1, 1, 0)
{
  PRIMITIVE_HEADER (1);
  CHECK_ARG (1, CC_BLOCK_P);
  PRIMITIVE_RETURN (cc_block_linkage_info (ARG_REF (1)));
}

DEFINE_PRIMITIVE ("control-point-next-frame", Prim_cpoint_next_frame, 2, 2, 0)
{
  PRIMITIVE_HEADER (1);
  CHECK_ARG (1, CONTROL_POINT_P);
  SCHEME_OBJECT cpoint = ARG_REF (1);
  unsigned long length = vector_length (cpoint);
  unsigned long index = arg_ulong_index_integer (2, length);
  SCHEME_OBJECT* frame = vector_loc (cpoint, index);
  if (!RETURN_CODE_P (cont_frame_ret (frame)))
    error_bad_range_arg (1);
  unsigned long offset = next_stack_frame_offset (frame);
  if (offset == ULONG_MAX)
    PRIMITIVE_RETURN (SHARP_F);
  unsigned long next_index = index + offset;
  if (next_index > length)
    error_external_return ();
  if (next_index < length
      && !RETURN_CODE_P (cont_frame_ret (vector_loc (cpoint, next_index))))
    error_external_return ();
  assert (ULONG_TO_FIXNUM_P (next_index));
  return ULONG_TO_FIXNUM (next_index);
}
