/* -*-C-*-

$Id: sysprim.c,v 9.57 2008/09/27 03:59:13 cph Exp $

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

/* Random system primitives.  Most are implemented in terms of
   utilities in os.c */

#include "scheme.h"
#include "prims.h"
#include "ostty.h"
#include "ostop.h"

#ifdef HAVE_FENV_H
#  include <fenv.h>
#endif

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
    (MAKE_POINTER_OBJECT ((arg_ulong_index_integer (1, N_TYPE_CODES)), Free));
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
  (ulong_to_integer (ADDRESS_TO_DATUM (address)))

DEFINE_PRIMITIVE ("GC-SPACE-STATUS", Prim_gc_space_status, 0, 0, 0)
{
  PRIMITIVE_HEADER (0);
  {
    SCHEME_OBJECT v = (make_vector (12, SHARP_F, true));
    VECTOR_SET (v, 0, (ULONG_TO_FIXNUM (sizeof (SCHEME_OBJECT))));
    VECTOR_SET (v, 1, (CONVERT_ADDRESS (constant_start)));
    VECTOR_SET (v, 2, (CONVERT_ADDRESS (constant_alloc_next)));
    VECTOR_SET (v, 3, (CONVERT_ADDRESS (constant_end)));
    VECTOR_SET (v, 4, (CONVERT_ADDRESS (heap_start)));
    VECTOR_SET (v, 5, (CONVERT_ADDRESS (Free)));
    VECTOR_SET (v, 6, (CONVERT_ADDRESS (heap_alloc_limit)));
    VECTOR_SET (v, 7, (CONVERT_ADDRESS (heap_end)));
    VECTOR_SET (v, 8, (CONVERT_ADDRESS (stack_start)));
    VECTOR_SET (v, 9, (CONVERT_ADDRESS (stack_pointer)));
    VECTOR_SET (v, 10, (CONVERT_ADDRESS (stack_guard)));
    VECTOR_SET (v, 11, (CONVERT_ADDRESS (stack_end)));
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

#pragma STDC FENV_ACCESS on

enum { FRMODE_NEAREST, FRMODE_TOWARD_ZERO, FRMODE_DOWNWARD, FRMODE_UPWARD };

DEFINE_PRIMITIVE ("FLOAT-ROUNDING-MODES", Prim_float_rounding_modes, 0, 0, 0)
{
  unsigned int modes = 0;
  PRIMITIVE_HEADER (0);
#ifdef HAVE_FEGETROUND
#  ifdef FE_TONEAREST
  modes |= (1 << FRMODE_NEAREST);
#  endif
#  ifdef FE_TOWARDZERO
  modes |= (1 << FRMODE_TOWARD_ZERO);
#  endif
#  ifdef FE_DOWNWARD
  modes |= (1 << FRMODE_DOWNWARD);
#  endif
#  ifdef FE_UPWARD
  modes |= (1 << FRMODE_UPWARD);
#  endif
#endif
  PRIMITIVE_RETURN (ulong_to_integer (modes));
}

DEFINE_PRIMITIVE ("GET-FLOAT-ROUNDING-MODE", Prim_get_float_rounding_mode, 0, 0, 0)
{
  int mode;
  PRIMITIVE_HEADER (0);
#ifdef HAVE_FEGETROUND
  mode = (fegetround ());
  if (mode < 0)
    error_external_return ();
  switch (mode)
    {
#ifdef FE_TONEAREST
    case FE_TONEAREST: PRIMITIVE_RETURN (ulong_to_integer (FRMODE_NEAREST));
#endif
#ifdef FE_TOWARDZERO
    case FE_TOWARDZERO: PRIMITIVE_RETURN (ulong_to_integer (FRMODE_TOWARD_ZERO));
#endif
#ifdef FE_DOWNWARD
    case FE_DOWNWARD: PRIMITIVE_RETURN (ulong_to_integer (FRMODE_DOWNWARD));
#endif
#ifdef FE_UPWARD
    case FE_UPWARD: PRIMITIVE_RETURN (ulong_to_integer (FRMODE_UPWARD));
#endif
    default: PRIMITIVE_RETURN (SHARP_F);
    }
#else
  error_unimplemented_primitive ();
  PRIMITIVE_RETURN (UNSPECIFIC);
#endif
}

static int float_rounding_mode = (-1);

DEFINE_PRIMITIVE ("SET-FLOAT-ROUNDING-MODE", Prim_set_float_rounding_mode, 1, 1, 0)
{
  int mode = (-1);
  PRIMITIVE_HEADER (1);
#ifdef HAVE_FESETROUND
  switch (arg_ulong_integer (1))
    {
#ifdef FE_TONEAREST
    case FRMODE_NEAREST: mode = FE_TONEAREST; break;
#endif
#ifdef FE_TOWARDZERO
    case FRMODE_TOWARD_ZERO: mode = FE_TOWARDZERO; break;
#endif
#ifdef FE_DOWNWARD
    case FRMODE_DOWNWARD: mode = FE_DOWNWARD; break;
#endif
#ifdef FE_UPWARD
    case FRMODE_UPWARD: mode = FE_UPWARD; break;
#endif
    default: error_bad_range_arg (1); break;
    }
  if ((fesetround (mode)) == 0)
    {
      float_rounding_mode = mode;
      PRIMITIVE_RETURN (SHARP_T);
    }
  else
    {
      float_rounding_mode = (-1);
      PRIMITIVE_RETURN (SHARP_F);
    }
#else
  error_unimplemented_primitive ();
  PRIMITIVE_RETURN (UNSPECIFIC);
#endif
}

/* This kludge is to work around the fact that setjmp saves the
    floating-point rounding mode and longjmp restores it.  */
void
fixup_float_rounding_mode (void)
{
#ifdef HAVE_FESETROUND
  if (float_rounding_mode >= 0)
    fesetround (float_rounding_mode);
#endif
}
